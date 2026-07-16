//! Effect-row inference for user-defined functions.
//!
//! Extends the proof boundary across the call graph. Every named function
//! gets an `EffectRow` recording the union of capabilities required by its
//! direct or transitive calls, plus determinism, purity, and recursion.
//!
//! Algorithm: bottom-up fixed-point over a per-function direct-effect base.
//! Each function's base row captures local facts (Date.now, fetchSync, calls
//! to imported module functions). Propagation iterates user-function calls
//! until rows stabilise. Recursion is detected by a final DFS over the
//! callee map; functions reachable from themselves are flagged.

const std = @import("std");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const module_binding = @import("module_binding.zig");
const builtin_modules = @import("builtin_modules.zig");
const manifest_registry_mod = @import("manifest_registry.zig");
const bool_checker = @import("bool_checker.zig");

const NodeIndex = ir.NodeIndex;
const IrView = ir.IrView;
const null_node = ir.null_node;
const Capability = module_binding.ModuleCapability;

pub const CapabilitySet = std.EnumSet(Capability);

/// A `zigttp:workflow` call (`call`, `saga`, `fanout`, or `follow`) found
/// while `durable_callback_depth > 0` - i.e. lexically nested inside a
/// `durable.step()` callback. `workflow.call` is only durable-recorded at
/// step depth 0 (see runtime_workflow.zig); nested inside a user `step()`
/// it silently loses durability at runtime with no error. This is an
/// unconditional structural fact, not gated behind a declared `Spec<...>`
/// or `Effects<...>` capsule - ZTS509 fires for every function regardless
/// of what it claims about itself.
pub const NestedWorkflowCall = struct {
    /// Index into `Analyzer.functions` for the function the call sits in.
    owner: usize,
    /// Static name of the offending export: "call", "saga", "fanout", or
    /// "follow". Borrowed from `imported.name`; never allocated.
    workflow_fn: []const u8,
};

pub const EffectRow = struct {
    capabilities: CapabilitySet = CapabilitySet.initEmpty(),
    deterministic: bool = true,
    pure: bool = true,
    recursive: bool = false,
    has_egress: bool = false,
    /// True when a direct or transitive call reaches an imported module export
    /// classified `.write`. Drives the per-function `read_only` capsule
    /// property: `read_only == !writes and !has_egress`.
    writes: bool = false,

    pub const empty: EffectRow = .{};

    pub fn merge(a: EffectRow, b: EffectRow) EffectRow {
        return .{
            .capabilities = a.capabilities.unionWith(b.capabilities),
            .deterministic = a.deterministic and b.deterministic,
            .pure = a.pure and b.pure,
            .recursive = a.recursive or b.recursive,
            .has_egress = a.has_egress or b.has_egress,
            .writes = a.writes or b.writes,
        };
    }

    pub fn eql(a: EffectRow, b: EffectRow) bool {
        return a.capabilities.eql(b.capabilities) and
            a.deterministic == b.deterministic and
            a.pure == b.pure and
            a.recursive == b.recursive and
            a.has_egress == b.has_egress and
            a.writes == b.writes;
    }

    /// `read_only` capsule property: the function performs no external writes
    /// and opens no egress. Derived, not stored, so it always tracks the
    /// other fields.
    pub fn readOnly(self: EffectRow) bool {
        return !self.writes and !self.has_egress;
    }
};

pub const FunctionEffect = struct {
    name: []const u8,
    binding_key: u32,
    decl_node: NodeIndex,
    body_node: NodeIndex,
    row: EffectRow = .{},
    /// Capabilities reached by this function's own direct module calls,
    /// captured before call-graph propagation. `row.capabilities` is the
    /// transitive union; `direct_caps` is the subset this function reaches
    /// itself, used to attribute a budget violation (ZTS607) to the helper
    /// that actually performs the call rather than every caller above it.
    direct_caps: CapabilitySet = CapabilitySet.initEmpty(),
    /// True when the function declaration sits under an `export`. The
    /// opt-in docs mode asks exported helpers to carry explicit capsules.
    exported: bool = false,
};

const ImportedFunction = struct {
    module: []const u8,
    name: []const u8,
};

const fetch_module_specifier: []const u8 = "zigttp:fetch";
const fetch_sync_export: []const u8 = "fetchSync";

/// Maximum fixed-point iterations. A single iteration propagates each user
/// call's current row into its caller. Convergence is bounded by the
/// longest acyclic chain plus one; in practice handlers rarely exceed
/// three or four hops, so 16 is a generous safety net.
const max_iterations: u8 = 16;

pub const Analyzer = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    manifest_registry: ?*const manifest_registry_mod.Registry,
    functions: std.ArrayListUnmanaged(FunctionEffect),
    /// Imported callee metadata keyed by local binding slot.
    imports: std.AutoHashMapUnmanaged(u16, ImportedFunction),
    /// Map binding slot to index into `functions`. Used to resolve identifier
    /// callees back to a user-defined function.
    user_fn_by_slot: std.AutoHashMapUnmanaged(u16, usize),
    /// CSR-encoded direct callees per function: callee_starts[i..i+1] selects
    /// a slice of callee_storage holding the callees of function i. Has
    /// `fn_count + 1` entries with a trailing sentinel.
    callee_starts: std.ArrayListUnmanaged(u32),
    callee_storage: std.ArrayListUnmanaged(usize),
    durable_callback_depth: u32,
    /// Every `zigttp:workflow` call found nested inside a `step()` callback.
    /// See `NestedWorkflowCall` - this is collected unconditionally during
    /// the same walk that computes effect rows, regardless of any declared
    /// `Spec<...>`/`Effects<...>` capsule.
    nested_workflow_calls: std.ArrayListUnmanaged(NestedWorkflowCall),

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*context.AtomTable) Analyzer {
        return initWithManifestRegistry(allocator, ir_view, atoms, null);
    }

    pub fn initWithManifestRegistry(
        allocator: std.mem.Allocator,
        ir_view: IrView,
        atoms: ?*context.AtomTable,
        manifest_registry: ?*const manifest_registry_mod.Registry,
    ) Analyzer {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .manifest_registry = manifest_registry,
            .functions = .empty,
            .imports = .empty,
            .user_fn_by_slot = .empty,
            .callee_starts = .empty,
            .callee_storage = .empty,
            .durable_callback_depth = 0,
            .nested_workflow_calls = .empty,
        };
    }

    pub fn deinit(self: *Analyzer) void {
        self.functions.deinit(self.allocator);
        self.imports.deinit(self.allocator);
        self.user_fn_by_slot.deinit(self.allocator);
        self.callee_starts.deinit(self.allocator);
        self.callee_storage.deinit(self.allocator);
        self.nested_workflow_calls.deinit(self.allocator);
    }

    pub fn analyze(self: *Analyzer, root: NodeIndex) !void {
        try self.scanImports();
        try self.collectFunctions(root);
        try self.computeBaseEffects();
        try self.propagate();
        try self.detectRecursion();
    }

    pub fn lookup(self: *const Analyzer, name: []const u8) ?EffectRow {
        for (self.functions.items) |fe| {
            if (std.mem.eql(u8, fe.name, name)) return fe.row;
        }
        return null;
    }

    pub fn all(self: *const Analyzer) []const FunctionEffect {
        return self.functions.items;
    }

    // ----- internal -----

    fn scanImports(self: *Analyzer) !void {
        const node_count = self.ir_view.nodeCount();
        var idx: usize = 0;
        while (idx < node_count) : (idx += 1) {
            const node: NodeIndex = @intCast(idx);
            if (self.ir_view.getTag(node) != .import_decl) continue;
            const import_decl = self.ir_view.getImportDecl(node) orelse continue;
            const module = self.ir_view.getString(import_decl.module_idx) orelse continue;
            for (0..import_decl.specifiers_count) |i| {
                const spec_idx = self.ir_view.getListIndex(import_decl.specifiers_start, @intCast(i));
                const spec = self.ir_view.getImportSpec(spec_idx) orelse continue;
                const name = self.resolveAtomName(spec.imported_atom) orelse continue;
                try self.imports.put(self.allocator, spec.local_binding.slot, .{
                    .module = module,
                    .name = name,
                });
            }
        }
    }

    fn collectFunctions(self: *Analyzer, root: NodeIndex) !void {
        try self.collectFunctionsIn(root, false);
    }

    fn collectFunctionsIn(self: *Analyzer, node: NodeIndex, exported: bool) WalkError!void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                for (0..block.stmts_count) |i| {
                    try self.collectFunctionsIn(self.ir_view.getListIndex(block.stmts_start, @intCast(i)), exported);
                }
            },
            .export_decl => {
                const export_decl = self.ir_view.getExportDecl(node) orelse return;
                try self.collectFunctionsIn(export_decl.declaration, true);
            },
            .function_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                try self.recordNamedFunction(decl.binding, node, decl.init, exported);
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                if (decl.init != null_node and self.isFunctionNode(decl.init)) {
                    try self.recordNamedFunction(decl.binding, node, decl.init, exported);
                }
            },
            else => {},
        }
    }

    fn recordNamedFunction(
        self: *Analyzer,
        binding: ir.BindingRef,
        decl_node: NodeIndex,
        fn_node: NodeIndex,
        exported: bool,
    ) !void {
        const func = self.ir_view.getFunction(fn_node) orelse return;
        const name = self.resolveAtomName(binding.name_atom) orelse return;
        const key = bool_checker.packBindingKey(binding.scope_id, binding.slot);
        const idx = self.functions.items.len;
        try self.functions.append(self.allocator, .{
            .name = name,
            .binding_key = key,
            .decl_node = decl_node,
            .body_node = func.body,
            .exported = exported,
        });
        try self.user_fn_by_slot.put(self.allocator, binding.slot, idx);
    }

    fn computeBaseEffects(self: *Analyzer) !void {
        const fn_count = self.functions.items.len;
        try self.callee_starts.resize(self.allocator, fn_count + 1);
        for (0..fn_count) |i| {
            self.callee_starts.items[i] = @intCast(self.callee_storage.items.len);
            var seen_users: std.AutoHashMapUnmanaged(usize, void) = .empty;
            defer seen_users.deinit(self.allocator);
            var row: EffectRow = .{};
            try self.walkBaseExpr(self.functions.items[i].body_node, i, &row, &seen_users);
            self.functions.items[i].row = row;
            // Snapshot the direct capability set before propagation unions in
            // the rows of transitively-called user functions.
            self.functions.items[i].direct_caps = row.capabilities;
        }
        self.callee_starts.items[fn_count] = @intCast(self.callee_storage.items.len);
    }

    fn calleeCount(self: *const Analyzer, fn_idx: usize) u32 {
        return self.callee_starts.items[fn_idx + 1] - self.callee_starts.items[fn_idx];
    }

    /// Direct callees of function `fn_idx`, as indices into `functions` -
    /// the public view of the CSR-encoded call graph.
    pub fn calleesOf(self: *const Analyzer, fn_idx: usize) []const usize {
        const start = self.callee_starts.items[fn_idx];
        const end = self.callee_starts.items[fn_idx + 1];
        return self.callee_storage.items[start..end];
    }

    const WalkError = std.mem.Allocator.Error;

    fn walkBaseStmt(
        self: *Analyzer,
        node: NodeIndex,
        owner: usize,
        row: *EffectRow,
        seen_users: *std.AutoHashMapUnmanaged(usize, void),
    ) WalkError!void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                for (0..block.stmts_count) |i| {
                    try self.walkBaseStmt(self.ir_view.getListIndex(block.stmts_start, @intCast(i)), owner, row, seen_users);
                }
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node) orelse return;
                try self.walkBaseExpr(if_stmt.condition, owner, row, seen_users);
                try self.walkBaseStmt(if_stmt.then_branch, owner, row, seen_users);
                try self.walkBaseStmt(if_stmt.else_branch, owner, row, seen_users);
            },
            .for_of_stmt => {
                const for_iter = self.ir_view.getForIter(node) orelse return;
                try self.walkBaseExpr(for_iter.iterable, owner, row, seen_users);
                try self.walkBaseStmt(for_iter.body, owner, row, seen_users);
            },
            .return_stmt, .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |value| {
                    try self.walkBaseExpr(value, owner, row, seen_users);
                }
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                // Nested named functions are their own units; the call graph
                // edge is recorded when this owner calls into them.
                if (decl.init != null_node and !self.isFunctionNode(decl.init)) {
                    try self.walkBaseExpr(decl.init, owner, row, seen_users);
                }
            },
            .function_decl => {
                // Skip nested declarations entirely - they're separate units.
            },
            else => try self.walkBaseExpr(node, owner, row, seen_users),
        }
    }

    fn walkBaseExpr(
        self: *Analyzer,
        node: NodeIndex,
        owner: usize,
        row: *EffectRow,
        seen_users: *std.AutoHashMapUnmanaged(usize, void),
    ) WalkError!void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .program, .block, .if_stmt, .for_of_stmt, .return_stmt, .expr_stmt, .var_decl, .function_decl => {
                try self.walkBaseStmt(node, owner, row, seen_users);
            },
            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return;
                try self.walkBaseExpr(bin.left, owner, row, seen_users);
                try self.walkBaseExpr(bin.right, owner, row, seen_users);
            },
            .unary_op, .spread => {
                const un = self.ir_view.getUnary(node) orelse return;
                try self.walkBaseExpr(un.operand, owner, row, seen_users);
            },
            .ternary => {
                const ternary = self.ir_view.getTernary(node) orelse return;
                try self.walkBaseExpr(ternary.condition, owner, row, seen_users);
                try self.walkBaseExpr(ternary.then_branch, owner, row, seen_users);
                try self.walkBaseExpr(ternary.else_branch, owner, row, seen_users);
            },
            .call, .method_call => {
                try self.handleCall(node, owner, row, seen_users);
                const call = self.ir_view.getCall(node) orelse return;
                const durable_step = self.isDurableStepCall(call);
                try self.walkBaseExpr(call.callee, owner, row, seen_users);
                for (0..call.args_count) |i| {
                    const arg = self.ir_view.getListIndex(call.args_start, @intCast(i));
                    if (durable_step and i == 1 and self.isFunctionNode(arg)) {
                        self.durable_callback_depth += 1;
                        defer self.durable_callback_depth -= 1;
                        try self.walkBaseExpr(arg, owner, row, seen_users);
                    } else {
                        try self.walkBaseExpr(arg, owner, row, seen_users);
                    }
                }
            },
            .member_access, .optional_chain, .computed_access => {
                const member = self.ir_view.getMember(node) orelse return;
                try self.walkBaseExpr(member.object, owner, row, seen_users);
                if (member.computed != null_node) try self.walkBaseExpr(member.computed, owner, row, seen_users);
            },
            .assignment => {
                const assign = self.ir_view.getAssignment(node) orelse return;
                try self.walkBaseExpr(assign.target, owner, row, seen_users);
                try self.walkBaseExpr(assign.value, owner, row, seen_users);
            },
            .array_literal => {
                const arr = self.ir_view.getArray(node) orelse return;
                for (0..arr.elements_count) |i| {
                    try self.walkBaseExpr(self.ir_view.getListIndex(arr.elements_start, @intCast(i)), owner, row, seen_users);
                }
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node) orelse return;
                for (0..obj.properties_count) |i| {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
                    const prop = self.ir_view.getProperty(prop_idx) orelse continue;
                    try self.walkBaseExpr(prop.value, owner, row, seen_users);
                }
            },
            .template_literal => {
                const tmpl = self.ir_view.getTemplate(node) orelse return;
                for (0..tmpl.parts_count) |i| {
                    const part = self.ir_view.getListIndex(tmpl.parts_start, @intCast(i));
                    if (self.ir_view.getOptValue(part)) |value| try self.walkBaseExpr(value, owner, row, seen_users);
                }
            },
            .match_expr => {
                const match = self.ir_view.getMatchExpr(node) orelse return;
                try self.walkBaseExpr(match.discriminant, owner, row, seen_users);
                for (0..match.arms_count) |i| {
                    const arm_idx = self.ir_view.getListIndex(match.arms_start, @intCast(i));
                    const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
                    try self.walkBaseExpr(arm.body, owner, row, seen_users);
                }
            },
            // Anonymous closures inherit their enclosing function's effect row
            // by-design: callbacks passed to map/filter run inside the caller.
            .function_expr, .arrow_function => {
                if (self.ir_view.getFunction(node)) |func| {
                    try self.walkBaseStmt(func.body, owner, row, seen_users);
                }
            },
            else => {},
        }
    }

    fn handleCall(
        self: *Analyzer,
        node: NodeIndex,
        owner: usize,
        row: *EffectRow,
        seen_users: *std.AutoHashMapUnmanaged(usize, void),
    ) WalkError!void {
        const call = self.ir_view.getCall(node) orelse return;

        if (self.calleeObjectProperty(call.callee)) |op| {
            if (self.durable_callback_depth == 0 and isNonDeterministic(op.object, op.property)) {
                row.deterministic = false;
                row.pure = false;
            }
        }

        if (self.ir_view.getTag(call.callee) != .identifier) return;
        const binding = self.ir_view.getBinding(call.callee) orelse return;

        if (self.imports.get(binding.slot)) |imported| {
            if (builtin_modules.fromSpecifier(imported.module)) |mb| {
                for (mb.required_capabilities) |cap| row.capabilities.insert(cap);
            }
            row.pure = false;
            // A `.write`-classified export modifies external state; that
            // demotes the enclosing function's `read_only` capsule property.
            if (self.importEffect(imported.module, imported.name) == .write) row.writes = true;
            if (std.mem.eql(u8, imported.module, fetch_module_specifier) and
                std.mem.eql(u8, imported.name, fetch_sync_export))
            {
                row.has_egress = true;
            }
            if (self.durable_callback_depth > 0 and
                std.mem.eql(u8, imported.module, "zigttp:workflow") and
                isNestedWorkflowExport(imported.name))
            {
                try self.nested_workflow_calls.append(self.allocator, .{
                    .owner = owner,
                    .workflow_fn = imported.name,
                });
            }
            return;
        }

        if (self.user_fn_by_slot.get(binding.slot)) |callee_idx| {
            if (callee_idx == owner) {
                // Self-recursion is captured here; non-self cycles are caught
                // during the dedicated detectRecursion pass.
                row.recursive = true;
            }
            const gop = try seen_users.getOrPut(self.allocator, callee_idx);
            if (!gop.found_existing) try self.callee_storage.append(self.allocator, callee_idx);
        }
    }

    fn isDurableStepCall(self: *const Analyzer, call: ir.Node.CallExpr) bool {
        if (self.ir_view.getTag(call.callee) != .identifier) return false;
        const binding = self.ir_view.getBinding(call.callee) orelse return false;
        const imported = self.imports.get(binding.slot) orelse return false;
        return std.mem.eql(u8, imported.module, "zigttp:durable") and
            std.mem.eql(u8, imported.name, "step");
    }

    /// ZTS509's export allow-list: the four `zigttp:workflow` exports that
    /// only durably record at step depth 0 (runtime_workflow.zig). Nested
    /// inside a `step()` callback, each one silently loses durability.
    fn isNestedWorkflowExport(name: []const u8) bool {
        return std.mem.eql(u8, name, "call") or
            std.mem.eql(u8, name, "saga") or
            std.mem.eql(u8, name, "fanout") or
            std.mem.eql(u8, name, "follow");
    }

    fn importEffect(self: *const Analyzer, module: []const u8, name: []const u8) module_binding.EffectClass {
        if (builtin_modules.findExport(module, name)) |exp| return exp.func.effect;
        const registry = self.manifest_registry orelse return .none;
        const exp = registry.findExport(module, name) orelse return .none;
        return exp.effect;
    }

    const ObjectProperty = struct { object: []const u8, property: []const u8 };

    fn calleeObjectProperty(self: *const Analyzer, callee: NodeIndex) ?ObjectProperty {
        if (self.ir_view.getTag(callee) != .member_access) return null;
        const member = self.ir_view.getMember(callee) orelse return null;
        const object_name = self.identifierName(member.object) orelse return null;
        const property_name = self.resolveAtomName(member.property) orelse return null;
        return .{ .object = object_name, .property = property_name };
    }

    fn propagate(self: *Analyzer) !void {
        var iter: u8 = 0;
        while (iter < max_iterations) : (iter += 1) {
            var changed = false;
            for (0..self.functions.items.len) |i| {
                const start = self.callee_starts.items[i];
                const count = self.calleeCount(i);
                var merged = self.functions.items[i].row;
                for (0..count) |k| {
                    const callee_idx = self.callee_storage.items[start + k];
                    merged = merged.merge(self.functions.items[callee_idx].row);
                }
                // Recursion flag on a callee should not necessarily propagate
                // to the caller; reset it back to the caller's own.
                merged.recursive = self.functions.items[i].row.recursive;
                if (!merged.eql(self.functions.items[i].row)) {
                    self.functions.items[i].row = merged;
                    changed = true;
                }
            }
            if (!changed) return;
        }
    }

    fn detectRecursion(self: *Analyzer) !void {
        const fn_count = self.functions.items.len;
        if (fn_count == 0) return;
        // Tarjan-style DFS with explicit on-stack marker. Any back edge to a
        // function currently on the stack proves a cycle; mark every function
        // in the active path that participates.
        const State = enum(u2) { unseen, on_stack, done };
        const states = try self.allocator.alloc(State, fn_count);
        defer self.allocator.free(states);
        @memset(states, .unseen);

        const FrameCursor = struct { fn_idx: usize, next_callee: u32 };
        const cursors = try self.allocator.alloc(FrameCursor, fn_count);
        defer self.allocator.free(cursors);

        for (0..fn_count) |start_idx| {
            if (states[start_idx] != .unseen) continue;
            states[start_idx] = .on_stack;
            cursors[0] = .{ .fn_idx = start_idx, .next_callee = 0 };
            var cursor_depth: usize = 1;

            while (cursor_depth > 0) {
                const top = &cursors[cursor_depth - 1];
                const top_idx = top.fn_idx;
                if (top.next_callee >= self.calleeCount(top_idx)) {
                    states[top_idx] = .done;
                    cursor_depth -= 1;
                    continue;
                }
                const start_off = self.callee_starts.items[top_idx];
                const callee_idx = self.callee_storage.items[start_off + top.next_callee];
                top.next_callee += 1;

                switch (states[callee_idx]) {
                    .unseen => {
                        states[callee_idx] = .on_stack;
                        cursors[cursor_depth] = .{ .fn_idx = callee_idx, .next_callee = 0 };
                        cursor_depth += 1;
                    },
                    .on_stack => {
                        // Back edge: mark every cursor frame from `callee_idx`
                        // up to the current top as part of the cycle.
                        var k: usize = 0;
                        while (k < cursor_depth and cursors[k].fn_idx != callee_idx) : (k += 1) {}
                        while (k < cursor_depth) : (k += 1) {
                            self.functions.items[cursors[k].fn_idx].row.recursive = true;
                        }
                    },
                    .done => {},
                }
            }
        }
    }

    fn isFunctionNode(self: *const Analyzer, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        return tag == .function_decl or tag == .function_expr or tag == .arrow_function;
    }

    fn identifierName(self: *const Analyzer, node: NodeIndex) ?[]const u8 {
        if (self.ir_view.getTag(node) != .identifier) return null;
        const binding = self.ir_view.getBinding(node) orelse return null;
        return self.resolveAtomName(binding.name_atom);
    }

    fn resolveAtomName(self: *const Analyzer, atom_value: u32) ?[]const u8 {
        const atom: object.Atom = @enumFromInt(atom_value);
        if (atom.isPredefined()) return atom.toPredefinedName();
        if (self.atoms) |table| return table.getName(atom);
        return null;
    }
};

fn isNonDeterministic(object_name: []const u8, property_name: []const u8) bool {
    if (std.mem.eql(u8, object_name, "Date") and std.mem.eql(u8, property_name, "now")) return true;
    if (std.mem.eql(u8, object_name, "Math") and std.mem.eql(u8, property_name, "random")) return true;
    return false;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;
const JsParser = @import("parser/root.zig").JsParser;
const module_manifest = @import("module_manifest.zig");

test "leaf pure function has empty effect row" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    var parser = JsParser.init(allocator, "function clean(s) { return s; }");
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const row = analyzer.lookup("clean") orelse return error.FunctionNotFound;
    try testing.expect(row.pure);
    try testing.expect(row.deterministic);
    try testing.expect(!row.recursive);
    try testing.expect(!row.has_egress);
}

test "Date.now marks the enclosing function non-deterministic" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    var parser = JsParser.init(allocator, "function nowSeconds() { return Date.now(); }");
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const row = analyzer.lookup("nowSeconds") orelse return error.FunctionNotFound;
    try testing.expect(!row.deterministic);
    try testing.expect(!row.pure);
}

test "Date.now inside durable step callback preserves determinism" {
    const allocator = std.testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    var parser = JsParser.init(allocator,
        \\import { step } from "zigttp:durable";
        \\function handler(req) { return step("ts", () => Date.now()); }
    );
    defer parser.deinit();
    parser.setAtomTable(&atoms);
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);
    const row = analyzer.lookup("handler") orelse return error.FunctionNotFound;
    try std.testing.expect(row.deterministic);
    try std.testing.expect(!row.pure);
}

test "Date.now as eager durable step argument is non-deterministic" {
    const allocator = std.testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    var parser = JsParser.init(allocator,
        \\import { step } from "zigttp:durable";
        \\function handler(req) { return step("ts", Date.now()); }
    );
    defer parser.deinit();
    parser.setAtomTable(&atoms);
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);
    const row = analyzer.lookup("handler") orelse return error.FunctionNotFound;
    try std.testing.expect(!row.deterministic);
    try std.testing.expect(!row.pure);
}

test "transitive non-determinism flows through callers" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    const source =
        \\function inner() { return Date.now(); }
        \\function outer() { return inner(); }
    ;
    var parser = JsParser.init(allocator, source);
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const outer = analyzer.lookup("outer") orelse return error.FunctionNotFound;
    try testing.expect(!outer.deterministic);
    try testing.expect(!outer.pure);
}

test "self-recursive function is flagged" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    var parser = JsParser.init(allocator, "function loop(n) { return loop(n); }");
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const row = analyzer.lookup("loop") orelse return error.FunctionNotFound;
    try testing.expect(row.recursive);
}

test "mutual recursion is flagged for both participants" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    const source =
        \\function a(n) { return b(n); }
        \\function b(n) { return a(n); }
    ;
    var parser = JsParser.init(allocator, source);
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const a_row = analyzer.lookup("a") orelse return error.FunctionNotFound;
    const b_row = analyzer.lookup("b") orelse return error.FunctionNotFound;
    try testing.expect(a_row.recursive);
    try testing.expect(b_row.recursive);
}

test "Math.random marks function non-deterministic" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    var parser = JsParser.init(allocator, "function pick() { return Math.random(); }");
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const row = analyzer.lookup("pick") orelse return error.FunctionNotFound;
    try testing.expect(!row.deterministic);
}

test "pure helper next to non-deterministic helper stays pure" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    const source =
        \\function clean(s) { return s; }
        \\function rnd() { return Math.random(); }
    ;
    var parser = JsParser.init(allocator, source);
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const clean = analyzer.lookup("clean") orelse return error.FunctionNotFound;
    const rnd = analyzer.lookup("rnd") orelse return error.FunctionNotFound;
    try testing.expect(clean.deterministic);
    try testing.expect(clean.pure);
    try testing.expect(!rnd.deterministic);
}

test "calling a write-classified import marks the function as writing" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    const source =
        \\import { cacheSet } from "zigttp:cache";
        \\function store(k) { return cacheSet("ns", k, "v"); }
        \\function clean(s) { return s; }
    ;
    var parser = JsParser.init(allocator, source);
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const store = analyzer.lookup("store") orelse return error.FunctionNotFound;
    const clean = analyzer.lookup("clean") orelse return error.FunctionNotFound;
    try testing.expect(store.writes);
    try testing.expect(!store.readOnly());
    try testing.expect(!clean.writes);
    try testing.expect(clean.readOnly());
}

test "write effect propagates transitively to callers" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    const source =
        \\import { cacheSet } from "zigttp:cache";
        \\function inner(k) { return cacheSet("ns", k, "v"); }
        \\function outer(k) { return inner(k); }
    ;
    var parser = JsParser.init(allocator, source);
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const outer = analyzer.lookup("outer") orelse return error.FunctionNotFound;
    try testing.expect(outer.writes);
}

test "partner write-classified import marks function and callers as writing" {
    const allocator = testing.allocator;

    const manifest_json =
        \\{
        \\  "schemaVersion": 1,
        \\  "specifier": "zigttp-ext:stripe",
        \\  "exports": [
        \\    { "name": "chargeCard", "effect": "write", "returns": "result" }
        \\  ]
        \\}
    ;
    var manifest = try module_manifest.parse(allocator, manifest_json);

    var registry = manifest_registry_mod.Registry.init(allocator);
    defer registry.deinit();
    registry.register(manifest) catch |err| {
        manifest.deinit(allocator);
        return err;
    };

    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    const source =
        \\import { chargeCard } from "zigttp-ext:stripe";
        \\function charge(tok) { return chargeCard(tok); }
        \\function wrapper(tok) { return charge(tok); }
        \\function clean(tok) { return tok; }
    ;
    var parser = JsParser.init(allocator, source);
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.initWithManifestRegistry(allocator, view, &atoms, &registry);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    const charge = analyzer.lookup("charge") orelse return error.FunctionNotFound;
    const wrapper = analyzer.lookup("wrapper") orelse return error.FunctionNotFound;
    const clean = analyzer.lookup("clean") orelse return error.FunctionNotFound;

    try testing.expect(charge.writes);
    try testing.expect(!charge.readOnly());
    try testing.expect(wrapper.writes);
    try testing.expect(!wrapper.readOnly());
    try testing.expect(!clean.writes);
    try testing.expect(clean.readOnly());
}

test "empty program is a no-op" {
    const allocator = testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    var parser = JsParser.init(allocator, "");
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = Analyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);
    try testing.expectEqual(@as(usize, 0), analyzer.all().len);
}
