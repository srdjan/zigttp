//! Default strict ZigTS profile.
//!
//! This pass enforces the smaller "expert" subset that keeps handler code
//! explicit enough for proof-driven tooling. It runs after parsing and the
//! normal type/sound passes, and before handler verification/contract work.

const std = @import("std");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const type_env_mod = @import("type_env.zig");
const type_checker_mod = @import("type_checker.zig");
const type_pool_mod = @import("type_pool.zig");
const bool_checker = @import("bool_checker.zig");
const repair_intent_mod = @import("repair_intent.zig");

pub const RepairIntent = repair_intent_mod.RepairIntent;

const NodeIndex = ir.NodeIndex;
const NodeTag = ir.NodeTag;
const IrView = ir.IrView;
const null_node = ir.null_node;
const TypeEnv = type_env_mod.TypeEnv;
const FunctionSig = type_env_mod.FunctionSig;
const TypeChecker = type_checker_mod.TypeChecker;
const null_type_idx = type_pool_mod.null_type_idx;

pub const Severity = enum {
    err,
    warning,

    pub fn label(self: Severity) []const u8 {
        return switch (self) {
            .err => "error",
            .warning => "warning",
        };
    }
};

pub const DiagnosticKind = enum {
    implicit_unknown,
    missing_public_annotation,
    dynamic_capability_access,
    non_exhaustive_profile_match,
    avoidable_let,
    computed_property_access,
    canonical_arrow_helper,
    canonical_export_function_const,
    canonical_public_helper_effects,
    canonical_public_helper_proof,
    canonical_ternary,
    canonical_compound_assignment,
    canonical_non_leading_spread,
    canonical_template_complex_interp,
    canonical_call_spread,
    canonical_default_parameter,
    canonical_destructure_depth,
    canonical_unused_index_alias,
    canonical_redundant_bool_compare,

    pub fn isCanonicalProfile(self: DiagnosticKind) bool {
        return switch (self) {
            .canonical_arrow_helper,
            .canonical_export_function_const,
            .canonical_public_helper_effects,
            .canonical_public_helper_proof,
            .canonical_ternary,
            .canonical_compound_assignment,
            .canonical_non_leading_spread,
            .canonical_template_complex_interp,
            .canonical_call_spread,
            .canonical_default_parameter,
            .canonical_destructure_depth,
            .canonical_unused_index_alias,
            .canonical_redundant_bool_compare,
            => true,
            else => false,
        };
    }
};

pub const Diagnostic = struct {
    severity: Severity,
    kind: DiagnosticKind,
    node: NodeIndex,
    message: []const u8,
    help: ?[]const u8,
    /// Slice B (expert-strategy §5): typed repair primitive the agent uses
    /// to pick an apply step directly. `null` when no single canonical
    /// repair applies (e.g. implicit_unknown — needs a type annotation that
    /// can take several shapes).
    repair_intent: ?RepairIntent = null,
};

const ImportedFunction = struct {
    slot: u16,
    module: []const u8,
    name: []const u8,
};

fn functionSigCovers(sig: ?FunctionSig, params_count: u16) bool {
    const s = sig orelse return false;
    return s.return_type != null_type_idx and s.param_count >= params_count;
}

pub const StrictChecker = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,
    type_env: ?*const TypeEnv,
    type_checker: ?*const TypeChecker,
    diagnostics: std.ArrayList(Diagnostic),
    assigned_bindings: std.AutoHashMapUnmanaged(u32, void),
    annotated_function_bindings: std.AutoHashMapUnmanaged(u32, void),
    static_literal_bindings: std.AutoHashMapUnmanaged(u32, void),
    call_counts: std.AutoHashMapUnmanaged(u32, u32),
    imported_functions: std.ArrayList(ImportedFunction),

    pub fn init(
        allocator: std.mem.Allocator,
        ir_view: IrView,
        atoms: ?*context.AtomTable,
        type_env: ?*const TypeEnv,
        type_checker: ?*const TypeChecker,
    ) StrictChecker {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .type_env = type_env,
            .type_checker = type_checker,
            .diagnostics = .empty,
            .assigned_bindings = .empty,
            .annotated_function_bindings = .empty,
            .static_literal_bindings = .empty,
            .call_counts = .empty,
            .imported_functions = .empty,
        };
    }

    pub fn deinit(self: *StrictChecker) void {
        self.imported_functions.deinit(self.allocator);
        self.call_counts.deinit(self.allocator);
        self.static_literal_bindings.deinit(self.allocator);
        self.annotated_function_bindings.deinit(self.allocator);
        self.assigned_bindings.deinit(self.allocator);
        self.diagnostics.deinit(self.allocator);
    }

    pub fn check(self: *StrictChecker, root: NodeIndex) !u32 {
        self.scanImports();
        self.collectAnnotatedFunctions(root);
        self.collectStaticLiterals(root);
        self.collectAssignments(root);
        self.collectCallCounts(root);
        self.walkStmt(root);

        var error_count: u32 = 0;
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .err) error_count += 1;
        }
        return error_count;
    }

    pub fn getDiagnostics(self: *const StrictChecker) []const Diagnostic {
        return self.diagnostics.items;
    }

    /// Drop borrowed references to TypeEnv/TypeChecker after `check()` has
    /// finished. Once sealed, the checker only holds owned diagnostics so it
    /// is safe to move into longer-lived storage without UB if the original
    /// borrows go out of scope. Idempotent.
    pub fn seal(self: *StrictChecker) void {
        self.type_env = null;
        self.type_checker = null;
    }

    pub fn formatDiagnostics(self: *const StrictChecker, source: []const u8, writer: anytype) !void {
        for (self.diagnostics.items) |diag| {
            const loc = self.ir_view.getLoc(diag.node) orelse continue;
            try writer.print("strict {s}: {s}\n", .{ diag.severity.label(), diag.message });
            try writer.print("  --> {d}:{d}\n", .{ loc.line, loc.column });
            if (getSourceLine(source, loc.line)) |line| {
                try writer.print("   |\n", .{});
                try writer.print("{d: >3} | {s}\n", .{ loc.line, line });
                try writer.print("   | ", .{});
                var col: u16 = 1;
                while (col < loc.column) : (col += 1) try writer.writeByte(' ');
                try writer.writeAll("^\n");
            }
            if (diag.help) |help| try writer.print("   = help: {s}\n", .{help});
            try writer.writeByte('\n');
        }
    }

    fn addDiagnostic(self: *StrictChecker, diag: Diagnostic) void {
        self.diagnostics.append(self.allocator, diag) catch {};
    }

    fn canonicalSeverity(self: *const StrictChecker) Severity {
        _ = self;
        return .err;
    }

    fn walkStmt(self: *StrictChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                for (0..block.stmts_count) |i| {
                    self.walkStmt(self.ir_view.getListIndex(block.stmts_start, @intCast(i)));
                }
            },
            .export_decl => {
                const export_decl = self.ir_view.getExportDecl(node) orelse return;
                if (export_decl.declaration != null_node) {
                    self.checkExportedDeclaration(export_decl.declaration);
                    self.walkStmt(export_decl.declaration);
                }
            },
            .function_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                self.checkFunctionAnnotation(decl.init);
                self.checkFunctionParams(decl.init);
                if (self.ir_view.getFunction(decl.init)) |func| {
                    self.walkStmt(func.body);
                }
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                if (decl.pattern != null_node) self.checkDestructurePattern(decl.pattern);
                if (decl.kind == .let and !self.assigned_bindings.contains(bindingKey(decl.binding))) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .avoidable_let,
                        .node = node,
                        .message = "let binding is never reassigned",
                        .help = "use const for bindings that do not change",
                        .repair_intent = .replace_let_with_const,
                    });
                }
                if (decl.kind == .@"const" and
                    self.ir_view.getTag(decl.init) == .arrow_function and
                    !self.isHandlerBinding(decl.binding) and
                    self.bindingCallCount(decl.binding) > 1)
                {
                    self.addDiagnostic(.{
                        .severity = self.canonicalSeverity(),
                        .kind = .canonical_arrow_helper,
                        .node = node,
                        .message = "reused arrow helper should be a named function",
                        .help = "rewrite reusable helpers as `function name(...) { ... }`; keep arrows for callbacks and local one-off values",
                        .repair_intent = .replace_arrow_with_function,
                    });
                }
                if (decl.init != null_node) {
                    if (self.isHandlerBinding(decl.binding) and self.isFunctionNode(decl.init)) {
                        self.checkFunctionAnnotation(decl.init);
                    }
                    self.walkExpr(decl.init);
                }
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node) orelse return;
                self.walkExpr(if_stmt.condition);
                self.walkStmt(if_stmt.then_branch);
                self.walkStmt(if_stmt.else_branch);
            },
            .for_of_stmt => {
                const for_iter = self.ir_view.getForIter(node) orelse return;
                if (!for_iter.is_const) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .avoidable_let,
                        .node = node,
                        .message = "for-of binding uses let",
                        .help = "use `for (const item of items)` unless the loop binding itself is reassigned",
                        .repair_intent = .replace_let_with_const,
                    });
                }
                self.checkUnusedIndexAlias(node, for_iter);
                self.walkExpr(for_iter.iterable);
                self.walkStmt(for_iter.body);
            },
            .return_stmt, .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |value| self.walkExpr(value);
            },
            else => self.walkExpr(node),
        }
    }

    fn walkExpr(self: *StrictChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return;
                self.checkRedundantBoolCompare(node, bin);
                self.walkExpr(bin.left);
                self.walkExpr(bin.right);
            },
            .unary_op, .spread => {
                const un = self.ir_view.getUnary(node) orelse return;
                self.walkExpr(un.operand);
            },
            .ternary => {
                const ternary = self.ir_view.getTernary(node) orelse return;
                self.addDiagnostic(.{
                    .severity = self.canonicalSeverity(),
                    .kind = .canonical_ternary,
                    .node = node,
                    .message = "ternary expression is not part of canonical ZigTS",
                    .help = "use an if/else statement, a match expression, or an immediately-invoked block",
                    .repair_intent = .replace_ternary_with_if,
                });
                self.walkExpr(ternary.condition);
                self.walkExpr(ternary.then_branch);
                self.walkExpr(ternary.else_branch);
            },
            .call, .method_call => {
                self.checkCall(node);
                const call = self.ir_view.getCall(node) orelse return;
                self.walkExpr(call.callee);
                for (0..call.args_count) |i| {
                    const arg = self.ir_view.getListIndex(call.args_start, @intCast(i));
                    const arg_tag = self.ir_view.getTag(arg);
                    if (arg_tag != null and arg_tag.? == .spread) {
                        self.addDiagnostic(.{
                            .severity = self.canonicalSeverity(),
                            .kind = .canonical_call_spread,
                            .node = arg,
                            .message = "spread arguments are not part of canonical ZigTS",
                            .help = "pass positional arguments or widen the helper signature to accept the array directly",
                            .repair_intent = .widen_signature_drop_spread,
                        });
                    }
                    self.walkExpr(arg);
                }
            },
            .member_access, .optional_chain => {
                const member = self.ir_view.getMember(node) orelse return;
                self.walkExpr(member.object);
            },
            .computed_access => {
                const member = self.ir_view.getMember(node) orelse return;
                self.walkExpr(member.object);
                if (member.computed != null_node) {
                    self.walkExpr(member.computed);
                    if (!self.isStaticComputedKey(member.computed)) {
                        self.addDiagnostic(.{
                            .severity = .err,
                            .kind = .computed_property_access,
                            .node = node,
                            .message = "dynamic computed property access is not part of strict ZigTS",
                            .help = "use a typed field, a literal key, or validate/narrow the object before indexing",
                        });
                    }
                }
            },
            .assignment => {
                const assign = self.ir_view.getAssignment(node) orelse return;
                if (assign.op != null) {
                    self.addDiagnostic(.{
                        .severity = self.canonicalSeverity(),
                        .kind = .canonical_compound_assignment,
                        .node = node,
                        .message = "compound assignment is not part of canonical ZigTS",
                        .help = "write the update explicitly: `x = x + 1` instead of `x += 1`",
                        .repair_intent = .replace_compound_assign_with_explicit,
                    });
                }
                self.walkExpr(assign.target);
                self.walkExpr(assign.value);
            },
            .array_literal => {
                const arr = self.ir_view.getArray(node) orelse return;
                for (0..arr.elements_count) |i| {
                    self.walkExpr(self.ir_view.getListIndex(arr.elements_start, @intCast(i)));
                }
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node) orelse return;
                for (0..obj.properties_count) |i| {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
                    const prop_tag = self.ir_view.getTag(prop_idx);
                    if (prop_tag != null and prop_tag.? == .object_spread) {
                        if (i > 0) {
                            self.addDiagnostic(.{
                                .severity = self.canonicalSeverity(),
                                .kind = .canonical_non_leading_spread,
                                .node = prop_idx,
                                .message = "object spread must appear before any explicit keys",
                                .help = "reorder so the spread is first: `{...base, x: 1}` instead of `{x: 1, ...base}`. Later keys still override.",
                                .repair_intent = .lead_with_spread,
                            });
                        }
                        if (self.ir_view.getOptValue(prop_idx)) |value| self.walkExpr(value);
                        continue;
                    }
                    const prop = self.ir_view.getProperty(prop_idx) orelse continue;
                    if (prop.is_computed and !self.isStaticComputedKey(prop.key)) {
                        self.addDiagnostic(.{
                            .severity = .err,
                            .kind = .computed_property_access,
                            .node = prop_idx,
                            .message = "dynamic computed object keys are not part of strict ZigTS",
                            .help = "use a literal field name so object shape stays compiler-visible",
                        });
                    }
                    self.walkExpr(prop.value);
                }
            },
            .template_literal => {
                const tmpl = self.ir_view.getTemplate(node) orelse return;
                for (0..tmpl.parts_count) |i| {
                    const part = self.ir_view.getListIndex(tmpl.parts_start, @intCast(i));
                    const value = self.ir_view.getOptValue(part) orelse continue;
                    const part_tag = self.ir_view.getTag(part);
                    const interp = part_tag != null and part_tag.? == .template_part_expr;
                    if (interp and !self.isSimpleTemplateInterp(value)) {
                        self.addDiagnostic(.{
                            .severity = self.canonicalSeverity(),
                            .kind = .canonical_template_complex_interp,
                            .node = part,
                            .message = "template interpolation must be an identifier or a literal-keyed property access",
                            .help = "hoist the expression into a `const` immediately above the template, then interpolate the new name",
                            .repair_intent = .name_const_above_template,
                        });
                    }
                    self.walkExpr(value);
                }
            },
            .match_expr => {
                const match = self.ir_view.getMatchExpr(node) orelse return;
                self.walkExpr(match.discriminant);
                if (!self.matchHasDefault(match)) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .non_exhaustive_profile_match,
                        .node = node,
                        .message = "match expression must be exhaustive in strict ZigTS",
                        .help = "cover every finite union member or add an explicit default when the type is not finite",
                        .repair_intent = .add_trailing_return,
                    });
                }
                for (0..match.arms_count) |i| {
                    const arm_idx = self.ir_view.getListIndex(match.arms_start, @intCast(i));
                    const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
                    self.walkExpr(arm.body);
                }
            },
            .function_expr, .arrow_function => {
                self.checkFunctionAnnotation(node);
                self.checkFunctionParams(node);
                if (self.ir_view.getFunction(node)) |func| self.walkStmt(func.body);
            },
            .jsx_element => {
                const jsx = self.ir_view.getJsxElement(node) orelse return;
                for (0..jsx.props_count) |i| {
                    const attr_idx = self.ir_view.getListIndex(jsx.props_start, @intCast(i));
                    if (self.ir_view.getJsxAttr(attr_idx)) |attr| self.walkExpr(attr.value);
                }
                for (0..jsx.children_count) |i| {
                    self.walkExpr(self.ir_view.getListIndex(jsx.children_start, @intCast(i)));
                }
            },
            else => {},
        }
    }

    fn checkExportedDeclaration(self: *StrictChecker, node: NodeIndex) void {
        const tag = self.ir_view.getTag(node) orelse return;
        if (tag == .function_decl) {
            const decl = self.ir_view.getVarDecl(node) orelse return;
            self.checkFunctionAnnotation(decl.init);
        } else if (tag == .var_decl) {
            const decl = self.ir_view.getVarDecl(node) orelse return;
            if (decl.init != null_node and self.isFunctionNode(decl.init)) {
                if (decl.kind == .@"const") {
                    self.addDiagnostic(.{
                        .severity = self.canonicalSeverity(),
                        .kind = .canonical_export_function_const,
                        .node = node,
                        .message = "exported function-valued const should be an export function declaration",
                        .help = "use `export function name(...) { ... }` unless the export is intentionally a first-class function value",
                        .repair_intent = .replace_export_arrow_with_function,
                    });
                }
                self.checkFunctionAnnotation(decl.init);
            }
        }
    }

    fn checkFunctionAnnotation(self: *StrictChecker, node: NodeIndex) void {
        const func = self.ir_view.getFunction(node) orelse return;
        if (!self.functionNeedsAnnotation(node, func)) return;

        const loc = self.ir_view.getLoc(node) orelse return;
        if (!self.hasCompleteFunctionAnnotation(func, loc.line)) {
            self.addDiagnostic(.{
                .severity = .err,
                .kind = .missing_public_annotation,
                .node = node,
                .message = "strict ZigTS requires explicit function parameter and return types",
                .help = "annotate each parameter and the return type, for example `function handler(req: Request): Response`",
            });
        }
    }

    /// Walk a destructuring pattern and flag nested binding patterns.
    /// `const {a: {b}} = obj` inflates review cost and tends to drift in
    /// agent output; canonical ZigTS keeps each destructure one level
    /// deep with intermediate `const` bindings for further drilling.
    fn checkDestructurePattern(self: *StrictChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .object_pattern, .array_pattern => {
                const arr = self.ir_view.getArray(node) orelse return;
                for (0..arr.elements_count) |i| {
                    const child = self.ir_view.getListIndex(arr.elements_start, @intCast(i));
                    self.checkDestructurePattern(child);
                }
            },
            .pattern_element => {
                // `.pattern_rest` is excluded: rest elements never carry a
                // nested pattern (`elem.key` is always null_node for them),
                // so they cannot introduce destructure depth.
                const elem = self.ir_view.getPatternElem(node) orelse return;
                if (elem.key != null_node) {
                    self.addDiagnostic(.{
                        .severity = self.canonicalSeverity(),
                        .kind = .canonical_destructure_depth,
                        .node = node,
                        .message = "destructuring patterns must be at most one level deep",
                        .help = "destructure one level, then drill into the value with a follow-up `const`: `const {a} = obj; const {b} = a;`",
                        .repair_intent = .flatten_destructure,
                    });
                    self.checkDestructurePattern(elem.key);
                }
            },
            else => {},
        }
    }

    /// Walk a function's parameter list and flag canonical violations:
    /// default values at the signature site (ZTS617). Param walking is
    /// separate from `checkFunctionAnnotation` because parameter shape
    /// matters even when the function carries no type annotation - the
    /// canonical rules apply unconditionally.
    fn checkFunctionParams(self: *StrictChecker, node: NodeIndex) void {
        const func = self.ir_view.getFunction(node) orelse return;
        for (0..func.params_count) |i| {
            const param_idx = self.ir_view.getListIndex(func.params_start, @intCast(i));
            const elem = self.ir_view.getPatternElem(param_idx) orelse continue;
            if (elem.default_value != null_node) {
                self.addDiagnostic(.{
                    .severity = self.canonicalSeverity(),
                    .kind = .canonical_default_parameter,
                    .node = param_idx,
                    .message = "default parameter values are not part of canonical ZigTS",
                    .help = "accept `(a: T | undefined)` and resolve the default in the body: `const resolved = a === undefined ? DEFAULT : a;`",
                    .repair_intent = .lift_default_to_body,
                });
            }
        }
    }

    /// ZTS619 canonical_unused_index_alias: detect for-of loops whose iterable
    /// is `<expr>.entries()` and whose body destructures the loop binding into
    /// `[<index>, <value>]` with `<index>` never read. The `.entries()` call
    /// and the destructure exist solely to introduce a name for the index;
    /// when that name is dead, iterating directly over `<expr>` collapses the
    /// loop body to a single binding and eliminates the alias-tracking branch
    /// the iterator-scope-confinement analysis would otherwise carry.
    fn checkUnusedIndexAlias(self: *StrictChecker, node: NodeIndex, for_iter: ir.Node.ForIterStmt) void {
        if (!self.isEntriesCall(for_iter.iterable)) return;
        if (for_iter.body == null_node) return;
        const body_tag = self.ir_view.getTag(for_iter.body) orelse return;

        // First statement of the loop body must be the destructuring binding.
        const first_stmt = if (body_tag == .block) blk: {
            const block = self.ir_view.getBlock(for_iter.body) orelse return;
            if (block.stmts_count == 0) return;
            break :blk self.ir_view.getListIndex(block.stmts_start, 0);
        } else for_iter.body;
        if (self.ir_view.getTag(first_stmt) != .var_decl) return;
        const decl = self.ir_view.getVarDecl(first_stmt) orelse return;
        if (decl.pattern == null_node) return;
        if (decl.init == null_node) return;

        // The destructure must source the loop binding directly: `const [...] = pair`
        // where `pair` is the for-of binding. Anything else is not the alias shape
        // ZTS619 targets.
        if (self.ir_view.getTag(decl.init) != .identifier) return;
        const init_binding = self.ir_view.getBinding(decl.init) orelse return;
        if (init_binding.scope_id != for_iter.binding.scope_id or
            init_binding.slot != for_iter.binding.slot) return;

        // Pattern must be a two-element array pattern: [index, value].
        if (self.ir_view.getTag(decl.pattern) != .array_pattern) return;
        const arr = self.ir_view.getArray(decl.pattern) orelse return;
        if (arr.elements_count != 2) return;

        const index_elem_idx = self.ir_view.getListIndex(arr.elements_start, 0);
        const index_elem = self.ir_view.getPatternElem(index_elem_idx) orelse return;
        if (index_elem.kind != .simple) return;

        // Conservative read check: if the index name appears anywhere in the
        // loop body as an identifier reference (other than its own binding
        // site), assume it's read and bail. This walks the body once.
        if (self.bindingReadInBody(for_iter.body, index_elem.binding)) return;

        self.addDiagnostic(.{
            .severity = self.canonicalSeverity(),
            .kind = .canonical_unused_index_alias,
            .node = node,
            .message = "for-of binds an index alias that is never read",
            .help = "drop `.entries()` and the destructure; iterate over the array directly",
            .repair_intent = .drop_unused_index_alias,
        });
    }

    /// ZTS620 canonical_redundant_bool_compare: detect a strict comparison of
    /// a statically-boolean value against a boolean literal (`x === true`,
    /// `x !== false`, and the operand-reversed forms). For a boolean `x` the
    /// comparison is exactly `x` or `!x`, so the literal comparison is a
    /// redundant spelling of the boolean test itself.
    ///
    /// The static-boolean guard is load-bearing for soundness: `x === true`
    /// is only equivalent to `x` when `x` is a boolean. For a non-boolean
    /// `x`, `x === true` is an identity test against the literal `true`,
    /// which differs from the truthiness test `x`. When type information is
    /// unavailable, or the value operand is not provably boolean, the rule
    /// does not fire.
    fn checkRedundantBoolCompare(self: *StrictChecker, node: NodeIndex, bin: ir.Node.BinaryExpr) void {
        if (bin.op != .strict_eq and bin.op != .strict_neq) return;

        const left_bool = self.boolLiteralValue(bin.left);
        const right_bool = self.boolLiteralValue(bin.right);

        // Exactly one operand must be a boolean literal. `true === false` is a
        // constant the optimizer handles; it is not the redundant-test shape.
        const lit_value: bool, const value_node: NodeIndex = blk: {
            if (left_bool != null and right_bool == null) break :blk .{ left_bool.?, bin.right };
            if (right_bool != null and left_bool == null) break :blk .{ right_bool.?, bin.left };
            return;
        };

        // Soundness guard: the non-literal operand must be statically boolean.
        const tc = self.type_checker orelse return;
        if (tc.inferType(value_node) != tc.env.pool.idx_boolean) return;

        // `=== true` / `!== false` reduce to `x`; `=== false` / `!== true`
        // reduce to `!x`.
        const positive = (bin.op == .strict_eq) == lit_value;
        const help = if (positive)
            "drop the comparison and use the boolean directly: `x`"
        else
            "drop the comparison and negate the boolean directly: `!x`";

        self.addDiagnostic(.{
            .severity = self.canonicalSeverity(),
            .kind = .canonical_redundant_bool_compare,
            .node = node,
            .message = "comparing a boolean against a boolean literal is not canonical ZigTS",
            .help = help,
            .repair_intent = .drop_redundant_bool_compare,
        });
    }

    /// The value of a boolean literal node, or null when `node` is not a
    /// `lit_bool`. `IrView.getBoolValue` does not tag-check, so the tag gate
    /// here is required to avoid reading a non-boolean node's payload as a
    /// bool.
    fn boolLiteralValue(self: *const StrictChecker, node: NodeIndex) ?bool {
        if (self.ir_view.getTag(node) != .lit_bool) return null;
        return self.ir_view.getBoolValue(node);
    }

    /// True when `node` is `<expr>.entries()` with no arguments.
    fn isEntriesCall(self: *const StrictChecker, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        if (tag != .call and tag != .method_call) return false;
        const call = self.ir_view.getCall(node) orelse return false;
        if (call.args_count != 0) return false;
        const callee_tag = self.ir_view.getTag(call.callee) orelse return false;
        if (callee_tag != .member_access) return false;
        const member = self.ir_view.getMember(call.callee) orelse return false;
        const method_name = self.resolveAtomName(member.property) orelse return false;
        return std.mem.eql(u8, method_name, "entries");
    }

    /// Walks `body` and returns true if `target` appears as an identifier
    /// reference. Bindings introduced inside the body (the var_decl that
    /// names `target` itself) do not count as reads; only identifier nodes
    /// that resolve to the binding do.
    fn bindingReadInBody(self: *const StrictChecker, body: NodeIndex, target: ir.BindingRef) bool {
        return self.scanBindingRead(body, target);
    }

    fn scanBindingRead(self: *const StrictChecker, node: NodeIndex, target: ir.BindingRef) bool {
        if (node == null_node) return false;
        const tag = self.ir_view.getTag(node) orelse return false;
        switch (tag) {
            .identifier => {
                const binding = self.ir_view.getBinding(node) orelse return false;
                return binding.scope_id == target.scope_id and binding.slot == target.slot;
            },
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return false;
                for (0..block.stmts_count) |i| {
                    if (self.scanBindingRead(self.ir_view.getListIndex(block.stmts_start, @intCast(i)), target)) return true;
                }
                return false;
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return false;
                // Skip the destructure pattern itself - the index name shows up
                // there as a binding site, not a read. Walk only the initializer.
                return self.scanBindingRead(decl.init, target);
            },
            .function_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return false;
                return self.scanBindingRead(decl.init, target);
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node) orelse return false;
                return self.scanBindingRead(if_stmt.condition, target) or
                    self.scanBindingRead(if_stmt.then_branch, target) or
                    self.scanBindingRead(if_stmt.else_branch, target);
            },
            .for_of_stmt => {
                const inner = self.ir_view.getForIter(node) orelse return false;
                return self.scanBindingRead(inner.iterable, target) or
                    self.scanBindingRead(inner.body, target);
            },
            .return_stmt, .expr_stmt => {
                const value = self.ir_view.getOptValue(node) orelse return false;
                return self.scanBindingRead(value, target);
            },
            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return false;
                return self.scanBindingRead(bin.left, target) or self.scanBindingRead(bin.right, target);
            },
            .unary_op, .spread => {
                const un = self.ir_view.getUnary(node) orelse return false;
                return self.scanBindingRead(un.operand, target);
            },
            .ternary => {
                const tern = self.ir_view.getTernary(node) orelse return false;
                return self.scanBindingRead(tern.condition, target) or
                    self.scanBindingRead(tern.then_branch, target) or
                    self.scanBindingRead(tern.else_branch, target);
            },
            .assignment => {
                const assign = self.ir_view.getAssignment(node) orelse return false;
                return self.scanBindingRead(assign.target, target) or
                    self.scanBindingRead(assign.value, target);
            },
            .call, .method_call => {
                const call = self.ir_view.getCall(node) orelse return false;
                if (self.scanBindingRead(call.callee, target)) return true;
                for (0..call.args_count) |i| {
                    if (self.scanBindingRead(self.ir_view.getListIndex(call.args_start, @intCast(i)), target)) return true;
                }
                return false;
            },
            .member_access, .optional_chain, .computed_access => {
                const member = self.ir_view.getMember(node) orelse return false;
                if (self.scanBindingRead(member.object, target)) return true;
                return self.scanBindingRead(member.computed, target);
            },
            .array_literal => {
                const arr = self.ir_view.getArray(node) orelse return false;
                for (0..arr.elements_count) |i| {
                    if (self.scanBindingRead(self.ir_view.getListIndex(arr.elements_start, @intCast(i)), target)) return true;
                }
                return false;
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node) orelse return false;
                for (0..obj.properties_count) |i| {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
                    const prop = self.ir_view.getProperty(prop_idx) orelse continue;
                    if (self.scanBindingRead(prop.value, target)) return true;
                }
                return false;
            },
            .template_literal => {
                const tmpl = self.ir_view.getTemplate(node) orelse return false;
                for (0..tmpl.parts_count) |i| {
                    const part = self.ir_view.getListIndex(tmpl.parts_start, @intCast(i));
                    if (self.ir_view.getOptValue(part)) |value| {
                        if (self.scanBindingRead(value, target)) return true;
                    }
                }
                return false;
            },
            .match_expr => {
                const match = self.ir_view.getMatchExpr(node) orelse return false;
                if (self.scanBindingRead(match.discriminant, target)) return true;
                for (0..match.arms_count) |i| {
                    const arm_idx = self.ir_view.getListIndex(match.arms_start, @intCast(i));
                    const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
                    if (self.scanBindingRead(arm.body, target)) return true;
                }
                return false;
            },
            else => return false,
        }
    }

    fn functionNeedsAnnotation(self: *StrictChecker, node: NodeIndex, func: ir.Node.FunctionExpr) bool {
        if (func.name_atom != 0) return true;

        // Anonymous callbacks passed to proof-relevant helpers must also be
        // annotated. v1 detects those at the call site via implicit_unknown.
        const loc = self.ir_view.getLoc(node) orelse return false;
        _ = loc;
        return false;
    }

    fn hasCompleteFunctionAnnotation(self: *const StrictChecker, func: ir.Node.FunctionExpr, line: u32) bool {
        const env = self.type_env orelse return false;
        if (functionSigCovers(env.getFnSigByLoc(line), func.params_count)) return true;
        if (func.name_atom == 0) return false;
        const name = self.resolveAtomName(func.name_atom) orelse return false;
        return functionSigCovers(env.getFnSigByName(name), func.params_count);
    }

    fn checkCall(self: *StrictChecker, node: NodeIndex) void {
        const call = self.ir_view.getCall(node) orelse return;
        if (self.importedFunctionForCallee(call.callee)) |imported| {
            if (literalRequiredArg(imported.module, imported.name)) |arg_pos| {
                if (arg_pos < call.args_count) {
                    const arg = self.ir_view.getListIndex(call.args_start, @intCast(arg_pos));
                    if (!self.isLiteralOrStaticTemplate(arg)) {
                        self.addDiagnostic(.{
                            .severity = .err,
                            .kind = .dynamic_capability_access,
                            .node = arg,
                            .message = "capability access must use a compiler-visible literal",
                            .help = "use a literal env key, cache namespace, SQL query name, egress URL, route path, or service name",
                        });
                    }
                }
            }
        }

        if (self.type_checker) |tc| {
            const inferred = tc.inferType(node);
            if (inferred == null_type_idx or inferred == tc.env.pool.idx_unknown) {
                if (self.isUserFunctionOrUnknownCall(call.callee)) {
                    self.addDiagnostic(.{
                        .severity = .err,
                        .kind = .implicit_unknown,
                        .node = node,
                        .message = "call result has implicit unknown type",
                        .help = "add a return type annotation, use a modeled virtual module, or narrow with a type guard/assert",
                    });
                }
            }
        }
    }

    fn isUserFunctionOrUnknownCall(self: *StrictChecker, callee: NodeIndex) bool {
        const tag = self.ir_view.getTag(callee) orelse return false;
        if (tag != .identifier) return false;
        const binding = self.ir_view.getBinding(callee) orelse return false;
        if (self.annotated_function_bindings.contains(bindingKey(binding))) return false;
        if (self.importedFunctionForSlot(binding.slot) != null) return false;
        const name = self.resolveAtomName(binding.slot) orelse return false;
        if (isKnownGlobalFunction(name)) {
            return false;
        }
        return true;
    }

    fn scanImports(self: *StrictChecker) void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx| {
            const node: NodeIndex = @intCast(idx);
            if (self.ir_view.getTag(node) != .import_decl) continue;
            const import_decl = self.ir_view.getImportDecl(node) orelse continue;
            const module = self.ir_view.getString(import_decl.module_idx) orelse continue;
            for (0..import_decl.specifiers_count) |i| {
                const spec_idx = self.ir_view.getListIndex(import_decl.specifiers_start, @intCast(i));
                const spec = self.ir_view.getImportSpec(spec_idx) orelse continue;
                const name = self.resolveAtomName(spec.imported_atom) orelse continue;
                self.imported_functions.append(self.allocator, .{
                    .slot = spec.local_binding.slot,
                    .module = module,
                    .name = name,
                }) catch {};
            }
        }
    }

    fn collectAnnotatedFunctions(self: *StrictChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                for (0..block.stmts_count) |i| {
                    self.collectAnnotatedFunctions(self.ir_view.getListIndex(block.stmts_start, @intCast(i)));
                }
            },
            .export_decl => {
                const export_decl = self.ir_view.getExportDecl(node) orelse return;
                self.collectAnnotatedFunctions(export_decl.declaration);
            },
            .function_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                if (self.functionHasAnnotation(decl.init)) {
                    self.annotated_function_bindings.put(self.allocator, bindingKey(decl.binding), {}) catch {};
                }
                if (self.ir_view.getFunction(decl.init)) |func| {
                    self.collectAnnotatedFunctions(func.body);
                }
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                if (decl.init != null_node and self.isFunctionNode(decl.init) and self.functionHasAnnotation(decl.init)) {
                    self.annotated_function_bindings.put(self.allocator, bindingKey(decl.binding), {}) catch {};
                }
                self.collectAnnotatedFunctions(decl.init);
            },
            .function_expr, .arrow_function => {
                if (self.ir_view.getFunction(node)) |func| self.collectAnnotatedFunctions(func.body);
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node) orelse return;
                self.collectAnnotatedFunctions(if_stmt.then_branch);
                self.collectAnnotatedFunctions(if_stmt.else_branch);
            },
            .for_of_stmt => {
                const for_iter = self.ir_view.getForIter(node) orelse return;
                self.collectAnnotatedFunctions(for_iter.body);
            },
            else => {},
        }
    }

    fn functionHasAnnotation(self: *const StrictChecker, node: NodeIndex) bool {
        const func = self.ir_view.getFunction(node) orelse return false;
        const loc = self.ir_view.getLoc(node) orelse return false;
        const sig = if (self.type_env) |env| env.getFnSigByLoc(loc.line) else null;
        return if (sig) |s|
            s.return_type != null_type_idx and s.param_count >= func.params_count
        else
            false;
    }

    fn collectAssignments(self: *StrictChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                for (0..block.stmts_count) |i| {
                    self.collectAssignments(self.ir_view.getListIndex(block.stmts_start, @intCast(i)));
                }
            },
            .assignment => {
                const assign = self.ir_view.getAssignment(node) orelse return;
                if (self.ir_view.getTag(assign.target) == .identifier) {
                    if (self.ir_view.getBinding(assign.target)) |binding| {
                        self.assigned_bindings.put(self.allocator, bindingKey(binding), {}) catch {};
                    }
                }
                self.collectAssignments(assign.value);
            },
            .function_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                self.collectAssignments(decl.init);
            },
            .function_expr, .arrow_function => {
                if (self.ir_view.getFunction(node)) |func| self.collectAssignments(func.body);
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                self.collectAssignments(decl.init);
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node) orelse return;
                self.collectAssignments(if_stmt.condition);
                self.collectAssignments(if_stmt.then_branch);
                self.collectAssignments(if_stmt.else_branch);
            },
            .for_of_stmt => {
                const for_iter = self.ir_view.getForIter(node) orelse return;
                self.collectAssignments(for_iter.iterable);
                self.collectAssignments(for_iter.body);
            },
            .return_stmt, .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |value| self.collectAssignments(value);
            },
            else => self.collectExprAssignments(node),
        }
    }

    fn collectStaticLiterals(self: *StrictChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                for (0..block.stmts_count) |i| {
                    self.collectStaticLiterals(self.ir_view.getListIndex(block.stmts_start, @intCast(i)));
                }
            },
            .export_decl => {
                const export_decl = self.ir_view.getExportDecl(node) orelse return;
                self.collectStaticLiterals(export_decl.declaration);
            },
            .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                if (decl.kind == .@"const" and self.isLiteralOrStaticTemplate(decl.init)) {
                    self.static_literal_bindings.put(self.allocator, bindingKey(decl.binding), {}) catch {};
                }
                self.collectStaticLiterals(decl.init);
            },
            .function_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                self.collectStaticLiterals(decl.init);
            },
            .function_expr, .arrow_function => {
                if (self.ir_view.getFunction(node)) |func| self.collectStaticLiterals(func.body);
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node) orelse return;
                self.collectStaticLiterals(if_stmt.then_branch);
                self.collectStaticLiterals(if_stmt.else_branch);
            },
            .for_of_stmt => {
                const for_iter = self.ir_view.getForIter(node) orelse return;
                self.collectStaticLiterals(for_iter.body);
            },
            else => {},
        }
    }

    fn collectExprAssignments(self: *StrictChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return;
                self.collectAssignments(bin.left);
                self.collectAssignments(bin.right);
            },
            .unary_op, .spread => {
                const un = self.ir_view.getUnary(node) orelse return;
                self.collectAssignments(un.operand);
            },
            .call, .method_call => {
                const call = self.ir_view.getCall(node) orelse return;
                self.collectAssignments(call.callee);
                for (0..call.args_count) |i| {
                    self.collectAssignments(self.ir_view.getListIndex(call.args_start, @intCast(i)));
                }
            },
            .member_access, .optional_chain, .computed_access => {
                const member = self.ir_view.getMember(node) orelse return;
                self.collectAssignments(member.object);
                self.collectAssignments(member.computed);
            },
            else => {},
        }
    }

    fn collectCallCounts(self: *StrictChecker, node: NodeIndex) void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;
        switch (tag) {
            .program, .block => {
                const block = self.ir_view.getBlock(node) orelse return;
                for (0..block.stmts_count) |i| {
                    self.collectCallCounts(self.ir_view.getListIndex(block.stmts_start, @intCast(i)));
                }
            },
            .export_decl => {
                const export_decl = self.ir_view.getExportDecl(node) orelse return;
                self.collectCallCounts(export_decl.declaration);
            },
            .function_decl, .var_decl => {
                const decl = self.ir_view.getVarDecl(node) orelse return;
                self.collectCallCounts(decl.init);
            },
            .function_expr, .arrow_function => {
                if (self.ir_view.getFunction(node)) |func| self.collectCallCounts(func.body);
            },
            .if_stmt => {
                const if_stmt = self.ir_view.getIfStmt(node) orelse return;
                self.collectCallCounts(if_stmt.condition);
                self.collectCallCounts(if_stmt.then_branch);
                self.collectCallCounts(if_stmt.else_branch);
            },
            .for_of_stmt => {
                const for_iter = self.ir_view.getForIter(node) orelse return;
                self.collectCallCounts(for_iter.iterable);
                self.collectCallCounts(for_iter.body);
            },
            .return_stmt, .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |value| self.collectCallCounts(value);
            },
            .binary_op => {
                const bin = self.ir_view.getBinary(node) orelse return;
                self.collectCallCounts(bin.left);
                self.collectCallCounts(bin.right);
            },
            .unary_op, .spread => {
                const un = self.ir_view.getUnary(node) orelse return;
                self.collectCallCounts(un.operand);
            },
            .ternary => {
                const ternary = self.ir_view.getTernary(node) orelse return;
                self.collectCallCounts(ternary.condition);
                self.collectCallCounts(ternary.then_branch);
                self.collectCallCounts(ternary.else_branch);
            },
            .call, .method_call => {
                const call = self.ir_view.getCall(node) orelse return;
                self.recordCall(call.callee);
                self.collectCallCounts(call.callee);
                for (0..call.args_count) |i| {
                    self.collectCallCounts(self.ir_view.getListIndex(call.args_start, @intCast(i)));
                }
            },
            .member_access, .optional_chain, .computed_access => {
                const member = self.ir_view.getMember(node) orelse return;
                self.collectCallCounts(member.object);
                self.collectCallCounts(member.computed);
            },
            .array_literal => {
                const arr = self.ir_view.getArray(node) orelse return;
                for (0..arr.elements_count) |i| {
                    self.collectCallCounts(self.ir_view.getListIndex(arr.elements_start, @intCast(i)));
                }
            },
            .object_literal => {
                const obj = self.ir_view.getObject(node) orelse return;
                for (0..obj.properties_count) |i| {
                    const prop_idx = self.ir_view.getListIndex(obj.properties_start, @intCast(i));
                    const prop = self.ir_view.getProperty(prop_idx) orelse continue;
                    self.collectCallCounts(prop.key);
                    self.collectCallCounts(prop.value);
                }
            },
            else => {},
        }
    }

    fn recordCall(self: *StrictChecker, callee: NodeIndex) void {
        if (self.ir_view.getTag(callee) != .identifier) return;
        const binding = self.ir_view.getBinding(callee) orelse return;
        const key = bindingKey(binding);
        const gop = self.call_counts.getOrPut(self.allocator, key) catch return;
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }

    fn bindingCallCount(self: *const StrictChecker, binding: ir.BindingRef) u32 {
        return self.call_counts.get(bindingKey(binding)) orelse 0;
    }

    fn importedFunctionForCallee(self: *const StrictChecker, callee: NodeIndex) ?ImportedFunction {
        if (self.ir_view.getTag(callee) != .identifier) return null;
        const binding = self.ir_view.getBinding(callee) orelse return null;
        return self.importedFunctionForSlot(binding.slot);
    }

    fn importedFunctionForSlot(self: *const StrictChecker, slot: u16) ?ImportedFunction {
        for (self.imported_functions.items) |func| {
            if (func.slot == slot) return func;
        }
        return null;
    }

    fn isHandlerBinding(self: *const StrictChecker, binding: ir.BindingRef) bool {
        const name = self.resolveAtomName(binding.slot) orelse return false;
        return std.mem.eql(u8, name, "handler");
    }

    fn isFunctionNode(self: *const StrictChecker, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        return tag == .function_decl or tag == .function_expr or tag == .arrow_function;
    }

    fn matchHasDefault(self: *const StrictChecker, match: ir.Node.MatchExpr) bool {
        for (0..match.arms_count) |i| {
            const arm_idx = self.ir_view.getListIndex(match.arms_start, @intCast(i));
            const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
            if (arm.pattern == null_node) return true;
        }
        return false;
    }

    fn isStaticComputedKey(self: *const StrictChecker, node: NodeIndex) bool {
        return switch (self.ir_view.getTag(node) orelse return false) {
            .lit_string, .lit_int => true,
            .identifier => blk: {
                const binding = self.ir_view.getBinding(node) orelse break :blk false;
                break :blk self.static_literal_bindings.contains(bindingKey(binding));
            },
            else => false,
        };
    }

    /// Canonical template interpolations are restricted to identifiers and
    /// chains of literal-keyed member access (`user.profile.name`). Anything
    /// else - function calls, arithmetic, ternaries, computed access - must
    /// be hoisted into a `const` above the template.
    fn isSimpleTemplateInterp(self: *const StrictChecker, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        return switch (tag) {
            .identifier => true,
            .member_access => blk: {
                const member = self.ir_view.getMember(node) orelse break :blk false;
                if (member.computed != null_node) break :blk false;
                break :blk self.isSimpleTemplateInterp(member.object);
            },
            else => false,
        };
    }

    fn isLiteralOrStaticTemplate(self: *const StrictChecker, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        if (tag == .lit_string) return true;
        if (tag == .lit_int) return true;
        if (tag == .identifier) {
            const binding = self.ir_view.getBinding(node) orelse return false;
            return self.static_literal_bindings.contains(bindingKey(binding));
        }
        if (tag != .template_literal) return false;
        const tmpl = self.ir_view.getTemplate(node) orelse return false;
        for (0..tmpl.parts_count) |i| {
            const part = self.ir_view.getListIndex(tmpl.parts_start, @intCast(i));
            if (self.ir_view.getTag(part) == .template_part_expr) return false;
        }
        return true;
    }

    fn resolveAtomName(self: *const StrictChecker, atom_value: u32) ?[]const u8 {
        const atom: object.Atom = @enumFromInt(atom_value);
        if (atom.isPredefined()) return atom.toPredefinedName();
        if (self.atoms) |table| return table.getName(atom);
        return null;
    }
};

fn literalRequiredArg(module: []const u8, name: []const u8) ?u8 {
    if (std.mem.eql(u8, module, "zigttp:env") and std.mem.eql(u8, name, "env")) return 0;
    if (std.mem.eql(u8, module, "zigttp:fetch") and std.mem.eql(u8, name, "fetchSync")) return 0;
    if (std.mem.eql(u8, module, "zigttp:service") and std.mem.eql(u8, name, "serviceCall")) return 0;
    if (std.mem.eql(u8, module, "zigttp:sql")) return 0;
    if (std.mem.eql(u8, module, "zigttp:cache")) return 0;
    return null;
}

fn isKnownGlobalFunction(name: []const u8) bool {
    const names = [_][]const u8{
        "Array",
        "Boolean",
        "Date",
        "Headers",
        "JSON",
        "Math",
        "Number",
        "Object",
        "Request",
        "Response",
        "String",
        "assert",
        "h",
        "parallel",
        "parseFloat",
        "parseInt",
        "race",
        "range",
        "renderToString",
    };
    for (names) |candidate| {
        if (std.mem.eql(u8, name, candidate)) return true;
    }
    return false;
}

fn bindingKey(binding: ir.BindingRef) u32 {
    return bool_checker.packBindingKey(binding.scope_id, binding.slot);
}

fn getSourceLine(source: []const u8, line_num: u32) ?[]const u8 {
    if (line_num == 0) return null;
    var current_line: u32 = 1;
    var start: usize = 0;
    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (current_line == line_num) return source[start..i];
            current_line += 1;
            start = i + 1;
        }
    }
    if (current_line == line_num) return source[start..];
    return null;
}

const testing = std.testing;

test "strict checker flags avoidable let" {
    const source = "function handler(req) { let x = 1; return Response.json({x}); }";
    var parser = @import("parser/root.zig").JsParser.init(testing.allocator, source);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var checker = StrictChecker.init(testing.allocator, view, null, null, null);
    defer checker.deinit();
    const errors = try checker.check(root);
    try testing.expect(errors > 0);
}

test "strict checker accepts reassigned let" {
    const source = "function handler(req) { let x = 1; x = 2; return Response.json({x}); }";
    var parser = @import("parser/root.zig").JsParser.init(testing.allocator, source);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var checker = StrictChecker.init(testing.allocator, view, null, null, null);
    defer checker.deinit();
    _ = try checker.check(root);
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .avoidable_let);
    }
}

test "canonical profile warns on reused arrow helper" {
    const source = "const parse = (x) => x; function handler(req) { const a = parse(1); const b = parse(2); return Response.json({a,b}); }";
    var parser = @import("parser/root.zig").JsParser.init(testing.allocator, source);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var checker = StrictChecker.init(testing.allocator, view, null, null, null);
    defer checker.deinit();
    const errors = try checker.check(root);
    try testing.expect(errors > 0);
    var saw = false;
    for (checker.getDiagnostics()) |diag| {
        if (diag.kind == .canonical_arrow_helper) {
            saw = true;
            try testing.expectEqual(Severity.err, diag.severity);
        }
    }
    try testing.expect(saw);
}

test "strict checker accepts one-off arrow helper value" {
    const source = "const parse = (x) => x; function handler(req) { const a = parse(1); return Response.json({a}); }";
    var parser = @import("parser/root.zig").JsParser.init(testing.allocator, source);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var checker = StrictChecker.init(testing.allocator, view, null, null, null);
    defer checker.deinit();
    const errors = try checker.check(root);
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_arrow_helper);
    }
    // Other strict diagnostics may fire because this untyped fixture uses an
    // unannotated function; the canonical arrow-helper rule should not.
    _ = errors;
}

fn expectKind(checker: *const StrictChecker, kind: DiagnosticKind) !void {
    for (checker.getDiagnostics()) |diag| {
        if (diag.kind == kind) return;
    }
    return error.DiagnosticNotEmitted;
}

fn checkSource(source: []const u8) !StrictChecker {
    var parser = @import("parser/root.zig").JsParser.init(testing.allocator, source);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var checker = StrictChecker.init(testing.allocator, view, null, null, null);
    errdefer checker.deinit();
    _ = try checker.check(root);
    return checker;
}

/// Typed harness for rules that need inferred types (e.g. ZTS620's boolean
/// guard). Heap-allocated so the env/checker that capture `&pool`/`&env`
/// keep stable addresses across the returned handle. Caller calls `deinit`.
const TypedHarness = struct {
    parser: @import("parser/root.zig").JsParser,
    pool: type_pool_mod.TypePool,
    env: TypeEnv,
    tc: TypeChecker,
    checker: StrictChecker,

    fn deinit(self: *TypedHarness) void {
        self.checker.deinit();
        self.tc.deinit();
        self.env.deinit();
        self.pool.deinit(testing.allocator);
        self.parser.deinit();
        testing.allocator.destroy(self);
    }
};

fn checkSourceTyped(source: []const u8) !*TypedHarness {
    const h = try testing.allocator.create(TypedHarness);
    errdefer testing.allocator.destroy(h);
    h.parser = @import("parser/root.zig").JsParser.init(testing.allocator, source);
    const root = try h.parser.parse();
    const view = IrView.fromIRStore(&h.parser.nodes, &h.parser.constants);
    h.pool = type_pool_mod.TypePool.init(testing.allocator);
    h.env = TypeEnv.init(testing.allocator, &h.pool);
    h.tc = TypeChecker.init(testing.allocator, view, null, &h.env, null);
    _ = try h.tc.check(root);
    h.checker = StrictChecker.init(testing.allocator, view, null, &h.env, &h.tc);
    _ = try h.checker.check(root);
    return h;
}

test "canonical_redundant_bool_compare fires on `=== true` for a boolean" {
    var h = try checkSourceTyped(
        "function handler(req) { const ready = req.method === \"GET\"; if (ready === true) { return Response.text(\"a\"); } return Response.text(\"b\"); }",
    );
    defer h.deinit();
    try expectKind(&h.checker, .canonical_redundant_bool_compare);
}

test "canonical_redundant_bool_compare fires on `!== false` for a boolean" {
    var h = try checkSourceTyped(
        "function handler(req) { const ready = req.method === \"GET\"; if (ready !== false) { return Response.text(\"a\"); } return Response.text(\"b\"); }",
    );
    defer h.deinit();
    try expectKind(&h.checker, .canonical_redundant_bool_compare);
}

test "canonical_redundant_bool_compare does not fire on a non-boolean comparison" {
    // `n === 1` compares a number against a number literal, not a boolean
    // against a boolean literal: leave it alone.
    var h = try checkSourceTyped(
        "function handler(req) { const n = 1; if (n === 1) { return Response.text(\"a\"); } return Response.text(\"b\"); }",
    );
    defer h.deinit();
    for (h.checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_redundant_bool_compare);
    }
}

test "canonical_redundant_bool_compare does not fire without type info" {
    // The null-type-checker harness cannot prove the operand is boolean, so
    // the soundness guard suppresses the rule rather than risk a non-boolean
    // identity comparison.
    var checker = try checkSource(
        "function handler(req) { const ready = req.method === \"GET\"; if (ready === true) { return Response.text(\"a\"); } return Response.text(\"b\"); }",
    );
    defer checker.deinit();
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_redundant_bool_compare);
    }
}

test "canonical_ternary fires on a ternary expression" {
    var checker = try checkSource("function handler(req) { const x = req.method === 'GET' ? 200 : 500; return Response.json({x}); }");
    defer checker.deinit();
    try expectKind(&checker, .canonical_ternary);
}

test "canonical_compound_assignment fires on +=" {
    var checker = try checkSource("function handler(req) { let n = 0; n += 1; return Response.json({n}); }");
    defer checker.deinit();
    try expectKind(&checker, .canonical_compound_assignment);
}

test "canonical_compound_assignment does not fire on plain =" {
    var checker = try checkSource("function handler(req) { let n = 0; n = n + 1; return Response.json({n}); }");
    defer checker.deinit();
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_compound_assignment);
    }
}

test "canonical_non_leading_spread fires when spread follows explicit keys" {
    var checker = try checkSource("function handler(req) { const base = {a: 1}; const next = {b: 2, ...base}; return Response.json(next); }");
    defer checker.deinit();
    try expectKind(&checker, .canonical_non_leading_spread);
}

test "canonical_non_leading_spread accepts leading spread" {
    var checker = try checkSource("function handler(req) { const base = {a: 1}; const next = {...base, b: 2}; return Response.json(next); }");
    defer checker.deinit();
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_non_leading_spread);
    }
}

test "canonical_template_complex_interp fires on a call inside interpolation" {
    var checker = try checkSource("function getName() { return 'x'; } function handler(req) { return Response.text(`hi ${getName()}`); }");
    defer checker.deinit();
    try expectKind(&checker, .canonical_template_complex_interp);
}

test "canonical_template_complex_interp accepts identifier and member access" {
    var checker = try checkSource("function handler(req) { const user = {name: 'a'}; return Response.text(`hi ${user.name}`); }");
    defer checker.deinit();
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_template_complex_interp);
    }
}

test "canonical_call_spread accepts positional args" {
    var checker = try checkSource("function send(a, b) { return a + b; } function handler(req) { return Response.json({n: send(1, 2)}); }");
    defer checker.deinit();
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_call_spread);
    }
}

test "canonical_default_parameter fires on a signature default" {
    var checker = try checkSource("function greet(name = 'world') { return name; } function handler(req) { return Response.text(greet()); }");
    defer checker.deinit();
    try expectKind(&checker, .canonical_default_parameter);
}

test "canonical_default_parameter accepts explicit undefined-resolved defaults" {
    var checker = try checkSource("function greet(name) { const resolved = name === undefined ? 'world' : name; return resolved; } function handler(req) { return Response.text(greet(undefined)); }");
    defer checker.deinit();
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_default_parameter);
    }
}

test "canonical_destructure_depth fires on nested object pattern" {
    var checker = try checkSource("function handler(req) { const payload = {user: {name: 'a'}}; const {user: {name}} = payload; return Response.text(name); }");
    defer checker.deinit();
    try expectKind(&checker, .canonical_destructure_depth);
}

test "canonical_destructure_depth accepts flat destructure" {
    var checker = try checkSource("function handler(req) { const payload = {user: 'a'}; const {user} = payload; return Response.text(user); }");
    defer checker.deinit();
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_destructure_depth);
    }
}

test "canonical_ternary diagnostic carries repair_intent = replace_ternary_with_if" {
    // Slice B (expert-strategy §5): every veto-able strict diagnostic must
    // populate the typed repair primitive so the agent picks an apply step
    // directly. ZTS612 is the representative canonical-profile case.
    var checker = try checkSource("function handler(req) { const x = req.method === 'GET' ? 200 : 500; return Response.json({x}); }");
    defer checker.deinit();
    var saw_ternary = false;
    for (checker.getDiagnostics()) |diag| {
        if (diag.kind == .canonical_ternary) {
            saw_ternary = true;
            try testing.expectEqual(
                @as(?RepairIntent, .replace_ternary_with_if),
                diag.repair_intent,
            );
        }
    }
    try testing.expect(saw_ternary);
}

// NOTE: a positive test for `f(...args)` is intentionally absent. The parser
// constructs the spread Node with `.data.unary` shape while the IR reader at
// parser/ir.zig:1107 expects `.data.opt_value`; the resulting safety panic
// prevents the detector from ever running. The detector here is in place so
// the rule fires automatically once the parser/IR mismatch is fixed.

test "canonical_unused_index_alias fires when index is never read" {
    var checker = try checkSource(
        \\function handler(req) {
        \\  const arr = [10, 20];
        \\  for (const pair of arr.entries()) {
        \\    const [_i, x] = pair;
        \\    const _used = x;
        \\  }
        \\  return Response.json({ok: true});
        \\}
    );
    defer checker.deinit();
    try expectKind(&checker, .canonical_unused_index_alias);
}

test "canonical_unused_index_alias does not fire when index is read" {
    var checker = try checkSource(
        \\function handler(req) {
        \\  const arr = [10, 20];
        \\  for (const pair of arr.entries()) {
        \\    const [i, x] = pair;
        \\    const _seen = i + x;
        \\  }
        \\  return Response.json({ok: true});
        \\}
    );
    defer checker.deinit();
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_unused_index_alias);
    }
}

test "canonical_unused_index_alias does not fire on plain for-of" {
    var checker = try checkSource(
        \\function handler(req) {
        \\  const arr = [10, 20];
        \\  for (const x of arr) {
        \\    const _used = x;
        \\  }
        \\  return Response.json({ok: true});
        \\}
    );
    defer checker.deinit();
    for (checker.getDiagnostics()) |diag| {
        try testing.expect(diag.kind != .canonical_unused_index_alias);
    }
}

test "canonical_unused_index_alias diagnostic carries repair_intent" {
    var checker = try checkSource(
        \\function handler(req) {
        \\  const arr = [10, 20];
        \\  for (const pair of arr.entries()) {
        \\    const [_i, x] = pair;
        \\    const _used = x;
        \\  }
        \\  return Response.json({ok: true});
        \\}
    );
    defer checker.deinit();
    var saw = false;
    for (checker.getDiagnostics()) |diag| {
        if (diag.kind == .canonical_unused_index_alias) {
            saw = true;
            try testing.expectEqual(
                @as(?RepairIntent, .drop_unused_index_alias),
                diag.repair_intent,
            );
        }
    }
    try testing.expect(saw);
}
