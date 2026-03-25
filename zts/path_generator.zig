//! Compile-Time Exhaustive Test Generation
//!
//! Enumerates every execution path through a handler function and generates
//! a declarative test case (JSONL) for each one. Paths are finite because
//! zigttp's JS subset has no back-edges (while/do-while banned) and no
//! exceptions (try/catch banned). The IR tree IS the control flow graph.
//!
//! Each test specifies: the request that triggers the path, virtual module
//! stubs with concrete return values derived from branch conditions, and
//! the expected response status from the return statement.
//!
//! Output format matches the existing declarative test runner (test_runner.zig)
//! so generated tests are immediately runnable via --test.

const std = @import("std");
const ir = @import("parser/ir.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const builtin_modules = @import("builtin_modules.zig");
const mb = @import("module_binding.zig");
const bool_checker_mod = @import("bool_checker.zig");

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const IrView = ir.IrView;
const null_node = ir.null_node;
const packBindingKey = bool_checker_mod.packBindingKey;

// ---------------------------------------------------------------------------
// Path constraints
// ---------------------------------------------------------------------------

const Constraint = union(enum) {
    req_method: []const u8,
    req_url: []const u8,
    req_url_prefix: []const u8,
    /// Virtual module call should return a truthy value.
    stub_truthy: StubInfo,
    /// Virtual module call should return a falsy value (null/undefined).
    stub_falsy: StubInfo,
    /// Result.ok should be true.
    result_ok: StubInfo,
    /// Result.ok should be false.
    result_not_ok: StubInfo,
};

const StubInfo = struct {
    module: []const u8,
    func: []const u8,
    returns: mb.ReturnKind,
};

// ---------------------------------------------------------------------------
// Generated test
// ---------------------------------------------------------------------------

const IoStub = struct {
    seq: u32,
    module: []const u8,
    func: []const u8,
    result_json: []const u8,
};

const GeneratedTest = struct {
    name: []const u8,
    method: []const u8,
    url: []const u8,
    has_auth_header: bool,
    expected_status: u16,
    io_stubs: std.ArrayList(IoStub),
};

// ---------------------------------------------------------------------------
// PathGenerator
// ---------------------------------------------------------------------------

pub const PathGenerator = struct {
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: ?*context.AtomTable,

    /// Binding tracking: local slot -> module function metadata.
    module_fn_bindings: std.AutoHashMapUnmanaged(u16, FnMeta),
    /// Variable init tracking: packed(scope, slot) -> NodeIndex of init expression.
    var_inits: std.AutoHashMapUnmanaged(u32, NodeIndex),
    /// Request parameter binding key.
    req_binding_key: ?u32,
    /// Slot bound to req.method.
    method_binding_key: ?u32,
    /// Slot bound to req.url or req.path.
    url_binding_key: ?u32,

    /// Accumulated constraints for current path.
    constraints: std.ArrayList(Constraint),
    /// Accumulated I/O call sequence for current path.
    io_seq: std.ArrayList(IoCall),
    /// Completed test cases.
    tests: std.ArrayList(GeneratedTest),
    /// Path counter for naming.
    path_count: u32,

    const FnMeta = struct {
        module: []const u8,
        func: []const u8,
        returns: mb.ReturnKind,
    };

    const IoCall = struct {
        module: []const u8,
        func: []const u8,
        returns: mb.ReturnKind,
        binding_key: ?u32,
    };

    pub fn init(allocator: std.mem.Allocator, ir_view: IrView, atoms: ?*context.AtomTable) PathGenerator {
        return .{
            .allocator = allocator,
            .ir_view = ir_view,
            .atoms = atoms,
            .module_fn_bindings = .empty,
            .var_inits = .empty,
            .req_binding_key = null,
            .method_binding_key = null,
            .url_binding_key = null,
            .constraints = .empty,
            .io_seq = .empty,
            .tests = .empty,
            .path_count = 0,
        };
    }

    pub fn deinit(self: *PathGenerator) void {
        self.module_fn_bindings.deinit(self.allocator);
        self.var_inits.deinit(self.allocator);
        self.constraints.deinit(self.allocator);
        self.io_seq.deinit(self.allocator);
        for (self.tests.items) |*t| t.io_stubs.deinit(self.allocator);
        self.tests.deinit(self.allocator);
    }

    /// Generate test cases for the given handler function.
    pub fn generate(self: *PathGenerator, handler_func: NodeIndex) !void {
        self.scanImports();
        self.findHandlerBindings(handler_func);

        const func = self.ir_view.getFunction(handler_func) orelse return;
        try self.walkPaths(func.body);
    }

    pub fn getTests(self: *const PathGenerator) []const GeneratedTest {
        return self.tests.items;
    }

    /// Write all generated tests as JSONL to writer.
    pub fn writeJsonl(self: *const PathGenerator, writer: anytype) !void {
        for (self.tests.items) |test_case| {
            // Test header
            try writer.writeAll("{\"type\":\"test\",\"name\":\"");
            try writeJsonEscaped(writer, test_case.name);
            try writer.writeAll("\"}\n");

            // Request
            try writer.writeAll("{\"type\":\"request\",\"method\":\"");
            try writer.writeAll(test_case.method);
            try writer.writeAll("\",\"url\":\"");
            try writeJsonEscaped(writer, test_case.url);
            if (test_case.has_auth_header) {
                try writer.writeAll("\",\"headers\":{\"authorization\":\"Bearer test-token\"},\"body\":null}\n");
            } else {
                try writer.writeAll("\",\"headers\":{},\"body\":null}\n");
            }

            // I/O stubs
            for (test_case.io_stubs.items) |stub| {
                try writer.print("{{\"type\":\"io\",\"seq\":{d},\"module\":\"{s}\",\"fn\":\"{s}\",\"result\":{s}}}\n", .{
                    stub.seq,
                    stub.module,
                    stub.func,
                    stub.result_json,
                });
            }

            // Expected response
            try writer.print("{{\"type\":\"expect\",\"status\":{d}}}\n", .{test_case.expected_status});
        }
    }

    // -------------------------------------------------------------------
    // Phase 1: Scan imports and handler bindings
    // -------------------------------------------------------------------

    fn scanImports(self: *PathGenerator) void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .import_decl) continue;

            const import_decl = self.ir_view.getImportDecl(idx) orelse continue;
            const module_str = self.ir_view.getString(import_decl.module_idx) orelse continue;
            if (!std.mem.startsWith(u8, module_str, "zigttp:")) continue;

            // Extract short module name after "zigttp:"
            const mod_name = module_str[7..];

            var j: u8 = 0;
            while (j < import_decl.specifiers_count) : (j += 1) {
                const spec_idx = self.ir_view.getListIndex(import_decl.specifiers_start, j);
                const spec = self.ir_view.getImportSpec(spec_idx) orelse continue;
                const imported_name = self.resolveAtomName(spec.imported_atom) orelse continue;

                if (builtin_modules.findFunction(imported_name)) |entry| {
                    self.module_fn_bindings.put(self.allocator, spec.local_binding.slot, .{
                        .module = mod_name,
                        .func = entry.func.name,
                        .returns = entry.func.returns,
                    }) catch {};
                }
            }
        }
    }

    fn findHandlerBindings(self: *PathGenerator, handler_func: NodeIndex) void {
        const func = self.ir_view.getFunction(handler_func) orelse return;
        if (func.params_count == 0) return;

        // First parameter is the request
        const param_idx = self.ir_view.getListIndex(func.params_start, 0);
        const param_tag = self.ir_view.getTag(param_idx) orelse return;
        if (param_tag == .identifier) {
            const binding = self.ir_view.getBinding(param_idx) orelse return;
            self.req_binding_key = packBindingKey(binding.scope_id, binding.slot);
        }

        // Scan body for `const method = req.method` and `const url = req.url` patterns
        const body = self.ir_view.getBlock(func.body) orelse return;
        var i: u16 = 0;
        while (i < body.stmts_count) : (i += 1) {
            const stmt_idx = self.ir_view.getListIndex(body.stmts_start, i);
            const stmt_tag = self.ir_view.getTag(stmt_idx) orelse continue;
            if (stmt_tag != .var_decl) continue;

            const vd = self.ir_view.getVarDecl(stmt_idx) orelse continue;
            if (vd.init == null_node) continue;

            const key = packBindingKey(vd.binding.scope_id, vd.binding.slot);
            self.var_inits.put(self.allocator, key, vd.init) catch {};

            // Check if init is req.method or req.url/req.path
            if (self.isReqMemberAccess(vd.init, "method")) {
                self.method_binding_key = key;
            } else if (self.isReqMemberAccess(vd.init, "url") or self.isReqMemberAccess(vd.init, "path")) {
                self.url_binding_key = key;
            }
        }
    }

    // -------------------------------------------------------------------
    // Phase 2: Path enumeration via recursive IR walk
    // -------------------------------------------------------------------

    fn walkPaths(self: *PathGenerator, node: NodeIndex) error{OutOfMemory}!void {
        if (node == null_node) return;
        const tag = self.ir_view.getTag(node) orelse return;

        switch (tag) {
            .return_stmt => {
                // Leaf: complete path found. Emit test case.
                const status = self.extractReturnStatus(node);
                try self.emitTestCase(status);
            },

            .block, .program => {
                const block = self.ir_view.getBlock(node) orelse return;
                try self.walkBlock(block);
            },

            .if_stmt => {
                const if_s = self.ir_view.getIfStmt(node) orelse return;

                const saved_constraints = self.constraints.items.len;
                const saved_io = self.io_seq.items.len;

                // Push all extractable constraints for then-branch
                self.extractAllConstraints(if_s.condition, false);
                try self.walkPaths(if_s.then_branch);
                self.constraints.shrinkRetainingCapacity(saved_constraints);
                self.io_seq.shrinkRetainingCapacity(saved_io);

                // Else-branch: push negated constraints
                if (if_s.else_branch != null_node) {
                    self.extractAllConstraints(if_s.condition, true);
                    try self.walkPaths(if_s.else_branch);
                    self.constraints.shrinkRetainingCapacity(saved_constraints);
                    self.io_seq.shrinkRetainingCapacity(saved_io);
                }
                // If no else and then-branch doesn't always return, fall-through
                // is handled by the caller walking the next statement in the block.
            },

            .match_expr => {
                const match_data = self.ir_view.getMatchExpr(node) orelse return;
                var i: u8 = 0;
                while (i < match_data.arms_count) : (i += 1) {
                    const arm_idx = self.ir_view.getListIndex(match_data.arms_start, i);
                    const arm = self.ir_view.getMatchArm(arm_idx) orelse continue;
                    const saved = self.constraints.items.len;
                    const saved_io = self.io_seq.items.len;
                    try self.walkPaths(arm.body);
                    self.constraints.shrinkRetainingCapacity(saved);
                    self.io_seq.shrinkRetainingCapacity(saved_io);
                }
            },

            .var_decl => {
                const vd = self.ir_view.getVarDecl(node) orelse return;
                if (vd.init != null_node) {
                    const key = packBindingKey(vd.binding.scope_id, vd.binding.slot);
                    self.var_inits.put(self.allocator, key, vd.init) catch {};
                    // Track module function calls for I/O stub generation
                    self.trackModuleCall(vd.init, key);
                }
            },

            .expr_stmt => {
                // Track standalone module calls (side effects)
                if (self.ir_view.getOptValue(node)) |expr| {
                    self.trackModuleCall(expr, null);
                }
            },

            .for_of_stmt => {
                const fi = self.ir_view.getForIter(node) orelse return;
                // Walk body path (non-empty collection case)
                try self.walkPaths(fi.body);
            },

            .switch_stmt => {
                const sw = self.ir_view.getSwitchStmt(node) orelse return;
                for (0..sw.cases_count) |i| {
                    const case_idx = self.ir_view.getListIndex(sw.cases_start, @intCast(i));
                    const cc = self.ir_view.getCaseClause(case_idx) orelse continue;
                    const saved = self.constraints.items.len;
                    const saved_io = self.io_seq.items.len;
                    const case_block = Node.BlockData{ .stmts_start = cc.body_start, .stmts_count = cc.body_count, .scope_id = 0 };
                    try self.walkBlock(case_block);
                    self.constraints.shrinkRetainingCapacity(saved);
                    self.io_seq.shrinkRetainingCapacity(saved_io);
                }
            },

            .function_decl, .function_expr, .arrow_function => {
                // Don't recurse into nested function definitions
            },

            .export_default => {
                if (self.ir_view.getOptValue(node)) |val| {
                    try self.walkPaths(val);
                }
            },

            else => {},
        }
    }

    fn walkBlock(self: *PathGenerator, block: Node.BlockData) !void {
        var i: u16 = 0;
        while (i < block.stmts_count) : (i += 1) {
            const stmt_idx = self.ir_view.getListIndex(block.stmts_start, i);
            const stmt_tag = self.ir_view.getTag(stmt_idx) orelse continue;

            if (stmt_tag == .if_stmt) {
                const if_s = self.ir_view.getIfStmt(stmt_idx) orelse continue;
                const then_always = self.stmtAlwaysReturns(if_s.then_branch);

                if (then_always and if_s.else_branch != null_node) {
                    // Both branches return: fork and stop
                    try self.walkPaths(stmt_idx);
                    return;
                } else if (then_always and if_s.else_branch == null_node) {
                    // Then-branch returns, no else: fork then-path, continue for fall-through
                    const saved = self.constraints.items.len;
                    const saved_io = self.io_seq.items.len;
                    self.extractAllConstraints(if_s.condition, false);
                    try self.walkPaths(if_s.then_branch);
                    self.constraints.shrinkRetainingCapacity(saved);
                    self.io_seq.shrinkRetainingCapacity(saved_io);

                    // Fall-through with negated constraints
                    self.extractAllConstraints(if_s.condition, true);
                    continue;
                } else {
                    // Neither always returns: just recurse normally
                    try self.walkPaths(stmt_idx);
                    continue;
                }
            }

            try self.walkPaths(stmt_idx);

            // If this statement always returns, remaining statements are unreachable
            if (stmt_tag == .return_stmt) return;
        }
    }

    // -------------------------------------------------------------------
    // Phase 3: Constraint extraction from conditions
    // -------------------------------------------------------------------

    /// Push all extractable constraints from a condition into the constraint list.
    /// When `negate` is true, each constraint is negated (for else/fall-through paths).
    fn extractAllConstraints(self: *PathGenerator, cond: NodeIndex, negate: bool) void {
        const tag = self.ir_view.getTag(cond) orelse return;

        // AND chains: extract from both sides
        if (tag == .binary_op) {
            const bin = self.ir_view.getBinary(cond) orelse return;
            if (bin.op == .and_op) {
                self.extractAllConstraints(bin.left, negate);
                self.extractAllConstraints(bin.right, negate);
                return;
            }
        }

        // Extract single constraint from this node
        if (self.extractConditionConstraint(cond)) |c| {
            const final = if (negate) self.negateConstraint(c) else c;
            if (final) |f| self.constraints.append(self.allocator, f) catch {};
        }
    }

    fn extractConditionConstraint(self: *PathGenerator, cond: NodeIndex) ?Constraint {
        const tag = self.ir_view.getTag(cond) orelse return null;

        switch (tag) {
            .binary_op => {
                const bin = self.ir_view.getBinary(cond) orelse return null;

                if (bin.op == .strict_eq or bin.op == .strict_neq) {
                    // Check pattern: identifier === "literal"
                    if (self.extractLiteralComparison(bin.left, bin.right)) |c| {
                        return if (bin.op == .strict_eq) c else self.negateConstraint(c);
                    }
                    // Reverse: "literal" === identifier
                    if (self.extractLiteralComparison(bin.right, bin.left)) |c| {
                        return if (bin.op == .strict_eq) c else self.negateConstraint(c);
                    }
                }

                // && chains handled by extractAllConstraints
                if (bin.op == .and_op) return null;
            },

            .unary_op => {
                // !expr -> negate
                const unary = self.ir_view.getUnary(cond) orelse return null;
                if (unary.op == .not) {
                    if (self.extractConditionConstraint(unary.operand)) |c| {
                        return self.negateConstraint(c);
                    }
                }
            },

            .identifier => {
                // Truthiness check: if (val) where val is a module return
                return self.extractTruthinessConstraint(cond);
            },

            .member_access => {
                // if (result.ok) pattern
                return self.extractResultOkConstraint(cond);
            },

            .call => {
                // if (moduleFunc(...)) - truthiness of direct call
                return self.extractCallTruthinessConstraint(cond);
            },

            else => {},
        }

        return null;
    }

    fn extractLiteralComparison(self: *PathGenerator, id_node: NodeIndex, lit_node: NodeIndex) ?Constraint {
        const id_tag = self.ir_view.getTag(id_node) orelse return null;
        if (id_tag != .identifier) return null;

        const lit_tag = self.ir_view.getTag(lit_node) orelse return null;
        if (lit_tag != .lit_string) return null;

        const str_idx = self.ir_view.getStringIdx(lit_node) orelse return null;
        const value = self.ir_view.getString(str_idx) orelse return null;

        const binding = self.ir_view.getBinding(id_node) orelse return null;
        const key = packBindingKey(binding.scope_id, binding.slot);

        // Direct req.method / req.url comparisons
        if (self.req_binding_key) |req_key| {
            if (key == req_key) return null; // comparing request itself, not a property
        }

        if (self.method_binding_key) |mk| {
            if (key == mk) return .{ .req_method = value };
        }

        if (self.url_binding_key) |uk| {
            if (key == uk) return .{ .req_url = value };
        }

        // Check if this binding was initialized from req.method or req.url
        if (self.var_inits.get(key)) |init_node| {
            if (self.isReqMemberAccess(init_node, "method")) return .{ .req_method = value };
            if (self.isReqMemberAccess(init_node, "url") or self.isReqMemberAccess(init_node, "path")) return .{ .req_url = value };
        }

        return null;
    }

    fn extractTruthinessConstraint(self: *PathGenerator, id_node: NodeIndex) ?Constraint {
        const binding = self.ir_view.getBinding(id_node) orelse return null;
        const key = packBindingKey(binding.scope_id, binding.slot);

        // Check if this binding was initialized from a module call
        const init_node = self.var_inits.get(key) orelse return null;
        const init_tag = self.ir_view.getTag(init_node) orelse return null;

        if (init_tag == .call) {
            const call = self.ir_view.getCall(init_node) orelse return null;
            if (self.getCalleeMeta(call.callee)) |meta| {
                return .{ .stub_truthy = .{ .module = meta.module, .func = meta.func, .returns = meta.returns } };
            }
        }

        // Check through nullish coalescing: const x = env("Y") ?? "default"
        if (init_tag == .binary_op) {
            const bin = self.ir_view.getBinary(init_node) orelse return null;
            if (bin.op == .nullish) {
                const lhs_tag = self.ir_view.getTag(bin.left) orelse return null;
                if (lhs_tag == .call) {
                    const call = self.ir_view.getCall(bin.left) orelse return null;
                    if (self.getCalleeMeta(call.callee)) |meta| {
                        return .{ .stub_truthy = .{ .module = meta.module, .func = meta.func, .returns = meta.returns } };
                    }
                }
            }
        }

        return null;
    }

    fn extractCallTruthinessConstraint(self: *PathGenerator, call_node: NodeIndex) ?Constraint {
        const call = self.ir_view.getCall(call_node) orelse return null;
        if (self.getCalleeMeta(call.callee)) |meta| {
            return .{ .stub_truthy = .{ .module = meta.module, .func = meta.func, .returns = meta.returns } };
        }
        return null;
    }

    fn extractResultOkConstraint(self: *PathGenerator, member_node: NodeIndex) ?Constraint {
        const member = self.ir_view.getMember(member_node) orelse return null;
        const prop_name = self.resolveAtomName(member.property) orelse return null;
        if (!std.mem.eql(u8, prop_name, "ok")) return null;

        // Object should be an identifier bound to a result-producing call
        const obj_tag = self.ir_view.getTag(member.object) orelse return null;
        if (obj_tag != .identifier) return null;

        const binding = self.ir_view.getBinding(member.object) orelse return null;
        const key = packBindingKey(binding.scope_id, binding.slot);
        const init_node = self.var_inits.get(key) orelse return null;
        const init_tag = self.ir_view.getTag(init_node) orelse return null;

        if (init_tag == .call) {
            const call = self.ir_view.getCall(init_node) orelse return null;
            if (self.getCalleeMeta(call.callee)) |meta| {
                if (meta.returns == .result) {
                    return .{ .result_ok = .{ .module = meta.module, .func = meta.func, .returns = meta.returns } };
                }
            }
        }

        return null;
    }

    fn negateConstraint(_: *const PathGenerator, c: Constraint) ?Constraint {
        return switch (c) {
            .req_method => null, // can't negate "not GET" into a useful constraint
            .req_url => null,
            .req_url_prefix => null,
            .stub_truthy => |info| .{ .stub_falsy = info },
            .stub_falsy => |info| .{ .stub_truthy = info },
            .result_ok => |info| .{ .result_not_ok = info },
            .result_not_ok => |info| .{ .result_ok = info },
        };
    }

    // -------------------------------------------------------------------
    // I/O call tracking
    // -------------------------------------------------------------------

    fn trackModuleCall(self: *PathGenerator, node: NodeIndex, binding_key: ?u32) void {
        const tag = self.ir_view.getTag(node) orelse return;

        if (tag == .call) {
            const call = self.ir_view.getCall(node) orelse return;
            if (self.getCalleeMeta(call.callee)) |meta| {
                self.io_seq.append(self.allocator, .{
                    .module = meta.module,
                    .func = meta.func,
                    .returns = meta.returns,
                    .binding_key = binding_key,
                }) catch {};
            }
            return;
        }

        // Look through nullish coalescing: env("X") ?? "default"
        if (tag == .binary_op) {
            const bin = self.ir_view.getBinary(node) orelse return;
            if (bin.op == .nullish) {
                self.trackModuleCall(bin.left, binding_key);
                return;
            }
        }
    }

    // -------------------------------------------------------------------
    // Response status extraction
    // -------------------------------------------------------------------

    fn extractReturnStatus(self: *PathGenerator, node: NodeIndex) u16 {
        const ret_val = self.ir_view.getOptValue(node) orelse return 200;
        const ret_tag = self.ir_view.getTag(ret_val) orelse return 200;

        if (ret_tag == .call or ret_tag == .method_call) {
            const call = self.ir_view.getCall(ret_val) orelse return 200;

            // Check for Response.json/text/html/redirect
            if (!self.isResponseHelper(call.callee)) return 200;

            if (call.args_count >= 2) {
                const opts = self.ir_view.getListIndex(call.args_start, 1);
                if (self.extractStatusFromOptions(opts)) |status| return status;
            }
        }

        return 200;
    }

    fn extractStatusFromOptions(self: *const PathGenerator, opts_node: NodeIndex) ?u16 {
        const tag = self.ir_view.getTag(opts_node) orelse return null;
        if (tag != .object_literal) return null;

        const obj = self.ir_view.getObject(opts_node) orelse return null;
        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            const prop_tag = self.ir_view.getTag(prop_idx) orelse continue;
            if (prop_tag != .object_property) continue;

            const prop = self.ir_view.getProperty(prop_idx) orelse continue;

            // Check if key is "status"
            const key_name = self.getPropertyKeyName(prop.key) orelse continue;
            if (!std.mem.eql(u8, key_name, "status")) continue;

            const val_tag = self.ir_view.getTag(prop.value) orelse continue;
            if (val_tag == .lit_int) {
                const val = self.ir_view.getIntValue(prop.value) orelse continue;
                if (val >= 100 and val <= 599) return @intCast(val);
            }
        }
        return null;
    }

    // -------------------------------------------------------------------
    // Test case emission
    // -------------------------------------------------------------------

    fn emitTestCase(self: *PathGenerator, status: u16) !void {
        self.path_count += 1;

        // Build test name from constraints
        var name_buf: [256]u8 = undefined;
        var name_len: usize = 0;
        const prefix = std.fmt.bufPrint(name_buf[0..], "path {d}", .{self.path_count}) catch "path";
        name_len = prefix.len;

        for (self.constraints.items) |c| {
            const desc: []const u8 = switch (c) {
                .req_method => |m| m,
                .req_url => |u| u,
                .stub_truthy => |s| s.func,
                .stub_falsy => "!falsy",
                .result_ok => "result.ok",
                .result_not_ok => "!result.ok",
                else => "",
            };
            if (desc.len > 0 and name_len + 2 + desc.len < name_buf.len) {
                name_buf[name_len] = ' ';
                name_len += 1;
                @memcpy(name_buf[name_len .. name_len + desc.len], desc);
                name_len += desc.len;
            }
        }

        const name = try self.allocator.dupe(u8, name_buf[0..name_len]);

        // Derive request properties from constraints
        var method: []const u8 = "GET";
        var url: []const u8 = "/";
        var has_auth = false;

        // Build I/O stubs from constraints + tracked calls
        var io_stubs: std.ArrayList(IoStub) = .empty;
        var seq: u32 = 0;

        // First pass: gather stub requirements from constraints
        var stub_overrides = std.StringHashMapUnmanaged([]const u8).empty;
        defer stub_overrides.deinit(self.allocator);

        for (self.constraints.items) |c| {
            switch (c) {
                .req_method => |m| method = m,
                .req_url => |u| url = u,
                .req_url_prefix => |p| url = p,
                .stub_truthy => |info| {
                    stub_overrides.put(self.allocator, info.func, stubValueForType(info.returns, true)) catch {};
                    if (std.mem.eql(u8, info.func, "parseBearer")) has_auth = true;
                },
                .stub_falsy => |info| {
                    stub_overrides.put(self.allocator, info.func, stubValueForType(info.returns, false)) catch {};
                },
                .result_ok => |info| {
                    stub_overrides.put(self.allocator, info.func, "{\"ok\":true,\"value\":{}}") catch {};
                },
                .result_not_ok => |info| {
                    stub_overrides.put(self.allocator, info.func, "{\"ok\":false,\"error\":\"test-error\"}") catch {};
                },
            }
        }

        // Second pass: emit I/O stubs for tracked calls on this path
        for (self.io_seq.items) |io_call| {
            const result_json = stub_overrides.get(io_call.func) orelse
                stubValueForType(io_call.returns, true);

            try io_stubs.append(self.allocator, .{
                .seq = seq,
                .module = io_call.module,
                .func = io_call.func,
                .result_json = result_json,
            });
            seq += 1;
        }

        try self.tests.append(self.allocator, .{
            .name = name,
            .method = method,
            .url = url,
            .has_auth_header = has_auth,
            .expected_status = status,
            .io_stubs = io_stubs,
        });
    }

    // -------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------

    fn stmtAlwaysReturns(self: *const PathGenerator, node: NodeIndex) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        return switch (tag) {
            .return_stmt => true,
            .block, .program => blk: {
                const block = self.ir_view.getBlock(node) orelse break :blk false;
                var i: u16 = 0;
                while (i < block.stmts_count) : (i += 1) {
                    const stmt_idx = self.ir_view.getListIndex(block.stmts_start, i);
                    if (self.stmtAlwaysReturns(stmt_idx)) break :blk true;
                }
                break :blk false;
            },
            .if_stmt => blk: {
                const if_s = self.ir_view.getIfStmt(node) orelse break :blk false;
                if (if_s.else_branch == null_node) break :blk false;
                break :blk self.stmtAlwaysReturns(if_s.then_branch) and self.stmtAlwaysReturns(if_s.else_branch);
            },
            else => false,
        };
    }

    fn isResponseHelper(self: *const PathGenerator, callee: NodeIndex) bool {
        const tag = self.ir_view.getTag(callee) orelse return false;
        if (tag != .member_access) return false;
        const member = self.ir_view.getMember(callee) orelse return false;

        const obj_tag = self.ir_view.getTag(member.object) orelse return false;
        if (obj_tag != .identifier) return false;
        const binding = self.ir_view.getBinding(member.object) orelse return false;
        if (binding.kind != .undeclared_global) return false;
        const obj_name = self.resolveAtomName(binding.slot) orelse return false;
        if (!std.mem.eql(u8, obj_name, "Response")) return false;

        const method_name = self.resolveAtomName(member.property) orelse return false;
        return std.mem.eql(u8, method_name, "json") or
            std.mem.eql(u8, method_name, "text") or
            std.mem.eql(u8, method_name, "html") or
            std.mem.eql(u8, method_name, "redirect");
    }

    fn isReqMemberAccess(self: *const PathGenerator, node: NodeIndex, prop: []const u8) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        if (tag != .member_access) return false;
        const member = self.ir_view.getMember(node) orelse return false;

        const obj_tag = self.ir_view.getTag(member.object) orelse return false;
        if (obj_tag != .identifier) return false;
        const binding = self.ir_view.getBinding(member.object) orelse return false;
        const key = packBindingKey(binding.scope_id, binding.slot);
        if (self.req_binding_key == null or key != self.req_binding_key.?) return false;

        const prop_name = self.resolveAtomName(member.property) orelse return false;
        return std.mem.eql(u8, prop_name, prop);
    }

    fn getCalleeMeta(self: *const PathGenerator, callee: NodeIndex) ?FnMeta {
        const tag = self.ir_view.getTag(callee) orelse return null;
        if (tag != .identifier) return null;
        const binding = self.ir_view.getBinding(callee) orelse return null;
        return self.module_fn_bindings.get(binding.slot);
    }

    fn getPropertyKeyName(self: *const PathGenerator, key_idx: NodeIndex) ?[]const u8 {
        const tag = self.ir_view.getTag(key_idx) orelse return null;
        if (tag == .identifier) {
            const binding = self.ir_view.getBinding(key_idx) orelse return null;
            return self.resolveAtomName(binding.slot);
        } else if (tag == .lit_string) {
            const str_idx = self.ir_view.getStringIdx(key_idx) orelse return null;
            return self.ir_view.getString(str_idx);
        }
        return null;
    }

    fn resolveAtomName(self: *const PathGenerator, atom_idx: u16) ?[]const u8 {
        if (self.atoms) |table| {
            const atom: object.Atom = @enumFromInt(atom_idx);
            if (atom.toPredefinedName()) |name| return name;
            return table.getName(atom);
        }
        if (self.ir_view.getString(atom_idx)) |name| return name;
        const atom: object.Atom = @enumFromInt(atom_idx);
        return atom.toPredefinedName();
    }
};

// ---------------------------------------------------------------------------
// Stub value synthesis
// ---------------------------------------------------------------------------

fn stubValueForType(returns: mb.ReturnKind, truthy: bool) []const u8 {
    if (!truthy) {
        return switch (returns) {
            .optional_string, .optional_object => "null",
            .result => "{\"ok\":false,\"error\":\"test-error\"}",
            .boolean => "false",
            .number => "0",
            else => "null",
        };
    }
    return switch (returns) {
        .optional_string, .string => "\"test-value\"",
        .optional_object, .object => "{\"id\":\"1\"}",
        .result => "{\"ok\":true,\"value\":{}}",
        .boolean => "true",
        .number => "42",
        .undefined => "null",
        .unknown => "\"test-value\"",
    };
}

fn writeJsonEscaped(writer: anytype, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => try writer.writeByte(c),
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "stubValueForType truthy" {
    try std.testing.expectEqualStrings("\"test-value\"", stubValueForType(.optional_string, true));
    try std.testing.expectEqualStrings("{\"ok\":true,\"value\":{}}", stubValueForType(.result, true));
    try std.testing.expectEqualStrings("true", stubValueForType(.boolean, true));
}

test "stubValueForType falsy" {
    try std.testing.expectEqualStrings("null", stubValueForType(.optional_string, false));
    try std.testing.expectEqualStrings("{\"ok\":false,\"error\":\"test-error\"}", stubValueForType(.result, false));
    try std.testing.expectEqualStrings("false", stubValueForType(.boolean, false));
}

test "writeJsonEscaped" {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);
    try writeJsonEscaped(&aw.writer, "hello \"world\"\nnewline");
    buf = aw.toArrayList();
    try std.testing.expectEqualStrings("hello \\\"world\\\"\\nnewline", buf.items);
}
