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
const handler_contract = @import("handler_contract.zig");
const contract_types = @import("contract_types.zig");

const Node = ir.Node;
const NodeIndex = ir.NodeIndex;
const IrView = ir.IrView;
const null_node = ir.null_node;
const packBindingKey = bool_checker_mod.packBindingKey;

// ---------------------------------------------------------------------------
// Path constraints
// ---------------------------------------------------------------------------

pub const Constraint = union(enum) {
    req_method: []const u8,
    req_url: []const u8,
    stub_truthy: StubInfo,
    stub_falsy: StubInfo,
    result_ok: StubInfo,
    result_not_ok: StubInfo,

    /// Whether this constraint represents an I/O failure condition.
    pub fn isFailure(self: Constraint) bool {
        return switch (self) {
            .stub_falsy, .result_not_ok => true,
            else => false,
        };
    }
};

pub const StubInfo = struct {
    module: []const u8,
    func: []const u8,
    returns: mb.ReturnKind,
};

// ---------------------------------------------------------------------------
// Generated test
// ---------------------------------------------------------------------------

pub const IoStub = struct {
    seq: u32,
    module: []const u8,
    func: []const u8,
    result_json: []const u8,
    /// Canonical arg signature (pipe-delimited), owned when non-null.
    /// See `handler_contract.PathIoCall.arg_signature` for the format.
    arg_signature: ?[]const u8 = null,
};

pub const GeneratedTest = struct {
    name: []const u8,
    method: []const u8,
    url: []const u8,
    has_auth_header: bool,
    expected_status: u16,
    io_stubs: std.ArrayList(IoStub),
    constraints: []const Constraint = &.{},
};

fn dupePathCondition(
    allocator: std.mem.Allocator,
    constraint: Constraint,
) !handler_contract.PathCondition {
    return switch (constraint) {
        .req_method => |value| .{
            .kind = .req_method,
            .value = try allocator.dupe(u8, value),
        },
        .req_url => |value| .{
            .kind = .req_url,
            .value = try allocator.dupe(u8, value),
        },
        .stub_truthy, .result_ok => |info| try dupeIoCondition(allocator, .io_ok, info),
        .stub_falsy, .result_not_ok => |info| try dupeIoCondition(allocator, .io_fail, info),
    };
}

fn dupeIoCondition(
    allocator: std.mem.Allocator,
    kind: handler_contract.PathCondition.Kind,
    info: StubInfo,
) !handler_contract.PathCondition {
    const module = try allocator.dupe(u8, info.module);
    errdefer allocator.free(module);
    const func = try allocator.dupe(u8, info.func);
    return .{ .kind = kind, .module = module, .func = func };
}

fn dupePathIoCall(
    allocator: std.mem.Allocator,
    stub: IoStub,
) !handler_contract.PathIoCall {
    const module = try allocator.dupe(u8, stub.module);
    errdefer allocator.free(module);
    const func = try allocator.dupe(u8, stub.func);
    errdefer allocator.free(func);
    const arg_signature = if (stub.arg_signature) |signature|
        try allocator.dupe(u8, signature)
    else
        null;
    return .{
        .module = module,
        .func = func,
        .arg_signature = arg_signature,
    };
}

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
    /// Active for...of loop bounds for the current path.
    loop_stack: std.ArrayList(LoopFrame),
    /// Owned provenance descriptions from popped loop frames. I/O and path
    /// cost snapshots borrow these strings until PathGenerator.deinit.
    retired_loop_descs: std.ArrayList([]const u8),
    /// Per-emitted-path I/O multiplicity snapshots.
    path_costs: std.ArrayList(PathCost),
    /// Completed test cases.
    tests: std.ArrayList(GeneratedTest),
    path_count: u32,

    pub const MAX_PATHS = 1024;

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
        mult: Mult,
        /// IR node of the originating call expression. Used at emission
        /// time to compute an argument signature for the behavior-path
        /// canonicalizer. null when the call was tracked through a wrapper
        /// (e.g. nullish-coalesce) and the original node is unavailable.
        call_node: ?NodeIndex,
    };

    const LoopFrame = struct {
        bound: LoopBound,
    };

    /// The trip-count bound resolved for one `for...of` iterable.
    pub const LoopBound = union(enum) {
        /// Trip count statically known.
        constant: u32,
        /// Trip count = |source|. desc is owned by PathGenerator.
        symbolic: contract_types.BoundProvenance,
        /// Iterable not identifiable at all. desc is owned by PathGenerator.
        unknown: contract_types.BoundProvenance,
    };

    const SourceMult = struct {
        coefficient: u32,
        source: contract_types.BoundProvenance,
    };

    const Mult = union(enum) {
        once,
        constant_x: u32,
        per_source: SourceMult,
        unbounded: contract_types.BoundProvenance,
    };

    const PathCostEntry = struct {
        module: []const u8,
        mult: Mult,
    };

    const PathCost = struct {
        entries: std.ArrayList(PathCostEntry) = .empty,

        fn deinit(self: *PathCost, allocator: std.mem.Allocator) void {
            self.entries.deinit(allocator);
        }
    };

    const ModuleCost = struct {
        module: []const u8,
        bound: contract_types.Bound,
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
            .loop_stack = .empty,
            .retired_loop_descs = .empty,
            .path_costs = .empty,
            .tests = .empty,
            .path_count = 0,
        };
    }

    pub fn deinit(self: *PathGenerator) void {
        self.module_fn_bindings.deinit(self.allocator);
        self.var_inits.deinit(self.allocator);
        self.constraints.deinit(self.allocator);
        self.io_seq.deinit(self.allocator);
        for (self.loop_stack.items) |*frame| {
            self.freeLoopBound(frame.bound);
        }
        self.loop_stack.deinit(self.allocator);
        for (self.retired_loop_descs.items) |desc| {
            self.allocator.free(desc);
        }
        self.retired_loop_descs.deinit(self.allocator);
        for (self.path_costs.items) |*cost| {
            cost.deinit(self.allocator);
        }
        self.path_costs.deinit(self.allocator);
        for (self.tests.items) |*t| {
            self.allocator.free(t.name);
            for (t.io_stubs.items) |*stub| {
                if (stub.arg_signature) |sig| self.allocator.free(sig);
            }
            t.io_stubs.deinit(self.allocator);
            if (t.constraints.len > 0) self.allocator.free(t.constraints);
        }
        self.tests.deinit(self.allocator);
    }

    /// Generate test cases for the given handler function.
    pub fn generate(self: *PathGenerator, handler_func: NodeIndex) !void {
        try self.scanImports();
        try self.findHandlerBindings(handler_func);

        const func = self.ir_view.getFunction(handler_func) orelse return;
        try self.walkPaths(func.body);
    }

    pub fn getTests(self: *const PathGenerator) []const GeneratedTest {
        return self.tests.items;
    }

    /// Fold every enumerated path's io_seq multiplicities into a per-module
    /// worst-path CostEnvelope. Caller owns the result.
    pub fn buildCostEnvelope(self: *const PathGenerator, allocator: std.mem.Allocator) !contract_types.CostEnvelope {
        var envelope = contract_types.CostEnvelope{
            .entries = .empty,
            .total = .{ .constant = 0 },
            .exhaustive = self.tests.items.len < MAX_PATHS,
        };
        errdefer envelope.deinit(allocator);

        for (self.path_costs.items) |path_cost| {
            var path_entries: std.ArrayList(ModuleCost) = .empty;
            defer path_entries.deinit(allocator);

            var path_total = contract_types.Bound{ .constant = 0 };
            for (path_cost.entries.items) |entry| {
                const call_bound = boundFromMult(entry.mult);
                path_total = contract_types.Bound.addBorrowed(path_total, call_bound);

                if (findModuleCost(path_entries.items, entry.module)) |idx| {
                    path_entries.items[idx].bound =
                        contract_types.Bound.addBorrowed(path_entries.items[idx].bound, call_bound);
                } else {
                    try path_entries.append(allocator, .{
                        .module = entry.module,
                        .bound = call_bound,
                    });
                }
            }

            for (path_entries.items) |module_cost| {
                if (findCostEntry(envelope.entries.items, module_cost.module)) |idx| {
                    const candidate = contract_types.Bound.maxBorrowed(envelope.entries.items[idx].bound, module_cost.bound);
                    const owned = try candidate.dupeOwned(allocator);
                    envelope.entries.items[idx].bound.deinitOwned(allocator);
                    envelope.entries.items[idx].bound = owned;
                } else {
                    const module = try allocator.dupe(u8, module_cost.module);
                    errdefer allocator.free(module);
                    var bound = try module_cost.bound.dupeOwned(allocator);
                    errdefer bound.deinitOwned(allocator);
                    try envelope.entries.append(allocator, .{
                        .module = module,
                        .bound = bound,
                    });
                }
            }

            const total_candidate = contract_types.Bound.maxBorrowed(envelope.total, path_total);
            const owned_total = try total_candidate.dupeOwned(allocator);
            envelope.total.deinitOwned(allocator);
            envelope.total = owned_total;
        }

        if (!envelope.exhaustive) {
            const truncated_desc = try allocator.dupe(u8, "path enumeration truncated at 1024");
            envelope.total.deinitOwned(allocator);
            envelope.total = .{ .unbounded = .{
                .line = 0,
                .column = 0,
                .desc = truncated_desc,
            } };
        }

        std.mem.sort(contract_types.CostEntry, envelope.entries.items, {}, costEntryLessThan);
        return envelope;
    }

    fn findModuleCost(items: []const ModuleCost, module: []const u8) ?usize {
        for (items, 0..) |item, idx| {
            if (std.mem.eql(u8, item.module, module)) return idx;
        }
        return null;
    }

    fn findCostEntry(items: []const contract_types.CostEntry, module: []const u8) ?usize {
        for (items, 0..) |item, idx| {
            if (std.mem.eql(u8, item.module, module)) return idx;
        }
        return null;
    }

    fn costEntryLessThan(_: void, a: contract_types.CostEntry, b: contract_types.CostEntry) bool {
        return std.mem.lessThan(u8, a.module, b.module);
    }

    fn boundFromMult(mult: Mult) contract_types.Bound {
        return switch (mult) {
            .once => .{ .constant = 1 },
            .constant_x => |count| .{ .constant = count },
            .per_source => |source| .{ .linear = .{
                .coefficient = source.coefficient,
                .base = 0,
                .source = source.source,
            } },
            .unbounded => |source| .{ .unbounded = source },
        };
    }

    /// Write all generated tests as JSONL to writer.
    pub fn writeJsonl(self: *const PathGenerator, writer: anytype) !void {
        for (self.tests.items) |test_case| {
            // Test header
            try writer.writeAll("{\"type\":\"test\",\"name\":");
            try handler_contract.writeJsonString(writer, test_case.name);
            try writer.writeAll("}\n");

            try writer.writeAll("{\"type\":\"request\",\"method\":\"");
            try writer.writeAll(test_case.method);
            try writer.writeAll("\",\"url\":");
            try handler_contract.writeJsonString(writer, test_case.url);
            if (test_case.has_auth_header) {
                try writer.writeAll(",\"headers\":{\"authorization\":\"Bearer test-token\"},\"body\":null}\n");
            } else {
                try writer.writeAll(",\"headers\":{},\"body\":null}\n");
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

    /// Convert generated tests into BehaviorPath structs for the contract.
    /// Caller owns the returned list and its contents.
    pub fn toBehaviorPaths(self: *const PathGenerator, allocator: std.mem.Allocator) !std.ArrayList(handler_contract.BehaviorPath) {
        var paths: std.ArrayList(handler_contract.BehaviorPath) = .empty;
        errdefer {
            for (paths.items) |*p| p.deinit(allocator);
            paths.deinit(allocator);
        }

        for (self.tests.items) |test_case| {
            var conditions: std.ArrayList(handler_contract.PathCondition) = .empty;
            errdefer {
                for (conditions.items) |*c| @constCast(c).deinit(allocator);
                conditions.deinit(allocator);
            }

            var is_failure = false;
            for (test_case.constraints) |constraint| {
                if (constraint.isFailure()) is_failure = true;

                var cond = try dupePathCondition(allocator, constraint);
                errdefer cond.deinit(allocator);
                try conditions.append(allocator, cond);
            }

            var io_sequence: std.ArrayList(handler_contract.PathIoCall) = .empty;
            errdefer {
                for (io_sequence.items) |*io| @constCast(io).deinit(allocator);
                io_sequence.deinit(allocator);
            }

            for (test_case.io_stubs.items) |stub| {
                var io_call = try dupePathIoCall(allocator, stub);
                errdefer io_call.deinit(allocator);
                try io_sequence.append(allocator, io_call);
            }

            const route_method = try allocator.dupe(u8, test_case.method);
            errdefer allocator.free(route_method);
            const route_pattern = try allocator.dupe(u8, test_case.url);
            errdefer allocator.free(route_pattern);
            try paths.append(allocator, .{
                .route_method = route_method,
                .route_pattern = route_pattern,
                .conditions = conditions,
                .io_sequence = io_sequence,
                .response_status = test_case.expected_status,
                .io_depth = @intCast(test_case.io_stubs.items.len),
                .is_failure_path = is_failure,
            });
        }

        return paths;
    }

    // -------------------------------------------------------------------
    // Phase 1: Scan imports and handler bindings
    // -------------------------------------------------------------------

    fn scanImports(self: *PathGenerator) error{OutOfMemory}!void {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .import_decl) continue;

            const import_decl = self.ir_view.getImportDecl(idx) orelse continue;
            const module_str = self.ir_view.getString(import_decl.module_idx) orelse continue;
            const binding = builtin_modules.fromSpecifier(module_str) orelse continue;

            var j: u8 = 0;
            while (j < import_decl.specifiers_count) : (j += 1) {
                const spec_idx = self.ir_view.getListIndex(import_decl.specifiers_start, j);
                const spec = self.ir_view.getImportSpec(spec_idx) orelse continue;
                const imported_name = self.resolveAtomName(spec.imported_atom) orelse continue;

                if (builtin_modules.findExport(module_str, imported_name)) |entry| {
                    try self.module_fn_bindings.put(self.allocator, spec.local_binding.slot, .{
                        .module = binding.name,
                        .func = entry.func.name,
                        .returns = entry.func.returns,
                    });
                }
            }
        }
    }

    fn findHandlerBindings(self: *PathGenerator, handler_func: NodeIndex) error{OutOfMemory}!void {
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
            try self.var_inits.put(self.allocator, key, vd.init);

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
                try self.extractAllConstraints(if_s.condition, false);
                try self.walkPaths(if_s.then_branch);
                self.constraints.shrinkRetainingCapacity(saved_constraints);
                self.io_seq.shrinkRetainingCapacity(saved_io);

                // Else-branch: push negated constraints
                if (if_s.else_branch != null_node) {
                    try self.extractAllConstraints(if_s.condition, true);
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
                    try self.var_inits.put(self.allocator, key, vd.init);
                    try self.trackModuleCall(vd.init, key);
                }
            },

            .expr_stmt => {
                if (self.ir_view.getOptValue(node)) |expr| {
                    try self.trackModuleCall(expr, null);
                }
            },

            .for_of_stmt => {
                const fi = self.ir_view.getForIter(node) orelse return;
                const bound = self.resolveLoopBound(fi.iterable, node);
                self.retired_loop_descs.ensureUnusedCapacity(self.allocator, 1) catch |err| {
                    self.freeLoopBound(bound);
                    return err;
                };
                self.loop_stack.append(self.allocator, .{ .bound = bound }) catch |err| {
                    self.freeLoopBound(bound);
                    return err;
                };
                defer {
                    const frame = self.loop_stack.pop().?;
                    self.retireLoopBound(frame.bound);
                }
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
                    try self.extractAllConstraints(if_s.condition, false);
                    try self.walkPaths(if_s.then_branch);
                    self.constraints.shrinkRetainingCapacity(saved);
                    self.io_seq.shrinkRetainingCapacity(saved_io);

                    // Fall-through with negated constraints
                    try self.extractAllConstraints(if_s.condition, true);
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
    fn extractAllConstraints(self: *PathGenerator, cond: NodeIndex, negate: bool) error{OutOfMemory}!void {
        const tag = self.ir_view.getTag(cond) orelse return;

        // AND chains: extract from both sides
        if (tag == .binary_op) {
            const bin = self.ir_view.getBinary(cond) orelse return;
            if (bin.op == .and_op) {
                try self.extractAllConstraints(bin.left, negate);
                try self.extractAllConstraints(bin.right, negate);
                return;
            }
        }

        // Extract single constraint from this node
        if (self.extractConditionConstraint(cond)) |c| {
            const final = if (negate) self.negateConstraint(c) else c;
            if (final) |f| try self.constraints.append(self.allocator, f);
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
            .req_method, .req_url => null,
            .stub_truthy => |info| .{ .stub_falsy = info },
            .stub_falsy => |info| .{ .stub_truthy = info },
            .result_ok => |info| .{ .result_not_ok = info },
            .result_not_ok => |info| .{ .result_ok = info },
        };
    }

    // -------------------------------------------------------------------
    // I/O call tracking
    // -------------------------------------------------------------------

    fn trackModuleCall(self: *PathGenerator, node: NodeIndex, binding_key: ?u32) error{OutOfMemory}!void {
        const tag = self.ir_view.getTag(node) orelse return;

        if (tag == .call) {
            const call = self.ir_view.getCall(node) orelse return;
            if (self.getCalleeMeta(call.callee)) |meta| {
                try self.io_seq.append(self.allocator, .{
                    .module = meta.module,
                    .func = meta.func,
                    .returns = meta.returns,
                    .binding_key = binding_key,
                    .mult = self.currentMultiplicity(),
                    .call_node = node,
                });
            }
            return;
        }

        // Look through nullish coalescing: env("X") ?? "default"
        if (tag == .binary_op) {
            const bin = self.ir_view.getBinary(node) orelse return;
            if (bin.op == .nullish) {
                try self.trackModuleCall(bin.left, binding_key);
                return;
            }
        }
    }

    fn currentMultiplicity(self: *const PathGenerator) Mult {
        if (self.loop_stack.items.len == 0) return .once;

        var constant_product: u32 = 1;
        var symbolic_count: u32 = 0;
        var symbolic_source: contract_types.BoundProvenance = .{};
        var innermost_dynamic: contract_types.BoundProvenance = .{};
        var saw_unknown = false;

        for (self.loop_stack.items) |frame| {
            switch (frame.bound) {
                .constant => |count| {
                    constant_product = saturatingMulU32(constant_product, count);
                },
                .symbolic => |source| {
                    symbolic_count += 1;
                    symbolic_source = source;
                    innermost_dynamic = source;
                },
                .unknown => |source| {
                    saw_unknown = true;
                    innermost_dynamic = source;
                },
            }
        }

        if (saw_unknown or symbolic_count > 1) {
            return .{ .unbounded = innermost_dynamic };
        }
        if (symbolic_count == 1) {
            return .{ .per_source = .{
                .coefficient = constant_product,
                .source = symbolic_source,
            } };
        }
        return .{ .constant_x = constant_product };
    }

    fn saturatingMulU32(a: u32, b: u32) u32 {
        const product, const overflow = @mulWithOverflow(a, b);
        return if (overflow != 0) std.math.maxInt(u32) else product;
    }

    /// Build a canonical argument signature for the call at `call_node`.
    /// Pipe-delimited, one token per argument position:
    ///   "lit:<raw>"   - string literal
    ///   "int:<raw>"   - integer literal
    ///   "bool:true"   - boolean literal
    ///   "bool:false"
    ///   "?"           - dynamic or unknown
    /// Returns an owned slice. Caller frees.
    fn computeArgSignature(self: *const PathGenerator, allocator: std.mem.Allocator, call_node: NodeIndex) !?[]const u8 {
        const tag = self.ir_view.getTag(call_node) orelse return null;
        if (tag != .call and tag != .method_call) return null;
        const call = self.ir_view.getCall(call_node) orelse return null;

        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);

        var j: u8 = 0;
        while (j < call.args_count) : (j += 1) {
            if (j > 0) try buf.append(allocator, '|');
            const arg_idx = self.ir_view.getListIndex(call.args_start, j);
            const arg_tag = self.ir_view.getTag(arg_idx) orelse {
                try buf.append(allocator, '?');
                continue;
            };
            switch (arg_tag) {
                .lit_string => {
                    const str_idx = self.ir_view.getStringIdx(arg_idx) orelse {
                        try buf.append(allocator, '?');
                        continue;
                    };
                    const raw = self.ir_view.getString(str_idx) orelse {
                        try buf.append(allocator, '?');
                        continue;
                    };
                    try buf.appendSlice(allocator, "lit:");
                    try buf.appendSlice(allocator, raw);
                },
                .lit_int => {
                    const v = self.ir_view.getIntValue(arg_idx) orelse {
                        try buf.append(allocator, '?');
                        continue;
                    };
                    try buf.print(allocator, "int:{d}", .{v});
                },
                .lit_bool => {
                    const b = self.ir_view.getBoolValue(arg_idx) orelse {
                        try buf.append(allocator, '?');
                        continue;
                    };
                    try buf.appendSlice(allocator, if (b) "bool:true" else "bool:false");
                },
                else => try buf.append(allocator, '?'),
            }
        }

        return try buf.toOwnedSlice(allocator);
    }

    // -------------------------------------------------------------------
    // Response status extraction
    // -------------------------------------------------------------------

    fn extractReturnStatus(self: *PathGenerator, node: NodeIndex) u16 {
        const ret_val = self.ir_view.getOptValue(node) orelse return 200;
        const ret_tag = self.ir_view.getTag(ret_val) orelse return 200;

        if (ret_tag == .call or ret_tag == .method_call) {
            const call = self.ir_view.getCall(ret_val) orelse return 200;

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
        if (self.path_count >= MAX_PATHS) return;
        self.path_count += 1;
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
            };
            if (desc.len > 0 and name_len + 2 + desc.len < name_buf.len) {
                name_buf[name_len] = ' ';
                name_len += 1;
                @memcpy(name_buf[name_len .. name_len + desc.len], desc);
                name_len += desc.len;
            }
        }

        const name = try self.allocator.dupe(u8, name_buf[0..name_len]);
        errdefer self.allocator.free(name);

        // Derive request properties from constraints
        var method: []const u8 = "GET";
        var url: []const u8 = "/";
        var has_auth = false;

        // Build I/O stubs from constraints + tracked calls
        var io_stubs: std.ArrayList(IoStub) = .empty;
        errdefer {
            for (io_stubs.items) |stub| {
                if (stub.arg_signature) |sig| self.allocator.free(sig);
            }
            io_stubs.deinit(self.allocator);
        }
        var seq: u32 = 0;

        // First pass: gather stub requirements from constraints
        var stub_overrides = std.StringHashMapUnmanaged([]const u8).empty;
        defer stub_overrides.deinit(self.allocator);

        for (self.constraints.items) |c| {
            switch (c) {
                .req_method => |m| method = m,
                .req_url => |u| url = u,
                .stub_truthy => |info| {
                    try stub_overrides.put(self.allocator, info.func, stubValueForType(info.returns, true));
                    if (std.mem.eql(u8, info.func, "parseBearer")) has_auth = true;
                },
                .stub_falsy => |info| {
                    try stub_overrides.put(self.allocator, info.func, stubValueForType(info.returns, false));
                },
                .result_ok => |info| {
                    try stub_overrides.put(self.allocator, info.func, "{\"ok\":true,\"value\":{}}");
                },
                .result_not_ok => |info| {
                    try stub_overrides.put(self.allocator, info.func, "{\"ok\":false,\"error\":\"test-error\"}");
                },
            }
        }

        // Second pass: emit I/O stubs for tracked calls on this path
        for (self.io_seq.items) |io_call| {
            const result_json = stub_overrides.get(io_call.func) orelse
                stubValueForType(io_call.returns, true);

            {
                const arg_sig: ?[]const u8 = if (io_call.call_node) |cn|
                    self.computeArgSignature(self.allocator, cn) catch null
                else
                    null;
                errdefer if (arg_sig) |sig| self.allocator.free(sig);

                try io_stubs.append(self.allocator, .{
                    .seq = seq,
                    .module = io_call.module,
                    .func = io_call.func,
                    .result_json = result_json,
                    .arg_signature = arg_sig,
                });
            }
            seq += 1;
        }

        // Snapshot current constraints for fault coverage analysis
        const constraints_snapshot = try self.allocator.dupe(Constraint, self.constraints.items);
        errdefer if (constraints_snapshot.len > 0) self.allocator.free(constraints_snapshot);
        try self.snapshotPathCost();

        try self.tests.append(self.allocator, .{
            .name = name,
            .method = method,
            .url = url,
            .has_auth_header = has_auth,
            .expected_status = status,
            .io_stubs = io_stubs,
            .constraints = constraints_snapshot,
        });
    }

    fn snapshotPathCost(self: *PathGenerator) !void {
        var path_cost = PathCost{};
        errdefer path_cost.deinit(self.allocator);

        try path_cost.entries.ensureTotalCapacity(self.allocator, self.io_seq.items.len);
        for (self.io_seq.items) |io_call| {
            path_cost.entries.appendAssumeCapacity(.{
                .module = io_call.module,
                .mult = io_call.mult,
            });
        }
        try self.path_costs.append(self.allocator, path_cost);
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
        const obj_name = self.resolveAtomName(binding.name_atom) orelse return false;
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
            return self.resolveAtomName(binding.name_atom);
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

    fn retireLoopBound(self: *PathGenerator, bound: LoopBound) void {
        switch (bound) {
            .constant => {},
            .symbolic => |p| if (p.desc.len > 0) self.retired_loop_descs.appendAssumeCapacity(p.desc),
            .unknown => |p| if (p.desc.len > 0) self.retired_loop_descs.appendAssumeCapacity(p.desc),
        }
    }

    fn freeLoopBound(self: *PathGenerator, bound: LoopBound) void {
        switch (bound) {
            .constant => {},
            .symbolic => |p| if (p.desc.len > 0) self.allocator.free(p.desc),
            .unknown => |p| if (p.desc.len > 0) self.allocator.free(p.desc),
        }
    }

    fn allocLoopDesc(self: *PathGenerator, comptime fmt: []const u8, args: anytype, fallback: []const u8) []const u8 {
        return std.fmt.allocPrint(self.allocator, fmt, args) catch
            self.allocator.dupe(u8, fallback) catch "";
    }

    // P2 SEAM
    /// P2 extends this body to discharge known-size iterables. Keep callers
    /// above this seam so later phases can edit this function in isolation.
    fn resolveLoopBound(self: *PathGenerator, iterable: NodeIndex, loop_node: NodeIndex) LoopBound {
        if (self.constantLoopBound(iterable)) |count| return .{ .constant = count };

        const loc: ir.SourceLocation = self.ir_view.getLoc(loop_node) orelse .{ .line = 0, .column = 0, .offset = 0 };
        const tag = self.ir_view.getTag(iterable) orelse {
            return .{ .unknown = .{
                .line = loc.line,
                .column = loc.column,
                .desc = self.allocLoopDesc("{s}", .{"for...of (unrecognized iterable)"}, "for...of (unrecognized iterable)"),
            } };
        };

        switch (tag) {
            // An identifier or member access names a real collection value we
            // iterate: the trip count is |that collection|, so the bound is
            // symbolic. We attach the collection's source name when it is
            // recoverable (see `iterableName`); a function-local whose name the
            // IR does not retain still yields a symbolic (named-less) bound.
            .identifier, .member_access => {
                if (self.iterableName(iterable)) |name| {
                    return .{ .symbolic = .{
                        .line = loc.line,
                        .column = loc.column,
                        .desc = self.allocLoopDesc("for...of over `{s}`", .{name}, "for...of loop"),
                    } };
                }
                return .{ .symbolic = .{
                    .line = loc.line,
                    .column = loc.column,
                    .desc = self.allocLoopDesc("{s}", .{"for...of loop"}, "for...of loop"),
                } };
            },
            else => return .{ .unknown = .{
                .line = loc.line,
                .column = loc.column,
                .desc = self.allocLoopDesc("{s}", .{"for...of (unrecognized iterable)"}, "for...of (unrecognized iterable)"),
            } },
        }
    }

    fn constantLoopBound(self: *PathGenerator, iterable: NodeIndex) ?u32 {
        const tag = self.ir_view.getTag(iterable) orelse return null;
        switch (tag) {
            .array_literal => return self.arrayLiteralCount(iterable),
            .call, .method_call => {
                if (self.rangeCallCount(iterable)) |count| return count;
                if (self.sliceCallCeiling(iterable)) |count| return count;
                if (self.objectKeysLiteralCount(iterable)) |count| return count;
            },
            .identifier => if (self.sqlLimitForIdentifier(iterable)) |count| return count,
            .member_access => if (self.schemaMaxItemsForMember(iterable)) |count| return count,
            else => {},
        }
        return null;
    }

    fn arrayLiteralCount(self: *const PathGenerator, node: NodeIndex) ?u32 {
        const arr = self.ir_view.getArray(node) orelse return null;
        if (arr.has_spread) return null;
        return arr.elements_count;
    }

    fn rangeCallCount(self: *const PathGenerator, node: NodeIndex) ?u32 {
        const call = self.ir_view.getCall(node) orelse return null;
        if (!self.isUndeclaredGlobalName(call.callee, "range")) return null;

        if (call.args_count == 1) {
            const end_node = self.ir_view.getListIndex(call.args_start, 0);
            const end = self.intLiteralValue(end_node) orelse return null;
            return nonNegativeI64ToU32(end);
        }
        if (call.args_count == 2) {
            const start_node = self.ir_view.getListIndex(call.args_start, 0);
            const end_node = self.ir_view.getListIndex(call.args_start, 1);
            const start = self.intLiteralValue(start_node) orelse return null;
            const end = self.intLiteralValue(end_node) orelse return null;
            return nonNegativeI64ToU32(end - start);
        }
        return null;
    }

    fn sliceCallCeiling(self: *const PathGenerator, node: NodeIndex) ?u32 {
        const call = self.ir_view.getCall(node) orelse return null;
        const member = self.memberCalleeNamed(call.callee, "slice") orelse return null;
        _ = member;
        if (call.args_count < 2) return null;

        const start_node = self.ir_view.getListIndex(call.args_start, 0);
        const end_node = self.ir_view.getListIndex(call.args_start, 1);
        const start = self.intLiteralValue(start_node) orelse return null;
        const end = self.intLiteralValue(end_node) orelse return null;
        if (start != 0 or end < 0) return null;
        return nonNegativeI64ToU32(end);
    }

    fn objectKeysLiteralCount(self: *const PathGenerator, node: NodeIndex) ?u32 {
        const call = self.ir_view.getCall(node) orelse return null;
        const member = self.memberCalleeNamed(call.callee, "keys") orelse return null;
        if (!self.isUndeclaredGlobalName(member.object, "Object")) return null;
        if (call.args_count != 1) return null;

        const obj_node = self.ir_view.getListIndex(call.args_start, 0);
        return self.objectLiteralPropertyCount(obj_node);
    }

    fn sqlLimitForIdentifier(self: *const PathGenerator, node: NodeIndex) ?u32 {
        const binding = self.ir_view.getBinding(node) orelse return null;
        const key = packBindingKey(binding.scope_id, binding.slot);
        const init_node = self.var_inits.get(key) orelse return null;
        const tag = self.ir_view.getTag(init_node) orelse return null;
        if (tag != .call) return null;
        const call = self.ir_view.getCall(init_node) orelse return null;
        const meta = self.getCalleeMeta(call.callee) orelse return null;
        if (!std.mem.eql(u8, meta.module, "sql")) return null;
        if (!std.mem.eql(u8, meta.func, "sql") and !std.mem.eql(u8, meta.func, "sqlMany")) return null;
        if (call.args_count == 0) return null;

        const statement_node = self.ir_view.getListIndex(call.args_start, 0);
        const statement = self.stringLiteralValue(statement_node) orelse return null;
        return trailingLimitValue(statement);
    }

    const ValidatedField = struct {
        binding_key: u32,
        field: []const u8,
    };

    fn schemaMaxItemsForMember(self: *PathGenerator, node: NodeIndex) ?u32 {
        const access = self.validatedFieldAccess(node) orelse return null;
        const init_node = self.var_inits.get(access.binding_key) orelse return null;
        const tag = self.ir_view.getTag(init_node) orelse return null;
        if (tag != .call) return null;
        const call = self.ir_view.getCall(init_node) orelse return null;
        const meta = self.getCalleeMeta(call.callee) orelse return null;
        if (!std.mem.eql(u8, meta.module, "validate")) return null;
        if (!std.mem.eql(u8, meta.func, "validateJson") and
            !std.mem.eql(u8, meta.func, "validateObject") and
            !std.mem.eql(u8, meta.func, "coerceJson"))
        {
            return null;
        }
        if (call.args_count == 0) return null;

        const schema_name_node = self.ir_view.getListIndex(call.args_start, 0);
        const schema_name = self.stringLiteralValue(schema_name_node) orelse return null;
        const schema_json = self.schemaJsonLiteral(schema_name) orelse return null;
        return self.schemaFieldMaxItems(schema_json, access.field);
    }

    fn validatedFieldAccess(self: *const PathGenerator, node: NodeIndex) ?ValidatedField {
        const outer = self.ir_view.getMember(node) orelse return null;
        const field = self.resolveAtomName(outer.property) orelse return null;

        var root = outer.object;
        if (self.ir_view.getTag(root) == .member_access) {
            const inner = self.ir_view.getMember(root) orelse return null;
            const inner_prop = self.resolveAtomName(inner.property) orelse return null;
            if (!std.mem.eql(u8, inner_prop, "value")) return null;
            root = inner.object;
        }

        const root_tag = self.ir_view.getTag(root) orelse return null;
        if (root_tag != .identifier) return null;
        const binding = self.ir_view.getBinding(root) orelse return null;
        return .{
            .binding_key = packBindingKey(binding.scope_id, binding.slot),
            .field = field,
        };
    }

    fn schemaJsonLiteral(self: *const PathGenerator, schema_name: []const u8) ?[]const u8 {
        const node_count = self.ir_view.nodeCount();
        for (0..node_count) |idx_usize| {
            const idx: NodeIndex = @intCast(idx_usize);
            const tag = self.ir_view.getTag(idx) orelse continue;
            if (tag != .call) continue;

            const call = self.ir_view.getCall(idx) orelse continue;
            const meta = self.getCalleeMeta(call.callee) orelse continue;
            if (!std.mem.eql(u8, meta.module, "validate")) continue;
            if (!std.mem.eql(u8, meta.func, "schemaCompile")) continue;
            if (call.args_count < 2) continue;

            const name_node = self.ir_view.getListIndex(call.args_start, 0);
            const found_name = self.stringLiteralValue(name_node) orelse continue;
            if (!std.mem.eql(u8, found_name, schema_name)) continue;

            const schema_node = self.ir_view.getListIndex(call.args_start, 1);
            return self.stringLiteralValue(schema_node);
        }
        return null;
    }

    fn schemaFieldMaxItems(self: *PathGenerator, schema_json: []const u8, field: []const u8) ?u32 {
        const parsed = std.json.parseFromSlice(std.json.Value, self.allocator, schema_json, .{}) catch return null;
        defer parsed.deinit();

        const root = switch (parsed.value) {
            .object => |obj| obj,
            else => return null,
        };
        const properties_val = root.get("properties") orelse return null;
        const properties = switch (properties_val) {
            .object => |obj| obj,
            else => return null,
        };
        const field_val = properties.get(field) orelse return null;
        const field_obj = switch (field_val) {
            .object => |obj| obj,
            else => return null,
        };
        const field_type = field_obj.get("type") orelse return null;
        switch (field_type) {
            .string => |s| if (!std.mem.eql(u8, s, "array")) return null,
            else => return null,
        }
        const max_items = field_obj.get("maxItems") orelse return null;
        return jsonValueToU32(max_items);
    }

    fn objectLiteralPropertyCount(self: *const PathGenerator, node: NodeIndex) ?u32 {
        const tag = self.ir_view.getTag(node) orelse return null;
        if (tag != .object_literal) return null;
        const obj = self.ir_view.getObject(node) orelse return null;
        var count: u32 = 0;
        var i: u16 = 0;
        while (i < obj.properties_count) : (i += 1) {
            const prop_idx = self.ir_view.getListIndex(obj.properties_start, i);
            const prop_tag = self.ir_view.getTag(prop_idx) orelse return null;
            switch (prop_tag) {
                .object_property, .object_method, .object_getter, .object_setter => count += 1,
                else => return null,
            }
        }
        return count;
    }

    fn memberCalleeNamed(self: *const PathGenerator, callee: NodeIndex, name: []const u8) ?Node.MemberExpr {
        const tag = self.ir_view.getTag(callee) orelse return null;
        if (tag != .member_access) return null;
        const member = self.ir_view.getMember(callee) orelse return null;
        const prop_name = self.resolveAtomName(member.property) orelse return null;
        if (!std.mem.eql(u8, prop_name, name)) return null;
        return member;
    }

    fn isUndeclaredGlobalName(self: *const PathGenerator, node: NodeIndex, name: []const u8) bool {
        const tag = self.ir_view.getTag(node) orelse return false;
        if (tag != .identifier) return false;
        const binding = self.ir_view.getBinding(node) orelse return false;
        if (binding.kind != .undeclared_global) return false;
        const actual = self.resolveAtomName(binding.name_atom) orelse return false;
        return std.mem.eql(u8, actual, name);
    }

    fn intLiteralValue(self: *const PathGenerator, node: NodeIndex) ?i64 {
        const tag = self.ir_view.getTag(node) orelse return null;
        if (tag != .lit_int) return null;
        return self.ir_view.getIntValue(node) orelse return null;
    }

    fn stringLiteralValue(self: *const PathGenerator, node: NodeIndex) ?[]const u8 {
        const tag = self.ir_view.getTag(node) orelse return null;
        if (tag != .lit_string) return null;
        const str_idx = self.ir_view.getStringIdx(node) orelse return null;
        return self.ir_view.getString(str_idx);
    }

    fn nonNegativeI64ToU32(value: i64) u32 {
        if (value <= 0) return 0;
        if (value > std.math.maxInt(u32)) return std.math.maxInt(u32);
        return @intCast(value);
    }

    fn trailingLimitValue(statement: []const u8) ?u32 {
        var end = statement.len;
        while (end > 0 and std.ascii.isWhitespace(statement[end - 1])) : (end -= 1) {}

        const digits_end = end;
        var digits_start = digits_end;
        while (digits_start > 0 and isAsciiDigit(statement[digits_start - 1])) : (digits_start -= 1) {}
        if (digits_start == digits_end) return null;

        var limit_end = digits_start;
        while (limit_end > 0 and std.ascii.isWhitespace(statement[limit_end - 1])) : (limit_end -= 1) {}
        if (limit_end < "LIMIT".len) return null;
        const limit_start = limit_end - "LIMIT".len;
        if (!std.ascii.eqlIgnoreCase(statement[limit_start..limit_end], "LIMIT")) return null;
        if (limit_start > 0 and !std.ascii.isWhitespace(statement[limit_start - 1])) return null;

        return std.fmt.parseInt(u32, statement[digits_start..digits_end], 10) catch null;
    }

    fn isAsciiDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn jsonValueToU32(v: std.json.Value) ?u32 {
        const max: f64 = @floatFromInt(std.math.maxInt(u32));
        return switch (v) {
            .integer => |i| if (i >= 0 and i <= std.math.maxInt(u32)) @intCast(i) else null,
            .float => |f| if (std.math.isFinite(f) and f >= 0 and f <= max) @intFromFloat(@trunc(f)) else null,
            else => null,
        };
    }

    /// Best-effort source name for a `for...of` iterable, for provenance only.
    /// Returns a BORROWED slice (atom/string table) or null when the name is
    /// not recoverable. `BindingRef.slot` is a real atom index only for
    /// global/builtin bindings; for a function-local it is a scope slot, so
    /// resolving it as an atom would yield a wrong (colliding) predefined name.
    /// For a local we instead recover the name from a request-field
    /// initializer, e.g. `const ids = req.ids`. P2 may extend this.
    fn iterableName(self: *const PathGenerator, iterable: NodeIndex) ?[]const u8 {
        const tag = self.ir_view.getTag(iterable) orelse return null;
        switch (tag) {
            .identifier => {
                const binding = self.ir_view.getBinding(iterable) orelse return null;
                if (binding.kind == .global or binding.kind == .undeclared_global) {
                    return self.resolveAtomName(binding.name_atom);
                }
                const key = packBindingKey(binding.scope_id, binding.slot);
                const init_node = self.var_inits.get(key) orelse return null;
                return self.memberPropertyName(init_node);
            },
            .member_access => return self.memberPropertyName(iterable),
            else => return null,
        }
    }

    fn memberPropertyName(self: *const PathGenerator, node: NodeIndex) ?[]const u8 {
        const tag = self.ir_view.getTag(node) orelse return null;
        if (tag != .member_access) return null;
        const member = self.ir_view.getMember(node) orelse return null;
        return self.resolveAtomName(member.property);
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

test "scanImports tracks virtual module functions with binding names" {
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\const value = env("NAME");
    ;

    var parser = @import("parser/parse.zig").Parser.init(allocator, source);
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    defer parser.deinit();

    _ = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);

    var generator = PathGenerator.init(allocator, ir_view, &atoms);
    defer generator.deinit();
    try generator.scanImports();

    var tracked_slot: ?u16 = null;
    const node_count = ir_view.nodeCount();
    for (0..node_count) |idx_usize| {
        const idx: NodeIndex = @intCast(idx_usize);
        const tag = ir_view.getTag(idx) orelse continue;
        if (tag != .import_decl) continue;
        const import_decl = ir_view.getImportDecl(idx) orelse continue;
        const module_str = ir_view.getString(import_decl.module_idx) orelse continue;
        if (!std.mem.eql(u8, module_str, "zigttp:env")) continue;
        const spec_idx = ir_view.getListIndex(import_decl.specifiers_start, 0);
        const spec = ir_view.getImportSpec(spec_idx) orelse continue;
        tracked_slot = spec.local_binding.slot;
        break;
    }

    const meta = generator.module_fn_bindings.get(tracked_slot orelse return error.ExpectedTrackedSlot) orelse return error.ExpectedTrackedMeta;
    try std.testing.expectEqualStrings("env", meta.module);
    try std.testing.expectEqualStrings("env", meta.func);
    try std.testing.expectEqual(mb.ReturnKind.optional_string, meta.returns);
}

const parser_mod = @import("parser/parse.zig");
const handler_verifier = @import("handler_verifier.zig");

const PathGeneratorFixture = struct {
    parser: parser_mod.Parser,
    atoms: context.AtomTable,
    generator: PathGenerator,

    fn deinit(self: *PathGeneratorFixture) void {
        self.generator.deinit();
        self.parser.deinit();
        self.atoms.deinit();
    }
};

const FailNextAllocation = struct {
    child: std.mem.Allocator,
    armed: bool = true,
    allocations_before_failure: usize = 0,

    fn init(child: std.mem.Allocator) FailNextAllocation {
        return .{ .child = child };
    }

    fn allocator(self: *FailNextAllocation) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn alloc(
        context_ptr: *anyopaque,
        len: usize,
        alignment: std.mem.Alignment,
        return_address: usize,
    ) ?[*]u8 {
        const self: *FailNextAllocation = @ptrCast(@alignCast(context_ptr));
        if (self.armed) {
            if (self.allocations_before_failure > 0) {
                self.allocations_before_failure -= 1;
                return self.child.rawAlloc(len, alignment, return_address);
            }
            self.armed = false;
            return null;
        }
        return self.child.rawAlloc(len, alignment, return_address);
    }

    fn resize(
        context_ptr: *anyopaque,
        memory: []u8,
        alignment: std.mem.Alignment,
        new_len: usize,
        return_address: usize,
    ) bool {
        const self: *FailNextAllocation = @ptrCast(@alignCast(context_ptr));
        return self.child.rawResize(memory, alignment, new_len, return_address);
    }

    fn remap(
        context_ptr: *anyopaque,
        memory: []u8,
        alignment: std.mem.Alignment,
        new_len: usize,
        return_address: usize,
    ) ?[*]u8 {
        const self: *FailNextAllocation = @ptrCast(@alignCast(context_ptr));
        return self.child.rawRemap(memory, alignment, new_len, return_address);
    }

    fn free(
        context_ptr: *anyopaque,
        memory: []u8,
        alignment: std.mem.Alignment,
        return_address: usize,
    ) void {
        const self: *FailNextAllocation = @ptrCast(@alignCast(context_ptr));
        self.child.rawFree(memory, alignment, return_address);
    }
};

fn expectGenerationFailsOnFirstAllocation(source: []const u8) !void {
    const allocator = std.testing.allocator;
    var parser = parser_mod.Parser.init(allocator, source);
    defer parser.deinit();
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);

    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse return error.HandlerNotFound;
    var failing = FailNextAllocation.init(allocator);
    var generator = PathGenerator.init(failing.allocator(), ir_view, &atoms);
    defer generator.deinit();

    try std.testing.expectError(error.OutOfMemory, generator.generate(handler_fn));
}

test "generate fails closed when import binding allocation fails" {
    try expectGenerationFailsOnFirstAllocation(
        \\import { env } from "zigttp:env";
        \\export function handler(req) {
        \\  const value = env("NAME");
        \\  return Response.json(value);
        \\}
    );
}

test "generate fails closed when handler binding allocation fails" {
    try expectGenerationFailsOnFirstAllocation(
        \\export function handler(req) {
        \\  const method = req.method;
        \\  if (method === "POST") return Response.json(true);
        \\  return Response.json(false);
        \\}
    );
}

fn expectWalkFailsOnNextAllocation(source: []const u8) !void {
    const allocator = std.testing.allocator;
    var parser = parser_mod.Parser.init(allocator, source);
    defer parser.deinit();
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse return error.HandlerNotFound;
    const function = ir_view.getFunction(handler_fn) orelse return error.HandlerNotFound;

    var failing = FailNextAllocation.init(allocator);
    failing.armed = false;
    var generator = PathGenerator.init(failing.allocator(), ir_view, &atoms);
    defer generator.deinit();
    try generator.scanImports();
    try generator.findHandlerBindings(handler_fn);
    failing.armed = true;

    try std.testing.expectError(error.OutOfMemory, generator.walkPaths(function.body));
}

test "walkPaths fails closed when local binding allocation fails" {
    try expectWalkFailsOnNextAllocation(
        \\export function handler(req) {
        \\  if (req) {
        \\    const local = req.url;
        \\    return Response.json(local);
        \\  }
        \\  return Response.json(false);
        \\}
    );
}

test "walkPaths fails closed when I/O sequence allocation fails" {
    try expectWalkFailsOnNextAllocation(
        \\import { env } from "zigttp:env";
        \\export function handler(req) {
        \\  env("NAME");
        \\  return Response.json(true);
        \\}
    );
}

test "emitTestCase keeps argument signature allocation failure conservative" {
    const allocator = std.testing.allocator;
    const source =
        \\import { env } from "zigttp:env";
        \\export function handler(req) {
        \\  env("NAME");
        \\  return Response.json(true);
        \\}
    ;
    var parser = parser_mod.Parser.init(allocator, source);
    defer parser.deinit();
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse return error.HandlerNotFound;

    var failing = FailNextAllocation.init(allocator);
    failing.armed = false;
    var generator = PathGenerator.init(failing.allocator(), ir_view, &atoms);
    defer generator.deinit();
    try generator.scanImports();
    try generator.findHandlerBindings(handler_fn);
    for (0..ir_view.nodeCount()) |idx| {
        try generator.trackModuleCall(@intCast(idx), null);
    }
    try std.testing.expectEqual(@as(usize, 1), generator.io_seq.items.len);

    // The first allocation duplicates the test name; fail the signature copy
    // immediately after it so the production fallback is exercised.
    failing.allocations_before_failure = 1;
    failing.armed = true;
    try generator.emitTestCase(200);

    try std.testing.expect(!failing.armed);
    try std.testing.expectEqual(@as(usize, 1), generator.tests.items.len);
    try std.testing.expectEqual(@as(usize, 1), generator.tests.items[0].io_stubs.items.len);
    try std.testing.expect(generator.tests.items[0].io_stubs.items[0].arg_signature == null);
}

fn generateWithAllocator(
    allocator: std.mem.Allocator,
    ir_view: IrView,
    atoms: *context.AtomTable,
    handler_fn: NodeIndex,
) !void {
    var generator = PathGenerator.init(allocator, ir_view, atoms);
    defer generator.deinit();
    try generator.generate(handler_fn);
}

test "generate cleans every fatal allocation failure" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  const ids = req.ids;
        \\  for (const id of ids) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json(true);
        \\}
    ;
    var parser = parser_mod.Parser.init(allocator, source);
    defer parser.deinit();
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    parser.setAtomTable(&atoms);
    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse return error.HandlerNotFound;

    try std.testing.checkAllAllocationFailures(
        allocator,
        generateWithAllocator,
        .{ ir_view, &atoms, handler_fn },
    );
}

fn costEnvelopeWithAllocator(
    allocator: std.mem.Allocator,
    generator: *const PathGenerator,
) !void {
    var envelope = try generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);
}

test "cost envelope cleans every allocation failure" {
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  const ids = req.ids;
        \\  for (const id of ids) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json(true);
        \\}
    ;
    var fixture = try generateFixture(std.testing.allocator, source);
    defer fixture.deinit();

    try std.testing.checkAllAllocationFailures(
        std.testing.allocator,
        costEnvelopeWithAllocator,
        .{&fixture.generator},
    );
}

fn behaviorPathsWithAllocator(
    allocator: std.mem.Allocator,
    generator: *const PathGenerator,
) !void {
    var paths = try generator.toBehaviorPaths(allocator);
    defer {
        for (paths.items) |*path| path.deinit(allocator);
        paths.deinit(allocator);
    }
}

test "behavior path conversion cleans every allocation failure" {
    const source =
        \\import { env } from "zigttp:env";
        \\export function handler(req) {
        \\  const value = env("NAME");
        \\  if (value) return Response.json(true);
        \\  return Response.json(false);
        \\}
    ;
    var fixture = try generateFixture(std.testing.allocator, source);
    defer fixture.deinit();

    try std.testing.checkAllAllocationFailures(
        std.testing.allocator,
        behaviorPathsWithAllocator,
        .{&fixture.generator},
    );
}

fn generateFixture(allocator: std.mem.Allocator, source: []const u8) !PathGeneratorFixture {
    var parser = parser_mod.Parser.init(allocator, source);
    errdefer parser.deinit();

    var atoms = context.AtomTable.init(allocator);
    errdefer atoms.deinit();
    parser.setAtomTable(&atoms);

    const root = try parser.parse();
    const ir_view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, root) orelse return error.HandlerNotFound;

    var generator = PathGenerator.init(allocator, ir_view, &atoms);
    errdefer generator.deinit();
    try generator.generate(handler_fn);

    return .{
        .parser = parser,
        .atoms = atoms,
        .generator = generator,
    };
}

test "for...of over request-derived array yields linear io multiplicity" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  const ids = req.ids;
        \\  sqlOne("setup");
        \\  for (const id of ids) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    try std.testing.expect(envelope.exhaustive);
    const total = envelope.total.linear;
    try std.testing.expectEqual(@as(u32, 1), total.coefficient);
    try std.testing.expectEqual(@as(u32, 1), total.base);
    try std.testing.expectEqual(@as(u32, 5), total.source.line);
    try std.testing.expectEqualStrings("for...of over `ids`", total.source.desc);
}

test "io call outside loops stays constant" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  sqlOne("setup");
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    try std.testing.expectEqual(contract_types.BoundClass.constant, envelope.total.class());
    try std.testing.expectEqual(@as(u32, 1), envelope.total.constant);
}

test "nested dynamic for...of degrades to unbounded" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  const ids = req.ids;
        \\  const names = req.names;
        \\  for (const id of ids) {
        \\    for (const name of names) {
        \\      sqlOne("row");
        \\    }
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    try std.testing.expectEqual(contract_types.BoundClass.unbounded, envelope.total.class());
    try std.testing.expectEqual(@as(u32, 6), envelope.total.unbounded.line);
    try std.testing.expectEqualStrings("for...of over `names`", envelope.total.unbounded.desc);
}

test "loop-aware envelope leaves generated test stubs unchanged" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  const ids = req.ids;
        \\  for (const id of ids) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    const tests = fixture.generator.getTests();
    try std.testing.expectEqual(@as(usize, 1), tests.len);
    try std.testing.expectEqual(@as(usize, 1), tests[0].io_stubs.items.len);

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);
    try std.testing.expectEqual(contract_types.BoundClass.linear, envelope.total.class());
    try std.testing.expectEqual(@as(u32, 1), envelope.total.linear.coefficient);
}

test "for...of over array literal discharges to constant" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  for (const id of [1, 2, 3]) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    try std.testing.expectEqual(contract_types.BoundClass.constant, envelope.total.class());
    try std.testing.expectEqual(@as(u32, 3), envelope.total.constant);
}

test "range(n) literal discharges loop bound" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  for (const id of range(4)) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    try std.testing.expectEqual(contract_types.BoundClass.constant, envelope.total.class());
    try std.testing.expectEqual(@as(u32, 4), envelope.total.constant);
}

test "slice(0, k) discharges loop bound" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  const ids = req.ids;
        \\  for (const id of ids.slice(0, 2)) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    try std.testing.expectEqual(contract_types.BoundClass.constant, envelope.total.class());
    try std.testing.expectEqual(@as(u32, 2), envelope.total.constant);
}

test "Object.keys of object literal discharges loop bound" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  for (const key of Object.keys({ a: 1, b: 2, c: 3 })) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    try std.testing.expectEqual(contract_types.BoundClass.constant, envelope.total.class());
    try std.testing.expectEqual(@as(u32, 3), envelope.total.constant);
}

test "sql LIMIT n discharges loop over query rows" {
    const allocator = std.testing.allocator;
    const source =
        \\import { sqlMany, sqlOne } from "zigttp:sql";
        \\export function handler(req) {
        \\  const rows = sqlMany("SELECT id FROM todos ORDER BY id LIMIT 3");
        \\  for (const row of rows) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    try std.testing.expectEqual(contract_types.BoundClass.constant, envelope.total.class());
    try std.testing.expectEqual(@as(u32, 4), envelope.total.constant);
}

test "validated field with maxItems discharges to constant ceiling" {
    const allocator = std.testing.allocator;
    const source =
        \\import { schemaCompile, validateJson } from "zigttp:validate";
        \\import { sqlOne } from "zigttp:sql";
        \\schemaCompile("payload", "{\"type\":\"object\",\"properties\":{\"items\":{\"type\":\"array\",\"maxItems\":2}}}");
        \\export function handler(req) {
        \\  const parsed = validateJson("payload", req.body);
        \\  if (!parsed.ok) return Response.json({}, { status: 400 });
        \\  for (const item of parsed.value.items) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    const sql_bound = envelope.find("sql") orelse return error.ExpectedSqlBound;
    try std.testing.expectEqual(contract_types.BoundClass.constant, sql_bound.class());
    try std.testing.expectEqual(@as(u32, 2), sql_bound.constant);
    try std.testing.expectEqual(contract_types.BoundClass.constant, envelope.total.class());
}

test "validated field without maxItems stays linear" {
    const allocator = std.testing.allocator;
    const source =
        \\import { schemaCompile, validateJson } from "zigttp:validate";
        \\import { sqlOne } from "zigttp:sql";
        \\schemaCompile("payload", "{\"type\":\"object\",\"properties\":{\"items\":{\"type\":\"array\"}}}");
        \\export function handler(req) {
        \\  const parsed = validateJson("payload", req.body);
        \\  if (!parsed.ok) return Response.json({}, { status: 400 });
        \\  for (const item of parsed.value.items) {
        \\    sqlOne("row");
        \\  }
        \\  return Response.json({});
        \\}
    ;

    var fixture = try generateFixture(allocator, source);
    defer fixture.deinit();

    var envelope = try fixture.generator.buildCostEnvelope(allocator);
    defer envelope.deinit(allocator);

    const sql_bound = envelope.find("sql") orelse return error.ExpectedSqlBound;
    try std.testing.expectEqual(contract_types.BoundClass.linear, sql_bound.class());
    try std.testing.expectEqual(@as(u32, 1), sql_bound.linear.coefficient);
    try std.testing.expectEqualStrings("for...of over `items`", sql_bound.linear.source.desc);
}
