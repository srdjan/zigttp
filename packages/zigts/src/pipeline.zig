//! Typed pipeline phases for the zigts compiler (WS2).
//!
//! Threads three phase wrappers across compilation:
//!
//!   ParsedModule    — IR is built; nothing has been checked yet.
//!   ResolvedModule  — Boolean and (optional) type checks have run.
//!   CheckedModule   — Handler verification + flow analysis have run.
//!
//! Each wrapper owns its pass structs by value (so a single `deinit()` cleans up
//! everything the wrapper added) and exposes diagnostics through accessors.
//! `CheckedModule` borrows `*const ResolvedModule` because `HandlerVerifier`
//! holds a pointer back to the `TypeChecker` and the borrow keeps that target
//! address stable in caller storage.
//!
//! Pass-ordering invariants ("type-check before flow-check", "resolve before
//! check") are encoded in the function signatures: `check` only accepts a
//! `ResolvedModule`, never a `ParsedModule`. Construction of `ParsedModule` is
//! still permissive in this phase (`fromExisting`) — gating that constructor is
//! WS5 work. A fourth `LoweredModule` phase is intentionally not added until
//! `precompile.zig`'s codegen-bearing orchestrator migrates; introducing the
//! type before it has a real producer would ship dead API.
//!
//! See: /Users/srdjans/.claude/plans/study-following-doc-document-luminous-fog.md

const std = @import("std");

const parser_mod = @import("parser/root.zig");
const bool_checker_mod = @import("bool_checker.zig");
const type_checker_mod = @import("type_checker.zig");
const strict_checker_mod = @import("strict_checker.zig");
const handler_verifier_mod = @import("handler_verifier.zig");
const flow_checker_mod = @import("flow_checker.zig");
const context_mod = @import("context.zig");
const type_env_mod = @import("type_env.zig");
const type_pool_mod = @import("type_pool.zig");
const type_map_mod = @import("type_map.zig");
const service_types_mod = @import("service_types.zig");
const modules_mod = @import("modules/root.zig");
const ir_mod = @import("parser/ir.zig");

const NodeIndex = ir_mod.NodeIndex;
const IrView = ir_mod.IrView;
const BoolChecker = bool_checker_mod.BoolChecker;
const TypeChecker = type_checker_mod.TypeChecker;
const StrictChecker = strict_checker_mod.StrictChecker;
const HandlerVerifier = handler_verifier_mod.HandlerVerifier;
const FlowChecker = flow_checker_mod.FlowChecker;
const AtomTable = context_mod.AtomTable;
const TypeEnv = type_env_mod.TypeEnv;
const TypePool = type_pool_mod.TypePool;
const TypeMap = type_map_mod.TypeMap;
const ServiceTypeContext = service_types_mod.ServiceTypeContext;

// ---------------------------------------------------------------------------
// Phase 1: Parsed
// ---------------------------------------------------------------------------

pub const ParsedModule = struct {
    ir_view: IrView,
    root: NodeIndex,
    atoms: ?*AtomTable,

    /// Adopt an already-parsed IR. Transitional: tighter gating belongs to WS5.
    pub fn fromExisting(ir_view: IrView, root: NodeIndex, atoms: ?*AtomTable) ParsedModule {
        return .{ .ir_view = ir_view, .root = root, .atoms = atoms };
    }
};

// ---------------------------------------------------------------------------
// Phase 2: Resolved (boolean + optional type checks)
// ---------------------------------------------------------------------------

pub const ResolveOptions = struct {
    /// When non-null, runs the TypeChecker as part of resolution. The pointed-to
    /// TypeEnv must outlive the returned ResolvedModule.
    type_env: ?*TypeEnv = null,
    service_type_context: ?*const ServiceTypeContext = null,
    /// Default-on strict ZigTS profile (ZTS6xx). Type-directed rules run when
    /// type context exists; syntax/profile rules still run for untyped sources.
    strict: bool = true,
};

pub const ResolvedModule = struct {
    parsed: ParsedModule,
    bool_checker: BoolChecker,
    type_checker: ?TypeChecker,
    strict_checker: ?StrictChecker,
    bool_error_count: u32,
    type_error_count: u32,
    strict_error_count: u32,

    pub fn deinit(self: *ResolvedModule) void {
        self.bool_checker.deinit();
        if (self.strict_checker) |*sc| sc.deinit();
        if (self.type_checker) |*tc| tc.deinit();
    }

    pub fn boolDiagnostics(self: *const ResolvedModule) []const bool_checker_mod.Diagnostic {
        return self.bool_checker.getDiagnostics();
    }

    pub fn typeDiagnostics(self: *const ResolvedModule) []const type_checker_mod.Diagnostic {
        if (self.type_checker) |*tc| return tc.getDiagnostics();
        return &.{};
    }

    pub fn strictDiagnostics(self: *const ResolvedModule) []const strict_checker_mod.Diagnostic {
        if (self.strict_checker) |*sc| return sc.getDiagnostics();
        return &.{};
    }

    pub fn formatBoolDiagnostics(
        self: *const ResolvedModule,
        source: []const u8,
        writer: anytype,
    ) !void {
        try self.bool_checker.formatDiagnostics(source, writer);
    }

    pub fn formatTypeDiagnostics(
        self: *const ResolvedModule,
        source: []const u8,
        writer: anytype,
    ) !void {
        if (self.type_checker) |*tc| try tc.formatDiagnostics(source, writer);
    }

    pub fn formatStrictDiagnostics(
        self: *const ResolvedModule,
        source: []const u8,
        writer: anytype,
    ) !void {
        if (self.strict_checker) |*sc| try sc.formatDiagnostics(source, writer);
    }
};

pub fn resolve(
    allocator: std.mem.Allocator,
    parsed: ParsedModule,
    opts: ResolveOptions,
) !ResolvedModule {
    var bool_checker = BoolChecker.init(allocator, parsed.ir_view, parsed.atoms);
    errdefer bool_checker.deinit();
    const bool_errors = try bool_checker.check(parsed.root);

    var type_checker_opt: ?TypeChecker = null;
    var type_errors: u32 = 0;
    if (opts.type_env) |env| {
        var tc = TypeChecker.init(
            allocator,
            parsed.ir_view,
            parsed.atoms,
            env,
            opts.service_type_context,
        );
        errdefer tc.deinit();
        type_errors = try tc.check(parsed.root);
        type_checker_opt = tc;
    }

    var strict_checker_opt: ?StrictChecker = null;
    var strict_errors: u32 = 0;
    if (opts.strict) {
        const env_ptr: ?*const TypeEnv = if (type_checker_opt) |*tc| tc.env else null;
        const tc_ptr: ?*const TypeChecker = if (type_checker_opt) |*tc| tc else null;
        var sc = StrictChecker.init(
            allocator,
            parsed.ir_view,
            parsed.atoms,
            env_ptr,
            tc_ptr,
        );
        errdefer sc.deinit();
        strict_errors = try sc.check(parsed.root);
        if (type_checker_opt) |*tc| try tc.ensureHealthy();
        // The borrowed TypeEnv/TypeChecker pointers were stack-local; releasing
        // them via seal() before the ResolvedModule is moved out keeps any
        // stray future use a clean nullopt instead of UB.
        sc.seal();
        strict_checker_opt = sc;
    }

    return .{
        .parsed = parsed,
        .bool_checker = bool_checker,
        .type_checker = type_checker_opt,
        .strict_checker = strict_checker_opt,
        .bool_error_count = bool_errors,
        .type_error_count = type_errors,
        .strict_error_count = strict_errors,
    };
}

// ---------------------------------------------------------------------------
// Phase 3: Checked (handler verification + flow analysis)
// ---------------------------------------------------------------------------

pub const CheckedModule = struct {
    resolved: *const ResolvedModule,
    verifier: HandlerVerifier,
    flow_checker: FlowChecker,
    verifier_error_count: u32,
    flow_error_count: u32,

    pub fn deinit(self: *CheckedModule) void {
        self.flow_checker.deinit();
        self.verifier.deinit();
        // Caller owns `resolved`; don't deinit it here.
    }

    pub fn verifierDiagnostics(self: *const CheckedModule) []const handler_verifier_mod.Diagnostic {
        return self.verifier.getDiagnostics();
    }

    pub fn flowDiagnostics(self: *const CheckedModule) []const flow_checker_mod.Diagnostic {
        return self.flow_checker.getDiagnostics();
    }

    pub fn defendedPaths(self: *const CheckedModule) []const flow_checker_mod.DefendedPath {
        return self.flow_checker.getDefendedPaths();
    }
};

pub fn check(
    allocator: std.mem.Allocator,
    resolved: *const ResolvedModule,
    handler_func: NodeIndex,
) !CheckedModule {
    const tc_ptr: ?*const TypeChecker = if (resolved.type_checker) |*tc| tc else null;
    const env_ptr: ?*const TypeEnv = if (resolved.type_checker) |*tc| tc.env else null;

    var verifier = HandlerVerifier.init(
        allocator,
        resolved.parsed.ir_view,
        resolved.parsed.atoms,
        env_ptr,
        tc_ptr,
    );
    errdefer verifier.deinit();
    const verifier_errors = try verifier.verify(handler_func);
    if (tc_ptr) |tc| try tc.ensureHealthy();

    var flow = FlowChecker.init(
        allocator,
        resolved.parsed.ir_view,
        resolved.parsed.atoms,
    );
    errdefer flow.deinit();
    const flow_errors = try flow.check(handler_func);

    return .{
        .resolved = resolved,
        .verifier = verifier,
        .flow_checker = flow,
        .verifier_error_count = verifier_errors,
        .flow_error_count = flow_errors,
    };
}

// ---------------------------------------------------------------------------
// TypeEnvStorage — caller-owned bundle of TypePool + TypeEnv with a stable
// address. `TypeEnv.init` captures `*TypePool`, so the pool must outlive the
// env and live at a fixed address; bundling them into one struct guarantees
// both. `init` requires a pointer-receiver because `populateFromTypeMap` and
// the env's pool reference both need stable addresses.
// ---------------------------------------------------------------------------

pub const TypeEnvStorage = struct {
    pool: TypePool = undefined,
    env: TypeEnv = undefined,
    initialized: bool = false,

    pub fn init(
        self: *TypeEnvStorage,
        allocator: std.mem.Allocator,
        type_map: *const TypeMap,
    ) !void {
        self.pool = TypePool.init(allocator);
        errdefer self.pool.deinit(allocator);
        try self.pool.ensureHealthy();
        self.env = TypeEnv.init(allocator, &self.pool);
        errdefer self.env.deinit();
        try self.pool.ensureHealthy();
        modules_mod.populateModuleTypes(&self.env, &self.pool, allocator);
        try self.pool.ensureHealthy();
        self.env.populateFromTypeMap(type_map);
        try self.finishInitialization();
    }

    fn finishInitialization(self: *TypeEnvStorage) !void {
        try self.pool.ensureHealthy();
        self.initialized = true;
    }

    pub fn deinit(self: *TypeEnvStorage, allocator: std.mem.Allocator) void {
        if (!self.initialized) return;
        self.env.deinit();
        self.pool.deinit(allocator);
        self.initialized = false;
    }

    pub fn envPtr(self: *TypeEnvStorage) ?*TypeEnv {
        return if (self.initialized) &self.env else null;
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;
const JsParser = parser_mod.JsParser;

fn parseSourceForTest(allocator: std.mem.Allocator, source: []const u8) !struct {
    js_parser: JsParser,
    root: NodeIndex,
} {
    var js_parser = JsParser.init(allocator, source);
    errdefer js_parser.deinit();
    const root = try js_parser.parse();
    return .{ .js_parser = js_parser, .root = root };
}

test "ParsedModule.fromExisting wraps an IR view" {
    const allocator = testing.allocator;
    var parsed_state = try parseSourceForTest(allocator, "const x = true;\n");
    defer parsed_state.js_parser.deinit();

    const view = IrView.fromIRStore(&parsed_state.js_parser.nodes, &parsed_state.js_parser.constants);
    const parsed = ParsedModule.fromExisting(view, parsed_state.root, null);
    try testing.expect(view.isValid(parsed.root));
}

test "pipeline.resolve does not emit a module after analysis allocation failure" {
    const allocator = testing.allocator;
    var parsed_state = try parseSourceForTest(allocator, "const enabled = true;\n");
    defer parsed_state.js_parser.deinit();
    const view = IrView.fromIRStore(&parsed_state.js_parser.nodes, &parsed_state.js_parser.constants);
    const parsed = ParsedModule.fromExisting(view, parsed_state.root, null);

    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    try testing.expectError(error.OutOfMemory, resolve(failing.allocator(), parsed, .{ .strict = false }));
}

test "TypeEnvStorage rejects a poisoned TypePool" {
    const allocator = testing.allocator;
    var type_map = TypeMap.init("");
    defer type_map.deinit(allocator);

    var storage: TypeEnvStorage = .{};
    defer storage.deinit(allocator);
    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });

    try testing.expectError(error.OutOfMemory, storage.init(failing.allocator(), &type_map));
    try testing.expect(storage.envPtr() == null);
}

test "TypeEnvStorage rejects a late poisoned TypePool" {
    const allocator = testing.allocator;
    var type_map = TypeMap.init("");
    defer type_map.deinit(allocator);

    var storage: TypeEnvStorage = .{};
    storage.pool = TypePool.init(allocator);
    defer storage.pool.deinit(allocator);
    try storage.pool.ensureHealthy();
    storage.env = TypeEnv.init(allocator, &storage.pool);
    defer storage.env.deinit();
    try storage.pool.ensureHealthy();
    modules_mod.populateModuleTypes(&storage.env, &storage.pool, allocator);
    try storage.pool.ensureHealthy();
    storage.env.populateFromTypeMap(&type_map);
    try storage.pool.ensureHealthy();

    const record = storage.pool.addRecord(allocator, &.{.{
        .name_start = 0,
        .name_len = 0,
        .type_idx = storage.pool.idx_string,
        .optional = false,
    }});
    try storage.pool.ensureHealthy();
    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    _ = storage.pool.makeReadonly(failing.allocator(), record);

    try testing.expectError(error.OutOfMemory, storage.finishInitialization());
    try testing.expect(storage.envPtr() == null);
}

test "pipeline.check does not emit a module after verifier allocation failure" {
    const allocator = testing.allocator;
    var parsed_state = try parseSourceForTest(allocator, "function handler(req) {}\n");
    defer parsed_state.js_parser.deinit();
    const view = IrView.fromIRStore(&parsed_state.js_parser.nodes, &parsed_state.js_parser.constants);
    const parsed = ParsedModule.fromExisting(view, parsed_state.root, null);
    var resolved = try resolve(allocator, parsed, .{ .strict = false });
    defer resolved.deinit();
    const handler_func = handler_verifier_mod.findHandlerFunction(view, parsed_state.root) orelse
        return error.HandlerNotFound;

    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    try testing.expectError(error.OutOfMemory, check(failing.allocator(), &resolved, handler_func));
}

test "pipeline.check rejects a TypePool poisoned after resolve" {
    const allocator = testing.allocator;
    var parsed_state = try parseSourceForTest(allocator, "function handler(req) { return Response.text('ok'); }\n");
    defer parsed_state.js_parser.deinit();
    const view = IrView.fromIRStore(&parsed_state.js_parser.nodes, &parsed_state.js_parser.constants);
    const parsed = ParsedModule.fromExisting(view, parsed_state.root, null);

    var type_map = TypeMap.init("");
    defer type_map.deinit(allocator);
    var storage: TypeEnvStorage = .{};
    try storage.init(allocator, &type_map);
    defer storage.deinit(allocator);

    var resolved = try resolve(allocator, parsed, .{ .type_env = storage.envPtr(), .strict = false });
    defer resolved.deinit();
    const handler_func = handler_verifier_mod.findHandlerFunction(view, parsed_state.root) orelse
        return error.HandlerNotFound;

    const record = storage.pool.addRecord(allocator, &.{.{
        .name_start = 0,
        .name_len = 0,
        .type_idx = storage.pool.idx_string,
        .optional = false,
    }});
    try storage.pool.ensureHealthy();
    var failing = std.testing.FailingAllocator.init(allocator, .{ .fail_index = 0 });
    _ = storage.pool.makeReadonly(failing.allocator(), record);

    try testing.expectError(error.OutOfMemory, check(allocator, &resolved, handler_func));
}

test "pipeline.resolve runs BoolChecker on clean source" {
    const allocator = testing.allocator;
    var parsed_state = try parseSourceForTest(allocator, "const x = true;\nconst y = !x;\n");
    defer parsed_state.js_parser.deinit();

    const view = IrView.fromIRStore(&parsed_state.js_parser.nodes, &parsed_state.js_parser.constants);
    const parsed = ParsedModule.fromExisting(view, parsed_state.root, null);
    var resolved = try resolve(allocator, parsed, .{});
    defer resolved.deinit();

    try testing.expectEqual(@as(u32, 0), resolved.bool_error_count);
    try testing.expectEqual(@as(u32, 0), resolved.type_error_count);
    try testing.expectEqual(@as(usize, 0), resolved.typeDiagnostics().len);
}

test "pipeline.resolve flags pointless object truthy condition" {
    const allocator = testing.allocator;
    // Mirror bool_checker.zig:2258 — object literal in `if` is always truthy.
    const source: []const u8 = "if ({}) { let x = 1; }";

    var js_parser = JsParser.init(allocator, source);
    defer js_parser.deinit();
    const root = try js_parser.parse();
    const ir_view = IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);

    const parsed = ParsedModule.fromExisting(ir_view, root, null);
    var resolved = try resolve(allocator, parsed, .{});
    defer resolved.deinit();

    try testing.expect(resolved.bool_error_count > 0);
    try testing.expect(resolved.boolDiagnostics().len > 0);
}

test "pipeline.resolve runs strict checker without type context" {
    const allocator = testing.allocator;
    const source: []const u8 = "function handler(req) { let x = 1; return Response.json({x}); }";

    var parsed_state = try parseSourceForTest(allocator, source);
    defer parsed_state.js_parser.deinit();

    const view = IrView.fromIRStore(&parsed_state.js_parser.nodes, &parsed_state.js_parser.constants);
    const parsed = ParsedModule.fromExisting(view, parsed_state.root, null);
    var resolved = try resolve(allocator, parsed, .{ .type_env = null });
    defer resolved.deinit();

    try testing.expect(resolved.strict_checker != null);
    try testing.expect(resolved.strict_error_count > 0);
    var saw_avoidable_let = false;
    for (resolved.strictDiagnostics()) |diag| {
        if (diag.kind == .avoidable_let) saw_avoidable_let = true;
    }
    try testing.expect(saw_avoidable_let);
}
