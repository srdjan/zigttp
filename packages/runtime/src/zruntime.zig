//! Native Zig Runtime for zigttp
//!
//! Pure Zig implementation using zts - no C dependencies.
//! Designed for FaaS with per-request isolation and fast cold starts.

const std = @import("std");
const builtin = @import("builtin");
const compat = @import("zigts").compat;
const ascii = std.ascii;

// Import zts module
const zq = @import("zigts");
const embedded_handler = @import("embedded_handler");
const durable_store_mod = @import("durable_store.zig");
const durable_fetch = @import("durable_fetch.zig");
const http_parser = @import("http_parser.zig");
const contract_runtime = @import("contract_runtime.zig");
pub const websocket_codec = @import("websocket_codec.zig");

// Bytecode caching for faster cold starts
const bytecode_cache = zq.bytecode_cache;

// HTTP protocol types (shared with server layer)
const http_types = @import("http_types.zig");
const websocket_pool = @import("websocket_pool.zig");
pub const QueryParam = http_types.QueryParam;
pub const HttpRequestView = http_types.HttpRequestView;
pub const HttpRequestOwned = http_types.HttpRequestOwned;
pub const HttpHeader = http_types.HttpHeader;
pub const ResponseHeader = http_types.ResponseHeader;
pub const HttpResponse = http_types.HttpResponse;

// ============================================================================
// Runtime Configuration
// ============================================================================

pub const RuntimeConfig = struct {
    /// Memory limit per context in bytes (0 = no limit)
    memory_limit: usize = 0,

    /// GC nursery size
    nursery_size: usize = 64 * 1024,

    /// Use hybrid arena allocation for ephemeral values (default: true)
    /// Set to false only if you need GC-managed strings (note: has leak issues)
    use_hybrid_allocation: bool = true,

    /// Arena size when hybrid allocation is enabled (default 1MB)
    arena_size: usize = 1024 * 1024,

    /// Enforce arena escape checking (default: true for HTTP handlers)
    /// Set to false for scripts/benchmarks where arena lifetime matches script lifetime
    enforce_arena_escape: bool = true,

    /// JIT compilation policy (default: lazy)
    /// - .disabled: Never JIT compile (fastest cold start, pure interpreter)
    /// - .lazy: JIT after threshold (default 100 calls, balanced)
    /// - .eager: Lower threshold (25 calls, faster warmup)
    jit_policy: ?zq.interpreter.JitPolicy = null,

    /// Override JIT compilation threshold (null = use policy default)
    jit_threshold: ?u32 = null,

    /// Enable native outbound HTTP bridge (`httpRequest(json)`).
    outbound_http_enabled: bool = false,

    /// Optional exact host allowlist for outbound HTTP requests.
    /// Example: `127.0.0.1` or `localhost`.
    outbound_allow_host: ?[]const u8 = null,

    /// Maximum response bytes captured by `httpRequest`.
    outbound_max_response_bytes: usize = 1024 * 1024,

    /// Connect timeout in milliseconds for outbound HTTP requests.
    outbound_timeout_ms: u32 = 10_000,

    /// SQLite database path for zigttp:sql.
    sqlite_path: ?[]const u8 = null,

    /// Trace output file path. When set, all handler I/O is recorded as JSONL.
    trace_file_path: ?[]const u8 = null,

    /// Replay input file path. When set, replays recorded traces instead of serving.
    replay_file_path: ?[]const u8 = null,

    /// Test file path. When set, runs declarative handler tests instead of serving.
    test_file_path: ?[]const u8 = null,

    /// Durable execution oplog directory. When set, each request gets a
    /// Directory backing explicit zigttp:durable runs.
    /// Incomplete oplogs are replayed on startup; completed ones are kept so
    /// duplicate durable keys can return the recorded response.
    durable_oplog_dir: ?[]const u8 = null,

    /// System registry used by zigttp:service for named internal calls.
    system_config_path: ?[]const u8 = null,
};

fn openTraceFile(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    return zq.file_io.openAppend(allocator, path);
}

/// Open an oplog file for write-ahead durable execution.
/// Creates the file (or truncates any stale content).
fn openOplogFile(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        0o644,
    ) catch return error.FileOpenFailed;

    return fd;
}

/// Open an existing oplog for append during replay/resume.
fn openOplogAppendFile(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .APPEND = true },
        0,
    ) catch return error.FileOpenFailed;

    return fd;
}

fn applyRuntimeConfig(ctx: *zq.Context, gc_state: *zq.GC, heap_state: *zq.heap.Heap, config: RuntimeConfig) void {
    // Configure arena escape checking (disabled for scripts/benchmarks)
    ctx.enforce_arena_escape = config.enforce_arena_escape;

    // Increase major GC threshold for FaaS workloads.
    // Default 10,000 triggers too frequently during request handling.
    // Higher threshold reduces GC pause frequency at cost of memory.
    gc_state.setMajorGCThreshold(50_000);

    if (config.memory_limit > 0) {
        gc_state.setMemoryLimit(config.memory_limit);
        heap_state.setMemoryLimit(config.memory_limit);
        if (ctx.hybrid) |h| {
            h.setMemoryLimit(config.memory_limit);
        }
    }

    // Apply JIT policy from config (global settings)
    if (config.jit_policy) |policy| {
        zq.interpreter.setJitPolicy(policy);
    }
    if (config.jit_threshold) |threshold| {
        zq.interpreter.setJitThreshold(threshold);
    }
}

fn applyEmbeddedCapabilityPolicy(ctx: *zq.Context) void {
    ctx.capability_policy = embedded_handler.capability_policy;
}

// ============================================================================
// File reading for module graph (POSIX, no async I/O dependency)
// ============================================================================

const readFilePosixForGraph = zq.file_io.readFileForModuleGraph;
const parseHeadersFromJson = @import("trace_helpers.zig").parseHeadersFromJson;

// ============================================================================
// Thread-local Runtime for native function callbacks
// ============================================================================

/// Thread-local reference to current runtime for use in native function callbacks
/// (e.g., renderToString needs to call component functions)
threadlocal var current_runtime: ?*Runtime = null;

const AotOverrideFn = *const fn (ctx: *zq.Context, args: []const zq.JSValue) anyerror!zq.JSValue;
threadlocal var aot_override: ?AotOverrideFn = null;

fn setAotOverrideForTest(callback: ?AotOverrideFn) void {
    if (builtin.is_test) {
        aot_override = callback;
    }
}

// ============================================================================
// Runtime Instance
// ============================================================================

pub const Runtime = struct {
    allocator: std.mem.Allocator,
    ctx: *zq.Context,
    gc_state: *zq.GC,
    heap: *zq.heap.Heap,
    interpreter: zq.Interpreter,
    strings: *zq.StringTable,
    owned_strings: ?zq.StringTable,
    handler_atom: ?zq.Atom,
    cached_handler_obj: ?*zq.JSObject,
    cached_handler_arg_count: u8,
    cached_dispatch: ?*const zq.bytecode.PatternDispatchTable,
    config: RuntimeConfig,
    outbound_io_backend: ?std.Io.Threaded,
    owns_resources: bool,
    active_request_id: std.atomic.Value(u64),
    last_request_body_len: usize,
    request_prototype: ?*zq.JSObject,
    response_prototype: ?*zq.JSObject,
    headers_prototype: ?*zq.JSObject,
    consumed_body_objects: std.AutoHashMapUnmanaged(*zq.JSObject, void),
    // Trace recording support
    trace_file: ?std.c.fd_t,
    trace_mutex: ?*zq.trace.TraceMutex,
    trace_recorder: ?*zq.TraceRecorder,
    active_request: ?HttpRequestView,
    active_durable_run: ?ActiveDurableRun,
    pending_durable_recovery: ?PendingDurableRecovery,
    // Hybrid allocation support
    arena_state: ?*zq.arena.Arena,
    hybrid_state: ?*zq.arena.HybridAllocator,
    /// WebSocket connection pool pointer, installed lazily by the frame
    /// loop on first dispatch. Remains null on handlers that never
    /// accept a WebSocket upgrade. See `installWebSocketModuleState`.
    ws_pool_ref: ?*websocket_pool.Pool = null,

    const Self = @This();

    const PendingDurableWait = union(enum) {
        timer: i64,
        signal: []const u8,

        fn deinit(self: *PendingDurableWait, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .signal => |name| allocator.free(name),
                .timer => {},
            }
        }
    };

    const ActiveDurableRun = struct {
        key: []const u8,
        oplog_path: []const u8,
        oplog_fd: std.c.fd_t,
        state: *zq.trace.DurableState,
        owned_events: ?[]const zq.trace.DurableEvent = null,
        source_snapshot: ?[]u8 = null,
        step_depth: u32 = 0,
        pending_wait: ?PendingDurableWait = null,

        fn setPendingTimer(self: *ActiveDurableRun, allocator: std.mem.Allocator, until_ms: i64) !void {
            if (self.pending_wait) |*wait| {
                wait.deinit(allocator);
            }
            self.pending_wait = .{ .timer = until_ms };
        }

        fn setPendingSignal(self: *ActiveDurableRun, allocator: std.mem.Allocator, name: []const u8) !void {
            if (self.pending_wait) |*wait| {
                wait.deinit(allocator);
            }
            self.pending_wait = .{ .signal = try allocator.dupe(u8, name) };
        }

        fn deinit(self: *ActiveDurableRun, allocator: std.mem.Allocator) void {
            allocator.free(self.key);
            allocator.free(self.oplog_path);
            if (self.owned_events) |events| allocator.free(events);
            if (self.source_snapshot) |source| allocator.free(source);
            if (self.pending_wait) |*wait| wait.deinit(allocator);
            self.state.deinit();
            allocator.destroy(self.state);
            std.Io.Threaded.closeFd(self.oplog_fd);
        }
    };

    const PendingDurableRecovery = struct {
        key: []const u8,
        oplog_path: []const u8,
        events: []const zq.trace.DurableEvent,
    };

    pub fn init(allocator: std.mem.Allocator, config: RuntimeConfig) !*Self {
        const self = try allocator.create(Self);
        errdefer allocator.destroy(self);

        // Initialize GC
        const gc_state = try allocator.create(zq.GC);
        errdefer allocator.destroy(gc_state);
        gc_state.* = try zq.GC.init(allocator, .{
            .nursery_size = config.nursery_size,
        });
        errdefer gc_state.deinit();

        // Initialize heap for size-class allocation and wire up to GC
        const heap_state = try allocator.create(zq.heap.Heap);
        errdefer allocator.destroy(heap_state);
        heap_state.* = zq.heap.Heap.init(allocator, .{});
        gc_state.setHeap(heap_state);

        // Initialize context
        const ctx = try zq.Context.init(allocator, gc_state, .{});
        errdefer ctx.deinit();

        applyRuntimeConfig(ctx, gc_state, heap_state, config);
        applyEmbeddedCapabilityPolicy(ctx);

        // Install core JS builtins (Array.prototype, Object, Math, JSON, etc.)
        try zq.builtins.initBuiltins(ctx);

        // Initialize hybrid allocation if enabled
        var arena_state: ?*zq.arena.Arena = null;
        var hybrid_state: ?*zq.arena.HybridAllocator = null;

        if (config.use_hybrid_allocation) {
            arena_state = try allocator.create(zq.arena.Arena);
            errdefer allocator.destroy(arena_state.?);
            arena_state.?.* = try zq.arena.Arena.init(allocator, .{ .size = config.arena_size });
            errdefer arena_state.?.deinit();

            hybrid_state = try allocator.create(zq.arena.HybridAllocator);
            errdefer allocator.destroy(hybrid_state.?);
            hybrid_state.?.* = .{
                .persistent = allocator,
                .arena = arena_state.?,
            };
            ctx.setHybridAllocator(hybrid_state.?);
            if (config.memory_limit > 0) {
                hybrid_state.?.setMemoryLimit(config.memory_limit);
            }
        }

        const interp = zq.Interpreter.init(ctx);

        self.* = .{
            .allocator = allocator,
            .ctx = ctx,
            .gc_state = gc_state,
            .heap = heap_state,
            .interpreter = interp,
            .strings = undefined,
            .owned_strings = zq.StringTable.init(allocator),
            .handler_atom = null,
            .cached_handler_obj = null,
            .cached_handler_arg_count = 1,
            .cached_dispatch = null,
            .config = config,
            .outbound_io_backend = if (config.outbound_http_enabled)
                std.Io.Threaded.init(allocator, .{ .environ = .empty })
            else
                null,
            .owns_resources = true,
            .active_request_id = std.atomic.Value(u64).init(0),
            .last_request_body_len = 0,
            .request_prototype = null,
            .response_prototype = null,
            .headers_prototype = null,
            .consumed_body_objects = .{},
            .trace_file = null,
            .trace_mutex = null,
            .trace_recorder = null,
            .active_request = null,
            .active_durable_run = null,
            .pending_durable_recovery = null,
            .arena_state = arena_state,
            .hybrid_state = hybrid_state,
            .ws_pool_ref = null,
        };
        self.strings = &self.owned_strings.?;
        errdefer self.owned_strings.?.deinit();

        // Open trace file if configured
        if (config.trace_file_path) |trace_path| {
            self.trace_file = openTraceFile(allocator, trace_path) catch |err| {
                std.log.err("Failed to open trace file '{s}': {}", .{ trace_path, err });
                return err;
            };
            const mutex = try allocator.create(zq.trace.TraceMutex);
            mutex.* = .{};
            self.trace_mutex = mutex;
        }

        // Install built-in bindings
        try self.installBindings();

        return self;
    }

    /// Initialize a runtime wrapper on top of a pooled zts runtime.
    /// The pooled runtime owns ctx/gc/heap; this wrapper owns only its own state.
    pub fn initFromPool(pool_rt: *zq.LockFreePool.Runtime, config: RuntimeConfig) !*Self {
        const allocator = pool_rt.ctx.allocator;
        const self = try allocator.create(Self);
        errdefer allocator.destroy(self);

        const interp = zq.Interpreter.init(pool_rt.ctx);

        self.* = .{
            .allocator = allocator,
            .ctx = pool_rt.ctx,
            .gc_state = pool_rt.gc_state,
            .heap = pool_rt.heap_state,
            .interpreter = interp,
            .strings = &pool_rt.strings,
            .owned_strings = null,
            .handler_atom = null,
            .cached_handler_obj = null,
            .cached_handler_arg_count = 1,
            .cached_dispatch = null,
            .config = config,
            .outbound_io_backend = if (config.outbound_http_enabled)
                std.Io.Threaded.init(allocator, .{ .environ = .empty })
            else
                null,
            .owns_resources = false,
            .active_request_id = std.atomic.Value(u64).init(0),
            .last_request_body_len = 0,
            .request_prototype = null,
            .response_prototype = null,
            .headers_prototype = null,
            .consumed_body_objects = .{},
            .trace_file = null,
            .trace_mutex = null,
            .trace_recorder = null,
            .active_request = null,
            .active_durable_run = null,
            .pending_durable_recovery = null,
            // Pool runtimes manage their own hybrid allocation
            .arena_state = null,
            .hybrid_state = null,
            .ws_pool_ref = null,
        };

        applyRuntimeConfig(pool_rt.ctx, pool_rt.gc_state, pool_rt.heap_state, config);
        applyEmbeddedCapabilityPolicy(pool_rt.ctx);

        // Install core JS builtins if the pooled runtime hasn't already done so.
        if (pool_rt.ctx.builtin_objects.items.len == 0) {
            try zq.builtins.initBuiltins(pool_rt.ctx);
        }
        try self.installBindings();

        return self;
    }

    pub fn deinit(self: *Self) void {
        if (self.owns_resources) {
            // Context teardown walks builtin objects and bytecode constants that may
            // still reference interned unique strings from this runtime.
            self.ctx.deinit();
            self.gc_state.deinit();
            self.heap.deinit();
            // Clean up hybrid allocation state after the context has released any
            // arena-backed request allocations it still knows about.
            if (self.arena_state) |a| {
                a.deinit();
                self.allocator.destroy(a);
            }
            if (self.hybrid_state) |h| {
                self.allocator.destroy(h);
            }
            self.allocator.destroy(self.gc_state);
            self.allocator.destroy(self.heap);
            if (self.trace_file) |fd| std.Io.Threaded.closeFd(fd);
            if (self.trace_mutex) |m| self.allocator.destroy(m);
        }
        self.consumed_body_objects.deinit(self.allocator);
        if (self.outbound_io_backend) |*io_backend| {
            io_backend.deinit();
        }
        if (self.trace_recorder) |rec| {
            rec.deinit();
            self.allocator.destroy(rec);
        }
        if (self.active_durable_run) |*run| {
            run.deinit(self.allocator);
        }
        if (self.owned_strings) |*owned_strings| {
            owned_strings.deinit();
        }
        self.allocator.destroy(self);
    }

    /// Install native API bindings (console, etc.)
    /// Note: Response is already set up by initBuiltins() with constructor and static methods
    fn installBindings(self: *Self) !void {
        // Use predefined handler atom
        self.handler_atom = zq.Atom.handler;
        self.cached_handler_obj = null;
        self.cached_handler_arg_count = 1;
        self.cached_dispatch = null;

        try self.installHttpConstructors();
        // Install console object
        try self.installConsole();
        try self.installHttpRequest();
        try self.installFetchSync();

        // Register all virtual module native functions eagerly.
        // This ensures import bindings resolve correctly whether the handler
        // was parsed or loaded from bytecode cache.
        try self.installVirtualModules();
        try self.installScopeModuleState();
        try self.installSqlModuleState();
        if (self.config.system_config_path) |_| {
            try self.installServiceModuleState();
        }
        try self.installFetchModuleState();

        // Install io module callbacks for parallel/race (requires outbound HTTP)
        if (self.config.outbound_http_enabled) {
            try self.installIoModuleState();
        }
        if (self.config.durable_oplog_dir != null) {
            try self.installDurableModuleState();
        }

        // Note: Response, h(), renderToString(), Fragment are all set up by initBuiltins()
        // Don't re-register them here as it would overwrite the proper constructor
    }

    fn installHttpConstructors(self: *Self) !void {
        const root_class_idx = self.ctx.root_class_idx;
        const pool = self.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

        const request_proto = try zq.JSObject.create(self.allocator, root_class_idx, null, pool);
        try self.addDynamicMethod(request_proto, "text", bodyTextNative, 0);
        try self.addDynamicMethod(request_proto, "json", bodyJsonNative, 0);
        try self.ctx.builtin_objects.append(self.allocator, request_proto);

        const response_proto = try zq.JSObject.create(self.allocator, root_class_idx, null, pool);
        try self.addDynamicMethod(response_proto, "text", bodyTextNative, 0);
        try self.addDynamicMethod(response_proto, "json", bodyJsonNative, 0);
        try self.ctx.builtin_objects.append(self.allocator, response_proto);

        const headers_proto = try zq.JSObject.create(self.allocator, root_class_idx, null, pool);
        try self.addDynamicMethod(headers_proto, "get", headersGetNative, 1);
        try self.addDynamicMethod(headers_proto, "set", headersSetNative, 2);
        try self.addDynamicMethod(headers_proto, "append", headersAppendNative, 2);
        try self.addDynamicMethod(headers_proto, "has", headersHasNative, 1);
        try self.addDynamicMethod(headers_proto, "delete", headersDeleteNative, 1);
        try self.ctx.builtin_objects.append(self.allocator, headers_proto);

        const headers_ctor_atom = try self.ctx.atoms.intern("Headers");
        const headers_ctor = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            headersConstructorNative,
            headers_ctor_atom,
            1,
        );
        try self.ctx.setPropertyChecked(headers_ctor, .prototype, headers_proto.toValue());
        try self.ctx.builtin_objects.append(self.allocator, headers_ctor);
        try self.ctx.setGlobal(headers_ctor_atom, headers_ctor.toValue());

        const request_ctor_atom = try self.ctx.atoms.intern("Request");
        const request_ctor = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            requestConstructorNative,
            request_ctor_atom,
            2,
        );
        try self.ctx.setPropertyChecked(request_ctor, .prototype, request_proto.toValue());
        try self.ctx.builtin_objects.append(self.allocator, request_ctor);
        try self.ctx.setGlobal(request_ctor_atom, request_ctor.toValue());

        const response_ctor = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            responseConstructorNative,
            .Response,
            2,
        );
        try self.ctx.setPropertyChecked(response_ctor, .prototype, response_proto.toValue());
        try self.addMethod(response_ctor, .json, responseJsonStaticNative, 1);
        try self.addMethod(response_ctor, .text, responseTextStaticNative, 1);
        try self.addMethod(response_ctor, .html, responseHtmlStaticNative, 1);
        try self.addDynamicMethod(response_ctor, "redirect", responseRedirectStaticNative, 1);
        try self.addMethod(response_ctor, .rawJson, responseRawJsonStaticNative, 1);
        try self.ctx.builtin_objects.append(self.allocator, response_ctor);
        try self.ctx.setGlobal(.Response, response_ctor.toValue());

        self.request_prototype = request_proto;
        self.response_prototype = response_proto;
        self.headers_prototype = headers_proto;
    }

    fn addMethod(self: *Self, obj: *zq.JSObject, atom: zq.Atom, func: zq.NativeFn, arg_count: u8) !void {
        const pool = self.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
        const fn_obj = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            self.ctx.root_class_idx,
            func,
            atom,
            arg_count,
        );
        try self.ctx.setPropertyChecked(obj, atom, fn_obj.toValue());
    }

    fn addDynamicMethod(self: *Self, obj: *zq.JSObject, name: []const u8, func: zq.NativeFn, arg_count: u8) !void {
        const atom = try self.ctx.atoms.intern(name);
        try self.addMethod(obj, atom, func, arg_count);
    }

    fn installConsole(self: *Self) !void {
        const root_class_idx = self.ctx.root_class_idx;
        const pool = self.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

        // Create console object
        const console_obj = try zq.JSObject.create(self.allocator, root_class_idx, null, pool);

        // Add console.log
        const log_atom: zq.Atom = .log;
        const log_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            consoleLog,
            log_atom,
            0,
        );
        try console_obj.setProperty(self.allocator, pool, log_atom, log_func.toValue());

        // Add console.error
        const error_atom = try self.ctx.atoms.intern("error");
        const error_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            consoleError,
            error_atom,
            0,
        );
        try console_obj.setProperty(self.allocator, pool, error_atom, error_func.toValue());

        // Add console.warn (aliases to stderr like console.error)
        const warn_atom = try self.ctx.atoms.intern("warn");
        const warn_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            consoleError,
            warn_atom,
            0,
        );
        try console_obj.setProperty(self.allocator, pool, warn_atom, warn_func.toValue());

        // Add console.info (aliases to console.log)
        const info_atom = try self.ctx.atoms.intern("info");
        const info_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            consoleLog,
            info_atom,
            0,
        );
        try console_obj.setProperty(self.allocator, pool, info_atom, info_func.toValue());

        // Add console.debug (aliases to console.log)
        const debug_atom = try self.ctx.atoms.intern("debug");
        const debug_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            consoleLog,
            debug_atom,
            0,
        );
        try console_obj.setProperty(self.allocator, pool, debug_atom, debug_func.toValue());

        // Track for cleanup in Context.deinit
        try self.ctx.builtin_objects.append(self.allocator, console_obj);

        // Register on global
        try self.ctx.setGlobal(.console, console_obj.toValue());
    }

    fn installHttpRequest(self: *Self) !void {
        const root_class_idx = self.ctx.root_class_idx;
        const pool = self.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

        const fn_atom = try self.ctx.atoms.intern("httpRequest");
        const fn_obj = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            httpRequestNative,
            fn_atom,
            1,
        );
        try self.ctx.builtin_objects.append(self.allocator, fn_obj);
        try self.ctx.setGlobal(fn_atom, fn_obj.toValue());
    }

    fn installFetchSync(self: *Self) !void {
        const root_class_idx = self.ctx.root_class_idx;
        const pool = self.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

        const fn_atom = try self.ctx.atoms.intern("fetchSync");
        // In replay mode, use a stub that returns recorded fetch responses.
        // In durable mode, use a hybrid wrapper that replays then records.
        const fetch_replay_stub = comptime zq.trace.makeReplayStub("http", "fetchSync");
        const fetch_durable_wrapper = comptime zq.trace.makeDurableWrapper("http", "fetchSync", fetchSyncNative);
        const func: zq.NativeFn = if (self.config.replay_file_path != null)
            fetch_replay_stub
        else if (self.config.durable_oplog_dir != null)
            fetch_durable_wrapper
        else
            fetchSyncNative;
        const fn_obj = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            func,
            fn_atom,
            1,
        );
        try self.ctx.builtin_objects.append(self.allocator, fn_obj);
        try self.ctx.setGlobal(fn_atom, fn_obj.toValue());
    }

    // Note: installResponseHelpers and installJsxRuntime removed - these are now handled by initBuiltins()

    /// Validate module imports from parsed IR.
    /// Called after parse() to verify that all import specifiers reference valid modules.
    /// Native functions are registered eagerly by installVirtualModules(), so this only validates.
    /// Returns true if file imports are present (requiring module graph compilation).
    fn resolveModuleImports(_: *Self, p: *const zq.parser.Parser) !bool {
        const imports = p.getImports() catch |err| {
            std.log.err("Failed to extract module imports: {}", .{err});
            return err;
        };
        defer p.freeImports(imports);

        var has_file_imports = false;

        for (imports) |import_info| {
            const result = zq.modules.resolve(import_info.module_specifier);
            switch (result) {
                .virtual => |binding| {
                    // Validate that all imported names exist in the module
                    if (zq.modules.validateImports(binding, import_info.specifier_names)) |missing| {
                        std.log.err("Module '{s}' has no export '{s}'", .{ import_info.module_specifier, missing });
                        return error.ModuleExportNotFound;
                    }
                },
                .file => {
                    has_file_imports = true;
                },
                .unknown => {
                    std.log.err("Unknown module: '{s}'; only zigttp:* virtual modules and relative file imports are supported", .{import_info.module_specifier});
                    return error.UnknownModule;
                },
            }
        }

        return has_file_imports;
    }

    /// Build a module graph from file imports, compile all dependencies,
    /// and run them in topological order so their exports are available
    /// as globals when the entry file executes.
    fn compileAndRunFileImports(self: *Self, entry_source: []const u8, filename: []const u8) !void {
        // Build module graph
        var graph = zq.modules.ModuleGraph.init(self.allocator);
        defer graph.deinit();

        graph.build(filename, entry_source, readFilePosixForGraph) catch |err| {
            switch (err) {
                error.CircularImport => std.log.err("Circular import detected starting from '{s}'", .{filename}),
                error.ImportFileNotFound => std.log.err("Could not read an imported file from '{s}'", .{filename}),
                error.ImportNestingTooDeep => std.log.err("Import nesting too deep (max 32) from '{s}'", .{filename}),
                else => std.log.err("Module graph error for '{s}': {}", .{ filename, err }),
            }
            return err;
        };

        if (graph.dependencyCount() == 0) {
            // No actual file dependencies found; run entry normally
            // This shouldn't happen since resolveModuleImports said there were file imports,
            // but handle gracefully.
            return;
        }

        // Compile all modules with shared atom table
        var module_compiler = zq.modules.ModuleCompiler.init(
            self.allocator,
            &self.ctx.atoms,
            self.strings,
        );
        var compile_result = module_compiler.compileAll(&graph) catch |err| {
            std.log.err("Multi-module compilation failed: {}", .{err});
            return err;
        };
        defer compile_result.deinit();

        // Run each module in execution order
        for (compile_result.modules) |*compiled_mod| {
            // Materialize shapes
            if (compiled_mod.shapes.len > 0) {
                try self.ctx.materializeShapes(compiled_mod.shapes);
            }

            // Run bytecode - this defines exported globals
            _ = try self.interpreter.run(&compiled_mod.func);
        }

        // The last module is the entry file; refresh handler cache
        try self.refreshHandlerCache();
    }

    /// Register all virtual module native functions on the context.
    /// Called during installBindings() for every runtime instance.
    /// In replay mode, registers stubs that return recorded values.
    /// In trace mode, registers wrappers that record I/O.
    /// In durable mode, registers hybrid replay/record wrappers.
    fn installVirtualModules(self: *Self) !void {
        if (self.config.replay_file_path != null) {
            // Replay mode: stubs that return recorded values from ReplayState
            inline for (zq.builtin_modules.all) |binding| {
                try zq.modules.registerVirtualModuleReplay(binding, self.ctx, self.allocator);
            }
        } else if (self.config.durable_oplog_dir != null) {
            // Durable mode: hybrid replay/record wrappers
            inline for (zq.builtin_modules.all) |binding| {
                try zq.modules.registerVirtualModuleDurable(binding, self.ctx, self.allocator);
            }
        } else if (self.config.trace_file_path != null) {
            // Use traced wrappers that record I/O to TraceRecorder
            inline for (zq.builtin_modules.all) |binding| {
                try zq.modules.registerVirtualModuleTraced(binding, self.ctx, self.allocator);
            }
        } else {
            inline for (zq.builtin_modules.all) |binding| {
                try zq.modules.registerVirtualModule(binding, self.ctx, self.allocator);
            }
        }
    }

    /// Install IoCallbacks into the io module's state slot.
    /// Enables parallel() and race() to call thunks and execute concurrent fetches.
    fn installIoModuleState(self: *Self) !void {
        const io_state = try self.allocator.create(zq.modules.io.IoCallbacks);
        io_state.* = .{
            .call_thunk_fn = ioCallThunk,
            .execute_fetches_fn = ioExecuteFetches,
            .build_response_fn = ioBuildResponse,
            .runtime_ptr = self,
        };
        self.ctx.setModuleState(
            zq.modules.io.MODULE_STATE_SLOT,
            @ptrCast(io_state),
            &zq.modules.io.IoCallbacks.deinitOpaque,
        );
    }

    fn installScopeModuleState(self: *Self) !void {
        const scope_state = try self.allocator.create(zq.modules.scope.ScopeCallbacks);
        scope_state.* = .{
            .call0_fn = ioCallThunk,
            .call1_fn = scopeCall1,
            .runtime_ptr = self,
            .current_state = null,
        };
        self.ctx.setModuleState(
            zq.modules.scope.MODULE_STATE_SLOT,
            @ptrCast(scope_state),
            &zq.modules.scope.ScopeCallbacks.deinitOpaque,
        );
    }

    fn installDurableModuleState(self: *Self) !void {
        const durable_state = try self.allocator.create(zq.modules.durable.DurableCallbacks);
        durable_state.* = .{
            .run_fn = durableRunCallback,
            .step_fn = durableStepCallback,
            .step_with_timeout_fn = durableStepWithTimeoutCallback,
            .sleep_until_fn = durableSleepUntilCallback,
            .wait_signal_fn = durableWaitSignalCallback,
            .signal_fn = durableSignalCallback,
            .signal_at_fn = durableSignalAtCallback,
            .runtime_ptr = self,
        };
        self.ctx.setModuleState(
            zq.modules.durable.MODULE_STATE_SLOT,
            @ptrCast(durable_state),
            &zq.modules.durable.DurableCallbacks.deinitOpaque,
        );
    }

    fn installSqlModuleState(self: *Self) !void {
        try zq.modules.sql.installStore(self.ctx, self.config.sqlite_path);
    }

    fn installServiceModuleState(self: *Self) !void {
        const system_path = self.config.system_config_path orelse return;
        try zq.modules.service.installState(self.ctx, system_path, self, serviceCallCallback);
    }

    fn installFetchModuleState(self: *Self) !void {
        try zq.modules.fetch.installState(self.ctx, self, fetchModuleCallback);
    }

    /// Install the WebSocket callback table on this runtime, pointed at
    /// the server-owned connection pool. Called lazily by the frame
    /// loop on first dispatch; idempotent re-installation rewrites the
    /// pool pointer in place. Runtimes that never see a WS event
    /// dispatch skip this entirely and pay no cost.
    pub fn installWebSocketModuleState(self: *Self, pool: *websocket_pool.Pool) !void {
        self.ws_pool_ref = pool;
        try zq.modules.websocket.installState(self.ctx, .{
            .runtime_ptr = self,
            .send_fn = wsSendCallback,
            .close_fn = wsCloseCallback,
            .serialize_attachment_fn = wsSerializeAttachmentCallback,
            .deserialize_attachment_fn = wsDeserializeAttachmentCallback,
            .get_web_sockets_fn = wsGetWebSocketsCallback,
            .room_from_path_fn = wsRoomFromPathCallback,
            .set_auto_response_fn = wsSetAutoResponseCallback,
        });
    }

    /// Load and compile JavaScript code
    pub fn loadCode(self: *Self, code: []const u8, filename: []const u8) !void {
        _ = try self.loadCodeWithCaching(code, filename, null);
    }

    /// Load and compile JavaScript code, optionally returning serialized bytecode for caching
    /// If cache_buffer is provided, serializes the bytecode and returns the serialized slice
    pub fn loadCodeWithCaching(self: *Self, code: []const u8, filename: []const u8, cache_buffer: ?[]u8) !?[]const u8 {
        var source_to_parse: []const u8 = code;
        var strip_result: ?zq.StripResult = null;
        defer if (strip_result) |*sr| sr.deinit();

        // Type strip for .ts/.tsx files
        const is_ts = std.mem.endsWith(u8, filename, ".ts");
        const is_tsx = std.mem.endsWith(u8, filename, ".tsx");
        if (is_ts or is_tsx) {
            strip_result = zq.strip(self.allocator, code, .{ .tsx_mode = is_tsx }) catch |err| {
                std.log.err("TypeScript strip error in {s}: {}", .{ filename, err });
                return err;
            };
            source_to_parse = strip_result.?.code;
        }

        // Parse the source code
        var p = zq.Parser.init(self.allocator, source_to_parse, self.strings, &self.ctx.atoms);
        defer p.deinit();

        // Enable JSX mode for .jsx and .tsx files
        if (std.mem.endsWith(u8, filename, ".jsx") or is_tsx) {
            p.enableJsx();
        }

        const bytecode_data = p.parse() catch |err| {
            // Print parse errors
            const errors = p.js_parser.getErrors();
            if (errors.len > 0) {
                for (errors) |parse_error| {
                    std.log.err("Parse error at {s}:{}:{}: {s}", .{
                        filename,
                        parse_error.location.line,
                        parse_error.location.column,
                        parse_error.message,
                    });
                }
            }
            return err;
        };

        // Run BoolChecker and TypeChecker on parsed IR
        {
            const ir_view = zq.parser.IrView.fromIRStore(&p.js_parser.nodes, &p.js_parser.constants);

            // BoolChecker: strict boolean enforcement
            var checker = zq.BoolChecker.init(self.allocator, ir_view, &self.ctx.atoms);
            defer checker.deinit();

            const error_count = try checker.check(p.root_node);
            const diags = checker.getDiagnostics();

            if (diags.len > 0) {
                var diag_output: std.ArrayList(u8) = .empty;
                defer diag_output.deinit(self.allocator);
                var diag_aw: std.Io.Writer.Allocating = .fromArrayList(self.allocator, &diag_output);
                checker.formatDiagnostics(source_to_parse, &diag_aw.writer) catch {};
                diag_output = diag_aw.toArrayList();
                if (diag_output.items.len > 0) {
                    std.log.err("{s}", .{diag_output.items});
                }
                std.log.err("{d} boolean check error(s), {d} warning(s)", .{
                    error_count,
                    diags.len - error_count,
                });
            }

            if (error_count > 0) {
                return error.SoundModeViolation;
            }

            // TypeChecker: full type annotation checking (when TypeMap available from .ts/.tsx stripping)
            if (strip_result) |sr| {
                const tm = sr.type_map;
                var type_pool = zq.TypePool.init(self.allocator);
                defer type_pool.deinit(self.allocator);

                var type_env = zq.TypeEnv.init(self.allocator, &type_pool);
                defer type_env.deinit();

                // Populate module types and user-declared types
                zq.modules.populateModuleTypes(&type_env, &type_pool, self.allocator);
                type_env.populateFromTypeMap(&tm);

                var tc = zq.TypeChecker.init(self.allocator, ir_view, &self.ctx.atoms, &type_env, null);
                defer tc.deinit();

                const tc_errors = try tc.check(p.root_node);
                const tc_diags = tc.getDiagnostics();

                if (tc_diags.len > 0) {
                    var tc_output: std.ArrayList(u8) = .empty;
                    defer tc_output.deinit(self.allocator);
                    var tc_aw: std.Io.Writer.Allocating = .fromArrayList(self.allocator, &tc_output);
                    tc.formatDiagnostics(source_to_parse, &tc_aw.writer) catch {};
                    tc_output = tc_aw.toArrayList();
                    if (tc_output.items.len > 0) {
                        std.log.err("{s}", .{tc_output.items});
                    }
                }

                if (tc_errors > 0) {
                    return error.SoundModeViolation;
                }
            }
        }

        // Resolve module imports and register virtual module native functions
        const has_file_imports = try self.resolveModuleImports(&p);

        // If file imports are present, build module graph and compile dependencies
        if (has_file_imports) {
            try self.compileAndRunFileImports(code, filename);
            return null; // Disable caching for multi-module handlers
        }

        // Materialize object literal shapes before execution
        const shapes = p.getShapes();
        if (shapes.len > 0) {
            try self.ctx.materializeShapes(shapes);
        }

        // Create FunctionBytecode struct to wrap the parsed result
        const func = zq.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = p.max_local_count,
            .stack_size = 256,
            .flags = .{},
            .code = bytecode_data,
            .constants = p.constants.items,
            .source_map = null,
        };

        // Bytecode verification: reject malformed bytecode before execution
        const verify_result = zq.BytecodeVerifier.verify(&func);
        if (!verify_result.valid) {
            std.log.err("Bytecode verification failed at offset {d}: {s}", .{
                verify_result.offset,
                verify_result.message,
            });
            return error.BytecodeVerificationFailed;
        }

        // Serialize for caching if buffer provided (includes atoms for true cache hit)
        var serialized: ?[]const u8 = null;
        if (cache_buffer) |buffer| {
            var writer = bytecode_cache.SliceWriter{ .buffer = buffer };
            bytecode_cache.serializeBytecodeWithAtomsAndShapes(
                &func,
                &self.ctx.atoms,
                shapes,
                &writer,
                self.allocator,
            ) catch {
                // Serialization failed (buffer too small), continue without caching
            };
            if (writer.pos > 0) {
                serialized = writer.getWritten();
            }
        }

        // Execute the compiled code to define functions
        _ = try self.interpreter.run(&func);
        try self.refreshHandlerCache();

        return serialized;
    }

    /// Load handler code (alias for loadCode for API compatibility)
    pub fn loadHandler(self: *Self, code: []const u8, filename: []const u8) !void {
        return self.loadCode(code, filename);
    }

    /// Call a global function by name with the provided arguments.
    /// Returns the function result or error.NotCallable if missing or not callable.
    pub fn callGlobalFunction(self: *Self, name: []const u8, args: []const zq.JSValue) !zq.JSValue {
        const atom = zq.object.lookupPredefinedAtom(name) orelse try self.ctx.atoms.intern(name);
        const func_val = self.ctx.getGlobal(atom) orelse return error.NotCallable;
        if (!func_val.isCallable()) return error.NotCallable;
        const func_obj = func_val.toPtr(zq.JSObject);
        return try self.callFunction(func_obj, args);
    }

    fn refreshHandlerCache(self: *Self) !void {
        const handler_atom = self.handler_atom orelse return error.NoHandler;
        const handler_val = self.ctx.getGlobal(handler_atom) orelse return error.NoHandler;
        if (!handler_val.isCallable()) return error.HandlerNotCallable;

        const handler_obj = handler_val.toPtr(zq.JSObject);
        self.cached_handler_obj = handler_obj;

        if (handler_obj.getBytecodeFunctionData()) |bc_data| {
            self.cached_dispatch = bc_data.bytecode.pattern_dispatch;
            // Detect handler arg count for capability injection support
            self.cached_handler_arg_count = @intCast(@min(bc_data.bytecode.arg_count, 255));
        } else {
            self.cached_dispatch = null;
        }
    }

    /// Load from cached serialized bytecode (Phase 1d: true cache hit with atoms and shapes)
    pub fn loadFromCachedBytecode(self: *Self, cached_data: []const u8) !void {
        try self.loadFromCachedBytecodeImpl(cached_data, true);
    }

    /// Load from cached bytecode without refreshing handler cache.
    /// Used for dependency modules that define exports but not the handler function.
    pub fn loadFromCachedBytecodeNoHandler(self: *Self, cached_data: []const u8) !void {
        try self.loadFromCachedBytecodeImpl(cached_data, false);
    }

    fn loadFromCachedBytecodeImpl(self: *Self, cached_data: []const u8, refresh_handler: bool) !void {
        var reader = bytecode_cache.SliceReader{ .data = cached_data };

        // Deserialize bytecode with atoms and shapes - skips parsing entirely
        const result = try bytecode_cache.deserializeBytecodeWithAtomsAndShapes(
            &reader,
            &self.ctx.atoms,
            self.allocator,
            self.strings,
        );
        // Note: We don't defer result.deinit() because the function and constants
        // need to stay alive. Shapes can be freed after materialization.
        defer {
            // Free shapes after materialization (they're copied into hidden classes)
            for (result.shapes) |shape| {
                self.allocator.free(shape);
            }
            self.allocator.free(result.shapes);
        }

        // Materialize object literal shapes before execution
        if (result.shapes.len > 0) {
            // Convert [][]object.Atom to []const []const object.Atom for materializeShapes
            const shapes_const: []const []const zq.object.Atom = @ptrCast(result.shapes);
            try self.ctx.materializeShapes(shapes_const);
        }

        // Execute the deserialized bytecode
        _ = try self.interpreter.run(result.func);
        if (refresh_handler) {
            try self.refreshHandlerCache();
        }
    }

    /// Serialize bytecode for caching (Phase 1b: cache miss path)
    pub fn serializeBytecode(self: *Self, func: *const zq.FunctionBytecode, buffer: []u8) ![]const u8 {
        var writer = bytecode_cache.SliceWriter{ .buffer = buffer };
        try bytecode_cache.serializeFunctionBytecode(func, &writer, self.allocator);
        return writer.getWritten();
    }

    /// Execute the handler function with a request
    pub fn executeHandler(self: *Self, request: HttpRequestView) !HttpResponse {
        return self.executeHandlerWithId(request, 0);
    }

    pub fn executeHandlerWithId(self: *Self, request: HttpRequestView, request_id: u64) !HttpResponse {
        return self.executeHandlerInternal(request, request_id, false);
    }

    /// Execute handler and return a response that borrows JS string bodies.
    /// Caller must ensure the runtime is not reset or reused until after send.
    pub fn executeHandlerBorrowed(self: *Self, request: HttpRequestView) !HttpResponse {
        return self.executeHandlerBorrowedWithId(request, 0);
    }

    pub fn executeHandlerBorrowedWithId(self: *Self, request: HttpRequestView, request_id: u64) !HttpResponse {
        return self.executeHandlerInternal(request, request_id, true);
    }

    fn executeHandlerInternal(self: *Self, request: HttpRequestView, request_id: u64, borrow_body: bool) !HttpResponse {
        self.last_request_body_len = if (request.body) |b| b.len else 0;
        self.active_request = request;
        defer self.active_request = null;
        defer {
            if (self.pending_durable_recovery != null) {
                self.pending_durable_recovery = null;
            }
        }

        if (self.cached_handler_obj == null) {
            try self.refreshHandlerCache();
        }
        const handler_obj = self.cached_handler_obj orelse return error.NoHandler;

        var tracked = false;
        if (builtin.mode == .Debug and request_id != 0) {
            const prev = self.active_request_id.swap(request_id, .acq_rel);
            if (prev != 0) {
                std.log.err(
                    "Runtime reused concurrently (prev={d} new={d} runtime=0x{x})",
                    .{ prev, request_id, @intFromPtr(self) },
                );
                return error.RuntimeInUse;
            }
            tracked = true;
        }
        defer if (tracked) self.active_request_id.store(0, .release);
        var reset_after = self.owns_resources;
        defer if (reset_after) self.resetForNextRequest();

        // === TRACE RECORDING: Set up per-request recorder ===
        var trace_timer: ?compat.Timer = null;
        if (self.trace_file != null and self.trace_mutex != null) {
            if (self.trace_recorder == null) {
                self.trace_recorder = self.allocator.create(zq.TraceRecorder) catch null;
                if (self.trace_recorder) |rec| {
                    rec.* = zq.TraceRecorder.init(
                        self.allocator,
                        self.trace_file.?,
                        self.trace_mutex.?,
                    );
                }
            }
            if (self.trace_recorder) |rec| {
                rec.reset();
                self.ctx.setModuleState(
                    zq.TRACE_STATE_SLOT,
                    @ptrCast(rec),
                    &zq.TraceRecorder.deinitOpaque,
                );

                // Record request
                var h_names: [64][]const u8 = undefined;
                var h_values: [64][]const u8 = undefined;
                const hcount = splitHeaderKV(request.headers.items, &h_names, &h_values);
                rec.recordRequestRaw(
                    request.method,
                    request.url,
                    h_names[0..hcount],
                    h_values[0..hcount],
                    request.body,
                );
                trace_timer = compat.Timer.start() catch null;
            }
        }
        defer if (self.trace_recorder) |rec| {
            // Record meta and flush after handler completes
            const duration_us: u64 = if (trace_timer) |*t| t.read() / 1000 else 0;
            rec.recordMeta(duration_us, self.config.trace_file_path orelse "unknown", 0);
            rec.flush();
            // Remove from module_state so next request gets a fresh one
            self.ctx.module_state[zq.TRACE_STATE_SLOT] = null;
        };

        // === FAST PATH: Native dispatch for static routes ===
        if (self.cached_dispatch) |dispatch| {
            if (self.tryFastPathDispatch(
                dispatch,
                request.url,
                request.path,
                borrow_body,
            )) |response| {
                // Native fast-path dispatch does not execute JS bytecode or allocate
                // request JS objects, so runtime reset is unnecessary on this path.
                reset_after = false;
                return response;
            }
        }

        // Create Request object from HttpRequest (shared between AOT and interpreter)
        const request_obj = try self.createRequestObject(request);

        // Build argument array: 1-arg (backward compat) or 2-arg (with capabilities)
        var args_buf: [2]zq.JSValue = undefined;
        args_buf[0] = request_obj;
        const args: []const zq.JSValue = if (self.cached_handler_arg_count >= 2) blk: {
            args_buf[1] = try self.createCapabilitiesObject();
            break :blk args_buf[0..2];
        } else args_buf[0..1];

        // === AOT PATH: Native Zig handler ===
        aot_attempt: {
            if (builtin.is_test) {
                if (aot_override) |override_fn| {
                    const override_result = override_fn(self.ctx, args) catch |err| {
                        if (err == error.AotBail) break :aot_attempt;
                        std.log.err("AOT override failed: {}", .{err});
                        return error.HandlerError;
                    };

                    const response = try self.extractResponseInternal(override_result, borrow_body);
                    if (borrow_body and response.requires_runtime) {
                        reset_after = false;
                    }
                    self.traceRecordResponse(&response);
                    return response;
                }
            }
            if (!embedded_handler.has_aot) break :aot_attempt;

            const aot_result = embedded_handler.aotHandler(self.ctx, args) catch |err| {
                if (err == error.AotBail) break :aot_attempt;
                std.log.err("AOT handler failed: {}", .{err});
                return error.HandlerError;
            };

            const response = try self.extractResponseInternal(aot_result, borrow_body);
            if (borrow_body and response.requires_runtime) {
                reset_after = false;
            }
            self.traceRecordResponse(&response);
            return response;
        }

        // === SLOW PATH: Full bytecode execution ===

        // Set thread-local runtime for native function callbacks (e.g., renderToString)
        current_runtime = self;
        defer current_runtime = null;

        // Set callback for JSX function component rendering
        zq.http.setCallFunctionCallback(callFunctionWrapper);
        defer zq.http.clearCallFunctionCallback();

        try zq.modules.scope.beginRequest(self.ctx);
        defer zq.modules.scope.endRequest(self.ctx);

        const result = self.callFunction(handler_obj, args) catch |err| {
            std.log.err("Handler execution failed: {}", .{err});
            return error.HandlerError;
        };

        // If the bytecode path set an exception but didn't propagate a Zig error
        // (e.g. a conditional opcode rejected its operand and left the stack in
        // an `undefined` state), surface it as a 500 instead of letting
        // extractResponseInternal return an empty default. This guards against
        // the "silent empty 200" class of bug.
        if (self.ctx.hasException()) {
            const exc = self.ctx.exception;
            self.ctx.clearException();
            var err_response = HttpResponse.init(self.allocator);
            err_response.status = 500;
            const exc_msg: []const u8 = if (exc.isString())
                exc.toPtr(zq.JSString).data()
            else
                "handler aborted without returning a Response";
            const body_owned = try self.allocator.dupe(u8, exc_msg);
            err_response.setBodyOwned(body_owned);
            try err_response.putHeader("Content-Type", "text/plain; charset=utf-8");
            self.traceRecordResponse(&err_response);
            return err_response;
        }

        // Convert result to HttpResponse
        const response = try self.extractResponseInternal(result, borrow_body);
        if (borrow_body and response.requires_runtime) {
            reset_after = false;
        }

        self.traceRecordResponse(&response);
        return response;
    }

    /// Record response to trace or durable oplog if active.
    fn traceRecordResponse(self: *Self, response: *const HttpResponse) void {
        // Trace mode: record to trace buffer
        const rec = self.trace_recorder orelse return;
        if (!rec.active) return;

        var h_names: [64][]const u8 = undefined;
        var h_values: [64][]const u8 = undefined;
        const hcount = splitHeaderKV(response.headers.items, &h_names, &h_values);
        rec.recordResponse(
            response.status,
            h_names[0..hcount],
            h_values[0..hcount],
            response.body,
        );
    }

    pub fn setPendingDurableRecovery(
        self: *Self,
        key: []const u8,
        oplog_path: []const u8,
        events: []const zq.trace.DurableEvent,
    ) void {
        self.pending_durable_recovery = .{
            .key = key,
            .oplog_path = oplog_path,
            .events = events,
        };
    }

    fn durableRun(self: *Self, ctx: *zq.Context, key: []const u8, run_val: zq.JSValue) anyerror!zq.JSValue {
        if (self.active_durable_run != null) {
            return zq.modules.util.throwError(ctx, "Error", "nested run() is not supported");
        }
        if (!run_val.isObject()) {
            return zq.modules.util.throwError(ctx, "TypeError", "run() expects a callable function");
        }
        _ = self.active_request orelse {
            return zq.modules.util.throwError(ctx, "Error", "run() is only valid during request handling");
        };

        if (try self.tryLoadCompletedDurableResponse(key)) |cached| {
            return cached;
        }

        const active = self.openActiveDurableRun(key) catch |err| switch (err) {
            error.DurableRecoveryKeyMismatch => return zq.modules.util.throwError(ctx, "Error", "durable recovery key did not match run() key"),
            error.DurableKeyCollision => return zq.modules.util.throwError(ctx, "Error", "durable key collision detected for oplog path"),
            else => return err,
        };

        self.active_durable_run = active;
        self.ctx.setModuleState(
            zq.trace.DURABLE_STATE_SLOT,
            @ptrCast(self.active_durable_run.?.state),
            &zq.trace.DurableState.deinitOpaque,
        );
        defer {
            self.ctx.module_state[zq.trace.DURABLE_STATE_SLOT] = null;
            if (self.active_durable_run) |*run| {
                run.deinit(self.allocator);
            }
            self.active_durable_run = null;
        }

        const func_obj = run_val.toPtr(zq.JSObject);
        const result = self.callFunction(func_obj, &.{}) catch |err| switch (err) {
            error.DurableSuspended => return try self.buildPendingDurableResponseValue(),
            else => return err,
        };
        const upgraded = try upgradeResponseValue(self.ctx, result);
        if (!self.isResponseLike(upgraded)) {
            return zq.modules.util.throwError(ctx, "TypeError", "run() callback must return a Response");
        }

        var response = try self.extractResponseInternal(upgraded, false);
        defer response.deinit();
        self.persistActiveDurableResponse(&response);
        return upgraded;
    }

    fn durableStep(self: *Self, ctx: *zq.Context, name: []const u8, step_val: zq.JSValue) anyerror!zq.JSValue {
        const active = self.active_durable_run orelse {
            return zq.modules.util.throwError(ctx, "Error", "step() must be called inside run()");
        };
        if (!step_val.isObject()) {
            return zq.modules.util.throwError(ctx, "TypeError", "step() expects a callable function");
        }
        if (active.step_depth > 0) {
            return zq.modules.util.throwError(ctx, "Error", "nested step() is not supported");
        }

        switch (active.state.beginStep(name)) {
            .cached => |result_json| {
                return zq.trace.jsonToJSValue(ctx, result_json);
            },
            .execute => {},
            .live => {
                active.state.persistStepStart(name);
            },
        }

        self.active_durable_run.?.step_depth += 1;
        defer self.active_durable_run.?.step_depth -= 1;

        const func_obj = step_val.toPtr(zq.JSObject);
        const result = try self.callFunction(func_obj, &.{});
        self.active_durable_run.?.state.persistStepResult(name, ctx, result);
        return result;
    }

    fn durableStepWithTimeout(self: *Self, ctx: *zq.Context, name: []const u8, timeout_ms: i64, step_val: zq.JSValue) anyerror!zq.JSValue {
        const active = self.active_durable_run orelse {
            return zq.modules.util.throwError(ctx, "Error", "stepWithTimeout() must be called inside run()");
        };
        if (!step_val.isObject()) {
            return zq.modules.util.throwError(ctx, "TypeError", "stepWithTimeout() expects a callable function");
        }
        if (active.step_depth > 0) {
            return zq.modules.util.throwError(ctx, "Error", "nested stepWithTimeout() is not supported");
        }

        switch (active.state.beginStep(name)) {
            .cached => |result_json| {
                return zq.trace.jsonToJSValue(ctx, result_json);
            },
            .execute => {},
            .live => {
                active.state.persistStepStart(name);
            },
        }

        self.active_durable_run.?.step_depth += 1;
        defer self.active_durable_run.?.step_depth -= 1;

        const deadline_ms = std.math.add(i64, unixMillis(), timeout_ms) catch std.math.maxInt(i64);
        const func_obj = step_val.toPtr(zq.JSObject);
        const result = self.callFunction(func_obj, &.{}) catch |err| {
            if (err == error.DurableSuspended) return err;
            // On execution error, check if we exceeded the deadline
            if (unixMillis() >= deadline_ms) {
                const timeout_result = try zq.modules.util.createPlainResultErr(ctx, "timeout");
                self.active_durable_run.?.state.persistStepResult(name, ctx, timeout_result);
                return timeout_result;
            }
            return err;
        };

        // Check if execution exceeded the deadline
        if (unixMillis() >= deadline_ms) {
            const timeout_result = try zq.modules.util.createPlainResultErr(ctx, "timeout");
            self.active_durable_run.?.state.persistStepResult(name, ctx, timeout_result);
            return timeout_result;
        }

        const ok_result = try zq.modules.util.createPlainResultOk(ctx, result);
        self.active_durable_run.?.state.persistStepResult(name, ctx, ok_result);
        return ok_result;
    }

    fn durableSleepUntil(self: *Self, ctx: *zq.Context, until_ms: i64) anyerror!zq.JSValue {
        if (self.active_durable_run == null) {
            return zq.modules.util.throwError(ctx, "Error", "sleepUntil() must be called inside run()");
        }
        const active = &self.active_durable_run.?;
        if (active.step_depth > 0) {
            return zq.modules.util.throwError(ctx, "Error", "sleepUntil() is not supported inside step()");
        }

        const now_ms = unixMillis();
        switch (active.state.beginTimerWait(until_ms)) {
            .ready => return zq.JSValue.undefined_val,
            .pending => |wait| {
                if (wait.until_ms <= now_ms) {
                    active.state.persistResumeTimer(now_ms);
                    return zq.JSValue.undefined_val;
                }
                try active.setPendingTimer(self.allocator, wait.until_ms);
                return error.DurableSuspended;
            },
            .live => {
                active.state.persistWaitTimer(until_ms);
                if (until_ms <= now_ms) {
                    active.state.persistResumeTimer(now_ms);
                    return zq.JSValue.undefined_val;
                }
                try active.setPendingTimer(self.allocator, until_ms);
                return error.DurableSuspended;
            },
        }
    }

    fn durableWaitSignal(self: *Self, ctx: *zq.Context, name: []const u8) anyerror!zq.JSValue {
        if (self.active_durable_run == null) {
            return zq.modules.util.throwError(ctx, "Error", "waitSignal() must be called inside run()");
        }
        const active = &self.active_durable_run.?;
        if (active.step_depth > 0) {
            return zq.modules.util.throwError(ctx, "Error", "waitSignal() is not supported inside step()");
        }

        var store = try self.initDurableStore();
        const now_ms = unixMillis();

        switch (active.state.beginSignalWait(name)) {
            .delivered => |payload_json| return zq.trace.jsonToJSValue(ctx, payload_json),
            .pending => {
                if (try store.tryConsumeSignal(active.key, name, now_ms)) |payload| {
                    var consumed = payload;
                    defer consumed.deinit();
                    active.state.persistResumeSignal(name, consumed.payload_json);
                    return zq.trace.jsonToJSValue(ctx, consumed.payload_json);
                }
                try active.setPendingSignal(self.allocator, name);
                return error.DurableSuspended;
            },
            .live => {
                active.state.persistWaitSignal(name);
                if (try store.tryConsumeSignal(active.key, name, now_ms)) |payload| {
                    var consumed = payload;
                    defer consumed.deinit();
                    active.state.persistResumeSignal(name, consumed.payload_json);
                    return zq.trace.jsonToJSValue(ctx, consumed.payload_json);
                }
                try active.setPendingSignal(self.allocator, name);
                return error.DurableSuspended;
            },
        }
    }

    fn durableSignal(self: *Self, ctx: *zq.Context, key: []const u8, name: []const u8, payload: zq.JSValue) anyerror!zq.JSValue {
        const exists = self.durableSignalTargetExists(key) catch |err| switch (err) {
            error.DurableKeyCollision => return zq.modules.util.throwError(ctx, "Error", "durable key collision detected for oplog path"),
            else => return err,
        };
        if (!exists) return zq.JSValue.false_val;

        const payload_json = self.serializeDurablePayload(ctx, payload) catch |err| switch (err) {
            error.InvalidDurablePayload => return zq.modules.util.throwError(ctx, "TypeError", "signal() payload must be JSON-serializable"),
            else => return err,
        };
        defer self.allocator.free(payload_json);

        var store = try self.initDurableStore();
        try store.enqueueSignal(key, name, payload_json);
        return zq.JSValue.true_val;
    }

    fn durableSignalAt(
        self: *Self,
        ctx: *zq.Context,
        key: []const u8,
        name: []const u8,
        at_ms: i64,
        payload: zq.JSValue,
    ) anyerror!zq.JSValue {
        const exists = self.durableSignalTargetExists(key) catch |err| switch (err) {
            error.DurableKeyCollision => return zq.modules.util.throwError(ctx, "Error", "durable key collision detected for oplog path"),
            else => return err,
        };
        if (!exists) return zq.JSValue.false_val;

        const payload_json = self.serializeDurablePayload(ctx, payload) catch |err| switch (err) {
            error.InvalidDurablePayload => return zq.modules.util.throwError(ctx, "TypeError", "signalAt() payload must be JSON-serializable"),
            else => return err,
        };
        defer self.allocator.free(payload_json);

        var store = try self.initDurableStore();
        try store.enqueueSignalAt(key, name, at_ms, payload_json);
        return zq.JSValue.true_val;
    }

    fn initDurableStore(self: *Self) !durable_store_mod.DurableStore {
        const dir = self.config.durable_oplog_dir orelse return error.DurableDisabled;
        return durable_store_mod.DurableStore.initFs(self.allocator, dir);
    }

    fn durableSignalTargetExists(self: *Self, key: []const u8) !bool {
        const path = try self.buildDurableOplogPath(key);
        defer self.allocator.free(path);

        const source = try self.readDurableLogIfExists(path) orelse return false;
        defer self.allocator.free(source);

        var parsed = try zq.trace.parseDurableOplog(self.allocator, source);
        defer parsed.deinit();

        if (parsed.run_key) |existing_key| {
            if (!std.mem.eql(u8, existing_key, key)) return error.DurableKeyCollision;
        }

        return !parsed.complete;
    }

    fn serializeDurablePayload(self: *Self, ctx: *zq.Context, payload: zq.JSValue) ![]u8 {
        if (payload.isUndefined()) {
            return self.allocator.dupe(u8, "null");
        }

        const json_val = zq.builtins.jsonStringify(ctx, zq.JSValue.undefined_val, &.{payload});
        if (ctx.hasException()) {
            ctx.clearException();
            return error.InvalidDurablePayload;
        }

        const json = getStringData(json_val) orelse return error.InvalidDurablePayload;
        return self.allocator.dupe(u8, json);
    }

    fn buildPendingDurableResponseValue(self: *Self) !zq.JSValue {
        const active = self.active_durable_run orelse return error.NoActiveDurableRun;
        const wait = active.pending_wait orelse return error.MissingDurableWait;
        var body: std.ArrayList(u8) = .empty;
        defer body.deinit(self.allocator);

        try body.appendSlice(self.allocator, "{\"pending\":true,\"durableKey\":\"");
        try appendEscapedJson(&body, self.allocator, active.key);
        try body.appendSlice(self.allocator, "\",\"wait\":{");

        switch (wait) {
            .timer => |until_ms| {
                try body.appendSlice(self.allocator, "\"type\":\"timer\",\"until\":");
                var tmp: [32]u8 = undefined;
                const printed = try std.fmt.bufPrint(&tmp, "{d}", .{until_ms});
                try body.appendSlice(self.allocator, printed);
            },
            .signal => |name| {
                try body.appendSlice(self.allocator, "\"type\":\"signal\",\"name\":\"");
                try appendEscapedJson(&body, self.allocator, name);
                try body.appendSlice(self.allocator, "\"");
            },
        }

        try body.appendSlice(self.allocator, "}}");
        const created = try createFetchResponse(self, 202, "Accepted", body.items, "application/json");
        return created.value;
    }

    fn tryLoadCompletedDurableResponse(self: *Self, key: []const u8) !?zq.JSValue {
        if (self.pending_durable_recovery != null) return null;

        const path = try self.buildDurableOplogPath(key);
        defer self.allocator.free(path);

        const source = try self.readDurableLogIfExists(path) orelse return null;
        defer self.allocator.free(source);

        var parsed = try zq.trace.parseDurableOplog(self.allocator, source);
        defer parsed.deinit();

        if (!parsed.complete) return null;
        if (parsed.run_key) |existing_key| {
            if (!std.mem.eql(u8, existing_key, key)) return error.DurableKeyCollision;
        }

        const response = parsed.response orelse return error.DurableMissingResponse;
        return try self.buildStoredResponseValue(response);
    }

    fn openActiveDurableRun(self: *Self, key: []const u8) !ActiveDurableRun {
        if (self.pending_durable_recovery) |pending| {
            if (!std.mem.eql(u8, pending.key, key)) return error.DurableRecoveryKeyMismatch;

            const fd = try openOplogAppendFile(self.allocator, pending.oplog_path);
            const state = try self.allocator.create(zq.trace.DurableState);
            state.* = zq.trace.DurableState.init(self.allocator, pending.events, fd);
            return .{
                .key = try self.allocator.dupe(u8, key),
                .oplog_path = try self.allocator.dupe(u8, pending.oplog_path),
                .oplog_fd = fd,
                .state = state,
            };
        }

        const path = try self.buildDurableOplogPath(key);
        errdefer self.allocator.free(path);

        if (try self.readDurableLogIfExists(path)) |source| {
            errdefer self.allocator.free(source);

            var parsed = try zq.trace.parseDurableOplog(self.allocator, source);
            defer parsed.deinit();

            if (parsed.run_key) |existing_key| {
                if (!std.mem.eql(u8, existing_key, key)) return error.DurableKeyCollision;
            }
            if (parsed.complete) return error.DurableAlreadyComplete;

            const events = parsed.events;
            parsed.events = &.{};

            const fd = try openOplogAppendFile(self.allocator, path);
            const state = try self.allocator.create(zq.trace.DurableState);
            state.* = zq.trace.DurableState.init(self.allocator, events, fd);
            return .{
                .key = try self.allocator.dupe(u8, key),
                .oplog_path = path,
                .oplog_fd = fd,
                .state = state,
                .owned_events = events,
                .source_snapshot = source,
            };
        }

        const request = self.active_request orelse return error.NoActiveRequest;
        const fd = try openOplogFile(self.allocator, path);
        errdefer std.Io.Threaded.closeFd(fd);

        const state = try self.allocator.create(zq.trace.DurableState);
        errdefer self.allocator.destroy(state);
        state.* = zq.trace.DurableState.init(self.allocator, &.{}, fd);
        state.persistRunKey(key);

        var h_names: [64][]const u8 = undefined;
        var h_values: [64][]const u8 = undefined;
        const hcount = splitHeaderKV(request.headers.items, &h_names, &h_values);
        state.persistRequest(
            request.method,
            request.url,
            h_names[0..hcount],
            h_values[0..hcount],
            request.body,
        );

        return .{
            .key = try self.allocator.dupe(u8, key),
            .oplog_path = path,
            .oplog_fd = fd,
            .state = state,
        };
    }

    fn buildDurableOplogPath(self: *Self, key: []const u8) ![]u8 {
        const dir = self.config.durable_oplog_dir orelse return error.DurableDisabled;
        return std.fmt.allocPrint(
            self.allocator,
            "{s}/durable-{x}.jsonl",
            .{ dir, std.hash.Fnv1a_64.hash(key) },
        );
    }

    fn readDurableLogIfExists(self: *Self, path: []const u8) !?[]u8 {
        return zq.file_io.readFile(self.allocator, path, 100 * 1024 * 1024) catch |err| switch (err) {
            error.FileNotFound => null,
            else => err,
        };
    }

    fn buildStoredResponseValue(self: *Self, response: zq.trace.ResponseTrace) !zq.JSValue {
        var headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
        defer headers.deinit(self.allocator);
        try parseHeadersFromJson(self.allocator, response.headers_json, &headers);
        const body = try zq.trace.unescapeJson(self.allocator, response.body);
        defer self.allocator.free(body);

        var content_type: ?[]const u8 = null;
        for (headers.items) |header| {
            if (ascii.eqlIgnoreCase(header.key, "content-type")) {
                content_type = header.value;
                break;
            }
        }

        const created = try createFetchResponse(
            self,
            response.status,
            statusTextFor(response.status),
            body,
            content_type,
        );

        for (headers.items) |header| {
            const key_atom = try getHeaderAtom(self.ctx, header.key);
            const value_str = try self.ctx.createString(header.value);
            try self.ctx.setPropertyChecked(created.headers, key_atom, value_str);
        }

        return created.value;
    }

    fn isResponseLike(self: *Self, value: zq.JSValue) bool {
        if (!value.isObject()) return false;
        const pool = self.ctx.hidden_class_pool orelse return false;
        const obj = value.toPtr(zq.JSObject);
        return obj.getProperty(pool, zq.Atom.status) != null and
            obj.getProperty(pool, zq.Atom.body) != null and
            obj.getProperty(pool, zq.Atom.headers) != null;
    }

    fn persistActiveDurableResponse(self: *Self, response: *const HttpResponse) void {
        if (self.active_durable_run) |*active| {
            var h_names: [64][]const u8 = undefined;
            var h_values: [64][]const u8 = undefined;
            const hcount = splitHeaderKV(response.headers.items, &h_names, &h_values);
            active.state.persistResponse(
                response.status,
                h_names[0..hcount],
                h_values[0..hcount],
                response.body,
            );
            active.state.markComplete();
        }
    }

    /// Try to dispatch request via native fast path.
    /// Returns response if a static pattern matches, null otherwise.
    fn tryFastPathDispatch(
        self: *Self,
        dispatch: *const zq.bytecode.PatternDispatchTable,
        url: []const u8,
        path: []const u8,
        borrow_body: bool,
    ) ?HttpResponse {
        const effective_path = if (path.len > 0) path else url;

        // O(1) exact match via hash lookup
        if (self.tryFastExactMatch(dispatch, .url, url, borrow_body)) |response| {
            return response;
        }
        if (!std.mem.eql(u8, effective_path, url)) {
            if (self.tryFastExactMatch(dispatch, .path, effective_path, borrow_body)) |response| {
                return response;
            }
        }

        // Linear scan for prefix matches (typically 2-3 patterns)
        for (dispatch.patterns) |*pattern| {
            if (pattern.pattern_type == .prefix) {
                const target = switch (pattern.url_atom) {
                    .path => effective_path,
                    else => url,
                };

                if (std.mem.startsWith(u8, target, pattern.url_bytes)) {
                    // Check if we have a template for native interpolation
                    if (pattern.response_template_prefix != null) {
                        const param = target[pattern.url_bytes.len..];
                        return self.buildTemplatedResponse(pattern, param) catch null;
                    }
                    // No template - fall back to bytecode
                    continue;
                }
            }
        }

        return null; // Fall back to bytecode execution
    }

    fn tryFastExactMatch(
        self: *Self,
        dispatch: *const zq.bytecode.PatternDispatchTable,
        route_atom: zq.Atom,
        target: []const u8,
        borrow_body: bool,
    ) ?HttpResponse {
        const target_hash = std.hash.Wyhash.hash(0, target);
        const idx = dispatch.exact_match_map.get(target_hash) orelse return null;
        const pattern = &dispatch.patterns[idx];

        // Verify atom + bytes (handle hash collision)
        if (pattern.pattern_type != .exact) return null;
        if (pattern.url_atom != route_atom) return null;
        if (!std.mem.eql(u8, target, pattern.url_bytes)) return null;
        if (pattern.body_source != .static) return null;

        return self.buildFastResponse(pattern, borrow_body) catch null;
    }

    /// Build a response by interpolating a dynamic parameter into a template.
    /// Used for prefix patterns like /api/greet/:name -> {"greeting":"Hello, {name}!"}
    fn buildTemplatedResponse(
        self: *Self,
        pattern: *const zq.bytecode.HandlerPattern,
        param: []const u8,
    ) !HttpResponse {
        const prefix = pattern.response_template_prefix orelse return error.NoTemplate;
        const suffix = pattern.response_template_suffix orelse "";

        // Build the response body: prefix + param + suffix
        const body_len = prefix.len + param.len + suffix.len;
        const body = try self.allocator.alloc(u8, body_len);
        errdefer self.allocator.free(body);

        var pos: usize = 0;
        @memcpy(body[pos..][0..prefix.len], prefix);
        pos += prefix.len;
        @memcpy(body[pos..][0..param.len], param);
        pos += param.len;
        @memcpy(body[pos..][0..suffix.len], suffix);

        var response = HttpResponse.init(self.allocator);
        response.status = pattern.status;
        response.body = body;
        response.body_owned = true;

        const content_type = switch (pattern.content_type_idx) {
            0 => "application/json",
            1 => "text/plain; charset=utf-8",
            else => "text/html; charset=utf-8",
        };
        try response.putHeaderBorrowed("Content-Type", content_type);

        return response;
    }

    /// Build a response from a pre-serialized static pattern.
    fn buildFastResponse(
        self: *Self,
        pattern: *const zq.bytecode.HandlerPattern,
        borrow_body: bool,
    ) !HttpResponse {
        var response = HttpResponse.init(self.allocator);
        response.status = pattern.status;

        // OPTIMIZATION: Use pre-built raw response if available (single write, no header construction)
        if (pattern.prebuilt_response) |prebuilt| {
            response.prebuilt_raw = prebuilt;
            // Still set body for logging/debugging purposes
            response.body = pattern.static_body;
            response.body_owned = false;
            return response;
        }

        // Fallback: construct response normally
        if (borrow_body) {
            response.body = pattern.static_body;
            response.body_owned = false;
        } else {
            const body_copy = try self.allocator.dupe(u8, pattern.static_body);
            response.body = body_copy;
            response.body_owned = true;
        }

        const content_type = switch (pattern.content_type_idx) {
            0 => "application/json",
            1 => "text/plain; charset=utf-8",
            else => "text/html; charset=utf-8",
        };
        try response.putHeaderBorrowed("Content-Type", content_type);

        return response;
    }

    /// Try to parse a query parameter value as a 32-bit signed integer.
    /// Returns null if the value is not a valid integer (empty, has non-digit chars, overflow).
    /// Supports optional leading minus sign for negative numbers.
    fn parseQueryInt(value: []const u8) ?i32 {
        if (value.len == 0) return null;
        if (value.len > 11) return null; // -2147483648 is 11 chars max

        var i: usize = 0;
        var negative = false;

        // Check for leading minus
        if (value[0] == '-') {
            negative = true;
            i = 1;
            if (value.len == 1) return null; // Just "-" is not valid
        }

        // Must have at least one digit
        if (i >= value.len) return null;

        var result: i64 = 0;
        while (i < value.len) : (i += 1) {
            const c = value[i];
            if (c < '0' or c > '9') return null; // Non-digit character
            result = result * 10 + (c - '0');
            // Check for overflow (using i64 to detect i32 overflow)
            if (result > 2147483647 and !negative) return null;
            if (result > 2147483648 and negative) return null;
        }

        if (negative) {
            return @intCast(-result);
        }
        return @intCast(result);
    }

    fn headerKeyToAtom(key: []const u8) ?zq.Atom {
        if (ascii.eqlIgnoreCase(key, "accept")) return .accept;
        if (ascii.eqlIgnoreCase(key, "host")) return .host;
        if (ascii.eqlIgnoreCase(key, "user-agent")) return .@"user-agent";
        if (ascii.eqlIgnoreCase(key, "content-type")) return .@"content-type";
        if (ascii.eqlIgnoreCase(key, "connection")) return .connection;
        if (ascii.eqlIgnoreCase(key, "accept-encoding")) return .@"accept-encoding";
        if (ascii.eqlIgnoreCase(key, "authorization")) return .authorization;
        return null;
    }

    /// Create a capabilities object for 2-arg handler invocation.
    /// The caps object contains virtual module functions grouped by namespace.
    /// For now, all registered virtual module functions are included.
    /// The type checker validates that the handler only accesses
    /// capabilities declared in its type annotation.
    fn createCapabilitiesObject(self: *Self) !zq.JSValue {
        // Create a plain empty object as the capabilities container.
        // Virtual module functions are already available via imports;
        // the caps object provides an alternative DI-style access pattern.
        // Full population with module namespaces is done lazily: the type
        // checker ensures only declared capabilities are accessed.
        const caps_obj = try self.ctx.createObject(null);
        return zq.JSValue.fromPtr(caps_obj);
    }

    fn createRequestObject(self: *Self, request: HttpRequestView) !zq.JSValue {
        // Use pre-shaped request object for faster creation (direct slot access).
        // http_shapes is initialized by default (use_http_shape_cache = true).
        // If not available, fall back to dynamic object creation.
        const shapes = self.ctx.http_shapes orelse return self.createRequestObjectDynamic(request);

        const req_obj = try self.ctx.createObjectWithClass(shapes.request.class_idx, self.request_prototype);

        // URL
        const url_str = try self.ctx.createString(request.url);
        req_obj.setSlot(shapes.request.url_slot, url_str);

        // Method - use cached string if available
        const method_val: zq.JSValue = if (self.ctx.getCachedMethod(request.method)) |cached|
            zq.JSValue.fromPtr(cached)
        else
            try self.ctx.createString(request.method);
        req_obj.setSlot(shapes.request.method_slot, method_val);

        // Path - URL without query string
        const path_slice = if (request.path.len > 0) request.path else request.url;
        const path_val: zq.JSValue = if (std.mem.eql(u8, path_slice, request.url))
            url_str
        else
            try self.ctx.createString(path_slice);
        req_obj.setSlot(shapes.request.path_slot, path_val);

        // Query - create object from parsed query parameters
        const query_obj = try buildQueryObject(self.ctx, request.query_params);
        req_obj.setSlot(shapes.request.query_slot, query_obj.toValue());

        // Body
        if (request.body) |body| {
            const body_str = try self.ctx.createString(body);
            req_obj.setSlot(shapes.request.body_slot, body_str);
        } else {
            req_obj.setSlot(shapes.request.body_slot, zq.JSValue.undefined_val);
        }

        // Headers
        const headers_obj = try self.ctx.createObjectWithClass(shapes.request_headers.class_idx, self.headers_prototype);
        for (request.headers.items) |header| {
            const key_atom = headerKeyToAtom(header.key) orelse
                try self.ctx.atoms.intern(header.key);
            const value_str = try self.ctx.createString(header.value);
            switch (key_atom) {
                .authorization => headers_obj.setSlot(shapes.request_headers.authorization_slot, value_str),
                .@"content-type" => headers_obj.setSlot(shapes.request_headers.content_type_slot, value_str),
                .accept => headers_obj.setSlot(shapes.request_headers.accept_slot, value_str),
                .host => headers_obj.setSlot(shapes.request_headers.host_slot, value_str),
                .@"user-agent" => headers_obj.setSlot(shapes.request_headers.user_agent_slot, value_str),
                .@"accept-encoding" => headers_obj.setSlot(shapes.request_headers.accept_encoding_slot, value_str),
                .connection => headers_obj.setSlot(shapes.request_headers.connection_slot, value_str),
                else => try self.ctx.setPropertyChecked(headers_obj, key_atom, value_str),
            }
        }
        req_obj.setSlot(shapes.request.headers_slot, headers_obj.toValue());

        return req_obj.toValue();
    }

    /// Fallback for creating request objects when http_shapes is not available.
    /// This is slower than the shaped path but handles edge cases.
    fn createRequestObjectDynamic(self: *Self, request: HttpRequestView) !zq.JSValue {
        const req_obj = try self.ctx.createObject(self.request_prototype);

        const url_str = try self.ctx.createString(request.url);
        try self.ctx.setPropertyChecked(req_obj, zq.Atom.url, url_str);

        const method_val: zq.JSValue = if (self.ctx.getCachedMethod(request.method)) |cached|
            zq.JSValue.fromPtr(cached)
        else
            try self.ctx.createString(request.method);
        try self.ctx.setPropertyChecked(req_obj, zq.Atom.method, method_val);

        const path_slice = if (request.path.len > 0) request.path else request.url;
        const path_val: zq.JSValue = if (std.mem.eql(u8, path_slice, request.url))
            url_str
        else
            try self.ctx.createString(path_slice);
        try self.ctx.setPropertyChecked(req_obj, zq.Atom.path, path_val);

        const query_obj = try buildQueryObject(self.ctx, request.query_params);
        try self.ctx.setPropertyChecked(req_obj, zq.Atom.query, query_obj.toValue());

        if (request.body) |body| {
            const body_str = try self.ctx.createString(body);
            try self.ctx.setPropertyChecked(req_obj, zq.Atom.body, body_str);
        }

        const headers_obj = try self.ctx.createObject(self.headers_prototype);
        for (request.headers.items) |header| {
            const key_atom = headerKeyToAtom(header.key) orelse
                try self.ctx.atoms.intern(header.key);
            const value_str = try self.ctx.createString(header.value);
            try self.ctx.setPropertyChecked(headers_obj, key_atom, value_str);
        }
        try self.ctx.setPropertyChecked(req_obj, zq.Atom.headers, headers_obj.toValue());

        return req_obj.toValue();
    }

    fn createString(self: *Self, str: []const u8) !zq.JSValue {
        return self.ctx.createString(str);
    }

    fn callFunction(self: *Self, func_obj: *zq.JSObject, args: []const zq.JSValue) !zq.JSValue {
        // Get bytecode function data
        const bc_data = func_obj.getBytecodeFunctionData() orelse {
            // Try native function
            const native_data = func_obj.getNativeFunctionData() orelse return error.NotCallable;
            return native_data.func(self.ctx, zq.JSValue.undefined_val, args);
        };

        const func_bc = bc_data.bytecode;
        return try self.interpreter.callBytecodeFunction(
            func_obj.toValue(),
            func_bc,
            zq.JSValue.undefined_val,
            args,
        );
    }

    /// Wrapper for calling JS functions from http.zig (used for JSX function components)
    fn callFunctionWrapper(func_obj: *zq.JSObject, args: []const zq.JSValue) anyerror!zq.JSValue {
        const runtime = current_runtime orelse return error.NoRuntime;
        return runtime.callFunction(func_obj, args);
    }

    fn extractResponse(self: *Self, result: zq.JSValue) !HttpResponse {
        return self.extractResponseInternal(result, false);
    }

    fn extractResponseInternal(self: *Self, result: zq.JSValue, borrow_body: bool) !HttpResponse {
        var response = HttpResponse.init(self.allocator);

        if (!result.isObject()) {
            // If result is a string, use it as body
            if (result.isString()) {
                const str = result.toPtr(zq.JSString);
                if (borrow_body) {
                    response.setBodyBorrowed(str.data(), @ptrCast(str));
                } else {
                    const owned = try self.allocator.dupe(u8, str.data());
                    response.setBodyOwned(owned);
                }
            }
            return response;
        }

        const result_obj = result.toPtr(zq.JSObject);
        const pool = self.ctx.hidden_class_pool orelse return response;

        // Fast path: use direct slot access if response matches pre-shaped class
        if (self.ctx.http_shapes) |shapes| {
            if (result_obj.hidden_class_idx == shapes.response.class_idx) {
                // Status - direct slot access with bounds validation
                const status_val = result_obj.getSlot(shapes.response.status_slot);
                if (status_val.isInt()) {
                    const raw = status_val.getInt();
                    response.status = if (raw < 100 or raw > 599) 500 else @intCast(raw);
                }

                // Body - direct slot access
                const body_val = result_obj.getSlot(shapes.response.body_slot);
                if (body_val.isString()) {
                    const str = body_val.toPtr(zq.JSString);
                    if (borrow_body) {
                        response.setBodyBorrowed(str.data(), @ptrCast(str));
                    } else {
                        const owned = try self.allocator.dupe(u8, str.data());
                        response.setBodyOwned(owned);
                    }
                }

                // Headers - still need property iteration for custom headers
                const headers_val = result_obj.getSlot(shapes.response.headers_slot);
                if (headers_val.isObject()) {
                    const headers_obj = headers_val.toPtr(zq.JSObject);
                    if (headers_obj.hidden_class_idx == shapes.response_headers.class_idx) {
                        const ct_val = headers_obj.getSlot(shapes.response_headers.content_type_slot);
                        if (ct_val.isString()) {
                            const str = ct_val.toPtr(zq.JSString);
                            if (borrow_body) {
                                try response.putHeaderBorrowedRuntime("Content-Type", str.data());
                            } else {
                                try response.putHeader("Content-Type", str.data());
                            }
                        }

                        const cl_val = headers_obj.getSlot(shapes.response_headers.content_length_slot);
                        if (cl_val.isString()) {
                            const str = cl_val.toPtr(zq.JSString);
                            if (borrow_body) {
                                try response.putHeaderBorrowedRuntime("content-length", str.data());
                            } else {
                                try response.putHeader("content-length", str.data());
                            }
                        }

                        const cc_val = headers_obj.getSlot(shapes.response_headers.cache_control_slot);
                        if (cc_val.isString()) {
                            const str = cc_val.toPtr(zq.JSString);
                            if (borrow_body) {
                                try response.putHeaderBorrowedRuntime("cache-control", str.data());
                            } else {
                                try response.putHeader("cache-control", str.data());
                            }
                        }
                    } else {
                        const keys = try headers_obj.getOwnEnumerableKeys(self.allocator, pool);
                        defer self.allocator.free(keys);
                        for (keys) |key_atom| {
                            const key_name = self.ctx.atoms.getName(key_atom) orelse continue;
                            const header_val = headers_obj.getOwnProperty(pool, key_atom) orelse continue;
                            if (header_val.isString()) {
                                const str = header_val.toPtr(zq.JSString);
                                if (borrow_body) {
                                    try response.putHeaderBorrowedRuntime(key_name, str.data());
                                } else {
                                    try response.putHeader(key_name, str.data());
                                }
                            }
                        }
                    }
                }

                return response;
            }
        }

        // Fallback: property-based extraction for non-standard response objects
        if (result_obj.getOwnProperty(pool, zq.Atom.status)) |status_val| {
            if (status_val.isInt()) {
                const raw = status_val.getInt();
                response.status = if (raw < 100 or raw > 599) 500 else @intCast(raw);
            }
        }

        if (result_obj.getOwnProperty(pool, zq.Atom.body)) |body_val| {
            if (body_val.isString()) {
                const str = body_val.toPtr(zq.JSString);
                if (borrow_body) {
                    response.setBodyBorrowed(str.data(), @ptrCast(str));
                } else {
                    const owned = try self.allocator.dupe(u8, str.data());
                    response.setBodyOwned(owned);
                }
            }
        }

        if (result_obj.getOwnProperty(pool, zq.Atom.headers)) |headers_val| {
            if (headers_val.isObject()) {
                const headers_obj = headers_val.toPtr(zq.JSObject);
                const keys = try headers_obj.getOwnEnumerableKeys(self.allocator, pool);
                defer self.allocator.free(keys);
                for (keys) |key_atom| {
                    const key_name = self.ctx.atoms.getName(key_atom) orelse continue;
                    const header_val = headers_obj.getOwnProperty(pool, key_atom) orelse continue;
                    if (header_val.isString()) {
                        const str = header_val.toPtr(zq.JSString);
                        if (borrow_body) {
                            try response.putHeaderBorrowedRuntime(key_name, str.data());
                        } else {
                            try response.putHeader(key_name, str.data());
                        }
                    }
                }
            }
        }

        return response;
    }

    /// Reset runtime for next request (isolation)
    pub fn resetForNextRequest(self: *Self) void {
        self.ctx.sp = 0;
        self.ctx.call_depth = 0;
        self.ctx.clearException();
        self.consumed_body_objects.clearRetainingCapacity();

        // Reset ephemeral allocations when hybrid allocation is enabled
        if (self.hybrid_state) |h| {
            h.resetEphemeral();
        } else {
            // Trigger minor GC if nursery is half full
            const used = self.gc_state.nursery.used();
            const base_threshold = self.gc_state.config.nursery_size / 2;
            const body_threshold = self.gc_state.config.nursery_size / 4;
            const threshold = if (self.last_request_body_len >= base_threshold) body_threshold else base_threshold;
            if (used > threshold) {
                self.gc_state.minorGC();
            }
            // Interleave major-GC sweep work between requests to reduce pause spikes.
            self.gc_state.runIncrementalGCStep(self.gc_state.config.sweep_chunk_size);
        }
        self.last_request_body_len = 0;

        // Note: We do NOT clear user globals here. The handler code and all its
        // dependencies (functions, constants, component definitions) are loaded
        // once per runtime and must persist across requests. Request isolation
        // is achieved through the handler pool (each request gets a pooled runtime)
        // and stack/exception clearing above.
    }

    fn prepareForPoolRelease(self: *Self) void {
        // Clear zruntime-layer request state that pool.zig's Runtime.reset()
        // does not know about. The engine-level sp/call_depth/exception clear
        // happens inside pool.release() -> Runtime.reset().
        self.consumed_body_objects.clearRetainingCapacity();
        self.last_request_body_len = 0;
    }

    // === GC Tuning Hooks ===

    /// Hint GC about expected request allocation size
    /// Call at request start for large request bodies
    pub fn hintRequestSize(self: *Self, body_len: usize) void {
        self.gc_state.hintRequestSize(body_len);
        self.last_request_body_len = body_len;
    }

    /// Reset GC hints after request completes
    pub fn resetRequestHint(self: *Self) void {
        self.gc_state.resetRequestHint();
    }

    /// Force minor GC if nursery exceeds watermark
    pub fn collectIfAbove(self: *Self, watermark: usize) void {
        self.gc_state.collectIfAbove(watermark);
    }

    /// Get current nursery usage (bytes)
    pub fn getNurseryUsage(self: *const Self) usize {
        return self.gc_state.getNurseryUsage();
    }

    /// Set major GC threshold
    pub fn setMajorGCThreshold(self: *Self, threshold: usize) void {
        self.gc_state.setMajorGCThreshold(threshold);
    }

    /// Get GC statistics
    pub fn getGCStats(self: *const Self) zq.GC.GCStats {
        return self.gc_state.getStats();
    }
};

// ============================================================================
// Native Function Implementations
// ============================================================================

const FetchResponseObjects = struct {
    value: zq.JSValue,
    response: *zq.JSObject,
    headers: *zq.JSObject,
};

const appendEscapedJson = durable_store_mod.appendEscaped;

const unixMillis = zq.trace.unixMillis;

fn getStringData(val: zq.JSValue) ?[]const u8 {
    if (val.isString()) {
        return val.toPtr(zq.JSString).data();
    }
    if (val.isStringSlice()) {
        return val.toPtr(zq.string.SliceString).data();
    }
    if (val.isRope()) {
        const rope = val.toPtr(zq.string.RopeNode);
        if (rope.kind == .leaf) {
            return rope.payload.leaf.data();
        }
    }
    return null;
}

fn getDynamicProperty(ctx: *zq.Context, obj: *zq.JSObject, pool: *const zq.HiddenClassPool, name: []const u8) ?zq.JSValue {
    const atom = ctx.atoms.intern(name) catch return null;
    return obj.getProperty(pool, atom);
}

fn getObjectProperty(ctx: *zq.Context, obj: *zq.JSObject, pool: *const zq.HiddenClassPool, atom: zq.Atom, name: []const u8) ?zq.JSValue {
    if (obj.getProperty(pool, atom)) |value| return value;
    if (getDynamicProperty(ctx, obj, pool, name)) |value| return value;

    const keys = obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return null;
    defer ctx.allocator.free(keys);
    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        if (!std.mem.eql(u8, key_name, name)) continue;
        return obj.getOwnProperty(pool, key_atom);
    }
    return null;
}

fn getHeaderAtom(ctx: *zq.Context, name: []const u8) !zq.Atom {
    if (ascii.eqlIgnoreCase(name, "content-type")) {
        return try ctx.atoms.intern("Content-Type");
    }
    return Runtime.headerKeyToAtom(name) orelse try ctx.atoms.intern(name);
}

const HeaderAssignMode = enum {
    replace,
    append,
};

fn statusTextFor(status: u16) []const u8 {
    return switch (status) {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        500 => "Internal Server Error",
        599 => "Network Connect Timeout Error",
        else => "Unknown",
    };
}

fn normalizeStatus(status_val: zq.JSValue, default_status: u16) u16 {
    if (!status_val.isInt()) return default_status;
    const raw = status_val.getInt();
    if (raw < 100 or raw > 599) return 500;
    return @intCast(raw);
}

fn getGlobalPrototype(ctx: *zq.Context, ctor_atom: zq.Atom) ?*zq.JSObject {
    const ctor_val = ctx.getGlobal(ctor_atom) orelse return null;
    if (!ctor_val.isObject()) return null;
    const pool = ctx.hidden_class_pool orelse return null;
    const ctor_obj = ctor_val.toPtr(zq.JSObject);
    const proto_val = ctor_obj.getProperty(pool, zq.Atom.prototype) orelse return null;
    if (!proto_val.isObject()) return null;
    return proto_val.toPtr(zq.JSObject);
}

fn getNamedGlobalPrototype(ctx: *zq.Context, name: []const u8) ?*zq.JSObject {
    const atom = ctx.atoms.intern(name) catch return null;
    return getGlobalPrototype(ctx, atom);
}

fn throwTypeError(ctx: *zq.Context, message: []const u8) zq.JSValue {
    const message_val = ctx.createString(message) catch {
        ctx.throwException(zq.JSValue.exception_val);
        return zq.JSValue.exception_val;
    };
    const args = [_]zq.JSValue{message_val};
    const err_obj = zq.builtins.typeErrorConstructor(ctx, zq.JSValue.undefined_val, &args);
    ctx.throwException(err_obj);
    return zq.JSValue.exception_val;
}

fn beginBodyRead(ctx: *zq.Context, this: zq.JSValue) zq.JSValue {
    if (!this.isObject()) {
        return throwTypeError(ctx, "Body reader target must be an object");
    }
    const obj = this.toPtr(zq.JSObject);
    if (current_runtime) |rt| {
        if (rt.consumed_body_objects.contains(obj)) {
            return throwTypeError(ctx, "Body has already been consumed");
        }
        rt.consumed_body_objects.put(rt.allocator, obj, {}) catch {
            return throwTypeError(ctx, "Failed to track body consumption");
        };
    }
    return getBodyValue(ctx, this) orelse zq.JSValue.undefined_val;
}

fn findHeaderPropertyAtom(
    ctx: *zq.Context,
    headers_obj: *zq.JSObject,
    pool: *const zq.HiddenClassPool,
    wanted: []const u8,
) ?zq.Atom {
    if (Runtime.headerKeyToAtom(wanted)) |known_atom| {
        if (headers_obj.getOwnProperty(pool, known_atom) != null) return known_atom;
    }
    if (ascii.eqlIgnoreCase(wanted, "content-type")) {
        const ct_atom = ctx.atoms.intern("Content-Type") catch return null;
        if (headers_obj.getOwnProperty(pool, ct_atom) != null) return ct_atom;
    }
    const keys = headers_obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return null;
    defer ctx.allocator.free(keys);
    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        if (ascii.eqlIgnoreCase(key_name, wanted)) return key_atom;
    }
    return null;
}

fn getHeaderValueCaseInsensitive(
    ctx: *zq.Context,
    headers_obj: *zq.JSObject,
    pool: *const zq.HiddenClassPool,
    wanted: []const u8,
) ?zq.JSValue {
    const atom = findHeaderPropertyAtom(ctx, headers_obj, pool, wanted) orelse return null;
    const value = headers_obj.getOwnProperty(pool, atom) orelse return null;
    if (value.isUndefined() or value.isNull()) return null;
    return value;
}

fn validateHeaderPair(name: []const u8, value: []const u8) bool {
    if (name.len == 0) return false;
    if (std.mem.indexOfAny(u8, name, "\r\n") != null) return false;
    if (std.mem.indexOfAny(u8, value, "\r\n") != null) return false;
    return true;
}

fn setHeaderValue(
    ctx: *zq.Context,
    headers_obj: *zq.JSObject,
    name: []const u8,
    value: []const u8,
    mode: HeaderAssignMode,
) !void {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const target_atom = findHeaderPropertyAtom(ctx, headers_obj, pool, name) orelse try getHeaderAtom(ctx, name);

    const final_value = switch (mode) {
        .replace => try ctx.createString(value),
        .append => blk: {
            if (getHeaderValueCaseInsensitive(ctx, headers_obj, pool, name)) |existing_val| {
                if (getStringData(existing_val)) |existing_str| {
                    const combined = try std.fmt.allocPrint(ctx.allocator, "{s}, {s}", .{ existing_str, value });
                    defer ctx.allocator.free(combined);
                    break :blk try ctx.createString(combined);
                }
            }
            break :blk try ctx.createString(value);
        },
    };
    try ctx.setPropertyChecked(headers_obj, target_atom, final_value);
}

fn createHeadersObjectDynamic(ctx: *zq.Context) !*zq.JSObject {
    return ctx.createObject(getNamedGlobalPrototype(ctx, "Headers"));
}

fn copyHeadersIntoObject(ctx: *zq.Context, src_val: zq.JSValue, dst_obj: *zq.JSObject) !void {
    if (!src_val.isObject()) return error.InvalidHeaders;

    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const src_obj = src_val.toPtr(zq.JSObject);
    const keys = try src_obj.getOwnEnumerableKeys(ctx.allocator, pool);
    defer ctx.allocator.free(keys);

    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        const value = src_obj.getOwnProperty(pool, key_atom) orelse continue;
        if (value.isUndefined() or value.isNull()) continue;
        const header_str = getStringData(value) orelse return error.InvalidHeaders;
        if (!validateHeaderPair(key_name, header_str)) return error.InvalidHeaders;
        try setHeaderValue(ctx, dst_obj, key_name, header_str, .replace);
    }
}

fn splitPathAndQuery(url: []const u8) struct {
    path: []const u8,
    query_string: []const u8,
} {
    if (std.mem.indexOfScalar(u8, url, '?')) |idx| {
        return .{
            .path = url[0..idx],
            .query_string = url[idx + 1 ..],
        };
    }
    return .{
        .path = url,
        .query_string = "",
    };
}

fn buildQueryObject(ctx: *zq.Context, query_params: []const QueryParam) !*zq.JSObject {
    const query_obj = try ctx.createObject(null);
    for (query_params) |param| {
        const key_atom = try ctx.atoms.intern(param.key);
        const param_val = if (Runtime.parseQueryInt(param.value)) |int_val|
            zq.JSValue.fromInt(int_val)
        else
            try ctx.createString(param.value);
        try ctx.setPropertyChecked(query_obj, key_atom, param_val);
    }
    return query_obj;
}

fn upgradeResponseValue(ctx: *zq.Context, value: zq.JSValue) !zq.JSValue {
    if (!value.isObject()) return value;

    const response_obj = value.toPtr(zq.JSObject);
    response_obj.prototype = getGlobalPrototype(ctx, .Response);

    const pool = ctx.hidden_class_pool orelse return value;
    if (response_obj.getProperty(pool, zq.Atom.headers)) |headers_val| {
        if (headers_val.isObject()) {
            headers_val.toPtr(zq.JSObject).prototype = getNamedGlobalPrototype(ctx, "Headers");
        }
    }
    return value;
}

const OwnedResponseHeader = struct {
    name: []u8,
    value: []u8,
};

const OwnedResponseHead = struct {
    reason: []u8,
    headers: std.ArrayListUnmanaged(OwnedResponseHeader) = .empty,

    fn deinit(self: *OwnedResponseHead, allocator: std.mem.Allocator) void {
        allocator.free(self.reason);
        for (self.headers.items) |header| {
            allocator.free(header.name);
            allocator.free(header.value);
        }
        self.headers.deinit(allocator);
    }

    fn contentType(self: *const OwnedResponseHead) ?[]const u8 {
        for (self.headers.items) |header| {
            if (ascii.eqlIgnoreCase(header.name, "content-type")) {
                return header.value;
            }
        }
        return null;
    }
};

fn snapshotResponseHead(allocator: std.mem.Allocator, head: anytype) !OwnedResponseHead {
    var owned = OwnedResponseHead{
        .reason = try allocator.dupe(u8, head.reason),
    };
    errdefer owned.deinit(allocator);

    var header_it = head.iterateHeaders();
    while (header_it.next()) |header| {
        const name = try allocator.dupe(u8, header.name);
        errdefer allocator.free(name);
        const value = try allocator.dupe(u8, header.value);
        errdefer allocator.free(value);
        try owned.headers.append(allocator, .{
            .name = name,
            .value = value,
        });
    }

    return owned;
}

fn createResponseHeadersObject(rt: *Runtime) !*zq.JSObject {
    if (rt.ctx.http_shapes) |shapes| {
        return rt.ctx.createObjectWithClass(shapes.response_headers.class_idx, rt.headers_prototype);
    }
    return rt.ctx.createObject(rt.headers_prototype);
}

fn createFetchResponse(rt: *Runtime, status: u16, status_text: []const u8, body: []const u8, content_type: ?[]const u8) !FetchResponseObjects {
    const body_val = try rt.ctx.createString(body);
    const body_str = body_val.toPtr(zq.JSString);
    const response_val = try zq.http.createResponseFromString(
        rt.ctx,
        body_str,
        status,
        content_type orelse "application/octet-stream",
    );
    const response_obj = response_val.toPtr(zq.JSObject);
    if (rt.response_prototype) |proto| {
        response_obj.prototype = proto;
    }

    const status_text_atom = if (rt.ctx.http_strings) |cache|
        cache.status_text_atom
    else
        try rt.ctx.atoms.intern("statusText");
    const status_text_val = try rt.ctx.createString(status_text);
    try rt.ctx.setPropertyChecked(response_obj, status_text_atom, status_text_val);

    const headers_obj = try createResponseHeadersObject(rt);
    try rt.ctx.setPropertyChecked(response_obj, zq.Atom.headers, headers_obj.toValue());

    if (content_type) |ct| {
        const ct_atom = try getHeaderAtom(rt.ctx, "content-type");
        const ct_val = try rt.ctx.createString(ct);
        try rt.ctx.setPropertyChecked(headers_obj, ct_atom, ct_val);
    }

    return .{
        .value = response_val,
        .response = response_obj,
        .headers = headers_obj,
    };
}

fn createFetchErrorResponse(rt: *Runtime, err_code: []const u8, details: []const u8) !zq.JSValue {
    const error_body = try httpRequestErrorJsonAlloc(rt.allocator, err_code, details);
    defer rt.allocator.free(error_body);

    const created = try createFetchResponse(rt, 599, err_code, error_body, "application/json");
    const error_atom = try rt.ctx.atoms.intern("error");
    const details_atom = try rt.ctx.atoms.intern("details");
    const error_val = try rt.ctx.createString(err_code);
    const details_val = try rt.ctx.createString(details);
    try rt.ctx.setPropertyChecked(created.response, error_atom, error_val);
    try rt.ctx.setPropertyChecked(created.response, details_atom, details_val);
    return created.value;
}

fn getBodyValue(ctx: *zq.Context, this: zq.JSValue) ?zq.JSValue {
    if (!this.isObject()) return null;
    const pool = ctx.hidden_class_pool orelse return null;
    const obj = this.toPtr(zq.JSObject);
    return obj.getProperty(pool, zq.Atom.body);
}

fn bodyTextNative(ctx_ptr: *anyopaque, this: zq.JSValue, _: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    const body_val = beginBodyRead(ctx, this);
    if (ctx.hasException()) return zq.JSValue.exception_val;
    if (body_val.isNull() or body_val.isUndefined()) {
        return ctx.createString("");
    }
    if (body_val.isAnyString()) {
        return body_val;
    }
    return ctx.createString("");
}

fn bodyJsonNative(ctx_ptr: *anyopaque, this: zq.JSValue, _: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    const body_val = beginBodyRead(ctx, this);
    if (ctx.hasException()) return zq.JSValue.exception_val;
    if (body_val.isNull() or body_val.isUndefined()) {
        return zq.JSValue.undefined_val;
    }
    const body = getStringData(body_val) orelse return zq.JSValue.undefined_val;
    return zq.builtins.parseJsonValue(ctx, body) catch zq.JSValue.undefined_val;
}

fn headersGetNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    if (!this.isObject() or args.len == 0) return zq.JSValue.undefined_val;

    const wanted = getStringData(args[0]) orelse return zq.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return zq.JSValue.undefined_val;
    const headers_obj = this.toPtr(zq.JSObject);
    return getHeaderValueCaseInsensitive(ctx, headers_obj, pool, wanted) orelse zq.JSValue.undefined_val;
}

fn headersHasNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    if (!this.isObject() or args.len == 0) return zq.JSValue.false_val;

    const wanted = getStringData(args[0]) orelse return zq.JSValue.false_val;
    const pool = ctx.hidden_class_pool orelse return zq.JSValue.false_val;
    const headers_obj = this.toPtr(zq.JSObject);
    return zq.JSValue.fromBool(getHeaderValueCaseInsensitive(ctx, headers_obj, pool, wanted) != null);
}

fn headersSetLikeNative(
    ctx: *zq.Context,
    this: zq.JSValue,
    args: []const zq.JSValue,
    mode: HeaderAssignMode,
) !zq.JSValue {
    if (!this.isObject()) return throwTypeError(ctx, "Headers target must be an object");
    if (args.len < 2) return throwTypeError(ctx, "Headers mutation requires name and value");

    const name = getStringData(args[0]) orelse return throwTypeError(ctx, "Header name must be string");
    const value = getStringData(args[1]) orelse return throwTypeError(ctx, "Header value must be string");
    if (!validateHeaderPair(name, value)) {
        return throwTypeError(ctx, "Header name/value is invalid");
    }
    try setHeaderValue(ctx, this.toPtr(zq.JSObject), name, value, mode);
    return zq.JSValue.undefined_val;
}

fn headersSetNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return headersSetLikeNative(ctx, this, args, .replace);
}

fn headersAppendNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return headersSetLikeNative(ctx, this, args, .append);
}

fn headersDeleteNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    if (!this.isObject() or args.len == 0) return zq.JSValue.undefined_val;

    const wanted = getStringData(args[0]) orelse return zq.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return zq.JSValue.undefined_val;
    const headers_obj = this.toPtr(zq.JSObject);
    const keys = headers_obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return zq.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        if (!ascii.eqlIgnoreCase(key_name, wanted)) continue;
        _ = headers_obj.deleteProperty(pool, key_atom);
    }
    return zq.JSValue.undefined_val;
}

fn headersConstructorNative(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    const headers_obj = try createHeadersObjectDynamic(ctx);

    if (args.len > 0 and !args[0].isUndefined() and !args[0].isNull()) {
        copyHeadersIntoObject(ctx, args[0], headers_obj) catch |err| switch (err) {
            error.InvalidHeaders => return throwTypeError(ctx, "Headers(init) expects object<string,string>"),
            else => return err,
        };
    }

    return headers_obj.toValue();
}

fn requestConstructorNative(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));

    const url = if (args.len > 0) getStringData(args[0]) else null;
    if (url == null or url.?.len == 0) {
        return throwTypeError(ctx, "Request(url, init?) expects a non-empty string url");
    }

    var method: []const u8 = "GET";
    var body: ?[]const u8 = null;
    const headers_obj = try createHeadersObjectDynamic(ctx);

    if (args.len > 1 and args[1].isObject()) {
        const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
        const init = args[1].toPtr(zq.JSObject);

        if (getObjectProperty(ctx, init, pool, zq.Atom.method, "method")) |method_val| {
            method = getStringData(method_val) orelse return throwTypeError(ctx, "Request method must be string");
        }
        if (getObjectProperty(ctx, init, pool, zq.Atom.body, "body")) |body_val| {
            if (body_val.isNull() or body_val.isUndefined()) {
                body = null;
            } else {
                body = getStringData(body_val) orelse return throwTypeError(ctx, "Request body must be string|null");
            }
        }
        if (getObjectProperty(ctx, init, pool, zq.Atom.headers, "headers")) |headers_val| {
            copyHeadersIntoObject(ctx, headers_val, headers_obj) catch |err| switch (err) {
                error.InvalidHeaders => return throwTypeError(ctx, "Request headers must be object<string,string>"),
                else => return err,
            };
        }
    }

    const split = splitPathAndQuery(url.?);
    const parsed_query = try http_parser.parseQueryString(ctx.allocator, split.query_string);
    defer if (parsed_query.storage) |storage| ctx.allocator.free(storage);
    defer if (parsed_query.decoded_storage) |storage| ctx.allocator.free(storage);

    const req_obj = try ctx.createObject(getNamedGlobalPrototype(ctx, "Request"));
    const url_val = try ctx.createString(url.?);
    try ctx.setPropertyChecked(req_obj, zq.Atom.url, url_val);
    try ctx.setPropertyChecked(req_obj, zq.Atom.method, try ctx.createString(method));
    try ctx.setPropertyChecked(req_obj, zq.Atom.path, try ctx.createString(split.path));
    try ctx.setPropertyChecked(req_obj, zq.Atom.query, (try buildQueryObject(ctx, parsed_query.params)).toValue());
    if (body) |body_str| {
        try ctx.setPropertyChecked(req_obj, zq.Atom.body, try ctx.createString(body_str));
    } else {
        try ctx.setPropertyChecked(req_obj, zq.Atom.body, zq.JSValue.undefined_val);
    }
    try ctx.setPropertyChecked(req_obj, zq.Atom.headers, headers_obj.toValue());

    return req_obj.toValue();
}

fn responseConstructorNative(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    const response_val = try zq.http.responseConstructor(ctx, zq.JSValue.undefined_val, args);
    if (ctx.hasException()) return error.NativeFunctionError;

    const upgraded = try upgradeResponseValue(ctx, response_val);
    if (args.len < 2 or !args[1].isObject()) return upgraded;

    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const init = args[1].toPtr(zq.JSObject);
    const headers_val = getObjectProperty(ctx, init, pool, zq.Atom.headers, "headers") orelse return upgraded;

    const response_obj = upgraded.toPtr(zq.JSObject);
    const headers_obj = try createHeadersObjectDynamic(ctx);
    if (response_obj.getProperty(pool, zq.Atom.headers)) |existing_headers_val| {
        if (existing_headers_val.isObject()) {
            try copyHeadersIntoObject(ctx, existing_headers_val, headers_obj);
        }
    }
    copyHeadersIntoObject(ctx, headers_val, headers_obj) catch |err| switch (err) {
        error.InvalidHeaders => return throwTypeError(ctx, "Response headers must be object<string,string>"),
        else => return err,
    };
    try ctx.setPropertyChecked(response_obj, zq.Atom.headers, headers_obj.toValue());
    return upgraded;
}

fn wrapResponseStatic(
    ctx: *zq.Context,
    this: zq.JSValue,
    args: []const zq.JSValue,
    func: *const fn (*anyopaque, zq.JSValue, []const zq.JSValue) anyerror!zq.JSValue,
) !zq.JSValue {
    const result = try func(ctx, this, args);
    return upgradeResponseValue(ctx, result);
}

fn responseJsonStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseJson);
}

fn responseTextStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseText);
}

fn responseHtmlStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseHtml);
}

fn responseRawJsonStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseRawJson);
}

fn responseRedirectStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseRedirect);
}

/// Split a slice of headers (any struct with .key/.value) into parallel
/// name/value arrays for trace recording. Returns the count written.
fn splitHeaderKV(headers: anytype, names: *[64][]const u8, values: *[64][]const u8) usize {
    const count = @min(headers.len, 64);
    for (headers[0..count], 0..) |hdr, i| {
        names[i] = hdr.key;
        values[i] = hdr.value;
    }
    return count;
}

fn outboundHostViolation(rt: *Runtime, host: []const u8) ?[]const u8 {
    if (rt.config.outbound_allow_host) |allowed_host| {
        if (!ascii.eqlIgnoreCase(host, allowed_host)) {
            return allowed_host;
        }
    }
    if (!rt.ctx.capability_policy.allowsEgressHost(host)) {
        return "capability policy";
    }
    return null;
}

fn fetchSyncNative(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const rt = current_runtime orelse return error.RuntimeUnavailable;
    const result = fetchSyncResult(rt, args) catch |err| {
        return createFetchErrorResponse(rt, "InternalError", @errorName(err));
    };

    // Record fetchSync to trace (it's outside virtual module dispatch)
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    if (ctx.getModuleState(zq.TraceRecorder, zq.TRACE_STATE_SLOT)) |recorder| {
        recorder.recordIO("http", "fetchSync", ctx, args, result);
    }

    return result;
}

/// Parsed fetch arguments: URL, URI, and optional init object.
/// Shared by fetchSyncResult and collectFetchForParallel.
const FetchArgs = struct {
    url: []const u8,
    uri: std.Uri,
    init_obj: ?*zq.JSObject,
};

/// Parse and validate the (url, init?) arguments common to all fetch call sites.
/// Returns a JS error response (status 599) on validation failure.
fn parseFetchArgs(rt: *Runtime, pool: *const zq.HiddenClassPool, args: []const zq.JSValue) !union(enum) {
    ok: FetchArgs,
    err: zq.JSValue,
} {
    if (args.len == 0) {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidArgs", "expected url string or init object") };
    }

    var init_obj: ?*zq.JSObject = null;
    const url = blk: {
        if (args[0].isObject()) {
            init_obj = args[0].toPtr(zq.JSObject);
            const url_val = getObjectProperty(rt.ctx, init_obj.?, pool, zq.Atom.url, "url") orelse {
                return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "missing url field") };
            };
            const url_str = getStringData(url_val) orelse {
                return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url must be a non-empty string") };
            };
            if (url_str.len == 0) {
                return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url must be a non-empty string") };
            }
            break :blk url_str;
        }
        const url_str = getStringData(args[0]) orelse {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidArgs", "expected url string or init object") };
        };
        if (url_str.len == 0) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url must be a non-empty string") };
        }
        if (args.len > 1 and args[1].isObject()) {
            init_obj = args[1].toPtr(zq.JSObject);
        }
        break :blk url_str;
    };

    const uri = std.Uri.parse(url) catch {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url parse failed") };
    };
    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = uri.getHost(&host_buf) catch {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url host is required") };
    };
    if (outboundHostViolation(rt, host.bytes)) |details| {
        return .{ .err = try createFetchErrorResponse(rt, "HostNotAllowed", details) };
    }

    return .{ .ok = .{ .url = url, .uri = uri, .init_obj = init_obj } };
}

fn fetchSyncResult(rt: *Runtime, args: []const zq.JSValue) !zq.JSValue {
    if (!rt.config.outbound_http_enabled) {
        return createFetchErrorResponse(rt, "OutboundHttpDisabled", "set runtime outbound_http_enabled=true");
    }

    if (zq.modules.io.parallel_collector) |collector| {
        return collectFetchForParallel(rt, collector, args);
    }

    const pool = rt.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    const parsed = try parseFetchArgs(rt, pool, args);
    const fetch_args = switch (parsed) {
        .ok => |fa| fa,
        .err => |err_val| return err_val,
    };
    const uri = fetch_args.uri;
    const init_obj = fetch_args.init_obj;

    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = uri.getHost(&host_buf) catch {
        return createFetchErrorResponse(rt, "InvalidUrl", "url host is required");
    };

    var method = std.http.Method.GET;
    var body: ?[]const u8 = null;
    var max_response_bytes = @max(@as(usize, 1), rt.config.outbound_max_response_bytes);
    var headers = std.array_list.Managed(std.http.Header).init(rt.allocator);
    defer headers.deinit();

    if (init_obj) |init| {
        if (getObjectProperty(rt.ctx, init, pool, zq.Atom.method, "method")) |method_val| {
            const method_name = getStringData(method_val) orelse {
                return createFetchErrorResponse(rt, "InvalidMethod", "method must be string");
            };
            method = parseHttpMethod(method_name) orelse {
                return createFetchErrorResponse(rt, "InvalidMethod", method_name);
            };
        }

        if (getObjectProperty(rt.ctx, init, pool, zq.Atom.body, "body")) |body_val| {
            if (body_val.isNull() or body_val.isUndefined()) {
                body = null;
            } else {
                body = getStringData(body_val) orelse {
                    return createFetchErrorResponse(rt, "InvalidBody", "body must be string|null");
                };
            }
        }

        if (getDynamicProperty(rt.ctx, init, pool, "maxResponseBytes")) |max_val| {
            if (!max_val.isInt()) {
                return createFetchErrorResponse(rt, "InvalidMaxResponseBytes", "maxResponseBytes must be integer");
            }
            const req_max = std.math.cast(usize, max_val.getInt()) orelse {
                return createFetchErrorResponse(rt, "InvalidMaxResponseBytes", "maxResponseBytes out of range");
            };
            max_response_bytes = @min(max_response_bytes, @max(@as(usize, 1), req_max));
        } else if (getDynamicProperty(rt.ctx, init, pool, "max_response_bytes")) |max_val| {
            if (!max_val.isInt()) {
                return createFetchErrorResponse(rt, "InvalidMaxResponseBytes", "max_response_bytes must be integer");
            }
            const req_max = std.math.cast(usize, max_val.getInt()) orelse {
                return createFetchErrorResponse(rt, "InvalidMaxResponseBytes", "max_response_bytes out of range");
            };
            max_response_bytes = @min(max_response_bytes, @max(@as(usize, 1), req_max));
        }

        if (getObjectProperty(rt.ctx, init, pool, zq.Atom.headers, "headers")) |headers_val| {
            if (!headers_val.isObject()) {
                return createFetchErrorResponse(rt, "InvalidHeaders", "headers must be object<string,string>");
            }
            const headers_obj = headers_val.toPtr(zq.JSObject);
            const keys = try headers_obj.getOwnEnumerableKeys(rt.allocator, pool);
            defer rt.allocator.free(keys);

            for (keys) |key_atom| {
                const key_name = rt.ctx.atoms.getName(key_atom) orelse continue;
                const header_val = headers_obj.getOwnProperty(pool, key_atom) orelse continue;
                const header_str = getStringData(header_val) orelse {
                    return createFetchErrorResponse(rt, "InvalidHeaders", "header values must be strings");
                };
                if (key_name.len == 0) {
                    return createFetchErrorResponse(rt, "InvalidHeaders", "header name must be non-empty");
                }
                if (std.mem.indexOfAny(u8, key_name, "\r\n") != null) {
                    return createFetchErrorResponse(rt, "InvalidHeaders", "header name contains newline");
                }
                if (std.mem.indexOfAny(u8, header_str, "\r\n") != null) {
                    return createFetchErrorResponse(rt, "InvalidHeaders", "header value contains newline");
                }
                try headers.append(.{ .name = key_name, .value = header_str });
            }
        }
    }

    var client = std.http.Client{
        .allocator = rt.allocator,
        .io = rt.outbound_io_backend.?.io(),
    };
    defer client.deinit();

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse {
        return createFetchErrorResponse(rt, "InvalidUrl", "unsupported URI scheme");
    };
    const timeout: std.Io.Timeout = if (rt.config.outbound_timeout_ms == 0) .none else blk: {
        const duration = std.Io.Duration.fromMilliseconds(@intCast(rt.config.outbound_timeout_ms));
        break :blk .{ .duration = .{ .raw = duration, .clock = .awake } };
    };
    const connection = client.connectTcpOptions(.{
        .host = host,
        .port = uri.port orelse switch (protocol) {
            .plain => 80,
            .tls => 443,
        },
        .protocol = protocol,
        .timeout = timeout,
    }) catch |err| {
        return createFetchErrorResponse(rt, "ConnectFailed", @errorName(err));
    };

    var req = client.request(method, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        .extra_headers = headers.items,
    }) catch |err| {
        client.connection_pool.release(connection, client.io);
        return createFetchErrorResponse(rt, "RequestInitFailed", @errorName(err));
    };
    defer req.deinit();

    if (body) |payload| {
        req.transfer_encoding = .{ .content_length = payload.len };
        var request_body = req.sendBodyUnflushed(&.{}) catch |err| {
            return createFetchErrorResponse(rt, "RequestSendFailed", @errorName(err));
        };
        request_body.writer.writeAll(payload) catch |err| {
            return createFetchErrorResponse(rt, "RequestSendFailed", @errorName(err));
        };
        request_body.end() catch |err| {
            return createFetchErrorResponse(rt, "RequestSendFailed", @errorName(err));
        };
        req.connection.?.flush() catch |err| {
            return createFetchErrorResponse(rt, "RequestSendFailed", @errorName(err));
        };
    } else {
        req.sendBodiless() catch |err| {
            return createFetchErrorResponse(rt, "RequestSendFailed", @errorName(err));
        };
    }

    var response = req.receiveHead(&.{}) catch |err| {
        return createFetchErrorResponse(rt, "ResponseHeadFailed", @errorName(err));
    };
    const status = @intFromEnum(response.head.status);
    var owned_head = try snapshotResponseHead(rt.allocator, response.head);
    defer owned_head.deinit(rt.allocator);

    var body_transfer: [64]u8 = undefined;
    var response_reader = response.reader(&body_transfer);
    const response_body = response_reader.allocRemaining(rt.allocator, std.Io.Limit.limited(max_response_bytes)) catch |err| switch (err) {
        error.StreamTooLong => return createFetchErrorResponse(rt, "ResponseTooLarge", "response exceeded max_response_bytes"),
        else => return createFetchErrorResponse(rt, "ResponseReadFailed", @errorName(err)),
    };
    defer rt.allocator.free(response_body);

    const created = try createFetchResponse(rt, status, owned_head.reason, response_body, owned_head.contentType());
    for (owned_head.headers.items) |header| {
        const key_atom = try getHeaderAtom(rt.ctx, header.name);
        const value_str = try rt.ctx.createString(header.value);
        try rt.ctx.setPropertyChecked(created.headers, key_atom, value_str);
    }

    return created.value;
}

// ============================================================================
// Parallel I/O support
// ============================================================================

/// Intercept a fetchSync call during parallel collection mode.
/// Records the URL/method/body/headers as a FetchDescriptor instead of
/// performing the actual HTTP request.
fn collectFetchForParallel(rt: *Runtime, collector: *zq.modules.io.ParallelCollector, args: []const zq.JSValue) !zq.JSValue {
    if (collector.count >= collector.capacity) {
        return createFetchErrorResponse(rt, "ParallelOverflow", "too many fetchSync calls in parallel thunk");
    }

    const pool = rt.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const a = collector.allocator;

    const parsed = try parseFetchArgs(rt, pool, args);
    const fetch_args = switch (parsed) {
        .ok => |fa| fa,
        .err => |err_val| return err_val,
    };
    const url = fetch_args.url;
    const init_obj = fetch_args.init_obj;

    var method = std.http.Method.GET;
    var body: ?[]const u8 = null;
    var max_response_bytes = @max(@as(usize, 1), rt.config.outbound_max_response_bytes);
    var headers_list: std.ArrayList(std.http.Header) = .empty;

    if (init_obj) |init| {
        if (getObjectProperty(rt.ctx, init, pool, zq.Atom.method, "method")) |method_val| {
            const method_name = getStringData(method_val) orelse {
                return createFetchErrorResponse(rt, "InvalidMethod", "method must be string");
            };
            method = parseHttpMethod(method_name) orelse {
                return createFetchErrorResponse(rt, "InvalidMethod", method_name);
            };
        }
        if (getObjectProperty(rt.ctx, init, pool, zq.Atom.body, "body")) |body_val| {
            if (!body_val.isNull() and !body_val.isUndefined()) {
                body = getStringData(body_val);
            }
        }
        if (getDynamicProperty(rt.ctx, init, pool, "maxResponseBytes")) |max_val| {
            if (max_val.isInt()) {
                if (std.math.cast(usize, max_val.getInt())) |req_max| {
                    max_response_bytes = @min(max_response_bytes, @max(@as(usize, 1), req_max));
                }
            }
        }
        if (getObjectProperty(rt.ctx, init, pool, zq.Atom.headers, "headers")) |headers_val| {
            if (headers_val.isObject()) {
                const headers_obj = headers_val.toPtr(zq.JSObject);
                const keys = try headers_obj.getOwnEnumerableKeys(a, pool);
                defer a.free(keys);
                for (keys) |key_atom| {
                    const key_name = rt.ctx.atoms.getName(key_atom) orelse continue;
                    const header_val = headers_obj.getOwnProperty(pool, key_atom) orelse continue;
                    const header_str = getStringData(header_val) orelse continue;
                    try headers_list.append(a, .{ .name = key_name, .value = header_str });
                }
            }
        }
    }

    // Store descriptor
    const idx = collector.count;
    collector.descriptors[idx] = .{
        .url = try a.dupe(u8, url),
        .method = method,
        .body = if (body) |b| try a.dupe(u8, b) else null,
        .headers = headers_list,
        .max_response_bytes = max_response_bytes,
    };
    collector.count += 1;

    // Return a placeholder response (status 0, empty body)
    // parallel() replaces this with the real response after concurrent execution.
    return zq.JSValue.undefined_val;
}

/// Call a zero-arg JS function via the interpreter.
/// Used by IoCallbacks.call_thunk_fn and ScopeCallbacks.call0_fn.
fn ioCallThunk(runtime_ptr: *anyopaque, thunk_val: zq.JSValue) anyerror!zq.JSValue {
    return callJsThunk(runtime_ptr, thunk_val, &.{});
}

fn callJsThunk(runtime_ptr: *anyopaque, func_val: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    if (!func_val.isObject()) return error.NotCallable;
    const func_obj = func_val.toPtr(zq.JSObject);
    return rt.callFunction(func_obj, args);
}

fn scopeCall1(
    runtime_ptr: *anyopaque,
    thunk_val: zq.JSValue,
    arg: zq.JSValue,
) anyerror!zq.JSValue {
    const args = [_]zq.JSValue{arg};
    return callJsThunk(runtime_ptr, thunk_val, &args);
}

fn durableRunCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    key: []const u8,
    run_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return rt.durableRun(ctx, key, run_val);
}

fn durableStepCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    name: []const u8,
    step_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return rt.durableStep(ctx, name, step_val);
}

fn durableStepWithTimeoutCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    name: []const u8,
    timeout_ms: i64,
    step_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return rt.durableStepWithTimeout(ctx, name, timeout_ms, step_val);
}

fn durableSleepUntilCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    until_ms: i64,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return rt.durableSleepUntil(ctx, until_ms);
}

fn durableWaitSignalCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    name: []const u8,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return rt.durableWaitSignal(ctx, name);
}

fn durableSignalCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    key: []const u8,
    name: []const u8,
    payload: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return rt.durableSignal(ctx, key, name, payload);
}

fn durableSignalAtCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    key: []const u8,
    name: []const u8,
    at_ms: i64,
    payload: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return rt.durableSignalAt(ctx, key, name, at_ms, payload);
}

const ServiceRoute = struct {
    method_text: []const u8,
    path_pattern: []const u8,
};

/// `zigttp:fetch.fetch(url, init?)` callback. Without `init.durable`
/// this delegates straight to `fetchSync` so trace recording, replay
/// mode, and outbound-host policy enforcement all behave identically
/// across the two surfaces. With `init.durable = { key, retries?,
/// backoff?, ttl_s? }` the call is wrapped in an oplog step under
/// `<durable>/fetch/<hash>.step`: a cached response within TTL is
/// returned without hitting the network; a miss executes the request
/// (with retry on 5xx + connection errors per the backoff policy) and
/// persists the final response.
fn fetchModuleCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    _ = ctx;
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    const pool = rt.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    const init_obj: ?*zq.JSObject = blk: {
        if (args.len >= 1 and args[0].isObject()) break :blk args[0].toPtr(zq.JSObject);
        if (args.len >= 2 and args[1].isObject()) break :blk args[1].toPtr(zq.JSObject);
        break :blk null;
    };

    if (init_obj) |obj| {
        switch (try parseDurableFetchOpts(rt, obj, pool)) {
            .none => {},
            .err => |err_val| return err_val,
            .ok => |opts| return runDurableFetch(rt, args, opts),
        }
    }
    return fetchSyncNative(@ptrCast(rt.ctx), zq.JSValue.undefined_val, args);
}

const DurableFetchOpts = durable_fetch.Options;

const ParsedDurableOpts = union(enum) {
    none,
    ok: DurableFetchOpts,
    err: zq.JSValue,
};

/// Extract `durable: { key, retries?, backoff?, ttl_s? }` from the
/// init object. Returns `.none` when `durable` is absent. Returns
/// `.err` with a JS error-response JSValue when the field is present
/// but malformed — authors who opt in to durable semantics deserve a
/// loud failure, not a silent fallback.
fn parseDurableFetchOpts(
    rt: *Runtime,
    init: *zq.JSObject,
    pool: *const zq.HiddenClassPool,
) !ParsedDurableOpts {
    const durable_val = getDynamicProperty(rt.ctx, init, pool, "durable") orelse return .none;
    if (durable_val.isNull() or durable_val.isUndefined()) return .none;
    if (!durable_val.isObject()) {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable must be an object") };
    }

    const obj = durable_val.toPtr(zq.JSObject);

    const key_val = getDynamicProperty(rt.ctx, obj, pool, "key") orelse {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.key is required") };
    };
    const key = getStringData(key_val) orelse {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.key must be a string") };
    };
    if (key.len == 0) {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.key must be non-empty") };
    }

    var opts: DurableFetchOpts = .{ .key = key };

    if (getDynamicProperty(rt.ctx, obj, pool, "retries")) |retries_val| {
        if (!retries_val.isInt()) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.retries must be an integer") };
        }
        const raw = retries_val.getInt();
        if (raw < 0 or raw > 16) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.retries must be in [0, 16]") };
        }
        opts.retries = @intCast(raw);
    }

    if (getDynamicProperty(rt.ctx, obj, pool, "backoff")) |backoff_val| {
        const text = getStringData(backoff_val) orelse {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.backoff must be \"none\" or \"exponential\"") };
        };
        if (std.mem.eql(u8, text, "none")) {
            opts.backoff = .none;
        } else if (std.mem.eql(u8, text, "exponential")) {
            opts.backoff = .exponential;
        } else {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.backoff must be \"none\" or \"exponential\"") };
        }
    }

    if (getDynamicProperty(rt.ctx, obj, pool, "ttl_s")) |ttl_val| {
        if (!ttl_val.isInt()) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.ttl_s must be an integer") };
        }
        const raw = ttl_val.getInt();
        if (raw <= 0 or raw > 7 * 24 * 3600) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.ttl_s must be in (0, 604800]") };
        }
        opts.ttl_s = @intCast(raw);
    }

    return .{ .ok = opts };
}

fn runDurableFetch(
    rt: *Runtime,
    args: []const zq.JSValue,
    opts: DurableFetchOpts,
) !zq.JSValue {
    const durable_dir = rt.config.durable_oplog_dir orelse {
        return createFetchErrorResponse(rt, "DurableNotConfigured", "fetch durable requires --durable <dir>");
    };

    // Extract method+url+body for hashing before the network call. This
    // duplicates a sliver of parseFetchArgs' work but keeps the hash
    // stable even if parseFetchArgs later normalizes fields.
    const summary = try summarizeFetchRequest(rt, args);
    const hash = durable_fetch.computeRequestHash(opts.key, summary.method, summary.url, summary.body);

    var store = durable_store_mod.DurableStore.initFs(rt.allocator, durable_dir);
    const fetch_dir = try store.subtreeDir(rt.allocator, "fetch");
    defer rt.allocator.free(fetch_dir);

    const now_ms = unixMillis();

    if (try durable_fetch.loadEntry(rt.allocator, fetch_dir, hash[0..], opts.ttl_s, now_ms)) |hit| {
        var entry = hit;
        defer entry.deinit(rt.allocator);
        return try rebuildResponseFromCache(rt, entry);
    }

    var attempt: u32 = 0;
    var last_response: zq.JSValue = zq.JSValue.undefined_val;
    while (true) : (attempt += 1) {
        last_response = try fetchSyncResult(rt, args);
        const status = extractResponseStatus(rt, last_response);
        const retryable = status >= 500;
        const exhausted = attempt >= opts.retries;
        if (!retryable or exhausted) break;
        if (opts.backoff == .exponential) {
            const delay_ms: i64 = @as(i64, 100) << @intCast(@min(attempt, 6));
            if (rt.outbound_io_backend) |*backend| {
                std.Io.sleep(backend.io(), .fromMilliseconds(delay_ms), .awake) catch {};
            }
        }
    }

    // Only cache terminal responses. 5xx (including our own 599 transport
    // errors) are transient by convention — caching them would freeze the
    // handler on a stale failure. A retry with the same idempotency key
    // should try again, not replay a bad response.
    const final_status = extractResponseStatus(rt, last_response);
    if (final_status < 500) {
        try persistResponseToCache(rt, fetch_dir, hash[0..], last_response, now_ms);
    }
    return last_response;
}

const RequestSummary = struct {
    method: []const u8,
    url: []const u8,
    body: []const u8,
};

fn summarizeFetchRequest(rt: *Runtime, args: []const zq.JSValue) !RequestSummary {
    const pool = rt.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    var url: []const u8 = "";
    var method: []const u8 = "GET";
    var body: []const u8 = "";

    if (args.len >= 1) {
        if (args[0].isObject()) {
            const obj = args[0].toPtr(zq.JSObject);
            if (getObjectProperty(rt.ctx, obj, pool, zq.Atom.url, "url")) |v|
                if (getStringData(v)) |s| {
                    url = s;
                };
        } else if (getStringData(args[0])) |s| {
            url = s;
        }
    }

    const init_obj: ?*zq.JSObject = blk: {
        if (args.len >= 1 and args[0].isObject()) break :blk args[0].toPtr(zq.JSObject);
        if (args.len >= 2 and args[1].isObject()) break :blk args[1].toPtr(zq.JSObject);
        break :blk null;
    };
    if (init_obj) |obj| {
        if (getObjectProperty(rt.ctx, obj, pool, zq.Atom.method, "method")) |v|
            if (getStringData(v)) |s| {
                method = s;
            };
        if (getObjectProperty(rt.ctx, obj, pool, zq.Atom.body, "body")) |v|
            if (getStringData(v)) |s| {
                body = s;
            };
    }

    return .{ .method = method, .url = url, .body = body };
}

fn extractResponseStatus(rt: *Runtime, response: zq.JSValue) u16 {
    if (!response.isObject()) return 0;
    const pool = rt.ctx.hidden_class_pool orelse return 0;
    const obj = response.toPtr(zq.JSObject);
    const status_val = obj.getProperty(pool, zq.Atom.status) orelse return 0;
    if (!status_val.isInt()) return 0;
    const raw = status_val.getInt();
    if (raw < 0 or raw > 999) return 0;
    return @intCast(raw);
}

fn rebuildResponseFromCache(rt: *Runtime, entry: durable_fetch.Entry) !zq.JSValue {
    const created = try createFetchResponse(
        rt,
        entry.status,
        entry.status_text,
        entry.body,
        entry.content_type,
    );
    return created.value;
}

fn persistResponseToCache(
    rt: *Runtime,
    fetch_dir: []const u8,
    hash_hex: []const u8,
    response: zq.JSValue,
    now_ms: i64,
) !void {
    if (!response.isObject()) return;
    const pool = rt.ctx.hidden_class_pool orelse return;
    const obj = response.toPtr(zq.JSObject);

    const status = extractResponseStatus(rt, response);
    if (status == 0) return;

    var status_text_buf: []const u8 = "";
    if (obj.getProperty(pool, try rt.ctx.atoms.intern("statusText"))) |v|
        if (getStringData(v)) |s| {
            status_text_buf = s;
        };

    var body_bytes: []const u8 = "";
    if (obj.getProperty(pool, zq.Atom.body)) |body_val| {
        if (body_val.isString()) body_bytes = body_val.toPtr(zq.JSString).data();
    }

    var content_type_opt: ?[]const u8 = null;
    if (obj.getProperty(pool, zq.Atom.headers)) |headers_val| {
        if (headers_val.isObject()) {
            const headers_obj = headers_val.toPtr(zq.JSObject);
            if (headers_obj.getProperty(pool, try getHeaderAtom(rt.ctx, "content-type"))) |ct_val| {
                if (getStringData(ct_val)) |s| content_type_opt = s;
            }
        }
    }

    const status_text_owned = try rt.allocator.dupe(u8, status_text_buf);
    defer rt.allocator.free(status_text_owned);
    const body_owned = try rt.allocator.dupe(u8, body_bytes);
    defer rt.allocator.free(body_owned);
    var ct_owned: ?[]u8 = null;
    defer if (ct_owned) |b| rt.allocator.free(b);
    if (content_type_opt) |s| ct_owned = try rt.allocator.dupe(u8, s);

    try durable_fetch.writeEntry(rt.allocator, fetch_dir, hash_hex, .{
        .at_ms = now_ms,
        .status = status,
        .status_text = status_text_owned,
        .content_type = ct_owned,
        .body = body_owned,
    });
}

// ===========================================================================
// WebSocket runtime callbacks (W1-d.4-b)
// ===========================================================================

/// Connection id for the WS frame currently being dispatched to JS.
/// Frame loop sets this before invoking onOpen/onMessage/onClose and
/// clears it afterwards. Callbacks like `send(ws, data)` read from this
/// when the first JS argument is omitted or when we need to double-check
/// the dispatched connection matches the one JS claims.
///
/// Thread-local: each ws frame-loop thread runs independent dispatches,
/// so per-thread storage keeps connections cleanly separated.
pub threadlocal var active_ws_connection: ?u64 = null;

fn wsConnectionIdFromArg(arg: zq.JSValue) ?u64 {
    if (arg.isInt()) {
        const raw = arg.getInt();
        if (raw <= 0) return null;
        return @as(u64, @intCast(raw));
    }
    return null;
}

fn wsPoolFromRuntime(runtime_ptr: *anyopaque) ?*websocket_pool.Pool {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return rt.ws_pool_ref;
}

/// Raw RFC 6455 frame write for server→client messages (unmasked).
/// Used by the `send` callback; symmetric with what the frame loop
/// reads. Payload size is capped at 64 KiB to match the inbound cap.
fn writeWebSocketFrame(fd: std.posix.fd_t, opcode: u4, payload: []const u8) !void {
    var header_buf: [10]u8 = undefined;
    header_buf[0] = 0x80 | @as(u8, opcode); // FIN = 1
    var header_len: usize = 2;
    if (payload.len < 126) {
        header_buf[1] = @as(u8, @intCast(payload.len));
    } else if (payload.len <= 0xFFFF) {
        header_buf[1] = 126;
        std.mem.writeInt(u16, header_buf[2..4], @as(u16, @intCast(payload.len)), .big);
        header_len = 4;
    } else {
        header_buf[1] = 127;
        std.mem.writeInt(u64, header_buf[2..10], payload.len, .big);
        header_len = 10;
    }

    try writeAllPosix(fd, header_buf[0..header_len]);
    if (payload.len > 0) try writeAllPosix(fd, payload);
}

fn writeAllPosix(fd: std.posix.fd_t, data: []const u8) !void {
    var remaining = data;
    while (remaining.len > 0) {
        const result = std.c.write(fd, remaining.ptr, remaining.len);
        if (result < 0) return error.WriteFailed;
        const n: usize = @intCast(result);
        if (n == 0) return error.WriteFailed;
        remaining = remaining[n..];
    }
}

fn wsSendCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 2) {
        return zq.modules.util.throwError(ctx, "TypeError", "send(ws, data) requires 2 arguments");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    const bytes = zq.modules.util.extractString(args[1]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "data must be a string (binary frames land in W2)");
    };
    const snap = pool.snapshot(id) orelse {
        return zq.modules.util.throwError(ctx, "Error", "ws connection no longer registered");
    };
    writeWebSocketFrame(snap.fd, 0x1, bytes) catch |err| {
        std.log.warn("ws send failed (id={d}): {}", .{ id, err });
        return zq.modules.util.throwError(ctx, "Error", "ws send failed");
    };
    return zq.JSValue.undefined_val;
}

fn wsCloseCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 1) {
        return zq.modules.util.throwError(ctx, "TypeError", "close(ws, code?, reason?) requires at least 1 argument");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    const code: u16 = blk: {
        if (args.len < 2) break :blk 1000;
        if (args[1].isInt()) {
            const raw = args[1].getInt();
            if (raw < 1000 or raw > 4999) {
                return zq.modules.util.throwError(ctx, "RangeError", "close code must be in [1000, 4999]");
            }
            break :blk @as(u16, @intCast(raw));
        }
        return zq.modules.util.throwError(ctx, "TypeError", "close code must be a number");
    };
    const reason: []const u8 = if (args.len >= 3)
        zq.modules.util.extractString(args[2]) orelse ""
    else
        "";

    const snap = pool.snapshot(id) orelse {
        return zq.modules.util.throwError(ctx, "Error", "ws connection no longer registered");
    };

    // RFC 6455 §5.5.1: close payload is [status_u16_be][reason_utf8].
    // Reason length is capped at 123 so total payload fits in a 125-byte
    // short frame (126+ would force extended-length headers the peer
    // has to honour).
    var payload_buf: [125]u8 = undefined;
    std.mem.writeInt(u16, payload_buf[0..2], code, .big);
    const reason_len = @min(reason.len, 123);
    if (reason_len > 0) {
        @memcpy(payload_buf[2..][0..reason_len], reason[0..reason_len]);
    }
    writeWebSocketFrame(snap.fd, 0x8, payload_buf[0 .. 2 + reason_len]) catch |err| {
        std.log.warn("ws close write failed (id={d}): {}", .{ id, err });
    };
    // Shutting down the write half lets the peer's read return EOS
    // promptly; the frame loop will then exit and clean up. Ignoring
    // the shutdown error here is intentional — the connection is
    // already being torn down. SHUT_WR == 1 on every supported platform.
    _ = std.c.shutdown(snap.fd, 1);
    return zq.JSValue.undefined_val;
}

fn wsSerializeAttachmentCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 2) {
        return zq.modules.util.throwError(ctx, "TypeError", "serializeAttachment(ws, value) requires 2 arguments");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    // W2-a accepts raw strings only. Handlers with structured state
    // pre-serialize (JSON.stringify will arrive with the structured
    // clone work; for now a string payload is the contract). W2-b
    // extends this into a typed-array path for binary attachments.
    const bytes = zq.modules.util.extractString(args[1]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "attachment must be a string (W2-a); binary lands in W2-b");
    };
    const ok = pool.setAttachment(id, bytes) catch |err| {
        std.log.warn("ws setAttachment failed (id={d}): {}", .{ id, err });
        return zq.modules.util.throwError(ctx, "Error", "attachment write failed");
    };
    if (!ok) {
        return zq.modules.util.throwError(ctx, "Error", "ws connection no longer registered");
    }
    return zq.JSValue.undefined_val;
}

fn wsDeserializeAttachmentCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 1) {
        return zq.modules.util.throwError(ctx, "TypeError", "deserializeAttachment(ws) requires 1 argument");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    // The pool hands us a freshly-duped slice; ctx.createString copies
    // into a JSString, so the interim allocation is short-lived and we
    // free it before returning.
    const bytes = pool.copyAttachment(id, ctx.allocator) catch |err| {
        std.log.warn("ws copyAttachment failed (id={d}): {}", .{ id, err });
        return zq.modules.util.throwError(ctx, "Error", "attachment read failed");
    } orelse return zq.JSValue.undefined_val;
    defer ctx.allocator.free(bytes);
    return try ctx.createString(bytes);
}

fn wsGetWebSocketsCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 1) {
        return zq.modules.util.throwError(ctx, "TypeError", "getWebSockets(roomKey) requires 1 argument");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const room_key = zq.modules.util.extractString(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "roomKey must be a string");
    };

    // Room membership snapshot: 256 peers is the W1 ceiling (see
    // max_room_peers in ws_frame_loop). Overflow returns the first N,
    // which W2 replaces with an iterator that doesn't cap.
    var ids_buf: [256]websocket_pool.ConnectionId = undefined;
    const ids = pool.collectRoom(room_key, &ids_buf);

    const arr = try ctx.createArray();
    arr.prototype = ctx.array_prototype;
    for (ids, 0..) |id, i| {
        const id_i32: i32 = std.math.cast(i32, id) orelse continue;
        try ctx.setIndexChecked(arr, @as(u32, @intCast(i)), zq.JSValue.fromInt(id_i32));
    }
    arr.setArrayLength(@as(u32, @intCast(ids.len)));
    return arr.toValue();
}

fn wsRoomFromPathCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    _ = runtime_ptr;
    _ = args;
    return zq.modules.util.throwError(ctx, "Error", "roomFromPath lands in W2");
}

fn wsSetAutoResponseCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 3) {
        return zq.modules.util.throwError(ctx, "TypeError", "setAutoResponse(ws, request, response) requires 3 arguments");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    const request_bytes = zq.modules.util.extractString(args[1]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "setAutoResponse request must be a string");
    };
    const response_bytes = zq.modules.util.extractString(args[2]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "setAutoResponse response must be a string");
    };
    const ok = pool.setAutoResponse(id, request_bytes, response_bytes) catch |err| {
        std.log.warn("ws setAutoResponse failed (id={d}): {}", .{ id, err });
        return zq.modules.util.throwError(ctx, "Error", "auto-response registration failed");
    };
    if (!ok) {
        return zq.modules.util.throwError(ctx, "Error", "ws connection no longer registered");
    }
    return zq.JSValue.undefined_val;
}

fn serviceCallCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    base_url: []const u8,
    route_pattern: []const u8,
    init_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    const route = parseServiceRoutePattern(route_pattern) catch {
        return createFetchErrorResponse(rt, "InvalidServiceRoute", "route pattern must be 'METHOD /path'");
    };

    const init_obj = if (init_val.isObject()) init_val.toPtr(zq.JSObject) else null;
    const url = buildServiceUrl(rt, ctx, base_url, route.path_pattern, init_obj) catch |err| {
        return switch (err) {
            error.MissingServiceParam => createFetchErrorResponse(rt, "MissingServiceParam", "missing required path parameter"),
            error.InvalidServiceParam => createFetchErrorResponse(rt, "InvalidServiceParam", "service path params must be string, number, or boolean"),
            error.InvalidServiceQuery => createFetchErrorResponse(rt, "InvalidServiceQuery", "service query values must be string, number, or boolean"),
            else => createFetchErrorResponse(rt, "InternalError", @errorName(err)),
        };
    };
    defer rt.allocator.free(url);

    const fetch_init = try ctx.createObject(null);
    try ctx.setPropertyChecked(fetch_init, zq.Atom.method, try ctx.createString(route.method_text));

    if (init_obj) |obj| {
        const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

        if (getDynamicProperty(ctx, obj, pool, "body")) |body_val| {
            if (!body_val.isUndefined() and !body_val.isNull()) {
                const body_str = getStringData(body_val) orelse {
                    return createFetchErrorResponse(rt, "InvalidBody", "serviceCall body must be string|null");
                };
                try ctx.setPropertyChecked(fetch_init, zq.Atom.body, try ctx.createString(body_str));
            }
        }

        if (getDynamicProperty(ctx, obj, pool, "headers")) |headers_val| {
            if (!headers_val.isObject()) {
                return createFetchErrorResponse(rt, "InvalidHeaders", "serviceCall headers must be object<string,string>");
            }
            const headers_obj = try ctx.createObject(null);
            const source_headers = headers_val.toPtr(zq.JSObject);
            const keys = try source_headers.getOwnEnumerableKeys(rt.allocator, pool);
            defer rt.allocator.free(keys);

            for (keys) |key_atom| {
                const header_val = source_headers.getOwnProperty(pool, key_atom) orelse continue;
                const header_str = getStringData(header_val) orelse {
                    return createFetchErrorResponse(rt, "InvalidHeaders", "serviceCall header values must be strings");
                };
                try ctx.setPropertyChecked(headers_obj, key_atom, try ctx.createString(header_str));
            }

            try ctx.setPropertyChecked(fetch_init, zq.Atom.headers, headers_obj.toValue());
        }
    }

    const fetch_args = [_]zq.JSValue{
        try ctx.createString(url),
        fetch_init.toValue(),
    };
    return fetchSyncResult(rt, &fetch_args);
}

fn parseServiceRoutePattern(route_pattern: []const u8) !ServiceRoute {
    const space = std.mem.indexOfScalar(u8, route_pattern, ' ') orelse return error.InvalidServiceRoute;
    const method_text = std.mem.trim(u8, route_pattern[0..space], " ");
    const path_pattern = std.mem.trim(u8, route_pattern[space + 1 ..], " ");
    if (method_text.len == 0 or path_pattern.len == 0 or path_pattern[0] != '/') {
        return error.InvalidServiceRoute;
    }
    if (parseHttpMethod(method_text) == null) return error.InvalidServiceRoute;
    return .{
        .method_text = method_text,
        .path_pattern = path_pattern,
    };
}

fn buildServiceUrl(
    rt: *Runtime,
    ctx: *zq.Context,
    base_url: []const u8,
    path_pattern: []const u8,
    init_obj: ?*zq.JSObject,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(rt.allocator);

    try buf.ensureTotalCapacity(rt.allocator, base_url.len + path_pattern.len + 32);
    try buf.appendSlice(rt.allocator, base_url);
    if (path_pattern.len > 0) {
        const base_has_slash = buf.items.len > 0 and buf.items[buf.items.len - 1] == '/';
        const path_has_slash = path_pattern[0] == '/';
        if (base_has_slash and path_has_slash) {
            buf.items.len -= 1;
        } else if (!base_has_slash and !path_has_slash) {
            try buf.append(rt.allocator, '/');
        }
        try appendServicePath(rt, ctx, &buf, path_pattern, init_obj);
    }

    try appendServiceQuery(rt, ctx, &buf, init_obj);
    return try buf.toOwnedSlice(rt.allocator);
}

fn appendServicePath(
    rt: *Runtime,
    ctx: *zq.Context,
    buf: *std.ArrayList(u8),
    path_pattern: []const u8,
    init_obj: ?*zq.JSObject,
) !void {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const params_obj = blk: {
        const obj = init_obj orelse break :blk null;
        const params_val = getDynamicProperty(ctx, obj, pool, "params") orelse break :blk null;
        if (!params_val.isObject()) return error.InvalidServiceParam;
        break :blk params_val.toPtr(zq.JSObject);
    };

    var i: usize = 0;
    while (i < path_pattern.len) {
        if (path_pattern[i] == ':' and (i == 0 or path_pattern[i - 1] == '/')) {
            const name_start = i + 1;
            var name_end = name_start;
            while (name_end < path_pattern.len and path_pattern[name_end] != '/') : (name_end += 1) {}

            const params = params_obj orelse return error.MissingServiceParam;
            const val = getDynamicProperty(ctx, params, pool, path_pattern[name_start..name_end]) orelse {
                return error.MissingServiceParam;
            };
            if (!try appendEncodedJsValue(rt, buf, val)) return error.InvalidServiceParam;
            i = name_end;
            continue;
        }

        const literal_start = i;
        while (i < path_pattern.len and !(path_pattern[i] == ':' and (i == 0 or path_pattern[i - 1] == '/'))) : (i += 1) {}
        try buf.appendSlice(rt.allocator, path_pattern[literal_start..i]);
    }
}

fn appendServiceQuery(
    rt: *Runtime,
    ctx: *zq.Context,
    buf: *std.ArrayList(u8),
    init_obj: ?*zq.JSObject,
) !void {
    const obj = init_obj orelse return;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const query_val = getDynamicProperty(ctx, obj, pool, "query") orelse return;
    if (!query_val.isObject()) return error.InvalidServiceQuery;

    const query_obj = query_val.toPtr(zq.JSObject);
    const keys = try query_obj.getOwnEnumerableKeys(rt.allocator, pool);
    defer rt.allocator.free(keys);

    var wrote_any = false;
    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        const query_item = query_obj.getOwnProperty(pool, key_atom) orelse continue;
        if (query_item.isUndefined()) continue;

        try buf.append(rt.allocator, if (wrote_any) '&' else '?');
        try appendPercentEncoded(rt, buf, key_name);
        try buf.append(rt.allocator, '=');
        if (!try appendEncodedJsValue(rt, buf, query_item)) return error.InvalidServiceQuery;
        wrote_any = true;
    }
}

fn appendEncodedJsValue(rt: *Runtime, buf: *std.ArrayList(u8), val: zq.JSValue) !bool {
    if (getStringData(val)) |text| {
        try appendPercentEncoded(rt, buf, text);
        return true;
    }
    if (val.isInt()) {
        var num_buf: [32]u8 = undefined;
        const text = std.fmt.bufPrint(&num_buf, "{d}", .{val.getInt()}) catch return false;
        try appendPercentEncoded(rt, buf, text);
        return true;
    }
    if (val.isBool()) {
        try appendPercentEncoded(rt, buf, if (val.getBool()) "true" else "false");
        return true;
    }
    if (val.isFloat()) {
        var num_buf: [64]u8 = undefined;
        const text = std.fmt.bufPrint(&num_buf, "{d}", .{val.getFloat64()}) catch return false;
        try appendPercentEncoded(rt, buf, text);
        return true;
    }
    return false;
}

fn appendPercentEncoded(rt: *Runtime, buf: *std.ArrayList(u8), input: []const u8) !void {
    for (input) |byte| {
        if (isUrlUnreserved(byte)) {
            try buf.append(rt.allocator, byte);
            continue;
        }

        try buf.append(rt.allocator, '%');
        const hex = std.fmt.bytesToHex([_]u8{byte}, .upper);
        try buf.appendSlice(rt.allocator, &hex);
    }
}

fn isUrlUnreserved(byte: u8) bool {
    return (byte >= 'A' and byte <= 'Z') or
        (byte >= 'a' and byte <= 'z') or
        (byte >= '0' and byte <= '9') or
        byte == '-' or
        byte == '_' or
        byte == '.' or
        byte == '~';
}

/// IoCallbacks.execute_fetches_fn - execute HTTP fetches concurrently using threads.
fn ioExecuteFetches(
    runtime_ptr: *anyopaque,
    descriptors: []const zq.modules.io.FetchDescriptor,
    results: []zq.modules.io.FetchResult,
) void {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    const count = descriptors.len;
    if (count == 0) return;

    // For a single fetch, execute inline (no thread overhead)
    if (count == 1) {
        results[0] = doFetchWorker(rt.allocator, rt.config, &descriptors[0]);
        return;
    }

    // Spawn worker threads for concurrent execution
    var threads: [zq.modules.io.MAX_PARALLEL]?std.Thread = .{null} ** zq.modules.io.MAX_PARALLEL;

    for (0..count) |i| {
        threads[i] = std.Thread.spawn(.{}, doFetchThread, .{
            rt.allocator,
            rt.config,
            &descriptors[i],
            &results[i],
        }) catch null;
    }

    // If thread spawn failed for any slot, execute inline as fallback
    for (0..count) |i| {
        if (threads[i] == null) {
            results[i] = doFetchWorker(rt.allocator, rt.config, &descriptors[i]);
        }
    }

    // Join all threads
    for (0..count) |i| {
        if (threads[i]) |t| t.join();
    }
}

/// Thread entry point for concurrent HTTP fetch.
fn doFetchThread(
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    desc: *const zq.modules.io.FetchDescriptor,
    result: *zq.modules.io.FetchResult,
) void {
    result.* = doFetchWorker(allocator, config, desc);
}

/// Execute a single HTTP fetch. Safe to call from any thread.
/// Creates its own I/O backend and HTTP client.
fn doFetchWorker(
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    desc: *const zq.modules.io.FetchDescriptor,
) zq.modules.io.FetchResult {
    return doFetchWorkerInner(allocator, config, desc) catch |err| {
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = allocator.dupe(u8, "InternalError") catch null,
            .error_details = allocator.dupe(u8, @errorName(err)) catch null,
        };
    };
}

fn doFetchWorkerInner(
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    desc: *const zq.modules.io.FetchDescriptor,
) !zq.modules.io.FetchResult {
    const uri = try std.Uri.parse(desc.url);

    // Thread-local I/O backend
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();

    var client = std.http.Client{
        .allocator = allocator,
        .io = io_backend.io(),
    };
    defer client.deinit();

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse {
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, "InvalidUrl"),
            .error_details = try allocator.dupe(u8, "unsupported URI scheme"),
        };
    };

    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = try uri.getHost(&host_buf);

    const timeout: std.Io.Timeout = if (config.outbound_timeout_ms == 0) .none else blk: {
        const duration = std.Io.Duration.fromMilliseconds(@intCast(config.outbound_timeout_ms));
        break :blk .{ .duration = .{ .raw = duration, .clock = .awake } };
    };

    const connection = client.connectTcpOptions(.{
        .host = host,
        .port = uri.port orelse switch (protocol) {
            .plain => 80,
            .tls => 443,
        },
        .protocol = protocol,
        .timeout = timeout,
    }) catch |err| {
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, "ConnectFailed"),
            .error_details = try allocator.dupe(u8, @errorName(err)),
        };
    };

    var req = client.request(desc.method, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        .extra_headers = desc.headers.items,
    }) catch |err| {
        client.connection_pool.release(connection, client.io);
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, "RequestInitFailed"),
            .error_details = try allocator.dupe(u8, @errorName(err)),
        };
    };
    defer req.deinit();

    if (desc.body) |payload| {
        req.transfer_encoding = .{ .content_length = payload.len };
        var request_body = req.sendBodyUnflushed(&.{}) catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, "RequestSendFailed"),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
        request_body.writer.writeAll(payload) catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, "RequestSendFailed"),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
        request_body.end() catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, "RequestSendFailed"),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
        req.connection.?.flush() catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, "RequestSendFailed"),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
    } else {
        req.sendBodiless() catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, "RequestSendFailed"),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
    }

    var response = req.receiveHead(&.{}) catch |err| {
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, "ResponseHeadFailed"),
            .error_details = try allocator.dupe(u8, @errorName(err)),
        };
    };

    const status = @intFromEnum(response.head.status);
    var owned_head = try snapshotResponseHead(allocator, response.head);
    defer owned_head.deinit(allocator);

    const max_response_bytes = @max(@as(usize, 1), desc.max_response_bytes);
    var body_transfer: [64]u8 = undefined;
    var response_reader = response.reader(&body_transfer);
    const response_body = response_reader.allocRemaining(allocator, std.Io.Limit.limited(max_response_bytes)) catch |err| switch (err) {
        error.StreamTooLong => {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, "ResponseTooLarge"),
                .error_details = try allocator.dupe(u8, "response exceeded max_response_bytes"),
            };
        },
        else => {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, "ResponseReadFailed"),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        },
    };

    // Build response headers list
    var resp_headers: std.ArrayList(zq.modules.io.FetchResult.ResponseHeader) = .empty;
    for (owned_head.headers.items) |h| {
        try resp_headers.append(allocator, .{
            .name = try allocator.dupe(u8, h.name),
            .value_str = try allocator.dupe(u8, h.value),
        });
    }

    return zq.modules.io.FetchResult{
        .status = status,
        .body = response_body,
        .content_type = if (owned_head.contentType()) |ct| try allocator.dupe(u8, ct) else null,
        .reason = try allocator.dupe(u8, owned_head.reason),
        .response_headers = resp_headers,
        .ok = true,
    };
}

/// IoCallbacks.build_response_fn - create a JS Response object from a FetchResult.
fn ioBuildResponse(runtime_ptr: *anyopaque, result: *const zq.modules.io.FetchResult) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));

    if (!result.ok) {
        // Build error response
        const err_code = result.error_code orelse "InternalError";
        const err_details = result.error_details orelse "unknown error";
        return createFetchErrorResponse(rt, err_code, err_details);
    }

    const body_str = result.body orelse "";
    const created = try createFetchResponse(
        rt,
        result.status,
        result.reason orelse "",
        body_str,
        result.content_type,
    );

    // Copy response headers
    for (result.response_headers.items) |h| {
        const key_atom = try getHeaderAtom(rt.ctx, h.name);
        const value_str = try rt.ctx.createString(h.value_str);
        try rt.ctx.setPropertyChecked(created.headers, key_atom, value_str);
    }

    return created.value;
}

fn httpRequestNative(_: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const rt = current_runtime orelse return error.RuntimeUnavailable;
    const out = httpRequestResultJsonAlloc(rt, args) catch |err| {
        const fallback = try httpRequestErrorJsonAlloc(rt.allocator, "InternalError", @errorName(err));
        defer rt.allocator.free(fallback);
        return rt.createString(fallback);
    };
    defer rt.allocator.free(out);
    return rt.createString(out);
}

fn httpRequestResultJsonAlloc(rt: *Runtime, args: []const zq.JSValue) ![]u8 {
    const a = rt.allocator;
    if (!rt.config.outbound_http_enabled) {
        return try httpRequestErrorJsonAlloc(a, "OutboundHttpDisabled", "set runtime outbound_http_enabled=true");
    }
    if (args.len == 0 or !args[0].isString()) {
        return try httpRequestErrorJsonAlloc(a, "InvalidArgs", "expected one JSON string argument");
    }

    const input_json = args[0].toPtr(zq.JSString).data();
    var parsed = std.json.parseFromSlice(std.json.Value, a, input_json, .{}) catch {
        return try httpRequestErrorJsonAlloc(a, "InvalidJson", "failed to parse request JSON");
    };
    defer parsed.deinit();
    if (parsed.value != .object) {
        return try httpRequestErrorJsonAlloc(a, "InvalidJson", "request JSON must be an object");
    }
    const obj = parsed.value.object;

    const url_v = obj.get("url") orelse {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "missing url field");
    };
    if (url_v != .string or url_v.string.len == 0) {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "url must be a non-empty string");
    }
    const uri = std.Uri.parse(url_v.string) catch {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "url parse failed");
    };

    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = uri.getHost(&host_buf) catch {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "url host is required");
    };
    if (outboundHostViolation(rt, host.bytes)) |details| {
        return try httpRequestErrorJsonAlloc(a, "HostNotAllowed", details);
    }

    const method = blk: {
        if (obj.get("method")) |method_v| {
            if (method_v != .string) {
                return try httpRequestErrorJsonAlloc(a, "InvalidMethod", "method must be string");
            }
            break :blk parseHttpMethod(method_v.string) orelse {
                return try httpRequestErrorJsonAlloc(a, "InvalidMethod", method_v.string);
            };
        }
        break :blk std.http.Method.GET;
    };

    const body: ?[]const u8 = if (obj.get("body")) |body_v| switch (body_v) {
        .null => null,
        .string => |s| s,
        else => return try httpRequestErrorJsonAlloc(a, "InvalidBody", "body must be string|null"),
    } else null;

    const max_response_bytes = blk: {
        const cfg_max = @max(@as(usize, 1), rt.config.outbound_max_response_bytes);
        if (obj.get("max_response_bytes")) |max_v| switch (max_v) {
            .integer => |i| {
                const req_max = std.math.cast(usize, i) orelse {
                    return try httpRequestErrorJsonAlloc(a, "InvalidMaxResponseBytes", "max_response_bytes out of range");
                };
                break :blk @min(cfg_max, @max(@as(usize, 1), req_max));
            },
            else => return try httpRequestErrorJsonAlloc(a, "InvalidMaxResponseBytes", "max_response_bytes must be integer"),
        };
        break :blk cfg_max;
    };

    var owned_header_slices = std.array_list.Managed([]u8).init(a);
    defer {
        for (owned_header_slices.items) |s| a.free(s);
        owned_header_slices.deinit();
    }
    var headers = std.array_list.Managed(std.http.Header).init(a);
    defer headers.deinit();

    if (obj.get("headers")) |headers_v| {
        if (headers_v != .object) {
            return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "headers must be object<string,string>");
        }
        var it = headers_v.object.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* != .string) {
                return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "header values must be strings");
            }
            const name = try a.dupe(u8, entry.key_ptr.*);
            errdefer a.free(name);
            const value = try a.dupe(u8, entry.value_ptr.string);
            errdefer a.free(value);
            if (name.len == 0) return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "header name must be non-empty");
            if (std.mem.indexOfAny(u8, name, "\r\n") != null) return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "header name contains newline");
            if (std.mem.indexOfAny(u8, value, "\r\n") != null) return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "header value contains newline");

            try owned_header_slices.append(name);
            try owned_header_slices.append(value);
            try headers.append(.{ .name = name, .value = value });
        }
    }

    var client = std.http.Client{
        .allocator = a,
        .io = rt.outbound_io_backend.?.io(),
    };
    defer client.deinit();

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "unsupported URI scheme");
    };
    const timeout: std.Io.Timeout = if (rt.config.outbound_timeout_ms == 0) .none else blk: {
        const duration = std.Io.Duration.fromMilliseconds(@intCast(rt.config.outbound_timeout_ms));
        break :blk .{ .duration = .{ .raw = duration, .clock = .awake } };
    };
    const connection = client.connectTcpOptions(.{
        .host = host,
        .port = uri.port orelse switch (protocol) {
            .plain => 80,
            .tls => 443,
        },
        .protocol = protocol,
        .timeout = timeout,
    }) catch |err| {
        return try httpRequestErrorJsonAlloc(a, "ConnectFailed", @errorName(err));
    };

    var req = client.request(method, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        .extra_headers = headers.items,
    }) catch |err| {
        client.connection_pool.release(connection, client.io);
        return try httpRequestErrorJsonAlloc(a, "RequestInitFailed", @errorName(err));
    };
    defer req.deinit();

    if (body) |payload| {
        req.transfer_encoding = .{ .content_length = payload.len };
        var request_body = req.sendBodyUnflushed(&.{}) catch |err| {
            return try httpRequestErrorJsonAlloc(a, "RequestSendFailed", @errorName(err));
        };
        request_body.writer.writeAll(payload) catch |err| {
            return try httpRequestErrorJsonAlloc(a, "RequestSendFailed", @errorName(err));
        };
        request_body.end() catch |err| {
            return try httpRequestErrorJsonAlloc(a, "RequestSendFailed", @errorName(err));
        };
        req.connection.?.flush() catch |err| {
            return try httpRequestErrorJsonAlloc(a, "RequestSendFailed", @errorName(err));
        };
    } else {
        req.sendBodiless() catch |err| {
            return try httpRequestErrorJsonAlloc(a, "RequestSendFailed", @errorName(err));
        };
    }

    var response = req.receiveHead(&.{}) catch |err| {
        return try httpRequestErrorJsonAlloc(a, "ResponseHeadFailed", @errorName(err));
    };
    const status = @intFromEnum(response.head.status);
    const ok = response.head.status.class() != .server_error and response.head.status.class() != .client_error;
    var owned_head = try snapshotResponseHead(a, response.head);
    defer owned_head.deinit(a);

    var body_transfer: [64]u8 = undefined;
    var response_reader = response.reader(&body_transfer);
    const response_body = response_reader.allocRemaining(a, std.Io.Limit.limited(max_response_bytes)) catch |err| switch (err) {
        error.StreamTooLong => return try httpRequestErrorJsonAlloc(a, "ResponseTooLarge", "response exceeded max_response_bytes"),
        else => return try httpRequestErrorJsonAlloc(a, "ResponseReadFailed", @errorName(err)),
    };
    defer a.free(response_body);

    var aw: std.Io.Writer.Allocating = .init(a);
    defer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try stream.beginObject();
    try stream.objectField("ok");
    try stream.write(ok);
    try stream.objectField("status");
    try stream.write(status);
    try stream.objectField("reason");
    try stream.write(owned_head.reason);
    if (owned_head.contentType()) |ct| {
        try stream.objectField("content_type");
        try stream.write(ct);
    }
    try stream.objectField("body");
    try stream.write(response_body);
    try stream.endObject();
    return try aw.toOwnedSlice();
}

fn parseHttpMethod(raw: []const u8) ?std.http.Method {
    if (ascii.eqlIgnoreCase(raw, "GET")) return .GET;
    if (ascii.eqlIgnoreCase(raw, "POST")) return .POST;
    if (ascii.eqlIgnoreCase(raw, "PUT")) return .PUT;
    if (ascii.eqlIgnoreCase(raw, "PATCH")) return .PATCH;
    if (ascii.eqlIgnoreCase(raw, "DELETE")) return .DELETE;
    if (ascii.eqlIgnoreCase(raw, "HEAD")) return .HEAD;
    if (ascii.eqlIgnoreCase(raw, "OPTIONS")) return .OPTIONS;
    return null;
}

fn httpRequestErrorJsonAlloc(a: std.mem.Allocator, err_code: []const u8, details: []const u8) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(a);
    defer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try stream.beginObject();
    try stream.objectField("ok");
    try stream.write(false);
    try stream.objectField("error");
    try stream.write(err_code);
    try stream.objectField("details");
    try stream.write(details);
    try stream.endObject();
    return try aw.toOwnedSlice();
}

fn consoleLog(_: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    for (args, 0..) |arg, i| {
        if (i > 0) writeToFd(std.c.STDOUT_FILENO, " ");
        printValue(arg, std.c.STDOUT_FILENO);
    }
    writeToFd(std.c.STDOUT_FILENO, "\n");
    return zq.JSValue.undefined_val;
}

fn consoleError(_: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    writeToFd(std.c.STDERR_FILENO, "\x1b[31m[ERROR]\x1b[0m ");
    for (args, 0..) |arg, i| {
        if (i > 0) writeToFd(std.c.STDERR_FILENO, " ");
        printValue(arg, std.c.STDERR_FILENO);
    }
    writeToFd(std.c.STDERR_FILENO, "\n");
    return zq.JSValue.undefined_val;
}

fn writeToFd(fd: std.c.fd_t, data: []const u8) void {
    _ = std.c.write(fd, data.ptr, data.len);
}

fn printValue(val: zq.JSValue, fd: std.c.fd_t) void {
    if (val.isUndefined()) {
        writeToFd(fd, "undefined");
    } else if (val.isNull()) {
        writeToFd(fd, "null");
    } else if (val.isTrue()) {
        writeToFd(fd, "true");
    } else if (val.isFalse()) {
        writeToFd(fd, "false");
    } else if (val.isInt()) {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return;
        writeToFd(fd, s);
    } else if (val.isFloat()) {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getFloat64()}) catch return;
        writeToFd(fd, s);
    } else if (val.isString()) {
        const str = val.toPtr(zq.JSString);
        writeToFd(fd, str.data());
    } else if (val.isObject()) {
        writeToFd(fd, "[Object]");
    } else {
        writeToFd(fd, "[unknown]");
    }
}

// ============================================================================
// Percentile Tracker for Latency Metrics
// ============================================================================

/// Mutex-protected ring buffer that records nanosecond-resolution latency samples.
/// Used only for diagnostic metrics in debug builds, so we prefer correctness
/// under contention over lock-free writes.
pub const PercentileTracker = struct {
    mutex: compat.Mutex = .{},
    samples: [SAMPLE_SIZE]u64 = [_]u64{0} ** SAMPLE_SIZE,
    total: usize = 0,

    const SAMPLE_SIZE = 1024;

    pub fn record(self: *PercentileTracker, ns: u64) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        const i = self.total % SAMPLE_SIZE;
        self.samples[i] = ns;
        self.total += 1;
    }

    /// Returns the approximate p-th percentile (0-100) in nanoseconds.
    /// Copies the ring buffer to a stack array and sorts via insertion sort.
    pub fn getPercentile(self: *PercentileTracker, p: f64) u64 {
        self.mutex.lock();
        defer self.mutex.unlock();

        const n = self.total;
        if (n == 0) return 0;
        const active = @min(n, SAMPLE_SIZE);

        var sorted: [SAMPLE_SIZE]u64 = undefined;
        for (0..active) |i| {
            sorted[i] = self.samples[i];
        }

        // Insertion sort: fast enough for <= 1024 samples, no stdlib dependency
        for (1..active) |i| {
            const key = sorted[i];
            var j: usize = i;
            while (j > 0 and sorted[j - 1] > key) {
                sorted[j] = sorted[j - 1];
                j -= 1;
            }
            sorted[j] = key;
        }

        const rank = @as(usize, @intFromFloat(p / 100.0 * @as(f64, @floatFromInt(active - 1))));
        return sorted[@min(rank, active - 1)];
    }
};

// ============================================================================
// Handler Pool (Lock-Free)
// ============================================================================

/// Lock-free pool of pre-initialized JavaScript runtimes, backed by zigts.LockFreePool.
/// Uses per-runtime wrappers to install builtins and load handler code once.
pub const HandlerPool = struct {
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    handler_code: []const u8,
    handler_filename: []const u8,
    max_size: usize,
    acquire_timeout_ms: u32,
    in_use: std.atomic.Value(usize),
    request_seq: std.atomic.Value(u64),
    total_wait_ns: std.atomic.Value(u64),
    max_wait_ns: std.atomic.Value(u64),
    total_exec_ns: std.atomic.Value(u64),
    max_exec_ns: std.atomic.Value(u64),
    exhausted_count: std.atomic.Value(u64),
    /// Release-time arena audit failures. Exposed via getMetrics().
    arena_audit_failures: std.atomic.Value(u64),
    /// Release-time persistent-string-in-arena violations. Exposed via getMetrics().
    persistent_escape_failures: std.atomic.Value(u64),
    /// Runtimes recycled (destroyed, not returned to pool) by policy.
    recycles: std.atomic.Value(u64),
    /// Contract-derived lifecycle policy. Set by the server after contract parse.
    pooling_policy: contract_runtime.PoolingPolicy = .reuse_bounded_by_count,
    pooling_thresholds: contract_runtime.PoolingThresholds = .{},
    wait_percentiles: PercentileTracker,
    exec_percentiles: PercentileTracker,
    pool: zq.LockFreePool,
    cache: bytecode_cache.BytecodeCache,
    cache_mutex: compat.Mutex,
    runtime_init_mutex: compat.Mutex,
    /// Pre-compiled bytecode embedded at build time (from -Dhandler option)
    embedded_bytecode: ?[]const u8,
    /// Runtime-provided dependency bytecodes (from self-extracting binary)
    runtime_dep_bytecodes: ?[]const []const u8,
    /// Shared trace file handle (owned by pool, shared across runtimes)
    trace_file: ?std.c.fd_t,
    /// Mutex protecting concurrent trace file writes
    trace_mutex: ?*zq.trace.TraceMutex,

    const Self = @This();
    const collect_pool_metrics = builtin.mode == .Debug;

    pub const ResponseHandle = struct {
        response: HttpResponse,
        runtime: *Runtime,
        base_rt: *zq.LockFreePool.Runtime,
        pool: *HandlerPool,

        pub fn deinit(self: *ResponseHandle) void {
            self.response.deinit();
            self.pool.releaseForRequest(self.base_rt);
        }
    };

    pub const WorkerRuntimeLease = struct {
        runtime: *Runtime,
        base_rt: *zq.LockFreePool.Runtime,
        pool: *HandlerPool,

        pub fn deinit(self: *WorkerRuntimeLease) void {
            self.pool.releaseForRequest(self.base_rt);
        }
    };

    pub fn init(
        allocator: std.mem.Allocator,
        config: RuntimeConfig,
        handler_code: []const u8,
        handler_filename: []const u8,
        max_size: usize,
        acquire_timeout_ms: u32,
    ) !Self {
        return initWithEmbedded(allocator, config, handler_code, handler_filename, max_size, acquire_timeout_ms, null);
    }

    /// Initialize with optional embedded bytecode (set before prewarm)
    pub fn initWithEmbedded(
        allocator: std.mem.Allocator,
        config: RuntimeConfig,
        handler_code: []const u8,
        handler_filename: []const u8,
        max_size: usize,
        acquire_timeout_ms: u32,
        embedded_bytecode: ?[]const u8,
    ) !Self {
        return initWithEmbeddedAndDeps(allocator, config, handler_code, handler_filename, max_size, acquire_timeout_ms, embedded_bytecode, null);
    }

    /// Initialize with optional embedded bytecode and runtime-provided dependency bytecodes
    pub fn initWithEmbeddedAndDeps(
        allocator: std.mem.Allocator,
        config: RuntimeConfig,
        handler_code: []const u8,
        handler_filename: []const u8,
        max_size: usize,
        acquire_timeout_ms: u32,
        embedded_bytecode: ?[]const u8,
        runtime_dep_bytecodes: ?[]const []const u8,
    ) !Self {
        const pool = try zq.LockFreePool.init(allocator, .{
            .max_size = max_size,
            .gc_config = .{ .nursery_size = config.nursery_size },
            .arena_config = .{ .size = config.arena_size },
            .use_hybrid_allocation = config.use_hybrid_allocation,
        });

        var self = Self{
            .allocator = allocator,
            .config = config,
            .handler_code = handler_code,
            .handler_filename = handler_filename,
            .max_size = max_size,
            .acquire_timeout_ms = acquire_timeout_ms,
            .in_use = std.atomic.Value(usize).init(0),
            .request_seq = std.atomic.Value(u64).init(0),
            .total_wait_ns = std.atomic.Value(u64).init(0),
            .max_wait_ns = std.atomic.Value(u64).init(0),
            .total_exec_ns = std.atomic.Value(u64).init(0),
            .max_exec_ns = std.atomic.Value(u64).init(0),
            .exhausted_count = std.atomic.Value(u64).init(0),
            .arena_audit_failures = std.atomic.Value(u64).init(0),
            .persistent_escape_failures = std.atomic.Value(u64).init(0),
            .recycles = std.atomic.Value(u64).init(0),
            .wait_percentiles = .{},
            .exec_percentiles = .{},
            .pool = pool,
            .cache = bytecode_cache.BytecodeCache.init(allocator),
            .cache_mutex = .{},
            .runtime_init_mutex = .{},
            .embedded_bytecode = embedded_bytecode,
            .runtime_dep_bytecodes = runtime_dep_bytecodes,
            .trace_file = null,
            .trace_mutex = null,
        };

        // Open shared trace file if configured
        if (config.trace_file_path) |trace_path| {
            self.trace_file = openTraceFile(allocator, trace_path) catch |err| {
                std.log.err("Failed to open trace file '{s}': {}", .{ trace_path, err });
                return err;
            };
            const mutex = try allocator.create(zq.trace.TraceMutex);
            mutex.* = .{};
            self.trace_mutex = mutex;
        }

        errdefer self.deinit();
        try self.prewarm();
        return self;
    }

    /// Set embedded bytecode (precompiled at build time)
    /// When set, this bytecode is used directly without parsing.
    /// Note: For proper prewarm, use initWithEmbedded instead.
    pub fn setEmbeddedBytecode(self: *Self, bytecode: []const u8) void {
        self.embedded_bytecode = bytecode;
    }

    pub fn deinit(self: *Self) void {
        self.pool.deinit();
        self.cache.deinit();
        if (self.trace_file) |fd| std.Io.Threaded.closeFd(fd);
        if (self.trace_mutex) |m| self.allocator.destroy(m);
    }

    /// Hot-swap handler code without restarting the server.
    ///
    /// Updates the handler source, clears the bytecode cache, and invalidates
    /// all idle runtimes so they recompile on next acquire. In-flight requests
    /// continue with the old handler and get the new code on their next cycle.
    ///
    /// `new_code` must be heap-allocated and owned by the caller. The pool
    /// takes a reference but does not free it - the caller manages the
    /// lifetime of both old and new code (keep the previous generation alive
    /// until the next reload cycle to avoid use-after-free on in-flight
    /// runtimes).
    pub fn reloadHandler(self: *Self, new_code: []const u8, new_filename: []const u8) usize {
        self.runtime_init_mutex.lock();
        self.cache_mutex.lock();

        self.handler_code = new_code;
        self.handler_filename = new_filename;
        self.cache.clear();
        self.embedded_bytecode = null;

        self.cache_mutex.unlock();
        self.runtime_init_mutex.unlock();

        // Checked-out runtimes (slot == null) are skipped here - they
        // get reinitialized on next acquire because the cache is cleared.
        var invalidated: usize = 0;
        for (self.pool.slots) |*slot| {
            if (slot.load(.acquire)) |base_rt| {
                self.invalidateRuntime(base_rt);
                invalidated += 1;
            }
        }

        return invalidated;
    }

    fn nextRequestId(self: *Self) u64 {
        return if (collect_pool_metrics) self.request_seq.fetchAdd(1, .acq_rel) + 1 else 0;
    }

    /// Acquire and pin a runtime to the current worker thread.
    /// Caller must release it with lease.deinit().
    pub fn acquireWorkerRuntime(self: *Self) !WorkerRuntimeLease {
        const base_rt = try self.acquireForRequest();
        errdefer self.releaseForRequest(base_rt);
        const rt = try self.ensureRuntime(base_rt);
        return .{
            .runtime = rt,
            .base_rt = base_rt,
            .pool = self,
        };
    }

    /// Execute handler with a request (acquire, run, release)
    pub fn executeHandler(self: *Self, request: HttpRequestView) !HttpResponse {
        const request_id = self.nextRequestId();
        const base_rt = try self.acquireForRequest();
        defer {
            self.releaseForRequest(base_rt);
        }
        var exec_timer: ?compat.Timer = null;
        if (collect_pool_metrics) {
            exec_timer = compat.Timer.start() catch null;
        }
        defer if (collect_pool_metrics) {
            if (exec_timer) |*t| self.recordExec(t.read());
        };

        var attempt: u8 = 0;
        var last_err: ?anyerror = null;
        while (attempt < 2) : (attempt += 1) {
            const rt = try self.ensureRuntime(base_rt);
            const result = rt.executeHandlerWithId(request, request_id) catch |err| {
                if (isHandlerInvalid(err)) {
                    std.log.warn("Handler invalid, rebuilding runtime (err={})", .{err});
                    self.invalidateRuntime(base_rt);
                    last_err = err;
                    continue;
                }
                return err;
            };
            if (!rt.owns_resources) {
                result.assertDetachedFromRuntime();
            }
            return result;
        }
        return last_err orelse error.HandlerNotCallable;
    }

    /// Execute handler and return a response handle that borrows JS strings.
    /// Caller must call handle.deinit() after sending the response.
    pub fn executeHandlerBorrowed(self: *Self, request: HttpRequestView) !ResponseHandle {
        const request_id = self.nextRequestId();
        const base_rt = try self.acquireForRequest();
        errdefer self.releaseForRequest(base_rt);
        var exec_timer: ?compat.Timer = null;
        if (collect_pool_metrics) {
            exec_timer = compat.Timer.start() catch null;
        }
        defer if (collect_pool_metrics) {
            if (exec_timer) |*t| self.recordExec(t.read());
        };

        var attempt: u8 = 0;
        var last_err: ?anyerror = null;
        while (attempt < 2) : (attempt += 1) {
            const rt = try self.ensureRuntime(base_rt);
            const response = rt.executeHandlerBorrowedWithId(request, request_id) catch |err| {
                if (isHandlerInvalid(err)) {
                    std.log.warn("Handler invalid, rebuilding runtime (err={})", .{err});
                    self.invalidateRuntime(base_rt);
                    last_err = err;
                    continue;
                }
                return err;
            };
            return .{
                .response = response,
                .runtime = rt,
                .base_rt = base_rt,
                .pool = self,
            };
        }
        return last_err orelse error.HandlerNotCallable;
    }

    /// Execute handler on a worker-pinned runtime lease.
    /// The caller must keep the lease alive until any borrowed response data
    /// has been sent, then release it with lease.deinit().
    pub fn executeHandlerBorrowedLeased(self: *Self, lease: *WorkerRuntimeLease, request: HttpRequestView) !HttpResponse {
        const request_id = self.nextRequestId();
        var exec_timer: ?compat.Timer = null;
        if (collect_pool_metrics) {
            exec_timer = compat.Timer.start() catch null;
        }
        defer if (collect_pool_metrics) {
            if (exec_timer) |*t| self.recordExec(t.read());
        };

        var attempt: u8 = 0;
        var last_err: ?anyerror = null;
        while (attempt < 2) : (attempt += 1) {
            const response = lease.runtime.executeHandlerBorrowedWithId(request, request_id) catch |err| {
                if (isHandlerInvalid(err)) {
                    std.log.warn("Leased runtime invalid, rebuilding (err={})", .{err});
                    self.invalidateRuntime(lease.base_rt);
                    lease.runtime = try self.ensureRuntime(lease.base_rt);
                    last_err = err;
                    continue;
                }
                return err;
            };
            return response;
        }
        return last_err orelse error.HandlerNotCallable;
    }

    pub fn getInUse(self: *const Self) usize {
        return self.in_use.load(.acquire);
    }

    /// Get cache statistics
    pub fn getCacheStats(self: *const Self) struct { hits: u64, misses: u64, hit_rate: f64 } {
        return .{
            .hits = self.cache.hits.load(.monotonic),
            .misses = self.cache.misses.load(.monotonic),
            .hit_rate = self.cache.hitRate(),
        };
    }

    /// Context for parallel prewarm workers
    const PrewarmCtx = struct {
        pool: *HandlerPool,
        success: std.atomic.Value(bool),
    };

    fn prewarm(self: *Self) !void {
        const prewarm_count = @min(@as(usize, 2), self.max_size);

        // Single runtime or test mode - sequential prewarm
        // (test allocators aren't thread-safe)
        if (prewarm_count <= 1 or builtin.is_test) {
            for (0..prewarm_count) |_| {
                const base_rt = try self.pool.acquire();
                errdefer self.pool.release(base_rt);
                _ = try self.ensureRuntime(base_rt);
                self.pool.release(base_rt);
            }
            return;
        }

        // Parallel prewarm for multiple runtimes (production only)
        var contexts: [2]PrewarmCtx = undefined;
        var threads: [2]?std.Thread = [_]?std.Thread{null} ** 2;

        // Spawn workers
        for (0..prewarm_count) |i| {
            contexts[i] = .{
                .pool = self,
                .success = std.atomic.Value(bool).init(false),
            };
            threads[i] = std.Thread.spawn(.{}, prewarmWorker, .{&contexts[i]}) catch null;
        }

        // Join all workers
        var success_count: usize = 0;
        for (0..prewarm_count) |i| {
            if (threads[i]) |t| {
                t.join();
                if (contexts[i].success.load(.acquire)) success_count += 1;
            }
        }

        // Require at least one successful prewarm
        if (success_count == 0) return error.PrewarmFailed;
    }

    fn prewarmWorker(ctx: *PrewarmCtx) void {
        const base_rt = ctx.pool.pool.acquire() catch return;
        defer ctx.pool.pool.release(base_rt);
        _ = ctx.pool.ensureRuntime(base_rt) catch return;
        ctx.success.store(true, .release);
    }

    fn acquireForRequest(self: *Self) !*zq.LockFreePool.Runtime {
        var wait_timer: ?compat.Timer = null;
        const timeout_ns: u64 = @as(u64, self.acquire_timeout_ms) * std.time.ns_per_ms;

        // Adaptive backoff parameters:
        // Phase 1: Spin without sleep (10 iterations)
        // Phase 2: Sleep starting at 10us, cap at 1ms (faster than 50us-5ms)
        // Phase 3: Fail fast after 100 retries (circuit breaker)
        const spin_iterations: u32 = 10;
        const max_retries: u32 = 100;
        const initial_backoff_ns: u64 = 10 * std.time.ns_per_us;
        const max_backoff_ns: u64 = 1 * std.time.ns_per_ms;

        var retry_count: u32 = 0;
        var backoff_ns: u64 = initial_backoff_ns;

        while (true) {
            if (self.max_size != 0) {
                // Use CAS loop to atomically check-and-increment only if under limit.
                // This prevents the race where multiple threads temporarily exceed max_size.
                var current = self.in_use.load(.acquire);
                var acquired = false;
                while (current < self.max_size) {
                    if (self.in_use.cmpxchgWeak(current, current + 1, .acq_rel, .acquire)) |actual| {
                        // CAS failed - another thread modified in_use, retry with actual value
                        current = actual;
                        continue;
                    }
                    // CAS succeeded - we atomically incremented and are under limit
                    acquired = true;
                    break;
                }
                if (acquired) break;

                // At capacity - proceed to retry/backoff logic
                retry_count += 1;

                // Check timeout first
                if (self.acquire_timeout_ms == 0) {
                    _ = self.exhausted_count.fetchAdd(1, .monotonic);
                    if (collect_pool_metrics) {
                        if (wait_timer) |*t| self.recordWait(t.read());
                    }
                    return error.PoolExhausted;
                }

                if (wait_timer == null) {
                    wait_timer = compat.Timer.start() catch null;
                }
                if (wait_timer) |*t| {
                    const elapsed = t.read();
                    if (elapsed >= timeout_ns) {
                        _ = self.exhausted_count.fetchAdd(1, .monotonic);
                        if (collect_pool_metrics) {
                            self.recordWait(elapsed);
                        }
                        return error.PoolExhausted;
                    }
                }

                // Circuit breaker: fail fast after max retries
                if (retry_count > max_retries) {
                    _ = self.exhausted_count.fetchAdd(1, .monotonic);
                    if (collect_pool_metrics) {
                        if (wait_timer) |*t| self.recordWait(t.read());
                    }
                    return error.PoolExhausted;
                }

                // Phase 1: Spin without sleep for brief contention
                if (retry_count <= spin_iterations) {
                    std.atomic.spinLoopHint();
                    continue;
                }

                // Phase 2: Sleep with jitter to prevent thundering herd
                // Use thread-unique seed: stack address differs per thread
                const jitter_range = backoff_ns / 4;
                const jitter = if (jitter_range > 0) blk: {
                    const thread_seed = @intFromPtr(&retry_count);
                    const combined = @as(u128, retry_count) * 7919 + @as(u128, thread_seed);
                    break :blk @as(u64, @truncate(combined % (jitter_range * 2)));
                } else 0;
                const sleep_ns = backoff_ns -| jitter_range + jitter;
                const ts = std.c.timespec{
                    .sec = @intCast(sleep_ns / std.time.ns_per_s),
                    .nsec = @intCast(sleep_ns % std.time.ns_per_s),
                };
                _ = std.c.nanosleep(&ts, null);
                backoff_ns = @min(backoff_ns * 2, max_backoff_ns);
                continue;
            } else {
                // No limit - monotonic is fine for metrics-only counter
                _ = self.in_use.fetchAdd(1, .monotonic);
                break;
            }
        }

        const rt = self.pool.acquire() catch |err| {
            _ = self.in_use.fetchSub(1, .monotonic);
            if (collect_pool_metrics) {
                if (wait_timer) |*t| {
                    self.recordWait(t.read());
                } else {
                    self.recordWait(0);
                }
            }
            return err;
        };
        if (collect_pool_metrics) {
            if (wait_timer) |*t| {
                self.recordWait(t.read());
            } else {
                self.recordWait(0);
            }
        }

        // TTL-based pooling is the only consumer of first_use_ns, so skip
        // the clock read for every other policy.
        if (self.pooling_policy == .reuse_bounded_by_ttl and rt.first_use_ns == null) {
            rt.first_use_ns = compat.realtimeNowNs() catch 0;
        }

        return rt;
    }

    pub fn setPoolingPolicy(self: *Self, policy: contract_runtime.PoolingPolicy) void {
        self.pooling_policy = policy;
    }

    fn emitRuntimeEvent(kind: zq.security_events.SecurityEventKind, detail: []const u8) void {
        zq.security_events.emitGlobal(
            zq.security_events.SecurityEvent.init(kind, "runtime", detail),
        );
    }

    fn releaseForRequest(self: *Self, rt: *zq.LockFreePool.Runtime) void {
        if (rt.user_data) |ptr| {
            const runtime: *Runtime = @ptrCast(@alignCast(ptr));
            runtime.prepareForPoolRelease();
        }

        // Sandbox audit runs only under runtime safety; a failed audit
        // forces recycle regardless of policy. The `comptime` guard lets
        // ReleaseFast drop both checks entirely.
        var audit_poisoned = false;
        if (comptime std.debug.runtime_safety) {
            rt.assertPersistentStringsOutsideArena() catch {
                _ = self.persistent_escape_failures.fetchAdd(1, .monotonic);
                std.log.err("sandbox: persistent string escaped into request arena", .{});
                emitRuntimeEvent(.persistent_string_escape, "persistent string escaped into request arena");
                audit_poisoned = true;
            };

            _ = rt.auditAndResetArena() catch {
                _ = self.arena_audit_failures.fetchAdd(1, .monotonic);
                std.log.err("sandbox: request arena audit failed", .{});
                emitRuntimeEvent(.arena_audit_failure, "post-reset arena state was non-zero");
                audit_poisoned = true;
            };
        }

        // request_count still reflects completed-BEFORE-this-one, so add
        // one when comparing against the policy threshold.
        const request_count_after: u64 = rt.request_count + 1;
        const policy_recycle = switch (self.pooling_policy) {
            .reuse_unbounded => false,
            .ephemeral => true,
            .reuse_bounded_by_count => request_count_after >= self.pooling_thresholds.max_requests,
            .reuse_bounded_by_ttl => blk: {
                const first = rt.first_use_ns orelse break :blk false;
                const now = compat.realtimeNowNs() catch break :blk false;
                break :blk now -| first >= self.pooling_thresholds.ttl_ns;
            },
        };

        if (audit_poisoned or policy_recycle) {
            _ = self.recycles.fetchAdd(1, .monotonic);
            self.pool.dropRuntime(rt);
            _ = self.in_use.fetchSub(1, .monotonic);
            return;
        }

        self.pool.release(rt);
        _ = self.in_use.fetchSub(1, .monotonic);
    }

    pub fn getMetrics(self: *Self) struct {
        requests: u64,
        exhausted: u64,
        avg_wait_ns: u64,
        max_wait_ns: u64,
        avg_exec_ns: u64,
        max_exec_ns: u64,
        wait_p50_ns: u64,
        wait_p95_ns: u64,
        wait_p99_ns: u64,
        exec_p50_ns: u64,
        exec_p95_ns: u64,
        exec_p99_ns: u64,
        arena_audit_failures: u64,
        persistent_escape_failures: u64,
        recycles: u64,
        pooling_policy: contract_runtime.PoolingPolicy,
    } {
        const requests = self.request_seq.load(.acquire);
        const wait_total = self.total_wait_ns.load(.acquire);
        const exec_total = self.total_exec_ns.load(.acquire);
        return .{
            .requests = requests,
            .exhausted = self.exhausted_count.load(.acquire),
            .avg_wait_ns = if (requests > 0) wait_total / requests else 0,
            .max_wait_ns = self.max_wait_ns.load(.acquire),
            .avg_exec_ns = if (requests > 0) exec_total / requests else 0,
            .max_exec_ns = self.max_exec_ns.load(.acquire),
            .wait_p50_ns = self.wait_percentiles.getPercentile(50.0),
            .wait_p95_ns = self.wait_percentiles.getPercentile(95.0),
            .wait_p99_ns = self.wait_percentiles.getPercentile(99.0),
            .exec_p50_ns = self.exec_percentiles.getPercentile(50.0),
            .exec_p95_ns = self.exec_percentiles.getPercentile(95.0),
            .exec_p99_ns = self.exec_percentiles.getPercentile(99.0),
            .arena_audit_failures = self.arena_audit_failures.load(.acquire),
            .persistent_escape_failures = self.persistent_escape_failures.load(.acquire),
            .recycles = self.recycles.load(.acquire),
            .pooling_policy = self.pooling_policy,
        };
    }

    fn recordWait(self: *Self, ns: u64) void {
        _ = self.total_wait_ns.fetchAdd(ns, .acq_rel);
        updateMax(&self.max_wait_ns, ns);
        self.wait_percentiles.record(ns);
    }

    fn recordExec(self: *Self, ns: u64) void {
        _ = self.total_exec_ns.fetchAdd(ns, .acq_rel);
        updateMax(&self.max_exec_ns, ns);
        self.exec_percentiles.record(ns);
    }

    fn isHandlerInvalid(err: anyerror) bool {
        return err == error.HandlerNotCallable or err == error.NoHandler or err == error.NotCallable;
    }

    fn invalidateRuntime(self: *Self, base_rt: *zq.LockFreePool.Runtime) void {
        if (base_rt.user_data != null) {
            runtimeUserDeinit(base_rt, self.allocator);
        }
    }

    fn updateMax(target: *std.atomic.Value(u64), value: u64) void {
        var current = target.load(.acquire);
        while (value > current) {
            if (target.cmpxchgStrong(current, value, .acq_rel, .acquire) == null) return;
            current = target.load(.acquire);
        }
    }

    fn ensureRuntime(self: *Self, base_rt: *zq.LockFreePool.Runtime) !*Runtime {
        if (base_rt.user_data) |ptr| {
            return @ptrCast(@alignCast(ptr));
        }

        self.runtime_init_mutex.lock();
        defer self.runtime_init_mutex.unlock();

        if (base_rt.user_data) |ptr| {
            return @ptrCast(@alignCast(ptr));
        }

        const rt = try Runtime.initFromPool(base_rt, self.config);
        errdefer rt.deinit();

        // Inject shared trace file/mutex from pool
        if (self.trace_file != null and self.trace_mutex != null) {
            rt.trace_file = self.trace_file;
            rt.trace_mutex = self.trace_mutex;
        }

        try self.loadHandlerCached(rt);
        base_rt.assertPersistentStringsOutsideArena() catch |err| switch (err) {
            error.PersistentStringEscapedIntoArena => {
                std.log.err("sandbox: persistent string escaped into request arena during handler load", .{});
                return error.PersistentStringEscapedIntoArena;
            },
        };

        base_rt.user_data = rt;
        base_rt.user_deinit = runtimeUserDeinit;
        return rt;
    }

    fn loadHandlerCached(self: *Self, rt: *Runtime) !void {
        // Temporarily disable hybrid mode during handler loading.
        // Handler function objects must use persistent allocation so they survive
        // arena resets between requests. Without this, closures or function objects
        // could reference arena-allocated memory that gets invalidated.
        const saved_hybrid = rt.ctx.hybrid;
        const saved_hybrid_mode = rt.gc_state.hybrid_mode;
        rt.ctx.hybrid = null;
        rt.gc_state.hybrid_mode = false;
        defer {
            rt.ctx.hybrid = saved_hybrid;
            rt.gc_state.hybrid_mode = saved_hybrid_mode;
        }

        // Fast path: use embedded bytecode if available (precompiled at build time)
        if (self.embedded_bytecode) |entry_bytecode| {
            // Load dependency modules first
            if (self.runtime_dep_bytecodes) |deps| {
                // Runtime-provided deps (self-extracting binary)
                for (deps) |dep_data| {
                    try rt.loadFromCachedBytecodeNoHandler(dep_data);
                }
            } else if (embedded_handler.dep_count > 0) {
                // Compile-time embedded deps
                for (embedded_handler.dep_bytecodes[0..embedded_handler.dep_count]) |dep_data| {
                    try rt.loadFromCachedBytecodeNoHandler(dep_data);
                }
            }
            try rt.loadFromCachedBytecode(entry_bytecode);
            return;
        }

        // Fallback: runtime compilation (for development without -Dhandler)
        const key = bytecode_cache.BytecodeCache.cacheKey(self.handler_code);

        // Fast path: check cache without lock (read-only, safe for concurrent access)
        if (self.cache.getRaw(key)) |cached_data| {
            try rt.loadFromCachedBytecode(cached_data);
            return;
        }

        // Slow path: acquire lock for parsing (double-checked locking pattern)
        // This prevents "thundering herd" where multiple threads all parse on cold start
        self.cache_mutex.lock();
        defer self.cache_mutex.unlock();

        // Double-check: another thread may have populated cache while we waited
        if (self.cache.getRaw(key)) |cached_data| {
            try rt.loadFromCachedBytecode(cached_data);
            return;
        }

        // Cache MISS confirmed: parse and compile (only one thread does this)
        var buffer: [65536]u8 = undefined;
        const serialized = try rt.loadCodeWithCaching(self.handler_code, self.handler_filename, &buffer);

        // Store in cache for future hits
        if (serialized) |data| {
            if (!self.cache.contains(key)) {
                self.cache.putRaw(key, data) catch |err| {
                    std.log.warn("Bytecode cache insert failed: {}", .{err});
                };
            }
        }
    }

    fn runtimeUserDeinit(base_rt: *zq.LockFreePool.Runtime, allocator: std.mem.Allocator) void {
        _ = allocator;
        if (base_rt.user_data) |ptr| {
            const rt: *Runtime = @ptrCast(@alignCast(ptr));
            rt.deinit();
            base_rt.user_data = null;
            base_rt.user_deinit = null;
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

const TestErrorInt = @Int(.unsigned, @bitSizeOf(anyerror));

const TestCapturedHeader = struct {
    name: []u8,
    value: []u8,
};

const TestCapturedRequest = struct {
    method: []u8,
    path: []u8,
    headers: std.ArrayListUnmanaged(TestCapturedHeader) = .empty,
    body: []u8,
    raw: []u8,

    fn deinit(self: *TestCapturedRequest, allocator: std.mem.Allocator) void {
        allocator.free(self.method);
        allocator.free(self.path);
        for (self.headers.items) |header| {
            allocator.free(header.name);
            allocator.free(header.value);
        }
        self.headers.deinit(allocator);
        allocator.free(self.body);
        allocator.free(self.raw);
    }

    fn getHeader(self: *const TestCapturedRequest, name: []const u8) ?[]const u8 {
        var i = self.headers.items.len;
        while (i > 0) {
            i -= 1;
            const item = self.headers.items[i];
            if (ascii.eqlIgnoreCase(item.name, name)) {
                return item.value;
            }
        }
        return null;
    }
};

const TestHttpServer = struct {
    allocator: std.mem.Allocator,
    io_backend: std.Io.Threaded,
    listener: std.Io.net.Server,
    port: u16,
    mode: Mode,
    thread: ?std.Thread = null,
    closed: bool = false,
    thread_error: std.atomic.Value(TestErrorInt) = std.atomic.Value(TestErrorInt).init(0),

    const Mode = enum {
        echo_request_json,
        large_plain_text,
    };

    fn init(allocator: std.mem.Allocator, mode: Mode) !TestHttpServer {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        const io = io_backend.io();
        const address = try std.Io.net.IpAddress.parseIp4("127.0.0.1", 0);
        const listener = try address.listen(io, .{ .reuse_address = true });
        return .{
            .allocator = allocator,
            .io_backend = io_backend,
            .listener = listener,
            .port = listener.socket.address.getPort(),
            .mode = mode,
        };
    }

    fn start(self: *TestHttpServer) !void {
        self.thread = try std.Thread.spawn(.{}, run, .{self});
    }

    fn url(self: *const TestHttpServer, allocator: std.mem.Allocator, path: []const u8) ![]u8 {
        return std.fmt.allocPrint(allocator, "http://127.0.0.1:{d}{s}", .{ self.port, path });
    }

    fn join(self: *TestHttpServer) !void {
        if (self.closed) return;
        self.closed = true;
        if (self.thread) |thread| {
            thread.join();
            self.thread = null;
        }
        const io = self.io_backend.io();
        self.listener.deinit(io);
        self.io_backend.deinit();
        const err_int = self.thread_error.swap(0, .acq_rel);
        if (err_int != 0) {
            return @errorFromInt(err_int);
        }
    }

    fn run(self: *TestHttpServer) void {
        self.runInner() catch |err| {
            self.thread_error.store(@intFromError(err), .release);
        };
    }

    fn runInner(self: *TestHttpServer) !void {
        const io = self.io_backend.io();
        var stream = while (true) {
            break self.listener.accept(io) catch |err| switch (err) {
                error.ConnectionAborted => continue,
                error.SocketNotListening => return,
                else => return err,
            };
        };
        defer stream.close(io);

        switch (self.mode) {
            .echo_request_json => try self.respondWithEcho(&stream, io),
            .large_plain_text => try self.respondWithLargeBody(&stream, io),
        }
    }

    fn respondWithEcho(self: *TestHttpServer, stream: *std.Io.net.Stream, io: std.Io) !void {
        var captured = try captureRequest(self.allocator, stream, io);
        defer captured.deinit(self.allocator);

        var aw: std.Io.Writer.Allocating = .init(self.allocator);
        defer aw.deinit();
        var json: std.json.Stringify = .{ .writer = &aw.writer };
        try json.beginObject();
        try json.objectField("method");
        try json.write(captured.method);
        try json.objectField("path");
        try json.write(captured.path);
        try json.objectField("contentType");
        try json.write(captured.getHeader("content-type") orelse "");
        try json.objectField("xTest");
        try json.write(captured.getHeader("x-test") orelse "");
        try json.objectField("contentLength");
        try json.write(captured.getHeader("content-length") orelse "");
        try json.objectField("transferEncoding");
        try json.write(captured.getHeader("transfer-encoding") orelse "");
        try json.objectField("body");
        try json.write(captured.body);
        try json.objectField("raw");
        try json.write(captured.raw);
        try json.endObject();
        const body = try aw.toOwnedSlice();
        defer self.allocator.free(body);

        try writeTestResponse(stream, io, 201, "Created", &.{
            "Content-Type: application/json",
            "x-reply: ok",
        }, body);
    }

    fn respondWithLargeBody(self: *TestHttpServer, stream: *std.Io.net.Stream, io: std.Io) !void {
        _ = self;
        try writeTestResponse(
            stream,
            io,
            200,
            "OK",
            &.{"Content-Type: text/plain"},
            "0123456789abcdef0123456789abcdef",
        );
    }
};

fn captureRequest(allocator: std.mem.Allocator, stream: *std.Io.net.Stream, io: std.Io) !TestCapturedRequest {
    const raw = try readRawRequestBytes(allocator, stream, io);
    errdefer allocator.free(raw);
    const header_sep = findTestHeaderEnd(raw) orelse return error.InvalidTestRequest;
    const header_end = header_sep + 4;
    const request_head = raw[0..header_sep];
    const line_end = std.mem.indexOf(u8, request_head, "\r\n") orelse return error.InvalidTestRequest;
    const request_line = request_head[0..line_end];
    const first_space = std.mem.indexOfScalar(u8, request_line, ' ') orelse return error.InvalidTestRequest;
    const rest = request_line[first_space + 1 ..];
    const second_space_rel = std.mem.indexOfScalar(u8, rest, ' ') orelse return error.InvalidTestRequest;

    const method = try allocator.dupe(u8, request_line[0..first_space]);
    errdefer allocator.free(method);
    const path = try allocator.dupe(u8, rest[0..second_space_rel]);
    errdefer allocator.free(path);

    var headers: std.ArrayListUnmanaged(TestCapturedHeader) = .empty;
    errdefer {
        for (headers.items) |header| {
            allocator.free(header.name);
            allocator.free(header.value);
        }
        headers.deinit(allocator);
    }
    var line_start = line_end + 2;
    while (line_start < request_head.len) {
        const next_line_end = std.mem.indexOfPos(u8, request_head, line_start, "\r\n") orelse request_head.len;
        if (next_line_end == line_start) break;
        const line = request_head[line_start..next_line_end];

        const colon = std.mem.indexOfScalar(u8, line, ':') orelse continue;
        const name = std.mem.trim(u8, line[0..colon], " \t");
        const value = std.mem.trim(u8, line[colon + 1 ..], " \t");
        const name_copy = try allocator.dupe(u8, name);
        errdefer allocator.free(name_copy);
        const value_copy = try allocator.dupe(u8, value);
        errdefer allocator.free(value_copy);
        try headers.append(allocator, .{
            .name = name_copy,
            .value = value_copy,
        });
        line_start = next_line_end + 2;
    }

    const body = try allocator.dupe(u8, raw[header_end..]);
    errdefer allocator.free(body);

    return .{
        .method = method,
        .path = path,
        .headers = headers,
        .body = body,
        .raw = raw,
    };
}

fn readRawRequestBytes(allocator: std.mem.Allocator, stream: *std.Io.net.Stream, io: std.Io) ![]u8 {
    var raw: std.ArrayList(u8) = .empty;
    defer raw.deinit(allocator);

    while (true) {
        var chunk: [1024]u8 = undefined;
        var vecs: [1][]u8 = .{chunk[0..]};
        const n = io.vtable.netRead(io.userdata, stream.socket.handle, &vecs) catch |err| switch (err) {
            error.ConnectionResetByPeer => break,
            else => return err,
        };
        if (n == 0) break;
        try raw.appendSlice(allocator, chunk[0..n]);
        if (requestMessageLength(raw.items)) |message_len| {
            if (message_len < raw.items.len) {
                try raw.resize(allocator, message_len);
            }
            break;
        }
    }

    return try raw.toOwnedSlice(allocator);
}

fn findTestHeaderEnd(raw: []const u8) ?usize {
    return std.mem.indexOf(u8, raw, "\r\n\r\n") orelse null;
}

fn requestMessageLength(raw: []const u8) ?usize {
    const header_sep = findTestHeaderEnd(raw) orelse return null;
    const header_end = header_sep + 4;
    const header_block = raw[0..header_sep];

    var content_length: ?usize = null;
    var chunked = false;
    var line_start: usize = 0;
    while (line_start < header_block.len) {
        const line_end = std.mem.indexOfPos(u8, header_block, line_start, "\r\n") orelse header_block.len;
        const line = header_block[line_start..line_end];
        line_start = line_end + 2;
        if (line.len == 0) continue;

        const colon = std.mem.indexOfScalar(u8, line, ':') orelse continue;
        const name = std.mem.trim(u8, line[0..colon], " \t");
        const value = std.mem.trim(u8, line[colon + 1 ..], " \t");

        if (ascii.eqlIgnoreCase(name, "content-length")) {
            content_length = std.fmt.parseInt(usize, value, 10) catch return null;
        } else if (ascii.eqlIgnoreCase(name, "transfer-encoding")) {
            chunked = ascii.indexOfIgnoreCase(value, "chunked") != null;
        }
    }

    if (chunked) {
        const body_len = chunkedBodyLength(raw[header_end..]) orelse return null;
        return header_end + body_len;
    }
    if (content_length) |len| {
        if (raw.len < header_end + len) return null;
        return header_end + len;
    }
    return header_end;
}

fn chunkedBodyLength(body: []const u8) ?usize {
    var idx: usize = 0;
    while (true) {
        const line_end = std.mem.indexOfPos(u8, body, idx, "\r\n") orelse return null;
        const size_line = body[idx..line_end];
        const semi = std.mem.indexOfScalar(u8, size_line, ';') orelse size_line.len;
        const size_text = std.mem.trim(u8, size_line[0..semi], " \t");
        const chunk_len = std.fmt.parseInt(usize, size_text, 16) catch return null;
        idx = line_end + 2;

        if (chunk_len == 0) {
            while (true) {
                const trailer_end = std.mem.indexOfPos(u8, body, idx, "\r\n") orelse return null;
                if (trailer_end == idx) return trailer_end + 2;
                idx = trailer_end + 2;
            }
        }

        if (body.len < idx + chunk_len + 2) return null;
        idx += chunk_len;
        if (!std.mem.eql(u8, body[idx .. idx + 2], "\r\n")) return null;
        idx += 2;
    }
}

fn writeTestResponse(
    stream: *std.Io.net.Stream,
    io: std.Io,
    status: u16,
    reason: []const u8,
    headers: []const []const u8,
    body: []const u8,
) !void {
    var out_buf: [4096]u8 = undefined;
    var writer = stream.writer(io, &out_buf);
    const out = &writer.interface;

    try out.print("HTTP/1.1 {d} {s}\r\n", .{ status, reason });
    try out.print("Content-Length: {d}\r\n", .{body.len});
    for (headers) |header| {
        try out.writeAll(header);
        try out.writeAll("\r\n");
    }
    try out.writeAll("Connection: close\r\n\r\n");
    if (body.len > 0) {
        try out.writeAll(body);
    }
    try writer.interface.flush();
}

fn durableTestDirPath(allocator: std.mem.Allocator, tmp_dir: *const std.testing.TmpDir) ![]u8 {
    return std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});
}

fn makeTestRequest(
    allocator: std.mem.Allocator,
    method: []const u8,
    url: []const u8,
    idempotency_key: ?[]const u8,
) !HttpRequestOwned {
    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, method),
        .url = try allocator.dupe(u8, url),
        .headers = .empty,
        .body = null,
    };
    errdefer request.deinit(allocator);

    if (idempotency_key) |key| {
        try request.headers.append(allocator, .{
            .key = try allocator.dupe(u8, "idempotency-key"),
            .value = try allocator.dupe(u8, key),
        });
    }

    return request;
}

fn seedIncompleteDurableRandomStep(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    key: []const u8,
    step_name: []const u8,
    result_json: []const u8,
) !void {
    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const path = try rt.buildDurableOplogPath(key);
    defer allocator.free(path);

    const fd = try openOplogFile(allocator, path);
    defer std.Io.Threaded.closeFd(fd);

    var state = zq.trace.DurableState.init(allocator, &.{}, fd);
    defer state.deinit();

    const header_names = [_][]const u8{"idempotency-key"};
    const header_values = [_][]const u8{key};
    const result = zq.trace.jsonToJSValue(rt.ctx, result_json);

    state.persistRunKey(key);
    state.persistRequest("GET", "/", &header_names, &header_values, null);
    state.persistStepStart(step_name);
    state.persistIO("builtin", "Math.random", rt.ctx, &.{}, result);
    state.persistStepResult(step_name, rt.ctx, result);
}

// Pull tests from sibling files that are not otherwise reachable from this
// module's import graph. Zig's test runner only analyzes files transitively
// reached from the test root, so handler_loader.zig (used by replay_runner,
// test_runner, and durable_recovery) needs an explicit hook here.
test {
    _ = @import("handler_loader.zig");
    _ = @import("websocket_codec.zig");
    _ = @import("websocket_pool.zig");
    _ = @import("ws_gateway.zig");
    _ = @import("ws_frame_loop.zig");
    _ = @import("durable_fetch.zig");
}

test "Runtime creation" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    try std.testing.expect(rt.ctx.sp == 0);
}

test "virtual module import alias resolves to callable binding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\import { env as getEnv } from "zigttp:env";
        \\function handler(req) {
        \\  return Response.text(typeof getEnv);
        \\}
    ;
    try rt.loadHandler(handler_code, "<import-alias>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();
    try std.testing.expectEqualStrings("function", response.body);
}

test "packaged extension module import resolves to callable binding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\import { double, isEven, clockModulo } from "zigttp-ext:math";
        \\function handler(req) {
        \\  return Response.json({
        \\    doubled: double(21),
        \\    even: isEven(42),
        \\    modulo: clockModulo(1)
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<extension-import>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();
    try std.testing.expectEqualStrings("{\"doubled\":42,\"even\":true,\"modulo\":0}", response.body);
}

test "built-in module import runs under capability wrapper context" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\import { uuid, nanoid } from "zigttp:id";
        \\function handler(req) {
        \\  const a = uuid();
        \\  const b = nanoid(4);
        \\  return Response.json({
        \\    uuidLen: a.length,
        \\    nanoLen: b.length
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<builtin-capability-wrapper>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();
    try std.testing.expectEqualStrings("{\"uuidLen\":36,\"nanoLen\":4}", response.body);
}

test "durable run reuses completed response for duplicate key" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\function handler(req) {
        \\  const key = req.headers.get("idempotency-key") ?? "missing";
        \\  return run(key, () => {
        \\    return Response.json({
        \\      key: key,
        \\      value: Math.random()
        \\    });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-duplicate>");

    var first_request = try makeTestRequest(allocator, "GET", "/", "order:123");
    defer first_request.deinit(allocator);
    var first_response = try rt.executeHandler(first_request.asView());
    defer first_response.deinit();
    const first_body = try allocator.dupe(u8, first_response.body);

    var second_request = try makeTestRequest(allocator, "GET", "/", "order:123");
    defer second_request.deinit(allocator);
    var second_response = try rt.executeHandler(second_request.asView());
    defer second_response.deinit();

    try std.testing.expectEqualStrings(first_body, second_response.body);

    const path = try rt.buildDurableOplogPath("order:123");
    defer allocator.free(path);

    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"fn\":\"Math.random\""));

    var parsed = try zq.trace.parseDurableOplog(allocator, source);
    defer parsed.deinit();

    try std.testing.expect(parsed.complete);
    try std.testing.expectEqualStrings("order:123", parsed.run_key.?);
    try std.testing.expect(parsed.response != null);
}

test "durable run resumes from completed step state" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    try seedIncompleteDurableRandomStep(
        allocator,
        durable_dir,
        "resume:123",
        "seed",
        "0.25",
    );

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { run, step } from "zigttp:durable";
        \\function handler(req) {
        \\  const key = req.headers.get("idempotency-key") ?? "missing";
        \\  return run(key, () => {
        \\    const seed = step("seed", () => Math.random());
        \\    const stamp = step("stamp", () => Date.now());
        \\    return Response.json({ seed: seed, stamp: stamp });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-resume>");

    var request = try makeTestRequest(allocator, "GET", "/", "resume:123");
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    var parsed_json = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed_json.deinit();
    const obj = parsed_json.value.object;

    try std.testing.expectEqual(@as(f64, 0.25), obj.get("seed").?.float);
    try std.testing.expect(obj.get("stamp") != null);

    const path = try rt.buildDurableOplogPath("resume:123");
    defer allocator.free(path);

    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"fn\":\"Math.random\""));
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"fn\":\"Date.now\""));

    var parsed = try zq.trace.parseDurableOplog(allocator, source);
    defer parsed.deinit();

    try std.testing.expect(parsed.complete);
    try std.testing.expectEqualStrings("resume:123", parsed.run_key.?);
}

test "durable sleepUntil returns pending response without duplicating wait" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { run, sleepUntil } from "zigttp:durable";
        \\function handler(req) {
        \\  return run("timer:123", () => {
        \\    sleepUntil(4102444800000);
        \\    return Response.json({ ok: true });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-sleep>");

    var first_request = try makeTestRequest(allocator, "GET", "/", null);
    defer first_request.deinit(allocator);
    var first_response = try rt.executeHandler(first_request.asView());
    defer first_response.deinit();
    try std.testing.expectEqual(@as(u16, 202), first_response.status);
    try std.testing.expect(std.mem.indexOf(u8, first_response.body, "\"type\":\"timer\"") != null);

    var second_request = try makeTestRequest(allocator, "GET", "/", null);
    defer second_request.deinit(allocator);
    var second_response = try rt.executeHandler(second_request.asView());
    defer second_response.deinit();
    try std.testing.expectEqual(@as(u16, 202), second_response.status);
    try std.testing.expect(std.mem.indexOf(u8, second_response.body, "\"pending\":true") != null);

    const path = try rt.buildDurableOplogPath("timer:123");
    defer allocator.free(path);

    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"type\":\"wait_timer\""));
    try std.testing.expectEqual(@as(usize, 0), std.mem.count(u8, source, "\"type\":\"resume_timer\""));
}

test "durable waitSignal resumes from queued signal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { run, waitSignal, signal } from "zigttp:durable";
        \\function handler(req) {
        \\  if (req.url === "/signal") {
        \\    return Response.json({ delivered: signal("job:123", "approved", { ok: true }) });
        \\  }
        \\  return run("job:123", () => {
        \\    const payload = waitSignal("approved");
        \\    return Response.json(payload);
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-signal>");

    var wait_request = try makeTestRequest(allocator, "GET", "/wait", null);
    defer wait_request.deinit(allocator);
    var pending_response = try rt.executeHandler(wait_request.asView());
    defer pending_response.deinit();
    try std.testing.expectEqual(@as(u16, 202), pending_response.status);
    try std.testing.expect(std.mem.indexOf(u8, pending_response.body, "\"type\":\"signal\"") != null);

    var signal_request = try makeTestRequest(allocator, "GET", "/signal", null);
    defer signal_request.deinit(allocator);
    var signal_response = try rt.executeHandler(signal_request.asView());
    defer signal_response.deinit();

    var parsed_signal = try std.json.parseFromSlice(std.json.Value, allocator, signal_response.body, .{});
    defer parsed_signal.deinit();
    try std.testing.expect(parsed_signal.value.object.get("delivered").?.bool);

    var resume_request = try makeTestRequest(allocator, "GET", "/wait", null);
    defer resume_request.deinit(allocator);
    var resumed_response = try rt.executeHandler(resume_request.asView());
    defer resumed_response.deinit();
    try std.testing.expectEqual(@as(u16, 200), resumed_response.status);

    var parsed_payload = try std.json.parseFromSlice(std.json.Value, allocator, resumed_response.body, .{});
    defer parsed_payload.deinit();
    try std.testing.expect(parsed_payload.value.object.get("ok").?.bool);

    const path = try rt.buildDurableOplogPath("job:123");
    defer allocator.free(path);

    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"type\":\"wait_signal\""));
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"type\":\"resume_signal\""));
}

test "durable signal returns false after completion" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { run, signal } from "zigttp:durable";
        \\function handler(req) {
        \\  if (req.url === "/signal") {
        \\    return Response.json({ delivered: signal("done:123", "approved", { ok: true }) });
        \\  }
        \\  return run("done:123", () => Response.json({ ok: true }));
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-signal-false>");

    var run_request = try makeTestRequest(allocator, "GET", "/run", null);
    defer run_request.deinit(allocator);
    var run_response = try rt.executeHandler(run_request.asView());
    defer run_response.deinit();
    try std.testing.expectEqual(@as(u16, 200), run_response.status);

    var signal_request = try makeTestRequest(allocator, "GET", "/signal", null);
    defer signal_request.deinit(allocator);
    var signal_response = try rt.executeHandler(signal_request.asView());
    defer signal_response.deinit();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, signal_response.body, .{});
    defer parsed.deinit();
    try std.testing.expect(!parsed.value.object.get("delivered").?.bool);
}

test "durable step outside run fails" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { step } from "zigttp:durable";
        \\function handler(req) {
        \\  step("seed", () => 1);
        \\  return Response.json({ ok: true });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-step-outside-run>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);

    const request_val = try rt.createRequestObject(request.asView());
    current_runtime = rt;
    defer current_runtime = null;
    try std.testing.expectError(error.NativeFunctionError, rt.callGlobalFunction("handler", &[_]zq.JSValue{request_val}));
    rt.resetForNextRequest();
}

test "httpRequest native binding reports disabled bridge by default" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\function handler(req) {
        \\  const out = httpRequest(JSON.stringify({ url: "http://example.com" }));
        \\  return Response.json(JSON.parse(out));
        \\}
    ;
    try rt.loadHandler(handler_code, "<http-bridge-disabled>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value == .object);
    const obj = parsed.value.object;
    const ok_v = obj.get("ok") orelse return error.BadGolden;
    try std.testing.expect(ok_v == .bool and !ok_v.bool);
    const err_v = obj.get("error") orelse return error.BadGolden;
    try std.testing.expect(err_v == .string);
    try std.testing.expectEqualStrings("OutboundHttpDisabled", err_v.string);
}

test "httpRequest native binding enforces allowlisted host before dialing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .outbound_http_enabled = true,
        .outbound_allow_host = "localhost",
    });
    defer rt.deinit();

    const handler_code =
        \\function handler(req) {
        \\  const out = httpRequest(JSON.stringify({ url: "http://example.com" }));
        \\  return Response.json(JSON.parse(out));
        \\}
    ;
    try rt.loadHandler(handler_code, "<http-bridge-allowlist>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value == .object);
    const obj = parsed.value.object;
    const ok_v = obj.get("ok") orelse return error.BadGolden;
    try std.testing.expect(ok_v == .bool and !ok_v.bool);
    const err_v = obj.get("error") orelse return error.BadGolden;
    try std.testing.expect(err_v == .string);
    try std.testing.expectEqualStrings("HostNotAllowed", err_v.string);
}

test "request helpers expose body parsing and case-insensitive headers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\function handler(req) {
        \\  const body = req.body ?? "";
        \\  const data = req.json();
        \\  return Response.json({
        \\    contentType: req.headers.get("content-type"),
        \\    auth: req.headers.get("AUTHORIZATION"),
        \\    missing: req.headers.get("x-missing"),
        \\    body: body,
        \\    name: data.name,
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<request-helpers>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = try allocator.dupe(u8, "{\"name\":\"zigttp\"}"),
    };
    defer request.deinit(allocator);

    try request.headers.append(allocator, .{
        .key = try allocator.dupe(u8, "Content-Type"),
        .value = try allocator.dupe(u8, "application/json"),
    });
    try request.headers.append(allocator, .{
        .key = try allocator.dupe(u8, "Authorization"),
        .value = try allocator.dupe(u8, "Bearer test-token"),
    });
    try request.headers.append(allocator, .{
        .key = try allocator.dupe(u8, "authorization"),
        .value = try allocator.dupe(u8, "Bearer override"),
    });

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("application/json", obj.get("contentType").?.string);
    try std.testing.expectEqualStrings("Bearer override", obj.get("auth").?.string);
    try std.testing.expect(obj.get("missing") == null); // undefined values are omitted from JSON
    try std.testing.expectEqualStrings("{\"name\":\"zigttp\"}", obj.get("body").?.string);
    try std.testing.expectEqualStrings("zigttp", obj.get("name").?.string);
}

test "request helpers define empty and invalid body semantics" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\function handler(req) {
        \\  return Response.json({
        \\    body: req.body ?? "",
        \\    jsonIsUndefined: req.json() === undefined,
        \\    missingIsUndefined: req.headers.get("x-missing") === undefined,
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<request-body-semantics>");

    var empty_request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer empty_request.deinit(allocator);

    var empty_response = try rt.executeHandler(empty_request.asView());
    defer empty_response.deinit();

    var empty_parsed = try std.json.parseFromSlice(std.json.Value, allocator, empty_response.body, .{});
    defer empty_parsed.deinit();
    const empty_obj = empty_parsed.value.object;
    try std.testing.expectEqualStrings("", empty_obj.get("body").?.string);
    try std.testing.expectEqual(true, empty_obj.get("jsonIsUndefined").?.bool);
    try std.testing.expectEqual(true, empty_obj.get("missingIsUndefined").?.bool);

    var invalid_request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = try allocator.dupe(u8, "{invalid json"),
    };
    defer invalid_request.deinit(allocator);

    var invalid_response = try rt.executeHandler(invalid_request.asView());
    defer invalid_response.deinit();

    var invalid_parsed = try std.json.parseFromSlice(std.json.Value, allocator, invalid_response.body, .{});
    defer invalid_parsed.deinit();
    const invalid_obj = invalid_parsed.value.object;
    try std.testing.expectEqualStrings("{invalid json", invalid_obj.get("body").?.string);
    try std.testing.expectEqual(true, invalid_obj.get("jsonIsUndefined").?.bool);
    try std.testing.expectEqual(true, invalid_obj.get("missingIsUndefined").?.bool);
}

test "Headers Request and Response factories share the HTTP model" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\function handler(req) {
        \\  const headers = Headers({
        \\    "Content-Type": "application/json",
        \\    "X-Test": "one"
        \\  });
        \\  headers.append("X-Test", "two");
        \\  headers.set("X-Mode", "fast");
        \\  const beforeDelete = headers.has("x-mode");
        \\  headers.delete("x-mode");
        \\  const request = Request("/items?id=41&name=zigttp", {
        \\    method: "POST",
        \\    headers: headers,
        \\    body: "{\"ok\":true}"
        \\  });
        \\  const data = request.json();
        \\  const response = Response("done", {
        \\    status: 201,
        \\    headers: { "X-Reply": "ok" }
        \\  });
        \\  return Response.json({
        \\    combinedHeader: headers.get("x-test"),
        \\    beforeDelete: beforeDelete,
        \\    afterDelete: headers.has("x-mode"),
        \\    requestMethod: request.method,
        \\    requestPath: request.path,
        \\    requestId: request.query.id,
        \\    requestName: request.query.name,
        \\    requestType: request.headers.get("content-type"),
        \\    requestOk: data.ok,
        \\    responseStatus: response.status,
        \\    responseOk: response.ok,
        \\    responseReply: response.headers.get("x-reply"),
        \\    responseType: response.headers.get("content-type"),
        \\    responseText: response.text()
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<http-factories>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("one, two", obj.get("combinedHeader").?.string);
    try std.testing.expectEqual(true, obj.get("beforeDelete").?.bool);
    try std.testing.expectEqual(false, obj.get("afterDelete").?.bool);
    try std.testing.expectEqualStrings("POST", obj.get("requestMethod").?.string);
    try std.testing.expectEqualStrings("/items", obj.get("requestPath").?.string);
    try std.testing.expectEqual(@as(i64, 41), obj.get("requestId").?.integer);
    try std.testing.expectEqualStrings("zigttp", obj.get("requestName").?.string);
    try std.testing.expectEqualStrings("application/json", obj.get("requestType").?.string);
    try std.testing.expectEqual(true, obj.get("requestOk").?.bool);
    try std.testing.expectEqual(@as(i64, 201), obj.get("responseStatus").?.integer);
    try std.testing.expectEqual(true, obj.get("responseOk").?.bool);
    try std.testing.expectEqualStrings("ok", obj.get("responseReply").?.string);
    try std.testing.expectEqualStrings("text/plain; charset=utf-8", obj.get("responseType").?.string);
    try std.testing.expectEqualStrings("done", obj.get("responseText").?.string);
}

test "body readers are single-use for inbound and constructed HTTP objects" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    try rt.loadHandler(
        \\function handler(req) {
        \\  req.text();
        \\  return Response.text(req.text());
        \\}
    , "<request-body-reuse>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = try allocator.dupe(u8, "ping"),
    };
    defer request.deinit(allocator);

    const request_val = try rt.createRequestObject(request.asView());
    current_runtime = rt;
    defer current_runtime = null;
    try std.testing.expectError(error.NativeFunctionError, rt.callGlobalFunction("handler", &[_]zq.JSValue{request_val}));
    rt.resetForNextRequest();

    try rt.loadHandler(
        \\function handler(req) {
        \\  const built = Response("pong");
        \\  built.text();
        \\  return Response.text(built.text());
        \\}
    , "<response-body-reuse>");

    const request_val_two = try rt.createRequestObject(request.asView());
    try std.testing.expectError(error.NativeFunctionError, rt.callGlobalFunction("handler", &[_]zq.JSValue{request_val_two}));
    rt.resetForNextRequest();
}

test "fetchSync returns response helpers and direct response objects" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const inspect_handler =
        \\function handler(req) {
        \\  const resp = fetchSync("http://example.com");
        \\  const data = resp.json();
        \\  return Response.json({
        \\    status: resp.status,
        \\    ok: resp.ok,
        \\    contentType: resp.headers.get("Content-Type"),
        \\    error: data.error,
        \\    details: data.details,
        \\  });
        \\}
    ;
    try rt.loadHandler(inspect_handler, "<fetchsync-inspect>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqual(@as(i64, 599), obj.get("status").?.integer);
    try std.testing.expect(obj.get("ok").?.bool == false);
    try std.testing.expectEqualStrings("application/json", obj.get("contentType").?.string);
    try std.testing.expectEqualStrings("OutboundHttpDisabled", obj.get("error").?.string);

    try rt.loadHandler("function handler(req) { return fetchSync('http://example.com'); }", "<fetchsync-direct>");
    var direct_response = try rt.executeHandler(request.asView());
    defer direct_response.deinit();

    try std.testing.expectEqual(@as(u16, 599), direct_response.status);
    try std.testing.expect(std.mem.indexOf(u8, direct_response.body, "\"error\":\"OutboundHttpDisabled\"") != null);
}

test "fetchSync returns structured errors for invalid init and allowlist failures" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .outbound_http_enabled = true,
        .outbound_allow_host = "localhost",
    });
    defer rt.deinit();

    const handler_code =
        \\function handler(req) {
        \\  const badHeadersResp = fetchSync("http://localhost", {
        \\    headers: { "X-Test": 42 }
        \\  });
        \\  const badHeaders = badHeadersResp.json();
        \\  return Response.json({
        \\    badHeadersStatus: badHeadersResp.status,
        \\    badHeadersOk: badHeadersResp.ok,
        \\    badHeadersStatusText: badHeadersResp.statusText,
        \\    badHeadersError: badHeaders.error,
        \\    badMethodError: fetchSync({ url: "http://localhost", method: "BOGUS" }).json().error,
        \\    badBodyError: fetchSync("http://localhost", { body: 42 }).json().error,
        \\    missingUrlError: fetchSync({ method: "GET" }).json().error,
        \\    hostBlockedError: fetchSync("http://example.com").json().error,
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<fetchsync-invalid>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqual(@as(i64, 599), obj.get("badHeadersStatus").?.integer);
    try std.testing.expectEqual(false, obj.get("badHeadersOk").?.bool);
    try std.testing.expectEqualStrings("InvalidHeaders", obj.get("badHeadersStatusText").?.string);
    try std.testing.expectEqualStrings("InvalidHeaders", obj.get("badHeadersError").?.string);
    try std.testing.expectEqualStrings("InvalidMethod", obj.get("badMethodError").?.string);
    try std.testing.expectEqualStrings("InvalidBody", obj.get("badBodyError").?.string);
    try std.testing.expectEqualStrings("InvalidUrl", obj.get("missingUrlError").?.string);
    try std.testing.expectEqualStrings("HostNotAllowed", obj.get("hostBlockedError").?.string);
}

test "fetchSync respects embedded capability policy host allowlist" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .outbound_http_enabled = true,
    });
    defer rt.deinit();
    rt.ctx.capability_policy = .{
        .egress = .{
            .enabled = true,
            .values = &[_][]const u8{"localhost"},
        },
    };

    const handler_code =
        \\function handler(req) {
        \\  return Response.json({
        \\    blocked: fetchSync("http://example.com").json().error,
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<fetchsync-policy>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("HostNotAllowed", obj.get("blocked").?.string);
}

test "fetchSync sends request data and exposes response helpers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server = try TestHttpServer.init(allocator, .echo_request_json);
    defer server.join() catch {};
    try server.start();

    const url = try server.url(allocator, "/inspect?mode=1");
    defer allocator.free(url);

    const rt = try Runtime.init(allocator, .{
        .outbound_http_enabled = true,
        .outbound_allow_host = "127.0.0.1",
    });
    defer rt.deinit();

    const handler_code = try std.fmt.allocPrint(
        allocator,
        \\function handler(req) {{
        \\  const init = {{
        \\    method: "POST",
        \\    headers: {{
        \\      "Content-Type": "text/plain",
        \\      "X-Test": "alpha"
        \\    }},
        \\    body: "ping"
        \\  }};
        \\  const resp = fetchSync("{s}", init);
        \\  const rawBody = resp.text();
        \\  const data = JSON.parse(rawBody);
        \\  return Response.json({{
        \\    status: resp.status,
        \\    ok: resp.ok,
        \\    contentType: resp.headers.get("content-type"),
        \\    reply: resp.headers.get("X-Reply"),
        \\    rawBody: rawBody,
        \\    method: data.method,
        \\    path: data.path,
        \\    requestType: data.contentType,
        \\    requestHeader: data.xTest,
        \\    requestBody: data.body,
        \\    requestLength: data.contentLength,
        \\    transferEncoding: data.transferEncoding,
        \\    requestRaw: data.raw
        \\  }});
        \\}}
    ,
        .{url},
    );
    defer allocator.free(handler_code);
    try rt.loadHandler(handler_code, "<fetchsync-success>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();
    try server.join();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqual(@as(i64, 201), obj.get("status").?.integer);
    try std.testing.expectEqual(true, obj.get("ok").?.bool);
    try std.testing.expectEqualStrings("application/json", obj.get("contentType").?.string);
    try std.testing.expectEqualStrings("ok", obj.get("reply").?.string);
    try std.testing.expect(std.mem.indexOf(u8, obj.get("rawBody").?.string, "\"method\":\"POST\"") != null);
    try std.testing.expectEqualStrings("POST", obj.get("method").?.string);
    try std.testing.expectEqualStrings("/inspect?mode=1", obj.get("path").?.string);
    try std.testing.expectEqualStrings("text/plain", obj.get("requestType").?.string);
    try std.testing.expectEqualStrings("alpha", obj.get("requestHeader").?.string);
    try std.testing.expectEqualStrings("ping", obj.get("requestBody").?.string);
    try std.testing.expectEqualStrings("4", obj.get("requestLength").?.string);
    try std.testing.expectEqualStrings("", obj.get("transferEncoding").?.string);
    try std.testing.expect(std.mem.indexOf(u8, obj.get("requestRaw").?.string, "POST /inspect?mode=1 HTTP/1.1\r\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, obj.get("requestRaw").?.string, "content-length: 4\r\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, obj.get("requestRaw").?.string, "Content-Type: text/plain\r\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, obj.get("requestRaw").?.string, "X-Test: alpha\r\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, obj.get("requestRaw").?.string, "\r\n\r\nping") != null);
}

test "buildServiceUrl renders service params and query" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const params = try rt.ctx.createObject(null);
    try rt.ctx.setPropertyChecked(params, try rt.ctx.atoms.intern("id"), try rt.ctx.createString("42"));

    const query = try rt.ctx.createObject(null);
    try rt.ctx.setPropertyChecked(query, try rt.ctx.atoms.intern("mode"), try rt.ctx.createString("a b"));

    const init = try rt.ctx.createObject(null);
    try rt.ctx.setPropertyChecked(init, try rt.ctx.atoms.intern("params"), params.toValue());
    try rt.ctx.setPropertyChecked(init, try rt.ctx.atoms.intern("query"), query.toValue());

    const url = try buildServiceUrl(rt, rt.ctx, "http://users.internal", "/inspect/:id", init);
    defer allocator.free(url);

    try std.testing.expectEqualStrings("http://users.internal/inspect/42?mode=a%20b", url);
}

test "fetchSync enforces response byte limits" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server = try TestHttpServer.init(allocator, .large_plain_text);
    defer server.join() catch {};
    try server.start();

    const url = try server.url(allocator, "/too-large");
    defer allocator.free(url);

    const rt = try Runtime.init(allocator, .{
        .outbound_http_enabled = true,
        .outbound_allow_host = "127.0.0.1",
        .outbound_max_response_bytes = 64,
    });
    defer rt.deinit();

    const handler_code = try std.fmt.allocPrint(
        allocator,
        \\function handler(req) {{
        \\  const resp = fetchSync("{s}", {{ max_response_bytes: 8 }});
        \\  const data = resp.json();
        \\  return Response.json({{
        \\    status: resp.status,
        \\    ok: resp.ok,
        \\    error: data.error,
        \\    details: data.details
        \\  }});
        \\}}
    ,
        .{url},
    );
    defer allocator.free(handler_code);
    try rt.loadHandler(handler_code, "<fetchsync-too-large>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();
    try server.join();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqual(@as(i64, 599), obj.get("status").?.integer);
    try std.testing.expectEqual(false, obj.get("ok").?.bool);
    try std.testing.expectEqualStrings("ResponseTooLarge", obj.get("error").?.string);
    try std.testing.expectEqualStrings("response exceeded max_response_bytes", obj.get("details").?.string);
}

test "HandlerPool basic operations" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const handler_code = "function handler(req) { return Response.text('ok'); }";
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 2, 0);
    defer {
        pool.deinit();
    }

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try pool.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqualStrings("ok", response.body);
}

test "HandlerPool bytecode cache" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const handler_code = "function handler(req) { return Response.text('cached'); }";
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 4, 0);
    defer {
        pool.deinit();
    }

    // First request triggers cache population during prewarm
    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try pool.executeHandler(request.asView());
    defer response.deinit();
    try std.testing.expectEqualStrings("cached", response.body);

    // Cache should have entries (hits from prewarm, misses from first parse)
    const hits = pool.cache.hits.load(.monotonic);
    const misses = pool.cache.misses.load(.monotonic);
    try std.testing.expect(hits + misses >= 1);
}

test "HandlerPool handler remains callable across resets" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const handler_code = "function handler(req) { return Response.text('ok'); }";
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 2, 0);
    defer pool.deinit();

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    const iterations: usize = 200;
    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        var response = try pool.executeHandler(request.asView());
        defer response.deinit();
        try std.testing.expectEqualStrings("ok", response.body);
    }
}

test "AOT override fallback and success" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const handler_code = "function handler(req) { return Response.json({ok:false}); }";
    var rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    try rt.loadHandler(handler_code, "<handler>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    const aot_ok: AotOverrideFn = struct {
        fn call(ctx: *zq.Context, args: []const zq.JSValue) anyerror!zq.JSValue {
            _ = args;
            return zq.http.createResponse(ctx, "{\"ok\":true}", 200, "application/json");
        }
    }.call;

    const aot_bail: AotOverrideFn = struct {
        fn call(_: *zq.Context, _: []const zq.JSValue) anyerror!zq.JSValue {
            return error.AotBail;
        }
    }.call;

    setAotOverrideForTest(aot_bail);
    defer setAotOverrideForTest(null);

    var fallback_response = try rt.executeHandler(request.asView());
    defer fallback_response.deinit();
    try std.testing.expectEqualStrings("{\"ok\":false}", fallback_response.body);

    setAotOverrideForTest(aot_ok);
    var aot_response = try rt.executeHandler(request.asView());
    defer aot_response.deinit();
    try std.testing.expectEqualStrings("{\"ok\":true}", aot_response.body);
}

test "HandlerPool concurrent stress" {
    // Expensive opt-in stress test. Stable on Zig 0.16.0 (5/5 runs).
    // Run explicitly with: ZTS_RUN_STRESS_TESTS=1 zig build test-zruntime
    if (std.c.getenv("ZTS_RUN_STRESS_TESTS") == null) return error.SkipZigTest;

    const allocator = std.heap.c_allocator;

    // Allow disabling JIT for this test via env var during debugging.
    if (std.c.getenv("ZTS_DISABLE_JIT_TESTS") != null) {
        zq.interpreter.disableJitForTests();
    }

    const handler_code = "function handler(req) { return Response.text('ok'); }";
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 8, 0);
    defer pool.deinit();

    const thread_count: usize = 8;
    const iterations: usize = 200;

    var errors = std.atomic.Value(u32).init(0);

    const ThreadCtx = struct {
        pool: *HandlerPool,
        allocator: std.mem.Allocator,
        errors: *std.atomic.Value(u32),
    };

    const Worker = struct {
        fn run(ctx: *ThreadCtx) void {
            const method = ctx.allocator.dupe(u8, "GET") catch {
                _ = ctx.errors.fetchAdd(1, .acq_rel);
                return;
            };
            const url = ctx.allocator.dupe(u8, "/") catch {
                ctx.allocator.free(method);
                _ = ctx.errors.fetchAdd(1, .acq_rel);
                return;
            };
            var request = HttpRequestOwned{
                .method = method,
                .url = url,
                .headers = .empty,
                .body = null,
            };
            defer request.deinit(ctx.allocator);

            var i: usize = 0;
            while (i < iterations) : (i += 1) {
                var response = ctx.pool.executeHandler(request.asView()) catch {
                    _ = ctx.errors.fetchAdd(1, .acq_rel);
                    continue;
                };
                response.deinit();
            }
        }
    };

    var threads: [thread_count]std.Thread = undefined;
    var contexts: [thread_count]ThreadCtx = undefined;
    for (0..thread_count) |i| {
        contexts[i] = .{
            .pool = &pool,
            .allocator = allocator,
            .errors = &errors,
        };
        threads[i] = try std.Thread.spawn(.{}, Worker.run, .{&contexts[i]});
    }
    for (threads) |t| t.join();

    try std.testing.expectEqual(@as(u32, 0), errors.load(.acquire));
}

test "string prototype methods are callable" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const proto = rt.ctx.string_prototype orelse return error.NoRootClass;
    const pool = rt.ctx.hidden_class_pool orelse return error.NoRootClass;
    const split_val = proto.getProperty(pool, zq.Atom.split) orelse return error.NoHandler;
    try std.testing.expect(split_val.isCallable());

    try rt.loadHandler("function handler(req){ return Response.text(typeof ''.split); }", "<test>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqualStrings("function", response.body);
}

test "request body split works" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    // First test: simple method call
    const simple_test = "function handler(req){ return Response.text('hello'); }";
    try rt.loadHandler(simple_test, "<test1>");

    var req1 = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };

    var resp1 = try rt.executeHandler(req1.asView());
    try std.testing.expectEqualStrings("hello", resp1.body);
    resp1.deinit();
    req1.deinit(allocator);

    // Reset and test property access
    const rt2 = try Runtime.init(allocator, .{});
    defer rt2.deinit();

    const prop_test = "function handler(req){ return Response.text(req.method); }";
    try rt2.loadHandler(prop_test, "<test2>");

    var req2 = HttpRequestOwned{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = try allocator.dupe(u8, "test"),
    };

    var resp2 = try rt2.executeHandler(req2.asView());
    try std.testing.expectEqualStrings("POST", resp2.body);
    resp2.deinit();
    req2.deinit(allocator);

    // Test typeof without var assignment
    const rt3 = try Runtime.init(allocator, .{});
    defer rt3.deinit();

    const typeof_split = "function handler(req){ return Response.text(typeof 'a&b'.split('&')); }";
    try rt3.loadHandler(typeof_split, "<test3>");

    var req3 = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };

    var resp3 = try rt3.executeHandler(req3.asView());
    try std.testing.expectEqualStrings("object", resp3.body);
    resp3.deinit();
    req3.deinit(allocator);

    // Test simple var assignment
    const rt4 = try Runtime.init(allocator, .{});
    defer rt4.deinit();

    const handler_code =
        "function handler(req){ let x = 'test'; return Response.text(x); }";
    try rt4.loadHandler(handler_code, "<test>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt4.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqualStrings("test", response.body);
}

test "for loop locals preserve numeric values" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        "function handler(req){ for (let i of range(1)) { return Response.text(typeof i); } return Response.text('none'); }";
    try rt.loadHandler(handler_code, "<test>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqualStrings("number", response.body);
}

test "object property access works" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\function handler(req){
        \\  const obj = { name: 'Alice', age: 30 };
        \\  const name = obj.name;
        \\  const age = obj.age;
        \\  return Response.text(name + '-' + age);
        \\}
    ;
    try rt.loadHandler(handler_code, "<test>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqualStrings("Alice-30", response.body);
}

test "array indexing works" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\function handler(req){
        \\  const arr = [1, 2, 3];
        \\  const a = arr[0];
        \\  const b = arr[1];
        \\  const c = arr[2];
        \\  return Response.text(a + '-' + b + '-' + c);
        \\}
    ;
    try rt.loadHandler(handler_code, "<test>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqualStrings("1-2-3", response.body);
}

test "JSX rendering works" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\function handler(req){
        \\  const elem = <div>Hello</div>;
        \\  return Response.html(renderToString(elem));
        \\}
    ;
    // Load as .jsx to enable JSX mode
    try rt.loadHandler(handler_code, "test.jsx");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqualStrings("<div>Hello</div>", response.body);
}

test "HandlerPool exhaustion and recovery" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.heap.c_allocator;

    const handler_code = "function handler(req) { return Response.text('ok'); }";
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 2, 0);
    defer pool.deinit();
    pool.acquire_timeout_ms = 0;

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var lease1 = try pool.acquireWorkerRuntime();
    var lease1_active = true;
    defer if (lease1_active) lease1.deinit();
    var lease2 = try pool.acquireWorkerRuntime();
    defer lease2.deinit();

    try std.testing.expectError(error.PoolExhausted, pool.acquireWorkerRuntime());
    const metrics = pool.getMetrics();
    try std.testing.expect(metrics.exhausted >= 1);

    lease1.deinit();
    lease1_active = false;

    var response = try pool.executeHandler(request.asView());
    defer response.deinit();
    try std.testing.expectEqualStrings("ok", response.body);
}

test "HandlerPool owned response survives pooled reuse" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.heap.c_allocator;

    const handler_code =
        \\function handler(req) {
        \\  return Response.text(req.body ?? "");
        \\}
    ;
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 1, 0);
    defer pool.deinit();

    var first_request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = try allocator.dupe(u8, "first"),
    };
    defer first_request.deinit(allocator);

    var second_request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = try allocator.dupe(u8, "second"),
    };
    defer second_request.deinit(allocator);

    var first_response = try pool.executeHandler(first_request.asView());
    defer first_response.deinit();

    var second_response = try pool.executeHandler(second_request.asView());
    defer second_response.deinit();

    try std.testing.expectEqualStrings("first", first_response.body);
    try std.testing.expectEqualStrings("second", second_response.body);
}

// Linux glibc currently aborts a small runtime test bucket with heap
// corruption ("double free or corruption (fasttop)" /
// "malloc_consolidate(): unaligned fastbin chunk"). The known repros cover
// five HandlerPool lifecycle tests plus the JIT overflow-slot literal test
// below. macOS does not detect the same corruption, and the prior Linux-only
// investigation did not isolate a root cause. Keep these gated on Linux until
// we can debug them under rr/valgrind on a Linux host.
const skip_linux_glibc_heap_corruption_tests = builtin.os.tag == .linux;

test "HandlerPool borrowed response pins runtime until release" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.heap.c_allocator;

    const handler_code =
        \\function handler(req) {
        \\  return Response.text(req.body ?? "");
        \\}
    ;
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 1, 0);
    defer pool.deinit();
    pool.acquire_timeout_ms = 0;

    var first_request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = try allocator.dupe(u8, "first"),
    };
    defer first_request.deinit(allocator);

    var second_request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = try allocator.dupe(u8, "second"),
    };
    defer second_request.deinit(allocator);

    var handle = try pool.executeHandlerBorrowed(first_request.asView());
    try std.testing.expectEqualStrings("first", handle.response.body);
    try std.testing.expectError(error.PoolExhausted, pool.executeHandler(second_request.asView()));

    handle.deinit();

    var response = try pool.executeHandler(second_request.asView());
    defer response.deinit();
    try std.testing.expectEqualStrings("second", response.body);
}

test "HandlerPool pooled teardown survives repeated pool lifecycles" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.heap.c_allocator;
    const handler_code =
        \\function handler(req) {
        \\  return Response.text(req.body ?? "ok");
        \\}
    ;

    const Cycle = struct {
        fn run(test_allocator: std.mem.Allocator, handler_source: []const u8, cycle: usize) !void {
            var pool = try HandlerPool.init(test_allocator, .{}, handler_source, "<handler>", 2, 0);
            defer pool.deinit();
            pool.acquire_timeout_ms = 0;

            var first_request = HttpRequestOwned{
                .method = try test_allocator.dupe(u8, "POST"),
                .url = try test_allocator.dupe(u8, "/"),
                .headers = .empty,
                .body = try std.fmt.allocPrint(test_allocator, "first-{d}", .{cycle}),
            };
            defer first_request.deinit(test_allocator);

            var second_request = HttpRequestOwned{
                .method = try test_allocator.dupe(u8, "POST"),
                .url = try test_allocator.dupe(u8, "/"),
                .headers = .empty,
                .body = try std.fmt.allocPrint(test_allocator, "second-{d}", .{cycle}),
            };
            defer second_request.deinit(test_allocator);

            var lease1 = try pool.acquireWorkerRuntime();
            var lease1_active = true;
            defer if (lease1_active) lease1.deinit();
            var lease2 = try pool.acquireWorkerRuntime();
            var lease2_active = true;
            defer if (lease2_active) lease2.deinit();

            const hits = pool.cache.hits.load(.acquire);
            const misses = pool.cache.misses.load(.acquire);
            try std.testing.expect(hits >= 1);
            try std.testing.expect(misses >= 1);

            try std.testing.expectError(error.PoolExhausted, pool.acquireWorkerRuntime());

            lease2.deinit();
            lease2_active = false;

            var first_response = try pool.executeHandler(first_request.asView());
            defer first_response.deinit();

            var second_response = try pool.executeHandler(second_request.asView());
            defer second_response.deinit();

            try std.testing.expectEqualStrings(first_request.body.?, first_response.body);
            try std.testing.expectEqualStrings(second_request.body.?, second_response.body);

            var handle = try pool.executeHandlerBorrowed(first_request.asView());
            try std.testing.expectEqualStrings(first_request.body.?, handle.response.body);
            try std.testing.expectError(error.PoolExhausted, pool.executeHandler(second_request.asView()));

            handle.deinit();
            lease1.deinit();
            lease1_active = false;

            var recovered = try pool.executeHandler(second_request.asView());
            defer recovered.deinit();
            try std.testing.expectEqualStrings(second_request.body.?, recovered.body);

            const metrics = pool.getMetrics();
            try std.testing.expect(metrics.exhausted >= 2);
        }
    };

    var cycle: usize = 0;
    while (cycle < 32) : (cycle += 1) {
        try Cycle.run(allocator, handler_code, cycle);
    }
}

test "JIT object literal overflow slots remain valid" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    if (std.c.getenv("ZTS_DISABLE_JIT_TESTS") != null or std.c.getenv("ZTS_DISABLE_JIT") != null) {
        return error.SkipZigTest;
    }

    const prev_policy = zq.interpreter.getJitPolicy();
    const prev_threshold = zq.interpreter.getJitThreshold();
    const prev_warmup = zq.interpreter.getJitFeedbackWarmup();
    defer {
        zq.interpreter.setJitPolicy(prev_policy);
        zq.interpreter.setJitThreshold(prev_threshold);
        zq.interpreter.setJitFeedbackWarmup(prev_warmup);
    }

    zq.interpreter.setJitPolicy(.eager);
    zq.interpreter.setJitThreshold(1);
    zq.interpreter.setJitFeedbackWarmup(1);

    const allocator = std.heap.c_allocator;
    const script =
        \\function handler(req) { return Response.text('ok'); }
        \\function run(seed) {
        \\  const obj = { p0: 1, p1: 2, p2: 3, p3: 4, p4: 5, p5: 6, p6: 7, p7: 8, p8: 9 };
        \\  return obj.p8;
        \\}
    ;

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    try rt.loadCode(script, "<jit-overflow-slots>");

    var i: usize = 0;
    while (i < 64) : (i += 1) {
        const args = [_]zq.JSValue{zq.JSValue.fromInt(@intCast(i))};
        const result = try rt.callGlobalFunction("run", &args);
        try std.testing.expect(result.isInt());
        try std.testing.expectEqual(@as(i32, 9), result.getInt());
    }
}

test "HandlerPool high contention stress" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.heap.c_allocator;

    // Allow disabling JIT for this test via env var during debugging.
    if (std.c.getenv("ZTS_DISABLE_JIT_TESTS") != null) {
        zq.interpreter.disableJitForTests();
    }

    const handler_code = "function handler(req) { return Response.text('ok'); }";
    // Use larger pool (8 slots) with reasonable timeout for contention
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 8, 0);
    defer pool.deinit();

    const thread_count: usize = 16;
    const iterations: usize = 25;

    var errors = std.atomic.Value(u32).init(0);
    var completed = std.atomic.Value(u32).init(0);

    const ThreadCtx = struct {
        pool: *HandlerPool,
        allocator: std.mem.Allocator,
        errors: *std.atomic.Value(u32),
        completed: *std.atomic.Value(u32),
    };

    const Worker = struct {
        fn run(ctx: *ThreadCtx) void {
            const method = ctx.allocator.dupe(u8, "GET") catch {
                _ = ctx.errors.fetchAdd(1, .acq_rel);
                return;
            };
            const url = ctx.allocator.dupe(u8, "/") catch {
                ctx.allocator.free(method);
                _ = ctx.errors.fetchAdd(1, .acq_rel);
                return;
            };
            var request = HttpRequestOwned{
                .method = method,
                .url = url,
                .headers = .empty,
                .body = null,
            };
            defer request.deinit(ctx.allocator);

            var i: usize = 0;
            while (i < iterations) : (i += 1) {
                var response = ctx.pool.executeHandler(request.asView()) catch {
                    _ = ctx.errors.fetchAdd(1, .acq_rel);
                    continue;
                };
                response.deinit();
                _ = ctx.completed.fetchAdd(1, .acq_rel);
            }
        }
    };

    var threads: [thread_count]std.Thread = undefined;
    var contexts: [thread_count]ThreadCtx = undefined;
    for (0..thread_count) |i| {
        contexts[i] = .{
            .pool = &pool,
            .allocator = allocator,
            .errors = &errors,
            .completed = &completed,
        };
        threads[i] = try std.Thread.spawn(.{}, Worker.run, .{&contexts[i]});
    }
    for (threads) |t| t.join();

    // Verify results - primary goal is no crashes under contention
    const error_count = errors.load(.acquire);
    const completed_count = completed.load(.acquire);
    const total = error_count + completed_count;

    // All requests should be accounted for (either completed or errored)
    try std.testing.expectEqual(thread_count * iterations, total);
    // At least some requests should succeed (proves pool works under contention)
    try std.testing.expect(completed_count > 0);
}

test "reloadHandler swaps handler code and new requests use new handler" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const old_code = "function handler(req) { return Response.text('v1'); }";
    var pool = try HandlerPool.init(allocator, .{}, old_code, "<handler>", 2, 0);
    defer pool.deinit();

    // Execute with old handler
    var req = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer req.deinit(allocator);

    {
        var response = try pool.executeHandler(req.asView());
        defer response.deinit();
        try std.testing.expectEqualStrings("v1", response.body);
    }

    // Reload with new handler
    const new_code = "function handler(req) { return Response.text('v2'); }";
    const invalidated = pool.reloadHandler(new_code, "<handler>");
    try std.testing.expect(invalidated > 0);

    // Execute with new handler - should return v2
    {
        var response = try pool.executeHandler(req.asView());
        defer response.deinit();
        try std.testing.expectEqualStrings("v2", response.body);
    }
}

test "reloadHandler clears bytecode cache" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const old_code = "function handler(req) { return Response.text('cached-v1'); }";
    var pool = try HandlerPool.init(allocator, .{}, old_code, "<handler>", 2, 0);
    defer pool.deinit();

    // Execute to populate cache
    var req = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer req.deinit(allocator);

    {
        var response = try pool.executeHandler(req.asView());
        defer response.deinit();
        try std.testing.expectEqualStrings("cached-v1", response.body);
    }

    // Cache should have entries
    try std.testing.expect(pool.cache.count() > 0);

    // Reload clears cache
    _ = pool.reloadHandler(
        "function handler(req) { return Response.text('cached-v2'); }",
        "<handler>",
    );
    try std.testing.expectEqual(@as(usize, 0), pool.cache.count());

    // New request uses new handler
    {
        var response = try pool.executeHandler(req.asView());
        defer response.deinit();
        try std.testing.expectEqualStrings("cached-v2", response.body);
    }
}
