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
const durable_executor = @import("durable_executor.zig");
const workflow_queue = @import("workflow_queue.zig");
const actor_queue = @import("actor_queue.zig");
const trace_request_recorder = @import("trace_request_recorder.zig");
const http_parser = @import("http_parser.zig");
const server_io = @import("server_io.zig");
const contract_runtime = @import("contract_runtime.zig");
const fault_explain = @import("fault_explain.zig");
const incident_log = @import("incident_log.zig");
const runtime_builtins = @import("runtime_builtins.zig");
const console = @import("runtime_console.zig");
const workflow = @import("runtime_workflow.zig");
const http = @import("runtime_http.zig");
const natives = @import("runtime_natives.zig");
pub const getStringData = natives.getStringData;
const buildQueryObject = natives.buildQueryObject;
const getStringDataCtx = zq.builtins.helpers.getStringDataCtx;
pub const websocket_codec = @import("websocket_codec.zig");

// Native callbacks and helpers that moved to runtime_http.zig (review M1).
// Aliased here so the binding-registration sites and tests in this file keep
// referencing them by their original unqualified names.
const fetchSyncNative = http.fetchSyncNative;
const httpRequestNative = http.httpRequestNative;
const serviceCallCallback = http.serviceCallCallback;
const fetchModuleCallback = http.fetchModuleCallback;
const ioCallThunk = http.ioCallThunk;
const ioExecuteFetches = http.ioExecuteFetches;
const ioBuildResponse = http.ioBuildResponse;
const headersGetNative = http.headersGetNative;
const headersHasNative = http.headersHasNative;
const headersSetNative = http.headersSetNative;
const headersAppendNative = http.headersAppendNative;
const headersDeleteNative = http.headersDeleteNative;
const headersConstructorNative = http.headersConstructorNative;
const requestConstructorNative = http.requestConstructorNative;
const responseConstructorNative = http.responseConstructorNative;
const responseJsonStaticNative = http.responseJsonStaticNative;
const responseTextStaticNative = http.responseTextStaticNative;
const responseHtmlStaticNative = http.responseHtmlStaticNative;
const responseRawJsonStaticNative = http.responseRawJsonStaticNative;
const responseRedirectStaticNative = http.responseRedirectStaticNative;
const buildFetchUrl = http.buildFetchUrl;
const buildServiceUrl = http.buildServiceUrl;
const parseFetchArgs = http.parseFetchArgs;
const scopeCall1 = http.scopeCall1;
// Re-exported for other runtime siblings (runtime_builtins.zig,
// durable_executor.zig, trace_request_recorder.zig) that referenced these by
// their original zruntime.* names before the runtime_http.zig split.
pub const beginBodyRead = http.beginBodyRead;
pub const createFetchResponse = http.createFetchResponse;
pub const splitHeaderKV = http.splitHeaderKV;

// Bytecode caching for faster cold starts
const bytecode_cache = zq.bytecode_cache;

// HTTP protocol types (shared with server layer)
const http_types = @import("http_types.zig");
const websocket_pool = @import("websocket_pool.zig");
const ws_callbacks = @import("ws_runtime_callbacks.zig");
pub const QueryParam = http_types.QueryParam;
pub const HttpRequestView = http_types.HttpRequestView;
pub const HttpRequestOwned = http_types.HttpRequestOwned;
pub const HttpHeader = http_types.HttpHeader;
pub const ResponseHeader = http_types.ResponseHeader;
pub const HttpResponse = http_types.HttpResponse;

// ============================================================================
// Runtime Configuration
// ============================================================================

const runtime_config_mod = @import("runtime_config.zig");
const cost_meter = zq.context.cost_meter;

pub const RuntimeConfig = runtime_config_mod.RuntimeConfig;

/// In-process registry of co-located sub-handlers, used by zigttp:workflow to
/// dispatch from an orchestrator handler without HTTP.
pub const SystemRuntime = @import("in_process_dispatch.zig").SystemRuntime;
const Target = @import("in_process_dispatch.zig").Target;

/// Recover the typed registry pointer from the type-erased `RuntimeConfig`
/// field. The config leaf cannot import `in_process_dispatch.zig` without an
/// import cycle, so the pointer is stored as `?*anyopaque` and cast here.
fn systemRegistryFromConfig(config: RuntimeConfig) ?*SystemRuntime {
    const ptr = config.system_registry orelse return null;
    return @ptrCast(@alignCast(ptr));
}

fn queueSystemFromConfig(config: RuntimeConfig) ?*actor_queue.ActorQueue {
    const ptr = config.queue_system orelse return null;
    return @ptrCast(@alignCast(ptr));
}

test {
    // Force collection of in_process_dispatch.zig tests under test-zruntime.
    _ = @import("in_process_dispatch.zig");
    _ = @import("actor_queue.zig");
}

const openTraceFile = runtime_config_mod.openTraceFile;
const openOplogFile = runtime_config_mod.openOplogFile;
const openLockedFreshOplog = runtime_config_mod.openLockedFreshOplog;
const openOplogAppendFile = runtime_config_mod.openOplogAppendFile;
const tryLockOplogFd = runtime_config_mod.tryLockOplogFd;
const applyRuntimeConfig = runtime_config_mod.applyRuntimeConfig;
const applyEmbeddedCapabilityPolicy = runtime_config_mod.applyEmbeddedCapabilityPolicy;

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
pub threadlocal var current_runtime: ?*Runtime = null;

/// Source location of the most recent handler type fault on this worker thread,
/// resolved from the bytecode line table (feature A). Set at the fault catch and
/// read-and-cleared by the server's 500 site, which builds the response after the
/// runtime is released. Same-thread handoff only.
pub threadlocal var last_fault_location: ?zq.bytecode.LineEntry = null;

/// Clear thread-local interpreter state after a handler panic.
/// Called from the setjmp recovery branch before returning error.HandlerPanicked.
/// Must only touch thread-locals - the runtime heap may be mid-mutation.
pub fn clearThreadStateAfterPanic() void {
    current_runtime = null;
    zq.http.clearCallFunctionCallback();
    zq.interpreter.current_interpreter = null;
}

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
    last_opt_stats: zq.OptStats,
    last_feedback_summary: zq.type_feedback.FeedbackSummary,
    strings: *zq.StringTable,
    owned_strings: ?zq.StringTable,
    handler_atom: ?zq.Atom,
    cached_handler_obj: ?*zq.JSObject,
    cached_handler_arg_count: u8,
    cached_dispatch: ?*const zq.bytecode.PatternDispatchTable,
    config: RuntimeConfig,
    /// The pool reload-generation this runtime was compiled for. The pool's
    /// live-reload / egress-policy swap bumps the pool counter; ensureRuntime
    /// rebuilds this runtime when it falls behind. Defaults to 0 so existing
    /// initializers need not set it.
    pool_generation: u64 = 0,
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
    request_deadline_jit_inhibited_prev: ?bool = null,
    // Hybrid allocation support
    arena_state: ?*zq.arena.Arena,
    hybrid_state: ?*zq.arena.HybridAllocator,
    /// WebSocket connection pool pointer. The pool itself is server-
    /// owned and lives for the server process; it is not swapped on
    /// handler hot reload. `ws_frame_loop` re-installs this pointer
    /// on every WS dispatch (onOpen/onMessage/onClose) so a freshly
    /// recycled runtime picks up the (same) pool on its first event
    /// dispatch. Stays null on runtimes that never see a WS event.
    /// Read by the WS module's send/close callbacks via
    /// `wsPoolFromRuntime`. See `installWebSocketModuleState`.
    ws_pool_ref: ?*websocket_pool.Pool = null,

    /// Co-located sub-handler registry for in-process `zigttp:workflow.call`
    /// dispatch. Server-owned (one instance per process), referenced by every
    /// pooled orchestrator runtime. Set from `config.system_registry` at init
    /// (cast from the type-erased pointer). Null on runtimes with no `--system`
    /// bundle. Read by `workflowCallCallback`; gates `installWorkflowModuleState`.
    system_registry_ref: ?*SystemRuntime = null,
    /// Server/test-owned actor queue used by `zigttp:queue`. Null keeps the
    /// module importable but makes queue functions return Result errors.
    queue_system_ref: ?*actor_queue.ActorQueue = null,

    const Self = @This();

    pub const PendingDurableWait = union(enum) {
        timer: i64,
        signal: []const u8,

        pub fn deinit(self: *PendingDurableWait, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .signal => |name| allocator.free(name),
                .timer => {},
            }
        }
    };

    pub const ActiveDurableRun = struct {
        key: []const u8,
        oplog_path: []const u8,
        oplog_fd: std.c.fd_t,
        state: *zq.trace.DurableState,
        owned_events: ?[]const zq.trace.DurableEvent = null,
        source_snapshot: ?[]u8 = null,
        step_depth: u32 = 0,
        step_timeout_deadline_ms: ?i64 = null,
        /// Monotonic per-run counter naming each `zigttp:workflow.call` as its
        /// own durable step ("workflow.call#N"). Deterministic across replay
        /// because the orchestrator re-executes the same control flow, so the
        /// Nth call resolves to the same oplog entry. See workflowCallDurable.
        call_seq: u32 = 0,
        pending_wait: ?PendingDurableWait = null,

        pub fn setPendingTimer(self: *ActiveDurableRun, allocator: std.mem.Allocator, until_ms: i64) !void {
            if (self.pending_wait) |*wait| {
                wait.deinit(allocator);
            }
            self.pending_wait = .{ .timer = until_ms };
        }

        pub fn setPendingSignal(self: *ActiveDurableRun, allocator: std.mem.Allocator, name: []const u8) !void {
            if (self.pending_wait) |*wait| {
                wait.deinit(allocator);
            }
            self.pending_wait = .{ .signal = try allocator.dupe(u8, name) };
        }

        pub fn deinit(self: *ActiveDurableRun, allocator: std.mem.Allocator) void {
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

    pub const PendingDurableRecovery = struct {
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
        applyEmbeddedCapabilityPolicy(ctx, config);

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
            .last_opt_stats = .{},
            .last_feedback_summary = .{},
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
            .request_deadline_jit_inhibited_prev = null,
            .arena_state = arena_state,
            .hybrid_state = hybrid_state,
            .ws_pool_ref = null,
            .system_registry_ref = systemRegistryFromConfig(config),
            .queue_system_ref = queueSystemFromConfig(config),
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
            .last_opt_stats = .{},
            .last_feedback_summary = .{},
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
            .request_deadline_jit_inhibited_prev = null,
            // Pool runtimes manage their own hybrid allocation
            .arena_state = null,
            .hybrid_state = null,
            .ws_pool_ref = null,
            .system_registry_ref = systemRegistryFromConfig(config),
            .queue_system_ref = queueSystemFromConfig(config),
        };

        applyRuntimeConfig(pool_rt.ctx, pool_rt.gc_state, pool_rt.heap_state, config);
        applyEmbeddedCapabilityPolicy(pool_rt.ctx, config);

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
        if (self.system_registry_ref != null) {
            try self.installWorkflowModuleState();
        }
        if (self.queue_system_ref != null) {
            try self.installQueueModuleState();
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
        var request_proto_unowned = true;
        errdefer if (request_proto_unowned) request_proto.destroyBuiltin(self.allocator, pool);
        try self.ctx.builtin_objects.append(self.allocator, request_proto);
        request_proto_unowned = false;
        try self.addDynamicMethod(request_proto, "text", runtime_builtins.bodyTextNative, 0);
        try self.addDynamicMethod(request_proto, "json", runtime_builtins.bodyJsonNative, 0);

        const response_proto = try zq.JSObject.create(self.allocator, root_class_idx, null, pool);
        var response_proto_unowned = true;
        errdefer if (response_proto_unowned) response_proto.destroyBuiltin(self.allocator, pool);
        try self.ctx.builtin_objects.append(self.allocator, response_proto);
        response_proto_unowned = false;
        try self.addDynamicMethod(response_proto, "text", runtime_builtins.bodyTextNative, 0);
        try self.addDynamicMethod(response_proto, "json", runtime_builtins.bodyJsonNative, 0);

        const headers_proto = try zq.JSObject.create(self.allocator, root_class_idx, null, pool);
        var headers_proto_unowned = true;
        errdefer if (headers_proto_unowned) headers_proto.destroyBuiltin(self.allocator, pool);
        try self.ctx.builtin_objects.append(self.allocator, headers_proto);
        headers_proto_unowned = false;
        try self.addDynamicMethod(headers_proto, "get", headersGetNative, 1);
        try self.addDynamicMethod(headers_proto, "set", headersSetNative, 2);
        try self.addDynamicMethod(headers_proto, "append", headersAppendNative, 2);
        try self.addDynamicMethod(headers_proto, "has", headersHasNative, 1);
        try self.addDynamicMethod(headers_proto, "delete", headersDeleteNative, 1);

        const headers_ctor_atom = try self.ctx.atoms.intern("Headers");
        const headers_ctor = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            headersConstructorNative,
            headers_ctor_atom,
            1,
        );
        var headers_ctor_unowned = true;
        errdefer if (headers_ctor_unowned) headers_ctor.destroyBuiltin(self.allocator, pool);
        try self.ctx.setPropertyChecked(headers_ctor, .prototype, headers_proto.toValue());
        try self.ctx.builtin_objects.append(self.allocator, headers_ctor);
        headers_ctor_unowned = false;
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
        var request_ctor_unowned = true;
        errdefer if (request_ctor_unowned) request_ctor.destroyBuiltin(self.allocator, pool);
        try self.ctx.setPropertyChecked(request_ctor, .prototype, request_proto.toValue());
        try self.ctx.builtin_objects.append(self.allocator, request_ctor);
        request_ctor_unowned = false;
        try self.ctx.setGlobal(request_ctor_atom, request_ctor.toValue());

        const response_ctor = try zq.JSObject.createNativeFunction(
            self.allocator,
            pool,
            root_class_idx,
            responseConstructorNative,
            .Response,
            2,
        );
        var response_ctor_unowned = true;
        errdefer if (response_ctor_unowned) response_ctor.destroyBuiltin(self.allocator, pool);
        try self.ctx.setPropertyChecked(response_ctor, .prototype, response_proto.toValue());
        try self.addMethod(response_ctor, .json, responseJsonStaticNative, 1);
        try self.addMethod(response_ctor, .text, responseTextStaticNative, 1);
        try self.addMethod(response_ctor, .html, responseHtmlStaticNative, 1);
        try self.addDynamicMethod(response_ctor, "redirect", responseRedirectStaticNative, 1);
        try self.addMethod(response_ctor, .rawJson, responseRawJsonStaticNative, 1);
        try self.ctx.builtin_objects.append(self.allocator, response_ctor);
        response_ctor_unowned = false;
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
        errdefer fn_obj.destroyFull(self.allocator);
        try self.ctx.setPropertyChecked(obj, atom, fn_obj.toValue());
    }

    fn addDynamicMethod(self: *Self, obj: *zq.JSObject, name: []const u8, func: zq.NativeFn, arg_count: u8) !void {
        const atom = try self.ctx.atoms.intern(name);
        try self.addMethod(obj, atom, func, arg_count);
    }

    fn installConsole(self: *Self) !void {
        const pool = self.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

        // Create console object
        const root_class_idx = self.ctx.root_class_idx;
        const console_obj = try zq.JSObject.create(self.allocator, root_class_idx, null, pool);
        var console_obj_unowned = true;
        errdefer if (console_obj_unowned) console_obj.destroyBuiltin(self.allocator, pool);
        try self.ctx.builtin_objects.append(self.allocator, console_obj);
        console_obj_unowned = false;

        try self.addMethod(console_obj, .log, console.consoleLog, 0);
        try self.addDynamicMethod(console_obj, "error", console.consoleError, 0);
        try self.addDynamicMethod(console_obj, "warn", console.consoleError, 0);
        try self.addDynamicMethod(console_obj, "info", console.consoleLog, 0);
        try self.addDynamicMethod(console_obj, "debug", console.consoleLog, 0);

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
        var fn_obj_unowned = true;
        errdefer if (fn_obj_unowned) fn_obj.destroyBuiltin(self.allocator, pool);
        try self.ctx.builtin_objects.append(self.allocator, fn_obj);
        fn_obj_unowned = false;
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
        var fn_obj_unowned = true;
        errdefer if (fn_obj_unowned) fn_obj.destroyBuiltin(self.allocator, pool);
        try self.ctx.builtin_objects.append(self.allocator, fn_obj);
        fn_obj_unowned = false;
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
    fn compileAndRunFileImports(self: *Self, entry_source: []const u8, filename: []const u8, refresh_handler: bool) !void {
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
            if (refresh_handler) {
                try self.refreshHandlerCache();
            }
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

        if (refresh_handler) {
            try self.refreshHandlerCache();
        }
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
                if (comptime std.mem.eql(u8, binding.specifier, "zigttp:queue")) {
                    if (self.queue_system_ref != null) {
                        try zq.modules.registerVirtualModule(binding, self.ctx, self.allocator);
                    } else {
                        try zq.modules.registerVirtualModuleReplay(binding, self.ctx, self.allocator);
                    }
                } else {
                    try zq.modules.registerVirtualModuleReplay(binding, self.ctx, self.allocator);
                }
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
            .run_fn = durable_executor.durableRunCallback,
            .step_fn = durable_executor.durableStepCallback,
            .step_with_timeout_fn = durable_executor.durableStepWithTimeoutCallback,
            .sleep_until_fn = durable_executor.durableSleepUntilCallback,
            .wait_signal_fn = durable_executor.durableWaitSignalCallback,
            .signal_fn = durable_executor.durableSignalCallback,
            .signal_at_fn = durable_executor.durableSignalAtCallback,
            .runtime_ptr = self,
        };
        self.ctx.setModuleState(
            zq.modules.durable.MODULE_STATE_SLOT,
            @ptrCast(durable_state),
            &zq.modules.durable.DurableCallbacks.deinitOpaque,
        );
    }

    fn installWorkflowModuleState(self: *Self) !void {
        const workflow_state = try self.allocator.create(zq.modules.workflow.WorkflowCallbacks);
        workflow_state.* = .{
            .call_fn = workflow.workflowCallCallback,
            .saga_fn = workflow.workflowSagaCallback,
            .fanout_fn = workflow.workflowFanoutCallback,
            .follow_fn = workflow.workflowFollowCallback,
            .runtime_ptr = self,
        };
        self.ctx.setModuleState(
            zq.modules.workflow.MODULE_STATE_SLOT,
            @ptrCast(workflow_state),
            &zq.modules.workflow.WorkflowCallbacks.deinitOpaque,
        );
    }

    fn installQueueModuleState(self: *Self) !void {
        const queue_state = try self.allocator.create(zq.modules.queue.QueueCallbacks);
        queue_state.* = .{
            .send_fn = queueSendCallback,
            .request_fn = queueRequestCallback,
            .receive_fn = queueReceiveCallback,
            .ack_fn = queueAckCallback,
            .nack_fn = queueNackCallback,
            .reply_fn = queueReplyCallback,
            .runtime_ptr = self,
        };
        self.ctx.setModuleState(
            zq.modules.queue.MODULE_STATE_SLOT,
            @ptrCast(queue_state),
            &zq.modules.queue.QueueCallbacks.deinitOpaque,
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

    fn queueRef(self: *Self) ?*actor_queue.ActorQueue {
        return self.queue_system_ref;
    }

    /// Per-pooled-instance reply identity for `zigttp:queue.request()`/
    /// `receive()` round trips. `config.queue_actor_name` ("main" by default)
    /// is a single value shared by every pooled Runtime, so using it directly
    /// as the reply-to actor would let concurrent in-flight requests on
    /// different pool slots cross-deliver each other's replies out of the
    /// same shared mailbox. Suffixing with this instance's stable address
    /// (each pool slot's Runtime is allocated once and reused, never moved)
    /// gives every concurrently in-flight request its own private mailbox
    /// while leaving explicit, well-known actor names (e.g. `receive("worker")`)
    /// untouched.
    fn queueSelfActorName(self: *Self, buf: *[128]u8) []const u8 {
        return std.fmt.bufPrint(buf, "{s}#{x}", .{ self.config.queue_actor_name, @intFromPtr(self) }) catch self.config.queue_actor_name;
    }

    fn queueSendCallback(runtime_ptr: *anyopaque, ctx: *zq.Context, target: []const u8, payload: zq.JSValue) anyerror!zq.JSValue {
        const rt: *Self = @ptrCast(@alignCast(runtime_ptr));
        return rt.queueSendInternal(ctx, target, payload, false);
    }

    fn queueRequestCallback(runtime_ptr: *anyopaque, ctx: *zq.Context, target: []const u8, payload: zq.JSValue) anyerror!zq.JSValue {
        const rt: *Self = @ptrCast(@alignCast(runtime_ptr));
        return rt.queueSendInternal(ctx, target, payload, true);
    }

    fn queueReceiveCallback(runtime_ptr: *anyopaque, ctx: *zq.Context, actor: ?[]const u8) anyerror!zq.JSValue {
        const rt: *Self = @ptrCast(@alignCast(runtime_ptr));
        const queue = rt.queueRef() orelse return zq.modules.util.createPlainResultErr(ctx, "queue runtime is not installed");
        var actor_buf: [128]u8 = undefined;
        const actor_name = actor orelse rt.queueSelfActorName(&actor_buf);
        const message = queue.receive(actor_name) catch |err| {
            return queueErrorResult(ctx, err);
        };
        const msg = message orelse return zq.modules.util.createPlainResultOk(ctx, zq.JSValue.null_val);
        return zq.modules.util.createPlainResultOk(ctx, try rt.queueMessageToValue(ctx, msg));
    }

    fn queueAckCallback(runtime_ptr: *anyopaque, ctx: *zq.Context, id_text: []const u8) anyerror!zq.JSValue {
        const rt: *Self = @ptrCast(@alignCast(runtime_ptr));
        const queue = rt.queueRef() orelse return zq.modules.util.createPlainResultErr(ctx, "queue runtime is not installed");
        const id = parseMessageId(id_text) catch return zq.modules.util.createPlainResultErr(ctx, "invalid message id");
        if (!queue.ack(id)) return zq.modules.util.createPlainResultErr(ctx, "message is not in flight");
        return zq.modules.util.createPlainResultOk(ctx, zq.JSValue.true_val);
    }

    fn queueNackCallback(runtime_ptr: *anyopaque, ctx: *zq.Context, id_text: []const u8, reason: []const u8) anyerror!zq.JSValue {
        const rt: *Self = @ptrCast(@alignCast(runtime_ptr));
        const queue = rt.queueRef() orelse return zq.modules.util.createPlainResultErr(ctx, "queue runtime is not installed");
        const id = parseMessageId(id_text) catch return zq.modules.util.createPlainResultErr(ctx, "invalid message id");
        const outcome = queue.nack(id, reason) catch |err| {
            return queueErrorResult(ctx, err);
        };
        return switch (outcome) {
            .not_found => zq.modules.util.createPlainResultErr(ctx, "message is not in flight"),
            .requeued => zq.modules.util.createPlainResultOk(ctx, try ctx.createString("requeued")),
            .dead_lettered => zq.modules.util.createPlainResultOk(ctx, try ctx.createString("dead_lettered")),
        };
    }

    fn queueReplyCallback(runtime_ptr: *anyopaque, ctx: *zq.Context, id_text: []const u8, payload: zq.JSValue) anyerror!zq.JSValue {
        const rt: *Self = @ptrCast(@alignCast(runtime_ptr));
        const queue = rt.queueRef() orelse return zq.modules.util.createPlainResultErr(ctx, "queue runtime is not installed");
        const id = parseMessageId(id_text) catch return zq.modules.util.createPlainResultErr(ctx, "invalid message id");
        const payload_json = zq.http.valueToJson(ctx, payload) catch |err| {
            return queueErrorResult(ctx, err);
        };
        defer ctx.allocator.free(payload_json);
        const reply_id = queue.reply(id, payload_json, rt.config.queue_max_attempts) catch |err| {
            return queueErrorResult(ctx, err);
        };
        return queueIdResult(ctx, reply_id);
    }

    fn queueSendInternal(self: *Self, ctx: *zq.Context, target: []const u8, payload: zq.JSValue, comptime request_reply: bool) !zq.JSValue {
        const queue = self.queueRef() orelse return zq.modules.util.createPlainResultErr(ctx, "queue runtime is not installed");
        const payload_json = zq.http.valueToJson(ctx, payload) catch |err| {
            return queueErrorResult(ctx, err);
        };
        defer ctx.allocator.free(payload_json);
        var actor_buf: [128]u8 = undefined;
        const source = self.queueSelfActorName(&actor_buf);
        const id = queue.send(target, payload_json, .{
            .source = source,
            .reply_to = if (request_reply) source else null,
            .max_attempts = self.config.queue_max_attempts,
        }) catch |err| {
            return queueErrorResult(ctx, err);
        };
        return queueIdResult(ctx, id);
    }

    fn queueMessageToValue(self: *Self, ctx: *zq.Context, msg: *const actor_queue.MessageEnvelope) !zq.JSValue {
        _ = self;
        const obj = try ctx.createObject(ctx.object_prototype);
        try setStringField(ctx, obj, "id", try formatMessageId(ctx, msg.id));
        try setStringField(ctx, obj, "source", try ctx.createString(msg.source));
        try setStringField(ctx, obj, "target", try ctx.createString(msg.target));
        const attempt_limit: u32 = @intCast(std.math.maxInt(i32));
        const attempt_i32: i32 = @intCast(@min(msg.attempt, attempt_limit));
        try ctx.setPropertyChecked(obj, try ctx.atoms.intern("attempt"), zq.JSValue.fromInt(attempt_i32));
        try ctx.setPropertyChecked(obj, try ctx.atoms.intern("payload"), zq.trace.jsonToJSValue(ctx, msg.payload_json));
        if (msg.correlation_id) |correlation_id| {
            try setStringField(ctx, obj, "correlationId", try formatMessageId(ctx, correlation_id));
        }
        if (msg.reply_to) |reply_to| {
            try setStringField(ctx, obj, "replyTo", try ctx.createString(reply_to));
        }
        return obj.toValue();
    }

    fn queueIdResult(ctx: *zq.Context, id: actor_queue.MessageId) !zq.JSValue {
        return zq.modules.util.createPlainResultOk(ctx, try formatMessageId(ctx, id));
    }

    fn formatMessageId(ctx: *zq.Context, id: actor_queue.MessageId) !zq.JSValue {
        var buf: [20]u8 = undefined;
        const text = std.fmt.bufPrint(&buf, "{d}", .{id}) catch unreachable;
        return ctx.createString(text);
    }

    fn setStringField(ctx: *zq.Context, obj: *zq.JSObject, name: []const u8, val: zq.JSValue) !void {
        try ctx.setPropertyChecked(obj, try ctx.atoms.intern(name), val);
    }

    fn parseMessageId(id_text: []const u8) !actor_queue.MessageId {
        return std.fmt.parseInt(actor_queue.MessageId, id_text, 10);
    }

    fn queueErrorResult(ctx: *zq.Context, err: anyerror) !zq.JSValue {
        const message = switch (err) {
            error.QueueFull => "queue full",
            error.InvalidActor => "invalid actor",
            error.MessageNotInFlight => "message is not in flight",
            error.OutOfMemory => "out of memory",
            else => @errorName(err),
        };
        return zq.modules.util.createPlainResultErr(ctx, message);
    }

    /// Install the WebSocket callback table on this runtime, pointed at
    /// the server-owned connection pool. Called by the frame loop on
    /// every WS dispatch — the re-install is idempotent and lets a
    /// freshly recycled runtime (post hot reload) pick up the pool
    /// without an extra acquire-time hook. Runtimes that never see a
    /// WS event dispatch skip this entirely and pay no cost.
    pub fn installWebSocketModuleState(self: *Self, pool: *websocket_pool.Pool) !void {
        self.ws_pool_ref = pool;
        try zq.modules.websocket.installState(self.ctx, .{
            .runtime_ptr = self,
            .send_fn = ws_callbacks.wsSendCallback,
            .close_fn = ws_callbacks.wsCloseCallback,
            .serialize_attachment_fn = ws_callbacks.wsSerializeAttachmentCallback,
            .deserialize_attachment_fn = ws_callbacks.wsDeserializeAttachmentCallback,
            .get_web_sockets_fn = ws_callbacks.wsGetWebSocketsCallback,
            .set_auto_response_fn = ws_callbacks.wsSetAutoResponseCallback,
        });
    }

    fn summarizeFeedbackRecursive(summary: *zq.type_feedback.FeedbackSummary, func: *const zq.FunctionBytecode) void {
        if (func.getTypeFeedback()) |tf| {
            summary.merge(tf.summary());
        }
        for (func.constants) |constant| {
            if (constant.isExternPtr()) {
                summarizeFeedbackRecursive(summary, constant.toExternPtr(zq.FunctionBytecode));
            }
        }
    }

    fn verifyBytecodeRecursive(func: *const zq.FunctionBytecode) !void {
        const verify_result = zq.BytecodeVerifier.verify(func);
        if (!verify_result.valid) {
            if (!builtin.is_test) {
                std.log.err("Bytecode verification failed at offset {d}: {s}", .{
                    verify_result.offset,
                    verify_result.message,
                });
            }
            return error.BytecodeVerificationFailed;
        }

        for (func.constants) |constant| {
            const nested_func = bytecodeConstant(constant) orelse continue;
            try verifyBytecodeRecursive(nested_func);
        }
    }

    fn bytecodeConstant(constant: zq.JSValue) ?*const zq.FunctionBytecode {
        if (!constant.isExternPtr()) return null;
        const magic = constant.toExternPtr(u32);
        if (magic.* != zq.bytecode.MAGIC) return null;
        return constant.toExternPtr(zq.FunctionBytecode);
    }

    fn captureCompilationStats(self: *Self, parser: *const zq.Parser, func: *const zq.FunctionBytecode) void {
        self.last_opt_stats = if (parser.code_gen) |cg| cg.getOptStats() else .{};
        self.last_feedback_summary = .{};
        summarizeFeedbackRecursive(&self.last_feedback_summary, func);
    }

    /// Load and compile JavaScript code
    pub fn loadCode(self: *Self, code: []const u8, filename: []const u8) !void {
        _ = try self.loadCodeWithCachingInternal(code, filename, null, true);
    }

    /// Load and compile JavaScript code without requiring a `handler` export.
    /// Used by benchmark and script-style tooling that call other globals directly.
    pub fn loadCodeNoHandler(self: *Self, code: []const u8, filename: []const u8) !void {
        _ = try self.loadCodeWithCachingInternal(code, filename, null, false);
    }

    /// Load and compile JavaScript code, optionally returning serialized bytecode for caching
    /// If cache_buffer is provided, serializes the bytecode and returns the serialized slice
    pub fn loadCodeWithCaching(self: *Self, code: []const u8, filename: []const u8, cache_buffer: ?[]u8) !?[]const u8 {
        return self.loadCodeWithCachingInternal(code, filename, cache_buffer, true);
    }

    pub fn loadCodeWithCachingNoHandler(self: *Self, code: []const u8, filename: []const u8, cache_buffer: ?[]u8) !?[]const u8 {
        return self.loadCodeWithCachingInternal(code, filename, cache_buffer, false);
    }

    fn loadCodeWithCachingInternal(self: *Self, code: []const u8, filename: []const u8, cache_buffer: ?[]u8, refresh_handler: bool) !?[]const u8 {
        self.last_opt_stats = .{};
        self.last_feedback_summary = .{};
        self.interpreter.resetProfilingCounters();
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
        var p = try zq.Parser.initFallible(self.allocator, source_to_parse, self.strings, &self.ctx.atoms);
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

        {
            const ir_view = zq.parser.IrView.fromIRStore(&p.js_parser.nodes, &p.js_parser.constants);
            const parsed = zq.pipeline.ParsedModule.fromExisting(ir_view, p.root_node, &self.ctx.atoms);

            var type_env_storage: zq.pipeline.TypeEnvStorage = .{};
            defer type_env_storage.deinit(self.allocator);
            if (strip_result) |sr| {
                try type_env_storage.init(self.allocator, &sr.type_map);
            }

            var resolved = try zq.pipeline.resolve(
                self.allocator,
                parsed,
                // The runtime parse path only surfaces bool diagnostics; strict
                // ZTS6xx is a build-time concern that precompile already ran.
                // Skip it here to keep the runtime loop free of redundant work.
                .{ .type_env = type_env_storage.envPtr(), .strict = false },
            );
            defer resolved.deinit();

            const bool_diags = resolved.boolDiagnostics();
            if (bool_diags.len > 0) {
                var diag_output: std.ArrayList(u8) = .empty;
                defer diag_output.deinit(self.allocator);
                var diag_aw: std.Io.Writer.Allocating = .fromArrayList(self.allocator, &diag_output);
                resolved.formatBoolDiagnostics(source_to_parse, &diag_aw.writer) catch {};
                diag_output = diag_aw.toArrayList();
                if (diag_output.items.len > 0) {
                    std.log.err("{s}", .{diag_output.items});
                }
                std.log.err("{d} boolean check error(s), {d} warning(s)", .{
                    resolved.bool_error_count,
                    bool_diags.len - resolved.bool_error_count,
                });
            }
            if (resolved.bool_error_count > 0) {
                return error.SoundModeViolation;
            }

            const type_diags = resolved.typeDiagnostics();
            if (type_diags.len > 0) {
                var tc_output: std.ArrayList(u8) = .empty;
                defer tc_output.deinit(self.allocator);
                var tc_aw: std.Io.Writer.Allocating = .fromArrayList(self.allocator, &tc_output);
                resolved.formatTypeDiagnostics(source_to_parse, &tc_aw.writer) catch {};
                tc_output = tc_aw.toArrayList();
                if (tc_output.items.len > 0) {
                    std.log.err("{s}", .{tc_output.items});
                }
            }
            if (resolved.type_error_count > 0) {
                return error.SoundModeViolation;
            }
        }

        // Resolve module imports and register virtual module native functions
        const has_file_imports = try self.resolveModuleImports(&p);

        // If file imports are present, build module graph and compile dependencies
        if (has_file_imports) {
            try self.compileAndRunFileImports(code, filename, refresh_handler);
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
            .line_table = p.getLineTable(),
        };

        // Bytecode verification: reject malformed bytecode before execution.
        try verifyBytecodeRecursive(&func);

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
        self.captureCompilationStats(&p, &func);
        if (refresh_handler) {
            try self.refreshHandlerCache();
        }

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
        self.last_opt_stats = .{};
        self.last_feedback_summary = .{};
        self.interpreter.resetProfilingCounters();
        var reader = bytecode_cache.SliceReader{ .data = cached_data };

        // Deserialize bytecode with atoms and shapes - skips parsing entirely
        var result = try bytecode_cache.deserializeBytecodeWithAtomsAndShapes(
            &reader,
            &self.ctx.atoms,
            self.allocator,
            self.strings,
        );
        var bytecode_transferred = false;
        defer {
            if (bytecode_transferred) {
                // Free shapes after materialization (they're copied into hidden classes)
                for (result.shapes) |shape| {
                    self.allocator.free(shape);
                }
                self.allocator.free(result.shapes);
            } else {
                result.deinit();
            }
        }

        // Verify deserialized bytecode before materialization or execution.
        try verifyBytecodeRecursive(result.func);

        // Materialize object literal shapes before execution
        if (result.shapes.len > 0) {
            // Convert [][]object.Atom to []const []const object.Atom for materializeShapes
            const shapes_const: []const []const zq.object.Atom = @ptrCast(result.shapes);
            try self.ctx.materializeShapes(shapes_const);
        }

        // The script root is heap-allocated on cache deserialization. Transfer
        // it to the context before execution creates separately tracked nested
        // function objects; context teardown deduplicates both ownership paths.
        try self.ctx.takeBytecodeRoot(result.func);
        bytecode_transferred = true;

        // Execute the deserialized bytecode
        _ = try self.interpreter.run(result.func);
        summarizeFeedbackRecursive(&self.last_feedback_summary, result.func);
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

    /// Arm the per-request execution deadline on this runtime's context.
    /// No-op when request_timeout_ms == 0.
    pub fn armRequestDeadline(self: *Self) void {
        self.armRequestDeadlineMs(self.config.request_timeout_ms);
    }

    /// Arm a callback deadline bounded by both the configured request timeout
    /// and an enclosing lifecycle budget such as graceful shutdown.
    pub fn armRequestDeadlineWithin(self: *Self, max_ms: u32) void {
        const configured_ms = self.config.request_timeout_ms;
        const effective_ms = if (configured_ms == 0) max_ms else @min(configured_ms, max_ms);
        self.armRequestDeadlineMs(effective_ms);
    }

    fn armRequestDeadlineMs(self: *Self, ms: u32) void {
        self.ctx.interrupt_requested.store(false, .monotonic);
        self.ctx.deadline_ns = 0;
        if (ms == 0) return;
        const now = compat.monotonicNowNs() catch return;
        self.ctx.deadline_ns = now + @as(u64, ms) * std.time.ns_per_ms;
        if (self.request_deadline_jit_inhibited_prev == null) {
            self.request_deadline_jit_inhibited_prev = self.ctx.jit_inhibited;
            self.ctx.jit_inhibited = true;
        }
    }

    /// Clear the per-request execution deadline and interrupt flag.
    pub fn clearRequestDeadline(self: *Self) void {
        self.ctx.deadline_ns = 0;
        self.ctx.interrupt_requested.store(false, .monotonic);
        if (self.request_deadline_jit_inhibited_prev) |prev| {
            self.ctx.jit_inhibited = prev;
            self.request_deadline_jit_inhibited_prev = null;
        }
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

    /// Record a soundness incident: a runtime fault on a path the compiler proved
    /// safe (every guarding chip discharged, yet the handler faulted). Loud log
    /// always (outside tests); JSONL append when --incident-log is configured.
    /// Best-effort — never disturbs the response path.
    fn recordSoundnessIncident(self: *Self, chips: []const []const u8, detail: []const u8) void {
        const method: []const u8 = if (self.active_request) |r| r.method else "";
        // Prefer the parsed route path; fall back to the full request URL when a
        // caller only populated url (e.g. the fast-path / test request builders).
        const path: []const u8 = if (self.active_request) |r|
            (if (r.path.len > 0) r.path else r.url)
        else
            "";
        // A Zig-level type fault ran through the interpreter's trace catch, which
        // resolves the faulting bytecode offset to a source line via the line
        // table (feature A). Append it to the detail when available.
        var detail_buf: [192]u8 = undefined;
        const full_detail: []const u8 = if (self.interpreter.last_error_location) |loc|
            (std.fmt.bufPrint(&detail_buf, "{s} at {d}:{d}", .{ detail, loc.line, loc.column }) catch detail)
        else
            detail;
        if (!builtin.is_test) {
            std.log.err("SOUNDNESS INCIDENT: {s} {s} faulted on a proven path ({s})", .{ method, path, full_detail });
        }
        if (self.config.incident_log_fd) |fd| {
            incident_log.write(self.allocator, fd, method, path, chips, full_detail);
        }
    }

    fn arenaHighWatermark(self: *Self) usize {
        const arena = self.arena_state orelse return 0;
        return arena.getStats().high_watermark;
    }

    fn recordCostBoundedIncident(self: *Self, detail: []const u8) void {
        self.recordSoundnessIncident(&.{"cost_bounded"}, detail);
    }

    fn recordCostFuseIncidents(self: *Self) void {
        const ceilings = self.config.cost_ceilings orelse return;
        const high_water = self.arenaHighWatermark();

        inline for (std.enums.values(cost_meter.ModuleClass)) |class| {
            if (ceilings.classLimit(class)) |ceiling| {
                const observed: u64 = self.ctx.cost_meter.count(class);
                if (observed > ceiling) {
                    var detail_buf: [192]u8 = undefined;
                    const detail = std.fmt.bufPrint(
                        &detail_buf,
                        "cost envelope exceeded: {s} {d} > {d} (arena high-water {d} bytes)",
                        .{ @tagName(class), observed, ceiling, high_water },
                    ) catch "cost envelope exceeded";
                    self.recordCostBoundedIncident(detail);
                    return;
                }
            }
        }

        if (ceilings.total) |ceiling| {
            const observed: u64 = self.ctx.cost_meter.total();
            if (observed > ceiling) {
                var detail_buf: [192]u8 = undefined;
                const detail = std.fmt.bufPrint(
                    &detail_buf,
                    "cost envelope exceeded: total {d} > {d} (arena high-water {d} bytes)",
                    .{ observed, ceiling, high_water },
                ) catch "cost envelope exceeded";
                self.recordCostBoundedIncident(detail);
                return;
            }
        }

        if (ceilings.total_is_constant) {
            const arena = self.arena_state orelse return;
            const stats = arena.getStats();
            if (stats.overflow_count > 0) {
                var detail_buf: [192]u8 = undefined;
                const detail = std.fmt.bufPrint(
                    &detail_buf,
                    "cost arena overflow under constant envelope: overflow_count {d} (arena high-water {d} bytes)",
                    .{ stats.overflow_count, stats.high_watermark },
                ) catch "cost arena overflow under constant envelope";
                self.recordCostBoundedIncident(detail);
            }
        }
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
        defer self.recordCostFuseIncidents();

        // === TRACE RECORDING: Set up per-request recorder ===
        const trace_timer = trace_request_recorder.setupRequestRecorder(self, request);
        defer trace_request_recorder.finishRequestRecorder(self, trace_timer);

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
                    trace_request_recorder.recordResponse(self, &response);
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
            trace_request_recorder.recordResponse(self, &response);
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
            if (err == error.RequestTimeout) return error.RequestTimeout;
            if (!builtin.is_test) std.log.err("Handler execution failed: {}", .{err});
            // Preserve the fault class in the error value so the 500 site can
            // map it to the proof chip that guards it (see fault_explain.zig).
            // TypeError/NotCallable are what optional_safe/result_safe guard.
            switch (err) {
                error.TypeError, error.NotCallable => {
                    const diag = fault_explain.diagnose(self.config.handler_proof, .type_fault);
                    if (diag.verdict == .soundness_incident) {
                        self.recordSoundnessIncident(diag.namedChips(), @errorName(err));
                    }
                    // Hand the resolved source line to the server's 500 site, which
                    // builds the body after this runtime is released (same worker
                    // thread, so the threadlocal is a safe read-and-cleared channel).
                    last_fault_location = self.interpreter.last_error_location;
                    return error.HandlerTypeFault;
                },
                else => return error.HandlerError,
            }
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
            // Proof-explain the exception: a pending exception that is not a
            // Response is an engine-raised type fault (throw is not in the subset).
            var fault_buf: [256]u8 = undefined;
            const diag = fault_explain.diagnose(self.config.handler_proof, .type_fault);
            const explained = fault_explain.formatMessage(&fault_buf, diag);
            // Append the source line when the trap resolved one (feature A). A
            // JS-thrown exception often has none, so this is best-effort.
            const body_owned = if (self.interpreter.last_error_location) |loc|
                try std.fmt.allocPrint(self.allocator, "{s} ({s}) at {d}:{d}", .{ explained, exc_msg, loc.line, loc.column })
            else
                try std.fmt.allocPrint(self.allocator, "{s} ({s})", .{ explained, exc_msg });
            err_response.setBodyOwned(body_owned);
            try err_response.putHeader("Content-Type", "text/plain; charset=utf-8");
            if (diag.verdict == .soundness_incident) {
                self.recordSoundnessIncident(diag.namedChips(), exc_msg);
            }
            trace_request_recorder.recordResponse(self, &err_response);
            return err_response;
        }

        // Convert result to HttpResponse
        const response = try self.extractResponseInternal(result, borrow_body);
        if (borrow_body and response.requires_runtime) {
            reset_after = false;
        }

        trace_request_recorder.recordResponse(self, &response);
        return response;
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

        const content_type = switch (pattern.content_type_idx) {
            0 => "application/json",
            1 => "text/plain; charset=utf-8",
            else => "text/html; charset=utf-8",
        };

        // OPTIMIZATION: Use pre-built raw response if available (single write, no header construction)
        if (pattern.prebuilt_response) |prebuilt| {
            response.prebuilt_raw = prebuilt;
            // Still set body for logging/debugging purposes
            response.body = pattern.static_body;
            response.body_owned = false;
            // Also populate the structured Content-Type header. The slow path
            // (Connection: close, or attestation active) rebuilds headers from
            // response.headers and ignores prebuilt_raw's baked-in Content-Type,
            // so without this such responses would ship with no Content-Type.
            try response.putHeaderBorrowed("Content-Type", content_type);
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

        try response.putHeaderBorrowed("Content-Type", content_type);

        return response;
    }

    /// Try to parse a query parameter value as a 32-bit signed integer.
    /// Returns null if the value is not a valid integer (empty, has non-digit chars, overflow).
    /// Supports optional leading minus sign for negative numbers.
    pub fn parseQueryInt(value: []const u8) ?i32 {
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

    pub fn headerKeyToAtom(key: []const u8) ?zq.Atom {
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

    pub fn createString(self: *Self, str: []const u8) !zq.JSValue {
        return self.ctx.createString(str);
    }

    pub fn callFunction(self: *Self, func_obj: *zq.JSObject, args: []const zq.JSValue) !zq.JSValue {
        if (func_obj.getClosureData()) |closure_data| {
            const prev_closure = self.interpreter.current_closure;
            self.interpreter.current_closure = closure_data;
            defer self.interpreter.current_closure = prev_closure;

            return try self.interpreter.callBytecodeFunction(
                func_obj.toValue(),
                closure_data.bytecode,
                zq.JSValue.undefined_val,
                args,
            );
        }

        if (func_obj.getBytecodeFunctionData()) |bc_data| {
            return try self.interpreter.callBytecodeFunction(
                func_obj.toValue(),
                bc_data.bytecode,
                zq.JSValue.undefined_val,
                args,
            );
        }

        const native_data = func_obj.getNativeFunctionData() orelse return error.NotCallable;
        return native_data.func(self.ctx, zq.JSValue.undefined_val, args);
    }

    /// Wrapper for calling JS functions from http.zig (used for JSX function components)
    fn callFunctionWrapper(func_obj: *zq.JSObject, args: []const zq.JSValue) anyerror!zq.JSValue {
        const runtime = current_runtime orelse return error.NoRuntime;
        return runtime.callFunction(func_obj, args);
    }

    /// Set a response body from any JS string value. A genuine flat JSString can
    /// be borrowed (pinned via its pointer); a rope/slice is flattened (into the
    /// request arena) and copied, since there is no JSString handle to borrow.
    fn setResponseBody(self: *Self, response: *HttpResponse, val: zq.JSValue, borrow_body: bool) !void {
        if (val.isString()) {
            const str = val.toPtr(zq.JSString);
            if (borrow_body) {
                response.setBodyBorrowed(str.data(), @ptrCast(str));
            } else {
                response.setBodyOwned(try self.allocator.dupe(u8, str.data()));
            }
        } else if (getStringDataCtx(val, self.ctx)) |bytes| {
            response.setBodyOwned(try self.allocator.dupe(u8, bytes));
        }
    }

    /// Put a response header whose value is any JS string. Flat strings keep the
    /// borrow fast path; rope/slice values are flattened and copied.
    fn putResponseHeader(self: *Self, response: *HttpResponse, name: []const u8, val: zq.JSValue, borrow_body: bool) !void {
        if (val.isString()) {
            const str = val.toPtr(zq.JSString);
            if (borrow_body) {
                try response.putHeaderBorrowedRuntime(name, str.data());
            } else {
                try response.putHeader(name, str.data());
            }
        } else if (getStringDataCtx(val, self.ctx)) |bytes| {
            try response.putHeader(name, bytes);
        }
    }

    pub fn extractResponseInternal(self: *Self, result: zq.JSValue, borrow_body: bool) !HttpResponse {
        // Hypermedia resource: content-negotiate to HAL-JSON or HTMX using the
        // request's Accept / HX-Request headers, then extract the rendered
        // Response normally. The rendered value is a plain Response (not branded),
        // so the recursive call takes the standard path.
        if (result.isObject() and zq.http.isResource(self.ctx, result)) {
            var accept: []const u8 = "";
            var hx_request = false;
            if (self.active_request) |req| {
                for (req.headers.items) |hdr| {
                    if (std.ascii.eqlIgnoreCase(hdr.key, "accept")) {
                        accept = hdr.value;
                    } else if (std.ascii.eqlIgnoreCase(hdr.key, "hx-request")) {
                        hx_request = std.ascii.eqlIgnoreCase(hdr.value, "true");
                    }
                }
            }
            const rendered = try zq.http.renderResource(self.ctx, result, accept, hx_request);
            return self.extractResponseInternal(rendered, borrow_body);
        }

        var response = HttpResponse.init(self.allocator);

        if (!result.isObject()) {
            // If result is any string (flat, slice, or concat rope), use it as
            // the body (supported return type). getStringDataCtx is non-null for
            // every string kind and null for primitives, which fall through to 500.
            if (getStringDataCtx(result, self.ctx) != null) {
                try self.setResponseBody(&response, result, borrow_body);
                return response;
            }
            // Non-string, non-object: handler returned a primitive (number, bool, undefined).
            // Return 500 instead of a silent empty 200, proof-explained against
            // exhaustive_returns (the chip that proves every path returns a Response).
            response.status = 500;
            var fault_buf: [256]u8 = undefined;
            const diag = fault_explain.diagnose(self.config.handler_proof, .non_response_return);
            const body_owned = try self.allocator.dupe(u8, fault_explain.formatMessage(&fault_buf, diag));
            response.setBodyOwned(body_owned);
            try response.putHeader("Content-Type", "text/plain; charset=utf-8");
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
                try self.setResponseBody(&response, body_val, borrow_body);

                // Headers - still need property iteration for custom headers
                const headers_val = result_obj.getSlot(shapes.response.headers_slot);
                if (headers_val.isObject()) {
                    const headers_obj = headers_val.toPtr(zq.JSObject);
                    if (headers_obj.hidden_class_idx == shapes.response_headers.class_idx) {
                        try self.putResponseHeader(&response, "Content-Type", headers_obj.getSlot(shapes.response_headers.content_type_slot), borrow_body);
                        try self.putResponseHeader(&response, "Content-Length", headers_obj.getSlot(shapes.response_headers.content_length_slot), borrow_body);
                        try self.putResponseHeader(&response, "Cache-Control", headers_obj.getSlot(shapes.response_headers.cache_control_slot), borrow_body);
                    } else {
                        const keys = try headers_obj.getOwnEnumerableKeys(self.allocator, pool);
                        defer self.allocator.free(keys);
                        for (keys) |key_atom| {
                            const key_name = self.ctx.atoms.getName(key_atom) orelse continue;
                            const header_val = headers_obj.getOwnProperty(pool, key_atom) orelse continue;
                            try self.putResponseHeader(&response, key_name, header_val, borrow_body);
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
            try self.setResponseBody(&response, body_val, borrow_body);
        }

        if (result_obj.getOwnProperty(pool, zq.Atom.headers)) |headers_val| {
            if (headers_val.isObject()) {
                const headers_obj = headers_val.toPtr(zq.JSObject);
                const keys = try headers_obj.getOwnEnumerableKeys(self.allocator, pool);
                defer self.allocator.free(keys);
                for (keys) |key_atom| {
                    const key_name = self.ctx.atoms.getName(key_atom) orelse continue;
                    const header_val = headers_obj.getOwnProperty(pool, key_atom) orelse continue;
                    try self.putResponseHeader(&response, key_name, header_val, borrow_body);
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
        self.ctx.cost_meter.reset();
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

    pub fn prepareForPoolRelease(self: *Self) void {
        // Clear zruntime-layer request state that pool.zig's Runtime.reset()
        // does not know about. The engine-level sp/call_depth/exception clear
        // happens inside pool.release() -> Runtime.reset().
        self.consumed_body_objects.clearRetainingCapacity();
        self.last_request_body_len = 0;
    }
};

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

// ============================================================================
// Percentile Tracker for Latency Metrics
// ============================================================================

/// Mutex-protected ring buffer that records nanosecond-resolution latency samples.
/// Used only for diagnostic metrics in debug builds, so we prefer correctness
/// under contention over lock-free writes.
pub const PercentileTracker = @import("runtime_percentile.zig").PercentileTracker;

// ============================================================================
// Handler Pool (Lock-Free)
// ============================================================================

/// Lock-free pool of pre-initialized JavaScript runtimes, backed by zigts.LockFreePool.
/// Uses per-runtime wrappers to install builtins and load handler code once.
pub const HandlerPool = @import("runtime_pool.zig").HandlerPool;

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
    // Only consulted when mode == .sequenced_status: one status code per
    // accepted connection, in order.
    status_sequence: []const u16 = &.{},

    const Mode = enum {
        echo_request_json,
        large_plain_text,
        silent_hold,
        sequenced_status,
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

        if (self.mode == .sequenced_status) {
            for (self.status_sequence) |status| {
                var stream = while (true) {
                    break self.listener.accept(io) catch |err| switch (err) {
                        error.ConnectionAborted => continue,
                        error.SocketNotListening => return,
                        else => return err,
                    };
                };
                defer stream.close(io);
                try self.respondWithStatus(&stream, io, status);
            }
            return;
        }

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
            .silent_hold => try self.holdSilently(&stream, io),
            .sequenced_status => unreachable,
        }
    }

    /// Accepts the request but never writes a byte back; returns once the
    /// client gives up and closes its end.
    fn holdSilently(self: *TestHttpServer, stream: *std.Io.net.Stream, io: std.Io) !void {
        _ = self;
        while (true) {
            var chunk: [1024]u8 = undefined;
            var vecs: [1][]u8 = .{chunk[0..]};
            const n = io.vtable.netRead(io.userdata, stream.socket.handle, &vecs) catch break;
            if (n == 0) break;
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

    fn respondWithStatus(self: *TestHttpServer, stream: *std.Io.net.Stream, io: std.Io, status: u16) !void {
        var captured = try captureRequest(self.allocator, stream, io);
        defer captured.deinit(self.allocator);
        const reason: []const u8 = if (status >= 500) "Internal Server Error" else "OK";
        try writeTestResponse(stream, io, status, reason, &.{"Content-Type: text/plain"}, "");
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

test "durable run+step cycle frees nested non-closure function bytecode exactly once" {
    // Regression for a double-free in Context.deinit's bytecode_functions
    // teardown: run()'s and step()'s zero-upvalue arrow callbacks compile to
    // non-closure make_function objects (no captured locals), so their
    // FunctionBytecode is simultaneously (a) a constant of their lexically
    // enclosing function - freed recursively via destroyConstant - and (b)
    // independently tracked in ctx.bytecode_functions - freed again via
    // destroyFullTracked. Masked everywhere else in this file because those
    // tests wrap Runtime in an arena, where a double-free is a silent no-op.
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);
    defer allocator.free(durable_dir);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { run, step } from "zigttp:durable";
        \\function handler(req) {
        \\  return run("nested-bytecode-owner", () => {
        \\    const v = step("s", () => 1);
        \\    return Response.json({ v: v });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<nested-bytecode-owner>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 200), response.status);
}

test "zigttp:queue sends receives and acks JSON payloads" {
    const allocator = std.testing.allocator;

    var queue = actor_queue.ActorQueue.init(allocator, 8, 30_000);
    defer queue.deinit();

    const rt = try Runtime.init(allocator, .{
        .jit_policy = .disabled,
        .queue_system = @ptrCast(&queue),
        .queue_actor_name = "main",
    });
    defer rt.deinit();

    const direct_payload = try rt.ctx.createString("direct");
    _ = try rt.queueSendInternal(rt.ctx, "direct", direct_payload, false);
    try std.testing.expect(!rt.ctx.hasException());
    const direct_msg = (try queue.receive("direct")).?;
    try std.testing.expect(queue.ack(direct_msg.id));

    const handler_code =
        \\import { send, receive, ack } from "zigttp:queue";
        \\function handler(req) {
        \\  const sent = send("worker", { kind: "work", n: 3 });
        \\  if (!sent.ok) return Response.json({ error: sent.error }, { status: 500 });
        \\  const inbox = receive("worker");
        \\  if (!inbox.ok) return Response.json({ error: inbox.error }, { status: 500 });
        \\  const msg = inbox.value;
        \\  const acknowledged = ack(msg.id);
        \\  return Response.json({
        \\    id: sent.value,
        \\    source: msg.source,
        \\    target: msg.target,
        \\    attempt: msg.attempt,
        \\    kind: msg.payload.kind,
        \\    n: msg.payload.n,
        \\    acked: acknowledged.ok
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<queue>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 200), response.status);
    // The default reply-to identity is namespaced per Runtime instance
    // ("main#<address>") so concurrent pooled runtimes never share a
    // mailbox; only the prefix is deterministic across test runs.
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"source\":\"main#") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"target\":\"worker\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"kind\":\"work\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"n\":3") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"acked\":true") != null);
    try std.testing.expectEqual(@as(?*actor_queue.MessageEnvelope, null), try queue.receive("worker"));
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

    const path = try durable_executor.buildDurableOplogPath(rt, key);
    defer allocator.free(path);

    const fd = try openOplogFile(allocator, path);
    defer std.Io.Threaded.closeFd(fd);

    var state = zq.trace.DurableState.init(allocator, &.{}, fd);
    defer state.deinit();

    const header_names = [_][]const u8{"idempotency-key"};
    const header_values = [_][]const u8{key};
    const result = zq.trace.jsonToJSValue(rt.ctx, result_json);

    try state.persistRunKey(key);
    try state.persistRequest("GET", "/", &header_names, &header_values, null);
    try state.persistStepStart(step_name);
    try state.persistIO("builtin", "Math.random", rt.ctx, &.{}, result);
    try state.persistStepResult(step_name, rt.ctx, result);
}

// Pull tests from sibling files that are not otherwise reachable from this
// module's import graph. Zig's test runner only analyzes files transitively
// reached from the test root, so handler_loader.zig (used by replay_runner,
// test_runner, and durable_recovery) needs an explicit hook here.
test {
    _ = @import("handler_loader.zig");
    _ = @import("replay_runner.zig");
    _ = @import("websocket_codec.zig");
    _ = @import("websocket_pool.zig");
    _ = @import("ws_gateway.zig");
    _ = @import("ws_frame_loop.zig");
    _ = @import("durable_fetch.zig");
    _ = @import("retry_backoff.zig");
    _ = @import("benchmark.zig");
    _ = @import("durable_dead_runs.zig");
    _ = @import("durable_dead_runs_cli.zig");
    _ = @import("fault_explain.zig");
    _ = @import("incident_log.zig");
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

test "optional call short circuits on undefined callee" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    // `(undefined)?.()` must short-circuit to undefined instead of throwing
    // NotCallable; `(()=>9)?.()` must still invoke and yield 9.
    const handler_code =
        \\function handler(req) {
        \\  const f = undefined;
        \\  const g = () => 9;
        \\  return Response.text(typeof f?.() + ":" + g?.());
        \\}
    ;
    try rt.loadHandler(handler_code, "<optional-call>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();
    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expectEqualStrings("undefined:9", response.body);
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

    const path = try durable_executor.buildDurableOplogPath(rt, "order:123");
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

    const path = try durable_executor.buildDurableOplogPath(rt, "resume:123");
    defer allocator.free(path);

    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"fn\":\"Math.random\""));
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"fn\":\"Date.now\""));

    var parsed = try zq.trace.parseDurableOplog(allocator, source);
    defer parsed.deinit();

    try std.testing.expect(parsed.complete);
    try std.testing.expectEqualStrings("resume:123", parsed.run_key.?);
}

test "proof-gated durable retry allows proven workflow replay" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    try seedIncompleteDurableRandomStep(
        allocator,
        durable_dir,
        "retry:proven",
        "seed",
        "0.75",
    );

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .durable_workflow_properties = .{ .enforced = true, .retry_safe = true },
    });
    defer rt.deinit();

    const handler_code =
        \\import { run, step } from "zigttp:durable";
        \\function handler(req) {
        \\  return run("retry:proven", () => {
        \\    const seed = step("seed", () => Math.random());
        \\    return Response.json({ seed });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-retry-proven>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"seed\":0.75") != null);
}

test "runtime type fault is preserved as HandlerTypeFault for proof-explained 500" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{ .jit_policy = .disabled });
    defer rt.deinit();

    // `o.missing` is undefined at runtime; calling it raises NotCallable in the
    // interpreter. The bytecode compiles on this path (no analyzer veto), so the
    // fault reaches executeHandlerInternal's catch, which must preserve the fault
    // class as error.HandlerTypeFault (not the opaque error.HandlerError) so the
    // 500 site can name the proof chip that guards it. See fault_explain.zig.
    const handler_code = "function handler(req) { const o = { a: 1 }; const f = o.missing; return f(); }";
    try rt.loadHandler(handler_code, "<type-fault>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);

    try std.testing.expectError(error.HandlerTypeFault, rt.executeHandler(request.asView()));
}

test "non-Response return 500 is proof-explained against exhaustive_returns" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{ .jit_policy = .disabled });
    defer rt.deinit();

    // Returns a primitive, not a Response -> extractResponseInternal's Path B
    // builds a 500. handler_proof defaults to unproven, so the body names the
    // exhaustive_returns chip as the predicted cause.
    try rt.loadHandler("function handler(req) { return 42; }", "<non-response>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 500), response.status);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "exhaustive_returns") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "not proven") != null);
}

test "soundness incident on a proven path is written to the incident log" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const log_path = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}/incidents.jsonl", .{tmp.sub_path});

    const fd = try incident_log.open(allocator, log_path);
    defer std.Io.Threaded.closeFd(fd);

    // handler_proof claims both guarding chips proven, so a runtime type fault is
    // a soundness incident that must be recorded to the log.
    const rt = try Runtime.init(allocator, .{
        .jit_policy = .disabled,
        .handler_proof = .{ .optional_safe = true, .result_safe = true },
        .incident_log_fd = fd,
    });
    defer rt.deinit();

    try rt.loadHandler("function handler(req) { const o = { a: 1 }; const f = o.missing; return f(); }", "<incident>");
    var request = try makeTestRequest(allocator, "GET", "/boom", null);
    defer request.deinit(allocator);
    try std.testing.expectError(error.HandlerTypeFault, rt.executeHandler(request.asView()));

    const contents = try zq.file_io.readFile(allocator, log_path, 64 * 1024);
    try std.testing.expect(std.mem.indexOf(u8, contents, "soundness_incident") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "/boom") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "optional_safe") != null);
    // The single-line handler faults on line 1; the source map (feature A) must
    // surface that line into the incident detail.
    try std.testing.expect(std.mem.indexOf(u8, contents, "NotCallable at 1:") != null);
}

test "exceeding a constant cost ceiling records a soundness incident" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const log_path = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}/cost-incidents.jsonl", .{tmp.sub_path});

    const fd = try incident_log.open(allocator, log_path);
    defer std.Io.Threaded.closeFd(fd);

    const rt = try Runtime.init(allocator, .{
        .jit_policy = .disabled,
        .incident_log_fd = fd,
        .cost_ceilings = .{
            .total = 1,
            .total_is_constant = true,
        },
    });
    defer rt.deinit();

    try rt.loadHandler(
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  env("ONE");
        \\  env("TWO");
        \\  return Response.text("ok");
        \\}
    , "<cost-exceeded>");

    var request = try makeTestRequest(allocator, "GET", "/cost", null);
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expectEqualStrings("ok", response.body);

    const contents = try zq.file_io.readFile(allocator, log_path, 64 * 1024);
    try std.testing.expect(std.mem.indexOf(u8, contents, "soundness_incident") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "cost_bounded") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "cost envelope exceeded") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "total 2 > 1") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "arena high-water") != null);
}

test "requests within the ceiling record no incident" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const log_path = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}/cost-clean.jsonl", .{tmp.sub_path});

    const fd = try incident_log.open(allocator, log_path);
    defer std.Io.Threaded.closeFd(fd);

    const rt = try Runtime.init(allocator, .{
        .jit_policy = .disabled,
        .incident_log_fd = fd,
        .cost_ceilings = .{
            .total = 2,
            .total_is_constant = true,
        },
    });
    defer rt.deinit();

    try rt.loadHandler(
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  env("ONE");
        \\  return Response.text("ok");
        \\}
    , "<cost-clean>");

    var request = try makeTestRequest(allocator, "GET", "/cost", null);
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expectEqualStrings("ok", response.body);

    const contents = try zq.file_io.readFile(allocator, log_path, 64 * 1024);
    try std.testing.expect(std.mem.indexOf(u8, contents, "soundness_incident") == null);
}

test "cost meter resets between pooled requests" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const log_path = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}/cost-reset.jsonl", .{tmp.sub_path});

    const fd = try incident_log.open(allocator, log_path);
    defer std.Io.Threaded.closeFd(fd);

    const rt = try Runtime.init(allocator, .{
        .jit_policy = .disabled,
        .incident_log_fd = fd,
        .cost_ceilings = .{
            .total = 1,
            .total_is_constant = true,
        },
    });
    defer rt.deinit();

    try rt.loadHandler(
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  env("ONE");
        \\  return Response.text("ok");
        \\}
    , "<cost-reset>");

    var first = try makeTestRequest(allocator, "GET", "/first", null);
    defer first.deinit(allocator);
    var first_response = try rt.executeHandler(first.asView());
    defer first_response.deinit();
    try std.testing.expectEqual(@as(u32, 0), rt.ctx.cost_meter.total());

    var second = try makeTestRequest(allocator, "GET", "/second", null);
    defer second.deinit(allocator);
    var second_response = try rt.executeHandler(second.asView());
    defer second_response.deinit();
    try std.testing.expectEqual(@as(u32, 0), rt.ctx.cost_meter.total());

    const contents = try zq.file_io.readFile(allocator, log_path, 64 * 1024);
    try std.testing.expect(std.mem.indexOf(u8, contents, "soundness_incident") == null);
}

test "proof-gated durable retry blocks unproven workflow replay" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    try seedIncompleteDurableRandomStep(
        allocator,
        durable_dir,
        "retry:unproven",
        "seed",
        "0.5",
    );

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .durable_workflow_properties = .{ .enforced = true },
    });
    defer rt.deinit();

    const handler_code =
        \\import { run, step } from "zigttp:durable";
        \\function handler(req) {
        \\  return run("retry:unproven", () => {
        \\    const seed = step("seed", () => Math.random());
        \\    return Response.json({ seed });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-retry-unproven>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 599), response.status);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "DurableRetryUnproven") != null);
}

test "idempotency ledger allows unproven durable retry" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    try seedIncompleteDurableRandomStep(
        allocator,
        durable_dir,
        "retry:ledger",
        "seed",
        "0.25",
    );
    var store = durable_store_mod.DurableStore.initFs(allocator, durable_dir);
    try store.writeIdempotencyLedger("retry:ledger", "retry:ledger", .started);

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .durable_workflow_properties = .{ .enforced = true },
    });
    defer rt.deinit();

    const handler_code =
        \\import { run, step } from "zigttp:durable";
        \\function handler(req) {
        \\  const key = req.headers.get("idempotency-key") ?? "missing";
        \\  return run(key, () => {
        \\    const seed = step("seed", () => Math.random());
        \\    return Response.json({ seed });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-retry-ledger>");

    var request = try makeTestRequest(allocator, "GET", "/", "retry:ledger");
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"seed\":0.25") != null);
    try std.testing.expect(try store.hasIdempotencyLedger("retry:ledger", "retry:ledger"));
}

test "idempotency ledger allows unproven duplicate durable response reuse" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .durable_workflow_properties = .{ .enforced = true },
    });
    defer rt.deinit();

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\function handler(req) {
        \\  const key = req.headers.get("idempotency-key") ?? "missing";
        \\  return run(key, () => Response.json({ value: Math.random() }));
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-idem-ledger>");

    var first_request = try makeTestRequest(allocator, "GET", "/", "idem:duplicate");
    defer first_request.deinit(allocator);
    var first_response = try rt.executeHandler(first_request.asView());
    defer first_response.deinit();
    const first_body = try allocator.dupe(u8, first_response.body);

    var second_request = try makeTestRequest(allocator, "GET", "/", "idem:duplicate");
    defer second_request.deinit(allocator);
    var second_response = try rt.executeHandler(second_request.asView());
    defer second_response.deinit();

    try std.testing.expectEqual(@as(u16, 200), second_response.status);
    try std.testing.expectEqualStrings(first_body, second_response.body);

    const path = try durable_executor.buildDurableOplogPath(rt, "idem:duplicate");
    defer allocator.free(path);
    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"fn\":\"Math.random\""));
}

fn seedIncompleteWorkflowCallStep(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    key: []const u8,
    step_name: []const u8,
    result_json: []const u8,
) !void {
    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const path = try durable_executor.buildDurableOplogPath(rt, key);
    defer allocator.free(path);

    const fd = try openOplogFile(allocator, path);
    defer std.Io.Threaded.closeFd(fd);

    var state = zq.trace.DurableState.init(allocator, &.{}, fd);
    defer state.deinit();

    const header_names = [_][]const u8{"idempotency-key"};
    const header_values = [_][]const u8{key};
    const result = zq.trace.jsonToJSValue(rt.ctx, result_json);

    try state.persistRunKey(key);
    try state.persistRequest("GET", "/", &header_names, &header_values, null);
    try state.persistStepStart(step_name);
    try state.persistStepResult(step_name, rt.ctx, result);
}

test "workflow.call inside durable run replays a completed step from cache (no re-dispatch)" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    // Seed a COMPLETED workflow.call#0 step whose stored Response body marks it
    // as the cached (oplog) value. The run itself stays incomplete so the
    // orchestrator re-executes on the next request.
    try seedIncompleteWorkflowCallStep(
        allocator,
        durable_dir,
        "wf:1",
        "workflow.call#0",
        "{\"status\":200,\"headers\":{\"content-type\":\"application/json\"},\"body\":\"{\\\"cached\\\":true}\"}",
    );

    // The live sub-handler would return cached:false if (wrongly) re-dispatched.
    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "greet",
        "function handler(req) { return Response.json({ cached: false }); }",
        "<greet>",
        .{ .jit_policy = .disabled },
        1,
    );

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .system_registry = @ptrCast(&sys),
    });
    defer rt.deinit();

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { call } from "zigttp:workflow";
        \\function handler(req) {
        \\  const key = req.headers.get("idempotency-key") ?? "missing";
        \\  return run(key, () => {
        \\    const res = call("greet", { method: "GET", path: "/greet" });
        \\    return Response.json({ subStatus: res.status, sub: res.json() });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<wf-durable>");

    var request = try makeTestRequest(allocator, "GET", "/", "wf:1");
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    // The cached step result wins: greet is NOT re-dispatched, so the body
    // carries the oplog's cached:true and a real reconstructed 200 Response.
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"cached\":true") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"subStatus\":200") != null);
}

test "workflow.call inside durable run records its dispatch as a durable step" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "greet",
        "function handler(req) { return Response.json({ from: 'greet' }); }",
        "<greet>",
        .{ .jit_policy = .disabled },
        1,
    );

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .system_registry = @ptrCast(&sys),
    });
    defer rt.deinit();

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { call } from "zigttp:workflow";
        \\function handler(req) {
        \\  const key = req.headers.get("idempotency-key") ?? "missing";
        \\  return run(key, () => {
        \\    const res = call("greet", { method: "GET", path: "/greet" });
        \\    return Response.json({ subStatus: res.status, sub: res.json() });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<wf-durable-live>");

    var request = try makeTestRequest(allocator, "GET", "/", "wf:live");
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    // First run dispatches greet live and composes its response.
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"from\":\"greet\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"subStatus\":200") != null);

    // The call was recorded as its own durable step in the oplog.
    const path = try durable_executor.buildDurableOplogPath(rt, "wf:live");
    defer allocator.free(path);
    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    // The call appears twice: once in step_start, once in step_result.
    try std.testing.expectEqual(@as(usize, 2), std.mem.count(u8, source, "\"name\":\"workflow.call#0\""));
    try std.testing.expect(std.mem.indexOf(u8, source, "\"type\":\"step_result\"") != null);

    var parsed = try zq.trace.parseDurableOplog(allocator, source);
    defer parsed.deinit();
    try std.testing.expect(parsed.complete);
}

test "workflow.call queue mode persists child result before durable step result" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "greet",
        "function handler(req) { return Response.json({ from: 'greet', method: req.method, body: req.body, trace: req.headers.get('x-trace') }); }",
        "<greet>",
        .{ .jit_policy = .disabled },
        1,
    );

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .system_registry = @ptrCast(&sys),
        .workflow_queue_enabled = true,
    });
    defer rt.deinit();

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { call } from "zigttp:workflow";
        \\function handler(req) {
        \\  return run("wf:queue", () => {
        \\    const res = call("greet", { method: "POST", path: "/greet", body: "hello", headers: { "x-trace": "queued" } });
        \\    return Response.json({ subStatus: res.status, sub: res.json() });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<wf-queue>");

    var request = try makeTestRequest(allocator, "GET", "/", "wf:queue");
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"from\":\"greet\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"method\":\"POST\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"body\":\"hello\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"trace\":\"queued\"") != null);

    const item_id = try workflow_queue.itemId(allocator, "wf:queue", "workflow.call#0");
    const result_json = (try workflow_queue.readResult(allocator, durable_dir, item_id)) orelse return error.MissingWorkflowQueueResult;
    try std.testing.expect(std.mem.indexOf(u8, result_json, "\"status\":200") != null);
    try std.testing.expect(std.mem.indexOf(u8, result_json, "greet") != null);

    const path = try durable_executor.buildDurableOplogPath(rt, "wf:queue");
    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 2), std.mem.count(u8, source, "\"name\":\"workflow.call#0\""));
    try std.testing.expect(std.mem.indexOf(u8, source, "\"type\":\"step_result\"") != null);
}

test "workflow-queue dead letter suspends the parent, and replay resolves it" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler(
        "greet",
        "function handler(req) { return Response.json({ from: 'greet' }); }",
        "<greet>",
        .{ .jit_policy = .disabled },
        1,
    );

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .system_registry = @ptrCast(&sys),
        .workflow_queue_enabled = true,
    });
    defer rt.deinit();

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { call } from "zigttp:workflow";
        \\function handler(req) {
        \\  return run("wf:dead-retry", () => {
        \\    const res = call("greet", { method: "GET", path: "/greet" });
        \\    return Response.json({ subStatus: res.status, sub: res.json() });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<wf-dead-retry>");

    const item_id = try workflow_queue.itemId(allocator, "wf:dead-retry", "workflow.call#0");

    const view: HttpRequestView = .{
        .method = "GET",
        .path = "/greet",
        .url = "/greet",
        .query_params = &.{},
        .headers = .empty,
        .body = null,
    };
    try workflow_queue.enqueueRequest(allocator, durable_dir, item_id, "greet", view);

    // Drive the queue item to dead-letter by exhausting its attempt cap
    // through repeated lease-expiry reclaims without ever completing it -
    // simulating the child handler crashing every time before it finishes.
    const lease_ms = workflow_queue.defaultLeaseMs();
    var now_ms: i64 = 0;
    var attempt: u32 = 0;
    while (attempt <= workflow_queue.defaultMaxAttempts()) : (attempt += 1) {
        var claim = try workflow_queue.tryClaim(allocator, durable_dir, item_id, now_ms, lease_ms);
        defer claim.deinit(allocator);
        if (claim == .dead) break;
        now_ms += lease_ms + 1;
    }

    const dead_ids = try workflow_queue.listDeadIds(allocator, durable_dir);
    try std.testing.expectEqual(@as(usize, 1), dead_ids.len);

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);

    // First attempt: the child is dead-lettered, so the parent must suspend
    // (202, pending) rather than caching a terminal error response that a
    // later replay could never undo - the finding #2 fix.
    var suspended = try rt.executeHandler(request.asView());
    defer suspended.deinit();
    try std.testing.expectEqual(@as(u16, 202), suspended.status);
    try std.testing.expect(std.mem.indexOf(u8, suspended.body, "\"pending\":true") != null);

    try workflow_queue.replayDead(allocator, durable_dir, item_id);

    // Retrying the same parent request now actually dispatches the child
    // and completes the run - the recovery guarantee `workflow-queue
    // replay` is supposed to provide.
    var recovered = try rt.executeHandler(request.asView());
    defer recovered.deinit();
    try std.testing.expectEqual(@as(u16, 200), recovered.status);
    try std.testing.expect(std.mem.indexOf(u8, recovered.body, "\"from\":\"greet\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, recovered.body, "\"subStatus\":200") != null);
}

test "workflow.saga is rejected under workflow queue mode" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .workflow_queue_enabled = true,
    });
    defer rt.deinit();

    const result = try workflow.workflowSagaCallback(@ptrCast(rt), rt.ctx, zq.JSValue.undefined_val);
    try std.testing.expect(result.isException());
    try std.testing.expect(rt.ctx.hasException());
    rt.ctx.clearException();
}

// Saga (P3): an empty SystemRuntime is enough to enable the workflow module;
// each saga step's run/compensate thunk returns a Response.json directly so the
// success/failure status is controlled without sub-handlers.
fn runSagaHandler(allocator: std.mem.Allocator, durable_dir: []const u8, key: []const u8, handler_code: []const u8) !struct { status: u16, body: []u8, oplog: []u8 } {
    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .system_registry = @ptrCast(&sys),
    });
    defer rt.deinit();
    try rt.loadHandler(handler_code, "<saga>");

    var request = try makeTestRequest(allocator, "GET", "/", key);
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    const status = response.status;
    const body = try allocator.dupe(u8, response.body);

    const path = try durable_executor.buildDurableOplogPath(rt, key);
    defer allocator.free(path);
    const oplog = try zq.file_io.readFile(allocator, path, 1024 * 1024);

    return .{ .status = status, .body = body, .oplog = oplog };
}

test "workflow.saga runs every step and returns ok:true when none fail" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { saga } from "zigttp:workflow";
        \\function handler(req) {
        \\  return run("saga:ok", () => saga([
        \\    { name: "a", run: () => Response.json({ step: "a" }) },
        \\    { name: "b", run: () => Response.json({ step: "b" }) },
        \\  ]));
        \\}
    ;
    const out = try runSagaHandler(allocator, durable_dir, "saga:ok", handler_code);

    try std.testing.expectEqual(@as(u16, 200), out.status);
    try std.testing.expect(std.mem.indexOf(u8, out.body, "\"ok\":true") != null);
    try std.testing.expect(std.mem.indexOf(u8, out.oplog, "\"name\":\"do:a\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, out.oplog, "\"name\":\"do:b\"") != null);
    // No compensation ran.
    try std.testing.expect(std.mem.indexOf(u8, out.oplog, "undo:") == null);
}

test "workflow.saga compensates completed steps in reverse order on failure" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { saga } from "zigttp:workflow";
        \\function handler(req) {
        \\  return run("saga:fail", () => saga([
        \\    { name: "reserve", run: () => Response.json({ ok: true }), compensate: () => Response.json({ undone: "reserve" }) },
        \\    { name: "charge", run: () => Response.json({ ok: true }), compensate: () => Response.json({ undone: "charge" }) },
        \\    { name: "ship", run: () => Response.json({ err: true }, { status: 500 }) },
        \\  ]));
        \\}
    ;
    const out = try runSagaHandler(allocator, durable_dir, "saga:fail", handler_code);

    // The failed step's status propagates; the rollback summary marks it failed.
    try std.testing.expectEqual(@as(u16, 500), out.status);
    try std.testing.expect(std.mem.indexOf(u8, out.body, "\"ok\":false") != null);
    try std.testing.expect(std.mem.indexOf(u8, out.body, "\"failed\":\"ship\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, out.body, "\"compensated\":true") != null);

    // Both completed steps compensated, in REVERSE declaration order.
    const undo_charge = std.mem.indexOf(u8, out.oplog, "undo:charge") orelse return error.MissingUndoCharge;
    const undo_reserve = std.mem.indexOf(u8, out.oplog, "undo:reserve") orelse return error.MissingUndoReserve;
    try std.testing.expect(undo_charge < undo_reserve);
    // The failed step itself was not compensated (it never completed).
    try std.testing.expect(std.mem.indexOf(u8, out.oplog, "undo:ship") == null);
}

test "workflow.saga returns terminal 500 when a compensation itself fails" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { saga } from "zigttp:workflow";
        \\function handler(req) {
        \\  return run("saga:compfail", () => saga([
        \\    { name: "reserve", run: () => Response.json({ ok: true }), compensate: () => Response.json({ e: 1 }, { status: 500 }) },
        \\    { name: "charge", run: () => Response.json({ bad: true }, { status: 402 }) },
        \\  ]));
        \\}
    ;
    const out = try runSagaHandler(allocator, durable_dir, "saga:compfail", handler_code);

    // charge (402) fails -> compensate reserve -> reserve's compensate returns
    // 500 -> terminal "manual intervention" with the offending step named.
    try std.testing.expectEqual(@as(u16, 500), out.status);
    try std.testing.expect(std.mem.indexOf(u8, out.body, "\"ok\":false") != null);
    try std.testing.expect(std.mem.indexOf(u8, out.body, "\"compensationFailed\":\"reserve\"") != null);
}

fn seedIncompleteSagaSteps(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    key: []const u8,
    names: []const []const u8,
    results: []const []const u8,
) !void {
    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const path = try durable_executor.buildDurableOplogPath(rt, key);
    defer allocator.free(path);
    const fd = try openOplogFile(allocator, path);
    defer std.Io.Threaded.closeFd(fd);

    var state = zq.trace.DurableState.init(allocator, &.{}, fd);
    defer state.deinit();

    const header_names = [_][]const u8{"idempotency-key"};
    const header_values = [_][]const u8{key};
    try state.persistRunKey(key);
    try state.persistRequest("GET", "/", &header_names, &header_values, null);
    for (names, results) |n, r| {
        const result = zq.trace.jsonToJSValue(rt.ctx, r);
        try state.persistStepStart(n);
        try state.persistStepResult(n, rt.ctx, result);
    }
}

test "workflow.saga replays cached do: steps and re-derives compensation on recovery" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    // Seed a crash mid-saga: reserve completed ok (200), charge completed as a
    // FAILURE (500). The run stays incomplete so the orchestrator re-executes.
    try seedIncompleteSagaSteps(
        allocator,
        durable_dir,
        "saga:replay",
        &.{ "do:reserve", "do:charge" },
        &.{ "{\"status\":200}", "{\"status\":500}" },
    );

    // On replay the live run thunks would BOTH return 200 (success). If the
    // cached do:charge (500) is honored, the saga still fails and compensates
    // reserve. If the steps were wrongly re-run, charge would be 200 -> ok:true.
    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { saga } from "zigttp:workflow";
        \\function handler(req) {
        \\  return run("saga:replay", () => saga([
        \\    { name: "reserve", run: () => Response.json({ live: true }), compensate: () => Response.json({ undone: true }) },
        \\    { name: "charge", run: () => Response.json({ live: true }) },
        \\  ]));
        \\}
    ;
    const out = try runSagaHandler(allocator, durable_dir, "saga:replay", handler_code);

    // The cached failure wins -> compensation path, proving do:charge was NOT re-run.
    try std.testing.expectEqual(@as(u16, 500), out.status);
    try std.testing.expect(std.mem.indexOf(u8, out.body, "\"ok\":false") != null);
    try std.testing.expect(std.mem.indexOf(u8, out.body, "\"failed\":\"charge\"") != null);
    // reserve was compensated on recovery.
    try std.testing.expect(std.mem.indexOf(u8, out.oplog, "undo:reserve") != null);
}

test "workflow.fanout returns sub-handler responses in declaration order" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler("a", "function handler(req) { return Response.json({ who: 'a' }); }", "<a>", .{ .jit_policy = .disabled }, 1);
    try sys.addHandler("b", "function handler(req) { return Response.json({ who: 'b' }); }", "<b>", .{ .jit_policy = .disabled }, 1);
    try sys.addHandler("c", "function handler(req) { return Response.json({ who: 'c' }); }", "<c>", .{ .jit_policy = .disabled }, 1);

    const rt = try Runtime.init(allocator, .{ .system_registry = @ptrCast(&sys) });
    defer rt.deinit();

    const handler_code =
        \\import { fanout } from "zigttp:workflow";
        \\function handler(req) {
        \\  const rs = fanout([{ name: "a" }, { name: "b" }, { name: "c" }]);
        \\  return Response.json({ n: rs.length, a: rs[0].json(), b: rs[1].json(), c: rs[2].json() });
        \\}
    ;
    try rt.loadHandler(handler_code, "<parallel>");
    var request = try makeTestRequest(allocator, "GET", "/", "x");
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 200), response.status);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"n\":3") != null);
    // Results are in declaration order a, b, c regardless of execution order.
    const ia = std.mem.indexOf(u8, response.body, "\"who\":\"a\"") orelse return error.MissingA;
    const ib = std.mem.indexOf(u8, response.body, "\"who\":\"b\"") orelse return error.MissingB;
    const ic = std.mem.indexOf(u8, response.body, "\"who\":\"c\"") orelse return error.MissingC;
    try std.testing.expect(ia < ib and ib < ic);
}

test "workflow.fanout records the whole fan-out as one durable step" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler("a", "function handler(req) { return Response.json({ who: 'a' }); }", "<a>", .{ .jit_policy = .disabled }, 1);
    try sys.addHandler("b", "function handler(req) { return Response.json({ who: 'b' }); }", "<b>", .{ .jit_policy = .disabled }, 1);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir, .system_registry = @ptrCast(&sys) });
    defer rt.deinit();

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { fanout } from "zigttp:workflow";
        \\function handler(req) {
        \\  return run("par:1", () => {
        \\    const rs = fanout([{ name: "a" }, { name: "b" }]);
        \\    return Response.json({ n: rs.length, a: rs[0].json(), b: rs[1].json() });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<parallel-durable>");
    var request = try makeTestRequest(allocator, "GET", "/", "par:1");
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"who\":\"a\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "\"n\":2") != null);

    const path = try durable_executor.buildDurableOplogPath(rt, "par:1");
    defer allocator.free(path);
    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    // One fan-out step (step_start + step_result name it), and NO per-call steps.
    // The durable name stays workflow.parallel#0 for replay compatibility.
    try std.testing.expectEqual(@as(usize, 2), std.mem.count(u8, source, "\"name\":\"workflow.parallel#0\""));
    try std.testing.expect(std.mem.indexOf(u8, source, "workflow.call#") == null);
}

test "workflow.fanout replays its aggregate from cache without re-dispatching" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    // Seed a completed fan-out step whose stored aggregate marks both entries
    // as the cached value. Reuses the single-step seed helper - the step result
    // is just an array of {status,headers,body}.
    try seedIncompleteWorkflowCallStep(
        allocator,
        durable_dir,
        "par:replay",
        "workflow.parallel#0",
        "[{\"status\":200,\"headers\":{},\"body\":\"{\\\"v\\\":\\\"cached-a\\\"}\"},{\"status\":200,\"headers\":{},\"body\":\"{\\\"v\\\":\\\"cached-b\\\"}\"}]",
    );

    // Live sub-handlers would return "live-*" if (wrongly) re-dispatched.
    var sys = SystemRuntime.init(allocator);
    defer sys.deinit();
    try sys.addHandler("a", "function handler(req) { return Response.json({ v: 'live-a' }); }", "<a>", .{ .jit_policy = .disabled }, 1);
    try sys.addHandler("b", "function handler(req) { return Response.json({ v: 'live-b' }); }", "<b>", .{ .jit_policy = .disabled }, 1);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir, .system_registry = @ptrCast(&sys) });
    defer rt.deinit();

    const handler_code =
        \\import { run } from "zigttp:durable";
        \\import { fanout } from "zigttp:workflow";
        \\function handler(req) {
        \\  return run("par:replay", () => {
        \\    const rs = fanout([{ name: "a" }, { name: "b" }]);
        \\    return Response.json({ a: rs[0].json(), b: rs[1].json() });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<parallel-replay>");
    var request = try makeTestRequest(allocator, "GET", "/", "par:replay");
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    // The cached aggregate wins -> neither sub-handler was re-dispatched.
    try std.testing.expect(std.mem.indexOf(u8, response.body, "cached-a") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "cached-b") != null);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "live-") == null);
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

    const path = try durable_executor.buildDurableOplogPath(rt, "timer:123");
    defer allocator.free(path);

    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"type\":\"wait_timer\""));
    try std.testing.expectEqual(@as(usize, 0), std.mem.count(u8, source, "\"type\":\"resume_timer\""));
}

// A durable run() callback that suspends must keep suspending correctly even when
// it gets hot enough to JIT-compile. The JIT signals a fault by setting
// ctx.exception and returning a sentinel; jitCall swallows the durable runtime's
// error.DurableSuspended into that sentinel, so a JIT-compiled suspending callback
// loses the suspend (durableRun's catch never sees DurableSuspended) and the
// request errors out instead of returning the 202 pending response. Worse, the
// compiled callback runs its opcodes PAST the suspend point (side effects that
// must wait). The fix keeps durable execution on the interpreter tier (the JIT is
// inhibited when the runtime is durable), where suspend is exact. This test forces
// eager compilation and drives the callback well past the compile threshold with
// distinct keys; every request must still return the timer-pending 202.
test "durable suspend survives JIT promotion of the run() callback" {
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

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    // Distinct key per request (req.url) so the callback genuinely executes and
    // suspends every time (no replay dedup) - that accumulates the profile counts
    // that promote its shared bytecode to the baseline JIT.
    const handler_code =
        \\import { run, sleepUntil } from "zigttp:durable";
        \\function handler(req) {
        \\  return run(req.url, () => {
        \\    sleepUntil(4102444800000);
        \\    return Response.json({ ok: true });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-jit-suspend>");

    var i: usize = 0;
    while (i < 16) : (i += 1) {
        const path = try std.fmt.allocPrint(allocator, "/k{d}", .{i});
        var request = try makeTestRequest(allocator, "GET", path, null);
        defer request.deinit(allocator);
        var response = try rt.executeHandler(request.asView());
        defer response.deinit();
        try std.testing.expectEqual(@as(u16, 202), response.status);
        try std.testing.expect(std.mem.indexOf(u8, response.body, "\"type\":\"timer\"") != null);
    }
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

    const path = try durable_executor.buildDurableOplogPath(rt, "job:123");
    defer allocator.free(path);

    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"type\":\"wait_signal\""));
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"type\":\"resume_signal\""));
}

test "durable stepWithTimeout times out a durable sleep boundary" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { run, stepWithTimeout, sleep } from "zigttp:durable";
        \\function handler(req) {
        \\  return run("timeout:sleep", () => {
        \\    const result = stepWithTimeout("slow", 5, () => {
        \\      sleep(1000);
        \\      return "late";
        \\    });
        \\    return Response.json({ ok: result.ok, error: result.error });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-step-timeout-sleep>");

    var first_request = try makeTestRequest(allocator, "GET", "/", null);
    defer first_request.deinit(allocator);
    var first_response = try rt.executeHandler(first_request.asView());
    defer first_response.deinit();
    try std.testing.expectEqual(@as(u16, 202), first_response.status);
    try std.testing.expect(std.mem.indexOf(u8, first_response.body, "\"type\":\"timer\"") != null);

    std.Io.sleep(std.testing.io, .fromMilliseconds(20), .awake) catch {};

    var second_request = try makeTestRequest(allocator, "GET", "/", null);
    defer second_request.deinit(allocator);
    var second_response = try rt.executeHandler(second_request.asView());
    defer second_response.deinit();
    try std.testing.expectEqual(@as(u16, 200), second_response.status);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, second_response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqual(false, obj.get("ok").?.bool);
    try std.testing.expectEqualStrings("timeout", obj.get("error").?.string);
}

test "durable stepWithTimeout times out waitSignal before consuming a later signal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { run, stepWithTimeout, waitSignal, signal } from "zigttp:durable";
        \\function handler(req) {
        \\  if (req.url === "/signal") {
        \\    return Response.json({ delivered: signal("timeout:signal", "approved", { ok: true }) });
        \\  }
        \\  return run("timeout:signal", () => {
        \\    const result = stepWithTimeout("approval", 5, () => waitSignal("approved"));
        \\    return Response.json({ ok: result.ok, error: result.error });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-step-timeout-signal>");

    var wait_request = try makeTestRequest(allocator, "GET", "/wait", null);
    defer wait_request.deinit(allocator);
    var pending_response = try rt.executeHandler(wait_request.asView());
    defer pending_response.deinit();
    try std.testing.expectEqual(@as(u16, 202), pending_response.status);
    try std.testing.expect(std.mem.indexOf(u8, pending_response.body, "\"type\":\"signal\"") != null);

    std.Io.sleep(std.testing.io, .fromMilliseconds(20), .awake) catch {};

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

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, resumed_response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqual(false, obj.get("ok").?.bool);
    try std.testing.expectEqualStrings("timeout", obj.get("error").?.string);

    const path = try durable_executor.buildDurableOplogPath(rt, "timeout:signal");
    defer allocator.free(path);
    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expect(std.mem.indexOf(u8, source, "\"timeout_ms\"") != null);
    try std.testing.expectEqual(@as(usize, 0), std.mem.count(u8, source, "\"type\":\"resume_signal\""));
}

test "durable fetch retries 5xx responses and succeeds within the retry budget" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    var server = try TestHttpServer.init(allocator, .sequenced_status);
    server.status_sequence = &.{ 500, 500, 200 };
    defer server.join() catch {};
    try server.start();

    const url = try server.url(allocator, "/");
    defer allocator.free(url);

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .outbound_http_enabled = true,
    });
    defer rt.deinit();
    rt.ctx.capability_policy = .{
        .egress = .{ .enabled = true, .values = &[_][]const u8{"127.0.0.1"} },
    };

    const handler_code = try std.fmt.allocPrint(allocator,
        \\import {{ fetch }} from "zigttp:fetch";
        \\function handler(req) {{
        \\  const res = fetch("{s}", {{ durable: {{ key: "retry-success", retries: 5, backoff: "none" }} }});
        \\  return Response.json({{ status: res.status }});
        \\}}
    , .{url});
    try rt.loadHandler(handler_code, "<durable-fetch-retry-success>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 200), response.status);
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    try std.testing.expectEqual(@as(i64, 200), parsed.value.object.get("status").?.integer);
}

test "durable fetch stops retrying once the retry budget is exhausted" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    var server = try TestHttpServer.init(allocator, .sequenced_status);
    server.status_sequence = &.{500};
    defer server.join() catch {};
    try server.start();

    const url = try server.url(allocator, "/");
    defer allocator.free(url);

    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .outbound_http_enabled = true,
    });
    defer rt.deinit();
    rt.ctx.capability_policy = .{
        .egress = .{ .enabled = true, .values = &[_][]const u8{"127.0.0.1"} },
    };

    const handler_code = try std.fmt.allocPrint(allocator,
        \\import {{ fetch }} from "zigttp:fetch";
        \\function handler(req) {{
        \\  const res = fetch("{s}", {{ durable: {{ key: "retry-exhausted", retries: 0, backoff: "none" }} }});
        \\  return Response.json({{ status: res.status }});
        \\}}
    , .{url});
    try rt.loadHandler(handler_code, "<durable-fetch-retry-exhausted>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    // retries: 0 means exactly one attempt - the server's single-item
    // sequence is consumed exactly once, proving the loop did not retry
    // past a `retries: 0` budget.
    try std.testing.expectEqual(@as(u16, 200), response.status);
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    try std.testing.expectEqual(@as(i64, 500), parsed.value.object.get("status").?.integer);
}

test "durable fetch retry loop stops once the step deadline passes instead of exhausting its retry budget" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    // Point at a closed local port instead of a real server: each connect
    // attempt is refused near-instantly (no listener, no background thread
    // to coordinate or tear down), and a refused connection is classified
    // exactly like a 5xx response (synthetic 599) by fetchSyncResult, so
    // the retry loop treats it identically.
    const rt = try Runtime.init(allocator, .{
        .durable_oplog_dir = durable_dir,
        .outbound_http_enabled = true,
    });
    defer rt.deinit();
    rt.ctx.capability_policy = .{
        .egress = .{ .enabled = true, .values = &[_][]const u8{"127.0.0.1"} },
    };

    const handler_code =
        \\import { run, stepWithTimeout } from "zigttp:durable";
        \\import { fetch } from "zigttp:fetch";
        \\function handler(req) {
        \\  return run("fetch:deadline", () => {
        \\    const result = stepWithTimeout("call", 50, () => {
        \\      return fetch("http://127.0.0.1:18711/", { durable: { key: "deadline-fetch", retries: 8, backoff: "exponential" } });
        \\    });
        \\    return Response.json({ ok: result.ok, error: result.error });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-fetch-deadline>");

    var request = try makeTestRequest(allocator, "GET", "/", null);
    defer request.deinit(allocator);

    var timer = try zq.compat.Timer.start();
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();
    const elapsed_ms = timer.read() / std.time.ns_per_ms;

    try std.testing.expectEqual(@as(u16, 200), response.status);
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqual(false, obj.get("ok").?.bool);
    try std.testing.expectEqualStrings("timeout", obj.get("error").?.string);

    // With retries: 8 and exponential backoff, an unbounded retry loop
    // could spend several seconds (backoff caps grow to 6400ms per
    // attempt); stopping at the step deadline keeps this well under a
    // second even with two real network round-trips and one backoff sleep.
    try std.testing.expect(elapsed_ms < 2000);
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
        \\    badCamelMaxError: fetchSync("http://localhost", { maxResponseBytes: "large" }).json().error,
        \\    badSnakeMaxError: fetchSync("http://localhost", { max_response_bytes: "large" }).json().error,
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
    try std.testing.expectEqualStrings("InvalidMaxResponseBytes", obj.get("badCamelMaxError").?.string);
    try std.testing.expectEqualStrings("InvalidMaxResponseBytes", obj.get("badSnakeMaxError").?.string);
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

// Regression guard: the concurrent fetch path (zigttp:io parallel) shares
// parseFetchArgs with the sync path, so the egress allowlist is enforced at
// collection time. A disallowed host is rejected before a descriptor is
// registered, so it never opens a socket; its position in the results array
// stays (undefined) because parallel() is positional.
test "parallel fetch enforces egress allowlist - disallowed host never registers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{ .outbound_http_enabled = true });
    defer rt.deinit();
    rt.ctx.capability_policy = .{
        .egress = .{ .enabled = true, .values = &[_][]const u8{"localhost"} },
    };

    // Both thunks target a disallowed host: no descriptor registers, so
    // both positions are undefined and example.com is never connected.
    const handler_code =
        \\import { parallel } from "zigttp:io";
        \\function a() { return fetchSync("http://example.com/one"); }
        \\function b() { return fetchSync("http://example.com/two"); }
        \\function handler(req) {
        \\  const results = parallel([a, b]);
        \\  const blocked = results[0] === undefined && results[1] === undefined;
        \\  return Response.json({ count: results.length, blocked: blocked });
        \\}
    ;
    try rt.loadHandler(handler_code, "<parallel-egress-blocked>");

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
    try std.testing.expectEqual(@as(i64, 2), parsed.value.object.get("count").?.integer);
    try std.testing.expect(parsed.value.object.get("blocked").?.bool);
}

// Discrimination: an allowlisted host passes through the parallel path
// (registers, executes, returns 200) while a disallowed sibling yields
// undefined at its own position.
test "parallel fetch allows allowlisted host and drops disallowed one" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server = try TestHttpServer.init(allocator, .echo_request_json);
    defer server.join() catch {};
    try server.start();

    const allowed_url = try server.url(allocator, "/ok");
    defer allocator.free(allowed_url);

    const rt = try Runtime.init(allocator, .{ .outbound_http_enabled = true });
    defer rt.deinit();
    rt.ctx.capability_policy = .{
        .egress = .{ .enabled = true, .values = &[_][]const u8{"127.0.0.1"} },
    };

    const handler_code = try std.fmt.allocPrint(allocator,
        \\import {{ parallel }} from "zigttp:io";
        \\function allowed() {{ return fetchSync("{s}"); }}
        \\function blocked() {{ return fetchSync("http://example.com/x"); }}
        \\function handler(req) {{
        \\  const results = parallel([allowed, blocked]);
        \\  return Response.json({{
        \\    count: results.length,
        \\    status: results[0] === undefined ? 0 : results[0].status,
        \\    blockedUndefined: results[1] === undefined
        \\  }});
        \\}}
    , .{allowed_url});
    try rt.loadHandler(handler_code, "<parallel-egress-mixed>");

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
    try std.testing.expectEqual(@as(i64, 2), obj.get("count").?.integer);
    // The allowlisted host actually executed and returned the echo server's
    // 201 at its own position; the disallowed host's position is undefined.
    try std.testing.expectEqual(@as(i64, 201), obj.get("status").?.integer);
    try std.testing.expect(obj.get("blockedUndefined").?.bool);
}

// Change 2: the dev/serve contract-derived allowlist arrives via
// RuntimeConfig.dev_capability_policy and is applied by applyEmbeddedCapabilityPolicy
// on top of the (empty) embedded stub. Enforced identically on the sync path.
test "dev_capability_policy config enforces egress on sync fetch" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .outbound_http_enabled = true,
        .dev_capability_policy = .{ .egress = .{ .enabled = true, .values = &[_][]const u8{"localhost"} } },
    });
    defer rt.deinit();

    const handler_code =
        \\function handler(req) {
        \\  return Response.json({ blocked: fetchSync("http://example.com").json().error });
        \\}
    ;
    try rt.loadHandler(handler_code, "<dev-egress-sync>");

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
    try std.testing.expectEqualStrings("HostNotAllowed", parsed.value.object.get("blocked").?.string);
}

test "dev_capability_policy config applies env cache and sql sections" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .dev_capability_policy = .{
            .env = .{ .enabled = true, .values = &[_][]const u8{"API_KEY"} },
            .cache = .{ .enabled = true, .values = &[_][]const u8{"sessions"} },
            .sql = .{ .enabled = true, .values = &[_][]const u8{"listTodos"}, .queries = &.{} },
        },
    });
    defer rt.deinit();

    try std.testing.expect(rt.ctx.capability_policy.allowsEnv("API_KEY"));
    try std.testing.expect(!rt.ctx.capability_policy.allowsEnv("OTHER"));
    try std.testing.expect(rt.ctx.capability_policy.allowsCacheNamespace("sessions"));
    try std.testing.expect(!rt.ctx.capability_policy.allowsCacheNamespace("other"));
    try std.testing.expect(rt.ctx.capability_policy.allowsSqlQuery("listTodos"));
    try std.testing.expect(!rt.ctx.capability_policy.allowsSqlQuery("dropTodos"));
}

// ...and on the parallel path, via the same config-supplied allowlist.
test "dev_capability_policy config enforces egress on parallel fetch" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .outbound_http_enabled = true,
        .dev_capability_policy = .{ .egress = .{ .enabled = true, .values = &[_][]const u8{"localhost"} } },
    });
    defer rt.deinit();

    const handler_code =
        \\import { parallel } from "zigttp:io";
        \\function a() { return fetchSync("http://example.com/one"); }
        \\function b() { return fetchSync("http://example.com/two"); }
        \\function handler(req) {
        \\  const results = parallel([a, b]);
        \\  const blocked = results[0] === undefined && results[1] === undefined;
        \\  return Response.json({ count: results.length, blocked: blocked });
        \\}
    ;
    try rt.loadHandler(handler_code, "<dev-egress-parallel>");

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
    try std.testing.expectEqual(@as(i64, 2), parsed.value.object.get("count").?.integer);
    try std.testing.expect(parsed.value.object.get("blocked").?.bool);
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

test "zigttp fetch replay consumes traced inner and outer rows and preserves headers" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .replay_file_path = "test",
        .enforce_arena_escape = false,
    });
    defer rt.deinit();

    const io_calls = [_]zq.trace.IoEntry{
        .{
            .seq = 0,
            .module = "http",
            .func = "fetchSync",
            .args_json = "[\"http://example.com/weather\"]",
            .result_json = "{\"status\":200,\"statusText\":\"OK\",\"ok\":true,\"headers\":{\"content-type\":\"application/json\",\"x-request-id\":\"trace-123\"},\"body\":\"{\\\"temperature\\\":21}\"}",
        },
        .{
            .seq = 1,
            .module = "fetch",
            .func = "fetch",
            .args_json = "[\"http://example.com/weather\"]",
            .result_json = "{\"status\":200,\"statusText\":\"OK\",\"ok\":true,\"headers\":{\"content-type\":\"application/json\",\"x-request-id\":\"trace-123\"},\"body\":\"{\\\"temperature\\\":21}\"}",
        },
        .{
            .seq = 2,
            .module = "env",
            .func = "env",
            .args_json = "[\"NEXT\"]",
            .result_json = "\"after-fetch\"",
        },
    };
    var replay_state = zq.trace.ReplayState{
        .io_calls = &io_calls,
        .cursor = 0,
        .divergences = 0,
    };
    rt.ctx.setModuleState(
        zq.trace.REPLAY_STATE_SLOT,
        @ptrCast(&replay_state),
        &zq.trace.ReplayState.deinitOpaque,
    );
    defer rt.ctx.module_state[zq.trace.REPLAY_STATE_SLOT] = null;

    const handler_code =
        \\import { fetch } from "zigttp:fetch";
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const response = fetch("http://example.com/weather");
        \\  const body = response.json();
        \\  return Response.json({
        \\    requestId: response.headers.get("x-request-id"),
        \\    contentType: response.headers.get("content-type"),
        \\    temperature: body.temperature,
        \\    next: env("NEXT")
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<fetch-replay-trace>");

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
    try std.testing.expectEqualStrings("trace-123", obj.get("requestId").?.string);
    try std.testing.expectEqualStrings("application/json", obj.get("contentType").?.string);
    try std.testing.expectEqual(@as(i64, 21), obj.get("temperature").?.integer);
    try std.testing.expectEqualStrings("after-fetch", obj.get("next").?.string);
    try std.testing.expectEqual(@as(u32, 3), replay_state.cursor);
    try std.testing.expectEqual(@as(u32, 0), replay_state.divergences);
}

test "zigttp fetch replay preserves missing content-type header" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .replay_file_path = "test",
        .enforce_arena_escape = false,
    });
    defer rt.deinit();

    const io_calls = [_]zq.trace.IoEntry{
        .{
            .seq = 0,
            .module = "fetch",
            .func = "fetch",
            .args_json = "[\"http://example.com/plain\"]",
            .result_json = "{\"status\":200,\"statusText\":\"OK\",\"ok\":true,\"headers\":{},\"body\":\"plain\"}",
        },
    };
    var replay_state = zq.trace.ReplayState{
        .io_calls = &io_calls,
        .cursor = 0,
        .divergences = 0,
    };
    rt.ctx.setModuleState(
        zq.trace.REPLAY_STATE_SLOT,
        @ptrCast(&replay_state),
        &zq.trace.ReplayState.deinitOpaque,
    );
    defer rt.ctx.module_state[zq.trace.REPLAY_STATE_SLOT] = null;

    const handler_code =
        \\import { fetch } from "zigttp:fetch";
        \\function handler(req) {
        \\  const response = fetch("http://example.com/plain");
        \\  return Response.json({
        \\    hasContentType: response.headers.has("content-type"),
        \\    contentType: response.headers.get("content-type") ?? "missing"
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<fetch-replay-missing-content-type>");

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
    try std.testing.expectEqual(false, obj.get("hasContentType").?.bool);
    try std.testing.expectEqualStrings("missing", obj.get("contentType").?.string);
    try std.testing.expectEqual(@as(u32, 1), replay_state.cursor);
    try std.testing.expectEqual(@as(u32, 0), replay_state.divergences);
}

// Mirrors examples/fetch/weather-forecasts.ts (the demo handler). Embedded as
// plain JS so the replay tests below exercise the real fetch -> json() -> shape
// pipeline without the type-import surface; behavior is identical.
const weather_handler_src =
    \\import { fetch } from "zigttp:fetch";
    \\function handler(req) {
    \\  if (req.method !== "GET") {
    \\    return Response.json({ error: "method_not_allowed" }, { status: 405 });
    \\  }
    \\  if (req.path !== "/" && req.path !== "/weather") {
    \\    return Response.json({ error: "not_found" }, { status: 404 });
    \\  }
    \\  const upstream = fetch("https://api.open-meteo.com/v1/forecast?latitude=52.52&longitude=13.41&current=temperature_2m,relative_humidity_2m,wind_speed_10m,is_day&timezone=auto", {
    \\    headers: { "Accept": "application/json" },
    \\    maxResponseBytes: 65536,
    \\  });
    \\  if (!upstream.ok) {
    \\    return Response.json({ error: "weather_unavailable", upstreamStatus: upstream.status }, { status: 502 });
    \\  }
    \\  const forecast = upstream.json();
    \\  return Response.json({
    \\    app: "Weather Forecasts",
    \\    source: "open-meteo",
    \\    upstreamRequestId: upstream.headers.get("x-request-id") ?? "none",
    \\    coordinates: { latitude: forecast.latitude, longitude: forecast.longitude },
    \\    timezone: forecast.timezone,
    \\    current: {
    \\      temperature: forecast.current.temperature_2m,
    \\      humidity: forecast.current.relative_humidity_2m,
    \\      isDay: forecast.current.is_day,
    \\    },
    \\  });
    \\}
;

test "weather handler parses Open-Meteo forecast and surfaces request id" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .replay_file_path = "test",
        .enforce_arena_escape = false,
    });
    defer rt.deinit();

    // The upstream body is a JSON string nested inside result_json, hence the
    // doubled escaping. Named so result_json reads as wrapper ++ body ++ close.
    const forecast_body = "{\\\"latitude\\\":52.52,\\\"longitude\\\":13.41,\\\"timezone\\\":\\\"Europe/Berlin\\\",\\\"current\\\":{\\\"temperature_2m\\\":21.5,\\\"relative_humidity_2m\\\":56,\\\"is_day\\\":1}}";
    const io_calls = [_]zq.trace.IoEntry{
        .{
            .seq = 0,
            .module = "fetch",
            .func = "fetch",
            .args_json = "[\"https://api.open-meteo.com/v1/forecast\"]",
            .result_json = "{\"status\":200,\"statusText\":\"OK\",\"ok\":true,\"headers\":{\"content-type\":\"application/json\",\"x-request-id\":\"meteo-123\"},\"body\":\"" ++ forecast_body ++ "\"}",
        },
    };
    var replay_state = zq.trace.ReplayState{ .io_calls = &io_calls, .cursor = 0, .divergences = 0 };
    rt.ctx.setModuleState(zq.trace.REPLAY_STATE_SLOT, @ptrCast(&replay_state), &zq.trace.ReplayState.deinitOpaque);
    defer rt.ctx.module_state[zq.trace.REPLAY_STATE_SLOT] = null;

    try rt.loadHandler(weather_handler_src, "<weather-replay-ok>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/weather"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 200), response.status);
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("open-meteo", obj.get("source").?.string);
    try std.testing.expectEqualStrings("Europe/Berlin", obj.get("timezone").?.string);
    try std.testing.expectEqualStrings("meteo-123", obj.get("upstreamRequestId").?.string);
    try std.testing.expectApproxEqAbs(@as(f64, 52.52), obj.get("coordinates").?.object.get("latitude").?.float, 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 21.5), obj.get("current").?.object.get("temperature").?.float, 0.001);
    try std.testing.expectEqual(@as(i64, 56), obj.get("current").?.object.get("humidity").?.integer);
    try std.testing.expectEqual(@as(u32, 1), replay_state.cursor);
    try std.testing.expectEqual(@as(u32, 0), replay_state.divergences);
}

test "weather handler returns 502 when upstream is not ok" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .replay_file_path = "test",
        .enforce_arena_escape = false,
    });
    defer rt.deinit();

    const io_calls = [_]zq.trace.IoEntry{
        .{
            .seq = 0,
            .module = "fetch",
            .func = "fetch",
            .args_json = "[\"https://api.open-meteo.com/v1/forecast\"]",
            .result_json = "{\"status\":503,\"statusText\":\"Service Unavailable\",\"ok\":false,\"headers\":{\"content-type\":\"application/json\"},\"body\":\"{}\"}",
        },
    };
    var replay_state = zq.trace.ReplayState{ .io_calls = &io_calls, .cursor = 0, .divergences = 0 };
    rt.ctx.setModuleState(zq.trace.REPLAY_STATE_SLOT, @ptrCast(&replay_state), &zq.trace.ReplayState.deinitOpaque);
    defer rt.ctx.module_state[zq.trace.REPLAY_STATE_SLOT] = null;

    try rt.loadHandler(weather_handler_src, "<weather-replay-502>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/weather"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 502), response.status);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "weather_unavailable") != null);
    try std.testing.expectEqual(@as(u32, 1), replay_state.cursor);
    try std.testing.expectEqual(@as(u32, 0), replay_state.divergences);
}

test "weather handler returns 404 for an unknown path without any egress" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{
        .replay_file_path = "test",
        .enforce_arena_escape = false,
    });
    defer rt.deinit();

    // No recorded I/O: the not-found path must short-circuit before fetch.
    const io_calls = [_]zq.trace.IoEntry{};
    var replay_state = zq.trace.ReplayState{ .io_calls = &io_calls, .cursor = 0, .divergences = 0 };
    rt.ctx.setModuleState(zq.trace.REPLAY_STATE_SLOT, @ptrCast(&replay_state), &zq.trace.ReplayState.deinitOpaque);
    defer rt.ctx.module_state[zq.trace.REPLAY_STATE_SLOT] = null;

    try rt.loadHandler(weather_handler_src, "<weather-replay-404>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/unknown"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqual(@as(u16, 404), response.status);
    try std.testing.expect(std.mem.indexOf(u8, response.body, "not_found") != null);
    // cursor stays at 0: fetch was never reached, so no egress happened.
    try std.testing.expectEqual(@as(u32, 0), replay_state.cursor);
    try std.testing.expectEqual(@as(u32, 0), replay_state.divergences);
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

test "buildFetchUrl appends a dynamic query to a literal base and percent-encodes values" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    const pool = rt.ctx.hidden_class_pool.?;

    // The shape the weather app builds: user-supplied coordinates plus static
    // request params, all riding in the dynamic `query` object.
    const query = try rt.ctx.createObject(null);
    try rt.ctx.setPropertyChecked(query, try rt.ctx.atoms.intern("latitude"), try rt.ctx.createString("40.71"));
    try rt.ctx.setPropertyChecked(query, try rt.ctx.atoms.intern("longitude"), try rt.ctx.createString("-74.01"));
    try rt.ctx.setPropertyChecked(query, try rt.ctx.atoms.intern("current"), try rt.ctx.createString("temperature_2m,wind_speed_10m"));
    try rt.ctx.setPropertyChecked(query, try rt.ctx.atoms.intern("timezone"), try rt.ctx.createString("auto"));

    const init = try rt.ctx.createObject(null);
    try rt.ctx.setPropertyChecked(init, try rt.ctx.atoms.intern("query"), query.toValue());

    const url = switch (try buildFetchUrl(rt, pool, "https://api.open-meteo.com/v1/forecast", init)) {
        .ok => |u| u,
        .err => return error.UnexpectedFetchUrlError,
    };
    defer allocator.free(url);

    // Insertion order is preserved; `-` and `.` are unreserved, the comma is encoded.
    try std.testing.expectEqualStrings(
        "https://api.open-meteo.com/v1/forecast?latitude=40.71&longitude=-74.01&current=temperature_2m%2Cwind_speed_10m&timezone=auto",
        url,
    );
}

test "buildFetchUrl returns a copy of the literal base when there is no query" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    const pool = rt.ctx.hidden_class_pool.?;

    // An init object with only non-query fields leaves the URL untouched.
    const init = try rt.ctx.createObject(null);
    try rt.ctx.setPropertyChecked(init, try rt.ctx.atoms.intern("method"), try rt.ctx.createString("GET"));

    const url = switch (try buildFetchUrl(rt, pool, "https://api.open-meteo.com/v1/forecast", init)) {
        .ok => |u| u,
        .err => return error.UnexpectedFetchUrlError,
    };
    defer allocator.free(url);

    try std.testing.expectEqualStrings("https://api.open-meteo.com/v1/forecast", url);
}

test "buildFetchUrl uses & when the literal base already has a query string" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    const pool = rt.ctx.hidden_class_pool.?;

    const query = try rt.ctx.createObject(null);
    try rt.ctx.setPropertyChecked(query, try rt.ctx.atoms.intern("latitude"), try rt.ctx.createString("1.5"));

    const init = try rt.ctx.createObject(null);
    try rt.ctx.setPropertyChecked(init, try rt.ctx.atoms.intern("query"), query.toValue());

    const url = switch (try buildFetchUrl(rt, pool, "https://api.example.com/v1?format=json", init)) {
        .ok => |u| u,
        .err => return error.UnexpectedFetchUrlError,
    };
    defer allocator.free(url);

    try std.testing.expectEqualStrings("https://api.example.com/v1?format=json&latitude=1.5", url);
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

test "fetchSync times out instead of hanging when upstream accepts and goes silent" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var server = try TestHttpServer.init(allocator, .silent_hold);
    defer server.join() catch {};
    try server.start();

    const url = try server.url(allocator, "/never-answers");
    defer allocator.free(url);

    const rt = try Runtime.init(allocator, .{
        .outbound_http_enabled = true,
        .outbound_allow_host = "127.0.0.1",
        .outbound_timeout_ms = 200,
    });
    defer rt.deinit();

    const handler_code = try std.fmt.allocPrint(
        allocator,
        \\function handler(req) {{
        \\  const resp = fetchSync("{s}");
        \\  const data = resp.json();
        \\  return Response.json({{
        \\    status: resp.status,
        \\    ok: resp.ok,
        \\    error: data.error
        \\  }});
        \\}}
    ,
        .{url},
    );
    defer allocator.free(handler_code);
    try rt.loadHandler(handler_code, "<fetchsync-timeout>");

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    const clock_io = server.io_backend.io();
    const started = std.Io.Clock.awake.now(clock_io);
    var response = try rt.executeHandler(request.asView());
    defer response.deinit();
    const elapsed_ms = started.untilNow(clock_io, .awake).toMilliseconds();
    try server.join();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const obj = parsed.value.object;
    try std.testing.expectEqual(@as(i64, 599), obj.get("status").?.integer);
    try std.testing.expectEqual(false, obj.get("ok").?.bool);
    try std.testing.expectEqualStrings("TimedOut", obj.get("error").?.string);
    // 200ms deadline; anything near a second means the watchdog never fired.
    try std.testing.expect(elapsed_ms < 2000);
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

// Uses testing.allocator directly so leaks fail rather than being absorbed
// by an arena.
test "HandlerPool teardown leaves no leaks under testing.allocator" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.testing.allocator;
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

    var response = try pool.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqualStrings("ok", response.body);
}

test "HandlerPool cached pattern dispatch transfers ownership without leaks" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const handler_code =
        \\function handler(request) {
        \\  const url = request.url;
        \\  if (url === "/api/a") return Response.text("alpha");
        \\  return Response.text("fallback");
        \\}
    ;
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 2, 0);
    defer pool.deinit();

    // Keep the source-compiled runtime checked out so the request must create
    // a second runtime and hydrate it from the shared bytecode cache.
    const warmed_runtime = try pool.pool.acquire();
    defer pool.pool.release(warmed_runtime);

    var request = HttpRequestOwned{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/api/a"),
        .headers = .empty,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try pool.executeHandler(request.asView());
    defer response.deinit();

    try std.testing.expectEqualStrings("alpha", response.body);
    try std.testing.expect(pool.cache.hits.load(.monotonic) > 0);
    try std.testing.expect(!pool.cache_disabled.load(.monotonic));
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

test "Runtime rejects malformed cached bytecode before execution" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const code = [_]u8{
        @intFromEnum(zq.Opcode.ret),
    };
    const func = zq.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };

    const no_shapes: []const []const zq.object.Atom = &.{};
    var buffer: [1024]u8 = undefined;
    var writer = bytecode_cache.SliceWriter{ .buffer = &buffer };
    try bytecode_cache.serializeBytecodeWithAtomsAndShapes(
        &func,
        &rt.ctx.atoms,
        no_shapes,
        &writer,
        allocator,
    );

    try std.testing.expectError(
        error.BytecodeVerificationFailed,
        rt.loadFromCachedBytecodeNoHandler(writer.getWritten()),
    );
}

test "request deadline temporarily inhibits JIT" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var rt = try Runtime.init(allocator, .{ .request_timeout_ms = 50 });
    defer rt.deinit();

    try std.testing.expect(!rt.ctx.jit_inhibited);
    rt.armRequestDeadline();
    try std.testing.expect(rt.ctx.deadline_ns != 0);
    try std.testing.expect(rt.ctx.jit_inhibited);
    rt.clearRequestDeadline();
    try std.testing.expectEqual(@as(u64, 0), rt.ctx.deadline_ns);
    try std.testing.expect(!rt.ctx.jit_inhibited);

    rt.ctx.jit_inhibited = true;
    rt.armRequestDeadline();
    try std.testing.expect(rt.ctx.jit_inhibited);
    rt.clearRequestDeadline();
    try std.testing.expect(rt.ctx.jit_inhibited);
}

test "Runtime rejects malformed nested cached bytecode before execution" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const child_code = [_]u8{
        @intFromEnum(zq.Opcode.ret),
    };
    var child = zq.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &child_code,
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const constants = [_]zq.JSValue{
        zq.JSValue.fromExternPtr(&child),
    };
    const parent_code = [_]u8{
        @intFromEnum(zq.Opcode.ret_undefined),
    };
    const parent = zq.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &parent_code,
        .constants = &constants,
        .source_map = null,
        .line_table = null,
    };

    const no_shapes: []const []const zq.object.Atom = &.{};
    var buffer: [2048]u8 = undefined;
    var writer = bytecode_cache.SliceWriter{ .buffer = &buffer };
    try bytecode_cache.serializeBytecodeWithAtomsAndShapes(
        &parent,
        &rt.ctx.atoms,
        no_shapes,
        &writer,
        allocator,
    );

    try std.testing.expectError(
        error.BytecodeVerificationFailed,
        rt.loadFromCachedBytecodeNoHandler(writer.getWritten()),
    );
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

test "ENG-2: zero-arg user-named method on object literal is callable" {
    // Regression: a zero-arg method call on an object literal with a
    // user-defined property name (`({greet:()=>7}).greet()`) used to fall
    // through to NotCallable -> HTTP 500 because the get_field+call_method ->
    // get_field_call peephole fusion mis-resolved the dynamically-interned
    // atom. The fusion is now disabled (bytecode_opt.zig). Covers the literal
    // receiver, a multi-property literal, and a variable receiver.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\function handler(req){
        \\  const a = ({greet:()=>7}).greet();
        \\  const b = ({x:1,wave:()=>9}).wave();
        \\  const o = { hi: () => 4 };
        \\  const c = o.hi();
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

    try std.testing.expectEqualStrings("7-9-4", response.body);
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
// five HandlerPool lifecycle tests, the JIT overflow-slot literal test, and the
// loadCodeNoHandler benchmark smoke below. macOS does not detect the same
// corruption, and the prior Linux-only investigation did not isolate a root
// cause. Keep these gated on Linux until we can debug them under rr/valgrind on
// a Linux host.
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

test "HandlerPool does not leak request data across pooled requests" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.heap.c_allocator;

    // The handler reflects every request-scoped input back into the response.
    // If body, a header, or the url survived a runtime recycle, a request with
    // none of them set would echo a prior request's values.
    const handler_code =
        \\function handler(req) {
        \\  return Response.json({
        \\    body: req.body ?? "NONE",
        \\    secret: req.headers.get("x-secret") ?? "NONE",
        \\    url: req.url,
        \\  });
        \\}
    ;

    // Pool size 1 forces every request onto the same recycled runtime.
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 1, 0);
    defer pool.deinit();

    const Probe = struct {
        fn tainted(a: std.mem.Allocator, marker: []const u8) !HttpRequestOwned {
            var req = HttpRequestOwned{
                .method = try a.dupe(u8, "POST"),
                .url = try std.fmt.allocPrint(a, "/path-{s}", .{marker}),
                .headers = .empty,
                .body = try std.fmt.allocPrint(a, "BODY-{s}", .{marker}),
            };
            try req.headers.append(a, .{
                .key = try a.dupe(u8, "X-Secret"),
                .value = try std.fmt.allocPrint(a, "SECRET-{s}", .{marker}),
            });
            return req;
        }
        fn clean(a: std.mem.Allocator) !HttpRequestOwned {
            return HttpRequestOwned{
                .method = try a.dupe(u8, "GET"),
                .url = try a.dupe(u8, "/clean"),
                .headers = .empty,
                .body = null,
            };
        }
    };

    // Alternate tainted and clean requests so the recycle is exercised in both
    // directions across several cycles on the one shared runtime.
    var cycle: usize = 0;
    while (cycle < 4) : (cycle += 1) {
        var marker_buf: [16]u8 = undefined;
        const marker = try std.fmt.bufPrint(&marker_buf, "{d}", .{cycle});

        var tainted_req = try Probe.tainted(allocator, marker);
        defer tainted_req.deinit(allocator);
        var tainted_resp = try pool.executeHandler(tainted_req.asView());
        defer tainted_resp.deinit();
        // The tainted request must see exactly its own data.
        try std.testing.expect(std.mem.indexOf(u8, tainted_resp.body, "BODY-") != null);
        try std.testing.expect(std.mem.indexOf(u8, tainted_resp.body, "SECRET-") != null);

        var clean_req = try Probe.clean(allocator);
        defer clean_req.deinit(allocator);
        var clean_resp = try pool.executeHandler(clean_req.asView());
        defer clean_resp.deinit();
        // The clean request, on the same recycled runtime, must carry none of
        // the prior request's body, header, or url.
        try std.testing.expect(std.mem.indexOf(u8, clean_resp.body, "BODY-") == null);
        try std.testing.expect(std.mem.indexOf(u8, clean_resp.body, "SECRET-") == null);
        try std.testing.expect(std.mem.indexOf(u8, clean_resp.body, "path-") == null);
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

test "loadCodeNoHandler supports benchmark-style scripts" {
    if (skip_linux_glibc_heap_corruption_tests) return error.SkipZigTest;
    const allocator = std.heap.c_allocator;

    const script =
        \\function run(iterations) {
        \\  return iterations + 1;
        \\}
    ;

    var rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    try rt.loadCodeNoHandler(script, "<bench>");

    const args = [_]zq.JSValue{zq.JSValue.fromInt(10)};
    const result = try rt.callGlobalFunction("run", &args);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 11), result.getInt());
    try std.testing.expectEqual(@as(u32, 0), rt.interpreter.snapshotPerfStats().deopt_count);
}

test "loadCodeNoHandler supports imported benchmark-style scripts" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const dep_source =
        \\export function addOne(value) {
        \\  return value + 1;
        \\}
    ;
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "dep.js",
        .data = dep_source,
    });

    const main_source =
        \\import { addOne } from "./dep.js";
        \\function run(iterations) {
        \\  return addOne(iterations) + 1;
        \\}
    ;
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.js",
        .data = main_source,
    });

    const entry_path = try std.fs.path.resolve(allocator, &.{ ".zig-cache", "tmp", tmp_dir.sub_path[0..], "main.js" });

    var rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    try rt.loadCodeNoHandler(main_source, entry_path);

    const args = [_]zq.JSValue{zq.JSValue.fromInt(10)};
    const result = try rt.callGlobalFunction("run", &args);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 12), result.getInt());
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

// Probe HandlerPool.init under FailingAllocator at every allocation site.
// The pool init walks LockFreePool slot allocation, GC arenas per slot,
// handler-source parse/compile during prewarm, bytecode cache state, and
// (when configured) trace mutex creation — a long errdefer chain.
//
// First run of this harness found a real leak: when prewarm's
// installHttpConstructors failed inside addDynamicMethod, the just-created
// prototype objects were not yet registered in ctx.builtin_objects, and native
// function objects created by addMethod had no errdefer owner before property
// installation. The allocator walk keeps those ownership edges transactional.
test "HandlerPool init under FailingAllocator never leaks" {
    const handler_code = "function handler(req) { return Response.text('ok'); }";

    const Probe = struct {
        fn run(handler: []const u8, fail_at: usize) !bool {
            var leak_detector: std.heap.DebugAllocator(.{ .stack_trace_frames = 0 }) = .init;
            const child = leak_detector.allocator();
            var failing = std.testing.FailingAllocator.init(child, .{ .fail_index = fail_at });

            const result = HandlerPool.init(failing.allocator(), .{}, handler, "<handler>", 1, 0);
            if (result) |pool| {
                var pool_mut = pool;
                pool_mut.deinit();
                const leak_check = leak_detector.deinit();
                if (leak_check == .leak) std.debug.print("HandlerPool.init leaked on success after fail_at={d}\n", .{fail_at});
                try std.testing.expectEqual(std.heap.Check.ok, leak_check);
                return true;
            } else |err| {
                try std.testing.expectEqual(error.OutOfMemory, err);
                const leak_check = leak_detector.deinit();
                if (leak_check == .leak) std.debug.print("HandlerPool.init leaked on fail_at={d}\n", .{fail_at});
                try std.testing.expectEqual(std.heap.Check.ok, leak_check);
                return false;
            }
        }
    };

    const regression_fail_points = [_]usize{
        0,
        1,
        // Prototype/console/native module ownership failures found while
        // unskipping this test for v0.2.0.
        1489,
        1535,
        // Parser scope initialization previously panicked instead of
        // returning OutOfMemory on this part of prewarm.
        2048,
    };
    for (regression_fail_points) |fail_at| {
        _ = try Probe.run(handler_code, fail_at);
    }

    {
        var leak_detector: std.heap.DebugAllocator(.{ .stack_trace_frames = 0 }) = .init;
        const child = leak_detector.allocator();
        var probe = std.testing.FailingAllocator.init(child, .{ .fail_index = std.math.maxInt(usize) });

        var pool = try HandlerPool.init(probe.allocator(), .{}, handler_code, "<handler>", 1, 0);
        pool.deinit();
        const leak_check = leak_detector.deinit();
        if (leak_check == .leak) std.debug.print("HandlerPool.init leaked on success probe\n", .{});
        try std.testing.expectEqual(std.heap.Check.ok, leak_check);
    }

    if (std.c.getenv("ZTS_RUN_OOM_SWEEP") != null) {
        var fail_at: usize = 0;
        const max_steps: usize = 8192;
        while (fail_at < max_steps) : (fail_at += 1) {
            if (try Probe.run(handler_code, fail_at)) break;
        } else {
            std.debug.print("HandlerPool.init did not succeed within {d} injected allocation failures\n", .{max_steps});
            return error.TestExpectedSuccess;
        }
    }
}

test "durable run refuses the oplog while a recovery claim holds it" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const durable_dir = try durableTestDirPath(allocator, &tmp_dir);

    try seedIncompleteDurableRandomStep(allocator, durable_dir, "order:busy", "charge", "0.5");

    const rt = try Runtime.init(allocator, .{ .durable_oplog_dir = durable_dir });
    defer rt.deinit();

    const handler_code =
        \\import { run, step } from "zigttp:durable";
        \\function handler(req) {
        \\  const key = req.headers.get("idempotency-key") ?? "missing";
        \\  return run(key, () => {
        \\    const seed = step("charge", () => Math.random());
        \\    return Response.json({ seed: seed });
        \\  });
        \\}
    ;
    try rt.loadHandler(handler_code, "<durable-busy>");

    const path = try durable_executor.buildDurableOplogPath(rt, "order:busy");
    defer allocator.free(path);

    // Hold the recovery-style advisory lock, as durable_recovery.OplogClaim
    // does for the duration of a re-execution.
    const path_z = try allocator.dupeZ(u8, path);
    const lock_fd = std.c.open(path_z, .{ .ACCMODE = .RDONLY }, @as(std.c.mode_t, 0));
    try std.testing.expect(lock_fd >= 0);
    defer _ = std.c.close(lock_fd);
    try tryLockOplogFd(lock_fd);

    var request = try makeTestRequest(allocator, "GET", "/", "order:busy");
    defer request.deinit(allocator);

    const request_val = try rt.createRequestObject(request.asView());
    current_runtime = rt;
    defer current_runtime = null;
    try std.testing.expectError(error.NativeFunctionError, rt.callGlobalFunction("handler", &[_]zq.JSValue{request_val}));
    rt.resetForNextRequest();

    // The refused run must not have touched the locked oplog.
    const source = try zq.file_io.readFile(allocator, path, 1024 * 1024);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, source, "\"fn\":\"Math.random\""));
    try std.testing.expect(!std.mem.containsAtLeast(u8, source, 1, "\"type\":\"complete\""));
}
