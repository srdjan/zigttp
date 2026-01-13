//! Native Zig Runtime for zigttp
//!
//! Pure Zig implementation using zts - no C dependencies.
//! Designed for FaaS with per-request isolation and fast cold starts.

const std = @import("std");
const ascii = std.ascii;

// Import zts module
const zq = @import("zts");

// Bytecode caching for faster cold starts
const bytecode_cache = zq.bytecode_cache;

// ============================================================================
// Runtime Configuration
// ============================================================================

pub const RuntimeConfig = struct {
    /// Memory limit per context in bytes (0 = no limit)
    memory_limit: usize = 0,

    /// Enable all Deno-compatible APIs
    enable_deno_apis: bool = true,

    /// Enable fetch() API
    enable_fetch: bool = true,

    /// Enable file system APIs
    enable_fs: bool = true,

    /// Enable timers (setTimeout, setInterval)
    enable_timers: bool = true,

    /// Enable network socket APIs
    enable_net: bool = false,

    /// Sandbox mode: restrict file system to specific paths
    sandbox_paths: ?[]const []const u8 = null,

    /// Max execution time per request in milliseconds (0 = no limit)
    max_execution_time_ms: u32 = 30_000,

    /// Enable JSX runtime (h, renderToString, Fragment)
    enable_jsx: bool = true,

    /// Clear user-defined globals between requests (builtins always preserved)
    reset_user_globals: bool = false,

    // Internal zts settings
    /// GC nursery size
    nursery_size: usize = 64 * 1024,
};

// ============================================================================
// HTTP Types (Native Zig)
// ============================================================================

pub const HttpRequest = struct {
    method: []const u8,
    url: []const u8,
    headers: std.ArrayListUnmanaged(HttpHeader),
    body: ?[]const u8,

    pub fn deinit(self: *HttpRequest, allocator: std.mem.Allocator) void {
        allocator.free(self.method);
        allocator.free(self.url);
        if (self.body) |b| allocator.free(b);
        for (self.headers.items) |header| {
            allocator.free(header.key);
            allocator.free(header.value);
        }
        self.headers.deinit(allocator);
    }
};

pub const HttpHeader = struct {
    key: []const u8,
    value: []const u8,
};

pub const ResponseHeader = struct {
    key: []const u8,
    value: []const u8,
    key_owned: bool,
    value_owned: bool,
};

pub const HttpResponse = struct {
    status: u16,
    headers: std.ArrayListUnmanaged(ResponseHeader),
    body: []const u8,
    body_owned: bool,
    body_owner: ?*zq.JSString,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) HttpResponse {
        return .{
            .status = 200,
            .headers = .{},
            .body = "",
            .body_owned = false,
            .body_owner = null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *HttpResponse) void {
        for (self.headers.items) |header| {
            if (header.key_owned) {
                self.allocator.free(header.key);
            }
            if (header.value_owned) {
                self.allocator.free(header.value);
            }
        }
        self.headers.deinit(self.allocator);
        if (self.body.len > 0 and self.body_owned) {
            self.allocator.free(self.body);
        }
        self.body_owner = null;
    }

    pub fn putHeader(self: *HttpResponse, key: []const u8, value: []const u8) !void {
        const key_dup = try self.allocator.dupe(u8, key);
        errdefer self.allocator.free(key_dup);
        const value_dup = try self.allocator.dupe(u8, value);
        errdefer self.allocator.free(value_dup);
        for (self.headers.items) |*header| {
            if (ascii.eqlIgnoreCase(header.key, key)) {
                if (header.key_owned) {
                    self.allocator.free(header.key);
                }
                if (header.value_owned) {
                    self.allocator.free(header.value);
                }
                header.* = .{
                    .key = key_dup,
                    .value = value_dup,
                    .key_owned = true,
                    .value_owned = true,
                };
                return;
            }
        }
        try self.headers.append(self.allocator, .{
            .key = key_dup,
            .value = value_dup,
            .key_owned = true,
            .value_owned = true,
        });
    }

    pub fn putHeaderBorrowed(self: *HttpResponse, key: []const u8, value: []const u8) !void {
        for (self.headers.items) |*header| {
            if (ascii.eqlIgnoreCase(header.key, key)) {
                if (header.key_owned) {
                    self.allocator.free(header.key);
                }
                if (header.value_owned) {
                    self.allocator.free(header.value);
                }
                header.* = .{
                    .key = key,
                    .value = value,
                    .key_owned = false,
                    .value_owned = false,
                };
                return;
            }
        }
        try self.headers.append(self.allocator, .{
            .key = key,
            .value = value,
            .key_owned = false,
            .value_owned = false,
        });
    }

    fn setBodyOwned(self: *HttpResponse, bytes: []const u8) void {
        self.body = bytes;
        self.body_owned = true;
        self.body_owner = null;
    }

    fn setBodyBorrowed(self: *HttpResponse, str: *zq.JSString) void {
        self.body = str.data();
        self.body_owned = false;
        self.body_owner = str;
    }
};

// ============================================================================
// Thread-local Runtime for native function callbacks
// ============================================================================

/// Thread-local reference to current runtime for use in native function callbacks
/// (e.g., renderToString needs to call component functions)
threadlocal var current_runtime: ?*Runtime = null;

// ============================================================================
// Runtime Instance
// ============================================================================

pub const Runtime = struct {
    allocator: std.mem.Allocator,
    ctx: *zq.Context,
    gc_state: *zq.GC,
    heap: *zq.heap.Heap,
    interpreter: zq.Interpreter,
    strings: zq.StringTable,
    handler_atom: ?zq.Atom,
    config: RuntimeConfig,
    owns_resources: bool,
    active_request_id: std.atomic.Value(u64),
    last_request_body_len: usize,

    const Self = @This();

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

        // Install core JS builtins (Array.prototype, Object, Math, JSON, etc.)
        try zq.builtins.initBuiltins(ctx);

        self.* = .{
            .allocator = allocator,
            .ctx = ctx,
            .gc_state = gc_state,
            .heap = heap_state,
            .interpreter = zq.Interpreter.init(ctx),
            .strings = zq.StringTable.init(allocator),
            .handler_atom = null,
            .config = config,
            .owns_resources = true,
            .active_request_id = std.atomic.Value(u64).init(0),
            .last_request_body_len = 0,
        };

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

        self.* = .{
            .allocator = allocator,
            .ctx = pool_rt.ctx,
            .gc_state = pool_rt.gc_state,
            .heap = pool_rt.heap_state,
            .interpreter = zq.Interpreter.init(pool_rt.ctx),
            .strings = zq.StringTable.init(allocator),
            .handler_atom = null,
            .config = config,
            .owns_resources = false,
            .active_request_id = std.atomic.Value(u64).init(0),
            .last_request_body_len = 0,
        };

        // Install core JS builtins (Array.prototype, Object, Math, JSON, etc.)
        try zq.builtins.initBuiltins(pool_rt.ctx);
        try self.installBindings();

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.strings.deinit();
        if (self.owns_resources) {
            self.ctx.deinit();
            self.gc_state.deinit();
            self.heap.deinit();
            self.allocator.destroy(self.gc_state);
            self.allocator.destroy(self.heap);
        }
        self.allocator.destroy(self);
    }

    /// Install native API bindings (console, etc.)
    /// Note: Response is already set up by initBuiltins() with constructor and static methods
    fn installBindings(self: *Self) !void {
        // Use predefined handler atom
        self.handler_atom = zq.Atom.handler;

        // Install console object
        try self.installConsole();

        // Note: Response, h(), renderToString(), Fragment are all set up by initBuiltins()
        // Don't re-register them here as it would overwrite the proper constructor
    }

    fn installConsole(self: *Self) !void {
        const root_class = self.ctx.root_class orelse return error.NoRootClass;

        // Create console object
        const console_obj = try zq.JSObject.create(self.allocator, root_class, null);

        // Add console.log
        const log_atom: zq.Atom = .log;
        const log_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            consoleLog,
            log_atom,
            0,
        );
        try console_obj.setProperty(self.allocator, log_atom, log_func.toValue());

        // Add console.error
        const error_atom = try self.ctx.atoms.intern("error");
        const error_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            consoleError,
            error_atom,
            0,
        );
        try console_obj.setProperty(self.allocator, error_atom, error_func.toValue());

        // Register on global
        try self.ctx.setGlobal(.console, console_obj.toValue());
    }

    // Note: installResponseHelpers and installJsxRuntime removed - these are now handled by initBuiltins()

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
        var p = zq.Parser.init(self.allocator, source_to_parse, &self.strings, &self.ctx.atoms);
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

        // Serialize for caching if buffer provided (includes atoms for true cache hit)
        var serialized: ?[]const u8 = null;
        if (cache_buffer) |buffer| {
            var writer = bytecode_cache.SliceWriter{ .buffer = buffer };
            bytecode_cache.serializeBytecodeWithAtoms(&func, &self.ctx.atoms, &writer, self.allocator) catch {
                // Serialization failed (buffer too small), continue without caching
            };
            if (writer.pos > 0) {
                serialized = writer.getWritten();
            }
        }

        // Execute the compiled code to define functions
        _ = try self.interpreter.run(&func);

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

    /// Load from cached serialized bytecode (Phase 1c: true cache hit with atoms)
    pub fn loadFromCachedBytecode(self: *Self, cached_data: []const u8) !void {
        var reader = bytecode_cache.SliceReader{ .data = cached_data };

        // Deserialize bytecode with atoms - skips parsing entirely
        const func = try bytecode_cache.deserializeBytecodeWithAtoms(
            &reader,
            &self.ctx.atoms,
            self.allocator,
            &self.strings,
        );

        // Execute the deserialized bytecode
        _ = try self.interpreter.run(func);
    }

    /// Serialize bytecode for caching (Phase 1b: cache miss path)
    pub fn serializeBytecode(self: *Self, func: *const zq.FunctionBytecode, buffer: []u8) ![]const u8 {
        var writer = bytecode_cache.SliceWriter{ .buffer = buffer };
        try bytecode_cache.serializeFunctionBytecode(func, &writer, self.allocator);
        return writer.getWritten();
    }

    /// Execute the handler function with a request
    pub fn executeHandler(self: *Self, request: HttpRequest) !HttpResponse {
        return self.executeHandlerWithId(request, 0);
    }

    pub fn executeHandlerWithId(self: *Self, request: HttpRequest, request_id: u64) !HttpResponse {
        return self.executeHandlerInternal(request, request_id, false);
    }

    /// Execute handler and return a response that borrows JS string bodies.
    /// Caller must ensure the runtime is not reset or reused until after send.
    pub fn executeHandlerBorrowed(self: *Self, request: HttpRequest) !HttpResponse {
        return self.executeHandlerBorrowedWithId(request, 0);
    }

    pub fn executeHandlerBorrowedWithId(self: *Self, request: HttpRequest, request_id: u64) !HttpResponse {
        return self.executeHandlerInternal(request, request_id, true);
    }

    fn executeHandlerInternal(self: *Self, request: HttpRequest, request_id: u64, borrow_body: bool) !HttpResponse {
        const handler_atom = self.handler_atom orelse return error.NoHandler;
        self.last_request_body_len = if (request.body) |b| b.len else 0;

        // Get handler function from globals
        const handler_val = self.ctx.getGlobal(handler_atom) orelse {
            return error.NoHandler;
        };

        if (!handler_val.isCallable()) {
            std.log.err(
                "Handler not callable (int={any} bool={any} string={any} object={any} ptr={any})",
                .{
                    handler_val.isInt(),
                    handler_val.isBool(),
                    handler_val.isString(),
                    handler_val.isObject(),
                    handler_val.isPtr(),
                },
            );
            return error.HandlerNotCallable;
        }

        var tracked = false;
        if (request_id != 0) {
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
        var reset_after = true;
        defer if (reset_after) self.resetForNextRequest();

        // Set thread-local runtime for native function callbacks (e.g., renderToString)
        current_runtime = self;
        defer current_runtime = null;

        // Set callback for JSX function component rendering
        zq.http.setCallFunctionCallback(callFunctionWrapper);
        defer zq.http.clearCallFunctionCallback();

        // Create Request object from HttpRequest
        const request_obj = try self.createRequestObject(request);

        // Call handler(request)
        const args = [_]zq.JSValue{request_obj};
        const handler_obj = handler_val.toPtr(zq.JSObject);

        const result = self.callFunction(handler_obj, &args) catch |err| {
            std.log.err("Handler execution failed: {}", .{err});
            return error.HandlerError;
        };

        // Convert result to HttpResponse
        const response = try self.extractResponseInternal(result, borrow_body);
        if (borrow_body) {
            reset_after = false;
        }

        return response;
    }

    fn createRequestObject(self: *Self, request: HttpRequest) !zq.JSValue {
        const req_obj = try self.ctx.createObject(null);

        // Set URL using predefined atom
        const url_str = try self.ctx.createString(request.url);
        try self.ctx.setPropertyChecked(req_obj, zq.Atom.url, url_str);

        // Set method using predefined atom
        const method_str = try self.ctx.createString(request.method);
        try self.ctx.setPropertyChecked(req_obj, zq.Atom.method, method_str);

        // Set body if present using predefined atom
        if (request.body) |body| {
            const body_str = try self.ctx.createString(body);
            try self.ctx.setPropertyChecked(req_obj, zq.Atom.body, body_str);
        }

        if (request.headers.items.len > 0) {
            const headers_obj = try self.ctx.createObject(null);
            for (request.headers.items) |header| {
                const key_atom = try self.ctx.atoms.intern(header.key);
                const value_str = try self.ctx.createString(header.value);
                try self.ctx.setPropertyChecked(headers_obj, key_atom, value_str);
            }
            try self.ctx.setPropertyChecked(req_obj, zq.Atom.headers, headers_obj.toValue());
        }

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
                    response.setBodyBorrowed(str);
                } else {
                    const owned = try self.allocator.dupe(u8, str.data());
                    response.setBodyOwned(owned);
                }
            }
            return response;
        }

        const result_obj = result.toPtr(zq.JSObject);

        // Extract status using predefined atom
        if (result_obj.getOwnProperty(zq.Atom.status)) |status_val| {
            if (status_val.isInt()) {
                response.status = @intCast(status_val.getInt());
            }
        }

        // Extract body using predefined atom
        if (result_obj.getOwnProperty(zq.Atom.body)) |body_val| {
            if (body_val.isString()) {
                const str = body_val.toPtr(zq.JSString);
                if (borrow_body) {
                    response.setBodyBorrowed(str);
                } else {
                    const owned = try self.allocator.dupe(u8, str.data());
                    response.setBodyOwned(owned);
                }
            }
        }

        // Extract headers
        if (result_obj.getOwnProperty(zq.Atom.headers)) |headers_val| {
            if (headers_val.isObject()) {
                const headers_obj = headers_val.toPtr(zq.JSObject);
                const keys = try headers_obj.getOwnEnumerableKeys(self.allocator);
                defer self.allocator.free(keys);
                for (keys) |key_atom| {
                    const key_name = self.ctx.atoms.getName(key_atom) orelse continue;
                    const header_val = headers_obj.getOwnProperty(key_atom) orelse continue;
                    if (header_val.isString()) {
                        const str = header_val.toPtr(zq.JSString);
                        if (borrow_body) {
                            try response.putHeaderBorrowed(key_name, str.data());
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
        // Clear stack
        self.ctx.sp = 0;
        self.ctx.call_depth = 0;
        self.ctx.clearException();

        // Trigger minor GC if nursery is half full
        const used = self.gc_state.nursery.used();
        const base_threshold = self.gc_state.config.nursery_size / 2;
        const body_threshold = self.gc_state.config.nursery_size / 4;
        const threshold = if (self.last_request_body_len >= base_threshold) body_threshold else base_threshold;
        if (used > threshold) {
            self.gc_state.minorGC();
        }
        self.last_request_body_len = 0;

        // Note: We do NOT clear user globals here. The handler code and all its
        // dependencies (functions, constants, component definitions) are loaded
        // once per runtime and must persist across requests. Request isolation
        // is achieved through the handler pool (each request gets a pooled runtime)
        // and stack/exception clearing above.
    }
};

// ============================================================================
// Native Function Implementations
// ============================================================================

fn consoleLog(_: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const stdout = std.fs.File.stdout();
    for (args, 0..) |arg, i| {
        if (i > 0) stdout.writeAll(" ") catch {};
        printValue(arg, stdout) catch {};
    }
    stdout.writeAll("\n") catch {};
    return zq.JSValue.undefined_val;
}

fn consoleError(_: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const stderr = std.fs.File.stderr();
    stderr.writeAll("\x1b[31m[ERROR]\x1b[0m ") catch {};
    for (args, 0..) |arg, i| {
        if (i > 0) stderr.writeAll(" ") catch {};
        printValue(arg, stderr) catch {};
    }
    stderr.writeAll("\n") catch {};
    return zq.JSValue.undefined_val;
}

fn printValue(val: zq.JSValue, file: std.fs.File) !void {
    if (val.isUndefined()) {
        try file.writeAll("undefined");
    } else if (val.isNull()) {
        try file.writeAll("null");
    } else if (val.isTrue()) {
        try file.writeAll("true");
    } else if (val.isFalse()) {
        try file.writeAll("false");
    } else if (val.isInt()) {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return;
        try file.writeAll(s);
    } else if (val.isString()) {
        const str = val.toPtr(zq.JSString);
        try file.writeAll(str.data());
    } else if (val.isObject()) {
        try file.writeAll("[Object]");
    } else {
        try file.writeAll("[unknown]");
    }
}

// ============================================================================
// Handler Pool (Lock-Free)
// ============================================================================

/// Lock-free pool of pre-initialized JavaScript runtimes, backed by zts.LockFreePool.
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
    pool: zq.LockFreePool,
    cache: bytecode_cache.BytecodeCache,

    const Self = @This();

    pub const ResponseHandle = struct {
        response: HttpResponse,
        runtime: *Runtime,
        base_rt: *zq.LockFreePool.Runtime,
        pool: *HandlerPool,

        pub fn deinit(self: *ResponseHandle) void {
            self.response.deinit();
            self.runtime.resetForNextRequest();
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
        const pool = try zq.LockFreePool.init(allocator, .{
            .max_size = max_size,
            .gc_config = .{ .nursery_size = config.nursery_size },
            .use_hybrid_allocation = true,
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
            .pool = pool,
            .cache = bytecode_cache.BytecodeCache.init(allocator),
        };

        try self.prewarm();
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.pool.deinit();
        self.cache.deinit();
    }

    /// Execute handler with a request (acquire, run, release)
    pub fn executeHandler(self: *Self, request: HttpRequest) !HttpResponse {
        const request_id = self.request_seq.fetchAdd(1, .acq_rel) + 1;
        const base_rt = try self.acquireForRequest();
        defer {
            self.releaseForRequest(base_rt);
        }

        const rt = try self.ensureRuntime(base_rt);
        var exec_timer = std.time.Timer.start() catch null;
        defer if (exec_timer) |*t| self.recordExec(t.read());
        return rt.executeHandlerWithId(request, request_id);
    }

    /// Execute handler and return a response handle that borrows JS strings.
    /// Caller must call handle.deinit() after sending the response.
    pub fn executeHandlerBorrowed(self: *Self, request: HttpRequest) !ResponseHandle {
        const request_id = self.request_seq.fetchAdd(1, .acq_rel) + 1;
        const base_rt = try self.acquireForRequest();
        errdefer self.releaseForRequest(base_rt);

        const rt = try self.ensureRuntime(base_rt);
        var exec_timer = std.time.Timer.start() catch null;
        defer if (exec_timer) |*t| self.recordExec(t.read());
        const response = try rt.executeHandlerBorrowedWithId(request, request_id);
        return .{
            .response = response,
            .runtime = rt,
            .base_rt = base_rt,
            .pool = self,
        };
    }

    pub fn getInUse(self: *const Self) usize {
        return self.in_use.load(.acquire);
    }

    /// Get cache statistics
    pub fn getCacheStats(self: *const Self) struct { hits: u64, misses: u64, hit_rate: f64 } {
        return .{
            .hits = self.cache.hits,
            .misses = self.cache.misses,
            .hit_rate = self.cache.hitRate(),
        };
    }

    fn prewarm(self: *Self) !void {
        const prewarm_count = @min(@as(usize, 4), self.max_size);
        for (0..prewarm_count) |_| {
            const base_rt = try self.pool.acquire();
            errdefer self.pool.release(base_rt);
            _ = try self.ensureRuntime(base_rt);
            self.pool.release(base_rt);
        }
    }

    fn acquireForRequest(self: *Self) !*zq.LockFreePool.Runtime {
        var wait_timer = std.time.Timer.start() catch null;
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
                // Use monotonic ordering - counter is only for limit/metrics, not synchronization
                const prev = self.in_use.fetchAdd(1, .monotonic);
                if (prev < self.max_size) break;
                _ = self.in_use.fetchSub(1, .monotonic);

                retry_count += 1;

                // Check timeout first
                if (self.acquire_timeout_ms == 0) {
                    _ = self.exhausted_count.fetchAdd(1, .monotonic);
                    if (wait_timer) |*t| self.recordWait(t.read());
                    return error.PoolExhausted;
                }

                if (wait_timer) |*t| {
                    if (t.read() >= timeout_ns) {
                        _ = self.exhausted_count.fetchAdd(1, .monotonic);
                        self.recordWait(t.read());
                        return error.PoolExhausted;
                    }
                }

                // Circuit breaker: fail fast after max retries
                if (retry_count > max_retries) {
                    _ = self.exhausted_count.fetchAdd(1, .monotonic);
                    if (wait_timer) |*t| self.recordWait(t.read());
                    return error.PoolExhausted;
                }

                // Phase 1: Spin without sleep for brief contention
                if (retry_count <= spin_iterations) {
                    std.atomic.spinLoopHint();
                    continue;
                }

                // Phase 2: Sleep with jitter to prevent thundering herd
                // Jitter: randomize within +/- 25% of backoff
                const jitter_range = backoff_ns / 4;
                const jitter = if (jitter_range > 0)
                    @as(u64, @truncate(@as(u128, retry_count * 7919) % (jitter_range * 2)))
                else
                    0;
                const sleep_ns = backoff_ns -| jitter_range + jitter;
                std.posix.nanosleep(0, sleep_ns);
                backoff_ns = @min(backoff_ns * 2, max_backoff_ns);
                continue;
            } else {
                _ = self.in_use.fetchAdd(1, .monotonic);
                break;
            }
        }

        const rt = zq.pool.acquireWithCache(&self.pool) catch |err| {
            _ = self.in_use.fetchSub(1, .monotonic);
            if (wait_timer) |*t| self.recordWait(t.read());
            return err;
        };
        if (wait_timer) |*t| self.recordWait(t.read());
        return rt;
    }

    fn releaseForRequest(self: *Self, rt: *zq.LockFreePool.Runtime) void {
        zq.pool.releaseWithCache(&self.pool, rt);
        _ = self.in_use.fetchSub(1, .monotonic);
    }

    pub fn getMetrics(self: *const Self) struct {
        requests: u64,
        exhausted: u64,
        avg_wait_ns: u64,
        max_wait_ns: u64,
        avg_exec_ns: u64,
        max_exec_ns: u64,
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
        };
    }

    fn recordWait(self: *Self, ns: u64) void {
        _ = self.total_wait_ns.fetchAdd(ns, .acq_rel);
        updateMax(&self.max_wait_ns, ns);
    }

    fn recordExec(self: *Self, ns: u64) void {
        _ = self.total_exec_ns.fetchAdd(ns, .acq_rel);
        updateMax(&self.max_exec_ns, ns);
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

        const rt = try Runtime.initFromPool(base_rt, self.config);
        errdefer rt.deinit();

        try self.loadHandlerCached(rt);

        base_rt.user_data = rt;
        base_rt.user_deinit = runtimeUserDeinit;
        return rt;
    }

    fn loadHandlerCached(self: *Self, rt: *Runtime) !void {
        // For now, always parse. The cache stores serialized bytecode but
        // we need to parse to populate atoms in each runtime's atom table.
        // TODO: Implement full atom serialization for true cache hit path.
        try rt.loadHandler(self.handler_code, self.handler_filename);
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

test "Runtime creation" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    try std.testing.expect(rt.ctx.sp == 0);
}

test "HandlerPool basic operations" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const handler_code = "function handler(req) { return Response.text('ok'); }";
    var pool = try HandlerPool.init(allocator, .{}, handler_code, "<handler>", 2, 0);
    defer {
        // Flush thread-local cache before deinit to avoid dangling pointer
        zq.pool.releaseThreadLocal(&pool.pool);
        pool.deinit();
    }

    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try pool.executeHandler(request);
    defer response.deinit();

    try std.testing.expectEqualStrings("ok", response.body);
}

test "HandlerPool concurrent stress" {
    const allocator = std.heap.c_allocator;

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
            var request = HttpRequest{
                .method = method,
                .url = url,
                .headers = .{},
                .body = null,
            };
            defer request.deinit(ctx.allocator);

            var i: usize = 0;
            while (i < iterations) : (i += 1) {
                var response = ctx.pool.executeHandler(request) catch {
                    _ = ctx.errors.fetchAdd(1, .acq_rel);
                    continue;
                };
                response.deinit();
            }

            // Flush thread-local runtime cache so pool can deinit cleanly in tests.
            zq.pool.releaseThreadLocal(&ctx.pool.pool);
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

    // Flush main thread cache (if any).
    zq.pool.releaseThreadLocal(&pool.pool);

    try std.testing.expectEqual(@as(u32, 0), errors.load(.acquire));
}

test "string prototype methods are callable" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const proto = rt.ctx.string_prototype orelse return error.NoRootClass;
    const split_val = proto.getProperty(zq.Atom.split) orelse return error.NoHandler;
    try std.testing.expect(split_val.isCallable());

    try rt.loadHandler("function handler(req){ return Response.text(typeof ''.split); }", "<test>");

    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request);
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

    var req1 = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = null,
    };

    var resp1 = try rt.executeHandler(req1);
    try std.testing.expectEqualStrings("hello", resp1.body);
    resp1.deinit();
    req1.deinit(allocator);

    // Reset and test property access
    const rt2 = try Runtime.init(allocator, .{});
    defer rt2.deinit();

    const prop_test = "function handler(req){ return Response.text(req.method); }";
    try rt2.loadHandler(prop_test, "<test2>");

    var req2 = HttpRequest{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = try allocator.dupe(u8, "test"),
    };

    var resp2 = try rt2.executeHandler(req2);
    try std.testing.expectEqualStrings("POST", resp2.body);
    resp2.deinit();
    req2.deinit(allocator);

    // Test typeof without var assignment
    const rt3 = try Runtime.init(allocator, .{});
    defer rt3.deinit();

    const typeof_split = "function handler(req){ return Response.text(typeof 'a&b'.split('&')); }";
    try rt3.loadHandler(typeof_split, "<test3>");

    var req3 = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = null,
    };

    var resp3 = try rt3.executeHandler(req3);
    try std.testing.expectEqualStrings("object", resp3.body);
    resp3.deinit();
    req3.deinit(allocator);

    // Test simple var assignment
    const rt4 = try Runtime.init(allocator, .{});
    defer rt4.deinit();

    const handler_code =
        "function handler(req){ let x = 'test'; return Response.text(x); }";
    try rt4.loadHandler(handler_code, "<test>");

    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt4.executeHandler(request);
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

    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request);
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

    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request);
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

    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request);
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

    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = .{},
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request);
    defer response.deinit();

    try std.testing.expectEqualStrings("<div>Hello</div>", response.body);
}
