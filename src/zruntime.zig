//! Native Zig Runtime for zigttp
//!
//! Pure Zig implementation using zquickjs - no C dependencies.
//! Designed for FaaS with per-request isolation and fast cold starts.

const std = @import("std");

// Import zquickjs module
const zq = @import("zquickjs");

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

    // Internal zquickjs settings
    /// GC nursery size
    nursery_size: usize = 64 * 1024,
};

// ============================================================================
// HTTP Types (Native Zig)
// ============================================================================

pub const HttpRequest = struct {
    method: []const u8,
    url: []const u8,
    headers: std.StringHashMap([]const u8),
    body: ?[]const u8,

    pub fn deinit(self: *HttpRequest, allocator: std.mem.Allocator) void {
        allocator.free(self.method);
        allocator.free(self.url);
        if (self.body) |b| allocator.free(b);
        self.headers.deinit();
    }
};

pub const HttpResponse = struct {
    status: u16,
    headers: std.StringHashMap([]const u8),
    body: []const u8,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) HttpResponse {
        return .{
            .status = 200,
            .headers = std.StringHashMap([]const u8).init(allocator),
            .body = "",
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *HttpResponse) void {
        self.headers.deinit();
        if (self.body.len > 0) {
            self.allocator.free(self.body);
        }
    }
};

// ============================================================================
// Runtime Instance
// ============================================================================

pub const Runtime = struct {
    allocator: std.mem.Allocator,
    ctx: *zq.Context,
    gc_state: *zq.GC,
    interpreter: zq.Interpreter,
    strings: zq.StringTable,
    handler_atom: ?zq.Atom,
    config: RuntimeConfig,

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

        // Initialize context
        const ctx = try zq.Context.init(allocator, gc_state, .{});
        errdefer ctx.deinit();

        self.* = .{
            .allocator = allocator,
            .ctx = ctx,
            .gc_state = gc_state,
            .interpreter = zq.Interpreter.init(ctx),
            .strings = zq.StringTable.init(allocator),
            .handler_atom = null,
            .config = config,
        };

        // Install built-in bindings
        try self.installBindings();

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.strings.deinit();
        self.ctx.deinit();
        self.gc_state.deinit();
        self.allocator.destroy(self.gc_state);
        self.allocator.destroy(self);
    }

    /// Install native API bindings (console, Response helpers, etc.)
    fn installBindings(self: *Self) !void {
        // Intern common atoms
        self.handler_atom = try self.ctx.atoms.intern("handler");

        // Install console object
        try self.installConsole();

        // Install Response helpers
        try self.installResponseHelpers();
    }

    fn installConsole(self: *Self) !void {
        const root_class = self.ctx.root_class orelse return error.NoRootClass;

        // Create console object
        const console_obj = try zq.JSObject.create(self.allocator, root_class, null);

        // Add console.log
        const log_atom = try self.ctx.atoms.intern("log");
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
        const console_atom = try self.ctx.atoms.intern("console");
        try self.ctx.setGlobal(console_atom, console_obj.toValue());
    }

    fn installResponseHelpers(self: *Self) !void {
        const root_class = self.ctx.root_class orelse return error.NoRootClass;

        // Create Response object with static methods
        const response_obj = try zq.JSObject.create(self.allocator, root_class, null);

        // Response.json()
        const json_atom = try self.ctx.atoms.intern("json");
        const json_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            responseJson,
            json_atom,
            2,
        );
        try response_obj.setProperty(self.allocator, json_atom, json_func.toValue());

        // Response.text()
        const text_atom = try self.ctx.atoms.intern("text");
        const text_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            responseText,
            text_atom,
            2,
        );
        try response_obj.setProperty(self.allocator, text_atom, text_func.toValue());

        // Response.html()
        const html_atom = try self.ctx.atoms.intern("html");
        const html_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            responseHtml,
            html_atom,
            2,
        );
        try response_obj.setProperty(self.allocator, html_atom, html_func.toValue());

        // Register on global
        const response_atom = try self.ctx.atoms.intern("Response");
        try self.ctx.setGlobal(response_atom, response_obj.toValue());
    }

    /// Load and compile JavaScript code
    pub fn loadCode(self: *Self, code: []const u8, filename: []const u8) !void {
        _ = filename;

        // Parse the source code
        var p = zq.Parser.init(self.allocator, code, &self.strings);
        defer p.deinit();

        const bytecode_data = try p.parse();

        // Create FunctionBytecode struct to wrap the parsed result
        const func = zq.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = p.local_count,
            .stack_size = 256,
            .flags = .{},
            .code = bytecode_data,
            .constants = p.constants.items,
            .source_map = null,
        };

        // Execute the compiled code to define functions
        _ = try self.interpreter.run(&func);
    }

    /// Load handler code (alias for loadCode for API compatibility)
    pub fn loadHandler(self: *Self, code: []const u8, filename: []const u8) !void {
        return self.loadCode(code, filename);
    }

    /// Execute the handler function with a request
    pub fn executeHandler(self: *Self, request: HttpRequest) !HttpResponse {
        const handler_atom = self.handler_atom orelse return error.NoHandler;

        // Get handler function from globals
        const handler_val = self.ctx.getGlobal(handler_atom) orelse return error.NoHandler;

        if (!handler_val.isCallable()) {
            return error.HandlerNotCallable;
        }

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
        return self.extractResponse(result);
    }

    fn createRequestObject(self: *Self, request: HttpRequest) !zq.JSValue {
        const root_class = self.ctx.root_class orelse return error.NoRootClass;
        const req_obj = try zq.JSObject.create(self.allocator, root_class, null);

        // Set URL
        const url_atom = try self.ctx.atoms.intern("url");
        const url_str = try self.createString(request.url);
        try req_obj.setProperty(self.allocator, url_atom, url_str);

        // Set method
        const method_atom = try self.ctx.atoms.intern("method");
        const method_str = try self.createString(request.method);
        try req_obj.setProperty(self.allocator, method_atom, method_str);

        // Set body if present
        if (request.body) |body| {
            const body_atom = try self.ctx.atoms.intern("body");
            const body_str = try self.createString(body);
            try req_obj.setProperty(self.allocator, body_atom, body_str);
        }

        return req_obj.toValue();
    }

    fn createString(self: *Self, str: []const u8) !zq.JSValue {
        const js_str = try zq.createString(self.allocator, str);
        return zq.JSValue.fromPtr(js_str);
    }

    fn callFunction(self: *Self, func_obj: *zq.JSObject, args: []const zq.JSValue) !zq.JSValue {
        // Get bytecode function data
        const bc_data = func_obj.getBytecodeFunctionData() orelse {
            // Try native function
            const native_data = func_obj.getNativeFunctionData() orelse return error.NotCallable;
            return native_data.func(self.ctx, zq.JSValue.undefined_val, args);
        };

        // Push arguments onto stack
        for (args) |arg| {
            try self.ctx.push(arg);
        }

        // Execute bytecode
        return self.interpreter.run(bc_data.bytecode);
    }

    fn extractResponse(self: *Self, result: zq.JSValue) !HttpResponse {
        var response = HttpResponse.init(self.allocator);

        if (!result.isObject()) {
            // If result is a string, use it as body
            if (result.isString()) {
                const str = result.toPtr(zq.JSString);
                response.body = try self.allocator.dupe(u8, str.data());
            }
            return response;
        }

        const result_obj = result.toPtr(zq.JSObject);

        // Extract status
        const status_atom = try self.ctx.atoms.intern("status");
        if (result_obj.getOwnProperty(status_atom)) |status_val| {
            if (status_val.isInt()) {
                response.status = @intCast(status_val.getInt());
            }
        }

        // Extract body
        const body_atom = try self.ctx.atoms.intern("body");
        if (result_obj.getOwnProperty(body_atom)) |body_val| {
            if (body_val.isString()) {
                const str = body_val.toPtr(zq.JSString);
                response.body = try self.allocator.dupe(u8, str.data());
            }
        }

        // Extract headers (TODO: Iterate headers object and populate response.headers)
        const headers_atom = try self.ctx.atoms.intern("headers");
        if (result_obj.getOwnProperty(headers_atom)) |headers_val| {
            _ = headers_val.isObject(); // Headers extraction not yet implemented
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
        if (self.gc_state.nursery.used() > self.gc_state.config.nursery_size / 2) {
            self.gc_state.minorGC();
        }

        // Reset atom table (clear user-defined atoms)
        self.ctx.atoms.reset();
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

fn responseJson(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return zq.JSValue.undefined_val;

    const root_class = ctx.root_class orelse return error.NoRootClass;
    const resp_obj = try zq.JSObject.create(ctx.allocator, root_class, null);

    // Set status 200
    const status_atom = try ctx.atoms.intern("status");
    try resp_obj.setProperty(ctx.allocator, status_atom, zq.JSValue.fromInt(200));

    // Set Content-Type header
    const headers_atom = try ctx.atoms.intern("headers");
    const headers_obj = try zq.JSObject.create(ctx.allocator, root_class, null);
    const ct_atom = try ctx.atoms.intern("Content-Type");
    const ct_val = try zq.createString(ctx.allocator, "application/json");
    try headers_obj.setProperty(ctx.allocator, ct_atom, zq.JSValue.fromPtr(ct_val));
    try resp_obj.setProperty(ctx.allocator, headers_atom, headers_obj.toValue());

    // Stringify body (simplified - just store the value for now)
    const body_atom = try ctx.atoms.intern("body");
    try resp_obj.setProperty(ctx.allocator, body_atom, args[0]);

    return resp_obj.toValue();
}

fn responseText(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return zq.JSValue.undefined_val;

    const root_class = ctx.root_class orelse return error.NoRootClass;
    const resp_obj = try zq.JSObject.create(ctx.allocator, root_class, null);

    const status_atom = try ctx.atoms.intern("status");
    try resp_obj.setProperty(ctx.allocator, status_atom, zq.JSValue.fromInt(200));

    const headers_atom = try ctx.atoms.intern("headers");
    const headers_obj = try zq.JSObject.create(ctx.allocator, root_class, null);
    const ct_atom = try ctx.atoms.intern("Content-Type");
    const ct_val = try zq.createString(ctx.allocator, "text/plain");
    try headers_obj.setProperty(ctx.allocator, ct_atom, zq.JSValue.fromPtr(ct_val));
    try resp_obj.setProperty(ctx.allocator, headers_atom, headers_obj.toValue());

    const body_atom = try ctx.atoms.intern("body");
    try resp_obj.setProperty(ctx.allocator, body_atom, args[0]);

    return resp_obj.toValue();
}

fn responseHtml(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return zq.JSValue.undefined_val;

    const root_class = ctx.root_class orelse return error.NoRootClass;
    const resp_obj = try zq.JSObject.create(ctx.allocator, root_class, null);

    const status_atom = try ctx.atoms.intern("status");
    try resp_obj.setProperty(ctx.allocator, status_atom, zq.JSValue.fromInt(200));

    const headers_atom = try ctx.atoms.intern("headers");
    const headers_obj = try zq.JSObject.create(ctx.allocator, root_class, null);
    const ct_atom = try ctx.atoms.intern("Content-Type");
    const ct_val = try zq.createString(ctx.allocator, "text/html");
    try headers_obj.setProperty(ctx.allocator, ct_atom, zq.JSValue.fromPtr(ct_val));
    try resp_obj.setProperty(ctx.allocator, headers_atom, headers_obj.toValue());

    const body_atom = try ctx.atoms.intern("body");
    try resp_obj.setProperty(ctx.allocator, body_atom, args[0]);

    return resp_obj.toValue();
}

// ============================================================================
// Runtime Pool
// ============================================================================

pub const RuntimePool = struct {
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    handler_code: []const u8,

    /// Pool of pre-initialized runtimes
    available: std.ArrayList(*Runtime),

    /// Currently in-use runtimes
    in_use: std.ArrayList(*Runtime),

    /// Mutex for thread safety
    mutex: std.Thread.Mutex,

    /// Max pool size
    max_size: usize,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, config: RuntimeConfig, handler_code: []const u8, max_size: usize) !Self {
        var pool = Self{
            .allocator = allocator,
            .config = config,
            .handler_code = handler_code,
            .available = .empty,
            .in_use = .empty,
            .mutex = .{},
            .max_size = max_size,
        };

        // Pre-warm with a few runtimes
        const prewarm_count = @min(4, max_size);
        for (0..prewarm_count) |_| {
            const rt = try pool.createRuntime();
            try pool.available.append(allocator, rt);
        }

        return pool;
    }

    pub fn deinit(self: *Self) void {
        for (self.available.items) |rt| {
            rt.deinit();
        }
        for (self.in_use.items) |rt| {
            rt.deinit();
        }
        self.available.deinit(self.allocator);
        self.in_use.deinit(self.allocator);
    }

    fn createRuntime(self: *Self) !*Runtime {
        const rt = try Runtime.init(self.allocator, self.config);
        errdefer rt.deinit();

        try rt.loadHandler(self.handler_code, "<handler>");
        return rt;
    }

    /// Acquire a runtime from the pool
    pub fn acquire(self: *Self) !*Runtime {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.available.items.len > 0) {
            const rt = self.available.pop().?;
            try self.in_use.append(self.allocator, rt);
            return rt;
        }

        // Create new runtime if under limit
        if (self.in_use.items.len < self.max_size) {
            const rt = try self.createRuntime();
            try self.in_use.append(self.allocator, rt);
            return rt;
        }

        return error.PoolExhausted;
    }

    /// Release a runtime back to the pool
    pub fn release(self: *Self, rt: *Runtime) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        // Remove from in_use
        for (self.in_use.items, 0..) |item, i| {
            if (item == rt) {
                _ = self.in_use.orderedRemove(i);
                break;
            }
        }

        // Reset for next request
        rt.resetForNextRequest();

        // Return to pool
        self.available.append(self.allocator, rt) catch {
            rt.deinit();
        };
    }

    /// Execute handler with a request (acquire, run, release)
    pub fn executeHandler(self: *Self, request: HttpRequest) !HttpResponse {
        const rt = try self.acquire();
        defer self.release(rt);
        return rt.executeHandler(request);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Runtime creation" {
    const allocator = std.testing.allocator;
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    try std.testing.expect(rt.ctx.sp == 0);
}

test "RuntimePool basic operations" {
    const allocator = std.testing.allocator;
    const handler_code = "function handler(req) { return { status: 200, body: 'ok' }; }";
    var pool = try RuntimePool.init(allocator, .{}, handler_code, 2);
    defer pool.deinit();

    const rt1 = try pool.acquire();
    const rt2 = try pool.acquire();

    try std.testing.expect(rt1 != rt2);

    pool.release(rt1);
    pool.release(rt2);
}
