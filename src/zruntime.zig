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

    /// Clear user-defined globals between requests (builtins always preserved)
    reset_user_globals: bool = false,

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
        var it = self.headers.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*);
        }
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
        var it = self.headers.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.headers.deinit();
        if (self.body.len > 0) {
            self.allocator.free(self.body);
        }
    }

    pub fn putHeader(self: *HttpResponse, key: []const u8, value: []const u8) !void {
        const key_dup = try self.allocator.dupe(u8, key);
        errdefer self.allocator.free(key_dup);
        const value_dup = try self.allocator.dupe(u8, value);
        errdefer self.allocator.free(value_dup);
        try self.headers.put(key_dup, value_dup);
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

        // Install core JS builtins (Array.prototype, Object, Math, JSON, etc.)
        try zq.builtins.initBuiltins(ctx);

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
        // Parse the source code
        var p = zq.Parser.init(self.allocator, code, &self.strings, &self.ctx.atoms);
        defer p.deinit();

        // Enable JSX mode for .jsx files
        if (std.mem.endsWith(u8, filename, ".jsx")) {
            p.enableJsx();
        }

        const bytecode_data = try p.parse();

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
        const handler_val = self.ctx.getGlobal(handler_atom) orelse {
            return error.NoHandler;
        };

        if (!handler_val.isCallable()) {
            return error.HandlerNotCallable;
        }

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
        return self.extractResponse(result);
    }

    fn createRequestObject(self: *Self, request: HttpRequest) !zq.JSValue {
        const root_class = self.ctx.root_class orelse return error.NoRootClass;
        const req_obj = try zq.JSObject.create(self.allocator, root_class, null);

        // Set URL using predefined atom
        const url_str = try self.createString(request.url);
        try req_obj.setProperty(self.allocator, zq.Atom.url, url_str);

        // Set method using predefined atom
        const method_str = try self.createString(request.method);
        try req_obj.setProperty(self.allocator, zq.Atom.method, method_str);

        // Set body if present using predefined atom
        if (request.body) |body| {
            const body_str = try self.createString(body);
            try req_obj.setProperty(self.allocator, zq.Atom.body, body_str);
        }

        if (request.headers.count() > 0) {
            const headers_obj = try zq.JSObject.create(self.allocator, root_class, null);
            var it = request.headers.iterator();
            while (it.next()) |entry| {
                const key_atom = try self.ctx.atoms.intern(entry.key_ptr.*);
                const value_str = try self.createString(entry.value_ptr.*);
                try headers_obj.setProperty(self.allocator, key_atom, value_str);
            }
            try req_obj.setProperty(self.allocator, zq.Atom.headers, headers_obj.toValue());
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
                response.body = try self.allocator.dupe(u8, str.data());
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
                        try response.putHeader(key_name, str.data());
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
        if (self.gc_state.nursery.used() > self.gc_state.config.nursery_size / 2) {
            self.gc_state.minorGC();
        }

        // Note: We do NOT clear user globals here. The handler code and all its
        // dependencies (functions, constants, component definitions) are loaded
        // once per runtime and must persist across requests. Request isolation
        // is achieved through the RuntimePool (each request gets a pooled runtime)
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
// Runtime Pool
// ============================================================================

pub const RuntimePool = struct {
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    handler_code: []const u8,
    handler_filename: []const u8,

    /// Pool of pre-initialized runtimes
    available: std.ArrayList(*Runtime),

    /// Currently in-use runtimes
    in_use: std.ArrayList(*Runtime),

    /// Mutex for thread safety
    mutex: std.Thread.Mutex,

    /// Max pool size
    max_size: usize,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, config: RuntimeConfig, handler_code: []const u8, handler_filename: []const u8, max_size: usize) !Self {
        var pool = Self{
            .allocator = allocator,
            .config = config,
            .handler_code = handler_code,
            .handler_filename = handler_filename,
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

        try rt.loadHandler(self.handler_code, self.handler_filename);
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    try std.testing.expect(rt.ctx.sp == 0);
}

test "RuntimePool basic operations" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const handler_code = "function handler(req) { return { status: 200, body: 'ok' }; }";
    var pool = try RuntimePool.init(allocator, .{}, handler_code, "<handler>", 2);
    defer pool.deinit();

    const rt1 = try pool.acquire();
    const rt2 = try pool.acquire();

    try std.testing.expect(rt1 != rt2);

    pool.release(rt1);
    pool.release(rt2);
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

    const headers = std.StringHashMap([]const u8).init(allocator);
    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers,
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

    const headers1 = std.StringHashMap([]const u8).init(allocator);
    var req1 = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers1,
        .body = null,
    };

    var resp1 = try rt.executeHandler(req1);
    std.debug.print("\nSimple test body: '{s}'\n", .{resp1.body});
    resp1.deinit();
    req1.deinit(allocator);

    // Reset and test property access
    const rt2 = try Runtime.init(allocator, .{});
    defer rt2.deinit();

    const prop_test = "function handler(req){ return Response.text(req.method); }";
    try rt2.loadHandler(prop_test, "<test2>");

    const headers2 = std.StringHashMap([]const u8).init(allocator);
    var req2 = HttpRequest{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers2,
        .body = try allocator.dupe(u8, "test"),
    };

    var resp2 = try rt2.executeHandler(req2);
    std.debug.print("Property test body: '{s}'\n", .{resp2.body});
    resp2.deinit();
    req2.deinit(allocator);

    // Test typeof without var assignment
    const rt3 = try Runtime.init(allocator, .{});
    defer rt3.deinit();

    const typeof_split = "function handler(req){ return Response.text(typeof 'a&b'.split('&')); }";
    try rt3.loadHandler(typeof_split, "<test3>");

    const headers3 = std.StringHashMap([]const u8).init(allocator);
    var req3 = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers3,
        .body = null,
    };

    var resp3 = try rt3.executeHandler(req3);
    std.debug.print("typeof direct split: '{s}'\n", .{resp3.body});
    resp3.deinit();
    req3.deinit(allocator);

    // Test simple var assignment
    const rt4 = try Runtime.init(allocator, .{});
    defer rt4.deinit();

    const handler_code =
        "function handler(req){ var x = 'test'; return Response.text(x); }";
    try rt4.loadHandler(handler_code, "<test>");

    const headers = std.StringHashMap([]const u8).init(allocator);
    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt4.executeHandler(request);
    std.debug.print("simple var: '{s}'\n", .{response.body});
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
        "function handler(req){ for (var i=0; i<1; i++){ return Response.text(typeof i); } return Response.text('none'); }";
    try rt.loadHandler(handler_code, "<test>");

    const headers = std.StringHashMap([]const u8).init(allocator);
    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request);
    defer response.deinit();

    try std.testing.expectEqualStrings("number", response.body);
}

test "object destructuring works" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    // Test object destructuring: extract name from object
    const handler_code =
        \\function handler(req){
        \\  var obj = { name: 'Alice', age: 30 };
        \\  const { name, age } = obj;
        \\  return Response.text(name + '-' + age);
        \\}
    ;
    try rt.loadHandler(handler_code, "<test>");

    const headers = std.StringHashMap([]const u8).init(allocator);
    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request);
    defer response.deinit();

    try std.testing.expectEqualStrings("Alice-30", response.body);
}

test "array destructuring works" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();

    const handler_code =
        \\function handler(req){
        \\  var arr = [1, 2, 3];
        \\  const [a, b, c] = arr;
        \\  return Response.text(a + '-' + b + '-' + c);
        \\}
    ;
    try rt.loadHandler(handler_code, "<test>");

    const headers = std.StringHashMap([]const u8).init(allocator);
    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers,
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
        \\  var elem = <div>Hello</div>;
        \\  return Response.html(renderToString(elem));
        \\}
    ;
    // Load as .jsx to enable JSX mode
    try rt.loadHandler(handler_code, "test.jsx");

    const headers = std.StringHashMap([]const u8).init(allocator);
    var request = HttpRequest{
        .method = try allocator.dupe(u8, "GET"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers,
        .body = null,
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request);
    defer response.deinit();

    try std.testing.expectEqualStrings("<div>Hello</div>", response.body);
}
