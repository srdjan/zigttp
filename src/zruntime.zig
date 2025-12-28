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

    /// Install native API bindings (console, Response helpers, etc.)
    fn installBindings(self: *Self) !void {
        // Use predefined handler atom
        self.handler_atom = zq.Atom.handler;

        // Install console object
        try self.installConsole();

        // Install Response helpers
        try self.installResponseHelpers();

        // Install JSX runtime (h, renderToString, Fragment)
        if (self.config.enable_jsx) {
            try self.installJsxRuntime();
        }
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

    fn installResponseHelpers(self: *Self) !void {
        const root_class = self.ctx.root_class orelse return error.NoRootClass;

        // Create Response object with static methods
        const response_obj = try zq.JSObject.create(self.allocator, root_class, null);

        // Response.json() - use predefined atoms
        const json_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            responseJson,
            zq.Atom.json,
            2,
        );
        try response_obj.setProperty(self.allocator, zq.Atom.json, json_func.toValue());

        // Response.text() - use predefined atoms
        const text_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            responseText,
            zq.Atom.text,
            2,
        );
        try response_obj.setProperty(self.allocator, zq.Atom.text, text_func.toValue());

        // Response.html() - use predefined atoms
        const html_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            responseHtml,
            zq.Atom.html,
            2,
        );
        try response_obj.setProperty(self.allocator, zq.Atom.html, html_func.toValue());

        // Register on global using predefined Response atom
        try self.ctx.setGlobal(zq.Atom.Response, response_obj.toValue());
    }

    fn installJsxRuntime(self: *Self) !void {
        const root_class = self.ctx.root_class orelse return error.NoRootClass;

        // h(tag, props, ...children) - creates virtual DOM node using predefined atom
        const h_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            jsxH,
            zq.Atom.h,
            2,
        );
        try self.ctx.setGlobal(zq.Atom.h, h_func.toValue());

        // renderToString(node) - renders virtual DOM to HTML using predefined atom
        const render_func = try zq.JSObject.createNativeFunction(
            self.allocator,
            root_class,
            jsxRenderToString,
            zq.Atom.renderToString,
            1,
        );
        try self.ctx.setGlobal(zq.Atom.renderToString, render_func.toValue());

        // Fragment constant using predefined atom
        const fragment_str = try zq.createString(self.allocator, "__fragment__");
        try self.ctx.setGlobal(zq.Atom.Fragment, zq.JSValue.fromPtr(fragment_str));
    }

    /// Load and compile JavaScript code
    pub fn loadCode(self: *Self, code: []const u8, filename: []const u8) !void {
        _ = filename;

        // Parse the source code
        var p = zq.Parser.init(self.allocator, code, &self.strings, &self.ctx.atoms);
        defer p.deinit();

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
        // Debug logging
        std.log.debug("callFunction: class_id={} is_callable={} slot0_isPtr={} slot1_isUndefined={}", .{
            @intFromEnum(func_obj.class_id),
            func_obj.flags.is_callable,
            func_obj.inline_slots[0].isPtr(),
            func_obj.inline_slots[1].isUndefined(),
        });

        // Get bytecode function data
        const bc_data = func_obj.getBytecodeFunctionData() orelse {
            // Try native function
            std.log.debug("callFunction: no bytecode data, trying native", .{});
            const native_data = func_obj.getNativeFunctionData() orelse {
                std.log.err("callFunction: no native data either, NotCallable. slot0_raw={x} slot1_raw={x}", .{
                    func_obj.inline_slots[0].raw,
                    func_obj.inline_slots[1].raw,
                });
                return error.NotCallable;
            };
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

fn responseJson(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return zq.JSValue.undefined_val;

    const root_class = ctx.root_class orelse return error.NoRootClass;
    const resp_obj = try zq.JSObject.create(ctx.allocator, root_class, null);

    // Set status 200 using predefined atom
    try resp_obj.setProperty(ctx.allocator, zq.Atom.status, zq.JSValue.fromInt(200));

    // Set Content-Type header
    const headers_obj = try zq.JSObject.create(ctx.allocator, root_class, null);
    const ct_atom = try ctx.atoms.intern("Content-Type");
    const ct_val = try zq.createString(ctx.allocator, "application/json");
    try headers_obj.setProperty(ctx.allocator, ct_atom, zq.JSValue.fromPtr(ct_val));
    try resp_obj.setProperty(ctx.allocator, zq.Atom.headers, headers_obj.toValue());

    // Stringify body (simplified - just store the value for now)
    try resp_obj.setProperty(ctx.allocator, zq.Atom.body, args[0]);

    return resp_obj.toValue();
}

fn responseText(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return zq.JSValue.undefined_val;

    const root_class = ctx.root_class orelse return error.NoRootClass;
    const resp_obj = try zq.JSObject.create(ctx.allocator, root_class, null);

    // Set status 200 using predefined atom
    try resp_obj.setProperty(ctx.allocator, zq.Atom.status, zq.JSValue.fromInt(200));

    // Set Content-Type header
    const headers_obj = try zq.JSObject.create(ctx.allocator, root_class, null);
    const ct_atom = try ctx.atoms.intern("Content-Type");
    const ct_val = try zq.createString(ctx.allocator, "text/plain");
    try headers_obj.setProperty(ctx.allocator, ct_atom, zq.JSValue.fromPtr(ct_val));
    try resp_obj.setProperty(ctx.allocator, zq.Atom.headers, headers_obj.toValue());

    // Set body using predefined atom
    try resp_obj.setProperty(ctx.allocator, zq.Atom.body, args[0]);

    return resp_obj.toValue();
}

fn responseHtml(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return zq.JSValue.undefined_val;

    const root_class = ctx.root_class orelse return error.NoRootClass;
    const resp_obj = try zq.JSObject.create(ctx.allocator, root_class, null);

    // Set status 200 using predefined atom
    try resp_obj.setProperty(ctx.allocator, zq.Atom.status, zq.JSValue.fromInt(200));

    // Set Content-Type header
    const headers_obj = try zq.JSObject.create(ctx.allocator, root_class, null);
    const ct_atom = try ctx.atoms.intern("Content-Type");
    const ct_val = try zq.createString(ctx.allocator, "text/html");
    try headers_obj.setProperty(ctx.allocator, ct_atom, zq.JSValue.fromPtr(ct_val));
    try resp_obj.setProperty(ctx.allocator, zq.Atom.headers, headers_obj.toValue());

    // Set body using predefined atom
    try resp_obj.setProperty(ctx.allocator, zq.Atom.body, args[0]);

    return resp_obj.toValue();
}

// ============================================================================
// JSX Runtime Functions
// ============================================================================

/// h(tag, props, ...children) - creates a virtual DOM node
/// Returns: { tag: string, props: object, children: array }
fn jsxH(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    const root_class = ctx.root_class orelse return error.NoRootClass;

    // Create the virtual DOM node object
    const node = try zq.JSObject.create(ctx.allocator, root_class, null);

    // Set tag (first argument) using predefined atom
    if (args.len > 0) {
        // Debug: log tag value when it's a component function
        if (args[0].isCallable()) {
            const func_obj = args[0].toPtr(zq.JSObject);
            std.log.debug("jsxH: storing component tag, class_id={} is_callable={} slot0_isPtr={} slot1_isUndefined={}", .{
                @intFromEnum(func_obj.class_id),
                func_obj.flags.is_callable,
                func_obj.inline_slots[0].isPtr(),
                func_obj.inline_slots[1].isUndefined(),
            });
        }
        try node.setProperty(ctx.allocator, zq.Atom.tag, args[0]);
    } else {
        try node.setProperty(ctx.allocator, zq.Atom.tag, zq.JSValue.null_val);
    }

    // Set props (second argument, default to empty object) using predefined atom
    if (args.len > 1 and !args[1].isNull() and !args[1].isUndefined()) {
        try node.setProperty(ctx.allocator, zq.Atom.props, args[1]);
    } else {
        const empty_props = try zq.JSObject.create(ctx.allocator, root_class, null);
        try node.setProperty(ctx.allocator, zq.Atom.props, empty_props.toValue());
    }

    // Set children (remaining arguments as array) using predefined atom
    const children_arr = try zq.JSObject.createArray(ctx.allocator, root_class);
    var child_idx: u32 = 0;
    for (args[2..]) |child| {
        if (!child.isNull() and !child.isUndefined()) {
            try children_arr.setIndex(ctx.allocator, child_idx, child);
            child_idx += 1;
        }
    }
    try node.setProperty(ctx.allocator, zq.Atom.children, children_arr.toValue());

    return node.toValue();
}

/// renderToString(node) - renders virtual DOM to HTML string
fn jsxRenderToString(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) {
        const empty = try zq.createString(ctx.allocator, "");
        return zq.JSValue.fromPtr(empty);
    }

    var buffer: std.ArrayList(u8) = .empty;
    defer buffer.deinit(ctx.allocator);

    try renderNode(ctx, ctx.allocator, args[0], &buffer);

    const result = try zq.createString(ctx.allocator, buffer.items);
    return zq.JSValue.fromPtr(result);
}

fn renderNode(ctx: *zq.Context, allocator: std.mem.Allocator, node: zq.JSValue, buffer: *std.ArrayList(u8)) !void {
    // null/undefined -> empty string
    if (node.isNull() or node.isUndefined()) return;

    // String -> escape and append
    if (node.isString()) {
        const str = node.toPtr(zq.JSString);
        try escapeHtml(allocator, buffer, str.data());
        return;
    }

    // Number -> convert to string
    if (node.isInt()) {
        var num_buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&num_buf, "{d}", .{node.getInt()}) catch return;
        try buffer.appendSlice(allocator, s);
        return;
    }

    // Boolean -> empty string
    if (node.isTrue() or node.isFalse()) return;

    // Object (virtual DOM node or array)
    if (node.isObject()) {
        const obj = node.toPtr(zq.JSObject);

        // Check if it's an array (render each element)
        if (obj.isArray()) {
            const len = obj.getArrayLength();
            for (0..len) |i| {
                if (obj.getIndex(@intCast(i))) |child| {
                    try renderNode(ctx, allocator, child, buffer);
                }
            }
            return;
        }

        // Virtual DOM node: { tag, props, children } - use predefined atoms
        const tag_val = obj.getOwnProperty(zq.Atom.tag) orelse return;

        // Handle component functions (tag is callable)
        if (tag_val.isCallable()) {
            const runtime = current_runtime orelse return error.NoRuntime;

            // Debug: log tag value details
            const func_obj = tag_val.toPtr(zq.JSObject);
            std.log.debug("renderNode: component tag is callable, class_id={} is_callable={}", .{
                @intFromEnum(func_obj.class_id),
                func_obj.flags.is_callable,
            });

            // Get props (or null if not present)
            const props_val = obj.getOwnProperty(zq.Atom.props) orelse zq.JSValue.null_val;

            // Get children and add to props if present
            const call_props = props_val;
            if (obj.getOwnProperty(zq.Atom.children)) |children_val| {
                // If props is an object, add children to it
                if (props_val.isObject()) {
                    const props_obj = props_val.toPtr(zq.JSObject);
                    props_obj.setProperty(allocator, zq.Atom.children, children_val) catch {};
                }
            }

            // Call the component function with props
            const call_args = [_]zq.JSValue{call_props};
            const component_result = runtime.callFunction(func_obj, &call_args) catch |err| {
                std.log.err("Component function call failed: {}", .{err});
                return;
            };

            // Recursively render the result
            try renderNode(ctx, allocator, component_result, buffer);
            return;
        }

        // Check for Fragment
        if (tag_val.isString()) {
            const tag_str = tag_val.toPtr(zq.JSString);
            if (std.mem.eql(u8, tag_str.data(), "__fragment__")) {
                // Fragment: just render children
                if (obj.getOwnProperty(zq.Atom.children)) |children| {
                    try renderNode(ctx, allocator, children, buffer);
                }
                return;
            }

            // Regular HTML tag
            const tag = tag_str.data();
            try buffer.appendSlice(allocator, "<");
            try buffer.appendSlice(allocator, tag);

            // Render props/attributes
            if (obj.getOwnProperty(zq.Atom.props)) |props_val| {
                if (props_val.isObject()) {
                    try renderProps(ctx, allocator, props_val.toPtr(zq.JSObject), buffer);
                }
            }

            // Void elements
            const void_elements = [_][]const u8{
                "area",  "base", "br",   "col",   "embed",  "hr",    "img",
                "input", "link", "meta", "param", "source", "track", "wbr",
            };
            for (void_elements) |ve| {
                if (std.mem.eql(u8, tag, ve)) {
                    try buffer.appendSlice(allocator, " />");
                    return;
                }
            }

            try buffer.appendSlice(allocator, ">");

            // Render children
            if (obj.getOwnProperty(zq.Atom.children)) |children| {
                try renderNode(ctx, allocator, children, buffer);
            }

            try buffer.appendSlice(allocator, "</");
            try buffer.appendSlice(allocator, tag);
            try buffer.appendSlice(allocator, ">");
        }
    }
}

fn renderProps(ctx: *zq.Context, allocator: std.mem.Allocator, props: *zq.JSObject, buffer: *std.ArrayList(u8)) !void {
    // Iterate over properties
    var iter = props.propertyIterator();
    while (iter.next()) |entry| {
        const name = ctx.atoms.getName(entry.atom) orelse continue;
        const val = entry.value;

        // Skip null/undefined/false values
        if (val.isNull() or val.isUndefined() or val.isFalse()) continue;

        // Skip event handlers (start with "on")
        if (name.len >= 2 and name[0] == 'o' and name[1] == 'n') continue;

        // Boolean true -> just attribute name
        if (val.isTrue()) {
            try buffer.appendSlice(allocator, " ");
            try buffer.appendSlice(allocator, name);
            continue;
        }

        // className -> class
        const attr_name = if (std.mem.eql(u8, name, "className")) "class" else name;

        try buffer.appendSlice(allocator, " ");
        try buffer.appendSlice(allocator, attr_name);
        try buffer.appendSlice(allocator, "=\"");

        // Value as string
        if (val.isString()) {
            const str = val.toPtr(zq.JSString);
            try escapeAttr(allocator, buffer, str.data());
        } else if (val.isInt()) {
            var num_buf: [32]u8 = undefined;
            const s = std.fmt.bufPrint(&num_buf, "{d}", .{val.getInt()}) catch continue;
            try buffer.appendSlice(allocator, s);
        }

        try buffer.appendSlice(allocator, "\"");
    }
}

fn escapeHtml(allocator: std.mem.Allocator, buffer: *std.ArrayList(u8), str: []const u8) !void {
    for (str) |c| {
        switch (c) {
            '&' => try buffer.appendSlice(allocator, "&amp;"),
            '<' => try buffer.appendSlice(allocator, "&lt;"),
            '>' => try buffer.appendSlice(allocator, "&gt;"),
            else => try buffer.append(allocator, c),
        }
    }
}

fn escapeAttr(allocator: std.mem.Allocator, buffer: *std.ArrayList(u8), str: []const u8) !void {
    for (str) |c| {
        switch (c) {
            '&' => try buffer.appendSlice(allocator, "&amp;"),
            '"' => try buffer.appendSlice(allocator, "&quot;"),
            else => try buffer.append(allocator, c),
        }
    }
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
    var pool = try RuntimePool.init(allocator, .{}, handler_code, 2);
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

    const handler_code =
        "function handler(req){ var parts = req.body.split('&'); return Response.text('' + parts.length); }";
    try rt.loadHandler(handler_code, "<test>");

    const headers = std.StringHashMap([]const u8).init(allocator);
    var request = HttpRequest{
        .method = try allocator.dupe(u8, "POST"),
        .url = try allocator.dupe(u8, "/"),
        .headers = headers,
        .body = try allocator.dupe(u8, "a=1&b=2"),
    };
    defer request.deinit(allocator);

    var response = try rt.executeHandler(request);
    defer response.deinit();

    try std.testing.expectEqualStrings("2", response.body);
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
