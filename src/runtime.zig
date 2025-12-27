//! JavaScript Runtime - Higher-level wrapper around mquickjs
//!
//! Architecture:
//! - RuntimePool: manages multiple isolated JS contexts
//! - Runtime: single JS execution context with event loop
//! - Per-request isolation: each request gets a fresh or pooled context
//!
//! Deno-like APIs:
//! - fetch(), Deno.readTextFile(), console.log(), etc.

const std = @import("std");
const mq = @import("mquickjs.zig");
const EventLoop = @import("event_loop.zig").EventLoop;
const bindings = @import("bindings.zig");

// ============================================================================
// Runtime Configuration
// ============================================================================

pub const RuntimeConfig = struct {
    /// Memory limit per context in bytes (0 = no limit, use system default)
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
};

// ============================================================================
// Runtime (Single Context)
// ============================================================================

pub const Runtime = struct {
    ctx: *mq.JSContext,
    config: RuntimeConfig,
    mem_buffer: []u8,
    allocator: std.mem.Allocator,
    event_loop: EventLoop,

    /// Handler code (precompiled)
    handler_loaded: bool = false,

    const Self = @This();

    // ------------------------------------------------------------------------
    // Lifecycle
    // ------------------------------------------------------------------------

    pub fn init(allocator: std.mem.Allocator, config: RuntimeConfig) !Self {
        // Allocate memory buffer for mquickjs
        const mem_size = if (config.memory_limit > 0) config.memory_limit else 512 * 1024;
        const mem_buffer = try allocator.alloc(u8, mem_size);
        errdefer allocator.free(mem_buffer);

        // Create JS context
        const ctx_result = mq.newContext(mem_buffer, null);
        const ctx = switch (ctx_result) {
            .ok => |c| c,
            .err => {
                allocator.free(mem_buffer);
                return error.ContextCreationFailed;
            },
        };

        var self = Self{
            .ctx = ctx,
            .config = config,
            .mem_buffer = mem_buffer,
            .allocator = allocator,
            .event_loop = EventLoop.init(allocator, ctx),
        };

        // Install bindings
        try self.installBindings();

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.event_loop.deinit();
        mq.freeContext(self.ctx);
        self.allocator.free(self.mem_buffer);
    }

    /// Reset runtime state for per-request isolation (CRITICAL for FaaS security)
    /// Clears all user-defined globals while preserving built-in APIs
    pub fn resetForNextRequest(self: *Self) !void {
        // Clear any pending async operations from previous request
        self.event_loop.clear();

        // Clear user-defined global variables while preserving built-ins
        try self.clearUserGlobals();
    }

    /// Clear user-defined global variables, keeping only protected built-ins
    /// Uses JavaScript evaluation for portability across JS engine implementations
    fn clearUserGlobals(self: *Self) !void {
        // JavaScript code to clear all user-defined globals while preserving built-ins
        // This runs in the JS context and uses Object.keys + delete
        const clear_globals_code =
            \\(function() {
            \\    var protected = {
            \\        'handler': true, 'Response': true, 'Request': true, 'console': true,
            \\        'Deno': true, 'h': true, 'renderToString': true, 'Fragment': true,
            \\        'globalThis': true, 'Object': true, 'Array': true, 'String': true,
            \\        'Number': true, 'Boolean': true, 'Function': true, 'Math': true,
            \\        'JSON': true, 'Promise': true, 'Date': true, 'Error': true,
            \\        'TypeError': true, 'RangeError': true, 'SyntaxError': true,
            \\        'ReferenceError': true, 'parseInt': true, 'parseFloat': true,
            \\        'isNaN': true, 'isFinite': true, 'encodeURI': true, 'decodeURI': true,
            \\        'encodeURIComponent': true, 'decodeURIComponent': true,
            \\        'undefined': true, 'NaN': true, 'Infinity': true, 'escape': true,
            \\        'unescape': true, 'fetch': true, 'setTimeout': true, 'setInterval': true,
            \\        'clearTimeout': true, 'clearInterval': true, 'escapeHtml': true,
            \\        'escapeAttr': true, 'renderStyle': true, 'eval': true, 'print': true,
            \\        'gc': true, 'Symbol': true, 'Map': true, 'Set': true, 'WeakMap': true,
            \\        'WeakSet': true, 'Proxy': true, 'Reflect': true, 'ArrayBuffer': true,
            \\        'DataView': true, 'Int8Array': true, 'Uint8Array': true,
            \\        'Int16Array': true, 'Uint16Array': true, 'Int32Array': true,
            \\        'Uint32Array': true, 'Float32Array': true, 'Float64Array': true,
            \\        'RegExp': true, 'URIError': true, 'EvalError': true
            \\    };
            \\    var keys = Object.keys(globalThis);
            \\    for (var i = 0; i < keys.length; i++) {
            \\        var key = keys[i];
            \\        if (!protected[key]) {
            \\            try { delete globalThis[key]; } catch(e) {}
            \\        }
            \\    }
            \\})();
        ;

        // Execute the cleanup code
        const result = mq.eval(self.ctx, clear_globals_code, "<runtime-cleanup>", .{});
        if (result == .err) {
            std.log.warn("Failed to clear user globals: {s}", .{result.err.message orelse "unknown"});
            return error.GlobalCleanupFailed;
        }
    }

    // ------------------------------------------------------------------------
    // Binding Installation
    // ------------------------------------------------------------------------

    fn installBindings(self: *Self) !void {
        // Set thread-local loop reference for native functions
        bindings.setCurrentLoop(&self.event_loop, self.allocator);

        // Install all native APIs
        bindings.installAllBindings(self.ctx) catch |err| {
            std.log.err("Failed to install bindings: {}", .{err});
            return error.BindingsFailed;
        };

        // Install Response helpers for HTTP handlers
        try self.installResponseHelpers();

        // Install JSX runtime if enabled
        if (self.config.enable_jsx) {
            self.installJsxRuntime() catch |err| {
                std.log.err("Failed to install JSX runtime: {}", .{err});
                return err;
            };
        }
    }

    fn installResponseHelpers(self: *Self) !void {
        const helpers_code =
            \\// Response class for HTTP handlers
            \\var Response = (function() {
            \\    function Response(body, init) {
            \\        init = init || {};
            \\        this.status = init.status || 200;
            \\        this.statusText = init.statusText || 'OK';
            \\        this.headers = init.headers || {};
            \\        this._body = body || '';
            \\        this.ok = this.status >= 200 && this.status < 300;
            \\    }
            \\    
            \\    Response.prototype.text = function() {
            \\        var self = this;
            \\        return Promise.resolve(self._body);
            \\    };
            \\    
            \\    Response.prototype.json = function() {
            \\        var self = this;
            \\        return Promise.resolve(JSON.parse(self._body));
            \\    };
            \\    
            \\    Response.json = function(data, init) {
            \\        init = init || {};
            \\        init.headers = init.headers || {};
            \\        init.headers['Content-Type'] = 'application/json';
            \\        return new Response(JSON.stringify(data), init);
            \\    };
            \\    
            \\    Response.text = function(text, init) {
            \\        init = init || {};
            \\        init.headers = init.headers || {};
            \\        init.headers['Content-Type'] = 'text/plain; charset=utf-8';
            \\        return new Response(text, init);
            \\    };
            \\    
            \\    Response.html = function(html, init) {
            \\        init = init || {};
            \\        init.headers = init.headers || {};
            \\        init.headers['Content-Type'] = 'text/html; charset=utf-8';
            \\        return new Response(html, init);
            \\    };
            \\    
            \\    Response.redirect = function(url, status) {
            \\        return new Response('', {
            \\            status: status || 302,
            \\            headers: { 'Location': url }
            \\        });
            \\    };
            \\    
            \\    return Response;
            \\})();
            \\
            \\// Request class for HTTP handlers  
            \\var Request = (function() {
            \\    function Request(url, init) {
            \\        init = init || {};
            \\        this.url = url;
            \\        this.method = (init.method || 'GET').toUpperCase();
            \\        this.headers = init.headers || {};
            \\        this._body = init.body || null;
            \\    }
            \\    
            \\    Request.prototype.text = function() {
            \\        return Promise.resolve(this._body || '');
            \\    };
            \\    
            \\    Request.prototype.json = function() {
            \\        var body = this._body;
            \\        return Promise.resolve(body ? JSON.parse(body) : null);
            \\    };
            \\    
            \\    return Request;
            \\})();
            \\
        ;

        const result = mq.eval(self.ctx, helpers_code, "<runtime>", .{});
        if (result == .err) {
            std.log.err("Failed to install Response helpers", .{});
            return error.HelperInstallFailed;
        }
    }

    fn installJsxRuntime(self: *Self) !void {
        const jsx_runtime_code =
            \\// JSX Runtime for Server-Side Rendering
            \\// Provides h(), renderToString(), and Fragment
            \\
            \\var Fragment = '__fragment__';
            \\
            \\function h(tag, props) {
            \\    var children = [];
            \\    for (var i = 2; i < arguments.length; i++) {
            \\        var child = arguments[i];
            \\        if (child === null || child === undefined) continue;
            \\        if (Array.isArray(child)) {
            \\            for (var j = 0; j < child.length; j++) {
            \\                if (child[j] !== null && child[j] !== undefined) {
            \\                    children.push(child[j]);
            \\                }
            \\            }
            \\        } else {
            \\            children.push(child);
            \\        }
            \\    }
            \\    return { tag: tag, props: props || {}, children: children };
            \\}
            \\
            \\function renderToString(node) {
            \\    if (node === null || node === undefined) return '';
            \\    if (typeof node === 'string') return escapeHtml(node);
            \\    if (typeof node === 'number') return String(node);
            \\    if (typeof node === 'boolean') return '';
            \\
            \\    if (Array.isArray(node)) {
            \\        var result = '';
            \\        for (var i = 0; i < node.length; i++) {
            \\            result += renderToString(node[i]);
            \\        }
            \\        return result;
            \\    }
            \\
            \\    if (typeof node === 'object' && node.tag !== undefined) {
            \\        var tag = node.tag;
            \\        var props = node.props;
            \\        var children = node.children;
            \\
            \\        // Fragment: just render children
            \\        if (tag === Fragment) {
            \\            return renderToString(children);
            \\        }
            \\
            \\        // Component function
            \\        if (typeof tag === 'function') {
            \\            var componentProps = {};
            \\            for (var key in props) {
            \\                componentProps[key] = props[key];
            \\            }
            \\            if (children.length > 0) {
            \\                componentProps.children = children.length === 1 ? children[0] : children;
            \\            }
            \\            return renderToString(tag(componentProps));
            \\        }
            \\
            \\        // HTML element
            \\        var html = '<' + tag;
            \\
            \\        // Render attributes
            \\        for (var name in props) {
            \\            var value = props[name];
            \\            if (value === false || value === null || value === undefined) continue;
            \\            if (value === true) {
            \\                html += ' ' + name;
            \\            } else if (name === 'style' && typeof value === 'object') {
            \\                html += ' style="' + renderStyle(value) + '"';
            \\            } else if (name === 'className') {
            \\                html += ' class="' + escapeAttr(String(value)) + '"';
            \\            } else if (name.indexOf('on') !== 0) {
            \\                // Skip event handlers for SSR
            \\                html += ' ' + name + '="' + escapeAttr(String(value)) + '"';
            \\            }
            \\        }
            \\
            \\        // Void elements
            \\        var voidElements = ['area', 'base', 'br', 'col', 'embed', 'hr', 'img',
            \\                           'input', 'link', 'meta', 'param', 'source', 'track', 'wbr'];
            \\        var isVoid = false;
            \\        for (var i = 0; i < voidElements.length; i++) {
            \\            if (voidElements[i] === tag) { isVoid = true; break; }
            \\        }
            \\
            \\        if (isVoid) return html + ' />';
            \\
            \\        html += '>';
            \\
            \\        // Handle dangerouslySetInnerHTML
            \\        if (props.dangerouslySetInnerHTML && props.dangerouslySetInnerHTML.__html) {
            \\            html += props.dangerouslySetInnerHTML.__html;
            \\        } else {
            \\            html += renderToString(children);
            \\        }
            \\
            \\        return html + '</' + tag + '>';
            \\    }
            \\
            \\    return '';
            \\}
            \\
            \\function escapeHtml(str) {
            \\    return String(str)
            \\        .replace(/&/g, '&amp;')
            \\        .replace(/</g, '&lt;')
            \\        .replace(/>/g, '&gt;');
            \\}
            \\
            \\function escapeAttr(str) {
            \\    return String(str)
            \\        .replace(/&/g, '&amp;')
            \\        .replace(/"/g, '&quot;');
            \\}
            \\
            \\function renderStyle(obj) {
            \\    var parts = [];
            \\    for (var key in obj) {
            \\        var cssKey = key.replace(/([A-Z])/g, '-$1').toLowerCase();
            \\        parts.push(cssKey + ': ' + obj[key]);
            \\    }
            \\    return parts.join('; ');
            \\}
            \\
        ;

        const result = mq.eval(self.ctx, jsx_runtime_code, "<jsx-runtime>", .{});
        if (result == .err) {
            std.log.err("Failed to install JSX runtime", .{});
            return error.JsxRuntimeInstallFailed;
        }
    }

    // ------------------------------------------------------------------------
    // Code Execution
    // ------------------------------------------------------------------------

    /// Load handler code (called once at startup)
    pub fn loadHandler(self: *Self, code: []const u8, filename: [:0]const u8) !void {
        bindings.setCurrentLoop(&self.event_loop, self.allocator);
        defer bindings.clearCurrentLoop();

        const result = mq.eval(self.ctx, code, filename, .{});
        switch (result) {
            .ok => {
                self.handler_loaded = true;
            },
            .err => |e| {
                std.log.err("Handler load error: {s}", .{e.message orelse "unknown"});
                return error.HandlerLoadFailed;
            },
        }

        // Verify handler function exists
        const global = mq.getGlobalObject(self.ctx);
        const handler_fn = mq.getPropertyStr(self.ctx, global, "handler");
        switch (handler_fn) {
            .ok => |f| {
                if (!mq.isFunction(self.ctx, f)) {
                    return error.HandlerNotFunction;
                }
            },
            .err => {
                // Check for default export
                const default_fn = mq.getPropertyStr(self.ctx, global, "default");
                switch (default_fn) {
                    .ok => |f| {
                        if (mq.isFunction(self.ctx, f)) {
                            // Copy default to handler
                            _ = mq.setPropertyStr(self.ctx, global, "handler", f);
                            return;
                        }
                    },
                    .err => {},
                }
                return error.HandlerNotDefined;
            },
        }
    }

    /// Evaluate arbitrary code
    pub fn eval(self: *Self, code: []const u8, filename: [:0]const u8) mq.Result(mq.JSValue) {
        bindings.setCurrentLoop(&self.event_loop, self.allocator);
        defer bindings.clearCurrentLoop();

        return mq.eval(self.ctx, code, filename, .{});
    }

    /// Call a global function
    pub fn callGlobal(self: *Self, func_name: [:0]const u8, args: []const mq.JSValue) mq.Result(mq.JSValue) {
        bindings.setCurrentLoop(&self.event_loop, self.allocator);
        defer bindings.clearCurrentLoop();

        const global = mq.getGlobalObject(self.ctx);
        const func_result = mq.getPropertyStr(self.ctx, global, func_name);

        const func = switch (func_result) {
            .ok => |f| f,
            .err => |e| return .{ .err = e },
        };

        if (!mq.isFunction(self.ctx, func)) {
            return .{ .err = .{ .kind = .type_error, .message = "not a function" } };
        }

        return mq.call(self.ctx, func, mq.undefined_(), args);
    }

    /// Invoke the handler function with a request object
    pub fn invokeHandler(self: *Self, request: HttpRequest) !HttpResponse {
        bindings.setCurrentLoop(&self.event_loop, self.allocator);
        defer bindings.clearCurrentLoop();

        // Create Request object
        const req_obj = try self.createRequestObject(request);

        // Call handler(request)
        const args = [_]mq.JSValue{req_obj};
        const result = self.callGlobal("handler", &args);

        // Run event loop to completion (for async handlers)
        self.event_loop.runUntilComplete() catch |err| {
            std.log.err("Event loop error: {}", .{err});
        };

        return switch (result) {
            .ok => |val| try self.extractResponse(val),
            .err => |e| {
                std.log.err("Handler error: {s}", .{e.message orelse "unknown"});
                return HttpResponse{
                    .status = 500,
                    .headers = std.StringHashMap([]const u8).init(self.allocator),
                    .body = "Internal Server Error",
                    .allocator = null, // No allocated memory in error response
                };
            },
        };
    }

    // ------------------------------------------------------------------------
    // Request/Response Conversion
    // ------------------------------------------------------------------------

    fn createRequestObject(self: *Self, request: HttpRequest) !mq.JSValue {
        const obj = switch (mq.newObject(self.ctx)) {
            .ok => |o| o,
            .err => return error.ObjectCreationFailed,
        };

        // URL
        const url = mq.fromString(self.ctx, request.url).unwrapOr(mq.undefined_());
        _ = mq.setPropertyStr(self.ctx, obj, "url", url);

        // Method
        const method = mq.fromString(self.ctx, request.method).unwrapOr(mq.undefined_());
        _ = mq.setPropertyStr(self.ctx, obj, "method", method);

        // Headers
        const headers_obj = mq.newObject(self.ctx).unwrapOr(mq.undefined_());
        var iter = request.headers.iterator();
        while (iter.next()) |entry| {
            const value = mq.fromString(self.ctx, entry.value_ptr.*).unwrapOr(mq.undefined_());

            // Need null-terminated key
            var key_buf: [256]u8 = undefined;
            const key = entry.key_ptr.*;
            if (key.len < key_buf.len) {
                @memcpy(key_buf[0..key.len], key);
                key_buf[key.len] = 0;
                _ = mq.setPropertyStr(self.ctx, headers_obj, key_buf[0..key.len :0], value);
            }
        }
        _ = mq.setPropertyStr(self.ctx, obj, "headers", headers_obj);

        // Body
        if (request.body) |body| {
            const body_val = mq.fromString(self.ctx, body).unwrapOr(mq.null_());
            _ = mq.setPropertyStr(self.ctx, obj, "body", body_val);
            _ = mq.setPropertyStr(self.ctx, obj, "_body", body_val);
        } else {
            _ = mq.setPropertyStr(self.ctx, obj, "body", mq.null_());
            _ = mq.setPropertyStr(self.ctx, obj, "_body", mq.null_());
        }

        return obj;
    }

    fn extractResponse(self: *Self, val: mq.JSValue) !HttpResponse {
        var response = HttpResponse{
            .status = 200,
            .headers = std.StringHashMap([]const u8).init(self.allocator),
            .body = "",
            .allocator = self.allocator,
        };

        // Check if it's a Response object
        if (mq.isObject(self.ctx, val)) {
            // Extract status
            const status_val = mq.getPropertyStr(self.ctx, val, "status");
            if (status_val == .ok) {
                response.status = @intCast(mq.toInt32(self.ctx, status_val.ok).unwrapOr(200));
            }

            // Extract body
            const body_val = mq.getPropertyStr(self.ctx, val, "_body");
            if (body_val == .ok and mq.isString(self.ctx, body_val.ok)) {
                const body_str = mq.toCString(self.ctx, body_val.ok);
                if (body_str == .ok) {
                    response.body = try self.allocator.dupe(u8, body_str.ok);
                }
            }

            // Extract headers
            const headers_val = mq.getPropertyStr(self.ctx, val, "headers");
            if (headers_val == .ok and mq.isObject(self.ctx, headers_val.ok)) {
                // Try to get common headers
                inline for (.{ "Content-Type", "Location", "Set-Cookie", "Cache-Control" }) |header| {
                    const h = mq.getPropertyStr(self.ctx, headers_val.ok, header);
                    if (h == .ok and mq.isString(self.ctx, h.ok)) {
                        const v = mq.toCString(self.ctx, h.ok);
                        if (v == .ok) {
                            // Duplicate the string to own the memory (toCString returns temp pointer)
                            const v_dup = try self.allocator.dupe(u8, v.ok);
                            errdefer self.allocator.free(v_dup);
                            try response.headers.put(header, v_dup);
                        }
                    }
                }
            }
        } else if (mq.isString(self.ctx, val)) {
            // Plain string response
            const str = mq.toCString(self.ctx, val);
            if (str == .ok) {
                response.body = try self.allocator.dupe(u8, str.ok);
                try response.headers.put("Content-Type", "text/plain; charset=utf-8");
            }
        }

        return response;
    }

    // ------------------------------------------------------------------------
    // Value Creation Helpers
    // ------------------------------------------------------------------------

    pub fn createString(self: *Self, str: []const u8) mq.Result(mq.JSValue) {
        return mq.fromString(self.ctx, str);
    }

    pub fn createObject(self: *Self) mq.Result(mq.JSValue) {
        return mq.newObject(self.ctx);
    }

    pub fn createArray(self: *Self) mq.Result(mq.JSValue) {
        return mq.newArray(self.ctx);
    }

    // ------------------------------------------------------------------------
    // Memory Stats
    // ------------------------------------------------------------------------

    pub fn getMemoryUsage(self: *Self) mq.MemoryUsage {
        return mq.getMemoryUsage(self.ctx);
    }
};

// ============================================================================
// HTTP Types
// ============================================================================

pub const HttpRequest = struct {
    url: []const u8,
    method: []const u8,
    headers: std.StringHashMap([]const u8),
    body: ?[]const u8,
};

pub const HttpResponse = struct {
    status: u16,
    headers: std.StringHashMap([]const u8),
    body: []const u8,
    allocator: ?std.mem.Allocator = null,

    pub fn deinit(self: *HttpResponse) void {
        // Free header values if we have an allocator
        if (self.allocator) |alloc| {
            var iter = self.headers.iterator();
            while (iter.next()) |entry| {
                // Only free dynamically allocated values (not compile-time strings)
                alloc.free(@constCast(entry.value_ptr.*));
            }
            // Free body if allocated
            if (self.body.len > 0) {
                alloc.free(@constCast(self.body));
            }
        }
        self.headers.deinit();
    }
};

// ============================================================================
// Runtime Pool (Per-Request Isolation)
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
            self.allocator.destroy(rt);
        }
        for (self.in_use.items) |rt| {
            rt.deinit();
            self.allocator.destroy(rt);
        }
        self.available.deinit(self.allocator);
        self.in_use.deinit(self.allocator);
    }

    fn createRuntime(self: *Self) !*Runtime {
        const rt = try self.allocator.create(Runtime);
        errdefer self.allocator.destroy(rt);

        rt.* = try Runtime.init(self.allocator, self.config);
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

        // Pool exhausted
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

        // CRITICAL: Reset runtime state for per-request isolation (FaaS security)
        // This prevents multi-tenant data leakage between requests
        rt.resetForNextRequest() catch {
            // If reset fails, destroy the runtime and don't return to pool
            std.log.warn("Runtime reset failed, destroying instance", .{});
            rt.deinit();
            self.allocator.destroy(rt);
            return;
        };

        self.available.append(self.allocator, rt) catch {
            rt.deinit();
            self.allocator.destroy(rt);
        };
    }

    /// Execute a handler with a pooled runtime
    pub fn executeHandler(self: *Self, request: HttpRequest) !HttpResponse {
        const rt = try self.acquire();
        defer self.release(rt);

        return try rt.invokeHandler(request);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "runtime init and deinit" {
    var rt = try Runtime.init(std.testing.allocator, .{});
    defer rt.deinit();
}

test "basic evaluation" {
    var rt = try Runtime.init(std.testing.allocator, .{});
    defer rt.deinit();

    const result = rt.eval("2 + 2", "<test>");
    try std.testing.expect(result == .ok);

    const val = mq.toInt32(rt.ctx, result.ok);
    try std.testing.expect(val == .ok);
    try std.testing.expectEqual(@as(i32, 4), val.ok);
}
