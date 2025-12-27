//! Async Event Loop for mquickjs
//!
//! Implements Promise-based async I/O by:
//! 1. JS calls async native function → returns Promise
//! 2. Native schedules work, stores resolver callbacks
//! 3. Event loop polls for completions
//! 4. Completions resolve/reject Promises
//! 5. Microtask queue drains after each completion

const std = @import("std");
const mq = @import("mquickjs.zig");

// ============================================================================
// Event Loop State
// ============================================================================

pub const EventLoop = struct {
    allocator: std.mem.Allocator,
    ctx: *mq.JSContext,

    /// Pending async operations
    pending_ops: std.ArrayList(PendingOp),

    /// Microtask queue (Promise .then callbacks)
    microtasks: std.ArrayList(Microtask),

    /// HTTP client for fetch()
    http_client: ?std.http.Client,

    /// Running flag
    running: bool,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, ctx: *mq.JSContext) Self {
        return .{
            .allocator = allocator,
            .ctx = ctx,
            .pending_ops = .empty,
            .microtasks = .empty,
            .http_client = null,
            .running = false,
        };
    }

    pub fn deinit(self: *Self) void {
        // Clean up each pending operation's allocated resources
        for (self.pending_ops.items) |*op| {
            op.cleanup(self.allocator);
        }

        if (self.http_client) |*client| {
            client.deinit();
        }
        self.pending_ops.deinit(self.allocator);
        self.microtasks.deinit(self.allocator);
    }

    /// Clear all pending operations and microtasks (for request isolation)
    pub fn clear(self: *Self) void {
        // Clean up each pending operation's allocated resources
        for (self.pending_ops.items) |*op| {
            op.cleanup(self.allocator);
        }
        self.pending_ops.clearRetainingCapacity();
        self.microtasks.clearRetainingCapacity();
        self.running = false;
    }

    /// Get or create HTTP client (lazy init)
    pub fn getHttpClient(self: *Self) *std.http.Client {
        if (self.http_client == null) {
            self.http_client = std.http.Client{ .allocator = self.allocator };
        }
        return &self.http_client.?;
    }

    /// Schedule an async operation
    pub fn scheduleOp(self: *Self, op: PendingOp) !void {
        try self.pending_ops.append(self.allocator, op);
    }

    /// Queue a microtask
    pub fn queueMicrotask(self: *Self, task: Microtask) !void {
        try self.microtasks.append(self.allocator, task);
    }

    /// Run the event loop until all operations complete
    pub fn runUntilComplete(self: *Self) !void {
        self.running = true;
        defer self.running = false;

        while (self.pending_ops.items.len > 0 or self.microtasks.items.len > 0) {
            // First, drain microtask queue
            try self.drainMicrotasks();

            // Then, poll pending operations
            if (self.pending_ops.items.len > 0) {
                try self.pollOperations();
            }
        }
    }

    /// Run one tick of the event loop (for integration with server)
    pub fn tick(self: *Self) !bool {
        // Drain microtasks
        try self.drainMicrotasks();

        // Poll one operation if any
        if (self.pending_ops.items.len > 0) {
            try self.pollOneOperation();
            return true;
        }

        return self.microtasks.items.len > 0;
    }

    fn drainMicrotasks(self: *Self) !void {
        while (self.microtasks.items.len > 0) {
            const task = self.microtasks.orderedRemove(0);
            task.execute(self.ctx);
        }
    }

    fn pollOperations(self: *Self) !void {
        var i: usize = 0;
        while (i < self.pending_ops.items.len) {
            var op = &self.pending_ops.items[i];
            const completed = try op.poll(self);

            if (completed) {
                _ = self.pending_ops.orderedRemove(i);
            } else {
                i += 1;
            }
        }
    }

    fn pollOneOperation(self: *Self) !void {
        if (self.pending_ops.items.len == 0) return;

        var op = &self.pending_ops.items[0];
        const completed = try op.poll(self);

        if (completed) {
            _ = self.pending_ops.orderedRemove(0);
        }
    }
};

// ============================================================================
// Pending Operations
// ============================================================================

pub const PendingOp = struct {
    kind: OpKind,
    state: OpState,

    /// Promise resolve/reject callbacks (stored as GC refs)
    resolve_ref: ?mq.JSGCRef,
    reject_ref: ?mq.JSGCRef,

    /// Operation-specific data
    data: OpData,

    const Self = @This();

    /// Clean up allocated resources when operation is cancelled or abandoned
    pub fn cleanup(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.kind) {
            .fetch => {
                if (self.data.fetch.body) |body| {
                    allocator.free(@constCast(body));
                }
                allocator.free(@constCast(self.data.fetch.url));
            },
            else => {},
        }
    }

    pub fn poll(self: *Self, loop: *EventLoop) !bool {
        return switch (self.kind) {
            .fetch => try self.pollFetch(loop),
            .read_file => try self.pollReadFile(loop),
            .write_file => try self.pollWriteFile(loop),
            .timeout => try self.pollTimeout(loop),
            .tcp_connect => try self.pollTcpConnect(loop),
        };
    }

    fn pollFetch(self: *Self, loop: *EventLoop) !bool {
        const fetch_data = &self.data.fetch;

        switch (self.state) {
            .pending => {
                // Start the HTTP request (one-shot)
                const client = loop.getHttpClient();
                var response_writer = std.Io.Writer.Allocating.init(loop.allocator);
                defer response_writer.deinit();

                const result = client.fetch(.{
                    .location = .{ .url = fetch_data.url },
                    .method = fetch_data.method,
                    .payload = fetch_data.body,
                    .response_writer = &response_writer.writer,
                    .keep_alive = false,
                }) catch {
                    try self.reject(loop.ctx, "Request failed");
                    if (fetch_data.body) |body| loop.allocator.free(@constCast(body));
                    loop.allocator.free(@constCast(fetch_data.url));
                    return true;
                };

                const body = response_writer.written();
                try self.resolveWithResponse(loop, @intFromEnum(result.status), body);

                if (fetch_data.body) |body_buf| loop.allocator.free(@constCast(body_buf));
                loop.allocator.free(@constCast(fetch_data.url));

                self.state = .completed;
                return true;
            },
            .in_progress => {
                return true;
            },
            .completed => return true,
        }
    }

    fn pollReadFile(self: *Self, loop: *EventLoop) !bool {
        const file_data = &self.data.read_file;

        const content = std.fs.cwd().readFileAlloc(
            loop.allocator,
            file_data.path,
            10 * 1024 * 1024,
        ) catch |err| {
            const msg = switch (err) {
                error.FileNotFound => "File not found",
                error.AccessDenied => "Access denied",
                else => "Read error",
            };
            try self.reject(loop.ctx, msg);
            return true;
        };
        defer loop.allocator.free(content);

        try self.resolveWithString(loop.ctx, content);
        return true;
    }

    fn pollWriteFile(self: *Self, loop: *EventLoop) !bool {
        const file_data = &self.data.write_file;

        std.fs.cwd().writeFile(.{
            .sub_path = file_data.path,
            .data = file_data.content,
        }) catch |err| {
            const msg = switch (err) {
                error.AccessDenied => "Access denied",
                else => "Write error",
            };
            try self.reject(loop.ctx, msg);
            return true;
        };

        try self.resolveWithUndefined(loop.ctx);
        return true;
    }

    fn pollTimeout(self: *Self, loop: *EventLoop) !bool {
        const timeout_data = &self.data.timeout;
        const now = std.time.milliTimestamp();

        if (now >= timeout_data.deadline_ms) {
            try self.resolveWithUndefined(loop.ctx);
            return true;
        }

        // Sleep efficiently based on remaining time
        const remaining_ms = timeout_data.deadline_ms - now;
        const sleep_ms: u64 = @intCast(@min(remaining_ms, 10)); // Cap at 10ms for responsiveness
        std.Thread.sleep(sleep_ms * std.time.ns_per_ms);
        return false;
    }

    fn pollTcpConnect(self: *Self, loop: *EventLoop) !bool {
        // TODO: Implement TCP socket connect
        try self.reject(loop.ctx, "TCP not yet implemented");
        return true;
    }

    fn resolveWithString(self: *Self, ctx: *mq.JSContext, str: []const u8) !void {
        if (self.resolve_ref) |*ref| {
            const resolve_ptr = mq.c.JS_PushGCRef(ctx, ref);
            if (resolve_ptr != null) {
                const str_val = mq.fromString(ctx, str);
                if (str_val == .ok) {
                    const args = [_]mq.JSValue{str_val.ok};
                    _ = mq.call(ctx, resolve_ptr.*, mq.undefined_(), &args);
                }
                _ = mq.c.JS_PopGCRef(ctx, ref);
            }
        }
    }

    fn resolveWithResponse(self: *Self, loop: *EventLoop, status: u16, body: []const u8) !void {
        const ctx = loop.ctx;

        if (self.resolve_ref) |*ref| {
            const resolve_ptr = mq.c.JS_PushGCRef(ctx, ref);
            if (resolve_ptr != null) {
                // Create Response object
                const resp_obj = mq.newObject(ctx);
                if (resp_obj == .ok) {
                    _ = mq.setPropertyStr(ctx, resp_obj.ok, "status", mq.fromInt(ctx, @intCast(status)));
                    _ = mq.setPropertyStr(ctx, resp_obj.ok, "ok", mq.fromBool(status >= 200 and status < 300));

                    const body_str = mq.fromString(ctx, body);
                    if (body_str == .ok) {
                        _ = mq.setPropertyStr(ctx, resp_obj.ok, "_body", body_str.ok);
                    }

                    // Add text() and json() methods
                    installResponseMethods(ctx, resp_obj.ok);

                    const args = [_]mq.JSValue{resp_obj.ok};
                    _ = mq.call(ctx, resolve_ptr.*, mq.undefined_(), &args);
                }
                _ = mq.c.JS_PopGCRef(ctx, ref);
            }
        }
    }

    fn resolveWithUndefined(self: *Self, ctx: *mq.JSContext) !void {
        if (self.resolve_ref) |*ref| {
            const resolve_ptr = mq.c.JS_PushGCRef(ctx, ref);
            if (resolve_ptr != null) {
                const args = [_]mq.JSValue{mq.undefined_()};
                _ = mq.call(ctx, resolve_ptr.*, mq.undefined_(), &args);
                _ = mq.c.JS_PopGCRef(ctx, ref);
            }
        }
    }

    fn reject(self: *Self, ctx: *mq.JSContext, message: []const u8) !void {
        if (self.reject_ref) |*ref| {
            const reject_ptr = mq.c.JS_PushGCRef(ctx, ref);
            if (reject_ptr != null) {
                const err_str = mq.fromString(ctx, message);
                if (err_str == .ok) {
                    const args = [_]mq.JSValue{err_str.ok};
                    _ = mq.call(ctx, reject_ptr.*, mq.undefined_(), &args);
                }
                _ = mq.c.JS_PopGCRef(ctx, ref);
            }
        }
    }
};

fn installResponseMethods(ctx: *mq.JSContext, resp: mq.JSValue) void {
    // text() method
    const text_fn = mq.newCFunction(ctx, responseText, "text", 0);
    if (text_fn == .ok) {
        _ = mq.setPropertyStr(ctx, resp, "text", text_fn.ok);
    }

    // json() method
    const json_fn = mq.newCFunction(ctx, responseJson, "json", 0);
    if (json_fn == .ok) {
        _ = mq.setPropertyStr(ctx, resp, "json", json_fn.ok);
    }
}

fn responseText(ctx_: ?*mq.JSContext, this: [*c]mq.JSValue, _: c_int, _: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    // Return Promise that resolves with body text
    const body = mq.getPropertyStr(ctx, this.*, "_body");
    if (body == .ok) {
        return createResolvedPromise(ctx, body.ok);
    }
    return createResolvedPromise(ctx, mq.fromString(ctx, "").unwrapOr(mq.undefined_()));
}

fn responseJson(ctx_: ?*mq.JSContext, this: [*c]mq.JSValue, _: c_int, _: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    // Return Promise that resolves with parsed JSON
    const body = mq.getPropertyStr(ctx, this.*, "_body");
    if (body == .ok) {
        const global = mq.getGlobalObject(ctx);
        const json = mq.getPropertyStr(ctx, global, "JSON");
        if (json == .ok) {
            const parse = mq.getPropertyStr(ctx, json.ok, "parse");
            if (parse == .ok) {
                const args = [_]mq.JSValue{body.ok};
                const result = mq.call(ctx, parse.ok, mq.undefined_(), &args);
                if (result == .ok) {
                    return createResolvedPromise(ctx, result.ok);
                }
            }
        }
    }
    return createRejectedPromise(ctx, "JSON parse error");
}

fn createResolvedPromise(ctx: *mq.JSContext, value: mq.JSValue) mq.JSValue {
    const global = mq.getGlobalObject(ctx);
    const promise_ctor = mq.getPropertyStr(ctx, global, "Promise");
    if (promise_ctor == .ok) {
        const resolve = mq.getPropertyStr(ctx, promise_ctor.ok, "resolve");
        if (resolve == .ok) {
            const args = [_]mq.JSValue{value};
            const result = mq.call(ctx, resolve.ok, promise_ctor.ok, &args);
            if (result == .ok) return result.ok;
        }
    }
    return mq.undefined_();
}

fn createRejectedPromise(ctx: *mq.JSContext, message: []const u8) mq.JSValue {
    const global = mq.getGlobalObject(ctx);
    const promise_ctor = mq.getPropertyStr(ctx, global, "Promise");
    if (promise_ctor == .ok) {
        const reject_fn = mq.getPropertyStr(ctx, promise_ctor.ok, "reject");
        if (reject_fn == .ok) {
            const err_str = mq.fromString(ctx, message);
            if (err_str == .ok) {
                const args = [_]mq.JSValue{err_str.ok};
                const result = mq.call(ctx, reject_fn.ok, promise_ctor.ok, &args);
                if (result == .ok) return result.ok;
            }
        }
    }
    return mq.undefined_();
}

pub const OpKind = enum {
    fetch,
    read_file,
    write_file,
    timeout,
    tcp_connect,
};

pub const OpState = enum {
    pending,
    in_progress,
    completed,
};

pub const OpData = union {
    fetch: FetchData,
    read_file: ReadFileData,
    write_file: WriteFileData,
    timeout: TimeoutData,
    tcp_connect: TcpConnectData,
};

pub const FetchData = struct {
    url: []const u8,
    method: std.http.Method,
    body: ?[]const u8,
    request: ?std.http.Client.Request,
    header_buf: [4096]u8,
};

pub const ReadFileData = struct {
    path: []const u8,
};

pub const WriteFileData = struct {
    path: []const u8,
    content: []const u8,
};

pub const TimeoutData = struct {
    deadline_ms: i64,
};

pub const TcpConnectData = struct {
    host: []const u8,
    port: u16,
};

// ============================================================================
// Microtasks
// ============================================================================

pub const Microtask = struct {
    callback: mq.JSValue,
    arg: mq.JSValue,

    pub fn execute(self: Microtask, ctx: *mq.JSContext) void {
        const args = [_]mq.JSValue{self.arg};
        _ = mq.call(ctx, self.callback, mq.undefined_(), &args);
    }
};
