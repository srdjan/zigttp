//! HTTP Server with JavaScript Request Handlers
//!
//! Architecture:
//! - Evented I/O using Zig's std.Io with kqueue/io_uring backend
//! - Per-request JS context isolation via LockFreePool-backed handler pool
//! - Deno-compatible handler API

const std = @import("std");
const builtin = @import("builtin");
const Io = std.Io;
const net = std.Io.net;
const zruntime = @import("zruntime.zig");
const Runtime = zruntime.Runtime;
const HandlerPool = zruntime.HandlerPool;
const RuntimeConfig = zruntime.RuntimeConfig;
const HttpRequest = zruntime.HttpRequest;
const HttpResponse = zruntime.HttpResponse;

// ============================================================================
// Server Configuration
// ============================================================================

pub const ServerConfig = struct {
    /// Port to listen on
    port: u16 = 8080,

    /// Host to bind to
    host: []const u8 = "127.0.0.1",

    /// JS handler script path or inline code
    handler: HandlerSource,

    /// Maximum request body size (default 1MB)
    max_body_size: usize = 1024 * 1024,

    /// Request timeout in milliseconds
    timeout_ms: u32 = 30_000,

    /// JS runtime configuration
    runtime_config: RuntimeConfig = .{},

    /// Number of runtime instances in pool
    pool_size: usize = 8,

    /// Log requests to stdout
    log_requests: bool = true,

    /// Enable CORS headers
    enable_cors: bool = false,

    /// Static file directory (null = disabled)
    static_dir: ?[]const u8 = null,
};

pub const HandlerSource = union(enum) {
    /// Inline JavaScript code
    inline_code: []const u8,

    /// Path to JavaScript file
    file_path: []const u8,
};

// ============================================================================
// Server Implementation
// ============================================================================

pub const Server = struct {
    config: ServerConfig,
    allocator: std.mem.Allocator,
    io_backend: IoBackend,
    evented_ready: bool,
    listener: ?net.Server,
    pool: ?HandlerPool,
    handler_code: []const u8,
    handler_filename: []const u8,
    running: bool,
    request_count: std.atomic.Value(u64),

    const Self = @This();
    const ConnectionEvent = enum { done, timeout };
    const IoBackend = if (useEventedBackend()) Io.Evented else Io.Threaded;

    pub fn init(allocator: std.mem.Allocator, config: ServerConfig) !Self {
        // Load handler code
        const handler_code, const handler_filename = switch (config.handler) {
            .inline_code => |code| .{ code, "<inline>" },
            .file_path => |path| blk: {
                const source = try std.fs.cwd().readFileAlloc(path, allocator, Io.Limit.limited(10 * 1024 * 1024));
                // JSX transformation is now handled by the parser via filename detection
                break :blk .{ source, path };
            },
        };

        return Self{
            .config = config,
            .allocator = allocator,
            .io_backend = undefined,
            .evented_ready = false,
            .listener = null,
            .pool = null,
            .handler_code = handler_code,
            .handler_filename = handler_filename,
            .running = false,
            .request_count = std.atomic.Value(u64).init(0),
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.pool) |*p| p.deinit();
        if (self.evented_ready) {
            const io = self.io_backend.io();
            if (self.listener) |*l| l.deinit(io);
            self.io_backend.deinit();
        }

        // Free handler code if loaded from file
        if (self.config.handler == .file_path) {
            self.allocator.free(self.handler_code);
        }
    }

    pub fn start(self: *Self) !void {
        try initIoBackend(&self.io_backend, self.allocator);
        self.evented_ready = true;
        const io = self.io_backend.io();

        // Initialize runtime pool
        self.pool = try HandlerPool.init(
            self.allocator,
            self.config.runtime_config,
            self.handler_code,
            self.handler_filename,
            self.config.pool_size,
        );

        // Parse address and create listener
        const address = try net.IpAddress.parseIp4(self.config.host, self.config.port);

        self.listener = try address.listen(io, .{
            .reuse_address = true,
        });

        self.running = true;

        std.log.info("Server listening on http://{s}:{d}", .{ self.config.host, self.config.port });
        std.log.info("   Pool size: {d} runtimes", .{self.config.pool_size});
    }

    pub fn run(self: *Self) !void {
        try self.start();
        try self.acceptLoop();
    }

    fn acceptLoop(self: *Self) !void {
        const io = self.io_backend.io();
        var listener = self.listener orelse return error.NotStarted;
        var group: Io.Group = .init;
        defer group.cancel(io);

        while (self.running) {
            const stream = listener.accept(io) catch |err| {
                if (err == error.ConnectionAborted) continue;
                return err;
            };

            group.async(io, handleConnectionTask, .{ self, stream, io });
        }
    }

    fn handleConnectionTask(self: *Self, stream: net.Stream, io: Io) void {
        var stream_mut = stream;
        if (self.config.timeout_ms == 0) {
            self.handleConnection(&stream_mut, io) catch |err| {
                if (err != error.Canceled) {
                    std.log.err("Connection error: {}", .{err});
                }
            };
            stream_mut.close(io);
            return;
        }
        self.handleConnectionWithTimeout(stream_mut, io);
    }

    fn handleConnectionWithTimeout(self: *Self, stream: net.Stream, io: Io) void {
        var queue_buf: [2]ConnectionEvent = undefined;
        var queue = Io.Queue(ConnectionEvent).init(&queue_buf);
        var group: Io.Group = .init;
        var timed_out = std.atomic.Value(bool).init(false);
        defer group.cancel(io);

        group.async(io, connectionRunner, .{ self, stream, io, &queue, &timed_out });
        group.async(io, timeoutRunner, .{ self.config.timeout_ms, io, &queue, &timed_out });

        _ = queue.getOne(io) catch return;
        group.cancel(io);
    }

    fn connectionRunner(
        self: *Self,
        stream: net.Stream,
        io: Io,
        queue: *Io.Queue(ConnectionEvent),
        timed_out: *std.atomic.Value(bool),
    ) void {
        var stream_mut = stream;
        defer stream_mut.close(io);
        self.handleConnection(&stream_mut, io) catch |err| {
            if (err == error.RequestTimedOut and timed_out.load(.acquire)) {
                self.sendErrorResponse(&stream_mut, io, 408, "Request Timeout") catch {};
                return;
            }
            if (err != error.Canceled and err != error.RequestTimedOut) {
                std.log.err("Connection error: {}", .{err});
            }
        };
        _ = queue.putOneUncancelable(io, .done) catch {};
    }

    fn timeoutRunner(
        timeout_ms: u32,
        io: Io,
        queue: *Io.Queue(ConnectionEvent),
        timed_out: *std.atomic.Value(bool),
    ) void {
        if (timeout_ms == 0) return;
        const duration = Io.Duration.fromMilliseconds(@intCast(timeout_ms));
        const timeout = Io.Timeout{ .duration = .{ .raw = duration, .clock = .awake } };
        _ = timeout.sleep(io) catch {};
        timed_out.store(true, .release);
        _ = queue.putOneUncancelable(io, .timeout) catch {};
    }

    fn handleConnection(self: *Self, stream: *net.Stream, io: Io) !void {
        const start_instant = std.time.Instant.now() catch null;

        var request_started = false;
        // Parse HTTP request
        var request = self.parseRequest(stream, io, &request_started) catch |err| {
            if (err == error.Canceled and request_started) {
                return error.RequestTimedOut;
            }
            return err;
        };
        defer request.deinit(self.allocator);

        // Handle static files if configured
        if (self.config.static_dir) |static_dir| {
            if (std.mem.startsWith(u8, request.url, "/static/")) {
                try self.serveStaticFile(stream, io, static_dir, request.url[7..]);
                return;
            }
        }

        // Invoke JS handler via pool
        if (self.pool) |*pool| {
            var response = pool.executeHandler(HttpRequest{
                .url = request.url,
                .method = request.method,
                .headers = request.headers,
                .body = request.body,
            }) catch |err| {
                // Return 503 Service Unavailable for pool exhaustion (allows load balancer retry)
                // Return 500 for other errors
                const status: u16 = if (err == error.PoolExhausted) 503 else 500;
                const message = if (err == error.PoolExhausted)
                    "Service Unavailable: Server at capacity, please retry"
                else
                    "Internal Server Error";
                std.log.err(
                    "Handler error: {} (responding with {}, in_use={d}/{d})",
                    .{ err, status, pool.getInUse(), pool.max_size },
                );
                try self.sendErrorResponse(stream, io, status, message);
                return;
            };
            defer response.deinit();

            // Add CORS headers if enabled
            if (self.config.enable_cors) {
                try response.putHeader("Access-Control-Allow-Origin", "*");
                try response.putHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
                try response.putHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");
            }

            // Send response
            try self.sendResponse(stream, io, &response);

            // Log request
            if (self.config.log_requests) {
                const elapsed_ms: i64 = if (start_instant) |start_time| blk: {
                    const now = std.time.Instant.now() catch break :blk 0;
                    break :blk @intCast(now.since(start_time) / std.time.ns_per_ms);
                } else 0;
                const count = self.request_count.fetchAdd(1, .monotonic) + 1;
                std.log.info("[{d}] {s} {s} -> {d} ({d}ms)", .{
                    count,
                    request.method,
                    request.url,
                    response.status,
                    elapsed_ms,
                });
            }
        } else {
            try self.sendErrorResponse(stream, io, 503, "Service Unavailable: runtime pool not initialized");
            return;
        }
    }

    fn parseRequest(self: *Self, stream: *net.Stream, io: Io, request_started: *bool) !ParsedRequest {
        var buf: [8192]u8 = undefined;
        var reader_buf: [4096]u8 = undefined;
        var headers = std.StringHashMap([]const u8).init(self.allocator);
        errdefer freeHeaderMap(self.allocator, &headers);

        // Use buffered reader for efficient parsing
        var reader = BufferedReader.init(stream, io, &reader_buf);

        // Read request line
        const request_line = try reader.readLine(&buf);
        request_started.* = true;
        var parts = std.mem.splitScalar(u8, request_line, ' ');

        // Duplicate method and url immediately before reading headers
        const method_slice = parts.next() orelse return error.InvalidRequest;
        const method = try self.allocator.dupe(u8, method_slice);
        errdefer self.allocator.free(method);

        const url_slice = parts.next() orelse return error.InvalidRequest;
        const url = try self.allocator.dupe(u8, url_slice);
        errdefer self.allocator.free(url);

        // Read headers
        while (true) {
            const line = try reader.readLine(&buf);
            if (line.len == 0) break;

            var header_parts = std.mem.splitSequence(u8, line, ": ");
            const key = header_parts.next() orelse continue;
            const value = header_parts.rest();

            // Duplicate strings since they point into buf
            const key_dup = try self.allocator.dupe(u8, key);
            const value_dup = try self.allocator.dupe(u8, value);
            headers.put(key_dup, value_dup) catch |err| {
                self.allocator.free(key_dup);
                self.allocator.free(value_dup);
                return err;
            };
        }

        // Read body if Content-Length present (case-insensitive)
        var body: ?[]u8 = null;
        if (findHeader(&headers, "Content-Length")) |len_str| {
            const content_length = std.fmt.parseInt(usize, len_str, 10) catch 0;
            if (content_length > 0 and content_length <= self.config.max_body_size) {
                body = try self.allocator.alloc(u8, content_length);
                errdefer if (body) |b| self.allocator.free(b);

                try reader.readExact(body.?);
            }
        }

        return ParsedRequest{
            .method = method,
            .url = url,
            .headers = headers,
            .body = body,
        };
    }

    fn sendResponse(self: *Self, stream: *net.Stream, io: Io, response: *HttpResponse) !void {
        _ = self;
        var out_buf: [4096]u8 = undefined;
        var writer = stream.writer(io, &out_buf);
        const out = &writer.interface;

        // Status line
        const status_text = getStatusText(response.status);
        try out.print("HTTP/1.1 {d} {s}\r\n", .{ response.status, status_text });

        // Headers
        try out.print("Content-Length: {d}\r\n", .{response.body.len});
        try out.writeAll("Connection: close\r\n");

        var header_iter = response.headers.iterator();
        while (header_iter.next()) |entry| {
            if (std.ascii.eqlIgnoreCase(entry.key_ptr.*, "Content-Length")) continue;
            if (std.ascii.eqlIgnoreCase(entry.key_ptr.*, "Connection")) continue;
            try out.print("{s}: {s}\r\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }

        try out.writeAll("\r\n");

        // Body
        if (response.body.len > 0) {
            try out.writeAll(response.body);
        }

        try writer.interface.flush();
    }

    fn sendErrorResponse(self: *Self, stream: *net.Stream, io: Io, status: u16, message: []const u8) !void {
        _ = self;
        var out_buf: [1024]u8 = undefined;
        var writer = stream.writer(io, &out_buf);
        const out = &writer.interface;

        const status_text = getStatusText(status);
        try out.print("HTTP/1.1 {d} {s}\r\n", .{ status, status_text });
        try out.print("Content-Length: {d}\r\n", .{message.len});
        try out.writeAll("Content-Type: text/plain\r\n");
        try out.writeAll("Connection: close\r\n\r\n");
        try out.writeAll(message);
        try writer.interface.flush();
    }

    fn serveStaticFile(self: *Self, stream: *net.Stream, io: Io, static_dir: []const u8, path: []const u8) !void {
        _ = self;

        // Security: comprehensive path validation
        if (!isPathSafe(path)) {
            var out_buf: [256]u8 = undefined;
            var writer = stream.writer(io, &out_buf);
            try writer.interface.writeAll("HTTP/1.1 403 Forbidden\r\nContent-Length: 9\r\n\r\nForbidden");
            try writer.interface.flush();
            return;
        }

        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ static_dir, path }) catch {
            return error.PathTooLong;
        };

        const file = std.fs.cwd().openFile(full_path, .{}) catch {
            var out_buf: [256]u8 = undefined;
            var writer = stream.writer(io, &out_buf);
            try writer.interface.writeAll("HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\n\r\nNot Found");
            try writer.interface.flush();
            return;
        };
        defer file.close();

        const stat = try file.stat();
        const content_type = getContentType(path);

        var out_buf: [4096]u8 = undefined;
        var writer = stream.writer(io, &out_buf);
        const out = &writer.interface;
        try out.print("HTTP/1.1 200 OK\r\nContent-Length: {d}\r\nContent-Type: {s}\r\n\r\n", .{
            stat.size,
            content_type,
        });

        // Stream file contents
        var buf: [8192]u8 = undefined;
        while (true) {
            const bytes_read = file.read(&buf) catch break;
            if (bytes_read == 0) break;
            try out.writeAll(buf[0..bytes_read]);
        }
        try writer.interface.flush();
    }
};

const ParsedRequest = struct {
    method: []const u8,
    url: []const u8,
    headers: std.StringHashMap([]const u8),
    body: ?[]u8,

    pub fn deinit(self: *ParsedRequest, allocator: std.mem.Allocator) void {
        allocator.free(self.method);
        allocator.free(self.url);
        if (self.body) |b| allocator.free(b);
        freeHeaderMap(allocator, &self.headers);
    }
};

// ============================================================================
// Helpers
// ============================================================================

/// Buffered reader for efficient HTTP parsing
/// Uses the std.Io.Reader interface for reading from Stream
const BufferedReader = struct {
    reader: net.Stream.Reader,

    pub fn init(stream: *net.Stream, io: Io, buffer: []u8) BufferedReader {
        return .{
            .reader = stream.reader(io, buffer),
        };
    }

    /// Read a line ending with \r\n or \n
    pub fn readLine(self: *BufferedReader, out_buf: []u8) ![]const u8 {
        var i: usize = 0;

        while (i < out_buf.len - 1) {
            const byte = self.reader.interface.takeByte() catch |err| {
                if (i > 0) return out_buf[0..i];
                return err;
            };

            if (byte == '\r') {
                // Consume \n if present
                _ = self.reader.interface.takeByte() catch {};
                return out_buf[0..i];
            }
            if (byte == '\n') return out_buf[0..i];

            out_buf[i] = byte;
            i += 1;
        }

        return out_buf[0..i];
    }

    /// Read exact number of bytes
    pub fn readExact(self: *BufferedReader, out: []u8) !void {
        try self.reader.interface.readSliceAll(out);
    }
};

fn findHeader(headers: *std.StringHashMap([]const u8), name: []const u8) ?[]const u8 {
    var it = headers.iterator();
    while (it.next()) |entry| {
        if (std.ascii.eqlIgnoreCase(entry.key_ptr.*, name)) {
            return entry.value_ptr.*;
        }
    }
    return null;
}

fn getStatusText(status: u16) []const u8 {
    return switch (status) {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        304 => "Not Modified",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        405 => "Method Not Allowed",
        422 => "Unprocessable Entity",
        429 => "Too Many Requests",
        500 => "Internal Server Error",
        502 => "Bad Gateway",
        503 => "Service Unavailable",
        504 => "Gateway Timeout",
        else => "Unknown",
    };
}

/// Validate that a path is safe (no traversal, no absolute paths)
fn isPathSafe(path: []const u8) bool {
    // Reject empty paths
    if (path.len == 0) return false;

    // Reject absolute paths (Unix and Windows)
    if (path[0] == '/' or path[0] == '\\') return false;

    // Reject Windows drive letters (C:, D:, etc.)
    if (path.len >= 2 and path[1] == ':') return false;

    // Check each path component for traversal
    var iter = std.mem.splitAny(u8, path, "/\\");
    while (iter.next()) |component| {
        // Reject ".." components (parent directory traversal)
        if (std.mem.eql(u8, component, "..")) return false;
        // Reject "." components (current directory - can be used in attacks)
        if (std.mem.eql(u8, component, ".")) return false;
    }

    return true;
}

fn getContentType(path: []const u8) []const u8 {
    const ext = std.fs.path.extension(path);
    if (std.mem.eql(u8, ext, ".html") or std.mem.eql(u8, ext, ".htm")) return "text/html; charset=utf-8";
    if (std.mem.eql(u8, ext, ".css")) return "text/css; charset=utf-8";
    if (std.mem.eql(u8, ext, ".js")) return "application/javascript; charset=utf-8";
    if (std.mem.eql(u8, ext, ".json")) return "application/json";
    if (std.mem.eql(u8, ext, ".png")) return "image/png";
    if (std.mem.eql(u8, ext, ".jpg") or std.mem.eql(u8, ext, ".jpeg")) return "image/jpeg";
    if (std.mem.eql(u8, ext, ".gif")) return "image/gif";
    if (std.mem.eql(u8, ext, ".svg")) return "image/svg+xml";
    if (std.mem.eql(u8, ext, ".ico")) return "image/x-icon";
    if (std.mem.eql(u8, ext, ".woff")) return "font/woff";
    if (std.mem.eql(u8, ext, ".woff2")) return "font/woff2";
    if (std.mem.eql(u8, ext, ".ttf")) return "font/ttf";
    if (std.mem.eql(u8, ext, ".txt")) return "text/plain; charset=utf-8";
    if (std.mem.eql(u8, ext, ".xml")) return "application/xml";
    if (std.mem.eql(u8, ext, ".pdf")) return "application/pdf";
    return "application/octet-stream";
}

fn freeHeaderMap(allocator: std.mem.Allocator, headers: *std.StringHashMap([]const u8)) void {
    var it = headers.iterator();
    while (it.next()) |entry| {
        allocator.free(entry.key_ptr.*);
        allocator.free(entry.value_ptr.*);
    }
    headers.deinit();
}

fn initIoBackend(io: anytype, allocator: std.mem.Allocator) !void {
    const Backend = @TypeOf(io.*);
    if (Backend == Io.Threaded) {
        io.* = Io.Threaded.init(allocator);
        return;
    }
    if (@hasDecl(Backend, "InitOptions")) {
        try io.init(allocator, .{});
    } else {
        try io.init(allocator);
    }
}

fn useEventedBackend() bool {
    return switch (builtin.os.tag) {
        .macos, .ios, .tvos, .watchos, .visionos => false,
        else => true,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "get content type" {
    try std.testing.expectEqualStrings("text/html; charset=utf-8", getContentType("index.html"));
    try std.testing.expectEqualStrings("application/javascript; charset=utf-8", getContentType("app.js"));
    try std.testing.expectEqualStrings("application/json", getContentType("data.json"));
    try std.testing.expectEqualStrings("image/png", getContentType("logo.png"));
}

test "get status text" {
    try std.testing.expectEqualStrings("OK", getStatusText(200));
    try std.testing.expectEqualStrings("Not Found", getStatusText(404));
    try std.testing.expectEqualStrings("Internal Server Error", getStatusText(500));
}

test "path safety validation" {
    // Safe paths
    try std.testing.expect(isPathSafe("index.html"));
    try std.testing.expect(isPathSafe("static/images/logo.png"));
    try std.testing.expect(isPathSafe("assets/css/style.css"));

    // Unsafe: directory traversal
    try std.testing.expect(!isPathSafe("../etc/passwd"));
    try std.testing.expect(!isPathSafe("foo/../bar"));
    try std.testing.expect(!isPathSafe(".."));

    // Unsafe: absolute paths
    try std.testing.expect(!isPathSafe("/etc/passwd"));
    try std.testing.expect(!isPathSafe("\\Windows\\System32"));

    // Unsafe: Windows drive letters
    try std.testing.expect(!isPathSafe("C:\\Windows"));
    try std.testing.expect(!isPathSafe("D:file.txt"));

    // Unsafe: current directory reference
    try std.testing.expect(!isPathSafe("."));
    try std.testing.expect(!isPathSafe("./hidden"));

    // Unsafe: empty path
    try std.testing.expect(!isPathSafe(""));
}
