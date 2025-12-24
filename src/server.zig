//! HTTP Server with JavaScript Request Handlers
//!
//! Architecture:
//! - Async I/O using Zig's std.net
//! - Per-request JS context isolation via RuntimePool
//! - Deno-compatible handler API

const std = @import("std");
const net = std.net;
const mq = @import("mquickjs.zig");
const jsx = @import("jsx.zig");
const Runtime = @import("runtime.zig").Runtime;
const RuntimePool = @import("runtime.zig").RuntimePool;
const RuntimeConfig = @import("runtime.zig").RuntimeConfig;
const HttpRequest = @import("runtime.zig").HttpRequest;
const HttpResponse = @import("runtime.zig").HttpResponse;

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
    listener: ?net.Server,
    pool: ?RuntimePool,
    handler_code: []const u8,
    running: bool,
    request_count: std.atomic.Value(u64),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, config: ServerConfig) !Self {
        // Load handler code
        const handler_code = switch (config.handler) {
            .inline_code => |code| code,
            .file_path => |path| blk: {
                const file = try std.fs.cwd().openFile(path, .{});
                defer file.close();
                const source = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);

                // Transform JSX if .jsx extension
                if (std.mem.endsWith(u8, path, ".jsx")) {
                    const result = jsx.transform(allocator, source) catch |err| {
                        std.log.err("JSX transform error: {}", .{err});
                        allocator.free(source);
                        return error.JsxTransformFailed;
                    };
                    allocator.free(source);
                    break :blk result.code;
                }

                break :blk source;
            },
        };

        return Self{
            .config = config,
            .allocator = allocator,
            .listener = null,
            .pool = null,
            .handler_code = handler_code,
            .running = false,
            .request_count = std.atomic.Value(u64).init(0),
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.pool) |*p| p.deinit();
        if (self.listener) |*l| l.deinit();

        // Free handler code if loaded from file
        if (self.config.handler == .file_path) {
            self.allocator.free(self.handler_code);
        }
    }

    pub fn start(self: *Self) !void {
        // Initialize runtime pool
        self.pool = try RuntimePool.init(
            self.allocator,
            self.config.runtime_config,
            self.handler_code,
            self.config.pool_size,
        );

        // Parse address
        const address = try net.Address.parseIp4(self.config.host, self.config.port);

        // Create listener
        self.listener = try address.listen(.{
            .reuse_address = true,
        });

        self.running = true;

        std.log.info("🚀 Server listening on http://{s}:{d}", .{ self.config.host, self.config.port });
        std.log.info("   Pool size: {d} runtimes", .{self.config.pool_size});
    }

    pub fn run(self: *Self) !void {
        try self.start();
        try self.acceptLoop();
    }

    fn acceptLoop(self: *Self) !void {
        var listener = self.listener orelse return error.NotStarted;

        while (self.running) {
            const conn = listener.accept() catch |err| {
                if (err == error.ConnectionAborted) continue;
                return err;
            };

            self.handleConnection(conn) catch |err| {
                std.log.err("Connection error: {}", .{err});
            };
        }
    }

    fn handleConnection(self: *Self, conn: net.Server.Connection) !void {
        defer conn.stream.close();

        const start_time = std.time.milliTimestamp();

        // Parse HTTP request
        var request = try self.parseRequest(conn.stream);
        defer {
            request.headers.deinit();
            if (request.body) |b| self.allocator.free(b);
        }

        // Handle static files if configured
        if (self.config.static_dir) |static_dir| {
            if (std.mem.startsWith(u8, request.url, "/static/")) {
                try self.serveStaticFile(conn.stream, static_dir, request.url[7..]);
                return;
            }
        }

        // Invoke JS handler via pool
        var pool = self.pool orelse {
            try self.sendErrorResponse(conn.stream, 503, "Service Unavailable: runtime pool not initialized");
            return;
        };

        var response = pool.executeHandler(HttpRequest{
            .url = request.url,
            .method = request.method,
            .headers = request.headers,
            .body = request.body,
        }) catch |err| {
            std.log.err("Handler error: {}", .{err});
            try self.sendErrorResponse(conn.stream, 500, "Internal Server Error");
            return;
        };
        defer response.deinit();

        // Add CORS headers if enabled
        if (self.config.enable_cors) {
            try response.headers.put("Access-Control-Allow-Origin", "*");
            try response.headers.put("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
            try response.headers.put("Access-Control-Allow-Headers", "Content-Type, Authorization");
        }

        // Send response
        try self.sendResponse(conn.stream, &response);

        // Log request
        if (self.config.log_requests) {
            const elapsed = std.time.milliTimestamp() - start_time;
            const count = self.request_count.fetchAdd(1, .monotonic) + 1;
            std.log.info("[{d}] {s} {s} → {d} ({d}ms)", .{
                count,
                request.method,
                request.url,
                response.status,
                elapsed,
            });
        }
    }

    fn parseRequest(self: *Self, stream: net.Stream) !ParsedRequest {
        var buf: [8192]u8 = undefined;
        var headers = std.StringHashMap([]const u8).init(self.allocator);
        errdefer headers.deinit();

        // Use buffered reader for efficient parsing (~90% fewer syscalls)
        var reader = BufferedReader.init(stream);

        // Read request line
        const request_line = try reader.readLine(&buf);
        var parts = std.mem.splitScalar(u8, request_line, ' ');

        // Duplicate method and url immediately before reading headers
        // (reading headers will overwrite buf, corrupting the slices)
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
            errdefer self.allocator.free(key_dup);

            const value_dup = try self.allocator.dupe(u8, value);
            errdefer self.allocator.free(value_dup);

            try headers.put(key_dup, value_dup);
        }

        // Read body if Content-Length present
        var body: ?[]u8 = null;
        if (headers.get("Content-Length")) |len_str| {
            const content_length = std.fmt.parseInt(usize, len_str, 10) catch 0;
            if (content_length > 0 and content_length <= self.config.max_body_size) {
                body = try self.allocator.alloc(u8, content_length);
                errdefer if (body) |b| self.allocator.free(b);

                // Use buffered reader for body too
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

    fn sendResponse(self: *Self, stream: net.Stream, response: *HttpResponse) !void {
        _ = self;
        var writer_buf: [4096]u8 = undefined;
        var writer = stream.writer(&writer_buf);
        defer writer.interface.flush() catch {};
        const out = &writer.interface;

        // Status line
        const status_text = getStatusText(response.status);
        try out.print("HTTP/1.1 {d} {s}\r\n", .{ response.status, status_text });

        // Headers
        try out.print("Content-Length: {d}\r\n", .{response.body.len});
        try out.writeAll("Connection: close\r\n");

        var header_iter = response.headers.iterator();
        while (header_iter.next()) |entry| {
            try out.print("{s}: {s}\r\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }

        try out.writeAll("\r\n");

        // Body
        if (response.body.len > 0) {
            try out.writeAll(response.body);
        }
    }

    fn sendErrorResponse(self: *Self, stream: net.Stream, status: u16, message: []const u8) !void {
        _ = self;
        var writer_buf: [1024]u8 = undefined;
        var writer = stream.writer(&writer_buf);
        defer writer.interface.flush() catch {};
        const out = &writer.interface;

        const status_text = getStatusText(status);
        try out.print("HTTP/1.1 {d} {s}\r\n", .{ status, status_text });
        try out.print("Content-Length: {d}\r\n", .{message.len});
        try out.writeAll("Content-Type: text/plain\r\n");
        try out.writeAll("Connection: close\r\n\r\n");
        try out.writeAll(message);
    }

    fn serveStaticFile(self: *Self, stream: net.Stream, static_dir: []const u8, path: []const u8) !void {
        _ = self;

        // Security: comprehensive path validation
        if (!isPathSafe(path)) {
            var writer_buf: [256]u8 = undefined;
            var writer = stream.writer(&writer_buf);
            defer writer.interface.flush() catch {};
            try writer.interface.writeAll("HTTP/1.1 403 Forbidden\r\nContent-Length: 9\r\n\r\nForbidden");
            return;
        }

        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ static_dir, path }) catch {
            return error.PathTooLong;
        };

        const file = std.fs.cwd().openFile(full_path, .{}) catch {
            var writer_buf: [256]u8 = undefined;
            var writer = stream.writer(&writer_buf);
            defer writer.interface.flush() catch {};
            try writer.interface.writeAll("HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\n\r\nNot Found");
            return;
        };
        defer file.close();

        const stat = try file.stat();
        const content_type = getContentType(path);

        var writer_buf: [4096]u8 = undefined;
        var writer = stream.writer(&writer_buf);
        defer writer.interface.flush() catch {};
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
    }
};

const ParsedRequest = struct {
    method: []const u8,
    url: []const u8,
    headers: std.StringHashMap([]const u8),
    body: ?[]u8,
};

// ============================================================================
// Helpers
// ============================================================================

/// Buffered reader for efficient HTTP parsing
/// Reduces syscalls by reading in chunks instead of byte-by-byte
const BufferedReader = struct {
    stream: net.Stream,
    buffer: [4096]u8,
    start: usize,
    end: usize,

    pub fn init(stream: net.Stream) BufferedReader {
        return .{
            .stream = stream,
            .buffer = undefined,
            .start = 0,
            .end = 0,
        };
    }

    /// Read a line ending with \r\n or \n
    pub fn readLine(self: *BufferedReader, out_buf: []u8) ![]const u8 {
        var i: usize = 0;

        while (i < out_buf.len - 1) {
            // Refill buffer if empty
            if (self.start >= self.end) {
                self.end = self.stream.read(&self.buffer) catch |err| {
                    if (err == error.EndOfStream or err == error.ConnectionResetByPeer) {
                        if (i > 0) return out_buf[0..i];
                        return err;
                    }
                    return err;
                };
                self.start = 0;
                if (self.end == 0) {
                    if (i > 0) return out_buf[0..i];
                    return error.EndOfStream;
                }
            }

            // Process buffered data
            while (self.start < self.end and i < out_buf.len - 1) {
                const byte = self.buffer[self.start];
                self.start += 1;

                if (byte == '\r') {
                    // Consume \n if present
                    if (self.start < self.end and self.buffer[self.start] == '\n') {
                        self.start += 1;
                    }
                    return out_buf[0..i];
                }
                if (byte == '\n') return out_buf[0..i];

                out_buf[i] = byte;
                i += 1;
            }
        }

        return out_buf[0..i];
    }

    /// Read exact number of bytes
    pub fn readExact(self: *BufferedReader, out: []u8) !void {
        var offset: usize = 0;

        // First, use buffered data
        const buffered = self.end - self.start;
        if (buffered > 0) {
            const to_copy = @min(buffered, out.len);
            @memcpy(out[0..to_copy], self.buffer[self.start .. self.start + to_copy]);
            self.start += to_copy;
            offset = to_copy;
        }

        // Read remaining directly from stream
        while (offset < out.len) {
            const n = self.stream.read(out[offset..]) catch |err| {
                if (err == error.EndOfStream) return error.EndOfStream;
                return err;
            };
            if (n == 0) return error.EndOfStream;
            offset += n;
        }
    }
};

fn readLine(stream: net.Stream, buf: []u8) ![]const u8 {
    var i: usize = 0;
    var byte_buf: [1]u8 = undefined;
    while (i < buf.len - 1) {
        const bytes_read = stream.read(&byte_buf) catch |err| {
            if (err == error.EndOfStream) break;
            return err;
        };
        if (bytes_read == 0) break;
        const byte = byte_buf[0];
        if (byte == '\r') {
            _ = stream.read(&byte_buf) catch {}; // consume \n
            break;
        }
        if (byte == '\n') break;
        buf[i] = byte;
        i += 1;
    }
    return buf[0..i];
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
