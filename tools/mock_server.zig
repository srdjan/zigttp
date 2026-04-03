//! Contract-Driven Mock Server
//!
//! Serves mock responses derived from PathGenerator test cases (JSONL format).
//! Each test case maps a request (method + URL) to an expected status code.
//! Frontend teams get a mock API provably consistent with the handler contract.
//!
//! Usage: zig build mock -- <tests.jsonl> [--port PORT]

const std = @import("std");
const zts = @import("zts");
const c = @cImport({
    @cInclude("sys/socket.h");
    @cInclude("netinet/in.h");
    @cInclude("unistd.h");
});

const MockRoute = struct {
    method: []const u8,
    url: []const u8,
    status: u16,
    name: []const u8,
};

pub fn main(init: std.process.Init.Minimal) !void {
    const allocator = std.heap.smp_allocator;

    var args_iter = std.process.Args.Iterator.init(init.args);
    defer args_iter.deinit();
    _ = args_iter.next(); // skip program name

    var args = std.ArrayList([]const u8).empty;
    defer args.deinit(allocator);
    while (args_iter.next()) |arg| try args.append(allocator, arg);

    try runWithArgs(allocator, args.items);
}

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const test_path = if (argv.len > 0) argv[0] else {
        std.debug.print("Usage: mock <tests.jsonl> [--port PORT]\n", .{});
        std.process.exit(2);
    };

    var port: u16 = 3001;
    var index: usize = 1;
    while (index < argv.len) : (index += 1) {
        const arg = argv[index];
        if (std.mem.eql(u8, arg, "--port")) {
            index += 1;
            const port_str = if (index < argv.len) argv[index] else {
                std.debug.print("--port requires a value\n", .{});
                std.process.exit(2);
            };
            port = std.fmt.parseInt(u16, port_str, 10) catch {
                std.debug.print("Invalid port: {s}\n", .{port_str});
                std.process.exit(2);
            };
        }
    }

    const source = zts.file_io.readFile(allocator, test_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ test_path, err });
        std.process.exit(2);
    };
    defer allocator.free(source);

    var routes: std.ArrayList(MockRoute) = .empty;
    defer routes.deinit(allocator);
    parseTestRoutes(source, &routes, allocator);

    std.debug.print("Mock server loaded {d} route(s) from {s}\n", .{ routes.items.len, test_path });
    for (routes.items) |route| {
        std.debug.print("  {s} {s} -> {d}\n", .{ route.method, route.url, route.status });
    }

    const sockfd = c.socket(c.AF_INET, c.SOCK_STREAM, 0);
    if (sockfd < 0) {
        std.debug.print("Failed to create socket\n", .{});
        std.process.exit(2);
    }
    defer _ = c.close(sockfd);

    var opt: c_int = 1;
    _ = c.setsockopt(sockfd, c.SOL_SOCKET, c.SO_REUSEADDR, &opt, @sizeOf(c_int));

    var addr: c.struct_sockaddr_in = std.mem.zeroes(c.struct_sockaddr_in);
    addr.sin_family = c.AF_INET;
    addr.sin_port = std.mem.nativeToBig(u16, port);
    addr.sin_addr.s_addr = c.INADDR_ANY;

    if (c.bind(sockfd, @ptrCast(&addr), @sizeOf(c.struct_sockaddr_in)) < 0) {
        std.debug.print("Failed to bind to port {d}\n", .{port});
        std.process.exit(2);
    }
    if (c.listen(sockfd, 8) < 0) {
        std.debug.print("Failed to listen\n", .{});
        std.process.exit(2);
    }

    std.debug.print("Mock server listening on http://127.0.0.1:{d}\n", .{port});

    while (true) {
        const client_fd = c.accept(sockfd, null, null);
        if (client_fd < 0) continue;
        defer _ = c.close(client_fd);

        // Prevent SIGPIPE on write to closed client (macOS)
        var nosigpipe: c_int = 1;
        _ = c.setsockopt(client_fd, c.SOL_SOCKET, c.SO_NOSIGPIPE, &nosigpipe, @sizeOf(c_int));

        handleRequest(client_fd, routes.items);
    }
}

fn handleRequest(fd: c_int, routes: []const MockRoute) void {
    var buf: [4096]u8 = undefined;
    const n = c.read(fd, &buf, buf.len);
    if (n <= 0) return;

    const request = buf[0..@intCast(n)];

    // Parse HTTP request line: "GET /path HTTP/1.1\r\n"
    const first_line_end = std.mem.indexOf(u8, request, "\r\n") orelse return;
    const line = request[0..first_line_end];

    const method_end = std.mem.indexOfScalar(u8, line, ' ') orelse return;
    const method = line[0..method_end];

    const path_start = method_end + 1;
    const path_end = std.mem.indexOfScalarPos(u8, line, path_start, ' ') orelse return;
    const url = line[path_start..path_end];

    // Find matching route (exact method+url, then fallback to url-only)
    var matched: ?*const MockRoute = null;
    for (routes) |*route| {
        if (std.mem.eql(u8, route.method, method) and std.mem.eql(u8, route.url, url)) {
            matched = route;
            break;
        }
    }
    if (matched == null) {
        for (routes) |*route| {
            if (std.mem.eql(u8, route.url, url)) {
                matched = route;
                break;
            }
        }
    }

    if (matched) |route| {
        std.debug.print("{s} {s} -> {d} ({s})\n", .{ method, url, route.status, route.name });
        sendResponse(fd, route.status, route.name);
    } else {
        std.debug.print("{s} {s} -> 404 (no matching route)\n", .{ method, url });
        sendResponse(fd, 404, "no matching route");
    }
}

fn sendResponse(fd: c_int, status: u16, name: []const u8) void {
    const status_text = switch (status) {
        200 => "OK",
        201 => "Created",
        202 => "Accepted",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        429 => "Too Many Requests",
        500 => "Internal Server Error",
        else => "Unknown",
    };

    var body_buf: [512]u8 = undefined;
    const body = std.fmt.bufPrint(&body_buf, "{{\"mock\":true,\"test\":\"{s}\",\"status\":{d}}}", .{ name, status }) catch "{}";

    var header_buf: [512]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf, "HTTP/1.1 {d} {s}\r\nContent-Type: application/json\r\nContent-Length: {d}\r\nAccess-Control-Allow-Origin: *\r\nConnection: close\r\n\r\n", .{ status, status_text, body.len }) catch return;

    _ = c.write(fd, header.ptr, header.len);
    _ = c.write(fd, body.ptr, body.len);
}

const findStr = zts.trace.findJsonStringValue;
const findInt = zts.trace.findJsonIntValue;

fn parseTestRoutes(source: []const u8, routes: *std.ArrayList(MockRoute), allocator: std.mem.Allocator) void {
    var current_name: []const u8 = "unnamed";
    var current_method: []const u8 = "GET";
    var current_url: []const u8 = "/";

    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;

        const type_val = findStr(trimmed, "\"type\"") orelse continue;
        if (std.mem.eql(u8, type_val, "test")) {
            current_name = findStr(trimmed, "\"name\"") orelse "unnamed";
        } else if (std.mem.eql(u8, type_val, "request")) {
            current_method = findStr(trimmed, "\"method\"") orelse "GET";
            current_url = findStr(trimmed, "\"url\"") orelse "/";
        } else if (std.mem.eql(u8, type_val, "expect")) {
            const status_i64 = findInt(trimmed, "\"status\"") orelse 200;
            const status: u16 = if (status_i64 > 0 and status_i64 <= 599) @intCast(status_i64) else 200;
            routes.append(allocator, .{
                .method = current_method,
                .url = current_url,
                .status = status,
                .name = current_name,
            }) catch |err| {
                std.debug.print("Failed to add route: {}\n", .{err});
                return;
            };
        }
    }
}
