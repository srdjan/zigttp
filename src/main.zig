//! zigttp-server - HTTP Server hosting zts JavaScript handlers
//!
//! Usage:
//!   zigttp-server [options] <handler.js>
//!   zigttp-server -e "function handler(req) { return Response.json({hello: 'world'}) }"
//!
//! Options:
//!   -p, --port <PORT>     Port to listen on (default: 8080)
//!   -h, --host <HOST>     Host to bind to (default: 127.0.0.1)
//!   -e, --eval <CODE>     Evaluate inline JavaScript handler
//!   -m, --memory <SIZE>   JS runtime memory limit (default: 0 = no limit)
//!   -n, --pool <N>        Runtime pool size (default: auto = 2 * cpu, min 8)
//!   -q, --quiet           Disable request logging
//!   --cors                Enable CORS headers
//!   --static <DIR>        Serve static files from directory
//!   --help                Show this help message
//!
//! Handler API (Deno-compatible):
//!   function handler(request) {
//!       // request.url, request.method, request.headers, request.body
//!       return Response.text(body, { status, headers });
//!   }
//!
//! Available APIs:
//!   - console.log/error/warn/info/debug
//!   - Response.json/text/html/redirect helpers
//!   - Math, String, Array, Object builtins
//!   - JSON.parse/stringify

const std = @import("std");
const Server = @import("server.zig").Server;
const ServerConfig = @import("server.zig").ServerConfig;
const HandlerSource = @import("server.zig").HandlerSource;
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const config = parseArgs() catch |err| {
        if (err == error.HelpRequested) {
            return;
        }
        std.log.err("Argument error: {}", .{err});
        return;
    };

    var server = Server.init(allocator, config) catch |err| {
        std.log.err("Failed to initialize server: {}", .{err});
        return;
    };
    defer server.deinit();

    server.run() catch |err| {
        std.log.err("Server error: {}", .{err});
        return;
    };
}

fn parseArgs() !ServerConfig {

    var config = ServerConfig{
        .handler = .{ .inline_code = "" },
        .runtime_config = .{},
    };

    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    defer args.deinit();

    // Skip program name
    _ = args.skip();

    var handler_set = false;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return error.HelpRequested;
        } else if (std.mem.eql(u8, arg, "-p") or std.mem.eql(u8, arg, "--port")) {
            const port_str = args.next() orelse return error.MissingPortValue;
            config.port = std.fmt.parseInt(u16, port_str, 10) catch return error.InvalidPort;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--host")) {
            config.host = args.next() orelse return error.MissingHostValue;
        } else if (std.mem.eql(u8, arg, "-e") or std.mem.eql(u8, arg, "--eval")) {
            const code = args.next() orelse return error.MissingEvalCode;
            config.handler = .{ .inline_code = code };
            handler_set = true;
        } else if (std.mem.eql(u8, arg, "-m") or std.mem.eql(u8, arg, "--memory")) {
            const size_str = args.next() orelse return error.MissingMemoryValue;
            config.runtime_config.memory_limit = parseSize(size_str) catch return error.InvalidMemorySize;
        } else if (std.mem.eql(u8, arg, "-n") or std.mem.eql(u8, arg, "--pool")) {
            const pool_str = args.next() orelse return error.MissingPoolValue;
            config.pool_size = std.fmt.parseInt(usize, pool_str, 10) catch return error.InvalidPoolSize;
        } else if (std.mem.eql(u8, arg, "-q") or std.mem.eql(u8, arg, "--quiet")) {
            config.log_requests = false;
        } else if (std.mem.eql(u8, arg, "--cors")) {
            config.enable_cors = true;
        } else if (std.mem.eql(u8, arg, "--static")) {
            config.static_dir = args.next() orelse return error.MissingStaticDir;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            // Positional argument: handler file
            config.handler = .{ .file_path = arg };
            handler_set = true;
        } else {
            std.log.err("Unknown option: {s}", .{arg});
            return error.UnknownOption;
        }
    }

    if (!handler_set) {
        std.log.err("No handler specified. Use -e for inline code or provide a file path.", .{});
        printHelp();
        return error.NoHandler;
    }

    return config;
}

fn parseSize(str: []const u8) !usize {
    var num_end: usize = 0;
    for (str, 0..) |c, i| {
        if (c >= '0' and c <= '9') {
            num_end = i + 1;
        } else {
            break;
        }
    }

    if (num_end == 0) return error.InvalidSize;

    const num = try std.fmt.parseInt(usize, str[0..num_end], 10);
    const suffix = str[num_end..];

    const multiplier: usize = if (suffix.len == 0)
        1
    else if (std.ascii.eqlIgnoreCase(suffix, "k") or std.ascii.eqlIgnoreCase(suffix, "kb"))
        1024
    else if (std.ascii.eqlIgnoreCase(suffix, "m") or std.ascii.eqlIgnoreCase(suffix, "mb"))
        1024 * 1024
    else if (std.ascii.eqlIgnoreCase(suffix, "g") or std.ascii.eqlIgnoreCase(suffix, "gb"))
        1024 * 1024 * 1024
    else
        return error.InvalidSizeSuffix;

    return num * multiplier;
}

fn printHelp() void {
    const help =
        \\zigttp-server - HTTP Server hosting zts JavaScript handlers
        \\
        \\Usage:
        \\  zigttp-server [options] <handler.js>
        \\  zigttp-server -e "handler = (req) => Response.json({hello: 'world'})"
        \\
        \\Options:
        \\  -p, --port <PORT>     Port to listen on (default: 8080)
        \\  -h, --host <HOST>     Host to bind to (default: 127.0.0.1)
        \\  -e, --eval <CODE>     Evaluate inline JavaScript handler
        \\  -m, --memory <SIZE>   JS runtime memory limit (default: 0 = no limit)
        \\                        Supports k/kb, m/mb, g/gb suffixes
        \\  -n, --pool <N>        Runtime pool size (default: auto = 2 * cpu, min 8)
        \\  -q, --quiet           Disable request logging
        \\  --cors                Enable CORS headers
        \\  --static <DIR>        Serve static files from directory
        \\  --help                Show this help message
        \\
        \\Handler API:
        \\  Your handler must export a 'handler' function that takes a request object
        \\  and returns a Response.
        \\
        \\  Request object:
        \\    {
        \\      method: string,    // HTTP method (GET, POST, etc.)
        \\      url: string,       // URL path
        \\      headers: object,   // HTTP headers
        \\      body: string|null  // Request body
        \\    }
        \\
        \\  Response helpers:
        \\    Response.json(data, init?)
        \\    Response.text(text, init?)
        \\    Response.html(html, init?)
        \\
        \\Example handler.js:
        \\  function handler(request) {
        \\      if (request.url === '/') {
        \\          return Response.html('<h1>Hello World</h1>');
        \\      }
        \\      if (request.url === '/api/data') {
        \\          return Response.json({ message: 'Hello', method: request.method });
        \\      }
        \\      return Response.text('Not Found', { status: 404 });
        \\  }
        \\
        \\Quick start:
        \\  zigttp-server -e "function handler(r) { return Response.json({ok:true}) }"
        \\  curl http://localhost:8080/
        \\
    ;

    std.fs.File.stdout().writeAll(help) catch {};
}

// ============================================================================
// Tests
// ============================================================================

test "parse size" {
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1k"));
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1K"));
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1kb"));
    try std.testing.expectEqual(@as(usize, 256 * 1024), try parseSize("256k"));
    try std.testing.expectEqual(@as(usize, 1024 * 1024), try parseSize("1m"));
    try std.testing.expectEqual(@as(usize, 1024 * 1024), try parseSize("1mb"));
    try std.testing.expectEqual(@as(usize, 100), try parseSize("100"));
}

test {
    // Run tests from imported modules
    _ = @import("zruntime.zig");
    _ = @import("server.zig");
}
