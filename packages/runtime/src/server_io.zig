//! Low-level I/O and HTTP utility helpers extracted from server.zig.
//!
//! All pure free functions over file descriptors, header slices, or
//! HttpHeader values — no Server struct coupling. Sibling to
//! server_response.zig and server_static.zig; these three together hold
//! the framing-and-IO primitives that server.zig composes into the
//! request lifecycle.

const std = @import("std");
const Io = std.Io;
const engine = @import("engine_adapter.zig");
const http_types = @import("http_types.zig");

const HttpHeader = http_types.HttpHeader;

/// True when a request's `Upgrade` header advertises a WebSocket upgrade.
/// Tolerant of multi-token upgrade values (rare, but legal per RFC 7230).
pub fn requestIsWebSocketUpgrade(headers: []const HttpHeader) bool {
    for (headers) |h| {
        if (std.ascii.eqlIgnoreCase(h.key, "upgrade")) {
            var it = std.mem.splitScalar(u8, h.value, ',');
            while (it.next()) |token| {
                const trimmed = std.mem.trim(u8, token, " \t");
                if (std.ascii.eqlIgnoreCase(trimmed, "websocket")) return true;
            }
        }
    }
    return false;
}

pub fn unixMillisNow() i64 {
    return engine.unixMillis();
}

/// Loop-until-done write to a raw file descriptor. Used by the threaded
/// backend for response sends that bypass the std.Io.Stream writer.
pub fn writeAllFd(fd: std.posix.fd_t, data: []const u8) !void {
    var remaining = data;
    while (remaining.len > 0) {
        const result = std.c.write(fd, remaining.ptr, remaining.len);
        if (result < 0) return error.WriteFailed;
        const n: usize = @intCast(result);
        if (n == 0) return error.WriteFailed;
        remaining = remaining[n..];
    }
}

/// True when `If-None-Match` header value matches `etag_hex`. Tolerant of
/// `W/` weak-validator prefix and surrounding double quotes per RFC 9110.
pub fn etagMatchesIfNoneMatch(if_none_match: ?[]const u8, etag_hex: []const u8) bool {
    const raw = if_none_match orelse return false;
    var trimmed = raw;
    if (std.mem.startsWith(u8, trimmed, "W/")) trimmed = trimmed[2..];
    trimmed = std.mem.trim(u8, trimmed, "\"");
    return std.mem.eql(u8, trimmed, etag_hex);
}

pub fn createUnixSocketPair() ![2]std.posix.fd_t {
    if (@TypeOf(std.posix.system.socketpair) == void) {
        return error.OperationUnsupported;
    }

    var fds: [2]std.posix.fd_t = undefined;
    while (true) switch (std.posix.errno(
        std.posix.system.socketpair(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0, &fds),
    )) {
        .SUCCESS => return fds,
        .INTR => continue,
        else => |err| return std.posix.unexpectedErrno(err),
    };
}

/// Write multiple buffers to fd using writev() scatter-gather I/O.
/// Avoids copying multiple buffers into one before writing.
pub fn writevAllFd(fd: std.posix.fd_t, iovecs: []std.posix.iovec_const) !void {
    var remaining_iovecs = iovecs;
    var first_offset: usize = 0;

    while (remaining_iovecs.len > 0) {
        // Adjust first iovec if we had a partial write
        var adjusted = remaining_iovecs;
        if (first_offset > 0) {
            adjusted[0].base = @ptrCast(@as([*]const u8, @ptrCast(adjusted[0].base)) + first_offset);
            adjusted[0].len -= first_offset;
        }

        const result = std.c.writev(fd, adjusted.ptr, @intCast(@min(adjusted.len, std.math.maxInt(c_int))));
        if (result < 0) return error.WriteFailed;
        const n: usize = @intCast(result);
        if (n == 0) return error.WriteFailed;

        // Advance through iovecs based on bytes written
        var bytes_remaining = n;
        while (bytes_remaining > 0 and remaining_iovecs.len > 0) {
            const current_len = if (first_offset > 0)
                remaining_iovecs[0].len - first_offset
            else
                remaining_iovecs[0].len;

            if (bytes_remaining >= current_len) {
                bytes_remaining -= current_len;
                remaining_iovecs = remaining_iovecs[1..];
                first_offset = 0;
            } else {
                first_offset += bytes_remaining;
                bytes_remaining = 0;
            }
        }
    }
}

/// Returns true for network errors that are expected during normal operation.
/// These include client disconnects, timeouts, and connection resets which
/// occur naturally under load and should not be logged at error level.
pub fn isExpectedNetworkError(err: anyerror) bool {
    return switch (err) {
        error.Canceled,
        error.RequestTimedOut,
        error.EndOfStream,
        error.ConnectionResetByPeer,
        error.BrokenPipe,
        error.ConnectionRefused,
        error.ReadFailed,
        error.WriteFailed,
        => true,
        else => false,
    };
}

pub fn findHeaderValue(headers: []const HttpHeader, name: []const u8) ?[]const u8 {
    for (headers) |header| {
        if (std.ascii.eqlIgnoreCase(header.key, name)) {
            return header.value;
        }
    }
    return null;
}

pub fn defaultPoolSize() usize {
    const cpu_count = std.Thread.getCpuCount() catch 1;
    const min_pool: usize = 8;
    const max_pool: usize = 128;
    const base = cpu_count * 2;
    if (base < min_pool) return min_pool;
    if (base > max_pool) return max_pool;
    return base;
}

pub fn initIoBackend(io: anytype, allocator: std.mem.Allocator) !void {
    const Backend = @TypeOf(io.*);
    if (Backend == Io.Threaded) {
        io.* = Io.Threaded.init(allocator, .{ .environ = .empty });
        return;
    }
    if (@hasDecl(Backend, "InitOptions")) {
        try io.init(allocator, .{});
    } else {
        try io.init(allocator);
    }
}
