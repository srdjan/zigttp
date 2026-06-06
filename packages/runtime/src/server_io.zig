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
        if (result < 0) {
            // A delivered signal must not truncate the response; retry.
            if (std.posix.errno(result) == .INTR) continue;
            return error.WriteFailed;
        }
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
    // `remaining[0]` may be partially written; `first_offset` is the number of
    // bytes already sent from it. Present writev with an adjusted *copy* of the
    // first entry (saved and restored so the caller's array is never left
    // mutated) and account `first_offset` exactly once. The previous version
    // shrank `remaining[0].len` in place and then subtracted `first_offset`
    // again when advancing, double-counting it and silently truncating bodies
    // larger than the socket send buffer.
    var remaining = iovecs;
    var first_offset: usize = 0;

    while (remaining.len > 0) {
        const saved = remaining[0];
        remaining[0] = .{
            .base = @ptrCast(@as([*]const u8, @ptrCast(saved.base)) + first_offset),
            .len = saved.len - first_offset,
        };
        const result = std.c.writev(fd, remaining.ptr, @intCast(@min(remaining.len, std.math.maxInt(c_int))));
        remaining[0] = saved;

        if (result < 0) {
            // Nothing was written on EINTR; retry rather than truncate.
            if (std.posix.errno(result) == .INTR) continue;
            return error.WriteFailed;
        }
        const n: usize = @intCast(result);
        if (n == 0) return error.WriteFailed;

        const cursor = advanceIovecCursor(remaining, first_offset, n);
        remaining = cursor.remaining;
        first_offset = cursor.first_offset;
    }
}

const IovecCursor = struct { remaining: []std.posix.iovec_const, first_offset: usize };

/// Advance the (remaining iovecs, offset-into-first) cursor by `written` bytes.
/// `avail` is the still-unwritten length of the current first entry: its full
/// length minus the bytes already accounted by `first_offset`. Pure - extracted
/// so the partial-write accounting is testable without real socket I/O.
fn advanceIovecCursor(remaining: []std.posix.iovec_const, first_offset: usize, written: usize) IovecCursor {
    var rem = remaining;
    var off = first_offset;
    var n = written;
    while (n > 0 and rem.len > 0) {
        const avail = rem[0].len - off;
        if (n >= avail) {
            n -= avail;
            rem = rem[1..];
            off = 0;
        } else {
            off += n;
            n = 0;
        }
    }
    return .{ .remaining = rem, .first_offset = off };
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

test "advanceIovecCursor accounts partial writes without double-counting the offset" {
    const hdr = "HEADER-100-bytes";
    const body = "BODY";
    var iovecs = [_]std.posix.iovec_const{
        .{ .base = hdr.ptr, .len = 100 },
        .{ .base = body.ptr, .len = 200 },
    };
    const total: usize = 300;

    // Drive a worst-case partial-write schedule (a sub-header write, then a
    // write that finishes the header and dips into the body, then the rest).
    // The previous offset double-count advanced past the header early and
    // mis-attributed bytes to the body, truncating the response.
    const schedule = [_]usize{ 50, 80, 170 };
    var rem: []std.posix.iovec_const = iovecs[0..];
    var off: usize = 0;
    var consumed: usize = 0;
    for (schedule) |w| {
        const c = advanceIovecCursor(rem, off, w);
        rem = c.remaining;
        off = c.first_offset;
        consumed += w;
    }
    try std.testing.expectEqual(total, consumed);
    try std.testing.expectEqual(@as(usize, 0), rem.len); // every iovec fully sent
    try std.testing.expectEqual(@as(usize, 0), off);

    // A single full write consumes everything in one step.
    const one = advanceIovecCursor(iovecs[0..], 0, total);
    try std.testing.expectEqual(@as(usize, 0), one.remaining.len);

    // A mid-first-iovec partial leaves the cursor on the first entry.
    const part = advanceIovecCursor(iovecs[0..], 0, 30);
    try std.testing.expectEqual(@as(usize, 2), part.remaining.len);
    try std.testing.expectEqual(@as(usize, 30), part.first_offset);
}
