//! Handshake-time orchestration for WebSocket upgrades.
//!
//! W1-d.2 scope: take a parsed HTTP request that carries an Upgrade
//! header, run the RFC 6455 validation, write the 101 response, and
//! register the connection in `websocket_pool`. Frame I/O and
//! onOpen/onMessage/onClose dispatch land in subsequent sub-slices;
//! a caller that invokes `upgrade` and gets `.ok(id)` owns the fd for
//! the remaining frame loop work.
//!
//! Why this lives separately from `websocket_codec`: the codec is the
//! pure protocol machinery (stateless header math, re-exports of
//! std.http.Server.WebSocket's wire types). The gateway is the glue
//! that pulls codec + pool + the server's socket writer together.
//! Keeping them apart lets the codec stay testable without touching
//! any I/O primitives.
//!
//! Failure shapes: every validation error maps to a well-typed result
//! so the caller (server.zig) can choose the right HTTP status without
//! re-parsing the error name. Missing/bad Upgrade headers become 400;
//! wrong WebSocket version becomes 426 with a Sec-WebSocket-Version
//! header per RFC 6455 §4.4; unexpected I/O failures propagate so the
//! outer accept loop can decide whether to close the socket.

const std = @import("std");
const http_types = @import("http_types.zig");
const websocket_codec = @import("websocket_codec.zig");
const websocket_pool = @import("websocket_pool.zig");
const trace = @import("zigts").trace;

const HttpHeader = http_types.HttpHeader;
pub const Pool = websocket_pool.Pool;
pub const ConnectionId = websocket_pool.ConnectionId;

/// Result of a handshake attempt. `reject` carries a plain HTTP status
/// so the server can emit the error without running through the
/// handshake response path.
pub const UpgradeOutcome = union(enum) {
    ok: ConnectionId,
    reject: RejectReason,
};

pub const RejectReason = struct {
    status: u16,
    reason: []const u8,
    /// When the rejection is specifically about an unsupported
    /// `Sec-WebSocket-Version`, the server should add a
    /// `Sec-WebSocket-Version: 13` header to the 426 response per RFC
    /// 6455 §4.4.
    wants_version_header: bool = false,
};

/// Orchestrate a full handshake:
///
///  1. Validate upgrade headers.
///  2. Compute the accept key.
///  3. Write the 101 response to `writer`. Caller is responsible for
///     flushing (most writers here buffer internally — `flush_fn` lets
///     a caller inject a flush without this module needing to know
///     the writer's dialect).
///  4. Register the connection in `pool` under a room key derived from
///     the request path (see `deriveRoomKey`).
///
/// On validation failure, nothing is written to the socket and nothing
/// is registered in the pool — the caller is expected to send its own
/// HTTP error response using the returned `reject.status`.
pub fn upgrade(
    pool: *Pool,
    writer: anytype,
    fd: std.posix.fd_t,
    request: *const http_types.HttpRequestView,
    now_ms: i64,
) !UpgradeOutcome {
    const parsed = websocket_codec.validateUpgrade(request.headers.items) catch |err| {
        return .{ .reject = mapValidationError(err) };
    };

    var accept_key: [websocket_codec.accept_key_len]u8 = undefined;
    websocket_codec.computeAcceptKey(&accept_key, parsed.client_key);

    try websocket_codec.writeHandshakeResponse(writer, &accept_key);

    const room_key = deriveRoomKey(request.path);
    const id = try pool.register(fd, room_key, now_ms);
    return .{ .ok = id };
}

/// Release a connection from the pool. The caller is responsible for
/// closing the fd; this helper exists so cleanup code paths don't
/// need to know about the pool's internal structure.
pub fn releaseConnection(pool: *Pool, id: ConnectionId) void {
    pool.unregister(id);
}

/// Default room-key derivation: use the request path minus the query
/// string. The path slice already excludes the query in
/// `HttpRequestView`, so this is just the path itself. A handler can
/// later override the room assignment via the `roomFromPath` /
/// attachment path, but that plumbing lands with the
/// `onOpen`/`onMessage` dispatch sub-slice.
fn deriveRoomKey(path: []const u8) []const u8 {
    if (path.len == 0) return "/";
    return path;
}

fn mapValidationError(err: websocket_codec.UpgradeError) RejectReason {
    return switch (err) {
        error.UnsupportedWebSocketVersion => .{
            .status = 426,
            .reason = "Upgrade Required",
            .wants_version_header = true,
        },
        error.MissingUpgradeHeader,
        error.MissingConnectionHeader,
        error.MissingWebSocketKey,
        error.InvalidWebSocketKey,
        error.MissingWebSocketVersion,
        => .{
            .status = 400,
            .reason = "Bad Request",
        },
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn buildRequest(
    allocator: std.mem.Allocator,
    path: []const u8,
    headers: []const HttpHeader,
) !http_types.HttpRequestView {
    var list: std.ArrayListUnmanaged(HttpHeader) = .empty;
    try list.appendSlice(allocator, headers);
    return .{
        .method = "GET",
        .url = path,
        .path = path,
        .query_params = &.{},
        .headers = list,
        .body = null,
    };
}

test "upgrade writes 101 response and registers connection" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);

    const headers = [_]HttpHeader{
        .{ .key = "Upgrade", .value = "websocket" },
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "Sec-WebSocket-Version", .value = "13" },
    };
    var request = try buildRequest(testing.allocator, "/chat/alpha", &headers);
    defer request.headers.deinit(testing.allocator);

    const outcome = try upgrade(&pool, &aw.writer, 99, &request, 1000);
    buf = aw.toArrayList();

    try testing.expect(outcome == .ok);

    // Response body must be the exact RFC 6455 101 with the known
    // accept key for the canonical test vector.
    try testing.expect(std.mem.startsWith(u8, buf.items, "HTTP/1.1 101 Switching Protocols\r\n"));
    try testing.expect(std.mem.indexOf(u8, buf.items, "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n") != null);
    try testing.expect(std.mem.endsWith(u8, buf.items, "\r\n\r\n"));

    // Pool membership: room derived from path, id is the one returned.
    try testing.expectEqual(@as(usize, 1), pool.countInRoom("/chat/alpha"));
    const snap = pool.snapshot(outcome.ok) orelse return error.TestFailed;
    try testing.expectEqual(@as(std.posix.fd_t, 99), snap.fd);
    try testing.expectEqualStrings("/chat/alpha", snap.room);
    try testing.expectEqual(@as(i64, 1000), snap.last_frame_at_ms);
}

test "upgrade rejects wrong version with 426 + wants_version_header" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);

    const headers = [_]HttpHeader{
        .{ .key = "Upgrade", .value = "websocket" },
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "Sec-WebSocket-Version", .value = "8" },
    };
    var request = try buildRequest(testing.allocator, "/chat/alpha", &headers);
    defer request.headers.deinit(testing.allocator);

    const outcome = try upgrade(&pool, &aw.writer, 99, &request, 1000);
    buf = aw.toArrayList();

    try testing.expect(outcome == .reject);
    try testing.expectEqual(@as(u16, 426), outcome.reject.status);
    try testing.expect(outcome.reject.wants_version_header);
    try testing.expectEqual(@as(usize, 0), buf.items.len);
    try testing.expectEqual(@as(usize, 0), pool.countInRoom("/chat/alpha"));
}

test "upgrade rejects missing Sec-WebSocket-Key with 400" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);

    const headers = [_]HttpHeader{
        .{ .key = "Upgrade", .value = "websocket" },
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Version", .value = "13" },
    };
    var request = try buildRequest(testing.allocator, "/chat/alpha", &headers);
    defer request.headers.deinit(testing.allocator);

    const outcome = try upgrade(&pool, &aw.writer, 99, &request, 1000);

    try testing.expect(outcome == .reject);
    try testing.expectEqual(@as(u16, 400), outcome.reject.status);
    try testing.expect(!outcome.reject.wants_version_header);
    try testing.expectEqual(@as(usize, 0), pool.countInRoom("/chat/alpha"));
}

test "upgrade registrations coexist in the same room" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    var buf_a: std.ArrayList(u8) = .empty;
    defer buf_a.deinit(testing.allocator);
    var aw_a: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf_a);

    var buf_b: std.ArrayList(u8) = .empty;
    defer buf_b.deinit(testing.allocator);
    var aw_b: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf_b);

    const headers = [_]HttpHeader{
        .{ .key = "Upgrade", .value = "websocket" },
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "Sec-WebSocket-Version", .value = "13" },
    };
    var req_a = try buildRequest(testing.allocator, "/room", &headers);
    defer req_a.headers.deinit(testing.allocator);
    var req_b = try buildRequest(testing.allocator, "/room", &headers);
    defer req_b.headers.deinit(testing.allocator);

    const a = try upgrade(&pool, &aw_a.writer, 10, &req_a, 1);
    const b = try upgrade(&pool, &aw_b.writer, 11, &req_b, 2);
    buf_a = aw_a.toArrayList();
    buf_b = aw_b.toArrayList();

    try testing.expectEqual(@as(usize, 2), pool.countInRoom("/room"));
    try testing.expect(a.ok != b.ok);
}

test "releaseConnection removes from the pool" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);

    const headers = [_]HttpHeader{
        .{ .key = "Upgrade", .value = "websocket" },
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "Sec-WebSocket-Version", .value = "13" },
    };
    var request = try buildRequest(testing.allocator, "/room", &headers);
    defer request.headers.deinit(testing.allocator);

    const outcome = try upgrade(&pool, &aw.writer, 99, &request, 1);
    buf = aw.toArrayList();
    try testing.expect(outcome == .ok);
    try testing.expectEqual(@as(usize, 1), pool.countInRoom("/room"));

    releaseConnection(&pool, outcome.ok);
    try testing.expectEqual(@as(usize, 0), pool.countInRoom("/room"));
}

test "deriveRoomKey defaults to '/' for empty path" {
    try testing.expectEqualStrings("/", deriveRoomKey(""));
    try testing.expectEqualStrings("/chat/alpha", deriveRoomKey("/chat/alpha"));
}
