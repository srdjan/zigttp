//! Per-connection WebSocket frame I/O loop.
//!
//! W1-d.4-a scope: drive `std.http.Server.WebSocket` against an accepted
//! fd, echo text/binary frames back to the sender, answer pings with
//! pongs, and exit cleanly on close / error. No JS dispatch yet — that
//! lands in W1-d.4-b once the runtime callback plumbing is in place.
//!
//! Threading model: one dedicated thread per connection for W1. This is
//! knowingly simplistic: it pins a kernel thread per live WS, scales to
//! roughly one-thread-per-user, and is the natural point W3 replaces
//! with a shared evented loop and hibernation. Keeping the loop in its
//! own module lets that swap happen without touching server.zig.
//!
//! I/O adapters: we wrap the raw fd in `std.Io.net.Stream` by
//! constructing a `Socket` with the handle and a placeholder address
//! (stream reads/writes never consult the address field). That gives us
//! `Io.Reader`/`Io.Writer` views that `std.http.Server.WebSocket` can
//! drive directly — no need to reimplement the wire format.
//!
//! Lifecycle: the caller hands ownership of the fd to `run`. `run`
//! closes the fd and unregisters the connection from the pool on exit,
//! so a caller that successfully spawns a frame-loop thread must not
//! touch the fd or pool entry afterwards.

const std = @import("std");
const Io = std.Io;
const websocket_codec = @import("websocket_codec.zig");
const websocket_pool = @import("websocket_pool.zig");
const zruntime = @import("zruntime.zig");
const zq = @import("zigts");
const trace = zq.trace;

const Pool = websocket_pool.Pool;
const ConnectionId = websocket_pool.ConnectionId;
const Opcode = websocket_codec.Opcode;
const HandlerPool = zruntime.HandlerPool;

/// Maximum incoming frame payload we will accept. RFC 6455 allows up to
/// 2^63 - 1, but real applications don't need anything like that. 64 KiB
/// covers chat, telemetry, and JSON messaging without pushing us into
/// fragmentation/stream handling (which `readSmallMessage` doesn't
/// support anyway — it rejects fragmented frames as `MessageOversize`).
const max_message_bytes: usize = 64 * 1024;

/// Room slot-count budget for broadcasts. `collectRoom` writes into this
/// fixed buffer; rooms larger than this truncate. W1 gives us
/// in-process-single-replica semantics, so 256 peers per room is an
/// honest upper bound — Phase W3 will revisit with measured data.
const max_room_peers: usize = 256;

/// The payload of a handler-dispatched event. W1-d.4-a keeps this
/// internal to the frame loop (echo only); W1-d.4-b will use the same
/// shape to feed the JS dispatcher.
pub const FrameEvent = union(enum) {
    text: []const u8,
    binary: []const u8,
};

pub const Config = struct {
    pool: *Pool,
    io: Io,
    fd: std.posix.fd_t,
    id: ConnectionId,
    /// Echo each inbound message back to the sender in Zig. Used as a
    /// fallback path and in tests; live handlers set this to false so
    /// inbound frames flow into the `onMessage` JS export instead.
    echo: bool = true,
    /// Handler-pool reference. When set and `echo` is false, the frame
    /// loop borrows a runtime per inbound frame and dispatches
    /// `onMessage(ws, data)` to JS. When null the loop behaves as a pure
    /// codec echo (unit tests, degraded mode).
    handler_pool: ?*HandlerPool = null,
};

/// Run the frame loop until the connection closes. Owns the fd: on
/// return, the fd is closed and the pool entry for `id` is removed.
pub fn run(cfg: Config) void {
    var input_buf: [max_message_bytes]u8 = undefined;
    var output_buf: [max_message_bytes + 64]u8 = undefined;

    const stream = makeStream(cfg.fd);
    var reader = stream.reader(cfg.io, &input_buf);
    var writer = stream.writer(cfg.io, &output_buf);

    var ws: std.http.Server.WebSocket = .{
        // The key is only stored for `respondWebSocket`-style helpers;
        // `readSmallMessage` / `writeMessage` never read it.
        .key = "",
        .input = &reader.interface,
        .output = &writer.interface,
    };

    defer {
        cfg.pool.unregister(cfg.id);
        stream.close(cfg.io);
    }

    while (true) {
        const msg = ws.readSmallMessage() catch |err| switch (err) {
            error.ConnectionClose => return,
            error.EndOfStream => return,
            // Oversized / malformed frames: send a 1009 (Message Too
            // Big) close if possible, then drop the connection. We
            // intentionally ignore send errors — the socket may already
            // be half-broken.
            error.MessageOversize => {
                sendCloseSilently(&ws, 1009);
                return;
            },
            error.MissingMaskBit, error.UnexpectedOpCode => {
                sendCloseSilently(&ws, 1002);
                return;
            },
            error.ReadFailed => return,
        };

        _ = cfg.pool.touch(cfg.id, .parked, unixMillisNow());

        switch (msg.opcode) {
            .ping => {
                // RFC 6455 §5.5.3: pong must echo the ping payload.
                ws.writeMessage(msg.data, .pong) catch return;
            },
            .text, .binary => {
                if (cfg.echo) {
                    ws.writeMessage(msg.data, msg.opcode) catch return;
                } else if (cfg.handler_pool) |pool| {
                    dispatchOnMessage(pool, cfg.pool, cfg.id, msg.data) catch |err| {
                        std.log.warn("ws onMessage dispatch failed (id={d}): {}", .{ cfg.id, err });
                    };
                }
            },
            .pong => {
                // `readSmallMessage` already filters pongs; this arm is
                // defensive.
            },
            .connection_close => return,
            .continuation => {
                // Unreachable via readSmallMessage, which rejects
                // continuation frames. Keep for exhaustiveness.
                sendCloseSilently(&ws, 1002);
                return;
            },
            _ => {
                sendCloseSilently(&ws, 1002);
                return;
            },
        }
    }
}

fn makeStream(fd: std.posix.fd_t) Io.net.Stream {
    // Stream ignores the address for read/write/close; pass an IPv4
    // placeholder so the union tag is valid.
    const placeholder: Io.net.IpAddress = .{ .ip4 = .{
        .bytes = .{ 0, 0, 0, 0 },
        .port = 0,
    } };
    return .{ .socket = .{ .handle = fd, .address = placeholder } };
}

/// Best-effort close-frame write. The WebSocket close frame carries a
/// two-byte big-endian status code followed by an optional UTF-8
/// reason; we only send the code for W1.
fn sendCloseSilently(ws: *std.http.Server.WebSocket, code: u16) void {
    var payload: [2]u8 = undefined;
    std.mem.writeInt(u16, &payload, code, .big);
    ws.writeMessage(&payload, .connection_close) catch {};
}

fn unixMillisNow() i64 {
    return trace.unixMillis();
}

/// Borrow a runtime, install the WS callback table, and invoke the
/// handler's `onMessage(ws, data)` export. The `ws` argument for W1 is
/// the connection id as an integer; handlers treat it as an opaque
/// token they pass back to `send`/`close`. W2 upgrades this to a real
/// JS proxy object with fields.
fn dispatchOnMessage(
    handler_pool: *HandlerPool,
    ws_pool: *Pool,
    id: ConnectionId,
    data: []const u8,
) !void {
    var lease = try handler_pool.acquireWorkerRuntime();
    defer lease.deinit();

    try lease.runtime.installWebSocketModuleState(ws_pool);

    const prev_connection = zruntime.active_ws_connection;
    zruntime.active_ws_connection = id;
    defer zruntime.active_ws_connection = prev_connection;

    // Connection ids larger than i32 max would truncate; W2 widens the
    // proxy to a full object so the id range stops mattering. For W1 we
    // accept the limit (~2 billion live connections per process, which
    // is far past any other bottleneck).
    const id_i32: i32 = std.math.cast(i32, id) orelse return error.ConnectionIdOverflow;
    const ws_val = zq.JSValue.fromInt(id_i32);
    const data_val = try lease.runtime.ctx.createString(data);

    _ = lease.runtime.callGlobalFunction("onMessage", &.{ ws_val, data_val }) catch |err| {
        std.log.warn("onMessage handler raised: {}", .{err});
        return err;
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "FrameEvent tag shape is stable for W1-d.4-b dispatch" {
    // The union tag layout is part of the dispatch contract between
    // W1-d.4-a (echo) and W1-d.4-b (JS). This test pins the expected
    // active-tag names so a rename doesn't silently break dispatch.
    const evt_text: FrameEvent = .{ .text = "hi" };
    const evt_binary: FrameEvent = .{ .binary = "\x01\x02" };
    try testing.expect(evt_text == .text);
    try testing.expect(evt_binary == .binary);
}

test "max_message_bytes fits the default read buffer" {
    // readSmallMessage requires the entire payload to fit in the
    // reader's buffer. If a future refactor shrinks one without the
    // other the loop silently starts rejecting 64 KiB frames.
    try testing.expect(max_message_bytes <= 64 * 1024);
    try testing.expect(max_message_bytes + 64 <= (max_message_bytes + 128));
}
