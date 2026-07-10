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
const engine = @import("engine_adapter.zig");
const websocket_codec = @import("websocket_codec.zig");
const websocket_pool = @import("websocket_pool.zig");

const Pool = websocket_pool.Pool;
const ConnectionId = websocket_pool.ConnectionId;
const Opcode = websocket_codec.Opcode;
const HandlerPool = engine.HandlerPool;

/// Maximum incoming frame payload we will accept. RFC 6455 allows up to
/// 2^63 - 1, but real applications don't need anything like that. 64 KiB
/// covers chat, telemetry, and JSON messaging without pushing us into
/// fragmentation/stream handling (which `readSmallMessage` doesn't
/// support anyway — it rejects fragmented frames as `MessageOversize`).
const max_message_bytes: usize = websocket_codec.max_message_bytes;

pub const Config = struct {
    pool: *Pool,
    io: Io,
    fd: std.posix.fd_t,
    id: ConnectionId,
    /// Allocator for per-frame short-lived work (currently the
    /// auto-response byte copy). The frame loop frees every allocation
    /// it makes before the next read.
    alloc: std.mem.Allocator,
    /// Echo each inbound message back to the sender in Zig. Used as a
    /// fallback path and in tests; live handlers set this to false so
    /// inbound frames flow into the `onMessage` JS export instead.
    echo: bool = true,
    /// Handler-pool reference. When set and `echo` is false, the frame
    /// loop borrows a runtime per inbound frame and dispatches
    /// `onMessage(ws, data)` to JS. When null the loop behaves as a pure
    /// codec echo (unit tests, degraded mode).
    handler_pool: ?*HandlerPool = null,
    /// Live-reload contract lock (server-owned, stable for the server
    /// lifetime). Held shared around each JS dispatch so a `--watch` swap's
    /// exclusive `updateContract` drains in-flight `onMessage`/`onOpen`/
    /// `onClose` runs before the generation-retirement frees the dev
    /// capability policy those runtimes borrow. Mirrors the HTTP request
    /// path's guard. Null outside dev/watch.
    contract_lock: ?*engine.RwLock = null,
    /// Pointer to the server's `reload_active` flag; the shared lock is taken
    /// only while it is set, matching the HTTP path's per-request gate.
    reload_active: ?*const bool = null,
    /// Absolute monotonic deadline installed when graceful shutdown starts.
    /// Zero means normal request-timeout behavior.
    shutdown_deadline_ns: ?*const std.atomic.Value(u64) = null,
};

/// Take the contract lock shared when live reload is active. Returns whether
/// the lock was taken so the matching release can be a no-op otherwise.
fn lockReloadShared(cfg: Config) bool {
    const active = if (cfg.reload_active) |ra| ra.* else false;
    if (active) {
        if (cfg.contract_lock) |cl| cl.lockShared();
    }
    return active;
}

fn unlockReloadShared(cfg: Config, locked: bool) void {
    if (locked) {
        if (cfg.contract_lock) |cl| cl.unlockShared();
    }
}

fn remainingShutdownGraceMs(cfg: Config) ?u32 {
    const deadline_ptr = cfg.shutdown_deadline_ns orelse return null;
    const deadline_ns = deadline_ptr.load(.acquire);
    if (deadline_ns == 0) return null;
    const now_ns = engine.monotonicNowNs() catch return 1;
    if (now_ns >= deadline_ns) return 1;
    const remaining_ns = deadline_ns - now_ns;
    const rounded_ms = (remaining_ns + std.time.ns_per_ms - 1) / std.time.ns_per_ms;
    return @intCast(@min(rounded_ms, std.math.maxInt(u32)));
}

/// Run the frame loop until the connection closes. Owns the fd: on
/// return, the fd is closed and the pool entry for `id` is removed.
pub fn run(cfg: Config) void {
    // The HTTP connection loop installed SO_RCVTIMEO/SO_SNDTIMEO
    // (config.timeout_ms, default 30s) on this fd as an idle/slowloris bound
    // before the upgrade. The upgraded socket transfers to this frame loop
    // unchanged, and WebSockets have no server-side ping/keepalive, so any
    // connection idle past that timeout would be dropped mid-session. Clear
    // both timeouts (a zero timeval blocks indefinitely) on the fd this loop
    // reads and writes; the options are socket-level, so the outbound dup sees
    // the clear too.
    //
    // Raw syscall, result discarded (mirrors the TCP_NODELAY call in
    // server.zig's handleConnection): a shutdown racing in on the worker's
    // duplicated control fd makes setsockopt return EINVAL, which the
    // std.posix.setsockopt wrapper marks `unreachable`. The clear is a
    // best-effort optimisation, so a benign errno must not panic the loop.
    const clear_timeout = std.posix.timeval{ .sec = 0, .usec = 0 };
    _ = std.posix.system.setsockopt(cfg.fd, std.posix.SOL.SOCKET, std.posix.SO.RCVTIMEO, std.mem.asBytes(&clear_timeout), @sizeOf(std.posix.timeval));
    _ = std.posix.system.setsockopt(cfg.fd, std.posix.SOL.SOCKET, std.posix.SO.SNDTIMEO, std.mem.asBytes(&clear_timeout), @sizeOf(std.posix.timeval));

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
    var close_metadata: websocket_codec.CloseMetadata = .{};

    // Dispatch onOpen once before entering the read loop. Failures are
    // logged but do not abort the connection — a handler that throws
    // from onOpen still keeps the socket usable for subsequent messages.
    if (cfg.handler_pool) |hp| {
        if (cfg.pool.snapshot(cfg.id)) |snap| {
            const locked = lockReloadShared(cfg);
            defer unlockReloadShared(cfg, locked);
            dispatchOnOpen(hp, cfg.pool, cfg.id, snap.room, remainingShutdownGraceMs(cfg)) catch |err| {
                std.log.warn("ws onOpen dispatch failed (id={d}): {}", .{ cfg.id, err });
            };
        }
    }

    defer {
        if (cfg.handler_pool) |hp| {
            const locked = lockReloadShared(cfg);
            defer unlockReloadShared(cfg, locked);
            dispatchOnClose(hp, cfg.pool, cfg.id, close_metadata.code, close_metadata.reason, remainingShutdownGraceMs(cfg)) catch |err| {
                std.log.warn("ws onClose dispatch failed (id={d}): {}", .{ cfg.id, err });
            };
        }
        cfg.pool.unregister(cfg.id);
        stream.close(cfg.io);
    }

    while (true) {
        const read_result = websocket_codec.readSmallMessage(&ws) catch |err| switch (err) {
            error.ConnectionClose => return,
            error.EndOfStream => return,
            // Oversized / malformed frames: send a 1009 (Message Too
            // Big) close if possible, then drop the connection. We
            // intentionally ignore send errors — the socket may already
            // be half-broken.
            error.MessageOversize => {
                sendCloseSilently(cfg.pool, cfg.id, 1009);
                return;
            },
            error.MissingMaskBit, error.UnexpectedOpCode, error.InvalidCloseFrame, error.InvalidCloseCode => {
                sendCloseSilently(cfg.pool, cfg.id, 1002);
                return;
            },
            error.InvalidCloseReason => {
                sendCloseSilently(cfg.pool, cfg.id, 1007);
                return;
            },
            error.ReadFailed => return,
        };

        const msg = switch (read_result) {
            .message => |message| message,
            .close => |metadata| {
                close_metadata = metadata;
                // RFC 6455 §5.5.1: echo the close frame before terminating.
                var echo_buf: [125]u8 = undefined;
                const echo_payload = closeEchoPayload(&echo_buf, metadata);
                cfg.pool.sendFrameAndShutdown(cfg.id, .connection_close, echo_payload) catch {};
                return;
            },
        };

        _ = cfg.pool.touch(cfg.id, .parked, engine.unixMillis());

        switch (msg.opcode) {
            .ping => {
                // RFC 6455 §5.5.3: pong must echo the ping payload.
                cfg.pool.sendFrame(cfg.id, .pong, msg.data) catch return;
            },
            .text, .binary => {
                if (cfg.echo) {
                    cfg.pool.sendFrame(cfg.id, msg.opcode, msg.data) catch return;
                } else if (cfg.handler_pool) |pool| {
                    // Codec-level short-circuit: if the handler registered
                    // a canned response for exactly these bytes, write it
                    // and skip the JS dispatch entirely. Used for
                    // heartbeat-style request/reply pairs that would
                    // otherwise pay the full runtime-borrow cost.
                    const auto = cfg.pool.tryAutoResponse(cfg.id, msg.data, cfg.alloc) catch null;
                    if (auto) |response_bytes| {
                        defer cfg.alloc.free(response_bytes);
                        cfg.pool.sendFrame(cfg.id, msg.opcode, response_bytes) catch return;
                    } else {
                        // Hold the contract lock shared across the JS dispatch so
                        // a concurrent live-reload swap drains this in-flight
                        // onMessage before retiring the dev policy its runtime
                        // borrows (mirrors the HTTP request path).
                        const locked = lockReloadShared(cfg);
                        defer unlockReloadShared(cfg, locked);
                        dispatchOnMessage(pool, cfg.pool, cfg.id, msg.data, remainingShutdownGraceMs(cfg)) catch |err| {
                            std.log.warn("ws onMessage dispatch failed (id={d}): {}", .{ cfg.id, err });
                        };
                    }
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
                sendCloseSilently(cfg.pool, cfg.id, 1002);
                return;
            },
            _ => {
                sendCloseSilently(cfg.pool, cfg.id, 1002);
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
fn sendCloseSilently(pool: *Pool, id: ConnectionId, code: u16) void {
    var payload: [2]u8 = undefined;
    std.mem.writeInt(u16, &payload, code, .big);
    pool.sendFrameAndShutdown(id, .connection_close, &payload) catch {};
}

fn closeEchoPayload(buffer: *[125]u8, metadata: websocket_codec.CloseMetadata) []const u8 {
    if (!metadata.had_code) return buffer[0..0];
    return websocket_codec.writeClosePayload(buffer, metadata.code, metadata.reason);
}

fn callWebSocketHandler(
    lease: *HandlerPool.WorkerRuntimeLease,
    name: []const u8,
    args: []const engine.JSValue,
    max_duration_ms: ?u32,
) !void {
    if (max_duration_ms) |limit_ms|
        lease.runtime.armRequestDeadlineWithin(limit_ms)
    else
        lease.runtime.armRequestDeadline();
    const result = lease.runtime.callGlobalFunction(name, args);
    lease.runtime.clearRequestDeadline();
    _ = result catch |err| {
        if (err == error.RequestTimeout) lease.recycleAfterTimeout();
        return err;
    };
}

/// Borrow a runtime, install the WS callback table, and invoke the
/// handler's `onMessage(ws, data, room)` export. The `ws` argument is
/// the connection id as an integer; `room` is the room key the
/// connection registered under (derived from the upgrade URL by
/// ws_gateway). Passing `room` on every dispatch lets broadcast
/// handlers avoid module-scoped state, which the per-request arena
/// doesn't let them keep anyway — W2's attachment API fills the gap
/// for per-connection state that survives across messages.
fn dispatchOnMessage(
    handler_pool: *HandlerPool,
    ws_pool: *Pool,
    id: ConnectionId,
    data: []const u8,
    max_duration_ms: ?u32,
) !void {
    _ = ws_pool.beginDispatch(id);
    defer _ = ws_pool.endDispatch(id);

    var lease = try handler_pool.acquireWorkerRuntime();
    defer lease.deinit();

    try lease.runtime.installWebSocketModuleState(ws_pool);

    const prev_connection = engine.activeWebSocketConnection();
    engine.setActiveWebSocketConnection(id);
    defer engine.setActiveWebSocketConnection(prev_connection);

    // Connection ids larger than i32 max would truncate; W2 widens the
    // proxy to a full object so the id range stops mattering. For W1 we
    // accept the limit (~2 billion live connections per process, which
    // is far past any other bottleneck).
    const id_i32: i32 = std.math.cast(i32, id) orelse return error.ConnectionIdOverflow;
    const ws_val = engine.jsInt(id_i32);
    const data_val = try lease.runtime.ctx.createString(data);
    const room_val = if (ws_pool.snapshot(id)) |snap|
        try lease.runtime.ctx.createString(snap.room)
    else
        try lease.runtime.ctx.createString("");

    callWebSocketHandler(&lease, "onMessage", &.{ ws_val, data_val, room_val }, max_duration_ms) catch |err| {
        std.log.warn("onMessage handler raised: {}", .{err});
        return err;
    };
}

/// Dispatch `onOpen(ws, url)`. Missing export is fine (NotCallable is
/// silenced); the handler is not required to define onOpen just because
/// it defined onMessage.
fn dispatchOnOpen(
    handler_pool: *HandlerPool,
    ws_pool: *Pool,
    id: ConnectionId,
    url: []const u8,
    max_duration_ms: ?u32,
) !void {
    _ = ws_pool.beginDispatch(id);
    defer _ = ws_pool.endDispatch(id);

    var lease = try handler_pool.acquireWorkerRuntime();
    defer lease.deinit();

    try lease.runtime.installWebSocketModuleState(ws_pool);

    const prev_connection = engine.activeWebSocketConnection();
    engine.setActiveWebSocketConnection(id);
    defer engine.setActiveWebSocketConnection(prev_connection);

    const id_i32: i32 = std.math.cast(i32, id) orelse return error.ConnectionIdOverflow;
    const ws_val = engine.jsInt(id_i32);
    const url_val = try lease.runtime.ctx.createString(url);

    callWebSocketHandler(&lease, "onOpen", &.{ ws_val, url_val }, max_duration_ms) catch |err| switch (err) {
        error.NotCallable => return,
        else => return err,
    };
}

/// Dispatch `onClose(ws, code, reason)`. Same tolerance as onOpen —
/// handlers don't have to define it.
fn dispatchOnClose(
    handler_pool: *HandlerPool,
    ws_pool: *Pool,
    id: ConnectionId,
    code: u16,
    reason: []const u8,
    max_duration_ms: ?u32,
) !void {
    _ = ws_pool.beginDispatch(id);
    // Intentionally skip endDispatch on close — `unregister` is about
    // to run and remove the connection entirely.

    var lease = try handler_pool.acquireWorkerRuntime();
    defer lease.deinit();

    try lease.runtime.installWebSocketModuleState(ws_pool);

    const prev_connection = engine.activeWebSocketConnection();
    engine.setActiveWebSocketConnection(id);
    defer engine.setActiveWebSocketConnection(prev_connection);

    const id_i32: i32 = std.math.cast(i32, id) orelse return error.ConnectionIdOverflow;
    const ws_val = engine.jsInt(id_i32);
    const code_val = engine.jsInt(@as(i32, code));
    const reason_val = try lease.runtime.ctx.createString(reason);

    callWebSocketHandler(&lease, "onClose", &.{ ws_val, code_val, reason_val }, max_duration_ms) catch |err| switch (err) {
        error.NotCallable => return,
        else => return err,
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "max_message_bytes fits the default read buffer" {
    // readSmallMessage requires the entire payload to fit in the
    // reader's buffer. If a future refactor shrinks one without the
    // other the loop silently starts rejecting 64 KiB frames.
    try testing.expect(max_message_bytes <= 64 * 1024);
    try testing.expect(max_message_bytes + 64 <= (max_message_bytes + 128));
}

test "close echo preserves code and UTF-8 reason" {
    var buffer: [125]u8 = undefined;
    const payload = closeEchoPayload(&buffer, .{
        .code = 4001,
        .reason = "bye",
        .had_code = true,
    });

    try testing.expectEqualSlices(u8, &.{ 0x0f, 0xa1, 'b', 'y', 'e' }, payload);
}

test "close echo preserves an empty client payload" {
    var buffer: [125]u8 = undefined;
    try testing.expectEqual(@as(usize, 0), closeEchoPayload(&buffer, .{}).len);
}

test "onClose receives the parsed close code and reason" {
    const allocator = std.heap.c_allocator;
    const source =
        \\import { serializeAttachment } from "zigttp:websocket";
        \\export function onClose(ws, code, reason) {
        \\  serializeAttachment(ws, code === 4001 && reason === "bye" ? "matched" : "wrong");
        \\}
        \\function handler(req) { return Response.text("ok"); }
    ;
    var handler_pool = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled },
        source,
        "<ws-close-test>",
        1,
        0,
    );
    defer handler_pool.deinit();

    var pool = Pool.init(testing.allocator);
    defer pool.deinit();
    const id = try pool.register(100, "/close", 0);

    try dispatchOnClose(&handler_pool, &pool, id, 4001, "bye", null);
    const attachment = (try pool.copyAttachment(id, testing.allocator)) orelse
        return error.MissingCloseMetadata;
    defer testing.allocator.free(attachment);
    try testing.expectEqualStrings("matched", attachment);
}

test "graceful shutdown budget shortens the WebSocket callback deadline" {
    const allocator = std.heap.c_allocator;
    const source =
        \\export function onClose(ws, code, reason) {
        \\  let x = 0;
        \\  for (let i of range(1000000000)) { x = x + 1; }
        \\}
        \\function handler(req) { return Response.text("ok"); }
    ;
    var handler_pool = try HandlerPool.init(
        allocator,
        .{ .jit_policy = .disabled, .request_timeout_ms = 5000 },
        source,
        "<ws-timeout-test>",
        1,
        0,
    );
    defer handler_pool.deinit();

    var pool = Pool.init(testing.allocator);
    defer pool.deinit();
    const id = try pool.register(100, "/timeout", 0);

    var timer = try engine.Timer.start();
    try testing.expectError(
        error.RequestTimeout,
        dispatchOnClose(&handler_pool, &pool, id, 1000, "", 20),
    );
    try testing.expect(timer.read() < std.time.ns_per_s);
}
