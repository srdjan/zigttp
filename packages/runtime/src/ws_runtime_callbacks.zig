//! WebSocket runtime callbacks: the native implementations behind the
//! `zigttp:websocket` module exports (send / close / serializeAttachment /
//! deserializeAttachment / getWebSockets / roomFromPath / setAutoResponse).
//!
//! Extracted from zruntime.zig to keep the request-lifecycle struct focused.
//! These are registered by `Runtime.installWebSocketModuleState`; each callback
//! receives the owning Runtime as an opaque pointer and dispatches against its
//! WebSocket pool (`Runtime.ws_pool_ref`). The `active_ws_connection`
//! thread-local stays in zruntime.zig because the frame loop owns it and these
//! callbacks take the connection id from their first JS argument instead.

const std = @import("std");
const zq = @import("zigts");
const websocket_pool = @import("websocket_pool.zig");
const server_io = @import("server_io.zig");
const zruntime = @import("zruntime.zig");

const Runtime = zruntime.Runtime;
const getStringDataCtx = zq.builtins.helpers.getStringDataCtx;

pub fn wsConnectionIdFromArg(arg: zq.JSValue) ?u64 {
    if (arg.isInt()) {
        const raw = arg.getInt();
        if (raw <= 0) return null;
        return @as(u64, @intCast(raw));
    }
    return null;
}

pub fn wsPoolFromRuntime(runtime_ptr: *anyopaque) ?*websocket_pool.Pool {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    return rt.ws_pool_ref;
}

/// Raw RFC 6455 frame write for server→client messages (unmasked).
/// Used by the `send` callback; symmetric with what the frame loop
/// reads. Payload size is capped at 64 KiB to match the inbound cap.
pub fn writeWebSocketFrame(fd: std.posix.fd_t, opcode: u4, payload: []const u8) !void {
    var header_buf: [10]u8 = undefined;
    header_buf[0] = 0x80 | @as(u8, opcode); // FIN = 1
    var header_len: usize = 2;
    if (payload.len < 126) {
        header_buf[1] = @as(u8, @intCast(payload.len));
    } else if (payload.len <= 0xFFFF) {
        header_buf[1] = 126;
        std.mem.writeInt(u16, header_buf[2..4], @as(u16, @intCast(payload.len)), .big);
        header_len = 4;
    } else {
        header_buf[1] = 127;
        std.mem.writeInt(u64, header_buf[2..10], payload.len, .big);
        header_len = 10;
    }

    // Gather-write the header and payload in a single writev syscall. Under the
    // thread-per-connection model two frame-loop threads can call this on the
    // same peer socket concurrently (broadcast); two separate write() calls
    // (header, then payload) gave a guaranteed interleave window between them.
    // One writev delivers a frame to the socket buffer atomically whenever it
    // fits (the common case under the 64 KiB cap). A short write on a frame
    // larger than the send buffer can still interleave; the fully-robust fix is
    // a per-connection send lock, deferred as it requires reworking the pool's
    // value-copied Connection storage.
    if (payload.len > 0) {
        var iovecs: [2]std.posix.iovec_const = .{
            .{ .base = header_buf[0..header_len].ptr, .len = header_len },
            .{ .base = payload.ptr, .len = payload.len },
        };
        try server_io.writevAllFd(fd, &iovecs);
    } else {
        try writeAllPosix(fd, header_buf[0..header_len]);
    }
}

pub fn writeAllPosix(fd: std.posix.fd_t, data: []const u8) !void {
    var remaining = data;
    while (remaining.len > 0) {
        const result = std.c.write(fd, remaining.ptr, remaining.len);
        if (result < 0) return error.WriteFailed;
        const n: usize = @intCast(result);
        if (n == 0) return error.WriteFailed;
        remaining = remaining[n..];
    }
}

pub fn wsSendCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 2) {
        return zq.modules.util.throwError(ctx, "TypeError", "send(ws, data) requires 2 arguments");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    // getStringDataCtx (not extractString) so a payload built by concatenation
    // (a concat rope once combined length >= 64, the common `"event: " + ...`
    // case) is flattened rather than rejected as "not a string".
    const bytes = getStringDataCtx(args[1], ctx) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "data must be a string (binary frames land in W2)");
    };
    // dup the fd under the pool lock rather than reading snapshot().fd and
    // writing outside the lock: the latter races the frame loop's
    // unregister-then-close and can write into a recycled fd (another peer).
    const fd = pool.dupFd(id) orelse {
        return zq.modules.util.throwError(ctx, "Error", "ws connection no longer registered");
    };
    defer _ = std.c.close(fd);
    writeWebSocketFrame(fd, 0x1, bytes) catch |err| {
        std.log.warn("ws send failed (id={d}): {}", .{ id, err });
        return zq.modules.util.throwError(ctx, "Error", "ws send failed");
    };
    return zq.JSValue.undefined_val;
}

pub fn wsCloseCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 1) {
        return zq.modules.util.throwError(ctx, "TypeError", "close(ws, code?, reason?) requires at least 1 argument");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    const code: u16 = blk: {
        if (args.len < 2) break :blk 1000;
        if (args[1].isInt()) {
            const raw = args[1].getInt();
            if (raw < 1000 or raw > 4999) {
                return zq.modules.util.throwError(ctx, "RangeError", "close code must be in [1000, 4999]");
            }
            break :blk @as(u16, @intCast(raw));
        }
        return zq.modules.util.throwError(ctx, "TypeError", "close code must be a number");
    };
    const reason: []const u8 = if (args.len >= 3)
        getStringDataCtx(args[2], ctx) orelse ""
    else
        "";

    // dup under the lock (see wsSendCallback) so the close frame and shutdown
    // never land on a recycled fd belonging to another connection.
    const fd = pool.dupFd(id) orelse {
        return zq.modules.util.throwError(ctx, "Error", "ws connection no longer registered");
    };
    defer _ = std.c.close(fd);

    // RFC 6455 §5.5.1: close payload is [status_u16_be][reason_utf8].
    // Reason length is capped at 123 so total payload fits in a 125-byte
    // short frame (126+ would force extended-length headers the peer
    // has to honour).
    var payload_buf: [125]u8 = undefined;
    std.mem.writeInt(u16, payload_buf[0..2], code, .big);
    const reason_len = @min(reason.len, 123);
    if (reason_len > 0) {
        @memcpy(payload_buf[2..][0..reason_len], reason[0..reason_len]);
    }
    writeWebSocketFrame(fd, 0x8, payload_buf[0 .. 2 + reason_len]) catch |err| {
        std.log.warn("ws close write failed (id={d}): {}", .{ id, err });
    };
    // Shutting down the write half (of the shared socket) lets the peer's read
    // return EOS promptly; the frame loop will then exit and clean up. Ignoring
    // the shutdown error here is intentional — the connection is already being
    // torn down. SHUT_WR == 1 on every supported platform.
    _ = std.c.shutdown(fd, 1);
    return zq.JSValue.undefined_val;
}

pub fn wsSerializeAttachmentCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 2) {
        return zq.modules.util.throwError(ctx, "TypeError", "serializeAttachment(ws, value) requires 2 arguments");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    // W2-a accepts raw strings only. Handlers with structured state
    // pre-serialize (JSON.stringify will arrive with the structured
    // clone work; for now a string payload is the contract). W2-b
    // extends this into a typed-array path for binary attachments.
    const bytes = getStringDataCtx(args[1], ctx) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "attachment must be a string (W2-a); binary lands in W2-b");
    };
    const ok = pool.setAttachment(id, bytes) catch |err| {
        std.log.warn("ws setAttachment failed (id={d}): {}", .{ id, err });
        return zq.modules.util.throwError(ctx, "Error", "attachment write failed");
    };
    if (!ok) {
        return zq.modules.util.throwError(ctx, "Error", "ws connection no longer registered");
    }
    return zq.JSValue.undefined_val;
}

pub fn wsDeserializeAttachmentCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 1) {
        return zq.modules.util.throwError(ctx, "TypeError", "deserializeAttachment(ws) requires 1 argument");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    // The pool hands us a freshly-duped slice; ctx.createString copies
    // into a JSString, so the interim allocation is short-lived and we
    // free it before returning.
    const bytes = pool.copyAttachment(id, ctx.allocator) catch |err| {
        std.log.warn("ws copyAttachment failed (id={d}): {}", .{ id, err });
        return zq.modules.util.throwError(ctx, "Error", "attachment read failed");
    } orelse return zq.JSValue.undefined_val;
    defer ctx.allocator.free(bytes);
    return try ctx.createString(bytes);
}

pub fn wsGetWebSocketsCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 1) {
        return zq.modules.util.throwError(ctx, "TypeError", "getWebSockets(roomKey) requires 1 argument");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const room_key = zq.modules.util.extractString(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "roomKey must be a string");
    };

    // Room membership snapshot: 256 peers is the W1 ceiling (see
    // max_room_peers in ws_frame_loop). Overflow returns the first N,
    // which W2 replaces with an iterator that doesn't cap.
    var ids_buf: [256]websocket_pool.ConnectionId = undefined;
    const ids = pool.collectRoom(room_key, &ids_buf);

    const arr = try ctx.createArray();
    arr.prototype = ctx.array_prototype;
    for (ids, 0..) |id, i| {
        const id_i32: i32 = std.math.cast(i32, id) orelse continue;
        try ctx.setIndexChecked(arr, @as(u32, @intCast(i)), zq.JSValue.fromInt(id_i32));
    }
    arr.setArrayLength(@as(u32, @intCast(ids.len)));
    return arr.toValue();
}

pub fn wsRoomFromPathCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    _ = runtime_ptr;
    _ = args;
    return zq.modules.util.throwError(ctx, "Error", "roomFromPath lands in W2");
}

pub fn wsSetAutoResponseCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 3) {
        return zq.modules.util.throwError(ctx, "TypeError", "setAutoResponse(ws, request, response) requires 3 arguments");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zigttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    const request_bytes = zq.modules.util.extractString(args[1]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "setAutoResponse request must be a string");
    };
    const response_bytes = zq.modules.util.extractString(args[2]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "setAutoResponse response must be a string");
    };
    const ok = pool.setAutoResponse(id, request_bytes, response_bytes) catch |err| {
        std.log.warn("ws setAutoResponse failed (id={d}): {}", .{ id, err });
        return zq.modules.util.throwError(ctx, "Error", "auto-response registration failed");
    };
    if (!ok) {
        return zq.modules.util.throwError(ctx, "Error", "ws connection no longer registered");
    }
    return zq.JSValue.undefined_val;
}
