//! WebSocket runtime callbacks: the native implementations behind the
//! `zttp:websocket` module exports (send / close / serializeAttachment /
//! deserializeAttachment / getWebSockets / setAutoResponse).
//!
//! Extracted from zruntime.zig to keep the request-lifecycle struct focused.
//! These are registered by `Runtime.installWebSocketModuleState`; each callback
//! receives the owning Runtime as an opaque pointer and dispatches against its
//! WebSocket pool (`Runtime.ws_pool_ref`). The `active_ws_connection`
//! thread-local stays in zruntime.zig because the frame loop owns it and these
//! callbacks take the connection id from their first JS argument instead.

const std = @import("std");
const zq = @import("zts");
const websocket_codec = @import("websocket_codec.zig");
const websocket_pool = @import("websocket_pool.zig");
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

pub fn wsSendCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    if (args.len < 2) {
        return zq.modules.util.throwError(ctx, "TypeError", "send(ws, data) requires 2 arguments");
    }
    const pool = wsPoolFromRuntime(runtime_ptr) orelse {
        return zq.modules.util.throwError(ctx, "Error", "zttp:websocket not bound to an active server");
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
    pool.sendFrame(id, .text, bytes) catch |err| {
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
        return zq.modules.util.throwError(ctx, "Error", "zttp:websocket not bound to an active server");
    };
    const id = wsConnectionIdFromArg(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "ws argument must be a connection id");
    };
    const code: u16 = blk: {
        if (args.len < 2) break :blk 1000;
        if (args[1].isInt()) {
            const raw = args[1].getInt();
            const parsed: u16 = std.math.cast(u16, raw) orelse
                return zq.modules.util.throwError(ctx, "RangeError", "close code is not valid on the wire");
            if (!websocket_codec.validCloseCode(parsed))
                return zq.modules.util.throwError(ctx, "RangeError", "close code is reserved or not valid on the wire");
            break :blk parsed;
        }
        return zq.modules.util.throwError(ctx, "TypeError", "close code must be a number");
    };
    const raw_reason: []const u8 = if (args.len >= 3)
        getStringDataCtx(args[2], ctx) orelse
            return zq.modules.util.throwError(ctx, "TypeError", "close reason must be a string")
    else
        "";
    const reason = websocket_codec.truncateCloseReason(raw_reason) catch
        return zq.modules.util.throwError(ctx, "TypeError", "close reason must be valid UTF-8");

    // `truncateCloseReason` already capped the reason at 123 bytes so the
    // packed payload fits a 125-byte short frame.
    var payload_buf: [125]u8 = undefined;
    const payload = websocket_codec.writeClosePayload(&payload_buf, code, reason);
    pool.sendFrameAndShutdown(id, .connection_close, payload) catch |err| {
        std.log.warn("ws close write failed (id={d}): {}", .{ id, err });
    };
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
        return zq.modules.util.throwError(ctx, "Error", "zttp:websocket not bound to an active server");
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
        return zq.modules.util.throwError(ctx, "Error", "zttp:websocket not bound to an active server");
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
        return zq.modules.util.throwError(ctx, "Error", "zttp:websocket not bound to an active server");
    };
    const room_key = zq.modules.util.extractString(args[0]) orelse {
        return zq.modules.util.throwError(ctx, "TypeError", "roomKey must be a string");
    };

    const ids = pool.snapshotRoom(room_key, ctx.allocator) catch |err| {
        std.log.warn("ws room snapshot failed: {}", .{err});
        return zq.modules.util.throwError(ctx, "Error", "room snapshot failed");
    };
    defer ctx.allocator.free(ids);

    const arr = try ctx.createArray();
    arr.prototype = ctx.array_prototype;
    for (ids, 0..) |id, i| {
        const id_i32: i32 = std.math.cast(i32, id) orelse continue;
        try ctx.setIndexChecked(arr, @as(u32, @intCast(i)), zq.JSValue.fromInt(id_i32));
    }
    arr.setArrayLength(@as(u32, @intCast(ids.len)));
    return arr.toValue();
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
        return zq.modules.util.throwError(ctx, "Error", "zttp:websocket not bound to an active server");
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

test "setAutoResponse callback registers a three-argument codec reply" {
    const allocator = std.testing.allocator;
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    var pool = websocket_pool.Pool.init(allocator);
    defer pool.deinit();
    const id = try pool.register(100, "/test", 0);
    rt.ws_pool_ref = &pool;

    const args = [_]zq.JSValue{
        zq.JSValue.fromInt(@intCast(id)),
        try rt.ctx.createString("ping"),
        try rt.ctx.createString("pong"),
    };
    _ = try wsSetAutoResponseCallback(rt, rt.ctx, &args);
    const response = (try pool.tryAutoResponse(id, "ping", allocator)) orelse
        return error.MissingAutoResponse;
    defer allocator.free(response);
    try std.testing.expectEqualStrings("pong", response);
}

test "close callback writes one valid boundary-truncated close frame and shuts down" {
    const allocator = std.testing.allocator;
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    const fds = try @import("server_io.zig").createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);
    defer std.Io.Threaded.closeFd(fds[1]);

    var pool = websocket_pool.Pool.init(allocator);
    defer pool.deinit();
    const id = try pool.register(fds[0], "/close", 0);
    rt.ws_pool_ref = &pool;

    var reason: [124]u8 = undefined;
    @memset(reason[0..122], 'a');
    reason[122] = 0xc3;
    reason[123] = 0xa9;
    const args = [_]zq.JSValue{
        zq.JSValue.fromInt(@intCast(id)),
        zq.JSValue.fromInt(4001),
        try rt.ctx.createString(&reason),
    };
    const result = try wsCloseCallback(rt, rt.ctx, &args);
    try std.testing.expect(result.isUndefined());

    var frame: [126]u8 = undefined;
    try websocket_pool.readExactFd(fds[1], &frame);
    try std.testing.expectEqual(@as(u8, 0x88), frame[0]);
    try std.testing.expectEqual(@as(u8, 124), frame[1]);
    try std.testing.expectEqual(@as(u8, 0x0f), frame[2]);
    try std.testing.expectEqual(@as(u8, 0xa1), frame[3]);
    try std.testing.expectEqualSlices(u8, reason[0..122], frame[4..]);

    var byte: [1]u8 = undefined;
    try std.testing.expectEqual(@as(isize, 0), std.c.read(fds[1], &byte, 1));
}

test "close callback rejects reserved codes and non-string reasons without writing" {
    const allocator = std.testing.allocator;
    const rt = try Runtime.init(allocator, .{});
    defer rt.deinit();
    const fds = try @import("server_io.zig").createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);
    defer std.Io.Threaded.closeFd(fds[1]);

    var pool = websocket_pool.Pool.init(allocator);
    defer pool.deinit();
    const id = try pool.register(fds[0], "/close-invalid", 0);
    rt.ws_pool_ref = &pool;

    const reserved_args = [_]zq.JSValue{
        zq.JSValue.fromInt(@intCast(id)),
        zq.JSValue.fromInt(1005),
    };
    const reserved_result = try wsCloseCallback(rt, rt.ctx, &reserved_args);
    try std.testing.expect(reserved_result.isException());
    try std.testing.expect(rt.ctx.hasException());
    rt.ctx.clearException();

    const non_string_args = [_]zq.JSValue{
        zq.JSValue.fromInt(@intCast(id)),
        zq.JSValue.fromInt(1000),
        zq.JSValue.fromInt(42),
    };
    const non_string_result = try wsCloseCallback(rt, rt.ctx, &non_string_args);
    try std.testing.expect(non_string_result.isException());
    try std.testing.expect(rt.ctx.hasException());

    var poll_fds = [_]std.posix.pollfd{.{
        .fd = fds[1],
        .events = std.posix.POLL.IN,
        .revents = 0,
    }};
    try std.testing.expectEqual(@as(usize, 0), try std.posix.poll(&poll_fds, 0));
}
