//! zigttp:websocket - bidirectional connections with hibernation.
//!
//! Exports:
//!   send(ws, data)                              -> undefined
//!   close(ws, code?, reason?)                   -> undefined
//!   serializeAttachment(ws, value)              -> undefined
//!   deserializeAttachment(ws)                   -> value | null
//!   getWebSockets(roomKey)                      -> ws[]
//!   roomFromPath(ws, paramName)                 -> string
//!   setAutoResponse(requestFrame, responseFrame) -> undefined
//!
//! Handler shape: three top-level exports `onOpen(ws, req)`,
//! `onMessage(ws, data)`, `onClose(ws, code, reason)` are dispatched by the
//! runtime. Hibernation is invisible to the handler — each call receives a
//! fresh runtime context reconstructed from the persisted attachment.
//!
//! W1-a scope: module binding shell. All exports throw
//! "runtime not installed" until the ws_gateway + websocket_pool land
//! in W1-c/W1-d. This commit makes `import ... from "zigttp:websocket"`
//! resolve and flow through the contract builder cleanly, so subsequent
//! slices can grow the runtime layer without moving the surface.

const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const util = @import("../internal/util.zig");
const mb = @import("../../module_binding.zig");

pub const MODULE_STATE_SLOT = @intFromEnum(@import("../../module_slots.zig").Slot.websocket);

/// Runtime-installed dispatch table. Set at `installState` time once the
/// ws_gateway owns the connection pool. W1-a leaves this unset, so every
/// export's native fn throws a clear "not yet implemented" error.
pub const WebSocketCallbacks = struct {
    runtime_ptr: *anyopaque,
    send_fn: *const fn (*anyopaque, *context.Context, []const value.JSValue) anyerror!value.JSValue,
    close_fn: *const fn (*anyopaque, *context.Context, []const value.JSValue) anyerror!value.JSValue,
    serialize_attachment_fn: *const fn (*anyopaque, *context.Context, []const value.JSValue) anyerror!value.JSValue,
    deserialize_attachment_fn: *const fn (*anyopaque, *context.Context, []const value.JSValue) anyerror!value.JSValue,
    get_web_sockets_fn: *const fn (*anyopaque, *context.Context, []const value.JSValue) anyerror!value.JSValue,
    room_from_path_fn: *const fn (*anyopaque, *context.Context, []const value.JSValue) anyerror!value.JSValue,
    set_auto_response_fn: *const fn (*anyopaque, *context.Context, []const value.JSValue) anyerror!value.JSValue,

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *WebSocketCallbacks = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }
};

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:websocket",
    .name = "websocket",
    .required_capabilities = &.{ .clock, .runtime_callback, .filesystem, .policy_check, .websocket },
    .stateful = true,
    .exports = &.{
        .{
            .name = "send",
            .func = sendNative,
            .arg_count = 2,
            .effect = .write,
            .returns = .undefined,
            .param_types = &.{ .object, .string },
        },
        .{
            .name = "close",
            .func = closeNative,
            .arg_count = 3,
            .effect = .write,
            .returns = .undefined,
            .param_types = &.{ .object, .number, .string },
        },
        .{
            .name = "serializeAttachment",
            .func = serializeAttachmentNative,
            .arg_count = 2,
            .effect = .write,
            .returns = .undefined,
            .param_types = &.{ .object, .object },
        },
        .{
            .name = "deserializeAttachment",
            .func = deserializeAttachmentNative,
            .arg_count = 1,
            .effect = .read,
            .returns = .optional_object,
            .param_types = &.{.object},
        },
        .{
            .name = "getWebSockets",
            .func = getWebSocketsNative,
            .arg_count = 1,
            .effect = .read,
            .returns = .object,
            .param_types = &.{.string},
        },
        .{
            .name = "roomFromPath",
            .func = roomFromPathNative,
            .arg_count = 2,
            .effect = .read,
            .returns = .string,
            .param_types = &.{ .object, .string },
        },
        .{
            .name = "setAutoResponse",
            .func = setAutoResponseNative,
            .arg_count = 2,
            .effect = .write,
            .returns = .undefined,
            .param_types = &.{ .string, .string },
        },
    },
};

pub const exports = binding.toModuleExports();

/// Install the callback table. W1-a does not call this anywhere — exports
/// will throw "not installed" until the gateway starts wiring W1-c.
pub fn installState(
    ctx: *context.Context,
    callbacks: WebSocketCallbacks,
) !void {
    if (ctx.getModuleState(WebSocketCallbacks, MODULE_STATE_SLOT)) |existing| {
        existing.* = callbacks;
        return;
    }
    const owned = try ctx.allocator.create(WebSocketCallbacks);
    errdefer ctx.allocator.destroy(owned);
    owned.* = callbacks;
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(owned), &WebSocketCallbacks.deinitOpaque);
}

fn notInstalled(ctx: *context.Context) anyerror!value.JSValue {
    return util.throwError(ctx, "Error", "zigttp:websocket runtime not installed (W1-a shell; ws_gateway lands in W1-c)");
}

fn dispatch(
    ctx_ptr: *anyopaque,
    args: []const value.JSValue,
    comptime field: []const u8,
) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const state = ctx.getModuleState(WebSocketCallbacks, MODULE_STATE_SLOT) orelse {
        return notInstalled(ctx);
    };
    mb.runtimeCallbackCapabilityChecked();
    const call_fn = @field(state, field);
    return call_fn(state.runtime_ptr, ctx, args);
}

fn sendNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return dispatch(ctx_ptr, args, "send_fn");
}

fn closeNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return dispatch(ctx_ptr, args, "close_fn");
}

fn serializeAttachmentNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return dispatch(ctx_ptr, args, "serialize_attachment_fn");
}

fn deserializeAttachmentNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return dispatch(ctx_ptr, args, "deserialize_attachment_fn");
}

fn getWebSocketsNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return dispatch(ctx_ptr, args, "get_web_sockets_fn");
}

fn roomFromPathNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return dispatch(ctx_ptr, args, "room_from_path_fn");
}

fn setAutoResponseNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return dispatch(ctx_ptr, args, "set_auto_response_fn");
}

test "websocket binding declares seven exports with websocket capability" {
    try std.testing.expectEqualStrings("zigttp:websocket", binding.specifier);
    try std.testing.expectEqual(@as(usize, 7), binding.exports.len);

    const expected_names = [_][]const u8{
        "send",
        "close",
        "serializeAttachment",
        "deserializeAttachment",
        "getWebSockets",
        "roomFromPath",
        "setAutoResponse",
    };
    for (expected_names, binding.exports) |want, got| {
        try std.testing.expectEqualStrings(want, got.name);
    }

    var saw_websocket_cap = false;
    for (binding.required_capabilities) |cap| {
        if (cap == .websocket) saw_websocket_cap = true;
    }
    try std.testing.expect(saw_websocket_cap);
}

test "websocket binding requires filesystem for attachment persistence" {
    var saw_filesystem = false;
    for (binding.required_capabilities) |cap| {
        if (cap == .filesystem) saw_filesystem = true;
    }
    try std.testing.expect(saw_filesystem);
}
