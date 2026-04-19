//! zigttp:websocket - bidirectional connections with hibernation.
//!
//! Seven exports. All dispatch through runtime-owned callbacks installed
//! by the zigts-side shim. Until installed, each call throws the same
//! "runtime not installed" error.

const std = @import("std");
const sdk = @import("zigttp-sdk");

pub const MODULE_STATE_SLOT: usize = 12; // module_slots.Slot.websocket

pub const WsCallFn = *const fn (
    runtime_ptr: *anyopaque,
    handle: *sdk.ModuleHandle,
    args: []const sdk.JSValue,
) anyerror!sdk.JSValue;

pub const WebSocketCallbacks = struct {
    runtime_ptr: *anyopaque,
    send_fn: WsCallFn,
    close_fn: WsCallFn,
    serialize_attachment_fn: WsCallFn,
    deserialize_attachment_fn: WsCallFn,
    get_web_sockets_fn: WsCallFn,
    room_from_path_fn: WsCallFn,
    set_auto_response_fn: WsCallFn,
};

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:websocket",
    .name = "websocket",
    .required_capabilities = &.{ .clock, .runtime_callback, .filesystem, .policy_check, .websocket },
    .stateful = true,
    .exports = &.{
        .{ .name = "send", .module_func = sendImpl, .arg_count = 2, .effect = .write, .returns = .undefined, .param_types = &.{ .object, .string } },
        .{ .name = "close", .module_func = closeImpl, .arg_count = 3, .effect = .write, .returns = .undefined, .param_types = &.{ .object, .number, .string } },
        .{ .name = "serializeAttachment", .module_func = serializeAttachmentImpl, .arg_count = 2, .effect = .write, .returns = .undefined, .param_types = &.{ .object, .object } },
        .{ .name = "deserializeAttachment", .module_func = deserializeAttachmentImpl, .arg_count = 1, .effect = .read, .returns = .optional_object, .param_types = &.{.object} },
        .{ .name = "getWebSockets", .module_func = getWebSocketsImpl, .arg_count = 1, .effect = .read, .returns = .object, .param_types = &.{.string} },
        .{ .name = "roomFromPath", .module_func = roomFromPathImpl, .arg_count = 2, .effect = .read, .returns = .string, .param_types = &.{ .object, .string } },
        .{ .name = "setAutoResponse", .module_func = setAutoResponseImpl, .arg_count = 2, .effect = .write, .returns = .undefined, .param_types = &.{ .string, .string } },
    },
};

fn dispatch(handle: *sdk.ModuleHandle, args: []const sdk.JSValue, comptime field: []const u8) anyerror!sdk.JSValue {
    const state = sdk.getModuleState(handle, WebSocketCallbacks, MODULE_STATE_SLOT) orelse {
        return sdk.throwError(handle, "Error", "zigttp:websocket runtime not installed");
    };
    try sdk.requireCapability(handle, .runtime_callback);
    return @field(state, field)(state.runtime_ptr, handle, args);
}

fn sendImpl(h: *sdk.ModuleHandle, _: sdk.JSValue, a: []const sdk.JSValue) anyerror!sdk.JSValue {
    return dispatch(h, a, "send_fn");
}
fn closeImpl(h: *sdk.ModuleHandle, _: sdk.JSValue, a: []const sdk.JSValue) anyerror!sdk.JSValue {
    return dispatch(h, a, "close_fn");
}
fn serializeAttachmentImpl(h: *sdk.ModuleHandle, _: sdk.JSValue, a: []const sdk.JSValue) anyerror!sdk.JSValue {
    return dispatch(h, a, "serialize_attachment_fn");
}
fn deserializeAttachmentImpl(h: *sdk.ModuleHandle, _: sdk.JSValue, a: []const sdk.JSValue) anyerror!sdk.JSValue {
    return dispatch(h, a, "deserialize_attachment_fn");
}
fn getWebSocketsImpl(h: *sdk.ModuleHandle, _: sdk.JSValue, a: []const sdk.JSValue) anyerror!sdk.JSValue {
    return dispatch(h, a, "get_web_sockets_fn");
}
fn roomFromPathImpl(h: *sdk.ModuleHandle, _: sdk.JSValue, a: []const sdk.JSValue) anyerror!sdk.JSValue {
    return dispatch(h, a, "room_from_path_fn");
}
fn setAutoResponseImpl(h: *sdk.ModuleHandle, _: sdk.JSValue, a: []const sdk.JSValue) anyerror!sdk.JSValue {
    return dispatch(h, a, "set_auto_response_fn");
}
