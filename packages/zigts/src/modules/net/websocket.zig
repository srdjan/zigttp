const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const adapter = @import("../../module_binding_adapter.zig");
const sdk = @import("zigttp-sdk");
const modules = @import("zigttp-modules");
const ws_module = modules.net.websocket;

pub const binding = adapter.adaptModuleBinding(ws_module.binding);
pub const exports = binding.toModuleExports();

pub const MODULE_STATE_SLOT: usize = ws_module.MODULE_STATE_SLOT;

pub const WsCallFn = *const fn (
    runtime_ptr: *anyopaque,
    ctx: *context.Context,
    args: []const value.JSValue,
) anyerror!value.JSValue;

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

const InstalledState = struct {
    callbacks: WebSocketCallbacks,
    base: ws_module.WebSocketCallbacks,

    fn sdkDispatch(comptime field: []const u8) ws_module.WsCallFn {
        return struct {
            fn call(
                installed_ptr: *anyopaque,
                handle: *sdk.ModuleHandle,
                args: []const sdk.JSValue,
            ) anyerror!sdk.JSValue {
                const self: *InstalledState = @ptrCast(@alignCast(installed_ptr));
                const call_fn = @field(self.callbacks, field);
                const result = try call_fn(self.callbacks.runtime_ptr, adapter.contextFromHandle(handle), adapter.internalArgs(args));
                return adapter.sdkValue(result);
            }
        }.call;
    }
};

pub fn installState(ctx: *context.Context, callbacks: WebSocketCallbacks) !void {
    if (ctx.getModuleState(ws_module.WebSocketCallbacks, MODULE_STATE_SLOT)) |existing| {
        const installed: *InstalledState = @fieldParentPtr("base", existing);
        installed.callbacks = callbacks;
        return;
    }

    const installed = try ctx.allocator.create(InstalledState);
    installed.* = .{
        .callbacks = callbacks,
        .base = .{
            .runtime_ptr = @ptrCast(installed),
            .send_fn = InstalledState.sdkDispatch("send_fn"),
            .close_fn = InstalledState.sdkDispatch("close_fn"),
            .serialize_attachment_fn = InstalledState.sdkDispatch("serialize_attachment_fn"),
            .deserialize_attachment_fn = InstalledState.sdkDispatch("deserialize_attachment_fn"),
            .get_web_sockets_fn = InstalledState.sdkDispatch("get_web_sockets_fn"),
            .room_from_path_fn = InstalledState.sdkDispatch("room_from_path_fn"),
            .set_auto_response_fn = InstalledState.sdkDispatch("set_auto_response_fn"),
        },
    };
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(&installed.base), &stateDeinitAdapter);
}

fn stateDeinitAdapter(ptr: *anyopaque, allocator: std.mem.Allocator) void {
    const base: *ws_module.WebSocketCallbacks = @ptrCast(@alignCast(ptr));
    const installed: *InstalledState = @fieldParentPtr("base", base);
    allocator.destroy(installed);
}
