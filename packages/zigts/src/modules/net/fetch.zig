const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const adapter = @import("../../module_binding_adapter.zig");
const sdk = @import("zigttp-sdk");
const modules = @import("zigttp-modules");
const fetch_module = modules.net.fetch;

pub const binding = adapter.adaptModuleBinding(fetch_module.binding);
pub const exports = binding.toModuleExports();

pub const MODULE_STATE_SLOT: usize = fetch_module.MODULE_STATE_SLOT;

pub const FetchCallFn = *const fn (
    runtime_ptr: *anyopaque,
    ctx: *context.Context,
    args: []const value.JSValue,
) anyerror!value.JSValue;

const InstalledState = struct {
    runtime_ptr: *anyopaque,
    call_fn: FetchCallFn,
    base: fetch_module.FetchState,

    fn sdkCall(
        installed_ptr: *anyopaque,
        handle: *sdk.ModuleHandle,
        args: []const sdk.JSValue,
    ) anyerror!sdk.JSValue {
        const self: *InstalledState = @ptrCast(@alignCast(installed_ptr));
        const result = try self.call_fn(self.runtime_ptr, adapter.contextFromHandle(handle), adapter.internalArgs(args));
        return adapter.sdkValue(result);
    }
};

pub fn installState(
    ctx: *context.Context,
    runtime_ptr: *anyopaque,
    call_fn: FetchCallFn,
) !void {
    if (ctx.getModuleState(fetch_module.FetchState, MODULE_STATE_SLOT)) |state| {
        const installed: *InstalledState = @fieldParentPtr("base", state);
        installed.runtime_ptr = runtime_ptr;
        installed.call_fn = call_fn;
        return;
    }

    const installed = try ctx.allocator.create(InstalledState);
    installed.* = .{
        .runtime_ptr = runtime_ptr,
        .call_fn = call_fn,
        .base = .{
            .runtime_ptr = @ptrCast(installed),
            .call_fn = InstalledState.sdkCall,
            .allocator = ctx.allocator,
        },
    };
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(&installed.base), &stateDeinitAdapter);
}

fn stateDeinitAdapter(ptr: *anyopaque, _: std.mem.Allocator) void {
    const base: *fetch_module.FetchState = @ptrCast(@alignCast(ptr));
    const installed: *InstalledState = @fieldParentPtr("base", base);
    installed.base.allocator.destroy(installed);
}
