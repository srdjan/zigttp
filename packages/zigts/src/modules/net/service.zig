const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const mb = @import("../../module_binding.zig");
const adapter = @import("../../module_binding_adapter.zig");
const sdk = @import("zigttp-sdk");
const modules = @import("zigttp-modules");
const system_linker = @import("../../system_linker.zig");
const service_module = modules.net.service;

pub const binding = adapter.adaptModuleBinding(service_module.binding);
pub const exports = binding.toModuleExports();

pub const MODULE_STATE_SLOT: usize = service_module.MODULE_STATE_SLOT;

pub const ServiceCallFn = *const fn (
    runtime_ptr: *anyopaque,
    ctx: *context.Context,
    base_url: []const u8,
    route_pattern: []const u8,
    init: value.JSValue,
) anyerror!value.JSValue;

const InstalledState = struct {
    runtime_ptr: *anyopaque,
    call_fn: ServiceCallFn,
    base: service_module.ServiceState,

    fn sdkCall(
        installed_ptr: *anyopaque,
        handle: *sdk.ModuleHandle,
        base_url: []const u8,
        route_pattern: []const u8,
        init: sdk.JSValue,
    ) anyerror!sdk.JSValue {
        const self: *InstalledState = @ptrCast(@alignCast(installed_ptr));
        const result = try self.call_fn(
            self.runtime_ptr,
            adapter.contextFromHandle(handle),
            base_url,
            route_pattern,
            adapter.internalValue(init),
        );
        return adapter.sdkValue(result);
    }
};

pub fn installState(
    ctx: *context.Context,
    system_path: []const u8,
    runtime_ptr: *anyopaque,
    call_fn: ServiceCallFn,
) !void {
    const token = mb.pushActiveModuleContext(service_module.binding.specifier, binding.required_capabilities);
    defer mb.popActiveModuleContext(token);

    if (ctx.getModuleState(service_module.ServiceState, MODULE_STATE_SLOT)) |existing| {
        const installed: *InstalledState = @fieldParentPtr("base", existing);
        installed.base.deinitSelf();
        installed.runtime_ptr = runtime_ptr;
        installed.call_fn = call_fn;
        installed.base = service_module.ServiceState.init(ctx.allocator, @ptrCast(installed), InstalledState.sdkCall);
        try populateServices(&installed.base, ctx.allocator, system_path);
        return;
    }

    const installed = try ctx.allocator.create(InstalledState);
    errdefer ctx.allocator.destroy(installed);
    installed.* = .{
        .runtime_ptr = runtime_ptr,
        .call_fn = call_fn,
        .base = service_module.ServiceState.init(ctx.allocator, @ptrCast(installed), InstalledState.sdkCall),
    };
    try populateServices(&installed.base, ctx.allocator, system_path);
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(&installed.base), &stateDeinitAdapter);
}

fn populateServices(
    state: *service_module.ServiceState,
    allocator: std.mem.Allocator,
    system_path: []const u8,
) !void {
    const system_json = try mb.readFileChecked(allocator, system_path, 1024 * 1024);
    defer allocator.free(system_json);

    var config = try system_linker.parseSystemConfig(allocator, system_json);
    defer config.deinit(allocator);

    for (config.handlers) |entry| {
        try state.register(entry.name, entry.base_url);
    }
}

fn stateDeinitAdapter(ptr: *anyopaque, _: std.mem.Allocator) void {
    const base: *service_module.ServiceState = @ptrCast(@alignCast(ptr));
    const installed: *InstalledState = @fieldParentPtr("base", base);
    const allocator = base.allocator;
    base.deinitSelf();
    allocator.destroy(installed);
}
