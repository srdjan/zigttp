//! zigttp:service - named internal service calls.
//!
//! State population (parsing system.json) happens in the zigts-side
//! installState shim; this module owns the per-call dispatch only.

const std = @import("std");
const sdk = @import("zigttp-sdk");
const util = @import("../internal/util.zig");

pub const MODULE_STATE_SLOT: usize = 9; // module_slots.Slot.service

pub const ServiceCallFn = *const fn (
    runtime_ptr: *anyopaque,
    handle: *sdk.ModuleHandle,
    base_url: []const u8,
    route_pattern: []const u8,
    init: sdk.JSValue,
) anyerror!sdk.JSValue;

pub const ServiceState = struct {
    allocator: std.mem.Allocator,
    services: std.StringHashMap([]const u8),
    runtime_ptr: *anyopaque,
    call_fn: ServiceCallFn,

    pub fn init(
        allocator: std.mem.Allocator,
        runtime_ptr: *anyopaque,
        call_fn: ServiceCallFn,
    ) ServiceState {
        return .{
            .allocator = allocator,
            .services = std.StringHashMap([]const u8).init(allocator),
            .runtime_ptr = runtime_ptr,
            .call_fn = call_fn,
        };
    }

    pub fn register(self: *ServiceState, name: []const u8, base_url: []const u8) !void {
        if (self.services.contains(name)) return error.DuplicateServiceName;
        try self.services.put(
            try self.allocator.dupe(u8, name),
            try self.allocator.dupe(u8, base_url),
        );
    }

    pub fn deinitSelf(self: *ServiceState) void {
        var it = self.services.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.services.deinit();
    }

    pub fn sdkDeinit(ptr: *anyopaque) callconv(.c) void {
        const self: *ServiceState = @ptrCast(@alignCast(ptr));
        const allocator = self.allocator;
        self.deinitSelf();
        allocator.destroy(self);
    }
};

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:service",
    .name = "service",
    .required_capabilities = &.{ .filesystem, .runtime_callback },
    .stateful = true,
    .exports = &.{
        .{
            .name = "serviceCall",
            .module_func = serviceCallImpl,
            .arg_count = 3,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .string, .object },
            .return_labels = .{ .external = true },
            .contract_extractions = &.{.{ .category = .service_call }},
        },
    },
};

fn serviceCallImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    const state = sdk.getModuleState(handle, ServiceState, MODULE_STATE_SLOT) orelse {
        return sdk.throwError(handle, "Error", "serviceCall() requires --system <FILE>");
    };

    if (args.len < 2) return util.throwTypeError(handle, "serviceCall() expects a service name and route pattern");

    const service_name = sdk.extractString(args[0]) orelse return util.throwTypeError(handle, "serviceCall() service name must be a string");
    const route_pattern = sdk.extractString(args[1]) orelse return util.throwTypeError(handle, "serviceCall() route pattern must be a string");
    const init_val = if (args.len > 2) args[2] else sdk.JSValue.undefined_val;
    if (!init_val.isUndefined() and !init_val.isNull() and !sdk.isObject(init_val)) {
        return util.throwTypeError(handle, "serviceCall() init must be an object");
    }

    const base_url = state.services.get(service_name) orelse {
        return sdk.throwError(handle, "Error", "serviceCall() references an unknown service");
    };

    try sdk.requireCapability(handle, .runtime_callback);
    return state.call_fn(state.runtime_ptr, handle, base_url, route_pattern, init_val);
}
