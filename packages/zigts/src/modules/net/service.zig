//! zigttp:service - named internal service calls backed by system.json
//!
//! Exports:
//!   serviceCall(service: string, route: string, init?: object) -> Response
//!
//! The runtime owns the actual HTTP execution path. This module validates
//! arguments, resolves the named service to a base URL, and delegates to the
//! runtime callback installed by src/zruntime.zig.

const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const system_linker = @import("../../system_linker.zig");
const util = @import("../util.zig");
const mb = @import("../../module_binding.zig");

pub const MODULE_STATE_SLOT = @intFromEnum(@import("../../module_slots.zig").Slot.service);

pub const ServiceCallFn = *const fn (
    *anyopaque,
    *context.Context,
    []const u8,
    []const u8,
    value.JSValue,
) anyerror!value.JSValue;

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:service",
    .name = "service",
    .required_capabilities = &.{ .filesystem, .runtime_callback },
    .stateful = true,
    .exports = &.{
        .{ .name = "serviceCall", .func = serviceCallNative, .arg_count = 3, .effect = .write, .returns = .object, .param_types = &.{ .string, .string, .object }, .return_labels = .{ .external = true }, .contract_extractions = &.{.{ .category = .service_call }} },
    },
};

pub const exports = binding.toModuleExports();

pub const ServiceState = struct {
    allocator: std.mem.Allocator,
    services: std.StringHashMap([]const u8),
    runtime_ptr: *anyopaque,
    call_fn: ServiceCallFn,

    pub fn init(
        allocator: std.mem.Allocator,
        system_path: []const u8,
        runtime_ptr: *anyopaque,
        call_fn: ServiceCallFn,
    ) !ServiceState {
        const token = mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
        defer mb.popActiveModuleContext(token);

        var self = ServiceState{
            .allocator = allocator,
            .services = std.StringHashMap([]const u8).init(allocator),
            .runtime_ptr = runtime_ptr,
            .call_fn = call_fn,
        };
        errdefer self.deinitSelf();

        const system_json = try mb.readFileChecked(allocator, system_path, 1024 * 1024);
        defer allocator.free(system_json);

        var config = try system_linker.parseSystemConfig(allocator, system_json);
        defer config.deinit(allocator);

        for (config.handlers) |entry| {
            if (self.services.contains(entry.name)) {
                return error.DuplicateServiceName;
            }
            try self.services.put(
                try allocator.dupe(u8, entry.name),
                try allocator.dupe(u8, entry.base_url),
            );
        }

        return self;
    }

    fn deinitSelf(self: *ServiceState) void {
        var it = self.services.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.services.deinit();
    }

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *ServiceState = @ptrCast(@alignCast(ptr));
        self.deinitSelf();
        allocator.destroy(self);
    }
};

pub fn installState(
    ctx: *context.Context,
    system_path: []const u8,
    runtime_ptr: *anyopaque,
    call_fn: ServiceCallFn,
) !void {
    if (ctx.getModuleState(ServiceState, MODULE_STATE_SLOT)) |state| {
        const new_state = try ServiceState.init(ctx.allocator, system_path, runtime_ptr, call_fn);
        state.deinitSelf();
        state.* = new_state;
        return;
    }

    const state = try ctx.allocator.create(ServiceState);
    errdefer ctx.allocator.destroy(state);
    state.* = try ServiceState.init(ctx.allocator, system_path, runtime_ptr, call_fn);
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(state), &ServiceState.deinitOpaque);
}

fn serviceCallNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const state = ctx.getModuleState(ServiceState, MODULE_STATE_SLOT) orelse {
        return util.throwError(ctx, "Error", "serviceCall() requires --system <FILE>");
    };

    if (args.len < 2) {
        return util.throwError(ctx, "TypeError", "serviceCall() expects a service name and route pattern");
    }

    const service_name = util.extractString(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "serviceCall() service name must be a string");
    };
    const route_pattern = util.extractString(args[1]) orelse {
        return util.throwError(ctx, "TypeError", "serviceCall() route pattern must be a string");
    };
    const init_val = if (args.len > 2) args[2] else value.JSValue.undefined_val;
    if (!init_val.isUndefined() and !init_val.isNull() and !init_val.isObject()) {
        return util.throwError(ctx, "TypeError", "serviceCall() init must be an object");
    }

    const base_url = state.services.get(service_name) orelse {
        return util.throwError(ctx, "Error", "serviceCall() references an unknown service");
    };

    mb.runtimeCallbackCapabilityChecked();
    return state.call_fn(state.runtime_ptr, ctx, base_url, route_pattern, init_val);
}

fn pushServiceTestContext() mb.ActiveModuleToken {
    return mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
}

test "service installState loads config and populates service map" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "system.json",
        .data =
        \\{
        \\  "version": 1,
        \\  "handlers": [
        \\    {
        \\      "name": "users",
        \\      "path": "users.ts",
        \\      "baseUrl": "http://users.internal"
        \\    }
        \\  ]
        \\}
        ,
    });
    const system_path = try tmp.dir.realPathFileAlloc(std.testing.io, "system.json", std.testing.allocator);
    defer std.testing.allocator.free(system_path);

    var gc_state = try @import("../../gc.zig").GC.init(std.testing.allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();
    var heap_state = @import("../../heap.zig").Heap.init(std.testing.allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    const ctx = try context.Context.init(std.testing.allocator, &gc_state, .{});
    defer ctx.deinit();

    const token = pushServiceTestContext();
    defer mb.popActiveModuleContext(token);

    var callback_invoked = false;
    try installState(ctx, system_path, @ptrFromInt(0x1), struct {
        fn call(
            _: *anyopaque,
            _: *context.Context,
            _: []const u8,
            _: []const u8,
            _: value.JSValue,
        ) anyerror!value.JSValue {
            return value.JSValue.undefined_val;
        }
    }.call);

    const state = ctx.getModuleState(ServiceState, MODULE_STATE_SLOT) orelse return error.TestFailed;
    const base_url = state.services.get("users") orelse return error.TestFailed;
    try std.testing.expectEqualStrings("http://users.internal", base_url);
    _ = &callback_invoked;
}
