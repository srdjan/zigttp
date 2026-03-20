//! zigttp:durable - crash-safe, replay-safe durable execution
//!
//! Exports:
//!   run(key: string, fn: () => Response) -> Response
//!   step(name: string, fn: () => unknown) -> unknown
//!
//! The actual execution/storage behavior lives in the runtime layer. This
//! module validates JS arguments, then delegates to runtime-owned callbacks.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const resolver = @import("resolver.zig");
const util = @import("util.zig");

pub const MODULE_STATE_SLOT = @intFromEnum(@import("../module_slots.zig").Slot.durable_api);

/// Runtime-owned callbacks installed by src/zruntime.zig when --durable is enabled.
pub const DurableCallbacks = struct {
    run_fn: *const fn (*anyopaque, *context.Context, []const u8, value.JSValue) anyerror!value.JSValue,
    step_fn: *const fn (*anyopaque, *context.Context, []const u8, value.JSValue) anyerror!value.JSValue,
    runtime_ptr: *anyopaque,

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *DurableCallbacks = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }
};

pub const exports = [_]resolver.ModuleExport{
    .{ .name = "run", .func = runNative, .arg_count = 2 },
    .{ .name = "step", .func = stepNative, .arg_count = 2 },
};

fn runNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "run() requires --durable <DIR>");
    };

    if (args.len < 2) {
        return util.throwError(ctx, "TypeError", "run() expects a string key and function");
    }

    const key = util.extractString(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "run() key must be a string");
    };
    if (!args[1].isCallable()) {
        return util.throwError(ctx, "TypeError", "run() expects a function as its second argument");
    }

    return callbacks.run_fn(callbacks.runtime_ptr, ctx, key, args[1]);
}

fn stepNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "step() requires --durable <DIR>");
    };

    if (args.len < 2) {
        return util.throwError(ctx, "TypeError", "step() expects a string name and function");
    }

    const name = util.extractString(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "step() name must be a string");
    };
    if (!args[1].isCallable()) {
        return util.throwError(ctx, "TypeError", "step() expects a function as its second argument");
    }

    return callbacks.step_fn(callbacks.runtime_ptr, ctx, name, args[1]);
}

fn getCallbacks(ctx: *context.Context) ?*DurableCallbacks {
    return ctx.getModuleState(DurableCallbacks, MODULE_STATE_SLOT);
}
