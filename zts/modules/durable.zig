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
    sleep_until_fn: *const fn (*anyopaque, *context.Context, i64) anyerror!value.JSValue,
    wait_signal_fn: *const fn (*anyopaque, *context.Context, []const u8) anyerror!value.JSValue,
    signal_fn: *const fn (*anyopaque, *context.Context, []const u8, []const u8, value.JSValue) anyerror!value.JSValue,
    signal_at_fn: *const fn (*anyopaque, *context.Context, []const u8, []const u8, i64, value.JSValue) anyerror!value.JSValue,
    runtime_ptr: *anyopaque,

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *DurableCallbacks = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }
};

pub const exports = [_]resolver.ModuleExport{
    .{ .name = "run", .func = runNative, .arg_count = 2 },
    .{ .name = "step", .func = stepNative, .arg_count = 2 },
    .{ .name = "sleep", .func = sleepNative, .arg_count = 1 },
    .{ .name = "sleepUntil", .func = sleepUntilNative, .arg_count = 1 },
    .{ .name = "waitSignal", .func = waitSignalNative, .arg_count = 1 },
    .{ .name = "signal", .func = signalNative, .arg_count = 2 },
    .{ .name = "signalAt", .func = signalAtNative, .arg_count = 3 },
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

fn sleepNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "sleep() requires --durable <DIR>");
    };

    if (args.len < 1) {
        return util.throwError(ctx, "TypeError", "sleep() expects a delay in milliseconds");
    }

    const delay_ms_f = util.extractFloat(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "sleep() delay must be a number");
    };
    if (!std.math.isFinite(delay_ms_f)) {
        return util.throwError(ctx, "TypeError", "sleep() delay must be finite");
    }

    const clamped = @max(delay_ms_f, 0);
    const delay_ms: i64 = @intFromFloat(clamped);
    const now_ms = unixMillis();
    const until_ms = std.math.add(i64, now_ms, delay_ms) catch std.math.maxInt(i64);
    return callbacks.sleep_until_fn(callbacks.runtime_ptr, ctx, until_ms);
}

fn sleepUntilNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "sleepUntil() requires --durable <DIR>");
    };

    if (args.len < 1) {
        return util.throwError(ctx, "TypeError", "sleepUntil() expects a unix timestamp in milliseconds");
    }

    const until_ms_f = util.extractFloat(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "sleepUntil() timestamp must be a number");
    };
    if (!std.math.isFinite(until_ms_f)) {
        return util.throwError(ctx, "TypeError", "sleepUntil() timestamp must be finite");
    }

    const until_ms: i64 = @intFromFloat(until_ms_f);
    return callbacks.sleep_until_fn(callbacks.runtime_ptr, ctx, until_ms);
}

fn waitSignalNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "waitSignal() requires --durable <DIR>");
    };

    if (args.len < 1) {
        return util.throwError(ctx, "TypeError", "waitSignal() expects a string name");
    }

    const name = util.extractString(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "waitSignal() name must be a string");
    };
    return callbacks.wait_signal_fn(callbacks.runtime_ptr, ctx, name);
}

fn signalNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "signal() requires --durable <DIR>");
    };

    if (args.len < 2) {
        return util.throwError(ctx, "TypeError", "signal() expects a durable key and signal name");
    }

    const key = util.extractString(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "signal() durable key must be a string");
    };
    const name = util.extractString(args[1]) orelse {
        return util.throwError(ctx, "TypeError", "signal() signal name must be a string");
    };
    const payload = if (args.len >= 3) args[2] else value.JSValue.undefined_val;
    return callbacks.signal_fn(callbacks.runtime_ptr, ctx, key, name, payload);
}

fn signalAtNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "signalAt() requires --durable <DIR>");
    };

    if (args.len < 3) {
        return util.throwError(ctx, "TypeError", "signalAt() expects a durable key, signal name, and unix timestamp in milliseconds");
    }

    const key = util.extractString(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "signalAt() durable key must be a string");
    };
    const name = util.extractString(args[1]) orelse {
        return util.throwError(ctx, "TypeError", "signalAt() signal name must be a string");
    };
    const at_ms_f = util.extractFloat(args[2]) orelse {
        return util.throwError(ctx, "TypeError", "signalAt() timestamp must be a number");
    };
    if (!std.math.isFinite(at_ms_f)) {
        return util.throwError(ctx, "TypeError", "signalAt() timestamp must be finite");
    }

    const at_ms: i64 = @intFromFloat(at_ms_f);
    const payload = if (args.len >= 4) args[3] else value.JSValue.undefined_val;
    return callbacks.signal_at_fn(callbacks.runtime_ptr, ctx, key, name, at_ms, payload);
}

fn getCallbacks(ctx: *context.Context) ?*DurableCallbacks {
    return ctx.getModuleState(DurableCallbacks, MODULE_STATE_SLOT);
}

fn unixMillis() i64 {
    var ts: std.posix.timespec = undefined;
    switch (std.posix.errno(std.posix.system.clock_gettime(.REALTIME, &ts))) {
        .SUCCESS => {
            const secs: i64 = @intCast(ts.sec);
            const nanos: i64 = @intCast(ts.nsec);
            return (secs * 1000) + @divTrunc(nanos, 1_000_000);
        },
        else => return 0,
    }
}
