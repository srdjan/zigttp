const std = @import("std");
const h = @import("helpers.zig");
const value = h.value;
const context = h.context;
const compat = h.compat;
const trace_mod = h.trace_mod;

const toNumber = h.toNumber;
const allocFloat = h.allocFloat;

pub fn mathAbs(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const arg = args[0];
    // Integer fast path: abs(int) = int for non-negative, -int for negative
    if (arg.isInt()) {
        const i = arg.getInt();
        if (i >= 0) return arg;
        // Handle overflow case: abs(-2147483648) cannot fit in i32
        if (i == std.math.minInt(i32)) {
            return allocFloat(ctx, 2147483648.0);
        }
        return value.JSValue.fromInt(-i);
    }
    const n = toNumber(arg) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @abs(n));
}

pub fn mathFloor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const arg = args[0];
    // Integer fast path: floor(int) = int
    if (arg.isInt()) return arg;
    const n = toNumber(arg) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @floor(n));
}

pub fn mathCeil(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const arg = args[0];
    // Integer fast path: ceil(int) = int
    if (arg.isInt()) return arg;
    const n = toNumber(arg) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @ceil(n));
}

pub fn mathRound(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const arg = args[0];
    // Integer fast path: round(int) = int
    if (arg.isInt()) return arg;
    const n = toNumber(arg) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @round(n));
}

pub fn mathTrunc(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const arg = args[0];
    // Integer fast path: trunc(int) = int
    if (arg.isInt()) return arg;
    const n = toNumber(arg) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @trunc(n));
}

pub fn mathMin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return allocFloat(ctx, std.math.inf(f64));

    // Fast path: two integer arguments (common case)
    if (args.len == 2 and args[0].isInt() and args[1].isInt()) {
        const a = args[0].getInt();
        const b = args[1].getInt();
        return value.JSValue.fromInt(@min(a, b));
    }

    var min_val: f64 = std.math.inf(f64);
    for (args) |arg| {
        const n = toNumber(arg) orelse return value.JSValue.undefined_val;
        if (std.math.isNan(n)) return allocFloat(ctx, std.math.nan(f64));
        min_val = @min(min_val, n);
    }
    return allocFloat(ctx, min_val);
}

pub fn mathMax(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return allocFloat(ctx, -std.math.inf(f64));

    // Fast path: two integer arguments (common case)
    if (args.len == 2 and args[0].isInt() and args[1].isInt()) {
        const a = args[0].getInt();
        const b = args[1].getInt();
        return value.JSValue.fromInt(@max(a, b));
    }

    var max_val: f64 = -std.math.inf(f64);
    for (args) |arg| {
        const n = toNumber(arg) orelse return value.JSValue.undefined_val;
        if (std.math.isNan(n)) return allocFloat(ctx, std.math.nan(f64));
        max_val = @max(max_val, n);
    }
    return allocFloat(ctx, max_val);
}

pub fn mathPow(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len < 2) return value.JSValue.undefined_val;
    const base = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    const exp = toNumber(args[1]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, std.math.pow(f64, base, exp));
}

pub fn mathSqrt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    if (n < 0) return allocFloat(ctx, std.math.nan(f64));
    return allocFloat(ctx, @sqrt(n));
}

pub fn mathSin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @sin(n));
}

pub fn mathCos(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @cos(n));
}

pub fn mathTan(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @tan(n));
}

pub fn mathLog(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @log(n));
}

pub fn mathExp(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @exp(n));
}

threadlocal var prng: ?std.Random.DefaultPrng = null;

fn getRandomFloat() f64 {
    if (prng == null) {
        const seed = compat.realtimeNowNs() catch 0x9e3779b97f4a7c15;
        prng = std.Random.DefaultPrng.init(seed);
    }
    return prng.?.random().float(f64);
}

/// Math.random() - Returns a pseudo-random float in [0, 1)
/// When trace recording is active, the result is recorded as an I/O call
/// so replay can reproduce the exact value.
/// When replay is active, the recorded value is returned instead.
/// When durable mode is active, replays from oplog or records with write-ahead.
pub fn mathRandom(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    _ = args;

    // Durable mode: replay from oplog or execute and persist
    if (ctx.getModuleState(trace_mod.DurableState, trace_mod.DURABLE_STATE_SLOT)) |durable| {
        if (durable.replayNext("builtin", "Math.random")) |entry| {
            return trace_mod.jsonToJSValue(ctx, entry.result_json);
        }
        // Live phase: generate real random, persist, return
        const r = getRandomFloat();
        const result = allocFloat(ctx, r);
        durable.persistIO("builtin", "Math.random", ctx, &.{}, result);
        return result;
    }

    // Replay mode: return recorded value
    if (ctx.getModuleState(trace_mod.ReplayState, trace_mod.REPLAY_STATE_SLOT)) |state| {
        const entry = state.nextIO("builtin", "Math.random") orelse return value.JSValue.undefined_val;
        return trace_mod.jsonToJSValue(ctx, entry.result_json);
    }

    // Normal execution
    const r = getRandomFloat();
    const result = allocFloat(ctx, r);

    // Record to trace if active (Math.random is a non-determinism source)
    if (ctx.getModuleState(trace_mod.TraceRecorder, trace_mod.TRACE_STATE_SLOT)) |recorder| {
        recorder.recordIO("builtin", "Math.random", ctx, &.{}, result);
    }

    return result;
}

pub fn mathSign(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    if (std.math.isNan(n)) return allocFloat(ctx, std.math.nan(f64));
    if (n > 0) return value.JSValue.fromInt(1);
    if (n < 0) return value.JSValue.fromInt(-1);
    return value.JSValue.fromInt(0);
}

/// Math constants
pub const math_constants = struct {
    pub const PI: f64 = 3.141592653589793;
    pub const E: f64 = 2.718281828459045;
    pub const LN2: f64 = 0.6931471805599453;
    pub const LN10: f64 = 2.302585092994046;
    pub const LOG2E: f64 = 1.4426950408889634;
    pub const LOG10E: f64 = 0.4342944819032518;
    pub const SQRT2: f64 = 1.4142135623730951;
    pub const SQRT1_2: f64 = 0.7071067811865476;
};
