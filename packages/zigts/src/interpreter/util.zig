//! Small numeric and bytecode-decoding helpers used throughout dispatch.

const std = @import("std");
const value = @import("../value.zig");

/// Modulo operation - integer fast path with float fallback. Per ECMAScript,
/// the result of `x % 0` is NaN (not a thrown error); only a genuinely
/// non-numeric operand fails.
pub inline fn modValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const bv = b.getInt();
        if (bv == 0) return value.JSValue.nan_val;
        const av = a.getInt();
        // INT_MIN % -1: quotient would overflow i32; LLVM srem is poison here.
        // JS result is -0 (treated as 0 for integer representation).
        if (av == std.math.minInt(i32) and bv == -1) return value.JSValue.fromInt(0);
        return value.JSValue.fromInt(@rem(av, bv));
    }
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    if (bn == 0.0) return value.JSValue.nan_val;
    return value.JSValue.fromFloat(@rem(an, bn));
}

// Arithmetic cores shared with the JIT helpers in context.zig. These are the
// numeric semantics only (no string concat, no per-operand trace), returning a
// NaN-boxed result or error.TypeError on a non-numeric operand. The interpreter
// keeps its own trace-emitting slow paths (arith.zig) because those distinguish
// which operand failed; these cores let the JIT helpers stay thin delegates,
// mirroring how jitMod already routes through modValues.

/// Numeric addition (integer fast path, overflow -> float). The JIT add helper
/// handles string concat before calling this.
pub inline fn addNumeric(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const sum, const overflow = @addWithOverflow(a.getInt(), b.getInt());
        if (overflow == 0) return value.JSValue.fromInt(sum);
        return value.JSValue.fromFloat(@as(f64, @floatFromInt(a.getInt())) + @as(f64, @floatFromInt(b.getInt())));
    }
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    return value.JSValue.fromFloat(an + bn);
}

pub inline fn subValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const diff, const overflow = @subWithOverflow(a.getInt(), b.getInt());
        if (overflow == 0) return value.JSValue.fromInt(diff);
        return value.JSValue.fromFloat(@as(f64, @floatFromInt(a.getInt())) - @as(f64, @floatFromInt(b.getInt())));
    }
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    return value.JSValue.fromFloat(an - bn);
}

pub inline fn mulValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const product, const overflow = @mulWithOverflow(a.getInt(), b.getInt());
        if (overflow == 0) return value.JSValue.fromInt(product);
        return value.JSValue.fromFloat(@as(f64, @floatFromInt(a.getInt())) * @as(f64, @floatFromInt(b.getInt())));
    }
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    return value.JSValue.fromFloat(an * bn);
}

pub inline fn negValue(a: value.JSValue) !value.JSValue {
    if (a.isInt()) {
        const v = a.getInt();
        if (v == std.math.minInt(i32)) return value.JSValue.fromFloat(-@as(f64, @floatFromInt(v)));
        return value.JSValue.fromInt(-v);
    }
    if (a.isFloat64()) return value.JSValue.fromFloat(-a.getFloat64());
    return error.TypeError;
}

/// Division always produces a float in this engine.
pub inline fn divValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    return value.JSValue.fromFloat(an / bn);
}

pub inline fn powValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    return value.JSValue.fromFloat(std.math.pow(f64, an, bn));
}

pub inline fn incValue(a: value.JSValue) !value.JSValue {
    if (a.isInt()) {
        const sum, const overflow = @addWithOverflow(a.getInt(), 1);
        if (overflow == 0) return value.JSValue.fromInt(sum);
        return value.JSValue.fromFloat(@as(f64, @floatFromInt(a.getInt())) + 1.0);
    }
    if (a.isFloat64()) return value.JSValue.fromFloat(a.getFloat64() + 1.0);
    return error.TypeError;
}

pub inline fn decValue(a: value.JSValue) !value.JSValue {
    if (a.isInt()) {
        const diff, const overflow = @subWithOverflow(a.getInt(), 1);
        if (overflow == 0) return value.JSValue.fromInt(diff);
        return value.JSValue.fromFloat(@as(f64, @floatFromInt(a.getInt())) - 1.0);
    }
    if (a.isFloat64()) return value.JSValue.fromFloat(a.getFloat64() - 1.0);
    return error.TypeError;
}

/// ECMAScript ToInt32 on a raw f64. Reduces modulo 2^32 *before* the integer
/// cast, so it is total over every finite input - the naive
/// `@intFromFloat(@trunc(f))` is illegal behavior (panic in safe builds, UB in
/// ReleaseFast) for `|f| >= 2^63`, e.g. `1e30 | 0`. Shared with the comptime
/// evaluator so compile-time and run-time bitwise ops agree.
pub inline fn floatToInt32(f: f64) i32 {
    if (!std.math.isFinite(f)) return 0;
    const t = @trunc(f);
    const m = @mod(t, 4294967296.0); // in [0, 2^32)
    // m is non-negative and below 2^32, so the u64 cast cannot overflow; the
    // truncate to u32 performs the ECMAScript modulo-2^32 wrap.
    const bits: u32 = @truncate(@as(u64, @intFromFloat(m)));
    return @bitCast(bits);
}

/// Convert a JSValue to Int32 per ECMAScript ToInt32 (used by bitwise ops).
pub inline fn toInt32(v: value.JSValue) i32 {
    if (v.isInt()) return v.getInt();
    if (v.isFloat64()) return floatToInt32(v.getFloat64());
    return 0;
}

/// Box a u32 as a JS number. The result of `>>>` is an *unsigned* 32-bit value;
/// when it exceeds maxInt(i32) it does not fit the signed int tag and must be
/// stored as a float, otherwise `(-1) >>> 0` reads back as `-1` instead of
/// `4294967295`.
pub inline fn fromUint32(u: u32) value.JSValue {
    if (u <= std.math.maxInt(i32)) return value.JSValue.fromInt(@intCast(u));
    return value.JSValue.fromFloat(@floatFromInt(u));
}

pub inline fn readI16(pc: [*]const u8) i16 {
    return @bitCast(@as(u16, pc[0]) | (@as(u16, pc[1]) << 8));
}

pub inline fn readU16(pc: [*]const u8) u16 {
    return @as(u16, pc[0]) | (@as(u16, pc[1]) << 8);
}
