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
        return value.JSValue.fromInt(@rem(a.getInt(), bv));
    }
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    if (bn == 0.0) return value.JSValue.nan_val;
    return value.JSValue.fromFloat(@rem(an, bn));
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
