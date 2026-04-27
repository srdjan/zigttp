//! Small numeric and bytecode-decoding helpers used throughout dispatch.

const std = @import("std");
const value = @import("../value.zig");

/// Modulo operation - integer fast path with float fallback.
pub inline fn modValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const bv = b.getInt();
        if (bv == 0) return error.DivisionByZero;
        return value.JSValue.fromInt(@rem(a.getInt(), bv));
    }
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    if (bn == 0.0) return value.JSValue.nan_val;
    return value.JSValue.fromFloat(@rem(an, bn));
}

/// Convert a JSValue to Int32 per ECMAScript ToInt32 (used by bitwise ops).
pub inline fn toInt32(v: value.JSValue) i32 {
    if (v.isInt()) return v.getInt();
    if (v.isFloat64()) {
        const f = v.getFloat64();
        if (std.math.isNan(f) or std.math.isInf(f) or f == 0) return 0;
        const int_val: i64 = @intFromFloat(@trunc(f));
        return @truncate(int_val);
    }
    return 0;
}

pub inline fn readI16(pc: [*]const u8) i16 {
    return @bitCast(@as(u16, pc[0]) | (@as(u16, pc[1]) << 8));
}

pub inline fn readU16(pc: [*]const u8) u16 {
    return @as(u16, pc[0]) | (@as(u16, pc[1]) << 8);
}
