const std = @import("std");

/// Public ABI-facing JS value representation for packaged virtual modules.
/// The runtime adapts this to its internal NaN-boxed value type by raw bit cast.
pub const JSValue = packed struct(u64) {
    raw: u64,

    const INT_PREFIX: u64 = 0xFFFD_0000_0000_0000;
    const SPECIAL_PREFIX: u64 = 0xFFFE_0000_0000_0000;
    const PREFIX_MASK: u64 = 0xFFFF_0000_0000_0000;
    const MIN_TAG_PREFIX: u64 = 0xFFFC;
    const CANONICAL_NAN_BITS: u64 = 0x7FF8_0000_0000_0000;

    pub const null_val: JSValue = .{ .raw = SPECIAL_PREFIX | 0 };
    pub const undefined_val: JSValue = .{ .raw = SPECIAL_PREFIX | 1 };
    pub const true_val: JSValue = .{ .raw = SPECIAL_PREFIX | 2 };
    pub const false_val: JSValue = .{ .raw = SPECIAL_PREFIX | 3 };

    pub inline fn isNull(self: JSValue) bool {
        return self.raw == null_val.raw;
    }

    pub inline fn isUndefined(self: JSValue) bool {
        return self.raw == undefined_val.raw;
    }

    pub inline fn isTrue(self: JSValue) bool {
        return self.raw == true_val.raw;
    }

    pub inline fn isFalse(self: JSValue) bool {
        return self.raw == false_val.raw;
    }

    pub inline fn isNullish(self: JSValue) bool {
        return self.isNull() or self.isUndefined();
    }

    pub inline fn isNumber(self: JSValue) bool {
        return self.isInt() or self.isRawDouble();
    }

    pub inline fn isInt(self: JSValue) bool {
        return (self.raw & PREFIX_MASK) == INT_PREFIX;
    }

    pub inline fn fromInt(v: i32) JSValue {
        const bits: u32 = @bitCast(v);
        return .{ .raw = INT_PREFIX | @as(u64, bits) };
    }

    pub inline fn getInt(self: JSValue) i32 {
        std.debug.assert(self.isInt());
        return @bitCast(@as(u32, @truncate(self.raw)));
    }

    pub inline fn toInt(self: JSValue) ?i32 {
        return if (self.isInt()) self.getInt() else null;
    }

    pub inline fn isBool(self: JSValue) bool {
        return self.raw == true_val.raw or self.raw == false_val.raw;
    }

    pub inline fn fromBool(v: bool) JSValue {
        return if (v) true_val else false_val;
    }

    pub inline fn getBool(self: JSValue) bool {
        std.debug.assert(self.isBool());
        return self.raw == true_val.raw;
    }

    pub inline fn isRawDouble(self: JSValue) bool {
        return (self.raw >> 48) < MIN_TAG_PREFIX;
    }

    pub inline fn fromFloat(v: f64) JSValue {
        const bits: u64 = @bitCast(v);
        if ((bits >> 48) >= MIN_TAG_PREFIX) {
            return .{ .raw = CANONICAL_NAN_BITS };
        }
        return .{ .raw = bits };
    }

    pub inline fn toFloat(self: JSValue) ?f64 {
        return if (self.isRawDouble()) @bitCast(self.raw) else null;
    }
};

/// Extract an i32 from a JSValue, handling both int and whole-number float
/// representations.
pub fn extractInt(val: JSValue) ?i32 {
    if (val.toInt()) |i| return i;
    if (val.toFloat()) |f| {
        // Range/finiteness check BEFORE @intFromFloat: converting a NaN or an
        // out-of-i32 float is illegal behavior (panic in safe builds, UB in
        // ReleaseFast). Mirrors numberFromF64's guard below.
        if (std.math.isFinite(f) and @floor(f) == f and f >= -2147483648.0 and f <= 2147483647.0) {
            return @intFromFloat(f);
        }
    }
    return null;
}

/// Extract an f64 from a JSValue, widening int values.
pub fn extractFloat(val: JSValue) ?f64 {
    if (val.toInt()) |i| return @floatFromInt(i);
    return val.toFloat();
}

/// Box a Zig f64 as a JS number, folding safe-integer values into the
/// int32 tagged representation. Matches the runtime's allocFloat hot path.
pub fn numberFromF64(v: f64) JSValue {
    if (!std.math.isNan(v) and !std.math.isInf(v) and @floor(v) == v and v >= -2147483648 and v <= 2147483647) {
        return JSValue.fromInt(@intFromFloat(v));
    }
    return JSValue.fromFloat(v);
}
