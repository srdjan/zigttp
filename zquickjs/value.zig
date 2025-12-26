//! JSValue NaN-boxing implementation
//!
//! 64-bit tagged value representation using NaN-boxing technique.
//! Compatible with C mquickjs JSValue encoding.

const std = @import("std");

/// 64-bit NaN-boxed JavaScript value
pub const JSValue = packed struct {
    raw: u64,

    /// Tag bits encoded in the lower bits
    pub const Tag = enum(u3) {
        int = 0, // 31-bit signed integer (LSB = 0)
        ptr = 1, // Heap object pointer
        special = 3, // Bool/null/undefined/exception
        short_float = 5, // Short float (64-bit mode)
    };

    // Special values: tag 3 in lower 3 bits, payload in upper bits
    pub const null_val: JSValue = .{ .raw = (0 << 3) | 3 };
    pub const undefined_val: JSValue = .{ .raw = (1 << 3) | 3 };
    pub const true_val: JSValue = .{ .raw = (2 << 3) | 3 };
    pub const false_val: JSValue = .{ .raw = (3 << 3) | 3 };
    pub const exception_val: JSValue = .{ .raw = (4 << 3) | 3 };

    /// Check if value is a 31-bit integer (fast path)
    pub inline fn isInt(self: JSValue) bool {
        return (self.raw & 1) == 0;
    }

    /// Extract 31-bit signed integer
    pub inline fn getInt(self: JSValue) i32 {
        std.debug.assert(self.isInt());
        return @bitCast(@as(u32, @truncate(self.raw >> 1)));
    }

    /// Create integer value
    pub inline fn fromInt(val: i32) JSValue {
        const as_u32: u32 = @bitCast(val);
        return .{ .raw = @as(u64, as_u32) << 1 };
    }

    /// Check if value is a pointer
    pub inline fn isPtr(self: JSValue) bool {
        return (self.raw & 0x7) == 1;
    }

    /// Create value from pointer
    pub inline fn fromPtr(ptr: *anyopaque) JSValue {
        return .{ .raw = @intFromPtr(ptr) | 1 };
    }

    /// Extract pointer (unsafe - caller must verify isPtr)
    pub inline fn toPtr(self: JSValue, comptime T: type) *T {
        std.debug.assert(self.isPtr());
        return @ptrFromInt(self.raw & ~@as(u64, 0x7));
    }

    /// Check if value is a special value
    pub inline fn isSpecial(self: JSValue) bool {
        return (self.raw & 0x7) == 3;
    }

    /// Check for specific special values
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

    pub inline fn isBool(self: JSValue) bool {
        return self.isTrue() or self.isFalse();
    }

    pub inline fn isException(self: JSValue) bool {
        return self.raw == exception_val.raw;
    }

    /// Get boolean value
    pub inline fn getBool(self: JSValue) bool {
        std.debug.assert(self.isBool());
        return self.isTrue();
    }

    /// Create boolean value
    pub inline fn fromBool(val: bool) JSValue {
        return if (val) true_val else false_val;
    }

    /// Check if value is nullish (null or undefined)
    pub inline fn isNullish(self: JSValue) bool {
        return self.isNull() or self.isUndefined();
    }
};

// Tests
test "JSValue integer encoding" {
    const zero = JSValue.fromInt(0);
    try std.testing.expect(zero.isInt());
    try std.testing.expectEqual(@as(i32, 0), zero.getInt());

    const positive = JSValue.fromInt(42);
    try std.testing.expect(positive.isInt());
    try std.testing.expectEqual(@as(i32, 42), positive.getInt());

    const negative = JSValue.fromInt(-123);
    try std.testing.expect(negative.isInt());
    try std.testing.expectEqual(@as(i32, -123), negative.getInt());

    const max = JSValue.fromInt(std.math.maxInt(i31));
    try std.testing.expect(max.isInt());

    const min = JSValue.fromInt(std.math.minInt(i31));
    try std.testing.expect(min.isInt());
}

test "JSValue special values" {
    try std.testing.expect(JSValue.null_val.isNull());
    try std.testing.expect(JSValue.null_val.isSpecial());
    try std.testing.expect(!JSValue.null_val.isInt());

    try std.testing.expect(JSValue.undefined_val.isUndefined());
    try std.testing.expect(JSValue.true_val.isTrue());
    try std.testing.expect(JSValue.true_val.isBool());
    try std.testing.expect(JSValue.false_val.isFalse());
    try std.testing.expect(JSValue.false_val.isBool());
    try std.testing.expect(JSValue.exception_val.isException());
}

test "JSValue boolean conversion" {
    try std.testing.expectEqual(JSValue.true_val, JSValue.fromBool(true));
    try std.testing.expectEqual(JSValue.false_val, JSValue.fromBool(false));
    try std.testing.expect(JSValue.true_val.getBool());
    try std.testing.expect(!JSValue.false_val.getBool());
}
