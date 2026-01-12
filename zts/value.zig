//! JSValue NaN-boxing implementation
//!
//! 64-bit tagged value representation using NaN-boxing technique.
//! Compatible with QuickJS-style JSValue encoding.

const std = @import("std");
const heap = @import("heap.zig");

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
    pub const nan_val: JSValue = .{ .raw = (5 << 3) | 3 }; // NaN special value

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

    // ========================================================================
    // Float64 Support (heap-boxed)
    // ========================================================================

    /// Float64 box header (heap-allocated)
    pub const Float64Box = extern struct {
        header: heap.MemBlockHeader,
        _pad: u32,
        value: f64,

        pub fn getValue(self: *const Float64Box) f64 {
            return self.value;
        }
    };

    /// Check if value is a boxed float64
    pub inline fn isFloat64(self: JSValue) bool {
        if (!self.isPtr()) return false;
        const box = self.toPtr(Float64Box);
        return box.header.tag == .float64;
    }

    /// Alias for isFloat64 for API compatibility
    pub inline fn isFloat(self: JSValue) bool {
        return self.isFloat64();
    }

    /// Check if value is any number (int or float)
    pub inline fn isNumber(self: JSValue) bool {
        return self.isInt() or self.isFloat64();
    }

    /// Get float64 value (unsafe - caller must verify isFloat64)
    pub inline fn getFloat64(self: JSValue) f64 {
        std.debug.assert(self.isFloat64());
        const box = self.toPtr(Float64Box);
        return box.value;
    }

    /// Convert value to f64 (works for int and float)
    pub inline fn toNumber(self: JSValue) ?f64 {
        if (self.isInt()) {
            return @floatFromInt(self.getInt());
        }
        if (self.isFloat64()) {
            return self.getFloat64();
        }
        return null;
    }

    // ========================================================================
    // Symbol Support (heap-boxed)
    // ========================================================================

    /// Symbol box header (heap-allocated)
    /// Symbols are unique identifiers with an optional description
    pub const SymbolBox = extern struct {
        header: u32, // MemTag.symbol + gc_mark (tag 8)
        id: u32, // Unique symbol ID
        description_ptr: ?[*]const u8, // Optional description string
        description_len: u32, // Length of description

        pub fn getId(self: *const SymbolBox) u32 {
            return self.id;
        }

        pub fn getDescription(self: *const SymbolBox) ?[]const u8 {
            if (self.description_ptr) |ptr| {
                return ptr[0..self.description_len];
            }
            return null;
        }
    };

    /// Well-known symbol IDs (predefined)
    pub const WellKnownSymbol = enum(u32) {
        iterator = 1,
        asyncIterator = 2,
        toStringTag = 3,
        toPrimitive = 4,
        hasInstance = 5,
        isConcatSpreadable = 6,
        species = 7,
        match = 8,
        replace = 9,
        search = 10,
        split = 11,
        unscopables = 12,
    };

    /// Check if value is a symbol
    pub inline fn isSymbol(self: JSValue) bool {
        if (!self.isPtr()) return false;
        const header = self.toPtr(u32);
        return ((header.* >> 1) & 0xF) == 8; // MemTag.symbol
    }

    /// Get symbol ID (unsafe - caller must verify isSymbol)
    pub inline fn getSymbolId(self: JSValue) u32 {
        std.debug.assert(self.isSymbol());
        const box = self.toPtr(SymbolBox);
        return box.id;
    }

    /// Get symbol description (unsafe - caller must verify isSymbol)
    pub inline fn getSymbolDescription(self: JSValue) ?[]const u8 {
        std.debug.assert(self.isSymbol());
        const box = self.toPtr(SymbolBox);
        return box.getDescription();
    }

    /// Check if symbol is a well-known symbol
    pub inline fn isWellKnownSymbol(self: JSValue, which: WellKnownSymbol) bool {
        if (!self.isSymbol()) return false;
        return self.getSymbolId() == @intFromEnum(which);
    }

    // ========================================================================
    // Type Checking Utilities
    // ========================================================================

    /// Check if value is a string (heap object with string tag)
    /// MemBlockHeader layout: [size_words:27][tag:4][gc_mark:1]
    /// So tag is at bits 1-4, need to shift right by 1 first
    pub inline fn isString(self: JSValue) bool {
        if (!self.isPtr()) return false;
        const header = self.toPtr(u32);
        return ((header.* >> 1) & 0xF) == 3; // MemTag.string
    }

    /// Check if value is an object (heap object with object tag)
    pub inline fn isObject(self: JSValue) bool {
        if (!self.isPtr()) return false;
        const header = self.toPtr(u32);
        return ((header.* >> 1) & 0xF) == 1; // MemTag.object
    }

    /// Check if value is a function
    pub inline fn isFunction(self: JSValue) bool {
        if (!self.isPtr()) return false;
        const header = self.toPtr(u32);
        return ((header.* >> 1) & 0xF) == 4; // MemTag.function_bytecode
    }

    /// Check if value is an array
    pub inline fn isArray(self: JSValue) bool {
        if (!self.isObject()) return false;
        const obj_ptr = self.toPtr(@import("object.zig").JSObject);
        return obj_ptr.class_id == .array;
    }

    /// Check if value is callable (function)
    pub inline fn isCallable(self: JSValue) bool {
        if (!self.isPtr()) return false;
        // Check if it's an object with is_callable flag
        if (self.isObject()) {
            const obj_ptr = self.toPtr(@import("object.zig").JSObject);
            return obj_ptr.flags.is_callable;
        }
        return self.isFunction();
    }

    // ========================================================================
    // Comparison and Conversion
    // ========================================================================

    /// Strict equality (===)
    pub inline fn strictEquals(self: JSValue, other: JSValue) bool {
        // Fast path: same raw value (covers same pointer, same int, same special)
        if (self.raw == other.raw) return true;

        // Float comparison (handle NaN)
        if (self.isFloat64() and other.isFloat64()) {
            const a = self.getFloat64();
            const b = other.getFloat64();
            // NaN !== NaN
            if (std.math.isNan(a) or std.math.isNan(b)) return false;
            return a == b;
        }

        // String comparison (compare by value, not pointer)
        if (self.isString() and other.isString()) {
            const str_a = self.toPtr(@import("string.zig").JSString);
            const str_b = other.toPtr(@import("string.zig").JSString);
            return std.mem.eql(u8, str_a.data(), str_b.data());
        }

        return false;
    }

    /// Type coercion to boolean (ToBoolean)
    pub inline fn toBoolean(self: JSValue) bool {
        if (self.isNull() or self.isUndefined() or self.isFalse()) return false;
        if (self.isTrue()) return true;
        if (self.isInt()) return self.getInt() != 0;
        if (self.isFloat64()) {
            const f = self.getFloat64();
            return f != 0.0 and !std.math.isNan(f);
        }
        // Objects are truthy
        return true;
    }

    /// Get the JS typeof result
    pub fn typeOf(self: JSValue) []const u8 {
        if (self.isUndefined()) return "undefined";
        if (self.isNull()) return "object"; // Historical quirk
        if (self.isBool()) return "boolean";
        if (self.isNumber()) return "number";
        if (self.isString()) return "string";
        if (self.isCallable()) return "function";
        return "object";
    }

    // ========================================================================
    // Debug Helpers
    // ========================================================================

    /// Format value for debug output
    pub fn format(self: JSValue, comptime _: []const u8, _: std.fmt.Options, writer: anytype) !void {
        if (self.isNull()) {
            try writer.writeAll("null");
        } else if (self.isUndefined()) {
            try writer.writeAll("undefined");
        } else if (self.isTrue()) {
            try writer.writeAll("true");
        } else if (self.isFalse()) {
            try writer.writeAll("false");
        } else if (self.isException()) {
            try writer.writeAll("<exception>");
        } else if (self.isInt()) {
            try writer.print("{d}", .{self.getInt()});
        } else if (self.isFloat64()) {
            try writer.print("{d}", .{self.getFloat64()});
        } else if (self.isPtr()) {
            try writer.print("<ptr:0x{x}>", .{self.raw & ~@as(u64, 0x7)});
        } else {
            try writer.print("<unknown:0x{x}>", .{self.raw});
        }
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

test "JSValue toBoolean coercion" {
    // Falsy values
    try std.testing.expect(!JSValue.null_val.toBoolean());
    try std.testing.expect(!JSValue.undefined_val.toBoolean());
    try std.testing.expect(!JSValue.false_val.toBoolean());
    try std.testing.expect(!JSValue.fromInt(0).toBoolean());

    // Truthy values
    try std.testing.expect(JSValue.true_val.toBoolean());
    try std.testing.expect(JSValue.fromInt(1).toBoolean());
    try std.testing.expect(JSValue.fromInt(-1).toBoolean());
    try std.testing.expect(JSValue.fromInt(42).toBoolean());
}

test "JSValue typeof" {
    try std.testing.expectEqualStrings("undefined", JSValue.undefined_val.typeOf());
    try std.testing.expectEqualStrings("object", JSValue.null_val.typeOf()); // JS quirk
    try std.testing.expectEqualStrings("boolean", JSValue.true_val.typeOf());
    try std.testing.expectEqualStrings("boolean", JSValue.false_val.typeOf());
    try std.testing.expectEqualStrings("number", JSValue.fromInt(42).typeOf());
}

test "JSValue strict equality" {
    // Same values
    try std.testing.expect(JSValue.null_val.strictEquals(JSValue.null_val));
    try std.testing.expect(JSValue.undefined_val.strictEquals(JSValue.undefined_val));
    try std.testing.expect(JSValue.true_val.strictEquals(JSValue.true_val));
    try std.testing.expect(JSValue.fromInt(42).strictEquals(JSValue.fromInt(42)));

    // Different values
    try std.testing.expect(!JSValue.null_val.strictEquals(JSValue.undefined_val));
    try std.testing.expect(!JSValue.true_val.strictEquals(JSValue.false_val));
    try std.testing.expect(!JSValue.fromInt(42).strictEquals(JSValue.fromInt(43)));
}

test "JSValue toNumber" {
    try std.testing.expectEqual(@as(?f64, 42.0), JSValue.fromInt(42).toNumber());
    try std.testing.expectEqual(@as(?f64, -123.0), JSValue.fromInt(-123).toNumber());
    try std.testing.expectEqual(@as(?f64, 0.0), JSValue.fromInt(0).toNumber());
    try std.testing.expectEqual(@as(?f64, null), JSValue.null_val.toNumber());
    try std.testing.expectEqual(@as(?f64, null), JSValue.undefined_val.toNumber());
}

test "JSValue pointer encoding" {
    var dummy: u64 = 0x12345678;
    const ptr_val = JSValue.fromPtr(&dummy);

    try std.testing.expect(ptr_val.isPtr());
    try std.testing.expect(!ptr_val.isInt());
    try std.testing.expect(!ptr_val.isSpecial());

    const recovered = ptr_val.toPtr(u64);
    try std.testing.expectEqual(&dummy, recovered);
}

test "JSValue format" {
    var buf: [64]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buf);
    JSValue.null_val.format("", .{}, &writer) catch unreachable;
    try std.testing.expectEqualStrings("null", writer.buffer[0..writer.end]);

    writer = .fixed(&buf);
    JSValue.fromInt(42).format("", .{}, &writer) catch unreachable;
    try std.testing.expectEqualStrings("42", writer.buffer[0..writer.end]);
}
