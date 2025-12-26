//! SIMD-accelerated string operations with interning
//!
//! UTF-8 strings with ASCII optimization and hash caching.

const std = @import("std");
const heap = @import("heap.zig");

/// JavaScript string representation
pub const JSString = struct {
    header: heap.MemBlockHeader,
    flags: Flags,
    len: u32,
    hash: u64, // Cached XXHash64
    // Followed by UTF-8 data

    pub const Flags = packed struct {
        is_unique: bool = false, // Interned string
        is_ascii: bool = false, // All bytes < 128
        is_numeric: bool = false, // Valid array index
        _reserved: u5 = 0,
    };

    /// Get string data
    pub fn data(self: *JSString) []const u8 {
        const ptr: [*]const u8 = @ptrCast(@as([*]u8, @ptrCast(self)) + @sizeOf(JSString));
        return ptr[0..self.len];
    }
};

/// SIMD string comparison (256-bit AVX2 when available)
pub fn compareStrings(a: []const u8, b: []const u8) std.math.Order {
    if (a.len != b.len) return std.math.order(a.len, b.len);
    if (a.len == 0) return .eq;

    const Vec = @Vector(32, u8);
    var i: usize = 0;

    // SIMD comparison for 32-byte chunks
    while (i + 32 <= a.len) : (i += 32) {
        const va: Vec = a[i..][0..32].*;
        const vb: Vec = b[i..][0..32].*;
        if (@reduce(.Or, va != vb)) {
            return scalarCompare(a[i..], b[i..]);
        }
    }

    // Handle remainder
    return scalarCompare(a[i..], b[i..]);
}

fn scalarCompare(a: []const u8, b: []const u8) std.math.Order {
    const min_len = @min(a.len, b.len);
    for (a[0..min_len], b[0..min_len]) |ca, cb| {
        if (ca != cb) return std.math.order(ca, cb);
    }
    return std.math.order(a.len, b.len);
}

/// Fast string equality check
pub fn eqlStrings(a: []const u8, b: []const u8) bool {
    return compareStrings(a, b) == .eq;
}

/// XXHash64 for string hashing
pub fn hashString(s: []const u8) u64 {
    return std.hash.XxHash64.hash(0, s);
}

/// Check if string is a valid array index
pub fn isArrayIndex(s: []const u8) ?u32 {
    if (s.len == 0 or s.len > 10) return null;
    if (s[0] == '0' and s.len > 1) return null; // No leading zeros

    var result: u32 = 0;
    for (s) |c| {
        if (c < '0' or c > '9') return null;
        const digit: u32 = c - '0';
        // Check for overflow
        const max_before_digit = (std.math.maxInt(u32) - digit) / 10;
        if (result > max_before_digit) return null;
        result = result * 10 + digit;
    }
    return result;
}

/// Check if all bytes are ASCII
pub fn isAscii(s: []const u8) bool {
    const Vec = @Vector(32, u8);
    const high_bit: Vec = @splat(0x80);
    var i: usize = 0;

    while (i + 32 <= s.len) : (i += 32) {
        const chunk: Vec = s[i..][0..32].*;
        if (@reduce(.Or, chunk & high_bit != @as(Vec, @splat(0)))) {
            return false;
        }
    }

    // Check remainder
    for (s[i..]) |c| {
        if (c >= 0x80) return false;
    }
    return true;
}

/// String intern table (placeholder)
pub const StringTable = struct {
    allocator: std.mem.Allocator,
    // TODO: Implement hash table for interned strings

    pub fn init(allocator: std.mem.Allocator) StringTable {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *StringTable) void {
        _ = self;
    }

    pub fn intern(self: *StringTable, s: []const u8) !*JSString {
        _ = self;
        _ = s;
        // TODO: Implement interning
        return error.NotImplemented;
    }
};

test "SIMD string comparison" {
    try std.testing.expectEqual(std.math.Order.eq, compareStrings("hello", "hello"));
    try std.testing.expectEqual(std.math.Order.lt, compareStrings("abc", "abd"));
    try std.testing.expectEqual(std.math.Order.gt, compareStrings("xyz", "abc"));
    try std.testing.expectEqual(std.math.Order.lt, compareStrings("short", "longer string"));
}

test "string hashing" {
    const hash1 = hashString("hello");
    const hash2 = hashString("hello");
    const hash3 = hashString("world");

    try std.testing.expectEqual(hash1, hash2);
    try std.testing.expect(hash1 != hash3);
}

test "array index detection" {
    try std.testing.expectEqual(@as(?u32, 0), isArrayIndex("0"));
    try std.testing.expectEqual(@as(?u32, 123), isArrayIndex("123"));
    try std.testing.expectEqual(@as(?u32, null), isArrayIndex("01")); // Leading zero
    try std.testing.expectEqual(@as(?u32, null), isArrayIndex("abc"));
    try std.testing.expectEqual(@as(?u32, null), isArrayIndex("")); // Empty
}

test "ASCII detection" {
    try std.testing.expect(isAscii("Hello, World!"));
    try std.testing.expect(!isAscii("Hello, 世界!"));
    try std.testing.expect(isAscii(""));
}
