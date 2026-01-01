//! InternPool - Canonical storage for runtime constants
//!
//! Provides O(1) equality checking and deduplication for:
//! - Integers (small values inline, larger via storage)
//! - Floats (deduplicated by bit pattern)
//! - Strings (interned with O(1) comparison)
//! - Atoms (property names)
//!
//! Well-known values like undefined, null, true, false, 0, 1 have
//! pre-assigned indices that require no storage.

const std = @import("std");
const value = @import("value.zig");
const object = @import("object.zig");

/// Index into the InternPool
/// Well-known values have fixed indices that don't require storage
pub const Index = enum(u32) {
    // Well-known values (indices 0-15 reserved, no storage needed)
    undefined = 0,
    null_val = 1,
    true_val = 2,
    false_val = 3,
    zero = 4,
    one = 5,
    neg_one = 6,
    empty_string = 7,
    nan = 8,
    pos_inf = 9,
    neg_inf = 10,
    // Reserved for future well-known values
    _reserved_11 = 11,
    _reserved_12 = 12,
    _reserved_13 = 13,
    _reserved_14 = 14,
    _reserved_15 = 15,
    /// Dynamic indices
    _,

    /// First dynamically allocated index
    pub const FIRST_DYNAMIC: u32 = 16;

    /// Sentinel for "no value"
    pub const none: Index = @enumFromInt(std.math.maxInt(u32));

    pub fn isWellKnown(self: Index) bool {
        return @intFromEnum(self) < FIRST_DYNAMIC;
    }

    pub fn isNone(self: Index) bool {
        return self == none;
    }

    pub fn toInt(self: Index) u32 {
        return @intFromEnum(self);
    }

    pub fn fromInt(val: u32) Index {
        return @enumFromInt(val);
    }

    /// Convert to JSValue
    pub fn toValue(self: Index, pool: *const InternPool) value.JSValue {
        return pool.getValue(self);
    }
};

/// Item tag - determines interpretation of data field
pub const Tag = enum(u8) {
    /// Signed 32-bit integer stored directly in data field
    int32,
    /// Float64 - data is index into float storage
    float64,
    /// String - data is start index into string_bytes, next u32 is length
    string,
    /// Atom (property name) - data is atom value
    atom,
    /// Function bytecode reference - data is index into function storage
    function,
};

/// Single item in the pool
pub const Item = struct {
    tag: Tag,
    data: u32,
};

/// Canonical storage for runtime constants
pub const InternPool = struct {
    allocator: std.mem.Allocator,

    /// Items stored as structure-of-arrays
    tags: std.ArrayListUnmanaged(Tag),
    data: std.ArrayListUnmanaged(u32),

    /// Extra data for variable-length content
    extra: std.ArrayListUnmanaged(u32),

    /// String byte storage
    string_bytes: std.ArrayListUnmanaged(u8),

    /// Float64 storage (separate for alignment)
    floats: std.ArrayListUnmanaged(f64),

    /// String deduplication map: hash -> index
    string_map: std.StringHashMapUnmanaged(Index),

    /// Float deduplication map: bits -> index
    float_map: std.AutoHashMapUnmanaged(u64, Index),

    /// Integer deduplication for values outside i8 range
    int_map: std.AutoHashMapUnmanaged(i32, Index),

    pub fn init(allocator: std.mem.Allocator) InternPool {
        return .{
            .allocator = allocator,
            .tags = .empty,
            .data = .empty,
            .extra = .empty,
            .string_bytes = .empty,
            .floats = .empty,
            .string_map = .{},
            .float_map = .{},
            .int_map = .{},
        };
    }

    pub fn deinit(self: *InternPool) void {
        self.tags.deinit(self.allocator);
        self.data.deinit(self.allocator);
        self.extra.deinit(self.allocator);
        self.string_bytes.deinit(self.allocator);
        self.floats.deinit(self.allocator);
        self.string_map.deinit(self.allocator);
        self.float_map.deinit(self.allocator);
        self.int_map.deinit(self.allocator);
    }

    /// Intern an integer, returning canonical index
    pub fn internInt(self: *InternPool, val: i32) !Index {
        // Check for well-known values
        if (val == 0) return .zero;
        if (val == 1) return .one;
        if (val == -1) return .neg_one;

        // Check dedup map for larger values
        if (self.int_map.get(val)) |existing| {
            return existing;
        }

        // Allocate new entry
        const idx = try self.addItem(.int32, @bitCast(val));
        try self.int_map.put(self.allocator, val, idx);
        return idx;
    }

    /// Intern a float64, returning canonical index
    pub fn internFloat(self: *InternPool, val: f64) !Index {
        const bits: u64 = @bitCast(val);

        // Check for well-known values
        if (std.math.isNan(val)) return .nan;
        if (val == std.math.inf(f64)) return .pos_inf;
        if (val == -std.math.inf(f64)) return .neg_inf;
        if (bits == 0) return .zero; // +0.0

        // Check dedup map
        if (self.float_map.get(bits)) |existing| {
            return existing;
        }

        // Store float and create entry
        const float_idx = self.floats.items.len;
        try self.floats.append(self.allocator, val);

        const idx = try self.addItem(.float64, @intCast(float_idx));
        try self.float_map.put(self.allocator, bits, idx);
        return idx;
    }

    /// Intern a string, returning canonical index
    pub fn internString(self: *InternPool, str: []const u8) !Index {
        // Check for empty string
        if (str.len == 0) return .empty_string;

        // Check dedup map
        if (self.string_map.get(str)) |existing| {
            return existing;
        }

        // Store string bytes
        const start = self.string_bytes.items.len;
        try self.string_bytes.appendSlice(self.allocator, str);

        // Store length in extra (for future use in retrieval)
        _ = self.extra.items.len; // len_idx would be used for structured retrieval
        try self.extra.append(self.allocator, @intCast(str.len));

        // Create entry: data = start, extra[len_idx] = length
        const idx = try self.addItem(.string, @intCast(start));

        // We need to store the string for the hash map key
        // The key points into string_bytes which we own
        const stored_str = self.string_bytes.items[start..][0..str.len];
        try self.string_map.put(self.allocator, stored_str, idx);

        return idx;
    }

    /// Intern an atom (property name)
    pub fn internAtom(self: *InternPool, atom: object.Atom) !Index {
        return self.addItem(.atom, @intFromEnum(atom));
    }

    /// Get the tag for an index
    pub fn getTag(self: *const InternPool, idx: Index) Tag {
        if (idx.isWellKnown()) {
            return switch (idx) {
                .undefined, .null_val => .int32, // Special handling in getValue
                .true_val, .false_val => .int32,
                .zero, .one, .neg_one => .int32,
                .empty_string => .string,
                .nan, .pos_inf, .neg_inf => .float64,
                else => .int32,
            };
        }
        const i = idx.toInt() - Index.FIRST_DYNAMIC;
        return self.tags.items[i];
    }

    /// Get JSValue for an index
    pub fn getValue(self: *const InternPool, idx: Index) value.JSValue {
        // Handle well-known values
        if (idx.isWellKnown()) {
            return switch (idx) {
                .undefined => value.JSValue.undefined_val,
                .null_val => value.JSValue.null_val,
                .true_val => value.JSValue.true_val,
                .false_val => value.JSValue.false_val,
                .zero => value.JSValue.fromInt(0),
                .one => value.JSValue.fromInt(1),
                .neg_one => value.JSValue.fromInt(-1),
                .empty_string => value.JSValue.undefined_val, // Would need string table
                .nan => value.JSValue.nan_val,
                .pos_inf => value.JSValue.nan_val, // Floats need heap boxing
                .neg_inf => value.JSValue.nan_val, // Floats need heap boxing
                else => value.JSValue.undefined_val,
            };
        }

        const i = idx.toInt() - Index.FIRST_DYNAMIC;
        const tag = self.tags.items[i];
        const data = self.data.items[i];

        return switch (tag) {
            .int32 => value.JSValue.fromInt(@bitCast(data)),
            .float64 => value.JSValue.nan_val, // Floats need heap boxing for full conversion
            .string => value.JSValue.undefined_val, // Would need string conversion
            .atom => value.JSValue.fromInt(@intCast(data)), // Atom as int for now
            .function => value.JSValue.undefined_val, // Would need function handling
        };
    }

    /// Get integer value for an index
    pub fn getInt(self: *const InternPool, idx: Index) ?i32 {
        if (idx == .zero) return 0;
        if (idx == .one) return 1;
        if (idx == .neg_one) return -1;

        if (idx.isWellKnown()) return null;

        const i = idx.toInt() - Index.FIRST_DYNAMIC;
        if (self.tags.items[i] != .int32) return null;
        return @bitCast(self.data.items[i]);
    }

    /// Get float value for an index
    pub fn getFloat(self: *const InternPool, idx: Index) ?f64 {
        if (idx == .zero) return 0.0;
        if (idx == .nan) return std.math.nan(f64);
        if (idx == .pos_inf) return std.math.inf(f64);
        if (idx == .neg_inf) return -std.math.inf(f64);

        if (idx.isWellKnown()) return null;

        const i = idx.toInt() - Index.FIRST_DYNAMIC;
        if (self.tags.items[i] != .float64) return null;
        return self.floats.items[self.data.items[i]];
    }

    /// Get string value for an index
    pub fn getString(self: *const InternPool, idx: Index) ?[]const u8 {
        if (idx == .empty_string) return "";

        if (idx.isWellKnown()) return null;

        const i = idx.toInt() - Index.FIRST_DYNAMIC;
        if (self.tags.items[i] != .string) return null;

        const start = self.data.items[i];
        // Find the corresponding length in extra
        // For now, scan string_bytes to find null terminator or use stored length
        // This is a simplification - in practice we'd store length
        var end = start;
        while (end < self.string_bytes.items.len and self.string_bytes.items[end] != 0) {
            end += 1;
        }
        return self.string_bytes.items[start..end];
    }

    /// Add an item and return its index
    fn addItem(self: *InternPool, tag: Tag, data: u32) !Index {
        const local_idx = self.tags.items.len;
        try self.tags.append(self.allocator, tag);
        try self.data.append(self.allocator, data);
        return Index.fromInt(@intCast(local_idx + Index.FIRST_DYNAMIC));
    }

    /// Number of interned items (excluding well-known)
    pub fn count(self: *const InternPool) usize {
        return self.tags.items.len;
    }
};

test "InternPool well-known values" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // Well-known values should work without any interning
    try std.testing.expectEqual(value.JSValue.undefined_val, Index.undefined.toValue(&pool));
    try std.testing.expectEqual(value.JSValue.null_val, Index.null_val.toValue(&pool));
    try std.testing.expectEqual(value.JSValue.true_val, Index.true_val.toValue(&pool));
    try std.testing.expectEqual(value.JSValue.false_val, Index.false_val.toValue(&pool));
}

test "InternPool integer interning" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // Well-known integers
    try std.testing.expectEqual(Index.zero, try pool.internInt(0));
    try std.testing.expectEqual(Index.one, try pool.internInt(1));
    try std.testing.expectEqual(Index.neg_one, try pool.internInt(-1));

    // Other integers should be deduplicated
    const idx42_a = try pool.internInt(42);
    const idx42_b = try pool.internInt(42);
    try std.testing.expectEqual(idx42_a, idx42_b);

    // Different values should have different indices
    const idx100 = try pool.internInt(100);
    try std.testing.expect(idx42_a != idx100);

    // Verify values
    try std.testing.expectEqual(@as(?i32, 42), pool.getInt(idx42_a));
    try std.testing.expectEqual(@as(?i32, 100), pool.getInt(idx100));
}

test "InternPool float interning" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // Well-known floats
    try std.testing.expectEqual(Index.nan, try pool.internFloat(std.math.nan(f64)));
    try std.testing.expectEqual(Index.pos_inf, try pool.internFloat(std.math.inf(f64)));
    try std.testing.expectEqual(Index.neg_inf, try pool.internFloat(-std.math.inf(f64)));

    // Regular floats should be deduplicated
    const idx_pi_a = try pool.internFloat(3.14159);
    const idx_pi_b = try pool.internFloat(3.14159);
    try std.testing.expectEqual(idx_pi_a, idx_pi_b);

    // Verify value
    const retrieved = pool.getFloat(idx_pi_a);
    try std.testing.expect(retrieved != null);
    try std.testing.expectApproxEqRel(@as(f64, 3.14159), retrieved.?, 0.00001);
}

test "InternPool string interning" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // Empty string
    try std.testing.expectEqual(Index.empty_string, try pool.internString(""));

    // Regular strings should be deduplicated
    const idx_hello_a = try pool.internString("hello");
    const idx_hello_b = try pool.internString("hello");
    try std.testing.expectEqual(idx_hello_a, idx_hello_b);

    // Different strings should have different indices
    const idx_world = try pool.internString("world");
    try std.testing.expect(idx_hello_a != idx_world);
}

test "InternPool Index properties" {
    try std.testing.expect(Index.undefined.isWellKnown());
    try std.testing.expect(Index.zero.isWellKnown());
    try std.testing.expect(!Index.fromInt(100).isWellKnown());
    try std.testing.expect(Index.none.isNone());
}
