//! InternPool - Canonical storage for interned values
//!
//! Every type and value gets a unique index; identical types/values map to
//! the same index, enabling O(1) comparison. This is the foundation for
//! bytecode caching and type comparison optimization.
//!
//! Design inspired by Zig compiler's InternPool pattern:
//! - Compact indices instead of pointers
//! - Deduplication via hash maps
//! - Well-known values for common constants

const std = @import("std");

/// First index for dynamically interned values
pub const FIRST_DYNAMIC_INDEX: u32 = 16;

/// Index into the InternPool
/// Well-known values have fixed indices for fast comparison
pub const Index = enum(u32) {
    // Well-known values (no storage needed)
    null_val = 0,
    undefined_val = 1,
    true_val = 2,
    false_val = 3,
    empty_string = 4,
    zero_int = 5,
    one_int = 6,
    nan_val = 7,
    pos_inf = 8,
    neg_inf = 9,
    _,

    /// Check if this is a well-known value
    pub fn isWellKnown(self: Index) bool {
        return @intFromEnum(self) < FIRST_DYNAMIC_INDEX;
    }

    /// Get the raw index value
    pub fn toInt(self: Index) u32 {
        return @intFromEnum(self);
    }
};

/// Tag for interned items
pub const Tag = enum(u8) {
    // Special well-known values (no extra storage)
    well_known,

    // Integer values
    int_i32, // data is the i32 value directly
    int_i64, // data is index into extra_data (8 bytes)

    // Float values
    float64, // data is index into extra_data (8 bytes)

    // Strings
    string, // data is offset into string_bytes, extra has length

    // Future extensions for bytecode caching:
    // function_sig,
    // object_shape,
    // array_type,
};

/// Item stored in the InternPool
pub const Item = struct {
    tag: Tag,
    /// Payload data - interpretation depends on tag
    /// For int_i32: the value itself
    /// For float64/int_i64: index into extra_data
    /// For string: offset into string_bytes
    data: u32,
};

/// InternPool for canonical value storage
pub const InternPool = struct {
    allocator: std.mem.Allocator,

    /// Items storage
    items: std.ArrayListUnmanaged(Item),

    /// Extra data for values that don't fit in 4 bytes
    extra_data: std.ArrayListUnmanaged(u32),

    /// String bytes storage (all interned strings concatenated)
    string_bytes: std.ArrayListUnmanaged(u8),

    /// String lengths (parallel to string offsets)
    string_lengths: std.ArrayListUnmanaged(u32),

    // Deduplication maps
    string_map: std.StringHashMapUnmanaged(Index),
    float_map: std.AutoHashMapUnmanaged(u64, Index), // f64 bits -> index
    int_map: std.AutoHashMapUnmanaged(i32, Index), // i32 -> index

    /// Next index to allocate
    next_index: u32,

    pub fn init(allocator: std.mem.Allocator) InternPool {
        return .{
            .allocator = allocator,
            .items = .empty,
            .extra_data = .empty,
            .string_bytes = .empty,
            .string_lengths = .empty,
            .string_map = .{},
            .float_map = .{},
            .int_map = .{},
            .next_index = FIRST_DYNAMIC_INDEX,
        };
    }

    pub fn deinit(self: *InternPool) void {
        self.items.deinit(self.allocator);
        self.extra_data.deinit(self.allocator);
        self.string_bytes.deinit(self.allocator);
        self.string_lengths.deinit(self.allocator);
        self.string_map.deinit(self.allocator);
        self.float_map.deinit(self.allocator);
        self.int_map.deinit(self.allocator);
    }

    // ========================================================================
    // Well-known value accessors
    // ========================================================================

    pub fn getNullVal(_: *const InternPool) Index {
        return .null_val;
    }

    pub fn getUndefinedVal(_: *const InternPool) Index {
        return .undefined_val;
    }

    pub fn getTrueVal(_: *const InternPool) Index {
        return .true_val;
    }

    pub fn getFalseVal(_: *const InternPool) Index {
        return .false_val;
    }

    pub fn getEmptyString(_: *const InternPool) Index {
        return .empty_string;
    }

    pub fn getZeroInt(_: *const InternPool) Index {
        return .zero_int;
    }

    pub fn getOneInt(_: *const InternPool) Index {
        return .one_int;
    }

    pub fn getNaN(_: *const InternPool) Index {
        return .nan_val;
    }

    // ========================================================================
    // String interning
    // ========================================================================

    /// Intern a string, returning its canonical index
    pub fn internString(self: *InternPool, str: []const u8) !Index {
        // Check for empty string (well-known)
        if (str.len == 0) {
            return .empty_string;
        }

        // Check if already interned
        if (self.string_map.get(str)) |existing| {
            return existing;
        }

        // Allocate new index
        const idx = self.allocateIndex();

        // Store string bytes
        const offset: u32 = @intCast(self.string_bytes.items.len);
        try self.string_bytes.appendSlice(self.allocator, str);
        try self.string_lengths.append(self.allocator, @intCast(str.len));

        // Store item
        try self.items.append(self.allocator, .{
            .tag = .string,
            .data = offset,
        });

        // Add to dedup map (key points into string_bytes)
        const stored_str = self.string_bytes.items[offset..][0..str.len];
        try self.string_map.put(self.allocator, stored_str, idx);

        return idx;
    }

    /// Get string value from index
    pub fn getString(self: *const InternPool, idx: Index) ?[]const u8 {
        if (idx == .empty_string) {
            return "";
        }

        const i = @intFromEnum(idx);
        if (i < FIRST_DYNAMIC_INDEX or i >= self.next_index) {
            return null;
        }

        const item_idx = i - FIRST_DYNAMIC_INDEX;
        if (item_idx >= self.items.items.len) {
            return null;
        }

        const item = self.items.items[item_idx];
        if (item.tag != .string) {
            return null;
        }

        const offset = item.data;
        const len = self.string_lengths.items[item_idx];
        return self.string_bytes.items[offset..][0..len];
    }

    // ========================================================================
    // Integer interning
    // ========================================================================

    /// Intern an i32 value
    pub fn internInt(self: *InternPool, val: i32) !Index {
        // Check well-known values
        if (val == 0) return .zero_int;
        if (val == 1) return .one_int;

        // Check if already interned
        if (self.int_map.get(val)) |existing| {
            return existing;
        }

        // Allocate new index
        const idx = self.allocateIndex();

        // Store item (value fits directly in data field)
        try self.items.append(self.allocator, .{
            .tag = .int_i32,
            .data = @bitCast(val),
        });

        // Add to dedup map
        try self.int_map.put(self.allocator, val, idx);

        return idx;
    }

    /// Get i32 value from index
    pub fn getInt(self: *const InternPool, idx: Index) ?i32 {
        // Check well-known values
        if (idx == .zero_int) return 0;
        if (idx == .one_int) return 1;

        const i = @intFromEnum(idx);
        if (i < FIRST_DYNAMIC_INDEX or i >= self.next_index) {
            return null;
        }

        const item_idx = i - FIRST_DYNAMIC_INDEX;
        if (item_idx >= self.items.items.len) {
            return null;
        }

        const item = self.items.items[item_idx];
        if (item.tag != .int_i32) {
            return null;
        }

        return @bitCast(item.data);
    }

    // ========================================================================
    // Float interning
    // ========================================================================

    /// Intern an f64 value
    pub fn internFloat(self: *InternPool, val: f64) !Index {
        const bits: u64 = @bitCast(val);

        // Check well-known values
        if (std.math.isNan(val)) return .nan_val;
        if (std.math.isPositiveInf(val)) return .pos_inf;
        if (std.math.isNegativeInf(val)) return .neg_inf;

        // Check if already interned
        if (self.float_map.get(bits)) |existing| {
            return existing;
        }

        // Allocate new index
        const idx = self.allocateIndex();

        // Store in extra_data (8 bytes = 2 u32s)
        const extra_start: u32 = @intCast(self.extra_data.items.len);
        try self.extra_data.append(self.allocator, @truncate(bits));
        try self.extra_data.append(self.allocator, @truncate(bits >> 32));

        // Store item
        try self.items.append(self.allocator, .{
            .tag = .float64,
            .data = extra_start,
        });

        // Add to dedup map
        try self.float_map.put(self.allocator, bits, idx);

        return idx;
    }

    /// Get f64 value from index
    pub fn getFloat(self: *const InternPool, idx: Index) ?f64 {
        // Check well-known values
        if (idx == .nan_val) return std.math.nan(f64);
        if (idx == .pos_inf) return std.math.inf(f64);
        if (idx == .neg_inf) return -std.math.inf(f64);

        const i = @intFromEnum(idx);
        if (i < FIRST_DYNAMIC_INDEX or i >= self.next_index) {
            return null;
        }

        const item_idx = i - FIRST_DYNAMIC_INDEX;
        if (item_idx >= self.items.items.len) {
            return null;
        }

        const item = self.items.items[item_idx];
        if (item.tag != .float64) {
            return null;
        }

        const extra_start = item.data;
        const low: u64 = self.extra_data.items[extra_start];
        const high: u64 = self.extra_data.items[extra_start + 1];
        const bits = low | (high << 32);
        return @bitCast(bits);
    }

    // ========================================================================
    // Internal helpers
    // ========================================================================

    fn allocateIndex(self: *InternPool) Index {
        const idx: Index = @enumFromInt(self.next_index);
        self.next_index += 1;
        return idx;
    }

    /// Get the tag for an index
    pub fn getTag(self: *const InternPool, idx: Index) ?Tag {
        const i = @intFromEnum(idx);
        if (i < FIRST_DYNAMIC_INDEX) {
            return .well_known;
        }
        if (i >= self.next_index) {
            return null;
        }
        const item_idx = i - FIRST_DYNAMIC_INDEX;
        if (item_idx >= self.items.items.len) {
            return null;
        }
        return self.items.items[item_idx].tag;
    }

    /// Number of interned items (excluding well-known values)
    pub fn count(self: *const InternPool) u32 {
        return self.next_index - FIRST_DYNAMIC_INDEX;
    }

    // ========================================================================
    // AtomTable Bridge - Converts between InternPool indices and Atoms
    // ========================================================================

    /// Bridge to AtomTable for shared string storage
    /// Allows InternPool to serve as canonical string storage while
    /// AtomTable provides fast property name lookups
    pub const AtomBridge = struct {
        pool: *InternPool,
        atom_table: *@import("context.zig").AtomTable,

        /// Intern a string in the pool and get its Atom for property access
        /// Returns the Atom (from AtomTable) for the string
        pub fn internAsAtom(self: *AtomBridge, str: []const u8) !@import("object.zig").Atom {
            // First, intern in the pool for canonical storage
            _ = try self.pool.internString(str);
            // Then get/create the Atom for property operations
            return self.atom_table.intern(str);
        }

        /// Get the InternPool index for an Atom's string
        /// Useful when serializing bytecode with canonical indices
        pub fn atomToIndex(self: *AtomBridge, atom: @import("object.zig").Atom) !?Index {
            const name = self.atom_table.getName(atom) orelse return null;
            return try self.pool.internString(name);
        }

        /// Bulk intern all Atom strings into the pool
        /// Call this after parsing to populate the pool with all property names
        pub fn syncFromAtoms(self: *AtomBridge) !void {
            var it = self.atom_table.strings.iterator();
            while (it.next()) |entry| {
                _ = try self.pool.internString(entry.key_ptr.*);
            }
        }
    };

    /// Create a bridge to an AtomTable
    pub fn bridge(self: *InternPool, atom_table: *@import("context.zig").AtomTable) AtomBridge {
        return .{
            .pool = self,
            .atom_table = atom_table,
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "well-known values" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    try std.testing.expectEqual(Index.null_val, pool.getNullVal());
    try std.testing.expectEqual(Index.undefined_val, pool.getUndefinedVal());
    try std.testing.expectEqual(Index.true_val, pool.getTrueVal());
    try std.testing.expectEqual(Index.false_val, pool.getFalseVal());
    try std.testing.expectEqual(Index.empty_string, pool.getEmptyString());
}

test "string interning" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // Empty string should return well-known index
    const empty = try pool.internString("");
    try std.testing.expectEqual(Index.empty_string, empty);

    // Intern some strings
    const hello = try pool.internString("hello");
    const world = try pool.internString("world");
    const hello2 = try pool.internString("hello"); // duplicate

    // Same string should return same index
    try std.testing.expectEqual(hello, hello2);
    try std.testing.expect(hello != world);

    // Retrieve strings
    try std.testing.expectEqualStrings("hello", pool.getString(hello).?);
    try std.testing.expectEqualStrings("world", pool.getString(world).?);
    try std.testing.expectEqualStrings("", pool.getString(.empty_string).?);
}

test "int interning" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // Well-known values
    const zero = try pool.internInt(0);
    const one = try pool.internInt(1);
    try std.testing.expectEqual(Index.zero_int, zero);
    try std.testing.expectEqual(Index.one_int, one);

    // Regular values
    const val42 = try pool.internInt(42);
    const val42_dup = try pool.internInt(42);
    const val_neg = try pool.internInt(-100);

    try std.testing.expectEqual(val42, val42_dup);
    try std.testing.expect(val42 != val_neg);

    // Retrieve values
    try std.testing.expectEqual(@as(i32, 0), pool.getInt(.zero_int).?);
    try std.testing.expectEqual(@as(i32, 1), pool.getInt(.one_int).?);
    try std.testing.expectEqual(@as(i32, 42), pool.getInt(val42).?);
    try std.testing.expectEqual(@as(i32, -100), pool.getInt(val_neg).?);
}

test "float interning" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // Well-known values
    const nan = try pool.internFloat(std.math.nan(f64));
    const inf = try pool.internFloat(std.math.inf(f64));
    const neg_inf = try pool.internFloat(-std.math.inf(f64));
    try std.testing.expectEqual(Index.nan_val, nan);
    try std.testing.expectEqual(Index.pos_inf, inf);
    try std.testing.expectEqual(Index.neg_inf, neg_inf);

    // Regular values
    const pi = try pool.internFloat(3.14159);
    const pi_dup = try pool.internFloat(3.14159);
    const e = try pool.internFloat(2.71828);

    try std.testing.expectEqual(pi, pi_dup);
    try std.testing.expect(pi != e);

    // Retrieve values
    try std.testing.expect(std.math.isNan(pool.getFloat(.nan_val).?));
    try std.testing.expect(std.math.isPositiveInf(pool.getFloat(.pos_inf).?));
    try std.testing.expect(std.math.isNegativeInf(pool.getFloat(.neg_inf).?));
    try std.testing.expectApproxEqRel(@as(f64, 3.14159), pool.getFloat(pi).?, 0.00001);
}

test "mixed interning" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    // Intern various types
    const str = try pool.internString("test");
    const int = try pool.internInt(123);
    const float = try pool.internFloat(1.5);

    // All should have different indices
    try std.testing.expect(str != int);
    try std.testing.expect(int != float);
    try std.testing.expect(str != float);

    // Check tags
    try std.testing.expectEqual(Tag.string, pool.getTag(str).?);
    try std.testing.expectEqual(Tag.int_i32, pool.getTag(int).?);
    try std.testing.expectEqual(Tag.float64, pool.getTag(float).?);
    try std.testing.expectEqual(Tag.well_known, pool.getTag(.null_val).?);
}

test "AtomBridge integration" {
    const context = @import("context.zig");

    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    var atoms = context.AtomTable.init(std.testing.allocator);
    defer atoms.deinit();

    var bridge_inst = pool.bridge(&atoms);

    // Intern a string and get atom
    const atom1 = try bridge_inst.internAsAtom("myProperty");
    const atom2 = try bridge_inst.internAsAtom("myProperty");

    // Same atom returned
    try std.testing.expectEqual(atom1, atom2);

    // String is in the pool
    const idx = try bridge_inst.atomToIndex(atom1);
    try std.testing.expect(idx != null);

    // String can be retrieved from pool
    const str = pool.getString(idx.?);
    try std.testing.expect(str != null);
    try std.testing.expectEqualStrings("myProperty", str.?);
}

test "AtomBridge syncFromAtoms" {
    const context = @import("context.zig");

    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    var atoms = context.AtomTable.init(std.testing.allocator);
    defer atoms.deinit();

    // Add some atoms first
    _ = try atoms.intern("propA");
    _ = try atoms.intern("propB");
    _ = try atoms.intern("propC");

    // Pool should be empty initially
    try std.testing.expectEqual(@as(u32, 0), pool.count());

    // Sync atoms to pool
    var bridge_inst = pool.bridge(&atoms);
    try bridge_inst.syncFromAtoms();

    // Pool should now have all the strings
    try std.testing.expectEqual(@as(u32, 3), pool.count());
}
