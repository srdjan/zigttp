//! SIMD-accelerated string operations with interning and rope support
//!
//! UTF-8 strings with ASCII optimization, hash caching, and lazy rope
//! evaluation for O(1) string concatenation.
//!
//! The rope data structure allows efficient string building:
//! - Concatenation: O(1) - creates a rope node instead of copying
//! - Flattening: O(n) - only when string content is accessed
//! - This eliminates the O(n^2) behavior of naive string building

const std = @import("std");
const heap = @import("heap.zig");
const arena_mod = @import("arena.zig");

/// JavaScript string representation
/// Uses extern struct for predictable C-compatible memory layout
pub const JSString = extern struct {
    header: heap.MemBlockHeader,
    flags: Flags,
    len: u32,
    hash: u64, // Cached XXHash64
    // Followed by UTF-8 data

    pub const Flags = packed struct {
        is_unique: bool = false, // Interned string
        is_ascii: bool = false, // All bytes < 128
        is_numeric: bool = false, // Valid array index
        hash_computed: bool = false,
        _reserved: u4 = 0,
    };

    /// Get string data (const version)
    pub fn data(self: *const JSString) []const u8 {
        const base: [*]const u8 = @ptrCast(self);
        return (base + @sizeOf(JSString))[0..self.len];
    }

    /// Get mutable string data
    pub fn dataMut(self: *JSString) []u8 {
        const base: [*]u8 = @ptrCast(self);
        return (base + @sizeOf(JSString))[0..self.len];
    }

    /// Get string length in bytes
    pub fn length(self: *const JSString) u32 {
        return self.len;
    }

    /// Get cached hash (compute on first use)
    pub fn getHash(self: *JSString) u64 {
        if (!self.flags.hash_computed) {
            self.hash = hashString(self.data());
            self.flags.hash_computed = true;
        }
        return self.hash;
    }

    pub fn getHashConst(self: *const JSString) u64 {
        return @constCast(self).getHash();
    }

    /// Get string length in UTF-8 codepoints
    pub fn codepointLength(self: *const JSString) u32 {
        if (self.flags.is_ascii) {
            return self.len;
        }
        return countCodepoints(self.data());
    }

    /// Check if string is empty
    pub fn isEmpty(self: *const JSString) bool {
        return self.len == 0;
    }

    /// Compare with another string
    pub fn compare(self: *const JSString, other: *const JSString) std.math.Order {
        // Fast path: same hash means likely equal
        const self_hash = self.getHashConst();
        const other_hash = other.getHashConst();
        if (self_hash == other_hash and self.len == other.len) {
            if (self == other) return .eq; // Same pointer
            if (eqlStrings(self.data(), other.data())) return .eq;
        }
        return compareStrings(self.data(), other.data());
    }

    /// Check equality with another string
    pub fn eql(self: *const JSString, other: *const JSString) bool {
        if (self == other) return true; // Same pointer (interned)
        const self_hash = self.getHashConst();
        const other_hash = other.getHashConst();
        if (self_hash != other_hash) return false;
        if (self.len != other.len) return false;
        return eqlStrings(self.data(), other.data());
    }

    /// Check equality with raw bytes
    pub fn eqlBytes(self: *const JSString, bytes: []const u8) bool {
        if (self.len != bytes.len) return false;
        return eqlStrings(self.data(), bytes);
    }

    /// Get character at byte index (for ASCII strings)
    pub fn charAt(self: *const JSString, index: u32) ?u8 {
        if (index >= self.len) return null;
        return self.data()[index];
    }

    /// Check if string starts with prefix
    pub fn startsWith(self: *const JSString, prefix: []const u8) bool {
        if (prefix.len > self.len) return false;
        return eqlStrings(self.data()[0..prefix.len], prefix);
    }

    /// Check if string ends with suffix
    pub fn endsWith(self: *const JSString, suffix: []const u8) bool {
        if (suffix.len > self.len) return false;
        const start = self.len - @as(u32, @intCast(suffix.len));
        return eqlStrings(self.data()[start..], suffix);
    }

    /// Find first occurrence of substring
    pub fn indexOf(self: *const JSString, needle: []const u8) ?u32 {
        if (needle.len == 0) return 0;
        if (needle.len > self.len) return null;

        const haystack = self.data();
        // Use SIMD for longer searches
        if (needle.len >= 4 and haystack.len >= 32) {
            return simdIndexOf(haystack, needle);
        }

        // Scalar fallback
        return scalarIndexOf(haystack, needle);
    }

    /// Find last occurrence of substring
    pub fn lastIndexOf(self: *const JSString, needle: []const u8) ?u32 {
        if (needle.len == 0) return self.len;
        if (needle.len > self.len) return null;

        const haystack = self.data();
        var i: usize = haystack.len - needle.len + 1;
        while (i > 0) {
            i -= 1;
            if (eqlStrings(haystack[i..][0..needle.len], needle)) {
                return @intCast(i);
            }
        }
        return null;
    }
};

// ============================================================================
// Rope Data Structure for O(1) String Concatenation
// ============================================================================

/// Rope node for efficient string building
/// Allows O(1) concatenation by creating tree nodes instead of copying data.
/// Flattening to a flat string is deferred until the content is actually needed.
pub const RopeNode = struct {
    header: heap.MemBlockHeader,
    /// Total length of all strings in this subtree
    total_len: u32,
    /// Depth of the rope tree (for balancing heuristics)
    depth: u16,
    /// Flags for the rope
    flags: RopeFlags,
    /// Union discriminator
    kind: RopeKind,
    /// Payload depends on kind
    payload: RopePayload,

    pub const RopeFlags = packed struct {
        is_ascii: bool = false,
        is_flattened: bool = false, // Has been flattened to a JSString
        _reserved: u6 = 0,
    };

    pub const RopeKind = enum(u8) {
        leaf = 0, // Points to a JSString
        concat = 1, // Points to two child RopeNodes
    };

    pub const RopePayload = extern union {
        /// For leaf nodes: pointer to the flat string
        leaf: *JSString,
        /// For concat nodes: left and right children
        concat: extern struct {
            left: *RopeNode,
            right: *RopeNode,
        },
    };

    /// Get the total length of the rope
    pub inline fn length(self: *const RopeNode) u32 {
        return self.total_len;
    }

    /// Check if the rope is empty
    pub inline fn isEmpty(self: *const RopeNode) bool {
        return self.total_len == 0;
    }

    /// Check if all content is ASCII
    pub inline fn isAscii(self: *const RopeNode) bool {
        return self.flags.is_ascii;
    }

    /// Get the underlying flat string if this is a leaf node
    pub fn asLeaf(self: *const RopeNode) ?*JSString {
        if (self.kind == .leaf) {
            return self.payload.leaf;
        }
        return null;
    }

    /// Flatten the rope to a contiguous string
    /// This is the only operation that requires O(n) work
    pub fn flatten(self: *RopeNode, allocator: std.mem.Allocator) !*JSString {
        // Leaf nodes are already flat
        if (self.kind == .leaf) {
            return self.payload.leaf;
        }

        // Allocate buffer for the flattened string
        const total_size = @sizeOf(JSString) + self.total_len;
        const mem = try allocator.alignedAlloc(u8, std.mem.Alignment.of(JSString), total_size);

        const str: *JSString = @ptrCast(@alignCast(mem.ptr));
        str.* = .{
            .header = heap.MemBlockHeader.init(.string, total_size),
            .flags = .{
                .is_unique = false,
                .is_ascii = self.flags.is_ascii,
                .is_numeric = false,
                .hash_computed = false,
            },
            .len = self.total_len,
            .hash = 0,
        };

        // Copy all leaf data into the buffer
        var offset: usize = 0;
        self.flattenInto(str.dataMut(), &offset);

        return str;
    }

    /// Flatten the rope using arena allocation
    pub fn flattenWithArena(self: *RopeNode, arena: *arena_mod.Arena) ?*JSString {
        // Leaf nodes are already flat
        if (self.kind == .leaf) {
            return self.payload.leaf;
        }

        // Allocate buffer for the flattened string
        const total_size = @sizeOf(JSString) + self.total_len;
        const mem = arena.alloc(total_size) orelse return null;

        const str: *JSString = @ptrCast(@alignCast(mem));
        str.* = .{
            .header = heap.MemBlockHeader.init(.string, total_size),
            .flags = .{
                .is_unique = false,
                .is_ascii = self.flags.is_ascii,
                .is_numeric = false,
                .hash_computed = false,
            },
            .len = self.total_len,
            .hash = 0,
        };

        // Copy all leaf data into the buffer
        var offset: usize = 0;
        self.flattenInto(str.dataMut(), &offset);

        return str;
    }

    /// Internal: recursively copy leaf data into a buffer
    fn flattenInto(self: *const RopeNode, buf: []u8, offset: *usize) void {
        switch (self.kind) {
            .leaf => {
                const data = self.payload.leaf.data();
                @memcpy(buf[offset.*..][0..data.len], data);
                offset.* += data.len;
            },
            .concat => {
                self.payload.concat.left.flattenInto(buf, offset);
                self.payload.concat.right.flattenInto(buf, offset);
            },
        }
    }

    /// Compare rope content with a byte slice (flattens lazily if needed)
    pub fn eqlBytes(self: *const RopeNode, bytes: []const u8) bool {
        if (self.total_len != bytes.len) return false;

        // For leaf nodes, compare directly
        if (self.kind == .leaf) {
            return eqlStrings(self.payload.leaf.data(), bytes);
        }

        // For concat nodes, we need to compare piece by piece
        var offset: usize = 0;
        return self.eqlBytesRecursive(bytes, &offset);
    }

    fn eqlBytesRecursive(self: *const RopeNode, bytes: []const u8, offset: *usize) bool {
        switch (self.kind) {
            .leaf => {
                const data = self.payload.leaf.data();
                if (!eqlStrings(data, bytes[offset.*..][0..data.len])) {
                    return false;
                }
                offset.* += data.len;
                return true;
            },
            .concat => {
                if (!self.payload.concat.left.eqlBytesRecursive(bytes, offset)) {
                    return false;
                }
                return self.payload.concat.right.eqlBytesRecursive(bytes, offset);
            },
        }
    }

    /// Get the hash of the rope content
    /// Note: This may require flattening for concat nodes
    pub fn getHash(self: *RopeNode, allocator: std.mem.Allocator) !u64 {
        if (self.kind == .leaf) {
            return self.payload.leaf.getHash();
        }

        // For concat nodes, we need to compute hash over all content
        // This could be optimized with incremental hashing, but for now flatten
        const flat = try self.flatten(allocator);
        defer {
            const total_size = @sizeOf(JSString) + flat.len;
            const ptr: [*]align(@alignOf(JSString)) u8 = @ptrCast(@alignCast(flat));
            allocator.free(ptr[0..total_size]);
        }
        return flat.getHash();
    }
};

/// Create a leaf rope node from an existing JSString
pub fn createRopeLeaf(allocator: std.mem.Allocator, str: *JSString) !*RopeNode {
    const node = try allocator.create(RopeNode);
    node.* = .{
        .header = heap.MemBlockHeader.init(.rope, @sizeOf(RopeNode)), // Reuse string tag
        .total_len = str.len,
        .depth = 0,
        .flags = .{ .is_ascii = str.flags.is_ascii },
        .kind = .leaf,
        .payload = .{ .leaf = str },
    };
    return node;
}

/// Create a leaf rope node using arena allocation
pub fn createRopeLeafWithArena(arena: *arena_mod.Arena, str: *JSString) ?*RopeNode {
    const node = arena.create(RopeNode) orelse return null;
    node.* = .{
        .header = heap.MemBlockHeader.init(.rope, @sizeOf(RopeNode)),
        .total_len = str.len,
        .depth = 0,
        .flags = .{ .is_ascii = str.flags.is_ascii },
        .kind = .leaf,
        .payload = .{ .leaf = str },
    };
    return node;
}

/// Concatenate two rope nodes - O(1) operation!
/// This is the key optimization: instead of copying, we create a new concat node
pub fn concatRopes(allocator: std.mem.Allocator, left: *RopeNode, right: *RopeNode) !*RopeNode {
    // Handle empty cases
    if (left.total_len == 0) return right;
    if (right.total_len == 0) return left;

    const node = try allocator.create(RopeNode);
    node.* = .{
        .header = heap.MemBlockHeader.init(.rope, @sizeOf(RopeNode)),
        .total_len = left.total_len + right.total_len,
        .depth = @max(left.depth, right.depth) + 1,
        .flags = .{ .is_ascii = left.flags.is_ascii and right.flags.is_ascii },
        .kind = .concat,
        .payload = .{ .concat = .{ .left = left, .right = right } },
    };

    // Rebalance if tree is getting too deep (prevents pathological cases)
    // Threshold of 32 is reasonable - 2^32 leaves would be needed to hit this
    // with perfectly balanced tree
    if (node.depth > 32) {
        // For now, just flatten when too deep
        // A more sophisticated implementation would use AVL-like rotations
        const flat = try node.flatten(allocator);
        allocator.destroy(node);
        return try createRopeLeaf(allocator, flat);
    }

    return node;
}

/// Concatenate two rope nodes using arena allocation - O(1) operation!
pub fn concatRopesWithArena(arena: *arena_mod.Arena, left: *RopeNode, right: *RopeNode) ?*RopeNode {
    // Handle empty cases
    if (left.total_len == 0) return right;
    if (right.total_len == 0) return left;

    const node = arena.create(RopeNode) orelse return null;
    node.* = .{
        .header = heap.MemBlockHeader.init(.rope, @sizeOf(RopeNode)),
        .total_len = left.total_len + right.total_len,
        .depth = @max(left.depth, right.depth) + 1,
        .flags = .{ .is_ascii = left.flags.is_ascii and right.flags.is_ascii },
        .kind = .concat,
        .payload = .{ .concat = .{ .left = left, .right = right } },
    };

    // For arena allocation, we don't rebalance since memory will be freed in bulk
    return node;
}

/// Concatenate a rope with a JSString - O(1) operation
pub fn concatRopeString(allocator: std.mem.Allocator, rope: *RopeNode, str: *JSString) !*RopeNode {
    const right = try createRopeLeaf(allocator, str);
    return try concatRopes(allocator, rope, right);
}

/// Concatenate a rope with a JSString using arena allocation
pub fn concatRopeStringWithArena(arena: *arena_mod.Arena, rope: *RopeNode, str: *JSString) ?*RopeNode {
    const right = createRopeLeafWithArena(arena, str) orelse return null;
    return concatRopesWithArena(arena, rope, right);
}

/// Concatenate two JSStrings into a rope - O(1) operation
pub fn createRopeFromStrings(allocator: std.mem.Allocator, a: *JSString, b: *JSString) !*RopeNode {
    const left = try createRopeLeaf(allocator, a);
    const right = try createRopeLeaf(allocator, b);
    return try concatRopes(allocator, left, right);
}

/// Concatenate two JSStrings into a rope using arena allocation
pub fn createRopeFromStringsWithArena(arena: *arena_mod.Arena, a: *JSString, b: *JSString) ?*RopeNode {
    const left = createRopeLeafWithArena(arena, a) orelse return null;
    const right = createRopeLeafWithArena(arena, b) orelse return null;
    return concatRopesWithArena(arena, left, right);
}

/// Free a rope node and all its children (but not the leaf JSStrings)
pub fn freeRope(allocator: std.mem.Allocator, node: *RopeNode) void {
    if (node.kind == .concat) {
        freeRope(allocator, node.payload.concat.left);
        freeRope(allocator, node.payload.concat.right);
    }
    allocator.destroy(node);
}

// ============================================================================
// SliceString - Zero-Copy String Slices
// ============================================================================

/// SliceString - a zero-copy view into a parent string
/// Instead of allocating and copying data, this references the parent string
/// with an offset and length. The parent is kept alive via GC tracing.
///
/// Use for substrings >= 16 bytes where the copy overhead exceeds the
/// indirection cost. For smaller slices, direct copy is more efficient.
pub const SliceString = extern struct {
    header: heap.MemBlockHeader, // tag = .string_slice
    flags: JSString.Flags, // Inherited from parent (especially is_ascii)
    len: u32, // Slice length in bytes
    hash: u64, // Computed on demand (0 = not computed)
    parent: *JSString, // Keeps parent alive via GC
    offset: u32, // Byte offset into parent's data
    _pad: u32 = 0, // Padding for alignment

    /// Minimum slice size to use SliceString instead of copying.
    /// SliceString header is ~40 bytes, so for tiny strings copying is cheaper.
    pub const MIN_SLICE_LEN: u32 = 16;

    /// Get the slice data (zero-copy view into parent)
    pub inline fn data(self: *const SliceString) []const u8 {
        return self.parent.data()[self.offset..][0..self.len];
    }

    /// Get string length in bytes
    pub inline fn length(self: *const SliceString) u32 {
        return self.len;
    }

    /// Check if string is empty
    pub inline fn isEmpty(self: *const SliceString) bool {
        return self.len == 0;
    }

    /// Get cached hash (compute on first use)
    pub fn getHash(self: *SliceString) u64 {
        if (self.hash == 0) {
            self.hash = hashString(self.data());
        }
        return self.hash;
    }

    /// Get hash (const version, may compute)
    pub fn getHashConst(self: *const SliceString) u64 {
        return @constCast(self).getHash();
    }

    /// Flatten slice to a new flat JSString (for operations that need mutable data)
    pub fn flatten(self: *const SliceString, allocator: std.mem.Allocator) !*JSString {
        return try createString(allocator, self.data());
    }

    /// Flatten using arena allocation
    pub fn flattenWithArena(self: *const SliceString, arena: *arena_mod.Arena) ?*JSString {
        return createStringWithArena(arena, self.data());
    }

    /// Check equality with a byte slice
    pub fn eqlBytes(self: *const SliceString, bytes: []const u8) bool {
        if (self.len != bytes.len) return false;
        return eqlStrings(self.data(), bytes);
    }

    /// Check if string starts with prefix
    pub fn startsWith(self: *const SliceString, prefix: []const u8) bool {
        if (prefix.len > self.len) return false;
        return eqlStrings(self.data()[0..prefix.len], prefix);
    }

    /// Check if string ends with suffix
    pub fn endsWith(self: *const SliceString, suffix: []const u8) bool {
        if (suffix.len > self.len) return false;
        const start = self.len - @as(u32, @intCast(suffix.len));
        return eqlStrings(self.data()[start..], suffix);
    }
};

/// Create a SliceString referencing a portion of a parent JSString.
/// For slices < MIN_SLICE_LEN bytes, prefer createString() for a direct copy.
pub fn createSlice(allocator: std.mem.Allocator, parent: *JSString, offset: u32, len: u32) !*SliceString {
    const slice = try allocator.create(SliceString);
    slice.* = .{
        .header = heap.MemBlockHeader.init(.string_slice, @sizeOf(SliceString)),
        .flags = .{
            .is_unique = false,
            .is_ascii = parent.flags.is_ascii, // Inherit from parent
            .is_numeric = false, // Slices are rarely valid indices
            .hash_computed = false,
        },
        .len = len,
        .hash = 0,
        .parent = parent,
        .offset = offset,
    };
    return slice;
}

/// Create a SliceString using arena allocation
pub fn createSliceWithArena(arena: *arena_mod.Arena, parent: *JSString, offset: u32, len: u32) ?*SliceString {
    const slice = arena.create(SliceString) orelse return null;
    slice.* = .{
        .header = heap.MemBlockHeader.init(.string_slice, @sizeOf(SliceString)),
        .flags = .{
            .is_unique = false,
            .is_ascii = parent.flags.is_ascii,
            .is_numeric = false,
            .hash_computed = false,
        },
        .len = len,
        .hash = 0,
        .parent = parent,
        .offset = offset,
    };
    return slice;
}

/// Free a SliceString (does not free the parent)
pub fn freeSlice(allocator: std.mem.Allocator, slice: *SliceString) void {
    allocator.destroy(slice);
}

// ============================================================================
// UTF-8 Utilities
// ============================================================================

/// Count UTF-8 codepoints in a string
pub fn countCodepoints(s: []const u8) u32 {
    var count: u32 = 0;
    var i: usize = 0;
    while (i < s.len) {
        // Count non-continuation bytes (those not starting with 10xxxxxx)
        if ((s[i] & 0xC0) != 0x80) {
            count += 1;
        }
        i += 1;
    }
    return count;
}

/// Get byte length of UTF-8 codepoint starting at given byte
pub fn codepointByteLength(first_byte: u8) u3 {
    if (first_byte < 0x80) return 1; // ASCII
    if (first_byte < 0xC0) return 1; // Invalid, treat as 1
    if (first_byte < 0xE0) return 2; // 2-byte
    if (first_byte < 0xF0) return 3; // 3-byte
    return 4; // 4-byte
}

/// Decode a UTF-8 codepoint at the given position
pub fn decodeCodepoint(s: []const u8) ?struct { codepoint: u21, len: u3 } {
    if (s.len == 0) return null;

    const first = s[0];
    if (first < 0x80) {
        return .{ .codepoint = first, .len = 1 };
    }

    const len = codepointByteLength(first);
    if (s.len < len) return null;

    var cp: u21 = switch (len) {
        2 => @as(u21, first & 0x1F) << 6,
        3 => @as(u21, first & 0x0F) << 12,
        4 => @as(u21, first & 0x07) << 18,
        else => return null,
    };

    for (1..len) |i| {
        if ((s[i] & 0xC0) != 0x80) return null;
        const shift: u5 = @intCast((len - 1 - i) * 6);
        cp |= @as(u21, s[i] & 0x3F) << shift;
    }

    return .{ .codepoint = cp, .len = len };
}

// ============================================================================
// SIMD String Search
// ============================================================================

/// SIMD-accelerated substring search
fn simdIndexOf(haystack: []const u8, needle: []const u8) ?u32 {
    if (needle.len == 0) return 0;
    if (needle.len > haystack.len) return null;

    const Vec = @Vector(32, u8);
    const first_char: Vec = @splat(needle[0]);

    var i: usize = 0;
    const search_end = haystack.len - needle.len + 1;

    // SIMD search for first character
    while (i + 32 <= search_end) : (i += 32) {
        const chunk: Vec = haystack[i..][0..32].*;
        const matches = chunk == first_char;
        const match_bytes: @Vector(32, u8) = @intFromBool(matches);
        const match_array: [32]u8 = @bitCast(match_bytes);

        // Check if any matches
        if (@reduce(.Or, matches)) {
            // Found potential match, verify full needle
            var j: usize = 0;
            while (j < 32) : (j += 1) {
                if (match_array[j] != 0) {
                    const pos = i + j;
                    if (pos + needle.len <= haystack.len) {
                        if (eqlStrings(haystack[pos..][0..needle.len], needle)) {
                            return @intCast(pos);
                        }
                    }
                }
            }
        }
    }

    // Scalar fallback for remainder
    return scalarIndexOf(haystack[i..], needle);
}

/// Scalar substring search
fn scalarIndexOf(haystack: []const u8, needle: []const u8) ?u32 {
    if (needle.len == 0) return 0;
    if (needle.len > haystack.len) return null;

    const first = needle[0];
    var i: usize = 0;
    const search_end = haystack.len - needle.len + 1;

    while (i < search_end) : (i += 1) {
        if (haystack[i] == first) {
            if (eqlStrings(haystack[i..][0..needle.len], needle)) {
                return @intCast(i);
            }
        }
    }
    return null;
}

// ============================================================================
// SIMD String Comparison
// ============================================================================

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

/// Fast string equality check (SIMD-accelerated for long strings)
pub fn eqlStrings(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    if (a.len == 0) return true;
    if (a.ptr == b.ptr) return true; // Same pointer

    const Vec = @Vector(32, u8);
    var i: usize = 0;

    // SIMD equality for 32-byte chunks
    while (i + 32 <= a.len) : (i += 32) {
        const va: Vec = a[i..][0..32].*;
        const vb: Vec = b[i..][0..32].*;
        if (@reduce(.Or, va != vb)) return false;
    }

    // Scalar fallback for remainder
    return std.mem.eql(u8, a[i..], b[i..]);
}

/// SIMD-accelerated single-byte search (memchr equivalent)
/// Returns index of first occurrence of needle in haystack, or null
pub fn findByte(haystack: []const u8, needle: u8) ?usize {
    const Vec = @Vector(32, u8);
    const needle_vec: Vec = @splat(needle);
    var i: usize = 0;

    // SIMD search for 32-byte chunks
    while (i + 32 <= haystack.len) : (i += 32) {
        const chunk: Vec = haystack[i..][0..32].*;
        const matches = chunk == needle_vec;
        if (@reduce(.Or, matches)) {
            // Found match, find exact position
            const match_bytes: [32]u8 = @bitCast(@as(@Vector(32, u8), @intFromBool(matches)));
            for (match_bytes, 0..) |m, j| {
                if (m != 0) return i + j;
            }
        }
    }

    // Scalar fallback for remainder
    for (haystack[i..], i..) |c, idx| {
        if (c == needle) return idx;
    }
    return null;
}

/// SIMD-accelerated count of byte occurrences
pub fn countByte(haystack: []const u8, needle: u8) usize {
    const Vec = @Vector(32, u8);
    const needle_vec: Vec = @splat(needle);
    var count: usize = 0;
    var i: usize = 0;

    // SIMD count for 32-byte chunks
    while (i + 32 <= haystack.len) : (i += 32) {
        const chunk: Vec = haystack[i..][0..32].*;
        const matches = chunk == needle_vec;
        count += @popCount(@as(u32, @bitCast(@as(@Vector(32, u1), @intFromBool(matches)))));
    }

    // Scalar fallback for remainder
    for (haystack[i..]) |c| {
        if (c == needle) count += 1;
    }
    return count;
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

// ============================================================================
// String Interning
// ============================================================================

/// String intern table for unique strings
pub const StringTable = struct {
    /// Hash map from hash -> list of strings with that hash
    buckets: std.AutoHashMap(u64, StringList),
    /// All allocated strings (for cleanup)
    strings: std.ArrayList(*JSString),
    allocator: std.mem.Allocator,
    /// Statistics
    intern_count: usize = 0,
    hit_count: usize = 0,

    const StringList = std.ArrayList(*JSString);

    pub fn init(allocator: std.mem.Allocator) StringTable {
        return .{
            .buckets = std.AutoHashMap(u64, StringList).init(allocator),
            .strings = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *StringTable) void {
        // Free all bucket lists
        var it = self.buckets.valueIterator();
        while (it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.buckets.deinit();

        // Free all strings
        for (self.strings.items) |str| {
            const total_size = @sizeOf(JSString) + str.len;
            const ptr: [*]align(@alignOf(JSString)) u8 = @ptrCast(@alignCast(str));
            self.allocator.free(ptr[0..total_size]);
        }
        self.strings.deinit(self.allocator);
    }

    /// Intern a string (get or create unique instance)
    pub fn intern(self: *StringTable, s: []const u8) !*JSString {
        const hash = hashString(s);

        // Check if already interned
        if (self.buckets.getPtr(hash)) |list| {
            for (list.items) |str| {
                if (str.len == s.len and eqlStrings(str.data(), s)) {
                    self.hit_count += 1;
                    return str;
                }
            }
            // Hash collision - add to existing bucket
            const new_str = try self.createString(s, hash);
            try list.append(self.allocator, new_str);
            return new_str;
        }

        // Create new bucket
        const new_str = try self.createString(s, hash);
        var list = StringList.empty;
        try list.append(self.allocator, new_str);
        try self.buckets.put(hash, list);
        return new_str;
    }

    /// Create a new JSString
    fn createString(self: *StringTable, s: []const u8, hash: u64) !*JSString {
        const total_size = @sizeOf(JSString) + s.len;
        const mem = try self.allocator.alignedAlloc(u8, std.mem.Alignment.of(JSString), total_size);

        const str: *JSString = @ptrCast(@alignCast(mem.ptr));
        str.* = .{
            .header = heap.MemBlockHeader.init(.string, total_size),
            .flags = .{
                .is_unique = true,
                .is_ascii = isAscii(s),
                .is_numeric = isArrayIndex(s) != null,
                .hash_computed = true,
            },
            .len = @intCast(s.len),
            .hash = hash,
        };

        // Copy string data after header
        @memcpy(str.dataMut(), s);

        try self.strings.append(self.allocator, str);
        self.intern_count += 1;
        return str;
    }

    /// Get interning statistics
    pub fn getStats(self: *const StringTable) struct { interned: usize, hits: usize } {
        return .{ .interned = self.intern_count, .hits = self.hit_count };
    }
};

/// Create a non-interned string (caller owns memory)
pub fn createString(allocator: std.mem.Allocator, s: []const u8) !*JSString {
    const total_size = @sizeOf(JSString) + s.len;
    const mem = try allocator.alignedAlloc(u8, std.mem.Alignment.of(JSString), total_size);

    const str: *JSString = @ptrCast(@alignCast(mem.ptr));
    str.* = .{
        .header = heap.MemBlockHeader.init(.string, total_size),
        .flags = .{
            .is_unique = false,
            .is_ascii = isAscii(s),
            .is_numeric = isArrayIndex(s) != null,
            .hash_computed = false,
        },
        .len = @intCast(s.len),
        .hash = 0,
    };

    @memcpy(str.dataMut(), s);
    return str;
}

/// Free a non-interned string
pub fn freeString(allocator: std.mem.Allocator, str: *JSString) void {
    const total_size = @sizeOf(JSString) + str.len;
    const ptr: [*]align(@alignOf(JSString)) u8 = @ptrCast(@alignCast(str));
    allocator.free(ptr[0..total_size]);
}

/// Create a string using arena allocation (ephemeral, dies at request end)
pub fn createStringWithArena(arena: *arena_mod.Arena, s: []const u8) ?*JSString {
    const total_size = @sizeOf(JSString) + s.len;
    const mem = arena.alloc(total_size) orelse return null;

    const str: *JSString = @ptrCast(@alignCast(mem));
    str.* = .{
        .header = heap.MemBlockHeader.init(.string, total_size),
        .flags = .{
            .is_unique = false,
            .is_ascii = isAscii(s),
            .is_numeric = isArrayIndex(s) != null,
            .hash_computed = false,
        },
        .len = @intCast(s.len),
        .hash = 0,
    };

    @memcpy(str.dataMut(), s);
    return str;
}

/// Concatenate two strings using arena allocation
pub fn concatStringsWithArena(arena: *arena_mod.Arena, a: *const JSString, b: *const JSString) ?*JSString {
    const total_len = a.len + b.len;
    const total_size = @sizeOf(JSString) + total_len;
    const mem = arena.alloc(total_size) orelse return null;

    const str: *JSString = @ptrCast(@alignCast(mem));
    const data_a = a.data();
    const data_b = b.data();

    str.* = .{
        .header = heap.MemBlockHeader.init(.string, total_size),
        .flags = .{
            .is_unique = false,
            .is_ascii = a.flags.is_ascii and b.flags.is_ascii,
            .is_numeric = false,
            .hash_computed = false,
        },
        .len = total_len,
        .hash = 0,
    };

    const data_mut = str.dataMut();
    @memcpy(data_mut[0..a.len], data_a);
    @memcpy(data_mut[a.len..], data_b);
    return str;
}

/// Concatenate two strings
pub fn concatStrings(allocator: std.mem.Allocator, a: *const JSString, b: *const JSString) !*JSString {
    const total_len = a.len + b.len;
    const total_size = @sizeOf(JSString) + total_len;
    const mem = try allocator.alignedAlloc(u8, std.mem.Alignment.of(JSString), total_size);

    const str: *JSString = @ptrCast(@alignCast(mem.ptr));
    const data_a = a.data();
    const data_b = b.data();

    str.* = .{
        .header = heap.MemBlockHeader.init(.string, total_size),
        .flags = .{
            .is_unique = false,
            .is_ascii = a.flags.is_ascii and b.flags.is_ascii,
            .is_numeric = false, // Concatenation rarely produces valid index
            .hash_computed = false,
        },
        .len = total_len,
        .hash = 0,
    };

    const data_mut = str.dataMut();
    @memcpy(data_mut[0..a.len], data_a);
    @memcpy(data_mut[a.len..], data_b);

    return str;
}

/// Concatenate multiple strings efficiently (single allocation)
/// More efficient than chained concatStrings for 3+ strings
pub fn concatMany(allocator: std.mem.Allocator, strings: []const *const JSString) !*JSString {
    if (strings.len == 0) {
        return try createString(allocator, "");
    }
    if (strings.len == 1) {
        // Return a copy of the single string
        return try createString(allocator, strings[0].data());
    }
    if (strings.len == 2) {
        return try concatStrings(allocator, strings[0], strings[1]);
    }

    // Calculate total length and check ASCII
    var total_len: u32 = 0;
    var all_ascii = true;
    for (strings) |s| {
        total_len += s.len;
        all_ascii = all_ascii and s.flags.is_ascii;
    }

    // Single allocation for result
    const total_size = @sizeOf(JSString) + total_len;
    const mem = try allocator.alignedAlloc(u8, std.mem.Alignment.of(JSString), total_size);

    const str: *JSString = @ptrCast(@alignCast(mem.ptr));
    str.* = .{
        .header = heap.MemBlockHeader.init(.string, total_size),
        .flags = .{
            .is_unique = false,
            .is_ascii = all_ascii,
            .is_numeric = false,
            .hash_computed = false,
        },
        .len = total_len,
        .hash = 0,
    };

    // Copy all strings into result
    const data_mut = str.dataMut();
    var offset: usize = 0;
    for (strings) |s| {
        @memcpy(data_mut[offset .. offset + s.len], s.data());
        offset += s.len;
    }

    return str;
}

/// Concatenate multiple strings using arena allocation (single allocation, no intermediate strings)
/// Returns null on allocation failure
pub fn concatManyWithArena(arena: *arena_mod.Arena, strings: []const *const JSString) ?*JSString {
    if (strings.len == 0) {
        return createStringWithArena(arena, "");
    }
    if (strings.len == 1) {
        return createStringWithArena(arena, strings[0].data());
    }
    if (strings.len == 2) {
        return concatStringsWithArena(arena, strings[0], strings[1]);
    }

    // Calculate total length and check ASCII
    var total_len: u32 = 0;
    var all_ascii = true;
    for (strings) |s| {
        total_len += s.len;
        all_ascii = all_ascii and s.flags.is_ascii;
    }

    // Single allocation for result
    const total_size = @sizeOf(JSString) + total_len;
    const mem = arena.alloc(total_size) orelse return null;

    const str: *JSString = @ptrCast(@alignCast(mem));
    str.* = .{
        .header = heap.MemBlockHeader.init(.string, total_size),
        .flags = .{
            .is_unique = false,
            .is_ascii = all_ascii,
            .is_numeric = false,
            .hash_computed = false,
        },
        .len = total_len,
        .hash = 0,
    };

    // Copy all strings into result
    const data_mut = str.dataMut();
    var offset: usize = 0;
    for (strings) |s| {
        @memcpy(data_mut[offset .. offset + s.len], s.data());
        offset += s.len;
    }

    return str;
}

/// StringBuilder for efficient incremental string building
/// Use when building a string from multiple parts dynamically
pub const StringBuilder = struct {
    parts: std.ArrayListUnmanaged(*const JSString),
    allocator: std.mem.Allocator,
    total_len: u32 = 0,
    all_ascii: bool = true,

    pub fn init(allocator: std.mem.Allocator) StringBuilder {
        return .{
            .parts = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *StringBuilder) void {
        self.parts.deinit(self.allocator);
    }

    /// Add a string to the builder
    pub fn append(self: *StringBuilder, str: *const JSString) !void {
        try self.parts.append(self.allocator, str);
        self.total_len += str.len;
        self.all_ascii = self.all_ascii and str.flags.is_ascii;
    }

    /// Add a raw string slice to the builder (creates temporary JSString)
    pub fn appendSlice(self: *StringBuilder, slice: []const u8) !void {
        const str = try createString(self.allocator, slice);
        try self.parts.append(self.allocator, str);
        self.total_len += @intCast(slice.len);
        self.all_ascii = self.all_ascii and isAscii(slice);
    }

    /// Build the final concatenated string
    pub fn build(self: *StringBuilder) !*JSString {
        return try concatMany(self.allocator, self.parts.items);
    }

    /// Clear the builder for reuse
    pub fn clear(self: *StringBuilder) void {
        self.parts.clearRetainingCapacity();
        self.total_len = 0;
        self.all_ascii = true;
    }
};

/// Format an integer to a buffer without allocation
/// Returns the slice of the buffer that contains the formatted number
pub fn formatIntToBuf(buf: []u8, n: i32) []const u8 {
    if (n == 0) {
        buf[0] = '0';
        return buf[0..1];
    }

    var value: u32 = if (n < 0) @intCast(-@as(i64, n)) else @intCast(n);
    var end: usize = buf.len;

    // Write digits from right to left
    while (value > 0) {
        end -= 1;
        buf[end] = '0' + @as(u8, @intCast(value % 10));
        value /= 10;
    }

    // Add negative sign if needed
    if (n < 0) {
        end -= 1;
        buf[end] = '-';
    }

    return buf[end..];
}

/// Format a float to a buffer without allocation
/// Returns the slice of the buffer that contains the formatted number
pub fn formatFloatToBuf(buf: []u8, f: f64) []const u8 {
    // Handle special cases
    if (std.math.isNan(f)) {
        @memcpy(buf[0..3], "NaN");
        return buf[0..3];
    }
    if (std.math.isPositiveInf(f)) {
        @memcpy(buf[0..8], "Infinity");
        return buf[0..8];
    }
    if (std.math.isNegativeInf(f)) {
        @memcpy(buf[0..9], "-Infinity");
        return buf[0..9];
    }

    // Check if it's an integer value
    if (@floor(f) == f and f >= -2147483648 and f <= 2147483647) {
        return formatIntToBuf(buf, @intFromFloat(f));
    }

    // Use std.fmt for general float formatting
    const result = std.fmt.bufPrint(buf, "{d}", .{f}) catch {
        @memcpy(buf[0..3], "NaN");
        return buf[0..3];
    };
    return result;
}

/// Concatenate a string and a number without intermediate string allocation
/// Uses arena allocation for the result
pub fn concatStringNumberWithArena(arena: *arena_mod.Arena, str: *const JSString, num_buf: []const u8) ?*JSString {
    const total_len = str.len + @as(u32, @intCast(num_buf.len));
    const total_size = @sizeOf(JSString) + total_len;
    const mem = arena.alloc(total_size) orelse return null;

    const result: *JSString = @ptrCast(@alignCast(mem));
    result.* = .{
        .header = heap.MemBlockHeader.init(.string, total_size),
        .flags = .{
            .is_unique = false,
            .is_ascii = str.flags.is_ascii and isAscii(num_buf),
            .is_numeric = false,
            .hash_computed = false,
        },
        .len = total_len,
        .hash = 0,
    };

    const data_mut = result.dataMut();
    @memcpy(data_mut[0..str.len], str.data());
    @memcpy(data_mut[str.len..], num_buf);
    return result;
}

/// Concatenate a number and a string without intermediate string allocation
/// Uses arena allocation for the result
pub fn concatNumberStringWithArena(arena: *arena_mod.Arena, num_buf: []const u8, str: *const JSString) ?*JSString {
    const total_len = @as(u32, @intCast(num_buf.len)) + str.len;
    const total_size = @sizeOf(JSString) + total_len;
    const mem = arena.alloc(total_size) orelse return null;

    const result: *JSString = @ptrCast(@alignCast(mem));
    result.* = .{
        .header = heap.MemBlockHeader.init(.string, total_size),
        .flags = .{
            .is_unique = false,
            .is_ascii = isAscii(num_buf) and str.flags.is_ascii,
            .is_numeric = false,
            .hash_computed = false,
        },
        .len = total_len,
        .hash = 0,
    };

    const data_mut = result.dataMut();
    @memcpy(data_mut[0..num_buf.len], num_buf);
    @memcpy(data_mut[num_buf.len..], str.data());
    return result;
}

// ============================================================================
// Tests
// ============================================================================

test "SIMD string comparison" {
    try std.testing.expectEqual(std.math.Order.eq, compareStrings("hello", "hello"));
    try std.testing.expectEqual(std.math.Order.lt, compareStrings("abc", "abd"));
    try std.testing.expectEqual(std.math.Order.gt, compareStrings("xyz", "abc"));
    try std.testing.expectEqual(std.math.Order.lt, compareStrings("short", "longer string"));
}

test "SIMD string equality" {
    // Short strings
    try std.testing.expect(eqlStrings("hello", "hello"));
    try std.testing.expect(!eqlStrings("hello", "world"));
    try std.testing.expect(!eqlStrings("hello", "hell"));

    // Empty strings
    try std.testing.expect(eqlStrings("", ""));

    // Long strings (>32 bytes, exercises SIMD path)
    const long1 = "This is a relatively long string that exceeds 32 bytes for SIMD testing";
    const long2 = "This is a relatively long string that exceeds 32 bytes for SIMD testing";
    const long3 = "This is a relatively long string that exceeds 32 bytes for SIMD DIFFER";
    try std.testing.expect(eqlStrings(long1, long2));
    try std.testing.expect(!eqlStrings(long1, long3));
}

test "SIMD findByte" {
    // Basic cases
    try std.testing.expectEqual(@as(?usize, 0), findByte("hello", 'h'));
    try std.testing.expectEqual(@as(?usize, 4), findByte("hello", 'o'));
    try std.testing.expectEqual(@as(?usize, null), findByte("hello", 'x'));

    // Empty string
    try std.testing.expectEqual(@as(?usize, null), findByte("", 'x'));

    // Long string (>32 bytes) - 48 chars
    const long = "0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF";
    try std.testing.expectEqual(@as(?usize, 10), findByte(long, 'A'));
    try std.testing.expectEqual(@as(?usize, 8), findByte(long, '8')); // First '8' at position 8

    // Find char that only appears after byte 32
    const long2 = "________________________________X_______________"; // 'X' at position 32
    try std.testing.expectEqual(@as(?usize, 32), findByte(long2, 'X'));
}

test "SIMD countByte" {
    try std.testing.expectEqual(@as(usize, 2), countByte("hello", 'l'));
    try std.testing.expectEqual(@as(usize, 1), countByte("hello", 'h'));
    try std.testing.expectEqual(@as(usize, 0), countByte("hello", 'x'));
    try std.testing.expectEqual(@as(usize, 0), countByte("", 'x'));

    // Long string with multiple occurrences
    const long = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"; // 52 a's
    try std.testing.expectEqual(@as(usize, 52), countByte(long, 'a'));
    try std.testing.expectEqual(@as(usize, 0), countByte(long, 'b'));
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

test "UTF-8 codepoint counting" {
    try std.testing.expectEqual(@as(u32, 5), countCodepoints("hello"));
    try std.testing.expectEqual(@as(u32, 2), countCodepoints("世界")); // 2 CJK chars
    try std.testing.expectEqual(@as(u32, 0), countCodepoints(""));
    try std.testing.expectEqual(@as(u32, 9), countCodepoints("hello, 世界")); // 7 ASCII + 2 CJK
}

test "UTF-8 codepoint byte length" {
    try std.testing.expectEqual(@as(u3, 1), codepointByteLength('a'));
    try std.testing.expectEqual(@as(u3, 2), codepointByteLength(0xC2)); // Start of 2-byte
    try std.testing.expectEqual(@as(u3, 3), codepointByteLength(0xE4)); // Start of 3-byte (CJK)
    try std.testing.expectEqual(@as(u3, 4), codepointByteLength(0xF0)); // Start of 4-byte (emoji)
}

test "UTF-8 codepoint decoding" {
    // ASCII
    const ascii = decodeCodepoint("A");
    try std.testing.expect(ascii != null);
    try std.testing.expectEqual(@as(u21, 'A'), ascii.?.codepoint);
    try std.testing.expectEqual(@as(u3, 1), ascii.?.len);

    // 3-byte CJK character (世 = U+4E16)
    const cjk = decodeCodepoint("世界");
    try std.testing.expect(cjk != null);
    try std.testing.expectEqual(@as(u21, 0x4E16), cjk.?.codepoint);
    try std.testing.expectEqual(@as(u3, 3), cjk.?.len);
}

test "string creation and data access" {
    const allocator = std.testing.allocator;

    const str = try createString(allocator, "hello");
    defer freeString(allocator, str);

    try std.testing.expectEqualStrings("hello", str.data());
    try std.testing.expectEqual(@as(u32, 5), str.len);
    try std.testing.expect(str.flags.is_ascii);
    try std.testing.expect(!str.flags.is_unique);
}

test "string interning" {
    const allocator = std.testing.allocator;

    var table = StringTable.init(allocator);
    defer table.deinit();

    const str1 = try table.intern("hello");
    const str2 = try table.intern("hello");
    const str3 = try table.intern("world");

    // Same string returns same pointer
    try std.testing.expectEqual(str1, str2);
    // Different strings return different pointers
    try std.testing.expect(str1 != str3);

    // Check flags
    try std.testing.expect(str1.flags.is_unique);

    // Check stats
    const stats = table.getStats();
    try std.testing.expectEqual(@as(usize, 2), stats.interned); // "hello" and "world"
    try std.testing.expectEqual(@as(usize, 1), stats.hits); // Second "hello" was a hit
}

test "string concatenation" {
    const allocator = std.testing.allocator;

    const str1 = try createString(allocator, "hello");
    defer freeString(allocator, str1);

    const str2 = try createString(allocator, " world");
    defer freeString(allocator, str2);

    const concat = try concatStrings(allocator, str1, str2);
    defer freeString(allocator, concat);

    try std.testing.expectEqualStrings("hello world", concat.data());
    try std.testing.expectEqual(@as(u32, 11), concat.len);
}

test "concatMany optimization" {
    const allocator = std.testing.allocator;

    const str1 = try createString(allocator, "Hello");
    defer freeString(allocator, str1);

    const str2 = try createString(allocator, ", ");
    defer freeString(allocator, str2);

    const str3 = try createString(allocator, "World");
    defer freeString(allocator, str3);

    const str4 = try createString(allocator, "!");
    defer freeString(allocator, str4);

    // Concatenate 4 strings in one allocation
    const strings = [_]*const JSString{ str1, str2, str3, str4 };
    const result = try concatMany(allocator, &strings);
    defer freeString(allocator, result);

    try std.testing.expectEqualStrings("Hello, World!", result.data());
    try std.testing.expectEqual(@as(u32, 13), result.len);
}

test "StringBuilder" {
    const allocator = std.testing.allocator;

    var builder = StringBuilder.init(allocator);
    defer builder.deinit();

    const str1 = try createString(allocator, "Hello");
    defer freeString(allocator, str1);
    const str2 = try createString(allocator, " ");
    defer freeString(allocator, str2);
    const str3 = try createString(allocator, "World");
    defer freeString(allocator, str3);

    try builder.append(str1);
    try builder.append(str2);
    try builder.append(str3);

    const result = try builder.build();
    defer freeString(allocator, result);

    try std.testing.expectEqualStrings("Hello World", result.data());
    try std.testing.expectEqual(@as(u32, 11), result.len);
}

test "JSString methods" {
    const allocator = std.testing.allocator;

    const str = try createString(allocator, "hello world");
    defer freeString(allocator, str);

    // startsWith
    try std.testing.expect(str.startsWith("hello"));
    try std.testing.expect(!str.startsWith("world"));

    // endsWith
    try std.testing.expect(str.endsWith("world"));
    try std.testing.expect(!str.endsWith("hello"));

    // indexOf
    try std.testing.expectEqual(@as(?u32, 0), str.indexOf("hello"));
    try std.testing.expectEqual(@as(?u32, 6), str.indexOf("world"));
    try std.testing.expectEqual(@as(?u32, null), str.indexOf("xyz"));

    // lastIndexOf
    try std.testing.expectEqual(@as(?u32, 6), str.lastIndexOf("world"));

    // charAt
    try std.testing.expectEqual(@as(?u8, 'h'), str.charAt(0));
    try std.testing.expectEqual(@as(?u8, 'd'), str.charAt(10));
    try std.testing.expectEqual(@as(?u8, null), str.charAt(100));

    // isEmpty
    try std.testing.expect(!str.isEmpty());
}

test "JSString equality" {
    const allocator = std.testing.allocator;

    const str1 = try createString(allocator, "test");
    defer freeString(allocator, str1);

    const str2 = try createString(allocator, "test");
    defer freeString(allocator, str2);

    const str3 = try createString(allocator, "other");
    defer freeString(allocator, str3);

    try std.testing.expect(str1.eql(str2));
    try std.testing.expect(!str1.eql(str3));
    try std.testing.expect(str1.eqlBytes("test"));
    try std.testing.expect(!str1.eqlBytes("other"));
}

// ============================================================================
// Rope Tests
// ============================================================================

test "RopeNode leaf creation" {
    const allocator = std.testing.allocator;

    const str = try createString(allocator, "hello");
    defer freeString(allocator, str);

    const leaf = try createRopeLeaf(allocator, str);
    defer allocator.destroy(leaf);

    try std.testing.expectEqual(@as(u32, 5), leaf.length());
    try std.testing.expect(leaf.kind == .leaf);
    try std.testing.expect(leaf.isAscii());
    try std.testing.expect(!leaf.isEmpty());
}

test "RopeNode concat - O(1) concatenation" {
    const allocator = std.testing.allocator;

    const str_a = try createString(allocator, "hello");
    defer freeString(allocator, str_a);

    const str_b = try createString(allocator, " world");
    defer freeString(allocator, str_b);

    const leaf_a = try createRopeLeaf(allocator, str_a);
    const leaf_b = try createRopeLeaf(allocator, str_b);

    // This should be O(1) - no copying!
    const concat = try concatRopes(allocator, leaf_a, leaf_b);
    defer freeRope(allocator, concat);

    try std.testing.expectEqual(@as(u32, 11), concat.length());
    try std.testing.expect(concat.kind == .concat);
    try std.testing.expect(concat.isAscii());
}

test "RopeNode flatten" {
    const allocator = std.testing.allocator;

    const str_a = try createString(allocator, "hello");
    defer freeString(allocator, str_a);

    const str_b = try createString(allocator, " world");
    defer freeString(allocator, str_b);

    const rope = try createRopeFromStrings(allocator, str_a, str_b);
    defer freeRope(allocator, rope);

    // Flatten the rope
    const flat = try rope.flatten(allocator);
    defer freeString(allocator, flat);

    try std.testing.expectEqualStrings("hello world", flat.data());
}

test "RopeNode eqlBytes" {
    const allocator = std.testing.allocator;

    const str_a = try createString(allocator, "hello");
    defer freeString(allocator, str_a);

    const str_b = try createString(allocator, " world");
    defer freeString(allocator, str_b);

    const rope = try createRopeFromStrings(allocator, str_a, str_b);
    defer freeRope(allocator, rope);

    // Test equality without flattening
    try std.testing.expect(rope.eqlBytes("hello world"));
    try std.testing.expect(!rope.eqlBytes("hello"));
    try std.testing.expect(!rope.eqlBytes("hello world!"));
}

test "RopeNode repeated concatenation - O(n) total instead of O(n^2)" {
    const allocator = std.testing.allocator;

    // Simulate building a string by repeated concatenation
    // With ropes, this is O(n) total work instead of O(n^2)

    const char_str = try createString(allocator, "x");
    defer freeString(allocator, char_str);

    var rope = try createRopeLeaf(allocator, char_str);

    // Build "xxxxxxxxxx" (10 x's) by repeated O(1) concatenation
    for (0..9) |_| {
        rope = try concatRopeString(allocator, rope, char_str);
    }
    defer freeRope(allocator, rope);

    // Verify result
    try std.testing.expectEqual(@as(u32, 10), rope.length());

    // Flatten and verify content
    const flat = try rope.flatten(allocator);
    defer freeString(allocator, flat);

    try std.testing.expectEqualStrings("xxxxxxxxxx", flat.data());
}

test "RopeNode empty handling" {
    const allocator = std.testing.allocator;

    const empty = try createString(allocator, "");
    defer freeString(allocator, empty);

    const hello = try createString(allocator, "hello");
    defer freeString(allocator, hello);

    const empty_rope = try createRopeLeaf(allocator, empty);
    const hello_rope = try createRopeLeaf(allocator, hello);

    // Concatenating with empty should return the non-empty rope
    const result1 = try concatRopes(allocator, empty_rope, hello_rope);
    try std.testing.expectEqual(hello_rope, result1);

    const result2 = try concatRopes(allocator, hello_rope, empty_rope);
    try std.testing.expectEqual(hello_rope, result2);

    // Clean up (only need to free the one we actually own)
    allocator.destroy(empty_rope);
    freeRope(allocator, hello_rope);
}

// ============================================================================
// SliceString Tests
// ============================================================================

test "SliceString creation and data access" {
    const allocator = std.testing.allocator;

    const parent = try createString(allocator, "hello world, this is a test string");
    defer freeString(allocator, parent);

    // Create a slice of "world"
    const slice = try createSlice(allocator, parent, 6, 5);
    defer freeSlice(allocator, slice);

    try std.testing.expectEqualStrings("world", slice.data());
    try std.testing.expectEqual(@as(u32, 5), slice.length());
    try std.testing.expect(!slice.isEmpty());
}

test "SliceString inherits ASCII flag" {
    const allocator = std.testing.allocator;

    const ascii_parent = try createString(allocator, "hello world, this is ASCII");
    defer freeString(allocator, ascii_parent);

    const slice = try createSlice(allocator, ascii_parent, 0, 5);
    defer freeSlice(allocator, slice);

    try std.testing.expect(slice.flags.is_ascii);
}

test "SliceString flatten" {
    const allocator = std.testing.allocator;

    const parent = try createString(allocator, "the quick brown fox");
    defer freeString(allocator, parent);

    const slice = try createSlice(allocator, parent, 4, 5); // "quick"
    defer freeSlice(allocator, slice);

    const flat = try slice.flatten(allocator);
    defer freeString(allocator, flat);

    try std.testing.expectEqualStrings("quick", flat.data());
    try std.testing.expectEqual(@as(u32, 5), flat.len);
}

test "SliceString eqlBytes" {
    const allocator = std.testing.allocator;

    const parent = try createString(allocator, "hello world");
    defer freeString(allocator, parent);

    const slice = try createSlice(allocator, parent, 0, 5); // "hello"
    defer freeSlice(allocator, slice);

    try std.testing.expect(slice.eqlBytes("hello"));
    try std.testing.expect(!slice.eqlBytes("world"));
    try std.testing.expect(!slice.eqlBytes("hello world"));
}

test "SliceString startsWith and endsWith" {
    const allocator = std.testing.allocator;

    const parent = try createString(allocator, "hello world, hello universe");
    defer freeString(allocator, parent);

    const slice = try createSlice(allocator, parent, 0, 11); // "hello world"
    defer freeSlice(allocator, slice);

    try std.testing.expect(slice.startsWith("hello"));
    try std.testing.expect(!slice.startsWith("world"));
    try std.testing.expect(slice.endsWith("world"));
    try std.testing.expect(!slice.endsWith("hello"));
}

test "SliceString MIN_SLICE_LEN threshold" {
    // Verify the minimum slice length constant
    try std.testing.expectEqual(@as(u32, 16), SliceString.MIN_SLICE_LEN);
}
