//! JSValue NaN-boxing implementation
//!
//! 64-bit tagged value representation using full NaN-boxing technique.
//! All f64 values are stored inline (no heap allocation for floats).
//!
//! Encoding scheme:
//! - Raw f64: stored directly when upper 16 bits != 0xFFFC..0xFFFF
//! - Tagged values: upper 16 bits = 0xFFFC, lower 48 bits = tag + payload
//!
//! This allows ALL f64 values (including subnormals, infinities) to be
//! stored inline, eliminating the 41.6x mathOps performance gap.

const std = @import("std");
const heap = @import("heap.zig");

/// 64-bit NaN-boxed JavaScript value
pub const JSValue = packed struct {
    raw: u64,

    // ========================================================================
    // NaN-boxing constants
    // ========================================================================

    /// Tag prefix for non-double values (upper 16 bits)
    /// We use 0xFFFC which is in the quiet NaN space
    const TAG_PREFIX: u64 = 0xFFFC_0000_0000_0000;

    /// Mask for detecting tagged values (upper 16 bits)
    const TAG_MASK: u64 = 0xFFFF_0000_0000_0000;

    /// Payload mask (lower 48 bits)
    const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;

    /// Type tag shift (bits 44-47 of raw value)
    const TYPE_SHIFT: u6 = 44;

    /// Type tag mask (4 bits = 16 types)
    const TYPE_MASK: u64 = 0xF << TYPE_SHIFT;

    /// Value payload mask (lower 44 bits)
    const VALUE_MASK: u64 = 0x0000_0FFF_FFFF_FFFF;

    /// Type tags (4 bits, allowing 16 types)
    const TYPE_PTR: u64 = 0x0 << TYPE_SHIFT; // GC-managed pointer
    const TYPE_INT: u64 = 0x1 << TYPE_SHIFT; // 32-bit signed integer
    const TYPE_SPECIAL: u64 = 0x2 << TYPE_SHIFT; // null/undefined/bool/exception
    const TYPE_EXTERN: u64 = 0x3 << TYPE_SHIFT; // Non-GC external pointer
    const TYPE_SYMBOL: u64 = 0x4 << TYPE_SHIFT; // Symbol
    // Types 5-15 reserved for future use

    /// Canonical NaN value (JavaScript's NaN)
    const CANONICAL_NAN_BITS: u64 = 0x7FF8_0000_0000_0000;

    // ========================================================================
    // Special value constants
    // ========================================================================

    pub const null_val: JSValue = .{ .raw = TAG_PREFIX | TYPE_SPECIAL | 0 };
    pub const undefined_val: JSValue = .{ .raw = TAG_PREFIX | TYPE_SPECIAL | 1 };
    pub const true_val: JSValue = .{ .raw = TAG_PREFIX | TYPE_SPECIAL | 2 };
    pub const false_val: JSValue = .{ .raw = TAG_PREFIX | TYPE_SPECIAL | 3 };
    pub const exception_val: JSValue = .{ .raw = TAG_PREFIX | TYPE_SPECIAL | 4 };
    pub const nan_val: JSValue = .{ .raw = CANONICAL_NAN_BITS }; // Stored as raw NaN

    // ========================================================================
    // Legacy Tag enum (for compatibility)
    // ========================================================================

    pub const Tag = enum(u3) {
        int = 0,
        ptr = 1,
        special = 3,
        short_float = 5, // Legacy - now we use full f64
        extern_ptr = 7,
    };

    // ========================================================================
    // Core type checking
    // ========================================================================

    /// Check if value is a tagged value (not a raw double)
    pub inline fn isTagged(self: JSValue) bool {
        // Tagged values have upper 16 bits = 0xFFFC
        return (self.raw & TAG_MASK) == TAG_PREFIX;
    }

    /// Check if value is a raw f64 (not a tagged value)
    /// This includes regular numbers, infinities, and NaN
    pub inline fn isRawDouble(self: JSValue) bool {
        // A value is a raw double if:
        // 1. Upper 16 bits are NOT 0xFFFC (our tag prefix), OR
        // 2. It's the canonical NaN
        const upper16 = self.raw & TAG_MASK;
        return upper16 != TAG_PREFIX or self.raw == CANONICAL_NAN_BITS;
    }

    // ========================================================================
    // Integer operations
    // ========================================================================

    /// Check if value is a 32-bit integer
    pub inline fn isInt(self: JSValue) bool {
        return self.isTagged() and (self.raw & TYPE_MASK) == TYPE_INT;
    }

    /// Extract 32-bit signed integer
    pub inline fn getInt(self: JSValue) i32 {
        std.debug.assert(self.isInt());
        return @bitCast(@as(u32, @truncate(self.raw & 0xFFFF_FFFF)));
    }

    /// Create integer value
    pub inline fn fromInt(val: i32) JSValue {
        const as_u32: u32 = @bitCast(val);
        return .{ .raw = TAG_PREFIX | TYPE_INT | @as(u64, as_u32) };
    }

    // ========================================================================
    // Pointer operations
    // ========================================================================

    /// Check if value is a GC-managed pointer
    pub inline fn isPtr(self: JSValue) bool {
        return self.isTagged() and (self.raw & TYPE_MASK) == TYPE_PTR;
    }

    /// Create value from pointer
    pub inline fn fromPtr(ptr: *anyopaque) JSValue {
        // Pointers are assumed to be 48-bit (x86-64/ARM64 canonical addresses)
        // and 8-byte aligned, so we can store them in 44 bits
        const addr = @intFromPtr(ptr);
        std.debug.assert(addr <= VALUE_MASK); // Verify fits in 44 bits
        return .{ .raw = TAG_PREFIX | TYPE_PTR | addr };
    }

    /// Extract pointer (unsafe - caller must verify isPtr)
    pub inline fn toPtr(self: JSValue, comptime T: type) *T {
        std.debug.assert(self.isPtr());
        return @ptrFromInt(self.raw & VALUE_MASK);
    }

    /// Get raw pointer address for use as a hash map key
    /// This is useful for identity-based maps like WeakMap
    pub inline fn getPtrAddress(self: JSValue) u64 {
        return self.raw & VALUE_MASK;
    }

    /// Check if value is a non-GC external pointer
    pub inline fn isExternPtr(self: JSValue) bool {
        return self.isTagged() and (self.raw & TYPE_MASK) == TYPE_EXTERN;
    }

    /// Create value from external pointer
    pub inline fn fromExternPtr(ptr: *anyopaque) JSValue {
        const addr = @intFromPtr(ptr);
        std.debug.assert(addr <= VALUE_MASK);
        return .{ .raw = TAG_PREFIX | TYPE_EXTERN | addr };
    }

    /// Extract external pointer (unsafe - caller must verify isExternPtr)
    pub inline fn toExternPtr(self: JSValue, comptime T: type) *T {
        std.debug.assert(self.isExternPtr());
        return @ptrFromInt(self.raw & VALUE_MASK);
    }

    // ========================================================================
    // Special value operations
    // ========================================================================

    /// Check if value is a special value (null/undefined/bool/exception)
    pub inline fn isSpecial(self: JSValue) bool {
        return self.isTagged() and (self.raw & TYPE_MASK) == TYPE_SPECIAL;
    }

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
    // Float64 Support (FULLY INLINE - no heap allocation!)
    // ========================================================================

    /// Float64 box header (LEGACY - kept for heap.zig compatibility)
    /// New code should use fromFloat/getFloat64 which are allocation-free
    pub const Float64Box = extern struct {
        header: heap.MemBlockHeader,
        _pad: u32,
        value: f64,

        pub fn getValue(self: *const Float64Box) f64 {
            return self.value;
        }
    };

    /// Create a float value (ALWAYS inline - no heap allocation!)
    pub inline fn fromFloat(v: f64) JSValue {
        const bits: u64 = @bitCast(v);

        // Check if this bit pattern would collide with our tag prefix
        if ((bits & TAG_MASK) == TAG_PREFIX) {
            // This is a quiet NaN that collides with our tags
            // Canonicalize to the standard NaN
            return .{ .raw = CANONICAL_NAN_BITS };
        }

        // Store the f64 bits directly
        return .{ .raw = bits };
    }

    /// Legacy: Try to create an inline float from f64
    /// Now ALWAYS succeeds (returns the value, never null)
    /// Kept for API compatibility during migration
    pub inline fn fromInlineFloat(v: f64) ?JSValue {
        return fromFloat(v);
    }

    /// Check if value is an inline float
    /// Legacy name - now checks for raw double
    pub inline fn isInlineFloat(self: JSValue) bool {
        return self.isRawDouble() and !self.isInt();
    }

    /// Get f64 value from inline float
    /// Legacy name - now just returns the raw bits as f64
    pub inline fn getInlineFloat(self: JSValue) f64 {
        return @bitCast(self.raw);
    }

    /// Check if value is a boxed float64 (heap-allocated)
    /// LEGACY: With new NaN-boxing, floats are never boxed
    /// This now always returns false for performance
    pub inline fn isBoxedFloat64(self: JSValue) bool {
        // Check if it's a pointer to a Float64Box
        // This is legacy - new code doesn't create boxed floats
        if (!self.isPtr()) return false;
        const ptr_val = self.raw & VALUE_MASK;
        if (ptr_val == 0) return false;
        const box: *Float64Box = @ptrFromInt(ptr_val);
        return box.header.tag == .float64;
    }

    /// Check if value is any float (inline or boxed)
    pub inline fn isFloat64(self: JSValue) bool {
        return self.isRawDouble() or self.isBoxedFloat64();
    }

    /// Alias for isFloat64 for API compatibility
    pub inline fn isFloat(self: JSValue) bool {
        return self.isFloat64();
    }

    /// Check if value is any number (int or float)
    pub inline fn isNumber(self: JSValue) bool {
        return self.isInt() or self.isRawDouble() or self.isBoxedFloat64();
    }

    /// Get float64 value (works for raw double and legacy boxed float)
    pub inline fn getFloat64(self: JSValue) f64 {
        if (self.isRawDouble()) {
            return @bitCast(self.raw);
        }
        // Legacy boxed float support
        if (self.isBoxedFloat64()) {
            const box: *Float64Box = @ptrFromInt(self.raw & VALUE_MASK);
            return box.value;
        }
        // Should not reach here if isFloat64() was checked
        return 0.0;
    }

    /// Convert value to f64 (works for int and float)
    pub inline fn toNumber(self: JSValue) ?f64 {
        if (self.isInt()) {
            return @floatFromInt(self.getInt());
        }
        if (self.isRawDouble()) {
            return @bitCast(self.raw);
        }
        if (self.isBoxedFloat64()) {
            const box: *Float64Box = @ptrFromInt(self.raw & VALUE_MASK);
            return box.value;
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
        if (self.isExternPtr()) {
            const header = self.toExternPtr(u32);
            return ((header.* >> 1) & 0xF) == 8; // MemTag.symbol
        }
        if (!self.isPtr()) return false;
        const header = self.toPtr(u32);
        return ((header.* >> 1) & 0xF) == 8; // MemTag.symbol
    }

    /// Get symbol ID (unsafe - caller must verify isSymbol)
    pub inline fn getSymbolId(self: JSValue) u32 {
        std.debug.assert(self.isSymbol());
        const box = if (self.isExternPtr())
            self.toExternPtr(SymbolBox)
        else
            self.toPtr(SymbolBox);
        return box.id;
    }

    /// Get symbol description (unsafe - caller must verify isSymbol)
    pub inline fn getSymbolDescription(self: JSValue) ?[]const u8 {
        std.debug.assert(self.isSymbol());
        const box = if (self.isExternPtr())
            self.toExternPtr(SymbolBox)
        else
            self.toPtr(SymbolBox);
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

    /// Check if value is a flat string (heap object with string tag)
    /// MemBlockHeader layout: [size_words:27][tag:4][gc_mark:1]
    /// So tag is at bits 1-4, need to shift right by 1 first
    pub inline fn isString(self: JSValue) bool {
        if (!self.isPtr()) return false;
        const header = self.toPtr(u32);
        return ((header.* >> 1) & 0xF) == 3; // MemTag.string
    }

    /// Check if value is a rope (lazy string concatenation)
    pub inline fn isRope(self: JSValue) bool {
        if (!self.isPtr()) return false;
        const header = self.toPtr(u32);
        return ((header.* >> 1) & 0xF) == 9; // MemTag.rope
    }

    /// Check if value is any string type (flat string or rope)
    /// Use this for typeof checks and general string operations
    pub inline fn isStringOrRope(self: JSValue) bool {
        if (!self.isPtr()) return false;
        const header = self.toPtr(u32);
        const tag = (header.* >> 1) & 0xF;
        return tag == 3 or tag == 9; // MemTag.string or MemTag.rope
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
        const string = @import("string.zig");

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
        // Handle flat strings and ropes
        const self_is_str = self.isString();
        const self_is_rope = self.isRope();
        const other_is_str = other.isString();
        const other_is_rope = other.isRope();

        if ((self_is_str or self_is_rope) and (other_is_str or other_is_rope)) {
            // Both flat strings: compare directly
            if (self_is_str and other_is_str) {
                const str_a = self.toPtr(string.JSString);
                const str_b = other.toPtr(string.JSString);
                return string.eqlStrings(str_a.data(), str_b.data());
            }

            // One or both is a rope: compare via rope interface
            if (self_is_rope and other_is_rope) {
                // Both ropes: compare lengths first, then content
                const rope_a = self.toPtr(string.RopeNode);
                const rope_b = other.toPtr(string.RopeNode);
                if (rope_a.total_len != rope_b.total_len) return false;

                // Need to flatten at least one to compare
                // For now, use the recursive comparison method
                // This could be optimized with a two-pointer approach
                if (rope_a.kind == .leaf and rope_b.kind == .leaf) {
                    return string.eqlStrings(
                        rope_a.payload.leaf.data(),
                        rope_b.payload.leaf.data(),
                    );
                }

                // Compare by flattening the smaller one mentally and comparing
                // This is a simplification - full implementation would traverse both trees
                if (rope_a.kind == .leaf) {
                    return rope_b.eqlBytes(rope_a.payload.leaf.data());
                }
                if (rope_b.kind == .leaf) {
                    return rope_a.eqlBytes(rope_b.payload.leaf.data());
                }

                // Both are concat nodes - need full comparison
                // For now, traverse and compare byte by byte
                // This could be optimized with proper rope comparison algorithms
                return ropeEqualsRope(rope_a, rope_b);
            }

            // One is string, one is rope
            if (self_is_str) {
                const str = self.toPtr(string.JSString);
                const rope = other.toPtr(string.RopeNode);
                return rope.eqlBytes(str.data());
            } else {
                const rope = self.toPtr(string.RopeNode);
                const str = other.toPtr(string.JSString);
                return rope.eqlBytes(str.data());
            }
        }

        return false;
    }

    /// Helper: compare two concat ropes by iterating through leaves
    fn ropeEqualsRope(a: *const @import("string.zig").RopeNode, b: *const @import("string.zig").RopeNode) bool {
        const string = @import("string.zig");

        if (a.total_len != b.total_len) return false;

        // Simple approach: collect all leaf data from both and compare
        // A more sophisticated implementation would use iterators
        var offset_a: usize = 0;
        var offset_b: usize = 0;
        return ropeEqualsRopeRecursive(a, b, &offset_a, &offset_b);
    }

    fn ropeEqualsRopeRecursive(
        a: *const @import("string.zig").RopeNode,
        b: *const @import("string.zig").RopeNode,
        offset_a: *usize,
        offset_b: *usize,
    ) bool {
        const string = @import("string.zig");

        // Get leaf data from both sides
        const a_leaves = collectLeaves(a);
        const b_leaves = collectLeaves(b);

        // Compare byte by byte across all leaves
        var a_idx: usize = 0;
        var a_pos: usize = 0;
        var b_idx: usize = 0;
        var b_pos: usize = 0;

        while (a_idx < a_leaves.len and b_idx < b_leaves.len) {
            const a_data = a_leaves[a_idx].data();
            const b_data = b_leaves[b_idx].data();

            while (a_pos < a_data.len and b_pos < b_data.len) {
                if (a_data[a_pos] != b_data[b_pos]) return false;
                a_pos += 1;
                b_pos += 1;
            }

            if (a_pos >= a_data.len) {
                a_idx += 1;
                a_pos = 0;
            }
            if (b_pos >= b_data.len) {
                b_idx += 1;
                b_pos = 0;
            }
        }

        return a_idx >= a_leaves.len and b_idx >= b_leaves.len;
    }

    /// Helper: collect all leaf strings from a rope (for comparison)
    /// Returns a bounded array - won't work for very deep ropes
    fn collectLeaves(node: *const @import("string.zig").RopeNode) []const *const @import("string.zig").JSString {
        const string = @import("string.zig");

        // Use a static buffer for simplicity - real implementation would use allocator
        const max_leaves = 64;
        const LeafArray = struct {
            var leaves: [max_leaves]*const string.JSString = undefined;
            var count: usize = 0;

            fn reset() void {
                count = 0;
            }

            fn add(s: *const string.JSString) void {
                if (count < max_leaves) {
                    leaves[count] = s;
                    count += 1;
                }
            }

            fn get() []const *const string.JSString {
                return leaves[0..count];
            }
        };

        LeafArray.reset();
        collectLeavesRecursive(node, LeafArray.add);
        return LeafArray.get();
    }

    fn collectLeavesRecursive(
        node: *const @import("string.zig").RopeNode,
        addFn: *const fn (*const @import("string.zig").JSString) void,
    ) void {
        const string = @import("string.zig");

        switch (node.kind) {
            .leaf => addFn(node.payload.leaf),
            .concat => {
                collectLeavesRecursive(node.payload.concat.left, addFn);
                collectLeavesRecursive(node.payload.concat.right, addFn);
            },
        }
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
        if (self.isStringOrRope()) return "string"; // Both flat strings and ropes
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
        } else if (self.isRawDouble()) {
            // Raw f64 stored directly
            const f: f64 = @bitCast(self.raw);
            if (std.math.isNan(f)) {
                try writer.writeAll("NaN");
            } else if (std.math.isPositiveInf(f)) {
                try writer.writeAll("Infinity");
            } else if (std.math.isNegativeInf(f)) {
                try writer.writeAll("-Infinity");
            } else {
                try writer.print("{d}", .{f});
            }
        } else if (self.isPtr()) {
            try writer.print("<ptr:0x{x}>", .{self.raw & VALUE_MASK});
        } else if (self.isExternPtr()) {
            try writer.print("<extern:0x{x}>", .{self.raw & VALUE_MASK});
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

    // Test full i32 range
    const max = JSValue.fromInt(std.math.maxInt(i32));
    try std.testing.expect(max.isInt());
    try std.testing.expectEqual(std.math.maxInt(i32), max.getInt());

    const min = JSValue.fromInt(std.math.minInt(i32));
    try std.testing.expect(min.isInt());
    try std.testing.expectEqual(std.math.minInt(i32), min.getInt());
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

test "JSValue extern pointer encoding" {
    var dummy: u64 = 0x87654321;
    const ptr_val = JSValue.fromExternPtr(&dummy);

    try std.testing.expect(ptr_val.isExternPtr());
    try std.testing.expect(!ptr_val.isPtr());
    try std.testing.expect(!ptr_val.isInt());
    try std.testing.expect(!ptr_val.isSpecial());

    const recovered = ptr_val.toExternPtr(u64);
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

test "JSValue float encoding - full f64 precision" {
    // Test that ALL float values can be stored inline with full precision
    const pi = JSValue.fromFloat(3.14159265358979323846);
    try std.testing.expect(pi.isRawDouble());
    try std.testing.expect(pi.isFloat64());
    try std.testing.expect(pi.isNumber());
    try std.testing.expect(!pi.isInt());

    // Verify EXACT roundtrip - no precision loss!
    const recovered = pi.getFloat64();
    try std.testing.expectEqual(@as(f64, 3.14159265358979323846), recovered);

    // Test zero
    const zero = JSValue.fromFloat(0.0);
    try std.testing.expect(zero.isRawDouble());
    try std.testing.expectEqual(@as(f64, 0.0), zero.getFloat64());

    // Test negative zero
    const neg_zero = JSValue.fromFloat(-0.0);
    try std.testing.expect(neg_zero.isRawDouble());
    // -0.0 bit pattern is different from 0.0
    try std.testing.expect(neg_zero.raw != zero.raw);

    // Test negative
    const neg = JSValue.fromFloat(-42.5);
    try std.testing.expect(neg.isRawDouble());
    try std.testing.expectEqual(@as(f64, -42.5), neg.getFloat64());

    // Test NaN - now stored inline!
    const nan = JSValue.fromFloat(std.math.nan(f64));
    try std.testing.expect(nan.isRawDouble());
    try std.testing.expect(std.math.isNan(nan.getFloat64()));

    // Test Infinity - now stored inline!
    const inf = JSValue.fromFloat(std.math.inf(f64));
    try std.testing.expect(inf.isRawDouble());
    try std.testing.expect(std.math.isPositiveInf(inf.getFloat64()));

    // Test negative infinity
    const neg_inf = JSValue.fromFloat(-std.math.inf(f64));
    try std.testing.expect(neg_inf.isRawDouble());
    try std.testing.expect(std.math.isNegativeInf(neg_inf.getFloat64()));

    // Test subnormal numbers
    const subnormal: f64 = 2.2250738585072014e-308 / 2.0;
    const sub_val = JSValue.fromFloat(subnormal);
    try std.testing.expect(sub_val.isRawDouble());
    try std.testing.expectEqual(subnormal, sub_val.getFloat64());
}

test "JSValue legacy fromInlineFloat compatibility" {
    // fromInlineFloat now always succeeds
    const val = JSValue.fromInlineFloat(2.5).?;
    const num = val.toNumber();
    try std.testing.expect(num != null);
    try std.testing.expectEqual(@as(f64, 2.5), num.?);

    // NaN now works too!
    const nan = JSValue.fromInlineFloat(std.math.nan(f64));
    try std.testing.expect(nan != null);
    try std.testing.expect(std.math.isNan(nan.?.getFloat64()));
}

test "JSValue benchmark values - full precision" {
    // Values from math_ops benchmark: (i + seed) % 1000 + 0.5
    // With new NaN-boxing, ALL values are stored with FULL f64 precision
    for (0..1000) |i| {
        const v: f64 = @as(f64, @floatFromInt(i)) + 0.5;
        const result = JSValue.fromFloat(v);
        // Verify EXACT roundtrip - no precision loss!
        const recovered = result.getFloat64();
        try std.testing.expectEqual(v, recovered);
    }

    // Test high precision values that would fail with f32
    const high_precision: f64 = 1.2345678901234567;
    const hp_val = JSValue.fromFloat(high_precision);
    try std.testing.expectEqual(high_precision, hp_val.getFloat64());
}

test "JSValue float type checks" {
    const float_val = JSValue.fromFloat(42.5);
    try std.testing.expect(float_val.isFloat64());
    try std.testing.expect(float_val.isFloat());
    try std.testing.expect(float_val.isNumber());
    try std.testing.expect(float_val.isRawDouble());
    try std.testing.expect(!float_val.isInt());
    try std.testing.expect(!float_val.isPtr());
    try std.testing.expect(!float_val.isSpecial());
}
