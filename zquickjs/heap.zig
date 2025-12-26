//! Heap memory management with typed block headers
//!
//! Dynamic allocator with size classes for efficient allocation.

const std = @import("std");
const value = @import("value.zig");

/// Memory block tags (4-bit)
pub const MemTag = enum(u4) {
    free = 0,
    object = 1,
    float64 = 2,
    string = 3,
    function_bytecode = 4,
    value_array = 5,
    byte_array = 6,
    varref = 7,
    // 8-15 reserved for future use
};

/// Memory block header (packed for minimal overhead)
pub const MemBlockHeader = packed struct {
    gc_mark: bool = false,
    tag: MemTag,
    // Size in words (for variable-size blocks)
    size_words: u27 = 0,

    pub fn init(tag: MemTag, size_bytes: usize) MemBlockHeader {
        const words = (size_bytes + 7) / 8;
        return .{
            .tag = tag,
            .size_words = @intCast(words),
        };
    }

    pub fn sizeBytes(self: MemBlockHeader) usize {
        return @as(usize, self.size_words) * 8;
    }
};

/// Size classes for efficient allocation
pub const SizeClass = enum(u4) {
    size_16 = 0,
    size_32 = 1,
    size_64 = 2,
    size_128 = 3,
    size_256 = 4,
    size_512 = 5,
    large = 15, // Anything larger

    pub fn fromSize(size: usize) SizeClass {
        if (size <= 16) return .size_16;
        if (size <= 32) return .size_32;
        if (size <= 64) return .size_64;
        if (size <= 128) return .size_128;
        if (size <= 256) return .size_256;
        if (size <= 512) return .size_512;
        return .large;
    }

    pub fn slotSize(self: SizeClass) usize {
        return switch (self) {
            .size_16 => 16,
            .size_32 => 32,
            .size_64 => 64,
            .size_128 => 128,
            .size_256 => 256,
            .size_512 => 512,
            .large => 0, // Variable
        };
    }
};

/// Heap configuration
pub const HeapConfig = struct {
    /// Total heap size
    total_size: usize = 64 * 1024 * 1024, // 64MB default
    /// Nursery size (young generation)
    nursery_size: usize = 4 * 1024 * 1024, // 4MB default
};

/// Placeholder heap structure (to be implemented in Phase 2)
pub const Heap = struct {
    config: HeapConfig,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, config: HeapConfig) Heap {
        return .{
            .config = config,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Heap) void {
        _ = self;
        // TODO: Free all allocated memory
    }

    /// Allocate memory block with given tag
    pub fn alloc(self: *Heap, tag: MemTag, size: usize) ?*anyopaque {
        _ = self;
        _ = tag;
        _ = size;
        // TODO: Implement allocation
        return null;
    }

    /// Free memory block
    pub fn free(self: *Heap, ptr: *anyopaque) void {
        _ = self;
        _ = ptr;
        // TODO: Implement deallocation
    }
};

test "MemBlockHeader encoding" {
    const header = MemBlockHeader.init(.object, 64);
    try std.testing.expectEqual(MemTag.object, header.tag);
    try std.testing.expect(!header.gc_mark);
    try std.testing.expectEqual(@as(usize, 64), header.sizeBytes());
}

test "SizeClass selection" {
    try std.testing.expectEqual(SizeClass.size_16, SizeClass.fromSize(8));
    try std.testing.expectEqual(SizeClass.size_16, SizeClass.fromSize(16));
    try std.testing.expectEqual(SizeClass.size_32, SizeClass.fromSize(24));
    try std.testing.expectEqual(SizeClass.size_64, SizeClass.fromSize(48));
    try std.testing.expectEqual(SizeClass.large, SizeClass.fromSize(1024));
}
