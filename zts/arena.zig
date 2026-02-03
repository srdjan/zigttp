//! Request-scoped arena allocator for ephemeral allocations.
//!
//! The arena provides O(1) bump allocation and O(1) reset, optimized for
//! FaaS request-response cycles where all request allocations die together.
//!
//! Architecture:
//! - Primary: 4MB bump-allocated region (configurable)
//! - Overflow: Linked list of heap allocations for large/excess requests
//! - Reset: Pointer reset + overflow cleanup
//!
//! Thread safety: Not thread-safe. Each Runtime owns one arena.

const std = @import("std");
const heap = @import("heap.zig");

pub const ArenaConfig = struct {
    /// Total arena size (default 4MB)
    size: usize = 4 * 1024 * 1024,
    /// Alignment for all allocations
    alignment: usize = 8,
    /// Track high watermark for diagnostics
    track_watermark: bool = true,
};

/// Overflow allocation node for when arena is exhausted
const OverflowNode = struct {
    next: ?*OverflowNode,
    size: usize,
    // Data follows immediately after this header
};

/// Request-scoped arena allocator
pub const Arena = struct {
    /// Base pointer of arena memory
    base: [*]align(8) u8,
    /// Current allocation pointer (grows up)
    ptr: [*]align(8) u8,
    /// End of arena
    limit: [*]u8,
    /// Total arena size
    size: usize,
    /// Peak usage in current/recent request
    high_watermark: usize,
    /// Allocation count for diagnostics
    alloc_count: u64,
    /// Overflow allocations linked list
    overflow_head: ?*OverflowNode,
    /// Total bytes in overflow
    overflow_bytes: usize,
    /// Overflow allocation count
    overflow_count: u64,
    /// Backing allocator for arena memory and overflow
    backing: std.mem.Allocator,
    /// Configuration
    config: ArenaConfig,

    /// Initialize arena with backing allocator
    pub fn init(backing: std.mem.Allocator, config: ArenaConfig) !Arena {
        const mem = try backing.alignedAlloc(u8, .@"8", config.size);
        return .{
            .base = @ptrCast(mem.ptr),
            .ptr = @ptrCast(mem.ptr),
            .limit = mem.ptr + mem.len,
            .size = config.size,
            .high_watermark = 0,
            .alloc_count = 0,
            .overflow_head = null,
            .overflow_bytes = 0,
            .overflow_count = 0,
            .backing = backing,
            .config = config,
        };
    }

    /// O(1) bump allocation - the hot path
    /// Returns null if arena exhausted and overflow fails
    pub inline fn alloc(self: *Arena, size: usize) ?*anyopaque {
        const aligned_size = std.mem.alignForward(usize, size, self.config.alignment);
        const current = @intFromPtr(self.ptr);
        const new_ptr = current + aligned_size;

        if (new_ptr <= @intFromPtr(self.limit)) {
            const result = self.ptr;
            self.ptr = @ptrFromInt(new_ptr);
            self.alloc_count += 1;

            if (self.config.track_watermark) {
                const used = new_ptr - @intFromPtr(self.base);
                if (used > self.high_watermark) {
                    self.high_watermark = used;
                }
            }
            return result;
        }

        // Arena exhausted - use overflow
        return self.allocOverflow(aligned_size);
    }

    /// O(1) bump allocation with custom alignment
    /// Use for types requiring alignment > 8 bytes (e.g., SIMD, Float64Box)
    pub inline fn allocAligned(self: *Arena, size: usize, alignment: usize) ?*anyopaque {
        const current = @intFromPtr(self.ptr);
        // Align the pointer first, then add size
        const aligned_ptr = std.mem.alignForward(usize, current, alignment);
        const aligned_size = std.mem.alignForward(usize, size, alignment);
        const new_ptr = aligned_ptr + aligned_size;

        if (new_ptr <= @intFromPtr(self.limit)) {
            self.ptr = @ptrFromInt(new_ptr);
            self.alloc_count += 1;

            if (self.config.track_watermark) {
                const used = new_ptr - @intFromPtr(self.base);
                if (used > self.high_watermark) {
                    self.high_watermark = used;
                }
            }
            return @ptrFromInt(aligned_ptr);
        }

        // Arena exhausted - use overflow
        return self.allocOverflow(aligned_size);
    }

    /// Allocate with type safety
    pub fn create(self: *Arena, comptime T: type) ?*T {
        const ptr = self.alloc(@sizeOf(T)) orelse return null;
        return @ptrCast(@alignCast(ptr));
    }

    /// Allocate with type safety and type's natural alignment
    /// Use for types that need alignment > 8 bytes
    pub fn createAligned(self: *Arena, comptime T: type) ?*T {
        const ptr = self.allocAligned(@sizeOf(T), @alignOf(T)) orelse return null;
        return @ptrCast(@alignCast(ptr));
    }

    /// Allocate slice
    pub fn allocSlice(self: *Arena, comptime T: type, n: usize) ?[]T {
        const ptr = self.alloc(@sizeOf(T) * n) orelse return null;
        const typed_ptr: [*]T = @ptrCast(@alignCast(ptr));
        return typed_ptr[0..n];
    }

    /// Allocate with MemBlockHeader prefix (heap-compatible)
    pub fn allocTagged(self: *Arena, tag: heap.MemTag, size: usize) ?*anyopaque {
        const header_size = @sizeOf(heap.MemBlockHeader);
        const total_size = header_size + size;
        const ptr = self.alloc(total_size) orelse return null;

        // Initialize header
        const header: *heap.MemBlockHeader = @ptrCast(@alignCast(ptr));
        header.* = heap.MemBlockHeader.init(tag, total_size);

        // Return pointer after header
        return @as([*]u8, @ptrCast(ptr)) + header_size;
    }

    /// O(1) reset - reclaim all arena memory instantly
    pub fn reset(self: *Arena) void {
        // Reset bump pointer
        self.ptr = self.base;
        self.alloc_count = 0;

        // Free overflow allocations
        self.freeOverflow();

        // Optionally poison memory in debug builds
        if (std.debug.runtime_safety) {
            const len = @intFromPtr(self.limit) - @intFromPtr(self.base);
            @memset(self.base[0..len], 0xAA);
        }
    }

    /// Get current usage statistics
    /// Includes overflow allocations in total usage calculations
    pub fn getStats(self: *const Arena) ArenaStats {
        const arena_used = @intFromPtr(self.ptr) - @intFromPtr(self.base);
        const total_used = arena_used + self.overflow_bytes;
        // Include overflow in high watermark for accurate peak tracking
        const effective_watermark = @max(self.high_watermark, arena_used) + self.overflow_bytes;
        return .{
            .used = arena_used,
            .total = self.size,
            .high_watermark = effective_watermark,
            .alloc_count = self.alloc_count,
            .overflow_bytes = self.overflow_bytes,
            .overflow_count = self.overflow_count,
            .usage_percent = @as(f32, @floatFromInt(total_used)) / @as(f32, @floatFromInt(self.size)) * 100.0,
        };
    }

    /// Current bytes used in arena (not including overflow)
    pub fn usedBytes(self: *const Arena) usize {
        return @intFromPtr(self.ptr) - @intFromPtr(self.base);
    }

    /// Remaining bytes in arena before overflow
    pub fn remainingBytes(self: *const Arena) usize {
        return @intFromPtr(self.limit) - @intFromPtr(self.ptr);
    }

    /// Check if a pointer belongs to this arena (including overflow allocations)
    pub fn contains(self: *const Arena, ptr: *anyopaque) bool {
        const p = @intFromPtr(ptr);
        const base = @intFromPtr(self.base);
        const limit = @intFromPtr(self.limit);
        if (p >= base and p < limit) return true;

        var node = self.overflow_head;
        while (node) |n| {
            const start = @intFromPtr(n) + @sizeOf(OverflowNode);
            const end = @intFromPtr(n) + n.size;
            if (p >= start and p < end) return true;
            node = n.next;
        }
        return false;
    }

    /// std.mem.Allocator interface for the arena (no-op free, no grow)
    pub fn allocator(self: *Arena) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &arena_allocator_vtable,
        };
    }

    /// Allocate from overflow heap when arena is exhausted
    fn allocOverflow(self: *Arena, size: usize) ?*anyopaque {
        const node_size = @sizeOf(OverflowNode);
        const total_size = node_size + size;

        const mem = self.backing.alignedAlloc(u8, .@"8", total_size) catch return null;

        const node: *OverflowNode = @ptrCast(@alignCast(mem.ptr));
        node.* = .{
            .next = self.overflow_head,
            .size = total_size,
        };
        self.overflow_head = node;
        self.overflow_bytes += total_size;
        self.overflow_count += 1;
        self.alloc_count += 1;

        // Return pointer after header
        return mem.ptr + node_size;
    }

    /// Free all overflow allocations
    fn freeOverflow(self: *Arena) void {
        var node = self.overflow_head;
        while (node) |n| {
            const next = n.next;
            const ptr: [*]align(8) u8 = @ptrCast(@alignCast(n));
            self.backing.free(ptr[0..n.size]);
            node = next;
        }
        self.overflow_head = null;
        self.overflow_bytes = 0;
        self.overflow_count = 0;
    }

    /// Release arena memory back to backing allocator
    pub fn deinit(self: *Arena) void {
        self.freeOverflow();
        const len = @intFromPtr(self.limit) - @intFromPtr(self.base);
        self.backing.free(self.base[0..len]);
    }
};

fn arenaAlloc(ctx: *anyopaque, len: usize, ptr_align: std.mem.Alignment, _: usize) ?[*]u8 {
    const arena: *Arena = @ptrCast(@alignCast(ctx));
    const alignment = @as(usize, 1) << @intFromEnum(ptr_align);
    const mem = arena.allocAligned(len, alignment) orelse return null;
    return @ptrCast(mem);
}

fn arenaResize(_: *anyopaque, buf: []u8, _: std.mem.Alignment, new_len: usize, _: usize) bool {
    // Allow shrinking in place; growing is unsupported
    return new_len <= buf.len;
}

fn arenaRemap(_: *anyopaque, buf: []u8, _: std.mem.Alignment, new_len: usize, _: usize) ?[*]u8 {
    // Support shrink in place; growing unsupported for arena
    if (new_len <= buf.len) return buf.ptr;
    return null;
}

fn arenaFree(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize) void {
    // No-op: arena memory is reclaimed on reset
}

const arena_allocator_vtable = std.mem.Allocator.VTable{
    .alloc = arenaAlloc,
    .resize = arenaResize,
    .remap = arenaRemap,
    .free = arenaFree,
};

pub const ArenaStats = struct {
    used: usize,
    total: usize,
    high_watermark: usize,
    alloc_count: u64,
    overflow_bytes: usize,
    overflow_count: u64,
    usage_percent: f32,
};

/// Hybrid allocator that routes between persistent and ephemeral allocations
pub const HybridAllocator = struct {
    /// Standard allocator for persistent objects (bytecode, prototypes, shapes)
    persistent: std.mem.Allocator,
    /// Arena for ephemeral request-scoped objects
    arena: *Arena,
    /// Memory limit (0 = no limit)
    memory_limit: usize = 0,
    /// Tracked persistent bytes (ephemeral usage tracked via arena stats)
    persistent_used: usize = 0,

    pub const Kind = enum {
        /// Lives forever - bytecode, prototypes, hidden classes
        persistent,
        /// Dies at request end - objects, strings, floats
        ephemeral,
    };

    /// Allocate based on lifetime kind
    pub fn alloc(self: *HybridAllocator, kind: Kind, size: usize) ?*anyopaque {
        return switch (kind) {
            .persistent => blk: {
                if (!self.withinBudget(size)) return null;
                const mem = self.persistent.alignedAlloc(u8, .@"8", size) catch return null;
                self.persistent_used += size;
                break :blk @ptrCast(mem.ptr);
            },
            .ephemeral => blk: {
                const aligned_size = std.mem.alignForward(usize, size, self.arena.config.alignment);
                if (!self.withinBudget(aligned_size)) return null;
                break :blk self.arena.alloc(size);
            },
        };
    }

    /// Allocate with type safety
    pub fn create(self: *HybridAllocator, kind: Kind, comptime T: type) ?*T {
        const ptr = self.alloc(kind, @sizeOf(T)) orelse return null;
        return @ptrCast(@alignCast(ptr));
    }

    /// Allocate tagged (with MemBlockHeader) for ephemeral objects
    pub fn allocTagged(self: *HybridAllocator, kind: Kind, tag: heap.MemTag, size: usize) ?*anyopaque {
        return switch (kind) {
            .persistent => blk: {
                const header_size = @sizeOf(heap.MemBlockHeader);
                const total = header_size + size;
                if (!self.withinBudget(total)) return null;
                const mem = self.persistent.alignedAlloc(u8, .@"8", total) catch return null;
                const header: *heap.MemBlockHeader = @ptrCast(@alignCast(mem.ptr));
                header.* = heap.MemBlockHeader.init(tag, total);
                self.persistent_used += total;
                break :blk mem.ptr + header_size;
            },
            .ephemeral => blk: {
                const header_size = @sizeOf(heap.MemBlockHeader);
                const total = header_size + size;
                const aligned_total = std.mem.alignForward(usize, total, self.arena.config.alignment);
                if (!self.withinBudget(aligned_total)) return null;
                break :blk self.arena.allocTagged(tag, size);
            },
        };
    }

    pub fn setMemoryLimit(self: *HybridAllocator, limit: usize) void {
        self.memory_limit = limit;
    }

    /// Reset ephemeral allocations (O(1) arena reset)
    /// Persistent allocations are untouched
    pub fn resetEphemeral(self: *HybridAllocator) void {
        self.arena.reset();
    }

    /// Get arena statistics
    pub fn getArenaStats(self: *const HybridAllocator) ArenaStats {
        return self.arena.getStats();
    }

    fn withinBudget(self: *const HybridAllocator, size: usize) bool {
        if (self.memory_limit == 0) return true;
        const arena_used = self.arena.usedBytes() + self.arena.overflow_bytes;
        return self.persistent_used + arena_used + size <= self.memory_limit;
    }
};

// =============================================================================
// Tests
// =============================================================================

test "Arena basic allocation" {
    var arena = try Arena.init(std.testing.allocator, .{ .size = 4096 });
    defer arena.deinit();

    const ptr1 = arena.alloc(64);
    try std.testing.expect(ptr1 != null);

    const ptr2 = arena.alloc(128);
    try std.testing.expect(ptr2 != null);
    try std.testing.expect(ptr2 != ptr1);

    const stats = arena.getStats();
    try std.testing.expect(stats.used >= 192);
    try std.testing.expectEqual(@as(u64, 2), stats.alloc_count);
}

test "Arena reset reclaims memory" {
    var arena = try Arena.init(std.testing.allocator, .{ .size = 4096 });
    defer arena.deinit();

    // Fill arena
    var count: usize = 0;
    while (arena.alloc(64) != null) {
        count += 1;
        if (count > 100) break; // Safety limit
    }

    const before = arena.getStats();
    try std.testing.expect(before.used > 3500);

    arena.reset();

    const after = arena.getStats();
    try std.testing.expectEqual(@as(usize, 0), after.used);
    try std.testing.expectEqual(@as(u64, 0), after.alloc_count);
}

test "Arena overflow handling" {
    var arena = try Arena.init(std.testing.allocator, .{ .size = 256 });
    defer arena.deinit();

    // Exhaust arena
    _ = arena.alloc(200);

    // Should use overflow
    const overflow_ptr = arena.alloc(100);
    try std.testing.expect(overflow_ptr != null);

    const stats = arena.getStats();
    try std.testing.expect(stats.overflow_bytes > 0);
    try std.testing.expectEqual(@as(u64, 1), stats.overflow_count);

    // Reset should free overflow
    arena.reset();
    const after = arena.getStats();
    try std.testing.expectEqual(@as(usize, 0), after.overflow_bytes);
}

test "Arena typed allocation" {
    var arena = try Arena.init(std.testing.allocator, .{ .size = 4096 });
    defer arena.deinit();

    const TestStruct = struct {
        a: u64,
        b: u32,
        c: bool,
    };

    const ptr = arena.create(TestStruct);
    try std.testing.expect(ptr != null);

    ptr.?.* = .{ .a = 42, .b = 100, .c = true };
    try std.testing.expectEqual(@as(u64, 42), ptr.?.a);
}

test "Arena slice allocation" {
    var arena = try Arena.init(std.testing.allocator, .{ .size = 4096 });
    defer arena.deinit();

    const slice = arena.allocSlice(u64, 10);
    try std.testing.expect(slice != null);
    try std.testing.expectEqual(@as(usize, 10), slice.?.len);

    for (slice.?, 0..) |*item, i| {
        item.* = @intCast(i * 10);
    }
    try std.testing.expectEqual(@as(u64, 50), slice.?[5]);
}

test "Arena tagged allocation" {
    var arena = try Arena.init(std.testing.allocator, .{ .size = 4096 });
    defer arena.deinit();

    const ptr = arena.allocTagged(.object, 64);
    try std.testing.expect(ptr != null);

    // Verify header is accessible before the returned pointer
    const header_ptr: *heap.MemBlockHeader = @ptrCast(@alignCast(@as([*]u8, @ptrCast(ptr.?)) - @sizeOf(heap.MemBlockHeader)));
    try std.testing.expectEqual(heap.MemTag.object, header_ptr.tag);
}

test "HybridAllocator routing" {
    var arena = try Arena.init(std.testing.allocator, .{ .size = 4096 });
    defer arena.deinit();

    var hybrid = HybridAllocator{
        .persistent = std.testing.allocator,
        .arena = &arena,
    };

    // Ephemeral allocation goes to arena
    const eph = hybrid.alloc(.ephemeral, 64);
    try std.testing.expect(eph != null);

    const arena_stats = hybrid.getArenaStats();
    try std.testing.expect(arena_stats.used >= 64);

    // Persistent allocation goes to backing allocator
    const pers = hybrid.alloc(.persistent, 64);
    try std.testing.expect(pers != null);
    // Must manually free persistent allocations
    std.testing.allocator.free(@as([*]align(8) u8, @ptrCast(@alignCast(pers.?)))[0..64]);

    // Reset only clears ephemeral
    hybrid.resetEphemeral();
    const after_stats = hybrid.getArenaStats();
    try std.testing.expectEqual(@as(usize, 0), after_stats.used);
}

test "Arena high watermark tracking" {
    var arena = try Arena.init(std.testing.allocator, .{ .size = 4096, .track_watermark = true });
    defer arena.deinit();

    _ = arena.alloc(100);
    _ = arena.alloc(200);

    const stats1 = arena.getStats();
    try std.testing.expect(stats1.high_watermark >= 300);

    arena.reset();

    // Watermark should persist after reset
    _ = arena.alloc(50);
    const stats2 = arena.getStats();
    try std.testing.expect(stats2.high_watermark >= 300);
    try std.testing.expect(stats2.used < 100);
}
