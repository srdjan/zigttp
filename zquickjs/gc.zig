//! Generational Garbage Collector
//!
//! Two-generation GC with bump allocation in nursery and mark-sweep in tenured.
//! Incorporates FUGC-inspired SIMD turbosweep for efficient collection.

const std = @import("std");
const heap = @import("heap.zig");
const value = @import("value.zig");

/// GC configuration with FUGC-inspired techniques
pub const GCConfig = struct {
    /// Nursery (young generation) size
    nursery_size: usize = 4 * 1024 * 1024, // 4MB
    /// Tenured (old generation) initial size
    tenured_initial_size: usize = 16 * 1024 * 1024, // 16MB
    /// Object survival threshold for promotion
    survival_threshold: u8 = 2,
    /// Enable SIMD bitvector sweeping
    simd_sweep: bool = true,
    /// Enable advancing wavefront (no marking regression)
    advancing_wavefront: bool = true,
    /// Enable soft handshakes (future: for concurrent pools)
    soft_handshakes: bool = false,
    /// Objects per SIMD sweep batch
    sweep_chunk_size: usize = 4096,
};

/// Nursery heap with bump allocation
pub const NurseryHeap = struct {
    base: [*]u8,
    ptr: [*]u8,
    limit: [*]u8,
    size: usize,

    pub fn init(memory: []u8) NurseryHeap {
        return .{
            .base = memory.ptr,
            .ptr = memory.ptr,
            .limit = memory.ptr + memory.len,
            .size = memory.len,
        };
    }

    /// Fast bump allocation (inline for hot path)
    pub inline fn alloc(self: *NurseryHeap, size: usize) ?*anyopaque {
        const aligned_size = std.mem.alignForward(usize, size, 8);
        if (@intFromPtr(self.ptr) + aligned_size > @intFromPtr(self.limit)) {
            return null; // Trigger minor GC
        }
        const result = self.ptr;
        self.ptr += aligned_size;
        return result;
    }

    /// Reset nursery after collection
    pub fn reset(self: *NurseryHeap) void {
        self.ptr = self.base;
    }

    /// Used bytes in nursery
    pub fn used(self: *NurseryHeap) usize {
        return @intFromPtr(self.ptr) - @intFromPtr(self.base);
    }
};

/// Tenured heap with mark-sweep and SIMD turbosweep
pub const TenuredHeap = struct {
    /// Mark bits: 1 bit per object slot, densely packed for SIMD
    mark_bitvector: []u64,
    /// Free list heads by size class
    free_lists: [16]?*FreeBlock,
    /// Total allocated bytes
    allocated: usize,
    allocator: std.mem.Allocator,

    const FreeBlock = struct {
        next: ?*FreeBlock,
        size: usize,
    };

    pub fn init(allocator: std.mem.Allocator, initial_size: usize) !TenuredHeap {
        const bitvector_size = (initial_size / 64 + 63) / 64;
        const bitvector = try allocator.alloc(u64, bitvector_size);
        @memset(bitvector, 0);

        return .{
            .mark_bitvector = bitvector,
            .free_lists = [_]?*FreeBlock{null} ** 16,
            .allocated = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *TenuredHeap) void {
        self.allocator.free(self.mark_bitvector);
    }

    /// SIMD turbosweep - process 256 objects per vector operation
    pub fn simdSweep(self: *TenuredHeap) void {
        // TODO: Implement SIMD-accelerated sweeping
        // For now, fall back to scalar sweep
        for (self.mark_bitvector, 0..) |mark_word, i| {
            const free_mask = ~mark_word;
            _ = free_mask; // TODO: Free unmarked objects
            self.mark_bitvector[i] = 0; // Clear marks for next cycle
        }
    }
};

/// Remembered set for cross-generation pointers
pub const RememberedSet = struct {
    entries: std.ArrayList(*anyopaque),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) RememberedSet {
        return .{
            .entries = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RememberedSet) void {
        self.entries.deinit(self.allocator);
    }

    pub fn add(self: *RememberedSet, ptr: *anyopaque) !void {
        try self.entries.append(self.allocator, ptr);
    }

    pub fn clear(self: *RememberedSet) void {
        self.entries.clearRetainingCapacity();
    }
};

/// Main GC structure
pub const GC = struct {
    nursery: NurseryHeap,
    tenured: TenuredHeap,
    remembered_set: RememberedSet,
    config: GCConfig,
    allocator: std.mem.Allocator,
    // Statistics
    minor_gc_count: u64 = 0,
    major_gc_count: u64 = 0,

    pub fn init(allocator: std.mem.Allocator, config: GCConfig) !GC {
        const nursery_mem = try allocator.alloc(u8, config.nursery_size);
        errdefer allocator.free(nursery_mem);

        var tenured = try TenuredHeap.init(allocator, config.tenured_initial_size);
        errdefer tenured.deinit();

        return .{
            .nursery = NurseryHeap.init(nursery_mem),
            .tenured = tenured,
            .remembered_set = RememberedSet.init(allocator),
            .config = config,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *GC) void {
        self.allocator.free(self.nursery.base[0..self.nursery.size]);
        self.tenured.deinit();
        self.remembered_set.deinit();
    }

    /// Allocate in nursery (fast path)
    pub inline fn allocNursery(self: *GC, size: usize) ?*anyopaque {
        return self.nursery.alloc(size);
    }

    /// Minor GC: evacuate live nursery objects to tenured
    pub fn minorGC(self: *GC) void {
        self.minor_gc_count += 1;
        // TODO: Implement Cheney's copying collection
        // 1. Scan roots
        // 2. Copy live objects to tenured
        // 3. Update references
        // 4. Reset nursery
        self.nursery.reset();
        self.remembered_set.clear();
    }

    /// Major GC: mark-sweep on tenured generation
    pub fn majorGC(self: *GC) void {
        self.major_gc_count += 1;
        // TODO: Implement mark-sweep
        // 1. Mark from roots
        // 2. Mark from remembered set
        // 3. Sweep unmarked (using SIMD turbosweep)
        if (self.config.simd_sweep) {
            self.tenured.simdSweep();
        }
    }

    /// Write barrier for cross-generation pointers
    pub fn writeBarrier(self: *GC, old_ptr: *anyopaque, new_val: value.JSValue) !void {
        if (new_val.isPtr()) {
            const new_ptr = new_val.toPtr(*anyopaque);
            // Check if old is in tenured and new is in nursery
            if (self.isInTenured(old_ptr) and self.isInNursery(new_ptr)) {
                try self.remembered_set.add(old_ptr);
            }
        }
    }

    fn isInNursery(self: *GC, ptr: *anyopaque) bool {
        const addr = @intFromPtr(ptr);
        const base = @intFromPtr(self.nursery.base);
        return addr >= base and addr < base + self.nursery.size;
    }

    fn isInTenured(self: *GC, ptr: *anyopaque) bool {
        // TODO: Implement proper tenured range check
        return !self.isInNursery(ptr);
    }
};

test "NurseryHeap bump allocation" {
    var buf: [1024]u8 = undefined;
    var nursery = NurseryHeap.init(&buf);

    const ptr1 = nursery.alloc(64);
    try std.testing.expect(ptr1 != null);
    try std.testing.expectEqual(@as(usize, 64), nursery.used());

    const ptr2 = nursery.alloc(128);
    try std.testing.expect(ptr2 != null);
    try std.testing.expectEqual(@as(usize, 192), nursery.used());

    nursery.reset();
    try std.testing.expectEqual(@as(usize, 0), nursery.used());
}

test "GC initialization" {
    const allocator = std.testing.allocator;
    var gc = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc.deinit();

    try std.testing.expectEqual(@as(u64, 0), gc.minor_gc_count);
    try std.testing.expectEqual(@as(u64, 0), gc.major_gc_count);
}
