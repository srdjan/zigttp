//! Heap memory management with typed block headers
//!
//! Dynamic allocator with size classes for efficient allocation.
//! Uses segregated free lists for fast allocation/deallocation.

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
    symbol = 8,
    // 9-15 reserved for future use
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

    pub const COUNT = 6; // Number of fixed size classes

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

    pub fn index(self: SizeClass) usize {
        return @intFromEnum(self);
    }
};

/// Heap configuration
pub const HeapConfig = struct {
    /// Total heap size
    total_size: usize = 64 * 1024 * 1024, // 64MB default
    /// Nursery size (young generation)
    nursery_size: usize = 4 * 1024 * 1024, // 4MB default
    /// Page size for size class allocations
    page_size: usize = 4096,
};

/// Free block node (embedded in freed memory)
const FreeNode = struct {
    next: ?*FreeNode,
};

/// Page header for tracking allocations
const PageHeader = struct {
    next: ?*PageHeader,
    size_class: SizeClass,
    used_count: u32,
    total_slots: u32,
};

/// Large allocation header
const LargeHeader = struct {
    next: ?*LargeHeader,
    prev: ?*LargeHeader,
    size: usize,
    tag: MemTag,
    gc_mark: bool,
};

/// Size class allocator with segregated free lists
pub const Heap = struct {
    config: HeapConfig,
    allocator: std.mem.Allocator,

    /// Free lists for each size class
    free_lists: [SizeClass.COUNT]?*FreeNode,
    /// Page lists for each size class
    pages: [SizeClass.COUNT]?*PageHeader,
    /// Large allocation list (doubly linked)
    large_allocs: ?*LargeHeader,

    /// Statistics
    stats: HeapStats,

    pub const HeapStats = struct {
        total_allocated: usize = 0,
        total_freed: usize = 0,
        small_alloc_count: usize = 0,
        large_alloc_count: usize = 0,
        page_count: usize = 0,
    };

    pub fn init(allocator: std.mem.Allocator, config: HeapConfig) Heap {
        return .{
            .config = config,
            .allocator = allocator,
            .free_lists = [_]?*FreeNode{null} ** SizeClass.COUNT,
            .pages = [_]?*PageHeader{null} ** SizeClass.COUNT,
            .large_allocs = null,
            .stats = .{},
        };
    }

    pub fn deinit(self: *Heap) void {
        // Free all pages for each size class
        for (0..SizeClass.COUNT) |i| {
            var page = self.pages[i];
            while (page) |p| {
                const next = p.next;
                const page_ptr: [*]u8 = @ptrCast(p);
                self.allocator.free(page_ptr[0..self.config.page_size]);
                page = next;
            }
        }

        // Free all large allocations
        var large = self.large_allocs;
        while (large) |l| {
            const next = l.next;
            const total_size = @sizeOf(LargeHeader) + l.size;
            const ptr: [*]u8 = @ptrCast(l);
            self.allocator.free(ptr[0..total_size]);
            large = next;
        }
    }

    /// Allocate memory block with given tag
    pub fn alloc(self: *Heap, tag: MemTag, size: usize) ?*anyopaque {
        const total_size = size + @sizeOf(MemBlockHeader);
        const size_class = SizeClass.fromSize(total_size);

        if (size_class == .large) {
            return self.allocLarge(tag, size);
        }

        return self.allocSmall(tag, size_class);
    }

    /// Allocate from size class free list or new page
    fn allocSmall(self: *Heap, tag: MemTag, size_class: SizeClass) ?*anyopaque {
        const idx = size_class.index();

        // Try free list first
        if (self.free_lists[idx]) |node| {
            self.free_lists[idx] = node.next;

            // Initialize header
            const header: *MemBlockHeader = @ptrCast(node);
            header.* = .{
                .tag = tag,
                .gc_mark = false,
                .size_words = @intCast(size_class.slotSize() / 8),
            };

            self.stats.small_alloc_count += 1;
            self.stats.total_allocated += size_class.slotSize();

            // Return pointer after header
            const ptr: [*]u8 = @ptrCast(node);
            return ptr + @sizeOf(MemBlockHeader);
        }

        // Allocate new page
        if (!self.allocPage(size_class)) {
            return null;
        }

        // Retry with new page
        return self.allocSmall(tag, size_class);
    }

    /// Allocate a new page for given size class
    fn allocPage(self: *Heap, size_class: SizeClass) bool {
        const page_bytes = self.allocator.alloc(u8, self.config.page_size) catch return false;
        const slot_size = size_class.slotSize();
        const header_size = @sizeOf(PageHeader);
        const usable_space = self.config.page_size - header_size;
        const num_slots: u32 = @intCast(usable_space / slot_size);

        // Initialize page header
        const page: *PageHeader = @ptrCast(@alignCast(page_bytes.ptr));
        const idx = size_class.index();
        page.* = .{
            .next = self.pages[idx],
            .size_class = size_class,
            .used_count = 0,
            .total_slots = num_slots,
        };
        self.pages[idx] = page;

        // Add all slots to free list
        var offset: usize = header_size;
        var i: u32 = 0;
        while (i < num_slots) : (i += 1) {
            const node: *FreeNode = @ptrCast(@alignCast(page_bytes.ptr + offset));
            node.next = self.free_lists[idx];
            self.free_lists[idx] = node;
            offset += slot_size;
        }

        self.stats.page_count += 1;
        return true;
    }

    /// Allocate large block (> 512 bytes)
    fn allocLarge(self: *Heap, tag: MemTag, size: usize) ?*anyopaque {
        const total_size = @sizeOf(LargeHeader) + size;
        const mem = self.allocator.alloc(u8, total_size) catch return null;

        const header: *LargeHeader = @ptrCast(@alignCast(mem.ptr));
        header.* = .{
            .next = self.large_allocs,
            .prev = null,
            .size = size,
            .tag = tag,
            .gc_mark = false,
        };

        if (self.large_allocs) |first| {
            first.prev = header;
        }
        self.large_allocs = header;

        self.stats.large_alloc_count += 1;
        self.stats.total_allocated += size;

        return mem.ptr + @sizeOf(LargeHeader);
    }

    /// Free memory block
    pub fn free(self: *Heap, ptr: *anyopaque) void {
        // Get header before the data
        const data_ptr: [*]u8 = @ptrCast(ptr);
        const header: *MemBlockHeader = @ptrCast(@alignCast(data_ptr - @sizeOf(MemBlockHeader)));

        const size = header.sizeBytes();
        const size_class = SizeClass.fromSize(size);

        if (size_class == .large) {
            self.freeLarge(ptr);
        } else {
            self.freeSmall(ptr, size_class);
        }
    }

    /// Return small block to free list
    fn freeSmall(self: *Heap, ptr: *anyopaque, size_class: SizeClass) void {
        const idx = size_class.index();

        // Get the full block (including header)
        const data_ptr: [*]u8 = @ptrCast(ptr);
        const block_ptr = data_ptr - @sizeOf(MemBlockHeader);

        // Add to free list
        const node: *FreeNode = @ptrCast(@alignCast(block_ptr));
        node.next = self.free_lists[idx];
        self.free_lists[idx] = node;

        self.stats.total_freed += size_class.slotSize();
    }

    /// Free large allocation
    fn freeLarge(self: *Heap, ptr: *anyopaque) void {
        const data_ptr: [*]u8 = @ptrCast(ptr);
        const header: *LargeHeader = @ptrCast(@alignCast(data_ptr - @sizeOf(LargeHeader)));

        // Unlink from list
        if (header.prev) |prev| {
            prev.next = header.next;
        } else {
            self.large_allocs = header.next;
        }
        if (header.next) |next| {
            next.prev = header.prev;
        }

        self.stats.total_freed += header.size;

        const total_size = @sizeOf(LargeHeader) + header.size;
        const full_ptr: [*]u8 = @ptrCast(header);
        self.allocator.free(full_ptr[0..total_size]);
    }

    /// Get memory tag for a pointer
    pub fn getTag(self: *Heap, ptr: *anyopaque) ?MemTag {
        _ = self;
        const data_ptr: [*]u8 = @ptrCast(ptr);
        const header: *MemBlockHeader = @ptrCast(@alignCast(data_ptr - @sizeOf(MemBlockHeader)));
        return header.tag;
    }

    /// Set GC mark bit
    pub fn setMark(self: *Heap, ptr: *anyopaque, marked: bool) void {
        _ = self;
        const data_ptr: [*]u8 = @ptrCast(ptr);
        const header: *MemBlockHeader = @ptrCast(@alignCast(data_ptr - @sizeOf(MemBlockHeader)));
        header.gc_mark = marked;
    }

    /// Get GC mark bit
    pub fn getMark(self: *Heap, ptr: *anyopaque) bool {
        _ = self;
        const data_ptr: [*]u8 = @ptrCast(ptr);
        const header: *MemBlockHeader = @ptrCast(@alignCast(data_ptr - @sizeOf(MemBlockHeader)));
        return header.gc_mark;
    }

    /// Allocate a Float64 box (raw allocation without extra header)
    pub fn allocFloat64(self: *Heap, val: f64) ?*value.JSValue.Float64Box {
        // Float64Box has its own header, so allocate raw without additional header
        const size = @sizeOf(value.JSValue.Float64Box);
        const size_class = SizeClass.fromSize(size);

        if (size_class == .large) {
            // Large allocation path - won't happen for Float64Box but handle it anyway
            return null;
        }

        const idx = size_class.index();

        // Try free list first
        if (self.free_lists[idx]) |node| {
            self.free_lists[idx] = node.next;
            self.stats.small_alloc_count += 1;
            self.stats.total_allocated += size_class.slotSize();

            const box: *value.JSValue.Float64Box = @ptrCast(@alignCast(node));
            box.* = .{
                .header = MemBlockHeader.init(.float64, size),
                ._pad = 0,
                .value = val,
            };
            return box;
        }

        // Allocate new page
        if (!self.allocPage(size_class)) {
            return null;
        }

        // Retry
        return self.allocFloat64(val);
    }

    /// Allocate raw memory without header (for objects with embedded headers)
    pub fn allocRaw(self: *Heap, size: usize) ?*anyopaque {
        const size_class = SizeClass.fromSize(size);

        if (size_class == .large) {
            // For raw large allocations, just use backing allocator
            const mem = self.allocator.alloc(u8, size) catch return null;
            self.stats.large_alloc_count += 1;
            self.stats.total_allocated += size;
            return mem.ptr;
        }

        const idx = size_class.index();

        // Try free list first
        if (self.free_lists[idx]) |node| {
            self.free_lists[idx] = node.next;
            self.stats.small_alloc_count += 1;
            self.stats.total_allocated += size_class.slotSize();
            return @ptrCast(node);
        }

        // Allocate new page
        if (!self.allocPage(size_class)) {
            return null;
        }

        return self.allocRaw(size);
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

test "Heap small allocation" {
    const allocator = std.testing.allocator;
    var heap = Heap.init(allocator, .{});
    defer heap.deinit();

    // Allocate small object
    const ptr1 = heap.alloc(.object, 8);
    try std.testing.expect(ptr1 != null);

    // Allocate another
    const ptr2 = heap.alloc(.object, 8);
    try std.testing.expect(ptr2 != null);
    try std.testing.expect(ptr1 != ptr2);

    // Stats updated
    try std.testing.expectEqual(@as(usize, 2), heap.stats.small_alloc_count);
    try std.testing.expectEqual(@as(usize, 1), heap.stats.page_count);
}

test "Heap large allocation" {
    const allocator = std.testing.allocator;
    var heap = Heap.init(allocator, .{});
    defer heap.deinit();

    // Allocate large block
    const ptr = heap.alloc(.byte_array, 1024);
    try std.testing.expect(ptr != null);

    try std.testing.expectEqual(@as(usize, 1), heap.stats.large_alloc_count);
    try std.testing.expectEqual(@as(usize, 1024), heap.stats.total_allocated);
}

test "Heap free and reuse" {
    const allocator = std.testing.allocator;
    var heap = Heap.init(allocator, .{});
    defer heap.deinit();

    // Allocate
    const ptr1 = heap.alloc(.object, 8).?;

    // Free
    heap.free(ptr1);

    // Allocate again - should get same memory from free list
    const ptr2 = heap.alloc(.object, 8).?;
    try std.testing.expectEqual(ptr1, ptr2);
}

test "Heap multiple size classes" {
    const allocator = std.testing.allocator;
    var heap = Heap.init(allocator, .{});
    defer heap.deinit();

    // Allocate different sizes
    const small = heap.alloc(.object, 8);
    const medium = heap.alloc(.object, 64);
    const larger = heap.alloc(.object, 256);

    try std.testing.expect(small != null);
    try std.testing.expect(medium != null);
    try std.testing.expect(larger != null);

    // Different pages for different size classes
    try std.testing.expectEqual(@as(usize, 3), heap.stats.page_count);
}

test "Heap GC mark bit" {
    const allocator = std.testing.allocator;
    var heap = Heap.init(allocator, .{});
    defer heap.deinit();

    const ptr = heap.alloc(.object, 32).?;

    // Initially unmarked
    try std.testing.expect(!heap.getMark(ptr));

    // Mark it
    heap.setMark(ptr, true);
    try std.testing.expect(heap.getMark(ptr));

    // Unmark it
    heap.setMark(ptr, false);
    try std.testing.expect(!heap.getMark(ptr));
}

test "Heap tag retrieval" {
    const allocator = std.testing.allocator;
    var heap = Heap.init(allocator, .{});
    defer heap.deinit();

    const obj_ptr = heap.alloc(.object, 32).?;
    const str_ptr = heap.alloc(.string, 32).?;
    const arr_ptr = heap.alloc(.value_array, 32).?;

    try std.testing.expectEqual(MemTag.object, heap.getTag(obj_ptr).?);
    try std.testing.expectEqual(MemTag.string, heap.getTag(str_ptr).?);
    try std.testing.expectEqual(MemTag.value_array, heap.getTag(arr_ptr).?);
}

test "Heap allocFloat64" {
    const allocator = std.testing.allocator;
    var heap = Heap.init(allocator, .{});
    defer heap.deinit();

    const box = heap.allocFloat64(3.14159);
    try std.testing.expect(box != null);
    try std.testing.expectEqual(@as(f64, 3.14159), box.?.value);
}
