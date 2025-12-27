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
    /// Gray stack initial capacity
    gray_stack_capacity: usize = 1024,
};

/// GC colors for tri-color marking
pub const GCColor = enum(u2) {
    white = 0, // Not visited, potentially garbage
    gray = 1, // Visited but children not scanned
    black = 2, // Visited and children scanned
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
    pub fn used(self: *const NurseryHeap) usize {
        return @intFromPtr(self.ptr) - @intFromPtr(self.base);
    }
};

/// Tenured heap with mark-sweep and SIMD turbosweep
pub const TenuredHeap = struct {
    /// Mark bits: 1 bit per object slot, densely packed for SIMD
    mark_bitvector: []u64,
    /// Object slots (indexed by mark bits)
    objects: std.ArrayList(*anyopaque),
    /// Free list heads by size class
    free_lists: [16]?*FreeBlock,
    /// Total allocated bytes
    allocated: usize,
    /// Objects freed in last sweep
    last_sweep_freed: usize,
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
            .objects = .empty,
            .free_lists = [_]?*FreeBlock{null} ** 16,
            .allocated = 0,
            .last_sweep_freed = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *TenuredHeap) void {
        self.objects.deinit(self.allocator);
        self.allocator.free(self.mark_bitvector);
    }

    /// Register an object in tenured space
    pub fn registerObject(self: *TenuredHeap, ptr: *anyopaque) !usize {
        const idx = self.objects.items.len;
        try self.objects.append(self.allocator, ptr);
        return idx;
    }

    /// Set mark bit for object at index
    pub fn setMark(self: *TenuredHeap, idx: usize) void {
        const word_idx = idx / 64;
        const bit_idx: u6 = @intCast(idx % 64);
        if (word_idx < self.mark_bitvector.len) {
            self.mark_bitvector[word_idx] |= (@as(u64, 1) << bit_idx);
        }
    }

    /// Check if object is marked
    pub fn isMarked(self: *TenuredHeap, idx: usize) bool {
        const word_idx = idx / 64;
        const bit_idx: u6 = @intCast(idx % 64);
        if (word_idx < self.mark_bitvector.len) {
            return (self.mark_bitvector[word_idx] & (@as(u64, 1) << bit_idx)) != 0;
        }
        return false;
    }

    /// SIMD turbosweep - process 256 objects per vector operation
    /// Uses Zig's @Vector for portable SIMD
    pub fn simdSweep(self: *TenuredHeap, free_callback: ?*const fn (*anyopaque) void, heap_ptr: ?*heap.Heap) void {
        const VecSize = 4; // 4 x u64 = 256 bits
        const Vec = @Vector(VecSize, u64);

        self.last_sweep_freed = 0;
        var i: usize = 0;

        // Process in SIMD chunks of 256 objects
        while (i + VecSize <= self.mark_bitvector.len) : (i += VecSize) {
            const marks: Vec = self.mark_bitvector[i..][0..VecSize].*;
            const all_zeros: Vec = @splat(0);

            // Check if any unmarked objects in this chunk
            if (@reduce(.Or, marks ^ all_zeros) != ~@as(u64, 0)) {
                // Some objects unmarked - process individually
                for (0..VecSize) |j| {
                    const word = self.mark_bitvector[i + j];
                    const unmarked = ~word;
                    if (unmarked != 0) {
                        self.processUnmarkedWord(i + j, unmarked, free_callback, heap_ptr);
                    }
                }
            }

            // Clear marks for next cycle
            @memset(self.mark_bitvector[i..][0..VecSize], 0);
        }

        // Handle remaining words
        while (i < self.mark_bitvector.len) : (i += 1) {
            const word = self.mark_bitvector[i];
            const unmarked = ~word;
            if (unmarked != 0) {
                self.processUnmarkedWord(i, unmarked, free_callback, heap_ptr);
            }
            self.mark_bitvector[i] = 0;
        }
    }

    fn processUnmarkedWord(self: *TenuredHeap, word_idx: usize, unmarked: u64, free_callback: ?*const fn (*anyopaque) void, heap_ptr: ?*heap.Heap) void {
        var bits = unmarked;
        while (bits != 0) {
            const bit_idx = @ctz(bits);
            const obj_idx = word_idx * 64 + bit_idx;

            if (obj_idx < self.objects.items.len) {
                const obj = self.objects.items[obj_idx];

                // Call optional callback first (for finalizers)
                if (free_callback) |cb| {
                    cb(obj);
                }

                // CRITICAL: Actually free the memory to prevent leak
                if (heap_ptr) |h| {
                    h.free(obj);
                } else {
                    // Fallback: use the allocator directly
                    self.allocator.destroy(@as(*u8, @ptrCast(obj)));
                }

                self.last_sweep_freed += 1;
            }

            bits &= bits - 1; // Clear lowest set bit
        }
    }

    /// Scalar sweep fallback (for small heaps or debugging)
    pub fn scalarSweep(self: *TenuredHeap, free_callback: ?*const fn (*anyopaque) void, heap_ptr: ?*heap.Heap) void {
        self.last_sweep_freed = 0;

        for (self.mark_bitvector, 0..) |mark_word, word_idx| {
            const free_mask = ~mark_word;
            if (free_mask != 0) {
                self.processUnmarkedWord(word_idx, free_mask, free_callback, heap_ptr);
            }
            self.mark_bitvector[word_idx] = 0;
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

/// Root set for GC traversal
pub const RootSet = struct {
    roots: std.ArrayList(value.JSValue),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) RootSet {
        return .{
            .roots = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RootSet) void {
        self.roots.deinit(self.allocator);
    }

    /// Add a root value
    pub fn addRoot(self: *RootSet, val: value.JSValue) !void {
        try self.roots.append(self.allocator, val);
    }

    /// Remove a root value
    pub fn removeRoot(self: *RootSet, val: value.JSValue) void {
        for (self.roots.items, 0..) |v, i| {
            if (v.raw == val.raw) {
                _ = self.roots.swapRemove(i);
                return;
            }
        }
    }

    /// Iterate all roots
    pub fn iterator(self: *const RootSet) []const value.JSValue {
        return self.roots.items;
    }
};

/// Gray stack for tri-color marking
pub const GrayStack = struct {
    stack: std.ArrayList(*anyopaque),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) GrayStack {
        return .{
            .stack = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *GrayStack) void {
        self.stack.deinit(self.allocator);
    }

    pub fn push(self: *GrayStack, ptr: *anyopaque) !void {
        try self.stack.append(self.allocator, ptr);
    }

    pub fn pop(self: *GrayStack) ?*anyopaque {
        return self.stack.pop();
    }

    pub fn isEmpty(self: *const GrayStack) bool {
        return self.stack.items.len == 0;
    }

    pub fn clear(self: *GrayStack) void {
        self.stack.clearRetainingCapacity();
    }
};

/// Main GC structure
pub const GC = struct {
    nursery: NurseryHeap,
    tenured: TenuredHeap,
    remembered_set: RememberedSet,
    root_set: RootSet,
    gray_stack: GrayStack,
    config: GCConfig,
    allocator: std.mem.Allocator,

    /// Forwarding pointers for evacuation (maps old ptr address -> new ptr)
    forwarding_pointers: std.AutoHashMap(usize, *anyopaque),

    /// Statistics
    minor_gc_count: u64 = 0,
    major_gc_count: u64 = 0,
    total_bytes_allocated: usize = 0,
    total_bytes_freed: usize = 0,

    /// Current GC phase (for incremental GC, future)
    phase: GCPhase = .idle,

    pub const GCPhase = enum {
        idle,
        marking,
        sweeping,
    };

    pub fn init(allocator: std.mem.Allocator, config: GCConfig) !GC {
        const nursery_mem = try allocator.alloc(u8, config.nursery_size);
        errdefer allocator.free(nursery_mem);

        var tenured = try TenuredHeap.init(allocator, config.tenured_initial_size);
        errdefer tenured.deinit();

        return .{
            .nursery = NurseryHeap.init(nursery_mem),
            .tenured = tenured,
            .remembered_set = RememberedSet.init(allocator),
            .root_set = RootSet.init(allocator),
            .gray_stack = GrayStack.init(allocator),
            .forwarding_pointers = std.AutoHashMap(usize, *anyopaque).init(allocator),
            .config = config,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *GC) void {
        self.forwarding_pointers.deinit();
        self.gray_stack.deinit();
        self.root_set.deinit();
        self.allocator.free(self.nursery.base[0..self.nursery.size]);
        self.tenured.deinit();
        self.remembered_set.deinit();
    }

    /// Allocate in nursery (fast path)
    pub inline fn allocNursery(self: *GC, size: usize) ?*anyopaque {
        const ptr = self.nursery.alloc(size);
        if (ptr != null) {
            self.total_bytes_allocated += size;
        }
        return ptr;
    }

    /// Allocate with automatic GC trigger
    pub fn allocWithGC(self: *GC, size: usize) !*anyopaque {
        // Try nursery first
        if (self.nursery.alloc(size)) |ptr| {
            self.total_bytes_allocated += size;
            return ptr;
        }

        // Nursery full - trigger minor GC
        self.minorGC();

        // Retry after GC
        if (self.nursery.alloc(size)) |ptr| {
            self.total_bytes_allocated += size;
            return ptr;
        }

        // Still no space - allocation too large for nursery
        return error.OutOfMemory;
    }

    /// Add a GC root
    pub fn addRoot(self: *GC, val: value.JSValue) !void {
        try self.root_set.addRoot(val);
    }

    /// Remove a GC root
    pub fn removeRoot(self: *GC, val: value.JSValue) void {
        self.root_set.removeRoot(val);
    }

    /// Allocate a Float64Box for boxed float values
    pub fn allocFloat(self: *GC, v: f64) !*value.JSValue.Float64Box {
        const size = @sizeOf(value.JSValue.Float64Box);
        const ptr = try self.allocWithGC(size);
        const box: *value.JSValue.Float64Box = @ptrCast(@alignCast(ptr));
        box.* = .{
            .header = 2, // MemTag.float64
            ._pad = 0,
            .value = v,
        };
        return box;
    }

    /// Minor GC: evacuate live nursery objects to tenured (Cheney-style copying collector)
    pub fn minorGC(self: *GC) void {
        self.minor_gc_count += 1;

        // 1. Mark phase: mark all reachable nursery objects
        self.markRoots();
        self.markRememberedSet();
        self.drainGrayStack();

        // 2. Evacuate surviving nursery objects to tenured space
        // This copies live objects and updates pointers via forwarding
        self.evacuateSurvivors();

        // 3. Update all pointers to forwarded objects
        self.updatePointers();

        // 4. Now safe to reset nursery (all live objects are in tenured)
        self.nursery.reset();
        self.remembered_set.clear();
        self.gray_stack.clear();

        // Clear forwarding pointers map for next GC cycle
        self.forwarding_pointers.clearRetainingCapacity();
    }

    /// Forwarding pointer tracking for evacuation
    fn getForwardingPointer(self: *GC, old_ptr: *anyopaque) ?*anyopaque {
        return self.forwarding_pointers.get(@intFromPtr(old_ptr));
    }

    fn setForwardingPointer(self: *GC, old_ptr: *anyopaque, new_ptr: *anyopaque) !void {
        try self.forwarding_pointers.put(@intFromPtr(old_ptr), new_ptr);
    }

    /// Evacuate surviving nursery objects to tenured space
    fn evacuateSurvivors(self: *GC) void {
        // Walk through all objects that were pushed to gray stack (marked as live)
        // For each object in nursery, copy to tenured and record forwarding pointer
        for (self.root_set.iterator()) |root| {
            if (root.isPtr()) {
                const ptr: *anyopaque = @ptrCast(root.toPtr(u8));
                if (self.isInNursery(ptr)) {
                    _ = self.evacuateObject(ptr) catch continue;
                }
            }
        }

        // Also evacuate objects from remembered set
        for (self.remembered_set.entries.items) |ptr| {
            if (self.isInNursery(ptr)) {
                _ = self.evacuateObject(ptr) catch continue;
            }
        }
    }

    /// Evacuate a single object from nursery to tenured
    fn evacuateObject(self: *GC, ptr: *anyopaque) !*anyopaque {
        // Check if already evacuated (has forwarding pointer)
        if (self.getForwardingPointer(ptr)) |fwd| {
            return fwd;
        }

        // Get object size from header
        const header: *heap.MemBlockHeader = @ptrCast(@alignCast(ptr));
        const size = header.sizeBytes();

        // Allocate in tenured space
        const new_ptr = self.allocator.alignedAlloc(u8, 8, size) catch return error.OutOfMemory;

        // Copy object data
        const src_bytes: [*]u8 = @ptrCast(ptr);
        @memcpy(new_ptr[0..size], src_bytes[0..size]);

        // Store forwarding pointer in old location (for pointer updates)
        try self.setForwardingPointer(ptr, new_ptr.ptr);

        // Register in tenured heap for tracking
        _ = self.tenured.registerObject(new_ptr.ptr) catch {};

        self.total_bytes_allocated += size;

        return new_ptr.ptr;
    }

    /// Update all pointers to point to new locations after evacuation
    fn updatePointers(self: *GC) void {
        // Update roots
        for (self.root_set.roots.items, 0..) |root, i| {
            if (root.isPtr()) {
                const old_ptr: *anyopaque = @ptrCast(root.toPtr(u8));
                if (self.getForwardingPointer(old_ptr)) |new_ptr| {
                    // Update the root to point to new location
                    self.root_set.roots.items[i] = value.JSValue.fromPtr(new_ptr);
                }
            }
        }

        // Update remembered set entries
        for (self.remembered_set.entries.items, 0..) |ptr, i| {
            if (self.getForwardingPointer(ptr)) |new_ptr| {
                self.remembered_set.entries.items[i] = new_ptr;
            }
        }

        // Update pointers within tenured objects
        for (self.tenured.objects.items) |obj_ptr| {
            self.updateObjectPointers(obj_ptr);
        }
    }

    /// Update pointers within a single object
    fn updateObjectPointers(self: *GC, ptr: *anyopaque) void {
        // In a real implementation, we'd need object layout information
        // to know which fields are pointers. For now, this is a placeholder.
        // The actual implementation would iterate object slots and update
        // any pointers that have forwarding addresses.
        _ = self;
        _ = ptr;
    }

    /// Major GC: mark-sweep on tenured generation
    /// Optionally accepts a heap pointer for proper memory deallocation
    pub fn majorGC(self: *GC) void {
        self.majorGCWithHeap(null);
    }

    /// Major GC with explicit heap reference for memory deallocation
    pub fn majorGCWithHeap(self: *GC, heap_ptr: ?*heap.Heap) void {
        self.major_gc_count += 1;
        self.phase = .marking;

        // 1. Mark from roots
        self.markRoots();

        // 2. Mark from remembered set
        self.markRememberedSet();

        // 3. Drain gray stack (process all gray objects)
        self.drainGrayStack();

        // 4. Sweep unmarked and FREE memory (CRITICAL fix for memory leak)
        self.phase = .sweeping;
        if (self.config.simd_sweep) {
            self.tenured.simdSweep(null, heap_ptr);
        } else {
            self.tenured.scalarSweep(null, heap_ptr);
        }

        self.total_bytes_freed += self.tenured.last_sweep_freed;
        self.phase = .idle;
    }

    /// Mark all root values
    fn markRoots(self: *GC) void {
        for (self.root_set.iterator()) |root| {
            self.markValue(root);
        }
    }

    /// Mark objects from remembered set
    fn markRememberedSet(self: *GC) void {
        for (self.remembered_set.entries.items) |ptr| {
            self.gray_stack.push(ptr) catch {};
        }
    }

    /// Process gray stack until empty (advancing wavefront)
    fn drainGrayStack(self: *GC) void {
        while (self.gray_stack.pop()) |ptr| {
            self.scanObject(ptr);
        }
    }

    /// Mark a value (if it's a pointer, add to gray stack)
    fn markValue(self: *GC, val: value.JSValue) void {
        if (val.isPtr()) {
            // toPtr returns *T, so we use u8 to get *u8 then cast to *anyopaque
            const ptr: *anyopaque = @ptrCast(val.toPtr(u8));
            // Add to gray stack for processing
            self.gray_stack.push(ptr) catch {};
        }
    }

    /// Scan an object's children (placeholder - needs object layout info)
    fn scanObject(self: *GC, ptr: *anyopaque) void {
        // In a real implementation, we'd:
        // 1. Get the object's type/layout from its header
        // 2. Iterate all pointer fields
        // 3. Mark each child value
        // 4. Mark this object as black (fully scanned)
        _ = self;
        _ = ptr;
    }

    /// Write barrier for cross-generation pointers
    /// Call this when storing a pointer into an old-generation object
    pub fn writeBarrier(self: *GC, old_ptr: *anyopaque, new_val: value.JSValue) !void {
        if (new_val.isPtr()) {
            const new_ptr = new_val.toPtr(*anyopaque);
            // Check if old is in tenured and new is in nursery
            if (self.isInTenured(old_ptr) and self.isInNursery(new_ptr)) {
                try self.remembered_set.add(old_ptr);
            }

            // For advancing wavefront: if we're marking and writing to black object,
            // mark the new value gray (SATB barrier)
            if (self.config.advancing_wavefront and self.phase == .marking) {
                self.markValue(new_val);
            }
        }
    }

    /// Check if pointer is in nursery
    pub fn isInNursery(self: *GC, ptr: *anyopaque) bool {
        const addr = @intFromPtr(ptr);
        const base = @intFromPtr(self.nursery.base);
        return addr >= base and addr < base + self.nursery.size;
    }

    /// Check if pointer is in tenured space
    pub fn isInTenured(self: *GC, ptr: *anyopaque) bool {
        return !self.isInNursery(ptr);
    }

    /// Get GC statistics
    pub fn getStats(self: *const GC) GCStats {
        return .{
            .minor_gc_count = self.minor_gc_count,
            .major_gc_count = self.major_gc_count,
            .nursery_used = self.nursery.used(),
            .nursery_size = self.nursery.size,
            .total_allocated = self.total_bytes_allocated,
            .total_freed = self.total_bytes_freed,
        };
    }

    pub const GCStats = struct {
        minor_gc_count: u64,
        major_gc_count: u64,
        nursery_used: usize,
        nursery_size: usize,
        total_allocated: usize,
        total_freed: usize,
    };
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

test "NurseryHeap overflow" {
    var buf: [64]u8 = undefined;
    var nursery = NurseryHeap.init(&buf);

    // Should succeed
    const ptr1 = nursery.alloc(32);
    try std.testing.expect(ptr1 != null);

    // Should fail (not enough space)
    const ptr2 = nursery.alloc(64);
    try std.testing.expect(ptr2 == null);
}

test "GC initialization" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    try std.testing.expectEqual(@as(u64, 0), gc_state.minor_gc_count);
    try std.testing.expectEqual(@as(u64, 0), gc_state.major_gc_count);
    try std.testing.expectEqual(GC.GCPhase.idle, gc_state.phase);
}

test "GC minor collection" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    // Allocate some memory
    _ = gc_state.allocNursery(100);
    _ = gc_state.allocNursery(200);
    try std.testing.expect(gc_state.nursery.used() > 0);

    // Trigger minor GC
    gc_state.minorGC();

    try std.testing.expectEqual(@as(u64, 1), gc_state.minor_gc_count);
    try std.testing.expectEqual(@as(usize, 0), gc_state.nursery.used());
}

test "GC root management" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    const val1 = value.JSValue.fromInt(42);
    const val2 = value.JSValue.true_val;

    try gc_state.addRoot(val1);
    try gc_state.addRoot(val2);

    try std.testing.expectEqual(@as(usize, 2), gc_state.root_set.roots.items.len);

    gc_state.removeRoot(val1);
    try std.testing.expectEqual(@as(usize, 1), gc_state.root_set.roots.items.len);
}

test "GC allocWithGC triggers collection" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 256 });
    defer gc_state.deinit();

    // Fill nursery
    _ = try gc_state.allocWithGC(100);
    _ = try gc_state.allocWithGC(100);

    // This should trigger GC
    _ = try gc_state.allocWithGC(100);

    try std.testing.expect(gc_state.minor_gc_count >= 1);
}

test "GC statistics" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    _ = gc_state.allocNursery(128);
    _ = gc_state.allocNursery(256);

    const stats = gc_state.getStats();
    try std.testing.expectEqual(@as(usize, 384), stats.total_allocated);
    try std.testing.expectEqual(@as(usize, 4096), stats.nursery_size);
}

test "RememberedSet operations" {
    const allocator = std.testing.allocator;
    var rs = RememberedSet.init(allocator);
    defer rs.deinit();

    var dummy1: u64 = 1;
    var dummy2: u64 = 2;

    try rs.add(&dummy1);
    try rs.add(&dummy2);

    try std.testing.expectEqual(@as(usize, 2), rs.entries.items.len);

    rs.clear();
    try std.testing.expectEqual(@as(usize, 0), rs.entries.items.len);
}

test "GrayStack operations" {
    const allocator = std.testing.allocator;
    var stack = GrayStack.init(allocator);
    defer stack.deinit();

    try std.testing.expect(stack.isEmpty());

    var dummy1: u64 = 1;
    var dummy2: u64 = 2;

    try stack.push(&dummy1);
    try stack.push(&dummy2);

    try std.testing.expect(!stack.isEmpty());

    const p2 = stack.pop();
    try std.testing.expectEqual(&dummy2, @as(*u64, @ptrCast(@alignCast(p2.?))));

    const p1 = stack.pop();
    try std.testing.expectEqual(&dummy1, @as(*u64, @ptrCast(@alignCast(p1.?))));

    try std.testing.expect(stack.isEmpty());
    try std.testing.expect(stack.pop() == null);
}

test "TenuredHeap mark bits" {
    const allocator = std.testing.allocator;
    var tenured = try TenuredHeap.init(allocator, 4096);
    defer tenured.deinit();

    var dummy1: u64 = 1;
    var dummy2: u64 = 2;

    const idx1 = try tenured.registerObject(&dummy1);
    const idx2 = try tenured.registerObject(&dummy2);

    // Initially unmarked
    try std.testing.expect(!tenured.isMarked(idx1));
    try std.testing.expect(!tenured.isMarked(idx2));

    // Mark first object
    tenured.setMark(idx1);
    try std.testing.expect(tenured.isMarked(idx1));
    try std.testing.expect(!tenured.isMarked(idx2));

    // Mark second object
    tenured.setMark(idx2);
    try std.testing.expect(tenured.isMarked(idx1));
    try std.testing.expect(tenured.isMarked(idx2));
}
