//! Generational Garbage Collector
//!
//! Two-generation GC with bump allocation in nursery and mark-sweep in tenured.
//! Incorporates FUGC-inspired SIMD turbosweep for efficient collection.

const std = @import("std");
const heap = @import("heap.zig");
const value = @import("value.zig");
const object = @import("object.zig");

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
    objects: std.ArrayList(?*anyopaque),
    /// Pointer -> slot index mapping for fast marking
    object_index: std.AutoHashMap(*anyopaque, usize),
    /// Free slot indices for reuse
    free_indices: std.ArrayListUnmanaged(usize),
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
            .object_index = std.AutoHashMap(*anyopaque, usize).init(allocator),
            .free_indices = .{},
            .free_lists = [_]?*FreeBlock{null} ** 16,
            .allocated = 0,
            .last_sweep_freed = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *TenuredHeap) void {
        self.objects.deinit(self.allocator);
        self.object_index.deinit();
        self.free_indices.deinit(self.allocator);
        self.allocator.free(self.mark_bitvector);
    }

    /// Register an object in tenured space
    pub fn registerObject(self: *TenuredHeap, ptr: *anyopaque) !usize {
        if (self.free_indices.items.len != 0) {
            const idx = self.free_indices.items[self.free_indices.items.len - 1];
            self.free_indices.items.len -= 1;
            self.objects.items[idx] = ptr;
            try self.object_index.put(ptr, idx);
            return idx;
        }
        const idx = self.objects.items.len;
        try self.objects.append(self.allocator, ptr);
        try self.object_index.put(ptr, idx);
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

    pub fn getIndex(self: *const TenuredHeap, ptr: *anyopaque) ?usize {
        return self.object_index.get(ptr);
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
                const obj_opt = self.objects.items[obj_idx];
                if (obj_opt) |obj| {
                    // Call optional callback first (for finalizers)
                    if (free_callback) |cb| {
                        cb(obj);
                    }

                    // CRITICAL: Actually free the memory to prevent leak
                    // Objects in tenured were allocated via heap, so we must use heap.free()
                    if (heap_ptr) |h| {
                        h.free(obj);
                    } else {
                        // Cannot safely free without heap - objects have MemBlockHeader
                        // This is a programming error: majorGC should be called with heap_ptr
                        std.log.warn("GC sweep: cannot free object without heap reference (memory leak)", .{});
                    }

                    _ = self.object_index.remove(obj);
                    self.objects.items[obj_idx] = null;
                    self.free_indices.append(self.allocator, obj_idx) catch {};
                    self.last_sweep_freed += 1;
                }
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

/// Remembered set for cross-generation pointers (tenured -> nursery)
/// Uses a hash set for O(1) deduplication to avoid scanning duplicates during minor GC
pub const RememberedSet = struct {
    entries: std.ArrayList(*anyopaque),
    seen: std.AutoHashMap(*anyopaque, void),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) RememberedSet {
        return .{
            .entries = .empty,
            .seen = std.AutoHashMap(*anyopaque, void).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RememberedSet) void {
        self.entries.deinit(self.allocator);
        self.seen.deinit();
    }

    /// Add a cross-generation pointer (deduplicates automatically)
    pub fn add(self: *RememberedSet, ptr: *anyopaque) !void {
        const result = try self.seen.getOrPut(ptr);
        if (!result.found_existing) {
            try self.entries.append(self.allocator, ptr);
        }
    }

    pub fn clear(self: *RememberedSet) void {
        self.entries.clearRetainingCapacity();
        self.seen.clearRetainingCapacity();
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

/// Float constant pool for common float values
/// Pre-allocates Float64Box instances to avoid repeated allocations
pub const FloatConstantPool = struct {
    zero: value.JSValue.Float64Box,
    one: value.JSValue.Float64Box,
    neg_one: value.JSValue.Float64Box,
    nan: value.JSValue.Float64Box,
    pos_inf: value.JSValue.Float64Box,
    neg_inf: value.JSValue.Float64Box,

    const box_size = @sizeOf(value.JSValue.Float64Box);

    pub fn init() FloatConstantPool {
        return .{
            .zero = .{
                .header = heap.MemBlockHeader.init(.float64, box_size),
                ._pad = 0,
                .value = 0.0,
            },
            .one = .{
                .header = heap.MemBlockHeader.init(.float64, box_size),
                ._pad = 0,
                .value = 1.0,
            },
            .neg_one = .{
                .header = heap.MemBlockHeader.init(.float64, box_size),
                ._pad = 0,
                .value = -1.0,
            },
            .nan = .{
                .header = heap.MemBlockHeader.init(.float64, box_size),
                ._pad = 0,
                .value = std.math.nan(f64),
            },
            .pos_inf = .{
                .header = heap.MemBlockHeader.init(.float64, box_size),
                ._pad = 0,
                .value = std.math.inf(f64),
            },
            .neg_inf = .{
                .header = heap.MemBlockHeader.init(.float64, box_size),
                ._pad = 0,
                .value = -std.math.inf(f64),
            },
        };
    }

    /// Try to get a cached Float64Box for a common value
    /// Returns null if the value is not in the cache
    pub fn get(self: *FloatConstantPool, v: f64) ?*value.JSValue.Float64Box {
        // Check for exact matches of common values
        if (v == 0.0) return &self.zero;
        if (v == 1.0) return &self.one;
        if (v == -1.0) return &self.neg_one;
        if (std.math.isNan(v)) return &self.nan;
        if (std.math.isPositiveInf(v)) return &self.pos_inf;
        if (std.math.isNegativeInf(v)) return &self.neg_inf;
        return null;
    }
};

/// Pool of pre-allocated Upvalue objects for efficient closure creation
/// Uses a free list for O(1) acquire/release operations
pub const UpvaluePool = struct {
    /// Free list of available upvalues (intrusive linked list via next pointer)
    free_list: ?*object.Upvalue = null,
    /// Number of upvalues currently in the free list
    free_count: u32 = 0,
    /// Total upvalues allocated (for stats)
    total_allocated: u32 = 0,
    /// Number of pool hits (reused from free list)
    pool_hits: u64 = 0,
    /// Allocator for new allocations when pool is empty
    allocator: std.mem.Allocator,
    /// Maximum size of the free list (to bound memory usage)
    max_pool_size: u32 = 256,

    pub fn init(allocator: std.mem.Allocator) UpvaluePool {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *UpvaluePool) void {
        // Free all pooled upvalues
        var current = self.free_list;
        while (current) |uv| {
            const next = uv.next;
            self.allocator.destroy(uv);
            current = next;
        }
        self.free_list = null;
        self.free_count = 0;
    }

    /// Acquire an upvalue from the pool or allocate a new one
    pub fn acquire(self: *UpvaluePool) !*object.Upvalue {
        if (self.free_list) |uv| {
            // Fast path: reuse from pool
            self.free_list = uv.next;
            self.free_count -= 1;
            self.pool_hits += 1;
            return uv;
        }
        // Slow path: allocate new
        const uv = try self.allocator.create(object.Upvalue);
        self.total_allocated += 1;
        return uv;
    }

    /// Release an upvalue back to the pool for reuse
    pub fn release(self: *UpvaluePool, uv: *object.Upvalue) void {
        if (self.free_count >= self.max_pool_size) {
            // Pool is full, just free it
            self.allocator.destroy(uv);
            return;
        }
        // Add to free list
        uv.* = .{
            .location = .{ .closed = value.JSValue.undefined_val },
            .next = self.free_list,
        };
        self.free_list = uv;
        self.free_count += 1;
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

    /// Optional heap reference for proper memory deallocation during major GC
    /// When set, majorGC() will properly free swept objects via heap.free()
    heap_ptr: ?*heap.Heap = null,

    /// Forwarding pointers for evacuation (maps old ptr address -> new ptr)
    forwarding_pointers: std.AutoHashMap(usize, *anyopaque),

    /// Float constant pool for common values (0.0, 1.0, -1.0, NaN, Infinity)
    float_pool: FloatConstantPool,

    /// Upvalue pool for efficient closure creation
    upvalue_pool: UpvaluePool,

    /// Statistics
    minor_gc_count: u64 = 0,
    major_gc_count: u64 = 0,
    total_bytes_allocated: usize = 0,
    total_bytes_freed: usize = 0,

    /// Number of float allocations saved by constant pool
    float_pool_hits: u64 = 0,

    /// Hybrid allocation mode - when true, GC is disabled
    /// Arena handles ephemeral allocations, persistent allocations don't need GC
    hybrid_mode: bool = false,

    /// Threshold for automatic major GC (number of tenured objects)
    major_gc_threshold: usize = 10000,

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
            .float_pool = FloatConstantPool.init(),
            .upvalue_pool = UpvaluePool.init(allocator),
            .config = config,
            .allocator = allocator,
            .heap_ptr = null,
        };
    }

    /// Set the heap reference for proper memory deallocation during major GC
    pub fn setHeap(self: *GC, h: *heap.Heap) void {
        self.heap_ptr = h;
    }

    pub fn deinit(self: *GC) void {
        self.forwarding_pointers.deinit();
        self.gray_stack.deinit();
        self.root_set.deinit();
        self.upvalue_pool.deinit();
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
    /// Uses constant pool for common values (0.0, 1.0, -1.0, NaN, Infinity) to avoid allocation
    pub fn allocFloat(self: *GC, v: f64) !*value.JSValue.Float64Box {
        // Fast path: check constant pool for common values
        if (self.float_pool.get(v)) |cached| {
            self.float_pool_hits += 1;
            return cached;
        }

        // Slow path: allocate new Float64Box
        const size = @sizeOf(value.JSValue.Float64Box);
        const ptr = try self.allocWithGC(size);
        const box: *value.JSValue.Float64Box = @ptrCast(@alignCast(ptr));
        box.* = .{
            .header = heap.MemBlockHeader.init(.float64, size),
            ._pad = 0,
            .value = v,
        };
        return box;
    }

    /// Acquire an Upvalue from the pool (or allocate new)
    /// Use this instead of allocator.create(Upvalue) for closure creation
    pub fn acquireUpvalue(self: *GC) !*object.Upvalue {
        return self.upvalue_pool.acquire();
    }

    /// Release an Upvalue back to the pool for reuse
    /// Call this when an upvalue is no longer needed
    pub fn releaseUpvalue(self: *GC, uv: *object.Upvalue) void {
        self.upvalue_pool.release(uv);
    }

    /// Minor GC: evacuate live nursery objects to tenured (Cheney-style copying collector)
    /// Skipped in hybrid mode where arena handles ephemeral allocations
    pub fn minorGC(self: *GC) void {
        // Skip GC in hybrid mode - arena handles ephemeral cleanup
        if (self.hybrid_mode) return;

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

        // 5. Check if major GC is needed to prevent unbounded tenured growth
        self.maybeDoMajorGC();
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
        var worklist: std.ArrayList(*anyopaque) = .empty;
        defer worklist.deinit(self.allocator);

        // Evacuate nursery roots and enqueue new copies for scanning.
        for (self.root_set.iterator()) |root| {
            if (root.isPtr()) {
                const ptr: *anyopaque = @ptrCast(root.toPtr(u8));
                self.evacuateIfNursery(ptr, &worklist);
            }
        }

        // Scan remembered-set entries (tenured objects) for nursery references.
        for (self.remembered_set.entries.items) |ptr| {
            self.scanObjectForEvacuation(ptr, &worklist);
        }

        // Cheney-style scan of evacuated objects to find transitive nursery refs.
        var i: usize = 0;
        while (i < worklist.items.len) : (i += 1) {
            self.scanObjectForEvacuation(worklist.items[i], &worklist);
        }
    }

    fn evacuateIfNursery(self: *GC, ptr: *anyopaque, worklist: *std.ArrayList(*anyopaque)) void {
        if (!self.isInNursery(ptr)) return;
        if (self.getForwardingPointer(ptr)) |_| return;
        const new_ptr = self.evacuateObject(ptr) catch return;
        worklist.append(self.allocator, new_ptr) catch {};
    }

    fn scanObjectForEvacuation(self: *GC, ptr: *anyopaque, worklist: *std.ArrayList(*anyopaque)) void {
        const header: *heap.MemBlockHeader = @ptrCast(@alignCast(ptr));
        switch (header.tag) {
            .object => self.scanJSObjectForEvacuation(ptr, worklist),
            .value_array => self.scanValueArrayForEvacuation(ptr, header.sizeBytes(), worklist),
            .varref => self.scanVarRefForEvacuation(ptr, worklist),
            else => {},
        }
    }

    fn scanJSObjectForEvacuation(self: *GC, ptr: *anyopaque, worklist: *std.ArrayList(*anyopaque)) void {
        const obj: *object.JSObject = @ptrCast(@alignCast(ptr));

        if (obj.prototype) |proto| {
            self.evacuateIfNursery(@ptrCast(proto), worklist);
        }

        for (&obj.inline_slots) |slot| {
            if (slot.isPtr()) {
                const child_ptr: *anyopaque = @ptrCast(slot.toPtr(u8));
                self.evacuateIfNursery(child_ptr, worklist);
            }
        }

        if (obj.overflow_slots) |slots| {
            for (slots[0..obj.overflow_capacity]) |slot| {
                if (slot.isPtr()) {
                    const child_ptr: *anyopaque = @ptrCast(slot.toPtr(u8));
                    self.evacuateIfNursery(child_ptr, worklist);
                }
            }
        }
    }

    fn scanValueArrayForEvacuation(self: *GC, ptr: *anyopaque, size_bytes: usize, worklist: *std.ArrayList(*anyopaque)) void {
        const header_size = @sizeOf(heap.MemBlockHeader);
        const data_size = size_bytes - header_size;
        const num_values = data_size / @sizeOf(value.JSValue);

        if (num_values == 0) return;

        const values_ptr: [*]value.JSValue = @ptrCast(@alignCast(@as([*]u8, @ptrCast(ptr)) + header_size));

        for (values_ptr[0..num_values]) |val| {
            if (val.isPtr()) {
                const child_ptr: *anyopaque = @ptrCast(val.toPtr(u8));
                self.evacuateIfNursery(child_ptr, worklist);
            }
        }
    }

    fn scanVarRefForEvacuation(self: *GC, ptr: *anyopaque, worklist: *std.ArrayList(*anyopaque)) void {
        const upvalue: *object.Upvalue = @ptrCast(@alignCast(ptr));

        switch (upvalue.location) {
            .closed => |val| {
                if (val.isPtr()) {
                    const child_ptr: *anyopaque = @ptrCast(val.toPtr(u8));
                    self.evacuateIfNursery(child_ptr, worklist);
                }
            },
            .open => {},
        }
    }

    /// Evacuate a single object from nursery to tenured
    fn evacuateObject(self: *GC, ptr: *anyopaque) !*anyopaque {
        // Check if already evacuated (has forwarding pointer)
        if (self.getForwardingPointer(ptr)) |fwd| {
            return fwd;
        }

        // Get object size from header (header is embedded at start of object)
        const header: *heap.MemBlockHeader = @ptrCast(@alignCast(ptr));
        const size = header.sizeBytes();
        if (size == 0) return error.InvalidObjectSize;

        // Allocate in tenured space - heap_ptr MUST be set for proper GC operation
        // Without heap reference, evacuated objects cannot be freed during major GC (memory leak)
        const h = self.heap_ptr orelse {
            std.log.err("GC.evacuateObject: heap_ptr not set - call gc.setHeap() after initialization", .{});
            return error.HeapNotConfigured;
        };
        const new_ptr = h.allocRaw(size) orelse return error.OutOfMemory;

        // Copy object data including header
        const src_bytes: [*]const u8 = @ptrCast(ptr);
        const dst_bytes: [*]u8 = @ptrCast(new_ptr);
        @memcpy(dst_bytes[0..size], src_bytes[0..size]);

        // Store forwarding pointer in old location (for pointer updates)
        try self.setForwardingPointer(ptr, new_ptr);

        // Register in tenured heap for tracking
        _ = self.tenured.registerObject(new_ptr) catch {};

        self.total_bytes_allocated += size;

        return new_ptr;
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
        for (self.tenured.objects.items) |obj_opt| {
            if (obj_opt) |obj_ptr| {
                self.updateObjectPointers(obj_ptr);
            }
        }
    }

    /// Update pointers within a single object to point to new locations
    fn updateObjectPointers(self: *GC, ptr: *anyopaque) void {
        // Get object type from header
        const header: *heap.MemBlockHeader = @ptrCast(@alignCast(ptr));

        switch (header.tag) {
            .object => self.updateJSObjectPointers(ptr),
            .value_array => self.updateValueArrayPointers(ptr, header.sizeBytes()),
            .varref => self.updateVarRefPointers(ptr),
            else => {}, // Other types don't contain updateable pointers
        }
    }

    /// Update pointers within a JSObject
    fn updateJSObjectPointers(self: *GC, ptr: *anyopaque) void {
        const obj: *object.JSObject = @ptrCast(@alignCast(ptr));

        // Update prototype pointer
        if (obj.prototype) |proto| {
            if (self.getForwardingPointer(@ptrCast(proto))) |new_ptr| {
                obj.prototype = @ptrCast(@alignCast(new_ptr));
            }
        }

        // Update inline slots
        for (&obj.inline_slots) |*slot| {
            if (slot.isPtr()) {
                const old_ptr: *anyopaque = @ptrCast(slot.toPtr(u8));
                if (self.getForwardingPointer(old_ptr)) |new_ptr| {
                    slot.* = value.JSValue.fromPtr(new_ptr);
                }
            }
        }

        // Update overflow slots
        if (obj.overflow_slots) |slots| {
            for (slots[0..obj.overflow_capacity]) |*slot| {
                if (slot.isPtr()) {
                    const old_ptr: *anyopaque = @ptrCast(slot.toPtr(u8));
                    if (self.getForwardingPointer(old_ptr)) |new_ptr| {
                        slot.* = value.JSValue.fromPtr(new_ptr);
                    }
                }
            }
        }
    }

    /// Update pointers within a value array
    fn updateValueArrayPointers(self: *GC, ptr: *anyopaque, size_bytes: usize) void {
        const header_size = @sizeOf(heap.MemBlockHeader);
        const data_size = size_bytes - header_size;
        const num_values = data_size / @sizeOf(value.JSValue);

        if (num_values == 0) return;

        const values_ptr: [*]value.JSValue = @ptrCast(@alignCast(@as([*]u8, @ptrCast(ptr)) + header_size));

        for (values_ptr[0..num_values]) |*val| {
            if (val.isPtr()) {
                const old_ptr: *anyopaque = @ptrCast(val.toPtr(u8));
                if (self.getForwardingPointer(old_ptr)) |new_ptr| {
                    val.* = value.JSValue.fromPtr(new_ptr);
                }
            }
        }
    }

    /// Update pointers within an upvalue
    fn updateVarRefPointers(self: *GC, ptr: *anyopaque) void {
        const upvalue: *object.Upvalue = @ptrCast(@alignCast(ptr));

        switch (upvalue.location) {
            .closed => |*val| {
                if (val.isPtr()) {
                    const old_ptr: *anyopaque = @ptrCast(val.toPtr(u8));
                    if (self.getForwardingPointer(old_ptr)) |new_ptr| {
                        val.* = value.JSValue.fromPtr(new_ptr);
                    }
                }
            },
            .open => {},
        }
    }

    /// Major GC: mark-sweep on tenured generation
    /// Uses the stored heap_ptr for proper memory deallocation
    /// Skipped in hybrid mode where arena handles ephemeral allocations
    pub fn majorGC(self: *GC) void {
        if (self.hybrid_mode) return;
        self.majorGCWithHeap(self.heap_ptr);
    }

    /// Check if major GC should be triggered based on tenured heap size
    /// Call this after minor GC to prevent unbounded tenured growth
    /// Skipped in hybrid mode
    pub fn maybeDoMajorGC(self: *GC) void {
        if (self.hybrid_mode) return;
        if (self.tenured.objects.items.len > self.major_gc_threshold) {
            self.majorGC();
        }
    }

    /// Major GC with explicit heap reference for memory deallocation
    /// Skipped in hybrid mode
    pub fn majorGCWithHeap(self: *GC, heap_ptr: ?*heap.Heap) void {
        if (self.hybrid_mode) return;
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
            self.markPtr(ptr);
        }
    }

    /// Process gray stack until empty (advancing wavefront)
    fn drainGrayStack(self: *GC) void {
        while (self.gray_stack.pop()) |ptr| {
            self.scanObject(ptr);
        }
    }

    fn markTenured(self: *GC, ptr: *anyopaque) bool {
        const idx = self.tenured.getIndex(ptr) orelse return false;
        if (self.tenured.isMarked(idx)) return false;
        self.tenured.setMark(idx);
        return true;
    }

    fn markPtr(self: *GC, ptr: *anyopaque) void {
        if (self.isInTenured(ptr)) {
            if (self.markTenured(ptr)) {
                self.gray_stack.push(ptr) catch {};
            }
            return;
        }
        // Nursery or unknown pointer: push for scanning
        self.gray_stack.push(ptr) catch {};
    }

    /// Mark a value (if it's a pointer, add to gray stack)
    fn markValue(self: *GC, val: value.JSValue) void {
        if (val.isPtr()) {
            // toPtr returns *T, so we use u8 to get *u8 then cast to *anyopaque
            const ptr: *anyopaque = @ptrCast(val.toPtr(u8));
            self.markPtr(ptr);
        }
    }

    /// Scan an object's children based on its type tag
    /// Traverses all pointer fields and marks child values
    fn scanObject(self: *GC, ptr: *anyopaque) void {
        // Get object type from header
        const header: *heap.MemBlockHeader = @ptrCast(@alignCast(ptr));

        switch (header.tag) {
            .object => self.scanJSObject(ptr),
            .value_array => self.scanValueArray(ptr, header.sizeBytes()),
            .string => {}, // Strings don't contain pointers to other objects
            .float64 => {}, // Float boxes don't contain pointers
            .symbol => {}, // Symbols don't contain GC-managed pointers
            .function_bytecode => {}, // Bytecode is static, constants are in constant pool
            .varref => self.scanVarRef(ptr),
            .byte_array => {}, // Raw bytes don't contain pointers
            .free => {}, // Should not encounter free blocks during marking
        }
    }

    /// Scan a JSObject's slots and prototype chain
    fn scanJSObject(self: *GC, ptr: *anyopaque) void {
        const obj: *object.JSObject = @ptrCast(@alignCast(ptr));

        // Mark prototype (if present)
        if (obj.prototype) |proto| {
            self.markPtr(@ptrCast(proto));
        }

        // Scan inline slots - each may contain pointer values
        for (&obj.inline_slots) |slot| {
            self.markValue(slot);
        }

        // Scan overflow slots (if present)
        if (obj.overflow_slots) |slots| {
            for (slots[0..obj.overflow_capacity]) |slot| {
                self.markValue(slot);
            }
        }
    }

    /// Scan a value array (array of JSValues)
    fn scanValueArray(self: *GC, ptr: *anyopaque, size_bytes: usize) void {
        // Value array: header followed by JSValue elements
        const header_size = @sizeOf(heap.MemBlockHeader);
        const data_size = size_bytes - header_size;
        const num_values = data_size / @sizeOf(value.JSValue);

        if (num_values == 0) return;

        const values_ptr: [*]value.JSValue = @ptrCast(@alignCast(@as([*]u8, @ptrCast(ptr)) + header_size));

        for (values_ptr[0..num_values]) |val| {
            self.markValue(val);
        }
    }

    /// Scan a variable reference (upvalue)
    fn scanVarRef(self: *GC, ptr: *anyopaque) void {
        const upvalue: *object.Upvalue = @ptrCast(@alignCast(ptr));

        // If closed, the value is stored inline
        switch (upvalue.location) {
            .closed => |val| {
                self.markValue(val);
            },
            .open => {}, // Open upvalues point to stack, not heap
        }
    }

    /// Write barrier for cross-generation pointers
    /// Call this when storing a pointer into an old-generation object
    pub fn writeBarrier(self: *GC, old_ptr: *anyopaque, new_val: value.JSValue) !void {
        if (new_val.isPtr()) {
            const new_ptr: *anyopaque = @ptrCast(new_val.toPtr(u8));
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

    // === GC Tuning Hooks for FaaS workloads ===

    /// Get current nursery usage (bytes)
    pub fn getNurseryUsage(self: *const GC) usize {
        return self.nursery.used();
    }

    /// Set threshold for automatic major GC triggering
    pub fn setMajorGCThreshold(self: *GC, threshold: usize) void {
        self.major_gc_threshold = threshold;
    }

    /// Get current major GC threshold
    pub fn getMajorGCThreshold(self: *const GC) usize {
        return self.major_gc_threshold;
    }

    /// Hint GC about expected request allocation size
    /// Adjusts thresholds to trigger earlier collection for large requests
    /// Call at request start, pair with resetRequestHint() at end
    pub fn hintRequestSize(self: *GC, body_len: usize) void {
        // For large request bodies, lower major GC threshold to collect earlier
        // This prevents memory pressure from large JSON parsing, etc.
        if (body_len > self.nursery.size / 2) {
            // Large request: trigger major GC at half normal threshold
            self.major_gc_threshold = @max(1000, self.major_gc_threshold / 2);
        } else if (body_len > self.nursery.size / 4) {
            // Medium request: trigger at 75% normal threshold
            self.major_gc_threshold = @max(1000, self.major_gc_threshold * 3 / 4);
        }
        // Small requests: keep default threshold
    }

    /// Reset GC hints after request completes
    /// Restores default thresholds
    pub fn resetRequestHint(self: *GC) void {
        self.major_gc_threshold = 10000; // Default value
    }

    /// Force a minor GC if nursery usage exceeds watermark
    /// Useful for bounded per-request memory control
    pub fn collectIfAbove(self: *GC, watermark: usize) void {
        if (self.nursery.used() > watermark) {
            self.minorGC();
        }
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

test "FloatConstantPool caching" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    // Allocate common values - should use constant pool
    const zero = try gc_state.allocFloat(0.0);
    const one = try gc_state.allocFloat(1.0);
    const neg_one = try gc_state.allocFloat(-1.0);
    const nan = try gc_state.allocFloat(std.math.nan(f64));
    const pos_inf = try gc_state.allocFloat(std.math.inf(f64));
    const neg_inf = try gc_state.allocFloat(-std.math.inf(f64));

    // Verify values are correct
    try std.testing.expectEqual(@as(f64, 0.0), zero.value);
    try std.testing.expectEqual(@as(f64, 1.0), one.value);
    try std.testing.expectEqual(@as(f64, -1.0), neg_one.value);
    try std.testing.expect(std.math.isNan(nan.value));
    try std.testing.expect(std.math.isPositiveInf(pos_inf.value));
    try std.testing.expect(std.math.isNegativeInf(neg_inf.value));

    // Verify constant pool was used (6 hits)
    try std.testing.expectEqual(@as(u64, 6), gc_state.float_pool_hits);

    // Allocate same values again - should return same pointers from pool
    const zero2 = try gc_state.allocFloat(0.0);
    const one2 = try gc_state.allocFloat(1.0);
    try std.testing.expectEqual(zero, zero2);
    try std.testing.expectEqual(one, one2);
    try std.testing.expectEqual(@as(u64, 8), gc_state.float_pool_hits);

    // Allocate a non-cached value - should allocate new box
    const pi = try gc_state.allocFloat(3.14159);
    try std.testing.expectEqual(@as(f64, 3.14159), pi.value);
    try std.testing.expectEqual(@as(u64, 8), gc_state.float_pool_hits); // No new hit
}

test "UpvaluePool acquire and release" {
    const allocator = std.testing.allocator;
    var pool = UpvaluePool.init(allocator);
    defer pool.deinit();

    // Initially empty pool
    try std.testing.expectEqual(@as(u32, 0), pool.free_count);
    try std.testing.expectEqual(@as(u64, 0), pool.pool_hits);

    // Acquire first upvalue - should allocate new
    const uv1 = try pool.acquire();
    try std.testing.expectEqual(@as(u32, 1), pool.total_allocated);
    try std.testing.expectEqual(@as(u64, 0), pool.pool_hits);

    // Acquire second upvalue - should allocate new
    const uv2 = try pool.acquire();
    try std.testing.expectEqual(@as(u32, 2), pool.total_allocated);

    // Release first upvalue back to pool
    pool.release(uv1);
    try std.testing.expectEqual(@as(u32, 1), pool.free_count);

    // Acquire again - should reuse from pool (hit)
    const uv3 = try pool.acquire();
    try std.testing.expectEqual(@as(u64, 1), pool.pool_hits);
    try std.testing.expectEqual(@as(u32, 0), pool.free_count);

    // uv3 should be the same as uv1 (reused)
    try std.testing.expectEqual(uv1, uv3);

    // Release both remaining upvalues
    pool.release(uv2);
    pool.release(uv3);
    try std.testing.expectEqual(@as(u32, 2), pool.free_count);
}

test "UpvaluePool max size limit" {
    const allocator = std.testing.allocator;
    var pool = UpvaluePool.init(allocator);
    pool.max_pool_size = 2; // Set small limit for testing
    defer pool.deinit();

    // Acquire 3 upvalues
    const uv1 = try pool.acquire();
    const uv2 = try pool.acquire();
    const uv3 = try pool.acquire();

    // Release all 3 - only 2 should be pooled, 1 freed
    pool.release(uv1);
    pool.release(uv2);
    pool.release(uv3); // This one exceeds max, should be freed

    try std.testing.expectEqual(@as(u32, 2), pool.free_count);
}

test "GC major collection" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    // Trigger major GC (should complete without error)
    gc_state.majorGC();

    try std.testing.expectEqual(@as(u64, 1), gc_state.major_gc_count);
    try std.testing.expectEqual(GC.GCPhase.idle, gc_state.phase);
}

test "GC maybeDoMajorGC below threshold" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    // With no tenured allocations, should not trigger major GC
    gc_state.maybeDoMajorGC();

    try std.testing.expectEqual(@as(u64, 0), gc_state.major_gc_count);
}

test "GC multiple minor collections" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 256 });
    defer gc_state.deinit();

    // Run multiple minor GCs
    gc_state.minorGC();
    gc_state.minorGC();
    gc_state.minorGC();

    try std.testing.expectEqual(@as(u64, 3), gc_state.minor_gc_count);
}

test "GC phase transitions" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    // Initial phase is idle
    try std.testing.expectEqual(GC.GCPhase.idle, gc_state.phase);

    // After minor GC, should return to idle
    gc_state.minorGC();
    try std.testing.expectEqual(GC.GCPhase.idle, gc_state.phase);

    // After major GC, should return to idle
    gc_state.majorGC();
    try std.testing.expectEqual(GC.GCPhase.idle, gc_state.phase);
}

test "GC isInNursery" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    // Allocate in nursery
    const ptr = gc_state.allocNursery(64);
    try std.testing.expect(ptr != null);
    try std.testing.expect(gc_state.isInNursery(ptr.?));
}

test "TenuredHeap SIMD sweep" {
    const allocator = std.testing.allocator;
    var tenured = try TenuredHeap.init(allocator, 4096);
    defer tenured.deinit();

    var heap_state = heap.Heap.init(allocator, .{});
    defer heap_state.deinit();

    const obj1 = heap_state.alloc(.object, @sizeOf(u64)) orelse return error.OutOfMemory;
    const obj2 = heap_state.alloc(.object, @sizeOf(u64)) orelse return error.OutOfMemory;

    const idx1 = try tenured.registerObject(obj1);
    const idx2 = try tenured.registerObject(obj2);

    // Mark only first object
    tenured.setMark(idx1);

    // SIMD sweep should clear marks
    tenured.simdSweep(null, &heap_state);

    // After sweep, marks should be cleared
    try std.testing.expect(!tenured.isMarked(idx1));
    try std.testing.expect(!tenured.isMarked(idx2));
}

test "TenuredHeap scalar sweep" {
    const allocator = std.testing.allocator;
    var tenured = try TenuredHeap.init(allocator, 4096);
    defer tenured.deinit();

    var heap_state = heap.Heap.init(allocator, .{});
    defer heap_state.deinit();

    const obj1 = heap_state.alloc(.object, @sizeOf(u64)) orelse return error.OutOfMemory;
    const obj2 = heap_state.alloc(.object, @sizeOf(u64)) orelse return error.OutOfMemory;

    const idx1 = try tenured.registerObject(obj1);
    const idx2 = try tenured.registerObject(obj2);

    // Mark only second object
    tenured.setMark(idx2);

    // Scalar sweep should clear marks
    tenured.scalarSweep(null, &heap_state);

    // After sweep, marks should be cleared
    try std.testing.expect(!tenured.isMarked(idx1));
    try std.testing.expect(!tenured.isMarked(idx2));
}

test "RootSet iterator" {
    const allocator = std.testing.allocator;
    var rs = RootSet.init(allocator);
    defer rs.deinit();

    const val1 = value.JSValue.fromInt(1);
    const val2 = value.JSValue.fromInt(2);
    const val3 = value.JSValue.fromInt(3);

    try rs.addRoot(val1);
    try rs.addRoot(val2);
    try rs.addRoot(val3);

    const roots = rs.iterator();
    try std.testing.expectEqual(@as(usize, 3), roots.len);
}

test "RememberedSet duplicate entries" {
    const allocator = std.testing.allocator;
    var rs = RememberedSet.init(allocator);
    defer rs.deinit();

    var dummy1: u64 = 42;
    var dummy2: u64 = 43;

    // Add same pointer twice - should be deduplicated
    try rs.add(&dummy1);
    try rs.add(&dummy1);

    // Only one entry due to deduplication
    try std.testing.expectEqual(@as(usize, 1), rs.entries.items.len);

    // Add different pointer
    try rs.add(&dummy2);

    // Now should have two entries
    try std.testing.expectEqual(@as(usize, 2), rs.entries.items.len);
}

test "GrayStack clear" {
    const allocator = std.testing.allocator;
    var stack = GrayStack.init(allocator);
    defer stack.deinit();

    var dummy1: u64 = 1;
    var dummy2: u64 = 2;

    try stack.push(&dummy1);
    try stack.push(&dummy2);

    try std.testing.expect(!stack.isEmpty());

    stack.clear();

    try std.testing.expect(stack.isEmpty());
}

test "GC allocNursery returns null when exhausted" {
    const allocator = std.testing.allocator;
    var gc_state = try GC.init(allocator, .{ .nursery_size = 64 });
    defer gc_state.deinit();

    // First allocation should succeed
    const ptr1 = gc_state.allocNursery(32);
    try std.testing.expect(ptr1 != null);

    // Second allocation should succeed
    const ptr2 = gc_state.allocNursery(16);
    try std.testing.expect(ptr2 != null);

    // Third allocation should fail (not enough space)
    const ptr3 = gc_state.allocNursery(32);
    try std.testing.expect(ptr3 == null);
}
