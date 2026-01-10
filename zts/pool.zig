//! Lock-free runtime pool for concurrent request handling
//!
//! Provides isolated JavaScript contexts with minimal contention.

const std = @import("std");
const context = @import("context.zig");
const gc = @import("gc.zig");
const heap = @import("heap.zig");
const arena_mod = @import("arena.zig");

/// Pool configuration
pub const PoolConfig = struct {
    /// Maximum number of runtimes in pool
    max_size: usize = 16,
    /// GC configuration for each runtime (used for persistent allocations)
    gc_config: gc.GCConfig = .{},
    /// Context configuration
    ctx_config: context.ContextConfig = .{},
    /// Arena configuration for ephemeral request allocations
    arena_config: arena_mod.ArenaConfig = .{},
    /// Use hybrid allocation (arena for ephemeral, GC for persistent)
    use_hybrid_allocation: bool = true,
};

/// Lock-free runtime pool
pub const LockFreePool = struct {
    slots: []std.atomic.Value(?*Runtime),
    size: std.atomic.Value(usize),
    config: PoolConfig,
    allocator: std.mem.Allocator,

    /// Individual runtime instance
    pub const Runtime = struct {
        // Persistent state (lives across requests)
        ctx: *context.Context,
        gc_state: *gc.GC,
        heap_state: *heap.Heap,

        // Request-scoped state (reset per request)
        request_arena: ?*arena_mod.Arena,
        hybrid: ?arena_mod.HybridAllocator,

        // Runtime state
        in_use: bool,
        use_hybrid: bool,

        // Diagnostics
        request_count: u64,
        peak_arena_usage: usize,
        overflow_count: u64,

        pub fn create(allocator: std.mem.Allocator, config: PoolConfig) !*Runtime {
            const rt = try allocator.create(Runtime);
            errdefer allocator.destroy(rt);

            const gc_state = try allocator.create(gc.GC);
            errdefer allocator.destroy(gc_state);
            gc_state.* = try gc.GC.init(allocator, config.gc_config);
            errdefer gc_state.deinit();

            // Initialize heap for size-class allocation and wire up to GC
            const heap_state = try allocator.create(heap.Heap);
            errdefer allocator.destroy(heap_state);
            heap_state.* = heap.Heap.init(allocator, .{});
            gc_state.setHeap(heap_state);

            // Initialize request arena if hybrid allocation enabled
            var request_arena: ?*arena_mod.Arena = null;
            var hybrid: ?arena_mod.HybridAllocator = null;

            if (config.use_hybrid_allocation) {
                const arena_ptr = try allocator.create(arena_mod.Arena);
                errdefer allocator.destroy(arena_ptr);
                arena_ptr.* = try arena_mod.Arena.init(allocator, config.arena_config);
                request_arena = arena_ptr;

                hybrid = arena_mod.HybridAllocator{
                    .persistent = allocator,
                    .arena = arena_ptr,
                };
            }

            const ctx = try context.Context.init(allocator, gc_state, config.ctx_config);

            rt.* = .{
                .ctx = ctx,
                .gc_state = gc_state,
                .heap_state = heap_state,
                .request_arena = request_arena,
                .hybrid = hybrid,
                .in_use = false,
                .use_hybrid = config.use_hybrid_allocation,
                .request_count = 0,
                .peak_arena_usage = 0,
                .overflow_count = 0,
            };

            // Wire up hybrid allocator to context
            if (rt.hybrid) |*h| {
                ctx.setHybridAllocator(h);
            }

            return rt;
        }

        pub fn destroy(self: *Runtime, allocator: std.mem.Allocator) void {
            self.ctx.deinit();
            self.gc_state.deinit();
            self.heap_state.deinit();
            if (self.request_arena) |arena| {
                arena.deinit();
                allocator.destroy(arena);
            }
            allocator.destroy(self.gc_state);
            allocator.destroy(self.heap_state);
            allocator.destroy(self);
        }

        /// Reset runtime state for reuse - O(1) with hybrid allocation
        pub fn reset(self: *Runtime) void {
            // Clear execution state
            self.ctx.sp = 0;
            self.ctx.call_depth = 0;
            self.ctx.clearException();

            if (self.use_hybrid) {
                // O(1) arena reset - instant cleanup
                if (self.request_arena) |arena| {
                    // Track peak usage before reset
                    const stats = arena.getStats();
                    if (stats.high_watermark > self.peak_arena_usage) {
                        self.peak_arena_usage = stats.high_watermark;
                    }
                    if (stats.overflow_count > 0) {
                        self.overflow_count += stats.overflow_count;
                    }
                    arena.reset();
                }
            } else {
                // Legacy GC-based cleanup
                if (self.gc_state.nursery.used() > self.gc_state.config.nursery_size / 2) {
                    self.gc_state.minorGC();
                }
            }

            self.request_count += 1;
        }

        /// Get arena statistics (if hybrid allocation enabled)
        pub fn getArenaStats(self: *const Runtime) ?arena_mod.ArenaStats {
            if (self.request_arena) |arena| {
                return arena.getStats();
            }
            return null;
        }

        /// Get cumulative runtime statistics
        pub fn getStats(self: *const Runtime) RuntimeStats {
            return .{
                .request_count = self.request_count,
                .peak_arena_usage = self.peak_arena_usage,
                .overflow_count = self.overflow_count,
                .current_arena_stats = self.getArenaStats(),
            };
        }
    };

    /// Runtime statistics
    pub const RuntimeStats = struct {
        request_count: u64,
        peak_arena_usage: usize,
        overflow_count: u64,
        current_arena_stats: ?arena_mod.ArenaStats,
    };

    pub fn init(allocator: std.mem.Allocator, config: PoolConfig) !LockFreePool {
        const slots = try allocator.alloc(std.atomic.Value(?*Runtime), config.max_size);
        for (slots) |*slot| {
            slot.* = std.atomic.Value(?*Runtime).init(null);
        }

        return .{
            .slots = slots,
            .size = std.atomic.Value(usize).init(0),
            .config = config,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *LockFreePool) void {
        // Destroy all pooled runtimes
        for (self.slots) |*slot| {
            if (slot.load(.acquire)) |runtime| {
                runtime.destroy(self.allocator);
            }
        }
        self.allocator.free(self.slots);
    }

    /// Acquire a runtime from pool (creates new if empty)
    pub fn acquire(self: *LockFreePool) !*Runtime {
        // Try to grab from pool (fast path)
        for (self.slots) |*slot| {
            const current = slot.load(.acquire);
            if (current) |runtime| {
                // Try to claim this slot
                if (slot.cmpxchgWeak(current, null, .release, .monotonic) == null) {
                    runtime.in_use = true;
                    return runtime;
                }
            }
        }

        // Create new runtime (slow path)
        const runtime = try Runtime.create(self.allocator, self.config);
        runtime.in_use = true;
        _ = self.size.fetchAdd(1, .monotonic);
        return runtime;
    }

    /// Release a runtime back to pool
    pub fn release(self: *LockFreePool, runtime: *Runtime) void {
        runtime.reset();
        runtime.in_use = false;

        // Try to return to pool
        for (self.slots) |*slot| {
            if (slot.cmpxchgWeak(null, runtime, .release, .monotonic) == null) {
                return; // Successfully pooled
            }
        }

        // Pool full, destroy runtime
        runtime.destroy(self.allocator);
        _ = self.size.fetchSub(1, .monotonic);
    }

    /// Get current pool size
    pub fn getSize(self: *LockFreePool) usize {
        return self.size.load(.monotonic);
    }

    /// Get number of available (idle) runtimes
    pub fn getAvailable(self: *LockFreePool) usize {
        var count: usize = 0;
        for (self.slots) |*slot| {
            if (slot.load(.acquire) != null) {
                count += 1;
            }
        }
        return count;
    }
};

/// Thread-local runtime cache for reduced pool contention
pub threadlocal var thread_local_runtime: ?*LockFreePool.Runtime = null;

/// Acquire runtime with thread-local caching
pub fn acquireWithCache(pool: *LockFreePool) !*LockFreePool.Runtime {
    if (thread_local_runtime) |rt| {
        if (!rt.in_use) {
            rt.in_use = true;
            return rt;
        }
    }
    const rt = try pool.acquire();
    thread_local_runtime = rt;
    return rt;
}

/// Release runtime with thread-local caching
pub fn releaseWithCache(pool: *LockFreePool, runtime: *LockFreePool.Runtime) void {
    if (thread_local_runtime == runtime) {
        runtime.reset();
        runtime.in_use = false;
        // Keep in thread-local cache
        return;
    }
    pool.release(runtime);
}

test "LockFreePool basic operations" {
    const allocator = std.testing.allocator;

    var pool = try LockFreePool.init(allocator, .{ .max_size = 4 });
    defer pool.deinit();

    // Acquire and release
    const rt1 = try pool.acquire();
    try std.testing.expect(rt1.in_use);

    pool.release(rt1);
    try std.testing.expect(!rt1.in_use);

    // Should get same runtime back
    const rt2 = try pool.acquire();
    try std.testing.expectEqual(rt1, rt2);
    pool.release(rt2);
}

test "LockFreePool multiple runtimes" {
    const allocator = std.testing.allocator;

    var pool = try LockFreePool.init(allocator, .{ .max_size = 4 });
    defer pool.deinit();

    // Acquire multiple
    const rt1 = try pool.acquire();
    const rt2 = try pool.acquire();
    const rt3 = try pool.acquire();

    try std.testing.expect(rt1 != rt2);
    try std.testing.expect(rt2 != rt3);
    try std.testing.expectEqual(@as(usize, 3), pool.getSize());

    pool.release(rt1);
    pool.release(rt2);
    pool.release(rt3);

    try std.testing.expectEqual(@as(usize, 3), pool.getAvailable());
}

test "LockFreePool beyond pool size creates new runtimes" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var pool = try LockFreePool.init(allocator, .{ .max_size = 2 });

    // Acquire more than pool size - creates new runtimes
    const rt1 = try pool.acquire();
    const rt2 = try pool.acquire();
    const rt3 = try pool.acquire();

    try std.testing.expectEqual(@as(usize, 3), pool.getSize());

    pool.release(rt1);
    pool.release(rt2);
    pool.release(rt3);
}

test "Runtime reset" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var pool = try LockFreePool.init(allocator, .{ .max_size = 2 });

    const rt = try pool.acquire();

    // Reset should complete without error
    rt.reset();

    pool.release(rt);
}

test "Hybrid runtime reset clears arena allocations" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var pool = try LockFreePool.init(allocator, .{ .max_size = 1, .use_hybrid_allocation = true });
    const rt = try pool.acquire();

    try std.testing.expect(rt.request_arena != null);
    const req_arena = rt.request_arena.?;

    // Allocate some ephemeral objects/strings in the request arena
    _ = try rt.ctx.createObject(null);
    _ = try rt.ctx.createString("hello");

    try std.testing.expect(req_arena.usedBytes() > 0);

    rt.reset();

    try std.testing.expectEqual(@as(usize, 0), req_arena.usedBytes());

    pool.release(rt);
}

test "acquireWithCache and releaseWithCache" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var pool = try LockFreePool.init(allocator, .{ .max_size = 4 });

    const rt = try acquireWithCache(&pool);
    try std.testing.expect(rt.in_use);

    releaseWithCache(&pool, rt);
    try std.testing.expect(!rt.in_use);
}
