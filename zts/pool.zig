//! Lock-free runtime pool for concurrent request handling
//!
//! Provides isolated JavaScript contexts with minimal contention.

const std = @import("std");
const context = @import("context.zig");
const gc = @import("gc.zig");
const heap = @import("heap.zig");

/// Pool configuration
pub const PoolConfig = struct {
    /// Maximum number of runtimes in pool
    max_size: usize = 16,
    /// GC configuration for each runtime
    gc_config: gc.GCConfig = .{},
    /// Context configuration
    ctx_config: context.ContextConfig = .{},
};

/// Lock-free runtime pool
pub const LockFreePool = struct {
    slots: []std.atomic.Value(?*Runtime),
    size: std.atomic.Value(usize),
    config: PoolConfig,
    allocator: std.mem.Allocator,

    /// Individual runtime instance
    pub const Runtime = struct {
        ctx: *context.Context,
        gc_state: *gc.GC,
        heap_state: *heap.Heap,
        in_use: bool,

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

            const ctx = try context.Context.init(allocator, gc_state, config.ctx_config);

            rt.* = .{
                .ctx = ctx,
                .gc_state = gc_state,
                .heap_state = heap_state,
                .in_use = false,
            };

            return rt;
        }

        pub fn destroy(self: *Runtime, allocator: std.mem.Allocator) void {
            self.ctx.deinit();
            self.gc_state.deinit();
            self.heap_state.deinit();
            allocator.destroy(self.gc_state);
            allocator.destroy(self.heap_state);
            allocator.destroy(self);
        }

        /// Reset runtime state for reuse
        pub fn reset(self: *Runtime) void {
            // Clear stack
            self.ctx.sp = 0;
            self.ctx.call_depth = 0;
            self.ctx.clearException();

            // Optionally trigger minor GC
            if (self.gc_state.nursery.used() > self.gc_state.config.nursery_size / 2) {
                self.gc_state.minorGC();
            }
        }
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
