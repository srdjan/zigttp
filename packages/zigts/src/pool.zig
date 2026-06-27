//! Lock-free runtime pool for concurrent request handling
//!
//! Provides isolated JavaScript contexts with minimal contention.

const std = @import("std");
const context = @import("context.zig");
const builtins = @import("builtins/root.zig");
const gc = @import("gc.zig");
const heap = @import("heap.zig");
const arena_mod = @import("arena.zig");
const string = @import("string.zig");

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
    /// Hint for next likely-free slot (reduces linear scanning)
    free_hint: std.atomic.Value(usize),
    /// O(1) count of available (idle) runtimes in pool slots
    available_count: std.atomic.Value(usize),
    config: PoolConfig,
    allocator: std.mem.Allocator,

    /// Individual runtime instance
    pub const Runtime = struct {
        // Persistent state (lives across requests)
        ctx: *context.Context,
        gc_state: *gc.GC,
        heap_state: *heap.Heap,
        strings: string.StringTable,

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
        /// Wall-clock ns of first acquire; null until used. Drives TTL recycling.
        first_use_ns: ?u64 = null,
        /// Set by auditAndResetArena so the next `reset` call skips the
        /// arena work (already done) and avoids a second full-buffer poison
        /// pass in runtime-safety builds. Cleared inside `reset`.
        arena_preaudited: bool = false,
        /// Optional user data for higher-level runtime wrappers
        user_data: ?*anyopaque,
        /// Optional user-data cleanup hook (called before core runtime destroy)
        user_deinit: ?*const fn (*Runtime, std.mem.Allocator) void,

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
            errdefer heap_state.deinit();
            gc_state.setHeap(heap_state);

            // Initialize request arena if hybrid allocation enabled.
            //
            // Zig's `errdefer` is scope-bound: an `errdefer arena.deinit()`
            // placed inside the `if` block only fires when the function
            // exits via error *while still inside that block*. Once the
            // block has exited normally, a later failure in
            // `context.Context.init` or `builtins.initBuiltins` would leak
            // the arena's backing buffer (up to 4 MiB per call). The
            // function-scope errdefer below catches that path via the
            // captured `request_arena` pointer.
            var request_arena: ?*arena_mod.Arena = null;
            var hybrid: ?arena_mod.HybridAllocator = null;
            errdefer if (request_arena) |a| {
                a.deinit();
                allocator.destroy(a);
            };

            if (config.use_hybrid_allocation) {
                const arena_ptr = try allocator.create(arena_mod.Arena);
                errdefer allocator.destroy(arena_ptr);
                arena_ptr.* = try arena_mod.Arena.init(allocator, config.arena_config);
                // From here on, ownership is transferred to `request_arena`
                // and the function-scope errdefer above handles cleanup.
                request_arena = arena_ptr;

                hybrid = arena_mod.HybridAllocator{
                    .persistent = allocator,
                    .arena = arena_ptr,
                };
            }

            const ctx = try context.Context.init(allocator, gc_state, config.ctx_config);
            errdefer ctx.deinit();

            // Install builtins before hybrid allocator is attached so they persist across arena resets.
            try builtins.initBuiltins(ctx);

            rt.* = .{
                .ctx = ctx,
                .gc_state = gc_state,
                .heap_state = heap_state,
                .strings = string.StringTable.init(allocator),
                .request_arena = request_arena,
                .hybrid = hybrid,
                .in_use = false,
                .use_hybrid = config.use_hybrid_allocation,
                .request_count = 0,
                .peak_arena_usage = 0,
                .overflow_count = 0,
                .user_data = null,
                .user_deinit = null,
            };

            // Wire up hybrid allocator to context
            if (rt.hybrid) |*h| {
                ctx.setHybridAllocator(h);
            }

            return rt;
        }

        pub fn destroy(self: *Runtime, allocator: std.mem.Allocator) void {
            // Higher-level wrappers may hold borrowed pointers into this
            // context. Let them clear their state before the engine-owned
            // context, heap, and string table are destroyed.
            if (self.user_deinit) |deinit_fn| {
                deinit_fn(self, allocator);
            }

            // Builtin and bytecode teardown may still walk persistent strings
            // owned by this pooled runtime.
            self.ctx.deinit();
            self.gc_state.deinit();
            self.heap_state.deinit();
            if (self.request_arena) |arena| {
                arena.deinit();
                allocator.destroy(arena);
            }
            allocator.destroy(self.gc_state);
            allocator.destroy(self.heap_state);
            self.strings.deinit();
            allocator.destroy(self);
        }

        /// Reset runtime state for reuse - O(1) with hybrid allocation
        pub fn reset(self: *Runtime) void {
            self.ctx.sp = 0;
            self.ctx.call_depth = 0;
            self.ctx.clearException();

            if (self.use_hybrid) {
                if (self.request_arena) |arena| {
                    if (self.arena_preaudited) {
                        self.arena_preaudited = false;
                    } else {
                        const stats = arena.getStats();
                        if (stats.high_watermark > self.peak_arena_usage) {
                            self.peak_arena_usage = stats.high_watermark;
                        }
                        if (stats.overflow_count > 0) {
                            self.overflow_count += stats.overflow_count;
                        }
                        arena.reset();
                    }
                }
            } else {
                if (self.gc_state.nursery.used() > self.gc_state.config.nursery_size / 2) {
                    self.gc_state.minorGC();
                }
            }

            self.request_count += 1;
        }

        /// Audit-reset the request arena, update cumulative stats, and mark
        /// the runtime so the next `reset` skips the arena work. Returns null
        /// on the legacy GC path.
        pub fn auditAndResetArena(self: *Runtime) arena_mod.Arena.AuditError!?arena_mod.Arena.AuditSnapshot {
            if (!self.use_hybrid) return null;
            const arena = self.request_arena orelse return null;

            const stats = arena.getStats();
            if (stats.high_watermark > self.peak_arena_usage) {
                self.peak_arena_usage = stats.high_watermark;
            }
            if (stats.overflow_count > 0) {
                self.overflow_count += stats.overflow_count;
            }

            const snapshot = try arena.resetWithAudit();
            self.arena_preaudited = true;
            return snapshot;
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

        pub const PersistentStringEscapeError = error{PersistentStringEscapedIntoArena};

        /// Verify no persistent string has drifted into the request arena.
        /// No-op in ReleaseFast (gated on `std.debug.runtime_safety`).
        pub fn assertPersistentStringsOutsideArena(
            self: *const Runtime,
        ) PersistentStringEscapeError!void {
            if (!std.debug.runtime_safety) return;
            const arena = self.request_arena orelse return;
            for (self.strings.strings.items) |str| {
                if (arena.contains(str)) {
                    return error.PersistentStringEscapedIntoArena;
                }
            }
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
            .free_hint = std.atomic.Value(usize).init(0),
            .available_count = std.atomic.Value(usize).init(0),
            .config = config,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *LockFreePool) void {
        // Unpublish idle runtimes before destroying them. Teardown may run
        // higher-level user_deinit hooks; while those hooks execute, the slot
        // must no longer advertise a runtime that is being destroyed.
        for (self.slots, 0..) |*slot, idx| {
            const runtime = slot.swap(null, .acq_rel) orelse continue;

            // Defensive cleanup for stale duplicate publications. Normal
            // release() rejects these in runtime-safety builds, but deinit()
            // should still avoid double-destroying the same runtime if a slot
            // table is already poisoned.
            for (self.slots[idx + 1 ..]) |*later| {
                if (later.load(.acquire) == runtime) {
                    later.store(null, .release);
                }
            }

            runtime.destroy(self.allocator);
        }
        self.allocator.free(self.slots);
    }

    /// Acquire a runtime from pool (creates new if empty)
    pub fn acquire(self: *LockFreePool) !*Runtime {
        const len = self.slots.len;
        const hint = self.free_hint.load(.monotonic);

        // Try hint position first (fast path)
        if (self.tryAcquireSlot(hint)) |runtime| {
            @branchHint(.likely);
            self.free_hint.store((hint + 1) % len, .monotonic);
            return runtime;
        }

        // Scan from hint with wrap-around
        for (1..len) |offset| {
            const idx = (hint + offset) % len;
            if (self.tryAcquireSlot(idx)) |runtime| {
                self.free_hint.store((idx + 1) % len, .monotonic);
                return runtime;
            }
        }

        // Create new runtime (slow path)
        const runtime = try Runtime.create(self.allocator, self.config);
        runtime.in_use = true;
        _ = self.size.fetchAdd(1, .monotonic);
        return runtime;
    }

    /// Try to acquire from a specific slot (returns null if slot empty or CAS fails)
    /// OPTIMIZATION: Removed atomic available_count from hot path
    fn tryAcquireSlot(self: *LockFreePool, idx: usize) ?*Runtime {
        const slot = &self.slots[idx];
        const current = slot.load(.acquire);
        if (current) |runtime| {
            if (slot.cmpxchgWeak(current, null, .release, .monotonic) == null) {
                runtime.in_use = true;
                return runtime;
            }
        }
        return null;
    }

    /// Destroy a runtime without attempting to return it to a pool slot.
    /// Used by higher-level recycle paths (e.g. contract-derived pooling
    /// policies) that want a fresh runtime for the next request.
    pub fn dropRuntime(self: *LockFreePool, runtime: *Runtime) void {
        if (std.debug.runtime_safety) {
            for (self.slots) |*slot| {
                if (slot.load(.acquire) == runtime) {
                    std.debug.panic("runtime dropped while still pooled: 0x{x}", .{@intFromPtr(runtime)});
                }
            }
        }
        runtime.in_use = false;
        runtime.destroy(self.allocator);
        _ = self.size.fetchSub(1, .monotonic);
    }

    /// Release a runtime back to pool
    pub fn release(self: *LockFreePool, runtime: *Runtime) void {
        if (std.debug.runtime_safety) {
            for (self.slots) |*slot| {
                if (slot.load(.acquire) == runtime) {
                    std.debug.panic("runtime released twice: 0x{x}", .{@intFromPtr(runtime)});
                }
            }
        }

        runtime.reset();
        runtime.in_use = false;

        const len = self.slots.len;
        const hint = self.free_hint.load(.monotonic);

        // Try to return to pool, starting from hint. Use cmpxchgStrong (not
        // Weak) so a spurious CAS failure on the only free slot cannot make a
        // single release pass fall through and destroy a still-reusable runtime.
        for (0..len) |offset| {
            const idx = (hint + offset) % len;
            if (self.slots[idx].cmpxchgStrong(null, runtime, .release, .monotonic) == null) {
                // Update hint to point to this slot (next acquire will find it)
                self.free_hint.store(idx, .monotonic);
                // OPTIMIZATION: Removed atomic increment - available_count computed lazily
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

    /// Get number of available (idle) runtimes - O(n) scan, but called rarely.
    /// OPTIMIZATION: Removed atomic counter from hot path (tryAcquireSlot/release).
    /// This function is only called for pool pressure checks which happen infrequently.
    pub fn getAvailable(self: *LockFreePool) usize {
        var count: usize = 0;
        for (self.slots) |*slot| {
            if (slot.load(.monotonic) != null) {
                count += 1;
            }
        }
        return count;
    }
};

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

test "Runtime destroy clears user data before context teardown" {
    const allocator = std.testing.allocator;

    const Hooks = struct {
        const State = struct {
            sequence: *u8,
        };

        fn userDeinit(runtime: *LockFreePool.Runtime, _: std.mem.Allocator) void {
            const sequence: *u8 = @ptrCast(@alignCast(runtime.user_data.?));
            std.debug.assert(sequence.* == 0);
            sequence.* = 1;
            runtime.user_data = null;
            runtime.user_deinit = null;
        }

        fn moduleDeinit(ptr: *anyopaque, alloc: std.mem.Allocator) void {
            const state: *State = @ptrCast(@alignCast(ptr));
            std.debug.assert(state.sequence.* == 1);
            state.sequence.* = 2;
            alloc.destroy(state);
        }
    };

    var sequence: u8 = 0;
    const rt = try LockFreePool.Runtime.create(allocator, .{});
    rt.user_data = &sequence;
    rt.user_deinit = Hooks.userDeinit;

    const state = try allocator.create(Hooks.State);
    state.* = .{ .sequence = &sequence };
    rt.ctx.module_state[0] = .{
        .ptr = state,
        .deinit_fn = Hooks.moduleDeinit,
    };

    rt.destroy(allocator);
    try std.testing.expectEqual(@as(u8, 2), sequence);
}

test "LockFreePool deinit unpublishes runtime before user deinit" {
    const allocator = std.testing.allocator;

    const Hooks = struct {
        const State = struct {
            pool: *LockFreePool,
            saw_published_runtime: bool = false,
        };

        fn userDeinit(runtime: *LockFreePool.Runtime, _: std.mem.Allocator) void {
            const state: *State = @ptrCast(@alignCast(runtime.user_data.?));
            for (state.pool.slots) |*slot| {
                if (slot.load(.acquire) == runtime) {
                    state.saw_published_runtime = true;
                }
            }
            runtime.user_data = null;
            runtime.user_deinit = null;
        }
    };

    var pool = try LockFreePool.init(allocator, .{ .max_size = 1 });
    var state = Hooks.State{ .pool = &pool };

    const rt = try pool.acquire();
    rt.user_data = &state;
    rt.user_deinit = Hooks.userDeinit;
    pool.release(rt);

    pool.deinit();

    try std.testing.expect(!state.saw_published_runtime);
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

    // getAvailable() now scans slots - verify at least the runtimes we released are there
    try std.testing.expect(pool.getAvailable() >= 3);
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

test "pool acquire and release" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var pool = try LockFreePool.init(allocator, .{ .max_size = 4 });

    const rt = try pool.acquire();
    try std.testing.expect(rt.in_use);

    pool.release(rt);
    try std.testing.expect(!rt.in_use);

    // Should reacquire same runtime
    const rt2 = try pool.acquire();
    try std.testing.expectEqual(rt, rt2);
    pool.release(rt2);
}

test "Runtime.create errdefer ladder closes every failure path" {
    // Walk FailingAllocator.fail_index forward through Runtime.create's
    // allocation sequence. testing.allocator catches any leak on the
    // failure path; an incomplete errdefer ladder (e.g. arena_ptr.deinit
    // missing after arena_mod.Arena.init succeeds and a later allocation
    // fails) trips the leak detector on the *first* fail_index that lands
    // past that point. The success path is identical to the existing
    // Runtime tests above — this exercises the rollback side only.
    var fail_at: usize = 0;
    const max_steps: usize = 4096;
    while (fail_at < max_steps) : (fail_at += 1) {
        var leak_detector: std.heap.DebugAllocator(.{ .stack_trace_frames = 0 }) = .init;
        const child = leak_detector.allocator();
        var failing = std.testing.FailingAllocator.init(child, .{ .fail_index = fail_at });
        const result = LockFreePool.Runtime.create(failing.allocator(), .{});
        if (result) |rt| {
            // We've exceeded the allocation count of a successful create();
            // destroy and stop walking.
            rt.destroy(failing.allocator());
            const leak_check = leak_detector.deinit();
            if (leak_check == .leak) std.debug.print("Runtime.create leaked on success after fail_at={d}\n", .{fail_at});
            try std.testing.expectEqual(std.heap.Check.ok, leak_check);
            break;
        } else |err| {
            // Any non-OOM error means our injected failure is being masked
            // by a different error class — the test would no longer be
            // exercising what it claims to.
            try std.testing.expectEqual(error.OutOfMemory, err);
            const leak_check = leak_detector.deinit();
            if (leak_check == .leak) std.debug.print("Runtime.create leaked on fail_at={d}\n", .{fail_at});
            try std.testing.expectEqual(std.heap.Check.ok, leak_check);
        }
    } else {
        // Ran the loop to completion without ever succeeding. Either the
        // runtime got dramatically heavier or the bound is too tight; fail
        // loud rather than silently passing.
        return error.RuntimeCreateAllocationCountExceededTestBound;
    }
}
