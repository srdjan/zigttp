//! Server-owned lifecycle for the dedicated WebSocket frame-loop threads.
//!
//! Admission reserves capacity before the HTTP 101 response is written. Every
//! accepted worker remains joinable and owns a duplicated control fd which the
//! server can shut down to wake a blocked frame read. The duplicated fd is
//! closed by the worker while holding the registry lock, so shutdown never
//! races a stale descriptor that the OS may have reused.

const std = @import("std");
const engine = @import("engine_adapter.zig");
const websocket_pool = @import("websocket_pool.zig");
const ws_frame_loop = @import("ws_frame_loop.zig");

pub const Lifecycle = enum {
    accepting,
    stopping,
    joined,
};

pub const Stats = struct {
    lifecycle: Lifecycle,
    live: usize,
    peak_live: usize,
    rejected_at_capacity: u64,
};

pub const ReserveError = error{
    AtCapacity,
    Stopping,
    OutOfMemory,
};

const StartSignal = enum(u8) {
    pending,
    run,
    cancel,
};

const Spawner = *const fn (*Worker) anyerror!std.Thread;
const SocketInterrupter = *const fn (*Worker) void;

const Worker = struct {
    owner: *Registry,
    config: ws_frame_loop.Config,
    control_fd: std.posix.fd_t,
    thread: std.Thread = undefined,
    start_signal: std.atomic.Value(u8) = .init(@intFromEnum(StartSignal.pending)),
    handoff_released: std.atomic.Value(bool) = .init(false),
    handle_active: bool = true,
    finished: bool = false,
};

pub const WorkerHandle = *Worker;

pub const Reservation = struct {
    registry: *Registry,
    active: bool = true,

    pub fn cancel(self: *Reservation) void {
        if (!self.active) return;
        self.registry.cancelReservation();
        self.active = false;
    }
};

pub const Registry = struct {
    allocator: std.mem.Allocator,
    max_connections: usize,
    workers: std.ArrayListUnmanaged(*Worker) = .empty,
    mutex: engine.Mutex = .{},
    lifecycle: Lifecycle = .accepting,
    live: usize = 0,
    pending_reservations: usize = 0,
    pending_handoffs: usize = 0,
    peak_live: usize = 0,
    rejected_at_capacity: u64 = 0,
    spawner: Spawner = defaultSpawner,
    socket_interrupter: SocketInterrupter = interruptSocket,

    pub fn init(allocator: std.mem.Allocator, max_connections: usize) Registry {
        return .{
            .allocator = allocator,
            .max_connections = max_connections,
        };
    }

    fn initWithSpawner(allocator: std.mem.Allocator, max_connections: usize, spawner: Spawner) Registry {
        var registry = init(allocator, max_connections);
        registry.spawner = spawner;
        return registry;
    }

    fn initWithSocketInterrupter(
        allocator: std.mem.Allocator,
        max_connections: usize,
        socket_interrupter: SocketInterrupter,
    ) Registry {
        var registry = init(allocator, max_connections);
        registry.socket_interrupter = socket_interrupter;
        return registry;
    }

    pub fn deinit(self: *Registry) void {
        self.stopAndJoin();
        self.workers.deinit(self.allocator);
    }

    /// Reserve one live-worker slot before validating or completing an HTTP
    /// upgrade. A zero maximum disables WebSocket admission explicitly.
    pub fn reserve(self: *Registry) ReserveError!Reservation {
        self.reapFinished();

        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.lifecycle != .accepting) return ReserveError.Stopping;
        if (self.live >= self.max_connections) {
            self.rejected_at_capacity += 1;
            return ReserveError.AtCapacity;
        }
        // Every outstanding reservation still owes the list one slot, and
        // `spawn` appends without checking capacity, so reserve for all of
        // them plus this one.
        self.workers.ensureUnusedCapacity(
            self.allocator,
            self.pending_reservations + 1,
        ) catch return ReserveError.OutOfMemory;
        self.live += 1;
        self.pending_reservations += 1;
        self.peak_live = @max(self.peak_live, self.live);
        return .{ .registry = self };
    }

    /// Spawn a joinable worker in a gated state. Call `start` only after the
    /// 101 response reaches the peer; call `cancel` if the handshake write
    /// fails. All failures roll back reservation capacity and fd ownership.
    pub fn spawn(
        self: *Registry,
        reservation: *Reservation,
        config: ws_frame_loop.Config,
    ) anyerror!WorkerHandle {
        std.debug.assert(reservation.active);
        std.debug.assert(reservation.registry == self);

        const control_fd = std.c.dup(config.fd);
        if (control_fd < 0) {
            reservation.cancel();
            return error.DuplicateSocketFailed;
        }

        const worker = self.allocator.create(Worker) catch |err| {
            std.Io.Threaded.closeFd(control_fd);
            reservation.cancel();
            return err;
        };
        errdefer self.allocator.destroy(worker);
        worker.* = .{
            .owner = self,
            .config = config,
            .control_fd = control_fd,
        };

        const thread = self.spawner(worker) catch |err| {
            std.Io.Threaded.closeFd(control_fd);
            reservation.cancel();
            return err;
        };
        worker.thread = thread;

        self.mutex.lock();
        self.workers.appendAssumeCapacity(worker);
        self.pending_reservations -= 1;
        self.pending_handoffs += 1;
        self.mutex.unlock();
        reservation.active = false;
        return worker;
    }

    /// Release the gated worker into the frame loop. False means shutdown won
    /// the race and the worker will close the un-upgraded connection instead.
    pub fn start(self: *Registry, worker: WorkerHandle) bool {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (!worker.handle_active) return false;
        const started = worker.start_signal.cmpxchgStrong(
            @intFromEnum(StartSignal.pending),
            @intFromEnum(StartSignal.run),
            .release,
            .acquire,
        ) == null;
        self.releaseHandleLocked(worker);
        return started;
    }

    /// Cancel a spawned-but-not-started worker, or wake one that already began.
    pub fn cancel(self: *Registry, worker: WorkerHandle) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (!worker.handle_active) return;
        requestStopLocked(worker);
        self.releaseHandleLocked(worker);
    }

    pub fn stopAccepting(self: *Registry) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.lifecycle == .accepting) self.lifecycle = .stopping;
    }

    /// Stop all workers and join them before any server state borrowed by their
    /// Config values is destroyed. Safe to call repeatedly.
    pub fn stopAndJoin(self: *Registry) void {
        self.stopAccepting();

        // A reservation is held only across validation, registration, and
        // thread creation. Wait for those paths to commit or roll back before
        // taking the stable worker set.
        while (true) {
            self.mutex.lock();
            const pending = self.pending_reservations;
            self.mutex.unlock();
            if (pending == 0) break;
            const ts = std.c.timespec{ .sec = 0, .nsec = std.time.ns_per_ms };
            _ = std.c.nanosleep(&ts, null);
        }

        self.mutex.lock();
        if (self.lifecycle == .joined) {
            self.mutex.unlock();
            return;
        }
        for (self.workers.items) |worker| requestStopLocked(worker);
        self.mutex.unlock();

        // Upgrade threads retain their raw WorkerHandle until the handshake
        // write commits via start() or rolls back via cancel(). The stop signal
        // above shuts down each worker's duplicated socket, which wakes a
        // blocked handshake write; wait for those handles to be relinquished
        // before any Worker allocation can be destroyed.
        while (true) {
            self.mutex.lock();
            const pending = self.pending_handoffs;
            self.mutex.unlock();
            if (pending == 0) break;
            const ts = std.c.timespec{ .sec = 0, .nsec = std.time.ns_per_ms };
            _ = std.c.nanosleep(&ts, null);
        }

        while (self.takeWorker()) |worker| {
            worker.thread.join();
            self.allocator.destroy(worker);
        }

        self.mutex.lock();
        self.lifecycle = .joined;
        self.mutex.unlock();
    }

    pub fn stats(self: *Registry) Stats {
        self.mutex.lock();
        defer self.mutex.unlock();
        return .{
            .lifecycle = self.lifecycle,
            .live = self.live,
            .peak_live = self.peak_live,
            .rejected_at_capacity = self.rejected_at_capacity,
        };
    }

    fn cancelReservation(self: *Registry) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        std.debug.assert(self.pending_reservations > 0);
        std.debug.assert(self.live > 0);
        self.pending_reservations -= 1;
        self.live -= 1;
    }

    fn finish(self: *Registry, worker: *Worker) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (worker.control_fd >= 0) {
            std.Io.Threaded.closeFd(worker.control_fd);
            worker.control_fd = -1;
        }
        worker.finished = true;
        std.debug.assert(self.live > 0);
        self.live -= 1;
    }

    fn takeWorker(self: *Registry) ?*Worker {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.workers.items.len == 0) return null;
        return self.workers.swapRemove(self.workers.items.len - 1);
    }

    fn takeFinished(self: *Registry) ?*Worker {
        self.mutex.lock();
        defer self.mutex.unlock();
        for (self.workers.items, 0..) |worker, i| {
            if (worker.finished and !worker.handle_active) return self.workers.swapRemove(i);
        }
        return null;
    }

    fn releaseHandleLocked(self: *Registry, worker: *Worker) void {
        std.debug.assert(worker.handle_active);
        std.debug.assert(self.pending_handoffs > 0);
        worker.handle_active = false;
        self.pending_handoffs -= 1;
        worker.handoff_released.store(true, .release);
    }

    fn reapFinished(self: *Registry) void {
        while (self.takeFinished()) |worker| {
            worker.thread.join();
            self.allocator.destroy(worker);
        }
    }
};

fn requestStopLocked(worker: *Worker) void {
    const signal: StartSignal = @enumFromInt(worker.start_signal.load(.acquire));
    switch (signal) {
        .pending => {
            // Publish cancellation only after the socket interrupt completes.
            // Observers may then use `.cancel` as the handoff boundary without
            // racing ahead of the shutdown that wakes the upgrade path.
            worker.owner.socket_interrupter(worker);
            worker.start_signal.store(@intFromEnum(StartSignal.cancel), .release);
        },
        .run => worker.owner.socket_interrupter(worker),
        .cancel => {},
    }
}

fn interruptSocket(worker: *Worker) void {
    if (worker.control_fd >= 0) {
        _ = std.c.shutdown(worker.control_fd, std.c.SHUT.RDWR);
    }
}

fn defaultSpawner(worker: *Worker) anyerror!std.Thread {
    return std.Thread.spawn(.{}, workerMain, .{worker});
}

fn workerMain(worker: *Worker) void {
    while (true) {
        const signal: StartSignal = @enumFromInt(worker.start_signal.load(.acquire));
        switch (signal) {
            .pending => {
                const ts = std.c.timespec{ .sec = 0, .nsec = std.time.ns_per_ms };
                _ = std.c.nanosleep(&ts, null);
            },
            .run => {
                ws_frame_loop.run(worker.config);
                break;
            },
            .cancel => {
                // The HTTP upgrade path still owns the original fd until it
                // releases its WorkerHandle after the handshake write. A
                // concurrent close here could let the OS reuse that fd while
                // writeAllFd is still running and redirect bytes to an
                // unrelated connection.
                while (!worker.handoff_released.load(.acquire)) {
                    std.atomic.spinLoopHint();
                }
                worker.config.pool.unregister(worker.config.id);
                std.Io.Threaded.closeFd(worker.config.fd);
                break;
            },
        }
    }
    worker.owner.finish(worker);
}

const testing = std.testing;

test "admission stops exactly at the configured cap and rolls back" {
    var registry = Registry.init(testing.allocator, 1);
    defer registry.deinit();

    var first = try registry.reserve();
    try testing.expectError(ReserveError.AtCapacity, registry.reserve());
    var stats = registry.stats();
    try testing.expectEqual(@as(usize, 1), stats.live);
    try testing.expectEqual(@as(usize, 1), stats.peak_live);
    try testing.expectEqual(@as(u64, 1), stats.rejected_at_capacity);

    first.cancel();
    var replacement = try registry.reserve();
    replacement.cancel();
    stats = registry.stats();
    try testing.expectEqual(@as(usize, 0), stats.live);
}

test "concurrent reservations each keep a spawn slot in reserve" {
    var registry = Registry.init(testing.allocator, 4);
    defer registry.deinit();

    var reservations: [4]Reservation = undefined;
    for (&reservations) |*reservation| reservation.* = try registry.reserve();
    // `spawn` uses appendAssumeCapacity, so the list must already hold a slot
    // for every reservation that has not spawned yet.
    try testing.expect(registry.workers.capacity >= reservations.len);
    for (&reservations) |*reservation| reservation.cancel();
}

test "zero maximum disables WebSocket admission" {
    var registry = Registry.init(testing.allocator, 0);
    defer registry.deinit();
    try testing.expectError(ReserveError.AtCapacity, registry.reserve());
}

test "stopping rejects new WebSocket reservations" {
    var registry = Registry.init(testing.allocator, 1);
    defer registry.deinit();
    registry.stopAccepting();
    try testing.expectError(ReserveError.Stopping, registry.reserve());
}

fn failingSpawner(_: *Worker) anyerror!std.Thread {
    return error.TestThreadSpawnFailure;
}

test "thread spawn failure rolls back capacity and socket ownership" {
    const fds = try testSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);
    defer std.Io.Threaded.closeFd(fds[1]);

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    var pool = websocket_pool.Pool.init(testing.allocator);
    defer pool.deinit();
    const id = try pool.register(fds[0], "/spawn-failure", 0);
    defer pool.unregister(id);

    var registry = Registry.initWithSpawner(testing.allocator, 1, failingSpawner);
    defer registry.deinit();
    var reservation = try registry.reserve();
    try testing.expectError(error.TestThreadSpawnFailure, registry.spawn(&reservation, .{
        .pool = &pool,
        .io = io_backend.io(),
        .fd = fds[0],
        .id = id,
        .alloc = testing.allocator,
    }));
    try testing.expectEqual(@as(usize, 0), registry.stats().live);
}

test "shutdown wakes and joins a live idle WebSocket worker" {
    const fds = try testSocketPair();
    defer std.Io.Threaded.closeFd(fds[1]);

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    var pool = websocket_pool.Pool.init(testing.allocator);
    defer pool.deinit();
    const id = try pool.register(fds[0], "/idle", 0);

    var registry = Registry.init(testing.allocator, 1);
    defer registry.deinit();
    var reservation = try registry.reserve();
    const worker = try registry.spawn(&reservation, .{
        .pool = &pool,
        .io = io_backend.io(),
        .fd = fds[0],
        .id = id,
        .alloc = testing.allocator,
    });
    try testing.expect(registry.start(worker));
    registry.stopAndJoin();
    registry.stopAndJoin();

    try testing.expect(pool.snapshot(id) == null);
    const stats = registry.stats();
    try testing.expectEqual(Lifecycle.joined, stats.lifecycle);
    try testing.expectEqual(@as(usize, 0), stats.live);
}

fn stopRegistry(registry: *Registry) void {
    registry.stopAndJoin();
}

fn expectPendingThenInterrupt(worker: *Worker) void {
    if (worker.start_signal.load(.acquire) != @intFromEnum(StartSignal.pending)) {
        @panic("socket interrupted after cancellation was published");
    }
    interruptSocket(worker);
}

test "shutdown cannot free a worker before its upgrade handle is released" {
    const fds = try testSocketPair();
    defer std.Io.Threaded.closeFd(fds[1]);

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    var pool = websocket_pool.Pool.init(testing.allocator);
    defer pool.deinit();
    const id = try pool.register(fds[0], "/pending", 0);

    var registry = Registry.initWithSocketInterrupter(
        testing.allocator,
        1,
        expectPendingThenInterrupt,
    );
    defer registry.deinit();
    var reservation = try registry.reserve();
    const worker = try registry.spawn(&reservation, .{
        .pool = &pool,
        .io = io_backend.io(),
        .fd = fds[0],
        .id = id,
        .alloc = testing.allocator,
    });

    const stop_thread = try std.Thread.spawn(.{}, stopRegistry, .{&registry});
    while (worker.start_signal.load(.acquire) == @intFromEnum(StartSignal.pending)) {
        std.atomic.spinLoopHint();
    }

    // stopAndJoin has canceled the gated worker, but must retain its storage
    // and original fd until this upgrade-side handle is released by start().
    try testing.expect(pool.snapshot(id) != null);
    var byte: [1]u8 = undefined;
    try testing.expectEqual(@as(isize, 0), std.c.read(fds[1], &byte, 1));
    try testing.expect(!registry.start(worker));
    stop_thread.join();
    try testing.expectEqual(Lifecycle.joined, registry.stats().lifecycle);
}

fn testSocketPair() ![2]std.posix.fd_t {
    if (@TypeOf(std.posix.system.socketpair) == void) return error.SkipZigTest;
    var fds: [2]std.posix.fd_t = undefined;
    while (true) switch (std.posix.errno(
        std.posix.system.socketpair(std.posix.AF.UNIX, std.posix.SOCK.STREAM, 0, &fds),
    )) {
        .SUCCESS => return fds,
        .INTR => continue,
        else => |err| return std.posix.unexpectedErrno(err),
    };
}
