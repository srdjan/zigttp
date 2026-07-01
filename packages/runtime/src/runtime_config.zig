//! Runtime configuration knobs plus the small helpers that translate
//! `RuntimeConfig` into GC/heap/JIT/capability-policy mutations and open
//! the trace / durable-oplog file descriptors.

const std = @import("std");
const zq = @import("zigts");
const embedded_handler = @import("embedded_handler");

pub const RuntimeConfig = struct {
    memory_limit: usize = 0,
    nursery_size: usize = 64 * 1024,
    use_hybrid_allocation: bool = true,
    arena_size: usize = 1024 * 1024,
    enforce_arena_escape: bool = true,
    jit_policy: ?zq.interpreter.JitPolicy = null,
    jit_threshold: ?u32 = null,
    /// Soft cap for native JIT code bytes per runtime context. 0 disables
    /// native-code eviction.
    jit_code_max_bytes: usize = 16 * 1024 * 1024,
    outbound_http_enabled: bool = false,
    outbound_allow_host: ?[]const u8 = null,
    outbound_max_response_bytes: usize = 1024 * 1024,
    outbound_timeout_ms: u32 = 10_000,
    sqlite_path: ?[]const u8 = null,
    trace_file_path: ?[]const u8 = null,
    replay_file_path: ?[]const u8 = null,
    test_file_path: ?[]const u8 = null,
    /// Per-request durable oplog directory. Incomplete oplogs are
    /// replayed on startup; completed ones keep duplicate keys idempotent.
    durable_oplog_dir: ?[]const u8 = null,
    system_config_path: ?[]const u8 = null,
    /// Route durable `zigttp:workflow` child dispatch through a persisted
    /// workflow queue. Requires both `durable_oplog_dir` and `system_config_path`.
    workflow_queue_enabled: bool = false,
    /// In-process co-located handler registry (`*in_process_dispatch.SystemRuntime`)
    /// for `zigttp:workflow.call`. Type-erased to keep this leaf config free of
    /// the runtime-pool import cycle; cast back in `zruntime.installBindings`.
    /// Server-owned and shared by every pooled orchestrator runtime; null when
    /// no `--system` bundle is loaded.
    system_registry: ?*anyopaque = null,
    /// Optional in-process actor queue (`*actor_queue.ActorQueue`) used by
    /// `zigttp:queue`. Type-erased to keep RuntimeConfig independent from the
    /// server/runtime ownership graph. Null keeps the module importable but
    /// makes queue operations return Result errors.
    queue_system: ?*anyopaque = null,
    /// When true, the server creates a process-owned in-memory actor queue and
    /// wires it into every pooled runtime unless queue_system is already set.
    queue_actor_enabled: bool = false,
    /// Actor identity used by `zigttp:queue.receive()` when no actor name is
    /// supplied, and as the source/reply actor for sends from this runtime.
    queue_actor_name: []const u8 = "main",
    /// Mailbox capacity used by server-owned ActorQueue instances.
    queue_capacity: usize = 1024,
    /// Delivery attempts before nack() moves a message to the dead-letter set.
    queue_max_attempts: u32 = 3,
    /// Soft lease window written into message metadata when receive() leases a
    /// message. The in-memory backend keeps the message in-flight until ack/nack.
    queue_lease_ms: i64 = 30_000,
    /// Dev/serve live path only: a contract-derived capability policy applied
    /// instead of the embedded (stub) policy by `applyEmbeddedCapabilityPolicy`.
    /// Null on AOT paths, which already carry their full policy in
    /// `embedded_handler.capability_policy`. Borrowed backing storage is owned
    /// by the dev server and retained across in-flight runtime generations.
    dev_capability_policy: ?zq.handler_policy.RuntimePolicy = null,
    /// Per-request handler execution deadline in ms. 0 = disabled.
    /// Checked cooperatively at interpreter loop back-edges and call entries.
    request_timeout_ms: u32 = 0,
    /// Test-only path that triggers a real panic at the guarded handler
    /// boundary for panic-isolation E2E coverage. Null in normal operation.
    debug_panic_path: ?[]const u8 = null,
};

pub fn openTraceFile(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    return zq.file_io.openAppend(allocator, path);
}

fn openOplogWritable(allocator: std.mem.Allocator, path: []const u8, truncate: bool) !std.c.fd_t {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = truncate },
        0o644,
    ) catch return error.FileOpenFailed;

    return fd;
}

pub fn openOplogFile(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    return openOplogWritable(allocator, path, true);
}

/// Open a fresh oplog for a new live run: create WITHOUT truncating, take the
/// oplog lock, then truncate. open() ignores advisory locks, so truncating
/// before holding the lock would wipe the lines a concurrent same-key holder
/// already appended.
pub fn openLockedFreshOplog(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    const fd = try openOplogWritable(allocator, path, false);
    errdefer std.Io.Threaded.closeFd(fd);
    try tryLockOplogFd(fd);
    if (std.c.ftruncate(fd, 0) != 0) return error.FileOpenFailed;
    return fd;
}

/// Take the same advisory lock durable_recovery's OplogClaim holds during a
/// recovery re-execution, on an already-open oplog fd. Non-blocking: a held
/// lock means either a recovery pass is re-executing this run or another
/// live request owns the same key; mutating the log concurrently from here
/// would double-apply effects. Released automatically when the fd closes.
pub fn tryLockOplogFd(fd: std.c.fd_t) error{DurableRunBusy}!void {
    if (std.c.flock(fd, std.posix.LOCK.EX | std.posix.LOCK.NB) != 0) {
        return error.DurableRunBusy;
    }
}

pub fn openOplogAppendFile(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .APPEND = true },
        0,
    ) catch return error.FileOpenFailed;

    return fd;
}

pub fn applyRuntimeConfig(ctx: *zq.Context, gc_state: *zq.GC, heap_state: *zq.heap.Heap, config: RuntimeConfig) void {
    ctx.enforce_arena_escape = config.enforce_arena_escape;

    // FaaS workloads see less benefit from the stock 10k threshold; the
    // higher value trades memory for fewer mid-request pauses.
    gc_state.setMajorGCThreshold(50_000);

    if (config.memory_limit > 0) {
        gc_state.setMemoryLimit(config.memory_limit);
        heap_state.setMemoryLimit(config.memory_limit);
        if (ctx.hybrid) |h| {
            h.setMemoryLimit(config.memory_limit);
        }
    }

    if (config.jit_policy) |policy| {
        zq.interpreter.setJitPolicy(policy);
    }
    if (config.jit_threshold) |threshold| {
        zq.interpreter.setJitThreshold(threshold);
    }
    ctx.setJitCodeMaxBytes(config.jit_code_max_bytes);

    // Durable execution must stay on the interpreter tier: the JIT loses the
    // suspend (jitCall swallows error.DurableSuspended into a sentinel and runs
    // on), so a hot durable handler would 500 instead of suspending and could
    // fire side effects past a suspend point. Inhibit JIT for the whole context
    // when a durable oplog is configured (the server runs in one mode).
    ctx.jit_inhibited = config.durable_oplog_dir != null;
}

pub fn applyEmbeddedCapabilityPolicy(ctx: *zq.Context, config: RuntimeConfig) void {
    ctx.capability_policy = embedded_handler.capability_policy;
    // Dev/serve override: the embedded policy is the empty stub here, so the
    // full contract-derived policy is the enforcement source in interpreted
    // mode. AOT builds leave dev_capability_policy null and keep the embedded
    // list.
    if (config.dev_capability_policy) |policy| {
        ctx.capability_policy = policy;
    }
}
