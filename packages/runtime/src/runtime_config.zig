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
    /// Dev/serve live path only: a contract-derived egress allowlist applied
    /// on top of the embedded (stub) policy by `applyEmbeddedCapabilityPolicy`.
    /// Null on AOT paths, which already carry their egress in
    /// `embedded_handler.capability_policy`. The slice is borrowed; its owner
    /// (the dev server) keeps it alive across the runtimes that read it.
    dev_egress_policy: ?zq.handler_policy.RuntimeAllowList = null,
};

pub fn openTraceFile(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    return zq.file_io.openAppend(allocator, path);
}

pub fn openOplogFile(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        0o644,
    ) catch return error.FileOpenFailed;

    return fd;
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
}

pub fn applyEmbeddedCapabilityPolicy(ctx: *zq.Context, config: RuntimeConfig) void {
    ctx.capability_policy = embedded_handler.capability_policy;
    // Dev/serve override: the embedded policy is the empty stub here, so the
    // contract-derived egress allowlist is the only enforcement in interpreted
    // mode. AOT builds leave dev_egress_policy null and keep the embedded list.
    if (config.dev_egress_policy) |egress| {
        ctx.capability_policy.egress = egress;
    }
}
