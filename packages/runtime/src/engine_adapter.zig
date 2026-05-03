//! Runtime-owned adapter for the zts engine surface used by the HTTP server.
//!
//! Keep direct `zigts` and `zruntime` imports here so server code depends on
//! handler execution capabilities instead of engine internals.

const std = @import("std");
const zq = @import("zigts");
const zruntime = @import("zruntime.zig");
const http_types = @import("http_types.zig");

pub const Runtime = zruntime.Runtime;
pub const HandlerPool = zruntime.HandlerPool;
pub const RuntimeConfig = zruntime.RuntimeConfig;
pub const ResponseHandle = HandlerPool.ResponseHandle;
pub const Instant = zq.compat.Instant;
pub const Timer = zq.compat.Timer;
pub const Mutex = zq.compat.Mutex;

pub fn readFile(allocator: std.mem.Allocator, path: []const u8, max_bytes: usize) ![]const u8 {
    return zq.file_io.readFile(allocator, path, max_bytes);
}

pub fn initSecurityEvents(allocator: std.mem.Allocator, capacity: usize) !void {
    try zq.security_events.initGlobal(allocator, capacity);
}

pub fn deinitSecurityEvents() void {
    zq.security_events.deinitGlobal();
}

pub fn unixMillis() i64 {
    return zq.trace.unixMillis();
}

pub fn initHandlerPool(
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    handler_code: []const u8,
    handler_filename: []const u8,
    pool_size: usize,
    pool_wait_timeout_ms: u32,
    embedded_bytecode: ?[]const u8,
    runtime_dep_bytecodes: ?[]const []const u8,
) !HandlerPool {
    return HandlerPool.initWithEmbeddedAndDeps(
        allocator,
        config,
        handler_code,
        handler_filename,
        pool_size,
        pool_wait_timeout_ms,
        embedded_bytecode,
        runtime_dep_bytecodes,
    );
}

pub fn executeHandlerBorrowed(pool: *HandlerPool, request: http_types.HttpRequestView) !ResponseHandle {
    return pool.executeHandlerBorrowed(request);
}

pub fn poolInUse(pool: *const HandlerPool) usize {
    return pool.getInUse();
}

pub fn poolCapacity(pool: *const HandlerPool) usize {
    return pool.max_size;
}
