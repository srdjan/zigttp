//! Runtime-owned adapter for the zts engine surface used by the HTTP server.
//!
//! Keep direct `zts` and `zruntime` imports here so server code depends on
//! handler execution capabilities instead of engine internals.

const std = @import("std");
const zq = @import("zts");
const zruntime = @import("zruntime.zig");
const http_types = @import("http_types.zig");

pub const Runtime = zruntime.Runtime;
pub const HandlerPool = zruntime.HandlerPool;
pub const RuntimeConfig = zruntime.RuntimeConfig;
pub const ResponseHandle = HandlerPool.ResponseHandle;
pub const HandlerContract = zq.HandlerContract;
pub const HandlerProperties = zq.handler_contract.HandlerProperties;
pub const RuntimePolicy = zq.handler_policy.RuntimePolicy;
pub const SqlQueryInfo = zq.handler_policy.SqlQueryInfo;
pub const normalizedSqlQuery = zq.handler_policy.normalizedSqlQuery;
pub const sqlQueryIsReadOnly = zq.handler_policy.sqlQueryIsReadOnly;
pub const JSValue = zq.JSValue;
pub const Instant = zq.compat.Instant;
pub const Timer = zq.compat.Timer;
pub const Mutex = zq.compat.Mutex;
pub const RwLock = zq.compat.RwLock;

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

pub fn monotonicNowNs() !u64 {
    return zq.compat.monotonicNowNs();
}

pub fn jsInt(value: i32) JSValue {
    return JSValue.fromInt(value);
}

pub fn defaultHandlerProperties() HandlerProperties {
    return .{
        .pure = false,
        .read_only = false,
        .stateless = false,
        .retry_safe = false,
        .deterministic = false,
        .has_egress = false,
    };
}

pub fn contractRuntimePolicy(contract: *const HandlerContract) RuntimePolicy {
    return zq.handler_policy.contractToRuntimePolicy(contract);
}

pub fn activeWebSocketConnection() ?u64 {
    return zruntime.active_ws_connection;
}

pub fn setActiveWebSocketConnection(id: ?u64) void {
    zruntime.active_ws_connection = id;
}

/// Read-and-clear the source line of the most recent handler type fault on this
/// worker thread (set by the runtime at the fault catch). The server's 500 site
/// uses it to include `line:column` in the response body after the runtime is
/// released.
pub fn takeFaultLocation() ?zq.bytecode.LineEntry {
    const loc = zruntime.last_fault_location;
    zruntime.last_fault_location = null;
    return loc;
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

test "takeFaultLocation reads and clears the thread-local fault location" {
    zruntime.last_fault_location = .{ .offset = 5, .line = 12, .column = 3 };
    const taken = takeFaultLocation();
    try std.testing.expect(taken != null);
    try std.testing.expectEqual(@as(u32, 12), taken.?.line);
    try std.testing.expectEqual(@as(u32, 3), taken.?.column);
    // A second take returns null: the location was cleared.
    try std.testing.expect(takeFaultLocation() == null);
}
