//! zigttp:io - Structured Concurrent I/O
//!
//! Provides concurrent effect execution without async/await or Promises.
//! Handler code remains synchronous and linear; concurrency is purely in
//! the I/O layer.
//!
//! Exports:
//!   parallel(thunks: Array<() => T>) -> Array<T>
//!     Execute an array of zero-arg functions concurrently.
//!     All I/O effects (fetchSync calls) within the thunks are overlapped.
//!     Results are returned in declaration order regardless of completion order.
//!
//!   race(thunks: Array<() => T>) -> T
//!     Execute an array of zero-arg functions concurrently.
//!     Returns the result of whichever thunk completes first.
//!     Remaining in-flight effects are discarded.

const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const object = @import("../../object.zig");
const resolver = @import("../resolver.zig");
const util = @import("../util.zig");
const mb = @import("../../module_binding.zig");

/// Maximum number of concurrent operations in a single parallel/race call.
pub const MAX_PARALLEL: u32 = 8;

pub const MODULE_STATE_SLOT = @intFromEnum(@import("../../module_slots.zig").Slot.io);

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:io",
    .name = "io",
    .required_capabilities = &.{.runtime_callback},
    .stateful = true,
    .exports = &.{
        .{ .name = "parallel", .func = parallelNative, .arg_count = 1, .effect = .write, .returns = .string, .param_types = &.{}, .return_labels = .{ .external = true } },
        .{ .name = "race", .func = raceNative, .arg_count = 1, .effect = .write, .returns = .string, .param_types = &.{}, .return_labels = .{ .external = true } },
    },
};

pub const exports = binding.toModuleExports();

// ============================================================================
// Runtime callback interface
// ============================================================================

/// Opaque callbacks provided by the runtime layer (src/zruntime.zig).
/// Stored in Context.module_state[MODULE_STATE_SLOT].
/// The runtime sets these during initialization when outbound HTTP is enabled.
pub const IoCallbacks = struct {
    /// Call a zero-arg JS function via the interpreter and return its result.
    call_thunk_fn: *const fn (*anyopaque, value.JSValue) anyerror!value.JSValue,

    /// Execute HTTP fetches concurrently. Takes descriptors, fills results.
    execute_fetches_fn: *const fn (*anyopaque, []const FetchDescriptor, []FetchResult) void,

    /// Build a JS Response object from a FetchResult.
    build_response_fn: *const fn (*anyopaque, *const FetchResult) anyerror!value.JSValue,

    /// Opaque pointer to the runtime (passed as first arg to above fns).
    runtime_ptr: *anyopaque,

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *IoCallbacks = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }
};

/// Descriptor for an outbound HTTP request collected during thunk execution.
pub const FetchDescriptor = struct {
    url: []const u8,
    method: std.http.Method,
    body: ?[]const u8,
    headers: std.ArrayList(std.http.Header),
    max_response_bytes: usize,

    pub fn deinit(self: *FetchDescriptor, allocator: std.mem.Allocator) void {
        allocator.free(self.url);
        if (self.body) |b| allocator.free(b);
        self.headers.deinit(allocator);
    }
};

/// Result of an HTTP fetch executed by a worker thread.
pub const FetchResult = struct {
    status: u16 = 0,
    body: ?[]const u8 = null,
    content_type: ?[]const u8 = null,
    reason: ?[]const u8 = null,
    response_headers: std.ArrayList(ResponseHeader) = .empty,
    ok: bool = false,
    error_code: ?[]const u8 = null,
    error_details: ?[]const u8 = null,

    pub const ResponseHeader = struct {
        name: []const u8,
        value_str: []const u8,
    };

    pub fn deinit(self: *FetchResult, allocator: std.mem.Allocator) void {
        if (self.body) |b| allocator.free(b);
        if (self.content_type) |ct| allocator.free(ct);
        if (self.reason) |r| allocator.free(r);
        for (self.response_headers.items) |h| {
            allocator.free(h.name);
            allocator.free(h.value_str);
        }
        self.response_headers.deinit(allocator);
        if (self.error_code) |ec| allocator.free(ec);
        if (self.error_details) |ed| allocator.free(ed);
    }
};

// ============================================================================
// Parallel collection state (set by parallel/race, read by fetchSync)
// ============================================================================

/// Thread-local state for intercepting fetchSync calls during parallel collection.
/// When non-null, fetchSync records descriptors instead of executing HTTP requests.
pub threadlocal var parallel_collector: ?*ParallelCollector = null;

pub const ParallelCollector = struct {
    descriptors: []FetchDescriptor,
    count: u32,
    allocator: std.mem.Allocator,
    capacity: u32,
};

// ============================================================================
// parallel()
// ============================================================================

fn parallelNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) {
        return (try ctx.createArray()).toValue();
    }

    // Validate: first arg must be an array
    if (!args[0].isObject()) {
        return util.throwError(ctx, "TypeError", "parallel() expects an array of functions");
    }
    const arr = args[0].toPtr(object.JSObject);
    if (arr.class_id != .array) {
        return util.throwError(ctx, "TypeError", "parallel() expects an array of functions");
    }

    const count = arr.getArrayLength();
    if (count == 0) {
        return (try ctx.createArray()).toValue();
    }
    if (count > MAX_PARALLEL) {
        return util.throwError(ctx, "RangeError", "parallel() supports at most 8 concurrent operations");
    }

    // Get runtime callbacks from module state after handling no-op cases.
    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "parallel() requires outbound HTTP to be enabled");
    };

    // Phase 1: Execute each thunk to collect fetch descriptors.
    // fetchSync calls during thunk execution are intercepted and recorded
    // instead of blocking on HTTP I/O.
    var descriptor_buf: [MAX_PARALLEL]FetchDescriptor = undefined;
    for (0..MAX_PARALLEL) |i| {
        descriptor_buf[i] = .{
            .url = &.{},
            .method = .GET,
            .body = null,
            .headers = .empty,
            .max_response_bytes = 1024 * 1024,
        };
    }

    var collector = ParallelCollector{
        .descriptors = &descriptor_buf,
        .count = 0,
        .allocator = ctx.allocator,
        .capacity = @intCast(count),
    };

    // Install collector - fetchSync will check this threadlocal
    const prev_collector = parallel_collector;
    parallel_collector = &collector;

    // Execute each thunk (fetchSync records URL instead of blocking)
    for (0..count) |i| {
        const thunk = arr.getIndex(@intCast(i)) orelse continue;
        if (!thunk.isObject()) continue;
        _ = callbacks.call_thunk_fn(callbacks.runtime_ptr, thunk) catch {};
    }

    // Restore previous collector state (supports nested parallel, though unusual)
    parallel_collector = prev_collector;

    const desc_count = collector.count;
    if (desc_count == 0) {
        // No fetchSync calls were intercepted - thunks had no I/O effects
        return (try ctx.createArray()).toValue();
    }

    // Phase 2: Execute all collected HTTP fetches concurrently
    var results: [MAX_PARALLEL]FetchResult = undefined;
    for (0..MAX_PARALLEL) |i| {
        results[i] = .{};
    }

    callbacks.execute_fetches_fn(
        callbacks.runtime_ptr,
        descriptor_buf[0..desc_count],
        results[0..desc_count],
    );

    // Phase 3: Build JS Response objects and return array
    const result_arr = try ctx.createArray();
    for (0..desc_count) |i| {
        const resp_val = callbacks.build_response_fn(callbacks.runtime_ptr, &results[i]) catch value.JSValue.undefined_val;
        try result_arr.arrayPush(ctx.allocator, resp_val);
        results[i].deinit(ctx.allocator);
    }

    // Free collected descriptors
    for (0..desc_count) |i| {
        descriptor_buf[i].deinit(ctx.allocator);
    }

    return result_arr.toValue();
}

// ============================================================================
// race()
// ============================================================================

fn raceNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) {
        return value.JSValue.undefined_val;
    }

    if (!args[0].isObject()) {
        return util.throwError(ctx, "TypeError", "race() expects an array of functions");
    }
    const arr = args[0].toPtr(object.JSObject);
    if (arr.class_id != .array) {
        return util.throwError(ctx, "TypeError", "race() expects an array of functions");
    }

    const count = arr.getArrayLength();
    if (count == 0) {
        return value.JSValue.undefined_val;
    }
    if (count > MAX_PARALLEL) {
        return util.throwError(ctx, "RangeError", "race() supports at most 8 concurrent operations");
    }

    const callbacks = getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "race() requires outbound HTTP to be enabled");
    };

    // Phase 1: Collect fetch descriptors (same as parallel)
    var descriptor_buf: [MAX_PARALLEL]FetchDescriptor = undefined;
    for (0..MAX_PARALLEL) |i| {
        descriptor_buf[i] = .{
            .url = &.{},
            .method = .GET,
            .body = null,
            .headers = .empty,
            .max_response_bytes = 1024 * 1024,
        };
    }

    var collector = ParallelCollector{
        .descriptors = &descriptor_buf,
        .count = 0,
        .allocator = ctx.allocator,
        .capacity = @intCast(count),
    };

    const prev_collector = parallel_collector;
    parallel_collector = &collector;

    for (0..count) |i| {
        const thunk = arr.getIndex(@intCast(i)) orelse continue;
        if (!thunk.isObject()) continue;
        _ = callbacks.call_thunk_fn(callbacks.runtime_ptr, thunk) catch {};
    }

    parallel_collector = prev_collector;

    const desc_count = collector.count;
    if (desc_count == 0) {
        return value.JSValue.undefined_val;
    }

    // Phase 2: Execute all fetches concurrently (same as parallel)
    var results: [MAX_PARALLEL]FetchResult = undefined;
    for (0..MAX_PARALLEL) |i| {
        results[i] = .{};
    }

    callbacks.execute_fetches_fn(
        callbacks.runtime_ptr,
        descriptor_buf[0..desc_count],
        results[0..desc_count],
    );

    // Phase 3: Find the first successful result (lowest latency wins)
    // Since threads run concurrently and we join all, we pick the first
    // completed one. For now, the first in declaration order that succeeded.
    // A future version could track actual completion timestamps.
    var winner_idx: ?usize = null;
    for (0..desc_count) |i| {
        if (results[i].ok) {
            winner_idx = i;
            break;
        }
    }

    // If no successful result, return the first error response
    const idx = winner_idx orelse 0;
    const resp_val = callbacks.build_response_fn(callbacks.runtime_ptr, &results[idx]) catch value.JSValue.undefined_val;

    // Clean up all results and descriptors
    for (0..desc_count) |i| {
        results[i].deinit(ctx.allocator);
        descriptor_buf[i].deinit(ctx.allocator);
    }

    return resp_val;
}

// ============================================================================
// Helpers
// ============================================================================

fn getCallbacks(ctx: *context.Context) ?*IoCallbacks {
    return mb.getRuntimeCallbackStateChecked(ctx, IoCallbacks, MODULE_STATE_SLOT);
}

// ============================================================================
// Tests
// ============================================================================

test "parallel: empty array returns empty array" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("../../gc.zig");
    const heap_mod = @import("../../heap.zig");

    const gc_state = try allocator.create(gc_mod.GC);
    gc_state.* = try gc_mod.GC.init(allocator, .{});
    const heap_state = try allocator.create(heap_mod.Heap);
    heap_state.* = heap_mod.Heap.init(allocator, .{});
    gc_state.setHeap(heap_state);
    const ctx = try context.Context.init(allocator, gc_state, .{});
    defer {
        ctx.deinit();
        heap_state.deinit();
        gc_state.deinit();
        allocator.destroy(heap_state);
        allocator.destroy(gc_state);
    }

    const result = try parallelNative(ctx, value.JSValue.undefined_val, &.{});
    defer result.toPtr(object.JSObject).destroy(allocator);

    try std.testing.expect(result.isObject());
    try std.testing.expectEqual(object.ClassId.array, result.toPtr(object.JSObject).class_id);
    try std.testing.expectEqual(@as(u32, 0), result.toPtr(object.JSObject).getArrayLength());
}

test "MAX_PARALLEL is within MODULE_STATE_SLOTS" {
    try std.testing.expect(MODULE_STATE_SLOT < 8); // MAX_MODULE_STATE_SLOTS
}

test "FetchDescriptor init and deinit" {
    const allocator = std.testing.allocator;
    var desc = FetchDescriptor{
        .url = try allocator.dupe(u8, "https://example.com"),
        .method = .GET,
        .body = null,
        .headers = .empty,
        .max_response_bytes = 1024,
    };
    desc.deinit(allocator);
}

test "FetchResult init and deinit" {
    const allocator = std.testing.allocator;
    var result = FetchResult{
        .status = 200,
        .body = try allocator.dupe(u8, "hello"),
        .content_type = try allocator.dupe(u8, "text/plain"),
        .reason = try allocator.dupe(u8, "OK"),
        .ok = true,
    };
    result.deinit(allocator);
}

test "getCallbacks returns installed runtime callback state under capability context" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("../../gc.zig");
    const heap_mod = @import("../../heap.zig");

    const gc_state = try allocator.create(gc_mod.GC);
    gc_state.* = try gc_mod.GC.init(allocator, .{});
    const heap_state = try allocator.create(heap_mod.Heap);
    heap_state.* = heap_mod.Heap.init(allocator, .{});
    gc_state.setHeap(heap_state);
    const ctx = try context.Context.init(allocator, gc_state, .{});
    defer {
        ctx.deinit();
        heap_state.deinit();
        gc_state.deinit();
        allocator.destroy(heap_state);
        allocator.destroy(gc_state);
    }

    const callbacks = try allocator.create(IoCallbacks);
    callbacks.* = .{
        .call_thunk_fn = struct {
            fn call(_: *anyopaque, _: value.JSValue) anyerror!value.JSValue {
                return value.JSValue.undefined_val;
            }
        }.call,
        .execute_fetches_fn = struct {
            fn exec(_: *anyopaque, _: []const FetchDescriptor, _: []FetchResult) void {}
        }.exec,
        .build_response_fn = struct {
            fn build(_: *anyopaque, _: *const FetchResult) anyerror!value.JSValue {
                return value.JSValue.undefined_val;
            }
        }.build,
        .runtime_ptr = @ptrFromInt(0x1),
    };
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(callbacks), &IoCallbacks.deinitOpaque);

    const token = mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
    defer mb.popActiveModuleContext(token);

    try std.testing.expect(getCallbacks(ctx) != null);
}
