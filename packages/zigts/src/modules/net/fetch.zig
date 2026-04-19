//! zigttp:fetch - web-standard outbound HTTP client.
//!
//! Exports:
//!   fetch(url: string, init?: object) -> Response
//!
//! This module is a thin import-based surface over the runtime's outbound
//! HTTP machinery. The actual HTTP execution lives in `zruntime.zig`
//! alongside the legacy global `fetchSync` primitive; this module gets
//! wired in at runtime init via `installState`.
//!
//! Two modes:
//!   - Non-durable (default): ephemeral outbound call with egress-host
//!     policy enforcement.
//!   - Durable: pass `init.durable = { key, retries?, backoff?, ttl_s? }`
//!     to oplog-wrap the call. A cached response within TTL is returned
//!     without hitting the network; a miss executes (with retry on 5xx
//!     per the backoff policy) and persists the terminal 2xx/3xx/4xx
//!     response under `<--durable>/fetch/<hash>.step`. 5xx are not
//!     cached so transient upstream failures don't freeze the caller.

const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const util = @import("../internal/util.zig");
const mb = @import("../../module_binding.zig");

pub const MODULE_STATE_SLOT = @intFromEnum(@import("../../module_slots.zig").Slot.fetch);

/// Runtime-installed callback. Receives the runtime pointer, execution
/// context, and the raw `(url, init?)` argument slice. Implementation
/// lives in `packages/runtime/src/zruntime.zig`.
pub const FetchCallFn = *const fn (
    *anyopaque,
    *context.Context,
    []const value.JSValue,
) anyerror!value.JSValue;

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:fetch",
    .name = "fetch",
    .required_capabilities = &.{ .network, .runtime_callback },
    .stateful = true,
    .exports = &.{
        .{
            .name = "fetch",
            .func = fetchNative,
            .arg_count = 2,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .object },
            .return_labels = .{ .external = true },
            .contract_extractions = &.{
                .{ .arg_position = 0, .category = .fetch_host, .transform = .extract_host },
            },
        },
    },
};

pub const exports = binding.toModuleExports();

pub const FetchState = struct {
    runtime_ptr: *anyopaque,
    call_fn: FetchCallFn,

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *FetchState = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }
};

pub fn installState(
    ctx: *context.Context,
    runtime_ptr: *anyopaque,
    call_fn: FetchCallFn,
) !void {
    if (ctx.getModuleState(FetchState, MODULE_STATE_SLOT)) |state| {
        state.runtime_ptr = runtime_ptr;
        state.call_fn = call_fn;
        return;
    }
    const state = try ctx.allocator.create(FetchState);
    errdefer ctx.allocator.destroy(state);
    state.* = .{ .runtime_ptr = runtime_ptr, .call_fn = call_fn };
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(state), &FetchState.deinitOpaque);
}

fn fetchNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const state = ctx.getModuleState(FetchState, MODULE_STATE_SLOT) orelse {
        return util.throwError(ctx, "Error", "fetch() requires runtime installation (no runtime callback wired)");
    };

    if (args.len == 0) {
        return util.throwError(ctx, "TypeError", "fetch() requires a URL string or init object");
    }

    mb.runtimeCallbackCapabilityChecked();
    return state.call_fn(state.runtime_ptr, ctx, args);
}

test "fetch binding declares required capabilities and export shape" {
    try std.testing.expectEqualStrings("zigttp:fetch", binding.specifier);
    try std.testing.expectEqual(@as(usize, 1), binding.exports.len);
    try std.testing.expectEqualStrings("fetch", binding.exports[0].name);
    try std.testing.expectEqual(@as(u8, 2), binding.exports[0].arg_count);

    var saw_network = false;
    var saw_runtime_callback = false;
    for (binding.required_capabilities) |cap| {
        if (cap == .network) saw_network = true;
        if (cap == .runtime_callback) saw_runtime_callback = true;
    }
    try std.testing.expect(saw_network);
    try std.testing.expect(saw_runtime_callback);
}

test "fetch export extracts host from URL at contract time" {
    const extractions = binding.exports[0].contract_extractions;
    try std.testing.expectEqual(@as(usize, 1), extractions.len);
    try std.testing.expectEqual(mb.ContractCategory.fetch_host, extractions[0].category);
    try std.testing.expectEqual(@as(u8, 0), extractions[0].arg_position);
    try std.testing.expectEqual(mb.ContractTransform.extract_host, extractions[0].transform.?);
}
