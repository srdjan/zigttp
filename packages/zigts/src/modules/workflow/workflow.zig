//! zigttp:workflow - in-process multi-handler orchestration
//!
//! Exports:
//!   call(name: string, init?: { method?, path?, body?, headers? }) -> Response
//!     Dispatch a request to a co-located sub-handler by name, in-process,
//!     with full per-call isolation (a separate pooled runtime, its own
//!     GC/arena, setjmp panic isolation). Returns the sub-handler's Response,
//!     copied into orchestrator-owned memory.
//!
//! The actual dispatch/copy-out behavior lives in the runtime layer
//! (src/zruntime.zig `workflowCallCallback` over the `SystemRuntime`
//! registry). This module validates JS arguments, then delegates to the
//! runtime-owned callback. `self_managed_io` keeps the generic
//! trace/replay/durable wrappers from auto-recording live Response objects:
//! the runtime callbacks snapshot workflow dispatches as plain
//! `{status,headers,body}` data and rebuild Responses during replay.

const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const util = @import("../internal/util.zig");
const mb = @import("../../module_binding.zig");

pub const MODULE_STATE_SLOT = @intFromEnum(@import("../../module_slots.zig").Slot.workflow);

/// Runtime-owned callbacks installed by src/zruntime.zig when a system
/// registry (a `--system` handler bundle) is present.
pub const WorkflowCallbacks = struct {
    call_fn: *const fn (*anyopaque, *context.Context, []const u8, value.JSValue) anyerror!value.JSValue,
    saga_fn: *const fn (*anyopaque, *context.Context, value.JSValue) anyerror!value.JSValue,
    fanout_fn: *const fn (*anyopaque, *context.Context, value.JSValue) anyerror!value.JSValue,
    follow_fn: *const fn (*anyopaque, *context.Context, value.JSValue, []const u8, value.JSValue) anyerror!value.JSValue,
    runtime_ptr: *anyopaque,

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *WorkflowCallbacks = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }
};

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:workflow",
    .name = "workflow",
    .required_capabilities = &.{.runtime_callback},
    .stateful = true,
    .self_managed_io = true,
    .exports = &.{
        .{ .name = "call", .func = callNative, .arg_count = 2, .effect = .write, .returns = .object, .param_types = &.{ .string, .unknown }, .return_labels = .{ .external = true } },
        .{ .name = "saga", .func = sagaNative, .arg_count = 1, .effect = .write, .returns = .object, .param_types = &.{.unknown}, .return_labels = .{ .external = true } },
        // Named `fanout`, not `parallel`: module exports share one flat global
        // name namespace (resolver registers each via ctx.setGlobal by name), so
        // `parallel` would collide with and clobber zigttp:io's `parallel`.
        .{ .name = "fanout", .func = fanoutNative, .arg_count = 1, .effect = .write, .returns = .object, .param_types = &.{.unknown}, .return_labels = .{ .external = true } },
        // follow(resource, rel, init?) - HATEOAS: resolve affordance `rel` on a
        // structured resource() to a bundle route and dispatch in-process. The
        // trailing `init` (body/headers) is optional, so required_arg_count = 2.
        .{ .name = "follow", .func = followNative, .arg_count = 3, .required_arg_count = 2, .effect = .write, .returns = .object, .param_types = &.{ .unknown, .string, .unknown }, .return_labels = .{ .external = true } },
    },
};

pub const exports = binding.toModuleExports();

fn callNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "call() requires a --system handler bundle");
    };

    if (args.len < 1) {
        return util.throwError(ctx, "TypeError", "call() expects a handler name");
    }

    const name = util.extractString(args[0]) orelse {
        return util.throwError(ctx, "TypeError", "call() handler name must be a string");
    };

    const init_val = if (args.len >= 2) args[1] else value.JSValue.undefined_val;
    return callbacks.call_fn(callbacks.runtime_ptr, ctx, name, init_val);
}

fn sagaNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "saga() requires a --system handler bundle");
    };

    if (args.len < 1 or !args[0].isObject()) {
        return util.throwError(ctx, "TypeError", "saga() expects an array of { name, run, compensate? } steps");
    }

    return callbacks.saga_fn(callbacks.runtime_ptr, ctx, args[0]);
}

fn fanoutNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "fanout() requires a --system handler bundle");
    };

    if (args.len < 1 or !args[0].isObject()) {
        return util.throwError(ctx, "TypeError", "fanout() expects an array of { name, method?, path?, body?, headers? } calls");
    }

    return callbacks.fanout_fn(callbacks.runtime_ptr, ctx, args[0]);
}

fn followNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try getCallbacks(ctx) orelse {
        return util.throwError(ctx, "Error", "follow() requires a --system handler bundle");
    };

    if (args.len < 2) {
        return util.throwError(ctx, "TypeError", "follow() expects (resource, rel, init?)");
    }
    if (!args[0].isObject()) {
        return util.throwError(ctx, "TypeError", "follow() first argument must be a resource()");
    }
    const rel = util.extractString(args[1]) orelse {
        return util.throwError(ctx, "TypeError", "follow() rel must be a string");
    };

    const init_val = if (args.len >= 3) args[2] else value.JSValue.undefined_val;
    return callbacks.follow_fn(callbacks.runtime_ptr, ctx, args[0], rel, init_val);
}

fn getCallbacks(ctx: *context.Context) error{CapabilityViolation}!?*WorkflowCallbacks {
    return mb.getRuntimeCallbackStateChecked(ctx, WorkflowCallbacks, MODULE_STATE_SLOT);
}
