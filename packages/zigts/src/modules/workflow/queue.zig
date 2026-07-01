//! zigttp:queue - actor-style in-process message queues.
//!
//! Exports synchronous Result-returning functions because the current zts
//! subset has no Promise/async runtime. The runtime layer owns delivery,
//! retention, and serialization; this module only validates arguments.

const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const util = @import("../internal/util.zig");
const mb = @import("../../module_binding.zig");

pub const MODULE_STATE_SLOT = @intFromEnum(@import("../../module_slots.zig").Slot.queue);

pub const QueueCallbacks = struct {
    send_fn: *const fn (*anyopaque, *context.Context, []const u8, value.JSValue) anyerror!value.JSValue,
    request_fn: *const fn (*anyopaque, *context.Context, []const u8, value.JSValue) anyerror!value.JSValue,
    receive_fn: *const fn (*anyopaque, *context.Context, ?[]const u8) anyerror!value.JSValue,
    ack_fn: *const fn (*anyopaque, *context.Context, []const u8) anyerror!value.JSValue,
    nack_fn: *const fn (*anyopaque, *context.Context, []const u8, []const u8) anyerror!value.JSValue,
    reply_fn: *const fn (*anyopaque, *context.Context, []const u8, value.JSValue) anyerror!value.JSValue,
    runtime_ptr: *anyopaque,

    pub fn deinitOpaque(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *QueueCallbacks = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }
};

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:queue",
    .name = "queue",
    .required_capabilities = &.{.runtime_callback},
    .stateful = true,
    .contract_section = "queue",
    .exports = &.{
        .{ .name = "send", .func = sendNative, .arg_count = 2, .effect = .write, .returns = .result, .param_types = &.{ .string, .unknown }, .failure_severity = .expected, .traceable = true },
        .{ .name = "request", .func = requestNative, .arg_count = 2, .effect = .write, .returns = .result, .param_types = &.{ .string, .unknown }, .failure_severity = .expected, .traceable = true },
        // actor is optional (defaults to "main"), so required_arg_count = 0.
        .{ .name = "receive", .func = receiveNative, .arg_count = 1, .required_arg_count = 0, .effect = .write, .returns = .result, .param_types = &.{.string}, .failure_severity = .expected, .traceable = true, .return_labels = .{ .external = true } },
        .{ .name = "ack", .func = ackNative, .arg_count = 1, .effect = .write, .returns = .result, .param_types = &.{.string}, .failure_severity = .expected, .traceable = true },
        // trailing reason is optional (defaults to "nack"), so required_arg_count = 1.
        .{ .name = "nack", .func = nackNative, .arg_count = 2, .required_arg_count = 1, .effect = .write, .returns = .result, .param_types = &.{ .string, .string }, .failure_severity = .expected, .traceable = true },
        .{ .name = "reply", .func = replyNative, .arg_count = 2, .effect = .write, .returns = .result, .param_types = &.{ .string, .unknown }, .failure_severity = .expected, .traceable = true },
    },
};

pub const exports = binding.toModuleExports();

fn sendNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try queueCallbacksOrErr(ctx);
    if (callbacks == null) return util.createPlainResultErr(ctx, "queue runtime is not installed");

    if (args.len < 2) return util.createPlainResultErr(ctx, "send() expects target and payload");
    const target = util.extractString(args[0]) orelse {
        return util.createPlainResultErr(ctx, "send() target must be a string");
    };
    return callbacks.?.send_fn(callbacks.?.runtime_ptr, ctx, target, args[1]);
}

fn requestNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try queueCallbacksOrErr(ctx);
    if (callbacks == null) return util.createPlainResultErr(ctx, "queue runtime is not installed");

    if (args.len < 2) return util.createPlainResultErr(ctx, "request() expects target and payload");
    const target = util.extractString(args[0]) orelse {
        return util.createPlainResultErr(ctx, "request() target must be a string");
    };
    return callbacks.?.request_fn(callbacks.?.runtime_ptr, ctx, target, args[1]);
}

fn receiveNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try queueCallbacksOrErr(ctx);
    if (callbacks == null) return util.createPlainResultErr(ctx, "queue runtime is not installed");

    const actor = if (args.len > 0 and !args[0].isUndefined() and !args[0].isNull())
        util.extractString(args[0]) orelse {
            return util.createPlainResultErr(ctx, "receive() actor must be a string when provided");
        }
    else
        null;
    return callbacks.?.receive_fn(callbacks.?.runtime_ptr, ctx, actor);
}

fn ackNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try queueCallbacksOrErr(ctx);
    if (callbacks == null) return util.createPlainResultErr(ctx, "queue runtime is not installed");

    if (args.len < 1) return util.createPlainResultErr(ctx, "ack() expects message id");
    const id = util.extractString(args[0]) orelse {
        return util.createPlainResultErr(ctx, "ack() id must be a string");
    };
    return callbacks.?.ack_fn(callbacks.?.runtime_ptr, ctx, id);
}

fn nackNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try queueCallbacksOrErr(ctx);
    if (callbacks == null) return util.createPlainResultErr(ctx, "queue runtime is not installed");

    if (args.len < 1) return util.createPlainResultErr(ctx, "nack() expects message id");
    const id = util.extractString(args[0]) orelse {
        return util.createPlainResultErr(ctx, "nack() id must be a string");
    };
    const reason = if (args.len >= 2)
        util.extractString(args[1]) orelse "nack"
    else
        "nack";
    return callbacks.?.nack_fn(callbacks.?.runtime_ptr, ctx, id, reason);
}

fn replyNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    const callbacks = try queueCallbacksOrErr(ctx);
    if (callbacks == null) return util.createPlainResultErr(ctx, "queue runtime is not installed");

    if (args.len < 2) return util.createPlainResultErr(ctx, "reply() expects message id and payload");
    const id = util.extractString(args[0]) orelse {
        return util.createPlainResultErr(ctx, "reply() id must be a string");
    };
    return callbacks.?.reply_fn(callbacks.?.runtime_ptr, ctx, id, args[1]);
}

fn queueCallbacksOrErr(ctx: *context.Context) error{CapabilityViolation}!?*QueueCallbacks {
    return mb.getRuntimeCallbackStateChecked(ctx, QueueCallbacks, MODULE_STATE_SLOT);
}

test "queue binding returns Result objects" {
    const send = binding.exports[0];
    try std.testing.expectEqualStrings("send", send.name);
    try std.testing.expectEqual(mb.ReturnKind.result, send.returns);
    const receive = binding.exports[2];
    try std.testing.expectEqualStrings("receive", receive.name);
    try std.testing.expectEqual(mb.ReturnKind.result, receive.returns);
}
