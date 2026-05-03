//! Native JS-callable runtime builtins extracted from zruntime.zig.
//!
//! Each `*Native` function matches the engine's native-fn signature
//! (ctx_ptr, this, args) -> anyerror!zq.JSValue and is registered as a
//! method on a JS prototype during runtime setup. Helpers used here
//! (`beginBodyRead`, `getStringData`) live in zruntime.zig because most
//! other natives in zruntime also depend on them.

const std = @import("std");
const zq = @import("zigts");
const zruntime = @import("zruntime.zig");

pub fn bodyTextNative(ctx_ptr: *anyopaque, this: zq.JSValue, _: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    const body_val = zruntime.beginBodyRead(ctx, this);
    if (ctx.hasException()) return zq.JSValue.exception_val;
    if (body_val.isNull() or body_val.isUndefined()) {
        return ctx.createString("");
    }
    if (body_val.isAnyString()) {
        return body_val;
    }
    return ctx.createString("");
}

pub fn bodyJsonNative(ctx_ptr: *anyopaque, this: zq.JSValue, _: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    const body_val = zruntime.beginBodyRead(ctx, this);
    if (ctx.hasException()) return zq.JSValue.exception_val;
    if (body_val.isNull() or body_val.isUndefined()) {
        return zq.JSValue.undefined_val;
    }
    const body = zruntime.getStringData(body_val) orelse return zq.JSValue.undefined_val;
    return zq.builtins.parseJsonValue(ctx, body) catch zq.JSValue.undefined_val;
}
