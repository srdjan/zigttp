//! Shared utilities for virtual modules
//!
//! Common helpers for value extraction and result object creation.
//! All virtual modules should import from here instead of duplicating
//! these patterns.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const builtins_helpers = @import("../builtins/helpers.zig");
const string = @import("../string.zig");
const object = @import("../object.zig");
const arena_mod = @import("../arena.zig");

/// Cast an opaque NativeFn context pointer to *Context.
pub fn castContext(ctx_ptr: *anyopaque) *context.Context {
    return @ptrCast(@alignCast(ctx_ptr));
}

/// Extract string data from a JSValue.
/// Handles flat JSString, SliceString, and RopeNode (flattened to leaf).
pub fn extractString(val: value.JSValue) ?[]const u8 {
    if (val.isString()) {
        return val.toPtr(string.JSString).data();
    }
    if (val.isStringSlice()) {
        return val.toPtr(string.SliceString).data();
    }
    if (val.isRope()) {
        const rope = val.toPtr(string.RopeNode);
        // Only handle leaf ropes that wrap a JSString directly
        if (rope.asLeaf()) |leaf| {
            return leaf.data();
        }
        // Concat ropes would need flattening with an allocator;
        // callers should ensure strings are flattened before passing to native modules
        return null;
    }
    return null;
}

/// Extract an i32 integer from a JSValue.
pub fn extractInt(val: value.JSValue) ?i32 {
    if (val.isInt()) {
        return val.getInt();
    }
    if (val.isNumber()) {
        const f = val.getFloat64();
        const i: i32 = @intFromFloat(f);
        if (@as(f64, @floatFromInt(i)) == f) return i;
    }
    return null;
}

/// Extract an f64 number from a JSValue.
pub fn extractFloat(val: value.JSValue) ?f64 {
    if (val.isInt()) {
        return @floatFromInt(val.getInt());
    }
    if (val.isNumber()) {
        return val.getFloat64();
    }
    return null;
}

/// Create a plain result object: { ok: true, value: payload }
pub fn createPlainResultOk(ctx: *context.Context, payload: value.JSValue) !value.JSValue {
    return builtins_helpers.createResultOk(ctx, payload);
}

/// Create a plain error result: { ok: false, error: message }
pub fn createPlainResultErr(ctx: *context.Context, message: []const u8) !value.JSValue {
    const msg_val = try ctx.createString(message);
    return builtins_helpers.createResultErr(ctx, msg_val);
}

/// Create a plain errors result: { ok: false, errors: errors_array }
pub fn createPlainResultErrs(ctx: *context.Context, errors: value.JSValue) !value.JSValue {
    return builtins_helpers.createResultErrWithField(ctx, errors, .errors);
}

pub fn throwError(ctx: *context.Context, name: []const u8, message: []const u8) value.JSValue {
    const obj = ctx.createObject(ctx.object_prototype) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    const name_val = ctx.createString(name) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };
    const message_val = ctx.createString(message) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    ctx.setPropertyChecked(obj, .name, name_val) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };
    ctx.setPropertyChecked(obj, .message, message_val) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    obj.class_id = .@"error";
    ctx.throwException(obj.toValue());
    return value.JSValue.exception_val;
}

pub fn throwCapabilityPolicyError(
    ctx: *context.Context,
    category: []const u8,
    subject: []const u8,
) value.JSValue {
    var message = std.ArrayList(u8).empty;
    defer message.deinit(ctx.allocator);

    message.appendSlice(ctx.allocator, category) catch {
        return throwError(ctx, "CapabilityPolicyError", "capability denied by policy");
    };
    message.appendSlice(ctx.allocator, " '") catch {
        return throwError(ctx, "CapabilityPolicyError", "capability denied by policy");
    };
    message.appendSlice(ctx.allocator, subject) catch {
        return throwError(ctx, "CapabilityPolicyError", "capability denied by policy");
    };
    message.appendSlice(ctx.allocator, "' is not allowed by capability policy") catch {
        return throwError(ctx, "CapabilityPolicyError", "capability denied by policy");
    };

    return throwError(ctx, "CapabilityPolicyError", message.items);
}
