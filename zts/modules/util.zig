//! Shared utilities for virtual modules
//!
//! Common helpers for value extraction and result object creation.
//! All virtual modules should import from here instead of duplicating
//! these patterns.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const string = @import("../string.zig");
const object = @import("../object.zig");
const arena_mod = @import("../arena.zig");

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
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const obj = try ctx.createObject(null);
    const ok_atom = object.Atom.ok;
    const value_atom = object.Atom.value;
    try obj.setProperty(ctx.allocator, pool, ok_atom, value.JSValue.true_val);
    try obj.setProperty(ctx.allocator, pool, value_atom, payload);
    return obj.toValue();
}

/// Create a plain error result: { ok: false, error: message }
pub fn createPlainResultErr(ctx: *context.Context, message: []const u8) !value.JSValue {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const obj = try ctx.createObject(null);
    const ok_atom = object.Atom.ok;
    const error_atom = try ctx.atoms.intern("error");
    const msg_val = try ctx.createString(message);
    try obj.setProperty(ctx.allocator, pool, ok_atom, value.JSValue.false_val);
    try obj.setProperty(ctx.allocator, pool, error_atom, msg_val);
    return obj.toValue();
}

/// Create a plain errors result: { ok: false, errors: errors_array }
pub fn createPlainResultErrs(ctx: *context.Context, errors: value.JSValue) !value.JSValue {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const obj = try ctx.createObject(null);
    const ok_atom = object.Atom.ok;
    const errors_atom = try ctx.atoms.intern("errors");
    try obj.setProperty(ctx.allocator, pool, ok_atom, value.JSValue.false_val);
    try obj.setProperty(ctx.allocator, pool, errors_atom, errors);
    return obj.toValue();
}
