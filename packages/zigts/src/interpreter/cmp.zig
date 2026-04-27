//! Pure ECMAScript-style value primitives: ordering, loose equality, and
//! string-length lookup across flat/rope/slice representations. Used by
//! the comparison and length opcodes in dispatch and by JIT intrinsics.
//!
//! Lived as top-level free functions on interpreter.zig until the Slice E
//! split. None of these take an Interpreter -- they are referentially
//! transparent and safe to call from any context.

const std = @import("std");
const value = @import("../value.zig");
const string = @import("../string.zig");

pub fn compareValues(a: value.JSValue, b: value.JSValue) !std.math.Order {
    // Integer fast path
    if (a.isInt() and b.isInt()) {
        return std.math.order(a.getInt(), b.getInt());
    }
    // Float comparison
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    // NaN comparisons are always false
    if (std.math.isNan(an) or std.math.isNan(bn)) {
        return error.TypeError;
    }
    return std.math.order(an, bn);
}

/// Loose equality (==)
pub fn looseEquals(a: value.JSValue, b: value.JSValue) bool {
    // Same type - strict equals
    if (a.raw == b.raw) return true;

    // null == undefined
    if ((a.isNull() and b.isUndefined()) or (a.isUndefined() and b.isNull())) return true;

    // Number comparison
    if (a.isNumber() and b.isNumber()) {
        const an = a.toNumber() orelse return false;
        const bn = b.toNumber() orelse return false;
        if (std.math.isNan(an) or std.math.isNan(bn)) return false;
        return an == bn;
    }

    // TODO: String coercion, object coercion
    return false;
}

/// Length of any string type: flat JSString, RopeNode, or SliceString.
pub fn getAnyStringLength(val: value.JSValue) value.JSValue {
    if (val.isString()) {
        const str = val.toPtr(string.JSString);
        return value.JSValue.fromInt(@intCast(str.len));
    }
    if (val.isRope()) {
        const rope = val.toPtr(string.RopeNode);
        return value.JSValue.fromInt(@intCast(rope.total_len));
    }
    if (val.isStringSlice()) {
        const slice = val.toPtr(string.SliceString);
        return value.JSValue.fromInt(@intCast(slice.len));
    }
    return value.JSValue.fromInt(0);
}

test "regression: getAnyStringLength handles all string types" {
    const allocator = std.testing.allocator;

    // Flat string
    const flat = try string.createString(allocator, "hello");
    defer string.freeString(allocator, flat);
    const flat_val = value.JSValue.fromPtr(flat);
    try std.testing.expectEqual(value.JSValue.fromInt(5), getAnyStringLength(flat_val));

    // Rope (leaf)
    const leaf = try string.createRopeLeaf(allocator, flat);
    defer allocator.destroy(leaf);
    const leaf_val = value.JSValue.fromPtr(leaf);
    try std.testing.expectEqual(value.JSValue.fromInt(5), getAnyStringLength(leaf_val));

    // Rope (concat)
    const str2 = try string.createString(allocator, " world");
    defer string.freeString(allocator, str2);
    const rope = try string.createRopeFromStrings(allocator, flat, str2);
    defer string.freeRope(allocator, rope);
    const rope_val = value.JSValue.fromPtr(rope);
    try std.testing.expectEqual(value.JSValue.fromInt(11), getAnyStringLength(rope_val));

    // SliceString
    const slice = try string.createSlice(allocator, flat, 1, 3);
    defer string.freeSlice(allocator, slice);
    const slice_val = value.JSValue.fromPtr(slice);
    try std.testing.expectEqual(value.JSValue.fromInt(3), getAnyStringLength(slice_val));
}
