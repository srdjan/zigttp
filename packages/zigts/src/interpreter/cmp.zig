//! Pure ECMAScript-style value primitives: ordering, loose equality, and
//! string-length lookup across flat/rope/slice representations. None take
//! an Interpreter -- safe to call from anywhere. (String ordering may flatten
//! a concat rope on first access; the result is cached in the rope node, so
//! observable behavior stays referentially transparent.)

const std = @import("std");
const value = @import("../value.zig");
const string = @import("../string.zig");
const helpers = @import("../builtins/helpers.zig");

/// Ordering of two values, or `null` when the operands are "unordered" (a NaN
/// operand, or an operand that does not coerce to a number). Per ECMAScript
/// every relational operator yields `false` for unordered operands - e.g.
/// `NaN < 5`, `undefined > 0` - so callers must treat `null` as false, not as
/// a thrown error. Two strings order lexicographically (byte-wise over UTF-8,
/// matching the engine's string equality semantics); this is the single
/// source of truth for `<`/`<=`/`>`/`>=` across the interpreter and both JIT
/// tiers (Context.jitCompare), so string relational comparisons agree on
/// every tier.
///
/// Context-aware callers (interpreter, JIT) should use compareValuesCtx and
/// pass the request Context so that concat-rope flattening uses the arena
/// rather than c_allocator. compareValues is kept for context-free sites
/// (tests, analyzer-only paths) and delegates to compareValuesCtx(a, b, null).
pub inline fn compareValues(a: value.JSValue, b: value.JSValue) ?std.math.Order {
    return compareValuesCtx(a, b, null);
}

/// Context-aware variant. Pass `ctx` as `*Context` cast to `*anyopaque` to
/// route concat-rope flattening through the request arena; pass `null` for the
/// context-free (c_allocator) path. The `?*anyopaque` indirection avoids a
/// circular import: context.zig imports cmp.zig, and cmp.zig imports
/// helpers.zig which imports context.zig, so cmp.zig cannot name *Context
/// directly.
pub inline fn compareValuesCtx(a: value.JSValue, b: value.JSValue, ctx: ?*anyopaque) ?std.math.Order {
    // Integer fast path
    if (a.isInt() and b.isInt()) {
        return std.math.order(a.getInt(), b.getInt());
    }
    // Lexicographic string ordering. Without this branch, two strings fell
    // through to toNumber() -> null and EVERY string relational comparison
    // silently evaluated false ("a" < "b" included) even though the analyzer
    // accepts same-type string comparisons. Only the cheap tag test lives in
    // this inline body: the compare itself is `noinline` so the flatten
    // machinery does not bloat every inlined comparison site (a measured ~9%
    // recursion-benchmark regression when inlined).
    if (a.isAnyString() and b.isAnyString()) {
        return orderStrings(a, b, ctx);
    }
    // Float comparison
    const an = a.toNumber() orelse return null;
    const bn = b.toNumber() orelse return null;
    if (std.math.isNan(an) or std.math.isNan(bn)) return null;
    return std.math.order(an, bn);
}

/// Cold string-ordering path; see the call site in `compareValuesCtx`.
noinline fn orderStrings(a: value.JSValue, b: value.JSValue, ctx: ?*anyopaque) ?std.math.Order {
    const ab = helpers.getStringDataAnyCtx(a, ctx) orelse return null;
    const bb = helpers.getStringDataAnyCtx(b, ctx) orelse return null;
    return string.compareStrings(ab, bb);
}

/// `a < b` with ECMAScript unordered-is-false semantics.
pub inline fn lessThan(a: value.JSValue, b: value.JSValue) bool {
    const o = compareValues(a, b) orelse return false;
    return o == .lt;
}

/// `a <= b` with ECMAScript unordered-is-false semantics.
pub inline fn lessEqual(a: value.JSValue, b: value.JSValue) bool {
    const o = compareValues(a, b) orelse return false;
    return o == .lt or o == .eq;
}

/// `a > b` with ECMAScript unordered-is-false semantics.
pub inline fn greaterThan(a: value.JSValue, b: value.JSValue) bool {
    const o = compareValues(a, b) orelse return false;
    return o == .gt;
}

/// `a >= b` with ECMAScript unordered-is-false semantics.
pub inline fn greaterEqual(a: value.JSValue, b: value.JSValue) bool {
    const o = compareValues(a, b) orelse return false;
    return o == .gt or o == .eq;
}

/// Loose equality (==)
pub inline fn looseEquals(a: value.JSValue, b: value.JSValue) bool {
    // Same type - strict equals; NaN != NaN even under loose equality
    if (a.raw == b.raw) {
        if (a.isRawDouble() and std.math.isNan(@as(f64, @bitCast(a.raw)))) return false;
        return true;
    }

    // null == undefined
    if ((a.isNull() and b.isUndefined()) or (a.isUndefined() and b.isNull())) return true;

    // Number comparison
    if (a.isNumber() and b.isNumber()) {
        const an = a.toNumber() orelse return false;
        const bn = b.toNumber() orelse return false;
        if (std.math.isNan(an) or std.math.isNan(bn)) return false;
        return an == bn;
    }

    // String and object coercion are intentionally absent: `==`/`!=` are
    // rejected at parse time ("use === / !=="), so the .eq/.neq opcodes are
    // never emitted from source - this function is reachable only from
    // hand-built bytecode. Implementing loose-coercion paths would reintroduce
    // exactly the control flow the language cut bans.
    return false;
}

/// Length of any string type: flat JSString, RopeNode, or SliceString.
pub inline fn getAnyStringLength(val: value.JSValue) value.JSValue {
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

test "regression: string relational comparison orders lexicographically" {
    const allocator = std.testing.allocator;

    const a = try string.createString(allocator, "a");
    defer string.freeString(allocator, a);
    const b = try string.createString(allocator, "b");
    defer string.freeString(allocator, b);
    const a_val = value.JSValue.fromPtr(a);
    const b_val = value.JSValue.fromPtr(b);

    try std.testing.expect(lessThan(a_val, b_val));
    try std.testing.expect(!lessThan(b_val, a_val));
    try std.testing.expect(greaterThan(b_val, a_val));
    try std.testing.expect(lessEqual(a_val, a_val));
    try std.testing.expect(greaterEqual(a_val, a_val));
    try std.testing.expect(!lessThan(a_val, a_val));

    // Slice representation: "bc" sliced from "abc" orders after "a".
    const abc = try string.createString(allocator, "abc");
    defer string.freeString(allocator, abc);
    const slice = try string.createSlice(allocator, abc, 1, 2);
    defer string.freeSlice(allocator, slice);
    try std.testing.expect(lessThan(a_val, value.JSValue.fromPtr(slice)));

    // Mixed string/number stays unordered (analyzer rejects it; runtime must
    // not invent a coercion).
    try std.testing.expect(!lessThan(a_val, value.JSValue.fromInt(5)));
    try std.testing.expect(!greaterThan(a_val, value.JSValue.fromInt(5)));
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
