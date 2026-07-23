//! Cold-path arithmetic and string-concatenation helpers. The hot integer
//! fast paths and `allocFloat` wrapper live in interpreter.zig so dispatch
//! keeps inlining them; everything here is `@branchHint(.cold)`.

const std = @import("std");
const value = @import("../value.zig");
const string = @import("../string.zig");
const arena_mod = @import("../arena.zig");
const interpreter = @import("../interpreter.zig");
const trace = @import("trace.zig");
const Interpreter = interpreter.Interpreter;

pub fn addValuesSlow(interp: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
    @branchHint(.cold);
    // String concatenation - if either operand is any string type (flat, rope, or slice)
    if (a.isAnyString() or b.isAnyString()) {
        return try concatToString(interp, a, b);
    }
    // Float path
    const an = a.toNumber() orelse {
        trace.traceTypeError(interp, "add(a)", a, b);
        return error.TypeError;
    };
    const bn = b.toNumber() orelse {
        trace.traceTypeError(interp, "add(b)", a, b);
        return error.TypeError;
    };
    return try interp.allocFloat(an + bn);
}

/// Slow path for the `add_num` opcode: skips the string-concat dispatch.
pub fn addNumericOnly(interp: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
    @branchHint(.cold);
    const an = a.toNumber() orelse {
        trace.traceTypeError(interp, "add_num(a)", a, b);
        return error.TypeError;
    };
    const bn = b.toNumber() orelse {
        trace.traceTypeError(interp, "add_num(b)", a, b);
        return error.TypeError;
    };
    return try interp.allocFloat(an + bn);
}

pub fn subValuesSlow(interp: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
    @branchHint(.cold);
    const an = a.toNumber() orelse {
        trace.traceTypeError(interp, "sub(a)", a, b);
        return error.TypeError;
    };
    const bn = b.toNumber() orelse {
        trace.traceTypeError(interp, "sub(b)", a, b);
        return error.TypeError;
    };
    return try interp.allocFloat(an - bn);
}

pub fn mulValuesSlow(interp: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
    @branchHint(.cold);
    const an = a.toNumber() orelse {
        trace.traceTypeError(interp, "mul(a)", a, b);
        return error.TypeError;
    };
    const bn = b.toNumber() orelse {
        trace.traceTypeError(interp, "mul(b)", a, b);
        return error.TypeError;
    };
    return try interp.allocFloat(an * bn);
}

pub fn concatToString(interp: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
    // Use rope-based concatenation for hybrid mode (arena allocation)
    if (interp.ctx.hybrid) |h| {
        return concatToStringRope(interp, a, b, h.arena);
    }

    // Non-hybrid mode: use traditional concatenation
    // (Rope optimization is most valuable with arena allocation)

    // Fast path: string + number (very common pattern)
    const str_a = try valueToString(interp, a);
    // Only free if we allocated a fresh string; cached ints (0-999) must not be freed.
    const free_a = !a.isAnyString() and !(a.isInt() and interp.ctx.small_int_cache.get(a.getInt()) != null);
    defer if (free_a) string.freeString(interp.ctx.allocator, str_a);

    const str_b = try valueToString(interp, b);
    const free_b = !b.isAnyString() and !(b.isInt() and interp.ctx.small_int_cache.get(b.getInt()) != null);
    defer if (free_b) string.freeString(interp.ctx.allocator, str_b);

    const result = try string.concatStrings(interp.ctx.allocator, str_a, str_b);
    return value.JSValue.fromPtr(result);
}

/// Rope-based concat is O(1); flat fallback runs only when arena is unavailable.
pub fn concatToStringRope(interp: *Interpreter, a: value.JSValue, b: value.JSValue, arena: *arena_mod.Arena) !value.JSValue {
    // Case 1: Both are already ropes - O(1) concat
    if (a.isRope() and b.isRope()) {
        const rope_a = a.toPtr(string.RopeNode);
        const rope_b = b.toPtr(string.RopeNode);
        const result = string.concatRopesWithArena(arena, rope_a, rope_b) orelse
            return error.OutOfMemory;
        return value.JSValue.fromPtr(result);
    }

    // Case 2: Left is rope, right is string - O(1) concat
    if (a.isRope() and b.isString()) {
        const rope_a = a.toPtr(string.RopeNode);
        const str_b = b.toPtr(string.JSString);
        const result = string.concatRopeStringWithArena(arena, rope_a, str_b) orelse
            return error.OutOfMemory;
        return value.JSValue.fromPtr(result);
    }

    // Case 3: Left is string, right is rope
    if (a.isString() and b.isRope()) {
        const str_a = a.toPtr(string.JSString);
        const rope_b = b.toPtr(string.RopeNode);
        const leaf_a = string.createRopeLeafWithArena(arena, str_a) orelse
            return error.OutOfMemory;
        const result = string.concatRopesWithArena(arena, leaf_a, rope_b) orelse
            return error.OutOfMemory;
        return value.JSValue.fromPtr(result);
    }

    // Case 4: Both are strings - create rope for future O(1) concats
    if (a.isString() and b.isString()) {
        const str_a = a.toPtr(string.JSString);
        const str_b = b.toPtr(string.JSString);

        // Small optimization: for very short strings, flat concat is fine
        // The rope overhead isn't worth it for tiny strings
        if (str_a.len + str_b.len < 64) {
            const result = string.concatStringsWithArena(arena, str_a, str_b) orelse
                return error.OutOfMemory;
            return value.JSValue.fromPtr(result);
        }

        // Create rope for larger strings
        const result = string.createRopeFromStringsWithArena(arena, str_a, str_b) orelse
            return error.OutOfMemory;
        return value.JSValue.fromPtr(result);
    }

    // Case 4b: Handle string slices - flatten and concat
    // SliceString on either side: flatten to flat string and use string path
    if (a.isStringSlice() or b.isStringSlice()) {
        const str_a = try valueToStringArena(interp, a, arena);
        const str_b = try valueToStringArena(interp, b, arena);

        if (str_a.len + str_b.len < 64) {
            const result = string.concatStringsWithArena(arena, str_a, str_b) orelse
                return error.OutOfMemory;
            return value.JSValue.fromPtr(result);
        }

        const result = string.createRopeFromStringsWithArena(arena, str_a, str_b) orelse
            return error.OutOfMemory;
        return value.JSValue.fromPtr(result);
    }

    // Case 5: Left is rope, right needs conversion
    if (a.isRope()) {
        const rope_a = a.toPtr(string.RopeNode);
        const str_b = try valueToStringArena(interp, b, arena);
        const result = string.concatRopeStringWithArena(arena, rope_a, str_b) orelse
            return error.OutOfMemory;
        return value.JSValue.fromPtr(result);
    }

    // Case 6: Right is rope, left needs conversion
    if (b.isRope()) {
        const rope_b = b.toPtr(string.RopeNode);
        const str_a = try valueToStringArena(interp, a, arena);
        const leaf_a = string.createRopeLeafWithArena(arena, str_a) orelse
            return error.OutOfMemory;
        const result = string.concatRopesWithArena(arena, leaf_a, rope_b) orelse
            return error.OutOfMemory;
        return value.JSValue.fromPtr(result);
    }

    // Case 7: Neither is string/rope - convert both
    const str_a = try valueToStringArena(interp, a, arena);
    const str_b = try valueToStringArena(interp, b, arena);

    // For general case, use flat concat (ropes mainly benefit repeated concats)
    const result = string.concatStringsWithArena(arena, str_a, str_b) orelse
        return error.OutOfMemory;
    return value.JSValue.fromPtr(result);
}

pub fn valueToStringArena(interp: *Interpreter, val: value.JSValue, arena: *arena_mod.Arena) !*string.JSString {
    // Handle rope: flatten to string
    if (val.isRope()) {
        const rope = val.toPtr(string.RopeNode);
        return rope.flattenWithArena(arena) orelse error.OutOfMemory;
    }

    // Handle string: return directly
    if (val.isString()) {
        return val.toPtr(string.JSString);
    }

    // Handle string slice: flatten to string
    if (val.isStringSlice()) {
        const slice = val.toPtr(string.SliceString);
        return slice.flattenWithArena(arena) orelse error.OutOfMemory;
    }

    // Handle number
    if (val.isInt()) {
        var buf: [32]u8 = undefined;
        const n = val.getInt();
        if (interp.ctx.small_int_cache.get(n)) |cached| {
            return cached;
        }
        const slice = string.formatIntToBuf(&buf, n);
        return string.createStringWithArena(arena, slice) orelse error.OutOfMemory;
    }

    if (val.isFloat64()) {
        var buf: [64]u8 = undefined;
        const slice = string.formatFloatToBuf(&buf, val.getFloat64());
        return string.createStringWithArena(arena, slice) orelse error.OutOfMemory;
    }

    // Handle other types
    if (val.isNull()) {
        return string.createStringWithArena(arena, "null") orelse error.OutOfMemory;
    }
    if (val.isUndefined()) {
        return string.createStringWithArena(arena, "undefined") orelse error.OutOfMemory;
    }
    if (val.isTrue()) {
        return string.createStringWithArena(arena, "true") orelse error.OutOfMemory;
    }
    if (val.isFalse()) {
        return string.createStringWithArena(arena, "false") orelse error.OutOfMemory;
    }

    // Object: call toString if available, otherwise return "[object Object]"
    return string.createStringWithArena(arena, "[object Object]") orelse error.OutOfMemory;
}

/// Calculates total length once, then a single allocation -- avoids the
/// quadratic cost of chained binary concatenation.
pub fn concatNValues(interp: *Interpreter, count: u8) !value.JSValue {
    // The fixed [16] stack buffers below are indexed [0..count). The only
    // emitter (tryEmitStringConcatChain) caps the chain at 16 operands, so
    // count > 16 is unreachable from compiled source; assert that invariant so
    // a future codegen change (or hand-built bytecode) cannot overflow the
    // buffers in ReleaseFast where bounds checks are elided.
    std.debug.assert(count <= 16);
    if (count == 0) {
        return value.JSValue.fromPtr(try interp.createString(""));
    }
    if (count == 1) {
        const val = interp.ctx.pop();
        const str = try valueToString(interp, val);
        return value.JSValue.fromPtr(str);
    }

    // Pop all values in reverse order to get left-to-right order
    var values: [16]value.JSValue = undefined;
    var i: u8 = count;
    while (i > 0) : (i -= 1) {
        values[i - 1] = interp.ctx.pop();
    }

    // Hybrid (request) mode: stringify every operand into the request arena so
    // no intermediate needs explicit freeing (mirrors concatToStringRope).
    // valueToString routes numbers/booleans/null/objects through
    // interp.createString, which allocates from the arena in hybrid mode, so
    // freeing those via ctx.allocator would be an invalid free; ropes/slices
    // flatten into the arena here too. The per-request arena reset reclaims all.
    if (interp.ctx.hybrid) |h| {
        var strings: [16]*string.JSString = undefined;
        for (0..count) |j| {
            strings[j] = try valueToStringArena(interp, values[j], h.arena);
        }
        const result = string.concatManyWithArena(h.arena, strings[0..count]) orelse
            return error.OutOfMemory;
        return value.JSValue.fromPtr(result);
    }

    // Non-hybrid mode: valueToString allocates a new string via ctx.allocator
    // for every operand except a plain string returned directly (isString) and
    // cached small ints. Ropes/slices flatten into a fresh ctx.allocator string,
    // so they must be freed too -- gate on isString (tag 3), not isAnyString.
    var strings: [16]*string.JSString = undefined;
    var fresh: [16]bool = [_]bool{false} ** 16;
    for (0..count) |j| {
        const v = values[j];
        strings[j] = valueToString(interp, v) catch |err| {
            for (0..j) |k| if (fresh[k]) string.freeString(interp.ctx.allocator, strings[k]);
            return err;
        };
        fresh[j] = !v.isString() and !(v.isInt() and interp.ctx.small_int_cache.get(v.getInt()) != null);
    }

    const result = string.concatMany(interp.ctx.allocator, strings[0..count]) catch |err| {
        for (0..count) |j| if (fresh[j]) string.freeString(interp.ctx.allocator, strings[j]);
        return err;
    };
    for (0..count) |j| if (fresh[j]) string.freeString(interp.ctx.allocator, strings[j]);
    return value.JSValue.fromPtr(result);
}

pub fn valueToString(interp: *Interpreter, val: value.JSValue) !*string.JSString {
    if (val.isString()) {
        return val.toPtr(string.JSString);
    }
    // Handle rope: flatten to flat string
    if (val.isRope()) {
        const rope = val.toPtr(string.RopeNode);
        return try rope.flatten(interp.ctx.allocator);
    }
    // Handle string slice: flatten to flat string
    if (val.isStringSlice()) {
        const slice = val.toPtr(string.SliceString);
        return try slice.flatten(interp.ctx.allocator);
    }
    if (val.isInt()) {
        const n = val.getInt();
        // Fast path: use cached strings for small integers 0-99
        if (interp.ctx.small_int_cache.get(n)) |cached| {
            return cached;
        }
        // Fallback: format larger integers
        var buf: [32]u8 = undefined;
        const slice = std.fmt.bufPrint(&buf, "{d}", .{n}) catch return try interp.createString("0");
        return try interp.createString(slice);
    }
    if (val.isNull()) {
        return try interp.createString("null");
    }
    if (val.isUndefined()) {
        return try interp.createString("undefined");
    }
    if (val.isTrue()) {
        return try interp.createString("true");
    }
    if (val.isFalse()) {
        return try interp.createString("false");
    }
    if (val.isObject()) {
        return try interp.createString("[object Object]");
    }
    // Float: use formatFloatToBuf to correctly emit "NaN", "Infinity", "-Infinity"
    if (val.toNumber()) |n| {
        var buf: [64]u8 = undefined;
        return try interp.createString(string.formatFloatToBuf(&buf, n));
    }
    return try interp.createString("undefined");
}

pub inline fn divValues(interp: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
    const an = a.toNumber() orelse {
        trace.traceTypeError(interp, "div(a)", a, b);
        return error.TypeError;
    };
    const bn = b.toNumber() orelse {
        trace.traceTypeError(interp, "div(b)", a, b);
        return error.TypeError;
    };
    return try interp.allocFloat(an / bn);
}

pub inline fn powValues(interp: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
    const an = a.toNumber() orelse {
        trace.traceTypeError(interp, "pow(a)", a, b);
        return error.TypeError;
    };
    const bn = b.toNumber() orelse {
        trace.traceTypeError(interp, "pow(b)", a, b);
        return error.TypeError;
    };
    return try interp.allocFloat(std.math.pow(f64, an, bn));
}

pub inline fn negValue(interp: *Interpreter, a: value.JSValue) !value.JSValue {
    if (a.isInt()) {
        const v = a.getInt();
        if (v == std.math.minInt(i32)) {
            // -minInt overflows i32; promote to float.
            return try interp.allocFloat(-@as(f64, @floatFromInt(v)));
        }
        return value.JSValue.fromInt(-v);
    }
    if (a.isFloat64()) {
        return try interp.allocFloat(-a.getFloat64());
    }
    return error.TypeError;
}
