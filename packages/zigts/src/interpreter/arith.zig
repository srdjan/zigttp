//! Cold-path arithmetic and string-concatenation helpers.
//!
//! These ran as methods on `Interpreter` until the Slice C split. They live
//! here as free functions taking `*Interpreter` so the dispatch-heavy
//! interpreter.zig stays focused on the dispatch loop. Struct fields are
//! pub by default in Zig, so `interp.ctx`, `interp.constants`, etc. work
//! the same as in a method body.
//!
//! Hot inline fast-path wrappers (subValues, mulValues, allocFloat) stay
//! in interpreter.zig so dispatch keeps inlining them; this module owns
//! their `@branchHint(.cold)` slow-path counterparts.

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
    defer if (!a.isAnyString()) string.freeString(interp.ctx.allocator, str_a);

    const str_b = try valueToString(interp, b);
    defer if (!b.isAnyString()) string.freeString(interp.ctx.allocator, str_b);

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

    // Convert all values to strings
    var strings: [16]*const string.JSString = undefined;
    for (0..count) |j| {
        strings[j] = try valueToString(interp, values[j]);
    }

    // Use concatMany for single-allocation concatenation
    if (interp.ctx.hybrid) |h| {
        const result = string.concatManyWithArena(h.arena, strings[0..count]) orelse
            return error.OutOfMemory;
        return value.JSValue.fromPtr(result);
    }

    const result = try string.concatMany(interp.ctx.allocator, strings[0..count]);
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
    // Float
    if (val.toNumber()) |n| {
        var buf: [64]u8 = undefined;
        const slice = std.fmt.bufPrint(&buf, "{d}", .{n}) catch return try interp.createString("NaN");
        return try interp.createString(slice);
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
