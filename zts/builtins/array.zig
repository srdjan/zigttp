const std = @import("std");
const h = @import("helpers.zig");

const value = h.value;
const object = h.object;
const context = h.context;
const string = h.string;
const http = h.http;

const getObject = h.getObject;
const getCallFn = h.getCallFn;
const invokeCallback = h.invokeCallback;
const createArrayWithPrototype = h.createArrayWithPrototype;
const getCallbackArg = h.getCallbackArg;
const valueToStringSimple = h.valueToStringSimple;

// ============================================================================
// Array methods
// ============================================================================

/// Get array length from object
fn getArrayLength(obj: *object.JSObject, pool: ?*const object.HiddenClassPool) i32 {
    if (obj.class_id == .array) {
        return @intCast(obj.getArrayLength());
    }
    if (pool) |p| {
        if (obj.getOwnProperty(p, .length)) |len_val| {
            if (len_val.isInt()) return len_val.getInt();
        }
    }
    return 0;
}

/// Array.isArray(value) - Check if value is an array
pub fn arrayIsArray(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.false_val;

    if (getObject(args[0])) |obj| {
        return if (obj.class_id == .array) value.JSValue.true_val else value.JSValue.false_val;
    }
    return value.JSValue.false_val;
}

/// Array.from(arrayLike, mapFn?, thisArg?) - Create array from array-like or iterable
pub fn arrayFrom(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.undefined_val;

    const source = args[0];
    const allocator = ctx.allocator;

    // Create result array
    const result = ctx.createArray() catch return value.JSValue.undefined_val;
    result.prototype = ctx.array_prototype;

    // Handle string - convert to array of characters
    if (source.isString()) {
        const str = source.toPtr(string.JSString);
        const data = str.data();
        var idx: u32 = 0;

        var i: usize = 0;
        while (i < data.len) {
            const char_len = std.unicode.utf8ByteSequenceLength(data[i]) catch 1;
            const char_slice = data[i..@min(i + char_len, data.len)];
            const char_str = string.createString(allocator, char_slice) catch return result.toValue();
            ctx.setIndexChecked(result, idx, value.JSValue.fromPtr(char_str)) catch return result.toValue();
            idx += 1;
            i += char_len;
        }
        return result.toValue();
    }

    // Handle array-like object with length property
    if (source.isObject()) {
        const src_obj = object.JSObject.fromValue(source);
        const pool = ctx.hidden_class_pool orelse return result.toValue();
        if (src_obj.getProperty(pool, .length)) |len_val| {
            if (len_val.isInt()) {
                const len = len_val.getInt();
                var idx: i32 = 0;
                while (idx < len) : (idx += 1) {
                    const elem = if (src_obj.class_id == .array)
                        (src_obj.getIndex(@intCast(idx)) orelse value.JSValue.undefined_val)
                    else blk: {
                        var idx_buf: [32]u8 = undefined;
                        const idx_slice = std.fmt.bufPrint(&idx_buf, "{d}", .{idx}) catch break :blk value.JSValue.undefined_val;
                        const idx_atom = ctx.atoms.intern(idx_slice) catch break :blk value.JSValue.undefined_val;
                        break :blk src_obj.getProperty(pool, idx_atom) orelse value.JSValue.undefined_val;
                    };
                    ctx.setIndexChecked(result, @intCast(idx), elem) catch return result.toValue();
                }
                return result.toValue();
            }
        }
    }

    return result.toValue();
}

/// Array.of(...elements) - Create array from arguments
pub fn arrayOf(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const result = ctx.createArray() catch return value.JSValue.undefined_val;
    result.prototype = ctx.array_prototype;

    for (args, 0..) |arg, i| {
        ctx.setIndexChecked(result, @intCast(i), arg) catch return value.JSValue.undefined_val;
    }
    return result.toValue();
}

/// Array.prototype.push(...items) - Append elements, return new length
pub fn arrayPush(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    if (obj.class_id != .array) return value.JSValue.undefined_val;

    for (args) |arg| {
        obj.arrayPush(ctx.allocator, arg) catch return value.JSValue.undefined_val;
    }
    return value.JSValue.fromInt(@intCast(obj.getArrayLength()));
}

/// Array.prototype.pop() - Remove and return last element
pub fn arrayPop(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    if (obj.class_id != .array) return value.JSValue.undefined_val;

    const len = obj.getArrayLength();
    if (len == 0) return value.JSValue.undefined_val;

    const last = obj.getIndex(len - 1) orelse value.JSValue.undefined_val;
    obj.setArrayLength(len - 1);
    return last;
}

/// Array.prototype.shift() - Remove and return first element
pub fn arrayShift(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    if (obj.class_id != .array) return value.JSValue.undefined_val;

    const len = obj.getArrayLength();
    if (len == 0) return value.JSValue.undefined_val;

    const first = obj.getIndex(0) orelse value.JSValue.undefined_val;

    // Shift all elements down by 1
    var i: u32 = 1;
    while (i < len) : (i += 1) {
        const val = obj.getIndex(i) orelse value.JSValue.undefined_val;
        obj.setIndex(ctx.allocator, i - 1, val) catch return first;
    }
    obj.setArrayLength(len - 1);
    return first;
}

/// Array.prototype.unshift(...items) - Insert elements at front, return new length
pub fn arrayUnshift(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    if (obj.class_id != .array) return value.JSValue.undefined_val;
    if (args.len == 0) return value.JSValue.fromInt(@intCast(obj.getArrayLength()));

    const len = obj.getArrayLength();
    const shift: u32 = @intCast(args.len);

    // Shift existing elements up by args.len (iterate from end to avoid overwriting)
    var i: u32 = len;
    while (i > 0) {
        i -= 1;
        const val = obj.getIndex(i) orelse value.JSValue.undefined_val;
        obj.setIndex(ctx.allocator, i + shift, val) catch return value.JSValue.undefined_val;
    }

    // Insert new elements at the front
    for (args, 0..) |arg, idx| {
        obj.setIndex(ctx.allocator, @intCast(idx), arg) catch return value.JSValue.undefined_val;
    }

    return value.JSValue.fromInt(@intCast(obj.getArrayLength()));
}

/// Array.prototype.splice(start, deleteCount?, ...items) - Remove/insert elements, return deleted
pub fn arraySplice(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    if (obj.class_id != .array) return value.JSValue.undefined_val;

    const len: i32 = @intCast(obj.getArrayLength());

    // Parse start index
    var start: i32 = if (args.len > 0 and args[0].isInt()) args[0].getInt() else 0;
    if (start < 0) start = @max(len + start, 0);
    if (start > len) start = len;

    // Parse delete count
    var delete_count: i32 = if (args.len > 1 and args[1].isInt()) args[1].getInt() else len - start;
    if (delete_count < 0) delete_count = 0;
    if (delete_count > len - start) delete_count = len - start;

    // Build array of deleted elements
    const deleted = ctx.createArray() catch return value.JSValue.undefined_val;
    deleted.prototype = ctx.array_prototype;
    var d: i32 = 0;
    while (d < delete_count) : (d += 1) {
        const val = obj.getIndex(@intCast(start + d)) orelse value.JSValue.undefined_val;
        ctx.setIndexChecked(deleted, @intCast(d), val) catch return value.JSValue.undefined_val;
    }

    const insert_items = if (args.len > 2) args[2..] else &[_]value.JSValue{};
    const insert_count: i32 = @intCast(insert_items.len);
    const shift_amount = insert_count - delete_count;

    if (shift_amount > 0) {
        // Inserting more than deleting: shift tail elements right (from end)
        var j: i32 = len - 1;
        while (j >= start + delete_count) : (j -= 1) {
            const val = obj.getIndex(@intCast(j)) orelse value.JSValue.undefined_val;
            obj.setIndex(ctx.allocator, @intCast(j + shift_amount), val) catch return deleted.toValue();
        }
    } else if (shift_amount < 0) {
        // Deleting more than inserting: shift tail elements left
        var j: i32 = start + delete_count;
        while (j < len) : (j += 1) {
            const val = obj.getIndex(@intCast(j)) orelse value.JSValue.undefined_val;
            obj.setIndex(ctx.allocator, @intCast(j + shift_amount), val) catch return deleted.toValue();
        }
    }

    // Insert new items at start position
    for (insert_items, 0..) |item, idx| {
        obj.setIndex(ctx.allocator, @intCast(start + @as(i32, @intCast(idx))), item) catch return deleted.toValue();
    }

    // Update length
    obj.setArrayLength(@intCast(len + shift_amount));
    return deleted.toValue();
}

/// Array.prototype.indexOf(searchElement, fromIndex?) - Find first index of element
/// Optimized: scans inline_slots and overflow_slots directly as contiguous u64
/// arrays, avoiding per-element getIndex overhead (class_id check, length check,
/// inline/overflow branch, undefined check).
pub fn arrayIndexOf(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    const obj = getObject(this) orelse return value.JSValue.fromInt(-1);
    if (obj.class_id != .array) return value.JSValue.fromInt(-1);

    const len = obj.getArrayLength();
    if (args.len == 0 or len == 0) return value.JSValue.fromInt(-1);

    const search_elem = args[0];
    var from_idx: i32 = 0;
    if (args.len > 1 and args[1].isInt()) {
        from_idx = args[1].getInt();
        if (from_idx < 0) from_idx = @max(0, @as(i32, @intCast(len)) + from_idx);
    }

    const start: u32 = @intCast(@max(from_idx, 0));
    if (start >= len) return value.JSValue.fromInt(-1);

    // NaN never equals anything via strict equality
    if (search_elem.isRawDouble() and std.math.isNan(@as(f64, @bitCast(search_elem.raw)))) {
        return value.JSValue.fromInt(-1);
    }

    // Strings and boxed floats need content comparison via strictEquals
    if (search_elem.isAnyString() or search_elem.isBoxedFloat64()) {
        return arrayIndexOfSlow(obj, search_elem, start, len);
    }

    // Fast path: raw u64 comparison for ints, raw floats, bools, null, undefined, object identity
    return arrayIndexOfRaw(obj, search_elem.raw, start, len);
}

/// Slow path: compare using strictEquals (for strings and boxed floats)
fn arrayIndexOfSlow(obj: *const object.JSObject, search_elem: value.JSValue, start: u32, len: u32) value.JSValue {
    var i: u32 = start;
    while (i < len) : (i += 1) {
        if (obj.getIndexUnchecked(i).strictEquals(search_elem)) {
            return value.JSValue.fromInt(@intCast(i));
        }
    }
    return value.JSValue.fromInt(-1);
}

/// Fast path: scan inline and overflow slots as contiguous u64 arrays
fn arrayIndexOfRaw(obj: *const object.JSObject, search_raw: u64, start: u32, len: u32) value.JSValue {
    const INLINE_CAP = object.JSObject.INLINE_SLOT_COUNT - 1; // 7 elements inline (slot 0 = length)

    // Scan inline slots (elements 0..min(len, 7))
    const inline_count = @min(len, INLINE_CAP);
    if (start < inline_count) {
        const slots_ptr: [*]const u64 = @ptrCast(&obj.inline_slots);
        var slot: u32 = start + 1; // slot = element_index + 1
        const slot_end: u32 = inline_count + 1;
        while (slot < slot_end) : (slot += 1) {
            if (slots_ptr[slot] == search_raw) {
                return value.JSValue.fromInt(@intCast(slot - 1));
            }
        }
    }

    // Scan overflow slots (elements 7..len)
    if (len > INLINE_CAP) {
        if (obj.overflow_slots) |overflow| {
            const begin: u32 = if (start > INLINE_CAP) start - INLINE_CAP else 0;
            const count: u32 = len - INLINE_CAP;
            const overflow_ptr: [*]const u64 = @ptrCast(overflow);
            var i: u32 = begin;
            while (i < count) : (i += 1) {
                if (overflow_ptr[i] == search_raw) {
                    return value.JSValue.fromInt(@intCast(i + INLINE_CAP));
                }
            }
        }
    }

    return value.JSValue.fromInt(-1);
}

/// Array.prototype.includes(searchElement, fromIndex?) - Check if array contains element
/// Uses SameValueZero: NaN matches NaN (unlike indexOf which uses strict equality).
pub fn arrayIncludes(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    // SameValueZero treats NaN as equal to NaN (unlike ===)
    if (args.len > 0) {
        const search = args[0];
        if (search.isRawDouble() and std.math.isNan(@as(f64, @bitCast(search.raw)))) {
            const obj = getObject(this) orelse return value.JSValue.false_val;
            if (obj.class_id != .array) return value.JSValue.false_val;
            const len = obj.getArrayLength();
            var i: u32 = 0;
            if (args.len > 1 and args[1].isInt()) {
                const from = args[1].getInt();
                if (from < 0) {
                    i = @intCast(@max(0, @as(i32, @intCast(len)) + from));
                } else {
                    i = @intCast(@max(from, 0));
                }
            }
            while (i < len) : (i += 1) {
                const val = obj.getIndexUnchecked(i);
                if (val.isRawDouble() and std.math.isNan(@as(f64, @bitCast(val.raw)))) {
                    return value.JSValue.true_val;
                }
            }
            return value.JSValue.false_val;
        }
    }

    // Non-NaN: SameValueZero is identical to strict equality
    const result = arrayIndexOf(ctx, this, args);
    if (result.isInt() and result.getInt() >= 0) {
        return value.JSValue.true_val;
    }
    return value.JSValue.false_val;
}

/// Array.prototype.join(separator?) - Join elements into string
/// Optimized: for arrays of integers/simple values, writes directly into a stack
/// buffer avoiding per-element JSString heap allocations.
pub fn arrayJoin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    if (obj.class_id != .array) return value.JSValue.undefined_val;

    const len = obj.getArrayLength();
    var separator: []const u8 = ",";
    if (args.len > 0 and !args[0].isUndefined()) {
        const sep_str = valueToStringSimple(ctx.allocator, args[0]) catch return value.JSValue.undefined_val;
        separator = sep_str.data();
    }

    // Fast path: build result in a stack buffer (no per-element heap allocation)
    var buf: [512]u8 = undefined;
    var pos: usize = 0;
    var fast_ok = true;

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (i > 0) {
            if (pos + separator.len > buf.len) {
                fast_ok = false;
                break;
            }
            @memcpy(buf[pos..][0..separator.len], separator);
            pos += separator.len;
        }

        const val = obj.getIndexUnchecked(i);
        if (val.isUndefined() or val.isNull()) continue;

        if (val.isInt()) {
            const written = writeIntToBuf(buf[pos..], val.getInt());
            if (written == 0) {
                fast_ok = false;
                break;
            }
            pos += written;
        } else if (val.isString()) {
            const data = val.toPtr(string.JSString).data();
            if (pos + data.len > buf.len) {
                fast_ok = false;
                break;
            }
            @memcpy(buf[pos..][0..data.len], data);
            pos += data.len;
        } else if (val.isStringSlice()) {
            const data = val.toPtr(string.SliceString).data();
            if (pos + data.len > buf.len) {
                fast_ok = false;
                break;
            }
            @memcpy(buf[pos..][0..data.len], data);
            pos += data.len;
        } else if (val.isTrue()) {
            if (pos + 4 > buf.len) {
                fast_ok = false;
                break;
            }
            @memcpy(buf[pos..][0..4], "true");
            pos += 4;
        } else if (val.isFalse()) {
            if (pos + 5 > buf.len) {
                fast_ok = false;
                break;
            }
            @memcpy(buf[pos..][0..5], "false");
            pos += 5;
        } else {
            fast_ok = false;
            break;
        }
    }

    if (fast_ok) {
        const result = createStringForJoin(ctx, buf[0..pos]) orelse return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }

    // Slow path: use ArrayList with heap-allocated string conversions
    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);

    i = 0;
    while (i < len) : (i += 1) {
        if (i > 0) {
            buffer.appendSlice(ctx.allocator, separator) catch return value.JSValue.undefined_val;
        }
        const val = obj.getIndexUnchecked(i);
        if (val.isUndefined() or val.isNull()) {
            continue;
        }
        const elem_str = valueToStringSimple(ctx.allocator, val) catch return value.JSValue.undefined_val;
        buffer.appendSlice(ctx.allocator, elem_str.data()) catch return value.JSValue.undefined_val;
    }

    const result = createStringForJoin(ctx, buffer.items) orelse return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// Write an i32 as decimal digits into a buffer. Returns bytes written, or 0 if buffer too small.
fn writeIntToBuf(buf: []u8, val: i32) usize {
    if (val == 0) {
        if (buf.len == 0) return 0;
        buf[0] = '0';
        return 1;
    }

    var n: u64 = if (val < 0) @intCast(-@as(i64, val)) else @intCast(val);
    var digits: [11]u8 = undefined;
    var dlen: usize = 0;
    while (n > 0) {
        digits[dlen] = @intCast('0' + @as(u8, @truncate(n % 10)));
        n /= 10;
        dlen += 1;
    }

    var out_pos: usize = 0;
    if (val < 0) {
        if (buf.len == 0) return 0;
        buf[0] = '-';
        out_pos = 1;
    }
    if (out_pos + dlen > buf.len) return 0;

    var j: usize = 0;
    while (j < dlen) : (j += 1) {
        buf[out_pos + j] = digits[dlen - 1 - j];
    }
    return out_pos + dlen;
}

/// Create a string using arena when available, general allocator otherwise.
fn createStringForJoin(ctx: *context.Context, data: []const u8) ?*string.JSString {
    if (ctx.hybrid) |hybrid| {
        return string.createStringWithArena(hybrid.arena, data);
    }
    return string.createString(ctx.allocator, data) catch null;
}

/// Array.prototype.toReversed() - Return new reversed array (non-mutating)
pub fn arrayToReversed(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    if (obj.class_id != .array) return value.JSValue.undefined_val;

    const len = obj.getArrayLength();
    const result = ctx.createArray() catch return value.JSValue.undefined_val;
    result.prototype = ctx.array_prototype;

    // Copy elements in reverse order
    var i: u32 = 0;
    while (i < len) : (i += 1) {
        const val = obj.getIndex(len - 1 - i) orelse value.JSValue.undefined_val;
        ctx.setIndexChecked(result, i, val) catch return value.JSValue.undefined_val;
    }
    result.setArrayLength(len);
    return result.toValue();
}

/// Array.prototype.slice(start?, end?) - Return shallow copy of portion
pub fn arraySlice(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj, ctx.hidden_class_pool);

    var start: i32 = 0;
    var end: i32 = len;

    if (args.len > 0 and args[0].isInt()) {
        start = args[0].getInt();
        if (start < 0) start = @max(0, len + start);
    }
    if (args.len > 1 and args[1].isInt()) {
        end = args[1].getInt();
        if (end < 0) end = @max(0, len + end);
    }

    // Return length of slice (proper array creation requires more infrastructure)
    return value.JSValue.fromInt(@max(0, end - start));
}

/// Array.prototype.concat(...items) - Merge arrays
pub fn arrayConcat(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const pool = ctx.hidden_class_pool;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    var total_len = getArrayLength(obj, pool);

    for (args) |arg| {
        if (getObject(arg)) |arr| {
            total_len += getArrayLength(arr, pool);
        } else {
            total_len += 1;
        }
    }

    return value.JSValue.fromInt(total_len);
}

/// Array.prototype.map(callback, thisArg?) - Map to new array
pub fn arrayMap(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const callback = getCallbackArg(args) orelse return value.JSValue.undefined_val;
    const call_fn = getCallFn() orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj, ctx.hidden_class_pool);
    if (len <= 0) return (createArrayWithPrototype(ctx) orelse return value.JSValue.undefined_val).toValue();

    const result = createArrayWithPrototype(ctx) orelse return value.JSValue.undefined_val;
    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = obj.getIndex(i) orelse value.JSValue.undefined_val;
        const call_args = [_]value.JSValue{ elem, value.JSValue.fromInt(@intCast(i)) };
        const mapped = invokeCallback(call_fn, callback, &call_args) orelse value.JSValue.undefined_val;
        result.arrayPush(ctx.allocator, mapped) catch return value.JSValue.undefined_val;
    }
    return result.toValue();
}

/// Array.prototype.filter(callback, thisArg?) - Filter to new array
pub fn arrayFilter(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const callback = getCallbackArg(args) orelse return value.JSValue.undefined_val;
    const call_fn = getCallFn() orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj, ctx.hidden_class_pool);
    if (len <= 0) return (createArrayWithPrototype(ctx) orelse return value.JSValue.undefined_val).toValue();

    const result = createArrayWithPrototype(ctx) orelse return value.JSValue.undefined_val;
    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = obj.getIndex(i) orelse value.JSValue.undefined_val;
        const call_args = [_]value.JSValue{ elem, value.JSValue.fromInt(@intCast(i)) };
        const keep = invokeCallback(call_fn, callback, &call_args) orelse value.JSValue.false_val;
        if (keep.toBoolean()) {
            result.arrayPush(ctx.allocator, elem) catch return value.JSValue.undefined_val;
        }
    }
    return result.toValue();
}

/// Array.prototype.reduce(callback, initialValue?) - Reduce to single value
pub fn arrayReduce(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const callback = getCallbackArg(args) orelse return value.JSValue.undefined_val;
    const call_fn = getCallFn() orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj, ctx.hidden_class_pool);

    var accumulator: value.JSValue = value.JSValue.undefined_val;
    var start_idx: u32 = 0;

    if (args.len > 1) {
        accumulator = args[1];
    } else if (len > 0) {
        accumulator = obj.getIndex(0) orelse value.JSValue.undefined_val;
        start_idx = 1;
    } else {
        return value.JSValue.undefined_val;
    }

    var i: u32 = start_idx;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = obj.getIndex(i) orelse value.JSValue.undefined_val;
        const call_args = [_]value.JSValue{ accumulator, elem, value.JSValue.fromInt(@intCast(i)) };
        accumulator = invokeCallback(call_fn, callback, &call_args) orelse value.JSValue.undefined_val;
    }
    return accumulator;
}

/// Array.prototype.forEach(callback, thisArg?) - Execute callback for each element
pub fn arrayForEach(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const callback = getCallbackArg(args) orelse return value.JSValue.undefined_val;
    const call_fn = getCallFn() orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj, ctx.hidden_class_pool);

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = obj.getIndex(i) orelse value.JSValue.undefined_val;
        const call_args = [_]value.JSValue{ elem, value.JSValue.fromInt(@intCast(i)) };
        _ = invokeCallback(call_fn, callback, &call_args);
    }
    return value.JSValue.undefined_val;
}

/// Array.prototype.every(callback, thisArg?) - Test if all elements pass
pub fn arrayEvery(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const callback = getCallbackArg(args) orelse return value.JSValue.true_val;
    const call_fn = getCallFn() orelse return value.JSValue.true_val;
    const len = getArrayLength(obj, ctx.hidden_class_pool);

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = obj.getIndex(i) orelse value.JSValue.undefined_val;
        const call_args = [_]value.JSValue{ elem, value.JSValue.fromInt(@intCast(i)) };
        const result = invokeCallback(call_fn, callback, &call_args) orelse value.JSValue.false_val;
        if (!result.toBoolean()) return value.JSValue.false_val;
    }
    return value.JSValue.true_val;
}

/// Array.prototype.some(callback, thisArg?) - Test if any element passes
pub fn arraySome(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const callback = getCallbackArg(args) orelse return value.JSValue.false_val;
    const call_fn = getCallFn() orelse return value.JSValue.false_val;
    const len = getArrayLength(obj, ctx.hidden_class_pool);

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = obj.getIndex(i) orelse value.JSValue.undefined_val;
        const call_args = [_]value.JSValue{ elem, value.JSValue.fromInt(@intCast(i)) };
        const result = invokeCallback(call_fn, callback, &call_args) orelse value.JSValue.false_val;
        if (result.toBoolean()) return value.JSValue.true_val;
    }
    return value.JSValue.false_val;
}

/// Array.prototype.find(callback, thisArg?) - Find first matching element
pub fn arrayFind(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const callback = getCallbackArg(args) orelse return value.JSValue.undefined_val;
    const call_fn = getCallFn() orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj, ctx.hidden_class_pool);

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = obj.getIndex(i) orelse value.JSValue.undefined_val;
        const call_args = [_]value.JSValue{ elem, value.JSValue.fromInt(@intCast(i)) };
        const result = invokeCallback(call_fn, callback, &call_args) orelse value.JSValue.false_val;
        if (result.toBoolean()) return elem;
    }
    return value.JSValue.undefined_val;
}

/// Array.prototype.findIndex(callback, thisArg?) - Find index of first matching element
pub fn arrayFindIndex(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const callback = getCallbackArg(args) orelse return value.JSValue.fromInt(-1);
    const call_fn = getCallFn() orelse return value.JSValue.fromInt(-1);
    const len = getArrayLength(obj, ctx.hidden_class_pool);

    var i: u32 = 0;
    while (i < @as(u32, @intCast(len))) : (i += 1) {
        const elem = obj.getIndex(i) orelse value.JSValue.undefined_val;
        const call_args = [_]value.JSValue{ elem, value.JSValue.fromInt(@intCast(i)) };
        const result = invokeCallback(call_fn, callback, &call_args) orelse value.JSValue.false_val;
        if (result.toBoolean()) return value.JSValue.fromInt(@intCast(i));
    }
    return value.JSValue.fromInt(-1);
}

/// Array.prototype.toSorted(compareFunc?) - Return new sorted array (non-mutating)
/// Implements default string comparison per JS spec. Custom compare functions not yet supported.
pub fn arrayToSorted(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    if (obj.class_id != .array) return value.JSValue.undefined_val;

    // Custom compare functions not yet supported
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Implement callback invocation for custom comparators
        // For now, ignore the compare function and use default sort
    }

    const len = obj.getArrayLength();
    if (len == 0) {
        const result = ctx.createArray() catch return value.JSValue.undefined_val;
        result.prototype = ctx.array_prototype;
        return result.toValue();
    }

    // Copy elements to temporary buffer
    const temp = ctx.allocator.alloc(value.JSValue, len) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(temp);

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        temp[i] = obj.getIndex(i) orelse value.JSValue.undefined_val;
    }

    // Sort using default string comparison (per JS spec)
    std.mem.sort(value.JSValue, temp, ctx.allocator, struct {
        pub fn lessThan(allocator: std.mem.Allocator, a: value.JSValue, b: value.JSValue) bool {
            // undefined values go to the end
            if (a.isUndefined() and b.isUndefined()) return false;
            if (a.isUndefined()) return false;
            if (b.isUndefined()) return true;

            // Convert to strings and compare lexicographically (JS default)
            const str_a = valueToStringSimple(allocator, a) catch return false;
            defer if (!a.isString()) allocator.destroy(str_a);
            const str_b = valueToStringSimple(allocator, b) catch return false;
            defer if (!b.isString()) allocator.destroy(str_b);

            const slice_a = str_a.data();
            const slice_b = str_b.data();

            // Lexicographic comparison
            const min_len = @min(slice_a.len, slice_b.len);
            for (slice_a[0..min_len], slice_b[0..min_len]) |ca, cb| {
                if (ca < cb) return true;
                if (ca > cb) return false;
            }
            return slice_a.len < slice_b.len;
        }
    }.lessThan);

    // Create result array with sorted elements
    const result = ctx.createArray() catch return value.JSValue.undefined_val;
    result.prototype = ctx.array_prototype;

    for (temp, 0..) |val, idx| {
        ctx.setIndexChecked(result, @intCast(idx), val) catch return value.JSValue.undefined_val;
    }
    result.setArrayLength(len);
    return result.toValue();
}
