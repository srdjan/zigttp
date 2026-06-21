const std = @import("std");
const h = @import("helpers.zig");
const value = h.value;
const context = h.context;
const string = h.string;
const object = h.object;

const getStringData = h.getStringData;
const getStringDataCtx = h.getStringDataCtx;
const getJSString = h.getJSString;
const getStringParent = h.getStringParent;
const allocFloat = h.allocFloat;

fn utf16CodeUnitAt(data: []const u8, target_idx: u32) ?u16 {
    var byte_idx: usize = 0;
    var unit_idx: u32 = 0;
    while (byte_idx < data.len) {
        const decoded = string.decodeCodepoint(data[byte_idx..]) orelse return null;
        if (decoded.codepoint <= 0xFFFF) {
            if (unit_idx == target_idx) return @intCast(decoded.codepoint);
            unit_idx += 1;
        } else {
            const scalar = decoded.codepoint - 0x10000;
            const high: u16 = 0xD800 + @as(u16, @intCast(scalar >> 10));
            const low: u16 = 0xDC00 + @as(u16, @intCast(scalar & 0x3FF));
            if (unit_idx == target_idx) return high;
            unit_idx += 1;
            if (unit_idx == target_idx) return low;
            unit_idx += 1;
        }
        byte_idx += decoded.len;
    }
    return null;
}

/// True when `this` is known to be pure ASCII, so UTF-16 code units coincide
/// with UTF-8 bytes. The flag is set from the actual bytes at creation and
/// conservatively ANDed across concats and inherited by slices, so a `true`
/// result is reliable; `false` only means "take the UTF-16 slow path".
fn stringIsAscii(this: value.JSValue) bool {
    if (this.isString()) return this.toPtr(string.JSString).flags.is_ascii;
    if (this.isStringSlice()) return this.toPtr(string.SliceString).flags.is_ascii;
    if (this.isRope()) return this.toPtr(string.RopeNode).flags.is_ascii;
    return false;
}

/// Count UTF-16 code units in UTF-8 `data`. Astral codepoints (> U+FFFF) count
/// as 2 (a surrogate pair); every BMP scalar counts as 1. Invalid bytes count
/// as one unit and advance one byte, matching the lenient decoding used by
/// `utf16CodeUnitAt`. ASCII bytes take a single-compare fast path.
fn utf16Length(data: []const u8) u32 {
    var units: u32 = 0;
    var i: usize = 0;
    while (i < data.len) {
        const b = data[i];
        if (b < 0x80) {
            units += 1;
            i += 1;
            continue;
        }
        const decoded = string.decodeCodepoint(data[i..]) orelse {
            units += 1;
            i += 1;
            continue;
        };
        units += if (decoded.codepoint > 0xFFFF) @as(u32, 2) else 1;
        i += decoded.len;
    }
    return units;
}

/// Map a UTF-16 code-unit index to a byte offset into UTF-8 `data`, clamped to
/// `data.len`. The returned offset always lands on a UTF-8 codepoint boundary,
/// so slicing with it can never split a multi-byte sequence. When `target`
/// falls between the two units of a surrogate pair (an astral codepoint), the
/// offset rounds down to the boundary before that codepoint: this engine's
/// UTF-8 string model cannot represent a lone surrogate, so an astral codepoint
/// is treated atomically rather than producing invalid UTF-8.
fn utf16IndexToByteOffset(data: []const u8, target: u32) usize {
    var units: u32 = 0;
    var i: usize = 0;
    while (i < data.len) {
        if (units >= target) return i;
        const b = data[i];
        if (b < 0x80) {
            units += 1;
            i += 1;
            continue;
        }
        const decoded = string.decodeCodepoint(data[i..]) orelse {
            units += 1;
            i += 1;
            continue;
        };
        const w: u32 = if (decoded.codepoint > 0xFFFF) 2 else 1;
        if (units + w > target) return i; // target splits this codepoint: round down
        units += w;
        i += decoded.len;
    }
    return data.len;
}

/// Return the UTF-8 byte slice of the codepoint whose UTF-16 code-unit range
/// covers `target`, or null when `target` is past the end. An astral codepoint
/// spans two units; both map to the whole codepoint here, since a lone
/// surrogate cannot be represented in UTF-8 (documented charAt limitation).
fn charCodepointSliceAt(data: []const u8, target: u32) ?[]const u8 {
    var units: u32 = 0;
    var i: usize = 0;
    while (i < data.len) {
        const b = data[i];
        if (b < 0x80) {
            if (units == target) return data[i .. i + 1];
            units += 1;
            i += 1;
            continue;
        }
        const decoded = string.decodeCodepoint(data[i..]) orelse {
            if (units == target) return data[i .. i + 1];
            units += 1;
            i += 1;
            continue;
        };
        const w: u32 = if (decoded.codepoint > 0xFFFF) 2 else 1;
        if (target >= units and target < units + w) return data[i .. i + decoded.len];
        units += w;
        i += decoded.len;
    }
    return null;
}

/// String.prototype.length - Get string length
pub fn stringLength(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    // ASCII fast path: UTF-16 code units equal byte length, so stay O(1) and
    // avoid flattening concat ropes.
    if (stringIsAscii(this)) {
        if (this.isRope()) {
            return value.JSValue.fromInt(@intCast(this.toPtr(string.RopeNode).total_len));
        }
        if (getStringDataCtx(this, ctx)) |data| {
            return value.JSValue.fromInt(@intCast(data.len));
        }
        return value.JSValue.fromInt(0);
    }
    // Non-ASCII: JS .length counts UTF-16 code units, not UTF-8 bytes.
    if (getStringDataCtx(this, ctx)) |data| {
        return value.JSValue.fromInt(@intCast(utf16Length(data)));
    }
    return value.JSValue.fromInt(0);
}

/// String.prototype.charAt(index) - Get character at index
pub fn stringCharAt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.undefined_val;
    var idx: u32 = 0;
    if (args.len > 0) {
        var i: i32 = 0;
        if (args[0].isInt()) {
            i = args[0].getInt();
        } else if (args[0].toNumber()) |n| {
            if (std.math.isFinite(n)) i = std.math.lossyCast(i32, @trunc(n));
        }
        if (i < 0) return value.JSValue.fromPtr(string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val);
        idx = @intCast(i);
    }
    // ASCII fast path: byte index == UTF-16 code-unit index.
    if (stringIsAscii(this)) {
        if (idx < data.len) {
            const result = string.createString(ctx.allocator, data[idx .. idx + 1]) catch return value.JSValue.undefined_val;
            return value.JSValue.fromPtr(result);
        }
        return value.JSValue.fromPtr(string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val);
    }
    // Non-ASCII: index by UTF-16 code unit. JS returns a lone surrogate for an
    // astral half; this UTF-8 model cannot, so an astral codepoint is returned
    // whole for either of its two unit indices (documented limitation).
    if (charCodepointSliceAt(data, idx)) |slice| {
        const result = string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }
    return value.JSValue.fromPtr(string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val);
}

/// String.prototype.charCodeAt(index) - Get char code at index
pub fn stringCharCodeAt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const data = getStringDataCtx(this, ctx) orelse return allocFloat(ctx, std.math.nan(f64));
    var idx: u32 = 0;
    if (args.len > 0) {
        var i: i32 = 0;
        if (args[0].isInt()) {
            i = args[0].getInt();
        } else if (args[0].toNumber()) |n| {
            if (std.math.isFinite(n)) i = std.math.lossyCast(i32, @trunc(n));
        }
        if (i < 0) return allocFloat(ctx, std.math.nan(f64));
        idx = @intCast(i);
    }
    if (utf16CodeUnitAt(data, idx)) |unit| {
        return value.JSValue.fromInt(@intCast(unit));
    }
    return allocFloat(ctx, std.math.nan(f64));
}

/// String.prototype.indexOf(searchString, position?) - Find substring
pub fn stringIndexOf(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromInt(-1);

    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.fromInt(-1);
    const needle_data = getStringDataCtx(args[0], ctx) orelse return value.JSValue.fromInt(-1);

    // Get start position (default 0)
    var start: usize = 0;
    if (args.len > 1) {
        if (args[1].isInt()) {
            const pos = args[1].getInt();
            if (pos >= 0) {
                start = @intCast(pos);
            }
        } else if (args[1].toNumber()) |n| {
            if (n >= 0) {
                start = std.math.lossyCast(usize, @floor(n));
            }
        }
    }

    // Empty needle: return start clamped to [0, data.len].
    if (needle_data.len == 0) return value.JSValue.fromInt(@intCast(@min(start, data.len)));

    // Clamp start to string length
    if (start >= data.len) return value.JSValue.fromInt(-1);
    if (needle_data.len > data.len - start) return value.JSValue.fromInt(-1);

    // Search manually from start position
    const search_area = data[start..];
    if (std.mem.indexOf(u8, search_area, needle_data)) |rel_idx| {
        return value.JSValue.fromInt(@intCast(start + rel_idx));
    }
    return value.JSValue.fromInt(-1);
}

/// String.prototype.lastIndexOf(searchString, position?) - Find substring from end
pub fn stringLastIndexOf(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromInt(-1);

    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.fromInt(-1);
    const needle_data = getStringDataCtx(args[0], ctx) orelse return value.JSValue.fromInt(-1);

    // Compute search end bound from optional position argument (default = data.len)
    var end: usize = data.len;
    if (args.len > 1) {
        if (args[1].isInt()) {
            const pos = args[1].getInt();
            if (pos >= 0) {
                end = @min(@as(usize, @intCast(pos)) + needle_data.len, data.len);
            } else {
                end = 0;
            }
        } else if (args[1].toNumber()) |n| {
            if (std.math.isNan(n)) {
                // ECMAScript spec: NaN position → +Infinity → search from end; leave end = data.len.
            } else if (n >= 0) {
                const floored = std.math.lossyCast(usize, @floor(n));
                end = @min(floored +| needle_data.len, data.len);
            } else {
                end = 0;
            }
        }
    }

    if (needle_data.len == 0) return value.JSValue.fromInt(@intCast(@min(end, data.len)));
    if (needle_data.len > end) return value.JSValue.fromInt(-1);

    if (std.mem.lastIndexOf(u8, data[0..end], needle_data)) |idx| {
        return value.JSValue.fromInt(@intCast(idx));
    }
    return value.JSValue.fromInt(-1);
}

/// String.prototype.startsWith(searchString, position?) - Check prefix
pub fn stringStartsWith(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.false_val;

    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.false_val;
    const prefix_data = getStringDataCtx(args[0], ctx) orelse return value.JSValue.false_val;

    var pos: usize = 0;
    if (args.len > 1) {
        if (args[1].isInt()) {
            const p = args[1].getInt();
            if (p > 0) pos = @intCast(p);
        } else if (args[1].toNumber()) |n| {
            if (n > 0) pos = std.math.lossyCast(usize, @floor(n));
        }
    }
    if (pos > data.len) return value.JSValue.false_val;
    const view = data[pos..];
    if (prefix_data.len > view.len) return value.JSValue.false_val;
    return if (string.eqlStrings(view[0..prefix_data.len], prefix_data)) value.JSValue.true_val else value.JSValue.false_val;
}

/// String.prototype.endsWith(searchString, endPosition?) - Check suffix
pub fn stringEndsWith(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.false_val;

    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.false_val;
    const suffix_data = getStringDataCtx(args[0], ctx) orelse return value.JSValue.false_val;

    var end: usize = data.len;
    if (args.len > 1) {
        if (args[1].isInt()) {
            const p = args[1].getInt();
            if (p >= 0) end = @min(@as(usize, @intCast(p)), data.len);
        } else if (args[1].toNumber()) |n| {
            if (n >= 0) end = @min(std.math.lossyCast(usize, @floor(n)), data.len);
        }
    }
    if (suffix_data.len > end) return value.JSValue.false_val;
    const start = end - suffix_data.len;
    return if (string.eqlStrings(data[start..end], suffix_data)) value.JSValue.true_val else value.JSValue.false_val;
}

/// String.prototype.includes(searchString, position?) - Check if contains
pub fn stringIncludes(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const result = stringIndexOf(ctx, this, args);
    if (result.isInt() and result.getInt() >= 0) {
        return value.JSValue.true_val;
    }
    return value.JSValue.false_val;
}

/// String.prototype.slice(start, end?) - Extract substring
/// Uses zero-copy SliceString for substrings >= 16 bytes
pub fn stringSlice(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Get data from any string type (flatten ropes if needed)
    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.undefined_val;

    // Indices are UTF-16 code units. For ASCII the count equals the byte length
    // and unit index equals byte offset, so the conversions below are no-ops.
    const is_ascii = stringIsAscii(this);
    const len_units: u32 = if (is_ascii) @intCast(data.len) else utf16Length(data);

    var start: i32 = 0;
    var end: i32 = @intCast(len_units);

    if (args.len > 0) {
        if (args[0].isInt()) {
            start = args[0].getInt();
        } else if (args[0].toNumber()) |n| {
            if (std.math.isFinite(n)) {
                start = std.math.lossyCast(i32, @trunc(n));
            } else if (n > 0) { // +Infinity → clamp to len below
                start = @intCast(len_units);
            } // NaN / -Infinity → start stays 0
        }
        if (start < 0) start = @max(0, @as(i32, @intCast(len_units)) + start);
    }
    if (args.len > 1) {
        if (args[1].isInt()) {
            end = args[1].getInt();
        } else if (args[1].toNumber()) |n| {
            if (std.math.isFinite(n)) {
                end = std.math.lossyCast(i32, @trunc(n));
            } else if (n > 0) { // +Infinity → len
                end = @intCast(len_units);
            } else { // -Infinity / NaN → 0
                end = 0;
            }
        }
        if (end < 0) end = @max(0, @as(i32, @intCast(len_units)) + end);
    }

    if (start < 0) start = 0;
    if (end < 0) end = 0;
    if (start > @as(i32, @intCast(len_units))) start = @intCast(len_units);
    if (end > @as(i32, @intCast(len_units))) end = @intCast(len_units);
    if (end < start) end = start;

    // Map UTF-16 code-unit indices to UTF-8 byte offsets at codepoint boundaries.
    const byte_start: u32 = if (is_ascii) @intCast(start) else @intCast(utf16IndexToByteOffset(data, @intCast(start)));
    const byte_end: u32 = if (is_ascii) @intCast(end) else @intCast(utf16IndexToByteOffset(data, @intCast(end)));
    const slice_len = byte_end - byte_start;

    // Fast path: copy if slice < 16 bytes (SliceString overhead exceeds copy cost)
    if (slice_len < string.SliceString.MIN_SLICE_LEN) {
        const slice = data[byte_start..byte_end];
        const result = ctx.createStringPtr(slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }

    // Zero-copy slice for larger substrings - need the flat parent string
    if (this.isString()) {
        const str = this.toPtr(string.JSString);
        const slice_str = ctx.createSlicePtr(str, byte_start, slice_len) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(slice_str);
    }

    // For SliceString: adjust offset relative to parent
    if (this.isStringSlice()) {
        const existing_slice = this.toPtr(string.SliceString);
        const new_offset = existing_slice.offset + byte_start;
        const slice_str = ctx.createSlicePtr(existing_slice.parent, new_offset, slice_len) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(slice_str);
    }

    // For other string types, copy
    const slice = data[byte_start..byte_end];
    const result = ctx.createStringPtr(slice) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.substring(start, end?) - Extract substring
/// Uses zero-copy SliceString for substrings >= 16 bytes
pub fn stringSubstring(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Get data from any string type (flatten ropes if needed)
    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.undefined_val;

    // Indices are UTF-16 code units. For ASCII the count equals the byte length
    // and unit index equals byte offset, so the conversions below are no-ops.
    const is_ascii = stringIsAscii(this);
    const len_units: u32 = if (is_ascii) @intCast(data.len) else utf16Length(data);

    var start: i32 = 0;
    var end: i32 = @intCast(len_units);

    if (args.len > 0) {
        if (args[0].isInt()) {
            start = args[0].getInt();
        } else if (args[0].toNumber()) |n| {
            if (std.math.isFinite(n)) {
                start = std.math.lossyCast(i32, @trunc(n));
            } else if (n > 0) { // +Infinity → len
                start = @intCast(len_units);
            } // NaN / -Infinity → start stays 0
        }
    }
    if (args.len > 1) {
        if (args[1].isInt()) {
            end = args[1].getInt();
        } else if (args[1].toNumber()) |n| {
            if (std.math.isFinite(n)) {
                end = std.math.lossyCast(i32, @trunc(n));
            } else if (n > 0) { // +Infinity → len
                end = @intCast(len_units);
            } else { // NaN / -Infinity → 0 per spec
                end = 0;
            }
        }
    }

    if (start < 0) start = 0;
    if (end < 0) end = 0;
    if (start > @as(i32, @intCast(len_units))) start = @intCast(len_units);
    if (end > @as(i32, @intCast(len_units))) end = @intCast(len_units);
    if (start > end) {
        const tmp = start;
        start = end;
        end = tmp;
    }

    // Map UTF-16 code-unit indices to UTF-8 byte offsets at codepoint boundaries.
    const byte_start: u32 = if (is_ascii) @intCast(start) else @intCast(utf16IndexToByteOffset(data, @intCast(start)));
    const byte_end: u32 = if (is_ascii) @intCast(end) else @intCast(utf16IndexToByteOffset(data, @intCast(end)));
    const slice_len = byte_end - byte_start;

    // Fast path: copy if slice < 16 bytes (SliceString overhead exceeds copy cost)
    if (slice_len < string.SliceString.MIN_SLICE_LEN) {
        const slice = data[byte_start..byte_end];
        const result = ctx.createStringPtr(slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }

    // Zero-copy slice for larger substrings - need the flat parent string
    if (this.isString()) {
        const str = this.toPtr(string.JSString);
        const slice_str = ctx.createSlicePtr(str, byte_start, slice_len) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(slice_str);
    }

    // For SliceString: adjust offset relative to parent
    if (this.isStringSlice()) {
        const existing_slice = this.toPtr(string.SliceString);
        const new_offset = existing_slice.offset + byte_start;
        const slice_str = ctx.createSlicePtr(existing_slice.parent, new_offset, slice_len) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(slice_str);
    }

    // For other string types, copy
    const slice = data[byte_start..byte_end];
    const result = ctx.createStringPtr(slice) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.toLowerCase() - Convert to lowercase
pub fn stringToLowerCase(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    const data = getStringDataCtx(this, ctx) orelse return this;
    if (data.len == 0) return this;

    const result = string.createString(ctx.allocator, data) catch return this;
    const result_data = result.dataMut();
    for (result_data) |*c| {
        c.* = std.ascii.toLower(c.*);
    }
    return value.JSValue.fromPtr(result);
}

/// String.prototype.toUpperCase() - Convert to uppercase
pub fn stringToUpperCase(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    const data = getStringDataCtx(this, ctx) orelse return this;
    if (data.len == 0) return this;

    const result = string.createString(ctx.allocator, data) catch return this;
    const result_data = result.dataMut();
    for (result_data) |*c| {
        c.* = std.ascii.toUpper(c.*);
    }
    return value.JSValue.fromPtr(result);
}

/// String.prototype.trim() - Remove whitespace from both ends
pub fn stringTrim(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    const data = getStringDataCtx(this, ctx) orelse return this;
    var start: usize = 0;
    var end: usize = data.len;
    while (start < end and std.ascii.isWhitespace(data[start])) : (start += 1) {}
    while (end > start and std.ascii.isWhitespace(data[end - 1])) : (end -= 1) {}
    const slice = data[start..end];
    const result = string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.trimStart() - Remove whitespace from start
pub fn stringTrimStart(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    const data = getStringDataCtx(this, ctx) orelse return this;
    var start: usize = 0;
    while (start < data.len and std.ascii.isWhitespace(data[start])) : (start += 1) {}
    const slice = data[start..];
    const result = string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.trimEnd() - Remove whitespace from end
pub fn stringTrimEnd(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    const data = getStringDataCtx(this, ctx) orelse return this;
    var end: usize = data.len;
    while (end > 0 and std.ascii.isWhitespace(data[end - 1])) : (end -= 1) {}
    const slice = data[0..end];
    const result = string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.split(separator, limit?) - Split into array
pub fn stringSplit(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.undefined_val;

    const result = ctx.createArray() catch return value.JSValue.undefined_val;
    result.prototype = ctx.array_prototype;

    var limit: ?u32 = null;
    if (args.len > 1 and args[1].isInt()) {
        const lim = args[1].getInt();
        if (lim <= 0) {
            result.setArrayLength(0);
            return result.toValue();
        }
        limit = @intCast(lim);
    }

    if (args.len == 0 or args[0].isUndefined()) {
        const part_str = string.createString(ctx.allocator, data) catch return value.JSValue.undefined_val;
        ctx.setIndexChecked(result, 0, value.JSValue.fromPtr(part_str)) catch return value.JSValue.undefined_val;
        result.setArrayLength(1);
        return result.toValue();
    }

    const sep = getStringDataCtx(args[0], ctx) orelse return value.JSValue.undefined_val;
    if (sep.len == 0) {
        // Empty separator: split into individual UTF-8 code points
        var count: u32 = 0;
        var i: usize = 0;
        while (i < data.len) {
            if (limit) |lim| if (count >= lim) break;
            const seq_len = std.unicode.utf8ByteSequenceLength(data[i]) catch 1;
            const end = @min(i + seq_len, data.len);
            const ch_str = string.createString(ctx.allocator, data[i..end]) catch return value.JSValue.undefined_val;
            ctx.setIndexChecked(result, count, value.JSValue.fromPtr(ch_str)) catch return value.JSValue.undefined_val;
            count += 1;
            i = end;
        }
        result.setArrayLength(count);
        return result.toValue();
    }

    var iter = std.mem.splitSequence(u8, data, sep);
    var count: u32 = 0;
    while (iter.next()) |part| {
        if (limit) |lim| {
            if (count >= lim) break;
        }
        const part_str = string.createString(ctx.allocator, part) catch return value.JSValue.undefined_val;
        ctx.setIndexChecked(result, count, value.JSValue.fromPtr(part_str)) catch return value.JSValue.undefined_val;
        count += 1;
    }
    result.setArrayLength(count);
    return result.toValue();
}

/// String.prototype.repeat(count) - Repeat string
pub fn stringRepeat(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.undefined_val;
    var count: u32 = 0;
    if (args.len > 0) {
        if (args[0].isInt()) {
            const c = args[0].getInt();
            if (c < 0) return value.JSValue.undefined_val;
            count = @intCast(c);
        } else if (args[0].toNumber()) |n| {
            if (n < 0 or std.math.isInf(n)) return value.JSValue.undefined_val;
            count = std.math.lossyCast(u32, @floor(n));
        }
    }
    if (count == 0 or data.len == 0) {
        const empty = string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(empty);
    }
    if (count == 1) return this;

    const total_len = @as(usize, data.len) * @as(usize, count);
    if (total_len > 1024 * 1024) return value.JSValue.undefined_val; // 1MB safety limit

    const buf = ctx.allocator.alloc(u8, total_len) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(buf);
    var pos: usize = 0;
    for (0..count) |_| {
        @memcpy(buf[pos..][0..data.len], data);
        pos += data.len;
    }
    const result = string.createString(ctx.allocator, buf) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.padStart(targetLength, padString?) - Pad at start
pub fn stringPadStart(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const data = getStringDataCtx(this, ctx) orelse return this;
    if (args.len == 0) return this;
    const target_len: usize = blk: {
        if (args[0].isInt()) {
            const t = args[0].getInt();
            break :blk if (t > 0) @intCast(t) else 0;
        }
        if (args[0].toNumber()) |n| {
            if (n > 0) break :blk std.math.lossyCast(usize, @floor(n));
        }
        break :blk 0;
    };
    if (target_len <= data.len) return this;

    const pad_str = if (args.len > 1) (getStringDataCtx(args[1], ctx) orelse " ") else " ";
    if (pad_str.len == 0) return this;

    const pad_needed = target_len - data.len;
    const buf = ctx.allocator.alloc(u8, target_len) catch return this;
    defer ctx.allocator.free(buf);

    // Fill padding
    var pos: usize = 0;
    while (pos < pad_needed) {
        const copy_len = @min(pad_str.len, pad_needed - pos);
        @memcpy(buf[pos..][0..copy_len], pad_str[0..copy_len]);
        pos += copy_len;
    }
    // Copy original string
    @memcpy(buf[pad_needed..][0..data.len], data);

    const result = string.createString(ctx.allocator, buf) catch return this;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.padEnd(targetLength, padString?) - Pad at end
pub fn stringPadEnd(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const data = getStringDataCtx(this, ctx) orelse return this;
    if (args.len == 0) return this;
    const target_len: usize = blk: {
        if (args[0].isInt()) {
            const t = args[0].getInt();
            break :blk if (t > 0) @intCast(t) else 0;
        }
        if (args[0].toNumber()) |n| {
            if (n > 0) break :blk std.math.lossyCast(usize, @floor(n));
        }
        break :blk 0;
    };
    if (target_len <= data.len) return this;

    const pad_str = if (args.len > 1) (getStringDataCtx(args[1], ctx) orelse " ") else " ";
    if (pad_str.len == 0) return this;

    const buf = ctx.allocator.alloc(u8, target_len) catch return this;
    defer ctx.allocator.free(buf);

    // Copy original string
    @memcpy(buf[0..data.len], data);
    // Fill padding
    var pos: usize = data.len;
    while (pos < target_len) {
        const copy_len = @min(pad_str.len, target_len - pos);
        @memcpy(buf[pos..][0..copy_len], pad_str[0..copy_len]);
        pos += copy_len;
    }

    const result = string.createString(ctx.allocator, buf) catch return this;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.concat(...strings) - Concatenate strings
/// Uses concatMany for efficient single-allocation concatenation
pub fn stringConcat(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;

    // Fast path: no args, return this
    if (args.len == 0) return this;

    // Get this as string (handle all string types: JSString, SliceString, Rope)
    const this_str: *const string.JSString = blk: {
        if (this.isString()) break :blk this.toPtr(string.JSString);
        if (this.isStringSlice()) {
            const slice = this.toPtr(string.SliceString);
            break :blk slice.flatten(allocator) catch return this;
        }
        if (this.isRope()) {
            const rope = this.toPtr(string.RopeNode);
            break :blk rope.flatten(allocator) catch return this;
        }
        return this;
    };

    // Collect all strings to concatenate
    var strings: std.ArrayListUnmanaged(*const string.JSString) = .empty;
    defer strings.deinit(allocator);

    strings.append(allocator, this_str) catch return this;

    for (args) |arg| {
        if (arg.isString()) {
            strings.append(allocator, arg.toPtr(string.JSString)) catch return this;
        } else if (arg.isStringSlice()) {
            const slice = arg.toPtr(string.SliceString);
            const flat = slice.flatten(allocator) catch return this;
            strings.append(allocator, flat) catch return this;
        } else if (arg.isRope()) {
            const rope = arg.toPtr(string.RopeNode);
            const flat = rope.flatten(allocator) catch return this;
            strings.append(allocator, flat) catch return this;
        }
    }

    // Use concatMany for efficient single-allocation concatenation
    const result = string.concatMany(allocator, strings.items) catch return this;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.replace(searchValue, replaceValue) - Replace first occurrence
pub fn stringReplace(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;

    if (args.len < 2) return this;
    const input = getStringDataCtx(this, ctx) orelse return this;
    const replacement = getStringDataCtx(args[1], ctx) orelse "";
    const search = getStringDataCtx(args[0], ctx) orelse return this;

    if (std.mem.indexOf(u8, input, search)) |idx| {
        var result = std.ArrayList(u8).empty;
        defer result.deinit(allocator);
        result.appendSlice(allocator, input[0..idx]) catch return this;
        result.appendSlice(allocator, replacement) catch return this;
        result.appendSlice(allocator, input[idx + search.len ..]) catch return this;
        const new_str = string.createString(allocator, result.items) catch return this;
        return value.JSValue.fromPtr(new_str);
    }

    return this;
}

/// String.prototype.replaceAll(searchValue, replaceValue) - Replace all occurrences
pub fn stringReplaceAll(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;

    if (args.len < 2) return this;
    const input = getStringDataCtx(this, ctx) orelse return this;

    const replacement = getStringDataCtx(args[1], ctx) orelse "";

    // String search - replace all
    const search = getStringDataCtx(args[0], ctx) orelse return this;
    if (search.len > 0) {
        var result = std.ArrayList(u8).empty;
        defer result.deinit(allocator);
        var pos: usize = 0;
        while (std.mem.indexOfPos(u8, input, pos, search)) |idx| {
            result.appendSlice(allocator, input[pos..idx]) catch return this;
            result.appendSlice(allocator, replacement) catch return this;
            pos = idx + search.len;
        }
        result.appendSlice(allocator, input[pos..]) catch return this;
        const new_str = string.createString(allocator, result.items) catch return this;
        return value.JSValue.fromPtr(new_str);
    }

    return this;
}

/// String(value) - Global String conversion function.
/// Converts the first argument to its string form: strings pass through,
/// numbers/booleans/undefined/null produce their canonical text, and objects
/// stringify to "[object Object]". String() with no arguments returns "".
pub fn stringConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) {
        return value.JSValue.fromPtr(string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val);
    }
    const val = args[0];

    // String(s) on a value that is already a string (flat, slice, or rope)
    // returns it unchanged - no allocation or copy, matching JS semantics.
    if (val.isAnyString()) return val;

    if (val.isInt()) {
        var buf: [32]u8 = undefined;
        const slice = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val);
    }
    if (val.isFloat()) {
        var buf: [64]u8 = undefined;
        const slice = string.formatFloatToBuf(&buf, val.getFloat64());
        return value.JSValue.fromPtr(string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val);
    }

    const literal: []const u8 = if (val.isUndefined())
        "undefined"
    else if (val.isNull())
        "null"
    else if (val.isTrue())
        "true"
    else if (val.isFalse())
        "false"
    else
        "[object Object]";

    return value.JSValue.fromPtr(string.createString(ctx.allocator, literal) catch return value.JSValue.undefined_val);
}

/// String.fromCharCode(...charCodes) - Create string from char codes
pub fn stringFromCharCode(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.fromPtr(string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val);

    // Each code point needs at most 3 UTF-8 bytes (BMP range 0-0xFFFF)
    var buf: [64 * 3]u8 = undefined;
    var out_len: usize = 0;
    const max_args = @min(args.len, 64);
    for (0..max_args) |i| {
        const raw_code: i32 = blk: {
            if (args[i].isInt()) {
                break :blk args[i].getInt();
            } else if (args[i].toNumber()) |n| {
                // NaN/Infinity: ToUint16 maps both to 0 per the JS spec.
                break :blk if (std.math.isFinite(n)) @intFromFloat(@mod(@trunc(n), 65536.0)) else 0;
            } else {
                break :blk 0;
            }
        };
        // Clamp to valid BMP range (mod 65536 per JS spec); @mod returns [0,65535] for positive divisor
        const code: u21 = @intCast(@mod(raw_code, @as(i32, 65536)));
        const enc_len = std.unicode.utf8Encode(code, buf[out_len..]) catch blk: {
            buf[out_len] = '?';
            break :blk 1;
        };
        out_len += enc_len;
    }
    const result = string.createString(ctx.allocator, buf[0..out_len]) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

test "stringCharCodeAt returns UTF-16 code units" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("../gc.zig");
    const heap_mod = @import("../heap.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();
    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const input = try ctx.createString("Aé😀");

    const a = stringCharCodeAt(ctx, input, &.{value.JSValue.fromInt(0)});
    try std.testing.expectEqual(@as(i32, 0x41), a.getInt());

    const e_acute = stringCharCodeAt(ctx, input, &.{value.JSValue.fromInt(1)});
    try std.testing.expectEqual(@as(i32, 0x00E9), e_acute.getInt());

    const high = stringCharCodeAt(ctx, input, &.{value.JSValue.fromInt(2)});
    try std.testing.expectEqual(@as(i32, 0xD83D), high.getInt());

    const low = stringCharCodeAt(ctx, input, &.{value.JSValue.fromInt(3)});
    try std.testing.expectEqual(@as(i32, 0xDE00), low.getInt());

    const out_of_range = stringCharCodeAt(ctx, input, &.{value.JSValue.fromInt(4)});
    try std.testing.expect(out_of_range.isFloat64());
    try std.testing.expect(std.math.isNan(out_of_range.getFloat64()));
}

test "string length/charAt/slice index by UTF-16 code units ENG-utf16" {
    // Regression for byte-indexed string methods: on the pre-change engine
    // "é".length was 2 and slice/charAt cut through UTF-8 sequences.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("../gc.zig");
    const heap_mod = @import("../heap.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();
    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const noargs = [_]value.JSValue{};

    // .length counts UTF-16 code units, not UTF-8 bytes.
    const e_acute = try ctx.createString("é"); // 2 bytes, 1 code unit
    try std.testing.expectEqual(@as(i32, 1), stringLength(ctx, e_acute, &noargs).getInt());

    const grin = try ctx.createString("😀"); // 4 bytes, 2 code units (surrogate pair)
    try std.testing.expectEqual(@as(i32, 2), stringLength(ctx, grin, &noargs).getInt());

    // ASCII length is unchanged (fast path).
    const ascii = try ctx.createString("hello");
    try std.testing.expectEqual(@as(i32, 5), stringLength(ctx, ascii, &noargs).getInt());

    // h é l l o : 5 code units across 6 bytes.
    const hello = try ctx.createString("héllo");
    try std.testing.expectEqual(@as(i32, 5), stringLength(ctx, hello, &noargs).getInt());

    // charAt returns whole codepoints and never splits a UTF-8 sequence.
    try std.testing.expectEqualStrings("é", getStringData(stringCharAt(ctx, hello, &.{value.JSValue.fromInt(1)})) orelse return error.TestUnexpectedResult);
    try std.testing.expectEqualStrings("o", getStringData(stringCharAt(ctx, hello, &.{value.JSValue.fromInt(4)})) orelse return error.TestUnexpectedResult);

    // slice / substring clamp by code units and cut on codepoint boundaries.
    try std.testing.expectEqualStrings("hé", getStringData(stringSlice(ctx, hello, &.{ value.JSValue.fromInt(0), value.JSValue.fromInt(2) })) orelse return error.TestUnexpectedResult);
    try std.testing.expectEqualStrings("éllo", getStringData(stringSlice(ctx, hello, &.{value.JSValue.fromInt(1)})) orelse return error.TestUnexpectedResult);
    try std.testing.expectEqualStrings("é", getStringData(stringSubstring(ctx, hello, &.{ value.JSValue.fromInt(1), value.JSValue.fromInt(2) })) orelse return error.TestUnexpectedResult);

    // Astral charAt: a lone surrogate cannot be represented in UTF-8, so the
    // whole codepoint is returned for either unit index (documented limitation).
    try std.testing.expectEqualStrings("😀", getStringData(stringCharAt(ctx, grin, &.{value.JSValue.fromInt(0)})) orelse return error.TestUnexpectedResult);
    try std.testing.expectEqualStrings("😀", getStringData(stringCharAt(ctx, grin, &.{value.JSValue.fromInt(1)})) orelse return error.TestUnexpectedResult);

    // Zero-copy SliceString path (>= 16-byte result) with leading non-ASCII:
    // "é" (2 bytes) + 20 ASCII bytes = 21 code units. Slicing from unit 1 must
    // start at the byte after the 2-byte 'é'.
    const long_input = "é" ++ ("a" ** 20);
    const long = try ctx.createString(long_input);
    try std.testing.expectEqual(@as(i32, 21), stringLength(ctx, long, &noargs).getInt());
    try std.testing.expectEqualStrings(long_input, getStringData(stringSlice(ctx, long, &.{ value.JSValue.fromInt(0), value.JSValue.fromInt(21) })) orelse return error.TestUnexpectedResult);
    try std.testing.expectEqualStrings("a" ** 20, getStringData(stringSlice(ctx, long, &.{value.JSValue.fromInt(1)})) orelse return error.TestUnexpectedResult);
}

test "stringConstructor converts scalars to strings ENG-String" {
    // The global String(x) must convert numbers and booleans to their text
    // forms (previously String() was unregistered and threw NotCallable -> 500).
    // A self-managed DebugAllocator that is intentionally not deinit'd: the
    // ctx-created JSStrings are GC-managed and not freed individually in tests.
    var dbg: std.heap.DebugAllocator(.{}) = .init;
    const allocator = dbg.allocator();
    const gc_mod = @import("../gc.zig");
    const heap_mod = @import("../heap.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();
    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const num = stringConstructor(ctx, value.JSValue.undefined_val, &.{value.JSValue.fromInt(5)});
    try std.testing.expectEqualStrings("5", getStringData(num) orelse return error.TestUnexpectedResult);

    const yes = stringConstructor(ctx, value.JSValue.undefined_val, &.{value.JSValue.fromBool(true)});
    try std.testing.expectEqualStrings("true", getStringData(yes) orelse return error.TestUnexpectedResult);

    const undef = stringConstructor(ctx, value.JSValue.undefined_val, &.{value.JSValue.undefined_val});
    try std.testing.expectEqualStrings("undefined", getStringData(undef) orelse return error.TestUnexpectedResult);

    const empty = stringConstructor(ctx, value.JSValue.undefined_val, &.{});
    try std.testing.expectEqualStrings("", getStringData(empty) orelse return error.TestUnexpectedResult);

    const flt = stringConstructor(ctx, value.JSValue.undefined_val, &.{value.JSValue.fromFloat(3.5)});
    try std.testing.expectEqualStrings("3.5", getStringData(flt) orelse return error.TestUnexpectedResult);
}
