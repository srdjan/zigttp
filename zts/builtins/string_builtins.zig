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

/// String.prototype.length - Get string length
pub fn stringLength(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    if (getStringDataCtx(this, ctx)) |data| {
        return value.JSValue.fromInt(@intCast(data.len));
    }
    // For ropes, use total_len directly without flattening
    if (this.isRope()) {
        const rope = this.toPtr(string.RopeNode);
        return value.JSValue.fromInt(@intCast(rope.total_len));
    }
    return value.JSValue.fromInt(0);
}

/// String.prototype.charAt(index) - Get character at index
pub fn stringCharAt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.undefined_val;
    var idx: u32 = 0;
    if (args.len > 0 and args[0].isInt()) {
        const i = args[0].getInt();
        if (i < 0) return value.JSValue.undefined_val;
        idx = @intCast(i);
    }
    if (idx < data.len) {
        const result = string.createString(ctx.allocator, data[idx .. idx + 1]) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }
    return value.JSValue.fromPtr(string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val);
}

/// String.prototype.charCodeAt(index) - Get char code at index
pub fn stringCharCodeAt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const data = getStringDataCtx(this, ctx) orelse return allocFloat(ctx, std.math.nan(f64));
    var idx: u32 = 0;
    if (args.len > 0 and args[0].isInt()) {
        const i = args[0].getInt();
        if (i < 0) return allocFloat(ctx, std.math.nan(f64));
        idx = @intCast(i);
    }
    if (idx < data.len) {
        return value.JSValue.fromInt(@intCast(data[idx]));
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
                const floored = @floor(n);
                if (floored < @as(f64, @floatFromInt(data.len))) {
                    start = @intFromFloat(floored);
                }
            }
        }
    }

    // Clamp start to string length
    if (start >= data.len) return value.JSValue.fromInt(-1);

    // Search in substring from start position
    if (needle_data.len == 0) return value.JSValue.fromInt(@intCast(start));
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

    if (needle_data.len == 0) return value.JSValue.fromInt(@intCast(data.len));
    if (needle_data.len > data.len) return value.JSValue.fromInt(-1);

    if (std.mem.lastIndexOf(u8, data, needle_data)) |idx| {
        return value.JSValue.fromInt(@intCast(idx));
    }
    return value.JSValue.fromInt(-1);
}

/// String.prototype.startsWith(searchString, position?) - Check prefix
pub fn stringStartsWith(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.false_val;

    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.false_val;
    const prefix_data = getStringDataCtx(args[0], ctx) orelse return value.JSValue.false_val;

    if (prefix_data.len > data.len) return value.JSValue.false_val;
    return if (string.eqlStrings(data[0..prefix_data.len], prefix_data)) value.JSValue.true_val else value.JSValue.false_val;
}

/// String.prototype.endsWith(searchString, length?) - Check suffix
pub fn stringEndsWith(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.false_val;

    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.false_val;
    const suffix_data = getStringDataCtx(args[0], ctx) orelse return value.JSValue.false_val;

    if (suffix_data.len > data.len) return value.JSValue.false_val;
    const start = data.len - suffix_data.len;
    return if (string.eqlStrings(data[start..], suffix_data)) value.JSValue.true_val else value.JSValue.false_val;
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

    var start: i32 = 0;
    var end: i32 = @intCast(data.len);

    if (args.len > 0 and args[0].isInt()) {
        start = args[0].getInt();
        if (start < 0) start = @max(0, @as(i32, @intCast(data.len)) + start);
    }
    if (args.len > 1 and args[1].isInt()) {
        end = args[1].getInt();
        if (end < 0) end = @max(0, @as(i32, @intCast(data.len)) + end);
    }

    if (start < 0) start = 0;
    if (end < 0) end = 0;
    if (start > @as(i32, @intCast(data.len))) start = @intCast(data.len);
    if (end > @as(i32, @intCast(data.len))) end = @intCast(data.len);
    if (end < start) end = start;

    const slice_len = @as(u32, @intCast(end - start));

    // Fast path: copy if slice < 16 bytes (SliceString overhead exceeds copy cost)
    if (slice_len < string.SliceString.MIN_SLICE_LEN) {
        const slice = data[@intCast(start)..@intCast(end)];
        const result = ctx.createStringPtr(slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }

    // Zero-copy slice for larger substrings - need the flat parent string
    if (this.isString()) {
        const str = this.toPtr(string.JSString);
        const slice_str = ctx.createSlicePtr(str, @intCast(start), slice_len) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(slice_str);
    }

    // For SliceString: adjust offset relative to parent
    if (this.isStringSlice()) {
        const existing_slice = this.toPtr(string.SliceString);
        const new_offset = existing_slice.offset + @as(u32, @intCast(start));
        const slice_str = ctx.createSlicePtr(existing_slice.parent, new_offset, slice_len) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(slice_str);
    }

    // For other string types, copy
    const slice = data[@intCast(start)..@intCast(end)];
    const result = ctx.createStringPtr(slice) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.substring(start, end?) - Extract substring
/// Uses zero-copy SliceString for substrings >= 16 bytes
pub fn stringSubstring(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Get data from any string type (flatten ropes if needed)
    const data = getStringDataCtx(this, ctx) orelse return value.JSValue.undefined_val;

    var start: i32 = 0;
    var end: i32 = @intCast(data.len);

    if (args.len > 0 and args[0].isInt()) start = args[0].getInt();
    if (args.len > 1 and args[1].isInt()) end = args[1].getInt();

    if (start < 0) start = 0;
    if (end < 0) end = 0;
    if (start > @as(i32, @intCast(data.len))) start = @intCast(data.len);
    if (end > @as(i32, @intCast(data.len))) end = @intCast(data.len);
    if (start > end) {
        const tmp = start;
        start = end;
        end = tmp;
    }

    const slice_len = @as(u32, @intCast(end - start));

    // Fast path: copy if slice < 16 bytes (SliceString overhead exceeds copy cost)
    if (slice_len < string.SliceString.MIN_SLICE_LEN) {
        const slice = data[@intCast(start)..@intCast(end)];
        const result = ctx.createStringPtr(slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }

    // Zero-copy slice for larger substrings - need the flat parent string
    if (this.isString()) {
        const str = this.toPtr(string.JSString);
        const slice_str = ctx.createSlicePtr(str, @intCast(start), slice_len) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(slice_str);
    }

    // For SliceString: adjust offset relative to parent
    if (this.isStringSlice()) {
        const existing_slice = this.toPtr(string.SliceString);
        const new_offset = existing_slice.offset + @as(u32, @intCast(start));
        const slice_str = ctx.createSlicePtr(existing_slice.parent, new_offset, slice_len) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(slice_str);
    }

    // For other string types, copy
    const slice = data[@intCast(start)..@intCast(end)];
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
        // Simple fallback: return whole string
        const part_str = string.createString(ctx.allocator, data) catch return value.JSValue.undefined_val;
        ctx.setIndexChecked(result, 0, value.JSValue.fromPtr(part_str)) catch return value.JSValue.undefined_val;
        result.setArrayLength(1);
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
    if (args.len > 0 and args[0].isInt()) {
        const c = args[0].getInt();
        if (c < 0) return value.JSValue.undefined_val;
        count = @intCast(c);
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
    var strings: std.ArrayListUnmanaged(*const string.JSString) = .{};
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

/// String.fromCharCode(...charCodes) - Create string from char codes
pub fn stringFromCharCode(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.fromPtr(string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val);

    var buf: [64]u8 = undefined;
    const len = @min(args.len, buf.len);
    for (0..len) |i| {
        if (args[i].isInt()) {
            const code = args[i].getInt();
            buf[i] = if (code >= 0 and code <= 255) @intCast(code) else '?';
        } else if (args[i].toNumber()) |n| {
            const code: i32 = @intFromFloat(@mod(@trunc(n), 65536));
            buf[i] = if (code >= 0 and code <= 255) @intCast(code) else '?';
        } else {
            buf[i] = 0;
        }
    }
    const result = string.createString(ctx.allocator, buf[0..len]) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}
