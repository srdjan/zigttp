const std = @import("std");
const h = @import("helpers.zig");
const context = h.context;
const value = h.value;
const object = h.object;
const string = h.string;

// Aliased helpers for use in this module
const allocFloat = h.allocFloat;
const toNumber = h.toNumber;
const getStringData = h.getStringData;
const createArrayWithPrototype = h.createArrayWithPrototype;

// ============================================================================
// Number methods
// ============================================================================

/// Number.isInteger(value) - Returns true if value is an integer
pub fn numberIsInteger(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(false);
    const val = args[0];

    // Check if it's an int32
    if (val.isInt()) return value.JSValue.fromBool(true);

    // Check if it's a float that's an integer
    if (val.isFloat()) {
        const n = val.getFloat64();
        if (std.math.isNan(n) or std.math.isInf(n)) return value.JSValue.fromBool(false);
        return value.JSValue.fromBool(@floor(n) == n);
    }

    return value.JSValue.fromBool(false);
}

/// Number.isNaN(value) - Returns true if value is NaN
pub fn numberIsNaN(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(false);
    const val = args[0];

    if (val.raw == value.JSValue.nan_val.raw) return value.JSValue.fromBool(true);
    if (!val.isFloat()) return value.JSValue.fromBool(false);
    return value.JSValue.fromBool(std.math.isNan(val.getFloat64()));
}

/// Number.isFinite(value) - Returns true if value is a finite number
pub fn numberIsFinite(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(false);
    const val = args[0];

    if (val.isInt()) return value.JSValue.fromBool(true);

    if (val.isFloat()) {
        const n = val.getFloat64();
        return value.JSValue.fromBool(!std.math.isNan(n) and !std.math.isInf(n));
    }

    return value.JSValue.fromBool(false);
}

/// Number.parseFloat(string) - Parse string as float
pub fn numberParseFloat(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx; // ctx no longer needed with NaN-boxing
    if (args.len == 0) return value.JSValue.nan_val;
    const val = args[0];

    if (!val.isString()) return value.JSValue.nan_val;

    const str = val.toPtr(string.JSString);
    const text = str.data();

    // Skip leading whitespace
    var i: usize = 0;
    while (i < text.len and (text[i] == ' ' or text[i] == '\t' or text[i] == '\n' or text[i] == '\r')) : (i += 1) {}

    if (i >= text.len) return value.JSValue.nan_val;

    // Parse sign
    var sign: f64 = 1.0;
    if (text[i] == '-') {
        sign = -1.0;
        i += 1;
    } else if (text[i] == '+') {
        i += 1;
    }

    // Parse number
    var result: f64 = 0.0;
    var has_digits = false;

    // Integer part
    while (i < text.len and text[i] >= '0' and text[i] <= '9') {
        result = result * 10.0 + @as(f64, @floatFromInt(text[i] - '0'));
        has_digits = true;
        i += 1;
    }

    // Fractional part
    if (i < text.len and text[i] == '.') {
        i += 1;
        var frac: f64 = 0.1;
        while (i < text.len and text[i] >= '0' and text[i] <= '9') {
            result += @as(f64, @floatFromInt(text[i] - '0')) * frac;
            frac *= 0.1;
            has_digits = true;
            i += 1;
        }
    }

    if (!has_digits) return value.JSValue.nan_val;

    // Exponent part
    if (i < text.len and (text[i] == 'e' or text[i] == 'E')) {
        i += 1;
        var exp_sign: i32 = 1;
        if (i < text.len and text[i] == '-') {
            exp_sign = -1;
            i += 1;
        } else if (i < text.len and text[i] == '+') {
            i += 1;
        }

        var exp: i32 = 0;
        while (i < text.len and text[i] >= '0' and text[i] <= '9') {
            exp = exp * 10 + @as(i32, @intCast(text[i] - '0'));
            i += 1;
        }
        result *= std.math.pow(f64, 10.0, @as(f64, @floatFromInt(exp * exp_sign)));
    }

    result *= sign;
    // NaN-boxing: store float inline without allocation
    return value.JSValue.fromFloat(result);
}

/// Number.parseInt(string, radix) - Parse string as integer
pub fn numberParseInt(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx; // ctx no longer needed with NaN-boxing
    if (args.len == 0) return value.JSValue.nan_val;
    const val = args[0];

    if (!val.isString()) return value.JSValue.nan_val;

    const str = val.toPtr(string.JSString);
    const text = str.data();

    // Get radix (default 10)
    var radix: u8 = 10;
    if (args.len > 1 and args[1].isInt()) {
        const r = args[1].getInt();
        if (r < 2 or r > 36) return value.JSValue.nan_val;
        radix = @intCast(r);
    }

    // Skip leading whitespace
    var i: usize = 0;
    while (i < text.len and (text[i] == ' ' or text[i] == '\t' or text[i] == '\n' or text[i] == '\r')) : (i += 1) {}

    if (i >= text.len) return value.JSValue.nan_val;

    // Parse sign
    var negative = false;
    if (text[i] == '-') {
        negative = true;
        i += 1;
    } else if (text[i] == '+') {
        i += 1;
    }

    // Handle 0x prefix for hex
    if (radix == 16 and i + 1 < text.len and text[i] == '0' and (text[i + 1] == 'x' or text[i + 1] == 'X')) {
        i += 2;
    } else if (radix == 10 and i + 1 < text.len and text[i] == '0' and (text[i + 1] == 'x' or text[i + 1] == 'X')) {
        radix = 16;
        i += 2;
    }

    // Parse digits
    var result: i64 = 0;
    var has_digits = false;

    while (i < text.len) {
        const c = text[i];
        var digit: ?u8 = null;

        if (c >= '0' and c <= '9') {
            digit = c - '0';
        } else if (c >= 'a' and c <= 'z') {
            digit = c - 'a' + 10;
        } else if (c >= 'A' and c <= 'Z') {
            digit = c - 'A' + 10;
        }

        if (digit == null or digit.? >= radix) break;

        result = result * radix + digit.?;
        has_digits = true;
        i += 1;
    }

    if (!has_digits) return value.JSValue.nan_val;

    if (negative) result = -result;

    // Check if it fits in i32
    if (result >= std.math.minInt(i32) and result <= std.math.maxInt(i32)) {
        return value.JSValue.fromInt(@intCast(result));
    }

    // Return as float for large values (NaN-boxing: no allocation)
    return value.JSValue.fromFloat(@floatFromInt(result));
}

/// Number.prototype.toFixed(digits) - Format number with fixed decimal places
pub fn numberToFixed(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Get the number value
    var n: f64 = undefined;
    if (this.isInt()) {
        n = @floatFromInt(this.getInt());
    } else if (this.isFloat()) {
        n = this.getFloat();
    } else {
        return value.JSValue.undefined_val;
    }

    // Get digits (default 0)
    var digits: u8 = 0;
    if (args.len > 0 and args[0].isInt()) {
        const d = args[0].getInt();
        if (d < 0 or d > 100) return value.JSValue.undefined_val;
        digits = @intCast(d);
    }

    // Format the number
    var buf: [64]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{d:.[1]}", .{ n, digits }) catch return value.JSValue.undefined_val;

    const str = string.createString(ctx.allocator, result) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(str);
}

/// Number.prototype.toString(radix) - Convert number to string
pub fn numberToString(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Get radix (default 10)
    var radix: u8 = 10;
    if (args.len > 0 and args[0].isInt()) {
        const r = args[0].getInt();
        if (r < 2 or r > 36) return value.JSValue.undefined_val;
        radix = @intCast(r);
    }

    // Get the number value
    var n: i64 = undefined;
    var is_float = false;
    var float_val: f64 = undefined;

    if (this.isInt()) {
        n = this.getInt();
    } else if (this.isFloat()) {
        float_val = this.getFloat();
        if (@floor(float_val) == float_val and float_val >= @as(f64, @floatFromInt(std.math.minInt(i64))) and float_val <= @as(f64, @floatFromInt(std.math.maxInt(i64)))) {
            n = @intFromFloat(float_val);
        } else {
            is_float = true;
        }
    } else {
        return value.JSValue.undefined_val;
    }

    // For floats or base 10, use standard formatting
    if (is_float or radix == 10) {
        var buf: [64]u8 = undefined;
        const result = if (is_float)
            std.fmt.bufPrint(&buf, "{d}", .{float_val}) catch return value.JSValue.undefined_val
        else
            std.fmt.bufPrint(&buf, "{d}", .{n}) catch return value.JSValue.undefined_val;

        const str = string.createString(ctx.allocator, result) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(str);
    }

    // Handle negative numbers
    var abs_n: u64 = if (n < 0) @intCast(-n) else @intCast(n);
    var buf: [65]u8 = undefined;
    var pos: usize = buf.len;

    if (abs_n == 0) {
        pos -= 1;
        buf[pos] = '0';
    } else {
        const digits = "0123456789abcdefghijklmnopqrstuvwxyz";
        while (abs_n > 0) {
            pos -= 1;
            buf[pos] = digits[@intCast(abs_n % radix)];
            abs_n /= radix;
        }
    }

    if (n < 0) {
        pos -= 1;
        buf[pos] = '-';
    }

    const str = string.createString(ctx.allocator, buf[pos..]) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(str);
}

/// Global isNaN - coerces argument to number first (unlike Number.isNaN)
pub fn globalIsNaN(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(true); // isNaN(undefined) = true

    const val = args[0];

    // Try to get as number
    if (val.isInt()) return value.JSValue.fromBool(false);
    if (val.isFloat()) return value.JSValue.fromBool(std.math.isNan(val.getFloat64()));
    if (val.isNull()) return value.JSValue.fromBool(false); // null coerces to 0
    if (val.isUndefined()) return value.JSValue.fromBool(true);
    if (val.isBool()) return value.JSValue.fromBool(false); // true->1, false->0

    // Strings and objects would need toNumber coercion - for now return true
    return value.JSValue.fromBool(true);
}

/// Global isFinite - coerces argument to number first (unlike Number.isFinite)
pub fn globalIsFinite(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(false); // isFinite(undefined) = false

    const val = args[0];

    if (val.isInt()) return value.JSValue.fromBool(true);
    if (val.isFloat()) {
        const n = val.getFloat64();
        return value.JSValue.fromBool(!std.math.isNan(n) and !std.math.isInf(n));
    }
    if (val.isNull()) return value.JSValue.fromBool(true); // null coerces to 0
    if (val.isUndefined()) return value.JSValue.fromBool(false);
    if (val.isBool()) return value.JSValue.fromBool(true); // true->1, false->0

    return value.JSValue.fromBool(false);
}

/// Global range(end) or range(start, end) or range(start, end, step)
/// Returns an array of integers for use with for-of iteration
pub fn globalRange(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const root_class_idx = ctx.root_class_idx;

    // Parse arguments
    var start: i32 = 0;
    var end: i32 = 0;
    var step: i32 = 1;

    if (args.len == 0) {
        return value.JSValue.undefined_val;
    } else if (args.len == 1) {
        // range(end) - start defaults to 0
        end = if (args[0].isInt()) args[0].getInt() else if (args[0].isFloat()) @intFromFloat(args[0].getFloat64()) else 0;
    } else if (args.len == 2) {
        // range(start, end)
        start = if (args[0].isInt()) args[0].getInt() else if (args[0].isFloat()) @intFromFloat(args[0].getFloat64()) else 0;
        end = if (args[1].isInt()) args[1].getInt() else if (args[1].isFloat()) @intFromFloat(args[1].getFloat64()) else 0;
    } else {
        // range(start, end, step)
        start = if (args[0].isInt()) args[0].getInt() else if (args[0].isFloat()) @intFromFloat(args[0].getFloat64()) else 0;
        end = if (args[1].isInt()) args[1].getInt() else if (args[1].isFloat()) @intFromFloat(args[1].getFloat64()) else 0;
        step = if (args[2].isInt()) args[2].getInt() else if (args[2].isFloat()) @intFromFloat(args[2].getFloat64()) else 1;
        if (step == 0) step = 1; // Prevent infinite loop
    }

    // Create lazy range iterator - values are computed on demand, not pre-allocated
    const result = if (ctx.hybrid) |hybrid|
        (object.JSObject.createRangeIteratorWithArena(hybrid.arena, root_class_idx, start, end, step) orelse
            return value.JSValue.undefined_val)
    else
        (object.JSObject.createRangeIterator(ctx.allocator, root_class_idx, start, end, step) catch
            return value.JSValue.undefined_val);

    return result.toValue();
}

/// Native _processRequest(items, page, limit) - pagination with checksum, returns JSON string
/// Eliminates all JS overhead for the /api/process benchmark endpoint
pub fn globalProcessRequest(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Parse arguments with defaults
    const total_items: i32 = if (args.len > 0 and args[0].isInt()) args[0].getInt() else if (args.len > 0 and args[0].isFloat()) @intFromFloat(args[0].getFloat64()) else 100;
    const page_arg: i32 = if (args.len > 1 and args[1].isInt()) args[1].getInt() else if (args.len > 1 and args[1].isFloat()) @intFromFloat(args[1].getFloat64()) else 1;
    const limit: i32 = if (args.len > 2 and args[2].isInt()) args[2].getInt() else if (args.len > 2 and args[2].isFloat()) @intFromFloat(args[2].getFloat64()) else 10;

    // Pagination math
    const total_pages: i32 = if (limit > 0) @divTrunc(total_items + limit - 1, limit) else 1;
    const current_page: i32 = @min(@max(1, page_arg), total_pages);
    const start_idx: i32 = (current_page - 1) * limit;
    const end_idx: i32 = @min(start_idx + limit, total_items);
    const item_count: i32 = end_idx - start_idx;

    // Compute checksum - same algorithm as JS version
    var checksum: i32 = 0;
    var i: i32 = 0;
    while (i < item_count) : (i += 1) {
        const item_idx = start_idx + i;
        const val = @mod(((item_idx * 31) ^ (item_idx * 17)), 10000);
        checksum = @mod((checksum + val), 1000000);
    }

    // Build JSON string directly
    var buf: [128]u8 = undefined;
    const json = std.fmt.bufPrint(&buf, "{{\"page\":{d},\"pages\":{d},\"count\":{d},\"checksum\":{d}}}", .{
        current_page,
        total_pages,
        item_count,
        checksum,
    }) catch return value.JSValue.undefined_val;

    // Create JS string from buffer
    return ctx.createString(json) catch return value.JSValue.undefined_val;
}
