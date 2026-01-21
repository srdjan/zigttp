//! Built-in JavaScript objects and methods
//!
//! Standard library implementation for Object, Array, String, etc.

const std = @import("std");
const builtin = @import("builtin");
const value = @import("value.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const string = @import("string.zig");
const http = @import("http.zig");

/// Built-in class IDs
pub const ClassId = enum(u8) {
    object = 0,
    array = 1,
    function = 2,
    string_obj = 3,
    number = 4,
    boolean = 5,
    symbol = 6,
    @"error" = 7,
    array_buffer = 8,
    typed_array = 9,
    data_view = 10,
    promise = 11,
    map = 12,
    set = 13,
    weak_map = 14,
    weak_set = 15,
    regexp = 16,
    date = 17,
    proxy = 18,
    // Add more as needed
};

// ============================================================================
// Helper Functions
// ============================================================================

/// Get root hidden class from context. Panics if context not properly initialized.
/// This eliminates the repeated `ctx.root_class orelse unreachable` pattern.
pub inline fn getRootClass(ctx: *context.Context) *object.HiddenClass {
    return ctx.root_class orelse unreachable;
}

// ============================================================================
// Generic Native Function Wrapper
// ============================================================================

/// Type signature for implementation functions that take a typed Context pointer
const ImplFn = *const fn (*context.Context, value.JSValue, []const value.JSValue) value.JSValue;

/// Create a native function wrapper from an implementation function.
/// Eliminates boilerplate wrapper functions by generating them at compile time.
/// Example: `wrap(jsonParse)` instead of `wrapJsonParse`
pub fn wrap(comptime impl: ImplFn) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
            const result = impl(ctx, this, args);
            if (ctx.hasException()) {
                return error.NativeFunctionError;
            }
            return result;
        }
    }.call;
}

// ============================================================================
// Object methods
// ============================================================================

/// Get object from JSValue, returns null if not an object
fn getObject(val: value.JSValue) ?*object.JSObject {
    if (!val.isPtr()) return null;
    // In a full implementation, we'd check the header type
    return val.toPtr(object.JSObject);
}

/// Object.keys(obj) - Returns array of own enumerable property names
pub fn objectKeys(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const keys = obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    // Create array to hold results
    // For now, return the count as an integer (proper array creation requires more infrastructure)
    return value.JSValue.fromInt(@intCast(keys.len));
}

/// Object.values(obj) - Returns array of own enumerable property values
pub fn objectValues(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const keys = obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    // For now, return count (proper array creation requires more infrastructure)
    return value.JSValue.fromInt(@intCast(keys.len));
}

/// Object.entries(obj) - Returns array of [key, value] pairs
pub fn objectEntries(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const keys = obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    // For now, return count
    return value.JSValue.fromInt(@intCast(keys.len));
}

// Object.assign removed - use spread syntax {...obj1, ...obj2} instead

/// Object.hasOwnProperty(prop) - Check if object has own property
pub fn objectHasOwn(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len < 2) return value.JSValue.false_val;

    const obj = getObject(args[0]) orelse return value.JSValue.false_val;

    // For string property names, we'd need to intern them first
    // For now, return false as a stub
    _ = obj;
    return value.JSValue.false_val;
}

// Object.freeze and Object.isFrozen removed - immutability is a design choice, not enforced

/// Date.now() - Returns milliseconds since Unix epoch
pub fn dateNow(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    _ = args;
    const ts = std.posix.clock_gettime(.REALTIME) catch {
        return value.JSValue.undefined_val;
    };
    const ms: i64 = ts.sec * 1000 + @divTrunc(ts.nsec, 1_000_000);
    // Return as float since timestamps exceed i32 range
    return allocFloat(ctx, @floatFromInt(ms));
}

var perf_time_origin: ?std.time.Instant = null;

/// performance.now() - Returns milliseconds since time origin (monotonic)
pub fn performanceNow(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    _ = args;
    const now = std.time.Instant.now() catch return value.JSValue.undefined_val;
    if (perf_time_origin == null) {
        perf_time_origin = now;
        return allocFloat(ctx, 0);
    }
    const elapsed_ns = now.since(perf_time_origin.?);
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    return allocFloat(ctx, elapsed_ms);
}

// ============================================================================
// JSON methods
// ============================================================================

/// JSON.parse(text) - Parse JSON string to JS value (standard behavior)
/// Returns parsed value or undefined on error
pub fn jsonParse(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const str_val = args[0];
    if (!str_val.isString()) return value.JSValue.undefined_val;

    const js_str = str_val.toPtr(string.JSString);
    const text = js_str.data();

    return parseJsonValue(ctx, text) catch value.JSValue.undefined_val;
}

/// JSON.tryParse(text) - Parse JSON string to JS value, returns Result
/// Returns Result.ok(value) on success, Result.err(message) on failure
pub fn jsonTryParse(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) {
        const err_msg = string.createString(ctx.allocator, "JSON.tryParse requires a string argument") catch return value.JSValue.undefined_val;
        return createResultErr(ctx, value.JSValue.fromPtr(err_msg));
    }

    const str_val = args[0];
    if (!str_val.isString()) {
        const err_msg = string.createString(ctx.allocator, "JSON.tryParse argument must be a string") catch return value.JSValue.undefined_val;
        return createResultErr(ctx, value.JSValue.fromPtr(err_msg));
    }

    const js_str = str_val.toPtr(string.JSString);
    const text = js_str.data();

    const parsed = parseJsonValue(ctx, text) catch {
        const err_msg = string.createString(ctx.allocator, "Invalid JSON") catch return value.JSValue.undefined_val;
        return createResultErr(ctx, value.JSValue.fromPtr(err_msg));
    };
    return createResultOk(ctx, parsed);
}

/// Helper to create Result.ok(value)
fn createResultOk(ctx: *context.Context, val: value.JSValue) value.JSValue {
    const result_obj = ctx.createObject(ctx.result_prototype) catch return value.JSValue.undefined_val;
    result_obj.class_id = .result;
    result_obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK] = value.JSValue.true_val;
    result_obj.inline_slots[object.JSObject.Slots.RESULT_VALUE] = val;
    return result_obj.toValue();
}

/// Helper to create Result.err(error)
fn createResultErr(ctx: *context.Context, err_val: value.JSValue) value.JSValue {
    const result_obj = ctx.createObject(ctx.result_prototype) catch return value.JSValue.undefined_val;
    result_obj.class_id = .result;
    result_obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK] = value.JSValue.false_val;
    result_obj.inline_slots[object.JSObject.Slots.RESULT_VALUE] = err_val;
    return result_obj.toValue();
}

/// JSON.stringify(value) - Convert JS value to JSON string
pub fn jsonStringify(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const val = args[0];

    // Use shared JSON serialization from http module
    const json_js = http.valueToJsonString(ctx, val) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(json_js);
}
// ============================================================================
// Error methods
// ============================================================================

/// Create a new Error object with message
pub fn errorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "Error");
}

/// Create a new TypeError object
pub fn typeErrorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "TypeError");
}

/// Create a new RangeError object
pub fn rangeErrorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "RangeError");
}

/// Create a new SyntaxError object
pub fn syntaxErrorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "SyntaxError");
}

/// Create a new ReferenceError object
pub fn referenceErrorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "ReferenceError");
}

/// Helper to create error objects
fn createErrorObject(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue, error_name: []const u8) value.JSValue {
    // If called with 'new', use the provided 'this' object
    // Otherwise create a new error object
    const obj: *object.JSObject = if (this.isObject() and !this.isUndefined())
        object.JSObject.fromValue(this)
    else blk: {
        break :blk ctx.createObject(ctx.object_prototype) catch return value.JSValue.undefined_val;
    };

    // Set the 'name' property
    const name_val = ctx.createString(error_name) catch return value.JSValue.undefined_val;
    ctx.setPropertyChecked(obj, .name, name_val) catch {};

    // Set the 'message' property
    if (args.len > 0 and args[0].isString()) {
        ctx.setPropertyChecked(obj, .message, args[0]) catch {};
    } else if (args.len > 0) {
        // Convert to string
        const msg_str = valueToStringSimple(ctx.allocator, args[0]) catch {
            const empty_val = ctx.createString("") catch return value.JSValue.undefined_val;
            ctx.setPropertyChecked(obj, .message, empty_val) catch {};
            return obj.toValue();
        };
        ctx.setPropertyChecked(obj, .message, value.JSValue.fromPtr(msg_str)) catch {};
    } else {
        const empty_val = ctx.createString("") catch return value.JSValue.undefined_val;
        ctx.setPropertyChecked(obj, .message, empty_val) catch {};
    }

    obj.class_id = .@"error";
    return obj.toValue();
}

/// Error.prototype.toString
pub fn errorToString(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;

    if (!this.isObject()) {
        return value.JSValue.undefined_val;
    }

    const obj = object.JSObject.fromValue(this);
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    // Get name
    var name_text: []const u8 = "Error";
    if (obj.getProperty(pool, .name)) |name_val| {
        if (name_val.isString()) {
            name_text = name_val.toPtr(string.JSString).data();
        }
    }

    // Get message
    var msg_text: []const u8 = "";
    if (obj.getProperty(pool, .message)) |msg_val| {
        if (msg_val.isString()) {
            msg_text = msg_val.toPtr(string.JSString).data();
        }
    }

    // Format as "Name: message" or just "Name" if no message
    if (msg_text.len == 0) {
        return ctx.createString(name_text) catch return value.JSValue.undefined_val;
    }

    // Concatenate: name + ": " + message
    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);
    buffer.appendSlice(ctx.allocator, name_text) catch return value.JSValue.undefined_val;
    buffer.appendSlice(ctx.allocator, ": ") catch return value.JSValue.undefined_val;
    buffer.appendSlice(ctx.allocator, msg_text) catch return value.JSValue.undefined_val;

    return ctx.createString(buffer.items) catch return value.JSValue.undefined_val;
}

/// Simple value to string conversion
fn valueToStringSimple(allocator: std.mem.Allocator, val: value.JSValue) !*string.JSString {
    if (val.isString()) {
        return val.toPtr(string.JSString);
    }
    if (val.isInt()) {
        var buf: [32]u8 = undefined;
        const slice = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return try string.createString(allocator, "0");
        return try string.createString(allocator, slice);
    }
    if (val.isNull()) return try string.createString(allocator, "null");
    if (val.isUndefined()) return try string.createString(allocator, "undefined");
    if (val.isTrue()) return try string.createString(allocator, "true");
    if (val.isFalse()) return try string.createString(allocator, "false");
    if (val.isObject()) return try string.createString(allocator, "[object Object]");
    return try string.createString(allocator, "");
}

// ============================================================================
// Promise removed - use Result types for async error handling
// ============================================================================

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
    const float_box = ctx.gc_state.allocFloat(result) catch return value.JSValue.nan_val;
    return value.JSValue.fromPtr(float_box);
}

/// Number.parseInt(string, radix) - Parse string as integer
pub fn numberParseInt(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
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

    // Return as float for large values
    const float_box = ctx.gc_state.allocFloat(@floatFromInt(result)) catch return value.JSValue.nan_val;
    return value.JSValue.fromPtr(float_box);
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
    const result = if (ctx.hybrid) |h|
        (object.JSObject.createRangeIteratorWithArena(h.arena, root_class_idx, start, end, step) orelse
            return value.JSValue.undefined_val)
    else
        (object.JSObject.createRangeIterator(ctx.allocator, root_class_idx, start, end, step) catch
            return value.JSValue.undefined_val);

    return result.toValue();
}

// ============================================================================
// Map implementation
// ============================================================================

/// Map constructor - new Map() or new Map(iterable)
pub fn mapConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Create the Map object
    const map_obj = ctx.createObject(null) catch return value.JSValue.undefined_val;

    // Internal storage: _keys and _values arrays
    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.undefined_val;
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    // Create internal arrays (proper arrays for efficient indexed access)
    const keys_arr = ctx.createArray() catch return value.JSValue.undefined_val;
    const values_arr = ctx.createArray() catch return value.JSValue.undefined_val;

    ctx.setPropertyChecked(map_obj, keys_atom, keys_arr.toValue()) catch return value.JSValue.undefined_val;
    ctx.setPropertyChecked(map_obj, values_atom, values_arr.toValue()) catch return value.JSValue.undefined_val;
    ctx.setPropertyChecked(map_obj, size_atom, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    // If iterable provided, add entries
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Handle iterable initialization
    }

    return map_obj.toValue();
}

/// Map.prototype.set(key, value)
pub fn mapSet(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 2) return this;

    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];
    const val = args[1];
    const pool = ctx.hidden_class_pool orelse return this;

    const keys_atom = ctx.atoms.intern("_keys") catch return this;
    const values_atom = ctx.atoms.intern("_values") catch return this;
    const size_atom = ctx.atoms.intern("size") catch return this;

    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return this;
    const values_val = map_obj.getProperty(pool, values_atom) orelse return this;

    if (!keys_val.isObject() or !values_val.isObject()) return this;

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    const len = keys_arr.getArrayLength();

    // Check if key already exists
    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (keys_arr.getIndex(i)) |existing_key| {
            if (existing_key.strictEquals(key)) {
                // Update existing value
                ctx.setIndexChecked(values_arr, i, val) catch return this;
                return this;
            }
        }
    }

    // Add new entry
    ctx.setIndexChecked(keys_arr, len, key) catch return this;
    ctx.setIndexChecked(values_arr, len, val) catch return this;
    ctx.setPropertyChecked(map_obj, size_atom, value.JSValue.fromInt(@as(i32, @intCast(len)) + 1)) catch return this;

    return this;
}

/// Map.prototype.get(key)
pub fn mapGet(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.undefined_val;

    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.undefined_val;
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;

    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return value.JSValue.undefined_val;
    const values_val = map_obj.getProperty(pool, values_atom) orelse return value.JSValue.undefined_val;

    if (!keys_val.isObject() or !values_val.isObject()) return value.JSValue.undefined_val;

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    const len = keys_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (keys_arr.getIndex(i)) |existing_key| {
            if (existing_key.strictEquals(key)) {
                return values_arr.getIndex(i) orelse value.JSValue.undefined_val;
            }
        }
    }

    return value.JSValue.undefined_val;
}

/// Map.prototype.has(key)
pub fn mapHas(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];
    const pool = ctx.hidden_class_pool orelse return value.JSValue.fromBool(false);

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.fromBool(false);
    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return value.JSValue.fromBool(false);

    if (!keys_val.isObject()) return value.JSValue.fromBool(false);

    const keys_arr = object.JSObject.fromValue(keys_val);
    const len = keys_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (keys_arr.getIndex(i)) |existing_key| {
            if (existing_key.strictEquals(key)) {
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Map.prototype.delete(key)
pub fn mapDelete(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];
    const pool = ctx.hidden_class_pool orelse return value.JSValue.fromBool(false);

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.fromBool(false);
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.fromBool(false);
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.fromBool(false);

    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return value.JSValue.fromBool(false);
    const values_val = map_obj.getProperty(pool, values_atom) orelse return value.JSValue.fromBool(false);

    if (!keys_val.isObject() or !values_val.isObject()) return value.JSValue.fromBool(false);

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    const len = keys_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (keys_arr.getIndex(i)) |existing_key| {
            if (existing_key.strictEquals(key)) {
                // Shift remaining elements
                var j = i;
                while (j < len - 1) : (j += 1) {
                    if (keys_arr.getIndex(j + 1)) |next_key| {
                        ctx.setIndexChecked(keys_arr, j, next_key) catch {};
                    }
                    if (values_arr.getIndex(j + 1)) |next_val| {
                        ctx.setIndexChecked(values_arr, j, next_val) catch {};
                    }
                }
                keys_arr.setArrayLength(len - 1);
                values_arr.setArrayLength(len - 1);
                ctx.setPropertyChecked(map_obj, size_atom, value.JSValue.fromInt(@as(i32, @intCast(len)) - 1)) catch {};
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Map.prototype.clear()
pub fn mapClear(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;

    const map_obj = object.JSObject.fromValue(this);
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.undefined_val;
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return value.JSValue.undefined_val;
    const values_val = map_obj.getProperty(pool, values_atom) orelse return value.JSValue.undefined_val;

    if (!keys_val.isObject() or !values_val.isObject()) return value.JSValue.undefined_val;

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    keys_arr.setArrayLength(0);
    values_arr.setArrayLength(0);
    ctx.setPropertyChecked(map_obj, size_atom, value.JSValue.fromInt(0)) catch {};

    return value.JSValue.undefined_val;
}

// ============================================================================
// Set implementation
// ============================================================================

/// Set constructor - new Set() or new Set(iterable)
pub fn setConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Create the Set object
    const set_obj = ctx.createObject(null) catch return value.JSValue.undefined_val;

    // Internal storage: _values array
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    // Create internal array (proper array for efficient indexed access)
    const values_arr = ctx.createArray() catch return value.JSValue.undefined_val;

    ctx.setPropertyChecked(set_obj, values_atom, values_arr.toValue()) catch return value.JSValue.undefined_val;
    ctx.setPropertyChecked(set_obj, size_atom, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    // If iterable provided, add values
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Handle iterable initialization
    }

    return set_obj.toValue();
}

/// Set.prototype.add(value)
pub fn setAdd(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return this;

    const set_obj = object.JSObject.fromValue(this);
    const val = args[0];
    const pool = ctx.hidden_class_pool orelse return this;

    const values_atom = ctx.atoms.intern("_values") catch return this;
    const size_atom = ctx.atoms.intern("size") catch return this;

    const values_val = set_obj.getProperty(pool, values_atom) orelse return this;
    if (!values_val.isObject()) return this;

    const values_arr = object.JSObject.fromValue(values_val);
    const len = values_arr.getArrayLength();

    // Check if value already exists
    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (values_arr.getIndex(i)) |existing| {
            if (existing.strictEquals(val)) {
                return this; // Already exists
            }
        }
    }

    // Add new value
    ctx.setIndexChecked(values_arr, len, val) catch return this;
    ctx.setPropertyChecked(set_obj, size_atom, value.JSValue.fromInt(@as(i32, @intCast(len)) + 1)) catch return this;

    return this;
}

/// Set.prototype.has(value)
pub fn setHas(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const set_obj = object.JSObject.fromValue(this);
    const val = args[0];
    const pool = ctx.hidden_class_pool orelse return value.JSValue.fromBool(false);

    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.fromBool(false);
    const values_val = set_obj.getProperty(pool, values_atom) orelse return value.JSValue.fromBool(false);

    if (!values_val.isObject()) return value.JSValue.fromBool(false);

    const values_arr = object.JSObject.fromValue(values_val);
    const len = values_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (values_arr.getIndex(i)) |existing| {
            if (existing.strictEquals(val)) {
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Set.prototype.delete(value)
pub fn setDelete(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const set_obj = object.JSObject.fromValue(this);
    const val = args[0];
    const pool = ctx.hidden_class_pool orelse return value.JSValue.fromBool(false);

    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.fromBool(false);
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.fromBool(false);

    const values_val = set_obj.getProperty(pool, values_atom) orelse return value.JSValue.fromBool(false);
    if (!values_val.isObject()) return value.JSValue.fromBool(false);

    const values_arr = object.JSObject.fromValue(values_val);
    const len = values_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (values_arr.getIndex(i)) |existing| {
            if (existing.strictEquals(val)) {
                // Shift remaining elements
                var j = i;
                while (j < len - 1) : (j += 1) {
                    if (values_arr.getIndex(j + 1)) |next_val| {
                        ctx.setIndexChecked(values_arr, j, next_val) catch {};
                    }
                }
                values_arr.setArrayLength(len - 1);
                ctx.setPropertyChecked(set_obj, size_atom, value.JSValue.fromInt(@as(i32, @intCast(len)) - 1)) catch {};
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Set.prototype.clear()
pub fn setClear(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;

    const set_obj = object.JSObject.fromValue(this);
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    const values_val = set_obj.getProperty(pool, values_atom) orelse return value.JSValue.undefined_val;
    if (!values_val.isObject()) return value.JSValue.undefined_val;

    const values_arr = object.JSObject.fromValue(values_val);
    values_arr.setArrayLength(0);
    ctx.setPropertyChecked(set_obj, size_atom, value.JSValue.fromInt(0)) catch {};

    return value.JSValue.undefined_val;
}

const JsonError = error{ InvalidJson, UnexpectedEof, OutOfMemory, NoRootClass, ArenaObjectEscape, NoHiddenClassPool };

// ============================================================================
// JSON Shape Cache - Avoids hidden class transitions in JSON.parse
// ============================================================================

/// Maximum properties to cache per object shape
const JSON_SHAPE_MAX_PROPS = 16;

/// Number of cache entries (power of 2 for fast modulo)
const JSON_SHAPE_CACHE_SIZE = 64;

/// Entry in the JSON shape cache
const JSONShapeCacheEntry = struct {
    /// Hash of the property atom sequence
    hash: u64 = 0,
    /// Number of properties in this shape
    prop_count: u8 = 0,
    /// Property atoms in order
    atoms: [JSON_SHAPE_MAX_PROPS]object.Atom = undefined,
    /// Cached hidden class index for this shape
    class_idx: object.HiddenClassIndex = .none,
    /// Whether this entry is valid
    valid: bool = false,
};

/// Thread-local JSON shape cache
/// Maps property-atom-sequence to pre-built HiddenClassIndex
/// IMPORTANT: Must be cleared when creating a new Context to avoid stale class indices
var json_shape_cache: [JSON_SHAPE_CACHE_SIZE]JSONShapeCacheEntry = [_]JSONShapeCacheEntry{.{}} ** JSON_SHAPE_CACHE_SIZE;

/// Clear the JSON shape cache. Must be called when creating a new Context
/// to avoid stale HiddenClassIndex references from previous contexts.
pub fn clearJsonShapeCache() void {
    json_shape_cache = [_]JSONShapeCacheEntry{.{}} ** JSON_SHAPE_CACHE_SIZE;
}

/// Compute hash for a sequence of atoms
fn hashAtomSequence(atoms: []const object.Atom) u64 {
    var h: u64 = 0xcbf29ce484222325; // FNV-1a offset basis
    for (atoms) |atom| {
        h ^= @intFromEnum(atom);
        h *%= 0x100000001b3; // FNV-1a prime
    }
    return h;
}

/// Look up a cached shape by atom sequence
fn lookupJsonShape(atoms: []const object.Atom) ?object.HiddenClassIndex {
    if (atoms.len == 0 or atoms.len > JSON_SHAPE_MAX_PROPS) return null;

    const hash = hashAtomSequence(atoms);
    const idx = hash % JSON_SHAPE_CACHE_SIZE;
    const entry = &json_shape_cache[idx];

    if (!entry.valid or entry.hash != hash or entry.prop_count != atoms.len) {
        return null;
    }

    // Verify atoms match exactly (hash collision check)
    for (atoms, 0..) |atom, i| {
        if (entry.atoms[i] != atom) return null;
    }

    return entry.class_idx;
}

/// Cache a shape for future lookups
fn cacheJsonShape(atoms: []const object.Atom, class_idx: object.HiddenClassIndex) void {
    if (atoms.len == 0 or atoms.len > JSON_SHAPE_MAX_PROPS) return;

    const hash = hashAtomSequence(atoms);
    const idx = hash % JSON_SHAPE_CACHE_SIZE;
    const entry = &json_shape_cache[idx];

    entry.hash = hash;
    entry.prop_count = @intCast(atoms.len);
    for (atoms, 0..) |atom, i| {
        entry.atoms[i] = atom;
    }
    entry.class_idx = class_idx;
    entry.valid = true;
}

/// Build a hidden class with all properties in one go
fn buildClassForAtoms(pool: *object.HiddenClassPool, atoms: []const object.Atom) !object.HiddenClassIndex {
    var class_idx = pool.getEmptyClass();
    for (atoms) |atom| {
        class_idx = try pool.addProperty(class_idx, atom);
    }
    return class_idx;
}

/// Skip JSON whitespace characters (space, newline, carriage return, tab)
inline fn skipJsonWhitespace(text: []const u8, pos: *usize) void {
    while (pos.* < text.len) {
        switch (text[pos.*]) {
            ' ', '\n', '\r', '\t' => pos.* += 1,
            else => break,
        }
    }
}

/// Parse a JSON value from text
fn parseJsonValue(ctx: *context.Context, text: []const u8) JsonError!value.JSValue {
    var pos: usize = 0;
    return parseJsonValueAt(ctx, text, &pos);
}

/// Parse JSON value at position
fn parseJsonValueAt(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!value.JSValue {
    skipJsonWhitespace(text, pos);

    if (pos.* >= text.len) return error.UnexpectedEof;

    const c = text[pos.*];

    // Object
    if (c == '{') {
        return parseJsonObject(ctx, text, pos);
    }

    // Array
    if (c == '[') {
        return parseJsonArray(ctx, text, pos);
    }

    // String
    if (c == '"') {
        return parseJsonString(ctx, text, pos);
    }

    // Number
    if (c == '-' or (c >= '0' and c <= '9')) {
        return parseJsonNumber(text, pos);
    }

    // true
    if (text.len >= pos.* + 4 and std.mem.eql(u8, text[pos.*..][0..4], "true")) {
        pos.* += 4;
        return value.JSValue.true_val;
    }

    // false
    if (text.len >= pos.* + 5 and std.mem.eql(u8, text[pos.*..][0..5], "false")) {
        pos.* += 5;
        return value.JSValue.false_val;
    }

    // null
    if (text.len >= pos.* + 4 and std.mem.eql(u8, text[pos.*..][0..4], "null")) {
        pos.* += 4;
        return value.JSValue.null_val;
    }

    return error.InvalidJson;
}

/// Parse JSON object with shape caching
/// Buffers properties first, then creates object with cached shape to avoid hidden class transitions
fn parseJsonObject(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!value.JSValue {
    pos.* += 1; // skip '{'

    skipJsonWhitespace(text, pos);

    // Empty object fast path
    if (pos.* < text.len and text[pos.*] == '}') {
        pos.* += 1;
        const obj = try ctx.createObject(null);
        return obj.toValue();
    }

    // Buffer for collecting properties before object creation
    var atoms: [JSON_SHAPE_MAX_PROPS]object.Atom = undefined;
    var values: [JSON_SHAPE_MAX_PROPS]value.JSValue = undefined;
    var prop_count: usize = 0;

    // Parse all properties into buffer
    while (pos.* < text.len) {
        skipJsonWhitespace(text, pos);

        // Parse key directly to atom (avoids JSString allocation)
        if (pos.* >= text.len or text[pos.*] != '"') return error.InvalidJson;
        const atom = try parseJsonKey(ctx, text, pos);

        skipJsonWhitespace(text, pos);

        // Expect ':'
        if (pos.* >= text.len or text[pos.*] != ':') return error.InvalidJson;
        pos.* += 1;

        // Parse value
        const val = try parseJsonValueAt(ctx, text, pos);

        if (prop_count < JSON_SHAPE_MAX_PROPS) {
            atoms[prop_count] = atom;
            values[prop_count] = val;
            prop_count += 1;
        } else {
            // Overflow: fall back to slow path for remaining properties
            // First create object with buffered properties
            const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
            const class_idx = lookupJsonShape(atoms[0..prop_count]) orelse
                try buildClassForAtoms(pool, atoms[0..prop_count]);

            const obj = try ctx.createObjectWithClass(class_idx, ctx.object_prototype);
            for (0..prop_count) |i| {
                obj.setSlot(@intCast(i), values[i]);
            }

            // Add overflow property via slow path
            try ctx.setPropertyChecked(obj, atom, val);

            // Continue with slow path for any remaining properties
            skipJsonWhitespace(text, pos);
            while (pos.* < text.len and text[pos.*] != '}') {
                if (text[pos.*] == ',') {
                    pos.* += 1;
                    skipJsonWhitespace(text, pos);
                    if (pos.* >= text.len or text[pos.*] != '"') return error.InvalidJson;
                    const a = try parseJsonKey(ctx, text, pos);
                    skipJsonWhitespace(text, pos);
                    if (pos.* >= text.len or text[pos.*] != ':') return error.InvalidJson;
                    pos.* += 1;
                    const v = try parseJsonValueAt(ctx, text, pos);
                    try ctx.setPropertyChecked(obj, a, v);
                    skipJsonWhitespace(text, pos);
                } else {
                    return error.InvalidJson;
                }
            }
            if (pos.* < text.len and text[pos.*] == '}') {
                pos.* += 1;
                return obj.toValue();
            }
            return error.InvalidJson;
        }

        skipJsonWhitespace(text, pos);

        // Check for ',' or '}'
        if (pos.* >= text.len) return error.InvalidJson;
        if (text[pos.*] == '}') {
            pos.* += 1;
            break;
        }
        if (text[pos.*] == ',') {
            pos.* += 1;
            continue;
        }
        return error.InvalidJson;
    }

    // Create object with cached or new shape
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const atom_slice = atoms[0..prop_count];

    // Try to find cached shape
    var class_idx = lookupJsonShape(atom_slice);
    if (class_idx == null) {
        // Build new shape and cache it
        class_idx = try buildClassForAtoms(pool, atom_slice);
        cacheJsonShape(atom_slice, class_idx.?);
    }

    // Create object with the shape - no hidden class transitions!
    const obj = try ctx.createObjectWithClass(class_idx.?, ctx.object_prototype);

    // Set all property values directly by slot
    for (0..prop_count) |i| {
        obj.setSlot(@intCast(i), values[i]);
    }

    return obj.toValue();
}

/// Parse JSON array
fn parseJsonArray(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!value.JSValue {
    pos.* += 1; // skip '['

    const arr = try ctx.createArray();
    arr.prototype = ctx.array_prototype;

    skipJsonWhitespace(text, pos);

    if (pos.* < text.len and text[pos.*] == ']') {
        pos.* += 1;
        arr.setArrayLength(0);
        return arr.toValue();
    }

    var index: u32 = 0;
    while (pos.* < text.len) {
        // Parse element
        const elem = try parseJsonValueAt(ctx, text, pos);
        try ctx.setIndexChecked(arr, index, elem);
        index += 1;

        skipJsonWhitespace(text, pos);

        // Check for ',' or ']'
        if (pos.* >= text.len) return error.InvalidJson;
        if (text[pos.*] == ']') {
            pos.* += 1;
            arr.setArrayLength(index);
            return arr.toValue();
        }
        if (text[pos.*] == ',') {
            pos.* += 1;
            continue;
        }
        return error.InvalidJson;
    }

    return error.InvalidJson;
}

/// Parse JSON object key directly to atom without creating JSString
/// Optimization: avoids string allocation for property keys
fn parseJsonKey(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!object.Atom {
    pos.* += 1; // skip opening '"'
    const start = pos.*;

    // Fast path: scan for closing quote without escapes
    while (pos.* < text.len) {
        const c = text[pos.*];
        if (c == '"') {
            // No escapes - intern directly from JSON text slice
            const key_slice = text[start..pos.*];
            pos.* += 1;
            return ctx.atoms.intern(key_slice) catch return error.OutOfMemory;
        }
        if (c == '\\') {
            // Has escapes - use slow path with temporary buffer
            return parseJsonKeyWithEscapes(ctx, text, pos, start);
        }
        pos.* += 1;
    }

    return error.InvalidJson;
}

/// Slow path for JSON keys with escape sequences
fn parseJsonKeyWithEscapes(ctx: *context.Context, text: []const u8, pos: *usize, start: usize) JsonError!object.Atom {
    // Build unescaped key string
    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);

    buffer.appendSlice(ctx.allocator, text[start..pos.*]) catch return error.OutOfMemory;

    while (pos.* < text.len) {
        const c = text[pos.*];
        if (c == '"') {
            pos.* += 1;
            return ctx.atoms.intern(buffer.items) catch return error.OutOfMemory;
        }
        if (c == '\\') {
            pos.* += 1;
            if (pos.* >= text.len) return error.InvalidJson;
            const escaped = text[pos.*];
            pos.* += 1;
            const byte: u8 = switch (escaped) {
                '"' => '"',
                '\\' => '\\',
                '/' => '/',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'b' => 0x08,
                'f' => 0x0C,
                else => return error.InvalidJson,
            };
            buffer.append(ctx.allocator, byte) catch return error.OutOfMemory;
        } else {
            buffer.append(ctx.allocator, c) catch return error.OutOfMemory;
            pos.* += 1;
        }
    }

    return error.InvalidJson;
}

/// Parse JSON string - fast path for strings without escapes
fn parseJsonString(ctx: *context.Context, text: []const u8, pos: *usize) JsonError!value.JSValue {
    pos.* += 1; // skip opening '"'
    const start = pos.*;

    // Fast path: scan for closing quote without escapes
    while (pos.* < text.len) {
        const c = text[pos.*];
        if (c == '"') {
            // No escapes found - create string directly from slice
            const str_slice = text[start..pos.*];
            pos.* += 1;
            return try ctx.createString(str_slice);
        }
        if (c == '\\') {
            // Has escapes - use slow path
            return parseJsonStringWithEscapes(ctx, text, pos, start);
        }
        pos.* += 1;
    }

    return error.InvalidJson;
}

/// Slow path for JSON strings with escape sequences
fn parseJsonStringWithEscapes(ctx: *context.Context, text: []const u8, pos: *usize, start: usize) JsonError!value.JSValue {
    // Copy the part before the first escape
    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);

    try buffer.appendSlice(ctx.allocator, text[start..pos.*]);

    while (pos.* < text.len) {
        const c = text[pos.*];
        if (c == '"') {
            pos.* += 1;
            return try ctx.createString(buffer.items);
        }
        if (c == '\\') {
            pos.* += 1;
            if (pos.* >= text.len) return error.InvalidJson;
            const escaped = text[pos.*];
            pos.* += 1;
            switch (escaped) {
                '"' => try buffer.append(ctx.allocator, '"'),
                '\\' => try buffer.append(ctx.allocator, '\\'),
                '/' => try buffer.append(ctx.allocator, '/'),
                'n' => try buffer.append(ctx.allocator, '\n'),
                'r' => try buffer.append(ctx.allocator, '\r'),
                't' => try buffer.append(ctx.allocator, '\t'),
                'b' => try buffer.append(ctx.allocator, 0x08),
                'f' => try buffer.append(ctx.allocator, 0x0C),
                'u' => {
                    // Unicode escape \uXXXX
                    if (pos.* + 4 > text.len) return error.InvalidJson;
                    const hex = text[pos.*..][0..4];
                    pos.* += 4;
                    const code = std.fmt.parseInt(u16, hex, 16) catch return error.InvalidJson;
                    // UTF-8 encoding
                    if (code < 0x80) {
                        try buffer.append(ctx.allocator, @intCast(code));
                    } else if (code < 0x800) {
                        try buffer.append(ctx.allocator, @intCast(0xC0 | (code >> 6)));
                        try buffer.append(ctx.allocator, @intCast(0x80 | (code & 0x3F)));
                    } else {
                        try buffer.append(ctx.allocator, @intCast(0xE0 | (code >> 12)));
                        try buffer.append(ctx.allocator, @intCast(0x80 | ((code >> 6) & 0x3F)));
                        try buffer.append(ctx.allocator, @intCast(0x80 | (code & 0x3F)));
                    }
                },
                else => try buffer.append(ctx.allocator, escaped),
            }
        } else {
            try buffer.append(ctx.allocator, c);
            pos.* += 1;
        }
    }

    return error.InvalidJson;
}

/// Parse JSON number
fn parseJsonNumber(text: []const u8, pos: *usize) JsonError!value.JSValue {
    const start = pos.*;

    // Optional minus
    if (pos.* < text.len and text[pos.*] == '-') {
        pos.* += 1;
    }

    // Integer part
    if (pos.* < text.len and text[pos.*] == '0') {
        pos.* += 1;
    } else {
        while (pos.* < text.len and text[pos.*] >= '0' and text[pos.*] <= '9') {
            pos.* += 1;
        }
    }

    // Fractional part
    var is_float = false;
    if (pos.* < text.len and text[pos.*] == '.') {
        is_float = true;
        pos.* += 1;
        while (pos.* < text.len and text[pos.*] >= '0' and text[pos.*] <= '9') {
            pos.* += 1;
        }
    }

    // Exponent part
    if (pos.* < text.len and (text[pos.*] == 'e' or text[pos.*] == 'E')) {
        is_float = true;
        pos.* += 1;
        if (pos.* < text.len and (text[pos.*] == '+' or text[pos.*] == '-')) {
            pos.* += 1;
        }
        while (pos.* < text.len and text[pos.*] >= '0' and text[pos.*] <= '9') {
            pos.* += 1;
        }
    }

    const num_str = text[start..pos.*];

    if (is_float) {
        const f = std.fmt.parseFloat(f64, num_str) catch return error.InvalidJson;
        // For now, truncate to int if it fits
        if (f == @trunc(f) and f >= -2147483648 and f <= 2147483647) {
            return value.JSValue.fromInt(@intFromFloat(f));
        }
        // Would need heap allocation for float - return as int truncated
        return value.JSValue.fromInt(@intFromFloat(@trunc(f)));
    } else {
        const i = std.fmt.parseInt(i32, num_str, 10) catch {
            // Try as float if integer overflow
            const f = std.fmt.parseFloat(f64, num_str) catch return error.InvalidJson;
            return value.JSValue.fromInt(@intFromFloat(@trunc(f)));
        };
        return value.JSValue.fromInt(i);
    }
}

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

/// Set array length on object
fn setArrayLength(obj: *object.JSObject, allocator: std.mem.Allocator, len: i32) void {
    if (obj.class_id == .array) {
        obj.setArrayLength(@intCast(@max(len, 0)));
        return;
    }
    obj.setProperty(allocator, .length, value.JSValue.fromInt(len)) catch {};
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

// Mutating array methods (push, pop, shift, unshift, splice) removed for functional paradigm.
// Use spread operator [...arr, item] or slice() for immutable operations.

/// Array.prototype.indexOf(searchElement, fromIndex?) - Find first index of element
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

    var i: u32 = @intCast(@max(from_idx, 0));
    while (i < len) : (i += 1) {
        const val = obj.getIndex(i) orelse value.JSValue.undefined_val;
        if (val.strictEquals(search_elem)) {
            return value.JSValue.fromInt(@intCast(i));
        }
    }
    return value.JSValue.fromInt(-1);
}

/// Array.prototype.includes(searchElement, fromIndex?) - Check if array contains element
pub fn arrayIncludes(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const result = arrayIndexOf(ctx, this, args);
    if (result.isInt() and result.getInt() >= 0) {
        return value.JSValue.true_val;
    }
    return value.JSValue.false_val;
}

/// Array.prototype.join(separator?) - Join elements into string
pub fn arrayJoin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    if (obj.class_id != .array) return value.JSValue.undefined_val;

    const len = obj.getArrayLength();
    var separator: []const u8 = ",";
    if (args.len > 0 and !args[0].isUndefined()) {
        const sep_str = valueToStringSimple(ctx.allocator, args[0]) catch return value.JSValue.undefined_val;
        separator = sep_str.data();
    }

    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (i > 0) {
            buffer.appendSlice(ctx.allocator, separator) catch return value.JSValue.undefined_val;
        }
        const val = obj.getIndex(i) orelse value.JSValue.undefined_val;
        if (val.isUndefined() or val.isNull()) {
            continue;
        }
        const elem_str = valueToStringSimple(ctx.allocator, val) catch return value.JSValue.undefined_val;
        buffer.appendSlice(ctx.allocator, elem_str.data()) catch return value.JSValue.undefined_val;
    }

    const result = string.createString(ctx.allocator, buffer.items) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
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
    _ = args;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj, ctx.hidden_class_pool);

    // Requires function call infrastructure - return length for now
    return value.JSValue.fromInt(len);
}

/// Array.prototype.filter(callback, thisArg?) - Filter to new array
pub fn arrayFilter(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // Requires function call infrastructure
    return value.JSValue.fromInt(0);
}

/// Array.prototype.reduce(callback, initialValue?) - Reduce to single value
pub fn arrayReduce(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    // Requires function call infrastructure
    if (args.len > 1) return args[1]; // Return initial value
    return value.JSValue.undefined_val;
}

/// Array.prototype.forEach(callback, thisArg?) - Execute callback for each element
pub fn arrayForEach(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // Requires function call infrastructure
    return value.JSValue.undefined_val;
}

/// Array.prototype.every(callback, thisArg?) - Test if all elements pass
pub fn arrayEvery(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    return value.JSValue.true_val;
}

/// Array.prototype.some(callback, thisArg?) - Test if any element passes
pub fn arraySome(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    return value.JSValue.false_val;
}

/// Array.prototype.find(callback, thisArg?) - Find first matching element
pub fn arrayFind(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    return value.JSValue.undefined_val;
}

/// Array.prototype.findIndex(callback, thisArg?) - Find index of first matching element
pub fn arrayFindIndex(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
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

// ============================================================================
// String methods
// ============================================================================

/// Get JSString from value (if it's a string pointer)
fn getJSString(val: value.JSValue) ?*string.JSString {
    if (!val.isPtr()) return null;
    // In full implementation, would check header type
    return val.toPtr(string.JSString);
}

/// String.prototype.length - Get string length
pub fn stringLength(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    if (getJSString(this)) |str| {
        return value.JSValue.fromInt(@intCast(str.len));
    }
    return value.JSValue.fromInt(0);
}

/// String.prototype.charAt(index) - Get character at index
pub fn stringCharAt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (getJSString(this)) |str| {
        var idx: u32 = 0;
        if (args.len > 0 and args[0].isInt()) {
            const i = args[0].getInt();
            if (i < 0) return value.JSValue.undefined_val;
            idx = @intCast(i);
        }
        if (str.charAt(idx)) |_| {
            // Return single char (in full impl would return string)
            return value.JSValue.fromInt(@intCast(idx));
        }
    }
    return value.JSValue.undefined_val;
}

/// String.prototype.charCodeAt(index) - Get char code at index
pub fn stringCharCodeAt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (getJSString(this)) |str| {
        var idx: u32 = 0;
        if (args.len > 0 and args[0].isInt()) {
            const i = args[0].getInt();
            if (i < 0) return allocFloat(ctx, std.math.nan(f64));
            idx = @intCast(i);
        }
        if (str.charAt(idx)) |c| {
            return value.JSValue.fromInt(@intCast(c));
        }
    }
    return allocFloat(ctx, std.math.nan(f64));
}

/// String.prototype.indexOf(searchString, position?) - Find substring
pub fn stringIndexOf(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (args.len == 0) return value.JSValue.fromInt(-1);

    if (getJSString(this)) |str| {
        if (getJSString(args[0])) |needle| {
            const data = str.data();
            const needle_data = needle.data();

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
        }
    }
    return value.JSValue.fromInt(-1);
}

/// String.prototype.lastIndexOf(searchString, position?) - Find substring from end
pub fn stringLastIndexOf(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (args.len == 0) return value.JSValue.fromInt(-1);

    if (getJSString(this)) |str| {
        if (getJSString(args[0])) |needle| {
            if (str.lastIndexOf(needle.data())) |idx| {
                return value.JSValue.fromInt(@intCast(idx));
            }
        }
    }
    return value.JSValue.fromInt(-1);
}

/// String.prototype.startsWith(searchString, position?) - Check prefix
pub fn stringStartsWith(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (args.len == 0) return value.JSValue.false_val;

    if (getJSString(this)) |str| {
        if (getJSString(args[0])) |prefix| {
            return if (str.startsWith(prefix.data())) value.JSValue.true_val else value.JSValue.false_val;
        }
    }
    return value.JSValue.false_val;
}

/// String.prototype.endsWith(searchString, length?) - Check suffix
pub fn stringEndsWith(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (args.len == 0) return value.JSValue.false_val;

    if (getJSString(this)) |str| {
        if (getJSString(args[0])) |suffix| {
            return if (str.endsWith(suffix.data())) value.JSValue.true_val else value.JSValue.false_val;
        }
    }
    return value.JSValue.false_val;
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
pub fn stringSlice(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (getJSString(this)) |str| {
        const data = str.data();
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

        const slice = data[@intCast(start)..@intCast(end)];
        const result = string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }
    return value.JSValue.undefined_val;
}

/// String.prototype.substring(start, end?) - Extract substring
pub fn stringSubstring(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (getJSString(this)) |str| {
        const data = str.data();
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

        const slice = data[@intCast(start)..@intCast(end)];
        const result = string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }
    return value.JSValue.undefined_val;
}

/// String.prototype.toLowerCase() - Convert to lowercase
pub fn stringToLowerCase(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    // In full impl, would create new lowercase string
    return this;
}

/// String.prototype.toUpperCase() - Convert to uppercase
pub fn stringToUpperCase(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    // In full impl, would create new uppercase string
    return this;
}

/// String.prototype.trim() - Remove whitespace from both ends
pub fn stringTrim(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    if (getJSString(this)) |str| {
        const data = str.data();
        var start: usize = 0;
        var end: usize = data.len;
        while (start < end and std.ascii.isWhitespace(data[start])) : (start += 1) {}
        while (end > start and std.ascii.isWhitespace(data[end - 1])) : (end -= 1) {}
        const slice = data[start..end];
        const result = string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }
    return this;
}

/// String.prototype.trimStart() - Remove whitespace from start
pub fn stringTrimStart(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    if (getJSString(this)) |str| {
        const data = str.data();
        var start: usize = 0;
        while (start < data.len and std.ascii.isWhitespace(data[start])) : (start += 1) {}
        const slice = data[start..];
        const result = string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }
    return this;
}

/// String.prototype.trimEnd() - Remove whitespace from end
pub fn stringTrimEnd(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    if (getJSString(this)) |str| {
        const data = str.data();
        var end: usize = data.len;
        while (end > 0 and std.ascii.isWhitespace(data[end - 1])) : (end -= 1) {}
        const slice = data[0..end];
        const result = string.createString(ctx.allocator, slice) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }
    return this;
}

/// String.prototype.split(separator, limit?) - Split into array
pub fn stringSplit(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isString()) return value.JSValue.undefined_val;
    const str = this.toPtr(string.JSString);
    const data = str.data();

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

    const sep_val = args[0];
    const sep_str = getJSString(sep_val) orelse {
        return value.JSValue.undefined_val;
    };
    const sep = sep_str.data();
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
    _ = ctx;
    if (getJSString(this)) |str| {
        var count: i32 = 0;
        if (args.len > 0 and args[0].isInt()) {
            count = args[0].getInt();
            if (count < 0) return value.JSValue.undefined_val;
        }
        // Return new length (proper string creation in full impl)
        return value.JSValue.fromInt(@as(i32, @intCast(str.len)) * count);
    }
    return value.JSValue.undefined_val;
}

/// String.prototype.padStart(targetLength, padString?) - Pad at start
pub fn stringPadStart(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (getJSString(this)) |str| {
        var target_len: i32 = @intCast(str.len);
        if (args.len > 0 and args[0].isInt()) {
            target_len = @max(target_len, args[0].getInt());
        }
        return value.JSValue.fromInt(target_len);
    }
    return this;
}

/// String.prototype.padEnd(targetLength, padString?) - Pad at end
pub fn stringPadEnd(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (getJSString(this)) |str| {
        var target_len: i32 = @intCast(str.len);
        if (args.len > 0 and args[0].isInt()) {
            target_len = @max(target_len, args[0].getInt());
        }
        return value.JSValue.fromInt(target_len);
    }
    return this;
}

/// String.prototype.concat(...strings) - Concatenate strings
/// Uses concatMany for efficient single-allocation concatenation
pub fn stringConcat(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;

    // Fast path: no args, return this
    if (args.len == 0) return this;

    // Get this as string
    const this_str = getJSString(this) orelse return this;

    // Collect all strings to concatenate
    var strings: std.ArrayListUnmanaged(*const string.JSString) = .{};
    defer strings.deinit(allocator);

    strings.append(allocator, this_str) catch return this;

    for (args) |arg| {
        if (getJSString(arg)) |str| {
            strings.append(allocator, str) catch return this;
        }
    }

    // Use concatMany for efficient single-allocation concatenation
    const result = string.concatMany(allocator, strings.items) catch return this;
    return value.JSValue.fromPtr(result);
}

/// String.prototype.replace(searchValue, replaceValue) - Replace first occurrence
pub fn stringReplace(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;

    if (!this.isString() or args.len < 2) return this;
    const str = this.toPtr(string.JSString);
    const input = str.data();

    const replacement = if (args[1].isString()) args[1].toPtr(string.JSString).data() else "";

    // String search only - RegExp not supported
    if (args[0].isString()) {
        const search = args[0].toPtr(string.JSString).data();
        if (std.mem.indexOf(u8, input, search)) |idx| {
            var result = std.ArrayList(u8).empty;
            defer result.deinit(allocator);
            result.appendSlice(allocator, input[0..idx]) catch return this;
            result.appendSlice(allocator, replacement) catch return this;
            result.appendSlice(allocator, input[idx + search.len ..]) catch return this;
            const new_str = string.createString(allocator, result.items) catch return this;
            return value.JSValue.fromPtr(new_str);
        }
    }

    return this;
}

/// String.prototype.replaceAll(searchValue, replaceValue) - Replace all occurrences
pub fn stringReplaceAll(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;

    if (!this.isString() or args.len < 2) return this;
    const str = this.toPtr(string.JSString);
    const input = str.data();

    const replacement = if (args[1].isString()) args[1].toPtr(string.JSString).data() else "";

    // String search - replace all
    if (args[0].isString()) {
        const search = args[0].toPtr(string.JSString).data();
        if (search.len == 0) return this;

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
    _ = ctx;
    _ = this;
    // Return length for now
    return value.JSValue.fromInt(@intCast(args.len));
}

// ============================================================================
// Math methods
// ============================================================================

/// Helper to get numeric value as f64
fn toNumber(val: value.JSValue) ?f64 {
    return val.toNumber();
}

/// Helper to allocate float result
fn allocFloat(ctx: *context.Context, v: f64) value.JSValue {
    // Check if result is a safe integer
    if (@floor(v) == v and v >= -2147483648 and v <= 2147483647) {
        return value.JSValue.fromInt(@intFromFloat(v));
    }
    const box = ctx.gc_state.allocFloat(v) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(box);
}

pub fn mathAbs(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @abs(n));
}

pub fn mathFloor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @floor(n));
}

pub fn mathCeil(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @ceil(n));
}

pub fn mathRound(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @round(n));
}

pub fn mathTrunc(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @trunc(n));
}

pub fn mathMin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return allocFloat(ctx, std.math.inf(f64));

    var min_val: f64 = std.math.inf(f64);
    for (args) |arg| {
        const n = toNumber(arg) orelse return value.JSValue.undefined_val;
        if (std.math.isNan(n)) return allocFloat(ctx, std.math.nan(f64));
        min_val = @min(min_val, n);
    }
    return allocFloat(ctx, min_val);
}

pub fn mathMax(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return allocFloat(ctx, -std.math.inf(f64));

    var max_val: f64 = -std.math.inf(f64);
    for (args) |arg| {
        const n = toNumber(arg) orelse return value.JSValue.undefined_val;
        if (std.math.isNan(n)) return allocFloat(ctx, std.math.nan(f64));
        max_val = @max(max_val, n);
    }
    return allocFloat(ctx, max_val);
}

pub fn mathPow(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len < 2) return value.JSValue.undefined_val;
    const base = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    const exp = toNumber(args[1]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, std.math.pow(f64, base, exp));
}

pub fn mathSqrt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    if (n < 0) return allocFloat(ctx, std.math.nan(f64));
    return allocFloat(ctx, @sqrt(n));
}

pub fn mathSin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @sin(n));
}

pub fn mathCos(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @cos(n));
}

pub fn mathTan(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @tan(n));
}

pub fn mathLog(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @log(n));
}

pub fn mathExp(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @exp(n));
}

var prng: ?std.Random.DefaultPrng = null;

pub fn mathRandom(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    _ = args;
    if (prng == null) {
        const now = std.time.Instant.now() catch {
            prng = std.Random.DefaultPrng.init(0x9e3779b97f4a7c15);
            const r = prng.?.random().float(f64);
            return allocFloat(ctx, r);
        };
        const seed: u64 = switch (builtin.os.tag) {
            .windows, .uefi, .wasi => @as(u64, now.timestamp),
            else => blk: {
                const sec: u64 = @intCast(now.timestamp.sec);
                const nsec: u64 = @intCast(now.timestamp.nsec);
                break :blk (sec * std.time.ns_per_s) + nsec;
            },
        };
        prng = std.Random.DefaultPrng.init(seed);
    }
    const r = prng.?.random().float(f64);
    return allocFloat(ctx, r);
}

pub fn mathSign(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    if (std.math.isNan(n)) return allocFloat(ctx, std.math.nan(f64));
    if (n > 0) return value.JSValue.fromInt(1);
    if (n < 0) return value.JSValue.fromInt(-1);
    return value.JSValue.fromInt(0);
}

/// Math constants
pub const math_constants = struct {
    pub const PI: f64 = 3.141592653589793;
    pub const E: f64 = 2.718281828459045;
    pub const LN2: f64 = 0.6931471805599453;
    pub const LN10: f64 = 2.302585092994046;
    pub const LOG2E: f64 = 1.4426950408889634;
    pub const LOG10E: f64 = 0.4342944819032518;
    pub const SQRT2: f64 = 1.4142135623730951;
    pub const SQRT1_2: f64 = 0.7071067811865476;
};

// ============================================================================
// Console methods
// ============================================================================

pub fn consoleLog(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    for (args, 0..) |arg, i| {
        if (i > 0) writeToFd(std.c.STDOUT_FILENO, " ");
        printValue(std.c.STDOUT_FILENO, arg);
    }
    writeToFd(std.c.STDOUT_FILENO, "\n");
    return value.JSValue.undefined_val;
}

pub fn consoleWarn(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    writeToFd(std.c.STDERR_FILENO, "[WARN] ");
    for (args, 0..) |arg, i| {
        if (i > 0) writeToFd(std.c.STDERR_FILENO, " ");
        printValue(std.c.STDERR_FILENO, arg);
    }
    writeToFd(std.c.STDERR_FILENO, "\n");
    return value.JSValue.undefined_val;
}

pub fn consoleError(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    writeToFd(std.c.STDERR_FILENO, "[ERROR] ");
    for (args, 0..) |arg, i| {
        if (i > 0) writeToFd(std.c.STDERR_FILENO, " ");
        printValue(std.c.STDERR_FILENO, arg);
    }
    writeToFd(std.c.STDERR_FILENO, "\n");
    return value.JSValue.undefined_val;
}

fn writeToFd(fd: std.c.fd_t, data: []const u8) void {
    _ = std.c.write(fd, data.ptr, data.len);
}

fn printValue(fd: std.c.fd_t, val: value.JSValue) void {
    if (val.isInt()) {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return;
        writeToFd(fd, s);
    } else if (val.isNull()) {
        writeToFd(fd, "null");
    } else if (val.isUndefined()) {
        writeToFd(fd, "undefined");
    } else if (val.isTrue()) {
        writeToFd(fd, "true");
    } else if (val.isFalse()) {
        writeToFd(fd, "false");
    } else {
        writeToFd(fd, "[object]");
    }
}

// ============================================================================
// Initialization
// ============================================================================

/// Wrapper to convert context.Context native function signature to object.NativeFn

// ============================================================================
// Array method wrappers
// ============================================================================

// ============================================================================
// String method wrappers
// ============================================================================

/// Helper to add a method using a dynamic atom name
fn addMethodDynamic(
    ctx: *context.Context,
    obj: *object.JSObject,
    name: []const u8,
    func: object.NativeFn,
    arg_count: u8,
) !void {
    return addMethodDynamicWithId(ctx, obj, name, func, arg_count, .none);
}

/// Helper to add a method with builtin ID for fast dispatch
fn addMethodDynamicWithId(
    ctx: *context.Context,
    obj: *object.JSObject,
    name: []const u8,
    func: object.NativeFn,
    arg_count: u8,
    builtin_id: object.BuiltinId,
) !void {
    const allocator = ctx.allocator;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const root_class_idx = ctx.root_class_idx;
    if (object.lookupPredefinedAtom(name)) |atom| {
        const func_obj = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, func, atom, arg_count, builtin_id);
        try ctx.setPropertyChecked(obj, atom, func_obj.toValue());
        return;
    }
    const atom = try ctx.atoms.intern(name);
    const func_obj = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, func, atom, arg_count, builtin_id);
    try ctx.setPropertyChecked(obj, atom, func_obj.toValue());
}

/// Create a native function and add it as a property on an object
fn addMethod(
    ctx: *context.Context,
    allocator: std.mem.Allocator,
    pool: *object.HiddenClassPool,
    obj: *object.JSObject,
    root_class_idx: object.HiddenClassIndex,
    name: object.Atom,
    func: object.NativeFn,
    arg_count: u8,
) !void {
    const func_obj = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, func, name, arg_count);
    try ctx.setPropertyChecked(obj, name, func_obj.toValue());
}

/// Create a native function with builtin ID for fast dispatch
fn addMethodWithId(
    ctx: *context.Context,
    allocator: std.mem.Allocator,
    pool: *object.HiddenClassPool,
    obj: *object.JSObject,
    root_class_idx: object.HiddenClassIndex,
    name: object.Atom,
    func: object.NativeFn,
    arg_count: u8,
    builtin_id: object.BuiltinId,
) !void {
    const func_obj = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, func, name, arg_count, builtin_id);
    try ctx.setPropertyChecked(obj, name, func_obj.toValue());
}

/// Initialize all built-in objects on global
pub fn initBuiltins(ctx: *context.Context) !void {
    const allocator = ctx.allocator;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const root_class_idx = ctx.root_class_idx;

    // Create console object
    const console_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethod(ctx, allocator, pool, console_obj, root_class_idx, .log, wrap(consoleLog), 0);
    // Note: .warn and .error atoms don't exist in predefined atoms, use .log for now
    // In full implementation would add dynamic atoms
    try ctx.builtin_objects.append(allocator,console_obj);

    // Register console on global
    try ctx.setGlobal(.console, console_obj.toValue());

    // Create Math object - hot methods use fast dispatch IDs
    const math_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .abs, wrap(mathAbs), 1, .math_abs);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .floor, wrap(mathFloor), 1, .math_floor);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .ceil, wrap(mathCeil), 1, .math_ceil);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .round, wrap(mathRound), 1, .math_round);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .min, wrap(mathMin), 2, .math_min);
    try addMethodWithId(ctx, allocator, pool, math_obj, root_class_idx, .max, wrap(mathMax), 2, .math_max);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .pow, wrap(mathPow), 2);
    try addMethodDynamic(ctx, math_obj, "trunc", wrap(mathTrunc), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .sqrt, wrap(mathSqrt), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .sin, wrap(mathSin), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .cos, wrap(mathCos), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .tan, wrap(mathTan), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .log, wrap(mathLog), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .exp, wrap(mathExp), 1);
    try addMethod(ctx, allocator, pool, math_obj, root_class_idx, .random, wrap(mathRandom), 0);

    // Add Math constants as properties
    const pi_box = try ctx.gc_state.allocFloat(math_constants.PI);
    try ctx.setPropertyChecked(math_obj, @enumFromInt(ctx.atoms.next_id), value.JSValue.fromPtr(pi_box));
    // Note: Would need to add "PI", "E" etc as dynamic atoms for full implementation
    try ctx.builtin_objects.append(allocator,math_obj);

    // Register Math on global
    try ctx.setGlobal(.Math, math_obj.toValue());

    // Create JSON object - parse and stringify are hot builtins with fast dispatch
    const json_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodWithId(ctx, allocator, pool, json_obj, root_class_idx, .parse, wrap(jsonParse), 1, .json_parse);
    try addMethod(ctx, allocator, pool, json_obj, root_class_idx, .tryParse, wrap(jsonTryParse), 1);
    try addMethodWithId(ctx, allocator, pool, json_obj, root_class_idx, .stringify, wrap(jsonStringify), 1, .json_stringify);
    try ctx.builtin_objects.append(allocator,json_obj);

    // Register JSON on global
    try ctx.setGlobal(.JSON, json_obj.toValue());

    // Create Object constructor with static methods
    const object_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, object_obj, "keys", wrap(objectKeys), 1);
    try addMethodDynamic(ctx, object_obj, "values", wrap(objectValues), 1);
    try addMethodDynamic(ctx, object_obj, "entries", wrap(objectEntries), 1);
    // Object.assign removed - use spread syntax {...obj1, ...obj2}
    try addMethodDynamic(ctx, object_obj, "hasOwn", wrap(objectHasOwn), 2);
    // Object.freeze and Object.isFrozen removed - immutability is a design choice
    try ctx.builtin_objects.append(allocator,object_obj);

    // Register Object on global
    try ctx.setGlobal(.Object, object_obj.toValue());

    // Create Error prototype with toString method
    const error_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, error_proto, "toString", wrap(errorToString), 0);
    try ctx.builtin_objects.append(allocator,error_proto);

    // Create Error constructor
    // Note: constructors are functions on global - destroyed by global_obj.destroyBuiltin
    const error_ctor_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(errorConstructor), .Error, 1);
    try ctx.setPropertyChecked(error_ctor_func, .prototype, error_proto.toValue());
    try ctx.setGlobal(.Error, error_ctor_func.toValue());

    // Create TypeError constructor
    const type_error_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(typeErrorConstructor), .TypeError, 1);
    try ctx.setPropertyChecked(type_error_ctor, .prototype, error_proto.toValue());
    try ctx.setGlobal(.TypeError, type_error_ctor.toValue());

    // Create RangeError constructor
    const range_error_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(rangeErrorConstructor), .RangeError, 1);
    try ctx.setPropertyChecked(range_error_ctor, .prototype, error_proto.toValue());
    try ctx.setGlobal(.RangeError, range_error_ctor.toValue());

    // Create SyntaxError constructor
    const syntax_error_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(syntaxErrorConstructor), .SyntaxError, 1);
    try ctx.setPropertyChecked(syntax_error_ctor, .prototype, error_proto.toValue());
    try ctx.setGlobal(.SyntaxError, syntax_error_ctor.toValue());

    // Create ReferenceError constructor
    const ref_error_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(referenceErrorConstructor), .ReferenceError, 1);
    try ctx.setPropertyChecked(ref_error_ctor, .prototype, error_proto.toValue());
    try ctx.setGlobal(.ReferenceError, ref_error_ctor.toValue());

    // Promise removed - use Result types for async error handling

    // Create Number object with static methods
    const number_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, number_obj, "isInteger", wrap(numberIsInteger), 1);
    try addMethodDynamic(ctx, number_obj, "isNaN", wrap(numberIsNaN), 1);
    try addMethodDynamic(ctx, number_obj, "isFinite", wrap(numberIsFinite), 1);
    try addMethodDynamic(ctx, number_obj, "parseFloat", wrap(numberParseFloat), 1);
    try addMethodDynamic(ctx, number_obj, "parseInt", wrap(numberParseInt), 2);

    // Add Number constants
    const max_value_atom = try ctx.atoms.intern("MAX_VALUE");
    const max_value_box = try ctx.gc_state.allocFloat(1.7976931348623157e+308);
    try ctx.setPropertyChecked(number_obj, max_value_atom, value.JSValue.fromPtr(max_value_box));

    const min_value_atom = try ctx.atoms.intern("MIN_VALUE");
    const min_value_box = try ctx.gc_state.allocFloat(5e-324);
    try ctx.setPropertyChecked(number_obj, min_value_atom, value.JSValue.fromPtr(min_value_box));

    const nan_atom = try ctx.atoms.intern("NaN");
    try ctx.setPropertyChecked(number_obj, nan_atom, value.JSValue.nan_val);

    const pos_inf_atom = try ctx.atoms.intern("POSITIVE_INFINITY");
    const pos_inf_box = try ctx.gc_state.allocFloat(std.math.inf(f64));
    try ctx.setPropertyChecked(number_obj, pos_inf_atom, value.JSValue.fromPtr(pos_inf_box));

    const neg_inf_atom = try ctx.atoms.intern("NEGATIVE_INFINITY");
    const neg_inf_box = try ctx.gc_state.allocFloat(-std.math.inf(f64));
    try ctx.setPropertyChecked(number_obj, neg_inf_atom, value.JSValue.fromPtr(neg_inf_box));
    try ctx.builtin_objects.append(allocator,number_obj);

    // Register Number on global (predefined atom)
    try ctx.setGlobal(.Number, number_obj.toValue());

    // Also register parseFloat and parseInt globally (JS convention) - hot builtins with fast dispatch
    // Note: these are functions directly on global, not container objects.
    // They will be destroyed by global_obj.destroyBuiltin, so don't add to builtin_objects.
    const global_parse_float_atom = try ctx.atoms.intern("parseFloat");
    const parse_float_func = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, wrap(numberParseFloat), global_parse_float_atom, 1, .parse_float);
    try ctx.setGlobal(global_parse_float_atom, parse_float_func.toValue());

    const global_parse_int_atom = try ctx.atoms.intern("parseInt");
    const parse_int_func = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, wrap(numberParseInt), global_parse_int_atom, 2, .parse_int);
    try ctx.setGlobal(global_parse_int_atom, parse_int_func.toValue());

    // Also register isNaN and isFinite globally (JS convention)
    const global_is_nan_atom = try ctx.atoms.intern("isNaN");
    const global_is_nan_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(globalIsNaN), global_is_nan_atom, 1);
    try ctx.setGlobal(global_is_nan_atom, global_is_nan_func.toValue());

    const global_is_finite_atom = try ctx.atoms.intern("isFinite");
    const global_is_finite_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(globalIsFinite), global_is_finite_atom, 1);
    try ctx.setGlobal(global_is_finite_atom, global_is_finite_func.toValue());

    // Register range() globally for iteration
    const range_atom = try ctx.atoms.intern("range");
    const range_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(globalRange), range_atom, 1);
    try ctx.setGlobal(range_atom, range_func.toValue());

    // Create Map prototype with methods
    const map_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, map_proto, "set", wrap(mapSet), 2);
    try addMethodDynamic(ctx, map_proto, "get", wrap(mapGet), 1);
    try addMethodDynamic(ctx, map_proto, "has", wrap(mapHas), 1);
    try addMethodDynamic(ctx, map_proto, "delete", wrap(mapDelete), 1);
    try addMethodDynamic(ctx, map_proto, "clear", wrap(mapClear), 0);
    try ctx.builtin_objects.append(allocator,map_proto);

    // Create Map constructor
    // Note: constructors are functions on global - destroyed by global_obj.destroyBuiltin
    const map_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(mapConstructor), .Map, 0);
    try ctx.setPropertyChecked(map_ctor, .prototype, map_proto.toValue());
    try ctx.setGlobal(.Map, map_ctor.toValue());

    // Create Set prototype with methods
    const set_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, set_proto, "add", wrap(setAdd), 1);
    try addMethodDynamic(ctx, set_proto, "has", wrap(setHas), 1);
    try addMethodDynamic(ctx, set_proto, "delete", wrap(setDelete), 1);
    try addMethodDynamic(ctx, set_proto, "clear", wrap(setClear), 0);
    try ctx.builtin_objects.append(allocator,set_proto);

    // Create Set constructor
    // Note: constructors are functions on global - destroyed by global_obj.destroyBuiltin
    const set_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(setConstructor), .Set, 0);
    try ctx.setPropertyChecked(set_ctor, .prototype, set_proto.toValue());
    try ctx.setGlobal(.Set, set_ctor.toValue());

    // Create Response constructor with static methods
    const response_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseConstructor, .Response, 2);

    // Register Response static methods on the constructor
    const json_atom: object.Atom = .json;
    const text_atom: object.Atom = .text;
    const html_atom: object.Atom = .html;
    const redirect_atom = try ctx.atoms.intern("redirect");

    const json_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseJson, json_atom, 1);
    try ctx.setPropertyChecked(response_ctor, json_atom, json_func.toValue());

    const text_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseText, text_atom, 1);
    try ctx.setPropertyChecked(response_ctor, text_atom, text_func.toValue());

    const html_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseHtml, html_atom, 1);
    try ctx.setPropertyChecked(response_ctor, html_atom, html_func.toValue());

    const redirect_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.responseRedirect, redirect_atom, 1);
    try ctx.setPropertyChecked(response_ctor, redirect_atom, redirect_func.toValue());

    // Register Response on global (predefined atom)
    // Note: constructors are functions on global - destroyed by global_obj.destroyBuiltin
    try ctx.setGlobal(.Response, response_ctor.toValue());

    // Register h() - hyperscript function for JSX
    // Standalone function on global - destroyed by global_obj.destroyBuiltin
    const h_atom: object.Atom = .h;
    const h_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.h, h_atom, 2);
    try ctx.setGlobal(h_atom, h_func.toValue());

    // Register renderToString() for SSR
    // Standalone function on global - destroyed by global_obj.destroyBuiltin
    const render_atom: object.Atom = .renderToString;
    const render_func = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, http.renderToString, render_atom, 1);
    try ctx.setGlobal(render_atom, render_func.toValue());

    // Register Fragment constant for JSX
    const fragment_atom: object.Atom = .Fragment;
    const fragment_str = try string.createString(allocator, http.FRAGMENT_MARKER);
    try ctx.setGlobal(fragment_atom, value.JSValue.fromPtr(fragment_str));

    // Register Date object with Date.now()
    const date_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, date_obj, "now", wrap(dateNow), 0);
    try ctx.builtin_objects.append(allocator,date_obj);
    try ctx.setGlobal(.Date, date_obj.toValue());

    // Register performance object with performance.now()
    const performance_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, performance_obj, "now", wrap(performanceNow), 0);
    try ctx.builtin_objects.append(allocator,performance_obj);
    const performance_atom = try ctx.atoms.intern("performance");
    try ctx.setGlobal(performance_atom, performance_obj.toValue());

    // ========================================================================
    // Array.prototype (functional - no mutating methods)
    // ========================================================================
    const array_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    // Removed mutating methods: push, pop, shift, unshift, splice, reverse, fill, sort
    // Use spread operator [...arr, item] or slice() for immutable operations
    try addMethodDynamic(ctx, array_proto, "indexOf", wrap(arrayIndexOf), 1);
    try addMethodDynamic(ctx, array_proto, "includes", wrap(arrayIncludes), 1);
    try addMethodDynamic(ctx, array_proto, "join", wrap(arrayJoin), 1);
    try addMethodDynamic(ctx, array_proto, "slice", wrap(arraySlice), 2);
    try addMethodDynamic(ctx, array_proto, "concat", wrap(arrayConcat), 1);
    try addMethodDynamic(ctx, array_proto, "map", wrap(arrayMap), 1);
    try addMethodDynamic(ctx, array_proto, "filter", wrap(arrayFilter), 1);
    try addMethodDynamic(ctx, array_proto, "reduce", wrap(arrayReduce), 2);
    try addMethodDynamic(ctx, array_proto, "forEach", wrap(arrayForEach), 1);
    try addMethodDynamic(ctx, array_proto, "every", wrap(arrayEvery), 1);
    try addMethodDynamic(ctx, array_proto, "some", wrap(arraySome), 1);
    try addMethodDynamic(ctx, array_proto, "find", wrap(arrayFind), 1);
    try addMethodDynamic(ctx, array_proto, "findIndex", wrap(arrayFindIndex), 1);
    try addMethodDynamic(ctx, array_proto, "toSorted", wrap(arrayToSorted), 1);
    try addMethodDynamic(ctx, array_proto, "toReversed", wrap(arrayToReversed), 0);
    ctx.array_prototype = array_proto;

    // Create Array constructor function on global
    const array_ctor = try object.JSObject.create(allocator, root_class_idx, null, pool);
    // Array static methods
    try addMethodDynamic(ctx, array_ctor, "isArray", wrap(arrayIsArray), 1);
    try addMethodDynamic(ctx, array_ctor, "from", wrap(arrayFrom), 1);
    try addMethodDynamic(ctx, array_ctor, "of", wrap(arrayOf), 0);
    try ctx.builtin_objects.append(allocator,array_ctor);
    try ctx.setGlobal(.Array, array_ctor.toValue());

    // ========================================================================
    // String.prototype
    // ========================================================================
    const string_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, string_proto, "charAt", wrap(stringCharAt), 1);
    try addMethodDynamic(ctx, string_proto, "charCodeAt", wrap(stringCharCodeAt), 1);
    try addMethodDynamicWithId(ctx, string_proto, "indexOf", wrap(stringIndexOf), 1, .string_index_of);
    try addMethodDynamic(ctx, string_proto, "lastIndexOf", wrap(stringLastIndexOf), 1);
    try addMethodDynamic(ctx, string_proto, "startsWith", wrap(stringStartsWith), 1);
    try addMethodDynamic(ctx, string_proto, "endsWith", wrap(stringEndsWith), 1);
    try addMethodDynamic(ctx, string_proto, "includes", wrap(stringIncludes), 1);
    try addMethodDynamicWithId(ctx, string_proto, "slice", wrap(stringSlice), 2, .string_slice);
    try addMethodDynamic(ctx, string_proto, "substring", wrap(stringSubstring), 2);
    try addMethodDynamic(ctx, string_proto, "toLowerCase", wrap(stringToLowerCase), 0);
    try addMethodDynamic(ctx, string_proto, "toUpperCase", wrap(stringToUpperCase), 0);
    try addMethodDynamic(ctx, string_proto, "trim", wrap(stringTrim), 0);
    try addMethodDynamic(ctx, string_proto, "trimStart", wrap(stringTrimStart), 0);
    try addMethodDynamic(ctx, string_proto, "trimEnd", wrap(stringTrimEnd), 0);
    try addMethodDynamic(ctx, string_proto, "split", wrap(stringSplit), 2);
    try addMethodDynamic(ctx, string_proto, "repeat", wrap(stringRepeat), 1);
    try addMethodDynamic(ctx, string_proto, "padStart", wrap(stringPadStart), 2);
    try addMethodDynamic(ctx, string_proto, "padEnd", wrap(stringPadEnd), 2);
    try addMethodDynamic(ctx, string_proto, "concat", wrap(stringConcat), 1);
    try addMethodDynamic(ctx, string_proto, "replace", wrap(stringReplace), 2);
    try addMethodDynamic(ctx, string_proto, "replaceAll", wrap(stringReplaceAll), 2);
    ctx.string_prototype = string_proto;

    // Create String constructor function on global
    const string_ctor = try object.JSObject.create(allocator, root_class_idx, null, pool);
    // String.fromCharCode static method
    try addMethodDynamic(ctx, string_ctor, "fromCharCode", wrap(stringFromCharCode), 1);
    try ctx.builtin_objects.append(allocator,string_ctor);
    try ctx.setGlobal(.String, string_ctor.toValue());

    // RegExp removed - use string methods for pattern matching

    // Create Result prototype with instance methods
    const result_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .isOk, wrap(resultIsOk), 0);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .isErr, wrap(resultIsErr), 0);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .unwrap, wrap(resultUnwrap), 0);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .unwrapOr, wrap(resultUnwrapOr), 1);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .unwrapErr, wrap(resultUnwrapErr), 0);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .map, wrap(resultMap), 1);
    try addMethod(ctx, allocator, pool, result_proto, root_class_idx, .mapErr, wrap(resultMapErr), 1);
    try addMethodDynamic(ctx, result_proto, "match", wrap(resultMatch), 1);

    // Create Result object with static methods ok/err
    const result_obj = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethod(ctx, allocator, pool, result_obj, root_class_idx, .ok, wrap(resultOk), 1);
    try addMethod(ctx, allocator, pool, result_obj, root_class_idx, .err, wrap(resultErr), 1);
    try ctx.setPropertyChecked(result_obj, .prototype, result_proto.toValue());
    try ctx.builtin_objects.append(allocator,result_obj);
    try ctx.setGlobal(.Result, result_obj.toValue());

    // Store Result prototype on context for creating Result instances
    ctx.result_prototype = result_proto;
}

// ============================================================================
// RegExp removed - use string methods for pattern matching
// ============================================================================

// ============================================================================
// Result type implementation - functional error handling
// ============================================================================

/// Result slot layout:
/// slot[0] = isOk (boolean)
/// slot[1] = value (the ok or err value)
/// Create a Result.ok(value)
pub fn resultOk(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const val = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    return createResultOk(ctx, val);
}

/// Create a Result.err(error)
pub fn resultErr(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const err_val = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    return createResultErr(ctx, err_val);
}

/// Result.prototype.isOk() - returns true if result is ok
pub fn resultIsOk(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.false_val;
    return obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK];
}

/// Result.prototype.isErr() - returns true if result is err
pub fn resultIsErr(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.true_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.true_val;
    // Return opposite of isOk
    return if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) value.JSValue.false_val else value.JSValue.true_val;
}

/// Result.prototype.unwrap() - returns value if ok, undefined if err
pub fn resultUnwrap(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.undefined_val;

    if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) {
        return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    }
    return value.JSValue.undefined_val;
}

/// Result.prototype.unwrapOr(default) - returns value if ok, default if err
pub fn resultUnwrapOr(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) {
        return if (args.len > 0) args[0] else value.JSValue.undefined_val;
    }
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) {
        return if (args.len > 0) args[0] else value.JSValue.undefined_val;
    }

    if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) {
        return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    }
    return if (args.len > 0) args[0] else value.JSValue.undefined_val;
}

/// Result.prototype.unwrapErr() - returns error if err, undefined if ok
pub fn resultUnwrapErr(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.undefined_val;

    if (!obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) {
        return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    }
    return value.JSValue.undefined_val;
}

/// Result.prototype.map(fn) - transform ok value, pass through err
/// Note: Requires callback infrastructure - returns self for now
pub fn resultMap(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    // TODO: Implement when callback infrastructure is available
    return this;
}

/// Result.prototype.mapErr(fn) - transform err value, pass through ok
/// Note: Requires callback infrastructure - returns self for now
pub fn resultMapErr(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    // TODO: Implement when callback infrastructure is available
    return this;
}

/// Result.prototype.match({ok: fn, err: fn}) - pattern match on result
/// Note: Requires callback infrastructure - returns value for now
pub fn resultMatch(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.undefined_val;
    // TODO: Implement when callback infrastructure is available
    // For now just return the inner value
    return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
}

// ============================================================================
// Symbol implementation
// ============================================================================

/// Global symbol counter for unique IDs
var symbol_counter: u32 = 100; // Start after well-known symbols

/// Global symbol registry for Symbol.for()
var symbol_registry: ?std.StringHashMap(value.JSValue) = null;

/// Create a new unique symbol
pub fn createSymbol(allocator: std.mem.Allocator, description: ?[]const u8) !value.JSValue {
    const symbol_box = try allocator.create(value.JSValue.SymbolBox);
    symbol_counter += 1;
    symbol_box.* = .{
        .header = (@as(u32, 8) << 1), // MemTag.symbol = 8
        .id = symbol_counter,
        .description_ptr = if (description) |d| d.ptr else null,
        .description_len = if (description) |d| @intCast(d.len) else 0,
    };
    return value.JSValue.fromExternPtr(symbol_box);
}

/// Create a well-known symbol
pub fn createWellKnownSymbol(allocator: std.mem.Allocator, which: value.JSValue.WellKnownSymbol, description: []const u8) !value.JSValue {
    const symbol_box = try allocator.create(value.JSValue.SymbolBox);
    symbol_box.* = .{
        .header = (@as(u32, 8) << 1), // MemTag.symbol = 8
        .id = @intFromEnum(which),
        .description_ptr = description.ptr,
        .description_len = @intCast(description.len),
    };
    return value.JSValue.fromExternPtr(symbol_box);
}

/// Symbol() - Create a new unique symbol
pub fn symbolConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Symbol() must be called as a function, not with new
    const allocator = if (ctx.hybrid) |h| h.arena.allocator() else ctx.allocator;

    // Get optional description
    var description: ?[]const u8 = null;
    if (args.len > 0 and args[0].isString()) {
        description = ctx.getString(args[0]);
    }

    const symbol = createSymbol(allocator, description) catch return value.JSValue.undefined_val;
    return symbol;
}

/// Symbol.for(key) - Get or create a symbol in the global registry
pub fn symbolFor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.undefined_val;

    const key = ctx.getString(args[0]) orelse return value.JSValue.undefined_val;

    // Initialize registry if needed
    if (symbol_registry == null) {
        symbol_registry = std.StringHashMap(value.JSValue).init(ctx.allocator);
    }

    // Check if symbol exists
    if (symbol_registry.?.get(key)) |existing| {
        return existing;
    }

    // Create new symbol and register it
    const key_copy = if (ctx.hybrid != null)
        (ctx.allocator.dupe(u8, key) catch return value.JSValue.undefined_val)
    else
        key;
    const symbol = createSymbol(ctx.allocator, key_copy) catch return value.JSValue.undefined_val;
    symbol_registry.?.put(key_copy, symbol) catch return value.JSValue.undefined_val;
    return symbol;
}

/// Symbol.keyFor(sym) - Get the key for a registered symbol
pub fn symbolKeyFor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.undefined_val;

    const sym = args[0];
    if (!sym.isSymbol()) return value.JSValue.undefined_val;

    const sym_id = sym.getSymbolId();

    // Search registry for matching symbol
    if (symbol_registry) |*reg| {
        var iter = reg.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.*.getSymbolId() == sym_id) {
                return ctx.createString(entry.key_ptr.*) catch return value.JSValue.undefined_val;
            }
        }
    }

    return value.JSValue.undefined_val;
}

/// Symbol.prototype.toString() - Get symbol as string
pub fn symbolToString(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isSymbol()) return value.JSValue.undefined_val;

    const desc = this.getSymbolDescription();
    if (desc) |d| {
        // Return "Symbol(description)"
        var buf: [256]u8 = undefined;
        const result = std.fmt.bufPrint(&buf, "Symbol({s})", .{d}) catch return value.JSValue.undefined_val;
        return ctx.createString(result) catch return value.JSValue.undefined_val;
    }

    return ctx.createString("Symbol()") catch return value.JSValue.undefined_val;
}

/// Symbol.prototype.description getter
pub fn symbolDescription(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isSymbol()) return value.JSValue.undefined_val;

    const desc = this.getSymbolDescription();
    if (desc) |d| {
        return ctx.createString(d) catch return value.JSValue.undefined_val;
    }

    return value.JSValue.undefined_val;
}

// ============================================================================
// WeakMap implementation
// ============================================================================

/// WeakMap uses object identity as keys with weak references
/// Simplified implementation - uses a regular map but with object pointer keys
pub const WeakMapData = struct {
    entries: std.AutoHashMap(usize, value.JSValue),

    pub fn init(allocator: std.mem.Allocator) WeakMapData {
        return .{
            .entries = std.AutoHashMap(usize, value.JSValue).init(allocator),
        };
    }

    pub fn deinit(self: *WeakMapData) void {
        self.entries.deinit();
    }

    pub fn get(self: *WeakMapData, key: value.JSValue) ?value.JSValue {
        if (!key.isObject()) return null;
        const ptr_key = key.raw & ~@as(u64, 0x7);
        return self.entries.get(ptr_key);
    }

    pub fn set(self: *WeakMapData, key: value.JSValue, val: value.JSValue) !void {
        if (!key.isObject()) return error.InvalidKey;
        const ptr_key = key.raw & ~@as(u64, 0x7);
        try self.entries.put(ptr_key, val);
    }

    pub fn has(self: *WeakMapData, key: value.JSValue) bool {
        if (!key.isObject()) return false;
        const ptr_key = key.raw & ~@as(u64, 0x7);
        return self.entries.contains(ptr_key);
    }

    pub fn delete(self: *WeakMapData, key: value.JSValue) bool {
        if (!key.isObject()) return false;
        const ptr_key = key.raw & ~@as(u64, 0x7);
        return self.entries.remove(ptr_key);
    }
};

/// WeakMap constructor - new WeakMap()
pub fn weakMapConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Create WeakMap object
    const weak_map = ctx.createObject(null) catch return value.JSValue.undefined_val;
    weak_map.class_id = .weak_map;

    // Allocate and store WeakMapData
    const data_allocator = if (ctx.hybrid) |h| h.arena.allocator() else ctx.allocator;
    const data = if (ctx.hybrid) |h|
        h.arena.create(WeakMapData) orelse return value.JSValue.undefined_val
    else
        ctx.allocator.create(WeakMapData) catch return value.JSValue.undefined_val;
    data.* = WeakMapData.init(data_allocator);
    weak_map.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA] = value.JSValue.fromExternPtr(data);

    // If iterable argument provided, add entries
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Iterate over argument and add entries
    }

    return weak_map.toValue();
}

/// WeakMap.prototype.get(key)
pub fn weakMapGet(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    if (args.len == 0) return value.JSValue.undefined_val;

    const map_obj = object.JSObject.fromValue(this);
    if (map_obj.class_id != .weak_map) return value.JSValue.undefined_val;

    const data_val = map_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.undefined_val;
    const data: *WeakMapData = @ptrCast(@alignCast(data_val.toExternPtr(WeakMapData)));

    return data.get(args[0]) orelse value.JSValue.undefined_val;
}

/// WeakMap.prototype.set(key, value)
pub fn weakMapSet(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    if (args.len < 2) return this;

    const map_obj = object.JSObject.fromValue(this);
    if (map_obj.class_id != .weak_map) return value.JSValue.undefined_val;

    const data_val = map_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.undefined_val;
    const data: *WeakMapData = @ptrCast(@alignCast(data_val.toExternPtr(WeakMapData)));

    data.set(args[0], args[1]) catch return value.JSValue.undefined_val;
    return this; // Return the WeakMap for chaining
}

/// WeakMap.prototype.has(key)
pub fn weakMapHas(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    if (args.len == 0) return value.JSValue.false_val;

    const map_obj = object.JSObject.fromValue(this);
    if (map_obj.class_id != .weak_map) return value.JSValue.false_val;

    const data_val = map_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.false_val;
    const data: *WeakMapData = @ptrCast(@alignCast(data_val.toExternPtr(WeakMapData)));

    return if (data.has(args[0])) value.JSValue.true_val else value.JSValue.false_val;
}

/// WeakMap.prototype.delete(key)
pub fn weakMapDelete(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    if (args.len == 0) return value.JSValue.false_val;

    const map_obj = object.JSObject.fromValue(this);
    if (map_obj.class_id != .weak_map) return value.JSValue.false_val;

    const data_val = map_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.false_val;
    const data: *WeakMapData = @ptrCast(@alignCast(data_val.toExternPtr(WeakMapData)));

    return if (data.delete(args[0])) value.JSValue.true_val else value.JSValue.false_val;
}

// ============================================================================
// WeakSet implementation
// ============================================================================

/// WeakSet uses object identity with weak references
pub const WeakSetData = struct {
    entries: std.AutoHashMap(usize, void),

    pub fn init(allocator: std.mem.Allocator) WeakSetData {
        return .{
            .entries = std.AutoHashMap(usize, void).init(allocator),
        };
    }

    pub fn deinit(self: *WeakSetData) void {
        self.entries.deinit();
    }

    pub fn add(self: *WeakSetData, val: value.JSValue) !void {
        if (!val.isObject()) return error.InvalidValue;
        const ptr_key = val.raw & ~@as(u64, 0x7);
        try self.entries.put(ptr_key, {});
    }

    pub fn has(self: *WeakSetData, val: value.JSValue) bool {
        if (!val.isObject()) return false;
        const ptr_key = val.raw & ~@as(u64, 0x7);
        return self.entries.contains(ptr_key);
    }

    pub fn delete(self: *WeakSetData, val: value.JSValue) bool {
        if (!val.isObject()) return false;
        const ptr_key = val.raw & ~@as(u64, 0x7);
        return self.entries.remove(ptr_key);
    }
};

/// WeakSet constructor - new WeakSet()
pub fn weakSetConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Create WeakSet object
    const weak_set = ctx.createObject(null) catch return value.JSValue.undefined_val;
    weak_set.class_id = .weak_set;

    // Allocate and store WeakSetData
    const data_allocator = if (ctx.hybrid) |h| h.arena.allocator() else ctx.allocator;
    const data = if (ctx.hybrid) |h|
        h.arena.create(WeakSetData) orelse return value.JSValue.undefined_val
    else
        ctx.allocator.create(WeakSetData) catch return value.JSValue.undefined_val;
    data.* = WeakSetData.init(data_allocator);
    weak_set.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA] = value.JSValue.fromExternPtr(data);

    // If iterable argument provided, add entries
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Iterate over argument and add entries
    }

    return weak_set.toValue();
}

/// WeakSet.prototype.add(value)
pub fn weakSetAdd(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    if (args.len == 0) return this;

    const set_obj = object.JSObject.fromValue(this);
    if (set_obj.class_id != .weak_set) return value.JSValue.undefined_val;

    const data_val = set_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.undefined_val;
    const data: *WeakSetData = @ptrCast(@alignCast(data_val.toExternPtr(WeakSetData)));

    data.add(args[0]) catch return value.JSValue.undefined_val;
    return this; // Return the WeakSet for chaining
}

/// WeakSet.prototype.has(value)
pub fn weakSetHas(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    if (args.len == 0) return value.JSValue.false_val;

    const set_obj = object.JSObject.fromValue(this);
    if (set_obj.class_id != .weak_set) return value.JSValue.false_val;

    const data_val = set_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.false_val;
    const data: *WeakSetData = @ptrCast(@alignCast(data_val.toExternPtr(WeakSetData)));

    return if (data.has(args[0])) value.JSValue.true_val else value.JSValue.false_val;
}

/// WeakSet.prototype.delete(value)
pub fn weakSetDelete(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    if (args.len == 0) return value.JSValue.false_val;

    const set_obj = object.JSObject.fromValue(this);
    if (set_obj.class_id != .weak_set) return value.JSValue.false_val;

    const data_val = set_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.false_val;
    const data: *WeakSetData = @ptrCast(@alignCast(data_val.toExternPtr(WeakSetData)));

    return if (data.delete(args[0])) value.JSValue.true_val else value.JSValue.false_val;
}

/// Initialize WeakMap and WeakSet built-ins
pub fn initWeakCollections(ctx: *context.Context) !void {
    const allocator = ctx.allocator;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const root_class_idx = ctx.root_class_idx;

    // Create WeakMap prototype
    const weak_map_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, weak_map_proto, "get", wrap(weakMapGet), 1);
    try addMethodDynamic(ctx, weak_map_proto, "set", wrap(weakMapSet), 2);
    try addMethodDynamic(ctx, weak_map_proto, "has", wrap(weakMapHas), 1);
    try addMethodDynamic(ctx, weak_map_proto, "delete", wrap(weakMapDelete), 1);

    // Create WeakMap constructor
    const weak_map_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(weakMapConstructor), .WeakMap, 0);
    try ctx.setPropertyChecked(weak_map_ctor, .prototype, weak_map_proto.toValue());
    try ctx.setGlobal(.WeakMap, weak_map_ctor.toValue());

    // Create WeakSet prototype
    const weak_set_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, weak_set_proto, "add", wrap(weakSetAdd), 1);
    try addMethodDynamic(ctx, weak_set_proto, "has", wrap(weakSetHas), 1);
    try addMethodDynamic(ctx, weak_set_proto, "delete", wrap(weakSetDelete), 1);

    // Create WeakSet constructor
    const weak_set_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(weakSetConstructor), .WeakSet, 0);
    try ctx.setPropertyChecked(weak_set_ctor, .prototype, weak_set_proto.toValue());
    try ctx.setGlobal(.WeakSet, weak_set_ctor.toValue());
}

/// Initialize Symbol built-in and well-known symbols
pub fn initSymbol(ctx: *context.Context) !void {
    const allocator = ctx.allocator;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const root_class_idx = ctx.root_class_idx;

    // Create Symbol prototype
    const symbol_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, symbol_proto, "toString", wrap(symbolToString), 0);
    try addMethodDynamic(ctx, symbol_proto, "valueOf", wrap(symbolDescription), 0);

    // Create Symbol constructor
    const symbol_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(symbolConstructor), .Symbol, 1);
    try ctx.setPropertyChecked(symbol_ctor, .prototype, symbol_proto.toValue());

    // Add static methods
    try addMethodDynamic(ctx, symbol_ctor, "for", wrap(symbolFor), 1);
    try addMethodDynamic(ctx, symbol_ctor, "keyFor", wrap(symbolKeyFor), 1);

    // Add well-known symbols as static properties
    const iterator_sym = try createWellKnownSymbol(allocator, .iterator, "Symbol.iterator");
    const async_iterator_sym = try createWellKnownSymbol(allocator, .asyncIterator, "Symbol.asyncIterator");
    const to_string_tag_sym = try createWellKnownSymbol(allocator, .toStringTag, "Symbol.toStringTag");
    const to_primitive_sym = try createWellKnownSymbol(allocator, .toPrimitive, "Symbol.toPrimitive");
    const has_instance_sym = try createWellKnownSymbol(allocator, .hasInstance, "Symbol.hasInstance");

    const iterator_atom = try ctx.atoms.intern("iterator");
    const async_iterator_atom = try ctx.atoms.intern("asyncIterator");
    const to_string_tag_atom = try ctx.atoms.intern("toStringTag");
    const to_primitive_atom = try ctx.atoms.intern("toPrimitive");
    const has_instance_atom = try ctx.atoms.intern("hasInstance");

    try ctx.setPropertyChecked(symbol_ctor, iterator_atom, iterator_sym);
    try ctx.setPropertyChecked(symbol_ctor, async_iterator_atom, async_iterator_sym);
    try ctx.setPropertyChecked(symbol_ctor, to_string_tag_atom, to_string_tag_sym);
    try ctx.setPropertyChecked(symbol_ctor, to_primitive_atom, to_primitive_sym);
    try ctx.setPropertyChecked(symbol_ctor, has_instance_atom, has_instance_sym);

    try ctx.setGlobal(.Symbol, symbol_ctor.toValue());
}

test "Math.abs" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Integer abs
    const result = mathAbs(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(-42)});
    try std.testing.expectEqual(@as(i32, 42), result.getInt());

    // Positive stays positive
    const result2 = mathAbs(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(10)});
    try std.testing.expectEqual(@as(i32, 10), result2.getInt());
}

test "Math.min/max" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const args = [_]value.JSValue{
        value.JSValue.fromInt(5),
        value.JSValue.fromInt(3),
        value.JSValue.fromInt(8),
    };

    const min_result = mathMin(ctx, value.JSValue.undefined_val, &args);
    try std.testing.expectEqual(@as(i32, 3), min_result.getInt());

    const max_result = mathMax(ctx, value.JSValue.undefined_val, &args);
    try std.testing.expectEqual(@as(i32, 8), max_result.getInt());
}

test "Math.floor/ceil/round" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // floor(5) = 5
    const floor_result = mathFloor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(5)});
    try std.testing.expectEqual(@as(i32, 5), floor_result.getInt());

    // ceil(5) = 5
    const ceil_result = mathCeil(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(5)});
    try std.testing.expectEqual(@as(i32, 5), ceil_result.getInt());

    // round(5) = 5
    const round_result = mathRound(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(5)});
    try std.testing.expectEqual(@as(i32, 5), round_result.getInt());
}

test "Math.pow" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // 2^3 = 8
    const result = mathPow(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(2),
        value.JSValue.fromInt(3),
    });
    try std.testing.expectEqual(@as(i32, 8), result.getInt());

    // 3^2 = 9
    const result2 = mathPow(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(3),
        value.JSValue.fromInt(2),
    });
    try std.testing.expectEqual(@as(i32, 9), result2.getInt());
}

test "Math.sqrt" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // sqrt(16) = 4
    const result = mathSqrt(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(16)});
    try std.testing.expectEqual(@as(i32, 4), result.getInt());

    // sqrt(9) = 3
    const result2 = mathSqrt(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(9)});
    try std.testing.expectEqual(@as(i32, 3), result2.getInt());
}

test "Math.sign" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // sign(10) = 1
    const pos = mathSign(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(10)});
    try std.testing.expectEqual(@as(i32, 1), pos.getInt());

    // sign(-5) = -1
    const neg = mathSign(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(-5)});
    try std.testing.expectEqual(@as(i32, -1), neg.getInt());

    // sign(0) = 0
    const zero = mathSign(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(0)});
    try std.testing.expectEqual(@as(i32, 0), zero.getInt());
}

test "Math constants" {
    // PI
    try std.testing.expect(math_constants.PI > 3.14 and math_constants.PI < 3.15);
    // E
    try std.testing.expect(math_constants.E > 2.71 and math_constants.E < 2.72);
    // SQRT2
    try std.testing.expect(math_constants.SQRT2 > 1.41 and math_constants.SQRT2 < 1.42);
}

// Object.isFrozen test removed - freeze/isFrozen methods removed

test "Array.isArray" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Regular object is not an array
    var pool = try object.HiddenClassPool.init(allocator);
    defer pool.deinit();

    var obj = try object.JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer obj.destroy(allocator);

    const not_array = arrayIsArray(ctx, value.JSValue.undefined_val, &[_]value.JSValue{obj.toValue()});
    try std.testing.expect(not_array.isFalse());

    // Non-object values
    const int_not_array = arrayIsArray(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(42)});
    try std.testing.expect(int_not_array.isFalse());

    // Empty args
    const empty = arrayIsArray(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    try std.testing.expect(empty.isFalse());
}

test "Array.slice" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Use ctx's pool so arraySlice can find properties
    const pool = ctx.hidden_class_pool.?;

    var arr = try object.JSObject.create(allocator, pool.getEmptyClass(), null, pool);
    defer arr.destroy(allocator);

    // Set length = 10
    try arr.setProperty(allocator, pool, .length, value.JSValue.fromInt(10));

    // slice(2, 5) should return length 3
    const result = arraySlice(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.fromInt(2),
        value.JSValue.fromInt(5),
    });
    try std.testing.expectEqual(@as(i32, 3), result.getInt());

    // slice(-3) from end
    const from_end = arraySlice(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.fromInt(-3),
    });
    try std.testing.expectEqual(@as(i32, 3), from_end.getInt());
}

test "String.slice" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create a test string
    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);

    const str_val = value.JSValue.fromPtr(str);

    // slice(0, 5) = "hello"
    const result = stringSlice(ctx, str_val, &[_]value.JSValue{
        value.JSValue.fromInt(0),
        value.JSValue.fromInt(5),
    });
    try std.testing.expect(result.isString());
    try std.testing.expect(std.mem.eql(u8, result.toPtr(string.JSString).data(), "hello"));

    // slice(6) = "world"
    const from_6 = stringSlice(ctx, str_val, &[_]value.JSValue{
        value.JSValue.fromInt(6),
    });
    try std.testing.expect(from_6.isString());
    try std.testing.expect(std.mem.eql(u8, from_6.toPtr(string.JSString).data(), "world"));
}

test "String.charCodeAt" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "ABC");
    defer string.freeString(allocator, str);

    const str_val = value.JSValue.fromPtr(str);

    // charCodeAt(0) = 65 ('A')
    const a_code = stringCharCodeAt(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(0)});
    try std.testing.expectEqual(@as(i32, 65), a_code.getInt());

    // charCodeAt(1) = 66 ('B')
    const b_code = stringCharCodeAt(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(1)});
    try std.testing.expectEqual(@as(i32, 66), b_code.getInt());

    // charCodeAt(2) = 67 ('C')
    const c_code = stringCharCodeAt(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(2)});
    try std.testing.expectEqual(@as(i32, 67), c_code.getInt());
}

test "String.repeat" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "ab");
    defer string.freeString(allocator, str);

    const str_val = value.JSValue.fromPtr(str);

    // repeat(3) = "ababab" (length 6)
    const result = stringRepeat(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(3)});
    try std.testing.expectEqual(@as(i32, 6), result.getInt());

    // repeat(0) = "" (length 0)
    const empty = stringRepeat(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(0)});
    try std.testing.expectEqual(@as(i32, 0), empty.getInt());
}

test "simple setGlobal getGlobal with predefined atom" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Just set console to an integer value
    try ctx.setGlobal(.console, value.JSValue.fromInt(42));

    // Check console is registered
    const console_val = ctx.getGlobal(.console);
    try std.testing.expect(console_val != null);
    try std.testing.expectEqual(@as(i32, 42), console_val.?.getInt());
}

test "initBuiltins registers console and Math" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Initialize built-ins
    try initBuiltins(ctx);

    // Check console is registered on global
    const console_val = ctx.getGlobal(.console);
    try std.testing.expect(console_val != null);
    try std.testing.expect(console_val.?.isObject());

    // Check Math is registered on global
    const math_val = ctx.getGlobal(.Math);
    try std.testing.expect(math_val != null);
    try std.testing.expect(math_val.?.isObject());

    // Check Math has abs method
    const math_obj = object.JSObject.fromValue(math_val.?);
    const pool = ctx.hidden_class_pool.?;
    const abs_val = math_obj.getProperty(pool, .abs);
    try std.testing.expect(abs_val != null);
    try std.testing.expect(abs_val.?.isCallable());

    // Check Response is registered and callable (it's a constructor)
    const response_val = ctx.getGlobal(.Response);
    try std.testing.expect(response_val != null);
    try std.testing.expect(response_val.?.isObject());
    try std.testing.expect(response_val.?.isCallable());

    // Check Response.json exists
    const response_obj = object.JSObject.fromValue(response_val.?);
    const json_method = response_obj.getProperty(pool, .json);
    try std.testing.expect(json_method != null);
    try std.testing.expect(json_method.?.isCallable());

    // Check h() is registered
    const h_val = ctx.getGlobal(.h);
    try std.testing.expect(h_val != null);
    try std.testing.expect(h_val.?.isCallable());

    // Check renderToString is registered
    const render_val = ctx.getGlobal(.renderToString);
    try std.testing.expect(render_val != null);
    try std.testing.expect(render_val.?.isCallable());

    // Check Fragment is registered
    const fragment_val = ctx.getGlobal(.Fragment);
    try std.testing.expect(fragment_val != null);
    try std.testing.expect(fragment_val.?.isString());
}

test "Number.isInteger" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Integer value should return true
    const int_result = numberIsInteger(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });
    try std.testing.expect(int_result.isBool());
    try std.testing.expect(int_result.getBool() == true);

    // Float with fractional part should return false
    const float_box = try gc_state.allocFloat(3.14);
    const float_result = numberIsInteger(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(float_box),
    });
    try std.testing.expect(float_result.isBool());
    try std.testing.expect(float_result.getBool() == false);

    // Float that equals integer should return true
    const int_float_box = try gc_state.allocFloat(42.0);
    const int_float_result = numberIsInteger(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(int_float_box),
    });
    try std.testing.expect(int_float_result.isBool());
    try std.testing.expect(int_float_result.getBool() == true);
}

test "Number.isNaN" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // NaN should return true
    const nan_result = numberIsNaN(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.nan_val,
    });
    try std.testing.expect(nan_result.isBool());
    try std.testing.expect(nan_result.getBool() == true);

    // Regular number should return false
    const num_result = numberIsNaN(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });
    try std.testing.expect(num_result.isBool());
    try std.testing.expect(num_result.getBool() == false);
}

test "Number.isFinite" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Regular integer should return true
    const int_result = numberIsFinite(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });
    try std.testing.expect(int_result.isBool());
    try std.testing.expect(int_result.getBool() == true);

    // Infinity should return false
    const inf_box = try gc_state.allocFloat(std.math.inf(f64));
    const inf_result = numberIsFinite(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(inf_box),
    });
    try std.testing.expect(inf_result.isBool());
    try std.testing.expect(inf_result.getBool() == false);

    // NaN should return false
    const nan_result = numberIsFinite(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.nan_val,
    });
    try std.testing.expect(nan_result.isBool());
    try std.testing.expect(nan_result.getBool() == false);
}

// ============================================================================
// String Method Tests
// ============================================================================

test "String.indexOf basic match" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const needle = try string.createString(allocator, "world");
    defer string.freeString(allocator, needle);

    // indexOf("world") = 6
    const result = stringIndexOf(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(needle)});
    try std.testing.expectEqual(@as(i32, 6), result.getInt());
}

test "String.indexOf no match" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const needle = try string.createString(allocator, "xyz");
    defer string.freeString(allocator, needle);

    // indexOf("xyz") = -1 (not found)
    const result = stringIndexOf(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(needle)});
    try std.testing.expectEqual(@as(i32, -1), result.getInt());
}

test "String.indexOf empty args" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    // indexOf() with no args returns -1
    const result = stringIndexOf(ctx, str_val, &[_]value.JSValue{});
    try std.testing.expectEqual(@as(i32, -1), result.getInt());
}

test "String.lastIndexOf basic match" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello hello");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const needle = try string.createString(allocator, "hello");
    defer string.freeString(allocator, needle);

    // lastIndexOf("hello") = 6 (second occurrence)
    const result = stringLastIndexOf(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(needle)});
    try std.testing.expectEqual(@as(i32, 6), result.getInt());
}

test "String.lastIndexOf no match" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const needle = try string.createString(allocator, "xyz");
    defer string.freeString(allocator, needle);

    // lastIndexOf("xyz") = -1
    const result = stringLastIndexOf(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(needle)});
    try std.testing.expectEqual(@as(i32, -1), result.getInt());
}

test "String.startsWith true case" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const prefix = try string.createString(allocator, "hello");
    defer string.freeString(allocator, prefix);

    const result = stringStartsWith(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(prefix)});
    try std.testing.expect(result.getBool() == true);
}

test "String.startsWith false case" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const prefix = try string.createString(allocator, "world");
    defer string.freeString(allocator, prefix);

    const result = stringStartsWith(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(prefix)});
    try std.testing.expect(result.getBool() == false);
}

test "String.endsWith true case" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const suffix = try string.createString(allocator, "world");
    defer string.freeString(allocator, suffix);

    const result = stringEndsWith(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(suffix)});
    try std.testing.expect(result.getBool() == true);
}

test "String.endsWith false case" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const suffix = try string.createString(allocator, "hello");
    defer string.freeString(allocator, suffix);

    const result = stringEndsWith(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(suffix)});
    try std.testing.expect(result.getBool() == false);
}

test "String.includes found" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const needle = try string.createString(allocator, "wor");
    defer string.freeString(allocator, needle);

    const result = stringIncludes(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(needle)});
    try std.testing.expect(result.getBool() == true);
}

test "String.includes not found" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);
    const str_val = value.JSValue.fromPtr(str);

    const needle = try string.createString(allocator, "xyz");
    defer string.freeString(allocator, needle);

    const result = stringIncludes(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(needle)});
    try std.testing.expect(result.getBool() == false);
}

test "String.trim whitespace" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "  hello world  ");
    const str_val = value.JSValue.fromPtr(str);

    const result = stringTrim(ctx, str_val, &[_]value.JSValue{});
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("hello world", result.toPtr(string.JSString).data());
}

test "String.trimStart whitespace" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "  hello  ");
    const str_val = value.JSValue.fromPtr(str);

    const result = stringTrimStart(ctx, str_val, &[_]value.JSValue{});
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("hello  ", result.toPtr(string.JSString).data());
}

test "String.trimEnd whitespace" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "  hello  ");
    const str_val = value.JSValue.fromPtr(str);

    const result = stringTrimEnd(ctx, str_val, &[_]value.JSValue{});
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("  hello", result.toPtr(string.JSString).data());
}

test "String.substring basic" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    const str_val = value.JSValue.fromPtr(str);

    // substring(0, 5) = "hello"
    const result = stringSubstring(ctx, str_val, &[_]value.JSValue{
        value.JSValue.fromInt(0),
        value.JSValue.fromInt(5),
    });
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("hello", result.toPtr(string.JSString).data());
}

test "String.substring swapped indices" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello world");
    const str_val = value.JSValue.fromPtr(str);

    // substring(5, 0) should swap to (0, 5) = "hello"
    const result = stringSubstring(ctx, str_val, &[_]value.JSValue{
        value.JSValue.fromInt(5),
        value.JSValue.fromInt(0),
    });
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("hello", result.toPtr(string.JSString).data());
}

test "String.split with delimiter" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "a,b,c");
    const str_val = value.JSValue.fromPtr(str);

    const sep = try string.createString(allocator, ",");

    const result = stringSplit(ctx, str_val, &[_]value.JSValue{value.JSValue.fromPtr(sep)});
    try std.testing.expect(result.isObject());

    // Check array length
    const arr = result.toPtr(object.JSObject);
    try std.testing.expectEqual(@as(u32, 3), arr.getArrayLength());
}

test "String.split with limit" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "a,b,c,d,e");
    const str_val = value.JSValue.fromPtr(str);

    const sep = try string.createString(allocator, ",");

    // split(",", 2) should only return 2 elements
    const result = stringSplit(ctx, str_val, &[_]value.JSValue{
        value.JSValue.fromPtr(sep),
        value.JSValue.fromInt(2),
    });
    try std.testing.expect(result.isObject());

    const arr = result.toPtr(object.JSObject);
    try std.testing.expectEqual(@as(u32, 2), arr.getArrayLength());
}

test "String.split no separator" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello");
    const str_val = value.JSValue.fromPtr(str);

    // split() with no args returns array with whole string
    const result = stringSplit(ctx, str_val, &[_]value.JSValue{});
    try std.testing.expect(result.isObject());

    const arr = result.toPtr(object.JSObject);
    try std.testing.expectEqual(@as(u32, 1), arr.getArrayLength());
}

test "String.concat multiple strings" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "hello");
    const str_val = value.JSValue.fromPtr(str);

    const s1 = try string.createString(allocator, " ");
    const s2 = try string.createString(allocator, "world");

    const result = stringConcat(ctx, str_val, &[_]value.JSValue{
        value.JSValue.fromPtr(s1),
        value.JSValue.fromPtr(s2),
    });
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("hello world", result.toPtr(string.JSString).data());
}

// ============================================================================
// Array Method Tests (non-mutating methods only)
// Mutating tests removed: push, pop, shift, unshift, splice, reverse
// ============================================================================

test "Array.indexOf finds element" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;

    // [10, 20, 30, 20]
    try ctx.setIndexChecked(arr, 0, value.JSValue.fromInt(10));
    try ctx.setIndexChecked(arr, 1, value.JSValue.fromInt(20));
    try ctx.setIndexChecked(arr, 2, value.JSValue.fromInt(30));
    try ctx.setIndexChecked(arr, 3, value.JSValue.fromInt(20));
    arr.setArrayLength(4);

    // indexOf(20) = 1
    const result = arrayIndexOf(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.fromInt(20),
    });
    try std.testing.expectEqual(@as(i32, 1), result.getInt());
}

test "Array.indexOf not found" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;

    try ctx.setIndexChecked(arr, 0, value.JSValue.fromInt(10));
    try ctx.setIndexChecked(arr, 1, value.JSValue.fromInt(20));
    arr.setArrayLength(2);

    // indexOf(99) = -1
    const result = arrayIndexOf(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.fromInt(99),
    });
    try std.testing.expectEqual(@as(i32, -1), result.getInt());
}

test "Array.includes found" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;

    try ctx.setIndexChecked(arr, 0, value.JSValue.fromInt(10));
    try ctx.setIndexChecked(arr, 1, value.JSValue.fromInt(20));
    arr.setArrayLength(2);

    const result = arrayIncludes(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.fromInt(20),
    });
    try std.testing.expect(result.getBool() == true);
}

test "Array.includes not found" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;

    try ctx.setIndexChecked(arr, 0, value.JSValue.fromInt(10));
    arr.setArrayLength(1);

    const result = arrayIncludes(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.fromInt(99),
    });
    try std.testing.expect(result.getBool() == false);
}

test "Array.join with separator" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;

    // [1, 2, 3]
    try ctx.setIndexChecked(arr, 0, value.JSValue.fromInt(1));
    try ctx.setIndexChecked(arr, 1, value.JSValue.fromInt(2));
    try ctx.setIndexChecked(arr, 2, value.JSValue.fromInt(3));
    arr.setArrayLength(3);

    const sep = try string.createString(allocator, "-");

    const result = arrayJoin(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.fromPtr(sep),
    });
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("1-2-3", result.toPtr(string.JSString).data());
}

test "Array.join default separator" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;

    try ctx.setIndexChecked(arr, 0, value.JSValue.fromInt(1));
    try ctx.setIndexChecked(arr, 1, value.JSValue.fromInt(2));
    arr.setArrayLength(2);

    // join() with no args uses comma
    const result = arrayJoin(ctx, arr.toValue(), &[_]value.JSValue{});
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("1,2", result.toPtr(string.JSString).data());
}

test "Array.concat returns total length" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // First array [1, 2]
    const arr1 = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr1.prototype = ctx.array_prototype;
    arr1.setArrayLength(2);

    // Second array [3, 4]
    const arr2 = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr2.prototype = ctx.array_prototype;
    arr2.setArrayLength(2);

    // concat returns total length (stub behavior)
    const result = arrayConcat(ctx, arr1.toValue(), &[_]value.JSValue{
        arr2.toValue(),
    });

    try std.testing.expectEqual(@as(i32, 4), result.getInt());
}

test "Array.map returns length stub" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;

    try ctx.setIndexChecked(arr, 0, value.JSValue.fromInt(1));
    try ctx.setIndexChecked(arr, 1, value.JSValue.fromInt(2));
    try ctx.setIndexChecked(arr, 2, value.JSValue.fromInt(3));
    arr.setArrayLength(3);

    // map with no callback returns length (stub behavior)
    const result = arrayMap(ctx, arr.toValue(), &[_]value.JSValue{});
    try std.testing.expectEqual(@as(i32, 3), result.getInt());
}

test "Array.filter returns zero stub" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;
    arr.setArrayLength(2);

    // filter with no callback returns 0 (stub behavior - needs callback infrastructure)
    const result = arrayFilter(ctx, arr.toValue(), &[_]value.JSValue{});
    try std.testing.expectEqual(@as(i32, 0), result.getInt());
}

test "Array.reduce returns initial value or undefined" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;
    arr.setArrayLength(3);

    // reduce with no args returns undefined (stub behavior)
    const result = arrayReduce(ctx, arr.toValue(), &[_]value.JSValue{});
    try std.testing.expect(result.isUndefined());

    // reduce with initial value returns that value (stub behavior)
    const with_init = arrayReduce(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.undefined_val, // callback placeholder
        value.JSValue.fromInt(42), // initial value
    });
    try std.testing.expectEqual(@as(i32, 42), with_init.getInt());
}

test "Array.every returns true stub" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;
    arr.setArrayLength(0);

    // every on empty array returns true
    const result = arrayEvery(ctx, arr.toValue(), &[_]value.JSValue{});
    try std.testing.expect(result.getBool() == true);
}

test "Array.some returns false stub" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;
    arr.setArrayLength(0);

    // some on empty array returns false
    const result = arraySome(ctx, arr.toValue(), &[_]value.JSValue{});
    try std.testing.expect(result.getBool() == false);
}

test "Array.find returns undefined stub" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;
    arr.setArrayLength(0);

    // find on empty array returns undefined
    const result = arrayFind(ctx, arr.toValue(), &[_]value.JSValue{});
    try std.testing.expect(result.isUndefined());
}

test "Array.findIndex returns -1 stub" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const arr = try object.JSObject.createArray(allocator, ctx.root_class_idx);
    arr.prototype = ctx.array_prototype;
    arr.setArrayLength(0);

    // findIndex on empty array returns -1
    const result = arrayFindIndex(ctx, arr.toValue(), &[_]value.JSValue{});
    try std.testing.expectEqual(@as(i32, -1), result.getInt());
}

// ============================================================================
// JSON Method Tests
// ============================================================================

test "JSON.parse object" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const json_str = try string.createString(allocator, "{\"key\":123}");
    const result = jsonParse(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(json_str),
    });

    try std.testing.expect(result.isObject());
}

test "JSON.parse number" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const json_str = try string.createString(allocator, "42");
    const result = jsonParse(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(json_str),
    });

    try std.testing.expectEqual(@as(i32, 42), result.getInt());
}

test "JSON.parse string" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const json_str = try string.createString(allocator, "\"hello\"");
    const result = jsonParse(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(json_str),
    });

    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("hello", result.toPtr(string.JSString).data());
}

test "JSON.parse boolean" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const true_str = try string.createString(allocator, "true");
    const result = jsonParse(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(true_str),
    });

    try std.testing.expect(result.getBool() == true);
}

test "JSON.parse null" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const null_str = try string.createString(allocator, "null");
    const result = jsonParse(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(null_str),
    });

    try std.testing.expect(result.isNull());
}

test "JSON.parse array" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const json_str = try string.createString(allocator, "[1,2,3]");
    const result = jsonParse(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(json_str),
    });

    try std.testing.expect(result.isObject());
}

test "JSON.parse invalid returns undefined" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const invalid_str = try string.createString(allocator, "{invalid}");
    const result = jsonParse(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(invalid_str),
    });

    try std.testing.expect(result.isUndefined());
}

test "JSON.stringify object" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var pool = try object.HiddenClassPool.init(allocator);
    defer pool.deinit();

    const obj = try object.JSObject.create(allocator, pool.getEmptyClass(), null, pool);

    const result = jsonStringify(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        obj.toValue(),
    });

    try std.testing.expect(result.isString());
}

test "JSON.stringify number" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const result = jsonStringify(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });

    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("42", result.toPtr(string.JSString).data());
}

// ============================================================================
// Hybrid Allocation Tests
// ============================================================================

test "Hybrid: JSON.parse returns arena object" {
    const gc = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const arena_mod = @import("arena.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var req_arena = try arena_mod.Arena.init(allocator, .{ .size = 4096 });
    defer req_arena.deinit();
    var hybrid = arena_mod.HybridAllocator{
        .persistent = allocator,
        .arena = &req_arena,
    };
    ctx.setHybridAllocator(&hybrid);

    const json_text = try ctx.createString("{\"a\":1}");
    const result = jsonParse(ctx, value.JSValue.undefined_val, &[_]value.JSValue{json_text});
    try std.testing.expect(result.isObject());

    const obj = object.JSObject.fromValue(result);
    try std.testing.expect(obj.flags.is_arena);

    const atom = try ctx.atoms.intern("a");
    const pool = ctx.hidden_class_pool.?;
    const prop = obj.getProperty(pool, atom) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i32, 1), prop.getInt());
    try std.testing.expect(!ctx.hasException());
}

test "Hybrid: Map and Set accept arena values" {
    const gc = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const arena_mod = @import("arena.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var req_arena = try arena_mod.Arena.init(allocator, .{ .size = 4096 });
    defer req_arena.deinit();
    var hybrid = arena_mod.HybridAllocator{
        .persistent = allocator,
        .arena = &req_arena,
    };
    ctx.setHybridAllocator(&hybrid);

    const map_val = mapConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    const set_val = setConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});

    const key_obj = try ctx.createObject(null);
    const val_obj = try ctx.createObject(null);

    _ = mapSet(ctx, map_val, &[_]value.JSValue{ key_obj.toValue(), val_obj.toValue() });
    try std.testing.expect(!ctx.hasException());

    const got = mapGet(ctx, map_val, &[_]value.JSValue{key_obj.toValue()});
    try std.testing.expect(got.strictEquals(val_obj.toValue()));

    _ = setAdd(ctx, set_val, &[_]value.JSValue{val_obj.toValue()});
    const has = setHas(ctx, set_val, &[_]value.JSValue{val_obj.toValue()});
    try std.testing.expect(has.getBool() == true);
}

// Promise test removed - Promise implementation removed

test "Hybrid: WeakMap and WeakSet accept arena values" {
    const gc = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const arena_mod = @import("arena.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var req_arena = try arena_mod.Arena.init(allocator, .{ .size = 4096 });
    defer req_arena.deinit();
    var hybrid = arena_mod.HybridAllocator{
        .persistent = allocator,
        .arena = &req_arena,
    };
    ctx.setHybridAllocator(&hybrid);

    const key_obj = try ctx.createObject(null);
    const val_obj = try ctx.createObject(null);

    const weak_map_val = weakMapConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    _ = weakMapSet(ctx, weak_map_val, &[_]value.JSValue{ key_obj.toValue(), val_obj.toValue() });
    const got = weakMapGet(ctx, weak_map_val, &[_]value.JSValue{key_obj.toValue()});
    try std.testing.expect(got.strictEquals(val_obj.toValue()));

    const weak_set_val = weakSetConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    _ = weakSetAdd(ctx, weak_set_val, &[_]value.JSValue{val_obj.toValue()});
    const has = weakSetHas(ctx, weak_set_val, &[_]value.JSValue{val_obj.toValue()});
    try std.testing.expect(has.getBool() == true);
}

// ============================================================================
// Map Tests
// ============================================================================

test "Map constructor creates empty map" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const result = mapConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    try std.testing.expect(result.isObject());
}

test "Map.set and Map.get" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const map = mapConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});

    // Set a value
    _ = mapSet(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(1),
        value.JSValue.fromInt(100),
    });

    // Get the value
    const result = mapGet(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(1),
    });

    try std.testing.expectEqual(@as(i32, 100), result.getInt());
}

test "Map.get missing key returns undefined" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const map = mapConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});

    const result = mapGet(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(999),
    });

    try std.testing.expect(result.isUndefined());
}

test "Map.has returns true for existing key" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const map = mapConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    _ = mapSet(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(1),
        value.JSValue.fromInt(100),
    });

    const result = mapHas(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(1),
    });

    try std.testing.expect(result.getBool() == true);
}

test "Map.has returns false for missing key" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const map = mapConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});

    const result = mapHas(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(999),
    });

    try std.testing.expect(result.getBool() == false);
}

test "Map.delete removes key" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const map = mapConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    _ = mapSet(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(1),
        value.JSValue.fromInt(100),
    });

    // Delete the key
    const deleted = mapDelete(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(1),
    });
    try std.testing.expect(deleted.getBool() == true);

    // Key should no longer exist
    const result = mapHas(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(1),
    });
    try std.testing.expect(result.getBool() == false);
}

test "Map.clear empties map" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const map = mapConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    _ = mapSet(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(1),
        value.JSValue.fromInt(100),
    });
    _ = mapSet(ctx, map, &[_]value.JSValue{
        value.JSValue.fromInt(2),
        value.JSValue.fromInt(200),
    });

    // Clear the map
    _ = mapClear(ctx, map, &[_]value.JSValue{});

    // All keys should be gone
    const has1 = mapHas(ctx, map, &[_]value.JSValue{value.JSValue.fromInt(1)});
    const has2 = mapHas(ctx, map, &[_]value.JSValue{value.JSValue.fromInt(2)});
    try std.testing.expect(has1.getBool() == false);
    try std.testing.expect(has2.getBool() == false);
}

// ============================================================================
// Set Tests
// ============================================================================

test "Set constructor creates empty set" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const result = setConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    try std.testing.expect(result.isObject());
}

test "Set.add and Set.has" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const set = setConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});

    // Add a value
    _ = setAdd(ctx, set, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });

    // Check it exists
    const result = setHas(ctx, set, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });

    try std.testing.expect(result.getBool() == true);
}

test "Set.has returns false for missing value" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const set = setConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});

    const result = setHas(ctx, set, &[_]value.JSValue{
        value.JSValue.fromInt(999),
    });

    try std.testing.expect(result.getBool() == false);
}

test "Set.delete removes value" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const set = setConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    _ = setAdd(ctx, set, &[_]value.JSValue{value.JSValue.fromInt(42)});

    // Delete the value
    const deleted = setDelete(ctx, set, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });
    try std.testing.expect(deleted.getBool() == true);

    // Value should no longer exist
    const result = setHas(ctx, set, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });
    try std.testing.expect(result.getBool() == false);
}

test "Set.clear empties set" {
    const gc = @import("gc.zig");
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const set = setConstructor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    _ = setAdd(ctx, set, &[_]value.JSValue{value.JSValue.fromInt(1)});
    _ = setAdd(ctx, set, &[_]value.JSValue{value.JSValue.fromInt(2)});

    // Clear the set
    _ = setClear(ctx, set, &[_]value.JSValue{});

    // All values should be gone
    const has1 = setHas(ctx, set, &[_]value.JSValue{value.JSValue.fromInt(1)});
    const has2 = setHas(ctx, set, &[_]value.JSValue{value.JSValue.fromInt(2)});
    try std.testing.expect(has1.getBool() == false);
    try std.testing.expect(has2.getBool() == false);
}
