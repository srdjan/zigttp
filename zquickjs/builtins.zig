//! Built-in JavaScript objects and methods
//!
//! Standard library implementation for Object, Array, String, etc.

const std = @import("std");
const value = @import("value.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const string = @import("string.zig");

/// Built-in class IDs
pub const ClassId = enum(u8) {
    object = 0,
    array = 1,
    function = 2,
    string = 3,
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

/// Native function signature
pub const NativeFunc = *const fn (*context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue;

/// Built-in function descriptor
pub const BuiltinFunc = struct {
    name: []const u8,
    func: NativeFunc,
    arg_count: u8,
};

// ============================================================================
// Object methods
// ============================================================================

pub fn objectKeys(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // TODO: Implement Object.keys
    return value.JSValue.undefined_val;
}

pub fn objectValues(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // TODO: Implement Object.values
    return value.JSValue.undefined_val;
}

pub fn objectEntries(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // TODO: Implement Object.entries
    return value.JSValue.undefined_val;
}

pub const object_methods = [_]BuiltinFunc{
    .{ .name = "keys", .func = objectKeys, .arg_count = 1 },
    .{ .name = "values", .func = objectValues, .arg_count = 1 },
    .{ .name = "entries", .func = objectEntries, .arg_count = 1 },
};

// ============================================================================
// Array methods
// ============================================================================

pub fn arrayPush(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // TODO: Implement Array.push
    return value.JSValue.undefined_val;
}

pub fn arrayPop(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // TODO: Implement Array.pop
    return value.JSValue.undefined_val;
}

pub fn arrayMap(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // TODO: Implement Array.map
    return value.JSValue.undefined_val;
}

pub fn arrayFilter(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // TODO: Implement Array.filter
    return value.JSValue.undefined_val;
}

pub fn arrayReduce(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // TODO: Implement Array.reduce
    return value.JSValue.undefined_val;
}

pub fn arrayForEach(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // TODO: Implement Array.forEach
    return value.JSValue.undefined_val;
}

pub const array_methods = [_]BuiltinFunc{
    .{ .name = "push", .func = arrayPush, .arg_count = 1 },
    .{ .name = "pop", .func = arrayPop, .arg_count = 0 },
    .{ .name = "map", .func = arrayMap, .arg_count = 1 },
    .{ .name = "filter", .func = arrayFilter, .arg_count = 1 },
    .{ .name = "reduce", .func = arrayReduce, .arg_count = 2 },
    .{ .name = "forEach", .func = arrayForEach, .arg_count = 1 },
};

// ============================================================================
// Math methods
// ============================================================================

pub fn mathAbs(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const arg = args[0];
    if (arg.isInt()) {
        const val = arg.getInt();
        return value.JSValue.fromInt(if (val < 0) -val else val);
    }
    return value.JSValue.undefined_val;
}

pub fn mathFloor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const arg = args[0];
    if (arg.isInt()) return arg; // Already integer
    // TODO: Handle floats
    return value.JSValue.undefined_val;
}

pub fn mathCeil(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const arg = args[0];
    if (arg.isInt()) return arg;
    // TODO: Handle floats
    return value.JSValue.undefined_val;
}

pub fn mathMin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    var min_val: i32 = std.math.maxInt(i32);
    for (args) |arg| {
        if (arg.isInt()) {
            min_val = @min(min_val, arg.getInt());
        }
    }
    return value.JSValue.fromInt(min_val);
}

pub fn mathMax(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    var max_val: i32 = std.math.minInt(i32);
    for (args) |arg| {
        if (arg.isInt()) {
            max_val = @max(max_val, arg.getInt());
        }
    }
    return value.JSValue.fromInt(max_val);
}

pub const math_methods = [_]BuiltinFunc{
    .{ .name = "abs", .func = mathAbs, .arg_count = 1 },
    .{ .name = "floor", .func = mathFloor, .arg_count = 1 },
    .{ .name = "ceil", .func = mathCeil, .arg_count = 1 },
    .{ .name = "min", .func = mathMin, .arg_count = 2 },
    .{ .name = "max", .func = mathMax, .arg_count = 2 },
};

// ============================================================================
// Console methods
// ============================================================================

pub fn consoleLog(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    const stdout = std.io.getStdOut().writer();
    for (args, 0..) |arg, i| {
        if (i > 0) stdout.writeAll(" ") catch {};
        printValue(stdout, arg) catch {};
    }
    stdout.writeAll("\n") catch {};
    return value.JSValue.undefined_val;
}

pub fn consoleWarn(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    const stderr = std.io.getStdErr().writer();
    stderr.writeAll("[WARN] ") catch {};
    for (args, 0..) |arg, i| {
        if (i > 0) stderr.writeAll(" ") catch {};
        printValue(stderr, arg) catch {};
    }
    stderr.writeAll("\n") catch {};
    return value.JSValue.undefined_val;
}

pub fn consoleError(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    const stderr = std.io.getStdErr().writer();
    stderr.writeAll("[ERROR] ") catch {};
    for (args, 0..) |arg, i| {
        if (i > 0) stderr.writeAll(" ") catch {};
        printValue(stderr, arg) catch {};
    }
    stderr.writeAll("\n") catch {};
    return value.JSValue.undefined_val;
}

fn printValue(writer: anytype, val: value.JSValue) !void {
    if (val.isInt()) {
        try writer.print("{d}", .{val.getInt()});
    } else if (val.isNull()) {
        try writer.writeAll("null");
    } else if (val.isUndefined()) {
        try writer.writeAll("undefined");
    } else if (val.isTrue()) {
        try writer.writeAll("true");
    } else if (val.isFalse()) {
        try writer.writeAll("false");
    } else {
        try writer.writeAll("[object]");
    }
}

pub const console_methods = [_]BuiltinFunc{
    .{ .name = "log", .func = consoleLog, .arg_count = 0 },
    .{ .name = "warn", .func = consoleWarn, .arg_count = 0 },
    .{ .name = "error", .func = consoleError, .arg_count = 0 },
};

// ============================================================================
// Initialization
// ============================================================================

/// Initialize all built-in objects on global
pub fn initBuiltins(ctx: *context.Context) !void {
    _ = ctx;
    // TODO: Create Object, Array, Math, console on global
}

test "Math.abs" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const result = mathAbs(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(-42)});
    try std.testing.expectEqual(@as(i32, 42), result.getInt());
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
