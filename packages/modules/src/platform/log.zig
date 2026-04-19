//! zigttp:log - Structured logging (NDJSON to stderr)

const std = @import("std");
const sdk = @import("zigttp-sdk");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:log",
    .name = "log",
    .required_capabilities = &.{ .clock, .stderr },
    .exports = &.{
        .{ .name = "logDebug", .module_func = makeLogImpl("debug"), .arg_count = 2, .returns = .unknown, .param_types = &.{ .string, .object }, .effect = .write, .traceable = false },
        .{ .name = "logInfo", .module_func = makeLogImpl("info"), .arg_count = 2, .returns = .unknown, .param_types = &.{ .string, .object }, .effect = .write, .traceable = false },
        .{ .name = "logWarn", .module_func = makeLogImpl("warn"), .arg_count = 2, .returns = .unknown, .param_types = &.{ .string, .object }, .effect = .write, .traceable = false },
        .{ .name = "logError", .module_func = makeLogImpl("error"), .arg_count = 2, .returns = .unknown, .param_types = &.{ .string, .object }, .effect = .write, .traceable = false },
    },
};

fn makeLogImpl(comptime level: []const u8) sdk.ModuleFn {
    return struct {
        fn f(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
            emitLog(handle, level, args) catch {};
            return sdk.JSValue.undefined_val;
        }
    }.f;
}

fn emitLog(handle: *sdk.ModuleHandle, level: []const u8, args: []const sdk.JSValue) !void {
    const message = if (args.len > 0) sdk.extractString(args[0]) orelse "" else "";
    const ts = try sdk.nowMs(handle);

    const allocator = sdk.getAllocator(handle);
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(allocator);

    try buf.appendSlice(allocator, "{\"level\":\"");
    try buf.appendSlice(allocator, level);
    try buf.appendSlice(allocator, "\",\"ts\":");
    try appendInt(&buf, allocator, ts);
    try buf.appendSlice(allocator, ",\"msg\":\"");
    try appendJsonEscaped(&buf, allocator, message);
    try buf.append(allocator, '"');

    if (args.len > 1 and sdk.isObject(args[1])) {
        const keys = sdk.objectKeys(handle, args[1]) catch {
            try buf.appendSlice(allocator, "}\n");
            _ = sdk.writeStderr(handle, buf.items) catch {};
            return;
        };
        const key_count = sdk.arrayLength(keys) orelse 0;
        var i: u32 = 0;
        while (i < key_count) : (i += 1) {
            const key_val = sdk.arrayGet(handle, keys, i) orelse continue;
            const key_name = sdk.extractString(key_val) orelse continue;
            const prop_val = sdk.objectGet(handle, args[1], key_name) orelse continue;

            try buf.appendSlice(allocator, ",\"");
            try appendJsonEscaped(&buf, allocator, key_name);
            try buf.appendSlice(allocator, "\":");
            try appendJsonValue(&buf, allocator, prop_val);
        }
    }

    try buf.appendSlice(allocator, "}\n");
    _ = sdk.writeStderr(handle, buf.items) catch {};
}

fn appendInt(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, n: i64) !void {
    var tmp: [20]u8 = undefined;
    const s = std.fmt.bufPrint(&tmp, "{d}", .{n}) catch return;
    try buf.appendSlice(allocator, s);
}

fn appendJsonEscaped(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, s: []const u8) !void {
    for (s) |c| switch (c) {
        '"' => try buf.appendSlice(allocator, "\\\""),
        '\\' => try buf.appendSlice(allocator, "\\\\"),
        '\n' => try buf.appendSlice(allocator, "\\n"),
        '\r' => try buf.appendSlice(allocator, "\\r"),
        '\t' => try buf.appendSlice(allocator, "\\t"),
        else => {
            if (c < 0x20) {
                var hex: [6]u8 = undefined;
                const esc = std.fmt.bufPrint(&hex, "\\u{x:0>4}", .{c}) catch continue;
                try buf.appendSlice(allocator, esc);
            } else {
                try buf.append(allocator, c);
            }
        },
    };
}

fn appendJsonValue(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, val: sdk.JSValue) !void {
    if (val.isUndefined() or val.isNull()) {
        try buf.appendSlice(allocator, "null");
    } else if (val.isTrue()) {
        try buf.appendSlice(allocator, "true");
    } else if (val.isFalse()) {
        try buf.appendSlice(allocator, "false");
    } else if (val.isInt()) {
        var tmp: [20]u8 = undefined;
        const s = std.fmt.bufPrint(&tmp, "{d}", .{val.getInt()}) catch return;
        try buf.appendSlice(allocator, s);
    } else if (sdk.extractFloat(val)) |f| {
        if (val.isInt()) {
            // already handled
        } else if (std.math.isNan(f) or std.math.isInf(f)) {
            try buf.appendSlice(allocator, "null");
        } else {
            var tmp: [32]u8 = undefined;
            const s = std.fmt.bufPrint(&tmp, "{d}", .{f}) catch return;
            try buf.appendSlice(allocator, s);
        }
    } else if (sdk.extractString(val)) |str| {
        try buf.append(allocator, '"');
        try appendJsonEscaped(buf, allocator, str);
        try buf.append(allocator, '"');
    } else {
        try buf.appendSlice(allocator, "\"[object]\"");
    }
}

test "appendJsonEscaped: plain string" {
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(std.testing.allocator);
    try appendJsonEscaped(&buf, std.testing.allocator, "hello world");
    try std.testing.expectEqualStrings("hello world", buf.items);
}

test "appendJsonEscaped: special chars" {
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(std.testing.allocator);
    try appendJsonEscaped(&buf, std.testing.allocator, "a\"b\\c\nd");
    try std.testing.expectEqualStrings("a\\\"b\\\\c\\nd", buf.items);
}

test "appendInt: positive" {
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(std.testing.allocator);
    try appendInt(&buf, std.testing.allocator, 1712345678901);
    try std.testing.expectEqualStrings("1712345678901", buf.items);
}

test "appendInt: negative" {
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(std.testing.allocator);
    try appendInt(&buf, std.testing.allocator, -42);
    try std.testing.expectEqualStrings("-42", buf.items);
}
