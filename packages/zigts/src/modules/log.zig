//! zigttp:log - Structured logging
//!
//! Exports:
//!   logDebug(message: string, context?: object) -> undefined
//!   logInfo(message: string, context?: object) -> undefined
//!   logWarn(message: string, context?: object) -> undefined
//!   logError(message: string, context?: object) -> undefined
//!
//! Emits newline-delimited JSON to stderr:
//!   {"level":"info","ts":1712345678901,"msg":"payment processed","userId":"123"}
//!
//! Context object properties are merged into the top-level JSON object.
//! Non-string values are coerced to their JS representation.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const object = @import("../object.zig");
const util = @import("util.zig");
const mb = @import("../module_binding.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:log",
    .name = "log",
    .required_capabilities = &.{ .clock, .stderr },
    .exports = &.{
        .{ .name = "logDebug", .func = logDebugNative, .arg_count = 2, .returns = .unknown, .param_types = &.{ .string, .object }, .effect = .write, .traceable = false },
        .{ .name = "logInfo", .func = logInfoNative, .arg_count = 2, .returns = .unknown, .param_types = &.{ .string, .object }, .effect = .write, .traceable = false },
        .{ .name = "logWarn", .func = logWarnNative, .arg_count = 2, .returns = .unknown, .param_types = &.{ .string, .object }, .effect = .write, .traceable = false },
        .{ .name = "logError", .func = logErrorNative, .arg_count = 2, .returns = .unknown, .param_types = &.{ .string, .object }, .effect = .write, .traceable = false },
    },
};

pub const exports = binding.toModuleExports();

// -------------------------------------------------------------------------
// Implementations - each delegates to emitLog with the level string
// -------------------------------------------------------------------------

fn makeLogFn(comptime level: []const u8) fn (*anyopaque, value.JSValue, []const value.JSValue) anyerror!value.JSValue {
    return struct {
        fn func(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            emitLog(util.castContext(ctx_ptr), level, args);
            return value.JSValue.undefined_val;
        }
    }.func;
}

const logDebugNative = makeLogFn("debug");
const logInfoNative = makeLogFn("info");
const logWarnNative = makeLogFn("warn");
const logErrorNative = makeLogFn("error");

// -------------------------------------------------------------------------
// Core log emitter
// -------------------------------------------------------------------------

fn emitLog(ctx: *context.Context, level: []const u8, args: []const value.JSValue) void {
    const message = if (args.len > 0) util.extractString(args[0]) orelse "" else "";
    const ts = mb.clockNowMsChecked();

    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(ctx.allocator);

    // Build JSON: {"level":"...","ts":...,"msg":"...",...context}
    buf.appendSlice(ctx.allocator, "{\"level\":\"") catch return;
    buf.appendSlice(ctx.allocator, level) catch return;
    buf.appendSlice(ctx.allocator, "\",\"ts\":") catch return;
    appendInt(&buf, ctx.allocator, ts) catch return;
    buf.appendSlice(ctx.allocator, ",\"msg\":\"") catch return;
    appendJsonEscaped(&buf, ctx.allocator, message) catch return;
    buf.append(ctx.allocator, '"') catch return;

    // Merge context object properties
    if (args.len > 1 and args[1].isObject()) {
        const ctx_obj = args[1].toPtr(object.JSObject);
        const pool = ctx.hidden_class_pool orelse {
            buf.appendSlice(ctx.allocator, "}\n") catch return;
            writeStderr(buf.items);
            return;
        };

        const keys = ctx_obj.getOwnEnumerableKeys(ctx.allocator, pool) catch {
            buf.appendSlice(ctx.allocator, "}\n") catch return;
            writeStderr(buf.items);
            return;
        };
        defer ctx.allocator.free(keys);

        for (keys) |atom| {
            const prop_val = ctx_obj.getProperty(pool, atom) orelse continue;
            const key_name = atomToString(atom, &ctx.atoms) orelse continue;

            buf.appendSlice(ctx.allocator, ",\"") catch continue;
            appendJsonEscaped(&buf, ctx.allocator, key_name) catch continue;
            buf.appendSlice(ctx.allocator, "\":") catch continue;
            appendJsonValue(&buf, ctx.allocator, prop_val) catch continue;
        }
    }

    buf.appendSlice(ctx.allocator, "}\n") catch return;
    writeStderr(buf.items);
}

// -------------------------------------------------------------------------
// JSON serialization helpers
// -------------------------------------------------------------------------

fn appendInt(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, n: i64) !void {
    var tmp: [20]u8 = undefined;
    const s = std.fmt.bufPrint(&tmp, "{d}", .{n}) catch return;
    try buf.appendSlice(allocator, s);
}

fn appendJsonEscaped(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
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
        }
    }
}

fn appendJsonValue(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, val: value.JSValue) !void {
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
    } else if (val.isFloat64()) {
        const f = val.getFloat64();
        if (std.math.isNan(f) or std.math.isInf(f)) {
            try buf.appendSlice(allocator, "null");
        } else {
            var tmp: [32]u8 = undefined;
            const s = std.fmt.bufPrint(&tmp, "{d}", .{f}) catch return;
            try buf.appendSlice(allocator, s);
        }
    } else if (val.isAnyString()) {
        try buf.append(allocator, '"');
        const str = util.extractString(val) orelse "";
        try appendJsonEscaped(buf, allocator, str);
        try buf.append(allocator, '"');
    } else {
        // Fallback for objects/arrays: "[object]"
        try buf.appendSlice(allocator, "\"[object]\"");
    }
}

const atomToString = util.atomToString;

fn writeStderr(data: []const u8) void {
    mb.writeStderrChecked(data);
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

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
