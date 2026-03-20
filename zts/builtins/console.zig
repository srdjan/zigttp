const std = @import("std");
const h = @import("helpers.zig");
const value = h.value;
const context = h.context;

const getStringData = h.getStringData;

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
    } else if (val.isFloat()) {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getFloat64()}) catch return;
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
