//! Console native callbacks extracted from zruntime.zig.
//!
//! `consoleLog` and `consoleError` are registered as the JS `console.*`
//! methods. They write directly to the process stdio file descriptors and
//! depend only on the engine value types, never on Runtime state, so they live
//! here as plain free functions with no back-import of `Runtime`.

const std = @import("std");
const zq = @import("zts");

pub fn consoleLog(_: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    for (args, 0..) |arg, i| {
        if (i > 0) writeToFd(std.c.STDOUT_FILENO, " ");
        printValue(arg, std.c.STDOUT_FILENO);
    }
    writeToFd(std.c.STDOUT_FILENO, "\n");
    return zq.JSValue.undefined_val;
}

pub fn consoleError(_: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    writeToFd(std.c.STDERR_FILENO, "\x1b[31m[ERROR]\x1b[0m ");
    for (args, 0..) |arg, i| {
        if (i > 0) writeToFd(std.c.STDERR_FILENO, " ");
        printValue(arg, std.c.STDERR_FILENO);
    }
    writeToFd(std.c.STDERR_FILENO, "\n");
    return zq.JSValue.undefined_val;
}

fn writeToFd(fd: std.c.fd_t, data: []const u8) void {
    _ = std.c.write(fd, data.ptr, data.len);
}

fn printValue(val: zq.JSValue, fd: std.c.fd_t) void {
    if (val.isUndefined()) {
        writeToFd(fd, "undefined");
    } else if (val.isNull()) {
        writeToFd(fd, "null");
    } else if (val.isTrue()) {
        writeToFd(fd, "true");
    } else if (val.isFalse()) {
        writeToFd(fd, "false");
    } else if (val.isInt()) {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return;
        writeToFd(fd, s);
    } else if (val.isFloat()) {
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getFloat64()}) catch return;
        writeToFd(fd, s);
    } else if (val.isString()) {
        const str = val.toPtr(zq.JSString);
        writeToFd(fd, str.data());
    } else if (val.isObject()) {
        writeToFd(fd, "[Object]");
    } else {
        writeToFd(fd, "[unknown]");
    }
}
