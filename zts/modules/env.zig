//! zigttp:env - Environment variable access
//!
//! Exports:
//!   env(name: string) -> string | undefined
//!     Reads an environment variable at call time.
//!     Returns undefined if not set.
//!
//! Works with comptime() for build-time evaluation:
//!   const x = comptime(env("APP_NAME"));  // inlined as string literal

const std = @import("std");
const object = @import("../object.zig");
const context = @import("../context.zig");
const value = @import("../value.zig");
const string = @import("../string.zig");
const resolver = @import("resolver.zig");

/// Module exports
pub const exports = [_]resolver.ModuleExport{
    .{ .name = "env", .func = envNative, .arg_count = 1 },
};

/// env(name) - read environment variable
fn envNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return value.JSValue.undefined_val;

    // Get the variable name from the first argument
    const name_str = extractString(args[0]) orelse return value.JSValue.undefined_val;

    // Need a null-terminated copy for the C getenv API
    var name_buf: [256]u8 = undefined;
    if (name_str.len >= name_buf.len) return value.JSValue.undefined_val;
    @memcpy(name_buf[0..name_str.len], name_str);
    name_buf[name_str.len] = 0;

    const c_result = std.c.getenv(name_buf[0..name_str.len :0]);
    if (c_result == null) return value.JSValue.undefined_val;

    const result = std.mem.sliceTo(c_result.?, 0);
    return ctx.createString(result) catch value.JSValue.undefined_val;
}

/// Extract string data from a JSValue (handles flat strings, ropes, and slices)
fn extractString(val: value.JSValue) ?[]const u8 {
    if (val.isString()) {
        return val.toPtr(string.JSString).data();
    }
    if (val.isStringSlice()) {
        const slice = val.toPtr(string.SliceString);
        return slice.data();
    }
    return null;
}
