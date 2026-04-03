const std = @import("std");
const context = @import("zts/context.zig");
const interpreter_mod = @import("zts/interpreter.zig");

test "print offsets" {
    std.debug.print("\nContext.stack offset: {}\n", .{@offsetOf(context.Context, "stack")});
    std.debug.print("Context.sp offset: {}\n", .{@offsetOf(context.Context, "sp")});
    std.debug.print("Context.jit_interpreter offset: {}\n", .{@offsetOf(context.Context, "jit_interpreter")});
    std.debug.print("Interpreter.pic_cache offset: {}\n", .{@offsetOf(interpreter_mod.Interpreter, "pic_cache")});
    std.debug.print("Interpreter.pic_cache element size: {}\n", .{@sizeOf(interpreter_mod.Interpreter.PICCacheEntry)});
}
