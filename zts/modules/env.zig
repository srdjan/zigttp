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
const context = @import("../context.zig");
const value = @import("../value.zig");
const resolver = @import("resolver.zig");
const util = @import("util.zig");
const mb = @import("../module_binding.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:env",
    .name = "env",
    .contract_section = "env",
    .sandboxable = true,
    .exports = &.{
        .{ .name = "env", .func = envNative, .arg_count = 1,
           .returns = .optional_string, .param_types = &.{.string},
           .contract_extractions = &.{.{ .category = .env }} },
    },
};

pub const exports = binding.toModuleExports();

/// env(name) - read environment variable
fn envNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.undefined_val;

    // Get the variable name from the first argument
    const name_str = util.extractString(args[0]) orelse return value.JSValue.undefined_val;
    if (!ctx.capability_policy.allowsEnv(name_str)) {
        return util.throwCapabilityPolicyError(ctx, "env access", name_str);
    }

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

test "env policy rejects disallowed env access" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("../gc.zig");
    const heap_mod = @import("../heap.zig");

    const gc_state = try allocator.create(gc_mod.GC);
    gc_state.* = try gc_mod.GC.init(allocator, .{});
    const heap_state = try allocator.create(heap_mod.Heap);
    heap_state.* = heap_mod.Heap.init(allocator, .{});
    gc_state.setHeap(heap_state);
    const ctx = try context.Context.init(allocator, gc_state, .{});
    defer {
        ctx.deinit();
        heap_state.deinit();
        gc_state.deinit();
        allocator.destroy(heap_state);
        allocator.destroy(gc_state);
    }

    ctx.capability_policy = .{
        .env = .{
            .enabled = true,
            .values = &[_][]const u8{"ALLOWED"},
        },
    };

    const key = try ctx.createString("BLOCKED");
    _ = try envNative(ctx, value.JSValue.undefined_val, &[_]value.JSValue{key});

    try std.testing.expect(ctx.hasException());
}
