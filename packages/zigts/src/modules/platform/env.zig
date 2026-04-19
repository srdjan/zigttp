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
const context = @import("../../context.zig");
const object = @import("../../object.zig");
const string = @import("../../string.zig");
const value = @import("../../value.zig");
const resolver = @import("../resolver.zig");
const util = @import("../util.zig");
const mb = @import("../../module_binding.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:env",
    .name = "env",
    .required_capabilities = &.{ .env, .policy_check },
    .contract_section = "env",
    .sandboxable = true,
    .exports = &.{
        .{
            .name = "env",
            .func = envNative,
            .arg_count = 1,
            .returns = .optional_string,
            .param_types = &.{.string},
            .failure_severity = .expected,
            .contract_extractions = &.{.{ .category = .env }},
            .return_labels = .{ .secret = true },
            // env is immutable within a request lifetime; two reads of the
            // same key within a single handler invocation always return the
            // same value, so the canonicalizer may collapse repeated calls.
            .laws = &.{.pure},
        },
    },
};

pub const exports = binding.toModuleExports();

/// env(name) - read environment variable
fn envNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.undefined_val;

    // Get the variable name from the first argument
    const name_str = util.extractString(args[0]) orelse return value.JSValue.undefined_val;
    if (!mb.allowsEnvChecked(ctx, name_str)) {
        return util.throwCapabilityPolicyError(ctx, "env access", name_str);
    }

    // Need a null-terminated copy for the C getenv API
    var name_buf: [256]u8 = undefined;
    if (name_str.len >= name_buf.len) return value.JSValue.undefined_val;
    @memcpy(name_buf[0..name_str.len], name_str);
    name_buf[name_str.len] = 0;

    const result = mb.readEnvChecked(name_buf[0..name_str.len :0]) orelse return value.JSValue.undefined_val;
    return ctx.createString(result) catch value.JSValue.undefined_val;
}

fn pushEnvTestContext() mb.ActiveModuleToken {
    return mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
}

test "env policy rejects disallowed env access" {
    const token = pushEnvTestContext();
    defer mb.popActiveModuleContext(token);

    const allocator = std.testing.allocator;
    const gc_mod = @import("../../gc.zig");
    const heap_mod = @import("../../heap.zig");

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
    defer string.freeString(allocator, key.toPtr(string.JSString));
    _ = try envNative(ctx, value.JSValue.undefined_val, &[_]value.JSValue{key});

    try std.testing.expect(ctx.hasException());
    if (ctx.exception.isObject()) {
        const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
        ctx.exception.toPtr(object.JSObject).destroyBuiltin(allocator, pool);
        ctx.clearException();
    }
}
