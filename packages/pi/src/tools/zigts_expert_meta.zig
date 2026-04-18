//! Both this tool and `zigts meta --json` go through
//! `expert_meta.writeJson`, so the TUI and CLI stay byte-identical.

const std = @import("std");
const expert_meta = @import("zigts_cli").expert_meta;
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_meta";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "policy meta",
    .description = "Show compiler version, policy version, policy hash, and rule counts. Takes no arguments.",
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len != 0) return registry_mod.ToolResult.err(allocator, name ++ ": v1 takes no arguments\n");

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    const info = expert_meta.compute();
    try expert_meta.writeJson(&aw.writer, &info);

    buf = aw.toArrayList();
    return .{
        .ok = true,
        .body = try buf.toOwnedSlice(allocator),
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "execute returns v1 meta envelope" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"compiler_version\":\"0.16.0\"") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"policy_version\":\"2026.04.2\"") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"mode\":\"embedded\"") != null);
}

test "execute rejects unexpected arguments" {
    var result = try execute(testing.allocator, &.{"unexpected"});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "takes no arguments") != null);
}

test "registry invokes the tool end-to-end" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);

    try reg.register(testing.allocator, tool);

    var result = try reg.invoke(testing.allocator, name, &.{});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"rule_count\":") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"categories\":{\"verifier\":") != null);
}
