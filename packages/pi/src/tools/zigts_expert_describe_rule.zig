//! Both this tool and `zigts expert describe-rule --json` go through
//! `describe_rule.writeRuleJson`, so the TUI and CLI stay byte-identical.

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;
const describe_rule = @import("zigts_cli").describe_rule;
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_describe_rule";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "describe rule",
    .description = "Describe a rule by name or code, or list all rules when called with no args.",
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    if (args.len == 0) {
        try w.writeAll("[");
        for (&rule_registry.all_rules, 0..) |*entry, i| {
            if (i > 0) try w.writeAll(",");
            try describe_rule.writeRuleJson(w, entry);
        }
        try w.writeAll("]\n");

        buf = aw.toArrayList();
        return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
    }

    const query = args[0];
    const entry = rule_registry.findByName(query) orelse
        rule_registry.findByCode(query) orelse
    {
        try w.print("Unknown rule: {s}\n", .{query});
        buf = aw.toArrayList();
        return .{ .ok = false, .body = try buf.toOwnedSlice(allocator) };
    };

    try describe_rule.writeRuleJson(w, entry);
    try w.writeAll("\n");

    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "list mode emits JSON array of rules" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expect(result.body.len > 2);
    try testing.expectEqual(@as(u8, '['), result.body[0]);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"code\":") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"category\":") != null);
}

test "lookup by code returns single rule object" {
    var result = try execute(testing.allocator, &.{"ZTS303"});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expectEqual(@as(u8, '{'), result.body[0]);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"code\":\"ZTS303\"") != null);
}

test "unknown query returns not-ok body" {
    var result = try execute(testing.allocator, &.{"not-a-real-rule"});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "Unknown rule") != null);
}
