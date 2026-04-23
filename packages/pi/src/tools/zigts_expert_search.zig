//! Both this tool and `zigts search --json` go through
//! `describe_rule.writeRuleJson`, so the TUI and CLI stay byte-identical.

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;
const describe_rule = @import("zigts_cli").describe_rule;
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_search";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "search rules",
    .description = "Search diagnostic rules by keyword substring across name, description, and help.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"query\":{\"type\":\"string\",\"description\":\"Keyword substring to search for.\"}},\"required\":[\"query\"]}",
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    return registry_mod.helpers.decodeSingleStringField(allocator, args_json, "query");
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_search requires a keyword argument\n");
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    const results = rule_registry.search(args[0]);

    try w.writeAll("[");
    for (results.constSlice(), 0..) |idx, i| {
        if (i > 0) try w.writeAll(",");
        try describe_rule.writeRuleJson(w, &rule_registry.all_rules[idx]);
    }
    try w.writeAll("]\n");

    buf = aw.toArrayList();
    return .{ .ok = true, .llm_text = try buf.toOwnedSlice(allocator) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "missing keyword returns not-ok body" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "requires a keyword") != null);
}

test "search emits JSON array and finds at least one match for a common term" {
    var result = try execute(testing.allocator, &.{"result"});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expectEqual(@as(u8, '['), result.llm_text[0]);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"code\":") != null);
}

test "search with no matches emits empty array" {
    var result = try execute(testing.allocator, &.{"zzzz-definitely-no-such-rule-zzzz"});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expectEqualStrings("[]\n", result.llm_text);
}
