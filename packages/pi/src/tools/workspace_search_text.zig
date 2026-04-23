const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const json_writer = @import("../providers/anthropic/json_writer.zig");

const name = "workspace_search_text";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "search text",
    .description = "Search the workspace for a text substring and return path/line matches.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"query\":{\"type\":\"string\"},\"path\":{\"type\":\"string\"},\"limit\":{\"type\":\"integer\",\"minimum\":1}},\"required\":[\"query\"]}",
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    var query_opt: ?[]const u8 = null;
    var path: []const u8 = ".";
    var limit: usize = 50;

    if (args.len > 0 and args[0].len > 0 and args[0][0] == '{') {
        var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
            return registry_mod.ToolResult.err(allocator, name ++ ": invalid JSON input\n");
        };
        defer parsed.deinit();
        if (parsed.value != .object) return registry_mod.ToolResult.err(allocator, name ++ ": expected JSON object\n");
        const obj = parsed.value.object;
        const query_val = obj.get("query") orelse return registry_mod.ToolResult.err(allocator, name ++ ": missing query\n");
        if (query_val != .string) return registry_mod.ToolResult.err(allocator, name ++ ": query must be a string\n");
        query_opt = query_val.string;
        if (obj.get("path")) |value| {
            if (value != .string) return registry_mod.ToolResult.err(allocator, name ++ ": path must be a string\n");
            path = value.string;
        }
        if (obj.get("limit")) |value| {
            if (value != .integer or value.integer <= 0) return registry_mod.ToolResult.err(allocator, name ++ ": limit must be a positive integer\n");
            limit = @intCast(value.integer);
        }
    } else if (args.len > 0) {
        query_opt = args[0];
        if (args.len > 1) path = args[1];
    } else {
        return registry_mod.ToolResult.err(allocator, name ++ ": missing query\n");
    }

    const query = query_opt.?;
    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, path);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    const argv: []const []const u8 = if (std.mem.eql(u8, relative, "."))
        &.{ "rg", "-n", "--no-heading", "--color", "never", "--hidden", "-g", "!.git", "-g", "!zig-out", "-g", "!.zig-cache", "-g", "!node_modules", query }
    else
        &.{ "rg", "-n", "--no-heading", "--color", "never", "--hidden", "-g", "!.git", "-g", "!zig-out", "-g", "!.zig-cache", "-g", "!node_modules", query, relative };

    var outcome = try common.runCommand(allocator, root, argv);
    defer outcome.deinit(allocator);

    const no_matches = outcome.exit_code != null and outcome.exit_code.? == 1 and outcome.stderr.len == 0;
    const semantic_ok = outcome.ok or no_matches;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"ok\":");
    try w.writeAll(if (semantic_ok) "true" else "false");
    try w.writeAll(",\"query\":");
    try json_writer.writeString(w, query);
    try w.writeAll(",\"matches\":[");

    var count: usize = 0;
    var truncated = false;
    var lines = std.mem.splitScalar(u8, outcome.stdout, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (count >= limit) {
            truncated = true;
            break;
        }
        var parts = std.mem.splitScalar(u8, line, ':');
        const file = parts.next() orelse continue;
        const line_str = parts.next() orelse continue;
        const text = parts.rest();
        if (count > 0) try w.writeByte(',');
        try w.writeAll("{\"path\":");
        try json_writer.writeString(w, file);
        try w.writeAll(",\"line\":");
        try w.print("{d}", .{std.fmt.parseInt(usize, line_str, 10) catch 0});
        try w.writeAll(",\"text\":");
        try json_writer.writeString(w, text);
        try w.writeByte('}');
        count += 1;
    }

    try w.writeAll("],\"truncated\":");
    try w.writeAll(if (truncated) "true" else "false");
    try w.writeAll(",\"stderr\":");
    try json_writer.writeString(w, outcome.stderr);
    try w.writeAll("}\n");

    buf = aw.toArrayList();
    return .{ .ok = semantic_ok, .llm_text = try buf.toOwnedSlice(allocator) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "workspace_search_text: missing query arg returns structured error" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "missing query") != null);
}

test "workspace_search_text: JSON args without query returns structured error" {
    var result = try execute(testing.allocator, &.{"{\"path\":\".\"}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "missing query") != null);
}

test "workspace_search_text: non-string query returns structured error" {
    var result = try execute(testing.allocator, &.{"{\"query\":42}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "query must be a string") != null);
}

test "workspace_search_text: malformed JSON returns structured error" {
    var result = try execute(testing.allocator, &.{"{not"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "invalid JSON input") != null);
}

test "workspace_search_text: ../ escape is rejected by resolveInsideWorkspace" {
    try testing.expectError(
        error.PathOutsideWorkspace,
        execute(testing.allocator, &.{ "needle", "../../etc" }),
    );
}
