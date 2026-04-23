const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const json_writer = @import("../providers/anthropic/json_writer.zig");

const name = "workspace_list_files";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "list files",
    .description = "List workspace files relative to the repo root.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"limit\":{\"type\":\"integer\",\"minimum\":1}},\"required\":[]}",
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    var path: []const u8 = ".";
    var limit: usize = 200;

    if (args.len > 0 and args[0].len > 0 and args[0][0] == '{') {
        var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
            return registry_mod.ToolResult.err(allocator, name ++ ": invalid JSON input\n");
        };
        defer parsed.deinit();
        if (parsed.value != .object) return registry_mod.ToolResult.err(allocator, name ++ ": expected JSON object\n");
        const obj = parsed.value.object;
        if (obj.get("path")) |value| {
            if (value != .string) return registry_mod.ToolResult.err(allocator, name ++ ": path must be a string\n");
            path = value.string;
        }
        if (obj.get("limit")) |value| {
            if (value != .integer or value.integer <= 0) return registry_mod.ToolResult.err(allocator, name ++ ": limit must be a positive integer\n");
            limit = @intCast(value.integer);
        }
    } else if (args.len > 0) {
        path = args[0];
    }

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, path);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    const argv: []const []const u8 = if (std.mem.eql(u8, relative, "."))
        &.{ "rg", "--files", "--hidden", "-g", "!.git", "-g", "!zig-out", "-g", "!.zig-cache", "-g", "!node_modules" }
    else
        &.{ "rg", "--files", "--hidden", "-g", "!.git", "-g", "!zig-out", "-g", "!.zig-cache", "-g", "!node_modules", relative };

    var outcome = try common.runCommand(allocator, root, argv);
    defer outcome.deinit(allocator);

    var files = std.ArrayList([]const u8).empty;
    defer files.deinit(allocator);
    var truncated = false;
    var lines = std.mem.splitScalar(u8, outcome.stdout, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (files.items.len >= limit) {
            truncated = true;
            break;
        }
        try files.append(allocator, line);
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("{\"ok\":");
    try w.writeAll(if (outcome.ok) "true" else "false");
    try w.writeAll(",\"path\":");
    try json_writer.writeString(w, relative);
    try w.writeAll(",\"truncated\":");
    try w.writeAll(if (truncated) "true" else "false");
    try w.writeAll(",\"files\":[");
    for (files.items, 0..) |file, i| {
        if (i > 0) try w.writeByte(',');
        try json_writer.writeString(w, file);
    }
    try w.writeAll("],\"stderr\":");
    try json_writer.writeString(w, outcome.stderr);
    try w.writeAll("}\n");

    buf = aw.toArrayList();
    return .{ .ok = outcome.ok, .llm_text = try buf.toOwnedSlice(allocator) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "workspace_list_files: malformed JSON args returns structured error" {
    var result = try execute(testing.allocator, &.{"{not json"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "invalid JSON input") != null);
}

test "workspace_list_files: non-string path returns structured error" {
    var result = try execute(testing.allocator, &.{"{\"path\":123}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "path must be a string") != null);
}

test "workspace_list_files: limit must be a positive integer" {
    var result = try execute(testing.allocator, &.{"{\"limit\":0}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "limit must be a positive integer") != null);
}

test "workspace_list_files: ../ escape is rejected by resolveInsideWorkspace" {
    try testing.expectError(
        error.PathOutsideWorkspace,
        execute(testing.allocator, &.{"../../etc"}),
    );
}
