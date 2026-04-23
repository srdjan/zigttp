const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const json_writer = @import("../providers/anthropic/json_writer.zig");

const name = "workspace_read_file";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "read file",
    .description = "Read a workspace file, optionally clipped to a line range.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"start_line\":{\"type\":\"integer\",\"minimum\":1},\"end_line\":{\"type\":\"integer\",\"minimum\":1}},\"required\":[\"path\"]}",
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    var path_opt: ?[]const u8 = null;
    var start_line: usize = 1;
    var end_line: ?usize = null;

    if (args.len > 0 and args[0].len > 0 and args[0][0] == '{') {
        var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
            return registry_mod.ToolResult.err(allocator, name ++ ": invalid JSON input\n");
        };
        defer parsed.deinit();
        if (parsed.value != .object) return registry_mod.ToolResult.err(allocator, name ++ ": expected JSON object\n");
        const obj = parsed.value.object;
        const path_val = obj.get("path") orelse return registry_mod.ToolResult.err(allocator, name ++ ": missing path\n");
        if (path_val != .string) return registry_mod.ToolResult.err(allocator, name ++ ": path must be a string\n");
        path_opt = path_val.string;
        if (obj.get("start_line")) |value| {
            if (value != .integer or value.integer <= 0) return registry_mod.ToolResult.err(allocator, name ++ ": start_line must be a positive integer\n");
            start_line = @intCast(value.integer);
        }
        if (obj.get("end_line")) |value| {
            if (value != .integer or value.integer <= 0) return registry_mod.ToolResult.err(allocator, name ++ ": end_line must be a positive integer\n");
            end_line = @intCast(value.integer);
        }
    } else if (args.len > 0) {
        path_opt = args[0];
    } else {
        return registry_mod.ToolResult.err(allocator, name ++ ": missing path\n");
    }

    const path = path_opt.?;
    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, path);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    const content = try zigts.file_io.readFile(allocator, relative, common.default_output_limit);
    defer allocator.free(content);

    var line_no: usize = 1;
    var selected = std.ArrayList(u8).empty;
    defer selected.deinit(allocator);
    var wrote_any = false;
    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| : (line_no += 1) {
        if (line_no < start_line) continue;
        if (end_line) |last| {
            if (line_no > last) break;
        }
        if (wrote_any) try selected.append(allocator, '\n');
        try selected.appendSlice(allocator, line);
        wrote_any = true;
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("{\"ok\":true,\"path\":");
    try json_writer.writeString(w, relative);
    try w.writeAll(",\"start_line\":");
    try w.print("{d}", .{start_line});
    try w.writeAll(",\"end_line\":");
    if (end_line) |last| {
        try w.print("{d}", .{last});
    } else {
        try w.writeAll("null");
    }
    try w.writeAll(",\"content\":");
    try json_writer.writeString(w, selected.items);
    try w.writeAll("}\n");

    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "workspace_read_file: missing path arg returns structured error" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "missing path") != null);
}

test "workspace_read_file: malformed JSON args returns structured error" {
    var result = try execute(testing.allocator, &.{"{not json"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "invalid JSON input") != null);
}

test "workspace_read_file: JSON args without path returns structured error" {
    var result = try execute(testing.allocator, &.{"{\"start_line\":1}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "missing path") != null);
}

test "workspace_read_file: negative start_line returns structured error" {
    var result = try execute(testing.allocator, &.{"{\"path\":\"x\",\"start_line\":-1}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "start_line must be a positive integer") != null);
}

test "workspace_read_file: ../ escape is rejected by resolveInsideWorkspace" {
    try testing.expectError(
        error.PathOutsideWorkspace,
        execute(testing.allocator, &.{"../../etc/passwd"}),
    );
}
