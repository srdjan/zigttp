//! Serializes the in-process `Registry` to the Anthropic Messages API
//! tool-use JSON shape. Every tool takes a single free-form string
//! argument today (the dispatch contract for `Registry.invoke` packs args
//! as `[]const []const u8` with a `.string` payload), so every tool
//! declares the same minimal `input_schema`. Richer per-tool schemas can
//! land alongside the individual tool modules when the loop needs them.

const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const json_writer = @import("json_writer.zig");
const apply_edit = @import("apply_edit.zig");

const input_schema_literal =
    "{\"type\":\"object\"," ++
    "\"properties\":{\"input\":{\"type\":\"string\"," ++
    "\"description\":\"Raw argument string passed to the tool.\"}}," ++
    "\"required\":[]}";

pub fn writeToolsArray(
    writer: anytype,
    registry: *const registry_mod.Registry,
) !void {
    try writer.writeByte('[');

    // `apply_edit` is a synthetic tool: it appears in the tools array so
    // the model knows how to propose an edit, but it is *not* a Registry
    // entry and never reaches `Registry.invoke`. The assembler +
    // anthropic client remap the matching tool_call into a first-class
    // `turn.ModelReply.edit` before the loop hands it to the state
    // machine. Its input_schema is richer than the minimal registry-tool
    // schema because the model needs to know the exact field names.
    try writer.writeAll("{\"name\":");
    try json_writer.writeString(writer, apply_edit.tool_name);
    try writer.writeAll(",\"description\":");
    try json_writer.writeString(writer, apply_edit.tool_description);
    try writer.writeAll(",\"input_schema\":");
    try writer.writeAll(apply_edit.input_schema_literal);
    try writer.writeByte('}');

    for (registry.list()) |entry| {
        try writer.writeByte(',');
        try writer.writeAll("{\"name\":");
        try json_writer.writeString(writer, entry.name);
        try writer.writeAll(",\"description\":");
        try json_writer.writeString(writer, entry.description);
        try writer.writeAll(",\"input_schema\":");
        try writer.writeAll(input_schema_literal);
        try writer.writeByte('}');
    }
    try writer.writeByte(']');
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;
const ToolDef = registry_mod.ToolDef;

fn unusedExecute(
    _: std.mem.Allocator,
    _: []const []const u8,
) anyerror!registry_mod.ToolResult {
    return error.TestUnexpectedCall;
}

fn serialize(registry: *const registry_mod.Registry) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeToolsArray(&aw.writer, registry);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(testing.allocator);
}

fn findTool(array: std.json.Value, name: []const u8) ?std.json.Value {
    if (array != .array) return null;
    for (array.array.items) |item| {
        if (item != .object) continue;
        const n = item.object.get("name") orelse continue;
        if (n != .string) continue;
        if (std.mem.eql(u8, n.string, name)) return item;
    }
    return null;
}

test "writeToolsArray: empty registry still emits the synthetic apply_edit tool" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);

    const out = try serialize(&reg);
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    try testing.expect(parsed.value == .array);
    try testing.expectEqual(@as(usize, 1), parsed.value.array.items.len);
    const synthetic = findTool(parsed.value, apply_edit.tool_name) orelse return error.TestFailed;
    try testing.expect(synthetic.object.get("input_schema").? == .object);
    // The synthetic schema declares file + content as required properties.
    const schema = synthetic.object.get("input_schema").?.object;
    const required = schema.get("required").?;
    try testing.expect(required == .array);
    try testing.expectEqual(@as(usize, 2), required.array.items.len);
}

test "writeToolsArray: registered tool appears alongside apply_edit, found by name" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);
    try reg.register(testing.allocator, .{
        .name = "zigts_expert_meta",
        .label = "meta",
        .description = "Emit policy metadata.",
        .execute = unusedExecute,
    });

    const out = try serialize(&reg);
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();

    try testing.expect(parsed.value == .array);
    try testing.expectEqual(@as(usize, 2), parsed.value.array.items.len);

    const meta = findTool(parsed.value, "zigts_expert_meta") orelse return error.TestFailed;
    try testing.expectEqualStrings("Emit policy metadata.", meta.object.get("description").?.string);

    // apply_edit is still present alongside registered tools.
    _ = findTool(parsed.value, apply_edit.tool_name) orelse return error.TestFailed;
}

test "writeToolsArray: multiple registered tools preserve insertion order" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);
    try reg.register(testing.allocator, .{
        .name = "alpha",
        .label = "a",
        .description = "first",
        .execute = unusedExecute,
    });
    try reg.register(testing.allocator, .{
        .name = "beta",
        .label = "b",
        .description = "second",
        .execute = unusedExecute,
    });

    const out = try serialize(&reg);
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();

    try testing.expectEqual(@as(usize, 3), parsed.value.array.items.len);
    // apply_edit is first, then alpha, then beta.
    try testing.expectEqualStrings(apply_edit.tool_name, parsed.value.array.items[0].object.get("name").?.string);
    try testing.expectEqualStrings("alpha", parsed.value.array.items[1].object.get("name").?.string);
    try testing.expectEqualStrings("beta", parsed.value.array.items[2].object.get("name").?.string);
}

test "writeToolsArray: description with quotes and backslashes is escaped" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);
    try reg.register(testing.allocator, .{
        .name = "tricky",
        .label = "tricky",
        .description = "has \"quotes\" and \\ backslash",
        .execute = unusedExecute,
    });

    const out = try serialize(&reg);
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    const tricky = findTool(parsed.value, "tricky") orelse return error.TestFailed;
    try testing.expectEqualStrings(
        "has \"quotes\" and \\ backslash",
        tricky.object.get("description").?.string,
    );
}
