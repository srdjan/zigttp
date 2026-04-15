//! Serializes the in-process `Registry` to the Anthropic Messages API
//! tool-use JSON shape. Every tool takes a single free-form string
//! argument today (the dispatch contract for `Registry.invoke` packs args
//! as `[]const []const u8` with a `.string` payload), so every tool
//! declares the same minimal `input_schema`. Richer per-tool schemas can
//! land alongside the individual tool modules when the loop needs them.

const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const json_writer = @import("json_writer.zig");

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
    var first = true;
    for (registry.list()) |entry| {
        if (!first) try writer.writeByte(',');
        first = false;
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

test "writeToolsArray: empty registry emits an empty array" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);

    const out = try serialize(&reg);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("[]", out);
}

test "writeToolsArray: single tool produces parseable JSON with name, description, and schema" {
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
    try testing.expectEqual(@as(usize, 1), parsed.value.array.items.len);

    const first = parsed.value.array.items[0];
    try testing.expect(first == .object);
    const name = first.object.get("name").?;
    try testing.expectEqualStrings("zigts_expert_meta", name.string);
    const desc = first.object.get("description").?;
    try testing.expectEqualStrings("Emit policy metadata.", desc.string);
    const schema = first.object.get("input_schema").?;
    try testing.expect(schema == .object);
    try testing.expectEqualStrings("object", schema.object.get("type").?.string);
}

test "writeToolsArray: multiple tools separated by commas, order preserved" {
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

    try testing.expectEqual(@as(usize, 2), parsed.value.array.items.len);
    try testing.expectEqualStrings("alpha", parsed.value.array.items[0].object.get("name").?.string);
    try testing.expectEqualStrings("beta", parsed.value.array.items[1].object.get("name").?.string);
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
    try testing.expectEqualStrings(
        "has \"quotes\" and \\ backslash",
        parsed.value.array.items[0].object.get("description").?.string,
    );
}
