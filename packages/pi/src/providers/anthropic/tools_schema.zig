//! Serializes the structured tool catalog the model sees into the Anthropic
//! Messages API tool-use JSON shape. The synthetic `apply_edit` tool is
//! prepended with its own schema and intercepted before it reaches the
//! registry.

const std = @import("std");
const registry_mod = @import("../../registry/registry.zig");
const json_writer = @import("json_writer.zig");
const apply_edit = @import("apply_edit.zig");

fn writeToolEntry(
    writer: anytype,
    tool: ToolDef,
    is_last: bool,
) !void {
    try writer.writeAll("{\"name\":");
    try json_writer.writeString(writer, tool.name);
    try writer.writeAll(",\"description\":");
    try json_writer.writeString(writer, tool.description);
    try writer.writeAll(",\"input_schema\":");
    try writer.writeAll(tool.input_schema);
    // The tools array is large and identical on every round-trip. A
    // cache_control breakpoint on the last tool tells the API to cache the
    // whole tools block, so later turns read it at 0.1x input cost instead of
    // re-billing the full schema each time.
    if (is_last) {
        try writer.writeAll(",\"cache_control\":{\"type\":\"ephemeral\"}");
    }
    try writer.writeByte('}');
}

pub fn writeToolsArray(
    writer: anytype,
    registry: *const registry_mod.Registry,
) !void {
    const entries = registry.list();
    var model_tool_count: usize = 0;
    for (entries) |entry| {
        if (entry.allowedOn(.model)) model_tool_count += 1;
    }
    try writer.writeByte('[');
    // apply_edit is the synthetic first tool; it is last only when no other
    // tools are registered.
    try writeToolEntry(writer, .{
        .name = apply_edit.tool_name,
        .label = "apply edit",
        .description = apply_edit.tool_description,
        .effect = .write_workspace,
        .input_schema = apply_edit.input_schema_literal,
        .decode_json = registry_mod.helpers.decodeJsonPassthrough,
        .execute = unusedExecute,
    }, model_tool_count == 0);
    var emitted: usize = 0;
    for (entries) |entry| {
        if (!entry.allowedOn(.model)) continue;
        try writer.writeByte(',');
        emitted += 1;
        try writeToolEntry(writer, entry, emitted == model_tool_count);
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

/// Test-only: find a tool entry in a parsed JSON array by its name field.
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

test "writeToolsArray: model catalog omits generic workspace writers" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);
    try reg.register(testing.allocator, .{
        .name = "writer",
        .label = "writer",
        .description = "test writer",
        .effect = .write_workspace,
        .input_schema = "{\"type\":\"object\",\"properties\":{},\"required\":[]}",
        .decode_json = registry_mod.helpers.decodeNoArgs,
        .execute = unusedExecute,
    });

    const out = try serialize(&reg);
    defer testing.allocator.free(out);
    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();

    try testing.expectEqual(@as(usize, 1), parsed.value.array.items.len);
    try testing.expect(findTool(parsed.value, "writer") == null);
    _ = findTool(parsed.value, apply_edit.tool_name) orelse return error.TestFailed;
}

test "writeToolsArray: registered tool appears alongside apply_edit, found by name" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);
    try reg.register(testing.allocator, .{
        .name = "zigts_expert_meta",
        .label = "meta",
        .effect = .analyze,
        .description = "Emit policy metadata.",
        .input_schema = "{\"type\":\"object\",\"properties\":{},\"required\":[]}",
        .decode_json = registry_mod.helpers.decodeNoArgs,
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

test "writeToolsArray: only the last tool carries a cache_control breakpoint" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);
    try reg.register(testing.allocator, .{
        .name = "alpha",
        .label = "a",
        .effect = .analyze,
        .description = "first",
        .input_schema = "{\"type\":\"object\",\"properties\":{},\"required\":[]}",
        .decode_json = registry_mod.helpers.decodeNoArgs,
        .execute = unusedExecute,
    });
    try reg.register(testing.allocator, .{
        .name = "beta",
        .label = "b",
        .effect = .analyze,
        .description = "second",
        .input_schema = "{\"type\":\"object\",\"properties\":{},\"required\":[]}",
        .decode_json = registry_mod.helpers.decodeNoArgs,
        .execute = unusedExecute,
    });

    const out = try serialize(&reg);
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    const items = parsed.value.array.items;
    // apply_edit, alpha, beta: only beta (the last) is cached.
    try testing.expect(items[0].object.get("cache_control") == null);
    try testing.expect(items[1].object.get("cache_control") == null);
    const cc = items[2].object.get("cache_control") orelse return error.TestFailed;
    try testing.expectEqualStrings("ephemeral", cc.object.get("type").?.string);
}

test "writeToolsArray: apply_edit alone carries the cache_control breakpoint" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);

    const out = try serialize(&reg);
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    const items = parsed.value.array.items;
    try testing.expectEqual(@as(usize, 1), items.len);
    const cc = items[0].object.get("cache_control") orelse return error.TestFailed;
    try testing.expectEqualStrings("ephemeral", cc.object.get("type").?.string);
}

test "writeToolsArray: multiple registered tools preserve insertion order" {
    var reg: registry_mod.Registry = .{};
    defer reg.deinit(testing.allocator);
    try reg.register(testing.allocator, .{
        .name = "alpha",
        .label = "a",
        .effect = .analyze,
        .description = "first",
        .input_schema = "{\"type\":\"object\",\"properties\":{},\"required\":[]}",
        .decode_json = registry_mod.helpers.decodeNoArgs,
        .execute = unusedExecute,
    });
    try reg.register(testing.allocator, .{
        .name = "beta",
        .label = "b",
        .effect = .analyze,
        .description = "second",
        .input_schema = "{\"type\":\"object\",\"properties\":{},\"required\":[]}",
        .decode_json = registry_mod.helpers.decodeNoArgs,
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
        .effect = .analyze,
        .description = "has \"quotes\" and \\ backslash",
        .input_schema = "{\"type\":\"object\",\"properties\":{\"query\":{\"type\":\"string\"}},\"required\":[\"query\"]}",
        .decode_json = registry_mod.helpers.decodeJsonPassthrough,
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
    const schema = tricky.object.get("input_schema").?.object;
    try testing.expectEqualStrings("string", schema.get("properties").?.object.get("query").?.object.get("type").?.string);
}
