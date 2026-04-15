//! Builds the Anthropic Messages API streaming request body. The system
//! prompt lives in a single-element `system` array tagged with an
//! ephemeral `cache_control` block so the ~60 KiB zigts-expert persona
//! amortizes across every turn in a session via Anthropic prompt caching.
//!
//! Prompt caching requires the cached block to be at least ~1024 tokens;
//! the persona bundle is measured at ~60 KiB (well above) so this is
//! always safe to set. Caller is responsible for picking a model string
//! that supports caching.

const std = @import("std");
const json_writer = @import("json_writer.zig");

pub const default_model = "claude-opus-4-6";
pub const default_max_tokens: u32 = 8192;

pub const RequestParams = struct {
    model: []const u8 = default_model,
    max_tokens: u32 = default_max_tokens,
    system_prompt: []const u8,
    user_text: []const u8,
    /// Pre-serialized tools array (as emitted by `tools_schema.writeToolsArray`).
    /// Null omits the `tools` field entirely.
    tools_json: ?[]const u8 = null,
    stream: bool = true,
};

pub fn writeRequestBody(
    writer: anytype,
    params: RequestParams,
) !void {
    try writer.writeByte('{');

    try writer.writeAll("\"model\":");
    try json_writer.writeString(writer, params.model);

    try writer.print(",\"max_tokens\":{d}", .{params.max_tokens});

    try writer.writeAll(",\"stream\":");
    try writer.writeAll(if (params.stream) "true" else "false");

    // Cached system block. The cache_control marker is the difference
    // between "this is a system prompt" and "this is a *reusable* system
    // prompt"; without it every turn pays the full input-token cost.
    try writer.writeAll(",\"system\":[{\"type\":\"text\",\"text\":");
    try json_writer.writeString(writer, params.system_prompt);
    try writer.writeAll(",\"cache_control\":{\"type\":\"ephemeral\"}}]");

    try writer.writeAll(",\"messages\":[{\"role\":\"user\",\"content\":");
    try json_writer.writeString(writer, params.user_text);
    try writer.writeAll("}]");

    if (params.tools_json) |tools| {
        try writer.writeAll(",\"tools\":");
        try writer.writeAll(tools);
    }

    try writer.writeByte('}');
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn serialize(params: RequestParams) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeRequestBody(&aw.writer, params);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(testing.allocator);
}

test "writeRequestBody: minimum fields produce valid parseable JSON" {
    const out = try serialize(.{
        .system_prompt = "you are a zigts expert",
        .user_text = "add a GET route",
    });
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    try testing.expect(parsed.value == .object);

    const root = parsed.value.object;
    try testing.expectEqualStrings(default_model, root.get("model").?.string);
    try testing.expectEqual(@as(i64, default_max_tokens), root.get("max_tokens").?.integer);
    try testing.expectEqual(true, root.get("stream").?.bool);
    try testing.expect(root.get("tools") == null);
}

test "writeRequestBody: system block is an array with cache_control ephemeral" {
    const out = try serialize(.{
        .system_prompt = "persona",
        .user_text = "hi",
    });
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();

    const system = parsed.value.object.get("system").?;
    try testing.expect(system == .array);
    try testing.expectEqual(@as(usize, 1), system.array.items.len);

    const block = system.array.items[0].object;
    try testing.expectEqualStrings("text", block.get("type").?.string);
    try testing.expectEqualStrings("persona", block.get("text").?.string);

    const cache = block.get("cache_control").?.object;
    try testing.expectEqualStrings("ephemeral", cache.get("type").?.string);
}

test "writeRequestBody: messages array carries exactly the user turn" {
    const out = try serialize(.{
        .system_prompt = "p",
        .user_text = "add a route",
    });
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();

    const msgs = parsed.value.object.get("messages").?.array.items;
    try testing.expectEqual(@as(usize, 1), msgs.len);
    try testing.expectEqualStrings("user", msgs[0].object.get("role").?.string);
    try testing.expectEqualStrings("add a route", msgs[0].object.get("content").?.string);
}

test "writeRequestBody: tools_json is embedded verbatim" {
    const tools = "[{\"name\":\"echo\",\"description\":\"d\",\"input_schema\":{}}]";
    const out = try serialize(.{
        .system_prompt = "p",
        .user_text = "u",
        .tools_json = tools,
    });
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();

    const tools_array = parsed.value.object.get("tools").?;
    try testing.expect(tools_array == .array);
    try testing.expectEqual(@as(usize, 1), tools_array.array.items.len);
    try testing.expectEqualStrings("echo", tools_array.array.items[0].object.get("name").?.string);
}

test "writeRequestBody: control characters in user_text are JSON-escaped" {
    const out = try serialize(.{
        .system_prompt = "p",
        .user_text = "line1\nline2\t\"q\"",
    });
    defer testing.allocator.free(out);

    // A successful re-parse proves the escaping is valid JSON.
    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    try testing.expectEqualStrings(
        "line1\nline2\t\"q\"",
        parsed.value.object.get("messages").?.array.items[0].object.get("content").?.string,
    );
}

test "writeRequestBody: stream=false serializes literal false" {
    const out = try serialize(.{
        .system_prompt = "p",
        .user_text = "u",
        .stream = false,
    });
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    try testing.expectEqual(false, parsed.value.object.get("stream").?.bool);
}
