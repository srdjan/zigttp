//! Builds the Anthropic Messages API request body from the structured
//! transcript plus an optional injected user prompt used for retry-draft
//! feedback.

const std = @import("std");
const json_writer = @import("json_writer.zig");
const transcript_mod = @import("../transcript.zig");

pub const default_model = "claude-opus-4-6";
pub const default_max_tokens: u32 = 8192;

pub const RequestParams = struct {
    model: []const u8 = default_model,
    max_tokens: u32 = default_max_tokens,
    system_prompt: []const u8,
    transcript: *const transcript_mod.Transcript,
    extra_user_text: ?[]const u8 = null,
    tools_json: ?[]const u8 = null,
    stream: bool = true,
};

const MessageRole = enum { user, assistant };

const ContentBlock = union(enum) {
    text: []const u8,
    tool_use: transcript_mod.OwnedToolCall,
    tool_result: transcript_mod.OwnedToolResult,
};

const MessageGroup = struct {
    role: MessageRole,
    blocks: std.ArrayListUnmanaged(ContentBlock) = .empty,
};

pub fn writeRequestBody(
    writer: anytype,
    allocator: std.mem.Allocator,
    params: RequestParams,
) !void {
    try writer.writeByte('{');

    try writer.writeAll("\"model\":");
    try json_writer.writeString(writer, params.model);
    try writer.print(",\"max_tokens\":{d}", .{params.max_tokens});
    try writer.writeAll(",\"stream\":");
    try writer.writeAll(if (params.stream) "true" else "false");

    try writer.writeAll(",\"system\":[{\"type\":\"text\",\"text\":");
    try json_writer.writeString(writer, params.system_prompt);
    try writer.writeAll(",\"cache_control\":{\"type\":\"ephemeral\"}}]");

    try writer.writeAll(",\"messages\":");
    try writeMessagesArray(writer, allocator, params.transcript, params.extra_user_text);

    if (params.tools_json) |tools| {
        try writer.writeAll(",\"tools\":");
        try writer.writeAll(tools);
    }

    try writer.writeByte('}');
}

fn writeMessagesArray(
    writer: anytype,
    allocator: std.mem.Allocator,
    transcript: *const transcript_mod.Transcript,
    extra_user_text: ?[]const u8,
) !void {
    var groups: std.ArrayListUnmanaged(MessageGroup) = .empty;
    defer {
        for (groups.items) |*group| group.blocks.deinit(allocator);
        groups.deinit(allocator);
    }

    for (transcript.entries.items) |*entry| {
        switch (entry.*) {
            .user_text => |body| try appendBlock(allocator, &groups, .user, .{ .text = body }),
            .model_text => |body| try appendBlock(allocator, &groups, .assistant, .{ .text = body }),
            .assistant_tool_use => |calls| {
                for (calls) |call| {
                    try appendBlock(allocator, &groups, .assistant, .{ .tool_use = call });
                }
            },
            .tool_result => |result| try appendBlock(allocator, &groups, .user, .{ .tool_result = result }),
            .proof_card, .diagnostic_box => {},
        }
    }

    if (extra_user_text) |body| {
        try groups.append(allocator, .{ .role = .user });
        try groups.items[groups.items.len - 1].blocks.append(allocator, .{ .text = body });
    }

    try writer.writeByte('[');
    for (groups.items, 0..) |group, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"role\":");
        try json_writer.writeString(writer, switch (group.role) {
            .user => "user",
            .assistant => "assistant",
        });
        try writer.writeAll(",\"content\":[");
        for (group.blocks.items, 0..) |block, block_index| {
            if (block_index > 0) try writer.writeByte(',');
            try writeBlock(writer, block);
        }
        try writer.writeAll("]}");
    }
    try writer.writeByte(']');
}

fn appendBlock(
    allocator: std.mem.Allocator,
    groups: *std.ArrayListUnmanaged(MessageGroup),
    role: MessageRole,
    block: ContentBlock,
) !void {
    if (groups.items.len == 0 or groups.items[groups.items.len - 1].role != role) {
        try groups.append(allocator, .{ .role = role });
    }
    try groups.items[groups.items.len - 1].blocks.append(allocator, block);
}

fn writeBlock(writer: anytype, block: ContentBlock) !void {
    switch (block) {
        .text => |body| {
            try writer.writeAll("{\"type\":\"text\",\"text\":");
            try json_writer.writeString(writer, body);
            try writer.writeByte('}');
        },
        .tool_use => |call| {
            try writer.writeAll("{\"type\":\"tool_use\",\"id\":");
            try json_writer.writeString(writer, call.id);
            try writer.writeAll(",\"name\":");
            try json_writer.writeString(writer, call.name);
            try writer.writeAll(",\"input\":");
            try writer.writeAll(call.args_json);
            try writer.writeByte('}');
        },
        .tool_result => |result| {
            try writer.writeAll("{\"type\":\"tool_result\",\"tool_use_id\":");
            try json_writer.writeString(writer, result.tool_use_id);
            try writer.writeAll(",\"content\":");
            try json_writer.writeString(writer, result.body);
            try writer.writeAll(",\"is_error\":");
            try writer.writeAll(if (result.ok) "false" else "true");
            try writer.writeByte('}');
        },
    }
}

const testing = std.testing;

fn serialize(
    allocator: std.mem.Allocator,
    params: RequestParams,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeRequestBody(&aw.writer, allocator, params);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

test "writeRequestBody: first turn emits one user message array" {
    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(testing.allocator);
    try transcript.append(testing.allocator, .{ .user_text = "add a GET route" });

    const out = try serialize(testing.allocator, .{
        .system_prompt = "you are a zigts expert",
        .transcript = &transcript,
    });
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();

    const root = parsed.value.object;
    const msgs = root.get("messages").?.array.items;
    try testing.expectEqual(@as(usize, 1), msgs.len);
    try testing.expectEqualStrings("user", msgs[0].object.get("role").?.string);
    const blocks = msgs[0].object.get("content").?.array.items;
    try testing.expectEqualStrings("text", blocks[0].object.get("type").?.string);
    try testing.expectEqualStrings("add a GET route", blocks[0].object.get("text").?.string);
}

test "writeRequestBody: tool-use and tool-result transcript entries serialize structurally" {
    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(testing.allocator);

    const calls = [_]@import("../turn.zig").ToolCall{
        .{ .id = "toolu_meta", .name = "zigts_expert_meta", .args_json = "{}" },
        .{ .id = "toolu_search", .name = "workspace_search_text", .args_json = "{\"query\":\"handler\"}" },
    };
    try transcript.append(testing.allocator, .{ .user_text = "inspect the workspace" });
    try transcript.append(testing.allocator, .{ .model_text = "I'll inspect the workspace first." });
    try transcript.append(testing.allocator, .{ .assistant_tool_use = &calls });
    try transcript.append(testing.allocator, .{ .tool_result = .{
        .tool_use_id = "toolu_meta",
        .tool_name = "zigts_expert_meta",
        .ok = true,
        .body = "{\"ok\":true}",
    } });
    try transcript.append(testing.allocator, .{ .tool_result = .{
        .tool_use_id = "toolu_search",
        .tool_name = "workspace_search_text",
        .ok = false,
        .body = "{\"ok\":false}",
    } });

    const out = try serialize(testing.allocator, .{
        .system_prompt = "persona",
        .transcript = &transcript,
    });
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();

    const msgs = parsed.value.object.get("messages").?.array.items;
    try testing.expectEqual(@as(usize, 3), msgs.len);
    try testing.expectEqualStrings("assistant", msgs[1].object.get("role").?.string);
    try testing.expectEqualStrings("user", msgs[2].object.get("role").?.string);
    const assistant_blocks = msgs[1].object.get("content").?.array.items;
    try testing.expectEqual(@as(usize, 3), assistant_blocks.len);
    try testing.expectEqualStrings("text", assistant_blocks[0].object.get("type").?.string);
    try testing.expectEqualStrings("tool_use", assistant_blocks[1].object.get("type").?.string);
    try testing.expectEqualStrings("tool_use", assistant_blocks[2].object.get("type").?.string);
    const user_blocks = msgs[2].object.get("content").?.array.items;
    try testing.expectEqualStrings("tool_result", user_blocks[0].object.get("type").?.string);
    try testing.expectEqualStrings("toolu_search", user_blocks[1].object.get("tool_use_id").?.string);
    try testing.expectEqual(true, user_blocks[1].object.get("is_error").?.bool);
}

test "writeRequestBody: extra retry prompt appends a final user text message" {
    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(testing.allocator);
    try transcript.append(testing.allocator, .{ .user_text = "add a route" });

    const out = try serialize(testing.allocator, .{
        .system_prompt = "p",
        .transcript = &transcript,
        .extra_user_text = "compiler veto failed",
    });
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    const msgs = parsed.value.object.get("messages").?.array.items;
    try testing.expectEqual(@as(usize, 2), msgs.len);
    const retry_blocks = msgs[1].object.get("content").?.array.items;
    try testing.expectEqualStrings("compiler veto failed", retry_blocks[0].object.get("text").?.string);
}
