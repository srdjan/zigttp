//! Consumes parsed SSE events and assembles an `turn.AssistantReply`.
//!
//! The Anthropic stream may carry multiple `tool_use` blocks in one assistant
//! message. Blocks are keyed by their SSE `index` and assembled independently.

const std = @import("std");
const turn = @import("../../turn.zig");
const events = @import("events.zig");

pub const AssembleError = error{
    Empty,
    ApiError,
};

pub const Outcome = struct {
    reply: turn.AssistantReply,
    stop_reason: ?[]const u8 = null,
    usage: turn.Usage = .{},
};

const BlockState = union(enum) {
    none,
    text: std.ArrayListUnmanaged(u8),
    tool_use: ToolUseState,
};

const ToolUseState = struct {
    id: []const u8,
    name: []const u8,
    input_json: std.ArrayListUnmanaged(u8) = .empty,
};

pub fn assemble(
    arena: std.mem.Allocator,
    event_list: []const events.Event,
) !Outcome {
    var blocks: std.ArrayListUnmanaged(BlockState) = .empty;
    var stop_reason: ?[]const u8 = null;
    var usage: turn.Usage = .{};

    for (event_list) |ev| {
        switch (ev) {
            .api_error => return AssembleError.ApiError,
            .message_start => |start_usage| {
                usage.input_tokens = start_usage.input_tokens;
                usage.cache_read_input_tokens = start_usage.cache_read_input_tokens;
                usage.cache_creation_input_tokens = start_usage.cache_creation_input_tokens;
            },
            .content_block_start => |start| {
                try ensureBlockCapacity(arena, &blocks, start.index);
                blocks.items[start.index] = switch (start.kind) {
                    .text => .{ .text = .empty },
                    .tool_use => |header| .{ .tool_use = .{
                        .id = header.id,
                        .name = header.name,
                    } },
                };
            },
            .content_block_delta => |delta| {
                try ensureBlockCapacity(arena, &blocks, delta.index);
                const block = &blocks.items[delta.index];
                switch (block.*) {
                    .text => |*buf| switch (delta.payload) {
                        .text => |text| try buf.appendSlice(arena, text),
                        else => {},
                    },
                    .tool_use => |*tool_use| switch (delta.payload) {
                        .input_json => |json| try tool_use.input_json.appendSlice(arena, json),
                        else => {},
                    },
                    .none => {},
                }
            },
            .message_delta => |delta| {
                stop_reason = delta.stop_reason;
                if (delta.output_tokens) |tokens| usage.output_tokens = tokens;
            },
            else => {},
        }
    }

    var narration: std.ArrayListUnmanaged(u8) = .empty;
    var tool_calls: std.ArrayListUnmanaged(turn.ToolCall) = .empty;

    for (blocks.items) |*block| {
        switch (block.*) {
            .none => {},
            .text => |buf| if (buf.items.len > 0) try narration.appendSlice(arena, buf.items),
            .tool_use => |tool_use| {
                const args_json = if (tool_use.input_json.items.len == 0)
                    try arena.dupe(u8, "{}")
                else
                    try arena.dupe(u8, tool_use.input_json.items);
                try tool_calls.append(arena, .{
                    .id = tool_use.id,
                    .name = tool_use.name,
                    .args_json = args_json,
                });
            },
        }
    }

    const preamble = if (narration.items.len == 0)
        null
    else
        try narration.toOwnedSlice(arena);

    if (tool_calls.items.len > 0) {
        return .{
            .reply = .{
                .preamble = preamble,
                .response = .{ .tool_calls = try tool_calls.toOwnedSlice(arena) },
            },
            .stop_reason = stop_reason,
            .usage = usage,
        };
    }

    if (preamble) |text| {
        return .{
            .reply = .{ .response = .{ .final_text = text } },
            .stop_reason = stop_reason,
            .usage = usage,
        };
    }

    return AssembleError.Empty;
}

fn ensureBlockCapacity(
    allocator: std.mem.Allocator,
    blocks: *std.ArrayListUnmanaged(BlockState),
    index: u32,
) !void {
    if (index < blocks.items.len) return;
    const new_len: usize = index + 1;
    const old_len = blocks.items.len;
    try blocks.resize(allocator, new_len);
    for (blocks.items[old_len..]) |*item| item.* = .none;
}

const testing = std.testing;
const sse_parser = @import("sse_parser.zig");

const cassette_text_simple = @embedFile("cassettes/text_simple.sse");
const cassette_text_multi_delta = @embedFile("cassettes/text_multi_delta.sse");
const cassette_tool_use = @embedFile("cassettes/tool_use.sse");
const cassette_error_overloaded = @embedFile("cassettes/error_overloaded.sse");

test "assemble: text_simple cassette yields final_text with stop_reason+tokens" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_text_simple);

    const outcome = try assemble(arena.allocator(), list);
    switch (outcome.reply.response) {
        .final_text => |text| try testing.expectEqualStrings("Hello, world!", text),
        else => return error.TestFailed,
    }
    try testing.expectEqualStrings("end_turn", outcome.stop_reason.?);
    try testing.expectEqual(@as(u64, 12), outcome.usage.input_tokens);
    try testing.expectEqual(@as(u64, 5), outcome.usage.output_tokens);
}

test "assemble: text_multi_delta cassette concatenates every delta in order" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_text_multi_delta);

    const outcome = try assemble(arena.allocator(), list);
    switch (outcome.reply.response) {
        .final_text => |text| try testing.expectEqualStrings("Hello, how can I help?", text),
        else => return error.TestFailed,
    }
    try testing.expectEqualStrings("end_turn", outcome.stop_reason.?);
}

test "assemble: tool_use cassette yields a tool_calls reply with assembled JSON args" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_tool_use);

    const outcome = try assemble(arena.allocator(), list);
    switch (outcome.reply.response) {
        .tool_calls => |calls| {
            try testing.expectEqual(@as(usize, 1), calls.len);
            try testing.expectEqualStrings("zigts_expert_describe_rule", calls[0].name);
            try testing.expectEqualStrings("{\"rule\":\"ZTS303\"}", calls[0].args_json);
        },
        else => return error.TestFailed,
    }
    try testing.expectEqualStrings("tool_use", outcome.stop_reason.?);
}

test "assemble: error_overloaded cassette returns AssembleError.ApiError" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_error_overloaded);

    try testing.expectError(AssembleError.ApiError, assemble(arena.allocator(), list));
}

test "assemble: text preamble plus tool batch preserves narration and every tool call" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const ta = arena.allocator();

    const ev_list = [_]events.Event{
        .{ .content_block_start = .{ .index = 0, .kind = .text } },
        .{ .content_block_delta = .{ .index = 0, .payload = .{ .text = "Inspecting workspace." } } },
        .{ .content_block_stop = .{ .index = 0 } },
        .{ .content_block_start = .{
            .index = 1,
            .kind = .{ .tool_use = .{ .id = "toolu_meta", .name = "zigts_expert_meta" } },
        } },
        .{ .content_block_stop = .{ .index = 1 } },
        .{ .content_block_start = .{
            .index = 2,
            .kind = .{ .tool_use = .{ .id = "toolu_search", .name = "workspace_search_text" } },
        } },
        .{ .content_block_delta = .{
            .index = 2,
            .payload = .{ .input_json = "{\"query\":\"handler\"}" },
        } },
        .{ .content_block_stop = .{ .index = 2 } },
        .{ .message_delta = .{ .stop_reason = "tool_use", .output_tokens = 13 } },
    };

    const outcome = try assemble(ta, &ev_list);
    try testing.expectEqualStrings("Inspecting workspace.", outcome.reply.preamble.?);
    switch (outcome.reply.response) {
        .tool_calls => |calls| {
            try testing.expectEqual(@as(usize, 2), calls.len);
            try testing.expectEqualStrings("toolu_meta", calls[0].id);
            try testing.expectEqualStrings("{}", calls[0].args_json);
            try testing.expectEqualStrings("workspace_search_text", calls[1].name);
            try testing.expectEqualStrings("{\"query\":\"handler\"}", calls[1].args_json);
            try testing.expectEqual(@as(u64, 13), outcome.usage.output_tokens);
        },
        else => return error.TestFailed,
    }
}
