//! Consumes parsed OpenAI Responses-API events and assembles a
//! `turn.AssistantReply`. Mirrors the shape of the Anthropic assembler:
//! returns the same `Outcome` struct, the same `AssembleError` set, and
//! the same `final_text | tool_calls (+ preamble)` discriminator so the
//! loop sees identical replies regardless of provider.
//!
//! The Responses API addresses every event by `output_index` (the position
//! of a top-level output item). We keep per-index state and walk the event
//! list once, just like the Anthropic side keys on `content_block_start.index`.

const std = @import("std");
const builtin = @import("builtin");
const turn = @import("../../turn.zig");
const events = @import("events.zig");

pub const AssembleError = error{
    Empty,
    ApiError,
    UnexpectedDeltaOrder,
};

pub const Outcome = struct {
    reply: turn.AssistantReply,
    stop_reason: ?[]const u8 = null,
    usage: turn.Usage = .{},
};

const ItemState = union(enum) {
    none,
    message: std.ArrayListUnmanaged(u8),
    function_call: FunctionCallState,
};

const FunctionCallState = struct {
    call_id: []const u8,
    name: []const u8,
    args: std.ArrayListUnmanaged(u8) = .empty,
};

pub fn assemble(
    arena: std.mem.Allocator,
    event_list: []const events.Event,
) !Outcome {
    var items: std.ArrayListUnmanaged(ItemState) = .empty;
    var stop_reason: ?[]const u8 = null;
    var usage: turn.Usage = .{};

    for (event_list) |ev| {
        switch (ev) {
            .api_error => |info| {
                // Surface kind/message so operators see why a turn failed
                // instead of just the bare ApiError tag. Gated by is_test so
                // the existing test that asserts the bare error stays quiet.
                if (!builtin.is_test) {
                    std.log.warn(
                        "openai api error: kind={s} message={s}",
                        .{ info.kind, info.message },
                    );
                }
                return AssembleError.ApiError;
            },
            .response_started => {},
            .output_item_added => |added| {
                try ensureCapacity(arena, &items, added.output_index);
                items.items[added.output_index] = switch (added.kind) {
                    .message => .{ .message = .empty },
                    .function_call => |header| .{ .function_call = .{
                        .call_id = header.call_id,
                        .name = header.name,
                    } },
                };
            },
            .output_text_delta => |delta| {
                try ensureCapacity(arena, &items, delta.output_index);
                const item = &items.items[delta.output_index];
                switch (item.*) {
                    .message => |*buf| try buf.appendSlice(arena, delta.text),
                    // Delta for a slot whose item.added arrived later (or
                    // never) is a streaming protocol violation — fail loudly
                    // instead of silently dropping the text/tool args.
                    else => return AssembleError.UnexpectedDeltaOrder,
                }
            },
            .function_call_arguments_delta => |delta| {
                try ensureCapacity(arena, &items, delta.output_index);
                const item = &items.items[delta.output_index];
                switch (item.*) {
                    .function_call => |*fc| try fc.args.appendSlice(arena, delta.delta),
                    else => return AssembleError.UnexpectedDeltaOrder,
                }
            },
            .output_item_done => {},
            .response_completed => |completed| {
                stop_reason = completed.stop_reason;
                usage.input_tokens = completed.usage.input_tokens;
                usage.output_tokens = completed.usage.output_tokens;
            },
        }
    }

    var narration: std.ArrayListUnmanaged(u8) = .empty;
    var tool_calls: std.ArrayListUnmanaged(turn.ToolCall) = .empty;

    for (items.items) |*item| {
        switch (item.*) {
            .none => {},
            .message => |buf| if (buf.items.len > 0) try narration.appendSlice(arena, buf.items),
            .function_call => |fc| {
                const args_json = if (fc.args.items.len == 0)
                    try arena.dupe(u8, "{}")
                else
                    try arena.dupe(u8, fc.args.items);
                try tool_calls.append(arena, .{
                    .id = fc.call_id,
                    .name = fc.name,
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

fn ensureCapacity(
    allocator: std.mem.Allocator,
    items: *std.ArrayListUnmanaged(ItemState),
    index: u32,
) !void {
    if (index < items.items.len) return;
    const new_len: usize = index + 1;
    const old_len = items.items.len;
    try items.resize(allocator, new_len);
    for (items.items[old_len..]) |*item| item.* = .none;
}

const testing = std.testing;
const sse_parser = @import("sse_parser.zig");

const cassette_text_simple = @embedFile("cassettes/text_simple.sse");
const cassette_text_multi_delta = @embedFile("cassettes/text_multi_delta.sse");
const cassette_tool_use = @embedFile("cassettes/tool_use.sse");
const cassette_multi_tool = @embedFile("cassettes/multi_tool.sse");
const cassette_error_overloaded = @embedFile("cassettes/error_overloaded.sse");

test "assemble: text_simple cassette yields final_text with usage" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_text_simple);

    const outcome = try assemble(arena.allocator(), list);
    switch (outcome.reply.response) {
        .final_text => |text| try testing.expectEqualStrings("Hello, world!", text),
        else => return error.TestFailed,
    }
    try testing.expectEqualStrings("completed", outcome.stop_reason.?);
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
    try testing.expectEqualStrings("completed", outcome.stop_reason.?);
}

test "assemble: tool_use cassette yields tool_calls reply with assembled JSON args" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_tool_use);

    const outcome = try assemble(arena.allocator(), list);
    switch (outcome.reply.response) {
        .tool_calls => |calls| {
            try testing.expectEqual(@as(usize, 1), calls.len);
            try testing.expectEqualStrings("call_abc123", calls[0].id);
            try testing.expectEqualStrings("zigts_expert_describe_rule", calls[0].name);
            try testing.expectEqualStrings("{\"rule\":\"ZTS303\"}", calls[0].args_json);
        },
        else => return error.TestFailed,
    }
}

test "assemble: multi_tool cassette yields both function calls in order with their args" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_multi_tool);

    const outcome = try assemble(arena.allocator(), list);
    switch (outcome.reply.response) {
        .tool_calls => |calls| {
            try testing.expectEqual(@as(usize, 2), calls.len);
            try testing.expectEqualStrings("call_meta_1", calls[0].id);
            try testing.expectEqualStrings("zigts_expert_meta", calls[0].name);
            try testing.expectEqualStrings("{}", calls[0].args_json);
            try testing.expectEqualStrings("call_search_2", calls[1].id);
            try testing.expectEqualStrings("workspace_search_text", calls[1].name);
            try testing.expectEqualStrings("{\"query\":\"handler\"}", calls[1].args_json);
        },
        else => return error.TestFailed,
    }
    // The multi_tool cassette interleaves a short narration preamble before
    // the tool batch, mirroring the Anthropic side's "preamble + tools" test.
    try testing.expectEqualStrings("Inspecting workspace.", outcome.reply.preamble.?);
}

test "assemble: error_overloaded cassette returns AssembleError.ApiError" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_error_overloaded);

    try testing.expectError(AssembleError.ApiError, assemble(arena.allocator(), list));
}

test "assemble: empty event list returns AssembleError.Empty" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    try testing.expectError(
        AssembleError.Empty,
        assemble(arena.allocator(), &.{}),
    );
}
