//! Consumes a list of parsed SSE events and assembles a `turn.ModelReply`.
//! Text blocks concatenate into a `.text` reply; a `tool_use` block
//! concatenates its `input_json_delta`s and produces a `.tool_call` reply.
//! When both appear (Anthropic may stream a preamble text block before
//! tool_use), the tool_call wins because the loop needs the action —
//! the preamble text is dropped here and can be surfaced by a future
//! pre-tool narration variant.
//!
//! `.edit` replies are *not* produced here. Edits flow through a dedicated
//! tool (e.g. `apply_edit`) whose tool_call the loop driver remaps into a
//! `.edit` reply before handing it to the turn machine. That remap lives
//! next to the loop, not in the assembler, because the assembler only sees
//! raw Anthropic events and knows nothing about the pi tool catalog.

const std = @import("std");
const turn = @import("../turn.zig");
const events = @import("events.zig");

pub const AssembleError = error{
    /// No content blocks arrived before the event stream ended.
    Empty,
    /// The stream carried an Anthropic api_error event.
    ApiError,
};

pub const Outcome = struct {
    reply: turn.ModelReply,
    stop_reason: ?[]const u8 = null,
    output_tokens: ?u64 = null,
};

pub fn assemble(
    arena: std.mem.Allocator,
    event_list: []const events.Event,
) !Outcome {
    var text_buf: std.ArrayListUnmanaged(u8) = .empty;
    var input_json_buf: std.ArrayListUnmanaged(u8) = .empty;
    var tool_name: ?[]const u8 = null;
    var stop_reason: ?[]const u8 = null;
    var output_tokens: ?u64 = null;

    for (event_list) |ev| {
        switch (ev) {
            .api_error => return AssembleError.ApiError,
            .content_block_start => |start| switch (start.kind) {
                .text => {},
                .tool_use => |header| {
                    tool_name = header.name;
                },
            },
            .content_block_delta => |delta| switch (delta.payload) {
                .text => |t| try text_buf.appendSlice(arena, t),
                .input_json => |j| try input_json_buf.appendSlice(arena, j),
            },
            .message_delta => |md| {
                stop_reason = md.stop_reason;
                if (md.output_tokens) |ot| output_tokens = ot;
            },
            else => {},
        }
    }

    if (tool_name) |name| {
        const args_json = if (input_json_buf.items.len == 0)
            try arena.dupe(u8, "{}")
        else
            try input_json_buf.toOwnedSlice(arena);
        return .{
            .reply = .{ .tool_call = .{ .name = name, .args_json = args_json } },
            .stop_reason = stop_reason,
            .output_tokens = output_tokens,
        };
    }

    if (text_buf.items.len > 0) {
        return .{
            .reply = .{ .text = try text_buf.toOwnedSlice(arena) },
            .stop_reason = stop_reason,
            .output_tokens = output_tokens,
        };
    }

    return AssembleError.Empty;
}

// ---------------------------------------------------------------------------
// Tests
//
// The assembler is driven off the slice-2 cassettes via `sse_parser.parseAll`
// so every test exercises the real wire format the client will see.
// ---------------------------------------------------------------------------

const testing = std.testing;
const sse_parser = @import("sse_parser.zig");

const cassette_text_simple = @embedFile("cassettes/text_simple.sse");
const cassette_text_multi_delta = @embedFile("cassettes/text_multi_delta.sse");
const cassette_tool_use = @embedFile("cassettes/tool_use.sse");
const cassette_error_overloaded = @embedFile("cassettes/error_overloaded.sse");

test "assemble: text_simple cassette yields a .text reply with stop_reason+tokens" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_text_simple);

    const outcome = try assemble(arena.allocator(), list);
    try testing.expect(outcome.reply == .text);
    try testing.expectEqualStrings("Hello, world!", outcome.reply.text);
    try testing.expectEqualStrings("end_turn", outcome.stop_reason.?);
    try testing.expectEqual(@as(u64, 5), outcome.output_tokens.?);
}

test "assemble: text_multi_delta cassette concatenates every delta in order" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_text_multi_delta);

    const outcome = try assemble(arena.allocator(), list);
    try testing.expect(outcome.reply == .text);
    try testing.expectEqualStrings("Hello, how can I help?", outcome.reply.text);
    try testing.expectEqualStrings("end_turn", outcome.stop_reason.?);
}

test "assemble: tool_use cassette yields a .tool_call with assembled JSON args" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try sse_parser.parseAll(arena.allocator(), cassette_tool_use);

    const outcome = try assemble(arena.allocator(), list);
    try testing.expect(outcome.reply == .tool_call);
    try testing.expectEqualStrings("zigts_expert_describe_rule", outcome.reply.tool_call.name);
    try testing.expectEqualStrings("{\"rule\":\"ZTS303\"}", outcome.reply.tool_call.args_json);
    try testing.expectEqualStrings("tool_use", outcome.stop_reason.?);
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
    try testing.expectError(AssembleError.Empty, assemble(arena.allocator(), &.{}));
}

test "assemble: tool_call with zero input_json deltas still produces {} args" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const ta = arena.allocator();

    const ev_list = [_]events.Event{
        .message_start,
        .{ .content_block_start = .{
            .index = 0,
            .kind = .{ .tool_use = .{ .id = "toolu_01", .name = "nop_tool" } },
        } },
        .{ .content_block_stop = .{ .index = 0 } },
        .{ .message_delta = .{ .stop_reason = "tool_use", .output_tokens = 3 } },
        .message_stop,
    };

    const outcome = try assemble(ta, &ev_list);
    try testing.expect(outcome.reply == .tool_call);
    try testing.expectEqualStrings("nop_tool", outcome.reply.tool_call.name);
    try testing.expectEqualStrings("{}", outcome.reply.tool_call.args_json);
}

test "assemble: text preamble plus tool_use yields the tool_call (preamble dropped)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const ta = arena.allocator();

    const ev_list = [_]events.Event{
        .{ .content_block_start = .{ .index = 0, .kind = .text } },
        .{ .content_block_delta = .{
            .index = 0,
            .payload = .{ .text = "I'll look that up." },
        } },
        .{ .content_block_stop = .{ .index = 0 } },
        .{ .content_block_start = .{
            .index = 1,
            .kind = .{ .tool_use = .{ .id = "toolu_02", .name = "lookup" } },
        } },
        .{ .content_block_delta = .{
            .index = 1,
            .payload = .{ .input_json = "{\"q\":\"x\"}" },
        } },
        .{ .content_block_stop = .{ .index = 1 } },
        .{ .message_delta = .{ .stop_reason = "tool_use", .output_tokens = 10 } },
    };

    const outcome = try assemble(ta, &ev_list);
    try testing.expect(outcome.reply == .tool_call);
    try testing.expectEqualStrings("lookup", outcome.reply.tool_call.name);
    try testing.expectEqualStrings("{\"q\":\"x\"}", outcome.reply.tool_call.args_json);
}
