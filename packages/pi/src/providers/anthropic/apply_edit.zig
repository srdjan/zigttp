//! Synthetic `apply_edit` tool: the bridge between the model's Anthropic-side
//! tool vocabulary and the turn machine's first-class `.edit` reply shape.
//!
//! It is exposed in the Anthropic tool catalog but never registered in the
//! in-process tool registry.

const std = @import("std");
const turn = @import("../../turn.zig");

pub const tool_name = "apply_edit";

pub const tool_description =
    "Propose a complete file edit. The zigttp compiler runs edit-simulate " ++
    "on the content before it reaches the user; if new violations appear, " ++
    "you will be re-prompted with the diagnostic and must try again.";

pub const input_schema_literal =
    "{\"type\":\"object\"," ++
    "\"properties\":{" ++
    "\"file\":{\"type\":\"string\",\"description\":\"Handler file path (e.g. handler.ts).\"}," ++
    "\"content\":{\"type\":\"string\",\"description\":\"Full file content after the edit.\"}," ++
    "\"before\":{\"type\":\"string\",\"description\":\"Optional: full file content before the edit.\"}" ++
    "}," ++
    "\"required\":[\"file\",\"content\"]}";

pub const RemapError = error{
    InvalidEditArgs,
};

pub fn maybeRemap(
    arena: std.mem.Allocator,
    reply: turn.AssistantReply,
) !turn.AssistantReply {
    switch (reply.response) {
        .tool_calls => |calls| {
            if (calls.len != 1) return reply;
            if (!std.mem.eql(u8, calls[0].name, tool_name)) return reply;

            const parsed = std.json.parseFromSliceLeaky(std.json.Value, arena, calls[0].args_json, .{}) catch {
                return RemapError.InvalidEditArgs;
            };
            if (parsed != .object) return RemapError.InvalidEditArgs;
            const obj = parsed.object;

            const file_v = obj.get("file") orelse return RemapError.InvalidEditArgs;
            const content_v = obj.get("content") orelse return RemapError.InvalidEditArgs;
            if (file_v != .string or content_v != .string) return RemapError.InvalidEditArgs;

            const before: ?[]const u8 = if (obj.get("before")) |v|
                (if (v == .string) v.string else null)
            else
                null;

            return .{
                .preamble = reply.preamble,
                .response = .{ .edit = .{
                    .file = file_v.string,
                    .content = content_v.string,
                    .before = before,
                } },
            };
        },
        else => return reply,
    }
}

const testing = std.testing;
const sse_parser = @import("sse_parser.zig");
const response_assembler = @import("response_assembler.zig");

test "maybeRemap: non-matching tool batch passes through unchanged" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const calls = [_]turn.ToolCall{
        .{ .id = "toolu_meta", .name = "zigts_expert_meta", .args_json = "{}" },
    };
    const reply: turn.AssistantReply = .{
        .response = .{ .tool_calls = &calls },
    };

    const out = try maybeRemap(arena.allocator(), reply);
    switch (out.response) {
        .tool_calls => |out_calls| try testing.expectEqualStrings("zigts_expert_meta", out_calls[0].name),
        else => return error.TestFailed,
    }
}

test "maybeRemap: single apply_edit tool call produces .edit reply" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const calls = [_]turn.ToolCall{
        .{ .id = "toolu_edit", .name = tool_name, .args_json = "{\"file\":\"handler.ts\",\"content\":\"function handler(req) { return Response.json({ok:true}); }\"}" },
    };
    const reply: turn.AssistantReply = .{
        .response = .{ .tool_calls = &calls },
    };

    const out = try maybeRemap(arena.allocator(), reply);
    switch (out.response) {
        .edit => |edit| {
            try testing.expectEqualStrings("handler.ts", edit.file);
            try testing.expect(std.mem.indexOf(u8, edit.content, "Response.json") != null);
        },
        else => return error.TestFailed,
    }
}

test "maybeRemap: apply_edit in a mixed tool batch stays as tool_calls" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const calls = [_]turn.ToolCall{
        .{ .id = "toolu_edit", .name = tool_name, .args_json = "{\"file\":\"h.ts\",\"content\":\"new\"}" },
        .{ .id = "toolu_meta", .name = "zigts_expert_meta", .args_json = "{}" },
    };
    const reply: turn.AssistantReply = .{
        .response = .{ .tool_calls = &calls },
    };

    const out = try maybeRemap(arena.allocator(), reply);
    try testing.expect(out.response == .tool_calls);
}

test "maybeRemap: malformed JSON returns InvalidEditArgs" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const calls = [_]turn.ToolCall{
        .{ .id = "toolu_edit", .name = tool_name, .args_json = "{not valid json" },
    };
    const reply: turn.AssistantReply = .{
        .response = .{ .tool_calls = &calls },
    };
    try testing.expectError(RemapError.InvalidEditArgs, maybeRemap(arena.allocator(), reply));
}

test "full pipeline: cassette -> parse -> assemble -> remap -> .edit reply" {
    const cassette = @embedFile("cassettes/apply_edit_tool.sse");
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const ta = arena.allocator();

    const events = try sse_parser.parseAll(ta, cassette);
    const outcome = try response_assembler.assemble(ta, events);
    const remapped = try maybeRemap(ta, outcome.reply);

    switch (remapped.response) {
        .edit => |edit| {
            try testing.expectEqualStrings("handler.ts", edit.file);
            try testing.expect(std.mem.indexOf(u8, edit.content, "Response.json") != null);
        },
        else => return error.TestFailed,
    }
    try testing.expectEqualStrings("tool_use", outcome.stop_reason.?);
}
