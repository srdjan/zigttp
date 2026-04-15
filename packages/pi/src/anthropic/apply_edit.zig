//! Synthetic `apply_edit` tool: the bridge between the model's
//! Anthropic-side tool-use vocabulary and the turn machine's first-class
//! `.edit` reply shape. Declared in the tools catalog so the model
//! knows how to call it; `maybeRemap` intercepts the matching tool_call
//! and returns `turn.ModelReply.edit` so the veto path runs. Never
//! registered in `Registry`, so it cannot accidentally dispatch through
//! `Registry.invoke`.

const std = @import("std");
const turn = @import("../turn.zig");

pub const tool_name = "apply_edit";

pub const tool_description =
    "Propose a complete file edit. The zigttp compiler runs edit-simulate " ++
    "on the content before it reaches the user; if new violations appear, " ++
    "you will be re-prompted with the diagnostic and must try again.";

/// Anthropic-shape JSON Schema the model uses to fill in the arguments.
/// `file` and `content` are required; `before` is optional context for
/// diff-aware violations on rules that care about what changed.
pub const input_schema_literal =
    "{\"type\":\"object\"," ++
    "\"properties\":{" ++
    "\"file\":{\"type\":\"string\"," ++
    "\"description\":\"Handler file path (e.g., handler.ts).\"}," ++
    "\"content\":{\"type\":\"string\"," ++
    "\"description\":\"Full file content after the edit.\"}," ++
    "\"before\":{\"type\":\"string\"," ++
    "\"description\":\"Optional: full file content before the edit.\"}" ++
    "}," ++
    "\"required\":[\"file\",\"content\"]}";

pub const RemapError = error{
    InvalidEditArgs,
};

/// If `reply` is a `.tool_call` to `apply_edit`, parse its JSON args into
/// a `turn.ModelReply.Edit` and return `.edit`. Otherwise return `reply`
/// unchanged. Slices on the returned Edit borrow from the JSON values
/// allocated in `arena`, so the caller must keep the arena alive as long
/// as the reply is in use.
pub fn maybeRemap(
    arena: std.mem.Allocator,
    reply: turn.ModelReply,
) !turn.ModelReply {
    switch (reply) {
        .tool_call => |tc| {
            if (!std.mem.eql(u8, tc.name, tool_name)) return reply;

            const parsed = std.json.parseFromSliceLeaky(
                std.json.Value,
                arena,
                tc.args_json,
                .{},
            ) catch return RemapError.InvalidEditArgs;
            if (parsed != .object) return RemapError.InvalidEditArgs;
            const obj = parsed.object;

            const file_v = obj.get("file") orelse return RemapError.InvalidEditArgs;
            if (file_v != .string) return RemapError.InvalidEditArgs;
            const content_v = obj.get("content") orelse return RemapError.InvalidEditArgs;
            if (content_v != .string) return RemapError.InvalidEditArgs;

            const before: ?[]const u8 = if (obj.get("before")) |v|
                (if (v == .string) v.string else null)
            else
                null;

            return .{ .edit = .{
                .file = file_v.string,
                .content = content_v.string,
                .before = before,
            } };
        },
        else => return reply,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;
const sse_parser = @import("sse_parser.zig");
const response_assembler = @import("response_assembler.zig");

test "maybeRemap: non-matching tool_call passes through unchanged" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const reply: turn.ModelReply = .{ .tool_call = .{
        .name = "zigts_expert_meta",
        .args_json = "{}",
    } };
    const out = try maybeRemap(arena.allocator(), reply);
    try testing.expect(out == .tool_call);
    try testing.expectEqualStrings("zigts_expert_meta", out.tool_call.name);
}

test "maybeRemap: text reply passes through unchanged" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const reply: turn.ModelReply = .{ .text = "hello" };
    const out = try maybeRemap(arena.allocator(), reply);
    try testing.expect(out == .text);
    try testing.expectEqualStrings("hello", out.text);
}

test "maybeRemap: apply_edit tool_call with file+content produces .edit reply" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const reply: turn.ModelReply = .{ .tool_call = .{
        .name = tool_name,
        .args_json = "{\"file\":\"handler.ts\",\"content\":\"function handler(req) { return Response.json({ok:true}); }\"}",
    } };
    const out = try maybeRemap(arena.allocator(), reply);
    try testing.expect(out == .edit);
    try testing.expectEqualStrings("handler.ts", out.edit.file);
    try testing.expect(std.mem.indexOf(u8, out.edit.content, "Response.json") != null);
    try testing.expect(out.edit.before == null);
}

test "maybeRemap: apply_edit with optional before field preserves it" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const reply: turn.ModelReply = .{ .tool_call = .{
        .name = tool_name,
        .args_json = "{\"file\":\"h.ts\",\"content\":\"new\",\"before\":\"old\"}",
    } };
    const out = try maybeRemap(arena.allocator(), reply);
    try testing.expect(out == .edit);
    try testing.expectEqualStrings("new", out.edit.content);
    try testing.expectEqualStrings("old", out.edit.before.?);
}

test "maybeRemap: missing file field returns InvalidEditArgs" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const reply: turn.ModelReply = .{ .tool_call = .{
        .name = tool_name,
        .args_json = "{\"content\":\"x\"}",
    } };
    try testing.expectError(RemapError.InvalidEditArgs, maybeRemap(arena.allocator(), reply));
}

test "maybeRemap: missing content field returns InvalidEditArgs" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const reply: turn.ModelReply = .{ .tool_call = .{
        .name = tool_name,
        .args_json = "{\"file\":\"h.ts\"}",
    } };
    try testing.expectError(RemapError.InvalidEditArgs, maybeRemap(arena.allocator(), reply));
}

test "maybeRemap: malformed JSON returns InvalidEditArgs" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const reply: turn.ModelReply = .{ .tool_call = .{
        .name = tool_name,
        .args_json = "{not valid json",
    } };
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

    try testing.expect(remapped == .edit);
    try testing.expectEqualStrings("handler.ts", remapped.edit.file);
    try testing.expect(std.mem.indexOf(u8, remapped.edit.content, "Response.json") != null);
    try testing.expectEqualStrings("tool_use", outcome.stop_reason.?);
}
