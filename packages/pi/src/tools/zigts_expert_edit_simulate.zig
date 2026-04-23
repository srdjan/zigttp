//! The compiler-veto primitive. `ToolResult.ok` is true iff no *new* violations
//! were introduced by the simulated edit; pre-existing violations in the same
//! file do not block the edit. This is the semantics the phase-2 agent loop
//! needs so a model editing a file with known issues isn't paralyzed.
//!
//! Wire format for args[0] matches the existing `--stdin-json` shape so the
//! CLI and registry paths are compatible: {"file", "content", "before"?}.

const std = @import("std");
const edit_simulate = @import("zigts_cli").edit_simulate;
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_edit_simulate";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "simulate edit",
    .description = "Run full analysis on proposed file content and report new vs. preexisting violations.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"file\":{\"type\":\"string\"},\"content\":{\"type\":\"string\"},\"before\":{\"type\":\"string\"}},\"required\":[\"file\",\"content\"]}",
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_edit_simulate requires a JSON input argument\n");
    }

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_edit_simulate: invalid JSON input\n");
    };
    defer parsed.deinit();

    if (parsed.value != .object) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_edit_simulate: expected JSON object\n");
    }
    const obj = parsed.value.object;

    const file_val = obj.get("file") orelse
        return registry_mod.ToolResult.err(allocator, "zigts_expert_edit_simulate: missing \"file\"\n");
    const content_val = obj.get("content") orelse
        return registry_mod.ToolResult.err(allocator, "zigts_expert_edit_simulate: missing \"content\"\n");
    if (file_val != .string or content_val != .string) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_edit_simulate: \"file\" and \"content\" must be strings\n");
    }

    const before_val = obj.get("before");
    const before: ?[]const u8 = if (before_val) |bv|
        (if (bv == .string) bv.string else null)
    else
        null;

    const input: edit_simulate.EditSimulateInput = .{
        .file = file_val.string,
        .content = content_val.string,
        .before = before,
    };

    var result = try edit_simulate.simulate(allocator, input);
    defer result.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try edit_simulate.writeResultJson(&aw.writer, &result);

    buf = aw.toArrayList();
    return .{
        .ok = result.new_count == 0,
        .llm_text = try buf.toOwnedSlice(allocator),
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "missing arg returns not-ok body" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "requires a JSON input") != null);
}

test "invalid JSON returns not-ok body" {
    var result = try execute(testing.allocator, &.{"{not json"});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "invalid JSON") != null);
}

test "missing file field returns not-ok body" {
    var result = try execute(testing.allocator, &.{"{\"content\":\"x\"}"});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "missing \"file\"") != null);
}

test "clean handler passes the veto" {
    const payload =
        \\{"file":"handler.ts","content":"function handler(req: Request): Response { return Response.json({ok: true}); }"}
    ;
    var result = try execute(testing.allocator, &.{payload});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"total\":0") != null);
}

test "broken handler (var) fails the veto with a new violation" {
    const payload =
        \\{"file":"handler.ts","content":"function handler(req: Request): Response { var x = 1; return Response.json({x}); }"}
    ;
    var result = try execute(testing.allocator, &.{payload});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"ZTS001\"") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"introduced_by_patch\":true") != null);
}

test "pre-existing violation with matching before passes the veto" {
    const payload =
        \\{"file":"handler.ts","content":"function handler(req: Request): Response { var x = 1; return Response.json({x, y: 2}); }","before":"function handler(req: Request): Response { var x = 1; return Response.json({x}); }"}
    ;
    var result = try execute(testing.allocator, &.{payload});
    defer result.deinit(testing.allocator);

    // The edit adds a property but the var is unchanged; the violation key
    // (code + message) matches the baseline so introduced_by_patch == false
    // and new_count == 0 → the veto passes.
    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"introduced_by_patch\":false") != null);
}
