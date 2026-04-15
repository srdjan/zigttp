//! Edit-simulate with an optional `diff_only` filter matching the CLI's
//! `zigts review-patch --diff-only` semantics. `ToolResult.ok` is true iff no
//! *new* violations were introduced — the same veto signal as
//! `zigts_expert_edit_simulate`, regardless of the filter.
//!
//! Wire format for args[0]: {"file", "content", "before"?, "diff_only"?}.

const std = @import("std");
const edit_simulate = @import("zigts_cli").edit_simulate;
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_review_patch";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "review patch",
    .description = "Simulate an edit with optional diff_only filter; report new vs preexisting violations.",
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_review_patch requires a JSON input argument\n");
    }

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_review_patch: invalid JSON input\n");
    };
    defer parsed.deinit();

    if (parsed.value != .object) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_review_patch: expected JSON object\n");
    }
    const obj = parsed.value.object;

    const file_val = obj.get("file") orelse
        return registry_mod.ToolResult.err(allocator, "zigts_expert_review_patch: missing \"file\"\n");
    const content_val = obj.get("content") orelse
        return registry_mod.ToolResult.err(allocator, "zigts_expert_review_patch: missing \"content\"\n");
    if (file_val != .string or content_val != .string) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_review_patch: \"file\" and \"content\" must be strings\n");
    }

    const before: ?[]const u8 = if (obj.get("before")) |bv|
        (if (bv == .string) bv.string else null)
    else
        null;
    const diff_only: bool = if (obj.get("diff_only")) |dv|
        (dv == .bool and dv.bool)
    else
        false;

    const input: edit_simulate.EditSimulateInput = .{
        .file = file_val.string,
        .content = content_val.string,
        .before = before,
    };

    var result = try edit_simulate.simulate(allocator, input);
    defer result.deinit(allocator);

    if (diff_only) {
        var write_idx: usize = 0;
        var new_count: u32 = 0;
        for (result.violations.items) |v| {
            if (v.introduced_by_patch) {
                result.violations.items[write_idx] = v;
                write_idx += 1;
                new_count += 1;
            }
        }
        result.violations.items.len = write_idx;
        result.total = new_count;
        result.new_count = new_count;
        result.preexisting_count = 0;
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try edit_simulate.writeResultJson(&aw.writer, &result);

    buf = aw.toArrayList();
    return .{
        .ok = result.new_count == 0,
        .body = try buf.toOwnedSlice(allocator),
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
    try testing.expect(std.mem.indexOf(u8, result.body, "requires a JSON input") != null);
}

test "clean handler passes" {
    const payload =
        \\{"file":"handler.ts","content":"function handler(req: Request): Response { return Response.json({ok: true}); }"}
    ;
    var result = try execute(testing.allocator, &.{payload});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"total\":0") != null);
}

test "diff_only filter drops preexisting violations from body and summary" {
    const payload =
        \\{"file":"handler.ts","content":"function handler(req: Request): Response { var x = 1; return Response.json({x, y: 2}); }","before":"function handler(req: Request): Response { var x = 1; return Response.json({x}); }","diff_only":true}
    ;
    var result = try execute(testing.allocator, &.{payload});
    defer result.deinit(testing.allocator);

    // The `var` violation is preexisting (matches baseline), so diff_only
    // strips it. With no new violations, the tool reports ok and an empty
    // violations array.
    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"violations\":[]") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"preexisting\":0") != null);
}

test "diff_only flag off keeps preexisting in body but still passes veto" {
    const payload =
        \\{"file":"handler.ts","content":"function handler(req: Request): Response { var x = 1; return Response.json({x, y: 2}); }","before":"function handler(req: Request): Response { var x = 1; return Response.json({x}); }"}
    ;
    var result = try execute(testing.allocator, &.{payload});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"introduced_by_patch\":false") != null);
}
