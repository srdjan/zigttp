const std = @import("std");
const proof_enrichment = @import("../proof_enrichment.zig");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const name = "zigts_expert_prove_patch";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "prove patch",
    .description = "Classify a before/after handler contract pair as equivalent, additive, or breaking and surface any counterexample.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"file\":{\"type\":\"string\"},\"before\":{\"type\":\"string\"},\"after\":{\"type\":\"string\"}},\"required\":[\"file\",\"after\"]}",
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, name ++ ": requires a JSON input argument\n");
    }

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
        return registry_mod.ToolResult.err(allocator, name ++ ": invalid JSON input\n");
    };
    defer parsed.deinit();

    if (parsed.value != .object) {
        return registry_mod.ToolResult.err(allocator, name ++ ": expected JSON object\n");
    }
    const obj = parsed.value.object;

    const file_val = obj.get("file") orelse
        return registry_mod.ToolResult.err(allocator, name ++ ": missing \"file\"\n");
    const after_val = obj.get("after") orelse
        return registry_mod.ToolResult.err(allocator, name ++ ": missing \"after\"\n");
    if (file_val != .string or after_val != .string) {
        return registry_mod.ToolResult.err(allocator, name ++ ": \"file\" and \"after\" must be strings\n");
    }

    const before: ?[]const u8 = if (obj.get("before")) |value|
        switch (value) {
            .null => null,
            .string => value.string,
            else => return registry_mod.ToolResult.err(allocator, name ++ ": \"before\" must be a string when present\n"),
        }
    else
        null;

    const workspace_root = try common.workspaceRoot(allocator);
    defer allocator.free(workspace_root);

    var summary = try proof_enrichment.loadProveSummaryForPatch(
        allocator,
        workspace_root,
        file_val.string,
        before,
        after_val.string,
    );
    defer if (summary) |*value| value.deinit(allocator);

    const llm_text = try proof_enrichment.formatProveSummary(allocator, summary);
    defer allocator.free(llm_text);
    return registry_mod.ToolResult.withPlainText(allocator, summary != null, llm_text);
}

const testing = std.testing;

test "prove_patch returns additive classification for widened handler" {
    const payload =
        \\{"file":"handler.ts","before":"function handler(req: Request): Response { return Response.json({ ok: true }); }","after":"import { env } from \"zigttp:env\";\nfunction handler(req: Request): Response { return Response.json({ ok: true, region: env(\"REGION\") }); }"}
    ;
    var result = try execute(testing.allocator, &.{payload});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "classification: additive") != null);
}
