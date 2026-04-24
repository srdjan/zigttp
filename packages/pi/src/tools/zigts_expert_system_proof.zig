const std = @import("std");
const proof_enrichment = @import("../proof_enrichment.zig");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const name = "zigts_expert_system_proof";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "system proof",
    .description = "Run cross-handler system linking proof and surface proof_level plus cross-boundary safety properties.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"system\":{\"type\":\"string\"},\"paths\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}}}",
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

    const explicit_system: ?[]const u8 = if (obj.get("system")) |value|
        switch (value) {
            .null => null,
            .string => value.string,
            else => return registry_mod.ToolResult.err(allocator, name ++ ": \"system\" must be a string when present\n"),
        }
    else
        null;

    const discovery_hint: ?[]const u8 = if (obj.get("paths")) |value|
        switch (value) {
            .null => null,
            .array => if (value.array.items.len == 0)
                null
            else blk: {
                if (value.array.items[0] != .string) {
                    return registry_mod.ToolResult.err(allocator, name ++ ": \"paths\" items must be strings\n");
                }
                break :blk value.array.items[0].string;
            },
            else => return registry_mod.ToolResult.err(allocator, name ++ ": \"paths\" must be an array when present\n"),
        }
    else
        null;

    const workspace_root = try common.workspaceRoot(allocator);
    defer allocator.free(workspace_root);

    var summary = try proof_enrichment.loadSystemSummary(
        allocator,
        workspace_root,
        explicit_system,
        discovery_hint,
    );
    defer if (summary) |*value| value.deinit(allocator);

    const llm_text = try proof_enrichment.formatSystemSummary(allocator, summary);
    defer allocator.free(llm_text);
    return registry_mod.ToolResult.withPlainText(allocator, summary != null, llm_text);
}

const testing = std.testing;

test "system_proof returns not-ok when no system config is discoverable" {
    var result = try execute(testing.allocator, &.{"{}"});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "unavailable") != null);
}
