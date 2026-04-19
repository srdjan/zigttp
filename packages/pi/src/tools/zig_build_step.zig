const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const name = "zig_build_step";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "zig build",
    .description = "Run a safe `zig build <step>` invocation in the repo root.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"step\":{\"type\":\"string\"}},\"required\":[\"step\"]}",
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    return registry_mod.helpers.decodeSingleStringField(allocator, args_json, "step");
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) return registry_mod.ToolResult.err(allocator, name ++ ": requires a build step\n");
    if (!isSafeStep(args[0])) return registry_mod.ToolResult.err(allocator, name ++ ": invalid build step\n");

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const argv = [_][]const u8{ "zig", "build", args[0] };
    var outcome = try common.runCommand(allocator, root, &argv);
    defer outcome.deinit(allocator);
    return try common.commandOutcomeToToolResult(allocator, &argv, &outcome);
}

fn isSafeStep(step: []const u8) bool {
    if (step.len == 0) return false;
    for (step) |ch| {
        if (!(std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '_')) return false;
    }
    return true;
}
