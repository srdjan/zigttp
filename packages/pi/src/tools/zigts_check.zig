const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const name = "zigts_check";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "zigts check",
    .description = "Run `zigts check --json` for a handler path through the local CLI.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"]}",
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    return registry_mod.helpers.decodeSingleStringField(allocator, args_json, "path");
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) return registry_mod.ToolResult.err(allocator, name ++ ": requires a path\n");

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, args[0]);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    const argv = [_][]const u8{ "zig", "build", "cli", "--", "check", relative, "--json" };
    var outcome = try common.runCommand(allocator, root, &argv);
    defer outcome.deinit(allocator);
    return try common.commandOutcomeToToolResult(allocator, &argv, &outcome);
}
