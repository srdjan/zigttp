const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const name = "zig_test_step";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "zig test",
    .description = "Run a safe `zig build <test-step>` invocation in the repo root.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"step\":{\"type\":\"string\"}},\"required\":[]}",
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    return registry_mod.helpers.decodeOptionalSingleStringField(allocator, args_json, "step");
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    const step = if (args.len == 0) "test" else args[0];
    if (!isSafeTestStep(step)) return registry_mod.ToolResult.err(allocator, name ++ ": invalid test step\n");

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const argv = [_][]const u8{ "zig", "build", step };
    var outcome = try common.runCommand(allocator, root, &argv);
    defer outcome.deinit(allocator);
    return try common.commandOutcomeToToolResult(allocator, &argv, &outcome);
}

fn isSafeTestStep(step: []const u8) bool {
    if (!std.mem.eql(u8, step, "test") and !std.mem.startsWith(u8, step, "test-")) return false;
    for (step) |ch| {
        if (!(std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '_')) return false;
    }
    return true;
}
