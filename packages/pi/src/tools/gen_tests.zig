const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");

const name = "workspace_gen_tests";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "gen tests",
    .description =
    \\Generate a JSONL test suite from the handler's compiler-proven behavioral
    \\paths and write it alongside the handler (e.g. handler.test.jsonl). Each
    \\proven execution path becomes one test case: a synthesized request, ordered
    \\I/O stubs, and expected response status. Call this after editing a handler
    \\to keep the test suite in sync with the implementation. The generated file
    \\is immediately runnable via `zig build run -- handler.ts --test file.jsonl`.
    ,
    .input_schema =
    \\{"type":"object","properties":{"handler_path":{"type":"string","description":"Path to the handler file"},"output_path":{"type":"string","description":"Path for the JSONL output. Defaults to the handler stem + .test.jsonl in the same directory."}},"required":["handler_path"]}
    ,
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    const trimmed = std.mem.trim(u8, args_json, " \t\r\n");
    if (trimmed.len == 0) return error.InvalidToolArgsJson;
    const parsed = std.json.parseFromSliceLeaky(std.json.Value, allocator, trimmed, .{}) catch {
        return error.InvalidToolArgsJson;
    };
    if (parsed != .object) return error.InvalidToolArgsJson;
    const obj = parsed.object;

    const handler_val = obj.get("handler_path") orelse return error.InvalidToolArgsJson;
    if (handler_val != .string) return error.InvalidToolArgsJson;

    if (obj.get("output_path")) |output_val| {
        if (output_val != .string) return error.InvalidToolArgsJson;
        const out = try allocator.alloc([]const u8, 2);
        out[0] = handler_val.string;
        out[1] = output_val.string;
        return out;
    }
    return &.{handler_val.string};
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) return registry_mod.ToolResult.err(allocator, name ++ ": requires handler_path\n");

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, args[0]);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    const has_explicit_output = args.len > 1 and args[1].len > 0;
    var output_path_owned: ?[]u8 = null;
    defer if (output_path_owned) |p| allocator.free(p);

    const output_path: []const u8 = if (has_explicit_output) args[1] else blk: {
        const ext = std.fs.path.extension(relative);
        const stem = relative[0..relative.len - ext.len];
        const owned = try std.fmt.allocPrint(allocator, "{s}.test.jsonl", .{stem});
        output_path_owned = owned;
        break :blk owned;
    };

    const argv = [_][]const u8{ "zig", "build", "cli", "--", "gen-tests", relative, "-o", output_path };
    var outcome = try common.runCommand(allocator, root, &argv);
    defer outcome.deinit(allocator);
    return try common.commandOutcomeToToolResult(allocator, &argv, &outcome);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "tool description mentions behavioral paths" {
    try testing.expect(std.mem.indexOf(u8, tool.description, "behavioral") != null);
}

test "missing args returns not-ok" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "requires handler_path") != null);
}
