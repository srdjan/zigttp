//! pi_apply_feature_plan - explicitly write an approved Route Forge candidate.

const std = @import("std");
const zigts = @import("zigts");
const edit_simulate = @import("zigts_cli").edit_simulate;
const registry_mod = @import("../registry/registry.zig");
const proof_enrichment = @import("../proof_enrichment.zig");
const ui_payload = @import("../ui_payload.zig");
const common = @import("common.zig");

const name = "pi_apply_feature_plan";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "apply-feature-plan",
    .description =
    \\Write an approved pi_feature_plan candidate to disk after rerunning
    \\the compiler veto against the current file contents. The input is
    \\{ "file": "...", "proposed_content": "..." }. The tool refuses to
    \\write if the candidate introduces new compiler violations.
    ,
    .input_schema =
    \\{"type":"object","properties":{"file":{"type":"string"},"proposed_content":{"type":"string"}},"required":["file","proposed_content"]}
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
    const out = try allocator.alloc([]const u8, 1);
    out[0] = try allocator.dupe(u8, trimmed);
    return out;
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) return registry_mod.ToolResult.err(allocator, name ++ ": requires a JSON input argument\n");

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
        return registry_mod.ToolResult.err(allocator, name ++ ": invalid JSON input\n");
    };
    defer parsed.deinit();
    if (parsed.value != .object) return registry_mod.ToolResult.err(allocator, name ++ ": expected JSON object\n");
    const obj = parsed.value.object;
    const file = obj.get("file") orelse return registry_mod.ToolResult.err(allocator, name ++ ": missing \"file\"\n");
    const proposed = obj.get("proposed_content") orelse return registry_mod.ToolResult.err(allocator, name ++ ": missing \"proposed_content\"\n");
    if (file != .string or proposed != .string) return registry_mod.ToolResult.err(allocator, name ++ ": file and proposed_content must be strings\n");

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, file.string);
    defer allocator.free(absolute);
    const before = zigts.file_io.readFile(allocator, absolute, common.default_output_limit) catch |e| {
        return registry_mod.ToolResult.errFmt(allocator, name ++ ": failed to read {s}: {s}\n", .{ absolute, @errorName(e) });
    };
    defer allocator.free(before);

    var result = try edit_simulate.simulate(allocator, .{
        .file = file.string,
        .content = proposed.string,
        .before = before,
    });
    defer result.deinit(allocator);
    if (result.new_count != 0) {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": refused write; candidate introduces {d} new compiler violation(s)\n",
            .{result.new_count},
        );
    }

    try zigts.file_io.writeFile(allocator, absolute, proposed.string);

    const policy_hash = zigts.rule_registry.policyHash();
    var patch_payload: ui_payload.UiPayload = .{ .verified_patch = try proof_enrichment.buildVerifiedPatchPayload(
        allocator,
        .{
            .workspace_root = root,
            .file = common.relativeToRoot(root, absolute),
            .before = before,
            .after = proposed.string,
            .policy_hash = policy_hash[0..],
            .applied_at_unix_ms = common.nowUnixMs(),
            .post_apply_ok = true,
            .post_apply_summary = "feature plan applied through compiler veto",
        },
    ) };
    defer patch_payload.deinit(allocator);

    return registry_mod.ToolResult.withUiPayload(
        allocator,
        true,
        "feature plan applied through compiler veto\n",
        patch_payload,
    );
}
