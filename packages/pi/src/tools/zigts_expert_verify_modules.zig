//! Both this tool and `zigts verify-modules --json` go through
//! `module_audit.writeJsonEnvelope`, so the TUI and CLI stay byte-identical.
//!
//! `ToolResult.ok` mirrors the v1 envelope's `ok` field so callers can branch
//! on the tool result without parsing the body.

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;
const module_audit = @import("zigts_cli").module_audit;
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_verify_modules";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "verify module(s)",
    .description = "Audit built-in virtual module files for capability and effect discipline.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"paths\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"builtins\":{\"type\":\"boolean\"},\"strict\":{\"type\":\"boolean\"}},\"required\":[]}",
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    var strict = false;
    var builtins = false;
    var paths = args;

    if (args.len == 1 and args[0].len > 0 and args[0][0] == '{') {
        var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
            return registry_mod.ToolResult.err(allocator, "zigts_expert_verify_modules: invalid JSON input\n");
        };
        defer parsed.deinit();
        if (parsed.value != .object) {
            return registry_mod.ToolResult.err(allocator, "zigts_expert_verify_modules: expected JSON object\n");
        }
        const obj = parsed.value.object;
        strict = if (obj.get("strict")) |value| value == .bool and value.bool else false;
        builtins = if (obj.get("builtins")) |value| value == .bool and value.bool else false;

        if (builtins) {
            var result = try module_audit.verifyBuiltins(allocator, .{ .strict = strict });
            defer result.deinit(allocator);

            var buf: std.ArrayList(u8) = .empty;
            defer buf.deinit(allocator);
            var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

            const hash = rule_registry.policyHash();
            try module_audit.writeJsonEnvelope(&aw.writer, &result, hash);

            buf = aw.toArrayList();
            return .{
                .ok = !result.hasErrors(),
                .body = try buf.toOwnedSlice(allocator),
            };
        }

        const paths_val = obj.get("paths") orelse
            return registry_mod.ToolResult.err(allocator, "zigts_expert_verify_modules requires at least one path or builtins=true\n");
        if (paths_val != .array or paths_val.array.items.len == 0) {
            return registry_mod.ToolResult.err(allocator, "zigts_expert_verify_modules requires at least one path or builtins=true\n");
        }
        const parsed_paths = try allocator.alloc([]const u8, paths_val.array.items.len);
        for (paths_val.array.items, 0..) |item, i| {
            if (item != .string) {
                return registry_mod.ToolResult.err(allocator, "zigts_expert_verify_modules: paths must be strings\n");
            }
            parsed_paths[i] = item.string;
        }
        paths = parsed_paths;
    }

    if (paths.len == 0) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_verify_modules requires at least one path\n");
    }

    var result = try module_audit.verifyPaths(allocator, paths, .{ .strict = strict });
    defer result.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    const hash = rule_registry.policyHash();
    try module_audit.writeJsonEnvelope(&aw.writer, &result, hash);

    buf = aw.toArrayList();
    return .{
        .ok = !result.hasErrors(),
        .body = try buf.toOwnedSlice(allocator),
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "missing args returns not-ok body" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "requires at least one path") != null);
}

test "nonexistent module path produces a v1 envelope" {
    var result = try execute(testing.allocator, &.{"/tmp/zigts-pi-not-a-module.zig"});
    defer result.deinit(testing.allocator);

    // The audit reports "unknown virtual module file" or similar, but the
    // envelope shape must match regardless of the specific diagnostic.
    try testing.expect(std.mem.indexOf(u8, result.body, "\"policy_version\":\"2026.04.2\"") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"checked_files\":") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"violations\":") != null);
}
