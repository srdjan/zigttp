//! `ToolResult.ok` mirrors the v1 envelope's `ok` field so callers can branch
//! on the tool result without parsing the body.

const std = @import("std");
const verify_paths_core = @import("../../verify_paths_core.zig");
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_verify_paths";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "verify handler(s)",
    .description = "Run full analysis on one or more handler files and emit the v1 envelope.",
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_verify_paths requires at least one path\n");
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    const outcome = try verify_paths_core.writeJsonEnvelope(allocator, &aw.writer, args);

    buf = aw.toArrayList();
    return .{
        .ok = outcome.ok,
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

test "execute on a file that does not exist emits ZTS000 envelope" {
    var result = try execute(testing.allocator, &.{"/tmp/zigts-pi-does-not-exist.ts"});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"ok\":false") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"ZTS000\"") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"policy_version\":\"2026.04.2\"") != null);
}
