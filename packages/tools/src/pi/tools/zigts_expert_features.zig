//! Both this tool and `zigts features --json` go through
//! `json_diagnostics.writeFeaturesJson`, so the TUI and CLI stay
//! byte-identical.

const std = @import("std");
const json_diagnostics = @import("../../json_diagnostics.zig");
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_features";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "language features",
    .description = "List allowed and blocked JS/TS features with suggested alternatives.",
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    _ = args;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try json_diagnostics.writeFeaturesJson(&aw.writer);

    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "features emits JSON array with at least one allowed and one blocked entry" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expectEqual(@as(u8, '['), result.body[0]);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"status\":\"allowed\"") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"status\":\"blocked\"") != null);
}
