//! Both this tool and `zigts modules --json` go through
//! `json_diagnostics.writeModulesJson`, so the TUI and CLI stay
//! byte-identical.

const std = @import("std");
const json_diagnostics = @import("../../json_diagnostics.zig");
const registry_mod = @import("../registry/registry.zig");

const name = "zigts_expert_modules";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "virtual modules",
    .description = "List built-in zigttp:* virtual modules and their exports.",
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

    try json_diagnostics.writeModulesJson(&aw.writer);

    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "modules emits JSON array with at least the env module" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expectEqual(@as(u8, '['), result.body[0]);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"specifier\":\"zigttp:env\"") != null);
    try testing.expect(std.mem.indexOf(u8, result.body, "\"exports\":") != null);
}
