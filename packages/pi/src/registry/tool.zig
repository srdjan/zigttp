//! Tool definition and result types for the in-process tool registry.
//!
//! A ToolDef has two invocation surfaces:
//! - `execute` for direct CLI/TUI dispatch with argv-style slices
//! - `decode_json` + `input_schema` for LLM tool-use
//!
//! This lets the human-facing shell stay ergonomic while the model-facing
//! surface is fully structured and schema-driven.

const std = @import("std");
const ui_payload = @import("../ui_payload.zig");

pub const SessionTreeNode = ui_payload.SessionTreeNode;
pub const SessionTreePayload = ui_payload.SessionTreePayload;
pub const UiPayload = ui_payload.UiPayload;

/// Execution outcome. `llm_text` is the compact provider-facing payload that
/// stays in the transcript. `ui_payload` is optional structured data for the
/// TUI, session replay, RPC, and JSON mode.
pub const ToolResult = struct {
    ok: bool,
    llm_text: []u8,
    ui_payload: ?UiPayload = null,

    pub fn deinit(self: *ToolResult, allocator: std.mem.Allocator) void {
        allocator.free(self.llm_text);
        if (self.ui_payload) |*payload| payload.deinit(allocator);
        self.* = .{ .ok = false, .llm_text = &.{}, .ui_payload = null };
    }

    pub fn err(allocator: std.mem.Allocator, msg: []const u8) !ToolResult {
        return .{
            .ok = false,
            .llm_text = try allocator.dupe(u8, msg),
        };
    }

    pub fn errFmt(
        allocator: std.mem.Allocator,
        comptime fmt: []const u8,
        args: anytype,
    ) !ToolResult {
        return .{
            .ok = false,
            .llm_text = try std.fmt.allocPrint(allocator, fmt, args),
        };
    }

    pub fn withUiPayload(
        allocator: std.mem.Allocator,
        ok: bool,
        llm_text: []const u8,
        payload: UiPayload,
    ) !ToolResult {
        return .{
            .ok = ok,
            .llm_text = try allocator.dupe(u8, llm_text),
            .ui_payload = try payload.clone(allocator),
        };
    }

    pub fn withPlainText(
        allocator: std.mem.Allocator,
        ok: bool,
        llm_text: []const u8,
    ) !ToolResult {
        return withUiPayload(allocator, ok, llm_text, .{
            .plain_text = @constCast(llm_text),
        });
    }

    pub fn withSessionTree(
        allocator: std.mem.Allocator,
        ok: bool,
        llm_text: []const u8,
        nodes: []const SessionTreeNode,
    ) !ToolResult {
        return withUiPayload(allocator, ok, llm_text, .{
            .session_tree = .{ .nodes = @constCast(nodes) },
        });
    }
};

/// Execute signature. Implementations must allocate `ToolResult.llm_text` with
/// the passed allocator; the caller owns it.
const ExecuteFn = *const fn (
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!ToolResult;

const DecodeJsonFn = *const fn (
    allocator: std.mem.Allocator,
    args_json: []const u8,
) anyerror![]const []const u8;

pub const ToolDef = struct {
    name: []const u8,
    label: []const u8,
    description: []const u8,
    input_schema: []const u8,
    decode_json: DecodeJsonFn,
    execute: ExecuteFn,
};

pub fn decodeNoArgs(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    const trimmed = std.mem.trim(u8, args_json, " \t\r\n");
    if (trimmed.len == 0 or std.mem.eql(u8, trimmed, "{}")) return &.{};

    const parsed = std.json.parseFromSliceLeaky(std.json.Value, allocator, trimmed, .{}) catch {
        return error.InvalidToolArgsJson;
    };
    if (parsed != .object or parsed.object.count() != 0) return error.InvalidToolArgsJson;
    return &.{};
}

pub fn decodeSingleStringField(
    allocator: std.mem.Allocator,
    args_json: []const u8,
    field: []const u8,
) ![]const []const u8 {
    const parsed = try parseObject(allocator, args_json);
    const value = parsed.get(field) orelse return error.InvalidToolArgsJson;
    if (value != .string) return error.InvalidToolArgsJson;
    return &.{value.string};
}

pub fn decodeOptionalSingleStringField(
    allocator: std.mem.Allocator,
    args_json: []const u8,
    field: []const u8,
) ![]const []const u8 {
    const parsed = try parseObject(allocator, args_json);
    if (parsed.get(field)) |value| {
        if (value != .string) return error.InvalidToolArgsJson;
        return &.{value.string};
    }
    return &.{};
}

pub fn decodeStringArrayField(
    allocator: std.mem.Allocator,
    args_json: []const u8,
    field: []const u8,
) ![]const []const u8 {
    const parsed = try parseObject(allocator, args_json);
    const value = parsed.get(field) orelse return error.InvalidToolArgsJson;
    if (value != .array) return error.InvalidToolArgsJson;

    const out = try allocator.alloc([]const u8, value.array.items.len);
    for (value.array.items, 0..) |item, i| {
        if (item != .string) return error.InvalidToolArgsJson;
        out[i] = item.string;
    }
    return out;
}

pub fn decodeJsonPassthrough(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    const trimmed = std.mem.trim(u8, args_json, " \t\r\n");
    if (trimmed.len == 0) return error.InvalidToolArgsJson;
    return &.{try allocator.dupe(u8, trimmed)};
}

fn parseObject(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) !std.json.ObjectMap {
    const trimmed = std.mem.trim(u8, args_json, " \t\r\n");
    if (trimmed.len == 0) return error.InvalidToolArgsJson;
    const parsed = std.json.parseFromSliceLeaky(std.json.Value, allocator, trimmed, .{}) catch {
        return error.InvalidToolArgsJson;
    };
    if (parsed != .object) return error.InvalidToolArgsJson;
    return parsed.object;
}

const testing = std.testing;

test "withSessionTree duplicates llm_text and payload nodes" {
    const nodes = [_]SessionTreeNode{
        .{
            .session_id = @constCast("root"),
            .parent_id = null,
            .created_at_unix_ms = 123,
            .depth = 0,
            .is_current = false,
            .is_orphan_root = false,
        },
        .{
            .session_id = @constCast("child"),
            .parent_id = @constCast("root"),
            .created_at_unix_ms = 456,
            .depth = 1,
            .is_current = true,
            .is_orphan_root = false,
        },
    };

    var result = try ToolResult.withSessionTree(testing.allocator, true, "body\n", &nodes);
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expectEqualStrings("body\n", result.llm_text);
    try testing.expect(result.ui_payload != null);
    switch (result.ui_payload.?) {
        .session_tree => |payload| {
            try testing.expectEqual(@as(usize, 2), payload.nodes.len);
            try testing.expectEqualStrings("root", payload.nodes[0].session_id);
            try testing.expectEqualStrings("child", payload.nodes[1].session_id);
            try testing.expect(payload.nodes[1].is_current);
        },
        else => return error.TestFailed,
    }
}
