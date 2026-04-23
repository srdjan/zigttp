//! Tool definition and result types for the in-process tool registry.
//!
//! A ToolDef has two invocation surfaces:
//! - `execute` for direct CLI/TUI dispatch with argv-style slices
//! - `decode_json` + `input_schema` for LLM tool-use
//!
//! This lets the human-facing shell stay ergonomic while the model-facing
//! surface is fully structured and schema-driven.

const std = @import("std");

pub const SessionTreeNode = struct {
    session_id: []u8,
    parent_id: ?[]u8,
    created_at_unix_ms: i64,
    depth: usize,
    is_current: bool,
    is_orphan_root: bool,

    pub fn deinit(self: *SessionTreeNode, allocator: std.mem.Allocator) void {
        allocator.free(self.session_id);
        if (self.parent_id) |parent_id| allocator.free(parent_id);
        self.* = .{
            .session_id = &.{},
            .parent_id = null,
            .created_at_unix_ms = 0,
            .depth = 0,
            .is_current = false,
            .is_orphan_root = false,
        };
    }
};

pub const SessionTreePayload = struct {
    nodes: []SessionTreeNode,

    pub fn deinit(self: *SessionTreePayload, allocator: std.mem.Allocator) void {
        for (self.nodes) |*node| node.deinit(allocator);
        allocator.free(self.nodes);
        self.* = .{ .nodes = &.{} };
    }
};

pub const UiPayload = union(enum) {
    session_tree: SessionTreePayload,

    pub fn deinit(self: *UiPayload, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .session_tree => |*payload| payload.deinit(allocator),
        }
    }
};

/// Execution outcome. Body is allocated with the same allocator passed to
/// execute() and is owned by the caller; free it with deinit().
pub const ToolResult = struct {
    ok: bool,
    body: []u8,
    ui_payload: ?UiPayload = null,

    pub fn deinit(self: *ToolResult, allocator: std.mem.Allocator) void {
        allocator.free(self.body);
        if (self.ui_payload) |*payload| payload.deinit(allocator);
        self.body = &.{};
        self.ui_payload = null;
    }

    /// Build a not-ok result whose body is an owned copy of `msg`. Every
    /// tool's argument-validation branch ends here.
    pub fn err(allocator: std.mem.Allocator, msg: []const u8) !ToolResult {
        const owned = try allocator.dupe(u8, msg);
        return .{ .ok = false, .body = owned };
    }

    pub fn errFmt(
        allocator: std.mem.Allocator,
        comptime fmt: []const u8,
        args: anytype,
    ) !ToolResult {
        return .{
            .ok = false,
            .body = try std.fmt.allocPrint(allocator, fmt, args),
        };
    }

    pub fn withSessionTree(
        allocator: std.mem.Allocator,
        ok: bool,
        body: []const u8,
        nodes: []const SessionTreeNode,
    ) !ToolResult {
        const owned_body = try allocator.dupe(u8, body);
        errdefer allocator.free(owned_body);

        const owned_nodes = try allocator.alloc(SessionTreeNode, nodes.len);
        errdefer {
            var i: usize = 0;
            while (i < nodes.len) : (i += 1) {
                owned_nodes[i].deinit(allocator);
            }
            allocator.free(owned_nodes);
        }
        for (owned_nodes) |*node| {
            node.* = .{
                .session_id = &.{},
                .parent_id = null,
                .created_at_unix_ms = 0,
                .depth = 0,
                .is_current = false,
                .is_orphan_root = false,
            };
        }

        for (nodes, 0..) |node, i| {
            owned_nodes[i] = .{
                .session_id = try allocator.dupe(u8, node.session_id),
                .parent_id = if (node.parent_id) |parent_id|
                    try allocator.dupe(u8, parent_id)
                else
                    null,
                .created_at_unix_ms = node.created_at_unix_ms,
                .depth = node.depth,
                .is_current = node.is_current,
                .is_orphan_root = node.is_orphan_root,
            };
        }

        return .{
            .ok = ok,
            .body = owned_body,
            .ui_payload = .{ .session_tree = .{ .nodes = owned_nodes } },
        };
    }
};

/// Execute signature. Implementations must allocate `ToolResult.body` with the
/// passed allocator; the caller owns it.
const ExecuteFn = *const fn (
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!ToolResult;

const DecodeJsonFn = *const fn (
    allocator: std.mem.Allocator,
    args_json: []const u8,
) anyerror![]const []const u8;

pub const ToolDef = struct {
    /// Stable snake_case identifier. Matches the eventual LLM tool name.
    name: []const u8,
    /// Short human label for rendering in a menu.
    label: []const u8,
    /// One-line description; displayed inline with the label.
    description: []const u8,
    /// Anthropic Messages API `input_schema` JSON literal.
    input_schema: []const u8,
    /// Structured JSON decoder used by the model-facing registry surface.
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

test "withSessionTree duplicates body and payload nodes" {
    var root = [_]u8{ 'r', 'o', 'o', 't' };
    var child = [_]u8{ 'c', 'h', 'i', 'l', 'd' };
    const nodes = [_]SessionTreeNode{
        .{
            .session_id = root[0..],
            .parent_id = null,
            .created_at_unix_ms = 123,
            .depth = 0,
            .is_current = false,
            .is_orphan_root = false,
        },
        .{
            .session_id = child[0..],
            .parent_id = root[0..],
            .created_at_unix_ms = 456,
            .depth = 1,
            .is_current = true,
            .is_orphan_root = false,
        },
    };

    var result = try ToolResult.withSessionTree(testing.allocator, true, "body\n", &nodes);
    defer result.deinit(testing.allocator);

    try testing.expect(result.ok);
    try testing.expectEqualStrings("body\n", result.body);
    try testing.expect(result.ui_payload != null);
    switch (result.ui_payload.?) {
        .session_tree => |payload| {
            try testing.expectEqual(@as(usize, 2), payload.nodes.len);
            try testing.expectEqualStrings("root", payload.nodes[0].session_id);
            try testing.expectEqualStrings("child", payload.nodes[1].session_id);
            try testing.expect(payload.nodes[1].is_current);
        },
    }
}
