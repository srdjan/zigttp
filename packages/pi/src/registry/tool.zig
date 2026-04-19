//! Tool definition and result types for the in-process tool registry.
//!
//! A ToolDef has two invocation surfaces:
//! - `execute` for direct CLI/TUI dispatch with argv-style slices
//! - `decode_json` + `input_schema` for LLM tool-use
//!
//! This lets the human-facing shell stay ergonomic while the model-facing
//! surface is fully structured and schema-driven.

const std = @import("std");

/// Execution outcome. Body is allocated with the same allocator passed to
/// execute() and is owned by the caller; free it with deinit().
pub const ToolResult = struct {
    ok: bool,
    body: []u8,

    pub fn deinit(self: *ToolResult, allocator: std.mem.Allocator) void {
        allocator.free(self.body);
        self.body = &.{};
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
