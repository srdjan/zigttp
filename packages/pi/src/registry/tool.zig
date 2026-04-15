//! Tool definition and result types for the in-process tool registry.
//!
//! A ToolDef is a first-party function the agent (or a keystroke in the phase-1
//! scaffold) can invoke. It carries enough metadata to render in a TUI and
//! eventually enough schema to advertise to an LLM tool-use API. For now the
//! input shape is an opaque argv-style slice; structured parameters land when
//! the registry grows a JSON Schema layer.
//!
//! See docs/zigts-expert-contract.md for the v1 output contract the first
//! first-party tools wrap.

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
};

/// Execute signature. Implementations must allocate `ToolResult.body` with the
/// passed allocator; the caller owns it.
const ExecuteFn = *const fn (
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!ToolResult;

pub const ToolDef = struct {
    /// Stable snake_case identifier. Matches the eventual LLM tool name.
    name: []const u8,
    /// Short human label for rendering in a menu.
    label: []const u8,
    /// One-line description; displayed inline with the label.
    description: []const u8,
    execute: ExecuteFn,
};
