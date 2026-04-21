//! Compile-time model registry. Lists known provider models with their
//! display names and capability flags. The active model is selected at
//! build time via the default in `request.zig`; `/model` and `--model`
//! switch the active model for the current session.

pub const Model = struct {
    id: []const u8,
    display_name: []const u8,
    /// Maximum context window in tokens (approximate).
    context_window: u32,
    /// Maximum output tokens per request.
    max_output_tokens: u32,
};

pub const registry = [_]Model{
    .{
        .id = "claude-opus-4-6",
        .display_name = "Claude Opus 4.6",
        .context_window = 200_000,
        .max_output_tokens = 32_000,
    },
    .{
        .id = "claude-sonnet-4-6",
        .display_name = "Claude Sonnet 4.6",
        .context_window = 200_000,
        .max_output_tokens = 64_000,
    },
    .{
        .id = "claude-haiku-4-5-20251001",
        .display_name = "Claude Haiku 4.5",
        .context_window = 200_000,
        .max_output_tokens = 8_192,
    },
};

pub fn findById(id: []const u8) ?*const Model {
    for (&registry) |*m| {
        if (std.mem.eql(u8, m.id, id)) return m;
    }
    return null;
}

const std = @import("std");
