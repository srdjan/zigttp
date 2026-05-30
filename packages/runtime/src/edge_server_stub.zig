//! No-op stand-in for edge_server.zig when the edge runtime is compiled out
//! (built without -Dedge). Mirrors the public surface that runtime_cli.edgeCommand
//! references so the dev CLI links; the command short-circuits on
//! `feature_options.enable_edge` before any of these are reached.

const std = @import("std");

pub const EdgeConfig = struct {
    pub fn deinit(self: *EdgeConfig, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }
};

pub fn loadConfig(allocator: std.mem.Allocator, path: []const u8) !EdgeConfig {
    _ = allocator;
    _ = path;
    return .{};
}

pub const EdgeServer = struct {
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, config: EdgeConfig) !Self {
        _ = allocator;
        _ = config;
        return .{};
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn run(self: *Self) !void {
        _ = self;
    }
};
