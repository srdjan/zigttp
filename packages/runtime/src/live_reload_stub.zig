const std = @import("std");
const Server = @import("server.zig").Server;

pub const Options = struct {
    prove: bool = false,
    force_swap: bool = false,
};

pub const LiveReloadState = struct {
    allocator: std.mem.Allocator,

    pub fn init(
        allocator: std.mem.Allocator,
        server: *Server,
        handler_path: []const u8,
        watch_paths: []const []const u8,
        options: Options,
    ) LiveReloadState {
        _ = server;
        _ = handler_path;
        _ = watch_paths;
        _ = options;
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *LiveReloadState) void {
        _ = self;
    }

    pub fn run(self: *LiveReloadState, io: std.Io) !void {
        _ = self;
        _ = io;
        return error.LiveReloadUnavailable;
    }
};
