const std = @import("std");

pub const index_html = "";

pub const Diagnostic = struct {
    code: []u8,
    severity: []u8,
    file: []u8,
    line: u32,
    column: u16,
    message: []u8,
    suggestion: ?[]u8,

    pub fn deinit(self: *Diagnostic, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }
};

pub const State = struct {
    pub fn init(allocator: std.mem.Allocator, handler_path: []const u8) !State {
        _ = allocator;
        _ = handler_path;
        return .{};
    }

    pub fn deinit(self: *State) void {
        _ = self;
    }

    pub fn updateChecking(self: *State) void {
        _ = self;
    }

    pub fn updateError(self: *State, message: []const u8) void {
        _ = self;
        _ = message;
    }

    pub fn updateDiagnostics(self: *State, message: []const u8, diagnostics: []const Diagnostic) void {
        _ = self;
        _ = message;
        _ = diagnostics;
    }

    pub fn stateJsonCopy(self: *State, allocator: std.mem.Allocator) ![]u8 {
        _ = self;
        return try allocator.dupe(u8, "{\"status\":\"disabled\"}\n");
    }

    pub fn generatedTests(self: *State, allocator: std.mem.Allocator) ![]u8 {
        _ = self;
        return try allocator.dupe(u8, "");
    }

    pub fn witnessDetailJson(self: *State, allocator: std.mem.Allocator, key: []const u8) anyerror![]u8 {
        _ = self;
        _ = allocator;
        _ = key;
        return error.WitnessNotFound;
    }
};

pub fn isStudioPath(path: []const u8) bool {
    _ = path;
    return false;
}

pub fn witnessDetailKey(path: []const u8) ?[]const u8 {
    _ = path;
    return null;
}

pub const sse_path = "/_zigttp/studio/events";

pub fn upgradeToSse(state: *State, fd: std.posix.fd_t, allocator: std.mem.Allocator) !void {
    _ = state;
    _ = fd;
    _ = allocator;
    return error.StudioDisabled;
}
