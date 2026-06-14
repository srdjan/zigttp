const std = @import("std");

pub const index_html = "";
pub const demo_state_path = "/_zigttp/studio/demo/state.json";
pub const demo_action_path = "/_zigttp/studio/demo/action";
pub const caller_verify_path = "/_zigttp/studio/caller-verify.json";

pub const DemoAction = enum { introduce_bug, repair_bug, deploy, reset };

pub const DemoConfig = struct {
    workspace_root: []const u8,
};

pub const CallerReceiptInput = struct {
    proofs_header_value: []const u8,
    attest_header_value: []const u8,
    key_fingerprint_hex: []const u8,
    host: []const u8,
    port: u16,
};

pub const Diagnostic = struct {
    code: []u8,
    severity: []u8,
    file: []u8,
    line: u32,
    column: u32,
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

    pub fn initDemo(allocator: std.mem.Allocator, handler_path: []const u8, config: DemoConfig) !State {
        _ = allocator;
        _ = handler_path;
        _ = config;
        return .{};
    }

    pub fn deinit(self: *State) void {
        _ = self;
    }

    pub fn setCallerReceipt(self: *State, input: CallerReceiptInput) !void {
        _ = self;
        _ = input;
    }

    pub fn clearCallerReceipt(self: *State) void {
        _ = self;
    }

    pub fn updateChecking(self: *State) void {
        _ = self;
    }

    pub fn updateFacts(self: *State, update: anytype) void {
        _ = self;
        _ = update;
    }

    pub fn broadcast(self: *State) void {
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

    pub fn callerVerifyJsonCopy(self: *State, allocator: std.mem.Allocator) anyerror![]u8 {
        _ = self;
        _ = allocator;
        return error.CallerReceiptUnavailable;
    }

    pub fn witnessDetailJson(self: *State, allocator: std.mem.Allocator, key: []const u8) anyerror![]u8 {
        _ = self;
        _ = allocator;
        _ = key;
        return error.WitnessNotFound;
    }

    pub fn demoStateJsonCopy(self: *State, allocator: std.mem.Allocator) ![]u8 {
        _ = self;
        _ = allocator;
        return error.DemoDisabled;
    }

    pub fn applyDemoAction(self: *State, allocator: std.mem.Allocator, action: DemoAction) ![]u8 {
        _ = self;
        _ = allocator;
        _ = action;
        return error.DemoDisabled;
    }
};

pub fn parseDemoAction(allocator: std.mem.Allocator, body: ?[]const u8) !DemoAction {
    _ = allocator;
    _ = body;
    return error.DemoDisabled;
}

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
