const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const json_writer = @import("../providers/anthropic/json_writer.zig");

pub const default_output_limit: usize = 256 * 1024;

pub const CommandOutcome = struct {
    ok: bool,
    exit_code: ?u8,
    term: []const u8,
    stdout: []u8,
    stderr: []u8,

    pub fn deinit(self: *CommandOutcome, allocator: std.mem.Allocator) void {
        allocator.free(self.term);
        allocator.free(self.stdout);
        allocator.free(self.stderr);
        self.* = .{
            .ok = false,
            .exit_code = null,
            .term = &.{},
            .stdout = &.{},
            .stderr = &.{},
        };
    }
};

pub fn workspaceRoot(allocator: std.mem.Allocator) ![]u8 {
    return try std.fs.path.resolve(allocator, &.{"."});
}

pub fn resolveInsideWorkspace(
    allocator: std.mem.Allocator,
    root: []const u8,
    requested_path: []const u8,
) ![]u8 {
    const resolved = if (std.fs.path.isAbsolute(requested_path))
        try std.fs.path.resolve(allocator, &.{requested_path})
    else
        try std.fs.path.resolve(allocator, &.{ root, requested_path });
    errdefer allocator.free(resolved);

    if (!isPathInsideRoot(root, resolved)) return error.PathOutsideWorkspace;
    return resolved;
}

pub fn relativeToRoot(root: []const u8, absolute: []const u8) []const u8 {
    if (std.mem.eql(u8, root, absolute)) return ".";
    const offset = if (root[root.len - 1] == std.fs.path.sep) root.len else root.len + 1;
    return absolute[offset..];
}

pub fn isPathInsideRoot(root: []const u8, candidate: []const u8) bool {
    if (!std.mem.startsWith(u8, candidate, root)) return false;
    if (candidate.len == root.len) return true;
    return candidate[root.len] == std.fs.path.sep;
}

pub fn runCommand(
    allocator: std.mem.Allocator,
    cwd: []const u8,
    argv: []const []const u8,
) !CommandOutcome {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();

    const run_result = try std.process.run(allocator, io_backend.io(), .{
        .argv = argv,
        .cwd = .{ .path = cwd },
        .stdout_limit = .limited(default_output_limit),
        .stderr_limit = .limited(default_output_limit),
    });
    errdefer allocator.free(run_result.stdout);
    errdefer allocator.free(run_result.stderr);

    const term_text, const exit_code, const ok = switch (run_result.term) {
        .exited => |code| .{ "exited", @as(?u8, code), code == 0 },
        .signal => .{ "signal", null, false },
        .stopped => .{ "stopped", null, false },
        .unknown => .{ "unknown", null, false },
    };

    return .{
        .ok = ok,
        .exit_code = exit_code,
        .term = try allocator.dupe(u8, term_text),
        .stdout = run_result.stdout,
        .stderr = run_result.stderr,
    };
}

pub fn commandOutcomeToToolResult(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    outcome: *const CommandOutcome,
) !registry_mod.ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"ok\":");
    try w.writeAll(if (outcome.ok) "true" else "false");
    try w.writeAll(",\"argv\":[");
    for (argv, 0..) |arg, i| {
        if (i > 0) try w.writeByte(',');
        try json_writer.writeString(w, arg);
    }
    try w.writeAll("],\"term\":");
    try json_writer.writeString(w, outcome.term);
    try w.writeAll(",\"exit_code\":");
    if (outcome.exit_code) |code| {
        try w.print("{d}", .{code});
    } else {
        try w.writeAll("null");
    }
    try w.writeAll(",\"stdout\":");
    try json_writer.writeString(w, outcome.stdout);
    try w.writeAll(",\"stderr\":");
    try json_writer.writeString(w, outcome.stderr);
    try w.writeAll("}\n");

    buf = aw.toArrayList();
    return .{
        .ok = outcome.ok,
        .body = try buf.toOwnedSlice(allocator),
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "isPathInsideRoot: exact match is inside" {
    try testing.expect(isPathInsideRoot("/a/b", "/a/b"));
}

test "isPathInsideRoot: strict child is inside" {
    try testing.expect(isPathInsideRoot("/a/b", "/a/b/c"));
    try testing.expect(isPathInsideRoot("/a/b", "/a/b/c/d.txt"));
}

test "isPathInsideRoot: prefix-only collision is rejected" {
    // `/a/bar` must not be considered inside `/a/b`; startsWith alone
    // would wrongly accept it. The separator check at root.len guards this.
    try testing.expect(!isPathInsideRoot("/a/b", "/a/bar"));
    try testing.expect(!isPathInsideRoot("/a/b", "/a/bc"));
}

test "isPathInsideRoot: unrelated path is outside" {
    try testing.expect(!isPathInsideRoot("/a/b", "/c/d"));
    try testing.expect(!isPathInsideRoot("/a/b", "/"));
}

test "resolveInsideWorkspace: plain relative path resolves inside" {
    const allocator = testing.allocator;
    const resolved = try resolveInsideWorkspace(allocator, "/tmp/ws", "handler.ts");
    defer allocator.free(resolved);
    try testing.expectEqualStrings("/tmp/ws/handler.ts", resolved);
}

test "resolveInsideWorkspace: rejects ../ escape" {
    const allocator = testing.allocator;
    try testing.expectError(
        error.PathOutsideWorkspace,
        resolveInsideWorkspace(allocator, "/tmp/ws", "../etc/passwd"),
    );
}

test "resolveInsideWorkspace: rejects absolute path outside root" {
    const allocator = testing.allocator;
    try testing.expectError(
        error.PathOutsideWorkspace,
        resolveInsideWorkspace(allocator, "/tmp/ws", "/etc/passwd"),
    );
}

test "resolveInsideWorkspace: accepts absolute path inside root" {
    const allocator = testing.allocator;
    const resolved = try resolveInsideWorkspace(allocator, "/tmp/ws", "/tmp/ws/deep/handler.ts");
    defer allocator.free(resolved);
    try testing.expectEqualStrings("/tmp/ws/deep/handler.ts", resolved);
}

test "resolveInsideWorkspace: collapses redundant path segments within root" {
    const allocator = testing.allocator;
    const resolved = try resolveInsideWorkspace(allocator, "/tmp/ws", "sub/../handler.ts");
    defer allocator.free(resolved);
    try testing.expectEqualStrings("/tmp/ws/handler.ts", resolved);
}

test "relativeToRoot: self is rendered as '.'" {
    try testing.expectEqualStrings(".", relativeToRoot("/tmp/ws", "/tmp/ws"));
}

test "relativeToRoot: strips the root prefix and separator" {
    try testing.expectEqualStrings("handler.ts", relativeToRoot("/tmp/ws", "/tmp/ws/handler.ts"));
    try testing.expectEqualStrings("sub/handler.ts", relativeToRoot("/tmp/ws", "/tmp/ws/sub/handler.ts"));
}

test "workspaceRoot: returns a non-empty allocator-owned path" {
    const allocator = testing.allocator;
    const root = try workspaceRoot(allocator);
    defer allocator.free(root);
    try testing.expect(root.len > 0);
}
