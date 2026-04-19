const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const json_writer = @import("../anthropic/json_writer.zig");

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
