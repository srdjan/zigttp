const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const ui_payload = @import("../ui_payload.zig");
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

/// Return the current working directory as an absolute path.
///
/// `std.fs.path.resolve(&.{"."})` in zig 0.16 does not expand "." against
/// the real cwd, so we reach through `realPathFileAlloc`. The sentinel is
/// stripped so callers free with the same allocator they use for every
/// other string they own.
pub fn realCwd(allocator: std.mem.Allocator) ![]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const cwd_z = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io_backend.io(), ".", allocator);
    defer allocator.free(cwd_z);
    return try allocator.dupe(u8, cwd_z);
}

pub fn workspaceRoot(allocator: std.mem.Allocator) ![]u8 {
    return realCwd(allocator) catch try std.fs.path.resolve(allocator, &.{"."});
}

/// Current unix time in milliseconds. `std.time.milliTimestamp` is unavailable
/// in the zig 0.16 dev builds this project targets, so both the turn loop and
/// the autoloop reach through `std.c.clock_gettime` here.
pub fn nowUnixMs() i64 {
    var ts: std.posix.timespec = undefined;
    _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
    return @as(i64, ts.sec) * 1000 + @divTrunc(@as(i64, ts.nsec), 1_000_000);
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

/// PATH used to resolve a bare command name when the parent process has no
/// PATH of its own (e.g. a headless/cron host). Covers the common locations
/// for `rg` and `zig` on macOS/Linux.
const default_search_path = "/usr/local/bin:/usr/bin:/bin:/opt/homebrew/bin";

/// Resolve a bare command name (e.g. `rg`, `zig`) to an absolute executable
/// path using the PARENT process's PATH. `runCommand` spawns children with an
/// EMPTY environ for sandbox isolation, so a child cannot resolve a bare name
/// itself - the exec fails with FileNotFound. Resolving it here keeps the child
/// environ empty while letting the exec succeed. Returns an owned absolute
/// path, or null when the name already contains '/' (use as-is) or is not found
/// on PATH (fall back to the original name, preserving prior behavior).
fn resolveBinaryPath(allocator: std.mem.Allocator, io: std.Io, name: []const u8) !?[]u8 {
    if (name.len == 0 or std.mem.indexOfScalar(u8, name, '/') != null) return null;
    const path = if (std.c.getenv("PATH")) |p| std.mem.span(p) else default_search_path;
    var it = std.mem.tokenizeScalar(u8, path, ':');
    while (it.next()) |dir| {
        if (dir.len == 0) continue;
        const candidate = std.fs.path.join(allocator, &.{ dir, name }) catch continue;
        std.Io.Dir.accessAbsolute(io, candidate, .{}) catch {
            allocator.free(candidate);
            continue;
        };
        return candidate;
    }
    return null;
}

pub fn runCommand(
    allocator: std.mem.Allocator,
    cwd: []const u8,
    argv: []const []const u8,
) !CommandOutcome {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    // The child runs with an empty environ (no PATH) for sandboxing, so resolve
    // a bare argv[0] to an absolute path up front; otherwise bare names like
    // `rg`/`zig` fail to exec with FileNotFound.
    const resolved_binary: ?[]u8 = if (argv.len > 0)
        try resolveBinaryPath(allocator, io, argv[0])
    else
        null;
    defer if (resolved_binary) |b| allocator.free(b);

    var argv_storage: ?[][]const u8 = null;
    defer if (argv_storage) |s| allocator.free(s);
    const exec_argv: []const []const u8 = if (resolved_binary) |abs| blk: {
        const s = try allocator.alloc([]const u8, argv.len);
        s[0] = abs;
        @memcpy(s[1..], argv[1..]);
        argv_storage = s;
        break :blk s;
    } else argv;

    const run_result = try std.process.run(allocator, io, .{
        .argv = exec_argv,
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
    const llm_text = try buf.toOwnedSlice(allocator);
    errdefer allocator.free(llm_text);

    const command = try std.mem.join(allocator, " ", argv);
    errdefer allocator.free(command);

    return .{
        .ok = outcome.ok,
        .llm_text = llm_text,
        .ui_payload = .{ .command_outcome = .{
            .title = try allocator.dupe(u8, argv[0]),
            .exit_code = outcome.exit_code,
            .stdout = try allocator.dupe(u8, outcome.stdout),
            .stderr = try allocator.dupe(u8, outcome.stderr),
            .command = command,
        } },
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

test "workspaceRoot: returns an absolute cwd path" {
    const allocator = testing.allocator;
    const root = try workspaceRoot(allocator);
    defer allocator.free(root);
    try testing.expect(root.len > 0);
    try testing.expect(std.fs.path.isAbsolute(root));
}

test "resolveBinaryPath resolves a bare command name to an absolute path" {
    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    // `sh` lives in /bin, which is on PATH and in default_search_path.
    const resolved = try resolveBinaryPath(testing.allocator, io_backend.io(), "sh");
    defer if (resolved) |r| testing.allocator.free(r);
    try testing.expect(resolved != null);
    try testing.expect(std.fs.path.isAbsolute(resolved.?));
    try testing.expect(std.mem.endsWith(u8, resolved.?, "/sh"));
}

test "resolveBinaryPath returns null when the name already contains a slash" {
    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    try testing.expect((try resolveBinaryPath(testing.allocator, io_backend.io(), "./foo")) == null);
    try testing.expect((try resolveBinaryPath(testing.allocator, io_backend.io(), "/usr/bin/env")) == null);
}

test "resolveBinaryPath returns null for a name not on PATH" {
    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    try testing.expect((try resolveBinaryPath(testing.allocator, io_backend.io(), "definitely_not_a_real_binary_zzqx")) == null);
}
