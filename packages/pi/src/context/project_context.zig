//! Project-context loader: AGENTS.md / CLAUDE.md walk from cwd up to the
//! project root, concatenated with path-labelled headings. The output is
//! appended to the system prompt as a read-only project-context section
//! (persona is never merged). Nothing else can add to the system prompt.
//!
//! Walk order: start at the resolved cwd; at each level read AGENTS.md then
//! CLAUDE.md if present; ascend until either `.git/` is observed at the
//! current level (inclusive, so the repo root's files are included) or the
//! filesystem root is reached. Files are concatenated in outer-first order
//! so nested rules read after (and conceptually refine) parent rules.
//!
//! Caps keep the loader bounded: per-file cap prevents a single enormous
//! AGENTS.md from dominating the prompt budget; total cap is a final guard
//! before the caller's own persona-cap truncation kicks in. Callers should
//! treat a `FileTooBig` from `zigts.file_io.readFile` as a signal to skip
//! that file rather than abort the whole load - the caller's prompt cap
//! still enforces the final envelope.

const std = @import("std");
const zigts = @import("zigts");
const file_io = zigts.file_io;

pub const Options = struct {
    /// Primary filename checked at each level before the alternate.
    filename_primary: []const u8 = "AGENTS.md",
    /// Secondary filename; both are read if both are present.
    filename_alternate: []const u8 = "CLAUDE.md",
    /// Maximum bytes read from a single file. Oversized files are skipped.
    per_file_cap: usize = 64 * 1024,
    /// Hard cap on the concatenated output. Return null once exceeded so
    /// the caller does not have to re-truncate.
    total_cap: usize = 512 * 1024,
    /// Stop walking (inclusive) when a directory contains `.git`.
    stop_at_git_root: bool = true,
};

/// Loads project context from the current working directory. Thin wrapper
/// around `loadFromDir` that resolves cwd realpath. Returns null when no
/// matching files were found.
pub fn loadFromCwd(allocator: std.mem.Allocator) !?[]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const cwd_real = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io, ".", allocator);
    defer allocator.free(cwd_real);
    return try loadFromDir(allocator, cwd_real, .{});
}

/// Loads project context starting at `cwd_abs` (must be an absolute path).
/// Test-friendly: pass a tmpdir to exercise the walk without depending on
/// the caller's actual cwd.
pub fn loadFromDir(
    allocator: std.mem.Allocator,
    cwd_abs: []const u8,
    options: Options,
) !?[]u8 {
    if (!std.fs.path.isAbsolute(cwd_abs)) return error.PathNotAbsolute;

    // Collect per-level bodies innermost-first, then reverse on emit.
    var levels: std.ArrayList(LevelBuf) = .empty;
    defer {
        for (levels.items) |*lvl| lvl.deinit(allocator);
        levels.deinit(allocator);
    }

    // Single backing allocation; `current` is shortened via std.fs.path.dirname
    // to walk up. Freed once at scope exit.
    const cwd_owned = try allocator.dupe(u8, cwd_abs);
    defer allocator.free(cwd_owned);
    var current: []const u8 = cwd_owned;

    while (true) {
        var level_buf: LevelBuf = .{};
        errdefer level_buf.deinit(allocator);

        try readIfPresent(allocator, current, options.filename_primary, options.per_file_cap, &level_buf);
        try readIfPresent(allocator, current, options.filename_alternate, options.per_file_cap, &level_buf);

        const has_git = options.stop_at_git_root and dirHasGit(allocator, current);

        if (level_buf.files.items.len > 0) {
            try levels.append(allocator, level_buf);
        } else {
            level_buf.deinit(allocator);
        }

        if (has_git) break;

        const parent = std.fs.path.dirname(current) orelse break;
        if (parent.len == current.len) break;
        current = parent;
    }

    if (levels.items.len == 0) return null;

    var aw: std.Io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();
    const w = &aw.writer;

    // Emit outer-first (parents before cwd).
    var i: usize = levels.items.len;
    while (i > 0) {
        i -= 1;
        const lvl = &levels.items[i];
        for (lvl.files.items) |*file| {
            try w.print("## {s}\n\n", .{file.path});
            try w.writeAll(file.body);
            if (file.body.len == 0 or file.body[file.body.len - 1] != '\n') try w.writeByte('\n');
            try w.writeByte('\n');
            if (aw.writer.end > options.total_cap) return error.ProjectContextTooLarge;
        }
    }

    var out = aw.toArrayList();
    return try out.toOwnedSlice(allocator);
}

const FileBuf = struct {
    path: []u8,
    body: []u8,

    fn deinit(self: *FileBuf, allocator: std.mem.Allocator) void {
        allocator.free(self.path);
        allocator.free(self.body);
    }
};

const LevelBuf = struct {
    files: std.ArrayList(FileBuf) = .empty,

    fn deinit(self: *LevelBuf, allocator: std.mem.Allocator) void {
        for (self.files.items) |*f| f.deinit(allocator);
        self.files.deinit(allocator);
    }
};

fn readIfPresent(
    allocator: std.mem.Allocator,
    dir: []const u8,
    name: []const u8,
    per_file_cap: usize,
    out: *LevelBuf,
) !void {
    const joined = try std.fs.path.join(allocator, &.{ dir, name });
    errdefer allocator.free(joined);

    // Oversized and missing files are silent skips. The per-file cap is an
    // abuse guard, not a correctness invariant: the model is better off with
    // some context than none, and the caller's prompt-cap still enforces the
    // final envelope.
    const body = file_io.readFile(allocator, joined, per_file_cap) catch |err| switch (err) {
        error.FileNotFound, error.FileTooBig => {
            allocator.free(joined);
            return;
        },
        else => return err,
    };
    errdefer allocator.free(body);

    try out.files.append(allocator, .{ .path = joined, .body = body });
}

fn dirHasGit(allocator: std.mem.Allocator, dir: []const u8) bool {
    const git_path = std.fs.path.join(allocator, &.{ dir, ".git" }) catch return false;
    defer allocator.free(git_path);
    // POSIX openat(RDONLY) succeeds on both regular files and directories,
    // so fileExists covers both a .git dir (normal repo) and a .git file
    // (submodule / linked worktree).
    return file_io.fileExists(allocator, git_path);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

const TmpTree = struct {
    root: []u8,

    fn init(allocator: std.mem.Allocator) !TmpTree {
        var ts: std.posix.timespec = undefined;
        _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
        const counter = test_counter.fetchAdd(1, .seq_cst);
        const name = try std.fmt.allocPrint(
            allocator,
            "zigttp-project-context-test-{d}-{d}-{d}",
            .{ @as(u64, @intCast(ts.sec)), @as(u64, @intCast(ts.nsec)), counter },
        );
        defer allocator.free(name);

        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        const io = io_backend.io();

        var tmp_root = try std.Io.Dir.openDirAbsolute(io, "/tmp", .{});
        defer tmp_root.close(io);
        tmp_root.deleteTree(io, name) catch {};
        try std.Io.Dir.createDirPath(tmp_root, io, name);

        const root = try std.fs.path.resolve(allocator, &.{ "/tmp", name });
        return .{ .root = root };
    }

    fn deinit(self: *TmpTree, allocator: std.mem.Allocator) void {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        const io = io_backend.io();
        var tmp_root = std.Io.Dir.openDirAbsolute(io, "/tmp", .{}) catch {
            allocator.free(self.root);
            return;
        };
        defer tmp_root.close(io);
        const basename = std.fs.path.basename(self.root);
        tmp_root.deleteTree(io, basename) catch {};
        allocator.free(self.root);
    }

    fn mkdir(self: *const TmpTree, allocator: std.mem.Allocator, rel: []const u8) !void {
        const abs_path = try std.fs.path.join(allocator, &.{ self.root, rel });
        defer allocator.free(abs_path);
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), abs_path);
    }

    fn write(self: *const TmpTree, allocator: std.mem.Allocator, rel: []const u8, body: []const u8) !void {
        const abs_path = try std.fs.path.join(allocator, &.{ self.root, rel });
        defer allocator.free(abs_path);
        if (std.fs.path.dirname(abs_path)) |parent| {
            var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
            defer io_backend.deinit();
            try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), parent);
        }
        try file_io.writeFile(allocator, abs_path, body);
    }

    fn absPath(self: *const TmpTree, allocator: std.mem.Allocator, rel: []const u8) ![]u8 {
        return std.fs.path.join(allocator, &.{ self.root, rel });
    }
};

var test_counter = std.atomic.Value(u64).init(0);

test "returns null when no context files exist" {
    var tree = try TmpTree.init(testing.allocator);
    defer tree.deinit(testing.allocator);

    const result = try loadFromDir(testing.allocator, tree.root, .{});
    try testing.expect(result == null);
}

test "reads a single AGENTS.md at cwd" {
    var tree = try TmpTree.init(testing.allocator);
    defer tree.deinit(testing.allocator);

    try tree.write(testing.allocator, "AGENTS.md", "project rule X\n");

    const result = (try loadFromDir(testing.allocator, tree.root, .{})) orelse return error.TestExpected;
    defer testing.allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "AGENTS.md") != null);
    try testing.expect(std.mem.indexOf(u8, result, "project rule X") != null);
}

test "reads both AGENTS.md and CLAUDE.md at the same level" {
    var tree = try TmpTree.init(testing.allocator);
    defer tree.deinit(testing.allocator);

    try tree.write(testing.allocator, "AGENTS.md", "AAA\n");
    try tree.write(testing.allocator, "CLAUDE.md", "BBB\n");

    const result = (try loadFromDir(testing.allocator, tree.root, .{})) orelse return error.TestExpected;
    defer testing.allocator.free(result);

    const a = std.mem.indexOf(u8, result, "AAA") orelse return error.TestExpected;
    const b = std.mem.indexOf(u8, result, "BBB") orelse return error.TestExpected;
    try testing.expect(a < b); // AGENTS before CLAUDE
}

test "walks upward and emits outer-first" {
    var tree = try TmpTree.init(testing.allocator);
    defer tree.deinit(testing.allocator);

    try tree.write(testing.allocator, "AGENTS.md", "ROOT\n");
    try tree.mkdir(testing.allocator, "sub");
    try tree.write(testing.allocator, "sub/AGENTS.md", "SUB\n");

    const sub_abs = try tree.absPath(testing.allocator, "sub");
    defer testing.allocator.free(sub_abs);

    const result = (try loadFromDir(testing.allocator, sub_abs, .{ .stop_at_git_root = false })) orelse return error.TestExpected;
    defer testing.allocator.free(result);

    const root_pos = std.mem.indexOf(u8, result, "ROOT") orelse return error.TestExpected;
    const sub_pos = std.mem.indexOf(u8, result, "SUB") orelse return error.TestExpected;
    try testing.expect(root_pos < sub_pos);
}

test "stops at .git directory inclusive" {
    var tree = try TmpTree.init(testing.allocator);
    defer tree.deinit(testing.allocator);

    // Tree: /tmp/<root>/.git/ (marker), /tmp/<root>/AGENTS.md, /tmp/<root>/inner/AGENTS.md
    try tree.mkdir(testing.allocator, ".git");
    try tree.write(testing.allocator, "AGENTS.md", "REPO_ROOT\n");
    try tree.write(testing.allocator, "inner/AGENTS.md", "INNER\n");

    const inner_abs = try tree.absPath(testing.allocator, "inner");
    defer testing.allocator.free(inner_abs);

    const result = (try loadFromDir(testing.allocator, inner_abs, .{})) orelse return error.TestExpected;
    defer testing.allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "REPO_ROOT") != null);
    try testing.expect(std.mem.indexOf(u8, result, "INNER") != null);
}

test "respects per-file cap by skipping oversized files" {
    var tree = try TmpTree.init(testing.allocator);
    defer tree.deinit(testing.allocator);

    var big: std.ArrayList(u8) = .empty;
    defer big.deinit(testing.allocator);
    try big.appendNTimes(testing.allocator, 'x', 1024);
    try tree.write(testing.allocator, "AGENTS.md", big.items);

    const result = try loadFromDir(testing.allocator, tree.root, .{ .per_file_cap = 512 });
    try testing.expect(result == null);
}

test "total cap surfaces as an error" {
    var tree = try TmpTree.init(testing.allocator);
    defer tree.deinit(testing.allocator);

    var big: std.ArrayList(u8) = .empty;
    defer big.deinit(testing.allocator);
    try big.appendNTimes(testing.allocator, 'y', 2048);
    try tree.write(testing.allocator, "AGENTS.md", big.items);

    const err = loadFromDir(testing.allocator, tree.root, .{
        .per_file_cap = 4096,
        .total_cap = 256,
    });
    try testing.expectError(error.ProjectContextTooLarge, err);
}

test "rejects relative cwd" {
    const err = loadFromDir(testing.allocator, "relative/path", .{});
    try testing.expectError(error.PathNotAbsolute, err);
}
