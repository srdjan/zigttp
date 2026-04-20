//! Loads `AGENTS.md` and `CLAUDE.md` files from the cwd up to the
//! filesystem root, concatenates them into one labeled document, and hands
//! the result to the expert persona as read-only project context.
//!
//! Emission order is outermost ancestor first, cwd last. Within a
//! directory, AGENTS.md precedes CLAUDE.md. Each section is labeled with
//! its absolute path so the model can reason about where a rule came from.
//!
//! Per-file cap: 32 KiB. Combined budget: 128 KiB. Truncation is signalled
//! in-band with a sentinel the caller can grep for.
//!
//! Returns `null` if no files were found. Returns an owned slice otherwise;
//! caller frees with `allocator.free`.

const std = @import("std");

const per_file_cap: usize = 32 * 1024;
const total_budget: usize = 128 * 1024;
const per_file_truncation_marker = "\n...[file truncated at 32 KiB per-file cap]\n";
const total_truncation_marker = "\n...[context truncated at 128 KiB budget]\n";

const candidate_names = [_][]const u8{ "AGENTS.md", "CLAUDE.md" };

pub fn loadProjectContext(allocator: std.mem.Allocator) !?[]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const cwd_abs = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io, ".", allocator);
    defer allocator.free(cwd_abs);

    var ancestors: std.ArrayList([]u8) = .empty;
    defer {
        for (ancestors.items) |p| allocator.free(p);
        ancestors.deinit(allocator);
    }

    {
        const current = try allocator.dupe(u8, cwd_abs);
        errdefer allocator.free(current);
        try ancestors.append(allocator, current);
    }
    while (true) {
        const tail = ancestors.items[ancestors.items.len - 1];
        const parent = std.fs.path.dirname(tail) orelse break;
        if (parent.len == tail.len) break;
        const dup = try allocator.dupe(u8, parent);
        errdefer allocator.free(dup);
        try ancestors.append(allocator, dup);
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    var any_found = false;
    var budget_exceeded = false;

    var i: usize = ancestors.items.len;
    outer: while (i > 0) {
        i -= 1;
        const dir = ancestors.items[i];
        for (candidate_names) |name| {
            const abs = try std.fs.path.resolve(allocator, &.{ dir, name });
            defer allocator.free(abs);

            const read = readFileCapped(allocator, abs, per_file_cap) catch |err| switch (err) {
                error.FileNotFound, error.AccessDenied, error.NotDir => continue,
                else => return err,
            };
            defer allocator.free(read.bytes);

            if (try appendSection(&aw, w, abs, read.bytes, read.truncated)) {
                budget_exceeded = true;
                break :outer;
            }
            any_found = true;
        }
    }

    if (!any_found) return null;

    if (budget_exceeded) {
        // Truncate the buffered output to the budget, then append the marker.
        // We intentionally append the marker even if it puts the total slightly
        // over the budget: the marker is the signal the caller reads.
        buf = aw.toArrayList();
        if (buf.items.len > total_budget) buf.shrinkRetainingCapacity(total_budget);
        try buf.appendSlice(allocator, total_truncation_marker);
        return try buf.toOwnedSlice(allocator);
    }

    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

const CappedRead = struct { bytes: []u8, truncated: bool };

/// Reads up to `cap` bytes from an absolute path. If the file is larger
/// than `cap`, returns the first `cap` bytes with `truncated=true`. Unlike
/// `zigts.file_io.readFile`, this never allocates beyond the cap and never
/// returns `error.FileTooBig`.
fn readFileCapped(allocator: std.mem.Allocator, path: []const u8, cap: usize) !CappedRead {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.Io.Threaded.closeFd(fd);

    var buf = try allocator.alloc(u8, cap);
    errdefer allocator.free(buf);

    var written: usize = 0;
    while (written < cap) {
        const n = try std.posix.read(fd, buf[written..]);
        if (n == 0) break;
        written += n;
    }

    // Probe one extra byte to detect whether the file had more data.
    var scratch: [1]u8 = undefined;
    const extra = std.posix.read(fd, &scratch) catch 0;
    const truncated = extra > 0;

    const result = try allocator.realloc(buf, written);
    return .{ .bytes = result, .truncated = truncated };
}

/// Writes one labeled section to the writer. Returns true when the running
/// buffer size crossed the total budget while writing this section, in
/// which case the caller should stop emitting further sections.
fn appendSection(
    aw: *std.Io.Writer.Allocating,
    w: *std.Io.Writer,
    path: []const u8,
    body: []const u8,
    body_was_capped: bool,
) !bool {
    try w.writeAll("=== ");
    try w.writeAll(path);
    try w.writeAll(" ===\n");
    try w.writeAll(body);
    if (body.len == 0 or body[body.len - 1] != '\n') try w.writeAll("\n");
    if (body_was_capped) try w.writeAll(per_file_truncation_marker);
    try w.writeAll("\n");

    return aw.writer.buffered().len > total_budget;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn cwdPathAlloc(allocator: std.mem.Allocator) ![]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const p = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io, ".", allocator);
    defer allocator.free(p);
    return try allocator.dupe(u8, p);
}

/// Test scaffolding: a scratch directory rooted outside the repo so the
/// walker up to `/` does not pick up the repo's own AGENTS.md / CLAUDE.md.
/// `std.testing.tmpDir` creates under `.zig-cache/tmp/` of cwd, which sits
/// inside the repo; we cannot use it here without losing isolation.
var isolated_tmp_counter = std.atomic.Value(u64).init(0);

const IsolatedTmp = struct {
    abs_path: []u8,
    name: []u8,

    fn init(allocator: std.mem.Allocator) !IsolatedTmp {
        var ts: std.posix.timespec = undefined;
        _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
        const counter = isolated_tmp_counter.fetchAdd(1, .seq_cst);
        const name = try std.fmt.allocPrint(
            allocator,
            "zigttp-ctx-test-{d}-{d}-{d}",
            .{ @as(u64, @intCast(ts.sec)), @as(u64, @intCast(ts.nsec)), counter },
        );
        errdefer allocator.free(name);

        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        const io = io_backend.io();

        var tmp_root = try std.Io.Dir.openDirAbsolute(io, "/tmp", .{});
        defer tmp_root.close(io);
        // Pre-clean in case a prior run crashed before cleanup.
        tmp_root.deleteTree(io, name) catch {};
        try std.Io.Dir.createDirPath(tmp_root, io, name);

        const abs_path = try std.fs.path.resolve(allocator, &.{ "/tmp", name });
        errdefer allocator.free(abs_path);

        return .{ .abs_path = abs_path, .name = name };
    }

    fn cleanup(self: *IsolatedTmp, allocator: std.mem.Allocator) void {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        const io = io_backend.io();
        var tmp_root = std.Io.Dir.openDirAbsolute(io, "/tmp", .{}) catch {
            allocator.free(self.abs_path);
            allocator.free(self.name);
            return;
        };
        defer tmp_root.close(io);
        tmp_root.deleteTree(io, self.name) catch {};
        allocator.free(self.abs_path);
        allocator.free(self.name);
    }
};

fn writeTestFile(
    allocator: std.mem.Allocator,
    root_abs: []const u8,
    sub_path: []const u8,
    data: []const u8,
) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    var root = try std.Io.Dir.openDirAbsolute(io, root_abs, .{});
    defer root.close(io);
    if (std.fs.path.dirname(sub_path)) |parent| {
        try std.Io.Dir.createDirPath(root, io, parent);
    }
    try root.writeFile(io, .{ .sub_path = sub_path, .data = data });
}

test "loadProjectContext returns null when no context files exist" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const saved = try cwdPathAlloc(allocator);
    defer allocator.free(saved);
    try std.Io.Threaded.chdir(tmp.abs_path);
    defer std.Io.Threaded.chdir(saved) catch {};

    const ctx = try loadProjectContext(allocator);
    try testing.expect(ctx == null);
}

test "loadProjectContext reads a single AGENTS.md at cwd" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    try writeTestFile(allocator, tmp.abs_path, "AGENTS.md", "hello from AGENTS.md");

    const saved = try cwdPathAlloc(allocator);
    defer allocator.free(saved);
    try std.Io.Threaded.chdir(tmp.abs_path);
    defer std.Io.Threaded.chdir(saved) catch {};

    const ctx = (try loadProjectContext(allocator)) orelse return error.TestUnexpectedResult;
    defer allocator.free(ctx);

    try testing.expect(std.mem.indexOf(u8, ctx, "hello from AGENTS.md") != null);
    try testing.expect(std.mem.indexOf(u8, ctx, "AGENTS.md ===") != null);
}

test "loadProjectContext concatenates nested AGENTS.md + CLAUDE.md in root-to-cwd order" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    try writeTestFile(allocator, tmp.abs_path, "AGENTS.md", "OUTER_AGENTS_MARKER");
    try writeTestFile(allocator, tmp.abs_path, "CLAUDE.md", "OUTER_CLAUDE_MARKER");
    try writeTestFile(allocator, tmp.abs_path, "inner/AGENTS.md", "INNER_AGENTS_MARKER");
    try writeTestFile(allocator, tmp.abs_path, "inner/CLAUDE.md", "INNER_CLAUDE_MARKER");

    const inner_path = try std.fs.path.resolve(allocator, &.{ tmp.abs_path, "inner" });
    defer allocator.free(inner_path);

    const saved = try cwdPathAlloc(allocator);
    defer allocator.free(saved);
    try std.Io.Threaded.chdir(inner_path);
    defer std.Io.Threaded.chdir(saved) catch {};

    const ctx = (try loadProjectContext(allocator)) orelse return error.TestUnexpectedResult;
    defer allocator.free(ctx);

    const outer_a = std.mem.indexOf(u8, ctx, "OUTER_AGENTS_MARKER") orelse return error.TestUnexpectedResult;
    const outer_c = std.mem.indexOf(u8, ctx, "OUTER_CLAUDE_MARKER") orelse return error.TestUnexpectedResult;
    const inner_a = std.mem.indexOf(u8, ctx, "INNER_AGENTS_MARKER") orelse return error.TestUnexpectedResult;
    const inner_c = std.mem.indexOf(u8, ctx, "INNER_CLAUDE_MARKER") orelse return error.TestUnexpectedResult;

    // Outer directory precedes inner (root-to-cwd order).
    try testing.expect(outer_a < inner_a);
    try testing.expect(outer_c < inner_c);
    // Within a directory, AGENTS.md precedes CLAUDE.md.
    try testing.expect(outer_a < outer_c);
    try testing.expect(inner_a < inner_c);
}

test "loadProjectContext truncates when a file exceeds the per-file cap" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const big = try allocator.alloc(u8, 64 * 1024);
    defer allocator.free(big);
    @memset(big, 'x');
    try writeTestFile(allocator, tmp.abs_path, "AGENTS.md", big);

    const saved = try cwdPathAlloc(allocator);
    defer allocator.free(saved);
    try std.Io.Threaded.chdir(tmp.abs_path);
    defer std.Io.Threaded.chdir(saved) catch {};

    const ctx = (try loadProjectContext(allocator)) orelse return error.TestUnexpectedResult;
    defer allocator.free(ctx);

    // The emitted body is capped at 32 KiB. Header plus marker add a fixed
    // amount, but the total must still be well under the 64 KiB source file.
    try testing.expect(ctx.len < 64 * 1024);
    try testing.expect(std.mem.indexOf(u8, ctx, "per-file cap") != null);
}

test "loadProjectContext honors the combined 128 KiB budget" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    // Build a nested tree deep enough that the combined AGENTS.md payload
    // forces a budget cutoff.  Each file is 30 KiB, so 6 files = 180 KiB of
    // body, comfortably over the 128 KiB budget.
    const payload = try allocator.alloc(u8, 30 * 1024);
    defer allocator.free(payload);
    @memset(payload, 'a');

    try writeTestFile(allocator, tmp.abs_path, "AGENTS.md", payload);
    try writeTestFile(allocator, tmp.abs_path, "a/AGENTS.md", payload);
    try writeTestFile(allocator, tmp.abs_path, "a/b/AGENTS.md", payload);
    try writeTestFile(allocator, tmp.abs_path, "a/b/c/AGENTS.md", payload);
    try writeTestFile(allocator, tmp.abs_path, "a/b/c/d/AGENTS.md", payload);
    try writeTestFile(allocator, tmp.abs_path, "a/b/c/d/e/AGENTS.md", payload);

    const deep_path = try std.fs.path.resolve(allocator, &.{ tmp.abs_path, "a", "b", "c", "d", "e" });
    defer allocator.free(deep_path);

    const saved = try cwdPathAlloc(allocator);
    defer allocator.free(saved);
    try std.Io.Threaded.chdir(deep_path);
    defer std.Io.Threaded.chdir(saved) catch {};

    const ctx = (try loadProjectContext(allocator)) orelse return error.TestUnexpectedResult;
    defer allocator.free(ctx);

    try testing.expect(std.mem.indexOf(u8, ctx, "128 KiB budget") != null);
    // Bounded: must not grow unboundedly past the budget + marker slack.
    try testing.expect(ctx.len <= total_budget + total_truncation_marker.len + 256);
}
