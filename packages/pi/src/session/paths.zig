//! Pure path plumbing for the lockdown session layout.
//!
//! Sessions live under `$HOME/.zigttp/sessions/<cwd_hash>/<session_id>/`,
//! where `cwd_hash` is the lowercase hex SHA-256 of the resolved workspace
//! realpath. Each session directory carries a `workspace.txt` pointer back
//! to the originating workspace so the folder is self-describing on disk.
//!
//! This module is plumbing only: it computes paths and writes the pointer
//! file. It does not create session directories itself, and it does not
//! talk to the loop.

const std = @import("std");

const zigts = @import("zigts");
const file_io = zigts.file_io;

const testing = std.testing;

/// Lowercase hex SHA-256 of the workspace realpath. 64 chars.
pub const CwdHash = [64]u8;

/// Absolute path to `$HOME/.zigttp/sessions`. Honors `$ZIGTTP_SESSIONS_DIR`
/// when set (used by tests to redirect under /tmp). Caller owns the slice.
/// Does not create the directory.
pub fn sessionRoot(allocator: std.mem.Allocator) ![]u8 {
    if (envVar("ZIGTTP_SESSIONS_DIR")) |override| {
        if (override.len > 0) return try allocator.dupe(u8, override);
    }
    const home = envVar("HOME") orelse return error.HomeNotSet;
    if (home.len == 0) return error.HomeNotSet;
    return try std.fs.path.join(allocator, &.{ home, ".zigttp", "sessions" });
}

fn envVar(name_z: [:0]const u8) ?[]const u8 {
    const raw = std.c.getenv(name_z.ptr) orelse return null;
    return std.mem.sliceTo(raw, 0);
}

/// Resolves cwd to its realpath, then returns SHA-256 of those bytes as
/// lowercase hex. Stable across repeated calls from the same cwd.
pub fn cwdHashFull(allocator: std.mem.Allocator) !CwdHash {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const realpath = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io, ".", allocator);
    defer allocator.free(realpath);

    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(realpath, &digest, .{});

    return hexLowerFixed(digest);
}

/// Returns `$ROOT/<cwd_hash>/<session_id>`. Caller owns the slice. Does not
/// create the directory.
pub fn sessionDir(
    allocator: std.mem.Allocator,
    session_id: []const u8,
) ![]u8 {
    const root = try sessionRoot(allocator);
    defer allocator.free(root);

    const hash = try cwdHashFull(allocator);
    return try std.fs.path.join(allocator, &.{ root, hash[0..], session_id });
}

/// Writes `<session_dir_path>/workspace.txt` with `realpath + "\n"`.
/// Creates parent directories if missing. Truncates existing files.
pub fn writeWorkspacePointer(
    allocator: std.mem.Allocator,
    session_dir_path: []const u8,
    realpath: []const u8,
) !void {
    {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), session_dir_path);
    }

    const pointer_path = try std.fs.path.join(allocator, &.{ session_dir_path, "workspace.txt" });
    defer allocator.free(pointer_path);

    const body = try std.fmt.allocPrint(allocator, "{s}\n", .{realpath});
    defer allocator.free(body);

    try file_io.writeFile(allocator, pointer_path, body);
}

fn hexLowerFixed(digest: [std.crypto.hash.sha2.Sha256.digest_length]u8) CwdHash {
    const alphabet = "0123456789abcdef";
    var out: CwdHash = undefined;
    for (digest, 0..) |b, i| {
        out[i * 2 + 0] = alphabet[b >> 4];
        out[i * 2 + 1] = alphabet[b & 0x0F];
    }
    return out;
}

// ---- Test scaffolding ------------------------------------------------------

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
            "zigttp-session-paths-test-{d}-{d}-{d}",
            .{ @as(u64, @intCast(ts.sec)), @as(u64, @intCast(ts.nsec)), counter },
        );
        errdefer allocator.free(name);

        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        const io = io_backend.io();

        var tmp_root = try std.Io.Dir.openDirAbsolute(io, "/tmp", .{});
        defer tmp_root.close(io);
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

extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;
extern "c" fn unsetenv(name: [*:0]const u8) c_int;

const EnvOverride = struct {
    name_z: [:0]const u8,
    previous: ?[]u8,

    fn set(allocator: std.mem.Allocator, name_z: [:0]const u8, value: []const u8) !EnvOverride {
        const prev_opt = std.c.getenv(name_z.ptr);
        const previous: ?[]u8 = if (prev_opt) |p| blk: {
            const slice = std.mem.sliceTo(p, 0);
            break :blk try allocator.dupe(u8, slice);
        } else null;
        errdefer if (previous) |p| allocator.free(p);

        const value_z = try allocator.dupeZ(u8, value);
        defer allocator.free(value_z);
        _ = setenv(name_z.ptr, value_z.ptr, 1);

        return .{ .name_z = name_z, .previous = previous };
    }

    fn restore(self: *EnvOverride, allocator: std.mem.Allocator) void {
        if (self.previous) |prev| {
            const prev_z = allocator.dupeZ(u8, prev) catch {
                allocator.free(prev);
                self.previous = null;
                return;
            };
            defer allocator.free(prev_z);
            _ = setenv(self.name_z.ptr, prev_z.ptr, 1);
            allocator.free(prev);
            self.previous = null;
        } else {
            _ = unsetenv(self.name_z.ptr);
        }
    }
};

// ---- Tests -----------------------------------------------------------------

test "cwdHashFull returns stable 64-char lowercase hex" {
    const allocator = testing.allocator;
    const first = try cwdHashFull(allocator);
    const second = try cwdHashFull(allocator);
    try testing.expectEqual(@as(usize, 64), first.len);
    try testing.expectEqualSlices(u8, first[0..], second[0..]);
    for (first) |c| {
        const is_digit = c >= '0' and c <= '9';
        const is_lower = c >= 'a' and c <= 'f';
        try testing.expect(is_digit or is_lower);
    }
}

test "sessionRoot honors ZIGTTP_SESSIONS_DIR" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    var override = try EnvOverride.set(allocator, "ZIGTTP_SESSIONS_DIR", tmp.abs_path);
    defer override.restore(allocator);

    const root = try sessionRoot(allocator);
    defer allocator.free(root);
    try testing.expectEqualStrings(tmp.abs_path, root);
}

test "sessionDir concatenates root, cwd_hash, and session_id" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    var override = try EnvOverride.set(allocator, "ZIGTTP_SESSIONS_DIR", tmp.abs_path);
    defer override.restore(allocator);

    const hash = try cwdHashFull(allocator);
    const dir = try sessionDir(allocator, "sess-42");
    defer allocator.free(dir);

    const expected = try std.fs.path.join(allocator, &.{ tmp.abs_path, hash[0..], "sess-42" });
    defer allocator.free(expected);
    try testing.expectEqualStrings(expected, dir);
}

test "writeWorkspacePointer round-trips with trailing newline" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    var override = try EnvOverride.set(allocator, "ZIGTTP_SESSIONS_DIR", tmp.abs_path);
    defer override.restore(allocator);

    const dir = try sessionDir(allocator, "sess-ptr");
    defer allocator.free(dir);

    const realpath = "/workspace/example/repo";
    try writeWorkspacePointer(allocator, dir, realpath);

    const pointer_path = try std.fs.path.join(allocator, &.{ dir, "workspace.txt" });
    defer allocator.free(pointer_path);

    const contents = try file_io.readFile(allocator, pointer_path, 4096);
    defer allocator.free(contents);

    const expected = realpath ++ "\n";
    try testing.expectEqualStrings(expected, contents);

    // Second write should truncate, not append.
    try writeWorkspacePointer(allocator, dir, realpath);
    const contents2 = try file_io.readFile(allocator, pointer_path, 4096);
    defer allocator.free(contents2);
    try testing.expectEqualStrings(expected, contents2);
}
