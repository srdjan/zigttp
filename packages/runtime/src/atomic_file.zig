//! Small atomic-file primitives shared by the durable persistence surfaces
//! (`workflow_queue.zig`'s dead-letter/lease files, `durable_dead_runs.zig`'s
//! quarantine records). Extracted after code review found the two callers
//! carrying byte-for-byte duplicate copies of these functions - a bug fix to
//! the atomic-write sequence previously had to be found and applied twice.

const std = @import("std");
const zq = @import("zigts");

const Allocator = std.mem.Allocator;
const writeAllChecked = zq.trace.writeAllChecked;

pub fn ensureDir(path: []const u8) !void {
    const z = try std.heap.c_allocator.dupeZ(u8, path);
    defer std.heap.c_allocator.free(z);
    switch (std.posix.errno(std.posix.system.mkdir(z, 0o700))) {
        .SUCCESS, .EXIST => {},
        else => return error.MakeDirFailed,
    }
}

pub fn deleteIfExists(path: []const u8) void {
    const z = std.heap.c_allocator.dupeZ(u8, path) catch return;
    defer std.heap.c_allocator.free(z);
    _ = std.c.unlink(z);
}

pub fn writeFileAtomic(allocator: Allocator, final_path: []const u8, bytes: []const u8) !void {
    const tmp_path = try std.fmt.allocPrint(
        allocator,
        "{s}.tmp-{d}-{x}",
        .{ final_path, std.c.getpid(), @intFromPtr(bytes.ptr) },
    );
    defer allocator.free(tmp_path);
    errdefer deleteIfExists(tmp_path);

    const tmp_z = try allocator.dupeZ(u8, tmp_path);
    defer allocator.free(tmp_z);
    const fd = try std.posix.openatZ(
        std.posix.AT.FDCWD,
        tmp_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        0o600,
    );
    defer std.Io.Threaded.closeFd(fd);
    try writeAllChecked(fd, bytes);
    if (std.c.fsync(fd) != 0) return error.FileWriteFailed;

    const final_z = try allocator.dupeZ(u8, final_path);
    defer allocator.free(final_z);
    if (std.c.rename(tmp_z, final_z) != 0) return error.RenameFailed;
}

test "writeFileAtomic writes the final content and leaves no tmp file behind" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(dir);

    const final_path = try std.fs.path.join(allocator, &.{ dir, "record.json" });
    defer allocator.free(final_path);

    const payload = "{\"a\":1}";
    const tmp_path = try std.fmt.allocPrint(allocator, "{s}.tmp-{d}-{x}", .{ final_path, std.c.getpid(), @intFromPtr(payload.ptr) });
    defer allocator.free(tmp_path);

    try writeFileAtomic(allocator, final_path, payload);

    const contents = try zq.file_io.readFile(allocator, final_path, 4096);
    defer allocator.free(contents);
    try std.testing.expectEqualStrings(payload, contents);

    const tmp_path_z = try allocator.dupeZ(u8, tmp_path);
    defer allocator.free(tmp_path_z);
    try std.testing.expect(std.c.access(tmp_path_z, 0) != 0);
}

test "ensureDir is idempotent and deleteIfExists is a no-op on a missing path" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(dir);

    const nested = try std.fs.path.join(allocator, &.{ dir, "nested" });
    defer allocator.free(nested);

    try ensureDir(nested);
    try ensureDir(nested); // second call must not error (EEXIST is success)

    const missing = try std.fmt.allocPrint(allocator, "{s}/does-not-exist", .{dir});
    defer allocator.free(missing);
    deleteIfExists(missing); // must not crash
}
