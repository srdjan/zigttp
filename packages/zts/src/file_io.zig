const std = @import("std");
const builtin = @import("builtin");
const module_graph = @import("modules/internal/module_graph.zig");

/// Read a file synchronously using POSIX operations (for use before Io is initialized).
/// Reads in 4KB chunks without fstat to avoid libc/Linux compatibility issues.
pub fn readFile(allocator: std.mem.Allocator, path: []const u8, max_size: usize) ![]u8 {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.Io.Threaded.closeFd(fd);

    var buffer: std.ArrayList(u8) = .empty;
    errdefer buffer.deinit(allocator);

    var chunk: [4096]u8 = undefined;
    while (true) {
        const bytes_read = try std.posix.read(fd, &chunk);
        if (bytes_read == 0) break;
        if (buffer.items.len + bytes_read > max_size) return error.FileTooBig;
        try buffer.appendSlice(allocator, chunk[0..bytes_read]);
    }

    return buffer.toOwnedSlice(allocator);
}

/// Read up to `window` bytes from the END of a file. When the window does not
/// begin at offset 0 the (likely partial) leading line is dropped, so the
/// result starts at the first complete line. Used as a graceful fallback for
/// append-only logs that have grown past a read cap. Returns an owned,
/// exactly-sized slice the caller frees.
pub fn readFileTail(allocator: std.mem.Allocator, path: []const u8, window: usize) ![]u8 {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.Io.Threaded.closeFd(fd);

    const st = try fstatFd(fd);
    const start: u64 = if (st.size > window) st.size - window else 0;

    var buffer: std.ArrayList(u8) = .empty;
    defer buffer.deinit(allocator);

    var offset: u64 = start;
    var chunk: [4096]u8 = undefined;
    while (true) {
        const result = std.c.pread(fd, &chunk, chunk.len, @intCast(offset));
        if (result < 0) return error.ReadFailed;
        const n: usize = @intCast(result);
        if (n == 0) break;
        try buffer.appendSlice(allocator, chunk[0..n]);
        offset += n;
    }

    var data: []const u8 = buffer.items;
    if (start > 0) {
        if (std.mem.indexOfScalar(u8, data, '\n')) |nl| {
            data = data[nl + 1 ..];
        } else {
            data = data[0..0];
        }
    }
    return try allocator.dupe(u8, data);
}

/// Check whether a file exists using POSIX open (no fstat/libc dependency).
pub fn fileExists(allocator: std.mem.Allocator, path: []const u8) bool {
    const path_z = allocator.dupeZ(u8, path) catch return false;
    defer allocator.free(path_z);
    const fd = std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0) catch return false;
    std.Io.Threaded.closeFd(fd);
    return true;
}

/// Write a file synchronously and atomically using POSIX operations. New files
/// are created 0600: callers include session transcripts, project memory, and
/// edited handler source, which carry tool output that must not be
/// world-readable.
///
/// The write goes to a sibling temp file in the same directory, is fsynced,
/// then renamed over the destination. A crash or a partial write therefore
/// leaves either the old file or the complete new file, never a truncated one -
/// critical for the expert apply_edit path, which would otherwise leave the
/// user's working handler corrupt and unrecoverable.
pub fn writeFile(allocator: std.mem.Allocator, path: []const u8, data: []const u8) !void {
    // Build a sibling temp path in the same directory so rename(2) is atomic
    // (same filesystem). PID + address keeps concurrent writers from colliding.
    const dir = std.fs.path.dirname(path) orelse ".";
    const base = std.fs.path.basename(path);
    const tmp_path = try std.fmt.allocPrint(
        allocator,
        "{s}/.{s}.tmp.{d}.{d}",
        .{ dir, base, std.c.getpid(), @intFromPtr(data.ptr) },
    );
    defer allocator.free(tmp_path);

    const tmp_path_z = try allocator.dupeZ(u8, tmp_path);
    defer allocator.free(tmp_path_z);

    // O_EXCL: never follow or clobber an existing path at the temp location.
    const fd = try std.posix.openatZ(
        std.posix.AT.FDCWD,
        tmp_path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .EXCL = true },
        0o600,
    );
    // On any failure after creating the temp file, remove the stray sibling so
    // a partial write never lingers. The fd itself is closed below, before the
    // rename, so this only unlinks.
    errdefer _ = std.c.unlink(tmp_path_z);

    {
        defer std.Io.Threaded.closeFd(fd);
        var total_written: usize = 0;
        while (total_written < data.len) {
            const result = std.c.write(fd, data[total_written..].ptr, data.len - total_written);
            if (result < 0) return error.WriteFailure;
            if (result == 0) return error.WriteFailure;
            total_written += @intCast(result);
        }
        // Flush to disk before the rename so the renamed file has full content.
        _ = std.c.fsync(fd);
    }

    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);
    if (std.c.rename(tmp_path_z, path_z) != 0) return error.WriteFailure;
}

const ExclusiveCommitCapability = struct {
    context: ?*anyopaque = null,
    before_link: *const fn (?*anyopaque, []const u8) anyerror!void = noOpBeforeExclusiveLink,

    fn beforeLink(self: ExclusiveCommitCapability, path: []const u8) !void {
        try self.before_link(self.context, path);
    }
};

fn noOpBeforeExclusiveLink(_: ?*anyopaque, _: []const u8) !void {}

/// Create a new file without replacing an existing destination. The complete
/// contents are fsynced to a sibling temp, then linked to the destination. The
/// hard link is atomic and fails if another writer created the destination.
pub fn writeFileCreateExclusive(allocator: std.mem.Allocator, path: []const u8, data: []const u8) !void {
    return writeFileCreateExclusiveWithCommit(allocator, path, data, .{});
}

fn writeFileCreateExclusiveWithCommit(
    allocator: std.mem.Allocator,
    path: []const u8,
    data: []const u8,
    commit: ExclusiveCommitCapability,
) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var atomic_file = try std.Io.Dir.cwd().createFileAtomic(io, path, .{
        .permissions = std.Io.File.Permissions.fromMode(0o600),
        .replace = false,
    });
    defer atomic_file.deinit(io);

    try atomic_file.file.writeStreamingAll(io, data);
    try atomic_file.file.sync(io);

    try commit.beforeLink(path);
    try atomic_file.link(io);
}

pub const FdStat = struct { size: u64, mode: u32 };

/// Size and permission bits of an open fd. `std.c.fstat` is deliberately
/// unexported on Linux (glibc versions its stat symbols), so the Linux arm
/// goes through the statx syscall - the stable kernel interface std.Io's own
/// backend uses - and other POSIX systems use libc fstat.
pub fn fstatFd(fd: std.c.fd_t) error{StatFailed}!FdStat {
    if (builtin.os.tag == .linux) {
        const linux = std.os.linux;
        var stx = std.mem.zeroes(linux.Statx);
        const rc = linux.statx(fd, "", linux.AT.EMPTY_PATH, .{ .TYPE = true, .MODE = true, .SIZE = true }, &stx);
        if (linux.errno(rc) != .SUCCESS) return error.StatFailed;
        return .{ .size = stx.size, .mode = stx.mode };
    }
    var st: std.c.Stat = undefined;
    if (std.c.fstat(fd, &st) != 0) return error.StatFailed;
    return .{ .size = @intCast(st.size), .mode = st.mode };
}

/// Open a file for append writes, creating it (0600, see writeFile) if
/// missing. Returns the raw POSIX fd; caller is responsible for closing with
/// `std.Io.Threaded.closeFd`.
pub fn openAppend(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    return std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true },
        0o600,
    ) catch return error.FileOpenFailed;
}

/// Read a file for ModuleGraph.build() - wraps readFile with ReadFileError mapping.
pub fn readFileForModuleGraph(allocator: std.mem.Allocator, path: []const u8) module_graph.ReadFileError![]const u8 {
    return readFile(allocator, path, 10 * 1024 * 1024) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.FileTooBig => return error.FileTooBig,
        else => return error.FileNotFound,
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

// std.testing.tmpDir creates `.zig-cache/tmp/<sub_path>` under cwd but exposes
// only an `Io.Dir` and the sub_path string. Reconstruct the absolute path from
// cwd so the absolute-path file ops in this module can reach the tmpdir.
fn tmpFilePath(allocator: std.mem.Allocator, tmp: std.testing.TmpDir, name: []const u8) ![]u8 {
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.c.getcwd(&cwd_buf, cwd_buf.len) == null) return error.CwdUnavailable;
    const cwd = std.mem.sliceTo(&cwd_buf, 0);
    return std.fs.path.join(allocator, &.{ cwd, ".zig-cache", "tmp", &tmp.sub_path, name });
}

fn statMode(allocator: std.mem.Allocator, path: []const u8) !u32 {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);
    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.Io.Threaded.closeFd(fd);
    return (try fstatFd(fd)).mode;
}

test "writeFile creates new files with mode 0600" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const path = try tmpFilePath(allocator, tmp, "transcript.jsonl");
    defer allocator.free(path);
    try writeFile(allocator, path, "line\n");

    const mode = try statMode(allocator, path);
    try testing.expectEqual(@as(u32, 0o600), mode & 0o777);
}

test "writeFileCreateExclusive preserves an existing file" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const path = try tmpFilePath(allocator, tmp, "README.md");
    defer allocator.free(path);
    try writeFileCreateExclusive(allocator, path, "original\n");
    try testing.expectError(error.PathAlreadyExists, writeFileCreateExclusive(allocator, path, "replacement\n"));

    const contents = try readFile(allocator, path, 1024);
    defer allocator.free(contents);
    try testing.expectEqualStrings("original\n", contents);
}

const ExclusiveRaceTarget = struct {
    allocator: std.mem.Allocator,
    contents: []const u8,

    fn create(context: ?*anyopaque, path: []const u8) !void {
        const self: *ExclusiveRaceTarget = @ptrCast(@alignCast(context.?));
        try writeFileCreateExclusive(self.allocator, path, self.contents);
    }
};

test "writeFileCreateExclusive does not clobber a target that appears before commit" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const path = try tmpFilePath(allocator, tmp, "race.txt");
    defer allocator.free(path);
    var race = ExclusiveRaceTarget{ .allocator = allocator, .contents = "winner\n" };

    try testing.expectError(error.PathAlreadyExists, writeFileCreateExclusiveWithCommit(
        allocator,
        path,
        "loser\n",
        .{ .context = &race, .before_link = ExclusiveRaceTarget.create },
    ));

    const contents = try readFile(allocator, path, 1024);
    defer allocator.free(contents);
    try testing.expectEqualStrings("winner\n", contents);
}

test "openAppend creates new files with mode 0600" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const path = try tmpFilePath(allocator, tmp, "memory.md");
    defer allocator.free(path);
    const fd = try openAppend(allocator, path);
    std.Io.Threaded.closeFd(fd);

    const mode = try statMode(allocator, path);
    try testing.expectEqual(@as(u32, 0o600), mode & 0o777);
}
