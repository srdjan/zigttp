//! Small POSIX I/O helpers shared by precompile.zig and its sibling
//! modules. Lives in its own file so sibling modules don't need to
//! back-reference precompile.zig just for filesystem utilities.

const std = @import("std");

pub fn writeFilePosix(path: []const u8, data: []const u8, allocator: std.mem.Allocator) !void {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = try std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        0o644,
    );
    defer std.Io.Threaded.closeFd(fd);

    var total_written: usize = 0;
    while (total_written < data.len) {
        const result = std.c.write(fd, data[total_written..].ptr, data.len - total_written);
        if (result < 0) return error.WriteFailure;
        if (result == 0) return error.WriteFailure;
        total_written += @intCast(result);
    }
}

/// Derive a sibling path by replacing the filename portion with a suffix.
/// E.g. deriveSiblingPath("src/generated/foo.zig", "contract.json") -> "src/generated/contract.json"
pub fn deriveSiblingPath(allocator: std.mem.Allocator, output_path: []const u8, suffix: []const u8) ![]u8 {
    var dir_end: usize = 0;
    for (output_path, 0..) |c, i| {
        if (c == '/') dir_end = i + 1;
    }
    const dir = output_path[0..dir_end];
    const result = try allocator.alloc(u8, dir.len + suffix.len);
    @memcpy(result[0..dir.len], dir);
    @memcpy(result[dir.len..], suffix);
    return result;
}
