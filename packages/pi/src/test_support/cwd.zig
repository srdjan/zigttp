//! Tiny helper: resolve the current working directory to an
//! allocator-owned plain `[]u8` (sentinel stripped). Shared between the
//! agent and session tests.

const std = @import("std");

pub fn cwdPathAlloc(allocator: std.mem.Allocator) ![]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    // realPathFileAlloc returns [:0]u8 (sentinel-terminated, len+1 allocation).
    // dupe strips the sentinel so the caller can free a plain []u8 symmetrically.
    const p = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io, ".", allocator);
    defer allocator.free(p);
    return allocator.dupe(u8, p);
}
