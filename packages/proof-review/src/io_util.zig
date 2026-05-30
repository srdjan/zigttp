const std = @import("std");

pub fn threadedIo(allocator: std.mem.Allocator) std.Io.Threaded {
    return std.Io.Threaded.init(allocator, .{ .environ = .empty });
}
