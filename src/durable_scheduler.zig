//! Durable wait scheduler.
//!
//! This is intentionally small for v1: a background thread polls the durable
//! directory and reuses the existing recovery path. Each durable run decides
//! locally whether its pending timer/signal is ready yet.

const std = @import("std");
const durable_recovery = @import("durable_recovery.zig");
const ServerConfig = @import("server.zig").ServerConfig;

pub const DurableScheduler = struct {
    config: ServerConfig,
    stop_requested: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    thread: ?std.Thread = null,

    pub fn start(self: *DurableScheduler) !void {
        self.thread = try std.Thread.spawn(.{}, runLoop, .{self});
    }

    pub fn deinit(self: *DurableScheduler) void {
        self.stop_requested.store(true, .release);
        if (self.thread) |thread| {
            thread.join();
            self.thread = null;
        }
    }

    fn runLoop(self: *DurableScheduler) void {
        var gpa: std.heap.DebugAllocator(.{}) = .init;
        defer _ = gpa.deinit();
        const allocator = gpa.allocator();

        while (!self.stop_requested.load(.acquire)) {
            _ = durable_recovery.recoverIncompleteOplogs(allocator, self.config) catch |err| {
                std.log.err("Durable scheduler poll failed: {}", .{err});
            };
            if (self.stop_requested.load(.acquire)) break;
            sleepOneSecond();
        }
    }
};

fn sleepOneSecond() void {
    var ts: std.posix.timespec = .{ .sec = 1, .nsec = 0 };
    while (true) {
        switch (std.posix.errno(std.posix.system.nanosleep(&ts, &ts))) {
            .INTR => continue,
            else => return,
        }
    }
}
