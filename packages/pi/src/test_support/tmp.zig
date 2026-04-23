//! Shared test scaffolding for isolated `/tmp` directories. Consolidates
//! the previously-per-file `IsolatedTmp` helpers into one definition. Each
//! caller supplies its own label prefix so failure messages still indicate
//! which test created the directory.

const std = @import("std");

var tmp_counter = std.atomic.Value(u64).init(0);

pub const IsolatedTmp = struct {
    abs_path: []u8,
    name: []u8,

    pub fn init(allocator: std.mem.Allocator, label: []const u8) !IsolatedTmp {
        var ts: std.posix.timespec = undefined;
        _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
        const counter = tmp_counter.fetchAdd(1, .seq_cst);
        const name = try std.fmt.allocPrint(
            allocator,
            "zigttp-{s}-test-{d}-{d}-{d}",
            .{ label, @as(u64, @intCast(ts.sec)), @as(u64, @intCast(ts.nsec)), counter },
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

    pub fn cleanup(self: *IsolatedTmp, allocator: std.mem.Allocator) void {
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

    pub fn childPath(self: *const IsolatedTmp, allocator: std.mem.Allocator, name: []const u8) ![]u8 {
        return try std.fs.path.resolve(allocator, &.{ self.abs_path, name });
    }

    pub fn mkdir(self: *const IsolatedTmp, allocator: std.mem.Allocator, rel: []const u8) !void {
        const path = try self.childPath(allocator, rel);
        defer allocator.free(path);
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), path);
    }

    pub fn writeFile(
        self: *const IsolatedTmp,
        allocator: std.mem.Allocator,
        rel: []const u8,
        body: []const u8,
    ) !void {
        const path = try self.childPath(allocator, rel);
        defer allocator.free(path);
        if (std.fs.path.dirname(path)) |parent| {
            var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
            defer io_backend.deinit();
            try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), parent);
        }
        const zigts = @import("zigts");
        try zigts.file_io.writeFile(allocator, path, body);
    }
};
