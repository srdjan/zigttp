const std = @import("std");
const project_config_mod = @import("project_config");
const zigts = @import("zigts");

pub fn collectArgs(allocator: std.mem.Allocator, args_vector: std.process.Args) ![]const []const u8 {
    var args_iter = std.process.Args.Iterator.init(args_vector);
    defer args_iter.deinit();

    var args = std.ArrayList([]const u8).empty;
    errdefer args.deinit(allocator);
    while (args_iter.next()) |arg| {
        try args.append(allocator, try allocator.dupe(u8, arg));
    }
    return args.toOwnedSlice(allocator);
}

pub fn printVersion() void {
    const version = "zigttp " ++ zigts.version.string ++ "\n";
    _ = std.c.write(std.c.STDOUT_FILENO, version.ptr, version.len);
}

pub fn writeStdoutLine(s: []const u8) void {
    _ = std.c.write(std.c.STDOUT_FILENO, s.ptr, s.len);
    _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
}

pub fn findPositionalPath(argv: []const []const u8) ?[]const u8 {
    var skip_next = false;
    for (argv) |arg| {
        if (skip_next) {
            skip_next = false;
            continue;
        }
        if (std.mem.eql(u8, arg, "-p") or
            std.mem.eql(u8, arg, "--port") or
            std.mem.eql(u8, arg, "-h") or
            std.mem.eql(u8, arg, "--host") or
            std.mem.eql(u8, arg, "-e") or
            std.mem.eql(u8, arg, "--eval") or
            std.mem.eql(u8, arg, "-m") or
            std.mem.eql(u8, arg, "--memory") or
            std.mem.eql(u8, arg, "-n") or
            std.mem.eql(u8, arg, "--pool") or
            std.mem.eql(u8, arg, "--static") or
            std.mem.eql(u8, arg, "--outbound-host") or
            std.mem.eql(u8, arg, "--outbound-timeout-ms") or
            std.mem.eql(u8, arg, "--outbound-max-response") or
            std.mem.eql(u8, arg, "--sqlite") or
            std.mem.eql(u8, arg, "--trace") or
            std.mem.eql(u8, arg, "--replay") or
            std.mem.eql(u8, arg, "--test") or
            std.mem.eql(u8, arg, "--durable") or
            std.mem.eql(u8, arg, "--system"))
        {
            skip_next = true;
            continue;
        }
        if (std.mem.startsWith(u8, arg, "-")) continue;
        return arg;
    }
    return null;
}

pub fn looksLikeHandlerFile(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".js") or
        std.mem.endsWith(u8, path, ".jsx") or
        std.mem.endsWith(u8, path, ".ts") or
        std.mem.endsWith(u8, path, ".tsx");
}

pub fn parseSize(str: []const u8) !usize {
    var num_end: usize = 0;
    for (str, 0..) |c, i| {
        if (c >= '0' and c <= '9') {
            num_end = i + 1;
        } else {
            break;
        }
    }

    if (num_end == 0) return error.InvalidSize;

    const num = try std.fmt.parseInt(usize, str[0..num_end], 10);
    const suffix = str[num_end..];

    const multiplier: usize = if (suffix.len == 0)
        1
    else if (std.ascii.eqlIgnoreCase(suffix, "k") or std.ascii.eqlIgnoreCase(suffix, "kb"))
        1024
    else if (std.ascii.eqlIgnoreCase(suffix, "m") or std.ascii.eqlIgnoreCase(suffix, "mb"))
        1024 * 1024
    else if (std.ascii.eqlIgnoreCase(suffix, "g") or std.ascii.eqlIgnoreCase(suffix, "gb"))
        1024 * 1024 * 1024
    else
        return error.InvalidSizeSuffix;

    return std.math.mul(usize, num, multiplier) catch return error.InvalidSize;
}

pub const WatchSet = struct {
    paths: []const []const u8,

    pub fn deinit(self: *WatchSet, allocator: std.mem.Allocator) void {
        for (self.paths) |path| allocator.free(path);
        allocator.free(self.paths);
    }

    pub fn computeStamp(self: *const WatchSet, io: std.Io) !u64 {
        var hash = std.hash.Wyhash.init(0);
        for (self.paths) |path| {
            try foldPathIntoHash(io, &hash, path);
        }
        return hash.final();
    }
};

pub fn buildWatchSet(allocator: std.mem.Allocator, argv: []const []const u8) !WatchSet {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const explicit_path = findPositionalPath(argv);
    var project = try project_config_mod.discover(allocator, io, explicit_path);
    defer if (project) |*p| p.deinit(allocator);

    var paths = std.ArrayList([]const u8).empty;
    errdefer {
        for (paths.items) |path| allocator.free(path);
        paths.deinit(allocator);
    }

    if (project) |*cfg| {
        try paths.append(allocator, try allocator.dupe(u8, cfg.manifest_path));
        try paths.append(allocator, try std.fs.path.resolve(allocator, &.{ cfg.root_dir, "src" }));
        if (try cfg.resolvedStaticDir(allocator)) |static_dir| {
            try paths.append(allocator, static_dir);
        }
        if (try cfg.resolvedSystemPath(allocator)) |system_path| {
            try paths.append(allocator, system_path);
        }
    } else if (explicit_path) |path| {
        if (looksLikeHandlerFile(path)) {
            const abs = try std.fs.path.resolve(allocator, &.{path});
            const dir_name = std.fs.path.dirname(abs) orelse ".";
            defer allocator.free(abs);
            try paths.append(allocator, try allocator.dupe(u8, dir_name));
        }
    }

    if (paths.items.len == 0) {
        try paths.append(allocator, try std.fs.path.resolve(allocator, &.{"."}));
    }

    return .{ .paths = try paths.toOwnedSlice(allocator) };
}

pub fn foldPathIntoHash(io: std.Io, hash: *std.hash.Wyhash, path: []const u8) !void {
    const stat = std.Io.Dir.statFile(std.Io.Dir.cwd(), io, path, .{}) catch |err| switch (err) {
        error.FileNotFound, error.NotDir => return,
        else => return err,
    };

    hash.update(path);
    hash.update(std.mem.asBytes(&stat.mtime.nanoseconds));
    hash.update(std.mem.asBytes(&stat.size));

    if (stat.kind != .directory) return;

    var dir = try std.Io.Dir.openDir(std.Io.Dir.cwd(), io, path, .{ .iterate = true });
    defer dir.close(io);

    var walker = try dir.walk(std.heap.smp_allocator);
    defer walker.deinit();

    while (try walker.next(io)) |entry| {
        const entry_stat = try std.Io.Dir.statFile(entry.dir, io, entry.basename, .{});
        hash.update(entry.path);
        hash.update(std.mem.asBytes(&entry_stat.mtime.nanoseconds));
        hash.update(std.mem.asBytes(&entry_stat.size));
    }
}

test "parse size" {
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1k"));
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1K"));
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1kb"));
    try std.testing.expectEqual(@as(usize, 256 * 1024), try parseSize("256k"));
    try std.testing.expectEqual(@as(usize, 1024 * 1024), try parseSize("1m"));
    try std.testing.expectEqual(@as(usize, 1024 * 1024), try parseSize("1mb"));
    try std.testing.expectEqual(@as(usize, 100), try parseSize("100"));
}
