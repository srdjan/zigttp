const std = @import("std");
const zigts = @import("zigts");
const io_util = @import("io_util.zig");

pub const BuildResult = struct {
    project_root: []const u8,
    binary_path: []const u8,
    binary_bytes: []u8,
    build_command: []const u8,
    binary_sha256: []const u8,

    pub fn deinit(self: *BuildResult, allocator: std.mem.Allocator) void {
        allocator.free(self.project_root);
        allocator.free(self.binary_path);
        allocator.free(self.binary_bytes);
        allocator.free(self.build_command);
        allocator.free(self.binary_sha256);
    }
};

pub fn buildLinuxArtifact(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    target_triple: []const u8,
) !BuildResult {
    var io_backend = io_util.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    const project_root = try findProjectRoot(allocator, io);
    errdefer allocator.free(project_root);
    const resolved_handler = try resolveAgainstCwd(allocator, io, handler_path);
    defer allocator.free(resolved_handler);
    const build_prefix = try std.fs.path.resolve(allocator, &.{ project_root, ".zigttp", "deploy-build", target_triple });
    defer allocator.free(build_prefix);
    const zig_exe = try resolveZigExecutable(allocator);
    defer allocator.free(zig_exe);
    var environ_map = try buildChildEnviron(allocator);
    defer environ_map.deinit();

    try ensureZigAvailable(allocator, io, zig_exe, &environ_map);

    const argv = [_][]const u8{
        zig_exe,
        "build",
        try std.fmt.allocPrint(allocator, "-Dhandler={s}", .{resolved_handler}),
        "-Doptimize=ReleaseFast",
        try std.fmt.allocPrint(allocator, "-Dtarget={s}", .{target_triple}),
        "--prefix",
        build_prefix,
    };
    defer allocator.free(argv[2]);
    defer allocator.free(argv[4]);

    const build_command = try std.mem.join(allocator, " ", &argv);
    errdefer allocator.free(build_command);

    const result = try std.process.run(allocator, io, .{
        .argv = &argv,
        .cwd = .{ .path = project_root },
        .environ_map = &environ_map,
        .stderr_limit = .limited(1024 * 1024),
        .stdout_limit = .limited(256 * 1024),
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .exited => |code| if (code != 0) {
            std.log.err("zig build failed:\n{s}", .{result.stderr});
            return error.BuildFailed;
        },
        else => return error.BuildFailed,
    }

    const binary_path = try std.fs.path.resolve(allocator, &.{ build_prefix, "bin", "zigttp" });
    errdefer allocator.free(binary_path);
    const binary_bytes = try zigts.file_io.readFile(allocator, binary_path, 256 * 1024 * 1024);
    errdefer allocator.free(binary_bytes);

    return .{
        .project_root = project_root,
        .binary_path = binary_path,
        .binary_bytes = binary_bytes,
        .build_command = build_command,
        .binary_sha256 = try sha256DigestString(allocator, binary_bytes),
    };
}

pub fn findProjectRoot(allocator: std.mem.Allocator, io: std.Io) ![]u8 {
    const current_z = try std.process.currentPathAlloc(io, allocator);
    defer allocator.free(current_z);
    var current = try allocator.dupe(u8, current_z);
    errdefer allocator.free(current);
    while (true) {
        const build_path = try std.fs.path.resolve(allocator, &.{ current, "build.zig" });
        defer allocator.free(build_path);
        if (std.Io.Dir.accessAbsolute(io, build_path, .{})) |_| {
            return current;
        } else |_| {}

        const parent = std.fs.path.dirname(current) orelse break;
        if (parent.len == current.len) break;
        const next = try allocator.dupe(u8, parent);
        allocator.free(current);
        current = next;
    }
    return error.ProjectRootNotFound;
}

fn ensureZigAvailable(
    allocator: std.mem.Allocator,
    io: std.Io,
    zig_exe: []const u8,
    environ_map: *const std.process.Environ.Map,
) !void {
    const result = try std.process.run(allocator, io, .{
        .argv = &.{ zig_exe, "version" },
        .environ_map = environ_map,
        .stdout_limit = .limited(1024),
        .stderr_limit = .limited(1024),
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    switch (result.term) {
        .exited => |code| if (code != 0) return error.ZigUnavailable,
        else => return error.ZigUnavailable,
    }
}

fn resolveAgainstCwd(allocator: std.mem.Allocator, io: std.Io, path: []const u8) ![]u8 {
    if (std.fs.path.isAbsolute(path)) return allocator.dupe(u8, path);
    const cwd = try std.process.currentPathAlloc(io, allocator);
    defer allocator.free(cwd);
    return std.fs.path.resolve(allocator, &.{ cwd, path });
}

fn resolveZigExecutable(allocator: std.mem.Allocator) ![]u8 {
    if (std.c.getenv("PATH")) |raw_path| {
        const path_value = std.mem.sliceTo(raw_path, 0);
        var parts = std.mem.splitScalar(u8, path_value, ':');
        while (parts.next()) |part| {
            if (part.len == 0) continue;
            const candidate = try std.fmt.allocPrint(allocator, "{s}/zig", .{part});
            errdefer allocator.free(candidate);
            if (zigts.file_io.fileExists(allocator, candidate)) {
                return candidate;
            }
            allocator.free(candidate);
        }
    }
    return error.ZigUnavailable;
}

fn buildChildEnviron(allocator: std.mem.Allocator) !std.process.Environ.Map {
    var env_map = std.process.Environ.Map.init(allocator);
    errdefer env_map.deinit();
    inline for (&.{
        "HOME",
        "PATH",
        "TMPDIR",
        "XDG_CACHE_HOME",
        "ZIG_GLOBAL_CACHE_DIR",
        "ZIG_LOCAL_CACHE_DIR",
        "NO_COLOR",
    }) |name| {
        if (std.c.getenv(name)) |raw| {
            try env_map.put(name, std.mem.sliceTo(raw, 0));
        }
    }
    return env_map;
}

fn sha256DigestString(allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
    var digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytes, &digest, .{});
    return std.fmt.allocPrint(allocator, "sha256:{s}", .{std.fmt.bytesToHex(digest, .lower)});
}
