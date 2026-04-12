const std = @import("std");
const zigts = @import("zigts");
const plan = @import("plan.zig");

pub fn parse(argv: []const []const u8) !void {
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return error.HelpRequested;
        }
        return error.UnknownOption;
    }
}

pub fn loadEnvFile(allocator: std.mem.Allocator, path: []const u8) ![]const plan.EnvVar {
    const bytes = zigts.file_io.readFile(allocator, path, 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return allocator.alloc(plan.EnvVar, 0),
        else => return err,
    };
    defer allocator.free(bytes);

    var list = std.ArrayList(plan.EnvVar).empty;
    errdefer {
        for (list.items) |item| {
            allocator.free(item.key);
            allocator.free(item.value);
        }
        list.deinit(allocator);
    }

    var lines = std.mem.splitScalar(u8, bytes, '\n');
    while (lines.next()) |line_raw| {
        const line = std.mem.trim(u8, line_raw, " \t\r");
        if (line.len == 0 or line[0] == '#') continue;
        const eq_idx = std.mem.indexOfScalar(u8, line, '=') orelse return error.InvalidEnvFile;
        const key = std.mem.trim(u8, line[0..eq_idx], " \t");
        const value = std.mem.trim(u8, line[eq_idx + 1 ..], " \t");
        if (key.len == 0) return error.InvalidEnvFile;
        try list.append(allocator, .{
            .key = try allocator.dupe(u8, key),
            .value = try allocator.dupe(u8, value),
        });
    }

    return try list.toOwnedSlice(allocator);
}

test "parse accepts empty args" {
    try parse(&.{});
}

test "parse rejects legacy flags" {
    try std.testing.expectError(error.UnknownOption, parse(&.{"--provider"}));
    try std.testing.expectError(error.UnknownOption, parse(&.{"--name"}));
    try std.testing.expectError(error.UnknownOption, parse(&.{"handler.ts"}));
}

test "parse surfaces help" {
    try std.testing.expectError(error.HelpRequested, parse(&.{"--help"}));
}

test "loadEnvFile parses key value pairs" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "deploy.env",
        .data =
        \\API_KEY=secret
        \\PORT=3000
        ,
    });

    const path = try std.fs.path.resolve(std.testing.allocator, &.{ tmp.sub_path, "deploy.env" });
    defer std.testing.allocator.free(path);

    const env_vars = try loadEnvFile(std.testing.allocator, path);
    defer {
        for (env_vars) |item| {
            std.testing.allocator.free(item.key);
            std.testing.allocator.free(item.value);
        }
        std.testing.allocator.free(env_vars);
    }

    try std.testing.expectEqual(@as(usize, 2), env_vars.len);
    try std.testing.expectEqualStrings("API_KEY", env_vars[0].key);
    try std.testing.expectEqualStrings("3000", env_vars[1].value);
}

test "loadEnvFile returns empty slice for missing file" {
    const env_vars = try loadEnvFile(std.testing.allocator, "/tmp/zigttp-nonexistent-env.env");
    defer std.testing.allocator.free(env_vars);
    try std.testing.expectEqual(@as(usize, 0), env_vars.len);
}
