const std = @import("std");
const zigts = @import("zigts");
const types = @import("types.zig");

pub const Options = struct {
    confirm: bool = false,
    // Borrowed from argv: the caller owns argv for the life of the deploy call.
    region: ?[]const u8 = null,
    // Default: block until the service reports ready. --no-wait opts out.
    // Only the bare --wait / --no-wait spellings are accepted; --wait=... is rejected.
    wait: bool = true,
};

pub fn parse(argv: []const []const u8) !Options {
    var options = Options{};
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return error.HelpRequested;
        }
        if (std.mem.eql(u8, arg, "--confirm")) {
            options.confirm = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--wait")) {
            options.wait = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--no-wait")) {
            options.wait = false;
            continue;
        }
        if (std.mem.eql(u8, arg, "--region")) {
            // --region requires a following value; MissingOptionValue distinguishes it
            // from UnknownOption so the CLI can emit a targeted diagnostic.
            if (i + 1 >= argv.len) return error.MissingOptionValue;
            i += 1;
            options.region = argv[i];
            continue;
        }
        return error.UnknownOption;
    }
    return options;
}

pub fn loadEnvFile(allocator: std.mem.Allocator, path: []const u8) ![]const types.EnvVar {
    const bytes = zigts.file_io.readFile(allocator, path, 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return allocator.alloc(types.EnvVar, 0),
        else => return err,
    };
    defer allocator.free(bytes);

    var list = std.ArrayList(types.EnvVar).empty;
    errdefer {
        for (list.items) |item| {
            allocator.free(item.key);
            allocator.free(item.value);
        }
        list.deinit(allocator);
    }

    var lines = std.mem.splitScalar(u8, bytes, '\n');
    var line_no: usize = 0;
    while (lines.next()) |line_raw| {
        line_no += 1;
        const line = std.mem.trim(u8, line_raw, " \t\r");
        if (line.len == 0 or line[0] == '#') continue;
        const eq_idx = std.mem.indexOfScalar(u8, line, '=') orelse {
            reportBadLine(path, line_no);
            return error.InvalidEnvFile;
        };
        const key = std.mem.trim(u8, line[0..eq_idx], " \t");
        const value = std.mem.trim(u8, line[eq_idx + 1 ..], " \t");
        if (key.len == 0) {
            reportBadLine(path, line_no);
            return error.InvalidEnvFile;
        }
        try list.append(allocator, .{
            .key = try allocator.dupe(u8, key),
            .value = try allocator.dupe(u8, value),
        });
    }

    return try list.toOwnedSlice(allocator);
}

fn reportBadLine(path: []const u8, line_no: usize) void {
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "{s}:{d}: expected KEY=value\n", .{ path, line_no }) catch {
        const fallback = "env: malformed line\n";
        _ = std.c.write(std.c.STDERR_FILENO, fallback.ptr, fallback.len);
        return;
    };
    _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
}

test "parse accepts empty args" {
    const options = try parse(&.{});
    try std.testing.expect(!options.confirm);
}

test "parse defaults wait to true" {
    const options = try parse(&.{});
    try std.testing.expect(options.wait);
}

test "parse accepts --no-wait" {
    const options = try parse(&.{"--no-wait"});
    try std.testing.expect(!options.wait);
}

test "parse accepts explicit --wait" {
    const options = try parse(&.{"--wait"});
    try std.testing.expect(options.wait);
}

test "parse rejects --wait= value form" {
    try std.testing.expectError(error.UnknownOption, parse(&.{"--wait=true"}));
    try std.testing.expectError(error.UnknownOption, parse(&.{"--wait=false"}));
}

test "parse accepts --wait --region --confirm in any order" {
    const a = try parse(&.{ "--no-wait", "--region", "eu-west", "--confirm" });
    try std.testing.expect(a.confirm);
    try std.testing.expect(!a.wait);
    try std.testing.expectEqualStrings("eu-west", a.region.?);

    const b = try parse(&.{ "--confirm", "--wait", "--region", "us-east" });
    try std.testing.expect(b.confirm);
    try std.testing.expect(b.wait);
    try std.testing.expectEqualStrings("us-east", b.region.?);
}

test "parse accepts confirm" {
    const options = try parse(&.{"--confirm"});
    try std.testing.expect(options.confirm);
}

test "parse rejects legacy flags" {
    try std.testing.expectError(error.UnknownOption, parse(&.{"--provider"}));
    try std.testing.expectError(error.UnknownOption, parse(&.{"--name"}));
    try std.testing.expectError(error.UnknownOption, parse(&.{"handler.ts"}));
}

test "parse surfaces help" {
    try std.testing.expectError(error.HelpRequested, parse(&.{"--help"}));
}

test "parse accepts --region" {
    const options = try parse(&.{ "--region", "us-east" });
    try std.testing.expect(options.region != null);
    try std.testing.expectEqualStrings("us-east", options.region.?);
}

test "parse rejects --region without value" {
    try std.testing.expectError(error.MissingOptionValue, parse(&.{"--region"}));
}

test "parse accepts --region and --confirm together" {
    const a = try parse(&.{ "--region", "eu-west", "--confirm" });
    try std.testing.expect(a.confirm);
    try std.testing.expectEqualStrings("eu-west", a.region.?);

    const b = try parse(&.{ "--confirm", "--region", "eu-west" });
    try std.testing.expect(b.confirm);
    try std.testing.expectEqualStrings("eu-west", b.region.?);
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

test "loadEnvFile rejects malformed line with diagnostic" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "bad.env",
        .data =
        \\GOOD=1
        \\bad_line_no_equals
        \\OTHER=2
        ,
    });

    const path = try std.fs.path.resolve(std.testing.allocator, &.{ tmp.sub_path, "bad.env" });
    defer std.testing.allocator.free(path);

    try std.testing.expectError(error.InvalidEnvFile, loadEnvFile(std.testing.allocator, path));
}

test "loadEnvFile returns empty slice for missing file" {
    const env_vars = try loadEnvFile(std.testing.allocator, "/tmp/zigttp-nonexistent-env.env");
    defer std.testing.allocator.free(env_vars);
    try std.testing.expectEqual(@as(usize, 0), env_vars.len);
}
