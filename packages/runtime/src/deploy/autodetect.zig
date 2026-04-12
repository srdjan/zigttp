const std = @import("std");
const file_io = @import("zigts").file_io;

const handler_candidates = [_][]const u8{
    "handler.ts",
    "handler.tsx",
    "handler.jsx",
    "handler.js",
    "src/handler.ts",
    "src/handler.tsx",
    "src/handler.jsx",
    "src/handler.js",
};

pub fn detectHandler(allocator: std.mem.Allocator) ![]u8 {
    for (handler_candidates) |rel| {
        if (file_io.fileExists(allocator, rel)) {
            return allocator.dupe(u8, rel);
        }
    }
    return error.HandlerNotFound;
}

pub fn detectName(allocator: std.mem.Allocator) ![]u8 {
    if (try readPackageJsonName(allocator)) |name| {
        defer allocator.free(name);
        return try sanitize(allocator, name);
    }
    if (try readGitRemoteBasename(allocator)) |name| {
        defer allocator.free(name);
        return try sanitize(allocator, name);
    }
    if (try cwdBasename(allocator)) |name| {
        defer allocator.free(name);
        return try sanitize(allocator, name);
    }
    return error.NameUndetectable;
}

fn readPackageJsonName(allocator: std.mem.Allocator) !?[]u8 {
    const bytes = file_io.readFile(allocator, "package.json", 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return null,
        else => return err,
    };
    defer allocator.free(bytes);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, bytes, .{}) catch return null;
    defer parsed.deinit();
    if (parsed.value != .object) return null;
    const root = parsed.value.object;
    const name_value = root.get("name") orelse return null;
    if (name_value != .string) return null;
    if (name_value.string.len == 0) return null;
    return try allocator.dupe(u8, name_value.string);
}

fn readGitRemoteBasename(allocator: std.mem.Allocator) !?[]u8 {
    const bytes = file_io.readFile(allocator, ".git/config", 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return null,
        else => return err,
    };
    defer allocator.free(bytes);

    var in_origin = false;
    var lines = std.mem.splitScalar(u8, bytes, '\n');
    while (lines.next()) |raw| {
        const line = std.mem.trim(u8, raw, " \t\r");
        if (line.len == 0) continue;
        if (line[0] == '[') {
            in_origin = std.mem.eql(u8, line, "[remote \"origin\"]");
            continue;
        }
        if (!in_origin) continue;
        if (!std.mem.startsWith(u8, line, "url")) continue;
        const eq_idx = std.mem.indexOfScalar(u8, line, '=') orelse continue;
        const url = std.mem.trim(u8, line[eq_idx + 1 ..], " \t");
        if (url.len == 0) continue;
        return try basenameFromGitUrl(allocator, url);
    }
    return null;
}

fn basenameFromGitUrl(allocator: std.mem.Allocator, url: []const u8) ![]u8 {
    var trimmed = url;
    if (std.mem.endsWith(u8, trimmed, ".git")) trimmed = trimmed[0 .. trimmed.len - 4];
    const slash_idx = std.mem.lastIndexOfScalar(u8, trimmed, '/') orelse {
        const colon_idx = std.mem.lastIndexOfScalar(u8, trimmed, ':') orelse return allocator.dupe(u8, trimmed);
        return allocator.dupe(u8, trimmed[colon_idx + 1 ..]);
    };
    return allocator.dupe(u8, trimmed[slash_idx + 1 ..]);
}

fn cwdBasename(allocator: std.mem.Allocator) !?[]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const cwd = std.process.currentPathAlloc(io_backend.io(), allocator) catch return null;
    defer allocator.free(cwd);
    const last = std.fs.path.basename(cwd);
    if (last.len == 0) return null;
    return try allocator.dupe(u8, last);
}

pub fn sanitize(allocator: std.mem.Allocator, name: []const u8) ![]u8 {
    var buf = try allocator.alloc(u8, name.len);
    errdefer allocator.free(buf);

    var len: usize = 0;
    var last_was_separator = true;
    for (name) |raw| {
        const c: u8 = switch (raw) {
            'A'...'Z' => raw - 'A' + 'a',
            'a'...'z', '0'...'9' => raw,
            else => '-',
        };
        if (c == '-') {
            if (last_was_separator) continue;
            last_was_separator = true;
        } else {
            last_was_separator = false;
        }
        buf[len] = c;
        len += 1;
    }
    while (len > 0 and buf[len - 1] == '-') len -= 1;

    if (len == 0) {
        allocator.free(buf);
        return error.NameUndetectable;
    }
    return allocator.realloc(buf, len);
}

test "sanitize collapses and lowercases" {
    const cases = [_]struct { in: []const u8, out: []const u8 }{
        .{ .in = "My-Demo", .out = "my-demo" },
        .{ .in = "--Name--", .out = "name" },
        .{ .in = "weird__name", .out = "weird-name" },
        .{ .in = "already-ok", .out = "already-ok" },
    };
    for (cases) |case| {
        const got = try sanitize(std.testing.allocator, case.in);
        defer std.testing.allocator.free(got);
        try std.testing.expectEqualStrings(case.out, got);
    }
}

test "detectHandler finds handler.ts in cwd" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "handler.ts", .data = "// stub" });

    const tmp_path = try tmp.dir.realpathAlloc(std.testing.io, allocator, ".");
    defer allocator.free(tmp_path);

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const cwd_before = try std.process.currentPathAlloc(io_backend.io(), allocator);
    defer allocator.free(cwd_before);
    try std.posix.chdir(tmp_path);
    defer std.posix.chdir(cwd_before) catch {};

    const found = try detectHandler(allocator);
    defer allocator.free(found);
    try std.testing.expectEqualStrings("handler.ts", found);
}

test "detectHandler errors when nothing matches" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(std.testing.io, allocator, ".");
    defer allocator.free(tmp_path);

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const cwd_before = try std.process.currentPathAlloc(io_backend.io(), allocator);
    defer allocator.free(cwd_before);
    try std.posix.chdir(tmp_path);
    defer std.posix.chdir(cwd_before) catch {};

    try std.testing.expectError(error.HandlerNotFound, detectHandler(allocator));
}

test "basenameFromGitUrl handles ssh and https" {
    const cases = [_]struct { in: []const u8, out: []const u8 }{
        .{ .in = "git@github.com:acme/demo.git", .out = "demo" },
        .{ .in = "https://github.com/acme/Zigttp-Demo.git", .out = "Zigttp-Demo" },
        .{ .in = "https://github.com/acme/noext", .out = "noext" },
    };
    for (cases) |case| {
        const got = try basenameFromGitUrl(std.testing.allocator, case.in);
        defer std.testing.allocator.free(got);
        try std.testing.expectEqualStrings(case.out, got);
    }
}
