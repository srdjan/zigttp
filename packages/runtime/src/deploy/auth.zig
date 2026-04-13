const std = @import("std");
const io_util = @import("io_util.zig");
const json_util = @import("json_util.zig");

pub const Credentials = struct {
    token: []u8,
    email: ?[]u8 = null,

    pub fn deinit(self: *Credentials, allocator: std.mem.Allocator) void {
        allocator.free(self.token);
        if (self.email) |value| allocator.free(value);
    }
};

const rel_dir = ".zigttp";
const rel_file = ".zigttp/credentials";

fn homeDir(allocator: std.mem.Allocator) ![]u8 {
    const raw = std.c.getenv("HOME") orelse return error.HomeDirMissing;
    return allocator.dupe(u8, std.mem.sliceTo(raw, 0));
}

fn openHome(allocator: std.mem.Allocator, io: std.Io) !std.Io.Dir {
    const home = try homeDir(allocator);
    defer allocator.free(home);
    return std.Io.Dir.openDirAbsolute(io, home, .{});
}

pub fn load(allocator: std.mem.Allocator) !Credentials {
    var io_backend = io_util.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    var home = openHome(allocator, io) catch return error.NotSignedIn;
    defer home.close(io);

    var file = home.openFile(io, rel_file, .{}) catch |err| switch (err) {
        error.FileNotFound => return error.NotSignedIn,
        else => return err,
    };
    defer file.close(io);

    var buf: [4096]u8 = undefined;
    var reader = file.reader(io, &buf);
    const bytes = try reader.interface.allocRemaining(allocator, .unlimited);
    defer allocator.free(bytes);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, bytes, .{}) catch return error.InvalidCredentials;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidCredentials;
    const root = parsed.value.object;

    const token = json_util.dupeRequired(allocator, root, "token") catch return error.InvalidCredentials;
    errdefer allocator.free(token);

    const email = try json_util.dupeOptional(allocator, root, "email");
    return .{ .token = token, .email = email };
}

pub fn save(allocator: std.mem.Allocator, creds: Credentials) !void {
    var io_backend = io_util.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    var home = try openHome(allocator, io);
    defer home.close(io);

    home.createDirPath(io, rel_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("token");
    try json.write(creds.token);
    if (creds.email) |value| {
        try json.objectField("email");
        try json.write(value);
    }
    try json.endObject();
    const bytes = aw.writer.buffered();

    var file = try home.createFile(io, rel_file, .{
        .truncate = true,
        .permissions = std.Io.File.Permissions.fromMode(0o600),
    });
    defer file.close(io);

    var buf: [256]u8 = undefined;
    var writer = file.writer(io, &buf);
    try writer.interface.writeAll(bytes);
    try writer.interface.flush();
}

// Returns true if credentials existed and were deleted, false if nothing was
// there. Callers rely on the distinction to pick between "Signed out." and
// "Already signed out." without a separate stat check.
pub fn clear(allocator: std.mem.Allocator) !bool {
    var io_backend = io_util.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    var home = openHome(allocator, io) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    defer home.close(io);

    home.deleteFile(io, rel_file) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    return true;
}

test "save → load → clear round trip" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const fake_home = try tmp.dir.realpathAlloc(std.testing.io, allocator, ".");
    defer allocator.free(fake_home);
    const home_z = try allocator.dupeZ(u8, fake_home);
    defer allocator.free(home_z);
    const previous_home = std.c.getenv("HOME");
    _ = std.c.setenv("HOME", home_z.ptr, 1);
    defer if (previous_home) |p| {
        _ = std.c.setenv("HOME", p, 1);
    } else {
        _ = std.c.unsetenv("HOME");
    };

    try save(allocator, .{
        .token = @constCast("tok-abc"),
        .email = @constCast("me@example.com"),
    });

    var loaded = try load(allocator);
    defer loaded.deinit(allocator);
    try std.testing.expectEqualStrings("tok-abc", loaded.token);
    try std.testing.expectEqualStrings("me@example.com", loaded.email.?);

    try std.testing.expect(try clear(allocator));
    try std.testing.expectError(error.NotSignedIn, load(allocator));
    try std.testing.expect(!try clear(allocator));
}

test "load returns NotSignedIn when credentials missing" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const fake_home = try tmp.dir.realpathAlloc(std.testing.io, allocator, ".");
    defer allocator.free(fake_home);
    const home_z = try allocator.dupeZ(u8, fake_home);
    defer allocator.free(home_z);
    const previous_home = std.c.getenv("HOME");
    _ = std.c.setenv("HOME", home_z.ptr, 1);
    defer if (previous_home) |p| {
        _ = std.c.setenv("HOME", p, 1);
    } else {
        _ = std.c.unsetenv("HOME");
    };

    try std.testing.expectError(error.NotSignedIn, load(allocator));
}
