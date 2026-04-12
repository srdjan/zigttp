const std = @import("std");

pub const Credentials = struct {
    token: []u8,
    email: ?[]u8 = null,

    pub fn deinit(self: *Credentials, allocator: std.mem.Allocator) void {
        allocator.free(self.token);
        if (self.email) |value| allocator.free(value);
    }
};

pub const Error = error{
    NotSignedIn,
    HomeDirMissing,
    InvalidCredentials,
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
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
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
    const token_value = root.get("token") orelse return error.InvalidCredentials;
    if (token_value != .string) return error.InvalidCredentials;

    const token = try allocator.dupe(u8, token_value.string);
    errdefer allocator.free(token);

    var email: ?[]u8 = null;
    if (root.get("email")) |value| {
        if (value == .string) email = try allocator.dupe(u8, value.string);
    }

    return .{ .token = token, .email = email };
}

pub fn save(allocator: std.mem.Allocator, creds: Credentials) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
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

pub fn clear(allocator: std.mem.Allocator) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var home = openHome(allocator, io) catch return;
    defer home.close(io);

    home.deleteFile(io, rel_file) catch |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    };
}
