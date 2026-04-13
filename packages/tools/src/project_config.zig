const std = @import("std");

const zigts = @import("zigts");

pub const ProjectConfig = struct {
    root_dir: []const u8,
    manifest_path: []const u8,
    entry: []const u8,
    port: u16 = 3000,
    host: []const u8 = "127.0.0.1",
    static_dir: ?[]const u8 = null,
    sqlite: ?[]const u8 = null,
    durable_dir: ?[]const u8 = null,
    system: ?[]const u8 = null,
    cors: bool = false,
    outbound_http: bool = false,
    outbound_hosts: []const []const u8 = &.{},

    pub fn deinit(self: *ProjectConfig, allocator: std.mem.Allocator) void {
        allocator.free(self.root_dir);
        allocator.free(self.manifest_path);
        allocator.free(self.entry);
        allocator.free(self.host);
        if (self.static_dir) |path| allocator.free(path);
        if (self.sqlite) |path| allocator.free(path);
        if (self.durable_dir) |path| allocator.free(path);
        if (self.system) |path| allocator.free(path);
        for (self.outbound_hosts) |host| allocator.free(host);
        allocator.free(self.outbound_hosts);
    }

    pub fn resolvePath(self: *const ProjectConfig, allocator: std.mem.Allocator, path: []const u8) ![]u8 {
        if (std.fs.path.isAbsolute(path)) {
            return try allocator.dupe(u8, path);
        }
        return try std.fs.path.resolve(allocator, &.{ self.root_dir, path });
    }

    pub fn resolvedEntry(self: *const ProjectConfig, allocator: std.mem.Allocator) ![]u8 {
        return try self.resolvePath(allocator, self.entry);
    }

    pub fn resolvedStaticDir(self: *const ProjectConfig, allocator: std.mem.Allocator) !?[]u8 {
        const path = self.static_dir orelse return null;
        return try self.resolvePath(allocator, path);
    }

    pub fn resolvedSqlitePath(self: *const ProjectConfig, allocator: std.mem.Allocator) !?[]u8 {
        const path = self.sqlite orelse return null;
        return try self.resolvePath(allocator, path);
    }

    pub fn resolvedDurableDir(self: *const ProjectConfig, allocator: std.mem.Allocator) !?[]u8 {
        const path = self.durable_dir orelse return null;
        return try self.resolvePath(allocator, path);
    }

    pub fn resolvedSystemPath(self: *const ProjectConfig, allocator: std.mem.Allocator) !?[]u8 {
        const path = self.system orelse return null;
        return try self.resolvePath(allocator, path);
    }
};

pub fn discover(
    allocator: std.mem.Allocator,
    io: std.Io,
    start_path: ?[]const u8,
) !?ProjectConfig {
    const start_dir = try findStartDir(allocator, io, start_path);
    defer allocator.free(start_dir);

    var current = try allocator.dupe(u8, start_dir);
    defer allocator.free(current);

    while (true) {
        const manifest_path = try std.fs.path.resolve(allocator, &.{ current, "zigttp.json" });
        defer allocator.free(manifest_path);

        if (std.Io.Dir.accessAbsolute(io, manifest_path, .{})) |_| {
            return try loadAbsolute(allocator, io, manifest_path);
        } else |err| switch (err) {
            error.FileNotFound => {},
            else => return err,
        }

        const parent = std.fs.path.dirname(current) orelse break;
        if (parent.len == current.len) break;

        const next = try allocator.dupe(u8, parent);
        allocator.free(current);
        current = next;
    }

    return null;
}

pub fn loadAbsolute(
    allocator: std.mem.Allocator,
    io: std.Io,
    manifest_path: []const u8,
) !ProjectConfig {
    const bytes = try zigts.file_io.readFile(allocator, manifest_path, 1024 * 1024);
    defer allocator.free(bytes);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, bytes, .{});
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidProjectConfig;
    const obj = parsed.value.object;

    const root_dir_src = std.fs.path.dirname(manifest_path) orelse return error.InvalidProjectConfig;
    const entry = try dupStringField(allocator, obj, "entry", "src/handler.ts");
    const host = try dupStringField(allocator, obj, "host", "127.0.0.1");

    var config = ProjectConfig{
        .root_dir = try allocator.dupe(u8, root_dir_src),
        .manifest_path = try allocator.dupe(u8, manifest_path),
        .entry = entry,
        .port = try parseU16Field(obj, "port", 3000),
        .host = host,
        .static_dir = try dupOptionalStringField(allocator, obj, "staticDir"),
        .sqlite = try dupOptionalStringField(allocator, obj, "sqlite"),
        .durable_dir = try dupOptionalStringField(allocator, obj, "durableDir"),
        .system = try dupOptionalStringField(allocator, obj, "system"),
        .cors = try parseBoolField(obj, "cors", false),
        .outbound_http = try parseBoolField(obj, "outboundHttp", false),
        .outbound_hosts = try dupStringArrayField(allocator, obj, "outboundHosts"),
    };
    errdefer config.deinit(allocator);

    if (config.static_dir == null) {
        const public_path = try std.fs.path.resolve(allocator, &.{ config.root_dir, "public" });
        defer allocator.free(public_path);
        if (std.Io.Dir.accessAbsolute(io, public_path, .{})) |_| {
            config.static_dir = try allocator.dupe(u8, "public");
        } else |_| {}
    }

    if (config.system == null) {
        const system_path = try std.fs.path.resolve(allocator, &.{ config.root_dir, "system.json" });
        defer allocator.free(system_path);
        if (std.Io.Dir.accessAbsolute(io, system_path, .{})) |_| {
            config.system = try allocator.dupe(u8, "system.json");
        } else |_| {}
    }

    return config;
}

fn findStartDir(
    allocator: std.mem.Allocator,
    io: std.Io,
    start_path: ?[]const u8,
) ![]u8 {
    const input = start_path orelse return try dupCurrentDir(allocator, io);
    const normalized = try normalizeAbsolutePath(allocator, io, input);
    errdefer allocator.free(normalized);

    if (std.mem.eql(u8, std.fs.path.basename(input), "zigttp.json")) {
        const dir_name = std.fs.path.dirname(normalized) orelse return error.InvalidProjectConfig;
        return try allocator.dupe(u8, dir_name);
    }

    var dir = std.Io.Dir.openDirAbsolute(io, normalized, .{}) catch |err| switch (err) {
        error.FileNotFound => return try dupCurrentDir(allocator, io),
        error.NotDir => {
            const dir_name = std.fs.path.dirname(normalized) orelse return error.InvalidProjectConfig;
            return try allocator.dupe(u8, dir_name);
        },
        else => return err,
    };
    defer dir.close(io);
    return normalized;
}

fn dupCurrentDir(allocator: std.mem.Allocator, io: std.Io) ![]u8 {
    const path_z = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io, ".", allocator);
    defer allocator.free(path_z);
    return try allocator.dupe(u8, path_z);
}

fn normalizeAbsolutePath(
    allocator: std.mem.Allocator,
    io: std.Io,
    path: []const u8,
) ![]u8 {
    if (std.fs.path.isAbsolute(path)) {
        return try allocator.dupe(u8, path);
    }

    const cwd = try dupCurrentDir(allocator, io);
    defer allocator.free(cwd);
    return try std.fs.path.resolve(allocator, &.{ cwd, path });
}

fn dupStringField(
    allocator: std.mem.Allocator,
    obj: std.json.ObjectMap,
    key: []const u8,
    default_value: []const u8,
) ![]u8 {
    const value = obj.get(key) orelse return try allocator.dupe(u8, default_value);
    if (value != .string) return error.InvalidProjectConfig;
    return try allocator.dupe(u8, value.string);
}

fn dupOptionalStringField(
    allocator: std.mem.Allocator,
    obj: std.json.ObjectMap,
    key: []const u8,
) !?[]u8 {
    const value = obj.get(key) orelse return null;
    if (value == .null) return null;
    if (value != .string) return error.InvalidProjectConfig;
    return try allocator.dupe(u8, value.string);
}

fn dupStringArrayField(
    allocator: std.mem.Allocator,
    obj: std.json.ObjectMap,
    key: []const u8,
) ![]const []const u8 {
    const value = obj.get(key) orelse return try allocator.alloc([]const u8, 0);
    if (value != .array) return error.InvalidProjectConfig;

    const items = try allocator.alloc([]const u8, value.array.items.len);
    errdefer allocator.free(items);

    for (value.array.items, 0..) |item, i| {
        if (item != .string) return error.InvalidProjectConfig;
        items[i] = try allocator.dupe(u8, item.string);
    }
    return items;
}

fn parseBoolField(obj: std.json.ObjectMap, key: []const u8, default_value: bool) !bool {
    const value = obj.get(key) orelse return default_value;
    return switch (value) {
        .bool => value.bool,
        else => error.InvalidProjectConfig,
    };
}

fn parseU16Field(obj: std.json.ObjectMap, key: []const u8, default_value: u16) !u16 {
    const value = obj.get(key) orelse return default_value;
    if (value != .integer) return error.InvalidProjectConfig;
    if (value.integer < 0 or value.integer > std.math.maxInt(u16)) return error.InvalidProjectConfig;
    return @intCast(value.integer);
}

test "project config parses and defaults public directory" {
    var io_backend = std.Io.Threaded.init(std.testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try std.Io.Dir.createDirPath(tmp.dir, io, "public");
    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "zigttp.json",
        .data =
        \\{
        \\  "entry": "src/app.ts",
        \\  "port": 4000,
        \\  "host": "0.0.0.0",
        \\  "cors": true,
        \\  "outboundHttp": true,
        \\  "outboundHosts": ["api.internal"]
        \\}
        ,
    });

    const manifest_path = try std.fs.path.resolve(std.testing.allocator, &.{ tmp.sub_path, "zigttp.json" });
    defer std.testing.allocator.free(manifest_path);

    var config = try loadAbsolute(std.testing.allocator, io, manifest_path);
    defer config.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("src/app.ts", config.entry);
    try std.testing.expectEqual(@as(u16, 4000), config.port);
    try std.testing.expectEqualStrings("0.0.0.0", config.host);
    try std.testing.expect(config.cors);
    try std.testing.expect(config.outbound_http);
    try std.testing.expectEqual(@as(usize, 1), config.outbound_hosts.len);
    try std.testing.expectEqualStrings("api.internal", config.outbound_hosts[0]);
    try std.testing.expect(config.static_dir != null);
    try std.testing.expectEqualStrings("public", config.static_dir.?);
}

test "discover finds manifest from relative handler path" {
    var io_backend = std.Io.Threaded.init(std.testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try std.Io.Dir.createDirPath(tmp.dir, io, "src");
    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "zigttp.json",
        .data =
        \\{
        \\  "entry": "src/handler.ts"
        \\}
        ,
    });
    try tmp.dir.writeFile(std.testing.io, .{
        .sub_path = "src/handler.ts",
        .data = "function handler(req) { return Response.text('ok') }\n",
    });

    const cwd = try dupCurrentDir(std.testing.allocator, io);
    defer std.testing.allocator.free(cwd);

    const handler_abs = try std.fs.path.resolve(std.testing.allocator, &.{ tmp.sub_path, "src", "handler.ts" });
    defer std.testing.allocator.free(handler_abs);

    const relative_handler = try std.fs.path.relative(std.testing.allocator, cwd, handler_abs);
    defer std.testing.allocator.free(relative_handler);

    var config = (try discover(std.testing.allocator, io, relative_handler)).?;
    defer config.deinit(std.testing.allocator);

    const expected_root = try std.fs.path.resolve(std.testing.allocator, &.{tmp.sub_path});
    defer std.testing.allocator.free(expected_root);

    try std.testing.expectEqualStrings(expected_root, config.root_dir);
    try std.testing.expectEqualStrings("src/handler.ts", config.entry);
}
