const std = @import("std");
const zigts = @import("zigts");
const plan = @import("plan.zig");

pub const Entry = struct {
    provider: plan.Provider,
    name: []const u8,
    scope_id: []const u8,
    service_id: []const u8,
    region: ?[]const u8,
    plan_id: ?[]const u8,
    url: ?[]const u8,
    last_image_digest: ?[]const u8,
    managed_env_keys: []const []const u8,

    pub fn deinit(self: *Entry, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.scope_id);
        allocator.free(self.service_id);
        if (self.region) |value| allocator.free(value);
        if (self.plan_id) |value| allocator.free(value);
        if (self.url) |value| allocator.free(value);
        if (self.last_image_digest) |value| allocator.free(value);
        for (self.managed_env_keys) |value| allocator.free(value);
        allocator.free(self.managed_env_keys);
    }
};

pub const Store = struct {
    entries: []Entry,

    pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
        for (self.entries) |*entry| entry.deinit(allocator);
        allocator.free(self.entries);
    }

    pub fn get(self: *const Store, provider: plan.Provider, name: []const u8) ?*const Entry {
        for (self.entries) |*entry| {
            if (entry.provider == provider and std.mem.eql(u8, entry.name, name)) return entry;
        }
        return null;
    }

    pub fn put(self: *Store, allocator: std.mem.Allocator, next: Entry) !void {
        for (self.entries, 0..) |*entry, idx| {
            if (entry.provider == next.provider and std.mem.eql(u8, entry.name, next.name)) {
                entry.deinit(allocator);
                self.entries[idx] = next;
                return;
            }
        }
        const resized = try allocator.realloc(self.entries, self.entries.len + 1);
        self.entries = resized;
        self.entries[self.entries.len - 1] = next;
    }
};

pub fn load(allocator: std.mem.Allocator) !Store {
    ensureStateDir() catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
    const path = statePath();
    const bytes = zigts.file_io.readFile(allocator, path, 1024 * 1024) catch |err| switch (err) {
        error.FileNotFound => return .{ .entries = try allocator.alloc(Entry, 0) },
        else => return err,
    };
    defer allocator.free(bytes);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, bytes, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidDeployState;
    const root = parsed.value.object;
    const entries_value = root.get("entries") orelse return .{ .entries = try allocator.alloc(Entry, 0) };
    if (entries_value != .array) return error.InvalidDeployState;

    var entries = std.ArrayList(Entry).empty;
    errdefer {
        for (entries.items) |*entry| entry.deinit(allocator);
        entries.deinit(allocator);
    }

    for (entries_value.array.items) |item| {
        if (item != .object) return error.InvalidDeployState;
        try entries.append(allocator, try parseEntry(allocator, item.object));
    }

    return .{ .entries = try entries.toOwnedSlice(allocator) };
}

pub fn save(allocator: std.mem.Allocator, store: *const Store) !void {
    try ensureStateDir();
    const path = statePath();
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("version");
    try json.write(@as(u32, 1));
    try json.objectField("entries");
    try json.beginArray();
    for (store.entries) |entry| {
        try json.beginObject();
        try json.objectField("provider");
        try json.write(entry.provider.toString());
        try json.objectField("name");
        try json.write(entry.name);
        try json.objectField("scopeId");
        try json.write(entry.scope_id);
        try json.objectField("serviceId");
        try json.write(entry.service_id);
        if (entry.region) |value| {
            try json.objectField("region");
            try json.write(value);
        }
        if (entry.plan_id) |value| {
            try json.objectField("planId");
            try json.write(value);
        }
        if (entry.url) |value| {
            try json.objectField("url");
            try json.write(value);
        }
        if (entry.last_image_digest) |value| {
            try json.objectField("lastImageDigest");
            try json.write(value);
        }
        try json.objectField("managedEnvKeys");
        try json.beginArray();
        for (entry.managed_env_keys) |key| try json.write(key);
        try json.endArray();
        try json.endObject();
    }
    try json.endArray();
    try json.endObject();
    const bytes = try aw.toOwnedSlice();
    defer allocator.free(bytes);
    try zigts.file_io.writeFile(allocator, path, bytes);
}

fn parseEntry(allocator: std.mem.Allocator, obj: std.json.ObjectMap) !Entry {
    const provider_raw = getString(obj, "provider") orelse return error.InvalidDeployState;
    const provider = plan.Provider.fromString(provider_raw) orelse return error.InvalidDeployState;
    const managed_value = obj.get("managedEnvKeys") orelse return error.InvalidDeployState;
    if (managed_value != .array) return error.InvalidDeployState;
    var managed = std.ArrayList([]const u8).empty;
    errdefer {
        for (managed.items) |item| allocator.free(item);
        managed.deinit(allocator);
    }
    for (managed_value.array.items) |item| {
        if (item != .string) return error.InvalidDeployState;
        try managed.append(allocator, try allocator.dupe(u8, item.string));
    }
    return .{
        .provider = provider,
        .name = try allocator.dupe(u8, getString(obj, "name") orelse return error.InvalidDeployState),
        .scope_id = try allocator.dupe(u8, getString(obj, "scopeId") orelse return error.InvalidDeployState),
        .service_id = try allocator.dupe(u8, getString(obj, "serviceId") orelse return error.InvalidDeployState),
        .region = if (getString(obj, "region")) |value| try allocator.dupe(u8, value) else null,
        .plan_id = if (getString(obj, "planId")) |value| try allocator.dupe(u8, value) else null,
        .url = if (getString(obj, "url")) |value| try allocator.dupe(u8, value) else null,
        .last_image_digest = if (getString(obj, "lastImageDigest")) |value| try allocator.dupe(u8, value) else null,
        .managed_env_keys = try managed.toOwnedSlice(allocator),
    };
}

fn getString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    if (value != .string) return null;
    return value.string;
}

fn ensureStateDir() !void {
    var io_backend = std.Io.Threaded.init(std.heap.smp_allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), ".zigttp");
}

pub fn statePath() []const u8 {
    return ".zigttp/deploy-state.json";
}

test "state store round trips entries" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const old_cwd = try std.process.currentPathAlloc(std.testing.io, std.testing.allocator);
    defer std.testing.allocator.free(old_cwd);
    try std.posix.chdir(tmp.sub_path);
    defer std.posix.chdir(old_cwd) catch {};

    var store = Store{ .entries = try std.testing.allocator.alloc(Entry, 0) };
    defer store.deinit(std.testing.allocator);

    try store.put(std.testing.allocator, .{
        .provider = .northflank,
        .name = try std.testing.allocator.dupe(u8, "demo"),
        .scope_id = try std.testing.allocator.dupe(u8, "owner-1"),
        .service_id = try std.testing.allocator.dupe(u8, "srv-1"),
        .region = try std.testing.allocator.dupe(u8, "oregon"),
        .plan_id = try std.testing.allocator.dupe(u8, "starter"),
        .url = try std.testing.allocator.dupe(u8, "https://demo.example"),
        .last_image_digest = try std.testing.allocator.dupe(u8, "sha256:abc"),
        .managed_env_keys = blk: {
            const keys = try std.testing.allocator.alloc([]const u8, 1);
            keys[0] = try std.testing.allocator.dupe(u8, "PORT");
            break :blk keys;
        },
    });
    try save(std.testing.allocator, &store);

    var loaded = try load(std.testing.allocator);
    defer loaded.deinit(std.testing.allocator);

    try std.testing.expect(loaded.get(.northflank, "demo") != null);
}
