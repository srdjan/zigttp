const std = @import("std");

pub fn getString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    if (value != .string) return null;
    return value.string;
}

pub fn dupeRequired(allocator: std.mem.Allocator, obj: std.json.ObjectMap, key: []const u8) ![]u8 {
    const value = getString(obj, key) orelse return error.MissingField;
    return allocator.dupe(u8, value);
}

pub fn dupeOptional(allocator: std.mem.Allocator, obj: std.json.ObjectMap, key: []const u8) !?[]u8 {
    const value = getString(obj, key) orelse return null;
    return try allocator.dupe(u8, value);
}

// getI64 reads an integer field, tolerating floats emitted by JSON serializers
// that upcast large numbers. Floats are truncated toward zero. Returns null if
// the key is absent or the value is not numeric.
pub fn getI64(obj: std.json.ObjectMap, key: []const u8) ?i64 {
    const value = obj.get(key) orelse return null;
    return switch (value) {
        .integer => |i| i,
        .float => |f| @intFromFloat(f),
        else => null,
    };
}

// Private alias for the file-level dupeRequired so OwnedSlices.dupeRequired
// can call it without shadowing.
const dupeRequiredOwned = dupeRequired;

// OwnedSlices tracks a set of allocator-owned slices so a parser with many
// required string fields can collapse its errdefer ladder into a single
// `errdefer owned.freeAll()`. On the success path, call `release()` to drop
// the tracking list without freeing the slices themselves; ownership transfers
// to the returned struct.
pub const OwnedSlices = struct {
    allocator: std.mem.Allocator,
    items: std.ArrayList([]u8),

    pub fn init(allocator: std.mem.Allocator) OwnedSlices {
        return .{
            .allocator = allocator,
            .items = std.ArrayList([]u8).empty,
        };
    }

    pub fn track(self: *OwnedSlices, slice: []u8) !void {
        try self.items.append(self.allocator, slice);
    }

    pub fn dupeRequired(
        self: *OwnedSlices,
        obj: std.json.ObjectMap,
        key: []const u8,
    ) ![]u8 {
        const slice = try dupeRequiredOwned(self.allocator, obj, key);
        errdefer self.allocator.free(slice);
        try self.track(slice);
        return slice;
    }

    pub fn freeAll(self: *OwnedSlices) void {
        for (self.items.items) |slice| self.allocator.free(slice);
        self.items.deinit(self.allocator);
    }

    // Called on the success path to drop the tracking list without freeing
    // the slices it references.
    pub fn release(self: *OwnedSlices) void {
        self.items.deinit(self.allocator);
    }
};
