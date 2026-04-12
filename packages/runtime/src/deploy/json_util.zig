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
