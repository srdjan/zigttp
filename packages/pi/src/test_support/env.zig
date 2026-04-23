//! Shared env-var override helper for tests. Consolidates the per-file
//! `EnvOverride` structs into one definition that carries `set`, `unset`,
//! and `restore`.

const std = @import("std");

extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;
extern "c" fn unsetenv(name: [*:0]const u8) c_int;

pub const EnvOverride = struct {
    name_z: [:0]const u8,
    previous: ?[]u8,

    pub fn set(
        allocator: std.mem.Allocator,
        name_z: [:0]const u8,
        value: []const u8,
    ) !EnvOverride {
        const previous = try capturePrevious(allocator, name_z);
        errdefer if (previous) |p| allocator.free(p);

        const value_z = try allocator.dupeZ(u8, value);
        defer allocator.free(value_z);
        _ = setenv(name_z.ptr, value_z.ptr, 1);

        return .{ .name_z = name_z, .previous = previous };
    }

    pub fn unset(
        allocator: std.mem.Allocator,
        name_z: [:0]const u8,
    ) !EnvOverride {
        const previous = try capturePrevious(allocator, name_z);
        _ = unsetenv(name_z.ptr);
        return .{ .name_z = name_z, .previous = previous };
    }

    pub fn restore(self: *EnvOverride, allocator: std.mem.Allocator) void {
        if (self.previous) |prev| {
            const prev_z = allocator.dupeZ(u8, prev) catch {
                allocator.free(prev);
                self.previous = null;
                return;
            };
            defer allocator.free(prev_z);
            _ = setenv(self.name_z.ptr, prev_z.ptr, 1);
            allocator.free(prev);
            self.previous = null;
        } else {
            _ = unsetenv(self.name_z.ptr);
        }
    }
};

fn capturePrevious(allocator: std.mem.Allocator, name_z: [:0]const u8) !?[]u8 {
    const prev_opt = std.c.getenv(name_z.ptr) orelse return null;
    const slice = std.mem.sliceTo(prev_opt, 0);
    return try allocator.dupe(u8, slice);
}
