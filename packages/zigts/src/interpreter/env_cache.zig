//! Tiny helpers for first-call-cached env var reads. Each helper takes a
//! pointer to an `?T` cache cell so the call site owns lifetime and reset.

const std = @import("std");

/// True if the env var is present (any value, including empty). Cached after
/// the first read.
pub inline fn cachedBoolPresent(name: [*:0]const u8, cache: *?bool) bool {
    if (cache.*) |c| return c;
    const present = std.c.getenv(name) != null;
    cache.* = present;
    return present;
}

/// Parse env var as usize with a fallback default. Cached after the first
/// read. A parse failure also yields `default`.
pub inline fn cachedUsize(name: [*:0]const u8, cache: *?usize, default: usize) usize {
    if (cache.*) |c| return c;
    const result: usize = blk: {
        const raw_ptr = std.c.getenv(name) orelse break :blk default;
        const raw = std.mem.sliceTo(raw_ptr, 0);
        break :blk std.fmt.parseUnsigned(usize, raw, 10) catch default;
    };
    cache.* = result;
    return result;
}

/// Like `cachedUsize` but also treats a parsed `0` as "use default", for
/// flags where 0 is meaningless.
pub inline fn cachedUsizeNonzero(name: [*:0]const u8, cache: *?usize, default: usize) usize {
    if (cache.*) |c| return c;
    const result: usize = blk: {
        const raw_ptr = std.c.getenv(name) orelse break :blk default;
        const raw = std.mem.sliceTo(raw_ptr, 0);
        const parsed = std.fmt.parseUnsigned(usize, raw, 10) catch break :blk default;
        break :blk if (parsed == 0) default else parsed;
    };
    cache.* = result;
    return result;
}
