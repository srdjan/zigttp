const std = @import("std");

pub fn exponentialCapMs(base_ms: i64, max_ms: i64, attempt: u32) i64 {
    if (base_ms <= 0) return 0;
    if (max_ms <= 0) return 0;
    const shift: u6 = @intCast(@min(attempt, 6));
    return @min(base_ms <<| shift, max_ms);
}

pub fn seedBytes(bytes: []const u8, salt: u64) u64 {
    return std.hash.Wyhash.hash(salt, bytes);
}

pub fn mix(seed: u64, value: u64) u64 {
    var bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &bytes, value, .little);
    return std.hash.Wyhash.hash(seed, &bytes);
}

/// Equal jitter: randomize within the upper half of the exponential cap.
/// This avoids hot retry loops while still desynchronizing concurrent runs.
pub fn boundedJitterMs(cap_ms: i64, seed: u64) i64 {
    if (cap_ms <= 1) return @max(cap_ms, 0);
    const floor = @divTrunc(cap_ms, 2);
    const span: u64 = @intCast(cap_ms - floor);
    const offset: i64 = @intCast(seed % (span + 1));
    return floor + offset;
}

pub fn retryDelayMs(base_ms: i64, max_ms: i64, attempt: u32, seed: u64) i64 {
    return boundedJitterMs(exponentialCapMs(base_ms, max_ms, attempt), seed);
}

test "retry backoff caps exponential delay" {
    try std.testing.expectEqual(@as(i64, 100), exponentialCapMs(100, 60_000, 0));
    try std.testing.expectEqual(@as(i64, 6_400), exponentialCapMs(100, 60_000, 6));
    try std.testing.expectEqual(@as(i64, 60_000), exponentialCapMs(1_000, 60_000, 9));
}

test "retry backoff does not sign-flip when base_ms << shift overflows i64" {
    // A plain `<<` silently wraps/flips sign on overflow instead of
    // saturating, which used to collapse the cap to a negative value (and
    // then, via boundedJitterMs's cap_ms <= 1 branch, to a 0ms backoff)
    // instead of clamping to max_ms.
    const huge_base: i64 = 200_000_000_000_000_000;
    try std.testing.expectEqual(@as(i64, 60_000), exponentialCapMs(huge_base, 60_000, 6));
}

test "retry backoff jitter stays inside upper half of cap" {
    const cap = exponentialCapMs(1_000, 60_000, 2);
    const delay = retryDelayMs(1_000, 60_000, 2, 0x1234);
    try std.testing.expect(delay >= @divTrunc(cap, 2));
    try std.testing.expect(delay <= cap);
}
