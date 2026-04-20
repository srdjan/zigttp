//! Pure session-id generator. Returns a 26-char string of the form
//! `<16 lowercase hex of ms timestamp>-<9 lowercase hex random>`.
//! The ms prefix makes IDs chronologically sort-ordered; the random
//! suffix disambiguates same-millisecond collisions.

const std = @import("std");

const alphabet = "0123456789abcdef";

/// Returns an owned 26-char session id. Caller frees with `allocator.free`.
pub fn generate(allocator: std.mem.Allocator) ![]u8 {
    const out = try allocator.alloc(u8, 26);
    errdefer allocator.free(out);

    var v: u64 = @bitCast(nowUnixMs());
    var i: usize = 16;
    while (i > 0) : (i -= 1) {
        out[i - 1] = alphabet[v & 0x0F];
        v >>= 4;
    }
    out[16] = '-';

    var rand_bytes: [5]u8 = undefined;
    fillRandom(&rand_bytes);
    // 5 bytes = 10 hex chars; use the first 9.
    for (0..9) |j| {
        const b = rand_bytes[j / 2];
        const nibble = if (j % 2 == 0) (b >> 4) else (b & 0x0F);
        out[17 + j] = alphabet[nibble];
    }

    return out;
}

var counter: std.atomic.Value(u64) = .init(0);

fn fillRandom(buf: []u8) void {
    var ts: std.posix.timespec = undefined;
    _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
    const salt = counter.fetchAdd(1, .seq_cst);
    var seed: u64 = @as(u64, @bitCast(@as(i64, ts.nsec))) ^ (salt *% 0x9E3779B97F4A7C15);
    for (buf) |*b| {
        seed = seed *% 6364136223846793005 +% 1442695040888963407;
        b.* = @intCast((seed >> 56) & 0xFF);
    }
}

fn nowUnixMs() i64 {
    var ts: std.posix.timespec = undefined;
    _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
    return @as(i64, ts.sec) * 1000 + @divTrunc(@as(i64, ts.nsec), 1_000_000);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn isLowerHex(c: u8) bool {
    return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f');
}

test "generate returns a 26-char string shaped <16hex>-<9hex>" {
    const id = try generate(testing.allocator);
    defer testing.allocator.free(id);

    try testing.expectEqual(@as(usize, 26), id.len);
    try testing.expectEqual(@as(u8, '-'), id[16]);
    for (id[0..16]) |c| try testing.expect(isLowerHex(c));
    for (id[17..]) |c| try testing.expect(isLowerHex(c));
}

test "generate returns different ids on back-to-back calls" {
    const a = try generate(testing.allocator);
    defer testing.allocator.free(a);
    const b = try generate(testing.allocator);
    defer testing.allocator.free(b);
    try testing.expect(!std.mem.eql(u8, a, b));
}

test "generate disambiguates same-millisecond collisions via the random suffix" {
    // Generate a burst fast enough that some will collide on the ms prefix.
    const n: usize = 64;
    var ids: [n][]u8 = undefined;
    for (0..n) |i| ids[i] = try generate(testing.allocator);
    defer for (ids) |s| testing.allocator.free(s);

    var saw_prefix_collision = false;
    for (0..n) |i| {
        for (i + 1..n) |j| {
            if (std.mem.eql(u8, ids[i][0..16], ids[j][0..16])) {
                saw_prefix_collision = true;
                try testing.expect(!std.mem.eql(u8, ids[i], ids[j]));
            }
        }
    }
    // If every call fell into a different ms we still win, but verify the
    // suffix path is exercised at least once by asserting at least one
    // ms-prefix collision was observed in a tight burst.
    try testing.expect(saw_prefix_collision);
}
