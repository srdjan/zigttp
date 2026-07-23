//! zttp:id - ID generation (UUID v4, ULID, nanoid)

const std = @import("std");
const sdk = @import("zttp-sdk");

pub const binding = sdk.ModuleBinding{
    .specifier = "zttp:id",
    .name = "id",
    .required_capabilities = &.{ .clock, .random },
    .exports = &.{
        .{ .name = "uuid", .module_func = uuidImpl, .arg_count = 0, .returns = .string, .param_types = &.{}, .effect = .read, .return_labels = .{ .internal = true } },
        .{ .name = "ulid", .module_func = ulidImpl, .arg_count = 0, .returns = .string, .param_types = &.{}, .effect = .read, .return_labels = .{ .internal = true } },
        .{ .name = "nanoid", .module_func = nanoidImpl, .arg_count = 1, .returns = .string, .param_types = &.{.number}, .effect = .read, .return_labels = .{ .internal = true } },
    },
};

/// Pure formatter: render 16 random bytes as a UUID v4 string. Splits the
/// version/variant bit-stamping and hex encoding away from `fillRandom` so
/// the format invariants are testable without a runtime handle.
fn formatUuidV4FromBytes(bytes_in: [16]u8, buf: *[36]u8) void {
    var bytes = bytes_in;
    bytes[6] = (bytes[6] & 0x0F) | 0x40;
    bytes[8] = (bytes[8] & 0x3F) | 0x80;

    const hex_chars = "0123456789abcdef";
    var out: usize = 0;
    for (bytes, 0..) |byte, i| {
        buf[out] = hex_chars[byte >> 4];
        buf[out + 1] = hex_chars[byte & 0x0F];
        out += 2;
        if (i == 3 or i == 5 or i == 7 or i == 9) {
            buf[out] = '-';
            out += 1;
        }
    }
}

fn formatUuidV4(handle: *sdk.ModuleHandle, buf: *[36]u8) !void {
    var bytes: [16]u8 = undefined;
    try sdk.fillRandom(handle, &bytes);
    formatUuidV4FromBytes(bytes, buf);
}

fn uuidImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, _: []const sdk.JSValue) anyerror!sdk.JSValue {
    var buf: [36]u8 = undefined;
    try formatUuidV4(handle, &buf);
    return sdk.createString(handle, &buf) catch sdk.JSValue.undefined_val;
}

const CROCKFORD_BASE32 = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";

/// Pure formatter: render a ULID from its timestamp (ms) and 10 random
/// bytes. Crockford base32 encoding of the 48-bit timestamp into 10 chars
/// followed by 80 bits of randomness into 16 chars.
fn formatUlidFromInputs(ms: u64, rand_bytes: [10]u8, buf: *[26]u8) void {
    var ts = ms;
    var i: usize = 10;
    while (i > 0) {
        i -= 1;
        buf[i] = CROCKFORD_BASE32[@intCast(ts & 0x1F)];
        ts >>= 5;
    }

    var bit_offset: u32 = 0;
    for (buf[10..26]) |*c| {
        const byte_idx = bit_offset / 8;
        const bit_shift: u4 = @intCast(bit_offset % 8);
        var bits: u16 = @as(u16, rand_bytes[byte_idx]) >> @intCast(bit_shift);
        if (bit_shift > 3 and byte_idx + 1 < 10) {
            bits |= @as(u16, rand_bytes[byte_idx + 1]) << @intCast(8 - bit_shift);
        }
        c.* = CROCKFORD_BASE32[@intCast(bits & 0x1F)];
        bit_offset += 5;
    }
}

fn formatUlid(handle: *sdk.ModuleHandle, buf: *[26]u8) !void {
    const now = try sdk.nowMs(handle);
    const ms: u64 = @intCast(@max(0, now));
    var rand_bytes: [10]u8 = undefined;
    try sdk.fillRandom(handle, &rand_bytes);
    formatUlidFromInputs(ms, rand_bytes, buf);
}

fn ulidImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, _: []const sdk.JSValue) anyerror!sdk.JSValue {
    var buf: [26]u8 = undefined;
    try formatUlid(handle, &buf);
    return sdk.createString(handle, &buf) catch sdk.JSValue.undefined_val;
}

const NANOID_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-";
const NANOID_DEFAULT_LEN: usize = 21;
const NANOID_MAX_LEN: usize = 128;

/// Pure formatter: render `len` nanoid characters from a buffer of random
/// bytes. Caller provides `random_bytes` (length >= len) and writes into
/// `out` (length >= len). Splitting this from `fillRandom` lets us verify
/// the alphabet mapping without a runtime handle.
fn formatNanoidFromBytes(random_bytes: []const u8, out: []u8) void {
    for (out, random_bytes[0..out.len]) |*c, byte| {
        c.* = NANOID_ALPHABET[byte & 0x3F];
    }
}

fn nanoidImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    const len: usize = if (args.len > 0) blk: {
        const n = sdk.extractInt(args[0]) orelse break :blk NANOID_DEFAULT_LEN;
        if (n < 1) break :blk NANOID_DEFAULT_LEN;
        break :blk @min(@as(usize, @intCast(n)), NANOID_MAX_LEN);
    } else NANOID_DEFAULT_LEN;

    var buf: [NANOID_MAX_LEN]u8 = undefined;
    var random_bytes: [NANOID_MAX_LEN]u8 = undefined;
    try sdk.fillRandom(handle, random_bytes[0..len]);
    formatNanoidFromBytes(random_bytes[0..len], buf[0..len]);

    return sdk.createString(handle, buf[0..len]) catch sdk.JSValue.undefined_val;
}

// ---------------------------------------------------------------------------
// Tests for the pure formatters. The impl functions are thin wrappers that
// only add `fillRandom` / `nowMs` callbacks plus a JSValue createString,
// none of which carry format invariants — the invariants live in the
// formatters and that is where the tests go.
// ---------------------------------------------------------------------------

const testing = std.testing;

test "formatUuidV4FromBytes: 36 chars, dashes at canonical positions, version 4 + RFC variant" {
    // All-zero input is the cleanest version/variant probe: byte 6 becomes
    // 0x40 (version 4 nibble) and byte 8 becomes 0x80 (RFC 4122 variant).
    const zeros: [16]u8 = .{0} ** 16;
    var buf: [36]u8 = undefined;
    formatUuidV4FromBytes(zeros, &buf);
    try testing.expectEqualStrings(
        "00000000-0000-4000-8000-000000000000",
        &buf,
    );

    // All-0xFF probes the opposite half of every byte; version nibble must
    // still be `4` and variant nibble must still be `8|9|a|b` (top two
    // bits 10). 0xFF & 0x3F = 0x3F, then | 0x80 = 0xBF -> 'b' nibble.
    const ones: [16]u8 = .{0xFF} ** 16;
    formatUuidV4FromBytes(ones, &buf);
    try testing.expectEqualStrings(
        "ffffffff-ffff-4fff-bfff-ffffffffffff",
        &buf,
    );

    // Dashes always at positions 8, 13, 18, 23 regardless of input.
    var mixed: [16]u8 = undefined;
    for (&mixed, 0..) |*b, i| b.* = @intCast(i * 17 & 0xFF);
    formatUuidV4FromBytes(mixed, &buf);
    try testing.expectEqual(@as(u8, '-'), buf[8]);
    try testing.expectEqual(@as(u8, '-'), buf[13]);
    try testing.expectEqual(@as(u8, '-'), buf[18]);
    try testing.expectEqual(@as(u8, '-'), buf[23]);
    try testing.expectEqual(@as(u8, '4'), buf[14]); // version nibble
}

test "formatUlidFromInputs: 26 chars, Crockford base32 only, timestamp encoded big-endian" {
    // Timestamp 0 and zero randomness -> all '0' characters.
    var buf: [26]u8 = undefined;
    formatUlidFromInputs(0, .{0} ** 10, &buf);
    try testing.expectEqualStrings("00000000000000000000000000", &buf);

    // A timestamp whose low 5 bits are 0b00001 should put '1' at index 9
    // (the least-significant base32 digit of the 10-char ts prefix).
    formatUlidFromInputs(1, .{0} ** 10, &buf);
    try testing.expectEqual(@as(u8, '1'), buf[9]);
    for (buf[0..9]) |c| try testing.expectEqual(@as(u8, '0'), c);

    // Every output character must be in the Crockford alphabet — no `I`,
    // `L`, `O`, or `U` (the characters explicitly excluded to avoid
    // visual ambiguity with 1/0).
    formatUlidFromInputs(0xDEADBEEF, .{ 0xAB, 0xCD, 0xEF, 0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE }, &buf);
    for (buf) |c| {
        try testing.expect(std.mem.indexOfScalar(u8, CROCKFORD_BASE32, c) != null);
        try testing.expect(c != 'I' and c != 'L' and c != 'O' and c != 'U');
    }
}

test "formatNanoidFromBytes: every output char is in the 64-char nanoid alphabet" {
    var buf: [NANOID_DEFAULT_LEN]u8 = undefined;
    const seed: [NANOID_DEFAULT_LEN]u8 = .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
    formatNanoidFromBytes(&seed, &buf);

    // First 21 alphabet positions, in order. The mapping is byte & 0x3F so
    // the seed bytes 0..20 directly index ALPHABET[0..20].
    try testing.expectEqualStrings("ABCDEFGHIJKLMNOPQRSTU", &buf);

    // Wrap-around probe: bytes 64..83 mask to 0..19 -> same characters.
    var seed2: [NANOID_DEFAULT_LEN]u8 = undefined;
    for (&seed2, 0..) |*b, i| b.* = @intCast(64 + i);
    formatNanoidFromBytes(&seed2, &buf);
    try testing.expectEqualStrings("ABCDEFGHIJKLMNOPQRSTU", &buf);

    // Every alphabet character is uppercase, lowercase, digit, '_' or '-'.
    var buf2: [NANOID_DEFAULT_LEN]u8 = undefined;
    var rand: [NANOID_DEFAULT_LEN]u8 = undefined;
    for (&rand, 0..) |*b, i| b.* = @intCast(i * 13 & 0xFF);
    formatNanoidFromBytes(&rand, &buf2);
    for (buf2) |c| {
        const ok = (c >= 'A' and c <= 'Z') or
            (c >= 'a' and c <= 'z') or
            (c >= '0' and c <= '9') or
            c == '_' or c == '-';
        try testing.expect(ok);
    }
}
