//! zigttp:id - ID generation
//!
//! Exports:
//!   uuid() -> string
//!     Generates a UUID v4 (random). Format: 8-4-4-4-12 hex digits.
//!
//!   ulid() -> string
//!     Generates a ULID (time-sortable). 26-char Crockford base32.
//!     Timestamp prefix ensures lexicographic ordering by creation time.
//!
//!   nanoid(len?: number) -> string
//!     Generates a URL-safe random ID. Default length 21.
//!     Alphabet: A-Za-z0-9_-
//!
//! All functions are non-deterministic (use PRNG seeded from OS clock).
//! Trace/replay wrapping is handled automatically (traceable = true).

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const util = @import("util.zig");
const mb = @import("../module_binding.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:id",
    .name = "id",
    .required_capabilities = &.{ .clock, .random },
    .exports = &.{
        .{ .name = "uuid", .func = uuidNative, .arg_count = 0, .returns = .string, .param_types = &.{}, .effect = .read, .return_labels = .{ .internal = true } },
        .{ .name = "ulid", .func = ulidNative, .arg_count = 0, .returns = .string, .param_types = &.{}, .effect = .read, .return_labels = .{ .internal = true } },
        .{ .name = "nanoid", .func = nanoidNative, .arg_count = 1, .returns = .string, .param_types = &.{.number}, .effect = .read, .return_labels = .{ .internal = true } },
    },
};

pub const exports = binding.toModuleExports();

// -------------------------------------------------------------------------
// UUID v4
// -------------------------------------------------------------------------

/// Generate 16 random bytes, set version (4) and variant (RFC 4122) bits,
/// format as 8-4-4-4-12 lowercase hex.
fn formatUuidV4(buf: *[36]u8) void {
    var bytes: [16]u8 = undefined;
    mb.fillRandomChecked(&bytes);

    // Set version 4: byte 6 high nibble = 0100
    bytes[6] = (bytes[6] & 0x0F) | 0x40;
    // Set variant RFC 4122: byte 8 high bits = 10xx
    bytes[8] = (bytes[8] & 0x3F) | 0x80;

    const hex_chars = "0123456789abcdef";
    var out: usize = 0;
    for (bytes, 0..) |byte, i| {
        buf[out] = hex_chars[byte >> 4];
        buf[out + 1] = hex_chars[byte & 0x0F];
        out += 2;
        // Insert hyphens after bytes 3, 5, 7, 9
        if (i == 3 or i == 5 or i == 7 or i == 9) {
            buf[out] = '-';
            out += 1;
        }
    }
}

fn uuidNative(ctx_ptr: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    var buf: [36]u8 = undefined;
    formatUuidV4(&buf);
    return ctx.createString(&buf) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// ULID
// -------------------------------------------------------------------------

const CROCKFORD_BASE32 = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";

/// Encode 48-bit timestamp (ms) as 10 Crockford base32 chars,
/// then 80 random bits as 16 base32 chars. Total: 26 chars.
fn formatUlid(buf: *[26]u8) void {
    const ms: u64 = @intCast(@max(0, mb.clockNowMsChecked()));

    // Timestamp: 10 chars encoding 48 bits, most significant first
    var ts = ms;
    var i: usize = 10;
    while (i > 0) {
        i -= 1;
        buf[i] = CROCKFORD_BASE32[@intCast(ts & 0x1F)];
        ts >>= 5;
    }

    // Randomness: 16 base32 chars from 10 random bytes (80 bits)
    // Extract 5 bits at a time across byte boundaries
    var rand_bytes: [10]u8 = undefined;
    mb.fillRandomChecked(&rand_bytes);

    var bit_offset: u32 = 0;
    for (buf[10..26]) |*c| {
        const byte_idx = bit_offset / 8;
        const bit_shift: u4 = @intCast(bit_offset % 8);
        // Extract 5 bits spanning one or two bytes
        var bits: u16 = @as(u16, rand_bytes[byte_idx]) >> @intCast(bit_shift);
        if (bit_shift > 3 and byte_idx + 1 < 10) {
            bits |= @as(u16, rand_bytes[byte_idx + 1]) << @intCast(8 - bit_shift);
        }
        c.* = CROCKFORD_BASE32[@intCast(bits & 0x1F)];
        bit_offset += 5;
    }
}

fn ulidNative(ctx_ptr: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);
    var buf: [26]u8 = undefined;
    formatUlid(&buf);
    return ctx.createString(&buf) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// Nanoid
// -------------------------------------------------------------------------

const NANOID_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-";
const NANOID_DEFAULT_LEN: usize = 21;
const NANOID_MAX_LEN: usize = 128;

fn nanoidNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    const len: usize = if (args.len > 0) blk: {
        const n = util.extractInt(args[0]) orelse break :blk NANOID_DEFAULT_LEN;
        if (n < 1) break :blk NANOID_DEFAULT_LEN;
        break :blk @min(@as(usize, @intCast(n)), NANOID_MAX_LEN);
    } else NANOID_DEFAULT_LEN;

    var buf: [NANOID_MAX_LEN]u8 = undefined;
    var random_bytes: [NANOID_MAX_LEN]u8 = undefined;
    mb.fillRandomChecked(random_bytes[0..len]);
    for (buf[0..len], random_bytes[0..len]) |*c, byte| {
        // Alphabet is 64 chars = 6 bits, mask random byte to avoid bias
        c.* = NANOID_ALPHABET[byte & 0x3F];
    }

    return ctx.createString(buf[0..len]) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "formatUuidV4: correct format" {
    const token = mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
    defer mb.popActiveModuleContext(token);

    var buf: [36]u8 = undefined;
    formatUuidV4(&buf);

    // Check hyphens at positions 8, 13, 18, 23
    try std.testing.expectEqual(@as(u8, '-'), buf[8]);
    try std.testing.expectEqual(@as(u8, '-'), buf[13]);
    try std.testing.expectEqual(@as(u8, '-'), buf[18]);
    try std.testing.expectEqual(@as(u8, '-'), buf[23]);

    // Check version nibble (position 14 = '4')
    try std.testing.expectEqual(@as(u8, '4'), buf[14]);

    // Check variant nibble (position 19 = 8, 9, a, or b)
    const variant = buf[19];
    try std.testing.expect(variant == '8' or variant == '9' or variant == 'a' or variant == 'b');

    // Check all non-hyphen chars are hex
    for (buf, 0..) |c, i| {
        if (i == 8 or i == 13 or i == 18 or i == 23) continue;
        try std.testing.expect((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f'));
    }
}

test "formatUuidV4: uniqueness" {
    const token = mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
    defer mb.popActiveModuleContext(token);

    var a: [36]u8 = undefined;
    var b: [36]u8 = undefined;
    formatUuidV4(&a);
    formatUuidV4(&b);
    try std.testing.expect(!std.mem.eql(u8, &a, &b));
}

test "formatUlid: correct length and charset" {
    const token = mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
    defer mb.popActiveModuleContext(token);

    var buf: [26]u8 = undefined;
    formatUlid(&buf);
    try std.testing.expectEqual(@as(usize, 26), buf.len);
    for (buf) |c| {
        try std.testing.expect(std.mem.indexOfScalar(u8, CROCKFORD_BASE32, c) != null);
    }
}

test "nanoid alphabet coverage" {
    // Generate a long nanoid and verify all chars are from the alphabet
    const token = mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
    defer mb.popActiveModuleContext(token);

    var bytes: [NANOID_MAX_LEN]u8 = undefined;
    mb.fillRandomChecked(&bytes);
    var buf: [NANOID_MAX_LEN]u8 = undefined;
    for (&buf, bytes) |*c, byte| {
        c.* = NANOID_ALPHABET[byte & 0x3F];
    }
    for (buf) |c| {
        try std.testing.expect(std.mem.indexOfScalar(u8, NANOID_ALPHABET, c) != null);
    }
}
