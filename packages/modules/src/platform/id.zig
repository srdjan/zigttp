//! zigttp:id - ID generation (UUID v4, ULID, nanoid)

const std = @import("std");
const sdk = @import("zigttp-sdk");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:id",
    .name = "id",
    .required_capabilities = &.{ .clock, .random },
    .exports = &.{
        .{ .name = "uuid", .module_func = uuidImpl, .arg_count = 0, .returns = .string, .param_types = &.{}, .effect = .read, .return_labels = .{ .internal = true } },
        .{ .name = "ulid", .module_func = ulidImpl, .arg_count = 0, .returns = .string, .param_types = &.{}, .effect = .read, .return_labels = .{ .internal = true } },
        .{ .name = "nanoid", .module_func = nanoidImpl, .arg_count = 1, .returns = .string, .param_types = &.{.number}, .effect = .read, .return_labels = .{ .internal = true } },
    },
};

fn formatUuidV4(handle: *sdk.ModuleHandle, buf: *[36]u8) !void {
    var bytes: [16]u8 = undefined;
    try sdk.fillRandom(handle, &bytes);

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

fn uuidImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, _: []const sdk.JSValue) anyerror!sdk.JSValue {
    var buf: [36]u8 = undefined;
    try formatUuidV4(handle, &buf);
    return sdk.createString(handle, &buf) catch sdk.JSValue.undefined_val;
}

const CROCKFORD_BASE32 = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";

fn formatUlid(handle: *sdk.ModuleHandle, buf: *[26]u8) !void {
    const now = try sdk.nowMs(handle);
    const ms: u64 = @intCast(@max(0, now));

    var ts = ms;
    var i: usize = 10;
    while (i > 0) {
        i -= 1;
        buf[i] = CROCKFORD_BASE32[@intCast(ts & 0x1F)];
        ts >>= 5;
    }

    var rand_bytes: [10]u8 = undefined;
    try sdk.fillRandom(handle, &rand_bytes);

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

fn ulidImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, _: []const sdk.JSValue) anyerror!sdk.JSValue {
    var buf: [26]u8 = undefined;
    try formatUlid(handle, &buf);
    return sdk.createString(handle, &buf) catch sdk.JSValue.undefined_val;
}

const NANOID_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-";
const NANOID_DEFAULT_LEN: usize = 21;
const NANOID_MAX_LEN: usize = 128;

fn nanoidImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    const len: usize = if (args.len > 0) blk: {
        const n = sdk.extractInt(args[0]) orelse break :blk NANOID_DEFAULT_LEN;
        if (n < 1) break :blk NANOID_DEFAULT_LEN;
        break :blk @min(@as(usize, @intCast(n)), NANOID_MAX_LEN);
    } else NANOID_DEFAULT_LEN;

    var buf: [NANOID_MAX_LEN]u8 = undefined;
    var random_bytes: [NANOID_MAX_LEN]u8 = undefined;
    try sdk.fillRandom(handle, random_bytes[0..len]);
    for (buf[0..len], random_bytes[0..len]) |*c, byte| {
        c.* = NANOID_ALPHABET[byte & 0x3F];
    }

    return sdk.createString(handle, buf[0..len]) catch sdk.JSValue.undefined_val;
}
