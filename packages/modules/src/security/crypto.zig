//! zigttp:crypto - Cryptographic functions
//!
//! Exports:
//!   sha256(data: string) -> string          Hex-encoded SHA-256 hash
//!   hmacSha256(key: string, data: string) -> string  Hex-encoded HMAC-SHA256
//!   base64Encode(data: string) -> string    Base64-encoded string
//!   base64Decode(data: string) -> string    Decoded base64 string
//!
//! All functions operate on string data and return hex/base64 encoded strings.
//! Native Zig crypto - no JS overhead.

const std = @import("std");
const sdk = @import("zigttp-sdk");
const util = @import("../internal/util.zig");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:crypto",
    .name = "crypto",
    .required_capabilities = &.{.crypto},
    .exports = &.{
        .{
            .name = "sha256",
            .module_func = sha256Impl,
            .arg_count = 1,
            .returns = .string,
            .param_types = &.{.string},
            .laws = &.{.pure},
        },
        .{
            .name = "hmacSha256",
            .module_func = hmacSha256Impl,
            .arg_count = 2,
            .returns = .string,
            .param_types = &.{ .string, .string },
            .laws = &.{.pure},
        },
        .{
            .name = "base64Encode",
            .module_func = base64EncodeImpl,
            .arg_count = 1,
            .returns = .string,
            .param_types = &.{.string},
            .laws = &.{ .pure, .{ .inverse_of = "base64Decode" } },
        },
        .{
            .name = "base64Decode",
            .module_func = base64DecodeImpl,
            .arg_count = 1,
            .returns = .string,
            .param_types = &.{.string},
            .laws = &.{ .pure, .{ .inverse_of = "base64Encode" } },
        },
    },
};

fn sha256Impl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const data = util.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    var digest: sdk.Sha256Digest = undefined;
    try sdk.sha256(handle, data, &digest);

    const hex = std.fmt.bytesToHex(digest, .lower);
    return util.createString(handle, &hex) catch sdk.JSValue.undefined_val;
}

fn hmacSha256Impl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.undefined_val;
    const key = util.extractString(args[0]) orelse return sdk.JSValue.undefined_val;
    const data = util.extractString(args[1]) orelse return sdk.JSValue.undefined_val;

    var mac: sdk.HmacSha256Mac = undefined;
    try sdk.hmacSha256(handle, data, key, &mac);

    const hex = std.fmt.bytesToHex(mac, .lower);
    return util.createString(handle, &hex) catch sdk.JSValue.undefined_val;
}

fn base64EncodeImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const data = util.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const encoder = std.base64.standard;
    const encoded_len = encoder.Encoder.calcSize(data.len);

    const allocator = util.getAllocator(handle);
    const buf = allocator.alloc(u8, encoded_len) catch return sdk.JSValue.undefined_val;
    defer allocator.free(buf);

    const encoded = encoder.Encoder.encode(buf, data);
    return util.createString(handle, encoded) catch sdk.JSValue.undefined_val;
}

fn base64DecodeImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const data = util.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const decoder = std.base64.standard;
    const decoded_len = decoder.Decoder.calcSizeForSlice(data) catch return sdk.JSValue.undefined_val;

    const allocator = util.getAllocator(handle);
    const buf = allocator.alloc(u8, decoded_len) catch return sdk.JSValue.undefined_val;
    defer allocator.free(buf);

    decoder.Decoder.decode(buf, data) catch return sdk.JSValue.undefined_val;
    return util.createString(handle, buf[0..decoded_len]) catch sdk.JSValue.undefined_val;
}
