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
const object = @import("../object.zig");
const context = @import("../context.zig");
const value = @import("../value.zig");
const string = @import("../string.zig");
const resolver = @import("resolver.zig");

/// Module exports
pub const exports = [_]resolver.ModuleExport{
    .{ .name = "sha256", .func = sha256Native, .arg_count = 1 },
    .{ .name = "hmacSha256", .func = hmacSha256Native, .arg_count = 2 },
    .{ .name = "base64Encode", .func = base64EncodeNative, .arg_count = 1 },
    .{ .name = "base64Decode", .func = base64DecodeNative, .arg_count = 1 },
};

/// Extract string data from a JSValue (handles flat strings and slices)
fn extractString(val: value.JSValue) ?[]const u8 {
    if (val.isString()) {
        return val.toPtr(string.JSString).data();
    }
    if (val.isStringSlice()) {
        return val.toPtr(string.SliceString).data();
    }
    return null;
}

/// sha256(data) -> hex string
fn sha256Native(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return value.JSValue.undefined_val;
    const data = extractString(args[0]) orelse return value.JSValue.undefined_val;

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(data);
    const digest = hasher.finalResult();

    // Convert to hex string
    const hex = std.fmt.bytesToHex(digest, .lower);
    return ctx.createString(&hex) catch value.JSValue.undefined_val;
}

/// hmacSha256(key, data) -> hex string
fn hmacSha256Native(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len < 2) return value.JSValue.undefined_val;
    const key = extractString(args[0]) orelse return value.JSValue.undefined_val;
    const data = extractString(args[1]) orelse return value.JSValue.undefined_val;

    var mac: [std.crypto.auth.hmac.sha2.HmacSha256.mac_length]u8 = undefined;
    std.crypto.auth.hmac.sha2.HmacSha256.create(&mac, data, key);

    const hex = std.fmt.bytesToHex(mac, .lower);
    return ctx.createString(&hex) catch value.JSValue.undefined_val;
}

/// base64Encode(data) -> base64 string
fn base64EncodeNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return value.JSValue.undefined_val;
    const data = extractString(args[0]) orelse return value.JSValue.undefined_val;

    const encoder = std.base64.standard;
    const encoded_len = encoder.Encoder.calcSize(data.len);

    const buf = ctx.allocator.alloc(u8, encoded_len) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(buf);

    const encoded = encoder.Encoder.encode(buf, data);
    return ctx.createString(encoded) catch value.JSValue.undefined_val;
}

/// base64Decode(data) -> decoded string
fn base64DecodeNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return value.JSValue.undefined_val;
    const data = extractString(args[0]) orelse return value.JSValue.undefined_val;

    const decoder = std.base64.standard;
    const decoded_len = decoder.Decoder.calcSizeForSlice(data) catch return value.JSValue.undefined_val;

    const buf = ctx.allocator.alloc(u8, decoded_len) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(buf);

    decoder.Decoder.decode(buf, data) catch return value.JSValue.undefined_val;
    return ctx.createString(buf[0..decoded_len]) catch value.JSValue.undefined_val;
}
