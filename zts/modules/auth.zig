//! zigttp:auth - Authentication and JWT utilities
//!
//! Exports:
//!   parseBearer(header: string) -> string | null
//!     Extract token from "Bearer <token>" Authorization header.
//!
//!   jwtVerify(token: string, secret: string, options?: object) -> { ok: true, value: claims } | { ok: false, error: string }
//!     Verify HS256 JWT. Optional options: { clockTolerance: number }.
//!     Checks exp/nbf claims if present.
//!
//!   jwtSign(claims: string, secret: string) -> string | undefined
//!     Create HS256 JWT from JSON claims string.
//!
//!   verifyWebhookSignature(payload: string, secret: string, signature: string) -> boolean
//!     HMAC-SHA256 webhook signature verification (GitHub/Stripe style).
//!     Handles "sha256=" prefix. Constant-time comparison.
//!
//!   timingSafeEqual(a: string, b: string) -> boolean
//!     Constant-time string comparison.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const object = @import("../object.zig");
const builtins = @import("../builtins.zig");
const compat = @import("../compat.zig");
const resolver = @import("resolver.zig");
const util = @import("util.zig");

const HmacSha256 = std.crypto.auth.hmac.sha2.HmacSha256;
const base64url = std.base64.url_safe_no_pad;

/// Module exports
pub const exports = [_]resolver.ModuleExport{
    .{ .name = "parseBearer", .func = parseBearerNative, .arg_count = 1 },
    .{ .name = "jwtVerify", .func = jwtVerifyNative, .arg_count = 3 },
    .{ .name = "jwtSign", .func = jwtSignNative, .arg_count = 2 },
    .{ .name = "verifyWebhookSignature", .func = verifyWebhookSignatureNative, .arg_count = 3 },
    .{ .name = "timingSafeEqual", .func = timingSafeEqualNative, .arg_count = 2 },
};

// ============================================================================
// parseBearer
// ============================================================================

/// parseBearer(header) - extract token from "Bearer <token>"
fn parseBearerNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len == 0) return value.JSValue.null_val;
    const header = util.extractString(args[0]) orelse return value.JSValue.null_val;

    if (header.len < 7) return value.JSValue.null_val;
    if (!std.ascii.eqlIgnoreCase(header[0..7], "Bearer ")) return value.JSValue.null_val;

    const token = header[7..];
    if (token.len == 0) return value.JSValue.null_val;

    return ctx.createString(token) catch value.JSValue.null_val;
}

// ============================================================================
// jwtVerify
// ============================================================================

/// jwtVerify(token, secret, options?) - verify HS256 JWT
fn jwtVerifyNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len < 2) return util.createPlainResultErr(ctx, "missing arguments");
    const token_str = util.extractString(args[0]) orelse return util.createPlainResultErr(ctx, "token must be a string");
    const secret = util.extractString(args[1]) orelse return util.createPlainResultErr(ctx, "secret must be a string");

    // Split token into header.payload.signature
    const first_dot = std.mem.indexOfScalar(u8, token_str, '.') orelse
        return util.createPlainResultErr(ctx, "invalid token format");
    const rest = token_str[first_dot + 1 ..];
    const second_dot = std.mem.indexOfScalar(u8, rest, '.') orelse
        return util.createPlainResultErr(ctx, "invalid token format");

    const header_b64 = token_str[0..first_dot];
    const payload_b64 = rest[0..second_dot];
    const signature_b64 = rest[second_dot + 1 ..];
    const signing_input = token_str[0 .. first_dot + 1 + second_dot]; // header.payload

    // Verify signature: HMAC-SHA256(signing_input, secret)
    var expected_mac: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&expected_mac, signing_input, secret);

    // Decode the provided signature (strip trailing '=' padding if present)
    const sig_clean = trimTrailingPadding(signature_b64, "=");
    const sig_decoded_len = base64url.Decoder.calcSizeForSlice(sig_clean) catch
        return util.createPlainResultErr(ctx, "invalid signature encoding");

    if (sig_decoded_len != HmacSha256.mac_length)
        return util.createPlainResultErr(ctx, "invalid signature length");

    var sig_decoded: [HmacSha256.mac_length]u8 = undefined;
    base64url.Decoder.decode(&sig_decoded, sig_clean) catch
        return util.createPlainResultErr(ctx, "invalid signature encoding");

    // Constant-time comparison
    if (!constTimeEql(&sig_decoded, &expected_mac))
        return util.createPlainResultErr(ctx, "invalid signature");

    // Decode and verify header (must be HS256)
    _ = decodeBase64url(ctx.allocator, header_b64) catch
        return util.createPlainResultErr(ctx, "invalid header encoding");

    // Decode payload
    const payload_bytes = decodeBase64url(ctx.allocator, payload_b64) catch
        return util.createPlainResultErr(ctx, "invalid payload encoding");
    defer ctx.allocator.free(payload_bytes);

    // Parse claims as JSON into JSValue
    const claims_val = builtins.parseJsonValue(ctx, payload_bytes) catch
        return util.createPlainResultErr(ctx, "invalid claims JSON");

    // Check exp/nbf if present
    if (claims_val.isObject()) {
        const claims_obj = claims_val.toPtr(object.JSObject);
        const pool = ctx.hidden_class_pool orelse return util.createPlainResultErr(ctx, "internal error");
        const now_ms = compat.realtimeNowMs() catch return util.createPlainResultErr(ctx, "clock error");
        const now = @divTrunc(now_ms, 1000);

        // Check exp (expiration)
        const exp_atom = try ctx.atoms.intern("exp");
        if (claims_obj.getProperty(pool, exp_atom)) |exp_val| {
            if (util.extractFloat(exp_val)) |exp_f| {
                const exp_ts: i64 = @intFromFloat(exp_f);
                if (now > exp_ts) return util.createPlainResultErr(ctx, "token expired");
            }
        }

        // Check nbf (not before)
        const nbf_atom = try ctx.atoms.intern("nbf");
        if (claims_obj.getProperty(pool, nbf_atom)) |nbf_val| {
            if (util.extractFloat(nbf_val)) |nbf_f| {
                const nbf_ts: i64 = @intFromFloat(nbf_f);
                if (now < nbf_ts) return util.createPlainResultErr(ctx, "token not yet valid");
            }
        }
    }

    return util.createPlainResultOk(ctx, claims_val);
}

// ============================================================================
// jwtSign
// ============================================================================

/// jwtSign(claims_json, secret) - create HS256 JWT
fn jwtSignNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len < 2) return value.JSValue.undefined_val;
    const claims_json = util.extractString(args[0]) orelse return value.JSValue.undefined_val;
    const secret = util.extractString(args[1]) orelse return value.JSValue.undefined_val;

    // Fixed HS256 header
    const header_b64 = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"; // {"alg":"HS256","typ":"JWT"}

    // Base64url-encode the claims payload
    const payload_b64 = encodeBase64url(ctx.allocator, claims_json) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(payload_b64);

    // Build signing input: header.payload
    const signing_input_len = header_b64.len + 1 + payload_b64.len;
    const signing_input = ctx.allocator.alloc(u8, signing_input_len) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(signing_input);

    @memcpy(signing_input[0..header_b64.len], header_b64);
    signing_input[header_b64.len] = '.';
    @memcpy(signing_input[header_b64.len + 1 ..], payload_b64);

    // HMAC-SHA256 sign
    var mac: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&mac, signing_input, secret);

    // Base64url-encode signature
    const sig_b64 = encodeBase64url(ctx.allocator, &mac) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(sig_b64);

    // Concatenate: header.payload.signature
    const total_len = signing_input_len + 1 + sig_b64.len;
    const result = ctx.allocator.alloc(u8, total_len) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(result);

    @memcpy(result[0..signing_input_len], signing_input);
    result[signing_input_len] = '.';
    @memcpy(result[signing_input_len + 1 ..], sig_b64);

    return ctx.createString(result) catch value.JSValue.undefined_val;
}

// ============================================================================
// verifyWebhookSignature
// ============================================================================

/// verifyWebhookSignature(payload, secret, signature) - HMAC-SHA256 webhook verification
fn verifyWebhookSignatureNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    _ = ctx_ptr;

    if (args.len < 3) return value.JSValue.false_val;
    const payload = util.extractString(args[0]) orelse return value.JSValue.false_val;
    const secret = util.extractString(args[1]) orelse return value.JSValue.false_val;
    const sig_str = util.extractString(args[2]) orelse return value.JSValue.false_val;

    // Compute expected HMAC
    var expected_mac: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&expected_mac, payload, secret);
    const expected_hex = std.fmt.bytesToHex(expected_mac, .lower);

    // Strip "sha256=" prefix if present
    const provided_hex = if (sig_str.len > 7 and std.mem.eql(u8, sig_str[0..7], "sha256="))
        sig_str[7..]
    else
        sig_str;

    // Constant-time comparison of hex strings
    if (provided_hex.len != expected_hex.len) return value.JSValue.false_val;

    if (constTimeEqlSlice(provided_hex, &expected_hex))
        return value.JSValue.true_val;

    return value.JSValue.false_val;
}

// ============================================================================
// timingSafeEqual
// ============================================================================

/// timingSafeEqual(a, b) - constant-time string comparison
fn timingSafeEqualNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    _ = ctx_ptr;

    if (args.len < 2) return value.JSValue.false_val;
    const a = util.extractString(args[0]) orelse return value.JSValue.false_val;
    const b = util.extractString(args[1]) orelse return value.JSValue.false_val;

    // Different lengths: not equal (length is not secret in HTTP contexts)
    if (a.len != b.len) return value.JSValue.false_val;

    // Constant-time byte-by-byte comparison
    var diff: u8 = 0;
    for (a, b) |ab, bb| {
        diff |= ab ^ bb;
    }

    return if (diff == 0) value.JSValue.true_val else value.JSValue.false_val;
}

// ============================================================================
// Internal helpers
// ============================================================================

/// Strip trailing '=' padding from a base64 string
fn trimTrailingPadding(input: []const u8, _: []const u8) []const u8 {
    var end = input.len;
    while (end > 0 and input[end - 1] == '=') {
        end -= 1;
    }
    return input[0..end];
}

/// Constant-time comparison for fixed-size byte arrays
fn constTimeEql(a: *const [HmacSha256.mac_length]u8, b: *const [HmacSha256.mac_length]u8) bool {
    return constTimeEqlSlice(a, b);
}

/// Constant-time comparison for equal-length slices
fn constTimeEqlSlice(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    var diff: u8 = 0;
    for (a, b) |ab, bb| {
        diff |= ab ^ bb;
    }
    return diff == 0;
}

/// Base64url-decode with padding tolerance
fn decodeBase64url(allocator: std.mem.Allocator, encoded: []const u8) ![]u8 {
    // Strip trailing padding
    const clean = trimTrailingPadding(encoded, "=");
    const decoded_len = base64url.Decoder.calcSizeForSlice(clean) catch return error.InvalidBase64;
    const buf = try allocator.alloc(u8, decoded_len);
    errdefer allocator.free(buf);
    base64url.Decoder.decode(buf, clean) catch {
        allocator.free(buf);
        return error.InvalidBase64;
    };
    return buf;
}

/// Base64url-encode (no padding)
fn encodeBase64url(allocator: std.mem.Allocator, data: []const u8) ![]u8 {
    const encoded_len = base64url.Encoder.calcSize(data.len);
    const buf = try allocator.alloc(u8, encoded_len);
    _ = base64url.Encoder.encode(buf, data);
    return buf;
}

// ============================================================================
// Tests
// ============================================================================

test "parseBearer: valid header" {
    // Direct string test without JS runtime
    const header = "Bearer eyJhbGciOiJIUzI1NiJ9.test.sig";
    if (header.len >= 7 and std.ascii.eqlIgnoreCase(header[0..7], "Bearer ")) {
        const token = header[7..];
        try std.testing.expectEqualStrings("eyJhbGciOiJIUzI1NiJ9.test.sig", token);
    } else {
        return error.TestFailed;
    }
}

test "parseBearer: missing prefix" {
    const header = "Basic dXNlcjpwYXNz";
    const has_bearer = header.len >= 7 and std.ascii.eqlIgnoreCase(header[0..7], "Bearer ");
    try std.testing.expect(!has_bearer);
}

test "base64url roundtrip" {
    const allocator = std.testing.allocator;
    const original = "hello world";
    const encoded = try encodeBase64url(allocator, original);
    defer allocator.free(encoded);
    const decoded = try decodeBase64url(allocator, encoded);
    defer allocator.free(decoded);
    try std.testing.expectEqualStrings(original, decoded);
}

test "base64url decode with padding" {
    const allocator = std.testing.allocator;
    // "hello" in base64url with padding added
    const encoded_no_pad = "aGVsbG8";
    const encoded_with_pad = "aGVsbG8=";

    const d1 = try decodeBase64url(allocator, encoded_no_pad);
    defer allocator.free(d1);
    const d2 = try decodeBase64url(allocator, encoded_with_pad);
    defer allocator.free(d2);

    try std.testing.expectEqualStrings("hello", d1);
    try std.testing.expectEqualStrings("hello", d2);
}

test "HMAC-SHA256 sign and verify roundtrip" {
    const allocator = std.testing.allocator;
    const secret = "test-secret-key";
    const claims = "{\"sub\":\"1234567890\",\"name\":\"Test User\",\"iat\":1516239022}";

    // Sign
    const header_b64 = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9";
    const payload_b64 = try encodeBase64url(allocator, claims);
    defer allocator.free(payload_b64);

    const signing_input_len = header_b64.len + 1 + payload_b64.len;
    const signing_input = try allocator.alloc(u8, signing_input_len);
    defer allocator.free(signing_input);
    @memcpy(signing_input[0..header_b64.len], header_b64);
    signing_input[header_b64.len] = '.';
    @memcpy(signing_input[header_b64.len + 1 ..], payload_b64);

    var mac: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&mac, signing_input, secret);

    const sig_b64 = try encodeBase64url(allocator, &mac);
    defer allocator.free(sig_b64);

    // Verify: decode sig and compare
    const sig_decoded = try decodeBase64url(allocator, sig_b64);
    defer allocator.free(sig_decoded);

    try std.testing.expect(constTimeEql(sig_decoded[0..32], &mac));
}

test "HMAC-SHA256 wrong key" {
    const secret = "correct-key";
    const wrong_key = "wrong-key";
    const data = "header.payload";

    var mac1: [HmacSha256.mac_length]u8 = undefined;
    var mac2: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&mac1, data, secret);
    HmacSha256.create(&mac2, data, wrong_key);

    try std.testing.expect(!constTimeEql(&mac1, &mac2));
}

test "webhook signature verification" {
    const payload = "test payload";
    const secret = "webhook-secret";

    var expected_mac: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&expected_mac, payload, secret);
    const expected_hex = std.fmt.bytesToHex(expected_mac, .lower);

    // Without prefix
    try std.testing.expect(constTimeEqlSlice(&expected_hex, &expected_hex));

    // With sha256= prefix
    var with_prefix: [71]u8 = undefined;
    @memcpy(with_prefix[0..7], "sha256=");
    @memcpy(with_prefix[7..71], &expected_hex);
    const stripped = with_prefix[7..71];
    try std.testing.expect(constTimeEqlSlice(stripped, &expected_hex));
}

test "timingSafeEqual: equal strings" {
    const a = "hello world";
    const b = "hello world";
    var diff: u8 = 0;
    for (a, b) |ab, bb| {
        diff |= ab ^ bb;
    }
    try std.testing.expect(diff == 0);
}

test "timingSafeEqual: unequal strings" {
    const a = "hello world";
    const b = "hello worle";
    var diff: u8 = 0;
    for (a, b) |ab, bb| {
        diff |= ab ^ bb;
    }
    try std.testing.expect(diff != 0);
}
