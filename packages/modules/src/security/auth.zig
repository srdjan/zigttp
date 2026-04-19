//! zigttp:auth - Authentication and JWT utilities
//!
//! Exports:
//!   parseBearer(header) -> string | undefined
//!   jwtVerify(token, secret, options?) -> Result<claims, string>
//!   jwtSign(claims_json, secret) -> string | undefined
//!   verifyWebhookSignature(payload, secret, signature) -> boolean
//!   timingSafeEqual(a, b) -> boolean

const std = @import("std");
const sdk = @import("zigttp-sdk");

const base64url = std.base64.url_safe_no_pad;
const MAC_LEN = 32;

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:auth",
    .name = "auth",
    .required_capabilities = &.{ .crypto, .clock },
    .exports = &.{
        .{
            .name = "parseBearer",
            .module_func = parseBearerImpl,
            .arg_count = 1,
            .returns = .optional_string,
            .param_types = &.{.string},
            .failure_severity = .expected,
            .contract_flags = .{ .sets_bearer_auth = true },
            .return_labels = .{ .credential = true },
            .laws = &.{.pure},
        },
        .{
            .name = "jwtVerify",
            .module_func = jwtVerifyImpl,
            .arg_count = 3,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_flags = .{ .sets_jwt_auth = true },
            .return_labels = .{ .credential = true, .validated = true },
            .laws = &.{
                .{ .absorbing = .{
                    .arg_position = 0,
                    .argument_shape = .empty_string_literal,
                    .residue = .result_err,
                } },
            },
        },
        .{ .name = "jwtSign", .module_func = jwtSignImpl, .arg_count = 2, .returns = .string, .param_types = &.{ .string, .string }, .return_labels = .{ .credential = true } },
        .{
            .name = "verifyWebhookSignature",
            .module_func = verifyWebhookSignatureImpl,
            .arg_count = 3,
            .returns = .boolean,
            .param_types = &.{ .string, .string, .string },
            .laws = &.{.pure},
        },
        .{
            .name = "timingSafeEqual",
            .module_func = timingSafeEqualImpl,
            .arg_count = 2,
            .returns = .boolean,
            .param_types = &.{ .string, .string },
            .laws = &.{.pure},
        },
    },
};

fn parseBearerImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const header = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    if (header.len < 7 or !std.ascii.eqlIgnoreCase(header[0..7], "Bearer ")) return sdk.JSValue.undefined_val;
    const token = header[7..];
    if (token.len == 0) return sdk.JSValue.undefined_val;
    return sdk.createString(handle, token) catch sdk.JSValue.undefined_val;
}

fn jwtVerifyImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const token_str = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "token must be a string");
    const secret = sdk.extractString(args[1]) orelse return sdk.resultErr(handle, "secret must be a string");

    const first_dot = std.mem.indexOfScalar(u8, token_str, '.') orelse
        return sdk.resultErr(handle, "invalid token format");
    const rest = token_str[first_dot + 1 ..];
    const second_dot = std.mem.indexOfScalar(u8, rest, '.') orelse
        return sdk.resultErr(handle, "invalid token format");

    const payload_b64 = rest[0..second_dot];
    const signature_b64 = rest[second_dot + 1 ..];
    const signing_input = token_str[0 .. first_dot + 1 + second_dot];

    var expected_mac: sdk.HmacSha256Mac = undefined;
    try sdk.hmacSha256(handle, signing_input, secret, &expected_mac);

    const sig_clean = trimTrailingPadding(signature_b64);
    const sig_decoded_len = base64url.Decoder.calcSizeForSlice(sig_clean) catch
        return sdk.resultErr(handle, "invalid signature encoding");
    if (sig_decoded_len != MAC_LEN)
        return sdk.resultErr(handle, "invalid signature length");

    var sig_decoded: [MAC_LEN]u8 = undefined;
    base64url.Decoder.decode(&sig_decoded, sig_clean) catch
        return sdk.resultErr(handle, "invalid signature encoding");

    if (!constTimeEqlSlice(&sig_decoded, &expected_mac))
        return sdk.resultErr(handle, "invalid signature");

    const allocator = sdk.getAllocator(handle);
    const payload_bytes = decodeBase64url(allocator, payload_b64) catch
        return sdk.resultErr(handle, "invalid payload encoding");
    defer allocator.free(payload_bytes);

    const claims_val = sdk.parseJson(handle, payload_bytes) catch
        return sdk.resultErr(handle, "invalid claims JSON");

    const now = @divTrunc(sdk.nowMs(handle) catch return sdk.resultErr(handle, "clock error"), 1000);

    if (sdk.objectGet(handle, claims_val, "exp")) |exp_val| {
        if (sdk.extractFloat(exp_val)) |exp_f| {
            if (now > @as(i64, @intFromFloat(exp_f))) return sdk.resultErr(handle, "token expired");
        }
    }
    if (sdk.objectGet(handle, claims_val, "nbf")) |nbf_val| {
        if (sdk.extractFloat(nbf_val)) |nbf_f| {
            if (now < @as(i64, @intFromFloat(nbf_f))) return sdk.resultErr(handle, "token not yet valid");
        }
    }

    return sdk.resultOk(handle, claims_val);
}

const HEADER_HS256_B64 = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9";

fn jwtSignImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.undefined_val;
    const claims_json = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;
    const secret = sdk.extractString(args[1]) orelse return sdk.JSValue.undefined_val;

    const allocator = sdk.getAllocator(handle);

    const payload_b64 = encodeBase64url(allocator, claims_json) catch return sdk.JSValue.undefined_val;
    defer allocator.free(payload_b64);

    const signing_input_len = HEADER_HS256_B64.len + 1 + payload_b64.len;
    const signing_input = allocator.alloc(u8, signing_input_len) catch return sdk.JSValue.undefined_val;
    defer allocator.free(signing_input);
    @memcpy(signing_input[0..HEADER_HS256_B64.len], HEADER_HS256_B64);
    signing_input[HEADER_HS256_B64.len] = '.';
    @memcpy(signing_input[HEADER_HS256_B64.len + 1 ..], payload_b64);

    var mac: sdk.HmacSha256Mac = undefined;
    try sdk.hmacSha256(handle, signing_input, secret, &mac);

    const sig_b64 = encodeBase64url(allocator, &mac) catch return sdk.JSValue.undefined_val;
    defer allocator.free(sig_b64);

    const total_len = signing_input_len + 1 + sig_b64.len;
    const result = allocator.alloc(u8, total_len) catch return sdk.JSValue.undefined_val;
    defer allocator.free(result);
    @memcpy(result[0..signing_input_len], signing_input);
    result[signing_input_len] = '.';
    @memcpy(result[signing_input_len + 1 ..], sig_b64);

    return sdk.createString(handle, result) catch sdk.JSValue.undefined_val;
}

fn verifyWebhookSignatureImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 3) return sdk.JSValue.false_val;
    const payload = sdk.extractString(args[0]) orelse return sdk.JSValue.false_val;
    const secret = sdk.extractString(args[1]) orelse return sdk.JSValue.false_val;
    const sig_str = sdk.extractString(args[2]) orelse return sdk.JSValue.false_val;

    var expected_mac: sdk.HmacSha256Mac = undefined;
    try sdk.hmacSha256(handle, payload, secret, &expected_mac);
    const expected_hex = std.fmt.bytesToHex(expected_mac, .lower);

    const provided_hex = if (sig_str.len > 7 and std.mem.eql(u8, sig_str[0..7], "sha256="))
        sig_str[7..]
    else
        sig_str;

    if (provided_hex.len != expected_hex.len) return sdk.JSValue.false_val;
    return if (constTimeEqlSlice(provided_hex, &expected_hex)) sdk.JSValue.true_val else sdk.JSValue.false_val;
}

fn timingSafeEqualImpl(_: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.false_val;
    const a = sdk.extractString(args[0]) orelse return sdk.JSValue.false_val;
    const b = sdk.extractString(args[1]) orelse return sdk.JSValue.false_val;
    if (a.len != b.len) return sdk.JSValue.false_val;
    return if (constTimeEqlSlice(a, b)) sdk.JSValue.true_val else sdk.JSValue.false_val;
}

fn trimTrailingPadding(input: []const u8) []const u8 {
    var end = input.len;
    while (end > 0 and input[end - 1] == '=') : (end -= 1) {}
    return input[0..end];
}

fn constTimeEqlSlice(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    var diff: u8 = 0;
    for (a, b) |ab, bb| diff |= ab ^ bb;
    return diff == 0;
}

fn decodeBase64url(allocator: std.mem.Allocator, encoded: []const u8) ![]u8 {
    const clean = trimTrailingPadding(encoded);
    const decoded_len = base64url.Decoder.calcSizeForSlice(clean) catch return error.InvalidBase64;
    const buf = try allocator.alloc(u8, decoded_len);
    errdefer allocator.free(buf);
    base64url.Decoder.decode(buf, clean) catch return error.InvalidBase64;
    return buf;
}

fn encodeBase64url(allocator: std.mem.Allocator, data: []const u8) ![]u8 {
    const encoded_len = base64url.Encoder.calcSize(data.len);
    const buf = try allocator.alloc(u8, encoded_len);
    _ = base64url.Encoder.encode(buf, data);
    return buf;
}

