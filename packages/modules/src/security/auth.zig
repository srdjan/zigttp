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

    const header_b64 = token_str[0..first_dot];
    const payload_b64 = rest[0..second_dot];
    const signature_b64 = rest[second_dot + 1 ..];
    const signing_input = token_str[0 .. first_dot + 1 + second_dot];

    const allocator = sdk.getAllocator(handle);

    // Reject tokens whose header does not advertise the algorithm we verify
    // against. Without this check the HMAC would still match a forged token
    // that supplied a different `alg` (e.g. "none", "HS512", or omitted),
    // because downstream code never reads the header. Per RFC 7519 §5.1 and
    // RFC 8725 §3, the verifier MUST reject any algorithm it did not expect.
    validateHs256Header(allocator, header_b64) catch |err| return sdk.resultErr(handle, switch (err) {
        error.HeaderTooLong => "jwt header too long",
        error.InvalidBase64 => "invalid header encoding",
        error.InvalidJson => "invalid header JSON",
        error.MissingAlg => "missing alg header",
        error.UnsupportedAlg => "unsupported alg",
    });

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

const JwtHeaderError = error{
    HeaderTooLong,
    InvalidBase64,
    InvalidJson,
    MissingAlg,
    UnsupportedAlg,
};

/// Largest JWT header we will parse. Real-world JWT headers carry `alg`,
/// `typ`, and occasionally `kid` or `cty` — well under 256 bytes encoded.
/// A 1 KiB ceiling rejects any obvious DoS via attacker-controlled headers
/// while leaving room for legitimate metadata.
const MAX_JWT_HEADER_LEN = 1024;

/// Validate that a base64url-encoded JWT header advertises the algorithm
/// we will verify against (HS256, the only `alg` the matching `jwtSign`
/// emits). Without this check the verifier would HMAC whatever it received
/// and accept tokens whose header claimed `none`, `HS512`, `RS256`, or
/// nothing at all — every classical JWT algorithm-confusion class begins
/// with the verifier failing to enforce the expected `alg`. See RFC 7519
/// §5.1 ("the `alg` Header Parameter") and RFC 8725 §3 ("Perform Algorithm
/// Verification").
fn validateHs256Header(allocator: std.mem.Allocator, header_b64: []const u8) JwtHeaderError!void {
    if (header_b64.len > MAX_JWT_HEADER_LEN) return error.HeaderTooLong;

    const clean = trimTrailingPadding(header_b64);
    const decoded_len = base64url.Decoder.calcSizeForSlice(clean) catch return error.InvalidBase64;
    if (decoded_len > MAX_JWT_HEADER_LEN) return error.HeaderTooLong;

    const decoded = allocator.alloc(u8, decoded_len) catch return error.InvalidBase64;
    defer allocator.free(decoded);
    base64url.Decoder.decode(decoded, clean) catch return error.InvalidBase64;

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, decoded, .{}) catch
        return error.InvalidJson;
    defer parsed.deinit();

    const obj = switch (parsed.value) {
        .object => |o| o,
        else => return error.InvalidJson,
    };

    const alg_value = obj.get("alg") orelse return error.MissingAlg;
    const alg_str = switch (alg_value) {
        .string => |s| s,
        else => return error.UnsupportedAlg,
    };

    if (!std.mem.eql(u8, alg_str, "HS256")) return error.UnsupportedAlg;
}

// ---------------------------------------------------------------------------
// Tests for the pure helpers. The runtime-coupled paths (jwtVerify,
// jwtSign, parseBearer, etc.) interact with sdk.extractString and
// sdk.parseJson, which are no-op stubs in the test_shim — those need
// handler-level integration coverage, not unit tests.
// ---------------------------------------------------------------------------

const testing = std.testing;

test "constTimeEqlSlice: equality, inequality, length mismatch" {
    try testing.expect(constTimeEqlSlice("abc", "abc"));
    try testing.expect(!constTimeEqlSlice("abc", "abd"));
    try testing.expect(!constTimeEqlSlice("abc", "ab"));
    try testing.expect(!constTimeEqlSlice("", "x"));
    try testing.expect(constTimeEqlSlice("", ""));

    // Differs only in the last byte — must still return false (catches an
    // accidental early-return optimisation that would re-introduce a
    // timing oracle).
    const a = "0123456789abcdef0123456789abcdee";
    const b = "0123456789abcdef0123456789abcdef";
    try testing.expect(!constTimeEqlSlice(a, b));
}

test "trimTrailingPadding strips one or more `=` characters" {
    try testing.expectEqualStrings("abc", trimTrailingPadding("abc"));
    try testing.expectEqualStrings("abc", trimTrailingPadding("abc="));
    try testing.expectEqualStrings("abc", trimTrailingPadding("abc=="));
    try testing.expectEqualStrings("", trimTrailingPadding("===="));
    try testing.expectEqualStrings("a=b", trimTrailingPadding("a=b"));
    try testing.expectEqualStrings("a=b", trimTrailingPadding("a=b="));
}

test "base64url encode/decode round-trip for arbitrary bytes" {
    const allocator = testing.allocator;
    const inputs = [_][]const u8{
        "",
        "f",
        "fo",
        "foo",
        "foob",
        "fooba",
        "foobar",
        "Many hands make light work.",
    };
    for (inputs) |input| {
        const encoded = try encodeBase64url(allocator, input);
        defer allocator.free(encoded);

        // base64url uses `-` and `_` instead of `+` and `/`, and no `=`.
        for (encoded) |c| {
            try testing.expect(c != '+' and c != '/' and c != '=');
        }

        const decoded = try decodeBase64url(allocator, encoded);
        defer allocator.free(decoded);
        try testing.expectEqualSlices(u8, input, decoded);
    }
}

test "decodeBase64url tolerates trailing `=` padding the encoder never emits" {
    // Some JWT libraries still emit padded base64url. The decoder must
    // accept both forms, otherwise we'd reject valid third-party tokens.
    const allocator = testing.allocator;
    const padded = "Zm9v"; // canonical, no padding
    const decoded = try decodeBase64url(allocator, padded);
    defer allocator.free(decoded);
    try testing.expectEqualStrings("foo", decoded);

    const padded_with_eq = "Zm9v==";
    const decoded2 = try decodeBase64url(allocator, padded_with_eq);
    defer allocator.free(decoded2);
    try testing.expectEqualStrings("foo", decoded2);
}

// ---------------------------------------------------------------------------
// validateHs256Header — guards against JWT algorithm-confusion bugs. The
// previous jwtVerifyImpl never read the header and would HMAC any token
// whose three dot-separated segments parsed, so a forged header that
// claimed `alg=none`, `alg=HS512`, or omitted `alg` slipped through as
// long as the signature happened to match the supplied secret. These
// tests pin the verifier to HS256 explicitly.
// ---------------------------------------------------------------------------

fn headerForTest(allocator: std.mem.Allocator, json: []const u8) ![]u8 {
    return encodeBase64url(allocator, json);
}

test "validateHs256Header accepts a real HS256 header" {
    const allocator = testing.allocator;
    const header = try headerForTest(allocator, "{\"alg\":\"HS256\",\"typ\":\"JWT\"}");
    defer allocator.free(header);
    try validateHs256Header(allocator, header);
}

test "validateHs256Header accepts the canonical jwtSign header verbatim" {
    // jwtSign emits HEADER_HS256_B64 verbatim; the verifier must accept it.
    try validateHs256Header(testing.allocator, HEADER_HS256_B64);
}

test "validateHs256Header rejects alg=none" {
    const allocator = testing.allocator;
    const header = try headerForTest(allocator, "{\"alg\":\"none\"}");
    defer allocator.free(header);
    try testing.expectError(error.UnsupportedAlg, validateHs256Header(allocator, header));
}

test "validateHs256Header rejects alg=HS512" {
    const allocator = testing.allocator;
    const header = try headerForTest(allocator, "{\"alg\":\"HS512\"}");
    defer allocator.free(header);
    try testing.expectError(error.UnsupportedAlg, validateHs256Header(allocator, header));
}

test "validateHs256Header rejects alg=RS256 (would otherwise enable HMAC-with-public-key confusion downstream)" {
    const allocator = testing.allocator;
    const header = try headerForTest(allocator, "{\"alg\":\"RS256\"}");
    defer allocator.free(header);
    try testing.expectError(error.UnsupportedAlg, validateHs256Header(allocator, header));
}

test "validateHs256Header rejects missing alg" {
    const allocator = testing.allocator;
    const header = try headerForTest(allocator, "{\"typ\":\"JWT\"}");
    defer allocator.free(header);
    try testing.expectError(error.MissingAlg, validateHs256Header(allocator, header));
}

test "validateHs256Header rejects non-string alg" {
    const allocator = testing.allocator;
    const header = try headerForTest(allocator, "{\"alg\":42}");
    defer allocator.free(header);
    try testing.expectError(error.UnsupportedAlg, validateHs256Header(allocator, header));
}

test "validateHs256Header rejects non-object JSON" {
    const allocator = testing.allocator;
    const header = try headerForTest(allocator, "\"not an object\"");
    defer allocator.free(header);
    try testing.expectError(error.InvalidJson, validateHs256Header(allocator, header));
}

test "validateHs256Header rejects malformed JSON inside the base64" {
    const allocator = testing.allocator;
    const header = try headerForTest(allocator, "{not json");
    defer allocator.free(header);
    try testing.expectError(error.InvalidJson, validateHs256Header(allocator, header));
}

test "validateHs256Header rejects invalid base64url" {
    // The character `!` is not in the base64url alphabet.
    try testing.expectError(error.InvalidBase64, validateHs256Header(testing.allocator, "!!!!"));
}

test "validateHs256Header rejects oversized headers" {
    // 2 KiB of `A`s — past the 1 KiB ceiling.
    const allocator = testing.allocator;
    const huge = try allocator.alloc(u8, MAX_JWT_HEADER_LEN + 1);
    defer allocator.free(huge);
    @memset(huge, 'A');
    try testing.expectError(error.HeaderTooLong, validateHs256Header(allocator, huge));
}

test "validateHs256Header is case-sensitive on alg value" {
    // RFC 7518 §3.1 defines `alg` values as case-sensitive strings; "hs256"
    // is not a valid alias for "HS256".
    const allocator = testing.allocator;
    const header = try headerForTest(allocator, "{\"alg\":\"hs256\"}");
    defer allocator.free(header);
    try testing.expectError(error.UnsupportedAlg, validateHs256Header(allocator, header));
}

