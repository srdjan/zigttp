//! Perf-as-proof receipts.
//!
//! Defines the JWS subtype attached to `kind=perf` events in
//! `.zttp/proofs.jsonl`. A perf receipt commits to:
//!   - the handler's content hash (so a receipt cannot be moved to a
//!     different handler post-hoc),
//!   - the observed p50 / p99 latency in microseconds,
//!   - the allocator byte total,
//!   - the sample count the probe completed before its wall-clock budget
//!     expired, and
//!   - the unix epoch second the probe finished.
//!
//! The schema is deliberately small but reserves slot names that future
//! slices will populate without breaking older verifiers:
//!   - `corpusId` (Slice K): the request corpus the probe drew from.
//!   - `baselineRef` (Slice K): the previous receipt this one compares to.
//!   - `regressionBudgetUs` (Slice K): the threshold beyond which a CI
//!     gate should reject.
//! Older verifiers ignore unknown fields; newer verifiers light up extra
//! checks only when the fields are present.
//!
//! Trust model mirrors `attest/envelope.zig`: an EdDSA signature commits to
//! "whoever held this private key produced these claims." Identity binding
//! happens via the persistent keypair at `~/.zttp/attest/keypair.bin`
//! that the runtime already uses for `kind=deploy` receipts; the perf
//! subtype reuses that key, but a distinct `typ` header keeps consumers
//! that only know how to interpret build receipts from misclassifying a
//! perf row.

const std = @import("std");
const Ed25519 = std.crypto.sign.Ed25519;
const Sha256 = std.crypto.hash.sha2.Sha256;
const b64 = std.base64.url_safe_no_pad;

pub const version_tag: []const u8 = "zttp-perf-v1";
pub const jws_typ: []const u8 = "zttp-perf+jws";

/// `handler_hash` is a 64-character lowercase hex string - the same form
/// `std.fmt.bytesToHex` produces and the ledger writes for `contract_sha`.
pub const PerfClaims = struct {
    /// SHA-256 of the handler source bytes the probe ran against.
    handler_hash: []const u8,
    /// 50th percentile per-iteration latency in microseconds. Zero is a
    /// legal value (sub-microsecond) but the probe always reports at
    /// least one digit of precision.
    p50_us: u64,
    /// 99th percentile per-iteration latency in microseconds.
    p99_us: u64,
    /// Total bytes the request-scoped allocator served across all probe
    /// iterations. Aggregate (not per-iter) so future variance metrics
    /// can divide by `sample_count` if needed.
    alloc_bytes: u64,
    /// Number of probe iterations completed before the wall-clock budget
    /// expired. Zero means the probe was skipped (see Slice H notes in
    /// `proof_enrichment.zig`); the skip reason is recorded out of band.
    sample_count: u64,
    /// Unix epoch seconds when the probe finished.
    signed_at_unix: i64,
};

pub const Envelope = struct {
    public_key: [Ed25519.PublicKey.encoded_length]u8,
    signature: [Ed25519.Signature.encoded_length]u8,
    jws_compact: []u8,

    pub fn deinit(self: *Envelope, allocator: std.mem.Allocator) void {
        if (self.jws_compact.len > 0) allocator.free(self.jws_compact);
    }

    pub fn intoJws(self: *Envelope) []u8 {
        const jws = self.jws_compact;
        self.jws_compact = "";
        return jws;
    }
};

pub const VerifyResult = struct {
    claims: PerfClaims,
    public_key: [Ed25519.PublicKey.encoded_length]u8,
    fingerprint_hex: [64]u8,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *VerifyResult) void {
        self.arena.deinit();
    }
};

pub const SignError = error{
    InvalidHexLength,
    OutOfMemory,
    SignFailed,
};

pub const VerifyError = error{
    MalformedJws,
    InvalidBase64,
    InvalidJson,
    UnsupportedAlgorithm,
    UnexpectedType,
    MissingPublicKey,
    SignatureMismatch,
    OutOfMemory,
};

pub fn keyFingerprint(public_key: [Ed25519.PublicKey.encoded_length]u8) [64]u8 {
    var digest: [32]u8 = undefined;
    Sha256.hash(&public_key, &digest, .{});
    return std.fmt.bytesToHex(digest, .lower);
}

pub fn sign(
    allocator: std.mem.Allocator,
    claims: PerfClaims,
    key_pair: Ed25519.KeyPair,
) SignError!Envelope {
    if (claims.handler_hash.len != 64) return error.InvalidHexLength;

    const public_key_bytes = key_pair.public_key.bytes;
    const pubkey_b64 = encodeBase64Owned(allocator, &public_key_bytes) catch return error.OutOfMemory;
    defer allocator.free(pubkey_b64);

    const fingerprint = keyFingerprint(public_key_bytes);

    const header_json = buildHeaderJson(allocator, fingerprint, pubkey_b64) catch return error.OutOfMemory;
    defer allocator.free(header_json);

    const payload_json = buildPayloadJson(allocator, claims) catch return error.OutOfMemory;
    defer allocator.free(payload_json);

    const header_b64 = encodeBase64Owned(allocator, header_json) catch return error.OutOfMemory;
    defer allocator.free(header_b64);

    const payload_b64 = encodeBase64Owned(allocator, payload_json) catch return error.OutOfMemory;
    defer allocator.free(payload_b64);

    const signing_input = joinDot(allocator, header_b64, payload_b64) catch return error.OutOfMemory;
    defer allocator.free(signing_input);

    const signature = key_pair.sign(signing_input, null) catch return error.SignFailed;
    const signature_bytes = signature.toBytes();
    const signature_b64 = encodeBase64Owned(allocator, &signature_bytes) catch return error.OutOfMemory;
    defer allocator.free(signature_b64);

    const jws_compact = joinDot(allocator, signing_input, signature_b64) catch return error.OutOfMemory;

    return .{
        .public_key = public_key_bytes,
        .signature = signature_bytes,
        .jws_compact = jws_compact,
    };
}

pub fn verify(
    allocator: std.mem.Allocator,
    jws_compact: []const u8,
) VerifyError!VerifyResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const ar = arena.allocator();

    const parts = splitThree(jws_compact) orelse return error.MalformedJws;

    const header_bytes = decodeBase64(ar, parts.header) catch return error.InvalidBase64;
    const payload_bytes = decodeBase64(ar, parts.payload) catch return error.InvalidBase64;
    const signature_bytes = decodeBase64(ar, parts.signature) catch return error.InvalidBase64;

    if (signature_bytes.len != Ed25519.Signature.encoded_length) return error.MalformedJws;
    var sig_array: [Ed25519.Signature.encoded_length]u8 = undefined;
    @memcpy(&sig_array, signature_bytes);

    const public_key_bytes = try parsePublicKey(ar, header_bytes);

    const signing_input = joinDot(ar, parts.header, parts.payload) catch return error.OutOfMemory;

    const public_key = Ed25519.PublicKey.fromBytes(public_key_bytes) catch return error.MissingPublicKey;
    const signature = Ed25519.Signature.fromBytes(sig_array);
    signature.verify(signing_input, public_key) catch return error.SignatureMismatch;

    const claims = parseClaims(ar, payload_bytes) catch return error.InvalidJson;

    return .{
        .claims = claims,
        .public_key = public_key_bytes,
        .fingerprint_hex = keyFingerprint(public_key_bytes),
        .arena = arena,
    };
}

// ---------------------------------------------------------------------------
// Internals
// ---------------------------------------------------------------------------

const JwsParts = struct { header: []const u8, payload: []const u8, signature: []const u8 };

fn splitThree(s: []const u8) ?JwsParts {
    const first = std.mem.indexOfScalar(u8, s, '.') orelse return null;
    const rest = s[first + 1 ..];
    const second_rel = std.mem.indexOfScalar(u8, rest, '.') orelse return null;
    const second = first + 1 + second_rel;
    if (std.mem.indexOfScalarPos(u8, s, second + 1, '.') != null) return null;
    return .{
        .header = s[0..first],
        .payload = s[first + 1 .. second],
        .signature = s[second + 1 ..],
    };
}

fn encodeBase64Owned(allocator: std.mem.Allocator, bytes: []const u8) ![]u8 {
    const out_len = b64.Encoder.calcSize(bytes.len);
    const out = try allocator.alloc(u8, out_len);
    _ = b64.Encoder.encode(out, bytes);
    return out;
}

fn decodeBase64(allocator: std.mem.Allocator, src: []const u8) ![]u8 {
    const exact_len = try b64.Decoder.calcSizeForSlice(src);
    const out = try allocator.alloc(u8, exact_len);
    try b64.Decoder.decode(out, src);
    return out;
}

fn joinDot(allocator: std.mem.Allocator, a: []const u8, c: []const u8) ![]u8 {
    const out = try allocator.alloc(u8, a.len + 1 + c.len);
    @memcpy(out[0..a.len], a);
    out[a.len] = '.';
    @memcpy(out[a.len + 1 ..], c);
    return out;
}

fn buildHeaderJson(
    allocator: std.mem.Allocator,
    fingerprint: [64]u8,
    pubkey_b64: []const u8,
) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "{{\"alg\":\"EdDSA\",\"typ\":\"" ++ jws_typ ++ "\",\"kid\":\"{s}\",\"jwk\":{{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"x\":\"{s}\"}}}}",
        .{ fingerprint, pubkey_b64 },
    );
}

const wire_key = struct {
    const v = "v";
    const handler_hash = "handlerHash";
    const p50_us = "p50Us";
    const p99_us = "p99Us";
    const alloc_bytes = "allocBytes";
    const sample_count = "sampleCount";
    const signed_at = "signedAt";
};

fn buildPayloadJson(allocator: std.mem.Allocator, claims: PerfClaims) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "{{\"" ++ wire_key.v ++ "\":\"" ++ version_tag ++ "\"," ++
            "\"" ++ wire_key.handler_hash ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.p50_us ++ "\":{d}," ++
            "\"" ++ wire_key.p99_us ++ "\":{d}," ++
            "\"" ++ wire_key.alloc_bytes ++ "\":{d}," ++
            "\"" ++ wire_key.sample_count ++ "\":{d}," ++
            "\"" ++ wire_key.signed_at ++ "\":{d}}}",
        .{
            claims.handler_hash,
            claims.p50_us,
            claims.p99_us,
            claims.alloc_bytes,
            claims.sample_count,
            claims.signed_at_unix,
        },
    );
}

const ParseHeaderError = error{
    InvalidJson,
    UnsupportedAlgorithm,
    UnexpectedType,
    MissingPublicKey,
    InvalidBase64,
    OutOfMemory,
};

fn parsePublicKey(
    allocator: std.mem.Allocator,
    header_bytes: []const u8,
) ParseHeaderError![Ed25519.PublicKey.encoded_length]u8 {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, header_bytes, .{}) catch return error.InvalidJson;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidJson;
    const obj = parsed.value.object;

    const alg = obj.get("alg") orelse return error.InvalidJson;
    if (alg != .string or !std.mem.eql(u8, alg.string, "EdDSA")) return error.UnsupportedAlgorithm;

    // Reject build-receipt JWS strings so a perf verifier cannot be tricked
    // into accepting a build receipt as a perf claim. The build receipt
    // would still verify cryptographically because it uses the same key,
    // but the payload schema is different and silent acceptance would
    // hide the mismatch.
    const typ = obj.get("typ") orelse return error.InvalidJson;
    if (typ != .string or !std.mem.eql(u8, typ.string, jws_typ)) return error.UnexpectedType;

    const jwk = obj.get("jwk") orelse return error.MissingPublicKey;
    if (jwk != .object) return error.MissingPublicKey;
    const x = jwk.object.get("x") orelse return error.MissingPublicKey;
    if (x != .string) return error.MissingPublicKey;

    const decoded = decodeBase64(allocator, x.string) catch return error.InvalidBase64;
    if (decoded.len != Ed25519.PublicKey.encoded_length) return error.MissingPublicKey;

    var out: [Ed25519.PublicKey.encoded_length]u8 = undefined;
    @memcpy(&out, decoded);
    return out;
}

fn parseClaims(allocator: std.mem.Allocator, payload_bytes: []const u8) !PerfClaims {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload_bytes, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidJson;
    const obj = parsed.value.object;

    return .{
        .handler_hash = try dupString(allocator, obj, wire_key.handler_hash),
        .p50_us = try readU64(obj, wire_key.p50_us),
        .p99_us = try readU64(obj, wire_key.p99_us),
        .alloc_bytes = try readU64(obj, wire_key.alloc_bytes),
        .sample_count = try readU64(obj, wire_key.sample_count),
        .signed_at_unix = try readI64(obj, wire_key.signed_at),
    };
}

fn dupString(allocator: std.mem.Allocator, obj: std.json.ObjectMap, key: []const u8) ![]const u8 {
    const val = obj.get(key) orelse return error.InvalidJson;
    if (val != .string) return error.InvalidJson;
    return try allocator.dupe(u8, val.string);
}

fn readI64(obj: std.json.ObjectMap, key: []const u8) !i64 {
    const val = obj.get(key) orelse return error.InvalidJson;
    if (val != .integer) return error.InvalidJson;
    return val.integer;
}

fn readU64(obj: std.json.ObjectMap, key: []const u8) !u64 {
    const val = obj.get(key) orelse return error.InvalidJson;
    if (val != .integer or val.integer < 0) return error.InvalidJson;
    return @intCast(val.integer);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

const test_seed: [Ed25519.KeyPair.seed_length]u8 = .{
    0x9d, 0x61, 0xb1, 0x9d, 0xef, 0xfd, 0x5a, 0x60,
    0xba, 0x84, 0x4a, 0xf4, 0x92, 0xec, 0x2c, 0xc4,
    0x44, 0x49, 0xc5, 0x69, 0x7b, 0x32, 0x69, 0x19,
    0x70, 0x3b, 0xac, 0x03, 0x1c, 0xae, 0x7f, 0x60,
};

fn testClaims() PerfClaims {
    return .{
        .handler_hash = "a" ** 64,
        .p50_us = 12,
        .p99_us = 240,
        .alloc_bytes = 8192,
        .sample_count = 4096,
        .signed_at_unix = 1_700_000_000,
    };
}

test "perf_receipt sign then verify recovers the same claims" {
    const allocator = testing.allocator;
    const kp = try Ed25519.KeyPair.generateDeterministic(test_seed);
    var env = try sign(allocator, testClaims(), kp);
    defer env.deinit(allocator);

    var result = try verify(allocator, env.jws_compact);
    defer result.deinit();

    try testing.expectEqualStrings("a" ** 64, result.claims.handler_hash);
    try testing.expectEqual(@as(u64, 12), result.claims.p50_us);
    try testing.expectEqual(@as(u64, 240), result.claims.p99_us);
    try testing.expectEqual(@as(u64, 8192), result.claims.alloc_bytes);
    try testing.expectEqual(@as(u64, 4096), result.claims.sample_count);
    try testing.expectEqual(@as(i64, 1_700_000_000), result.claims.signed_at_unix);
    try testing.expectEqualSlices(u8, &kp.public_key.bytes, &result.public_key);
}

test "perf_receipt rejects claims with a non-64-char handler hash" {
    const allocator = testing.allocator;
    const kp = try Ed25519.KeyPair.generateDeterministic(test_seed);
    var bad = testClaims();
    bad.handler_hash = "short";
    try testing.expectError(error.InvalidHexLength, sign(allocator, bad, kp));
}

test "perf_receipt flipping a payload byte invalidates the signature" {
    const allocator = testing.allocator;
    const kp = try Ed25519.KeyPair.generateDeterministic(test_seed);
    var env = try sign(allocator, testClaims(), kp);
    defer env.deinit(allocator);

    const parts = splitThree(env.jws_compact).?;
    const payload_start = parts.header.len + 1;
    const tampered = try allocator.dupe(u8, env.jws_compact);
    defer allocator.free(tampered);
    tampered[payload_start] ^= 0x01;

    try testing.expectError(error.SignatureMismatch, verify(allocator, tampered));
}

test "perf_receipt build-receipt typ header is rejected as wrong subtype" {
    const allocator = testing.allocator;
    const kp = try Ed25519.KeyPair.generateDeterministic(test_seed);

    // Hand-roll a JWS whose header advertises the build-receipt typ but
    // whose payload is our perf shape. The signature is still valid; only
    // the typ check protects us.
    const pubkey_b64 = try encodeBase64Owned(allocator, &kp.public_key.bytes);
    defer allocator.free(pubkey_b64);
    const fingerprint = keyFingerprint(kp.public_key.bytes);
    const header_json = try std.fmt.allocPrint(
        allocator,
        "{{\"alg\":\"EdDSA\",\"typ\":\"zttp-attest+jws\",\"kid\":\"{s}\",\"jwk\":{{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"x\":\"{s}\"}}}}",
        .{ fingerprint, pubkey_b64 },
    );
    defer allocator.free(header_json);
    const payload_json = try buildPayloadJson(allocator, testClaims());
    defer allocator.free(payload_json);
    const header_b64 = try encodeBase64Owned(allocator, header_json);
    defer allocator.free(header_b64);
    const payload_b64 = try encodeBase64Owned(allocator, payload_json);
    defer allocator.free(payload_b64);
    const signing_input = try joinDot(allocator, header_b64, payload_b64);
    defer allocator.free(signing_input);
    const signature = try kp.sign(signing_input, null);
    const signature_bytes = signature.toBytes();
    const signature_b64 = try encodeBase64Owned(allocator, &signature_bytes);
    defer allocator.free(signature_b64);
    const forged = try joinDot(allocator, signing_input, signature_b64);
    defer allocator.free(forged);

    try testing.expectError(error.UnexpectedType, verify(allocator, forged));
}

test "perf_receipt version tag string is stable across builds" {
    try testing.expectEqualStrings("zttp-perf-v1", version_tag);
    try testing.expectEqualStrings("zttp-perf+jws", jws_typ);
}
