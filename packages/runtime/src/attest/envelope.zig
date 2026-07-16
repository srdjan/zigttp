//! Proof receipt envelope.
//!
//! Produces and verifies a compact JWS (RFC 7515 Section 7.1) that commits
//! to a handler build's contract hash, bytecode hash, analyzer policy hash,
//! serialized runtime policy hash, and a human-readable property summary.
//! Ed25519 signatures, public key embedded in the protected header so the JWS
//! is self-contained.
//!
//! Trust model: this confirms "whoever held this private key signed
//! these claims," not "this key belongs to the legitimate operator."
//! External verifiers should pin trusted keys or inspect the well-known
//! attestation document served by the deployed binary.

const std = @import("std");
const Ed25519 = std.crypto.sign.Ed25519;
const Sha256 = std.crypto.hash.sha2.Sha256;
const b64 = std.base64.url_safe_no_pad;

pub const version_tag: []const u8 = "zigttp-attest-v1";
pub const unpinned_runtime_policy_sha256: []const u8 = "0" ** 64;

/// Forty-byte payload caller assembles; sign() embeds it into the JWS.
/// Hex fields are 64-character lowercase strings (the form `bytesToHex`
/// produces and the form `proofs/bundle.zig` writes into manifests).
pub const Claims = struct {
    contract_sha256: []const u8,
    bytecode_sha256: []const u8,
    policy_sha256: []const u8,
    capability_hash: []const u8,
    runtime_policy_sha256: []const u8 = unpinned_runtime_policy_sha256,
    compiler_version: []const u8,
    signed_at_unix: i64,
    property_summary: []const u8,
    routes_count: u64,
    durable_workflow_proof_level: []const u8 = "none",
    durable_workflow_retry_safe: bool = false,
    durable_workflow_idempotent: bool = false,
    durable_workflow_fault_covered: bool = false,
};

pub const Envelope = struct {
    public_key: [Ed25519.PublicKey.encoded_length]u8,
    signature: [Ed25519.Signature.encoded_length]u8,
    jws_compact: []u8,

    pub fn deinit(self: *Envelope, allocator: std.mem.Allocator) void {
        if (self.jws_compact.len > 0) allocator.free(self.jws_compact);
    }

    /// Transfer JWS ownership to the caller and zero the field. After this,
    /// `deinit` is a no-op. Use when only the JWS string is needed and the
    /// stack-resident `public_key` / `signature` can be dropped.
    pub fn intoJws(self: *Envelope) []u8 {
        const jws = self.jws_compact;
        self.jws_compact = "";
        return jws;
    }
};

pub const VerifyResult = struct {
    claims: Claims,
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
};

pub const VerifyError = error{
    MalformedJws,
    InvalidBase64,
    InvalidJson,
    UnsupportedAlgorithm,
    MissingPublicKey,
    SignatureMismatch,
    OutOfMemory,
};

/// SHA-256 of the public key bytes, lowercase hex. The same form the proof
/// ledger uses for content addressing.
pub fn keyFingerprint(public_key: [Ed25519.PublicKey.encoded_length]u8) [64]u8 {
    var digest: [32]u8 = undefined;
    Sha256.hash(&public_key, &digest, .{});
    return std.fmt.bytesToHex(digest, .lower);
}

/// Build a deterministic keypair from a 32-byte seed. Tests use a fixed seed
/// so signing output is reproducible across runs.
pub fn keyPairFromSeed(seed: [Ed25519.KeyPair.seed_length]u8) !Ed25519.KeyPair {
    return Ed25519.KeyPair.generateDeterministic(seed);
}

pub fn sign(
    allocator: std.mem.Allocator,
    claims: Claims,
    key_pair: Ed25519.KeyPair,
) SignError!Envelope {
    try validateHexField(claims.contract_sha256);
    try validateHexField(claims.bytecode_sha256);
    try validateHexField(claims.policy_sha256);
    try validateHexField(claims.capability_hash);
    try validateHexField(claims.runtime_policy_sha256);

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

    const signature = key_pair.sign(signing_input, null) catch unreachable;
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

fn validateHexField(field: []const u8) SignError!void {
    if (field.len != 64) return error.InvalidHexLength;
}

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
        "{{\"alg\":\"EdDSA\",\"typ\":\"zigttp-attest+jws\",\"kid\":\"{s}\",\"jwk\":{{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"x\":\"{s}\"}}}}",
        .{ fingerprint, pubkey_b64 },
    );
}

/// Single source of truth for JSON keys on the wire. Writer and parser both
/// reference these so a rename can't drift between encode and decode.
const wire_key = struct {
    const v = "v";
    const contract_sha256 = "contractSha256";
    const bytecode_sha256 = "bytecodeSha256";
    const policy_sha256 = "policySha256";
    const capability_hash = "capabilityHash";
    const runtime_policy_sha256 = "runtimePolicySha256";
    const compiler_version = "compilerVersion";
    const signed_at = "signedAt";
    const property_summary = "propertySummary";
    const routes_count = "routesCount";
    const durable_workflow_proof_level = "durableWorkflowProofLevel";
    const durable_workflow_retry_safe = "durableWorkflowRetrySafe";
    const durable_workflow_idempotent = "durableWorkflowIdempotent";
    const durable_workflow_fault_covered = "durableWorkflowFaultCovered";
};

fn buildPayloadJson(allocator: std.mem.Allocator, claims: Claims) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "{{\"" ++ wire_key.v ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.contract_sha256 ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.bytecode_sha256 ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.policy_sha256 ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.capability_hash ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.runtime_policy_sha256 ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.compiler_version ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.signed_at ++ "\":{d}," ++
            "\"" ++ wire_key.property_summary ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.routes_count ++ "\":{d}," ++
            "\"" ++ wire_key.durable_workflow_proof_level ++ "\":\"{s}\"," ++
            "\"" ++ wire_key.durable_workflow_retry_safe ++ "\":{}," ++
            "\"" ++ wire_key.durable_workflow_idempotent ++ "\":{}," ++
            "\"" ++ wire_key.durable_workflow_fault_covered ++ "\":{}}}",
        .{
            version_tag,
            claims.contract_sha256,
            claims.bytecode_sha256,
            claims.policy_sha256,
            claims.capability_hash,
            claims.runtime_policy_sha256,
            claims.compiler_version,
            claims.signed_at_unix,
            claims.property_summary,
            claims.routes_count,
            claims.durable_workflow_proof_level,
            claims.durable_workflow_retry_safe,
            claims.durable_workflow_idempotent,
            claims.durable_workflow_fault_covered,
        },
    );
}

const ParseHeaderError = error{
    InvalidJson,
    UnsupportedAlgorithm,
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

fn parseClaims(allocator: std.mem.Allocator, payload_bytes: []const u8) !Claims {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload_bytes, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidJson;
    const obj = parsed.value.object;

    return .{
        .contract_sha256 = try dupString(allocator, obj, wire_key.contract_sha256),
        .bytecode_sha256 = try dupString(allocator, obj, wire_key.bytecode_sha256),
        .policy_sha256 = try dupString(allocator, obj, wire_key.policy_sha256),
        .capability_hash = try dupString(allocator, obj, wire_key.capability_hash),
        .runtime_policy_sha256 = try dupStringDefault(
            allocator,
            obj,
            wire_key.runtime_policy_sha256,
            unpinned_runtime_policy_sha256,
        ),
        .compiler_version = try dupString(allocator, obj, wire_key.compiler_version),
        .property_summary = try dupString(allocator, obj, wire_key.property_summary),
        .signed_at_unix = try readI64(obj, wire_key.signed_at),
        .routes_count = try readU64(obj, wire_key.routes_count),
        .durable_workflow_proof_level = try dupStringDefault(allocator, obj, wire_key.durable_workflow_proof_level, "none"),
        .durable_workflow_retry_safe = try readBoolDefault(obj, wire_key.durable_workflow_retry_safe, false),
        .durable_workflow_idempotent = try readBoolDefault(obj, wire_key.durable_workflow_idempotent, false),
        .durable_workflow_fault_covered = try readBoolDefault(obj, wire_key.durable_workflow_fault_covered, false),
    };
}

fn dupString(allocator: std.mem.Allocator, obj: std.json.ObjectMap, key: []const u8) ![]const u8 {
    const val = obj.get(key) orelse return error.InvalidJson;
    if (val != .string) return error.InvalidJson;
    return try allocator.dupe(u8, val.string);
}

fn dupStringDefault(allocator: std.mem.Allocator, obj: std.json.ObjectMap, key: []const u8, default: []const u8) ![]const u8 {
    const val = obj.get(key) orelse return try allocator.dupe(u8, default);
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

fn readBoolDefault(obj: std.json.ObjectMap, key: []const u8, default: bool) !bool {
    const val = obj.get(key) orelse return default;
    if (val != .bool) return error.InvalidJson;
    return val.bool;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const test_seed: [Ed25519.KeyPair.seed_length]u8 = .{
    0x9d, 0x61, 0xb1, 0x9d, 0xef, 0xfd, 0x5a, 0x60,
    0xba, 0x84, 0x4a, 0xf4, 0x92, 0xec, 0x2c, 0xc4,
    0x44, 0x49, 0xc5, 0x69, 0x7b, 0x32, 0x69, 0x19,
    0x70, 0x3b, 0xac, 0x03, 0x1c, 0xae, 0x7f, 0x60,
};

fn testClaims() Claims {
    return .{
        .contract_sha256 = "a" ** 64,
        .bytecode_sha256 = "b" ** 64,
        .policy_sha256 = "c" ** 64,
        .capability_hash = "d" ** 64,
        .runtime_policy_sha256 = "e" ** 64,
        .compiler_version = "0.0.0-test",
        .signed_at_unix = 1_700_000_000,
        .property_summary = "pure,read_only,injection_safe",
        .routes_count = 3,
        .durable_workflow_proof_level = "complete",
        .durable_workflow_retry_safe = true,
        .durable_workflow_idempotent = true,
        .durable_workflow_fault_covered = true,
    };
}

test "sign then verify recovers the same claims" {
    const allocator = std.testing.allocator;
    const kp = try keyPairFromSeed(test_seed);
    var env = try sign(allocator, testClaims(), kp);
    defer env.deinit(allocator);

    var result = try verify(allocator, env.jws_compact);
    defer result.deinit();

    try std.testing.expectEqualStrings("a" ** 64, result.claims.contract_sha256);
    try std.testing.expectEqualStrings("b" ** 64, result.claims.bytecode_sha256);
    try std.testing.expectEqualStrings("c" ** 64, result.claims.policy_sha256);
    try std.testing.expectEqualStrings("d" ** 64, result.claims.capability_hash);
    try std.testing.expectEqualStrings("e" ** 64, result.claims.runtime_policy_sha256);
    try std.testing.expectEqualStrings("0.0.0-test", result.claims.compiler_version);
    try std.testing.expectEqualStrings("pure,read_only,injection_safe", result.claims.property_summary);
    try std.testing.expectEqual(@as(i64, 1_700_000_000), result.claims.signed_at_unix);
    try std.testing.expectEqual(@as(u64, 3), result.claims.routes_count);
    try std.testing.expectEqualStrings("complete", result.claims.durable_workflow_proof_level);
    try std.testing.expect(result.claims.durable_workflow_retry_safe);
    try std.testing.expect(result.claims.durable_workflow_idempotent);
    try std.testing.expect(result.claims.durable_workflow_fault_covered);
    try std.testing.expectEqualSlices(u8, &kp.public_key.bytes, &result.public_key);
}

fn signPayloadJsonForTest(
    allocator: std.mem.Allocator,
    payload_json: []const u8,
    key_pair: Ed25519.KeyPair,
) ![]u8 {
    const public_key_bytes = key_pair.public_key.bytes;
    const pubkey_b64 = try encodeBase64Owned(allocator, &public_key_bytes);
    defer allocator.free(pubkey_b64);
    const header_json = try buildHeaderJson(
        allocator,
        keyFingerprint(public_key_bytes),
        pubkey_b64,
    );
    defer allocator.free(header_json);
    const header_b64 = try encodeBase64Owned(allocator, header_json);
    defer allocator.free(header_b64);
    const payload_b64 = try encodeBase64Owned(allocator, payload_json);
    defer allocator.free(payload_b64);
    const signing_input = try joinDot(allocator, header_b64, payload_b64);
    defer allocator.free(signing_input);
    const signature = key_pair.sign(signing_input, null) catch unreachable;
    const signature_bytes = signature.toBytes();
    const signature_b64 = try encodeBase64Owned(allocator, &signature_bytes);
    defer allocator.free(signature_b64);
    return joinDot(allocator, signing_input, signature_b64);
}

test "verify defaults a missing runtime policy claim to unpinned" {
    const allocator = std.testing.allocator;
    const payload =
        "{\"v\":\"zigttp-attest-v1\"," ++
        "\"contractSha256\":\"" ++ "a" ** 64 ++ "\"," ++
        "\"bytecodeSha256\":\"" ++ "b" ** 64 ++ "\"," ++
        "\"policySha256\":\"" ++ "c" ** 64 ++ "\"," ++
        "\"capabilityHash\":\"" ++ "d" ** 64 ++ "\"," ++
        "\"compilerVersion\":\"0.0.0-test\"," ++
        "\"signedAt\":1700000000," ++
        "\"propertySummary\":\"pure\"," ++
        "\"routesCount\":1}";
    const key_pair = try keyPairFromSeed(test_seed);
    const jws = try signPayloadJsonForTest(allocator, payload, key_pair);
    defer allocator.free(jws);

    var result = try verify(allocator, jws);
    defer result.deinit();

    try std.testing.expectEqualStrings(
        unpinned_runtime_policy_sha256,
        result.claims.runtime_policy_sha256,
    );
}

test "parseClaims defaults missing durable workflow fields" {
    const allocator = std.testing.allocator;
    const payload =
        "{\"v\":\"zigttp-attest-v1\"," ++
        "\"contractSha256\":\"" ++ "a" ** 64 ++ "\"," ++
        "\"bytecodeSha256\":\"" ++ "b" ** 64 ++ "\"," ++
        "\"policySha256\":\"" ++ "c" ** 64 ++ "\"," ++
        "\"capabilityHash\":\"" ++ "d" ** 64 ++ "\"," ++
        "\"compilerVersion\":\"0.0.0-test\"," ++
        "\"signedAt\":1700000000," ++
        "\"propertySummary\":\"pure\"," ++
        "\"routesCount\":1}";

    const claims = try parseClaims(allocator, payload);
    defer {
        allocator.free(claims.contract_sha256);
        allocator.free(claims.bytecode_sha256);
        allocator.free(claims.policy_sha256);
        allocator.free(claims.capability_hash);
        allocator.free(claims.runtime_policy_sha256);
        allocator.free(claims.compiler_version);
        allocator.free(claims.property_summary);
        allocator.free(claims.durable_workflow_proof_level);
    }

    try std.testing.expectEqualStrings("none", claims.durable_workflow_proof_level);
    try std.testing.expectEqualStrings(unpinned_runtime_policy_sha256, claims.runtime_policy_sha256);
    try std.testing.expect(!claims.durable_workflow_retry_safe);
    try std.testing.expect(!claims.durable_workflow_idempotent);
    try std.testing.expect(!claims.durable_workflow_fault_covered);
}

test "signing is deterministic for a fixed seed" {
    const allocator = std.testing.allocator;
    const kp = try keyPairFromSeed(test_seed);

    var first = try sign(allocator, testClaims(), kp);
    defer first.deinit(allocator);
    var second = try sign(allocator, testClaims(), kp);
    defer second.deinit(allocator);

    try std.testing.expectEqualStrings(first.jws_compact, second.jws_compact);
}

test "flipping a payload byte invalidates the signature" {
    const allocator = std.testing.allocator;
    const kp = try keyPairFromSeed(test_seed);
    var env = try sign(allocator, testClaims(), kp);
    defer env.deinit(allocator);

    const parts = splitThree(env.jws_compact).?;
    const payload_start = parts.header.len + 1;
    const tampered = try allocator.dupe(u8, env.jws_compact);
    defer allocator.free(tampered);
    tampered[payload_start] ^= 0x01;

    try std.testing.expectError(error.SignatureMismatch, verify(allocator, tampered));
}

test "swapping the embedded public key fails verification" {
    const allocator = std.testing.allocator;
    const kp = try keyPairFromSeed(test_seed);
    var env = try sign(allocator, testClaims(), kp);
    defer env.deinit(allocator);

    var alt_seed = test_seed;
    alt_seed[0] ^= 0xFF;
    const alt_kp = try keyPairFromSeed(alt_seed);

    const parts = splitThree(env.jws_compact).?;
    const original_header_bytes = try decodeBase64(allocator, parts.header);
    defer allocator.free(original_header_bytes);

    const alt_pubkey_b64 = try encodeBase64Owned(allocator, &alt_kp.public_key.bytes);
    defer allocator.free(alt_pubkey_b64);
    const tampered_header = try buildHeaderJson(
        allocator,
        keyFingerprint(alt_kp.public_key.bytes),
        alt_pubkey_b64,
    );
    defer allocator.free(tampered_header);
    const tampered_header_b64 = try encodeBase64Owned(allocator, tampered_header);
    defer allocator.free(tampered_header_b64);

    const head_payload = try joinDot(allocator, tampered_header_b64, parts.payload);
    defer allocator.free(head_payload);
    const forged = try joinDot(allocator, head_payload, parts.signature);
    defer allocator.free(forged);

    try std.testing.expectError(error.SignatureMismatch, verify(allocator, forged));
}

test "keyFingerprint is sha256 of the public key, lowercase hex" {
    const kp = try keyPairFromSeed(test_seed);
    const fp = keyFingerprint(kp.public_key.bytes);

    var digest: [32]u8 = undefined;
    Sha256.hash(&kp.public_key.bytes, &digest, .{});
    const expected = std.fmt.bytesToHex(digest, .lower);
    try std.testing.expectEqualSlices(u8, &expected, &fp);
}

test "rejects claims with a non-64-char hex field" {
    const allocator = std.testing.allocator;
    const kp = try keyPairFromSeed(test_seed);
    var bad = testClaims();
    bad.contract_sha256 = "tooShort";
    try std.testing.expectError(error.InvalidHexLength, sign(allocator, bad, kp));
}

test "malformed JWS string is rejected before any crypto runs" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(error.MalformedJws, verify(allocator, "no-dots-here"));
    try std.testing.expectError(error.MalformedJws, verify(allocator, "only.one"));
    try std.testing.expectError(error.MalformedJws, verify(allocator, "too.many.dots.here"));
}

test "splitThree returns null on wrong dot count" {
    try std.testing.expectEqual(@as(?JwsParts, null), splitThree(""));
    try std.testing.expectEqual(@as(?JwsParts, null), splitThree("abc"));
    try std.testing.expectEqual(@as(?JwsParts, null), splitThree("a.b"));
    try std.testing.expectEqual(@as(?JwsParts, null), splitThree("a.b.c.d"));
    const ok = splitThree("aa.bb.cc").?;
    try std.testing.expectEqualStrings("aa", ok.header);
    try std.testing.expectEqualStrings("bb", ok.payload);
    try std.testing.expectEqualStrings("cc", ok.signature);
}
