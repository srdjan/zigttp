//! Well-known proof receipt document.
//!
//! Precomputes the `GET /.well-known/zttp-attest` response body once at
//! startup. The doc carries the same JWS the runtime emits in the
//! `Zttp-Attest` response header plus the full contract surface and the
//! public-key JWK, so a security scanner or registry crawler can validate
//! the attestation without issuing a request through every handler route.
//!
//! Body shape:
//! ```
//! { "v": "zttp-attest-v1",
//!   "attest": "<compact JWS>",
//!   "contract": { ...verbatim contract.json... },
//!   "publicKey": { "kty": "OKP", "crv": "Ed25519",
//!                  "x": "<base64url pubkey>", "kid": "<sha256 fingerprint>" } }
//! ```
//!
//! Content-addressed: the ETag is SHA-256 of the body. Identical builds
//! produce identical bodies, identical ETags. Verifiers and CDNs may cache
//! aggressively.

const std = @import("std");
const envelope = @import("envelope.zig");
const Ed25519 = std.crypto.sign.Ed25519;
const Sha256 = std.crypto.hash.sha2.Sha256;
const b64 = std.base64.url_safe_no_pad;

pub const route_path: []const u8 = "/.well-known/zttp-attest";
pub const content_type: []const u8 = "application/json";
/// One hour. The doc is content-addressed via ETag, so caches re-validate
/// cheaply when a fresh build changes the body.
pub const cache_max_age_seconds: u32 = 3600;

pub const Doc = struct {
    body: []const u8,
    etag_hex: [64]u8,

    pub fn deinit(self: *Doc, allocator: std.mem.Allocator) void {
        allocator.free(self.body);
    }
};

/// True for an unpinned (all-zero or empty) hex hash claim, mirroring the
/// skip-on-zero semantics used for the policy/bytecode hashes.
fn isUnpinnedHex(hex: []const u8) bool {
    if (hex.len == 0) return true;
    for (hex) |c| if (c != '0') return false;
    return true;
}

pub fn build(
    allocator: std.mem.Allocator,
    contract_json: []const u8,
    attestation_jws: []const u8,
    public_key: [Ed25519.PublicKey.encoded_length]u8,
    /// The `contractSha256` claim from the signed JWS. The contract embedded in
    /// the doc is served unsigned, so it MUST hash to this value; otherwise an
    /// operator or MITM that can rewrite the well-known body (but not the key)
    /// could advertise a contract that disagrees with the signature. Fail closed
    /// on mismatch. Pass an all-zero/empty hex to skip (unpinned).
    expected_contract_sha256: []const u8,
) !Doc {
    if (!isUnpinnedHex(expected_contract_sha256)) {
        var contract_digest: [32]u8 = undefined;
        Sha256.hash(contract_json, &contract_digest, .{});
        const actual_hex = std.fmt.bytesToHex(contract_digest, .lower);
        if (!std.ascii.eqlIgnoreCase(&actual_hex, expected_contract_sha256)) {
            return error.ContractHashMismatch;
        }
    }

    const fingerprint = envelope.keyFingerprint(public_key);

    const pubkey_b64_len = b64.Encoder.calcSize(public_key.len);
    var pubkey_b64_buf: [b64.Encoder.calcSize(Ed25519.PublicKey.encoded_length)]u8 = undefined;
    _ = b64.Encoder.encode(pubkey_b64_buf[0..pubkey_b64_len], &public_key);

    const body = try std.fmt.allocPrint(
        allocator,
        "{{\"v\":\"" ++ envelope.version_tag ++ "\"," ++
            "\"attest\":\"{s}\"," ++
            "\"contract\":{s}," ++
            "\"publicKey\":{{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"x\":\"{s}\",\"kid\":\"{s}\"}}}}",
        .{ attestation_jws, contract_json, pubkey_b64_buf[0..pubkey_b64_len], fingerprint },
    );
    errdefer allocator.free(body);

    var digest: [32]u8 = undefined;
    Sha256.hash(body, &digest, .{});

    return .{
        .body = body,
        .etag_hex = std.fmt.bytesToHex(digest, .lower),
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

const sample_contract = "{\"version\":14,\"routes\":[],\"properties\":{\"pure\":true}}";
const sample_jws = "eyJh.eyJ2.signature";

fn samplePubKey() [32]u8 {
    return .{ 0x9d, 0x61, 0xb1, 0x9d, 0xef, 0xfd, 0x5a, 0x60 } ++ [_]u8{0} ** 24;
}

fn sampleSha() [64]u8 {
    var digest: [32]u8 = undefined;
    Sha256.hash(sample_contract, &digest, .{});
    return std.fmt.bytesToHex(digest, .lower);
}

test "build embeds attest, contract, and public key" {
    var doc = try build(testing.allocator, sample_contract, sample_jws, samplePubKey(), &sampleSha());
    defer doc.deinit(testing.allocator);

    try testing.expect(std.mem.indexOf(u8, doc.body, "\"v\":\"zttp-attest-v1\"") != null);
    try testing.expect(std.mem.indexOf(u8, doc.body, "\"attest\":\"eyJh.eyJ2.signature\"") != null);
    try testing.expect(std.mem.indexOf(u8, doc.body, "\"contract\":{\"version\":14") != null);
    try testing.expect(std.mem.indexOf(u8, doc.body, "\"publicKey\":{") != null);
    try testing.expect(std.mem.indexOf(u8, doc.body, "\"kty\":\"OKP\"") != null);
    try testing.expect(std.mem.indexOf(u8, doc.body, "\"crv\":\"Ed25519\"") != null);
}

test "identical inputs produce byte-identical bodies and ETags" {
    var first = try build(testing.allocator, sample_contract, sample_jws, samplePubKey(), &sampleSha());
    defer first.deinit(testing.allocator);
    var second = try build(testing.allocator, sample_contract, sample_jws, samplePubKey(), &sampleSha());
    defer second.deinit(testing.allocator);

    try testing.expectEqualStrings(first.body, second.body);
    try testing.expectEqualSlices(u8, &first.etag_hex, &second.etag_hex);
}

test "different attest JWS yields different ETag" {
    var first = try build(testing.allocator, sample_contract, sample_jws, samplePubKey(), &sampleSha());
    defer first.deinit(testing.allocator);
    var second = try build(testing.allocator, sample_contract, "different.jws.bytes", samplePubKey(), &sampleSha());
    defer second.deinit(testing.allocator);

    try testing.expect(!std.mem.eql(u8, &first.etag_hex, &second.etag_hex));
}

test "different public key yields different ETag" {
    var alt_key = samplePubKey();
    alt_key[0] ^= 0xFF;

    var first = try build(testing.allocator, sample_contract, sample_jws, samplePubKey(), &sampleSha());
    defer first.deinit(testing.allocator);
    var second = try build(testing.allocator, sample_contract, sample_jws, alt_key, &sampleSha());
    defer second.deinit(testing.allocator);

    try testing.expect(!std.mem.eql(u8, &first.etag_hex, &second.etag_hex));
}

test "ETag is sha256 of body, lowercase hex" {
    var doc = try build(testing.allocator, sample_contract, sample_jws, samplePubKey(), &sampleSha());
    defer doc.deinit(testing.allocator);

    var digest: [32]u8 = undefined;
    Sha256.hash(doc.body, &digest, .{});
    const expected = std.fmt.bytesToHex(digest, .lower);
    try testing.expectEqualSlices(u8, &expected, &doc.etag_hex);
}

test "build fails closed when the contract does not match the signed hash" {
    // A contract that does not hash to the JWS's contractSha256 must be
    // rejected, not served unsigned alongside a signature that disagrees.
    try testing.expectError(error.ContractHashMismatch, build(
        testing.allocator,
        sample_contract,
        sample_jws,
        samplePubKey(),
        &([_]u8{'a'} ** 64), // wrong hash
    ));
    // An all-zero (unpinned) claim skips the binding check.
    var doc = try build(testing.allocator, sample_contract, sample_jws, samplePubKey(), &([_]u8{'0'} ** 64));
    doc.deinit(testing.allocator);
}

test "kid in publicKey equals envelope.keyFingerprint of the embedded x" {
    var doc = try build(testing.allocator, sample_contract, sample_jws, samplePubKey(), &sampleSha());
    defer doc.deinit(testing.allocator);

    const expected_fp = envelope.keyFingerprint(samplePubKey());
    const needle = "\"kid\":\"";
    const start = std.mem.indexOf(u8, doc.body, needle).?;
    const after = start + needle.len;
    try testing.expectEqualSlices(u8, &expected_fp, doc.body[after .. after + 64]);
}
