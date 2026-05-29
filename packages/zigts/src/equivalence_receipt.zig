//! Signed behavioral-equivalence receipt (proof-carrying changes).
//!
//! An equivalence receipt binds the verdict `contract_diff` computes between
//! two handler versions - equivalent, equivalent_modulo_laws, additive, or
//! breaking - to the hashes of both contracts, signed with the operator's
//! persistent Ed25519 identity. A third party can then verify that a given
//! edit was classified behaviorally equivalent without trusting the machine
//! that produced the claim. The signature covers a canonical string of the
//! verdict; the output is a compact JWS, the same envelope shape the perf and
//! attestation paths use.
//!
//! This lives in the zigts layer (like `perf_receipt.zig`) so both the tools
//! CLI and the runtime can sign without inverting the build graph. The
//! `claim_scope` field records how strong the claim is: an effectful handler
//! is signed as `structural`, never as a clean `pure` equivalence.

const std = @import("std");
const Ed25519 = std.crypto.sign.Ed25519;

/// The full verdict carried across the PI -> runtime seam and into the
/// ledger. `sign` covers the verdict-defining fields; `laws_fired` is
/// descriptive and rides in the ledger payload unsigned.
pub const Verdict = struct {
    handler_path: []const u8,
    /// Hex SHA-256 of the before/after contract JSON bytes.
    before_contract_hash: []const u8,
    after_contract_hash: []const u8,
    /// One of: equivalent | equivalent_modulo_laws | additive | breaking.
    classification: []const u8,
    /// One of: pure | deterministic | structural.
    claim_scope: []const u8,
    preserved: u32 = 0,
    response_changed: u32 = 0,
    added: u32 = 0,
    removed: u32 = 0,
    laws_fired: []const []const u8 = &.{},
};

/// Compact-JWS envelope. `compact` is owned and freed by `deinit`.
pub const Envelope = struct {
    compact: []const u8,

    pub fn deinit(self: *Envelope, allocator: std.mem.Allocator) void {
        allocator.free(self.compact);
    }
};

fn buildSigningInput(allocator: std.mem.Allocator, v: Verdict) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "zigttp-equivalence-v1\n{s}\n{s}\n{s}\n{s}\n{d}\n{d}\n{d}\n{d}",
        .{
            v.before_contract_hash,
            v.after_contract_hash,
            v.classification,
            v.claim_scope,
            v.preserved,
            v.response_changed,
            v.added,
            v.removed,
        },
    );
}

pub fn sign(
    allocator: std.mem.Allocator,
    v: Verdict,
    key_pair: Ed25519.KeyPair,
) !Envelope {
    const signing_input = try buildSigningInput(allocator, v);
    defer allocator.free(signing_input);

    var sig = try key_pair.sign(signing_input, null);
    const sig_bytes = sig.toBytes();

    const b64 = std.base64.url_safe_no_pad.Encoder;

    var header_buf: [128]u8 = undefined;
    const header_json = "{\"alg\":\"EdDSA\",\"typ\":\"zigttp-equivalence+jws\"}";
    const header_b64_len = b64.calcSize(header_json.len);
    _ = b64.encode(header_buf[0..header_b64_len], header_json);

    const payload_b64_len = b64.calcSize(signing_input.len);
    const payload_buf = try allocator.alloc(u8, payload_b64_len);
    defer allocator.free(payload_buf);
    _ = b64.encode(payload_buf, signing_input);

    const sig_b64_len = b64.calcSize(sig_bytes.len);
    const sig_buf = try allocator.alloc(u8, sig_b64_len);
    defer allocator.free(sig_buf);
    _ = b64.encode(sig_buf, &sig_bytes);

    const compact = try std.fmt.allocPrint(allocator, "{s}.{s}.{s}", .{
        header_buf[0..header_b64_len],
        payload_buf,
        sig_buf,
    });

    return Envelope{ .compact = compact };
}

test "sign produces a three-part compact jws" {
    const seed = [_]u8{9} ** 32;
    const key_pair = Ed25519.KeyPair.generateDeterministic(seed) catch unreachable;
    var env = try sign(std.testing.allocator, .{
        .handler_path = "src/handler.ts",
        .before_contract_hash = "aa",
        .after_contract_hash = "bb",
        .classification = "equivalent",
        .claim_scope = "pure",
        .preserved = 3,
    }, key_pair);
    defer env.deinit(std.testing.allocator);
    try std.testing.expect(std.mem.count(u8, env.compact, ".") == 2);
}

test "signing input is stable for identical verdicts" {
    const v: Verdict = .{
        .handler_path = "h.ts",
        .before_contract_hash = "x",
        .after_contract_hash = "y",
        .classification = "breaking",
        .claim_scope = "structural",
        .response_changed = 1,
    };
    const a = try buildSigningInput(std.testing.allocator, v);
    defer std.testing.allocator.free(a);
    const b = try buildSigningInput(std.testing.allocator, v);
    defer std.testing.allocator.free(b);
    try std.testing.expectEqualStrings(a, b);
    try std.testing.expect(std.mem.startsWith(u8, a, "zigttp-equivalence-v1\n"));
}
