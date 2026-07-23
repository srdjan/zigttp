//! Signed workflow/hypermedia receipt (`kind=workflow`).
//!
//! A workflow receipt binds the cross-handler hypermedia verdict `linkSystem`
//! computes for a bundle - whether every emitted `resource()` affordance
//! resolves to a real bundle route, and whether those targets expose known
//! responses - to a hash of the bundle's system contract, signed with the
//! operator's persistent Ed25519 identity. A third party can then verify that a
//! given bundle was proven free of dangling HATEOAS links without trusting the
//! machine that produced the claim. The signature covers the canonical bundle
//! hash plus the proof verdict; the local `system_path` is context only. The
//! output is a compact JWS, the same envelope shape the perf, equivalence and
//! attestation paths use.
//!
//! This lives in the zts layer (like `equivalence_receipt.zig`) so both the
//! tools CLI and the runtime can sign without inverting the build graph: the
//! runtime loads the persistent keypair and injects it; this module only signs.

const std = @import("std");
const Ed25519 = std.crypto.sign.Ed25519;

/// The hypermedia verdict for a linked bundle. `sign` covers the bundle hash and
/// proof facts, so a changed proof level or affordance count changes the
/// signature. `system_path` is intentionally not signed because it is local
/// machine context; `system_hash` is the portable bundle identity.
pub const Verdict = struct {
    /// Local manifest path used for operator context. Not part of signing input.
    system_path: []const u8,
    /// Hex SHA-256 of the canonical system-contract JSON (the bundle identity).
    system_hash: []const u8,
    /// One of: complete | partial | none.
    proof_level: []const u8,
    all_affordances_resolved: bool,
    affordance_responses_covered: bool,
    resolved_affordances: u32 = 0,
    dangling_affordances: u32 = 0,
    dynamic_affordances: u32 = 0,
    handler_count: u32 = 0,
    /// The bundle's declared external entry point (system.json's `entry`
    /// field), or null when the manifest declares none. Signed, so a receipt
    /// cannot be replayed against a bundle whose declared entry changed.
    entry_handler: ?[]const u8 = null,
    /// True when `entry_handler` is set and that handler's own route table is
    /// proven POST-only (`HandlerProperties.post_only`). False - not
    /// "unproven" - when no entry is declared: an opt-in fact, not a default
    /// penalty for bundles that never asked for this proof.
    entry_post_only: bool = false,
};

/// Compact-JWS envelope. `compact` is owned and freed by `deinit`.
pub const Envelope = struct {
    compact: []const u8,

    pub fn deinit(self: *Envelope, allocator: std.mem.Allocator) void {
        allocator.free(self.compact);
    }
};

fn boolStr(b: bool) []const u8 {
    return if (b) "true" else "false";
}

fn buildSigningInput(allocator: std.mem.Allocator, v: Verdict) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "zttp-workflow-v1\n{s}\n{s}\n{s}\n{s}\n{d}\n{d}\n{d}\n{d}\n{s}\n{s}",
        .{
            v.system_hash,
            v.proof_level,
            boolStr(v.all_affordances_resolved),
            boolStr(v.affordance_responses_covered),
            v.resolved_affordances,
            v.dangling_affordances,
            v.dynamic_affordances,
            v.handler_count,
            v.entry_handler orelse "",
            boolStr(v.entry_post_only),
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
    const header_json = "{\"alg\":\"EdDSA\",\"typ\":\"zttp-workflow+jws\"}";
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
    const seed = [_]u8{7} ** 32;
    const key_pair = Ed25519.KeyPair.generateDeterministic(seed) catch unreachable;
    var env = try sign(std.testing.allocator, .{
        .system_path = "system.json",
        .system_hash = "ab",
        .proof_level = "complete",
        .all_affordances_resolved = true,
        .affordance_responses_covered = true,
        .resolved_affordances = 2,
    }, key_pair);
    defer env.deinit(std.testing.allocator);
    try std.testing.expect(std.mem.count(u8, env.compact, ".") == 2);
}

test "signing input is stable and a dangling affordance changes it" {
    const base: Verdict = .{
        .system_path = "s.json",
        .system_hash = "x",
        .proof_level = "complete",
        .all_affordances_resolved = true,
        .affordance_responses_covered = true,
        .resolved_affordances = 1,
    };
    const a = try buildSigningInput(std.testing.allocator, base);
    defer std.testing.allocator.free(a);
    const b = try buildSigningInput(std.testing.allocator, base);
    defer std.testing.allocator.free(b);
    try std.testing.expectEqualStrings(a, b);
    try std.testing.expect(std.mem.startsWith(u8, a, "zttp-workflow-v1\n"));

    var dangling = base;
    dangling.all_affordances_resolved = false;
    dangling.dangling_affordances = 1;
    dangling.proof_level = "partial";
    const c = try buildSigningInput(std.testing.allocator, dangling);
    defer std.testing.allocator.free(c);
    try std.testing.expect(!std.mem.eql(u8, a, c));
}

test "signing input ignores local system path and binds bundle hash" {
    const base: Verdict = .{
        .system_path = "/tmp/a/system.json",
        .system_hash = "hash-a",
        .proof_level = "complete",
        .all_affordances_resolved = true,
        .affordance_responses_covered = true,
        .resolved_affordances = 1,
    };
    const a = try buildSigningInput(std.testing.allocator, base);
    defer std.testing.allocator.free(a);

    var moved = base;
    moved.system_path = "/other/machine/system.json";
    const b = try buildSigningInput(std.testing.allocator, moved);
    defer std.testing.allocator.free(b);
    try std.testing.expectEqualStrings(a, b);

    var changed = base;
    changed.system_hash = "hash-b";
    const c = try buildSigningInput(std.testing.allocator, changed);
    defer std.testing.allocator.free(c);
    try std.testing.expect(!std.mem.eql(u8, a, c));
}

test "signing input binds the declared entry and its post-only proof" {
    const base: Verdict = .{
        .system_path = "s.json",
        .system_hash = "x",
        .proof_level = "complete",
        .all_affordances_resolved = true,
        .affordance_responses_covered = true,
        .resolved_affordances = 1,
    };
    const no_entry = try buildSigningInput(std.testing.allocator, base);
    defer std.testing.allocator.free(no_entry);

    var with_entry = base;
    with_entry.entry_handler = "orchestrator";
    with_entry.entry_post_only = true;
    const a = try buildSigningInput(std.testing.allocator, with_entry);
    defer std.testing.allocator.free(a);
    try std.testing.expect(!std.mem.eql(u8, no_entry, a));

    var renamed_entry = with_entry;
    renamed_entry.entry_handler = "gateway";
    const b = try buildSigningInput(std.testing.allocator, renamed_entry);
    defer std.testing.allocator.free(b);
    try std.testing.expect(!std.mem.eql(u8, a, b));

    var not_post_only = with_entry;
    not_post_only.entry_post_only = false;
    const c = try buildSigningInput(std.testing.allocator, not_post_only);
    defer std.testing.allocator.free(c);
    try std.testing.expect(!std.mem.eql(u8, a, c));
}
