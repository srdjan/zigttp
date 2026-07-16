//! Shared proof-receipt signer for build-time and dev-time attestation.

const std = @import("std");
const zigts = @import("zigts");

const envelope = @import("envelope.zig");
const header_strings = @import("header_strings.zig");
const identity = @import("identity.zig");
const proof_ledger = @import("../proof_ledger.zig");

const Ed25519 = std.crypto.sign.Ed25519;

pub const compiler_version_tag: []const u8 = "zigttp-attest-slice1";

/// Produces a compact JWS for build/deploy artifacts. Uses the persistent
/// identity under ~/.zigttp/attest and fails if that identity cannot be
/// loaded safely. Caller owns the returned bytes.
pub fn buildJws(
    allocator: std.mem.Allocator,
    contract_json: []const u8,
    bytecode: []const u8,
    contract: *const zigts.HandlerContract,
    runtime_policy_sha256: []const u8,
) ![]u8 {
    const loaded = identity.loadOrCreate(allocator) catch |err| {
        std.log.err(
            "attest: failed to load identity from ~/.zigttp/attest/keypair.bin: {s}. Inspect the file, fix the permissions (chmod 600), or delete it to mint a fresh key.",
            .{@errorName(err)},
        );
        return err;
    };
    if (loaded.source == .generated) {
        std.log.info("attest: minted persistent identity (fingerprint {s})", .{loaded.fingerprint_hex[0..16]});
    }
    return try buildJwsWithKey(
        allocator,
        contract_json,
        bytecode,
        contract,
        runtime_policy_sha256,
        loaded.key_pair,
    );
}

/// Produces a compact JWS for local dev receipts. Dev servers may run with a
/// stripped environment, so this falls back to a process-local ephemeral key
/// when the persistent identity cannot be loaded.
pub fn buildDevJws(
    allocator: std.mem.Allocator,
    contract_json: []const u8,
    bytecode: []const u8,
    contract: *const zigts.HandlerContract,
) ![]u8 {
    return try buildJwsWithKey(
        allocator,
        contract_json,
        bytecode,
        contract,
        envelope.unpinned_runtime_policy_sha256,
        loadPersistentOrEphemeralKey(allocator),
    );
}

fn buildJwsWithKey(
    allocator: std.mem.Allocator,
    contract_json: []const u8,
    bytecode: []const u8,
    contract: *const zigts.HandlerContract,
    runtime_policy_sha256: []const u8,
    key_pair: Ed25519.KeyPair,
) ![]u8 {
    var contract_sha: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(contract_json, &contract_sha, .{});
    const contract_sha_hex = std.fmt.bytesToHex(contract_sha, .lower);

    var bytecode_sha: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytecode, &bytecode_sha, .{});
    const bytecode_sha_hex = std.fmt.bytesToHex(bytecode_sha, .lower);

    const policy_sha_hex = zigts.rule_registry.policyHash();
    const capability_hash_hex = std.fmt.bytesToHex(contract.capabilities.hash, .lower);

    const props_or_default = contract.properties orelse zigts.handler_contract.HandlerProperties{
        .pure = false,
        .read_only = false,
        .stateless = false,
        .retry_safe = false,
        .deterministic = false,
        .has_egress = false,
    };
    const property_summary = try header_strings.formatProofChips(allocator, props_or_default);
    defer if (property_summary.len > 0) allocator.free(property_summary);

    const claims = envelope.Claims{
        .contract_sha256 = &contract_sha_hex,
        .bytecode_sha256 = &bytecode_sha_hex,
        .policy_sha256 = &policy_sha_hex,
        .capability_hash = &capability_hash_hex,
        .runtime_policy_sha256 = runtime_policy_sha256,
        .compiler_version = compiler_version_tag,
        .signed_at_unix = @divTrunc(proof_ledger.defaultNowMs(), std.time.ms_per_s),
        .property_summary = property_summary,
        .routes_count = @intCast(contract.api.routes.items.len),
        .durable_workflow_proof_level = contract.durable.workflow.proof_level.toString(),
        .durable_workflow_retry_safe = contract.durable.workflow.properties.retry_safe,
        .durable_workflow_idempotent = contract.durable.workflow.properties.idempotent,
        .durable_workflow_fault_covered = contract.durable.workflow.properties.fault_covered,
    };

    var env = try envelope.sign(allocator, claims, key_pair);
    return env.intoJws();
}

fn loadPersistentOrEphemeralKey(allocator: std.mem.Allocator) Ed25519.KeyPair {
    const loaded = identity.loadOrCreate(allocator) catch |err| {
        std.log.warn("attest: persistent identity unavailable ({s}); using ephemeral dev key", .{@errorName(err)});
        return ephemeralKeyPair();
    };
    if (loaded.source == .generated) {
        std.log.info("attest: minted persistent identity (fingerprint {s})", .{loaded.fingerprint_hex[0..16]});
    }
    return loaded.key_pair;
}

fn ephemeralKeyPair() Ed25519.KeyPair {
    var seed: [Ed25519.KeyPair.seed_length]u8 = undefined;
    // A signing key must come from a CSPRNG. The old fallback seeded a
    // non-cryptographic xoshiro PRNG from time^pid - a low-entropy, partially
    // predictable space an attacker could search to forge attestations. This
    // Zig version exposes no portable OS-CSPRNG fallback (no std.posix.getrandom;
    // arc4random_buf is absent on non-glibc Linux), so fail closed rather than
    // mint a weak key - mirroring zigttp:id, which panics rather than seed its
    // CSPRNG from anything but /dev/urandom.
    fillCsprng(&seed) catch @panic("attest: /dev/urandom unavailable; refusing to mint a signing key from a weak seed");
    return envelope.keyPairFromSeed(seed) catch unreachable;
}

fn fillCsprng(buf: *[Ed25519.KeyPair.seed_length]u8) !void {
    const fd = std.c.open("/dev/urandom", .{ .ACCMODE = .RDONLY }, @as(std.c.mode_t, 0));
    if (fd < 0) return error.UrandomOpenFailed;
    defer _ = std.c.close(fd);
    var filled: usize = 0;
    while (filled < buf.len) {
        const n = std.c.read(fd, buf[filled..].ptr, buf.len - filled);
        if (n <= 0) return error.UrandomReadFailed;
        filled += @intCast(n);
    }
}
