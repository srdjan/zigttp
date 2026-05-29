//! Equivalence-receipt probe library: the runtime-backed implementation of
//! the PI `equivalence_probe` seam (proof-carrying changes).
//!
//! The PI agent host registers `recordEquivalenceReceipt` via a function
//! pointer at startup (see `dev_cli.zig`, next to the perf-probe and
//! witness-replay injections). PI cannot import this directly: extracting a
//! contract needs the compile pipeline (`zigts_cli.precompile`), signing
//! needs the persistent attest identity, and the ledger pulls in the deploy
//! stack - and the runtime binaries consume PI, so a direct dependency would
//! invert the build graph.
//!
//! On each applied edit this:
//!   1. compiles the pre- and post-edit sources to contracts,
//!   2. diffs them and derives the behavioral verdict (the same
//!      `contract_diff` classification `prove-behavior` reports, upgraded so
//!      a changed/removed response path reads as breaking),
//!   3. signs the verdict with the persistent attest keypair, and
//!   4. appends a signed `kind=equivalence` row to `.zigttp/proofs.jsonl`.
//!
//! Best-effort: a source that fails to compile, a missing `$HOME`, or an
//! unwritable ledger must never abort the apply.

const std = @import("std");
const zq = @import("zigts");
const zigts_cli = @import("zigts_cli");
const proof_ledger = @import("proof_ledger.zig");
const identity = @import("attest/identity.zig");
const review = @import("zigttp_deploy").review;

const precompile = zigts_cli.precompile;
const contract_diff = zq.contract_diff;
const equivalence_receipt = zq.equivalence_receipt;
const Sha256 = std.crypto.hash.sha2.Sha256;
const Ed25519 = std.crypto.sign.Ed25519;

pub fn recordEquivalenceReceipt(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    before_bytes: []const u8,
    after_bytes: []const u8,
    applied_at_unix_ms: i64,
) anyerror!void {
    const signer = identity.loadOrCreate(allocator) catch return;
    return recordEquivalenceReceiptWithKey(
        allocator,
        handler_path,
        before_bytes,
        after_bytes,
        applied_at_unix_ms,
        signer.key_pair,
    );
}

/// Key-injected core. The public entry loads the persistent attest keypair;
/// tests pass a deterministic key so the receipt can be verified without
/// touching `$HOME`.
pub fn recordEquivalenceReceiptWithKey(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    before_bytes: []const u8,
    after_bytes: []const u8,
    applied_at_unix_ms: i64,
    key_pair: Ed25519.KeyPair,
) anyerror!void {
    var before_result = precompile.runCheckOnlyFromSource(allocator, before_bytes, handler_path, null, true, null, false) catch return;
    defer before_result.deinit(allocator);
    var after_result = precompile.runCheckOnlyFromSource(allocator, after_bytes, handler_path, null, true, null, false) catch return;
    defer after_result.deinit(allocator);

    const before_contract = if (before_result.contract) |*c| c else return;
    const after_contract = if (after_result.contract) |*c| c else return;

    return signAndAppend(allocator, handler_path, before_contract, after_contract, applied_at_unix_ms, key_pair);
}

/// Sign and append a `kind=equivalence` row from already-compiled contracts,
/// loading the persistent attest key. Callers that have just compiled the two
/// versions (e.g. `proofs gate`) use this to avoid recompiling them. Same
/// best-effort contract as the source-bytes entry points.
pub fn recordEquivalenceReceiptFromContracts(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    before_contract: *const zq.HandlerContract,
    after_contract: *const zq.HandlerContract,
    applied_at_unix_ms: i64,
) anyerror!void {
    const signer = identity.loadOrCreate(allocator) catch return;
    return signAndAppend(allocator, handler_path, before_contract, after_contract, applied_at_unix_ms, signer.key_pair);
}

fn signAndAppend(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    before_contract: *const zq.HandlerContract,
    after_contract: *const zq.HandlerContract,
    applied_at_unix_ms: i64,
    key_pair: Ed25519.KeyPair,
) anyerror!void {
    var diff = contract_diff.diffContracts(allocator, before_contract, after_contract) catch return;
    defer diff.deinit(allocator);

    const classification = diff.behavioralVerdict();
    const scope = contract_diff.claimScope(after_contract.properties);

    const before_hash = contractHashHex(allocator, before_contract) catch return;
    const after_hash = contractHashHex(allocator, after_contract) catch return;

    var preserved: u32 = 0;
    var response_changed: u32 = 0;
    var added: u32 = 0;
    var removed: u32 = 0;
    if (diff.behavior_diff) |bd| {
        preserved = bd.preserved;
        response_changed = bd.response_changed;
        added = bd.added;
        removed = bd.removed;
    }

    const verdict: equivalence_receipt.Verdict = .{
        .handler_path = handler_path,
        .before_contract_hash = &before_hash,
        .after_contract_hash = &after_hash,
        .classification = classification.toString(),
        .claim_scope = scope,
        .preserved = preserved,
        .response_changed = response_changed,
        .added = added,
        .removed = removed,
        .laws_fired = diff.laws_used.items,
    };

    var envelope = equivalence_receipt.sign(allocator, verdict, key_pair) catch return;
    defer envelope.deinit(allocator);

    var facts = minimalFacts(allocator, &after_hash) catch return;
    defer facts.deinit(allocator);

    const payload: proof_ledger.EquivalencePayload = .{
        .before_contract_hash = &before_hash,
        .after_contract_hash = &after_hash,
        .classification = classification.toString(),
        .laws_fired = diff.laws_used.items,
        .preserved = preserved,
        .response_changed = response_changed,
        .added = added,
        .removed = removed,
        .claim_scope = scope,
        .sig = envelope.compact,
    };

    try proof_ledger.appendEvent(allocator, .{
        .kind = .equivalence,
        .facts = &facts,
        .handler_path = handler_path,
        .now_unix_ms = applied_at_unix_ms,
        .equivalence = &payload,
    });
}

fn hashHex(bytes: []const u8) [64]u8 {
    var digest: [Sha256.digest_length]u8 = undefined;
    Sha256.hash(bytes, &digest, .{});
    return std.fmt.bytesToHex(digest, .lower);
}

fn contractHashHex(allocator: std.mem.Allocator, contract: *const zq.HandlerContract) ![64]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try zq.writeContractJson(contract, &aw.writer);
    return hashHex(aw.writer.buffered());
}

/// A `kind=equivalence` row carries its evidence in the `equivalence` object;
/// `facts` only needs a `contract_sha` so `zigttp proofs` can key the row.
fn minimalFacts(allocator: std.mem.Allocator, sha: []const u8) !review.ReviewFacts {
    const contract_sha = try allocator.dupe(u8, sha);
    errdefer allocator.free(contract_sha);
    return .{
        .contract_sha = contract_sha,
        .proof_level = .none,
        .env_keys = try allocator.alloc([]const u8, 0),
        .egress_hosts = try allocator.alloc([]const u8, 0),
        .cache_namespaces = try allocator.alloc([]const u8, 0),
        .routes = try allocator.alloc(review.Route, 0),
        .capabilities = try allocator.alloc([]const u8, 0),
        .properties = .{},
        .declared_specs = try allocator.alloc(review.SpecState, 0),
    };
}

const testing = std.testing;

test "minimalFacts round-trips contract_sha and deinits cleanly" {
    var facts = try minimalFacts(testing.allocator, "a" ** 64);
    defer facts.deinit(testing.allocator);
    try testing.expectEqualStrings("a" ** 64, facts.contract_sha);
}

test "hashHex is stable and matches the known SHA-256 of the input" {
    const a = hashHex("hello");
    const b = hashHex("hello");
    try testing.expectEqualStrings(&a, &b);
    try testing.expectEqualStrings(
        "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
        &a,
    );
}

test "contractHashHex hashes the serialized contract JSON" {
    const path = try testing.allocator.dupe(u8, "handler.ts");
    var contract = zq.HandlerContract{
        .handler = .{ .path = path, .line = 1, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = .{ .queries = .empty, .dynamic = false },
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
        },
        .scope = .{
            .used = false,
            .names = .empty,
        },
        .api = .{
            .schemas = .empty,
            .requests = .{ .schema_refs = .empty, .dynamic = false },
            .auth = .{ .bearer = false, .jwt = false },
            .routes = .empty,
            .schemas_dynamic = false,
            .routes_dynamic = false,
        },
        .verification = null,
        .aot = null,
    };
    defer contract.deinit(testing.allocator);

    const got = try contractHashHex(testing.allocator, &contract);

    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try zq.writeContractJson(&contract, &aw.writer);
    const expected = hashHex(aw.writer.buffered());

    try testing.expectEqualStrings(&expected, &got);
}

// behavioralVerdict and claimScope now live in `contract_diff` (shared with
// the `prove-behavior` CLI); their unit tests live there too.
//
// The full compile -> diff -> sign -> ledger chain is exercised end-to-end by
// the `zigttp prove-behavior` CLI (shared verdict computation) and covered
// per layer: signing in `equivalence_receipt.zig`, ledger round-trip in
// `proof_ledger.zig`. Spinning the whole JS compile pipeline inside a unit
// test under a non-leak-checked allocator is deliberately avoided here.
