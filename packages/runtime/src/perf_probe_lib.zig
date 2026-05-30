//! Perf-receipt probe library: the runtime-backed implementation of the PI
//! `perf_probe` seam (Slice H wiring of the expert-strategy §5 roadmap).
//!
//! The PI agent host registers `recordPerfReceipt` via a function pointer at
//! startup (see `dev_cli.zig`, next to the witness-replay injection). PI
//! cannot import this directly: the probe needs the JS engine
//! (`benchmark.runHandlerCorpusFromSource`) and the ledger pulls in the
//! deploy stack, and the runtime binaries consume PI, so a direct dependency
//! would invert the build graph.
//!
//! On each applied edit this:
//!   1. runs a fixed-budget latency probe over the post-apply handler,
//!   2. hashes the handler bytes (binds the receipt to this exact source),
//!   3. signs the latency claims with the persistent attest keypair, and
//!   4. appends a signed `kind=perf` row to `.zigttp/proofs.jsonl`.
//!
//! Everything here is best-effort: a probe that cannot load the handler, a
//! missing `$HOME`, or an unwritable ledger must never abort the apply. A
//! handler that faults on the first call (e.g. it dereferences `req`) yields
//! `sample_count == 0`; the row is still written so an audit can prove no
//! receipt was silently suppressed.

const std = @import("std");
const zq = @import("zigts");
const benchmark = @import("benchmark.zig");
const proof_ledger = @import("proof_ledger.zig");
const identity = @import("attest/identity.zig");
const review = @import("zigttp_proof_review").review;

const perf_receipt = zq.perf_receipt;
const Sha256 = std.crypto.hash.sha2.Sha256;

/// Wall-clock budget for the apply-time probe. Kept tight so the receipt
/// does not add perceptible latency to an interactive `apply_edit`; the
/// probe runs as many iterations as fit and reports what it gathered.
/// Tunable — measure on the example corpus before changing.
const probe_budget_ns: u64 = 150 * std.time.ns_per_ms;

const Ed25519 = std.crypto.sign.Ed25519;

pub fn recordPerfReceipt(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    after_bytes: []const u8,
    applied_at_unix_ms: i64,
) anyerror!void {
    // No persistent key, no receipt — but the apply already succeeded, so
    // swallow the error rather than failing the edit.
    const signer = identity.loadOrCreate(allocator) catch return;
    return recordPerfReceiptWithKey(
        allocator,
        handler_path,
        after_bytes,
        applied_at_unix_ms,
        signer.key_pair,
    );
}

/// Key-injected core. The public entry loads the persistent attest keypair;
/// tests pass a deterministic key so the receipt can be verified without
/// touching `$HOME`.
pub fn recordPerfReceiptWithKey(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    after_bytes: []const u8,
    applied_at_unix_ms: i64,
    key_pair: Ed25519.KeyPair,
) anyerror!void {
    // 1. Latency probe. A load/exec failure surfaces as zeroed stats
    // (sample_count == 0), which we still record as a skipped-probe row.
    const stats = benchmark.runHandlerCorpusFromSource(
        after_bytes,
        handler_path,
        probe_budget_ns,
        allocator,
    ) catch benchmark.HandlerCorpusStats{};

    // 2. Bind the receipt to the exact handler bytes.
    var digest: [Sha256.digest_length]u8 = undefined;
    Sha256.hash(after_bytes, &digest, .{});
    const handler_hash = std.fmt.bytesToHex(digest, .lower); // [64]u8

    // 3. Sign the claims with the supplied identity.
    const claims: perf_receipt.PerfClaims = .{
        .handler_hash = &handler_hash,
        .p50_us = stats.p50_us,
        .p99_us = stats.p99_us,
        .alloc_bytes = stats.alloc_bytes,
        .sample_count = stats.sample_count,
        .signed_at_unix = @divTrunc(applied_at_unix_ms, std.time.ms_per_s),
    };
    var envelope = perf_receipt.sign(allocator, claims, key_pair) catch return;
    defer envelope.deinit(allocator);

    // 4. Append the signed row. Facts borrow the hash/jws; we do not call
    // PerfPayload.deinit (those buffers are owned by the stack array and the
    // envelope respectively).
    var facts = try minimalFacts(allocator, &handler_hash);
    defer facts.deinit(allocator);

    const payload: proof_ledger.PerfPayload = .{
        .handler_hash = &handler_hash,
        .p50_us = stats.p50_us,
        .p99_us = stats.p99_us,
        .alloc_bytes = stats.alloc_bytes,
        .sample_count = stats.sample_count,
        .sig = envelope.jws_compact,
    };

    try proof_ledger.appendEvent(allocator, .{
        .kind = .perf,
        .facts = &facts,
        .handler_path = handler_path,
        .now_unix_ms = applied_at_unix_ms,
        .perf = &payload,
    });
}

/// A `kind=perf` row carries its evidence in the `perf` object; the `facts`
/// object only needs a `contract_sha` so `zigttp proofs` can key the row.
/// All list fields are allocated empty so `ReviewFacts.deinit` frees them
/// uniformly.
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
    try testing.expectEqual(@as(usize, 0), facts.env_keys.len);
    try testing.expectEqual(@as(usize, 0), facts.routes.len);
    try testing.expectEqual(@as(usize, 0), facts.declared_specs.len);
}

test "recordPerfReceiptWithKey writes a verifiable signed perf row" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try proof_ledger.chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var seed: [Ed25519.KeyPair.seed_length]u8 = undefined;
    @memset(&seed, 0x42);
    const key_pair = try Ed25519.KeyPair.generateDeterministic(seed);

    const handler =
        \\function handler(req) {
        \\  return Response.json({ ok: true });
        \\}
    ;

    // page_allocator for the record call: the probe's Runtime intentionally
    // skips deinit (matches the bench harness), so testing.allocator would
    // flag the unfreed pages as leaks. readEvents/verify below use the
    // leak-checked testing.allocator.
    try recordPerfReceiptWithKey(
        std.heap.page_allocator,
        "handler.ts",
        handler,
        1_700_000_002_000,
        key_pair,
    );

    const events = try proof_ledger.readEvents(testing.allocator);
    defer proof_ledger.freeEvents(testing.allocator, events);

    try testing.expectEqual(@as(usize, 1), events.len);
    try testing.expectEqual(proof_ledger.EventKind.perf, events[0].kind);
    try testing.expect(events[0].perf != null);

    // The receipt binds to the exact handler bytes.
    var digest: [Sha256.digest_length]u8 = undefined;
    Sha256.hash(handler, &digest, .{});
    const expected_hash = std.fmt.bytesToHex(digest, .lower);
    try testing.expectEqualStrings(&expected_hash, events[0].perf.?.handler_hash);

    // The embedded JWS verifies and its claims agree with the row.
    var verified = try perf_receipt.verify(testing.allocator, events[0].perf.?.sig);
    defer verified.deinit();
    try testing.expectEqualStrings(&expected_hash, verified.claims.handler_hash);
    try testing.expectEqual(events[0].perf.?.p50_us, verified.claims.p50_us);
    try testing.expectEqual(events[0].perf.?.sample_count, verified.claims.sample_count);
}
