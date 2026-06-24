//! Workflow-receipt probe: the runtime-backed signer for the `zigttp link`
//! hypermedia seam.
//!
//! The keyless tools `link` command (`system_build.zig`) computes the bundle's
//! `SystemAnalysis` but cannot sign: signing needs the persistent attest
//! identity, which lives in the runtime layer (and the runtime binaries consume
//! tools, so a direct tools -> runtime dependency would invert the build
//! graph). The developer CLI registers `recordWorkflowReceipt` via a function
//! pointer at startup (see `dev_cli.zig`), mirroring the perf and equivalence
//! probe injections. The standalone `zigts` binary leaves the probe null and
//! prints no receipt.
//!
//! On a successful `zigttp link` this signs the hypermedia verdict (proof
//! level + affordance resolution) over a hash of the bundle's system contract
//! with the persistent keypair and writes a compact `kind=workflow` JWS next to
//! the link outputs. Best-effort: a missing `$HOME` or unwritable output dir
//! must never abort the link.

const std = @import("std");
const zq = @import("zigts");
const zigts_cli = @import("zigts_cli");
const identity = @import("attest/identity.zig");

const precompile = zigts_cli.precompile;
const system_linker = zq.system_linker;
const hypermedia_receipt = zq.hypermedia_receipt;
const Sha256 = std.crypto.hash.sha2.Sha256;
const Ed25519 = std.crypto.sign.Ed25519;

/// Probe entry registered from `dev_cli.zig`. Loads the persistent attest
/// keypair and signs; best-effort, never raises.
pub fn recordWorkflowReceipt(
    allocator: std.mem.Allocator,
    system_path: []const u8,
    output_dir: []const u8,
    analysis: *const system_linker.SystemAnalysis,
) void {
    const signer = identity.loadOrCreate(allocator) catch return;
    recordWorkflowReceiptWithKey(allocator, system_path, output_dir, analysis, signer.key_pair) catch return;
}

/// Key-injected core. Tests pass a deterministic key so the receipt can be
/// verified without touching `$HOME`.
pub fn recordWorkflowReceiptWithKey(
    allocator: std.mem.Allocator,
    system_path: []const u8,
    output_dir: []const u8,
    analysis: *const system_linker.SystemAnalysis,
    key_pair: Ed25519.KeyPair,
) !void {
    const system_hash = try systemContractHashHex(allocator, analysis);

    const verdict: hypermedia_receipt.Verdict = .{
        .system_path = system_path,
        .system_hash = &system_hash,
        .proof_level = @tagName(analysis.proof_level),
        .all_affordances_resolved = analysis.properties.all_affordances_resolved,
        .affordance_responses_covered = analysis.properties.affordance_responses_covered,
        .resolved_affordances = @intCast(analysis.affordance_links.items.len),
        .dangling_affordances = analysis.dangling_affordances,
        .dynamic_affordances = analysis.dynamic_affordances,
        .handler_count = @intCast(analysis.config.handlers.len),
    };

    var envelope = try hypermedia_receipt.sign(allocator, verdict, key_pair);
    defer envelope.deinit(allocator);

    const receipt_path = try std.fs.path.join(allocator, &.{ output_dir, "workflow-receipt.jws" });
    defer allocator.free(receipt_path);

    try precompile.writeFilePosix(receipt_path, envelope.compact, allocator);
    std.debug.print("Wrote {s} (kind=workflow, allAffordancesResolved={s})\n", .{
        receipt_path,
        if (verdict.all_affordances_resolved) "true" else "false",
    });
}

fn systemContractHashHex(allocator: std.mem.Allocator, analysis: *const system_linker.SystemAnalysis) ![64]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    // The analysis JSON is the bundle identity: it folds in every handler's
    // routes, the resolved links, the proof level and the affordance summary.
    try system_linker.writeSystemContractJson(analysis, &aw.writer);
    var digest: [Sha256.digest_length]u8 = undefined;
    Sha256.hash(aw.writer.buffered(), &digest, .{});
    return std.fmt.bytesToHex(digest, .lower);
}
