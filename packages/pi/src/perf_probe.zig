//! Perf-as-proof receipt probe (PI side).
//!
//! Perf-receipt primitives live across the signed JWS (`zigts.perf_receipt`),
//! the `kind=perf` ledger row (`runtime/proof_ledger.zig`), and the
//! engine-backed latency probe (`runtime/benchmark.zig`). Wiring them onto
//! applied edits goes through this seam because the probe needs the JS engine
//! and the ledger pulls in the deploy stack; neither is importable from PI
//! without inverting the build dependency.
//!
//! This module is the cycle-free seam, mirroring `witness_replay.zig`: the
//! binary that ships PI together with the runtime stack registers a probe
//! function pointer at startup via `setProbeFn`. PI's apply path calls
//! `record` after a green write; if no probe is registered (unit tests,
//! analyzer-only builds) or the user passed `--no-perf-receipt`, the call is
//! a silent no-op and the edit still lands.
//!
//! The probe is best-effort by contract: a failed probe never fails an
//! apply. The auditable artifact is the signed `kind=perf` row the probe
//! writes to `.zigttp/proofs.jsonl`; this side only triggers it.

const std = @import("std");

/// Registered by the runtime-backed implementation. `handler_path` is the
/// absolute path of the just-written handler; `after_bytes` is its
/// post-apply content (already on disk, passed to avoid a re-read);
/// `applied_at_unix_ms` stamps the ledger row so perf events sort with the
/// deploy/swap/check events already in the ledger.
pub const ProbeFn = *const fn (
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    after_bytes: []const u8,
    applied_at_unix_ms: i64,
) anyerror!void;

var probe_fn: ?ProbeFn = null;
var enabled: bool = true;

/// Register the runtime-backed implementation. Called once at startup from a
/// binary that ships both PI and the runtime stack. Last-write-wins; the
/// codebase only registers from one boot path.
pub fn setProbeFn(f: ProbeFn) void {
    probe_fn = f;
}

/// Toggle from the `--perf-receipt` / `--no-perf-receipt` expert flag.
/// Default on, matching the attestation default.
pub fn setEnabled(on: bool) void {
    enabled = on;
}

/// Restore the unconfigured/default state. For tests that install a stub
/// probe and must leave shared module state clean for later tests.
pub fn reset() void {
    probe_fn = null;
    enabled = true;
}

pub fn isConfigured() bool {
    return probe_fn != null;
}

pub fn isEnabled() bool {
    return enabled;
}

/// Emit a signed perf receipt for a just-applied edit. No-op when disabled,
/// when no probe is wired, or when the probe errors — the receipt is an
/// audit artifact, never a gate on the apply.
pub fn record(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    after_bytes: []const u8,
    applied_at_unix_ms: i64,
) void {
    if (!enabled) return;
    const f = probe_fn orelse return;
    f(allocator, handler_path, after_bytes, applied_at_unix_ms) catch {};
}

const testing = std.testing;

// A configured probe is invoked exactly once with the content we hand it.
var test_calls: usize = 0;
var test_last_len: usize = 0;

fn testProbe(
    _: std.mem.Allocator,
    _: []const u8,
    after_bytes: []const u8,
    _: i64,
) anyerror!void {
    test_calls += 1;
    test_last_len = after_bytes.len;
}

test "record is a no-op when no probe is configured" {
    reset(); // unconfigured, enabled — the default startup state
    defer reset();
    test_calls = 0;
    record(testing.allocator, "/abs/handler.ts", "fn handler() {}", 1);
    try testing.expectEqual(@as(usize, 0), test_calls);
}

test "record invokes a configured probe with the post-apply bytes" {
    defer reset();
    test_calls = 0;
    test_last_len = 0;
    setProbeFn(testProbe);
    setEnabled(true);
    record(testing.allocator, "/abs/handler.ts", "fn handler() {}", 7);
    try testing.expectEqual(@as(usize, 1), test_calls);
    try testing.expectEqual(@as(usize, "fn handler() {}".len), test_last_len);
}

test "record skips the probe when disabled" {
    defer reset();
    test_calls = 0;
    setProbeFn(testProbe);
    setEnabled(false);
    record(testing.allocator, "/abs/handler.ts", "fn handler() {}", 9);
    try testing.expectEqual(@as(usize, 0), test_calls);
}
