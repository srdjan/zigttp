//! Proof-carrying changes: equivalence-receipt probe (PI side).
//!
//! After a green apply, the expert loop can emit a signed
//! `kind=equivalence` receipt recording the behavioral verdict between the
//! pre- and post-edit handler (the same `contract_diff` classification the
//! `prove-behavior` command computes). This is the audit counterpart to the
//! perf probe: it answers "did this edit change what the handler does?" and
//! signs the answer.
//!
//! Like `perf_probe.zig`, this is a cycle-free seam. Signing pulls in the
//! persistent attest identity and the ledger pulls in the deploy stack, and
//! the runtime binaries consume PI, so PI cannot import the implementation
//! directly without inverting the build graph. The host binary that ships PI
//! together with the runtime registers `recordEquivalenceReceipt` via a
//! function pointer at startup (see `dev_cli.zig`, next to the perf-probe
//! registration). When no probe is registered (unit tests, analyzer-only
//! builds) or the user disabled it, `record` is a silent no-op and the edit
//! still lands.
//!
//! Best-effort by contract: a failed probe never fails an apply. The
//! auditable artifact is the signed `kind=equivalence` row the probe writes
//! to `.zttp/proofs.jsonl`; this side only triggers it.

const std = @import("std");

/// Registered by the runtime-backed implementation. `handler_path` is the
/// absolute path of the just-written handler; `before_bytes`/`after_bytes`
/// are the pre- and post-edit sources; `applied_at_unix_ms` stamps the row.
pub const ProbeFn = *const fn (
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    before_bytes: []const u8,
    after_bytes: []const u8,
    applied_at_unix_ms: i64,
) anyerror!void;

var probe_fn: ?ProbeFn = null;
var enabled: bool = true;

/// Register the runtime-backed implementation. Called once at startup from a
/// binary that ships both PI and the runtime stack. Last-write-wins.
pub fn setProbeFn(f: ProbeFn) void {
    probe_fn = f;
}

/// Toggle from the `--equivalence-receipt` / `--no-equivalence-receipt`
/// expert flag. Default on, matching the attestation default.
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

/// Emit a signed equivalence receipt for a just-applied edit. No-op when
/// disabled, when no probe is wired, when there is no before-image (a new
/// file has nothing to be equivalent to), or when the probe errors - the
/// receipt is an audit artifact, never a gate on the apply.
pub fn record(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    before_bytes: ?[]const u8,
    after_bytes: []const u8,
    applied_at_unix_ms: i64,
) void {
    if (!enabled) return;
    const before = before_bytes orelse return;
    const f = probe_fn orelse return;
    f(allocator, handler_path, before, after_bytes, applied_at_unix_ms) catch {};
}

const testing = std.testing;

var test_calls: usize = 0;
var test_last_before_len: usize = 0;

fn testProbe(
    _: std.mem.Allocator,
    _: []const u8,
    before_bytes: []const u8,
    _: []const u8,
    _: i64,
) anyerror!void {
    test_calls += 1;
    test_last_before_len = before_bytes.len;
}

test "record is a no-op when no probe is configured" {
    reset();
    defer reset();
    test_calls = 0;
    record(testing.allocator, "/abs/handler.ts", "before", "after", 1);
    try testing.expectEqual(@as(usize, 0), test_calls);
}

test "record invokes a configured probe with the before bytes" {
    defer reset();
    test_calls = 0;
    test_last_before_len = 0;
    setProbeFn(testProbe);
    setEnabled(true);
    record(testing.allocator, "/abs/handler.ts", "before123", "after", 7);
    try testing.expectEqual(@as(usize, 1), test_calls);
    try testing.expectEqual(@as(usize, "before123".len), test_last_before_len);
}

test "record skips when there is no before image" {
    defer reset();
    test_calls = 0;
    setProbeFn(testProbe);
    setEnabled(true);
    record(testing.allocator, "/abs/handler.ts", null, "after", 9);
    try testing.expectEqual(@as(usize, 0), test_calls);
}

test "record skips the probe when disabled" {
    defer reset();
    test_calls = 0;
    setProbeFn(testProbe);
    setEnabled(false);
    record(testing.allocator, "/abs/handler.ts", "before", "after", 9);
    try testing.expectEqual(@as(usize, 0), test_calls);
}
