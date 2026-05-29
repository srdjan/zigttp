//! Capsule-replay probe (PI side) — the expert loop's awareness of the Proof
//! Flight Recorder.
//!
//! When a workspace has a recorded capsule (`.zigttp/capsules/default/`, from
//! `zigttp dev --record-proof`), every applied expert edit can be replayed
//! against it: do the requests the developer actually exercised still
//! reproduce? A regression means the edit changed observed behavior on a real
//! request — the single most concrete "you broke something" signal the loop
//! can give, grounded in traffic rather than static analysis.
//!
//! Like `perf_probe.zig` and `equivalence_probe.zig`, this is a cycle-free
//! seam. Replay needs the JS engine and the capsule reader, which live in the
//! runtime stack that consumes PI, so PI cannot import them directly. The host
//! binary registers the runtime-backed probe via `setProbeFn` at startup; when
//! nothing is registered (unit tests, analyzer-only builds) or no capsule
//! exists, `check` is a silent no-op and the edit lands unchanged.
//!
//! Best-effort and non-gating: the probe reports a regression count, never
//! blocks the apply. Surfacing it to the user (and any future veto policy) is
//! the caller's choice.

const std = @import("std");

/// Result of replaying the active capsule against just-applied content.
/// `total == 0` means there was nothing to replay (no capsule, or it had no
/// recorded requests); the caller treats that as "not applicable".
pub const ReplayTally = struct {
    total: u32 = 0,
    regressed: u32 = 0,
};

/// Registered by the runtime-backed implementation. `handler_path` is the
/// absolute path of the just-written handler; `after_bytes` is its post-apply
/// content. Returns the replay tally for the workspace's active capsule.
pub const ProbeFn = *const fn (
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    handler_path: []const u8,
    after_bytes: []const u8,
) anyerror!ReplayTally;

var probe_fn: ?ProbeFn = null;
var enabled: bool = true;

/// Register the runtime-backed implementation. Called once at startup from a
/// binary that ships both PI and the runtime stack. Last-write-wins.
pub fn setProbeFn(f: ProbeFn) void {
    probe_fn = f;
}

/// Toggle from the `--capsule-check` / `--no-capsule-check` expert flag.
/// Default on.
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

/// Replay the workspace's active capsule against the just-applied content.
/// Returns a zero tally when disabled, unwired, or the probe errors — the
/// check is an audit signal, never a gate on the apply.
pub fn check(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    handler_path: []const u8,
    after_bytes: []const u8,
) ReplayTally {
    if (!enabled) return .{};
    const f = probe_fn orelse return .{};
    return f(allocator, workspace_root, handler_path, after_bytes) catch .{};
}

const testing = std.testing;

var test_calls: usize = 0;

fn stubNoRegression(
    _: std.mem.Allocator,
    _: []const u8,
    _: []const u8,
    _: []const u8,
) anyerror!ReplayTally {
    test_calls += 1;
    return .{ .total = 2, .regressed = 0 };
}

fn stubRegression(
    _: std.mem.Allocator,
    _: []const u8,
    _: []const u8,
    _: []const u8,
) anyerror!ReplayTally {
    test_calls += 1;
    return .{ .total = 2, .regressed = 1 };
}

test "check is a no-op tally when no probe is configured" {
    reset();
    defer reset();
    test_calls = 0;
    const tally = check(testing.allocator, "/ws", "/ws/h.ts", "after");
    try testing.expectEqual(@as(u32, 0), tally.total);
    try testing.expectEqual(@as(usize, 0), test_calls);
}

test "check returns the probe's tally when wired" {
    defer reset();
    test_calls = 0;
    setProbeFn(stubNoRegression);
    setEnabled(true);
    const tally = check(testing.allocator, "/ws", "/ws/h.ts", "after");
    try testing.expectEqual(@as(u32, 2), tally.total);
    try testing.expectEqual(@as(u32, 0), tally.regressed);
    try testing.expectEqual(@as(usize, 1), test_calls);
}

test "check surfaces a regression count" {
    defer reset();
    test_calls = 0;
    setProbeFn(stubRegression);
    setEnabled(true);
    const tally = check(testing.allocator, "/ws", "/ws/h.ts", "after");
    try testing.expectEqual(@as(u32, 1), tally.regressed);
}

test "check skips the probe when disabled" {
    defer reset();
    test_calls = 0;
    setProbeFn(stubNoRegression);
    setEnabled(false);
    const tally = check(testing.allocator, "/ws", "/ws/h.ts", "after");
    try testing.expectEqual(@as(u32, 0), tally.total);
    try testing.expectEqual(@as(usize, 0), test_calls);
}
