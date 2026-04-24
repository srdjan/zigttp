//! Stable identity for counterexample witnesses across edits.
//!
//! A witness is identified by `(property, origin_node_id?, sink_node_id?)`
//! at its ideal. When node IDs are unavailable the key falls back to
//! line/column. The autoloop orchestrator compares before/after witness
//! sets by these keys to decide `witnesses_defeated` vs `witnesses_new`
//! for each VerifiedPatch event.
//!
//! The actual hash computation lives on `counterexample.CounterexampleWitness.stableKey`
//! so that compiler-internal code can write a key without taking a `pi`
//! dependency. This module is the `pi`-side wrapper that allocates and
//! returns an owned hex-string slice - what the ledger payload stores.

const std = @import("std");
const zigts = @import("zigts");
const counterexample = zigts.counterexample;

/// Allocate the stable-key hex string for a witness. Caller owns the
/// returned slice.
pub fn forWitness(
    allocator: std.mem.Allocator,
    witness: counterexample.CounterexampleWitness,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try witness.stableKey(&aw.writer);
    buf = aw.toArrayList();
    return buf.toOwnedSlice(allocator);
}

/// Compute the set difference `a - b` keyed by stable witness key. Each
/// returned slice element is an owned copy of a key from `a` that has no
/// match in `b`. Caller owns the outer slice and every element.
pub fn difference(
    allocator: std.mem.Allocator,
    a: []const []const u8,
    b: []const []const u8,
) ![][]u8 {
    var seen: std.StringHashMapUnmanaged(void) = .empty;
    defer seen.deinit(allocator);
    for (b) |key| try seen.put(allocator, key, {});

    var out: std.ArrayList([]u8) = .empty;
    errdefer {
        for (out.items) |item| allocator.free(item);
        out.deinit(allocator);
    }
    for (a) |key| {
        if (seen.contains(key)) continue;
        const copy = try allocator.dupe(u8, key);
        errdefer allocator.free(copy);
        try out.append(allocator, copy);
    }
    return out.toOwnedSlice(allocator);
}

test "forWitness produces a 64-char hex digest" {
    const allocator = std.testing.allocator;
    var witness = try counterexample.solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 3, .column = 1 },
        .sink = .{ .line = 5, .column = 12 },
        .origin_node_id = 7,
        .sink_node_id = 19,
        .summary = "t",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer witness.deinit(allocator);

    const key = try forWitness(allocator, witness);
    defer allocator.free(key);

    try std.testing.expectEqual(@as(usize, 64), key.len);
}

test "difference returns keys present in a but absent in b" {
    const allocator = std.testing.allocator;
    const a = [_][]const u8{ "k1", "k2", "k3" };
    const b = [_][]const u8{"k2"};

    const diff = try difference(allocator, &a, &b);
    defer {
        for (diff) |item| allocator.free(item);
        allocator.free(diff);
    }

    try std.testing.expectEqual(@as(usize, 2), diff.len);
}

test "difference returns empty slice when a is subset of b" {
    const allocator = std.testing.allocator;
    const a = [_][]const u8{ "k1", "k2" };
    const b = [_][]const u8{ "k1", "k2", "k3" };

    const diff = try difference(allocator, &a, &b);
    defer allocator.free(diff);

    try std.testing.expectEqual(@as(usize, 0), diff.len);
}
