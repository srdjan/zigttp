//! Read-only derivations over the session transcript.
//!
//! The autoloop orchestrator (D2) needs three signals at every iteration:
//! the current HandlerProperties for a file (to decide convergence), the
//! hash of the most recent VerifiedPatch (to populate parent_hash on the
//! next one), and the set of witnesses still pending against the current
//! goal context (to feed regression detection). All three are derivable
//! from the transcript by scanning the verified_patch entries.
//!
//! Derivation beats materialization here because the transcript is the
//! only source of truth that survives resume via reconstructTranscript.
//! Any cached map on AgentSession would need a rebuild path that tracks
//! the reconstructor; doing the scan on demand removes that duplication
//! entirely. Each call is O(n) where n is transcript length; sessions
//! with thousands of patches can memoize on top if it ever matters.

const std = @import("std");
const transcript_mod = @import("transcript.zig");
const ui_payload = @import("ui_payload.zig");

/// Unwrap a transcript entry as a VerifiedPatchPayload for the requested
/// file, or null if it is not a verified_patch entry, has no payload,
/// carries a non-verified_patch payload, or is for a different file. The
/// shared predicate behind every "find the latest patch for file" lookup
/// in this module and in tui/app.zig.
pub fn patchPayloadIfMatching(
    entry: *const transcript_mod.OwnedEntry,
    file: []const u8,
) ?ui_payload.VerifiedPatchPayload {
    switch (entry.*) {
        .verified_patch => |message| {
            const payload = message.ui_payload orelse return null;
            switch (payload) {
                .verified_patch => |patch| {
                    if (!std.mem.eql(u8, patch.file, file)) return null;
                    return patch;
                },
                else => return null,
            }
        },
        else => return null,
    }
}

/// Return the `after_properties` of the most recent verified_patch entry
/// whose `file` matches, or null if no patch for that file has landed.
pub fn currentProperties(
    transcript: *const transcript_mod.Transcript,
    file: []const u8,
) ?ui_payload.PropertiesSnapshot {
    var i = transcript.len();
    while (i > 0) {
        i -= 1;
        if (patchPayloadIfMatching(transcript.at(i), file)) |patch| {
            return patch.after_properties;
        }
    }
    return null;
}

/// Return the `patch_hash` of the most recent verified_patch for `file`,
/// or null if none exists or the patch predates the chain metadata.
pub fn lastPatchHash(
    transcript: *const transcript_mod.Transcript,
    file: []const u8,
) ?[32]u8 {
    var i = transcript.len();
    while (i > 0) {
        i -= 1;
        if (patchPayloadIfMatching(transcript.at(i), file)) |patch| {
            return patch.patch_hash;
        }
    }
    return null;
}

/// Read a PropertiesSnapshot field by its string name. Returns false for
/// unknown names, which deliberately aliases "unknown" with "unsatisfied"
/// so a typo in the goal list never silently passes the termination check.
pub fn propertyByName(props: ui_payload.PropertiesSnapshot, name: []const u8) bool {
    inline for (@typeInfo(ui_payload.PropertiesSnapshot).@"struct".fields) |field| {
        if (field.type != bool) continue;
        if (std.mem.eql(u8, field.name, name)) {
            return @field(props, field.name);
        }
    }
    return false;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn appendPatch(
    allocator: std.mem.Allocator,
    tr: *transcript_mod.Transcript,
    file: []const u8,
    hash: ?[32]u8,
    after: ui_payload.PropertiesSnapshot,
) !void {
    var patch: ui_payload.UiPayload = .{ .verified_patch = .{
        .file = try allocator.dupe(u8, file),
        .policy_hash = try allocator.dupe(u8, "p" ** 64),
        .applied_at_unix_ms = 0,
        .stats = .{ .total = 0, .new = 0, .preexisting = 0 },
        .before = null,
        .after = try allocator.dupe(u8, ""),
        .unified_diff = try allocator.dupe(u8, ""),
        .hunks = try allocator.alloc(ui_payload.DiffHunk, 0),
        .violations = try allocator.alloc(ui_payload.ViolationDeltaItem, 0),
        .before_properties = null,
        .after_properties = after,
        .prove = null,
        .system = null,
        .rule_citations = try allocator.alloc([]u8, 0),
        .patch_hash = hash,
        .post_apply_ok = true,
        .post_apply_summary = null,
    } };
    errdefer patch.deinit(allocator);

    try tr.entries.append(allocator, .{ .verified_patch = .{
        .llm_text = try allocator.dupe(u8, "verified"),
        .ui_payload = patch,
    } });
}

fn zeroProps() ui_payload.PropertiesSnapshot {
    return .{
        .pure = false,
        .read_only = false,
        .stateless = false,
        .retry_safe = false,
        .deterministic = false,
        .has_egress = false,
        .no_secret_leakage = false,
        .no_credential_leakage = false,
        .input_validated = false,
        .pii_contained = false,
        .idempotent = false,
        .max_io_depth = null,
        .injection_safe = false,
        .state_isolated = false,
        .fault_covered = false,
        .result_safe = false,
        .optional_safe = false,
    };
}

test "currentProperties returns the latest patch for the matching file" {
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);

    var p1 = zeroProps();
    p1.retry_safe = false;
    try appendPatch(testing.allocator, &tr, "handler.ts", null, p1);

    var p2 = zeroProps();
    p2.retry_safe = true;
    try appendPatch(testing.allocator, &tr, "handler.ts", null, p2);

    const current = currentProperties(&tr, "handler.ts");
    try testing.expect(current != null);
    try testing.expect(current.?.retry_safe);
}

test "currentProperties returns null when no patch matches the file" {
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);

    try appendPatch(testing.allocator, &tr, "other.ts", null, zeroProps());

    const current = currentProperties(&tr, "handler.ts");
    try testing.expect(current == null);
}

test "currentProperties is scoped per file" {
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);

    var a = zeroProps();
    a.pure = true;
    try appendPatch(testing.allocator, &tr, "a.ts", null, a);

    var b = zeroProps();
    b.retry_safe = true;
    try appendPatch(testing.allocator, &tr, "b.ts", null, b);

    const ca = currentProperties(&tr, "a.ts").?;
    const cb = currentProperties(&tr, "b.ts").?;
    try testing.expect(ca.pure);
    try testing.expect(!ca.retry_safe);
    try testing.expect(cb.retry_safe);
    try testing.expect(!cb.pure);
}

test "lastPatchHash returns the most recent hash for the file" {
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);

    var h1: [32]u8 = undefined;
    for (&h1, 0..) |*b, i| b.* = @intCast(i);
    var h2: [32]u8 = undefined;
    for (&h2, 0..) |*b, i| b.* = @intCast(i + 100);

    try appendPatch(testing.allocator, &tr, "handler.ts", h1, zeroProps());
    try appendPatch(testing.allocator, &tr, "handler.ts", h2, zeroProps());

    const latest = lastPatchHash(&tr, "handler.ts");
    try testing.expect(latest != null);
    try testing.expectEqualSlices(u8, &h2, &latest.?);
}

test "lastPatchHash returns null when only legacy patches exist" {
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);

    try appendPatch(testing.allocator, &tr, "handler.ts", null, zeroProps());

    try testing.expect(lastPatchHash(&tr, "handler.ts") == null);
}

