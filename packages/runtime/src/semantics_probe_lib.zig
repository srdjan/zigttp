//! Semantics-receipt probe: the runtime-backed signer for `zigttp spec-check`.
//!
//! The keyless `zigts spec-check` proves the registry and prints its hash but
//! cannot sign: signing needs the persistent attest identity, which lives in the
//! runtime layer. The developer CLI registers `recordSemanticsReceipt` via a
//! function pointer before dispatching analyzer commands (see `dev_cli.zig`),
//! mirroring the perf / equivalence / workflow probe injections. The standalone
//! `zigts` binary leaves the probe null and emits no receipt.
//!
//! On a clean `zigttp spec-check` (all five mechanisms pass) this signs the
//! conformance receipt - the semantics/IR/opcode hashes plus the proof and
//! differential counts - with the persistent keypair and writes a compact
//! `kind=semantics` JWS to `.zigttp/semantics-receipt.jws`. It self-verifies
//! before persisting, so a receipt that cannot be checked is never written.
//! Best-effort: a missing `$HOME` or unwritable cwd must never abort spec-check.

const std = @import("std");
const builtin = @import("builtin");
const zq = @import("zigts");
const identity = @import("attest/identity.zig");

const semantics_check = zq.semantics_check;
const mkdirIfAbsent = @import("capsule.zig").mkdirIfAbsent;
const Ed25519 = std.crypto.sign.Ed25519;

const receipt_dir = ".zigttp";
const receipt_path = ".zigttp/semantics-receipt.jws";

pub const ReceiptOutput = struct {
    dir: []const u8,
    path: []const u8,
};

/// Probe entry registered from `dev_cli.zig`. Loads the persistent attest
/// keypair and signs; best-effort, never raises.
pub fn recordSemanticsReceipt(allocator: std.mem.Allocator, receipt: semantics_check.Receipt) void {
    const signer = identity.loadOrCreate(allocator) catch return;
    recordWithKeyAt(allocator, receipt, signer.key_pair, .{ .dir = receipt_dir, .path = receipt_path }) catch return;
}

/// Key-injected core. Tests pass a deterministic key and an explicit path so the
/// receipt round-trip can be checked without touching `$HOME` or the cwd.
pub fn recordWithKeyAt(
    allocator: std.mem.Allocator,
    receipt: semantics_check.Receipt,
    key_pair: Ed25519.KeyPair,
    output: ReceiptOutput,
) !void {
    const compact = try semantics_check.sign(allocator, receipt, key_pair);
    defer allocator.free(compact);

    // Self-verify before persisting: a receipt we cannot verify is not emitted.
    if (!try semantics_check.verify(allocator, compact, key_pair.public_key)) {
        return error.SelfVerifyFailed;
    }

    try mkdirIfAbsent(allocator, output.dir);
    try zq.file_io.writeFile(allocator, output.path, compact);
    std.debug.print("Wrote {s} (kind=semantics, hash={s}, differential={d}/{d}, smt={d}/{d})\n", .{
        output.path,                receipt.semantics_hash, receipt.differential_passed,
        receipt.differential_total, receipt.smt_proved,     receipt.smt_total,
    });
}

pub fn recordWithKeyTo(
    allocator: std.mem.Allocator,
    receipt: semantics_check.Receipt,
    key_pair: Ed25519.KeyPair,
    dir: []const u8,
    path: []const u8,
) !void {
    try recordWithKeyAt(allocator, receipt, key_pair, .{ .dir = dir, .path = path });
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

fn sampleReceipt() semantics_check.Receipt {
    return .{
        .semantics_hash = ("a" ** 64).*,
        .ir_table_hash = ("b" ** 64).*,
        .opcode_table_hash = ("c" ** 64).*,
        .nodes_proven = 12,
        .nodes_total = 81,
        .opcodes_specified = 7,
        .opcodes_total = 132,
        .binop_instances = 5,
        .unop_instances = 2,
        .refinements_proven = 1,
        .differential_passed = 10,
        .differential_total = 10,
        .smt_available = 1,
        .smt_proved = 5,
        .smt_total = 5,
        .audit_refuted = 3,
        .audit_total = 3,
        .failures = 0,
    };
}

fn createSymlinkAbsolute(
    allocator: std.mem.Allocator,
    target_path: []const u8,
    link_path: []const u8,
) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    try std.Io.Dir.symLinkAbsolute(io_backend.io(), target_path, link_path, .{});
}

test "recorded semantics receipt round-trips under a deterministic key" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(dir);
    const path = try std.fs.path.join(allocator, &.{ dir, "semantics-receipt.jws" });
    defer allocator.free(path);

    const seed = [_]u8{42} ** 32;
    const kp = try Ed25519.KeyPair.generateDeterministic(seed);

    try recordWithKeyAt(allocator, sampleReceipt(), kp, .{ .dir = dir, .path = path });

    // Read the persisted JWS back and verify it against the same public key.
    const written = try std.fs.cwd().readFileAlloc(allocator, path, 1 << 16);
    defer allocator.free(written);
    try std.testing.expect(try semantics_check.verify(allocator, written, kp.public_key));

    // A different key must reject it.
    const other_seed = [_]u8{7} ** 32;
    const other = try Ed25519.KeyPair.generateDeterministic(other_seed);
    try std.testing.expect(!try semantics_check.verify(allocator, written, other.public_key));
}

test "recorded semantics receipt replaces a symlink without truncating its target" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(dir);
    const target = try std.fs.path.join(allocator, &.{ dir, "outside.txt" });
    defer allocator.free(target);
    const path = try std.fs.path.join(allocator, &.{ dir, "semantics-receipt.jws" });
    defer allocator.free(path);

    try zq.file_io.writeFile(allocator, target, "outside");
    try createSymlinkAbsolute(allocator, target, path);

    const seed = [_]u8{42} ** 32;
    const kp = try Ed25519.KeyPair.generateDeterministic(seed);
    try recordWithKeyAt(allocator, sampleReceipt(), kp, .{ .dir = dir, .path = path });

    const target_after = try std.fs.cwd().readFileAlloc(allocator, target, 64);
    defer allocator.free(target_after);
    try std.testing.expectEqualStrings("outside", target_after);

    const written = try std.fs.cwd().readFileAlloc(allocator, path, 1 << 16);
    defer allocator.free(written);
    try std.testing.expect(try semantics_check.verify(allocator, written, kp.public_key));
}
