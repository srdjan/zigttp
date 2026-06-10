//! `zigttp proofs bundle` packages a handler's contract, optional binary,
//! and optional replay artifacts into a directory layout a third party
//! can verify deterministically. `zigttp proofs verify <dir>` re-checks
//! every SHA-256 in the manifest against the actual file bytes.
//!
//! Layout under `--out <dir>`:
//!   - bundle.json            manifest (tool version, sha256s of all parts)
//!   - handler.contract.json  byte-for-byte copy of the input contract
//!   - binary                 copy of `--binary <path>` (when supplied)
//!   - binary.sha256          hex digest as a text file (when supplied)
//!   - replay/<filename>      copy of `--replay <path>` (when supplied)
//!
//! Hard redaction guardrail: the bundle is built from explicit file
//! arguments only. `rejectSuspiciousPath` blocks parent-dir traversal and
//! system roots. `verify` additionally rejects manifest component paths
//! that are absolute or contain `..` segments, so a crafted bundle.json
//! cannot make the verifier hash files outside the bundle directory.
//! Contracts are not introspected for value fields; the contract format
//! is the authority on what fields exist.

const std = @import("std");
const zigts = @import("zigts");
const static_mod = @import("../server_static.zig");

pub const tool_version: []const u8 = "zigttp-bundle-1";

pub const BundleArgs = struct {
    contract_path: []const u8,
    binary_path: ?[]const u8 = null,
    replay_path: ?[]const u8 = null,
    out_dir: []const u8,
};

pub const ComponentVerdict = struct {
    name: []const u8,
    pass: bool,
    expected_sha: [64]u8,
    actual_sha: [64]u8,
};

pub fn writeBundle(allocator: std.mem.Allocator, args: BundleArgs, stdout: *std.Io.Writer, stderr: *std.Io.Writer) !void {
    if (args.contract_path.len == 0) {
        try stderr.writeAll("zigttp proofs bundle: --contract <path> is required\n");
        return error.MissingContractArg;
    }
    if (args.out_dir.len == 0) {
        try stderr.writeAll("zigttp proofs bundle: --out <dir> is required\n");
        return error.MissingOutArg;
    }

    try rejectSuspiciousPath(args.contract_path);
    if (args.binary_path) |p| try rejectSuspiciousPath(p);
    if (args.replay_path) |p| try rejectSuspiciousPath(p);
    try rejectSuspiciousPath(args.out_dir);

    try ensureDir(allocator, args.out_dir);

    const contract_bytes = try zigts.file_io.readFile(allocator, args.contract_path, 256 * 1024 * 1024);
    defer allocator.free(contract_bytes);
    const contract_sha = sha256Hex(contract_bytes);
    const contract_dest = try std.fs.path.join(allocator, &.{ args.out_dir, "handler.contract.json" });
    defer allocator.free(contract_dest);
    try zigts.file_io.writeFile(allocator, contract_dest, contract_bytes);

    var binary_sha_hex: ?[64]u8 = null;
    if (args.binary_path) |path| {
        const binary_bytes = try zigts.file_io.readFile(allocator, path, 256 * 1024 * 1024);
        defer allocator.free(binary_bytes);
        const sha = sha256Hex(binary_bytes);
        binary_sha_hex = sha;
        const bin_dest = try std.fs.path.join(allocator, &.{ args.out_dir, "binary" });
        defer allocator.free(bin_dest);
        try zigts.file_io.writeFile(allocator, bin_dest, binary_bytes);
        const sha_dest = try std.fs.path.join(allocator, &.{ args.out_dir, "binary.sha256" });
        defer allocator.free(sha_dest);
        try zigts.file_io.writeFile(allocator, sha_dest, &sha);
    }

    var replay_sha_hex: ?[64]u8 = null;
    var replay_basename: ?[]const u8 = null;
    if (args.replay_path) |path| {
        // Replay traces are JSONL records, expected to be KB-MB. Cap below
        // the 256 MiB contract/binary limit so a misrouted binary blob
        // path fails fast instead of silently bundling.
        const replay_bytes = try zigts.file_io.readFile(allocator, path, 32 * 1024 * 1024);
        defer allocator.free(replay_bytes);
        const sha = sha256Hex(replay_bytes);
        replay_sha_hex = sha;

        const replay_dir = try std.fs.path.join(allocator, &.{ args.out_dir, "replay" });
        defer allocator.free(replay_dir);
        try ensureDir(allocator, replay_dir);

        const basename = std.fs.path.basename(path);
        replay_basename = basename;
        const replay_dest = try std.fs.path.join(allocator, &.{ replay_dir, basename });
        defer allocator.free(replay_dest);
        try zigts.file_io.writeFile(allocator, replay_dest, replay_bytes);
    }

    var manifest_buf: std.ArrayList(u8) = .empty;
    defer manifest_buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &manifest_buf);
    try writeManifest(&aw.writer, .{
        .contract_sha = contract_sha,
        .binary_sha = binary_sha_hex,
        .replay_sha = replay_sha_hex,
        .replay_basename = replay_basename,
    });
    manifest_buf = aw.toArrayList();

    const manifest_dest = try std.fs.path.join(allocator, &.{ args.out_dir, "bundle.json" });
    defer allocator.free(manifest_dest);
    try zigts.file_io.writeFile(allocator, manifest_dest, manifest_buf.items);

    try stdout.print("Wrote bundle to {s}/\n", .{args.out_dir});
    try stdout.print("  handler.contract.json  sha256={s}\n", .{contract_sha});
    if (binary_sha_hex) |s| try stdout.print("  binary                 sha256={s}\n", .{s});
    if (replay_sha_hex) |s| try stdout.print("  replay/{s}         sha256={s}\n", .{ replay_basename.?, s });
}

const ManifestFields = struct {
    contract_sha: [64]u8,
    binary_sha: ?[64]u8,
    replay_sha: ?[64]u8,
    replay_basename: ?[]const u8,
};

fn writeManifest(writer: *std.Io.Writer, m: ManifestFields) !void {
    try writer.writeAll("{\n");
    try writer.print("  \"toolVersion\": \"{s}\",\n", .{tool_version});
    try writer.writeAll("  \"createdAt\": \"1970-01-01T00:00:00Z\",\n");
    try writer.writeAll("  \"components\": {\n");
    try writer.print("    \"contract\": {{ \"path\": \"handler.contract.json\", \"sha256\": \"{s}\" }}", .{m.contract_sha});
    if (m.binary_sha) |s| {
        try writer.writeAll(",\n");
        try writer.print("    \"binary\": {{ \"path\": \"binary\", \"sha256\": \"{s}\" }}", .{s});
    }
    if (m.replay_sha) |s| {
        try writer.writeAll(",\n");
        try writer.print("    \"replay\": {{ \"path\": \"replay/{s}\", \"sha256\": \"{s}\" }}", .{ m.replay_basename.?, s });
    }
    try writer.writeAll("\n  }\n}\n");
}

pub fn verify(allocator: std.mem.Allocator, bundle_dir_path: []const u8, stdout: *std.Io.Writer, stderr: *std.Io.Writer) !void {
    try rejectSuspiciousPath(bundle_dir_path);

    const manifest_path = try std.fs.path.join(allocator, &.{ bundle_dir_path, "bundle.json" });
    defer allocator.free(manifest_path);

    const manifest_bytes = zigts.file_io.readFile(allocator, manifest_path, 16 * 1024 * 1024) catch {
        try stderr.print("zigttp proofs verify: cannot read manifest at '{s}'\n", .{manifest_path});
        return error.NoBundleJson;
    };
    defer allocator.free(manifest_bytes);

    var verdicts: std.ArrayList(ComponentVerdict) = .empty;
    defer {
        for (verdicts.items) |v| allocator.free(v.name);
        verdicts.deinit(allocator);
    }
    try verifyComponents(allocator, bundle_dir_path, manifest_bytes, &verdicts, stderr);

    var any_failed = false;
    for (verdicts.items) |v| {
        const label: []const u8 = if (v.pass) "OK  " else "FAIL";
        try stdout.print("  {s}  {s}\n", .{ label, v.name });
        if (!v.pass) {
            any_failed = true;
            try stdout.print("        expected sha256 {s}\n", .{v.expected_sha});
            try stdout.print("        actual   sha256 {s}\n", .{v.actual_sha});
        }
    }

    if (any_failed) return error.Sha256Mismatch;
    try stdout.writeAll("\nBundle verified: every component sha256 matches the manifest.\n");
}

fn verifyComponents(
    allocator: std.mem.Allocator,
    bundle_dir_path: []const u8,
    manifest_bytes: []const u8,
    out: *std.ArrayList(ComponentVerdict),
    stderr: *std.Io.Writer,
) !void {
    var offset: usize = 0;
    while (findNextComponent(manifest_bytes, offset)) |entry| : (offset = entry.next_offset) {
        // The manifest is untrusted input: an absolute or `..`-containing
        // component path would make the verifier hash arbitrary files.
        if (!static_mod.isPathSafe(entry.path)) {
            try stderr.print("zigttp proofs verify: component path '{s}' escapes the bundle directory\n", .{entry.path});
            return error.SuspiciousPath;
        }
        const file_full = try std.fs.path.join(allocator, &.{ bundle_dir_path, entry.path });
        defer allocator.free(file_full);

        const file_bytes = zigts.file_io.readFile(allocator, file_full, 256 * 1024 * 1024) catch {
            return error.MissingComponent;
        };
        defer allocator.free(file_bytes);

        const actual = sha256Hex(file_bytes);
        const name_dup = try allocator.dupe(u8, entry.name);
        try out.append(allocator, .{
            .name = name_dup,
            .pass = std.mem.eql(u8, &actual, &entry.expected_sha),
            .expected_sha = entry.expected_sha,
            .actual_sha = actual,
        });
    }
}

const ManifestEntry = struct {
    name: []const u8,
    path: []const u8,
    expected_sha: [64]u8,
    next_offset: usize,
};

/// Minimal manifest scanner: finds the next `"<name>": { "path": "...", "sha256": "..." }`
/// entry starting at or after `start`. Returns null when no further entry
/// exists. The scanner is tolerant of whitespace but strict about the
/// expected key names and shape.
fn findNextComponent(manifest: []const u8, start: usize) ?ManifestEntry {
    if (start >= manifest.len) return null;
    const path_idx = std.mem.indexOfPos(u8, manifest, start, "\"path\":") orelse return null;
    const name_close = std.mem.lastIndexOf(u8, manifest[0..path_idx], "\"") orelse return null;
    const name_open_search = manifest[0..name_close];
    const name_open = std.mem.lastIndexOf(u8, name_open_search, "\"") orelse return null;
    const name = manifest[name_open + 1 .. name_close];

    const path_value_open = std.mem.indexOfScalarPos(u8, manifest, path_idx + "\"path\":".len, '"') orelse return null;
    const path_value_close = std.mem.indexOfScalarPos(u8, manifest, path_value_open + 1, '"') orelse return null;
    const path = manifest[path_value_open + 1 .. path_value_close];

    const sha_key = std.mem.indexOfPos(u8, manifest, path_value_close, "\"sha256\":") orelse return null;
    const sha_open = std.mem.indexOfScalarPos(u8, manifest, sha_key + "\"sha256\":".len, '"') orelse return null;
    const sha_close = std.mem.indexOfScalarPos(u8, manifest, sha_open + 1, '"') orelse return null;
    const sha_bytes = manifest[sha_open + 1 .. sha_close];
    if (sha_bytes.len != 64) return null;

    var sha_buf: [64]u8 = undefined;
    @memcpy(&sha_buf, sha_bytes);

    return .{
        .name = name,
        .path = path,
        .expected_sha = sha_buf,
        .next_offset = sha_close + 1,
    };
}

fn rejectSuspiciousPath(path: []const u8) !void {
    if (path.len == 0) return;
    if (std.mem.indexOf(u8, path, "..") != null) return error.SuspiciousPath;
    if (std.mem.startsWith(u8, path, "/etc/") or std.mem.startsWith(u8, path, "/var/")) return error.SuspiciousPath;
}

fn ensureDir(allocator: std.mem.Allocator, path: []const u8) !void {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);
    switch (std.posix.errno(std.posix.system.mkdir(path_z, 0o755))) {
        .SUCCESS, .EXIST => {},
        else => return error.MakeDirFailed,
    }
}

fn sha256Hex(bytes: []const u8) [64]u8 {
    var digest: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytes, &digest, .{});
    return std.fmt.bytesToHex(digest, .lower);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "sha256Hex of empty buffer matches known digest" {
    const sha = sha256Hex("");
    try std.testing.expectEqualStrings("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", &sha);
}

test "rejectSuspiciousPath blocks parent traversal and system roots" {
    try std.testing.expectError(error.SuspiciousPath, rejectSuspiciousPath("../etc/passwd"));
    try std.testing.expectError(error.SuspiciousPath, rejectSuspiciousPath("/etc/shadow"));
    try std.testing.expectError(error.SuspiciousPath, rejectSuspiciousPath("/var/log/whatever"));
    try rejectSuspiciousPath("contract.json");
    try rejectSuspiciousPath("./out/bundle");
}

test "findNextComponent reads contract entry from a minimal manifest" {
    const manifest =
        \\{
        \\  "toolVersion": "zigttp-bundle-1",
        \\  "components": {
        \\    "contract": { "path": "handler.contract.json", "sha256": "abc1230000000000000000000000000000000000000000000000000000000abc" }
        \\  }
        \\}
    ;
    const entry = findNextComponent(manifest, 0) orelse return error.TestUnexpectedNull;
    try std.testing.expectEqualStrings("contract", entry.name);
    try std.testing.expectEqualStrings("handler.contract.json", entry.path);
    try std.testing.expectEqualStrings("abc1230000000000000000000000000000000000000000000000000000000abc", &entry.expected_sha);
}

test "findNextComponent walks multiple entries via next_offset" {
    const manifest =
        \\{
        \\  "components": {
        \\    "contract": { "path": "handler.contract.json", "sha256": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" },
        \\    "binary": { "path": "binary", "sha256": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" }
        \\  }
        \\}
    ;
    const first = findNextComponent(manifest, 0) orelse return error.TestUnexpectedNull;
    try std.testing.expectEqualStrings("contract", first.name);
    const second = findNextComponent(manifest, first.next_offset) orelse return error.TestUnexpectedNull;
    try std.testing.expectEqualStrings("binary", second.name);
    try std.testing.expect(findNextComponent(manifest, second.next_offset) == null);
}

const test_chdir = @import("../proof_ledger.zig").chdirTmpForTest;

test "verify rejects a manifest component path with parent traversal" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try test_chdir(&tmp);
    defer std.testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    // The target exists outside the bundle dir and its sha256 matches the
    // manifest, so a verifier that read it would report the component OK.
    try zigts.file_io.writeFile(std.testing.allocator, "secret", "outside-the-bundle");
    try ensureDir(std.testing.allocator, "bundle");
    const secret_sha = sha256Hex("outside-the-bundle");
    var manifest_buf: [256]u8 = undefined;
    const manifest = try std.fmt.bufPrint(
        &manifest_buf,
        "{{\n  \"components\": {{\n    \"contract\": {{ \"path\": \"../secret\", \"sha256\": \"{s}\" }}\n  }}\n}}\n",
        .{secret_sha},
    );
    try zigts.file_io.writeFile(std.testing.allocator, "bundle/bundle.json", manifest);

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer err.deinit();
    try std.testing.expectError(error.SuspiciousPath, verify(std.testing.allocator, "bundle", &out.writer, &err.writer));
    try std.testing.expect(std.mem.indexOf(u8, out.writer.buffered(), "OK") == null);
    try std.testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "../secret") != null);
}

test "verify rejects an absolute manifest component path" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try test_chdir(&tmp);
    defer std.testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try zigts.file_io.writeFile(std.testing.allocator, "secret", "outside-the-bundle");
    const cwd = try std.process.currentPathAlloc(std.testing.io, std.testing.allocator);
    defer std.testing.allocator.free(cwd);
    const abs_secret = try std.fs.path.join(std.testing.allocator, &.{ cwd, "secret" });
    defer std.testing.allocator.free(abs_secret);

    try ensureDir(std.testing.allocator, "bundle");
    const secret_sha = sha256Hex("outside-the-bundle");
    const manifest = try std.fmt.allocPrint(
        std.testing.allocator,
        "{{\n  \"components\": {{\n    \"contract\": {{ \"path\": \"{s}\", \"sha256\": \"{s}\" }}\n  }}\n}}\n",
        .{ abs_secret, secret_sha },
    );
    defer std.testing.allocator.free(manifest);
    try zigts.file_io.writeFile(std.testing.allocator, "bundle/bundle.json", manifest);

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer err.deinit();
    try std.testing.expectError(error.SuspiciousPath, verify(std.testing.allocator, "bundle", &out.writer, &err.writer));
    try std.testing.expect(std.mem.indexOf(u8, out.writer.buffered(), "OK") == null);
    try std.testing.expect(std.mem.indexOf(u8, err.writer.buffered(), abs_secret) != null);
}

test "verify passes a bundle written by writeBundle" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try test_chdir(&tmp);
    defer std.testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try zigts.file_io.writeFile(std.testing.allocator, "contract.json", "{\"routes\":[]}");

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer err.deinit();
    try writeBundle(std.testing.allocator, .{
        .contract_path = "contract.json",
        .out_dir = "bundle",
    }, &out.writer, &err.writer);

    var verify_out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer verify_out.deinit();
    var verify_err = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer verify_err.deinit();
    try verify(std.testing.allocator, "bundle", &verify_out.writer, &verify_err.writer);
    const text = verify_out.writer.buffered();
    try std.testing.expect(std.mem.indexOf(u8, text, "OK") != null);
    try std.testing.expect(std.mem.indexOf(u8, text, "Bundle verified") != null);
}
