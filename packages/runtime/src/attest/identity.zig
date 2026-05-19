//! Slice 2 of proof receipts (see docs/roadmap/attest-slice-2.md).
//!
//! Persistent organisation-level Ed25519 keypair. Slice 1 minted an ephemeral
//! key per build; slice 2 loads or generates a stable key from
//! `~/.zigttp/attest/keypair.bin`, so every deploy by the same operator
//! attests under the same identity that verifiers can pin once and trust
//! across future builds.
//!
//! File format: 64 raw bytes - the 32-byte Ed25519 seed followed by the
//! 32-byte derived public key. No header, no encoding, no encryption.
//! Mode 0600 enforced on both write and load. Encryption, hardware-backed
//! custody, and rotation CLI land in slice 3.

const std = @import("std");
const Ed25519 = std.crypto.sign.Ed25519;
const Sha256 = std.crypto.hash.sha2.Sha256;

/// Named tags this module surfaces. Callers can pattern-match these for
/// operator-facing diagnostics; everything else (raw POSIX errors from open,
/// read, write, mkdirat) flows through the inferred return type unchanged.
pub const KeyError = error{
    HomeDirUnavailable,
    KeyFileMalformed,
    KeyFilePermissionsTooOpen,
    UrandomOpenFailed,
    UrandomReadFailed,
    WriteFailed,
    OutOfMemory,
};

pub const KeySource = enum { generated, loaded };

pub const SignerIdentity = struct {
    key_pair: Ed25519.KeyPair,
    fingerprint_hex: [64]u8,
    source: KeySource,
};

const file_size_bytes: usize = Ed25519.KeyPair.seed_length + Ed25519.PublicKey.encoded_length;
const file_mode: std.posix.mode_t = 0o600;
/// Permission bits below the user-rwx triple. Any one of these set on the
/// key file means group or other can read or modify the private key.
const permission_group_bits: std.posix.mode_t = 0o077;

/// Default user-scoped entry. Resolves `$HOME/.zigttp/attest/keypair.bin`,
/// loads on cache hit, generates and writes on cache miss.
pub fn loadOrCreate(allocator: std.mem.Allocator) !SignerIdentity {
    const home_raw = std.c.getenv("HOME") orelse return error.HomeDirUnavailable;
    const home = std.mem.sliceTo(home_raw, 0);

    const dir_path = try std.fs.path.join(allocator, &.{ home, ".zigttp", "attest" });
    defer allocator.free(dir_path);

    const file_path = try std.fs.path.join(allocator, &.{ dir_path, "keypair.bin" });
    defer allocator.free(file_path);

    return loadOrCreateAt(dir_path, file_path);
}

/// Lower-level entry that takes explicit paths; lets tests target a tmpdir.
pub fn loadOrCreateAt(dir_path: []const u8, file_path: []const u8) !SignerIdentity {
    if (loadExisting(file_path)) |id| {
        return id;
    } else |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    }
    try ensureDirectory(dir_path);
    return generate(file_path);
}

fn loadExisting(file_path: []const u8) !SignerIdentity {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path_z = try toCStr(&path_buf, file_path);

    const fd = std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        else => return err,
    };
    defer std.Io.Threaded.closeFd(fd);

    var stat: std.c.Stat = undefined;
    if (std.c.fstat(fd, &stat) != 0) return error.KeyFileMalformed;
    if (stat.size != file_size_bytes) return error.KeyFileMalformed;
    if (stat.mode & permission_group_bits != 0) return error.KeyFilePermissionsTooOpen;

    var bytes: [file_size_bytes]u8 = undefined;
    var filled: usize = 0;
    while (filled < bytes.len) {
        const n = try std.posix.read(fd, bytes[filled..]);
        if (n == 0) return error.KeyFileMalformed;
        filled += n;
    }

    return buildIdentity(bytes[0..Ed25519.KeyPair.seed_length].*, .loaded);
}

fn generate(file_path: []const u8) !SignerIdentity {
    var seed: [Ed25519.KeyPair.seed_length]u8 = undefined;
    try fillCsprng(&seed);

    const identity = try buildIdentity(seed, .generated);

    var bytes: [file_size_bytes]u8 = undefined;
    @memcpy(bytes[0..Ed25519.KeyPair.seed_length], &seed);
    @memcpy(bytes[Ed25519.KeyPair.seed_length..], &identity.key_pair.public_key.bytes);

    try writeFileStrict(file_path, &bytes);
    return identity;
}

fn buildIdentity(seed: [Ed25519.KeyPair.seed_length]u8, source: KeySource) !SignerIdentity {
    const key_pair = Ed25519.KeyPair.generateDeterministic(seed) catch return error.KeyFileMalformed;
    var digest: [32]u8 = undefined;
    Sha256.hash(&key_pair.public_key.bytes, &digest, .{});
    return .{
        .key_pair = key_pair,
        .fingerprint_hex = std.fmt.bytesToHex(digest, .lower),
        .source = source,
    };
}

fn ensureDirectory(path: []const u8) !void {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path_z = try toCStr(&path_buf, path);

    // mkdirat with FDCWD on an absolute path is the portable equivalent of
    // `mkdir(absolute)`. Recurse into the parent on ENOENT so the call
    // emulates `mkdir -p` without a Threaded IO surface (the dev CLI runs
    // this before any io_backend is initialised).
    switch (std.posix.errno(std.posix.system.mkdirat(std.posix.AT.FDCWD, path_z, 0o755))) {
        .SUCCESS, .EXIST => return,
        .NOENT => {
            const parent = std.fs.path.dirname(path) orelse return error.FileNotFound;
            try ensureDirectory(parent);
            switch (std.posix.errno(std.posix.system.mkdirat(std.posix.AT.FDCWD, path_z, 0o755))) {
                .SUCCESS, .EXIST => return,
                else => |e| return std.posix.unexpectedErrno(e),
            }
        },
        else => |e| return std.posix.unexpectedErrno(e),
    }
}

fn writeFileStrict(file_path: []const u8, bytes: []const u8) !void {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path_z = try toCStr(&path_buf, file_path);

    const fd = try std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        file_mode,
    );
    defer std.Io.Threaded.closeFd(fd);

    var total: usize = 0;
    while (total < bytes.len) {
        const n = std.c.write(fd, bytes[total..].ptr, bytes.len - total);
        if (n <= 0) return error.WriteFailed;
        total += @intCast(n);
    }

    // openat honours mode on first creation but the user's umask may have
    // stripped permissions further. An explicit chmod restores the strict
    // 0600; on an already-tight file it is a no-op.
    _ = std.c.chmod(path_z, file_mode);
}

fn toCStr(buf: *[std.fs.max_path_bytes]u8, path: []const u8) ![:0]const u8 {
    if (path.len >= buf.len) return error.NameTooLong;
    @memcpy(buf[0..path.len], path);
    buf[path.len] = 0;
    return buf[0..path.len :0];
}

fn fillCsprng(buf: *[Ed25519.KeyPair.seed_length]u8) !void {
    const fd = std.c.open("/dev/urandom", .{ .ACCMODE = .RDONLY }, @as(std.c.mode_t, 0));
    if (fd < 0) return error.UrandomOpenFailed;
    defer _ = std.c.close(fd);
    var filled: usize = 0;
    while (filled < buf.len) {
        const n = std.c.read(fd, buf[filled..].ptr, buf.len - filled);
        if (n <= 0) return error.UrandomReadFailed;
        filled += @intCast(n);
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn tmpKeyFile(allocator: std.mem.Allocator, tmp: std.testing.TmpDir) ![2][]const u8 {
    // std.testing.tmpDir creates `.zig-cache/tmp/<sub_path>` under cwd, but
    // exposes only an `Io.Dir` and the sub_path string. Reconstruct the
    // absolute path from cwd so the absolute-path file ops in this module
    // can reach the tmpdir.
    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.c.getcwd(&cwd_buf, cwd_buf.len) == null) return error.CwdUnavailable;
    const cwd = std.mem.sliceTo(&cwd_buf, 0);
    const dir_path = try std.fs.path.join(allocator, &.{ cwd, ".zig-cache", "tmp", &tmp.sub_path });
    const file_path = try std.fs.path.join(allocator, &.{ dir_path, "keypair.bin" });
    return .{ dir_path, file_path };
}

test "first call generates a key; second call loads the same fingerprint" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const paths = try tmpKeyFile(allocator, tmp);
    defer allocator.free(paths[0]);
    defer allocator.free(paths[1]);

    const first = try loadOrCreateAt(paths[0], paths[1]);
    try testing.expectEqual(KeySource.generated, first.source);

    const second = try loadOrCreateAt(paths[0], paths[1]);
    try testing.expectEqual(KeySource.loaded, second.source);
    try testing.expectEqualSlices(u8, &first.fingerprint_hex, &second.fingerprint_hex);
    try testing.expectEqualSlices(u8, &first.key_pair.public_key.bytes, &second.key_pair.public_key.bytes);
}

test "generated file is exactly 64 bytes" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const paths = try tmpKeyFile(allocator, tmp);
    defer allocator.free(paths[0]);
    defer allocator.free(paths[1]);

    _ = try loadOrCreateAt(paths[0], paths[1]);

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path_z = try toCStr(&path_buf, paths[1]);
    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.Io.Threaded.closeFd(fd);
    var stat: std.c.Stat = undefined;
    try testing.expect(std.c.fstat(fd, &stat) == 0);
    try testing.expectEqual(@as(@TypeOf(stat.size), file_size_bytes), stat.size);
}

test "loading a file with permissions wider than 0600 is rejected" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const paths = try tmpKeyFile(allocator, tmp);
    defer allocator.free(paths[0]);
    defer allocator.free(paths[1]);

    _ = try loadOrCreateAt(paths[0], paths[1]);

    const path_z = try allocator.dupeZ(u8, paths[1]);
    defer allocator.free(path_z);
    _ = std.c.chmod(path_z, 0o644);

    try testing.expectError(error.KeyFilePermissionsTooOpen, loadOrCreateAt(paths[0], paths[1]));
}

test "loading a too-short file is rejected as malformed" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const paths = try tmpKeyFile(allocator, tmp);
    defer allocator.free(paths[0]);
    defer allocator.free(paths[1]);

    try writeFileStrict(paths[1], "only thirty bytes which is not enough");

    try testing.expectError(error.KeyFileMalformed, loadOrCreateAt(paths[0], paths[1]));
}

test "fingerprint equals sha256 of public key, lowercase hex" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const paths = try tmpKeyFile(allocator, tmp);
    defer allocator.free(paths[0]);
    defer allocator.free(paths[1]);

    const id = try loadOrCreateAt(paths[0], paths[1]);

    var digest: [32]u8 = undefined;
    Sha256.hash(&id.key_pair.public_key.bytes, &digest, .{});
    const expected = std.fmt.bytesToHex(digest, .lower);
    try testing.expectEqualSlices(u8, &expected, &id.fingerprint_hex);
}
