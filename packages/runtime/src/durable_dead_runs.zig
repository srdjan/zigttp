//! Dead-run records for durable executions that permanently fail recovery.
//!
//! `durable_recovery.zig`'s `RetryTracker` quarantines a run in memory after
//! `RetryTracker.quarantine_threshold` consecutive recovery failures, but
//! that in-memory state is invisible and resets on every process restart.
//! This module persists a small record alongside the oplog (never mutating
//! it) the moment a run first crosses that threshold, and gives operators
//! `list`/`show`/`replay`/`discard` verbs mirroring `workflow_queue.zig`'s
//! existing dead-letter API.
//!
//! Records live at `<durable_dir>/dead-runs/<id>.json`, where `id` is the
//! oplog basename minus its `.jsonl` suffix (e.g. `durable-1a2b3c4d`) - the
//! same hashed-key identity already used for the oplog filename itself, so
//! no new id scheme is introduced.
//!
//! `replay` deletes the record (the untouched oplog is retried on the next
//! recovery poll); `discard` rewrites it with `state: "discarded"` so the
//! run stays permanently unretried but is still on disk for forensic
//! inspection - matching this repo's "explicit, inspectable cleanup, never
//! automatic destructive retention" convention.

const std = @import("std");
const zq = @import("zigts");
const durable_store = @import("durable_store.zig");
const atomic_file = @import("atomic_file.zig");

const Allocator = std.mem.Allocator;
const appendEscaped = durable_store.appendEscaped;

const oplog_suffix = ".jsonl";

/// Derive a dead-run id from an oplog basename (e.g. `durable-1a2b3c4d.jsonl`
/// -> `durable-1a2b3c4d`). Returns `null` if `filename` doesn't look like an
/// oplog file. Borrows from `filename`; does not allocate.
pub fn deadRunId(filename: []const u8) ?[]const u8 {
    if (!std.mem.endsWith(u8, filename, oplog_suffix)) return null;
    return filename[0 .. filename.len - oplog_suffix.len];
}

pub fn isValidDeadRunId(id: []const u8) bool {
    if (id.len == 0) return false;
    for (id) |ch| {
        if (std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '_') continue;
        return false;
    }
    return true;
}

const DeadRunPaths = struct {
    allocator: Allocator,
    dir: []u8,
    file: []u8,
    lock_file: []u8,

    fn deinit(self: *DeadRunPaths) void {
        self.allocator.free(self.dir);
        self.allocator.free(self.file);
        self.allocator.free(self.lock_file);
        self.* = undefined;
    }
};

fn deadRunPaths(allocator: Allocator, durable_dir: []const u8, id: []const u8) !DeadRunPaths {
    if (!isValidDeadRunId(id)) return error.InvalidDeadRunId;
    const dir = try std.fs.path.join(allocator, &.{ durable_dir, "dead-runs" });
    errdefer allocator.free(dir);
    const file_name = try std.fmt.allocPrint(allocator, "{s}.json", .{id});
    defer allocator.free(file_name);
    const file = try std.fs.path.join(allocator, &.{ dir, file_name });
    errdefer allocator.free(file);
    const lock_name = try std.fmt.allocPrint(allocator, "{s}.lock", .{id});
    defer allocator.free(lock_name);
    const lock_file = try std.fs.path.join(allocator, &.{ dir, lock_name });
    errdefer allocator.free(lock_file);
    return .{ .allocator = allocator, .dir = dir, .file = file, .lock_file = lock_file };
}

/// Serializes `writeDeadRun`/`replayDeadRun`/`discardDeadRun` against each
/// other for the same id, across processes. Without this, two concurrent
/// CLI invocations (or a CLI invocation racing the server's own write) each
/// do an unlocked read-check-act sequence, so one operator's reported
/// success can be silently reversed by the other. The lock lives in a
/// stable sidecar file that replay/discard never rename or delete - an
/// `flock` on the record file itself would have a gap here, since
/// `discardDeadRun`'s atomic rename swaps in a new inode that a
/// concurrently-held lock on the old fd would no longer protect.
pub const RecordLock = struct {
    fd: std.c.fd_t,

    pub fn acquire(dir: []const u8, lock_path: []const u8) !RecordLock {
        try atomic_file.ensureDir(dir);
        const z = try std.heap.c_allocator.dupeZ(u8, lock_path);
        defer std.heap.c_allocator.free(z);
        const fd = std.posix.openatZ(
            std.posix.AT.FDCWD,
            z,
            .{ .ACCMODE = .WRONLY, .CREAT = true },
            0o600,
        ) catch return error.LockOpenFailed;
        if (std.c.flock(fd, std.posix.LOCK.EX) != 0) {
            std.Io.Threaded.closeFd(fd);
            return error.LockFailed;
        }
        return .{ .fd = fd };
    }

    pub fn release(self: *RecordLock) void {
        _ = std.c.flock(self.fd, std.posix.LOCK.UN);
        std.Io.Threaded.closeFd(self.fd);
    }
};

fn fileExists(path: []const u8) !bool {
    const z = try std.heap.c_allocator.dupeZ(u8, path);
    defer std.heap.c_allocator.free(z);
    const rc = std.c.access(z, std.c.F_OK);
    if (rc == 0) return true;
    return switch (std.posix.errno(rc)) {
        .NOENT => false,
        else => error.FileExistenceCheckFailed,
    };
}

/// Like `atomic_file.deleteIfExists`, but for callers (e.g. `replayDeadRun`)
/// that must not report success while the record is still on disk - a
/// swallowed unlink failure here would leave `hasDeadRun` seeing the file on
/// the very next recovery poll even though the CLI already told the operator
/// replay succeeded.
fn deleteChecked(path: []const u8) !void {
    const z = try std.heap.c_allocator.dupeZ(u8, path);
    defer std.heap.c_allocator.free(z);
    if (std.c.unlink(z) != 0) return error.DeleteFailed;
}

/// True if a dead-run record exists for `id`, regardless of state
/// (quarantined or discarded). The recovery loop uses this to skip a run
/// entirely, and to detect that a standing quarantine was replayed out from
/// under it (by a separate `durable dead-runs replay` invocation).
pub fn hasDeadRun(allocator: Allocator, durable_dir: []const u8, id: []const u8) !bool {
    var paths = deadRunPaths(allocator, durable_dir, id) catch |err| switch (err) {
        error.InvalidDeadRunId => return false,
        else => return err,
    };
    defer paths.deinit();
    return try fileExists(paths.file);
}

fn encodeRecord(
    allocator: Allocator,
    state: []const u8,
    run_key: []const u8,
    oplog_filename: []const u8,
    reason: []const u8,
    quarantined_at_ms: i64,
    consecutive_failures: u32,
) ![]u8 {
    var payload: std.ArrayList(u8) = .empty;
    errdefer payload.deinit(allocator);
    try payload.appendSlice(allocator, "{\"state\":\"");
    try appendEscaped(&payload, allocator, state);
    try payload.appendSlice(allocator, "\",\"run_key\":\"");
    try appendEscaped(&payload, allocator, run_key);
    try payload.appendSlice(allocator, "\",\"oplog_filename\":\"");
    try appendEscaped(&payload, allocator, oplog_filename);
    try payload.appendSlice(allocator, "\",\"reason\":\"");
    try appendEscaped(&payload, allocator, reason);
    try payload.appendSlice(allocator, "\",\"quarantined_at_ms\":");
    var int_buf: [32]u8 = undefined;
    try payload.appendSlice(allocator, try std.fmt.bufPrint(&int_buf, "{d}", .{quarantined_at_ms}));
    try payload.appendSlice(allocator, ",\"consecutive_failures\":");
    try payload.appendSlice(allocator, try std.fmt.bufPrint(&int_buf, "{d}", .{consecutive_failures}));
    try payload.appendSlice(allocator, "}");
    return payload.toOwnedSlice(allocator);
}

/// Persist a dead-run record the moment a run first crosses
/// `RetryTracker.quarantine_threshold`. Idempotent in effect (an accidental
/// repeat write just overwrites with equivalent content); the recovery loop
/// only calls this on the crossing poll, guarded by `RetryTracker`'s return
/// value, so in practice it fires exactly once per quarantine event.
pub fn writeDeadRun(
    allocator: Allocator,
    durable_dir: []const u8,
    id: []const u8,
    run_key: []const u8,
    oplog_filename: []const u8,
    reason: []const u8,
    quarantined_at_ms: i64,
    consecutive_failures: u32,
) !void {
    var paths = try deadRunPaths(allocator, durable_dir, id);
    defer paths.deinit();
    var lock = try RecordLock.acquire(paths.dir, paths.lock_file);
    defer lock.release();
    const payload = try encodeRecord(
        allocator,
        "quarantined",
        run_key,
        oplog_filename,
        reason,
        quarantined_at_ms,
        consecutive_failures,
    );
    defer allocator.free(payload);
    try atomic_file.writeFileAtomic(allocator, paths.file, payload);
}

/// Raw JSON for a dead-run record, or `null` if none exists. Used by `show`.
pub fn readDeadRun(allocator: Allocator, durable_dir: []const u8, id: []const u8) !?[]u8 {
    var paths = try deadRunPaths(allocator, durable_dir, id);
    defer paths.deinit();
    if (!(try fileExists(paths.file))) return null;
    return try zq.file_io.readFile(allocator, paths.file, 64 * 1024);
}

fn readState(allocator: Allocator, path: []const u8) !?[]const u8 {
    if (!(try fileExists(path))) return null;
    const source = try zq.file_io.readFile(allocator, path, 64 * 1024);
    defer allocator.free(source);
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, source, .{}) catch return error.InvalidDeadRunRecord;
    defer parsed.deinit();
    const obj = switch (parsed.value) {
        .object => |o| o,
        else => return error.InvalidDeadRunRecord,
    };
    const state = switch (obj.get("state") orelse return error.InvalidDeadRunRecord) {
        .string => |s| s,
        else => return error.InvalidDeadRunRecord,
    };
    return try allocator.dupe(u8, state);
}

/// List dead-run ids that are still `quarantined` (discarded records are
/// intentionally excluded - they no longer need operator attention).
/// Caller owns the returned slice and each id.
pub fn listDeadRunIds(allocator: Allocator, durable_dir: []const u8) ![][]u8 {
    const dir_path = try std.fs.path.join(allocator, &.{ durable_dir, "dead-runs" });
    defer allocator.free(dir_path);

    const dir_z = try allocator.dupeZ(u8, dir_path);
    defer allocator.free(dir_z);
    const c = @cImport({
        @cInclude("dirent.h");
    });
    const dir = c.opendir(dir_z) orelse return &[_][]u8{};
    defer _ = c.closedir(dir);

    var out: std.ArrayList([]u8) = .empty;
    errdefer {
        for (out.items) |id| allocator.free(id);
        out.deinit(allocator);
    }

    while (c.readdir(dir)) |entry| {
        const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
        const name = std.mem.sliceTo(name_ptr, 0);
        if (!std.mem.endsWith(u8, name, ".json")) continue;
        const id = name[0 .. name.len - ".json".len];

        const file_path = try std.fs.path.join(allocator, &.{ dir_path, name });
        defer allocator.free(file_path);
        // A single malformed record must not hide every other quarantined
        // run from the operator - skip it and keep scanning instead of
        // letting the error propagate out of the whole directory listing.
        const state = (readState(allocator, file_path) catch continue) orelse continue;
        defer allocator.free(state);
        if (!std.mem.eql(u8, state, "quarantined")) continue;

        try out.append(allocator, try allocator.dupe(u8, id));
    }

    std.mem.sort([]u8, out.items, {}, lessThanBytes);
    return try out.toOwnedSlice(allocator);
}

fn lessThanBytes(_: void, lhs: []u8, rhs: []u8) bool {
    return std.mem.lessThan(u8, lhs, rhs);
}

/// Clear a dead-run record so the next recovery poll retries the run from
/// its untouched oplog. Fails if the record is missing or already
/// discarded - `discard` is a deliberate terminal decision, not silently
/// reversible via `replay`.
pub fn replayDeadRun(allocator: Allocator, durable_dir: []const u8, id: []const u8) !void {
    var paths = try deadRunPaths(allocator, durable_dir, id);
    defer paths.deinit();
    var lock = try RecordLock.acquire(paths.dir, paths.lock_file);
    defer lock.release();
    const state = (try readState(allocator, paths.file)) orelse return error.DeadRunMissing;
    defer allocator.free(state);
    if (!std.mem.eql(u8, state, "quarantined")) return error.DeadRunAlreadyDiscarded;
    try deleteChecked(paths.file);
}

/// Mark a dead-run record as permanently discarded. The record stays on
/// disk (for `show`/audit) but the run is never retried again and the
/// record no longer appears in `list`. The oplog is never touched.
pub fn discardDeadRun(allocator: Allocator, durable_dir: []const u8, id: []const u8) !void {
    var paths = try deadRunPaths(allocator, durable_dir, id);
    defer paths.deinit();
    var lock = try RecordLock.acquire(paths.dir, paths.lock_file);
    defer lock.release();
    const source = if (try fileExists(paths.file))
        try zq.file_io.readFile(allocator, paths.file, 64 * 1024)
    else
        return error.DeadRunMissing;
    defer allocator.free(source);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, source, .{}) catch return error.InvalidDeadRunRecord;
    defer parsed.deinit();
    const obj = switch (parsed.value) {
        .object => |o| o,
        else => return error.InvalidDeadRunRecord,
    };
    const state = switch (obj.get("state") orelse return error.InvalidDeadRunRecord) {
        .string => |s| s,
        else => return error.InvalidDeadRunRecord,
    };
    if (!std.mem.eql(u8, state, "quarantined")) return error.DeadRunAlreadyDiscarded;

    const run_key = switch (obj.get("run_key") orelse .null) {
        .string => |s| s,
        else => "",
    };
    const oplog_filename = switch (obj.get("oplog_filename") orelse .null) {
        .string => |s| s,
        else => "",
    };
    const reason = switch (obj.get("reason") orelse .null) {
        .string => |s| s,
        else => "",
    };
    const quarantined_at_ms: i64 = switch (obj.get("quarantined_at_ms") orelse .null) {
        .integer => |n| n,
        else => 0,
    };
    const consecutive_failures: u32 = switch (obj.get("consecutive_failures") orelse .null) {
        .integer => |n| @intCast(n),
        else => 0,
    };

    const payload = try encodeRecord(
        allocator,
        "discarded",
        run_key,
        oplog_filename,
        reason,
        quarantined_at_ms,
        consecutive_failures,
    );
    defer allocator.free(payload);
    try atomic_file.writeFileAtomic(allocator, paths.file, payload);
}

test "deadRunId strips the .jsonl suffix" {
    try std.testing.expectEqualStrings("durable-1a2b3c4d", deadRunId("durable-1a2b3c4d.jsonl").?);
    try std.testing.expect(deadRunId("not-an-oplog.txt") == null);
}

test "hasDeadRun reports absent only for ENOENT" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const durable_dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(durable_dir);

    try std.testing.expect(!(try hasDeadRun(allocator, durable_dir, "durable-missing")));

    const dead_runs_path = try std.fs.path.join(allocator, &.{ durable_dir, "dead-runs" });
    defer allocator.free(dead_runs_path);
    try zq.file_io.writeFile(allocator, dead_runs_path, "not a directory");

    try std.testing.expectError(
        error.FileExistenceCheckFailed,
        hasDeadRun(allocator, durable_dir, "durable-unknown"),
    );
}

test "write, list, show, replay, discard round-trip" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const durable_dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(durable_dir);

    try writeDeadRun(allocator, durable_dir, "durable-abc", "job:1", "durable-abc.jsonl", "handler error: boom", 1000, 10);

    try std.testing.expect(try hasDeadRun(allocator, durable_dir, "durable-abc"));

    const ids = try listDeadRunIds(allocator, durable_dir);
    defer {
        for (ids) |id| allocator.free(id);
        allocator.free(ids);
    }
    try std.testing.expectEqual(@as(usize, 1), ids.len);
    try std.testing.expectEqualStrings("durable-abc", ids[0]);

    const shown = (try readDeadRun(allocator, durable_dir, "durable-abc")).?;
    defer allocator.free(shown);
    try std.testing.expect(std.mem.indexOf(u8, shown, "\"state\":\"quarantined\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, shown, "job:1") != null);

    try replayDeadRun(allocator, durable_dir, "durable-abc");
    try std.testing.expect(!(try hasDeadRun(allocator, durable_dir, "durable-abc")));
    try std.testing.expectError(error.DeadRunMissing, replayDeadRun(allocator, durable_dir, "durable-abc"));
}

test "RecordLock provides mutual exclusion for the same id across threads" {
    // Regression for a bug caught in code review: replayDeadRun and
    // discardDeadRun used to be plain unlocked read-check-act sequences, so
    // two concurrent CLI invocations against the same id could interleave
    // and silently reverse each other's reported success. This drives
    // RecordLock directly (rather than replay/discard) so the assertion is
    // about the lock's own mutual-exclusion property, not just a lucky
    // non-interleaved outcome.
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const durable_dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(durable_dir);

    var paths = try deadRunPaths(allocator, durable_dir, "durable-lock-test");
    defer paths.deinit();

    var lock = try RecordLock.acquire(paths.dir, paths.lock_file);

    var released = std.atomic.Value(bool).init(false);
    var acquired_after_release = std.atomic.Value(bool).init(false);

    const Waiter = struct {
        fn run(dir: []const u8, lock_path: []const u8, released_flag: *std.atomic.Value(bool), result_flag: *std.atomic.Value(bool)) void {
            var waiter_lock = RecordLock.acquire(dir, lock_path) catch return;
            // If this blocking acquire returns before the holder released,
            // the lock failed to serialize the two sides.
            result_flag.store(released_flag.load(.seq_cst), .seq_cst);
            waiter_lock.release();
        }
    };

    const thread = try std.Thread.spawn(.{}, Waiter.run, .{ paths.dir, paths.lock_file, &released, &acquired_after_release });
    std.Io.sleep(std.testing.io, .fromMilliseconds(50), .awake) catch {};
    released.store(true, .seq_cst);
    lock.release();
    thread.join();

    try std.testing.expect(acquired_after_release.load(.seq_cst));
}

test "discard keeps the record but stops it from listing or replaying" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const durable_dir = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(durable_dir);

    try writeDeadRun(allocator, durable_dir, "durable-xyz", "job:2", "durable-xyz.jsonl", "parse error", 2000, 10);
    try discardDeadRun(allocator, durable_dir, "durable-xyz");

    // Still present on disk (hasDeadRun keeps blocking retry)...
    try std.testing.expect(try hasDeadRun(allocator, durable_dir, "durable-xyz"));
    // ...but no longer listed as needing operator attention...
    const ids = try listDeadRunIds(allocator, durable_dir);
    defer {
        for (ids) |id| allocator.free(id);
        allocator.free(ids);
    }
    try std.testing.expectEqual(@as(usize, 0), ids.len);
    // ...and cannot be silently un-discarded via replay.
    try std.testing.expectError(error.DeadRunAlreadyDiscarded, replayDeadRun(allocator, durable_dir, "durable-xyz"));
    try std.testing.expectError(error.DeadRunAlreadyDiscarded, discardDeadRun(allocator, durable_dir, "durable-xyz"));
}
