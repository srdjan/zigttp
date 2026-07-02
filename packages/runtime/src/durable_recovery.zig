//! Durable Execution Recovery
//!
//! On server startup, scans the durable directory for incomplete oplog files
//! and re-executes their handlers. The handler's run(key, ...) call reuses the
//! recorded oplog, replays deterministic effects, and continues live for any
//! remaining work.
//!
//! An oplog is incomplete if it contains a request entry but no "complete" marker.
//! Completed oplogs are kept so duplicate durable keys can reuse the recorded
//! response.

const std = @import("std");
const builtin = @import("builtin");
const zq = @import("zigts");
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;
const Runtime = @import("zruntime.zig").Runtime;
const HttpRequestView = @import("http_types.zig").HttpRequestView;
const HttpHeader = @import("http_types.zig").HttpHeader;
const ServerConfig = @import("server.zig").ServerConfig;
const tryLockOplogFd = @import("runtime_config.zig").tryLockOplogFd;
const handler_loader = @import("handler_loader.zig");
const durable_executor = @import("durable_executor.zig");
const DurableStore = @import("durable_store.zig").DurableStore;
const runtime_natives = @import("runtime_natives.zig");
const retry_backoff = @import("retry_backoff.zig");
const dead_runs = @import("durable_dead_runs.zig");

const c = @cImport({
    @cInclude("dirent.h");
});

const trace = zq.trace;

/// Per-oplog retry state for the polling scheduler. A run whose recovery
/// FAILS (handler error, unreadable log) backs off exponentially and is
/// quarantined after repeated failures, instead of hot-retrying at the poll
/// rate forever. Runs that are merely still suspended on a wait are neutral:
/// backing those off would delay their resumption.
pub const RetryTracker = struct {
    const quarantine_threshold: u32 = 10;
    const base_backoff_ms: i64 = 1_000;
    const max_backoff_ms: i64 = 60_000;

    const Entry = struct {
        consecutive_failures: u32 = 0,
        next_attempt_ms: i64 = 0,
    };

    allocator: std.mem.Allocator,
    map: std.StringHashMapUnmanaged(Entry) = .empty,

    pub fn init(allocator: std.mem.Allocator) RetryTracker {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *RetryTracker) void {
        var it = self.map.iterator();
        while (it.next()) |entry| self.allocator.free(entry.key_ptr.*);
        self.map.deinit(self.allocator);
    }

    pub fn shouldAttempt(self: *const RetryTracker, name: []const u8, now_ms: i64) bool {
        const entry = self.map.get(name) orelse return true;
        if (entry.consecutive_failures >= quarantine_threshold) return false;
        return now_ms >= entry.next_attempt_ms;
    }

    /// True when `name`'s in-memory state is at or past quarantine. Used to
    /// double-check a persisted dead-run record still backs the quarantine
    /// before honoring it - the record may have been replayed out from
    /// under this process by a separate `durable dead-runs replay`
    /// invocation, which can only delete the file, not reach into this
    /// process's in-memory map.
    pub fn isQuarantined(self: *const RetryTracker, name: []const u8) bool {
        const entry = self.map.get(name) orelse return false;
        return entry.consecutive_failures >= quarantine_threshold;
    }

    /// Records a failure. Returns `true` exactly on the call that first
    /// crosses `quarantine_threshold` (the caller uses this to persist a
    /// dead-run record exactly once per quarantine event, not on every
    /// subsequent poll).
    pub fn recordFailure(self: *RetryTracker, name: []const u8, now_ms: i64) bool {
        const gop = self.map.getOrPut(self.allocator, name) catch return false;
        if (!gop.found_existing) {
            gop.key_ptr.* = self.allocator.dupe(u8, name) catch {
                _ = self.map.remove(name);
                return false;
            };
            gop.value_ptr.* = .{};
        }
        gop.value_ptr.consecutive_failures += 1;
        if (gop.value_ptr.consecutive_failures >= quarantine_threshold) {
            const just_crossed = gop.value_ptr.consecutive_failures == quarantine_threshold;
            if (just_crossed and !builtin.is_test) {
                std.log.warn(
                    "Durable run '{s}' quarantined after {d} consecutive recovery failures",
                    .{ name, gop.value_ptr.consecutive_failures },
                );
            }
            return just_crossed;
        }
        const attempt = gop.value_ptr.consecutive_failures - 1;
        var seed = retry_backoff.seedBytes(name, 0x44555241424c4552);
        seed = retry_backoff.mix(seed, @intCast(gop.value_ptr.consecutive_failures));
        seed = retry_backoff.mix(seed, @bitCast(now_ms));
        const backoff = retry_backoff.retryDelayMs(base_backoff_ms, max_backoff_ms, attempt, seed);
        gop.value_ptr.next_attempt_ms = now_ms + backoff;
        return false;
    }

    pub fn clear(self: *RetryTracker, name: []const u8) void {
        if (self.map.fetchRemove(name)) |removed| {
            self.allocator.free(removed.key);
        }
    }
};

/// Scan the durable directory and recover any incomplete runs.
/// Returns the number of runs recovered.
pub fn recoverIncompleteOplogs(allocator: std.mem.Allocator, config: ServerConfig) !u32 {
    return recoverIncompleteOplogsTracked(allocator, config, null);
}

/// Like `recoverIncompleteOplogs`, with per-oplog retry backoff state for
/// the polling scheduler.
pub fn recoverIncompleteOplogsTracked(
    allocator: std.mem.Allocator,
    config: ServerConfig,
    tracker: ?*RetryTracker,
) !u32 {
    const oplog_dir = config.runtime_config.durable_oplog_dir orelse return 0;

    const dir_path_z = try allocator.dupeZ(u8, oplog_dir);
    defer allocator.free(dir_path_z);

    const dir = c.opendir(dir_path_z) orelse {
        // Directory doesn't exist yet - nothing to recover
        return 0;
    };
    defer _ = c.closedir(dir);

    // Collect oplog filenames
    var oplog_files: std.ArrayList([]const u8) = .empty;
    defer {
        for (oplog_files.items) |name| allocator.free(name);
        oplog_files.deinit(allocator);
    }

    while (c.readdir(dir)) |entry| {
        const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
        const name = std.mem.sliceTo(name_ptr, 0);
        if (!std.mem.startsWith(u8, name, "durable-")) continue;
        if (!std.mem.endsWith(u8, name, ".jsonl")) continue;
        try oplog_files.append(allocator, try allocator.dupe(u8, name));
    }

    if (oplog_files.items.len == 0) return 0;

    var recovered: u32 = 0;

    for (oplog_files.items) |filename| {
        // Build full path. Sized for PATH_MAX-class paths so a long oplog
        // directory does not silently drop a recoverable run.
        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ oplog_dir, filename }) catch continue;

        // Check the persisted dead-run record before consulting the
        // in-memory tracker at all. The in-memory RetryTracker alone is not
        // authoritative: it resets to empty on every process restart -
        // exactly the gap this record closes - so a restarted process must
        // not silently forget a standing quarantine just because its map is
        // fresh. This costs one cheap stat-style check per oplog per poll
        // (there is no cache-before-I/O shortcut that stays correct across
        // restarts), but `hasDeadRun` on the common case (no record) is a
        // single fast syscall, not a read.
        if (tracker) |t| {
            const id = dead_runs.deadRunId(filename) orelse filename;
            const is_dead = dead_runs.hasDeadRun(allocator, oplog_dir, id) catch true; // fail closed: an I/O error looks like "still dead" rather than retrying blind
            if (is_dead) continue;
            // No persisted record, but this process's own map still thinks
            // it's quarantined: an operator ran `durable dead-runs replay`
            // in a separate process, which can only delete the file - it
            // cannot reach into this process's in-memory RetryTracker.
            // Resync now that the record backing that quarantine is gone.
            if (t.isQuarantined(filename)) t.clear(filename);
            if (!t.shouldAttempt(filename, trace.unixMillis())) continue;
        }

        // Read oplog file
        const source = readFile(allocator, full_path) catch |err| {
            if (!builtin.is_test) std.log.err("Failed to read oplog '{s}': {}", .{ full_path, err });
            if (tracker) |t| {
                if (t.recordFailure(filename, trace.unixMillis())) {
                    writeDeadRunRecord(allocator, oplog_dir, filename, "", @errorName(err));
                }
            }
            continue;
        };
        defer allocator.free(source);

        if (!trace.isIncompleteOplog(source)) {
            // Completed durable runs remain on disk for duplicate-key reuse.
            continue;
        }

        // Parse the incomplete oplog
        var parsed = trace.parseIncompleteOplog(allocator, source) catch |err| {
            if (!builtin.is_test) std.log.err("Failed to parse oplog '{s}': {}", .{ full_path, err });
            if (tracker) |t| {
                if (t.recordFailure(filename, trace.unixMillis())) {
                    writeDeadRunRecord(allocator, oplog_dir, filename, "", @errorName(err));
                }
            }
            continue;
        };
        defer parsed.deinit();

        const run_key = parsed.run_key orelse {
            if (!builtin.is_test) std.log.err("Skipping oplog without durable run key: {s}", .{full_path});
            continue;
        };

        // A run suspended on a timer whose recorded deadline is still in the
        // future would provably re-suspend; skip the Runtime spin-up and
        // handler replay until the deadline passes.
        if (parsed.events.len > 0) {
            switch (parsed.events[parsed.events.len - 1]) {
                .wait_timer => |w| if (w.until_ms > trace.unixMillis()) continue,
                // A run blocked on waitSignal would provably re-suspend until a
                // matching signal arrives. Skip the Runtime spin-up + full oplog
                // replay (which otherwise repeats every poll with no backoff,
                // since .pending is neutral to the retry tracker) when no
                // matching signal is present yet.
                .wait_signal => |w| if (signalDefinitelyAbsent(allocator, oplog_dir, run_key, w.name)) {
                    if (w.timeout_ms) |deadline| {
                        if (deadline > trace.unixMillis()) continue;
                    } else continue;
                },
                else => {},
            }
        }

        std.log.info("Recovering durable run: {s} {s} {s} ({d} recorded events)", .{
            run_key,
            parsed.request.method,
            parsed.request.url,
            parsed.events.len,
        });

        var recover_err_name: []const u8 = "recovery failed";
        const outcome = recoverOne(allocator, config, &parsed, full_path) catch |err| blk: {
            if (!builtin.is_test) std.log.err("Recovery failed for '{s}': {}", .{ full_path, err });
            recover_err_name = @errorName(err);
            break :blk .failed;
        };

        switch (outcome) {
            .recovered => {
                recovered += 1;
                if (tracker) |t| t.clear(filename);
            },
            // Still suspended on a wait (or claimed elsewhere): neutral.
            .pending => {},
            .failed => if (tracker) |t| {
                if (t.recordFailure(filename, trace.unixMillis())) {
                    writeDeadRunRecord(allocator, oplog_dir, filename, run_key, recover_err_name);
                }
            },
        }
    }

    if (recovered > 0) {
        std.log.info("Recovered {d} incomplete request(s)", .{recovered});
    }

    return recovered;
}

/// Persist a dead-run record for `filename` right on the poll that crossed
/// `RetryTracker.quarantine_threshold`. Best-effort: a write failure here
/// must not crash the recovery loop or block quarantine from taking
/// effect in memory - it only means the record stays invisible to `durable
/// dead-runs list` until the next crossing (which, since the run is
/// already quarantined in memory, will not happen on its own; an operator
/// would need to notice via logs instead). This is a narrower failure mode
/// than the gap this feature closes, not a new one.
fn writeDeadRunRecord(
    allocator: std.mem.Allocator,
    oplog_dir: []const u8,
    filename: []const u8,
    run_key: []const u8,
    reason: []const u8,
) void {
    const id = dead_runs.deadRunId(filename) orelse filename;
    dead_runs.writeDeadRun(
        allocator,
        oplog_dir,
        id,
        run_key,
        filename,
        reason,
        trace.unixMillis(),
        RetryTracker.quarantine_threshold,
    ) catch |err| {
        if (!builtin.is_test) std.log.err("Failed to persist dead-run record for '{s}': {}", .{ filename, err });
    };
}

/// True only when a scan of the durable signal store succeeded and found no
/// signal matching (run_key, name). Fails safe: any scan error returns false so
/// the caller still attempts recovery (the prior always-recover behavior). A
/// scheduled (signalAt) signal whose time has not arrived is correctly treated
/// as absent - scanSignals filters it - so the run is skipped until it is due.
fn signalDefinitelyAbsent(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    run_key: []const u8,
    name: []const u8,
) bool {
    var store = DurableStore.initFs(allocator, durable_dir);
    const signals = store.scanRecoverableSignals(allocator, trace.unixMillis()) catch return false;
    defer {
        for (signals) |*s| s.deinit();
        allocator.free(signals);
    }
    for (signals) |s| {
        if (std.mem.eql(u8, s.key, run_key) and std.mem.eql(u8, s.name, name)) return false;
    }
    return true;
}

/// Exclusive non-blocking claim on a single oplog file, held for the duration
/// of a recovery re-execution. The scheduler re-runs `recoverIncompleteOplogs`
/// every second, so without a claim two passes (or a pass overlapping a live
/// request that still owns the oplog) could re-execute the same handler
/// concurrently and double-apply its effects. `flock(LOCK_EX | LOCK_NB)` fails
/// closed: if the lock is already held the second claimant skips the file. The
/// lock is advisory and released when the fd closes. The live request path
/// takes the same lock on its own oplog fd (zruntime.openActiveDurableRun via
/// runtime_config.tryLockOplogFd), so a recovery pass and a live run on the
/// same key exclude each other in both directions.
const OplogClaim = struct {
    fd: c_int,

    /// Try to claim `path`. Returns null when the lock is already held by
    /// another recovery pass (skip this oplog) or the file cannot be opened.
    fn acquire(allocator: std.mem.Allocator, path: []const u8) ?OplogClaim {
        const path_z = allocator.dupeZ(u8, path) catch return null;
        defer allocator.free(path_z);
        const fd = std.c.open(path_z, .{ .ACCMODE = .RDONLY }, @as(std.c.mode_t, 0));
        if (fd < 0) return null;
        tryLockOplogFd(fd) catch {
            _ = std.c.close(fd);
            return null;
        };
        return .{ .fd = fd };
    }

    fn release(self: OplogClaim) void {
        // Closing the fd drops the advisory lock; an explicit LOCK.UN first
        // makes the release intent obvious and independent of close timing.
        _ = std.c.flock(self.fd, std.posix.LOCK.UN);
        _ = std.c.close(self.fd);
    }
};

const RecoverOutcome = enum { recovered, pending, failed };

fn recoverOne(
    allocator: std.mem.Allocator,
    config: ServerConfig,
    parsed: *const trace.IncompleteOplog,
    oplog_path: []const u8,
) !RecoverOutcome {
    // Claim the oplog before re-executing. If another pass already holds it,
    // skip rather than double-execute the handler.
    const claim = OplogClaim.acquire(allocator, oplog_path) orelse return .pending;
    defer claim.release();

    const recovery_config = config.runtime_config;

    const rt = try Runtime.init(allocator, recovery_config);
    defer rt.deinit();

    const loaded = handler_loader.load(allocator, config.handler) catch |err| {
        if (!builtin.is_test) {
            switch (err) {
                error.UnsupportedHandlerSource => std.log.err("Durable recovery requires a file_path or inline_code handler source", .{}),
                else => std.log.err("Durable recovery failed to load handler: {}", .{err}),
            }
        }
        return err;
    };
    const handler_code = loaded.code;
    const handler_filename = loaded.filename;
    defer allocator.free(handler_code);

    try rt.loadCode(handler_code, handler_filename);
    durable_executor.setPendingDurableRecovery(
        rt,
        parsed.run_key orelse return error.MissingRunKey,
        oplog_path,
        parsed.events,
    );

    // Build request from oplog
    var headers_list: std.ArrayListUnmanaged(HttpHeader) = .empty;
    defer headers_list.deinit(allocator);
    try parseHeadersFromJson(allocator, parsed.request.headers_json, &headers_list);
    const body = if (parsed.request.body) |raw|
        try trace.unescapeJson(allocator, raw)
    else
        null;
    defer if (body) |owned| allocator.free(owned);

    // Split req.path / req.query so a durable handler that routes on req.path or
    // reads req.query re-executes the same branch during recovery as it did live.
    const target = runtime_natives.parseRequestTarget(allocator, parsed.request.url);
    defer target.deinit(allocator);

    const request = HttpRequestView{
        .method = parsed.request.method,
        .url = parsed.request.url,
        .path = target.path,
        .query_params = target.params,
        .headers = headers_list,
        .body = body,
    };

    // Execute handler - durable wrappers will replay from oplog, then continue live
    var response = rt.executeHandler(request) catch |err| {
        if (!builtin.is_test) std.log.err("Recovery handler execution failed: {}", .{err});
        return .failed;
    };
    defer response.deinit();

    const updated = readFile(allocator, oplog_path) catch return .failed;
    defer allocator.free(updated);
    if (trace.isIncompleteOplog(updated)) {
        // A trailing un-resumed wait means the run legitimately suspended
        // again; anything else incomplete is a failed execution.
        return if (endsWithPendingWait(allocator, updated)) .pending else .failed;
    }

    std.log.info("Recovered: {s} {s} -> {d}", .{ parsed.request.method, parsed.request.url, response.status });
    return .recovered;
}

fn endsWithPendingWait(allocator: std.mem.Allocator, source: []const u8) bool {
    var parsed = trace.parseDurableOplog(allocator, source) catch return false;
    defer parsed.deinit();
    if (parsed.events.len == 0) return false;
    return switch (parsed.events[parsed.events.len - 1]) {
        .wait_timer, .wait_signal => true,
        else => false,
    };
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    return zq.file_io.readFile(allocator, path, 100 * 1024 * 1024);
}

const parseHeadersFromJson = @import("trace_helpers.zig").parseHeadersFromJson;

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "durable recovery: OplogClaim refuses a second concurrent claim on the same oplog" {
    // The scheduler re-runs recoverIncompleteOplogs every second. Two passes (or
    // a pass overlapping a live request) must not both re-execute the same oplog.
    // The exclusive non-blocking flock claim guarantees the second claimant is
    // turned away (skip), so the handler runs at most once per oplog at a time.
    const allocator = testing.allocator;

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    // A real file the claim can open and lock.
    {
        var f = try tmp.dir.createFile(io, "durable-key.jsonl", .{});
        f.close(io);
    }
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(io, &dir_buf);
    const oplog_path = try std.fmt.allocPrint(allocator, "{s}/durable-key.jsonl", .{dir_buf[0..dir_len]});
    defer allocator.free(oplog_path);

    // First pass claims the oplog.
    const first = OplogClaim.acquire(allocator, oplog_path) orelse
        return error.FirstClaimShouldSucceed;

    // A concurrent second pass must be refused while the first holds the lock.
    try testing.expect(OplogClaim.acquire(allocator, oplog_path) == null);

    // After the first pass releases, the oplog is claimable again.
    first.release();
    const second = OplogClaim.acquire(allocator, oplog_path) orelse
        return error.ReclaimAfterReleaseShouldSucceed;
    second.release();
}

test "durable recovery: OplogClaim.acquire returns null for a missing oplog file" {
    const allocator = testing.allocator;

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(io, &dir_buf);
    const missing = try std.fmt.allocPrint(allocator, "{s}/does-not-exist.jsonl", .{dir_buf[0..dir_len]});
    defer allocator.free(missing);
    try testing.expect(OplogClaim.acquire(allocator, missing) == null);
}

test "durable recovery: full path buffer fits oplog dir + filename beyond 512 bytes" {
    // Regression for the silently-dropped recovery: the path buffer used to be
    // a fixed [512]u8, so a long oplog directory plus a durable-*.jsonl
    // filename overflowed and bufPrint skipped the run with no log line.
    const oplog_dir = "a" ** 800;
    const filename = "durable-" ++ ("b" ** 64) ++ ".jsonl";

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const full_path = try std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ oplog_dir, filename });

    try testing.expect(full_path.len > 512);
    try testing.expect(std.mem.endsWith(u8, full_path, filename));
}

test "durable recovery: RetryTracker backs off and quarantines persistent failures" {
    var tracker = RetryTracker.init(testing.allocator);
    defer tracker.deinit();

    const name = "durable-abc.jsonl";
    try testing.expect(tracker.shouldAttempt(name, 0));

    // First failure: jittered 1s-cap backoff.
    _ = tracker.recordFailure(name, 0);
    var seed = retry_backoff.seedBytes(name, 0x44555241424c4552);
    seed = retry_backoff.mix(seed, 1);
    seed = retry_backoff.mix(seed, @as(u64, @bitCast(@as(i64, 0))));
    const first_delay = retry_backoff.retryDelayMs(RetryTracker.base_backoff_ms, RetryTracker.max_backoff_ms, 0, seed);
    try testing.expect(first_delay >= 500);
    try testing.expect(first_delay <= 1_000);
    try testing.expect(!tracker.shouldAttempt(name, first_delay - 1));
    try testing.expect(tracker.shouldAttempt(name, first_delay));

    _ = tracker.recordFailure(name, 1_000);
    seed = retry_backoff.seedBytes(name, 0x44555241424c4552);
    seed = retry_backoff.mix(seed, 2);
    seed = retry_backoff.mix(seed, @as(u64, @bitCast(@as(i64, 1_000))));
    const second_delay = retry_backoff.retryDelayMs(RetryTracker.base_backoff_ms, RetryTracker.max_backoff_ms, 1, seed);
    try testing.expect(second_delay >= 1_000);
    try testing.expect(second_delay <= 2_000);
    try testing.expect(!tracker.shouldAttempt(name, 1_000 + second_delay - 1));
    try testing.expect(tracker.shouldAttempt(name, 1_000 + second_delay));

    // Backoff still caps at 60s before jitter chooses inside the cap.
    for (0..5) |_| _ = tracker.recordFailure(name, 10_000);
    try testing.expect(!tracker.shouldAttempt(name, 10_000 + 29_999));
    try testing.expect(tracker.shouldAttempt(name, 10_000 + 60_000));

    // Success clears the entry entirely.
    tracker.clear(name);
    try testing.expect(tracker.shouldAttempt(name, 0));

    // Ten consecutive failures quarantine the run for good.
    for (0..RetryTracker.quarantine_threshold) |_| _ = tracker.recordFailure(name, 0);
    try testing.expect(!tracker.shouldAttempt(name, std.math.maxInt(i64)));
}

test "durable recovery: attempts (rather than skips) a run whose waitSignal timeout has already expired" {
    // Before the timeout_ms override, a wait_signal tail with no matching
    // signal was always skipped (`continue`), forever, even once its own
    // deadline had passed - the recovery scheduler would never re-enter a
    // durable run stuck waiting on a signal that will never arrive once its
    // stepWithTimeout budget expired.
    //
    // The handler source is deliberately a nonexistent file path so
    // recoverOne fails at handler_loader.load(), before it ever touches the
    // JS runtime - this isolates the scheduler-level branch under test
    // (attempted vs skipped) from a separate, pre-existing bug: recoverOne
    // double-frees nested function bytecode when a recovered run actually
    // completes JS execution (reproduces even for a trivial run()+step()
    // handler with no waitSignal/stepWithTimeout involved, so it is not
    // specific to this branch - it needs its own dedicated fix).
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    const run_key = "timeout:recovery";
    const filename = try std.fmt.allocPrint(allocator, "durable-{x}.jsonl", .{std.hash.Fnv1a_64.hash(run_key)});
    defer allocator.free(filename);
    const oplog_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ durable_dir, filename });
    defer allocator.free(oplog_path);

    // A suspended run whose last event is a wait_signal with an
    // already-expired absolute timeout_ms deadline (1ms since epoch) and no
    // matching signal anywhere in the durable dir.
    const oplog =
        "{\"type\":\"durable_run\",\"key\":\"timeout:recovery\"}\n" ++
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"wait_signal\",\"name\":\"approved\",\"timeout_ms\":1}\n";
    try zq.file_io.writeFile(allocator, oplog_path, oplog);

    var tracker = RetryTracker.init(allocator);
    defer tracker.deinit();
    try testing.expect(tracker.shouldAttempt(filename, trace.unixMillis()));

    const config = ServerConfig{
        .handler = .{ .file_path = "/nonexistent/handler-for-recovery-attempt-test.ts" },
        .runtime_config = .{ .durable_oplog_dir = durable_dir },
    };
    const recovered = try recoverIncompleteOplogsTracked(allocator, config, &tracker);
    try testing.expectEqual(@as(u32, 0), recovered);

    // Skipped (the pre-fix behavior) would leave the tracker untouched
    // forever, since nothing else ever delivers the signal or advances the
    // deadline. Attempted-but-failed (handler_loader.load rejects the
    // nonexistent path) records a failure, proving recoverOne was actually
    // invoked for this oplog instead of the branch falling through to
    // `continue`.
    try testing.expect(!tracker.shouldAttempt(filename, trace.unixMillis()));
}

test "durable recovery: full dead-run lifecycle - quarantine writes a record, replay clears it" {
    // Drives RetryTracker.quarantine_threshold-1 failures directly (mirroring
    // the "RetryTracker backs off and quarantines" test above) so the
    // backoff timing on those synthetic pre-failures can't race real
    // wall-clock time, then crosses the threshold through the real
    // recoverIncompleteOplogsTracked path (same deliberately-nonexistent-
    // handler trick as the timeout-recovery test above, to stay clear of
    // the separate, pre-existing recoverOne double-free bug on real JS
    // execution) so the actual writeDeadRunRecord call site under test.
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    const run_key = "lifecycle:dead-run";
    const filename = try std.fmt.allocPrint(allocator, "durable-{x}.jsonl", .{std.hash.Fnv1a_64.hash(run_key)});
    defer allocator.free(filename);
    const oplog_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ durable_dir, filename });
    defer allocator.free(oplog_path);

    const oplog =
        "{\"type\":\"durable_run\",\"key\":\"lifecycle:dead-run\"}\n" ++
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/\",\"headers\":{},\"body\":null}\n";
    try zq.file_io.writeFile(allocator, oplog_path, oplog);

    var tracker = RetryTracker.init(allocator);
    defer tracker.deinit();
    for (0..RetryTracker.quarantine_threshold - 1) |i| {
        _ = tracker.recordFailure(filename, @intCast(i));
    }
    try testing.expect(!tracker.isQuarantined(filename));
    try testing.expect(!(try dead_runs.hasDeadRun(allocator, durable_dir, dead_runs.deadRunId(filename).?)));

    const config = ServerConfig{
        .handler = .{ .file_path = "/nonexistent/handler-for-dead-run-lifecycle-test.ts" },
        .runtime_config = .{ .durable_oplog_dir = durable_dir },
    };
    const recovered = try recoverIncompleteOplogsTracked(allocator, config, &tracker);
    try testing.expectEqual(@as(u32, 0), recovered);

    // The 10th failure crossed the threshold through the real integration
    // path, so a dead-run record must now exist with the real run_key and
    // the real recoverOne failure reason (not a placeholder).
    try testing.expect(tracker.isQuarantined(filename));
    const id = dead_runs.deadRunId(filename).?;
    try testing.expect(try dead_runs.hasDeadRun(allocator, durable_dir, id));
    const record = (try dead_runs.readDeadRun(allocator, durable_dir, id)).?;
    defer allocator.free(record);
    try testing.expect(std.mem.indexOf(u8, record, "\"state\":\"quarantined\"") != null);
    try testing.expect(std.mem.indexOf(u8, record, run_key) != null);

    // The oplog itself must be untouched by any of this (KTD2 / stop
    // condition: dead-run records live alongside, never inside, the oplog).
    const oplog_after = try zq.file_io.readFile(allocator, oplog_path, 4096);
    defer allocator.free(oplog_after);
    try testing.expectEqualStrings(oplog, oplog_after);

    // A further poll does not re-write or duplicate the record, and does
    // not re-attempt recovery (still gated by the persisted record).
    const recovered_again = try recoverIncompleteOplogsTracked(allocator, config, &tracker);
    try testing.expectEqual(@as(u32, 0), recovered_again);
    try testing.expectEqual(@as(u32, RetryTracker.quarantine_threshold), tracker.map.get(filename).?.consecutive_failures);

    // Replay clears the persisted record. The in-memory tracker is a
    // separate process's concern in production (replay only deletes the
    // file); the next poll's gate resyncs it, matching the cross-process
    // design (KTD1).
    try dead_runs.replayDeadRun(allocator, durable_dir, id);
    try testing.expect(!(try dead_runs.hasDeadRun(allocator, durable_dir, id)));

    const recovered_after_replay = try recoverIncompleteOplogsTracked(allocator, config, &tracker);
    try testing.expectEqual(@as(u32, 0), recovered_after_replay);
    try testing.expect(!tracker.isQuarantined(filename));
    // recoverOne fails again (handler still missing) - resynced from zero,
    // not simply left at the pre-replay count.
    try testing.expectEqual(@as(u32, 1), tracker.map.get(filename).?.consecutive_failures);
}

test "durable recovery: recoverOne completes real JS execution without double-freeing bytecode" {
    // Regression: recoverOne re-executes the handler to completion (unlike
    // this file's other tests, which stop short of real JS execution - see
    // the "waitSignal timeout has already expired" test above). Completing
    // execution used to double-free nested non-closure function bytecode in
    // Context.deinit (run()'s and step()'s zero-upvalue arrow callbacks are
    // both a constant of their enclosing function AND independently tracked
    // in ctx.bytecode_functions - see object.zig's destroyFullTracked).
    // Invisible under test-zruntime's arena-wrapped allocator; only test-cli's
    // per-test-fresh GPA runner catches it.
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    const run_key = "recover:complete-execution";
    const filename = try std.fmt.allocPrint(allocator, "durable-{x}.jsonl", .{std.hash.Fnv1a_64.hash(run_key)});
    defer allocator.free(filename);
    const oplog_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ durable_dir, filename });
    defer allocator.free(oplog_path);

    // An oplog that recorded the run started, but crashed before any step.
    const oplog =
        "{\"type\":\"durable_run\",\"key\":\"recover:complete-execution\"}\n" ++
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/\",\"headers\":{},\"body\":null}\n";
    try zq.file_io.writeFile(allocator, oplog_path, oplog);

    const handler_code =
        \\import { run, step } from "zigttp:durable";
        \\function handler(req) {
        \\  return run("recover:complete-execution", () => {
        \\    const v = step("s", () => 1);
        \\    return Response.json({ v: v });
        \\  });
        \\}
    ;

    const config = ServerConfig{
        .handler = .{ .inline_code = handler_code },
        .runtime_config = .{ .durable_oplog_dir = durable_dir },
    };
    const recovered = try recoverIncompleteOplogsTracked(allocator, config, null);
    try testing.expectEqual(@as(u32, 1), recovered);
}

test "durable recovery: a trailing un-resumed wait reads as pending, not failed" {
    const suspended =
        "{\"type\":\"durable_run\",\"key\":\"order:1\"}\n" ++
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/a\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"wait_timer\",\"until_ms\":99999999999}\n";
    try testing.expect(endsWithPendingWait(testing.allocator, suspended));

    const failed_midway =
        "{\"type\":\"durable_run\",\"key\":\"order:1\"}\n" ++
        "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/a\",\"headers\":{},\"body\":null}\n" ++
        "{\"type\":\"io\",\"seq\":0,\"module\":\"env\",\"fn\":\"env\",\"args\":[\"K\"],\"result\":\"V\"}\n";
    try testing.expect(!endsWithPendingWait(testing.allocator, failed_midway));
}
