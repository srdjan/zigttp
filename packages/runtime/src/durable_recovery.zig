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

    pub fn recordFailure(self: *RetryTracker, name: []const u8, now_ms: i64) void {
        const gop = self.map.getOrPut(self.allocator, name) catch return;
        if (!gop.found_existing) {
            gop.key_ptr.* = self.allocator.dupe(u8, name) catch {
                _ = self.map.remove(name);
                return;
            };
            gop.value_ptr.* = .{};
        }
        gop.value_ptr.consecutive_failures += 1;
        if (gop.value_ptr.consecutive_failures >= quarantine_threshold) {
            if (gop.value_ptr.consecutive_failures == quarantine_threshold and !builtin.is_test) {
                std.log.warn(
                    "Durable run '{s}' quarantined after {d} consecutive recovery failures",
                    .{ name, gop.value_ptr.consecutive_failures },
                );
            }
            return;
        }
        const shift: u6 = @intCast(@min(gop.value_ptr.consecutive_failures - 1, 6));
        const backoff = @min(base_backoff_ms << shift, max_backoff_ms);
        gop.value_ptr.next_attempt_ms = now_ms + backoff;
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

        // Gate on the tracker before any I/O: a backed-off or quarantined
        // run must not be re-read and re-parsed every poll second.
        if (tracker) |t| {
            if (!t.shouldAttempt(filename, trace.unixMillis())) continue;
        }

        // Read oplog file
        const source = readFile(allocator, full_path) catch |err| {
            std.log.err("Failed to read oplog '{s}': {}", .{ full_path, err });
            if (tracker) |t| t.recordFailure(filename, trace.unixMillis());
            continue;
        };
        defer allocator.free(source);

        if (!trace.isIncompleteOplog(source)) {
            // Completed durable runs remain on disk for duplicate-key reuse.
            continue;
        }

        // Parse the incomplete oplog
        var parsed = trace.parseIncompleteOplog(allocator, source) catch |err| {
            std.log.err("Failed to parse oplog '{s}': {}", .{ full_path, err });
            if (tracker) |t| t.recordFailure(filename, trace.unixMillis());
            continue;
        };
        defer parsed.deinit();

        const run_key = parsed.run_key orelse {
            std.log.err("Skipping oplog without durable run key: {s}", .{full_path});
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
                .wait_signal => |w| if (signalDefinitelyAbsent(allocator, oplog_dir, run_key, w.name)) continue,
                else => {},
            }
        }

        std.log.info("Recovering durable run: {s} {s} {s} ({d} recorded events)", .{
            run_key,
            parsed.request.method,
            parsed.request.url,
            parsed.events.len,
        });

        const outcome = recoverOne(allocator, config, &parsed, full_path) catch |err| blk: {
            std.log.err("Recovery failed for '{s}': {}", .{ full_path, err });
            break :blk .failed;
        };

        switch (outcome) {
            .recovered => {
                recovered += 1;
                if (tracker) |t| t.clear(filename);
            },
            // Still suspended on a wait (or claimed elsewhere): neutral.
            .pending => {},
            .failed => if (tracker) |t| t.recordFailure(filename, trace.unixMillis()),
        }
    }

    if (recovered > 0) {
        std.log.info("Recovered {d} incomplete request(s)", .{recovered});
    }

    return recovered;
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
    const signals = store.scanSignals(allocator, trace.unixMillis()) catch return false;
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
        switch (err) {
            error.UnsupportedHandlerSource => std.log.err("Durable recovery requires a file_path or inline_code handler source", .{}),
            else => std.log.err("Durable recovery failed to load handler: {}", .{err}),
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
        std.log.err("Recovery handler execution failed: {}", .{err});
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

    // First failure: 1s backoff.
    tracker.recordFailure(name, 0);
    try testing.expect(!tracker.shouldAttempt(name, 500));
    try testing.expect(tracker.shouldAttempt(name, 1_000));

    // Backoff doubles and caps at 60s.
    tracker.recordFailure(name, 1_000);
    try testing.expect(!tracker.shouldAttempt(name, 2_500));
    try testing.expect(tracker.shouldAttempt(name, 3_000));
    for (0..5) |_| tracker.recordFailure(name, 10_000);
    try testing.expect(!tracker.shouldAttempt(name, 10_000 + 59_999));
    try testing.expect(tracker.shouldAttempt(name, 10_000 + 60_000));

    // Success clears the entry entirely.
    tracker.clear(name);
    try testing.expect(tracker.shouldAttempt(name, 0));

    // Ten consecutive failures quarantine the run for good.
    for (0..RetryTracker.quarantine_threshold) |_| tracker.recordFailure(name, 0);
    try testing.expect(!tracker.shouldAttempt(name, std.math.maxInt(i64)));
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
