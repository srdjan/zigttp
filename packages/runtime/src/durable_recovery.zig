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
const zq = @import("zigts");
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;
const Runtime = @import("zruntime.zig").Runtime;
const HttpRequestView = @import("http_types.zig").HttpRequestView;
const HttpHeader = @import("http_types.zig").HttpHeader;
const ServerConfig = @import("server.zig").ServerConfig;
const handler_loader = @import("handler_loader.zig");

const c = @cImport({
    @cInclude("dirent.h");
});

const trace = zq.trace;

/// Scan the durable directory and recover any incomplete runs.
/// Returns the number of runs recovered.
pub fn recoverIncompleteOplogs(allocator: std.mem.Allocator, config: ServerConfig) !u32 {
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
        // Build full path
        var path_buf: [512]u8 = undefined;
        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ oplog_dir, filename }) catch continue;

        // Read oplog file
        const source = readFile(allocator, full_path) catch |err| {
            std.log.err("Failed to read oplog '{s}': {}", .{ full_path, err });
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
            continue;
        };
        defer parsed.deinit();

        const run_key = parsed.run_key orelse {
            std.log.err("Skipping oplog without durable run key: {s}", .{full_path});
            continue;
        };

        std.log.info("Recovering durable run: {s} {s} {s} ({d} recorded events)", .{
            run_key,
            parsed.request.method,
            parsed.request.url,
            parsed.events.len,
        });

        const success = recoverOne(allocator, config, &parsed, full_path) catch |err| {
            std.log.err("Recovery failed for '{s}': {}", .{ full_path, err });
            continue;
        };

        if (success) {
            recovered += 1;
        }
    }

    if (recovered > 0) {
        std.log.info("Recovered {d} incomplete request(s)", .{recovered});
    }

    return recovered;
}

/// Exclusive non-blocking claim on a single oplog file, held for the duration
/// of a recovery re-execution. The scheduler re-runs `recoverIncompleteOplogs`
/// every second, so without a claim two passes (or a pass overlapping a live
/// request that still owns the oplog) could re-execute the same handler
/// concurrently and double-apply its effects. `flock(LOCK_EX | LOCK_NB)` fails
/// closed: if the lock is already held the second claimant skips the file. The
/// lock is advisory and released when the fd closes.
const OplogClaim = struct {
    fd: c_int,

    /// Try to claim `path`. Returns null when the lock is already held by
    /// another recovery pass (skip this oplog) or the file cannot be opened.
    fn acquire(allocator: std.mem.Allocator, path: []const u8) ?OplogClaim {
        const path_z = allocator.dupeZ(u8, path) catch return null;
        defer allocator.free(path_z);
        const fd = std.c.open(path_z, .{ .ACCMODE = .RDONLY }, @as(std.c.mode_t, 0));
        if (fd < 0) return null;
        if (std.c.flock(fd, std.posix.LOCK.EX | std.posix.LOCK.NB) != 0) {
            _ = std.c.close(fd);
            return null;
        }
        return .{ .fd = fd };
    }

    fn release(self: OplogClaim) void {
        // Closing the fd drops the advisory lock; an explicit LOCK.UN first
        // makes the release intent obvious and independent of close timing.
        _ = std.c.flock(self.fd, std.posix.LOCK.UN);
        _ = std.c.close(self.fd);
    }
};

fn recoverOne(
    allocator: std.mem.Allocator,
    config: ServerConfig,
    parsed: *const trace.IncompleteOplog,
    oplog_path: []const u8,
) !bool {
    // Claim the oplog before re-executing. If another pass already holds it,
    // skip rather than double-execute the handler.
    const claim = OplogClaim.acquire(allocator, oplog_path) orelse return false;
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
    rt.setPendingDurableRecovery(
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

    const request = HttpRequestView{
        .method = parsed.request.method,
        .url = parsed.request.url,
        .headers = headers_list,
        .body = body,
    };

    // Execute handler - durable wrappers will replay from oplog, then continue live
    var response = rt.executeHandler(request) catch |err| {
        std.log.err("Recovery handler execution failed: {}", .{err});
        return false;
    };
    defer response.deinit();

    const updated = readFile(allocator, oplog_path) catch return false;
    defer allocator.free(updated);
    if (trace.isIncompleteOplog(updated)) return false;

    std.log.info("Recovered: {s} {s} -> {d}", .{ parsed.request.method, parsed.request.url, response.status });
    return true;
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
