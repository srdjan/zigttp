const std = @import("std");
const zq = @import("zigts");
const http_types = @import("http_types.zig");
const codec = @import("workflow_queue_envelope.zig");
const atomic_file = @import("atomic_file.zig");

const Allocator = std.mem.Allocator;
const HttpHeader = http_types.HttpHeader;
const HttpRequestView = http_types.HttpRequestView;
const HttpResponse = http_types.HttpResponse;

/// Re-exported so external callers (e.g. runtime_workflow.zig) that already
/// spell this as `workflow_queue.QueuedRequest` keep working unchanged.
pub const QueuedRequest = codec.QueuedRequest;

const DEFAULT_LEASE_MS: i64 = 30_000;
const DEFAULT_MAX_ATTEMPTS: u32 = 3;
const MAX_QUEUE_FILE_BYTES: usize = 1024 * 1024;

const c = @cImport({
    @cInclude("dirent.h");
});

pub const ClaimResult = union(enum) {
    done: []u8,
    dead: []u8,
    claimed: QueuedRequest,
    /// Another caller holds the active lease. Carries the wall-clock time
    /// (same clock as `now_ms`) after which retrying is worthwhile, so
    /// callers can wake near the real lease expiry instead of polling on a
    /// fixed interval.
    busy: i64,

    pub fn deinit(self: *ClaimResult, allocator: Allocator) void {
        switch (self.*) {
            .done => |bytes| allocator.free(bytes),
            .dead => |bytes| allocator.free(bytes),
            .claimed => |*request| request.deinit(),
            .busy => {},
        }
        self.* = undefined;
    }
};

const QueuePaths = struct {
    allocator: Allocator,
    root: []u8,
    pending_dir: []u8,
    leased_dir: []u8,
    done_dir: []u8,
    dead_dir: []u8,
    pending_file: []u8,
    leased_file: []u8,
    done_file: []u8,
    dead_file: []u8,

    fn deinit(self: *QueuePaths) void {
        self.allocator.free(self.root);
        self.allocator.free(self.pending_dir);
        self.allocator.free(self.leased_dir);
        self.allocator.free(self.done_dir);
        self.allocator.free(self.dead_dir);
        self.allocator.free(self.pending_file);
        self.allocator.free(self.leased_file);
        self.allocator.free(self.done_file);
        self.allocator.free(self.dead_file);
        self.* = undefined;
    }
};

pub fn defaultLeaseMs() i64 {
    return DEFAULT_LEASE_MS;
}

pub fn defaultMaxAttempts() u32 {
    return DEFAULT_MAX_ATTEMPTS;
}

pub fn isValidItemId(id: []const u8) bool {
    if (id.len == 0) return false;
    for (id) |ch| {
        if (std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '_') continue;
        return false;
    }
    return true;
}

pub fn itemId(allocator: Allocator, run_key: []const u8, step_name: []const u8) ![]u8 {
    var hasher = std.hash.Wyhash.init(0);
    hasher.update(run_key);
    hasher.update(&[_]u8{0});
    hasher.update(step_name);
    const hash = hasher.final();
    return std.fmt.allocPrint(allocator, "{x}", .{hash});
}

pub fn enqueueRequest(
    allocator: Allocator,
    durable_dir: []const u8,
    id: []const u8,
    target: []const u8,
    view: HttpRequestView,
) !void {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    try ensureQueueDirs(&p);

    if (try fileExists(p.done_file)) return;
    if (try fileExists(p.dead_file)) return;
    if (try fileExists(p.pending_file)) return;
    if (try fileExists(p.leased_file)) return;

    const now_ms = zq.trace.unixMillis();
    const payload = try codec.requestEnvelopeJson(allocator, target, view, .{
        .created_at_ms = now_ms,
        .updated_at_ms = now_ms,
    });
    defer allocator.free(payload);
    try atomic_file.writeFileAtomic(allocator, p.pending_file, payload);
}

pub fn readResult(allocator: Allocator, durable_dir: []const u8, id: []const u8) !?[]u8 {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    return readDoneFile(allocator, p.done_file);
}

pub fn readDead(allocator: Allocator, durable_dir: []const u8, id: []const u8) !?[]u8 {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    return readDeadFile(allocator, p.dead_file);
}

pub fn listDeadIds(allocator: Allocator, durable_dir: []const u8) ![][]u8 {
    const dead_dir = try std.fs.path.join(allocator, &.{ durable_dir, "workflow-queue", "dead" });
    defer allocator.free(dead_dir);

    const dead_dir_z = try allocator.dupeZ(u8, dead_dir);
    defer allocator.free(dead_dir_z);

    const dir = c.opendir(dead_dir_z) orelse return try allocator.alloc([]u8, 0);
    defer _ = c.closedir(dir);

    var ids: std.ArrayList([]u8) = .empty;
    errdefer {
        for (ids.items) |id| allocator.free(id);
        ids.deinit(allocator);
    }

    while (c.readdir(dir)) |entry| {
        const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
        const name = std.mem.sliceTo(name_ptr, 0);
        if (!std.mem.endsWith(u8, name, ".json")) continue;
        const id = name[0 .. name.len - ".json".len];
        if (id.len == 0) continue;
        try ids.append(allocator, try allocator.dupe(u8, id));
    }

    std.mem.sort([]u8, ids.items, {}, lessThanBytes);
    return ids.toOwnedSlice(allocator);
}

/// Lists ids stranded under a stray `.reclaim-*` staging file in the leased
/// directory (see findStrayReclaimFile) so an operator can notice items that
/// crashed mid-reclaim instead of them silently parking forever. A
/// subsequent tryClaim on the returned id recovers it automatically.
pub fn listOrphanedReclaimIds(allocator: Allocator, durable_dir: []const u8) ![][]u8 {
    const leased_dir = try std.fs.path.join(allocator, &.{ durable_dir, "workflow-queue", "leased" });
    defer allocator.free(leased_dir);

    const leased_dir_z = try allocator.dupeZ(u8, leased_dir);
    defer allocator.free(leased_dir_z);

    const dir = c.opendir(leased_dir_z) orelse return try allocator.alloc([]u8, 0);
    defer _ = c.closedir(dir);

    var ids: std.ArrayList([]u8) = .empty;
    errdefer {
        for (ids.items) |id| allocator.free(id);
        ids.deinit(allocator);
    }

    const marker = ".json.reclaim-";
    while (c.readdir(dir)) |entry| {
        const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
        const name = std.mem.sliceTo(name_ptr, 0);
        const marker_idx = std.mem.indexOf(u8, name, marker) orelse continue;
        const id = name[0..marker_idx];
        if (id.len == 0) continue;
        try ids.append(allocator, try allocator.dupe(u8, id));
    }

    std.mem.sort([]u8, ids.items, {}, lessThanBytes);
    return ids.toOwnedSlice(allocator);
}

pub fn replayDead(allocator: Allocator, durable_dir: []const u8, id: []const u8) !void {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    try ensureQueueDirs(&p);

    if (try fileExists(p.done_file)) return error.WorkflowQueueResultAlreadyDone;
    if (try fileExists(p.pending_file)) return error.WorkflowQueueItemAlreadyPending;
    if (try fileExists(p.leased_file)) return error.WorkflowQueueItemAlreadyLeased;

    const dead = (try readDeadFile(allocator, p.dead_file)) orelse return error.WorkflowQueueDeadLetterMissing;
    defer allocator.free(dead);

    const request_json = try codec.requestJsonFromDeadLetter(allocator, dead);
    defer allocator.free(request_json);

    var request = try codec.parseQueuedRequest(allocator, request_json);
    defer request.deinit();
    const now_ms = zq.trace.unixMillis();
    const pending_payload = try codec.requestEnvelopeJsonFromQueued(allocator, request, .{
        .attempts = 0,
        .lease_until_ms = 0,
        .lease_owner = null,
        .created_at_ms = if (request.created_at_ms != 0) request.created_at_ms else now_ms,
        .updated_at_ms = now_ms,
        .last_error = null,
    });
    defer allocator.free(pending_payload);

    try atomic_file.writeFileAtomic(allocator, p.pending_file, pending_payload);
    atomic_file.deleteIfExists(p.dead_file);
}

pub fn discardDead(allocator: Allocator, durable_dir: []const u8, id: []const u8) !void {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    if (!(try fileExists(p.dead_file))) return error.WorkflowQueueDeadLetterMissing;
    atomic_file.deleteIfExists(p.dead_file);
}

pub fn tryClaim(
    allocator: Allocator,
    durable_dir: []const u8,
    id: []const u8,
    now_ms: i64,
    lease_ms: i64,
) !ClaimResult {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    try ensureQueueDirs(&p);

    if (try readDoneFile(allocator, p.done_file)) |result| {
        return .{ .done = result };
    }
    if (try readDeadFile(allocator, p.dead_file)) |dead| {
        // replayDead() writes the fresh pending envelope before removing
        // the dead-letter file; if that removal was interrupted (crash or
        // a failed unlink - deleteIfExists is best-effort), a stale dead
        // file can transiently coexist with a legitimate pending/leased
        // one. Don't let it permanently mask a successfully replayed item
        // - prefer pending/leased and finish the interrupted cleanup.
        if (!(try fileExists(p.pending_file)) and !(try fileExists(p.leased_file))) {
            return .{ .dead = dead };
        }
        allocator.free(dead);
        atomic_file.deleteIfExists(p.dead_file);
    }

    var owner_buf: [32]u8 = undefined;
    const owner = try claimOwner(&owner_buf);

    if (renamePath(p.pending_file, p.leased_file)) {
        return try claimLeasedFile(allocator, p.leased_file, p.dead_file, now_ms + lease_ms, now_ms, owner);
    } else |err| switch (err) {
        // A concurrent tryClaim already won the pending->leased rename;
        // fall through to the lease-aware check below instead of
        // re-claiming the file it just created.
        error.FileNotFound => {},
        else => return err,
    }

    if (try fileExists(p.leased_file)) {
        return try handleLeasedFile(allocator, &p, now_ms, lease_ms, owner);
    }

    // No pending/leased/done/dead file exists for this id. A crash during a
    // previous reclaim attempt (between the leased->reclaim rename below and
    // the rename-back/dead-letter that follows it) can leave the item under
    // a private `.reclaim-*` staging name that no other code path scans for,
    // stranding it forever. Recover it here instead of guessing `.busy`.
    const file_name = try std.fmt.allocPrint(allocator, "{s}.json", .{id});
    defer allocator.free(file_name);
    if (try findStrayReclaimFile(allocator, p.leased_dir, file_name, now_ms, lease_ms)) |stray_file| {
        defer allocator.free(stray_file);
        if (renamePath(stray_file, p.leased_file)) {
            return try handleLeasedFile(allocator, &p, now_ms, lease_ms, owner);
        } else |err| switch (err) {
            // Another caller already won the recovery race.
            error.FileNotFound => return .{ .busy = now_ms + lease_ms },
            else => return err,
        }
    }

    return .{ .busy = now_ms + lease_ms };
}

fn handleLeasedFile(allocator: Allocator, p: *const QueuePaths, now_ms: i64, lease_ms: i64, owner: []const u8) !ClaimResult {
    // The caller's existence check can race with a concurrent winner's own
    // rename-away between the check and this read; treat that exactly like
    // every other race-loss branch in this file (report busy) instead of
    // leaking the internal TOCTOU as a hard error.
    const envelope = zq.file_io.readFile(allocator, p.leased_file, MAX_QUEUE_FILE_BYTES) catch |err| switch (err) {
        error.FileNotFound => return .{ .busy = now_ms + lease_ms },
        else => return err,
    };
    defer allocator.free(envelope);
    const lease_until_ms = codec.parseLeaseUntilMs(envelope) catch 0;
    if (lease_until_ms > now_ms) return .{ .busy = lease_until_ms };

    // The lease has expired, but another caller may be racing to
    // reclaim the same item at the same time. Renaming the leased file
    // to a reclaim-specific staging path is atomic and single-winner
    // (the same guarantee the pending->leased transition above relies
    // on): only the caller whose rename succeeds may claim and
    // re-dispatch this item, so two racers can never both get `.claimed`
    // back for the same expired lease.
    const reclaim_file = try std.fmt.allocPrint(allocator, "{s}.reclaim-{d}-{d}-{x}", .{ p.leased_file, now_ms, std.c.getpid(), @intFromPtr(p) });
    defer allocator.free(reclaim_file);
    if (renamePath(p.leased_file, reclaim_file)) {
        var reclaim_active = true;
        errdefer if (reclaim_active) renamePath(reclaim_file, p.leased_file) catch {};

        const locked_envelope = try zq.file_io.readFile(allocator, reclaim_file, MAX_QUEUE_FILE_BYTES);
        defer allocator.free(locked_envelope);
        const locked_lease_until_ms = codec.parseLeaseUntilMs(locked_envelope) catch 0;
        if (locked_lease_until_ms > now_ms) {
            try renamePath(reclaim_file, p.leased_file);
            reclaim_active = false;
            return .{ .busy = locked_lease_until_ms };
        }

        var result = try claimLeasedFile(allocator, reclaim_file, p.dead_file, now_ms + lease_ms, now_ms, owner);
        errdefer result.deinit(allocator);
        switch (result) {
            .claimed => {
                try renamePath(reclaim_file, p.leased_file);
                reclaim_active = false;
            },
            .dead => {
                atomic_file.deleteIfExists(reclaim_file);
                reclaim_active = false;
            },
            .done, .busy => {
                try renamePath(reclaim_file, p.leased_file);
                reclaim_active = false;
            },
        }
        return result;
    } else |err| switch (err) {
        // Another caller already won the reclaim race and just set a
        // fresh lease of roughly `now_ms + lease_ms`.
        error.FileNotFound => return .{ .busy = now_ms + lease_ms },
        else => return err,
    }
}

/// Scans `leased_dir` for a stray `{file_name}.reclaim-{started_at_ms}-*`
/// staging file left behind by a reclaim attempt that crashed before
/// renaming it back to `file_name` or dead-lettering it.
///
/// The filename encodes the wall-clock time (same clock as `now_ms`) the
/// reclaim attempt started. A live, still-in-flight reclaim's own
/// rename/read/write/rename cycle finishes in milliseconds; `writeFileAtomic`
/// recreates the file at its original path via rename regardless of whether
/// something else moved it away in the interim, so treating *any* sighting
/// of a `.reclaim-*` file as abandoned would let a fast concurrent claimer
/// steal a file its rightful (still-alive) owner is actively using,
/// producing two independent `.claimed` results for one item. Requiring the
/// staging file be at least `min_age_ms` old closes that window while still
/// bounding recovery of a genuinely crashed reclaim to a fixed, finite delay.
fn findStrayReclaimFile(allocator: Allocator, leased_dir: []const u8, file_name: []const u8, now_ms: i64, min_age_ms: i64) !?[]u8 {
    const prefix = try std.fmt.allocPrint(allocator, "{s}.reclaim-", .{file_name});
    defer allocator.free(prefix);

    const dir_z = try allocator.dupeZ(u8, leased_dir);
    defer allocator.free(dir_z);
    const dir = c.opendir(dir_z) orelse return null;
    defer _ = c.closedir(dir);

    while (c.readdir(dir)) |entry| {
        const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
        const name = std.mem.sliceTo(name_ptr, 0);
        if (!std.mem.startsWith(u8, name, prefix)) continue;

        const rest = name[prefix.len..];
        const dash_idx = std.mem.indexOfScalar(u8, rest, '-') orelse continue;
        const started_at_ms = std.fmt.parseInt(i64, rest[0..dash_idx], 10) catch continue;
        if (now_ms - started_at_ms < min_age_ms) continue;

        return try std.fs.path.join(allocator, &.{ leased_dir, name });
    }
    return null;
}

pub fn completeResponse(
    allocator: Allocator,
    durable_dir: []const u8,
    id: []const u8,
    response: HttpResponse,
) !void {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    try ensureQueueDirs(&p);

    const payload = try codec.responseEnvelopeJson(allocator, response);
    defer allocator.free(payload);
    try atomic_file.writeFileAtomic(allocator, p.done_file, payload);
    atomic_file.deleteIfExists(p.pending_file);
    atomic_file.deleteIfExists(p.leased_file);
}

pub fn completeError(
    allocator: Allocator,
    durable_dir: []const u8,
    id: []const u8,
    code: []const u8,
    detail: []const u8,
) !void {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    try ensureQueueDirs(&p);

    const payload = try codec.errorEnvelopeJson(allocator, code, detail);
    defer allocator.free(payload);
    try atomic_file.writeFileAtomic(allocator, p.done_file, payload);
    atomic_file.deleteIfExists(p.pending_file);
    atomic_file.deleteIfExists(p.leased_file);
}

pub fn markDead(
    allocator: Allocator,
    durable_dir: []const u8,
    id: []const u8,
    reason: []const u8,
) !void {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    try ensureQueueDirs(&p);

    const payload = try codec.deadEnvelopeJson(allocator, reason, 0, reason, zq.trace.unixMillis(), "operator", null);
    defer allocator.free(payload);
    try atomic_file.writeFileAtomic(allocator, p.dead_file, payload);
    atomic_file.deleteIfExists(p.pending_file);
    atomic_file.deleteIfExists(p.leased_file);
}

fn claimLeasedFile(
    allocator: Allocator,
    leased_file: []const u8,
    dead_file: []const u8,
    lease_until_ms: i64,
    now_ms: i64,
    lease_owner: []const u8,
) !ClaimResult {
    const envelope = try zq.file_io.readFile(allocator, leased_file, MAX_QUEUE_FILE_BYTES);
    defer allocator.free(envelope);
    var request = codec.parseQueuedRequest(allocator, envelope) catch |err| {
        const dead = try writeDeadLetter(
            allocator,
            dead_file,
            "invalid workflow queue envelope",
            0,
            @errorName(err),
            now_ms,
            "leased",
            null,
        );
        atomic_file.deleteIfExists(leased_file);
        return dead;
    };
    errdefer request.deinit();

    const next_attempts = request.attempts +| 1;
    if (next_attempts > DEFAULT_MAX_ATTEMPTS) {
        const last_error = request.last_error orelse "lease expired too many times";
        const dead = try writeDeadLetter(
            allocator,
            dead_file,
            "workflow queue max attempts exceeded",
            request.attempts,
            last_error,
            now_ms,
            "leased",
            envelope,
        );
        request.deinit();
        atomic_file.deleteIfExists(leased_file);
        return dead;
    }

    const payload = try codec.requestEnvelopeJsonFromQueued(allocator, request, .{
        .attempts = next_attempts,
        .lease_until_ms = lease_until_ms,
        .lease_owner = lease_owner,
        .created_at_ms = request.created_at_ms,
        .updated_at_ms = now_ms,
        .last_error = request.last_error,
    });
    defer allocator.free(payload);
    try atomic_file.writeFileAtomic(allocator, leased_file, payload);
    request.attempts = next_attempts;
    request.lease_until_ms = lease_until_ms;
    if (request.lease_owner) |old_owner| {
        request.allocator.free(old_owner);
        request.lease_owner = null;
    }
    request.lease_owner = try request.allocator.dupe(u8, lease_owner);
    request.updated_at_ms = now_ms;
    return .{ .claimed = request };
}

fn writeDeadLetter(
    allocator: Allocator,
    dead_file: []const u8,
    reason: []const u8,
    attempts: u32,
    last_error: []const u8,
    dead_at_ms: i64,
    source: []const u8,
    request_json: ?[]const u8,
) !ClaimResult {
    const payload = try codec.deadEnvelopeJson(allocator, reason, attempts, last_error, dead_at_ms, source, request_json);
    errdefer allocator.free(payload);
    try atomic_file.writeFileAtomic(allocator, dead_file, payload);
    return .{ .dead = payload };
}

fn claimOwner(buf: []u8) ![]const u8 {
    return std.fmt.bufPrint(buf, "pid:{d}", .{std.c.getpid()});
}

fn lessThanBytes(_: void, lhs: []u8, rhs: []u8) bool {
    return std.mem.lessThan(u8, lhs, rhs);
}

fn queuePaths(allocator: Allocator, durable_dir: []const u8, id: []const u8) !QueuePaths {
    if (!isValidItemId(id)) return error.InvalidWorkflowQueueItemId;

    const root = try std.fs.path.join(allocator, &.{ durable_dir, "workflow-queue" });
    errdefer allocator.free(root);
    const pending_dir = try std.fs.path.join(allocator, &.{ root, "pending" });
    errdefer allocator.free(pending_dir);
    const leased_dir = try std.fs.path.join(allocator, &.{ root, "leased" });
    errdefer allocator.free(leased_dir);
    const done_dir = try std.fs.path.join(allocator, &.{ root, "done" });
    errdefer allocator.free(done_dir);
    const dead_dir = try std.fs.path.join(allocator, &.{ root, "dead" });
    errdefer allocator.free(dead_dir);

    const file_name = try std.fmt.allocPrint(allocator, "{s}.json", .{id});
    defer allocator.free(file_name);
    const pending_file = try std.fs.path.join(allocator, &.{ pending_dir, file_name });
    errdefer allocator.free(pending_file);
    const leased_file = try std.fs.path.join(allocator, &.{ leased_dir, file_name });
    errdefer allocator.free(leased_file);
    const done_file = try std.fs.path.join(allocator, &.{ done_dir, file_name });
    errdefer allocator.free(done_file);
    const dead_file = try std.fs.path.join(allocator, &.{ dead_dir, file_name });
    errdefer allocator.free(dead_file);

    return .{
        .allocator = allocator,
        .root = root,
        .pending_dir = pending_dir,
        .leased_dir = leased_dir,
        .done_dir = done_dir,
        .dead_dir = dead_dir,
        .pending_file = pending_file,
        .leased_file = leased_file,
        .done_file = done_file,
        .dead_file = dead_file,
    };
}

fn ensureQueueDirs(p: *const QueuePaths) !void {
    try atomic_file.ensureDir(p.root);
    try atomic_file.ensureDir(p.pending_dir);
    try atomic_file.ensureDir(p.leased_dir);
    try atomic_file.ensureDir(p.done_dir);
    try atomic_file.ensureDir(p.dead_dir);
}

fn readDoneFile(allocator: Allocator, done_file: []const u8) !?[]u8 {
    if (!(try fileExists(done_file))) return null;
    return try zq.file_io.readFile(allocator, done_file, MAX_QUEUE_FILE_BYTES);
}

fn readDeadFile(allocator: Allocator, dead_file: []const u8) !?[]u8 {
    if (!(try fileExists(dead_file))) return null;
    return try zq.file_io.readFile(allocator, dead_file, MAX_QUEUE_FILE_BYTES);
}

fn fileExists(path: []const u8) !bool {
    const z = try std.heap.c_allocator.dupeZ(u8, path);
    defer std.heap.c_allocator.free(z);
    if (std.c.access(z, std.c.F_OK) == 0) return true;
    return false;
}

fn renamePath(src: []const u8, dst: []const u8) !void {
    const src_z = try std.heap.c_allocator.dupeZ(u8, src);
    defer std.heap.c_allocator.free(src_z);
    const dst_z = try std.heap.c_allocator.dupeZ(u8, dst);
    defer std.heap.c_allocator.free(dst_z);
    const rc = std.c.rename(src_z, dst_z);
    if (rc != 0) {
        switch (std.posix.errno(rc)) {
            .NOENT => return error.FileNotFound,
            else => return error.RenameFailed,
        }
    }
}

test "workflow queue claim completes with durable result" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    var headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
    defer headers.deinit(allocator);
    try headers.append(allocator, .{ .key = "accept", .value = "application/json" });
    const view: HttpRequestView = .{
        .method = "POST",
        .path = "/work",
        .url = "/work?x=1",
        .query_params = &.{.{ .key = "x", .value = "1" }},
        .headers = headers,
        .body = "hello",
    };

    try enqueueRequest(allocator, durable_dir, "item-a", "child", view);
    var claim = try tryClaim(allocator, durable_dir, "item-a", 10, 100);
    defer claim.deinit(allocator);

    const request = switch (claim) {
        .claimed => |*request| request,
        else => return error.ExpectedClaimedQueueItem,
    };
    try std.testing.expectEqualStrings("child", request.target);
    try std.testing.expectEqualStrings("POST", request.method);
    try std.testing.expectEqualStrings("/work", request.path);
    try std.testing.expectEqualStrings("x", request.query_params[0].key);
    try std.testing.expectEqualStrings("1", request.query_params[0].value);
    try std.testing.expectEqualStrings("hello", request.body.?);
    try std.testing.expectEqual(@as(u32, 1), request.attempts);
    try std.testing.expectEqual(@as(i64, 110), request.lease_until_ms);
    try std.testing.expect(request.lease_owner != null);
    try std.testing.expect(std.mem.startsWith(u8, request.lease_owner.?, "pid:"));

    var response_headers: std.ArrayListUnmanaged(http_types.ResponseHeader) = .empty;
    defer response_headers.deinit(allocator);
    try response_headers.append(allocator, .{
        .key = "content-type",
        .value = "text/plain",
        .key_owned = false,
        .value_owned = false,
    });
    const response: HttpResponse = .{
        .status = 202,
        .headers = response_headers,
        .body = "queued",
        .body_owned = false,
        .body_owner = null,
        .requires_runtime = false,
        .allocator = allocator,
        .prebuilt_raw = null,
    };
    try completeResponse(allocator, durable_dir, "item-a", response);

    const result = (try readResult(allocator, durable_dir, "item-a")).?;
    defer allocator.free(result);
    try std.testing.expect(std.mem.indexOf(u8, result, "\"status\":202") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "\"body\":\"queued\"") != null);
}

test "workflow queue files are private when created atomically" {
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    var p = try queuePaths(allocator, durable_dir, "item-private");
    defer p.deinit();
    try ensureQueueDirs(&p);
    try atomic_file.writeFileAtomic(allocator, p.pending_file, "{\"target\":\"worker\"}");

    const path_z = try allocator.dupeZ(u8, p.pending_file);
    defer allocator.free(path_z);
    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.Io.Threaded.closeFd(fd);
    const stat = try zq.file_io.fstatFd(fd);
    try std.testing.expectEqual(@as(u32, 0o600), stat.mode & 0o777);
}

test "workflow queue lease remains busy until expiry" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    const view: HttpRequestView = .{
        .method = "GET",
        .path = "/",
        .url = "/",
        .query_params = &.{},
        .headers = .empty,
        .body = null,
    };

    try enqueueRequest(allocator, durable_dir, "item-b", "child", view);
    var first = try tryClaim(allocator, durable_dir, "item-b", 10, 100);
    defer first.deinit(allocator);
    try std.testing.expect(first == .claimed);

    var second = try tryClaim(allocator, durable_dir, "item-b", 20, 100);
    defer second.deinit(allocator);
    try std.testing.expect(second == .busy);

    var third = try tryClaim(allocator, durable_dir, "item-b", 111, 100);
    defer third.deinit(allocator);
    try std.testing.expect(third == .claimed);

    var fourth = try tryClaim(allocator, durable_dir, "item-b", 112, 100);
    defer fourth.deinit(allocator);
    try std.testing.expect(fourth == .busy);
}

test "workflow queue concurrent reclaim of an expired lease yields exactly one claimant" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try std.testing.allocator.dupe(u8, dir_buf[0..dir_len]);
    defer std.testing.allocator.free(durable_dir);

    const view: HttpRequestView = .{
        .method = "GET",
        .path = "/",
        .url = "/",
        .query_params = &.{},
        .headers = .empty,
        .body = null,
    };

    try enqueueRequest(std.testing.allocator, durable_dir, "item-race", "child", view);
    var first = try tryClaim(std.testing.allocator, durable_dir, "item-race", 0, 10);
    defer first.deinit(std.testing.allocator);
    try std.testing.expect(first == .claimed);

    const thread_count: usize = 8;
    var claimed_count = std.atomic.Value(u32).init(0);
    var busy_count = std.atomic.Value(u32).init(0);
    var error_count = std.atomic.Value(u32).init(0);

    const ThreadCtx = struct {
        durable_dir: []const u8,
        claimed_count: *std.atomic.Value(u32),
        busy_count: *std.atomic.Value(u32),
        error_count: *std.atomic.Value(u32),
    };

    const Worker = struct {
        fn run(ctx: *ThreadCtx) void {
            // Concurrent tryClaim calls from real threads race the same
            // atomic rename this module's single-winner guarantee relies
            // on; std.testing.allocator is not safe under concurrent use,
            // so each thread gets its own thread-safe allocator.
            const allocator = std.heap.c_allocator;
            var result = tryClaim(allocator, ctx.durable_dir, "item-race", 1000, 1000) catch {
                _ = ctx.error_count.fetchAdd(1, .acq_rel);
                return;
            };
            defer result.deinit(allocator);
            switch (result) {
                .claimed => _ = ctx.claimed_count.fetchAdd(1, .acq_rel),
                .busy => _ = ctx.busy_count.fetchAdd(1, .acq_rel),
                else => _ = ctx.error_count.fetchAdd(1, .acq_rel),
            }
        }
    };

    var threads: [thread_count]std.Thread = undefined;
    var contexts: [thread_count]ThreadCtx = undefined;
    for (0..thread_count) |i| {
        contexts[i] = .{
            .durable_dir = durable_dir,
            .claimed_count = &claimed_count,
            .busy_count = &busy_count,
            .error_count = &error_count,
        };
        threads[i] = try std.Thread.spawn(.{}, Worker.run, .{&contexts[i]});
    }
    for (threads) |t| t.join();

    try std.testing.expectEqual(@as(u32, 0), error_count.load(.acquire));
    try std.testing.expectEqual(@as(u32, 1), claimed_count.load(.acquire));
    try std.testing.expectEqual(@as(u32, thread_count - 1), busy_count.load(.acquire));
}

test "workflow queue corrupt pending moves to non-replayable dead letter" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    var p = try queuePaths(allocator, durable_dir, "item-corrupt-pending");
    defer p.deinit();
    try ensureQueueDirs(&p);
    try atomic_file.writeFileAtomic(allocator, p.pending_file, "{\"target\":");

    var claim = try tryClaim(allocator, durable_dir, "item-corrupt-pending", 10, 100);
    defer claim.deinit(allocator);
    const dead = switch (claim) {
        .dead => |bytes| bytes,
        else => return error.ExpectedDeadQueueItem,
    };
    try std.testing.expect(std.mem.indexOf(u8, dead, "invalid workflow queue envelope") != null);
    try std.testing.expect(std.mem.indexOf(u8, dead, "\"request_json\":null") != null);
    try std.testing.expect(!(try fileExists(p.pending_file)));
    try std.testing.expect(!(try fileExists(p.leased_file)));
    try std.testing.expect(try fileExists(p.dead_file));
    try std.testing.expectError(error.WorkflowQueueDeadLetterNotReplayable, replayDead(allocator, durable_dir, "item-corrupt-pending"));
}

test "workflow queue corrupt expired lease moves to visible dead letter" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    var p = try queuePaths(allocator, durable_dir, "item-corrupt-leased");
    defer p.deinit();
    try ensureQueueDirs(&p);
    try atomic_file.writeFileAtomic(allocator, p.leased_file, "{\"lease_until_ms\":1,\"target\":false}");

    var claim = try tryClaim(allocator, durable_dir, "item-corrupt-leased", 10, 100);
    defer claim.deinit(allocator);
    const dead = switch (claim) {
        .dead => |bytes| bytes,
        else => return error.ExpectedDeadQueueItem,
    };
    try std.testing.expect(std.mem.indexOf(u8, dead, "invalid workflow queue envelope") != null);
    try std.testing.expect(!(try fileExists(p.leased_file)));
    try std.testing.expect(try fileExists(p.dead_file));
}

test "workflow queue max attempts dead letter can be replayed" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    const view: HttpRequestView = .{
        .method = "POST",
        .path = "/child",
        .url = "/child",
        .query_params = &.{},
        .headers = .empty,
        .body = "body",
    };

    var p = try queuePaths(allocator, durable_dir, "item-max-attempts");
    defer p.deinit();
    try ensureQueueDirs(&p);
    const leased_payload = try codec.requestEnvelopeJson(allocator, "child", view, .{
        .attempts = defaultMaxAttempts(),
        .lease_until_ms = 1,
        .lease_owner = "pid:old",
        .created_at_ms = 1,
        .updated_at_ms = 2,
        .last_error = "boom",
    });
    defer allocator.free(leased_payload);
    try atomic_file.writeFileAtomic(allocator, p.leased_file, leased_payload);

    var claim = try tryClaim(allocator, durable_dir, "item-max-attempts", 10, 100);
    defer claim.deinit(allocator);
    const dead = switch (claim) {
        .dead => |bytes| bytes,
        else => return error.ExpectedDeadQueueItem,
    };
    try std.testing.expect(std.mem.indexOf(u8, dead, "workflow queue max attempts exceeded") != null);
    try std.testing.expect(std.mem.indexOf(u8, dead, "\"attempts\":3") != null);
    try std.testing.expect(std.mem.indexOf(u8, dead, "\"last_error\":\"boom\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, dead, "\"request_json\":\"") != null);
    try std.testing.expect(!(try fileExists(p.leased_file)));
    try std.testing.expect(try fileExists(p.dead_file));

    const ids = try listDeadIds(allocator, durable_dir);
    defer {
        for (ids) |id| allocator.free(id);
        allocator.free(ids);
    }
    try std.testing.expectEqual(@as(usize, 1), ids.len);
    try std.testing.expectEqualStrings("item-max-attempts", ids[0]);

    try replayDead(allocator, durable_dir, "item-max-attempts");
    try std.testing.expect(!(try fileExists(p.dead_file)));
    try std.testing.expect(try fileExists(p.pending_file));

    var replayed = try tryClaim(allocator, durable_dir, "item-max-attempts", 20, 100);
    defer replayed.deinit(allocator);
    const request = switch (replayed) {
        .claimed => |*req| req,
        else => return error.ExpectedClaimedQueueItem,
    };
    try std.testing.expectEqualStrings("child", request.target);
    try std.testing.expectEqualStrings("body", request.body.?);
    try std.testing.expectEqual(@as(u32, 1), request.attempts);
    try std.testing.expect(request.last_error == null);
}

test "workflow queue claim recovers from a dead letter left behind by an interrupted replay" {
    // Simulates replayDead() being interrupted (crash / failed unlink)
    // after it writes the fresh pending envelope but before it removes the
    // stale dead-letter file, so both files exist at once. tryClaim must
    // not get permanently stuck reporting `.dead` in that state.
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    const view: HttpRequestView = .{
        .method = "POST",
        .path = "/child",
        .url = "/child",
        .query_params = &.{},
        .headers = .empty,
        .body = "body",
    };

    var p = try queuePaths(allocator, durable_dir, "item-stuck-dead");
    defer p.deinit();
    try ensureQueueDirs(&p);

    const dead_payload = try codec.deadEnvelopeJson(allocator, "boom", 3, "boom", 1, "workflow-queue", null);
    defer allocator.free(dead_payload);
    try atomic_file.writeFileAtomic(allocator, p.dead_file, dead_payload);

    const pending_payload = try codec.requestEnvelopeJson(allocator, "child", view, .{
        .created_at_ms = 2,
        .updated_at_ms = 2,
    });
    defer allocator.free(pending_payload);
    try atomic_file.writeFileAtomic(allocator, p.pending_file, pending_payload);

    var claim = try tryClaim(allocator, durable_dir, "item-stuck-dead", 20, 100);
    defer claim.deinit(allocator);
    const request = switch (claim) {
        .claimed => |*req| req,
        else => return error.ExpectedClaimedQueueItem,
    };
    try std.testing.expectEqualStrings("child", request.target);
    try std.testing.expect(!(try fileExists(p.dead_file)));
}

test "workflow queue recovers an item stranded in an orphaned .reclaim-* file" {
    // Simulates a crash between the leased->reclaim rename in
    // handleLeasedFile and the rename-back/dead-letter that follows it,
    // leaving the item under a private `.reclaim-*` staging name that
    // tryClaim's normal pending/leased/done/dead checks never see.
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    const view: HttpRequestView = .{
        .method = "GET",
        .path = "/",
        .url = "/",
        .query_params = &.{},
        .headers = .empty,
        .body = null,
    };

    try enqueueRequest(allocator, durable_dir, "item-orphan", "child", view);
    var first = try tryClaim(allocator, durable_dir, "item-orphan", 10, 100);
    defer first.deinit(allocator);
    try std.testing.expect(first == .claimed);

    var p = try queuePaths(allocator, durable_dir, "item-orphan");
    defer p.deinit();

    // Simulate the crash: rename the leased file out to a stray reclaim
    // staging path exactly as handleLeasedFile would, then stop (no
    // rename-back, no dead-letter).
    const stray_file = try std.fmt.allocPrint(allocator, "{s}.reclaim-0-99999-dead", .{p.leased_file});
    defer allocator.free(stray_file);
    try renamePath(p.leased_file, stray_file);
    try std.testing.expect(!(try fileExists(p.leased_file)));
    try std.testing.expect(try fileExists(stray_file));

    const orphaned_before = try listOrphanedReclaimIds(allocator, durable_dir);
    defer {
        for (orphaned_before) |id| allocator.free(id);
        allocator.free(orphaned_before);
    }
    try std.testing.expectEqual(@as(usize, 1), orphaned_before.len);
    try std.testing.expectEqualStrings("item-orphan", orphaned_before[0]);

    // A later claim attempt must recover the stray file instead of
    // reporting `.busy` forever.
    var recovered = try tryClaim(allocator, durable_dir, "item-orphan", 300, 100);
    defer recovered.deinit(allocator);
    try std.testing.expect(recovered == .claimed);
    try std.testing.expect(try fileExists(p.leased_file));
    try std.testing.expect(!(try fileExists(stray_file)));

    const orphaned_after = try listOrphanedReclaimIds(allocator, durable_dir);
    defer {
        for (orphaned_after) |id| allocator.free(id);
        allocator.free(orphaned_after);
    }
    try std.testing.expectEqual(@as(usize, 0), orphaned_after.len);
}

test "workflow queue does not steal a reclaim file that is still in flight" {
    // A `.reclaim-*` file that started "now" (relative to the caller's own
    // now_ms) must not be treated as abandoned: its rightful owner's own
    // rename/read/write/rename cycle may still be in progress. Without the
    // staleness check, a concurrent tryClaim on the same id would recover
    // it prematurely and race the true owner's own writes to the same
    // envelope, producing two independent `.claimed` results for one item.
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    const view: HttpRequestView = .{
        .method = "GET",
        .path = "/",
        .url = "/",
        .query_params = &.{},
        .headers = .empty,
        .body = null,
    };

    try enqueueRequest(allocator, durable_dir, "item-inflight", "child", view);
    var first = try tryClaim(allocator, durable_dir, "item-inflight", 10, 100);
    defer first.deinit(allocator);
    try std.testing.expect(first == .claimed);

    var p = try queuePaths(allocator, durable_dir, "item-inflight");
    defer p.deinit();

    // Simulate a reclaim that started at now_ms=1000 and is still mid-flight
    // (the file exists under its staging name, nothing has renamed it back
    // yet) - the same shape handleLeasedFile produces before it finishes.
    const stray_file = try std.fmt.allocPrint(allocator, "{s}.reclaim-1000-99999-dead", .{p.leased_file});
    defer allocator.free(stray_file);
    try renamePath(p.leased_file, stray_file);

    // A concurrent claim attempt at the same now_ms=1000 must not recover
    // this file - it looks freshly started, not abandoned - so it reports
    // busy rather than racing the true owner's own writes to it.
    var concurrent = try tryClaim(allocator, durable_dir, "item-inflight", 1000, 100);
    defer concurrent.deinit(allocator);
    try std.testing.expect(concurrent == .busy);
    try std.testing.expect(try fileExists(stray_file));
    try std.testing.expect(!(try fileExists(p.leased_file)));

    // Once enough time has passed that a live reclaim could not still be
    // running, the same file becomes recoverable.
    var later = try tryClaim(allocator, durable_dir, "item-inflight", 1000 + 100, 100);
    defer later.deinit(allocator);
    try std.testing.expect(later == .claimed);
}

test "workflow queue dead letters block enqueue until discarded" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = try allocator.dupe(u8, dir_buf[0..dir_len]);
    defer allocator.free(durable_dir);

    const view: HttpRequestView = .{
        .method = "GET",
        .path = "/",
        .url = "/",
        .query_params = &.{},
        .headers = .empty,
        .body = null,
    };

    try markDead(allocator, durable_dir, "item-discard", "operator review");
    try enqueueRequest(allocator, durable_dir, "item-discard", "child", view);

    var claim = try tryClaim(allocator, durable_dir, "item-discard", 10, 100);
    defer claim.deinit(allocator);
    try std.testing.expect(claim == .dead);

    const dead_payload = (try readDead(allocator, durable_dir, "item-discard")) orelse return error.ExpectedDeadQueueItem;
    defer allocator.free(dead_payload);
    try std.testing.expect(std.mem.indexOf(u8, dead_payload, "operator review") != null);

    try discardDead(allocator, durable_dir, "item-discard");
    try std.testing.expect((try readDead(allocator, durable_dir, "item-discard")) == null);
    try std.testing.expectError(error.WorkflowQueueDeadLetterMissing, discardDead(allocator, durable_dir, "item-discard"));

    try enqueueRequest(allocator, durable_dir, "item-discard", "child", view);
    var after_discard = try tryClaim(allocator, durable_dir, "item-discard", 20, 100);
    defer after_discard.deinit(allocator);
    try std.testing.expect(after_discard == .claimed);
}
