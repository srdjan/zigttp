const std = @import("std");
const zq = @import("zigts");
const http_types = @import("http_types.zig");
const durable_store = @import("durable_store.zig");

const Allocator = std.mem.Allocator;
const HttpHeader = http_types.HttpHeader;
const HttpRequestView = http_types.HttpRequestView;
const HttpResponse = http_types.HttpResponse;
const QueryParam = http_types.QueryParam;
const appendEscaped = durable_store.appendEscaped;
const writeAllChecked = zq.trace.writeAllChecked;

const DEFAULT_LEASE_MS: i64 = 30_000;
const DEFAULT_MAX_ATTEMPTS: u32 = 3;
const MAX_QUEUE_FILE_BYTES: usize = 1024 * 1024;

const c = @cImport({
    @cInclude("dirent.h");
});

const QueueMeta = struct {
    attempts: u32 = 0,
    lease_until_ms: i64 = 0,
    lease_owner: ?[]const u8 = null,
    created_at_ms: i64 = 0,
    updated_at_ms: i64 = 0,
    last_error: ?[]const u8 = null,
};

pub const Header = struct {
    key: []u8,
    value: []u8,
};

pub const QueuedRequest = struct {
    allocator: Allocator,
    target: []u8,
    method: []u8,
    path: []u8,
    url: []u8,
    query_params: []QueryParam,
    body: ?[]u8,
    headers: []Header,
    attempts: u32 = 0,
    lease_until_ms: i64 = 0,
    lease_owner: ?[]u8 = null,
    created_at_ms: i64 = 0,
    updated_at_ms: i64 = 0,
    last_error: ?[]u8 = null,

    pub fn deinit(self: *QueuedRequest) void {
        self.allocator.free(self.target);
        self.allocator.free(self.method);
        self.allocator.free(self.path);
        self.allocator.free(self.url);
        for (self.query_params) |param| {
            self.allocator.free(param.key);
            self.allocator.free(param.value);
        }
        self.allocator.free(self.query_params);
        if (self.body) |body| self.allocator.free(body);
        for (self.headers) |header| {
            self.allocator.free(header.key);
            self.allocator.free(header.value);
        }
        self.allocator.free(self.headers);
        if (self.lease_owner) |lease_owner| self.allocator.free(lease_owner);
        if (self.last_error) |last_error| self.allocator.free(last_error);
        self.* = undefined;
    }

    pub fn toView(self: QueuedRequest, allocator: Allocator) !HttpRequestView {
        var headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
        errdefer headers.deinit(allocator);
        try headers.ensureTotalCapacity(allocator, self.headers.len);
        for (self.headers) |header| {
            headers.appendAssumeCapacity(.{
                .key = header.key,
                .value = header.value,
            });
        }
        return .{
            .method = self.method,
            .path = self.path,
            .url = self.url,
            .query_params = self.query_params,
            .headers = headers,
            .body = self.body,
        };
    }
};

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
    const payload = try requestEnvelopeJson(allocator, target, view, .{
        .created_at_ms = now_ms,
        .updated_at_ms = now_ms,
    });
    defer allocator.free(payload);
    try writeFileAtomic(allocator, p.pending_file, payload);
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

pub fn replayDead(allocator: Allocator, durable_dir: []const u8, id: []const u8) !void {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    try ensureQueueDirs(&p);

    if (try fileExists(p.done_file)) return error.WorkflowQueueResultAlreadyDone;
    if (try fileExists(p.pending_file)) return error.WorkflowQueueItemAlreadyPending;
    if (try fileExists(p.leased_file)) return error.WorkflowQueueItemAlreadyLeased;

    const dead = (try readDeadFile(allocator, p.dead_file)) orelse return error.WorkflowQueueDeadLetterMissing;
    defer allocator.free(dead);

    const request_json = try requestJsonFromDeadLetter(allocator, dead);
    defer allocator.free(request_json);

    var request = try parseQueuedRequest(allocator, request_json);
    defer request.deinit();
    const now_ms = zq.trace.unixMillis();
    const pending_payload = try requestEnvelopeJsonFromQueued(allocator, request, .{
        .attempts = 0,
        .lease_until_ms = 0,
        .lease_owner = null,
        .created_at_ms = if (request.created_at_ms != 0) request.created_at_ms else now_ms,
        .updated_at_ms = now_ms,
        .last_error = null,
    });
    defer allocator.free(pending_payload);

    try writeFileAtomic(allocator, p.pending_file, pending_payload);
    deleteIfExists(p.dead_file);
}

pub fn discardDead(allocator: Allocator, durable_dir: []const u8, id: []const u8) !void {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    if (!(try fileExists(p.dead_file))) return error.WorkflowQueueDeadLetterMissing;
    deleteIfExists(p.dead_file);
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
        deleteIfExists(p.dead_file);
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
        const envelope = try zq.file_io.readFile(allocator, p.leased_file, MAX_QUEUE_FILE_BYTES);
        defer allocator.free(envelope);
        const lease_until_ms = parseLeaseUntilMs(envelope) catch 0;
        if (lease_until_ms > now_ms) return .{ .busy = lease_until_ms };

        // The lease has expired, but another caller may be racing to
        // reclaim the same item at the same time. Renaming the leased file
        // to a reclaim-specific staging path is atomic and single-winner
        // (the same guarantee the pending->leased transition above relies
        // on): only the caller whose rename succeeds may claim and
        // re-dispatch this item, so two racers can never both get `.claimed`
        // back for the same expired lease.
        const reclaim_file = try std.fmt.allocPrint(allocator, "{s}.reclaim-{d}-{x}", .{ p.leased_file, std.c.getpid(), @intFromPtr(&p) });
        defer allocator.free(reclaim_file);
        if (renamePath(p.leased_file, reclaim_file)) {
            var result = try claimLeasedFile(allocator, reclaim_file, p.dead_file, now_ms + lease_ms, now_ms, owner);
            errdefer result.deinit(allocator);
            switch (result) {
                .claimed => try renamePath(reclaim_file, p.leased_file),
                .dead => deleteIfExists(reclaim_file),
                .done, .busy => {},
            }
            return result;
        } else |err| switch (err) {
            // Another caller already won the reclaim race and just set a
            // fresh lease of roughly `now_ms + lease_ms`.
            error.FileNotFound => return .{ .busy = now_ms + lease_ms },
            else => return err,
        }
    }

    return .{ .busy = now_ms + lease_ms };
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

    const payload = try responseEnvelopeJson(allocator, response);
    defer allocator.free(payload);
    try writeFileAtomic(allocator, p.done_file, payload);
    deleteIfExists(p.pending_file);
    deleteIfExists(p.leased_file);
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

    const payload = try errorEnvelopeJson(allocator, code, detail);
    defer allocator.free(payload);
    try writeFileAtomic(allocator, p.done_file, payload);
    deleteIfExists(p.pending_file);
    deleteIfExists(p.leased_file);
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

    const payload = try deadEnvelopeJson(allocator, reason, 0, reason, zq.trace.unixMillis(), "operator", null);
    defer allocator.free(payload);
    try writeFileAtomic(allocator, p.dead_file, payload);
    deleteIfExists(p.pending_file);
    deleteIfExists(p.leased_file);
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
    var request = parseQueuedRequest(allocator, envelope) catch |err| {
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
        deleteIfExists(leased_file);
        return dead;
    };
    errdefer request.deinit();

    const next_attempts = request.attempts +| 1;
    if (next_attempts > DEFAULT_MAX_ATTEMPTS) {
        const last_error = request.last_error orelse "lease expired too many times";
        defer request.deinit();
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
        deleteIfExists(leased_file);
        return dead;
    }

    const payload = try requestEnvelopeJsonFromQueued(allocator, request, .{
        .attempts = next_attempts,
        .lease_until_ms = lease_until_ms,
        .lease_owner = lease_owner,
        .created_at_ms = request.created_at_ms,
        .updated_at_ms = now_ms,
        .last_error = request.last_error,
    });
    defer allocator.free(payload);
    try writeFileAtomic(allocator, leased_file, payload);
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
    const payload = try deadEnvelopeJson(allocator, reason, attempts, last_error, dead_at_ms, source, request_json);
    errdefer allocator.free(payload);
    try writeFileAtomic(allocator, dead_file, payload);
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
    try ensureDir(p.root);
    try ensureDir(p.pending_dir);
    try ensureDir(p.leased_dir);
    try ensureDir(p.done_dir);
    try ensureDir(p.dead_dir);
}

fn ensureDir(path: []const u8) !void {
    const z = try std.heap.c_allocator.dupeZ(u8, path);
    defer std.heap.c_allocator.free(z);
    switch (std.posix.errno(std.posix.system.mkdir(z, 0o755))) {
        .SUCCESS, .EXIST => {},
        else => return error.MakeDirFailed,
    }
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

fn deleteIfExists(path: []const u8) void {
    const z = std.heap.c_allocator.dupeZ(u8, path) catch return;
    defer std.heap.c_allocator.free(z);
    _ = std.c.unlink(z);
}

fn writeFileAtomic(allocator: Allocator, final_path: []const u8, bytes: []const u8) !void {
    const tmp_path = try std.fmt.allocPrint(
        allocator,
        "{s}.tmp-{d}-{x}",
        .{ final_path, std.c.getpid(), @intFromPtr(bytes.ptr) },
    );
    defer allocator.free(tmp_path);
    errdefer deleteIfExists(tmp_path);

    const tmp_z = try allocator.dupeZ(u8, tmp_path);
    defer allocator.free(tmp_z);
    const fd = try std.posix.openatZ(
        std.posix.AT.FDCWD,
        tmp_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        0o644,
    );
    defer std.Io.Threaded.closeFd(fd);
    try writeAllChecked(fd, bytes);
    if (std.c.fsync(fd) != 0) return error.FileWriteFailed;
    try renamePath(tmp_path, final_path);
}

fn requestEnvelopeJson(allocator: Allocator, target: []const u8, view: HttpRequestView, meta: QueueMeta) ![]u8 {
    var payload: std.ArrayList(u8) = .empty;
    errdefer payload.deinit(allocator);
    try payload.appendSlice(allocator, "{\"target\":\"");
    try appendEscaped(&payload, allocator, target);
    try payload.appendSlice(allocator, "\",\"method\":\"");
    try appendEscaped(&payload, allocator, view.method);
    try payload.appendSlice(allocator, "\",\"path\":\"");
    try appendEscaped(&payload, allocator, view.path);
    try payload.appendSlice(allocator, "\",\"url\":\"");
    try appendEscaped(&payload, allocator, view.url);
    try payload.appendSlice(allocator, "\",\"query\":[");
    for (view.query_params, 0..) |param, i| {
        if (i != 0) try payload.appendSlice(allocator, ",");
        try payload.appendSlice(allocator, "{\"key\":\"");
        try appendEscaped(&payload, allocator, param.key);
        try payload.appendSlice(allocator, "\",\"value\":\"");
        try appendEscaped(&payload, allocator, param.value);
        try payload.appendSlice(allocator, "\"}");
    }
    try payload.appendSlice(allocator, "],\"headers\":{");
    for (view.headers.items, 0..) |header, i| {
        if (i != 0) try payload.appendSlice(allocator, ",");
        try payload.appendSlice(allocator, "\"");
        try appendEscaped(&payload, allocator, header.key);
        try payload.appendSlice(allocator, "\":\"");
        try appendEscaped(&payload, allocator, header.value);
        try payload.appendSlice(allocator, "\"");
    }
    try payload.appendSlice(allocator, "},\"body\":");
    if (view.body) |body| {
        try payload.appendSlice(allocator, "\"");
        try appendEscaped(&payload, allocator, body);
        try payload.appendSlice(allocator, "\"");
    } else {
        try payload.appendSlice(allocator, "null");
    }
    var int_buf: [32]u8 = undefined;
    try payload.appendSlice(allocator, ",\"attempts\":");
    try payload.appendSlice(allocator, try std.fmt.bufPrint(&int_buf, "{d}", .{meta.attempts}));
    try payload.appendSlice(allocator, ",\"lease_until_ms\":");
    try payload.appendSlice(allocator, try std.fmt.bufPrint(&int_buf, "{d}", .{meta.lease_until_ms}));
    try payload.appendSlice(allocator, ",\"lease_owner\":");
    if (meta.lease_owner) |lease_owner| {
        try payload.appendSlice(allocator, "\"");
        try appendEscaped(&payload, allocator, lease_owner);
        try payload.appendSlice(allocator, "\"");
    } else {
        try payload.appendSlice(allocator, "null");
    }
    try payload.appendSlice(allocator, ",\"created_at_ms\":");
    try payload.appendSlice(allocator, try std.fmt.bufPrint(&int_buf, "{d}", .{meta.created_at_ms}));
    try payload.appendSlice(allocator, ",\"updated_at_ms\":");
    try payload.appendSlice(allocator, try std.fmt.bufPrint(&int_buf, "{d}", .{meta.updated_at_ms}));
    try payload.appendSlice(allocator, ",\"last_error\":");
    if (meta.last_error) |last_error| {
        try payload.appendSlice(allocator, "\"");
        try appendEscaped(&payload, allocator, last_error);
        try payload.appendSlice(allocator, "\"");
    } else {
        try payload.appendSlice(allocator, "null");
    }
    try payload.appendSlice(allocator, "}");
    return payload.toOwnedSlice(allocator);
}

fn requestEnvelopeJsonFromQueued(allocator: Allocator, request: QueuedRequest, meta: QueueMeta) ![]u8 {
    var view = try request.toView(allocator);
    defer view.headers.deinit(allocator);
    return requestEnvelopeJson(allocator, request.target, view, meta);
}

fn responseEnvelopeJson(allocator: Allocator, response: HttpResponse) ![]u8 {
    var payload: std.ArrayList(u8) = .empty;
    errdefer payload.deinit(allocator);
    try payload.appendSlice(allocator, "{\"status\":");
    var status_buf: [16]u8 = undefined;
    const status_printed = try std.fmt.bufPrint(&status_buf, "{d}", .{response.status});
    try payload.appendSlice(allocator, status_printed);
    try payload.appendSlice(allocator, ",\"headers\":{");
    for (response.headers.items, 0..) |header, i| {
        if (i != 0) try payload.appendSlice(allocator, ",");
        try payload.appendSlice(allocator, "\"");
        try appendEscaped(&payload, allocator, header.key);
        try payload.appendSlice(allocator, "\":\"");
        try appendEscaped(&payload, allocator, header.value);
        try payload.appendSlice(allocator, "\"");
    }
    try payload.appendSlice(allocator, "},\"body\":\"");
    try appendEscaped(&payload, allocator, response.body);
    try payload.appendSlice(allocator, "\"}");
    return payload.toOwnedSlice(allocator);
}

fn errorEnvelopeJson(allocator: Allocator, code: []const u8, detail: []const u8) ![]u8 {
    var payload: std.ArrayList(u8) = .empty;
    errdefer payload.deinit(allocator);
    try payload.appendSlice(allocator, "{\"status\":599,\"headers\":{\"content-type\":\"application/json\"},\"body\":\"{\\\"error\\\":\\\"");
    try appendEscaped(&payload, allocator, code);
    try payload.appendSlice(allocator, "\\\",\\\"detail\\\":\\\"");
    try appendEscaped(&payload, allocator, detail);
    try payload.appendSlice(allocator, "\\\"}\"}");
    return payload.toOwnedSlice(allocator);
}

fn deadEnvelopeJson(
    allocator: Allocator,
    reason: []const u8,
    attempts: u32,
    last_error: []const u8,
    dead_at_ms: i64,
    source: []const u8,
    request_json: ?[]const u8,
) ![]u8 {
    var payload: std.ArrayList(u8) = .empty;
    errdefer payload.deinit(allocator);
    try payload.appendSlice(allocator, "{\"reason\":\"");
    try appendEscaped(&payload, allocator, reason);
    try payload.appendSlice(allocator, "\",\"attempts\":");
    var int_buf: [32]u8 = undefined;
    try payload.appendSlice(allocator, try std.fmt.bufPrint(&int_buf, "{d}", .{attempts}));
    try payload.appendSlice(allocator, ",\"last_error\":\"");
    try appendEscaped(&payload, allocator, last_error);
    try payload.appendSlice(allocator, "\",\"dead_at_ms\":");
    try payload.appendSlice(allocator, try std.fmt.bufPrint(&int_buf, "{d}", .{dead_at_ms}));
    try payload.appendSlice(allocator, ",\"source\":\"");
    try appendEscaped(&payload, allocator, source);
    try payload.appendSlice(allocator, "\",\"request_json\":");
    if (request_json) |json| {
        try payload.appendSlice(allocator, "\"");
        try appendEscaped(&payload, allocator, json);
        try payload.appendSlice(allocator, "\"");
    } else {
        try payload.appendSlice(allocator, "null");
    }
    try payload.appendSlice(allocator, "}");
    return payload.toOwnedSlice(allocator);
}

fn requestJsonFromDeadLetter(allocator: Allocator, source: []const u8) ![]u8 {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, source, .{});
    defer parsed.deinit();
    const obj = switch (parsed.value) {
        .object => |object| object,
        else => return error.InvalidWorkflowQueueDeadLetter,
    };
    const maybe_request = dupeOptionalStringField(allocator, obj, "request_json") catch |err| switch (err) {
        error.InvalidWorkflowQueueEnvelope => return error.InvalidWorkflowQueueDeadLetter,
        else => return err,
    };
    return maybe_request orelse error.WorkflowQueueDeadLetterNotReplayable;
}

fn parseQueuedRequest(allocator: Allocator, source: []const u8) !QueuedRequest {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, source, .{});
    defer parsed.deinit();
    const obj = switch (parsed.value) {
        .object => |object| object,
        else => return error.InvalidWorkflowQueueEnvelope,
    };

    const target = try dupeStringField(allocator, obj, "target");
    errdefer allocator.free(target);
    const method = try dupeStringField(allocator, obj, "method");
    errdefer allocator.free(method);
    const path = try dupeStringField(allocator, obj, "path");
    errdefer allocator.free(path);
    const url = try dupeStringField(allocator, obj, "url");
    errdefer allocator.free(url);
    const query_params = try dupeQueryParamsField(allocator, obj, "query");
    errdefer {
        for (query_params) |param| {
            allocator.free(param.key);
            allocator.free(param.value);
        }
        allocator.free(query_params);
    }
    const body = try dupeOptionalStringField(allocator, obj, "body");
    errdefer if (body) |bytes| allocator.free(bytes);
    const lease_owner = try dupeOptionalStringField(allocator, obj, "lease_owner");
    errdefer if (lease_owner) |bytes| allocator.free(bytes);
    const last_error = try dupeOptionalStringField(allocator, obj, "last_error");
    errdefer if (last_error) |bytes| allocator.free(bytes);

    const headers_value = obj.get("headers") orelse return error.InvalidWorkflowQueueEnvelope;
    const headers_obj = switch (headers_value) {
        .object => |object| object,
        else => return error.InvalidWorkflowQueueEnvelope,
    };

    var headers = try allocator.alloc(Header, headers_obj.count());
    var initialized_headers: usize = 0;
    errdefer {
        for (headers[0..initialized_headers]) |header| {
            allocator.free(header.key);
            allocator.free(header.value);
        }
        allocator.free(headers);
    }
    var i: usize = 0;
    var it = headers_obj.iterator();
    while (it.next()) |entry| : (i += 1) {
        const value = switch (entry.value_ptr.*) {
            .string => |string| string,
            else => return error.InvalidWorkflowQueueEnvelope,
        };
        const key = try allocator.dupe(u8, entry.key_ptr.*);
        const copied_value = allocator.dupe(u8, value) catch |err| {
            allocator.free(key);
            return err;
        };
        headers[i] = .{
            .key = key,
            .value = copied_value,
        };
        initialized_headers += 1;
    }

    return .{
        .allocator = allocator,
        .target = target,
        .method = method,
        .path = path,
        .url = url,
        .query_params = query_params,
        .body = body,
        .headers = headers,
        .attempts = parseU32Field(obj, "attempts", 0),
        .lease_until_ms = parseI64Field(obj, "lease_until_ms", 0),
        .lease_owner = lease_owner,
        .created_at_ms = parseI64Field(obj, "created_at_ms", 0),
        .updated_at_ms = parseI64Field(obj, "updated_at_ms", 0),
        .last_error = last_error,
    };
}

fn parseLeaseUntilMs(source: []const u8) !i64 {
    var parsed = try std.json.parseFromSlice(std.json.Value, std.heap.page_allocator, source, .{});
    defer parsed.deinit();
    const obj = switch (parsed.value) {
        .object => |object| object,
        else => return error.InvalidWorkflowQueueEnvelope,
    };
    const value = obj.get("lease_until_ms") orelse return 0;
    return switch (value) {
        .integer => |integer| @intCast(integer),
        else => 0,
    };
}

fn parseU32Field(obj: std.json.ObjectMap, name: []const u8, default: u32) u32 {
    const value = obj.get(name) orelse return default;
    return switch (value) {
        .integer => |integer| std.math.cast(u32, integer) orelse default,
        else => default,
    };
}

fn parseI64Field(obj: std.json.ObjectMap, name: []const u8, default: i64) i64 {
    const value = obj.get(name) orelse return default;
    return switch (value) {
        .integer => |integer| @intCast(integer),
        else => default,
    };
}

fn dupeStringField(allocator: Allocator, obj: std.json.ObjectMap, name: []const u8) ![]u8 {
    const value = obj.get(name) orelse return error.InvalidWorkflowQueueEnvelope;
    return switch (value) {
        .string => |string| try allocator.dupe(u8, string),
        else => error.InvalidWorkflowQueueEnvelope,
    };
}

fn dupeOptionalStringField(allocator: Allocator, obj: std.json.ObjectMap, name: []const u8) !?[]u8 {
    const value = obj.get(name) orelse return null;
    return switch (value) {
        .null => null,
        .string => |string| try allocator.dupe(u8, string),
        else => error.InvalidWorkflowQueueEnvelope,
    };
}

fn dupeQueryParamsField(allocator: Allocator, obj: std.json.ObjectMap, name: []const u8) ![]QueryParam {
    const query_value = obj.get(name) orelse return try allocator.alloc(QueryParam, 0);
    const arr = switch (query_value) {
        .array => |array| array,
        else => return error.InvalidWorkflowQueueEnvelope,
    };

    var params = try allocator.alloc(QueryParam, arr.items.len);
    var initialized: usize = 0;
    errdefer {
        for (params[0..initialized]) |param| {
            allocator.free(param.key);
            allocator.free(param.value);
        }
        allocator.free(params);
    }

    for (arr.items, 0..) |item, i| {
        const entry = switch (item) {
            .object => |object| object,
            else => return error.InvalidWorkflowQueueEnvelope,
        };
        const key = try dupeStringField(allocator, entry, "key");
        const query_param_value = dupeStringField(allocator, entry, "value") catch |err| {
            allocator.free(key);
            return err;
        };
        params[i] = .{ .key = key, .value = query_param_value };
        initialized += 1;
    }
    return params;
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
    try writeFileAtomic(allocator, p.pending_file, "{\"target\":");

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
    try writeFileAtomic(allocator, p.leased_file, "{\"lease_until_ms\":1,\"target\":false}");

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
    const leased_payload = try requestEnvelopeJson(allocator, "child", view, .{
        .attempts = defaultMaxAttempts(),
        .lease_until_ms = 1,
        .lease_owner = "pid:old",
        .created_at_ms = 1,
        .updated_at_ms = 2,
        .last_error = "boom",
    });
    defer allocator.free(leased_payload);
    try writeFileAtomic(allocator, p.leased_file, leased_payload);

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

    const dead_payload = try deadEnvelopeJson(allocator, "boom", 3, "boom", 1, "workflow-queue", null);
    defer allocator.free(dead_payload);
    try writeFileAtomic(allocator, p.dead_file, dead_payload);

    const pending_payload = try requestEnvelopeJson(allocator, "child", view, .{
        .created_at_ms = 2,
        .updated_at_ms = 2,
    });
    defer allocator.free(pending_payload);
    try writeFileAtomic(allocator, p.pending_file, pending_payload);

    var claim = try tryClaim(allocator, durable_dir, "item-stuck-dead", 20, 100);
    defer claim.deinit(allocator);
    const request = switch (claim) {
        .claimed => |*req| req,
        else => return error.ExpectedClaimedQueueItem,
    };
    try std.testing.expectEqualStrings("child", request.target);
    try std.testing.expect(!(try fileExists(p.dead_file)));
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
