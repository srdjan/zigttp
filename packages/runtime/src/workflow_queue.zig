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
const MAX_QUEUE_FILE_BYTES: usize = 1024 * 1024;

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
    claimed: QueuedRequest,
    /// Another caller holds the active lease. Carries the wall-clock time
    /// (same clock as `now_ms`) after which retrying is worthwhile, so
    /// callers can wake near the real lease expiry instead of polling on a
    /// fixed interval.
    busy: i64,

    pub fn deinit(self: *ClaimResult, allocator: Allocator) void {
        switch (self.*) {
            .done => |bytes| allocator.free(bytes),
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
    if (try fileExists(p.pending_file)) return;
    if (try fileExists(p.leased_file)) return;

    const payload = try requestEnvelopeJson(allocator, target, view, 0);
    defer allocator.free(payload);
    try writeFileAtomic(allocator, p.pending_file, payload);
}

pub fn readResult(allocator: Allocator, durable_dir: []const u8, id: []const u8) !?[]u8 {
    var p = try queuePaths(allocator, durable_dir, id);
    defer p.deinit();
    return readDoneFile(allocator, p.done_file);
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

    if (renamePath(p.pending_file, p.leased_file)) {
        return try claimLeasedFile(allocator, p.leased_file, now_ms + lease_ms);
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
            var result = try claimLeasedFile(allocator, reclaim_file, now_ms + lease_ms);
            errdefer result.deinit(allocator);
            try renamePath(reclaim_file, p.leased_file);
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

    var payload: std.ArrayList(u8) = .empty;
    defer payload.deinit(allocator);
    try payload.appendSlice(allocator, "{\"reason\":\"");
    try appendEscaped(&payload, allocator, reason);
    try payload.appendSlice(allocator, "\"}");
    try writeFileAtomic(allocator, p.dead_file, payload.items);
    deleteIfExists(p.pending_file);
    deleteIfExists(p.leased_file);
}

fn claimLeasedFile(allocator: Allocator, leased_file: []const u8, lease_until_ms: i64) !ClaimResult {
    const envelope = try zq.file_io.readFile(allocator, leased_file, MAX_QUEUE_FILE_BYTES);
    defer allocator.free(envelope);
    var request = try parseQueuedRequest(allocator, envelope);
    errdefer request.deinit();

    const payload = try requestEnvelopeJsonFromQueued(allocator, request, lease_until_ms);
    defer allocator.free(payload);
    try writeFileAtomic(allocator, leased_file, payload);
    return .{ .claimed = request };
}

fn queuePaths(allocator: Allocator, durable_dir: []const u8, id: []const u8) !QueuePaths {
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

fn requestEnvelopeJson(allocator: Allocator, target: []const u8, view: HttpRequestView, lease_until_ms: i64) ![]u8 {
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
    try payload.appendSlice(allocator, ",\"lease_until_ms\":");
    var lease_buf: [32]u8 = undefined;
    const lease_printed = try std.fmt.bufPrint(&lease_buf, "{d}", .{lease_until_ms});
    try payload.appendSlice(allocator, lease_printed);
    try payload.appendSlice(allocator, "}");
    return payload.toOwnedSlice(allocator);
}

fn requestEnvelopeJsonFromQueued(allocator: Allocator, request: QueuedRequest, lease_until_ms: i64) ![]u8 {
    var view = try request.toView(allocator);
    defer view.headers.deinit(allocator);
    return requestEnvelopeJson(allocator, request.target, view, lease_until_ms);
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
}
