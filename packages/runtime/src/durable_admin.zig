//! Opt-in durable admin API.
//!
//! Exposes a small HTTP control plane for workflow inspection and signal
//! delivery on top of the existing durable oplog and signal store.

const std = @import("std");
const zq = @import("zigts");
const http_types = @import("http_types.zig");
const durable_store_mod = @import("durable_store.zig");

const HttpRequestView = http_types.HttpRequestView;
const HttpResponse = http_types.HttpResponse;

const c = @cImport({
    @cInclude("dirent.h");
});

pub const prefix = "/_zigttp/durable";

pub const Config = struct {
    enabled: bool = false,
    durable_dir: ?[]const u8 = null,
    contract_json: ?[]const u8 = null,
    admin_key: ?[]const u8 = null,
};

const RunStatus = enum {
    pending_timer,
    pending_signal,
    completed,
    recoverable,
    corrupt,

    fn asString(self: RunStatus) []const u8 {
        return switch (self) {
            .pending_timer => "pending_timer",
            .pending_signal => "pending_signal",
            .completed => "completed",
            .recoverable => "recoverable",
            .corrupt => "corrupt",
        };
    }
};

const WaitInfo = union(enum) {
    none,
    timer: i64,
    signal: []const u8,

    fn deinit(self: *WaitInfo, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .signal => |name| allocator.free(name),
            else => {},
        }
        self.* = .none;
    }
};

const RunSummary = struct {
    key: []const u8,
    status: RunStatus,
    request_method: ?[]const u8 = null,
    request_url: ?[]const u8 = null,
    response_status: ?u16 = null,
    event_count: usize = 0,
    wait: WaitInfo = .none,
    allocator: std.mem.Allocator,

    fn deinit(self: *RunSummary) void {
        self.allocator.free(self.key);
        if (self.request_method) |value| self.allocator.free(value);
        if (self.request_url) |value| self.allocator.free(value);
        self.wait.deinit(self.allocator);
    }
};

pub fn handlesPath(path: []const u8) bool {
    return std.mem.eql(u8, path, prefix) or
        std.mem.eql(u8, path, prefix ++ "/") or
        std.mem.startsWith(u8, path, prefix ++ "/");
}

pub fn maybeHandle(
    allocator: std.mem.Allocator,
    config: Config,
    request: HttpRequestView,
) !?HttpResponse {
    if (!config.enabled) return null;
    if (!handlesPath(request.path)) return null;

    if (config.admin_key) |expected| {
        const provided = findHeaderValue(request.headers.items, "x-zigttp-admin-key") orelse {
            return try jsonError(allocator, 401, "missing admin key");
        };
        if (!std.mem.eql(u8, provided, expected)) {
            return try jsonError(allocator, 401, "invalid admin key");
        }
    }

    const durable_dir = config.durable_dir orelse {
        return try jsonError(allocator, 503, "durable execution is not enabled");
    };

    if (std.mem.eql(u8, request.path, prefix) or std.mem.eql(u8, request.path, prefix ++ "/")) {
        if (!std.mem.eql(u8, request.method, "GET")) return try methodNotAllowed(allocator, "GET");
        return try jsonObject(allocator, 200, "{\"ok\":true}");
    }

    if (std.mem.eql(u8, request.path, prefix ++ "/contract")) {
        if (!std.mem.eql(u8, request.method, "GET")) return try methodNotAllowed(allocator, "GET");
        const contract_json = config.contract_json orelse {
            return try jsonError(allocator, 503, "contract is unavailable");
        };
        return try jsonResponse(allocator, 200, contract_json);
    }

    if (std.mem.eql(u8, request.path, prefix ++ "/runs")) {
        if (!std.mem.eql(u8, request.method, "GET")) return try methodNotAllowed(allocator, "GET");
        return try listRunsResponse(allocator, durable_dir);
    }

    if (!std.mem.startsWith(u8, request.path, prefix ++ "/runs/")) {
        return try jsonError(allocator, 404, "unknown durable admin route");
    }

    const tail = request.path[(prefix ++ "/runs/").len..];
    const slash = std.mem.indexOfScalar(u8, tail, '/');
    const key_raw = if (slash) |idx| tail[0..idx] else tail;
    const decoded_key = try decodePathSegment(allocator, key_raw);
    defer allocator.free(decoded_key);

    if (slash == null) {
        if (!std.mem.eql(u8, request.method, "GET")) return try methodNotAllowed(allocator, "GET");
        return try inspectRunResponse(allocator, durable_dir, decoded_key);
    }

    const rest = tail[slash.? + 1 ..];
    if (!std.mem.startsWith(u8, rest, "signals/")) {
        return try jsonError(allocator, 404, "unknown durable admin route");
    }
    if (!std.mem.eql(u8, request.method, "POST")) return try methodNotAllowed(allocator, "POST");

    const signal_raw = rest["signals/".len..];
    if (signal_raw.len == 0) return try jsonError(allocator, 400, "signal name is required");
    if (std.mem.indexOfScalar(u8, signal_raw, '/')) |_| {
        return try jsonError(allocator, 404, "unknown durable admin route");
    }

    const decoded_signal = try decodePathSegment(allocator, signal_raw);
    defer allocator.free(decoded_signal);

    const payload_json = if (request.body) |body|
        try normalizePayloadJson(allocator, body)
    else
        try allocator.dupe(u8, "null");
    defer allocator.free(payload_json);

    return try signalRunResponse(allocator, durable_dir, decoded_key, decoded_signal, payload_json);
}

fn listRunsResponse(allocator: std.mem.Allocator, durable_dir: []const u8) !HttpResponse {
    const runs = try collectRunSummaries(allocator, durable_dir);
    defer {
        for (runs) |*run| run.deinit();
        allocator.free(runs);
    }

    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(allocator);

    try body.appendSlice(allocator, "{\"runs\":[");
    for (runs, 0..) |run, i| {
        if (i > 0) try body.append(allocator, ',');
        try appendRunSummaryJson(&body, allocator, &run, false);
    }
    try body.appendSlice(allocator, "]}");
    return jsonResponseOwned(allocator, 200, try body.toOwnedSlice(allocator));
}

fn inspectRunResponse(allocator: std.mem.Allocator, durable_dir: []const u8, key: []const u8) !HttpResponse {
    const summary_opt = try loadRunSummary(allocator, durable_dir, key);
    if (summary_opt == null) return jsonError(allocator, 404, "durable run not found");

    var summary = summary_opt.?;
    defer summary.deinit();

    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(allocator);
    try appendRunSummaryJson(&body, allocator, &summary, true);
    return jsonResponseOwned(allocator, 200, try body.toOwnedSlice(allocator));
}

fn signalRunResponse(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    key: []const u8,
    signal_name: []const u8,
    payload_json: []const u8,
) !HttpResponse {
    const summary_opt = try loadRunSummary(allocator, durable_dir, key);
    if (summary_opt == null) return jsonError(allocator, 404, "durable run not found");

    var summary = summary_opt.?;
    defer summary.deinit();

    if (summary.status == .completed) {
        return jsonError(allocator, 409, "durable run is already complete");
    }
    if (summary.status == .corrupt) {
        return jsonError(allocator, 409, "durable run is corrupt");
    }

    var store = durable_store_mod.DurableStore.initFs(allocator, durable_dir);
    try store.enqueueSignal(key, signal_name, payload_json);

    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(allocator);
    try body.appendSlice(allocator, "{\"ok\":true,\"key\":");
    try appendJsonString(&body, allocator, key);
    try body.appendSlice(allocator, ",\"signal\":");
    try appendJsonString(&body, allocator, signal_name);
    try body.appendSlice(allocator, "}");
    return jsonResponseOwned(allocator, 202, try body.toOwnedSlice(allocator));
}

fn collectRunSummaries(allocator: std.mem.Allocator, durable_dir: []const u8) ![]RunSummary {
    const dir_path_z = try allocator.dupeZ(u8, durable_dir);
    defer allocator.free(dir_path_z);

    const dir = c.opendir(dir_path_z) orelse return try allocator.alloc(RunSummary, 0);
    defer _ = c.closedir(dir);

    var summaries: std.ArrayList(RunSummary) = .empty;
    errdefer {
        for (summaries.items) |*summary| summary.deinit();
        summaries.deinit(allocator);
    }

    while (c.readdir(dir)) |entry| {
        const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
        const name = std.mem.sliceTo(name_ptr, 0);
        if (!std.mem.startsWith(u8, name, "durable-")) continue;
        if (!std.mem.endsWith(u8, name, ".jsonl")) continue;

        var full_path_buf: [512]u8 = undefined;
        const full_path = std.fmt.bufPrint(&full_path_buf, "{s}/{s}", .{ durable_dir, name }) catch continue;

        if (buildRunSummaryFromPath(allocator, full_path)) |summary| {
            try summaries.append(allocator, summary);
        } else |_| {}
    }

    std.mem.sort(RunSummary, summaries.items, {}, lessRunSummary);
    return summaries.toOwnedSlice(allocator);
}

fn loadRunSummary(allocator: std.mem.Allocator, durable_dir: []const u8, key: []const u8) !?RunSummary {
    const path = try buildDurableOplogPath(allocator, durable_dir, key);
    defer allocator.free(path);

    return buildRunSummaryFromPath(allocator, path) catch |err| switch (err) {
        error.FileNotFound => null,
        else => err,
    };
}

fn buildRunSummaryFromPath(allocator: std.mem.Allocator, path: []const u8) !RunSummary {
    const source = try zq.file_io.readFile(allocator, path, 100 * 1024 * 1024);
    defer allocator.free(source);

    var parsed = zq.trace.parseDurableOplog(allocator, source) catch {
        return .{
            .key = try allocator.dupe(u8, std.fs.path.basename(path)),
            .status = .corrupt,
            .allocator = allocator,
        };
    };
    defer parsed.deinit();

    const key = if (parsed.run_key) |run_key|
        try allocator.dupe(u8, run_key)
    else
        try allocator.dupe(u8, std.fs.path.basename(path));

    const request_method = if (parsed.request) |req|
        try allocator.dupe(u8, req.method)
    else
        null;
    errdefer if (request_method) |value| allocator.free(value);

    const request_url = if (parsed.request) |req|
        try allocator.dupe(u8, req.url)
    else
        null;
    errdefer if (request_url) |value| allocator.free(value);

    var wait = try summarizePendingWait(allocator, parsed.events);
    errdefer wait.deinit(allocator);

    const status: RunStatus = if (parsed.complete)
        .completed
    else switch (wait) {
        .timer => .pending_timer,
        .signal => .pending_signal,
        .none => .recoverable,
    };

    return .{
        .key = key,
        .status = status,
        .request_method = request_method,
        .request_url = request_url,
        .response_status = if (parsed.response) |response| response.status else null,
        .event_count = parsed.events.len,
        .wait = wait,
        .allocator = allocator,
    };
}

fn summarizePendingWait(allocator: std.mem.Allocator, events: []const zq.trace.DurableEvent) !WaitInfo {
    var wait: WaitInfo = .none;
    for (events) |event| {
        switch (event) {
            .wait_timer => |timer_wait| {
                wait.deinit(allocator);
                wait = .{ .timer = timer_wait.until_ms };
            },
            .resume_timer => {
                wait.deinit(allocator);
                wait = .none;
            },
            .wait_signal => |signal_wait| {
                wait.deinit(allocator);
                wait = .{ .signal = try allocator.dupe(u8, signal_wait.name) };
            },
            .resume_signal => {
                wait.deinit(allocator);
                wait = .none;
            },
            else => {},
        }
    }
    return wait;
}

fn appendRunSummaryJson(
    body: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    summary: *const RunSummary,
    detailed: bool,
) !void {
    try body.append(allocator, '{');
    try body.appendSlice(allocator, "\"key\":");
    try appendJsonString(body, allocator, summary.key);
    try body.appendSlice(allocator, ",\"status\":");
    try appendJsonString(body, allocator, summary.status.asString());
    try body.appendSlice(allocator, ",\"eventCount\":");
    try appendInt(body, allocator, summary.event_count);

    if (summary.request_method) |method| {
        try body.appendSlice(allocator, ",\"requestMethod\":");
        try appendJsonString(body, allocator, method);
    }
    if (summary.request_url) |url| {
        try body.appendSlice(allocator, ",\"requestUrl\":");
        try appendJsonString(body, allocator, url);
    }
    if (summary.response_status) |status| {
        try body.appendSlice(allocator, ",\"responseStatus\":");
        try appendInt(body, allocator, status);
    }
    switch (summary.wait) {
        .timer => |until_ms| {
            try body.appendSlice(allocator, ",\"wait\":{\"type\":\"timer\",\"until\":");
            try appendInt(body, allocator, until_ms);
            try body.append(allocator, '}');
        },
        .signal => |name| {
            try body.appendSlice(allocator, ",\"wait\":{\"type\":\"signal\",\"name\":");
            try appendJsonString(body, allocator, name);
            try body.append(allocator, '}');
        },
        .none => {},
    }
    if (detailed) {
        try body.appendSlice(allocator, ",\"detailed\":true");
    }
    try body.append(allocator, '}');
}

fn buildDurableOplogPath(allocator: std.mem.Allocator, durable_dir: []const u8, key: []const u8) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "{s}/durable-{x}.jsonl",
        .{ durable_dir, std.hash.Fnv1a_64.hash(key) },
    );
}

fn normalizePayloadJson(allocator: std.mem.Allocator, payload: []const u8) ![]u8 {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();
    return try allocator.dupe(u8, payload);
}

fn decodePathSegment(allocator: std.mem.Allocator, segment: []const u8) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);

    var i: usize = 0;
    while (i < segment.len) : (i += 1) {
        if (segment[i] == '%' and i + 2 < segment.len) {
            const hex = segment[i + 1 .. i + 3];
            const byte = std.fmt.parseInt(u8, hex, 16) catch return error.InvalidPathEncoding;
            try out.append(allocator, byte);
            i += 2;
            continue;
        }
        if (segment[i] == '+') {
            try out.append(allocator, ' ');
            continue;
        }
        try out.append(allocator, segment[i]);
    }
    return out.toOwnedSlice(allocator);
}

fn methodNotAllowed(allocator: std.mem.Allocator, allowed: []const u8) !HttpResponse {
    var response = try jsonError(allocator, 405, "method not allowed");
    try response.putHeader("Allow", allowed);
    return response;
}

fn jsonError(allocator: std.mem.Allocator, status: u16, message: []const u8) !HttpResponse {
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(allocator);
    try body.appendSlice(allocator, "{\"error\":");
    try appendJsonString(&body, allocator, message);
    try body.append(allocator, '}');
    return jsonResponseOwned(allocator, status, try body.toOwnedSlice(allocator));
}

fn jsonObject(allocator: std.mem.Allocator, status: u16, body_json: []const u8) !HttpResponse {
    return jsonResponse(allocator, status, body_json);
}

fn jsonResponse(allocator: std.mem.Allocator, status: u16, body_json: []const u8) !HttpResponse {
    return jsonResponseOwned(allocator, status, try allocator.dupe(u8, body_json));
}

fn jsonResponseOwned(allocator: std.mem.Allocator, status: u16, body_json: []const u8) !HttpResponse {
    var response = HttpResponse.init(allocator);
    response.status = status;
    response.setBodyOwned(body_json);
    try response.putHeader("Content-Type", "application/json");
    return response;
}

fn appendJsonString(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, value: []const u8) !void {
    try buf.append(allocator, '"');
    for (value) |byte| {
        switch (byte) {
            '"' => try buf.appendSlice(allocator, "\\\""),
            '\\' => try buf.appendSlice(allocator, "\\\\"),
            '\n' => try buf.appendSlice(allocator, "\\n"),
            '\r' => try buf.appendSlice(allocator, "\\r"),
            '\t' => try buf.appendSlice(allocator, "\\t"),
            else => {
                if (byte < 0x20) {
                    var tmp: [6]u8 = undefined;
                    _ = try std.fmt.bufPrint(&tmp, "\\u00{x:0>2}", .{byte});
                    try buf.appendSlice(allocator, &tmp);
                } else {
                    try buf.append(allocator, byte);
                }
            },
        }
    }
    try buf.append(allocator, '"');
}

fn appendInt(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, value: anytype) !void {
    var tmp: [64]u8 = undefined;
    const slice = try std.fmt.bufPrint(&tmp, "{d}", .{value});
    try buf.appendSlice(allocator, slice);
}

fn findHeaderValue(headers: []const http_types.HttpHeader, name: []const u8) ?[]const u8 {
    for (headers) |header| {
        if (std.ascii.eqlIgnoreCase(header.key, name)) return header.value;
    }
    return null;
}

fn lessRunSummary(_: void, lhs: RunSummary, rhs: RunSummary) bool {
    return std.mem.order(u8, lhs.key, rhs.key) == .lt;
}

fn durableAdminTestDir(allocator: std.mem.Allocator, tmp_dir: *const std.testing.TmpDir) ![]u8 {
    return std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}/durable-admin", .{tmp_dir.sub_path});
}

fn makeAdminRequest(
    method: []const u8,
    path: []const u8,
    body: ?[]const u8,
    headers: []const http_types.HttpHeader,
) HttpRequestView {
    return .{
        .method = method,
        .url = path,
        .path = path,
        .headers = .{ .items = @constCast(headers), .capacity = headers.len },
        .body = body,
    };
}

fn writeDurableOplog(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    key: []const u8,
    source: []const u8,
) !void {
    try std.fs.cwd().makePath(durable_dir);
    const path = try buildDurableOplogPath(allocator, durable_dir, key);
    defer allocator.free(path);
    try zq.file_io.writeFile(allocator, path, source);
}

test "durable admin returns null when disabled" {
    const allocator = std.testing.allocator;
    const headers = [_]http_types.HttpHeader{};
    const response = try maybeHandle(
        allocator,
        .{ .enabled = false },
        makeAdminRequest("GET", prefix ++ "/runs", null, headers[0..]),
    );
    try std.testing.expect(response == null);
}

test "durable admin lists runs and inspects pending signal waits" {
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const durable_dir = try durableAdminTestDir(allocator, &tmp_dir);
    defer allocator.free(durable_dir);

    try writeDurableOplog(
        allocator,
        durable_dir,
        "job:123",
        "{\"type\":\"durable_run\",\"key\":\"job:123\"}\n" ++
            "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/jobs/123\",\"headers\":{},\"body\":null}\n" ++
            "{\"type\":\"wait_signal\",\"name\":\"approved\"}\n",
    );
    try writeDurableOplog(
        allocator,
        durable_dir,
        "job:done",
        "{\"type\":\"durable_run\",\"key\":\"job:done\"}\n" ++
            "{\"type\":\"request\",\"method\":\"POST\",\"url\":\"/jobs/done\",\"headers\":{},\"body\":null}\n" ++
            "{\"type\":\"response\",\"status\":201,\"headers\":{},\"body\":\"ok\"}\n" ++
            "{\"type\":\"complete\"}\n",
    );

    const headers = [_]http_types.HttpHeader{};
    var list_response = (try maybeHandle(
        allocator,
        .{ .enabled = true, .durable_dir = durable_dir },
        makeAdminRequest("GET", prefix ++ "/runs", null, headers[0..]),
    )).?;
    defer list_response.deinit();

    try std.testing.expectEqual(@as(u16, 200), list_response.status);
    try std.testing.expect(std.mem.indexOf(u8, list_response.body, "\"key\":\"job:123\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, list_response.body, "\"status\":\"pending_signal\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, list_response.body, "\"key\":\"job:done\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, list_response.body, "\"responseStatus\":201") != null);

    var inspect_response = (try maybeHandle(
        allocator,
        .{ .enabled = true, .durable_dir = durable_dir },
        makeAdminRequest("GET", prefix ++ "/runs/job%3A123", null, headers[0..]),
    )).?;
    defer inspect_response.deinit();

    try std.testing.expectEqual(@as(u16, 200), inspect_response.status);
    try std.testing.expect(std.mem.indexOf(u8, inspect_response.body, "\"wait\":{\"type\":\"signal\",\"name\":\"approved\"}") != null);
    try std.testing.expect(std.mem.indexOf(u8, inspect_response.body, "\"detailed\":true") != null);
}

test "durable admin enforces admin key and enqueues signals" {
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const durable_dir = try durableAdminTestDir(allocator, &tmp_dir);
    defer allocator.free(durable_dir);

    try writeDurableOplog(
        allocator,
        durable_dir,
        "job:123",
        "{\"type\":\"durable_run\",\"key\":\"job:123\"}\n" ++
            "{\"type\":\"request\",\"method\":\"GET\",\"url\":\"/jobs/123\",\"headers\":{},\"body\":null}\n" ++
            "{\"type\":\"wait_signal\",\"name\":\"approved\"}\n",
    );

    const no_headers = [_]http_types.HttpHeader{};
    var unauthorized = (try maybeHandle(
        allocator,
        .{ .enabled = true, .durable_dir = durable_dir, .admin_key = "secret" },
        makeAdminRequest("POST", prefix ++ "/runs/job%3A123/signals/approved", "{\"ok\":true}", no_headers[0..]),
    )).?;
    defer unauthorized.deinit();
    try std.testing.expectEqual(@as(u16, 401), unauthorized.status);

    const key_headers = [_]http_types.HttpHeader{
        .{ .key = "x-zigttp-admin-key", .value = "secret" },
    };
    var accepted = (try maybeHandle(
        allocator,
        .{ .enabled = true, .durable_dir = durable_dir, .admin_key = "secret" },
        makeAdminRequest("POST", prefix ++ "/runs/job%3A123/signals/approved", "{\"ok\":true}", key_headers[0..]),
    )).?;
    defer accepted.deinit();

    try std.testing.expectEqual(@as(u16, 202), accepted.status);
    try std.testing.expect(std.mem.indexOf(u8, accepted.body, "\"signal\":\"approved\"") != null);

    var store = durable_store_mod.DurableStore.initFs(allocator, durable_dir);
    var consumed = (try store.tryConsumeSignal("job:123", "approved", std.time.milliTimestamp())).?;
    defer consumed.deinit();
    try std.testing.expectEqualStrings("{\"ok\":true}", consumed.payload_json);
}
