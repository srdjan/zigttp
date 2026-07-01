//! On-disk JSON envelope encode/decode for workflow_queue.zig's persisted
//! request/response/error/dead-letter files. Split out of workflow_queue.zig
//! (which owns the claim/lease/dead-letter file-state-machine) since this
//! codec has no dependency on that state machine beyond the shared
//! QueuedRequest/Header/QueueMeta shapes it defines.

const std = @import("std");
const http_types = @import("http_types.zig");
const durable_store = @import("durable_store.zig");

const Allocator = std.mem.Allocator;
const HttpHeader = http_types.HttpHeader;
const HttpRequestView = http_types.HttpRequestView;
const HttpResponse = http_types.HttpResponse;
const QueryParam = http_types.QueryParam;
const appendEscaped = durable_store.appendEscaped;

pub const QueueMeta = struct {
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

pub fn requestEnvelopeJson(allocator: Allocator, target: []const u8, view: HttpRequestView, meta: QueueMeta) ![]u8 {
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

pub fn requestEnvelopeJsonFromQueued(allocator: Allocator, request: QueuedRequest, meta: QueueMeta) ![]u8 {
    var view = try request.toView(allocator);
    defer view.headers.deinit(allocator);
    return requestEnvelopeJson(allocator, request.target, view, meta);
}

pub fn responseEnvelopeJson(allocator: Allocator, response: HttpResponse) ![]u8 {
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

pub fn errorEnvelopeJson(allocator: Allocator, code: []const u8, detail: []const u8) ![]u8 {
    var payload: std.ArrayList(u8) = .empty;
    errdefer payload.deinit(allocator);
    try payload.appendSlice(allocator, "{\"status\":599,\"headers\":{\"content-type\":\"application/json\"},\"body\":\"{\\\"error\\\":\\\"");
    try appendEscaped(&payload, allocator, code);
    try payload.appendSlice(allocator, "\\\",\\\"detail\\\":\\\"");
    try appendEscaped(&payload, allocator, detail);
    try payload.appendSlice(allocator, "\\\"}\"}");
    return payload.toOwnedSlice(allocator);
}

pub fn deadEnvelopeJson(
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

pub fn requestJsonFromDeadLetter(allocator: Allocator, source: []const u8) ![]u8 {
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

pub fn parseQueuedRequest(allocator: Allocator, source: []const u8) !QueuedRequest {
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

pub fn parseLeaseUntilMs(source: []const u8) !i64 {
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
