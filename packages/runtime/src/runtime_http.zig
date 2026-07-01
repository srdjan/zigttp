//! Outbound HTTP / fetch / service native implementations extracted from
//! zruntime.zig (review finding M1).
//!
//! This module owns the request/response/headers JS constructors, the
//! synchronous `fetch` bridge, durable-fetch caching, the `zigttp:service`
//! call path, the parallel-I/O fetch workers, and the low-level
//! `zigttp:http.request` native. These are plain free functions taking an
//! explicit `*Runtime` (or recovering it from the ambient `current_runtime`
//! threadlocal), so they live here and back-import `Runtime` from zruntime.
//! zruntime registers the exported native callbacks during binding setup, and
//! runtime_workflow reaches the response builders here by alias.

const std = @import("std");
const ascii = std.ascii;
const zq = @import("zigts");
const zruntime = @import("zruntime.zig");
const durable_store_mod = @import("durable_store.zig");
const durable_fetch = @import("durable_fetch.zig");
const http_parser = @import("http_parser.zig");
const retry_backoff = @import("retry_backoff.zig");

const Runtime = zruntime.Runtime;
const HttpResponse = zruntime.HttpResponse;
const HttpRequestView = zruntime.HttpRequestView;
const HttpRequestOwned = zruntime.HttpRequestOwned;
const QueryParam = zruntime.QueryParam;
const HttpHeader = zruntime.HttpHeader;
const ResponseHeader = zruntime.ResponseHeader;
const RuntimeConfig = zruntime.RuntimeConfig;

const FetchResponseObjects = struct {
    value: zq.JSValue,
    response: *zq.JSObject,
    headers: *zq.JSObject,
};

const appendEscapedJson = durable_store_mod.appendEscaped;

const unixMillis = zq.trace.unixMillis;

fn effectiveOutboundTimeoutMs(rt: *Runtime) u32 {
    var timeout_ms = rt.config.outbound_timeout_ms;
    if (rt.active_durable_run) |active| {
        if (active.step_timeout_deadline_ms) |deadline_ms| {
            const remaining_ms = deadline_ms - unixMillis();
            const step_timeout_ms: u32 = if (remaining_ms <= 0)
                1
            else
                @intCast(@min(remaining_ms, std.math.maxInt(u32)));
            if (timeout_ms == 0 or step_timeout_ms < timeout_ms) timeout_ms = step_timeout_ms;
        }
    }
    return timeout_ms;
}

fn outboundTimeout(timeout_ms: u32) std.Io.Timeout {
    if (timeout_ms == 0) return .none;
    const duration = std.Io.Duration.fromMilliseconds(@intCast(timeout_ms));
    return .{ .duration = .{ .raw = duration, .clock = .awake } };
}

const natives = @import("runtime_natives.zig");
pub const getStringData = natives.getStringData;
// Context-aware string accessor that flattens concat ropes into the request
// arena. The local natives.getStringData only handles flat strings, slices, and
// already-flattened (.leaf) ropes — it returns null for a concat rope, which is
// what string concatenation (>= 64 bytes) produces. Response extraction must use
// this flattening variant or it silently drops rope/slice bodies and headers.
const getStringDataCtx = zq.builtins.helpers.getStringDataCtx;
const getDynamicProperty = natives.getDynamicProperty;
const getObjectProperty = natives.getObjectProperty;
const getHeaderAtom = natives.getHeaderAtom;
const HeaderAssignMode = natives.HeaderAssignMode;
const statusTextFor = natives.statusTextFor;
const getNamedGlobalPrototype = natives.getNamedGlobalPrototype;
const throwTypeError = natives.throwTypeError;

pub fn beginBodyRead(ctx: *zq.Context, this: zq.JSValue) zq.JSValue {
    if (!this.isObject()) {
        return throwTypeError(ctx, "Body reader target must be an object");
    }
    const obj = this.toPtr(zq.JSObject);
    if (zruntime.current_runtime) |rt| {
        if (rt.consumed_body_objects.contains(obj)) {
            return throwTypeError(ctx, "Body has already been consumed");
        }
        rt.consumed_body_objects.put(rt.allocator, obj, {}) catch {
            return throwTypeError(ctx, "Failed to track body consumption");
        };
    }
    return getBodyValue(ctx, this) orelse zq.JSValue.undefined_val;
}

const getHeaderValueCaseInsensitive = natives.getHeaderValueCaseInsensitive;
const validateHeaderPair = natives.validateHeaderPair;
const setHeaderValue = natives.setHeaderValue;
const createHeadersObjectDynamic = natives.createHeadersObjectDynamic;
const copyHeadersIntoObject = natives.copyHeadersIntoObject;
const splitPathAndQuery = natives.splitPathAndQuery;
const buildQueryObject = natives.buildQueryObject;
const upgradeResponseValue = natives.upgradeResponseValue;

const OwnedResponseHeader = struct {
    name: []u8,
    value: []u8,
};

const OwnedResponseHead = struct {
    reason: []u8,
    headers: std.ArrayListUnmanaged(OwnedResponseHeader) = .empty,

    fn deinit(self: *OwnedResponseHead, allocator: std.mem.Allocator) void {
        allocator.free(self.reason);
        for (self.headers.items) |header| {
            allocator.free(header.name);
            allocator.free(header.value);
        }
        self.headers.deinit(allocator);
    }

    fn contentType(self: *const OwnedResponseHead) ?[]const u8 {
        for (self.headers.items) |header| {
            if (ascii.eqlIgnoreCase(header.name, "content-type")) {
                return header.value;
            }
        }
        return null;
    }
};

fn snapshotResponseHead(allocator: std.mem.Allocator, head: anytype) !OwnedResponseHead {
    var owned = OwnedResponseHead{
        .reason = try allocator.dupe(u8, head.reason),
    };
    errdefer owned.deinit(allocator);

    var header_it = head.iterateHeaders();
    while (header_it.next()) |header| {
        const name = try allocator.dupe(u8, header.name);
        errdefer allocator.free(name);
        const value = try allocator.dupe(u8, header.value);
        errdefer allocator.free(value);
        try owned.headers.append(allocator, .{
            .name = name,
            .value = value,
        });
    }

    return owned;
}

fn createResponseHeadersObject(rt: *Runtime) !*zq.JSObject {
    if (rt.ctx.http_shapes) |shapes| {
        return rt.ctx.createObjectWithClass(shapes.response_headers.class_idx, rt.headers_prototype);
    }
    return rt.ctx.createObject(rt.headers_prototype);
}

pub fn createFetchResponse(rt: *Runtime, status: u16, status_text: []const u8, body: []const u8, content_type: ?[]const u8) !FetchResponseObjects {
    const body_val = try rt.ctx.createString(body);
    const body_str = body_val.toPtr(zq.JSString);
    const response_val = try zq.http.createResponseFromString(
        rt.ctx,
        body_str,
        status,
        content_type orelse "application/octet-stream",
    );
    const response_obj = response_val.toPtr(zq.JSObject);
    if (rt.response_prototype) |proto| {
        response_obj.prototype = proto;
    }

    const status_text_atom = if (rt.ctx.http_strings) |cache|
        cache.status_text_atom
    else
        try rt.ctx.atoms.intern("statusText");
    const status_text_val = try rt.ctx.createString(status_text);
    try rt.ctx.setPropertyChecked(response_obj, status_text_atom, status_text_val);

    const headers_obj = try createResponseHeadersObject(rt);
    try rt.ctx.setPropertyChecked(response_obj, zq.Atom.headers, headers_obj.toValue());

    if (content_type) |ct| {
        const ct_atom = try getHeaderAtom(rt.ctx, "content-type");
        const ct_val = try rt.ctx.createString(ct);
        try rt.ctx.setPropertyChecked(headers_obj, ct_atom, ct_val);
    }

    return .{
        .value = response_val,
        .response = response_obj,
        .headers = headers_obj,
    };
}

pub fn createFetchErrorResponse(rt: *Runtime, err_code: []const u8, details: []const u8) !zq.JSValue {
    const error_body = try httpRequestErrorJsonAlloc(rt.allocator, err_code, details);
    defer rt.allocator.free(error_body);

    const created = try createFetchResponse(rt, 599, err_code, error_body, "application/json");
    const error_atom = try rt.ctx.atoms.intern("error");
    const details_atom = try rt.ctx.atoms.intern("details");
    const error_val = try rt.ctx.createString(err_code);
    const details_val = try rt.ctx.createString(details);
    try rt.ctx.setPropertyChecked(created.response, error_atom, error_val);
    try rt.ctx.setPropertyChecked(created.response, details_atom, details_val);
    return created.value;
}

fn getBodyValue(ctx: *zq.Context, this: zq.JSValue) ?zq.JSValue {
    if (!this.isObject()) return null;
    const pool = ctx.hidden_class_pool orelse return null;
    const obj = this.toPtr(zq.JSObject);
    return obj.getProperty(pool, zq.Atom.body);
}

pub fn headersGetNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    if (!this.isObject() or args.len == 0) return zq.JSValue.undefined_val;

    const wanted = getStringDataCtx(args[0], ctx) orelse return zq.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return zq.JSValue.undefined_val;
    const headers_obj = this.toPtr(zq.JSObject);
    return getHeaderValueCaseInsensitive(ctx, headers_obj, pool, wanted) orelse zq.JSValue.undefined_val;
}

pub fn headersHasNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    if (!this.isObject() or args.len == 0) return zq.JSValue.false_val;

    const wanted = getStringDataCtx(args[0], ctx) orelse return zq.JSValue.false_val;
    const pool = ctx.hidden_class_pool orelse return zq.JSValue.false_val;
    const headers_obj = this.toPtr(zq.JSObject);
    return zq.JSValue.fromBool(getHeaderValueCaseInsensitive(ctx, headers_obj, pool, wanted) != null);
}

fn headersSetLikeNative(
    ctx: *zq.Context,
    this: zq.JSValue,
    args: []const zq.JSValue,
    mode: HeaderAssignMode,
) !zq.JSValue {
    if (!this.isObject()) return throwTypeError(ctx, "Headers target must be an object");
    if (args.len < 2) return throwTypeError(ctx, "Headers mutation requires name and value");

    const name = getStringDataCtx(args[0], ctx) orelse return throwTypeError(ctx, "Header name must be string");
    const value = getStringDataCtx(args[1], ctx) orelse return throwTypeError(ctx, "Header value must be string");
    if (!validateHeaderPair(name, value)) {
        return throwTypeError(ctx, "Header name/value is invalid");
    }
    try setHeaderValue(ctx, this.toPtr(zq.JSObject), name, value, mode);
    return zq.JSValue.undefined_val;
}

pub fn headersSetNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return headersSetLikeNative(ctx, this, args, .replace);
}

pub fn headersAppendNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return headersSetLikeNative(ctx, this, args, .append);
}

pub fn headersDeleteNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    if (!this.isObject() or args.len == 0) return zq.JSValue.undefined_val;

    const wanted = getStringDataCtx(args[0], ctx) orelse return zq.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return zq.JSValue.undefined_val;
    const headers_obj = this.toPtr(zq.JSObject);
    const keys = headers_obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return zq.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        if (!ascii.eqlIgnoreCase(key_name, wanted)) continue;
        _ = headers_obj.deleteProperty(pool, key_atom);
    }
    return zq.JSValue.undefined_val;
}

pub fn headersConstructorNative(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    const headers_obj = try createHeadersObjectDynamic(ctx);

    if (args.len > 0 and !args[0].isUndefined() and !args[0].isNull()) {
        copyHeadersIntoObject(ctx, args[0], headers_obj) catch |err| switch (err) {
            error.InvalidHeaders => return throwTypeError(ctx, "Headers(init) expects object<string,string>"),
            else => return err,
        };
    }

    return headers_obj.toValue();
}

pub fn requestConstructorNative(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));

    const url = if (args.len > 0) getStringData(args[0]) else null;
    if (url == null or url.?.len == 0) {
        return throwTypeError(ctx, "Request(url, init?) expects a non-empty string url");
    }

    var method: []const u8 = "GET";
    var body: ?[]const u8 = null;
    const headers_obj = try createHeadersObjectDynamic(ctx);

    if (args.len > 1 and args[1].isObject()) {
        const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
        const init = args[1].toPtr(zq.JSObject);

        if (getObjectProperty(ctx, init, pool, zq.Atom.method, "method")) |method_val| {
            method = getStringData(method_val) orelse return throwTypeError(ctx, "Request method must be string");
        }
        if (getObjectProperty(ctx, init, pool, zq.Atom.body, "body")) |body_val| {
            if (body_val.isNull() or body_val.isUndefined()) {
                body = null;
            } else {
                body = getStringData(body_val) orelse return throwTypeError(ctx, "Request body must be string|null");
            }
        }
        if (getObjectProperty(ctx, init, pool, zq.Atom.headers, "headers")) |headers_val| {
            copyHeadersIntoObject(ctx, headers_val, headers_obj) catch |err| switch (err) {
                error.InvalidHeaders => return throwTypeError(ctx, "Request headers must be object<string,string>"),
                else => return err,
            };
        }
    }

    const split = splitPathAndQuery(url.?);
    const parsed_query = try http_parser.parseQueryString(ctx.allocator, split.query_string, http_parser.DEFAULT_MAX_QUERY_LENGTH);
    defer if (parsed_query.storage) |storage| ctx.allocator.free(storage);
    defer if (parsed_query.decoded_storage) |storage| ctx.allocator.free(storage);

    const req_obj = try ctx.createObject(getNamedGlobalPrototype(ctx, "Request"));
    const url_val = try ctx.createString(url.?);
    try ctx.setPropertyChecked(req_obj, zq.Atom.url, url_val);
    try ctx.setPropertyChecked(req_obj, zq.Atom.method, try ctx.createString(method));
    try ctx.setPropertyChecked(req_obj, zq.Atom.path, try ctx.createString(split.path));
    try ctx.setPropertyChecked(req_obj, zq.Atom.query, (try buildQueryObject(ctx, parsed_query.params)).toValue());
    if (body) |body_str| {
        try ctx.setPropertyChecked(req_obj, zq.Atom.body, try ctx.createString(body_str));
    } else {
        try ctx.setPropertyChecked(req_obj, zq.Atom.body, zq.JSValue.undefined_val);
    }
    try ctx.setPropertyChecked(req_obj, zq.Atom.headers, headers_obj.toValue());

    return req_obj.toValue();
}

pub fn responseConstructorNative(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    const response_val = try zq.http.responseConstructor(ctx, zq.JSValue.undefined_val, args);
    if (ctx.hasException()) return error.NativeFunctionError;

    const upgraded = try upgradeResponseValue(ctx, response_val);
    if (args.len < 2 or !args[1].isObject()) return upgraded;

    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const init = args[1].toPtr(zq.JSObject);
    const headers_val = getObjectProperty(ctx, init, pool, zq.Atom.headers, "headers") orelse return upgraded;

    const response_obj = upgraded.toPtr(zq.JSObject);
    const headers_obj = try createHeadersObjectDynamic(ctx);
    if (response_obj.getProperty(pool, zq.Atom.headers)) |existing_headers_val| {
        if (existing_headers_val.isObject()) {
            try copyHeadersIntoObject(ctx, existing_headers_val, headers_obj);
        }
    }
    copyHeadersIntoObject(ctx, headers_val, headers_obj) catch |err| switch (err) {
        error.InvalidHeaders => return throwTypeError(ctx, "Response headers must be object<string,string>"),
        else => return err,
    };
    try ctx.setPropertyChecked(response_obj, zq.Atom.headers, headers_obj.toValue());
    return upgraded;
}

fn wrapResponseStatic(
    ctx: *zq.Context,
    this: zq.JSValue,
    args: []const zq.JSValue,
    func: *const fn (*anyopaque, zq.JSValue, []const zq.JSValue) anyerror!zq.JSValue,
) !zq.JSValue {
    const result = try func(ctx, this, args);
    return upgradeResponseValue(ctx, result);
}

pub fn responseJsonStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseJson);
}

pub fn responseTextStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseText);
}

pub fn responseHtmlStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseHtml);
}

pub fn responseRawJsonStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseRawJson);
}

pub fn responseRedirectStaticNative(ctx_ptr: *anyopaque, this: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    return wrapResponseStatic(ctx, this, args, zq.http.responseRedirect);
}

/// Split a slice of headers (any struct with .key/.value) into parallel
/// name/value arrays for trace recording. Returns the count written.
pub fn splitHeaderKV(headers: anytype, names: *[64][]const u8, values: *[64][]const u8) usize {
    const count = @min(headers.len, 64);
    for (headers[0..count], 0..) |hdr, i| {
        names[i] = hdr.key;
        values[i] = hdr.value;
    }
    return count;
}

fn outboundHostViolation(rt: *Runtime, host: []const u8) ?[]const u8 {
    if (rt.config.outbound_allow_host) |allowed_host| {
        if (!ascii.eqlIgnoreCase(host, allowed_host)) {
            zq.policy.emitDenied(.{
                .action = .http_outbound,
                .resource = .{ .kind = zq.policy.resource_kind_host, .id = host },
            }, .not_in_allowlist);
            return allowed_host;
        }
    }
    if (!rt.ctx.capability_policy.allowsEgressHost(host)) {
        zq.policy.emitDenied(.{
            .action = .http_outbound,
            .resource = .{ .kind = zq.policy.resource_kind_host, .id = host },
        }, .not_in_allowlist);
        return "capability policy";
    }
    return null;
}

pub fn fetchSyncNative(ctx_ptr: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const rt = zruntime.current_runtime orelse return error.RuntimeUnavailable;
    const result = fetchSyncResult(rt, args) catch |err| {
        return createFetchErrorResponse(rt, "InternalError", @errorName(err));
    };

    // Record fetchSync to trace (it's outside virtual module dispatch)
    const ctx: *zq.Context = @ptrCast(@alignCast(ctx_ptr));
    if (ctx.getModuleState(zq.TraceRecorder, zq.TRACE_STATE_SLOT)) |recorder| {
        recorder.recordIO("http", "fetchSync", ctx, args, result);
    }

    return result;
}

/// Parsed fetch arguments: URL, URI, and optional init object.
/// Shared by fetchSyncResult and collectFetchForParallel.
const FetchArgs = struct {
    url: []const u8,
    uri: std.Uri,
    init_obj: ?*zq.JSObject,
};

const FetchArgsResult = union(enum) {
    ok: FetchArgs,
    err: zq.JSValue,
};

const FetchInitOptions = struct {
    method: std.http.Method = .GET,
    body: ?[]const u8 = null,
    max_response_bytes: usize,
    headers: std.ArrayList(std.http.Header) = .empty,

    fn deinit(self: *FetchInitOptions, allocator: std.mem.Allocator) void {
        self.headers.deinit(allocator);
    }
};

const FetchInitResult = union(enum) {
    ok: FetchInitOptions,
    err: zq.JSValue,
};

fn fetchInitError(
    rt: *Runtime,
    allocator: std.mem.Allocator,
    options: *FetchInitOptions,
    code: []const u8,
    details: []const u8,
) !FetchInitResult {
    options.deinit(allocator);
    return .{ .err = try createFetchErrorResponse(rt, code, details) };
}

/// Resolve a URI host into a fixed buffer without the std panic. `Uri.getHost`
/// percent-decodes the host into the caller's `[HostName.max_len]u8` buffer and
/// `catch unreachable`s on overflow (std Uri.zig), so a crafted URL whose
/// percent-DECODED host exceeds 255 bytes (e.g. `https://%41…×300.com/`) would
/// crash the worker. The decoded length never exceeds the encoded length, so an
/// encoded host that fits the buffer is always safe; reject an over-long host up
/// front (a real DNS name is <= 253 bytes), returning the same UriMissingHost
/// error the call sites already surface as a clean InvalidUrl.
fn resolveHostSafe(uri: std.Uri, buf: *[std.Io.net.HostName.max_len]u8) std.Uri.GetHostError!std.Io.net.HostName {
    if (uri.host) |h| {
        const encoded = switch (h) {
            .raw, .percent_encoded => |s| s,
        };
        if (encoded.len > buf.len) return error.UriMissingHost;
    }
    return uri.getHost(buf);
}

/// Parse and validate the (url, init?) arguments common to all fetch call sites.
/// Returns a JS error response (status 599) on validation failure.
pub fn parseFetchArgs(rt: *Runtime, pool: *const zq.HiddenClassPool, args: []const zq.JSValue) !FetchArgsResult {
    if (args.len == 0) {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidArgs", "expected url string or init object") };
    }

    var init_obj: ?*zq.JSObject = null;
    const url = blk: {
        if (args[0].isObject()) {
            init_obj = args[0].toPtr(zq.JSObject);
            const url_val = getObjectProperty(rt.ctx, init_obj.?, pool, zq.Atom.url, "url") orelse {
                return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "missing url field") };
            };
            const url_str = getStringData(url_val) orelse {
                return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url must be a non-empty string") };
            };
            if (url_str.len == 0) {
                return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url must be a non-empty string") };
            }
            break :blk url_str;
        }
        const url_str = getStringData(args[0]) orelse {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidArgs", "expected url string or init object") };
        };
        if (url_str.len == 0) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url must be a non-empty string") };
        }
        if (args.len > 1 and args[1].isObject()) {
            init_obj = args[1].toPtr(zq.JSObject);
        }
        break :blk url_str;
    };

    // The base `url` is the compile-time literal (so egress stays provable);
    // any dynamic `query` object in the init is appended here at runtime.
    const final_url = switch (try buildFetchUrl(rt, pool, url, init_obj)) {
        .ok => |u| u,
        .err => |e| return .{ .err = e },
    };
    // From here `final_url` is owned on rt.allocator and the caller frees it.
    // The local .err returns below must free it explicitly: a union .err is a
    // normal (non-error) return, so an errdefer would not fire.
    const uri = std.Uri.parse(final_url) catch {
        rt.allocator.free(final_url);
        return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url parse failed") };
    };
    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = resolveHostSafe(uri, &host_buf) catch {
        rt.allocator.free(final_url);
        return .{ .err = try createFetchErrorResponse(rt, "InvalidUrl", "url host is required") };
    };
    if (outboundHostViolation(rt, host.bytes)) |details| {
        rt.allocator.free(final_url);
        return .{ .err = try createFetchErrorResponse(rt, "HostNotAllowed", details) };
    }

    return .{ .ok = .{ .url = final_url, .uri = uri, .init_obj = init_obj } };
}

const FetchUrlResult = union(enum) {
    ok: []const u8,
    err: zq.JSValue,
};

/// Build the final fetch URL from the literal base plus an optional dynamic
/// `query` object on the init. The base URL stays a compile-time literal so
/// the contract proves the egress host; the query object carries the dynamic,
/// possibly user-supplied values (e.g. weather coordinates), percent-encoded
/// here. Mirrors the appendServiceQuery encoding used by zigttp:service.
///
/// Returns an owned slice on `rt.allocator` (a copy of the base when there is
/// no query, for uniform caller ownership), or a JS error response if `query`
/// is present but not an object / holds an unencodable value.
pub fn buildFetchUrl(
    rt: *Runtime,
    pool: *const zq.HiddenClassPool,
    base_url: []const u8,
    init_obj: ?*zq.JSObject,
) !FetchUrlResult {
    const query_obj = blk: {
        const init = init_obj orelse break :blk null;
        const query_val = getDynamicProperty(rt.ctx, init, pool, "query") orelse break :blk null;
        if (query_val.isUndefined() or query_val.isNull()) break :blk null;
        if (!query_val.isObject()) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidQuery", "query must be object<string, string|number|boolean>") };
        }
        break :blk query_val.toPtr(zq.JSObject);
    };

    const q = query_obj orelse return .{ .ok = try rt.allocator.dupe(u8, base_url) };

    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(rt.allocator);
    try buf.appendSlice(rt.allocator, base_url);

    const keys = try q.getOwnEnumerableKeys(rt.allocator, pool);
    defer rt.allocator.free(keys);

    var wrote_any = std.mem.indexOfScalar(u8, base_url, '?') != null;
    for (keys) |key_atom| {
        const key_name = rt.ctx.atoms.getName(key_atom) orelse continue;
        const item = q.getOwnProperty(pool, key_atom) orelse continue;
        if (item.isUndefined()) continue;

        try buf.append(rt.allocator, if (wrote_any) '&' else '?');
        try appendPercentEncoded(rt, &buf, key_name);
        try buf.append(rt.allocator, '=');
        if (!try appendEncodedJsValue(rt, &buf, item)) {
            buf.deinit(rt.allocator);
            // Disarm the errdefer: a `.err` union return does not trip it, but
            // the fallible createFetchErrorResponse below would, double-deinit'ing
            // the just-freed (now undefined) buffer. Reset to empty so the
            // errdefer's deinit is a safe no-op on either exit.
            buf = .empty;
            return .{ .err = try createFetchErrorResponse(rt, "InvalidQuery", "query values must be string|number|boolean") };
        }
        wrote_any = true;
    }

    return .{ .ok = try buf.toOwnedSlice(rt.allocator) };
}

fn parseFetchInitOptions(
    rt: *Runtime,
    allocator: std.mem.Allocator,
    pool: *const zq.HiddenClassPool,
    init_obj: ?*zq.JSObject,
) !FetchInitResult {
    var options = FetchInitOptions{
        .max_response_bytes = @max(@as(usize, 1), rt.config.outbound_max_response_bytes),
    };

    const init = init_obj orelse return .{ .ok = options };

    if (getObjectProperty(rt.ctx, init, pool, zq.Atom.method, "method")) |method_val| {
        const method_name = getStringData(method_val) orelse {
            return fetchInitError(rt, allocator, &options, "InvalidMethod", "method must be string");
        };
        options.method = parseHttpMethod(method_name) orelse {
            return fetchInitError(rt, allocator, &options, "InvalidMethod", method_name);
        };
    }

    if (getObjectProperty(rt.ctx, init, pool, zq.Atom.body, "body")) |body_val| {
        if (body_val.isNull() or body_val.isUndefined()) {
            options.body = null;
        } else {
            options.body = getStringData(body_val) orelse {
                return fetchInitError(rt, allocator, &options, "InvalidBody", "body must be string|null");
            };
        }
    }

    if (getDynamicProperty(rt.ctx, init, pool, "maxResponseBytes")) |max_val| {
        if (!max_val.isInt()) {
            return fetchInitError(rt, allocator, &options, "InvalidMaxResponseBytes", "maxResponseBytes must be integer");
        }
        const req_max = std.math.cast(usize, max_val.getInt()) orelse {
            return fetchInitError(rt, allocator, &options, "InvalidMaxResponseBytes", "maxResponseBytes out of range");
        };
        options.max_response_bytes = @min(options.max_response_bytes, @max(@as(usize, 1), req_max));
    } else if (getDynamicProperty(rt.ctx, init, pool, "max_response_bytes")) |max_val| {
        if (!max_val.isInt()) {
            return fetchInitError(rt, allocator, &options, "InvalidMaxResponseBytes", "max_response_bytes must be integer");
        }
        const req_max = std.math.cast(usize, max_val.getInt()) orelse {
            return fetchInitError(rt, allocator, &options, "InvalidMaxResponseBytes", "max_response_bytes out of range");
        };
        options.max_response_bytes = @min(options.max_response_bytes, @max(@as(usize, 1), req_max));
    }

    if (getObjectProperty(rt.ctx, init, pool, zq.Atom.headers, "headers")) |headers_val| {
        if (!headers_val.isObject()) {
            return fetchInitError(rt, allocator, &options, "InvalidHeaders", "headers must be object<string,string>");
        }
        const headers_obj = headers_val.toPtr(zq.JSObject);
        const keys = try headers_obj.getOwnEnumerableKeys(allocator, pool);
        defer allocator.free(keys);

        for (keys) |key_atom| {
            const key_name = rt.ctx.atoms.getName(key_atom) orelse continue;
            const header_val = headers_obj.getOwnProperty(pool, key_atom) orelse continue;
            const header_str = getStringData(header_val) orelse {
                return fetchInitError(rt, allocator, &options, "InvalidHeaders", "header values must be strings");
            };
            if (key_name.len == 0) {
                return fetchInitError(rt, allocator, &options, "InvalidHeaders", "header name must be non-empty");
            }
            if (std.mem.indexOfAny(u8, key_name, "\r\n") != null) {
                return fetchInitError(rt, allocator, &options, "InvalidHeaders", "header name contains newline");
            }
            if (std.mem.indexOfAny(u8, header_str, "\r\n") != null) {
                return fetchInitError(rt, allocator, &options, "InvalidHeaders", "header value contains newline");
            }
            try options.headers.append(allocator, .{ .name = key_name, .value = header_str });
        }
    }

    return .{ .ok = options };
}

// Shared outbound-fetch plumbing for the three call sites below (the synchronous
// fetchSyncResult, the parallel worker doFetchWorkerInner, and the native HTTP
// bridge). All three pre-create the connection via connectTcpOptions to apply a
// connect timeout, so they share the same two std.http.Client quirks.

/// std.http.Client loads the CA bundle and stamps client.now lazily inside
/// request(); but because we pre-create the connection, Connection.Tls.create
/// would assert on a null client.now. Bootstrap that TLS state up front. After
/// this, request() sees client.now != null and skips its own rescan, and
/// client.deinit() frees the bundle. No-op for plain HTTP.
fn bootstrapClientTls(client: *std.http.Client, protocol: std.http.Client.Protocol) error{CertificateBundleLoadFailure}!void {
    if (protocol != .tls) return;
    const now = std.Io.Clock.real.now(client.io);
    client.ca_bundle.rescan(client.allocator, client.io, now) catch return error.CertificateBundleLoadFailure;
    client.now = now;
}

/// Read a response body, transparently decoding Content-Encoding and
/// Transfer-Encoding. The plain response.reader() returns raw chunked/compressed
/// bytes, and std.http.Client advertises gzip/deflate by default, so a compressed
/// or chunked body would otherwise reach the caller verbatim (and JSON.parse then
/// yields {}). readerDecompressing handles gzip/deflate/zstd; identity passes
/// through. Pair with `.accept_encoding = .omit` on the request to prefer identity.
fn readResponseBody(response: *std.http.Client.Response, allocator: std.mem.Allocator, max_bytes: usize) ![]u8 {
    var body_transfer: [4096]u8 = undefined;
    var decompress: std.http.Decompress = undefined;
    var decompress_buf: [std.compress.flate.max_window_len]u8 = undefined;
    var reader = response.readerDecompressing(&body_transfer, &decompress, &decompress_buf);
    // allocRemaining reports StreamTooLong once the limit is *reached*, not just
    // exceeded, so a body of exactly max_bytes would be wrongly rejected. Read
    // one extra byte and enforce the real cap here so exactly-max-bytes succeeds
    // while anything larger still surfaces StreamTooLong (-> 599 ResponseTooLarge).
    const body = try reader.allocRemaining(allocator, std.Io.Limit.limited(max_bytes +| 1));
    if (body.len > max_bytes) {
        allocator.free(body);
        return error.StreamTooLong;
    }
    return body;
}

/// Wall-clock deadline for one outbound exchange, enforced by a watchdog
/// thread that full-shutdowns the socket once the deadline passes. The std
/// paths cannot bound this themselves: ConnectTcpOptions.timeout is declared
/// but never read by std.http.Client, and SO_RCVTIMEO is unusable because the
/// Threaded backend treats a socket EAGAIN as a programmer bug (panics in
/// Debug). After shutdown, blocked reads surface EndOfStream and blocked
/// writes SocketUnconnected, which the call sites map to a clean fetch error.
/// `disarm` must run before the connection is released so the watchdog can
/// never shut down a recycled fd.
const FetchDeadline = struct {
    stream: std.Io.net.Stream,
    timeout_ms: u32,
    io: std.Io,
    event: std.Io.Event = .unset,
    fired: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    thread: ?std.Thread = null,

    fn arm(self: *FetchDeadline) void {
        if (self.timeout_ms == 0) return;
        self.thread = std.Thread.spawn(.{}, watch, .{self}) catch null;
    }

    fn watch(self: *FetchDeadline) void {
        const duration: std.Io.Clock.Duration = .{
            .raw = std.Io.Duration.fromMilliseconds(@intCast(self.timeout_ms)),
            .clock = .awake,
        };
        const deadline = std.Io.Clock.Timestamp.fromNow(self.io, duration);
        while (true) {
            if (self.event.waitTimeout(self.io, .{ .deadline = deadline })) |_| {
                return;
            } else |err| switch (err) {
                error.Canceled => return,
                // waitTimeout reports spurious wakeups as Timeout; trust
                // only the clock.
                error.Timeout => if (deadline.durationFromNow(self.io).raw.nanoseconds <= 0) break,
            }
        }
        if (self.event.isSet()) return;
        self.fired.store(true, .seq_cst);
        self.stream.shutdown(self.io, .both) catch {};
    }

    fn disarm(self: *FetchDeadline) void {
        const thread = self.thread orelse return;
        self.event.set(self.io);
        thread.join();
        self.thread = null;
    }

    fn expired(self: *const FetchDeadline) bool {
        return self.fired.load(.seq_cst);
    }

    fn failCode(self: *const FetchDeadline, fallback: []const u8) []const u8 {
        return if (self.expired()) "TimedOut" else fallback;
    }
};

fn fetchSyncResult(rt: *Runtime, args: []const zq.JSValue) !zq.JSValue {
    if (!rt.config.outbound_http_enabled) {
        return createFetchErrorResponse(rt, "OutboundHttpDisabled", "set runtime outbound_http_enabled=true");
    }

    if (zq.modules.io.parallel_collector) |collector| {
        return collectFetchForParallel(rt, collector, args);
    }

    const pool = rt.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    const parsed = try parseFetchArgs(rt, pool, args);
    const fetch_args = switch (parsed) {
        .ok => |fa| fa,
        .err => |err_val| return err_val,
    };
    defer rt.allocator.free(fetch_args.url);
    const uri = fetch_args.uri;
    const init_obj = fetch_args.init_obj;

    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = resolveHostSafe(uri, &host_buf) catch {
        return createFetchErrorResponse(rt, "InvalidUrl", "url host is required");
    };

    var options = switch (try parseFetchInitOptions(rt, rt.allocator, pool, init_obj)) {
        .ok => |parsed_options| parsed_options,
        .err => |err_val| return err_val,
    };
    defer options.deinit(rt.allocator);

    var client = std.http.Client{
        .allocator = rt.allocator,
        .io = rt.outbound_io_backend.?.io(),
    };
    defer client.deinit();

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse {
        return createFetchErrorResponse(rt, "InvalidUrl", "unsupported URI scheme");
    };
    bootstrapClientTls(&client, protocol) catch {
        return createFetchErrorResponse(rt, "ConnectFailed", "CertificateBundleLoadFailure");
    };
    const timeout_ms = effectiveOutboundTimeoutMs(rt);
    const timeout = outboundTimeout(timeout_ms);
    const connection = client.connectTcpOptions(.{
        .host = host,
        .port = uri.port orelse switch (protocol) {
            .plain => 80,
            .tls => 443,
        },
        .protocol = protocol,
        .timeout = timeout,
    }) catch |err| {
        return createFetchErrorResponse(rt, "ConnectFailed", @errorName(err));
    };

    var req = client.request(options.method, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        // Prefer an identity body (see readResponseBody); we decode anyway.
        .headers = .{ .accept_encoding = .omit },
        .extra_headers = options.headers.items,
    }) catch |err| {
        client.connection_pool.release(connection, client.io);
        return createFetchErrorResponse(rt, "RequestInitFailed", @errorName(err));
    };
    defer req.deinit();

    var deadline: FetchDeadline = .{
        .stream = connection.stream_reader.stream,
        .timeout_ms = timeout_ms,
        .io = client.io,
    };
    deadline.arm();
    // Declared after req's deinit defer so disarm joins the watchdog first.
    defer deadline.disarm();

    if (options.body) |payload| {
        req.transfer_encoding = .{ .content_length = payload.len };
        var request_body = req.sendBodyUnflushed(&.{}) catch |err| {
            return createFetchErrorResponse(rt, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
        request_body.writer.writeAll(payload) catch |err| {
            return createFetchErrorResponse(rt, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
        request_body.end() catch |err| {
            return createFetchErrorResponse(rt, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
        req.connection.?.flush() catch |err| {
            return createFetchErrorResponse(rt, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
    } else {
        req.sendBodiless() catch |err| {
            return createFetchErrorResponse(rt, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
    }

    var response = req.receiveHead(&.{}) catch |err| {
        return createFetchErrorResponse(rt, deadline.failCode("ResponseHeadFailed"), @errorName(err));
    };
    const status = @intFromEnum(response.head.status);
    var owned_head = try snapshotResponseHead(rt.allocator, response.head);
    defer owned_head.deinit(rt.allocator);

    const response_body = readResponseBody(&response, rt.allocator, options.max_response_bytes) catch |err| switch (err) {
        error.StreamTooLong => return createFetchErrorResponse(rt, "ResponseTooLarge", "response exceeded max_response_bytes"),
        else => return createFetchErrorResponse(rt, deadline.failCode("ResponseReadFailed"), @errorName(err)),
    };
    defer rt.allocator.free(response_body);

    // A connection-close body terminated by the watchdog's shutdown reads as
    // a clean EOF; reject it instead of returning a truncated response.
    if (deadline.expired()) {
        return createFetchErrorResponse(rt, "TimedOut", "exceeded outbound_timeout_ms");
    }

    const created = try createFetchResponse(rt, status, owned_head.reason, response_body, owned_head.contentType());
    for (owned_head.headers.items) |header| {
        const key_atom = try getHeaderAtom(rt.ctx, header.name);
        const value_str = try rt.ctx.createString(header.value);
        try rt.ctx.setPropertyChecked(created.headers, key_atom, value_str);
    }

    return created.value;
}

// ============================================================================
// Parallel I/O support
// ============================================================================

/// Intercept a fetchSync call during parallel collection mode.
/// Records the URL/method/body/headers as a FetchDescriptor instead of
/// performing the actual HTTP request.
fn collectFetchForParallel(rt: *Runtime, collector: *zq.modules.io.ParallelCollector, args: []const zq.JSValue) !zq.JSValue {
    if (collector.count >= collector.capacity) {
        return createFetchErrorResponse(rt, "ParallelOverflow", "too many fetchSync calls in parallel thunk");
    }

    const pool = rt.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const a = collector.allocator;

    const parsed = try parseFetchArgs(rt, pool, args);
    const fetch_args = switch (parsed) {
        .ok => |fa| fa,
        .err => |err_val| return err_val,
    };
    const url = fetch_args.url;
    defer rt.allocator.free(fetch_args.url);
    const init_obj = fetch_args.init_obj;

    var options = switch (try parseFetchInitOptions(rt, a, pool, init_obj)) {
        .ok => |parsed_options| parsed_options,
        .err => |err_val| return err_val,
    };
    errdefer options.deinit(a);

    const owned_url = try a.dupe(u8, url);
    errdefer a.free(owned_url);

    const owned_body = if (options.body) |b| try a.dupe(u8, b) else null;
    errdefer if (owned_body) |b| a.free(b);

    // Store descriptor
    const idx = collector.count;
    collector.descriptors[idx] = .{
        .url = owned_url,
        .method = options.method,
        .body = owned_body,
        .headers = options.headers,
        .max_response_bytes = options.max_response_bytes,
    };
    collector.count += 1;

    // Return a placeholder response (status 0, empty body)
    // parallel() replaces this with the real response after concurrent execution.
    return zq.JSValue.undefined_val;
}

/// Call a zero-arg JS function via the interpreter.
/// Used by IoCallbacks.call_thunk_fn and ScopeCallbacks.call0_fn.
pub fn ioCallThunk(runtime_ptr: *anyopaque, thunk_val: zq.JSValue) anyerror!zq.JSValue {
    return callJsThunk(runtime_ptr, thunk_val, &.{});
}

fn callJsThunk(runtime_ptr: *anyopaque, func_val: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    if (!func_val.isObject()) return error.NotCallable;
    const func_obj = func_val.toPtr(zq.JSObject);
    return rt.callFunction(func_obj, args);
}

pub fn scopeCall1(
    runtime_ptr: *anyopaque,
    thunk_val: zq.JSValue,
    arg: zq.JSValue,
) anyerror!zq.JSValue {
    const args = [_]zq.JSValue{arg};
    return callJsThunk(runtime_ptr, thunk_val, &args);
}

const ServiceRoute = struct {
    method_text: []const u8,
    path_pattern: []const u8,
};

/// `zigttp:fetch.fetch(url, init?)` callback. Without `init.durable`
/// this delegates straight to `fetchSync` so trace recording, replay
/// mode, and outbound-host policy enforcement all behave identically
/// across the two surfaces. With `init.durable = { key, retries?,
/// backoff?, ttl_s? }` the call is wrapped in an oplog step under
/// `<durable>/fetch/<hash>.step`: a cached response within TTL is
/// returned without hitting the network; a miss executes the request
/// (with retry on 5xx + connection errors per the backoff policy) and
/// persists the final response.
pub fn fetchModuleCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    args: []const zq.JSValue,
) anyerror!zq.JSValue {
    _ = ctx;
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    const pool = rt.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    if (rt.config.replay_file_path != null) {
        return fetchModuleReplay(rt);
    }

    const init_obj: ?*zq.JSObject = blk: {
        if (args.len >= 1 and args[0].isObject()) break :blk args[0].toPtr(zq.JSObject);
        if (args.len >= 2 and args[1].isObject()) break :blk args[1].toPtr(zq.JSObject);
        break :blk null;
    };

    if (init_obj) |obj| {
        switch (try parseDurableFetchOpts(rt, obj, pool)) {
            .none => {},
            .err => |err_val| return err_val,
            .ok => |opts| return runDurableFetch(rt, args, opts),
        }
    }
    return fetchSyncNative(@ptrCast(rt.ctx), zq.JSValue.undefined_val, args);
}

fn fetchModuleReplay(rt: *Runtime) !zq.JSValue {
    const state = rt.ctx.getModuleState(zq.trace.ReplayState, zq.trace.REPLAY_STATE_SLOT) orelse {
        return createFetchErrorResponse(rt, "ReplayNotConfigured", "fetch replay state is not installed");
    };
    const entry_opt = if (replayNextIs(state, "http", "fetchSync")) blk: {
        const inner = state.nextIO("http", "fetchSync");
        // Traced zigttp:fetch records fetchSync first, then the outer module
        // wrapper. Replay executes the module implementation directly, so it
        // must consume both rows to keep the cursor aligned.
        if (replayNextIs(state, "fetch", "fetch")) {
            _ = state.nextIO("fetch", "fetch");
        }
        break :blk inner;
    } else state.nextIO("fetch", "fetch");
    const entry = entry_opt orelse {
        return createFetchErrorResponse(rt, "ReplayMiss", "no recorded zigttp:fetch response");
    };

    const status_raw = zq.trace.findJsonIntValue(entry.result_json, "\"status\"") orelse 200;
    const status: u16 = @intCast(@max(100, @min(599, status_raw)));

    const status_text_raw = zq.trace.findJsonStringValue(entry.result_json, "\"statusText\"") orelse "OK";
    const status_text = try zq.trace.unescapeJson(rt.allocator, status_text_raw);
    defer rt.allocator.free(status_text);

    const body_raw = zq.trace.findJsonStringValue(entry.result_json, "\"body\"");
    const body = if (body_raw) |raw|
        try zq.trace.unescapeJson(rt.allocator, raw)
    else
        try rt.allocator.dupe(u8, entry.result_json);
    defer rt.allocator.free(body);

    const headers_json = zq.trace.findJsonObjectValue(entry.result_json, "\"headers\"");
    const content_type_raw = if (headers_json) |headers|
        zq.trace.findJsonStringValue(headers, "\"content-type\"") orelse
            zq.trace.findJsonStringValue(headers, "\"Content-Type\"")
    else
        zq.trace.findJsonStringValue(entry.result_json, "\"contentType\"");
    const content_type_owned: ?[]u8 = if (content_type_raw) |raw|
        try zq.trace.unescapeJson(rt.allocator, raw)
    else
        null;
    defer if (content_type_owned) |content_type| rt.allocator.free(content_type);

    const created = try createFetchResponse(rt, status, status_text, body, content_type_owned);
    if (headers_json) |headers| {
        try copyRecordedFetchHeaders(rt, created.headers, headers);
    }
    return created.value;
}

fn replayNextIs(state: *const zq.trace.ReplayState, module_name: []const u8, fn_name: []const u8) bool {
    const cursor: usize = @intCast(state.cursor);
    if (cursor >= state.io_calls.len) return false;
    const entry = state.io_calls[cursor];
    return std.mem.eql(u8, entry.module, module_name) and std.mem.eql(u8, entry.func, fn_name);
}

fn copyRecordedFetchHeaders(rt: *Runtime, headers_obj: *zq.JSObject, headers_json: []const u8) !void {
    var pos = skipJsonWhitespace(headers_json, 0);
    if (pos >= headers_json.len or headers_json[pos] != '{') return;
    pos += 1;

    while (pos < headers_json.len) {
        pos = skipJsonWhitespace(headers_json, pos);
        if (pos >= headers_json.len or headers_json[pos] == '}') return;
        if (headers_json[pos] != '"') return;

        const key_raw = jsonStringSlice(headers_json, pos) orelse return;
        pos = key_raw.end;
        pos = skipJsonWhitespace(headers_json, pos);
        if (pos >= headers_json.len or headers_json[pos] != ':') return;
        pos += 1;
        pos = skipJsonWhitespace(headers_json, pos);
        if (pos >= headers_json.len or headers_json[pos] != '"') return;

        const value_raw = jsonStringSlice(headers_json, pos) orelse return;
        pos = value_raw.end;

        const key = try zq.trace.unescapeJson(rt.allocator, key_raw.data);
        defer rt.allocator.free(key);
        const value = try zq.trace.unescapeJson(rt.allocator, value_raw.data);
        defer rt.allocator.free(value);

        const key_atom = try getHeaderAtom(rt.ctx, key);
        const value_str = try rt.ctx.createString(value);
        try rt.ctx.setPropertyChecked(headers_obj, key_atom, value_str);

        pos = skipJsonWhitespace(headers_json, pos);
        if (pos < headers_json.len and headers_json[pos] == ',') {
            pos += 1;
            continue;
        }
        if (pos < headers_json.len and headers_json[pos] == '}') return;
    }
}

const JsonStringSlice = struct {
    data: []const u8,
    end: usize,
};

fn jsonStringSlice(json: []const u8, start: usize) ?JsonStringSlice {
    if (start >= json.len or json[start] != '"') return null;
    var pos = start + 1;
    const data_start = pos;
    while (pos < json.len) : (pos += 1) {
        if (json[pos] == '\\') {
            pos += 1;
            continue;
        }
        if (json[pos] == '"') {
            return .{ .data = json[data_start..pos], .end = pos + 1 };
        }
    }
    return null;
}

fn skipJsonWhitespace(json: []const u8, start: usize) usize {
    var pos = start;
    while (pos < json.len and switch (json[pos]) {
        ' ', '\t', '\r', '\n' => true,
        else => false,
    }) : (pos += 1) {}
    return pos;
}

const DurableFetchOpts = durable_fetch.Options;

const ParsedDurableOpts = union(enum) {
    none,
    ok: DurableFetchOpts,
    err: zq.JSValue,
};

/// Extract `durable: { key, retries?, backoff?, ttl_s? }` from the
/// init object. Returns `.none` when `durable` is absent. Returns
/// `.err` with a JS error-response JSValue when the field is present
/// but malformed — authors who opt in to durable semantics deserve a
/// loud failure, not a silent fallback.
fn parseDurableFetchOpts(
    rt: *Runtime,
    init: *zq.JSObject,
    pool: *const zq.HiddenClassPool,
) !ParsedDurableOpts {
    const durable_val = getDynamicProperty(rt.ctx, init, pool, "durable") orelse return .none;
    if (durable_val.isNull() or durable_val.isUndefined()) return .none;
    if (!durable_val.isObject()) {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable must be an object") };
    }

    const obj = durable_val.toPtr(zq.JSObject);

    const key_val = getDynamicProperty(rt.ctx, obj, pool, "key") orelse {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.key is required") };
    };
    const key = getStringData(key_val) orelse {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.key must be a string") };
    };
    if (key.len == 0) {
        return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.key must be non-empty") };
    }

    var opts: DurableFetchOpts = .{ .key = key };

    if (getDynamicProperty(rt.ctx, obj, pool, "retries")) |retries_val| {
        if (!retries_val.isInt()) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.retries must be an integer") };
        }
        const raw = retries_val.getInt();
        if (raw < 0 or raw > 16) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.retries must be in [0, 16]") };
        }
        opts.retries = @intCast(raw);
    }

    if (getDynamicProperty(rt.ctx, obj, pool, "backoff")) |backoff_val| {
        const text = getStringData(backoff_val) orelse {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.backoff must be \"none\" or \"exponential\"") };
        };
        if (std.mem.eql(u8, text, "none")) {
            opts.backoff = .none;
        } else if (std.mem.eql(u8, text, "exponential")) {
            opts.backoff = .exponential;
        } else {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.backoff must be \"none\" or \"exponential\"") };
        }
    }

    if (getDynamicProperty(rt.ctx, obj, pool, "ttl_s")) |ttl_val| {
        if (!ttl_val.isInt()) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.ttl_s must be an integer") };
        }
        const raw = ttl_val.getInt();
        if (raw <= 0 or raw > 7 * 24 * 3600) {
            return .{ .err = try createFetchErrorResponse(rt, "InvalidDurable", "durable.ttl_s must be in (0, 604800]") };
        }
        opts.ttl_s = @intCast(raw);
    }

    return .{ .ok = opts };
}

fn runDurableFetch(
    rt: *Runtime,
    args: []const zq.JSValue,
    opts: DurableFetchOpts,
) !zq.JSValue {
    const durable_dir = rt.config.durable_oplog_dir orelse {
        return createFetchErrorResponse(rt, "DurableNotConfigured", "fetch durable requires --durable <dir>");
    };

    // Extract method+url+body for hashing before the network call. This
    // duplicates a sliver of parseFetchArgs' work but keeps the hash
    // stable even if parseFetchArgs later normalizes fields.
    const summary = try summarizeFetchRequest(rt, args);
    defer rt.allocator.free(summary.url); // owned effective url (see summarizeFetchRequest)
    const hash = durable_fetch.computeRequestHash(opts.key, summary.method, summary.url, summary.body);

    var store = durable_store_mod.DurableStore.initFs(rt.allocator, durable_dir);
    const fetch_dir = try store.subtreeDir(rt.allocator, "fetch");
    defer rt.allocator.free(fetch_dir);

    const now_ms = unixMillis();

    if (try durable_fetch.loadEntry(rt.allocator, fetch_dir, hash[0..], opts.ttl_s, now_ms)) |hit| {
        var entry = hit;
        defer entry.deinit(rt.allocator);
        return try rebuildResponseFromCache(rt, entry);
    }

    var attempt: u32 = 0;
    var last_response: zq.JSValue = zq.JSValue.undefined_val;
    while (true) : (attempt += 1) {
        last_response = try fetchSyncResult(rt, args);
        const status = extractResponseStatus(rt, last_response);
        const retryable = status >= 500;
        const exhausted = attempt >= opts.retries;
        if (!retryable or exhausted) break;
        if (opts.backoff == .exponential) {
            var seed = retry_backoff.seedBytes(hash[0..], 0x4645544348524554);
            seed = retry_backoff.mix(seed, attempt);
            seed = retry_backoff.mix(seed, @as(u64, @bitCast(unixMillis())));
            const delay_ms = retry_backoff.retryDelayMs(100, 6_400, attempt, seed);
            if (rt.outbound_io_backend) |*backend| {
                std.Io.sleep(backend.io(), .fromMilliseconds(delay_ms), .awake) catch {};
            }
        }
    }

    // Only cache terminal responses. 5xx (including our own 599 transport
    // errors) are transient by convention — caching them would freeze the
    // handler on a stale failure. A retry with the same idempotency key
    // should try again, not replay a bad response.
    const final_status = extractResponseStatus(rt, last_response);
    if (final_status < 500) {
        try persistResponseToCache(rt, fetch_dir, hash[0..], last_response, now_ms);
    }
    return last_response;
}

const RequestSummary = struct {
    method: []const u8,
    url: []const u8,
    body: []const u8,
};

fn summarizeFetchRequest(rt: *Runtime, args: []const zq.JSValue) !RequestSummary {
    const pool = rt.ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    var url: []const u8 = "";
    var method: []const u8 = "GET";
    var body: []const u8 = "";

    if (args.len >= 1) {
        if (args[0].isObject()) {
            const obj = args[0].toPtr(zq.JSObject);
            if (getObjectProperty(rt.ctx, obj, pool, zq.Atom.url, "url")) |v|
                if (getStringData(v)) |s| {
                    url = s;
                };
        } else if (getStringData(args[0])) |s| {
            url = s;
        }
    }

    const init_obj: ?*zq.JSObject = blk: {
        if (args.len >= 1 and args[0].isObject()) break :blk args[0].toPtr(zq.JSObject);
        if (args.len >= 2 and args[1].isObject()) break :blk args[1].toPtr(zq.JSObject);
        break :blk null;
    };
    if (init_obj) |obj| {
        if (getObjectProperty(rt.ctx, obj, pool, zq.Atom.method, "method")) |v|
            if (getStringData(v)) |s| {
                method = s;
            };
        if (getObjectProperty(rt.ctx, obj, pool, zq.Atom.body, "body")) |v|
            if (getStringData(v)) |s| {
                body = s;
            };
    }

    // Hash the EFFECTIVE url (literal base + appended dynamic `query`), not the
    // bare literal, so two durable fetches differing only in `query` don't
    // collide on one cache entry. buildFetchUrl is the same effective-URL
    // builder the live fetch uses; on error (e.g. egress violation) the real
    // fetch surfaces it, so here we fall back to the literal for a stable hash.
    // The returned url is owned; the caller frees `summary.url`.
    const effective_url: []const u8 = switch (try buildFetchUrl(rt, pool, url, init_obj)) {
        .ok => |u| u,
        .err => try rt.allocator.dupe(u8, url),
    };

    return .{ .method = method, .url = effective_url, .body = body };
}

pub fn extractResponseStatus(rt: *Runtime, response: zq.JSValue) u16 {
    if (!response.isObject()) return 0;
    const pool = rt.ctx.hidden_class_pool orelse return 0;
    const obj = response.toPtr(zq.JSObject);
    const status_val = obj.getProperty(pool, zq.Atom.status) orelse return 0;
    if (!status_val.isInt()) return 0;
    const raw = status_val.getInt();
    if (raw < 0 or raw > 999) return 0;
    return @intCast(raw);
}

fn rebuildResponseFromCache(rt: *Runtime, entry: durable_fetch.Entry) !zq.JSValue {
    const created = try createFetchResponse(
        rt,
        entry.status,
        entry.status_text,
        entry.body,
        entry.content_type,
    );
    return created.value;
}

fn persistResponseToCache(
    rt: *Runtime,
    fetch_dir: []const u8,
    hash_hex: []const u8,
    response: zq.JSValue,
    now_ms: i64,
) !void {
    if (!response.isObject()) return;
    const pool = rt.ctx.hidden_class_pool orelse return;
    const obj = response.toPtr(zq.JSObject);

    const status = extractResponseStatus(rt, response);
    if (status == 0) return;

    var status_text_buf: []const u8 = "";
    if (obj.getProperty(pool, try rt.ctx.atoms.intern("statusText"))) |v|
        if (getStringData(v)) |s| {
            status_text_buf = s;
        };

    var body_bytes: []const u8 = "";
    if (obj.getProperty(pool, zq.Atom.body)) |body_val| {
        if (body_val.isString()) body_bytes = body_val.toPtr(zq.JSString).data();
    }

    var content_type_opt: ?[]const u8 = null;
    if (obj.getProperty(pool, zq.Atom.headers)) |headers_val| {
        if (headers_val.isObject()) {
            const headers_obj = headers_val.toPtr(zq.JSObject);
            if (headers_obj.getProperty(pool, try getHeaderAtom(rt.ctx, "content-type"))) |ct_val| {
                if (getStringData(ct_val)) |s| content_type_opt = s;
            }
        }
    }

    const status_text_owned = try rt.allocator.dupe(u8, status_text_buf);
    defer rt.allocator.free(status_text_owned);
    const body_owned = try rt.allocator.dupe(u8, body_bytes);
    defer rt.allocator.free(body_owned);
    var ct_owned: ?[]u8 = null;
    defer if (ct_owned) |b| rt.allocator.free(b);
    if (content_type_opt) |s| ct_owned = try rt.allocator.dupe(u8, s);

    try durable_fetch.writeEntry(rt.allocator, fetch_dir, hash_hex, .{
        .at_ms = now_ms,
        .status = status,
        .status_text = status_text_owned,
        .content_type = ct_owned,
        .body = body_owned,
    });
}

// ===========================================================================
// Workflow runtime callbacks (zigttp:workflow)
// ===========================================================================

/// Owned request view parts parsed from a `{ method?, path?, body?, headers? }`
/// JS object. All strings are duped into the caller's arena, so the resulting
/// view does not borrow the orchestrator JS heap across the dispatch boundary.
const RequestParts = struct {
    method: []const u8 = "GET",
    url: []const u8 = "/",
    path: []const u8 = "/",
    query_params: []const QueryParam = &.{},
    body: ?[]const u8 = null,
    headers: std.ArrayListUnmanaged(HttpHeader) = .empty,

    pub fn view(self: RequestParts) HttpRequestView {
        return .{ .method = self.method, .url = self.url, .path = self.path, .query_params = self.query_params, .headers = self.headers, .body = self.body };
    }
};

/// Parse the request fields shared by `workflow.call` and `workflow.fanout`.
/// Returns error.InvalidWorkflowInit on a malformed field type (the caller maps
/// it to a fail-soft error Response). A non-object input yields defaults.
pub fn parseHttpRequestParts(ctx: *zq.Context, obj_val: zq.JSValue, arena: std.mem.Allocator) !RequestParts {
    var parts = RequestParts{};
    if (!obj_val.isObject()) return parts;
    const obj = obj_val.toPtr(zq.JSObject);
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    if (getDynamicProperty(ctx, obj, pool, "method")) |m| {
        if (!m.isUndefined() and !m.isNull()) {
            parts.method = try arena.dupe(u8, getStringDataCtx(m, ctx) orelse return error.InvalidWorkflowInit);
        }
    }
    if (getDynamicProperty(ctx, obj, pool, "path")) |p| {
        if (!p.isUndefined() and !p.isNull()) {
            parts.path = try arena.dupe(u8, getStringDataCtx(p, ctx) orelse return error.InvalidWorkflowInit);
            parts.url = parts.path;
        }
    }
    if (getDynamicProperty(ctx, obj, pool, "body")) |b| {
        if (!b.isUndefined() and !b.isNull()) {
            parts.body = try arena.dupe(u8, getStringDataCtx(b, ctx) orelse return error.InvalidWorkflowInit);
        }
    }
    if (getDynamicProperty(ctx, obj, pool, "headers")) |h| {
        if (!h.isUndefined() and !h.isNull()) {
            if (!h.isObject()) return error.InvalidWorkflowInit;
            const hobj = h.toPtr(zq.JSObject);
            const keys = try hobj.getOwnEnumerableKeys(arena, pool);
            for (keys) |key_atom| {
                const hv = hobj.getOwnProperty(pool, key_atom) orelse continue;
                const hv_str = getStringDataCtx(hv, ctx) orelse return error.InvalidWorkflowInit;
                const key_name = ctx.atoms.getName(key_atom) orelse continue;
                try parts.headers.append(arena, .{
                    .key = try arena.dupe(u8, key_name),
                    .value = try arena.dupe(u8, hv_str),
                });
            }
        }
    }
    return parts;
}

pub fn serviceCallCallback(
    runtime_ptr: *anyopaque,
    ctx: *zq.Context,
    base_url: []const u8,
    route_pattern: []const u8,
    init_val: zq.JSValue,
) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    const route = parseServiceRoutePattern(route_pattern) catch {
        return createFetchErrorResponse(rt, "InvalidServiceRoute", "route pattern must be 'METHOD /path'");
    };

    const init_obj = if (init_val.isObject()) init_val.toPtr(zq.JSObject) else null;
    const url = buildServiceUrl(rt, ctx, base_url, route.path_pattern, init_obj) catch |err| {
        return switch (err) {
            error.MissingServiceParam => createFetchErrorResponse(rt, "MissingServiceParam", "missing required path parameter"),
            error.InvalidServiceParam => createFetchErrorResponse(rt, "InvalidServiceParam", "service path params must be string, number, or boolean"),
            error.InvalidServiceQuery => createFetchErrorResponse(rt, "InvalidServiceQuery", "service query values must be string, number, or boolean"),
            else => createFetchErrorResponse(rt, "InternalError", @errorName(err)),
        };
    };
    defer rt.allocator.free(url);

    const fetch_init = try ctx.createObject(null);
    try ctx.setPropertyChecked(fetch_init, zq.Atom.method, try ctx.createString(route.method_text));

    if (init_obj) |obj| {
        const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

        if (getDynamicProperty(ctx, obj, pool, "body")) |body_val| {
            if (!body_val.isUndefined() and !body_val.isNull()) {
                const body_str = getStringData(body_val) orelse {
                    return createFetchErrorResponse(rt, "InvalidBody", "serviceCall body must be string|null");
                };
                try ctx.setPropertyChecked(fetch_init, zq.Atom.body, try ctx.createString(body_str));
            }
        }

        if (getDynamicProperty(ctx, obj, pool, "headers")) |headers_val| {
            if (!headers_val.isObject()) {
                return createFetchErrorResponse(rt, "InvalidHeaders", "serviceCall headers must be object<string,string>");
            }
            const headers_obj = try ctx.createObject(null);
            const source_headers = headers_val.toPtr(zq.JSObject);
            const keys = try source_headers.getOwnEnumerableKeys(rt.allocator, pool);
            defer rt.allocator.free(keys);

            for (keys) |key_atom| {
                const header_val = source_headers.getOwnProperty(pool, key_atom) orelse continue;
                const header_str = getStringData(header_val) orelse {
                    return createFetchErrorResponse(rt, "InvalidHeaders", "serviceCall header values must be strings");
                };
                try ctx.setPropertyChecked(headers_obj, key_atom, try ctx.createString(header_str));
            }

            try ctx.setPropertyChecked(fetch_init, zq.Atom.headers, headers_obj.toValue());
        }
    }

    const fetch_args = [_]zq.JSValue{
        try ctx.createString(url),
        fetch_init.toValue(),
    };
    return fetchSyncResult(rt, &fetch_args);
}

fn parseServiceRoutePattern(route_pattern: []const u8) !ServiceRoute {
    const space = std.mem.indexOfScalar(u8, route_pattern, ' ') orelse return error.InvalidServiceRoute;
    const method_text = std.mem.trim(u8, route_pattern[0..space], " ");
    const path_pattern = std.mem.trim(u8, route_pattern[space + 1 ..], " ");
    if (method_text.len == 0 or path_pattern.len == 0 or path_pattern[0] != '/') {
        return error.InvalidServiceRoute;
    }
    if (parseHttpMethod(method_text) == null) return error.InvalidServiceRoute;
    return .{
        .method_text = method_text,
        .path_pattern = path_pattern,
    };
}

pub fn buildServiceUrl(
    rt: *Runtime,
    ctx: *zq.Context,
    base_url: []const u8,
    path_pattern: []const u8,
    init_obj: ?*zq.JSObject,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(rt.allocator);

    try buf.ensureTotalCapacity(rt.allocator, base_url.len + path_pattern.len + 32);
    try buf.appendSlice(rt.allocator, base_url);
    if (path_pattern.len > 0) {
        const base_has_slash = buf.items.len > 0 and buf.items[buf.items.len - 1] == '/';
        const path_has_slash = path_pattern[0] == '/';
        if (base_has_slash and path_has_slash) {
            buf.items.len -= 1;
        } else if (!base_has_slash and !path_has_slash) {
            try buf.append(rt.allocator, '/');
        }
        try appendServicePath(rt, ctx, &buf, path_pattern, init_obj);
    }

    try appendServiceQuery(rt, ctx, &buf, init_obj);
    return try buf.toOwnedSlice(rt.allocator);
}

fn appendServicePath(
    rt: *Runtime,
    ctx: *zq.Context,
    buf: *std.ArrayList(u8),
    path_pattern: []const u8,
    init_obj: ?*zq.JSObject,
) !void {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const params_obj = blk: {
        const obj = init_obj orelse break :blk null;
        const params_val = getDynamicProperty(ctx, obj, pool, "params") orelse break :blk null;
        if (!params_val.isObject()) return error.InvalidServiceParam;
        break :blk params_val.toPtr(zq.JSObject);
    };

    var i: usize = 0;
    while (i < path_pattern.len) {
        if (path_pattern[i] == ':' and (i == 0 or path_pattern[i - 1] == '/')) {
            const name_start = i + 1;
            var name_end = name_start;
            while (name_end < path_pattern.len and path_pattern[name_end] != '/') : (name_end += 1) {}

            const params = params_obj orelse return error.MissingServiceParam;
            const val = getDynamicProperty(ctx, params, pool, path_pattern[name_start..name_end]) orelse {
                return error.MissingServiceParam;
            };
            if (!try appendEncodedJsValue(rt, buf, val)) return error.InvalidServiceParam;
            i = name_end;
            continue;
        }

        const literal_start = i;
        while (i < path_pattern.len and !(path_pattern[i] == ':' and (i == 0 or path_pattern[i - 1] == '/'))) : (i += 1) {}
        try buf.appendSlice(rt.allocator, path_pattern[literal_start..i]);
    }
}

fn appendServiceQuery(
    rt: *Runtime,
    ctx: *zq.Context,
    buf: *std.ArrayList(u8),
    init_obj: ?*zq.JSObject,
) !void {
    const obj = init_obj orelse return;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const query_val = getDynamicProperty(ctx, obj, pool, "query") orelse return;
    if (!query_val.isObject()) return error.InvalidServiceQuery;

    const query_obj = query_val.toPtr(zq.JSObject);
    const keys = try query_obj.getOwnEnumerableKeys(rt.allocator, pool);
    defer rt.allocator.free(keys);

    // Seed from the already-built URL/path so a base or route pattern that
    // already carries a query string ('?...') appends with '&', not a second
    // '?' (mirrors buildFetchUrl).
    var wrote_any = std.mem.indexOfScalar(u8, buf.items, '?') != null;
    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        const query_item = query_obj.getOwnProperty(pool, key_atom) orelse continue;
        if (query_item.isUndefined()) continue;

        try buf.append(rt.allocator, if (wrote_any) '&' else '?');
        try appendPercentEncoded(rt, buf, key_name);
        try buf.append(rt.allocator, '=');
        if (!try appendEncodedJsValue(rt, buf, query_item)) return error.InvalidServiceQuery;
        wrote_any = true;
    }
}

fn appendEncodedJsValue(rt: *Runtime, buf: *std.ArrayList(u8), val: zq.JSValue) !bool {
    if (getStringData(val)) |text| {
        try appendPercentEncoded(rt, buf, text);
        return true;
    }
    if (val.isInt()) {
        var num_buf: [32]u8 = undefined;
        const text = std.fmt.bufPrint(&num_buf, "{d}", .{val.getInt()}) catch return false;
        try appendPercentEncoded(rt, buf, text);
        return true;
    }
    if (val.isBool()) {
        try appendPercentEncoded(rt, buf, if (val.getBool()) "true" else "false");
        return true;
    }
    if (val.isFloat()) {
        var num_buf: [64]u8 = undefined;
        const text = std.fmt.bufPrint(&num_buf, "{d}", .{val.getFloat64()}) catch return false;
        try appendPercentEncoded(rt, buf, text);
        return true;
    }
    return false;
}

fn appendPercentEncoded(rt: *Runtime, buf: *std.ArrayList(u8), input: []const u8) !void {
    return appendPercentEncodedInto(rt.allocator, buf, input);
}

pub fn appendPercentEncodedInto(allocator: std.mem.Allocator, buf: anytype, input: []const u8) !void {
    for (input) |byte| {
        if (isUrlUnreserved(byte)) {
            try buf.append(allocator, byte);
            continue;
        }

        try buf.append(allocator, '%');
        const hex = std.fmt.bytesToHex([_]u8{byte}, .upper);
        try buf.appendSlice(allocator, &hex);
    }
}

fn isUrlUnreserved(byte: u8) bool {
    return (byte >= 'A' and byte <= 'Z') or
        (byte >= 'a' and byte <= 'z') or
        (byte >= '0' and byte <= '9') or
        byte == '-' or
        byte == '_' or
        byte == '.' or
        byte == '~';
}

/// IoCallbacks.execute_fetches_fn - execute HTTP fetches concurrently using threads.
pub fn ioExecuteFetches(
    runtime_ptr: *anyopaque,
    descriptors: []const zq.modules.io.FetchDescriptor,
    results: []zq.modules.io.FetchResult,
) void {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));
    const count = descriptors.len;
    if (count == 0) return;

    // For a single fetch, execute inline (no thread overhead)
    if (count == 1) {
        results[0] = doFetchWorker(rt.allocator, rt.config, &descriptors[0]);
        return;
    }

    // Spawn worker threads for concurrent execution
    var threads: [zq.modules.io.MAX_PARALLEL]?std.Thread = .{null} ** zq.modules.io.MAX_PARALLEL;

    for (0..count) |i| {
        threads[i] = std.Thread.spawn(.{}, doFetchThread, .{
            rt.allocator,
            rt.config,
            &descriptors[i],
            &results[i],
        }) catch null;
    }

    // If thread spawn failed for any slot, execute inline as fallback
    for (0..count) |i| {
        if (threads[i] == null) {
            results[i] = doFetchWorker(rt.allocator, rt.config, &descriptors[i]);
        }
    }

    // Join all threads
    for (0..count) |i| {
        if (threads[i]) |t| t.join();
    }
}

/// Thread entry point for concurrent HTTP fetch.
fn doFetchThread(
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    desc: *const zq.modules.io.FetchDescriptor,
    result: *zq.modules.io.FetchResult,
) void {
    result.* = doFetchWorker(allocator, config, desc);
}

/// Execute a single HTTP fetch. Safe to call from any thread.
/// Creates its own I/O backend and HTTP client.
fn doFetchWorker(
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    desc: *const zq.modules.io.FetchDescriptor,
) zq.modules.io.FetchResult {
    return doFetchWorkerInner(allocator, config, desc) catch |err| {
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = allocator.dupe(u8, "InternalError") catch null,
            .error_details = allocator.dupe(u8, @errorName(err)) catch null,
        };
    };
}

fn doFetchWorkerInner(
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    desc: *const zq.modules.io.FetchDescriptor,
) !zq.modules.io.FetchResult {
    const uri = try std.Uri.parse(desc.url);

    // Thread-local I/O backend
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();

    var client = std.http.Client{
        .allocator = allocator,
        .io = io_backend.io(),
    };
    defer client.deinit();

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse {
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, "InvalidUrl"),
            .error_details = try allocator.dupe(u8, "unsupported URI scheme"),
        };
    };

    bootstrapClientTls(&client, protocol) catch {
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, "ConnectFailed"),
            .error_details = try allocator.dupe(u8, "CertificateBundleLoadFailure"),
        };
    };

    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = try resolveHostSafe(uri, &host_buf);

    const timeout: std.Io.Timeout = if (config.outbound_timeout_ms == 0) .none else blk: {
        const duration = std.Io.Duration.fromMilliseconds(@intCast(config.outbound_timeout_ms));
        break :blk .{ .duration = .{ .raw = duration, .clock = .awake } };
    };

    const connection = client.connectTcpOptions(.{
        .host = host,
        .port = uri.port orelse switch (protocol) {
            .plain => 80,
            .tls => 443,
        },
        .protocol = protocol,
        .timeout = timeout,
    }) catch |err| {
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, "ConnectFailed"),
            .error_details = try allocator.dupe(u8, @errorName(err)),
        };
    };

    var req = client.request(desc.method, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        // Prefer an identity body (see readResponseBody); we decode anyway.
        .headers = .{ .accept_encoding = .omit },
        .extra_headers = desc.headers.items,
    }) catch |err| {
        client.connection_pool.release(connection, client.io);
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, "RequestInitFailed"),
            .error_details = try allocator.dupe(u8, @errorName(err)),
        };
    };
    defer req.deinit();

    var deadline: FetchDeadline = .{
        .stream = connection.stream_reader.stream,
        .timeout_ms = config.outbound_timeout_ms,
        .io = client.io,
    };
    deadline.arm();
    // Declared after req's deinit defer so disarm joins the watchdog first.
    defer deadline.disarm();

    if (desc.body) |payload| {
        req.transfer_encoding = .{ .content_length = payload.len };
        var request_body = req.sendBodyUnflushed(&.{}) catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, deadline.failCode("RequestSendFailed")),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
        request_body.writer.writeAll(payload) catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, deadline.failCode("RequestSendFailed")),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
        request_body.end() catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, deadline.failCode("RequestSendFailed")),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
        req.connection.?.flush() catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, deadline.failCode("RequestSendFailed")),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
    } else {
        req.sendBodiless() catch |err| {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, deadline.failCode("RequestSendFailed")),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        };
    }

    var response = req.receiveHead(&.{}) catch |err| {
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, deadline.failCode("ResponseHeadFailed")),
            .error_details = try allocator.dupe(u8, @errorName(err)),
        };
    };

    const status = @intFromEnum(response.head.status);
    var owned_head = try snapshotResponseHead(allocator, response.head);
    defer owned_head.deinit(allocator);

    const max_response_bytes = @max(@as(usize, 1), desc.max_response_bytes);
    const response_body = readResponseBody(&response, allocator, max_response_bytes) catch |err| switch (err) {
        error.StreamTooLong => {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, "ResponseTooLarge"),
                .error_details = try allocator.dupe(u8, "response exceeded max_response_bytes"),
            };
        },
        else => {
            return zq.modules.io.FetchResult{
                .status = 599,
                .ok = false,
                .error_code = try allocator.dupe(u8, deadline.failCode("ResponseReadFailed")),
                .error_details = try allocator.dupe(u8, @errorName(err)),
            };
        },
    };

    // A connection-close body terminated by the watchdog's shutdown reads as
    // a clean EOF; reject it instead of returning a truncated response.
    if (deadline.expired()) {
        allocator.free(response_body);
        return zq.modules.io.FetchResult{
            .status = 599,
            .ok = false,
            .error_code = try allocator.dupe(u8, "TimedOut"),
            .error_details = try allocator.dupe(u8, "exceeded outbound_timeout_ms"),
        };
    }

    // Build response headers list
    var resp_headers: std.ArrayList(zq.modules.io.FetchResult.ResponseHeader) = .empty;
    errdefer {
        for (resp_headers.items) |h| {
            allocator.free(h.name);
            allocator.free(h.value_str);
        }
        resp_headers.deinit(allocator);
    }
    for (owned_head.headers.items) |h| {
        const name = try allocator.dupe(u8, h.name);
        errdefer allocator.free(name);
        const value = try allocator.dupe(u8, h.value);
        errdefer allocator.free(value);
        try resp_headers.append(allocator, .{ .name = name, .value_str = value });
    }

    return zq.modules.io.FetchResult{
        .status = status,
        .body = response_body,
        .content_type = if (owned_head.contentType()) |ct| try allocator.dupe(u8, ct) else null,
        .reason = try allocator.dupe(u8, owned_head.reason),
        .response_headers = resp_headers,
        .ok = true,
    };
}

/// IoCallbacks.build_response_fn - create a JS Response object from a FetchResult.
pub fn ioBuildResponse(runtime_ptr: *anyopaque, result: *const zq.modules.io.FetchResult) anyerror!zq.JSValue {
    const rt: *Runtime = @ptrCast(@alignCast(runtime_ptr));

    if (!result.ok) {
        // Build error response
        const err_code = result.error_code orelse "InternalError";
        const err_details = result.error_details orelse "unknown error";
        return createFetchErrorResponse(rt, err_code, err_details);
    }

    const body_str = result.body orelse "";
    const created = try createFetchResponse(
        rt,
        result.status,
        result.reason orelse "",
        body_str,
        result.content_type,
    );

    // Copy response headers
    for (result.response_headers.items) |h| {
        const key_atom = try getHeaderAtom(rt.ctx, h.name);
        const value_str = try rt.ctx.createString(h.value_str);
        try rt.ctx.setPropertyChecked(created.headers, key_atom, value_str);
    }

    return created.value;
}

pub fn httpRequestNative(_: *anyopaque, _: zq.JSValue, args: []const zq.JSValue) anyerror!zq.JSValue {
    const rt = zruntime.current_runtime orelse return error.RuntimeUnavailable;
    const out = httpRequestResultJsonAlloc(rt, args) catch |err| {
        const fallback = try httpRequestErrorJsonAlloc(rt.allocator, "InternalError", @errorName(err));
        defer rt.allocator.free(fallback);
        return rt.createString(fallback);
    };
    defer rt.allocator.free(out);
    return rt.createString(out);
}

fn httpRequestResultJsonAlloc(rt: *Runtime, args: []const zq.JSValue) ![]u8 {
    const a = rt.allocator;
    if (!rt.config.outbound_http_enabled) {
        return try httpRequestErrorJsonAlloc(a, "OutboundHttpDisabled", "set runtime outbound_http_enabled=true");
    }
    if (args.len == 0 or !args[0].isString()) {
        return try httpRequestErrorJsonAlloc(a, "InvalidArgs", "expected one JSON string argument");
    }

    const input_json = args[0].toPtr(zq.JSString).data();
    var parsed = std.json.parseFromSlice(std.json.Value, a, input_json, .{}) catch {
        return try httpRequestErrorJsonAlloc(a, "InvalidJson", "failed to parse request JSON");
    };
    defer parsed.deinit();
    if (parsed.value != .object) {
        return try httpRequestErrorJsonAlloc(a, "InvalidJson", "request JSON must be an object");
    }
    const obj = parsed.value.object;

    const url_v = obj.get("url") orelse {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "missing url field");
    };
    if (url_v != .string or url_v.string.len == 0) {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "url must be a non-empty string");
    }
    const uri = std.Uri.parse(url_v.string) catch {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "url parse failed");
    };

    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = resolveHostSafe(uri, &host_buf) catch {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "url host is required");
    };
    if (outboundHostViolation(rt, host.bytes)) |details| {
        return try httpRequestErrorJsonAlloc(a, "HostNotAllowed", details);
    }

    const method = blk: {
        if (obj.get("method")) |method_v| {
            if (method_v != .string) {
                return try httpRequestErrorJsonAlloc(a, "InvalidMethod", "method must be string");
            }
            break :blk parseHttpMethod(method_v.string) orelse {
                return try httpRequestErrorJsonAlloc(a, "InvalidMethod", method_v.string);
            };
        }
        break :blk std.http.Method.GET;
    };

    const body: ?[]const u8 = if (obj.get("body")) |body_v| switch (body_v) {
        .null => null,
        .string => |s| s,
        else => return try httpRequestErrorJsonAlloc(a, "InvalidBody", "body must be string|null"),
    } else null;

    const max_response_bytes = blk: {
        const cfg_max = @max(@as(usize, 1), rt.config.outbound_max_response_bytes);
        if (obj.get("max_response_bytes")) |max_v| switch (max_v) {
            .integer => |i| {
                const req_max = std.math.cast(usize, i) orelse {
                    return try httpRequestErrorJsonAlloc(a, "InvalidMaxResponseBytes", "max_response_bytes out of range");
                };
                break :blk @min(cfg_max, @max(@as(usize, 1), req_max));
            },
            else => return try httpRequestErrorJsonAlloc(a, "InvalidMaxResponseBytes", "max_response_bytes must be integer"),
        };
        break :blk cfg_max;
    };

    var owned_header_slices = std.array_list.Managed([]u8).init(a);
    defer {
        for (owned_header_slices.items) |s| a.free(s);
        owned_header_slices.deinit();
    }
    var headers = std.array_list.Managed(std.http.Header).init(a);
    defer headers.deinit();

    if (obj.get("headers")) |headers_v| {
        if (headers_v != .object) {
            return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "headers must be object<string,string>");
        }
        var it = headers_v.object.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* != .string) {
                return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "header values must be strings");
            }
            const name = try a.dupe(u8, entry.key_ptr.*);
            errdefer a.free(name);
            const value = try a.dupe(u8, entry.value_ptr.string);
            errdefer a.free(value);
            if (name.len == 0) return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "header name must be non-empty");
            if (std.mem.indexOfAny(u8, name, "\r\n") != null) return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "header name contains newline");
            if (std.mem.indexOfAny(u8, value, "\r\n") != null) return try httpRequestErrorJsonAlloc(a, "InvalidHeaders", "header value contains newline");

            try owned_header_slices.append(name);
            try owned_header_slices.append(value);
            try headers.append(.{ .name = name, .value = value });
        }
    }

    var client = std.http.Client{
        .allocator = a,
        .io = rt.outbound_io_backend.?.io(),
    };
    defer client.deinit();

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse {
        return try httpRequestErrorJsonAlloc(a, "InvalidUrl", "unsupported URI scheme");
    };
    bootstrapClientTls(&client, protocol) catch {
        return try httpRequestErrorJsonAlloc(a, "ConnectFailed", "CertificateBundleLoadFailure");
    };
    const timeout_ms = effectiveOutboundTimeoutMs(rt);
    const timeout = outboundTimeout(timeout_ms);
    const connection = client.connectTcpOptions(.{
        .host = host,
        .port = uri.port orelse switch (protocol) {
            .plain => 80,
            .tls => 443,
        },
        .protocol = protocol,
        .timeout = timeout,
    }) catch |err| {
        return try httpRequestErrorJsonAlloc(a, "ConnectFailed", @errorName(err));
    };

    var req = client.request(method, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        // Prefer an identity body (see readResponseBody); we decode anyway.
        .headers = .{ .accept_encoding = .omit },
        .extra_headers = headers.items,
    }) catch |err| {
        client.connection_pool.release(connection, client.io);
        return try httpRequestErrorJsonAlloc(a, "RequestInitFailed", @errorName(err));
    };
    defer req.deinit();

    var deadline: FetchDeadline = .{
        .stream = connection.stream_reader.stream,
        .timeout_ms = timeout_ms,
        .io = client.io,
    };
    deadline.arm();
    // Declared after req's deinit defer so disarm joins the watchdog first.
    defer deadline.disarm();

    if (body) |payload| {
        req.transfer_encoding = .{ .content_length = payload.len };
        var request_body = req.sendBodyUnflushed(&.{}) catch |err| {
            return try httpRequestErrorJsonAlloc(a, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
        request_body.writer.writeAll(payload) catch |err| {
            return try httpRequestErrorJsonAlloc(a, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
        request_body.end() catch |err| {
            return try httpRequestErrorJsonAlloc(a, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
        req.connection.?.flush() catch |err| {
            return try httpRequestErrorJsonAlloc(a, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
    } else {
        req.sendBodiless() catch |err| {
            return try httpRequestErrorJsonAlloc(a, deadline.failCode("RequestSendFailed"), @errorName(err));
        };
    }

    var response = req.receiveHead(&.{}) catch |err| {
        return try httpRequestErrorJsonAlloc(a, deadline.failCode("ResponseHeadFailed"), @errorName(err));
    };
    const status = @intFromEnum(response.head.status);
    const ok = response.head.status.class() != .server_error and response.head.status.class() != .client_error;
    var owned_head = try snapshotResponseHead(a, response.head);
    defer owned_head.deinit(a);

    const response_body = readResponseBody(&response, a, max_response_bytes) catch |err| switch (err) {
        error.StreamTooLong => return try httpRequestErrorJsonAlloc(a, "ResponseTooLarge", "response exceeded max_response_bytes"),
        else => return try httpRequestErrorJsonAlloc(a, deadline.failCode("ResponseReadFailed"), @errorName(err)),
    };
    defer a.free(response_body);

    // A connection-close body terminated by the watchdog's shutdown reads as
    // a clean EOF; reject it instead of returning a truncated response.
    if (deadline.expired()) {
        return try httpRequestErrorJsonAlloc(a, "TimedOut", "exceeded outbound_timeout_ms");
    }

    var aw: std.Io.Writer.Allocating = .init(a);
    defer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try stream.beginObject();
    try stream.objectField("ok");
    try stream.write(ok);
    try stream.objectField("status");
    try stream.write(status);
    try stream.objectField("reason");
    try stream.write(owned_head.reason);
    if (owned_head.contentType()) |ct| {
        try stream.objectField("content_type");
        try stream.write(ct);
    }
    try stream.objectField("body");
    try stream.write(response_body);
    try stream.endObject();
    return try aw.toOwnedSlice();
}

fn parseHttpMethod(raw: []const u8) ?std.http.Method {
    if (ascii.eqlIgnoreCase(raw, "GET")) return .GET;
    if (ascii.eqlIgnoreCase(raw, "POST")) return .POST;
    if (ascii.eqlIgnoreCase(raw, "PUT")) return .PUT;
    if (ascii.eqlIgnoreCase(raw, "PATCH")) return .PATCH;
    if (ascii.eqlIgnoreCase(raw, "DELETE")) return .DELETE;
    if (ascii.eqlIgnoreCase(raw, "HEAD")) return .HEAD;
    if (ascii.eqlIgnoreCase(raw, "OPTIONS")) return .OPTIONS;
    return null;
}

pub fn httpRequestErrorJsonAlloc(a: std.mem.Allocator, err_code: []const u8, details: []const u8) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(a);
    defer aw.deinit();
    var stream: std.json.Stringify = .{ .writer = &aw.writer };
    try stream.beginObject();
    try stream.objectField("ok");
    try stream.write(false);
    try stream.objectField("error");
    try stream.write(err_code);
    try stream.objectField("details");
    try stream.write(details);
    try stream.endObject();
    return try aw.toOwnedSlice();
}
