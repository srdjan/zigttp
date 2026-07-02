//! Pure native helpers: string-data readout, header-atom routing,
//! prototype walking, status normalisation, query object construction.
//! Threadlocal-aware natives stay in zruntime.zig.

const std = @import("std");
const zq = @import("zigts");
const ascii = std.ascii;

const http_types = @import("http_types.zig");
const QueryParam = http_types.QueryParam;
const http_parser = @import("http_parser.zig");

const zruntime = @import("zruntime.zig");
const Runtime = zruntime.Runtime;

pub fn getStringData(val: zq.JSValue) ?[]const u8 {
    if (val.isString()) {
        return val.toPtr(zq.JSString).data();
    }
    if (val.isStringSlice()) {
        return val.toPtr(zq.string.SliceString).data();
    }
    if (val.isRope()) {
        const rope = val.toPtr(zq.string.RopeNode);
        if (rope.kind == .leaf) {
            return rope.payload.leaf.data();
        }
    }
    return null;
}

/// Flattening string accessor: like getStringData but materializes a concat
/// rope (caching it as a leaf). Use at sinks that must accept any JS string,
/// since a `"a" + b` concatenation of combined length >= 64 is a concat rope
/// that getStringData reports as null.
const getStringDataCtx = zq.builtins.helpers.getStringDataCtx;

pub fn getDynamicProperty(ctx: *zq.Context, obj: *zq.JSObject, pool: *const zq.HiddenClassPool, name: []const u8) ?zq.JSValue {
    const atom = ctx.atoms.intern(name) catch return null;
    return obj.getProperty(pool, atom);
}

pub fn getObjectProperty(ctx: *zq.Context, obj: *zq.JSObject, pool: *const zq.HiddenClassPool, atom: zq.Atom, name: []const u8) ?zq.JSValue {
    if (obj.getProperty(pool, atom)) |value| return value;
    if (getDynamicProperty(ctx, obj, pool, name)) |value| return value;

    const keys = obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return null;
    defer ctx.allocator.free(keys);
    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        if (!std.mem.eql(u8, key_name, name)) continue;
        return obj.getOwnProperty(pool, key_atom);
    }
    return null;
}

pub fn getHeaderAtom(ctx: *zq.Context, name: []const u8) !zq.Atom {
    if (ascii.eqlIgnoreCase(name, "content-type")) {
        return try ctx.atoms.intern("Content-Type");
    }
    return Runtime.headerKeyToAtom(name) orelse try ctx.atoms.intern(name);
}

pub const HeaderAssignMode = enum {
    replace,
    append,
};

pub fn statusTextFor(status: u16) []const u8 {
    return switch (status) {
        200 => "OK",
        201 => "Created",
        204 => "No Content",
        301 => "Moved Permanently",
        302 => "Found",
        304 => "Not Modified",
        400 => "Bad Request",
        401 => "Unauthorized",
        403 => "Forbidden",
        404 => "Not Found",
        405 => "Method Not Allowed",
        408 => "Request Timeout",
        413 => "Payload Too Large",
        414 => "URI Too Long",
        422 => "Unprocessable Entity",
        429 => "Too Many Requests",
        431 => "Request Header Fields Too Large",
        500 => "Internal Server Error",
        501 => "Not Implemented",
        502 => "Bad Gateway",
        503 => "Service Unavailable",
        504 => "Gateway Timeout",
        599 => "Network Connect Timeout Error",
        else => "Unknown",
    };
}

pub fn normalizeStatus(status_val: zq.JSValue, default_status: u16) u16 {
    if (!status_val.isInt()) return default_status;
    const raw = status_val.getInt();
    if (raw < 100 or raw > 599) return 500;
    return @intCast(raw);
}

pub fn getGlobalPrototype(ctx: *zq.Context, ctor_atom: zq.Atom) ?*zq.JSObject {
    const ctor_val = ctx.getGlobal(ctor_atom) orelse return null;
    if (!ctor_val.isObject()) return null;
    const pool = ctx.hidden_class_pool orelse return null;
    const ctor_obj = ctor_val.toPtr(zq.JSObject);
    const proto_val = ctor_obj.getProperty(pool, zq.Atom.prototype) orelse return null;
    if (!proto_val.isObject()) return null;
    return proto_val.toPtr(zq.JSObject);
}

pub fn getNamedGlobalPrototype(ctx: *zq.Context, name: []const u8) ?*zq.JSObject {
    const atom = ctx.atoms.intern(name) catch return null;
    return getGlobalPrototype(ctx, atom);
}

pub fn throwTypeError(ctx: *zq.Context, message: []const u8) zq.JSValue {
    const message_val = ctx.createString(message) catch {
        ctx.throwException(zq.JSValue.exception_val);
        return zq.JSValue.exception_val;
    };
    const args = [_]zq.JSValue{message_val};
    const err_obj = zq.builtins.typeErrorConstructor(ctx, zq.JSValue.undefined_val, &args);
    ctx.throwException(err_obj);
    return zq.JSValue.exception_val;
}

pub fn findHeaderPropertyAtom(
    ctx: *zq.Context,
    headers_obj: *zq.JSObject,
    pool: *const zq.HiddenClassPool,
    wanted: []const u8,
) ?zq.Atom {
    if (Runtime.headerKeyToAtom(wanted)) |known_atom| {
        if (headers_obj.getOwnProperty(pool, known_atom) != null) return known_atom;
    }
    if (ascii.eqlIgnoreCase(wanted, "content-type")) {
        const ct_atom = ctx.atoms.intern("Content-Type") catch return null;
        if (headers_obj.getOwnProperty(pool, ct_atom) != null) return ct_atom;
    }
    const keys = headers_obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return null;
    defer ctx.allocator.free(keys);
    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        if (ascii.eqlIgnoreCase(key_name, wanted)) return key_atom;
    }
    return null;
}

pub fn getHeaderValueCaseInsensitive(
    ctx: *zq.Context,
    headers_obj: *zq.JSObject,
    pool: *const zq.HiddenClassPool,
    wanted: []const u8,
) ?zq.JSValue {
    const atom = findHeaderPropertyAtom(ctx, headers_obj, pool, wanted) orelse return null;
    const value = headers_obj.getOwnProperty(pool, atom) orelse return null;
    if (value.isUndefined() or value.isNull()) return null;
    return value;
}

pub fn validateHeaderPair(name: []const u8, value: []const u8) bool {
    if (name.len == 0) return false;
    if (std.mem.indexOfAny(u8, name, "\r\n") != null) return false;
    if (std.mem.indexOfAny(u8, value, "\r\n") != null) return false;
    return true;
}

pub fn setHeaderValue(
    ctx: *zq.Context,
    headers_obj: *zq.JSObject,
    name: []const u8,
    value: []const u8,
    mode: HeaderAssignMode,
) !void {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const target_atom = findHeaderPropertyAtom(ctx, headers_obj, pool, name) orelse try getHeaderAtom(ctx, name);

    const final_value = switch (mode) {
        .replace => try ctx.createString(value),
        .append => blk: {
            if (getHeaderValueCaseInsensitive(ctx, headers_obj, pool, name)) |existing_val| {
                if (getStringDataCtx(existing_val, ctx)) |existing_str| {
                    const combined = try std.fmt.allocPrint(ctx.allocator, "{s}, {s}", .{ existing_str, value });
                    defer ctx.allocator.free(combined);
                    break :blk try ctx.createString(combined);
                }
            }
            break :blk try ctx.createString(value);
        },
    };
    try ctx.setPropertyChecked(headers_obj, target_atom, final_value);
}

pub fn createHeadersObjectDynamic(ctx: *zq.Context) !*zq.JSObject {
    return ctx.createObject(getNamedGlobalPrototype(ctx, "Headers"));
}

pub fn copyHeadersIntoObject(ctx: *zq.Context, src_val: zq.JSValue, dst_obj: *zq.JSObject) !void {
    if (!src_val.isObject()) return error.InvalidHeaders;

    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const src_obj = src_val.toPtr(zq.JSObject);
    const keys = try src_obj.getOwnEnumerableKeys(ctx.allocator, pool);
    defer ctx.allocator.free(keys);

    for (keys) |key_atom| {
        const key_name = ctx.atoms.getName(key_atom) orelse continue;
        const value = src_obj.getOwnProperty(pool, key_atom) orelse continue;
        if (value.isUndefined() or value.isNull()) continue;
        const header_str = getStringDataCtx(value, ctx) orelse return error.InvalidHeaders;
        if (!validateHeaderPair(key_name, header_str)) return error.InvalidHeaders;
        try setHeaderValue(ctx, dst_obj, key_name, header_str, .replace);
    }
}

pub fn splitPathAndQuery(url: []const u8) struct {
    path: []const u8,
    query_string: []const u8,
} {
    if (std.mem.indexOfScalar(u8, url, '?')) |idx| {
        return .{
            .path = url[0..idx],
            .query_string = url[idx + 1 ..],
        };
    }
    return .{
        .path = url,
        .query_string = "",
    };
}

/// Parsed request target for a recorded or synthetic request: req.path split
/// from the query string, plus the parsed req.query params and their backing
/// storage. Every recorded-request consumer (test runner, replay runner,
/// durable recovery, intent assertions) builds an HttpRequestView from a raw
/// URL; without this split, zruntime falls back to the full url for req.path
/// (keeping the "?query" suffix) and req.query is empty - diverging from the
/// live server and causing false replay regressions on unchanged handlers.
pub const RequestTarget = struct {
    path: []const u8,
    params: []const QueryParam,
    storage: ?[]QueryParam = null,
    decoded_storage: ?[]u8 = null,

    pub fn deinit(self: RequestTarget, allocator: std.mem.Allocator) void {
        if (self.storage) |s| allocator.free(s);
        if (self.decoded_storage) |s| allocator.free(s);
    }
};

/// Split `url` into req.path and parsed req.query params exactly as the live
/// server does (server.zig). A query-parse failure degrades to an empty param
/// set rather than aborting the recorded run.
pub fn parseRequestTarget(allocator: std.mem.Allocator, url: []const u8) RequestTarget {
    const split = splitPathAndQuery(url);
    const qr = http_parser.parseQueryString(allocator, split.query_string, http_parser.DEFAULT_MAX_QUERY_LENGTH) catch
        http_parser.QueryParseResult{ .storage = null, .params = &.{}, .decoded_storage = null };
    return .{
        .path = split.path,
        .params = qr.params,
        .storage = qr.storage,
        .decoded_storage = qr.decoded_storage,
    };
}

pub fn buildQueryObject(ctx: *zq.Context, query_params: []const QueryParam) !*zq.JSObject {
    const query_obj = try ctx.createObject(null);
    for (query_params) |param| {
        const key_atom = try ctx.atoms.intern(param.key);
        const param_val = if (Runtime.parseQueryInt(param.value)) |int_val|
            zq.JSValue.fromInt(int_val)
        else
            try ctx.createString(param.value);
        try ctx.setPropertyChecked(query_obj, key_atom, param_val);
    }
    return query_obj;
}

pub fn upgradeResponseValue(ctx: *zq.Context, value: zq.JSValue) !zq.JSValue {
    if (!value.isObject()) return value;

    const response_obj = value.toPtr(zq.JSObject);
    response_obj.prototype = getGlobalPrototype(ctx, .Response);

    const pool = ctx.hidden_class_pool orelse return value;
    if (response_obj.getProperty(pool, zq.Atom.headers)) |headers_val| {
        if (headers_val.isObject()) {
            headers_val.toPtr(zq.JSObject).prototype = getNamedGlobalPrototype(ctx, "Headers");
        }
    }
    return value;
}

test "parseRequestTarget splits path and parses query params" {
    const allocator = std.testing.allocator;
    const t = parseRequestTarget(allocator, "/forecast?lat=52&lon=13");
    defer t.deinit(allocator);
    try std.testing.expectEqualStrings("/forecast", t.path);
    try std.testing.expectEqual(@as(usize, 2), t.params.len);
}

test "parseRequestTarget with no query yields the full path and empty params" {
    const allocator = std.testing.allocator;
    const t = parseRequestTarget(allocator, "/health");
    defer t.deinit(allocator);
    try std.testing.expectEqualStrings("/health", t.path);
    try std.testing.expectEqual(@as(usize, 0), t.params.len);
}
