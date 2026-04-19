//! zigttp:url - URL parsing and encoding

const std = @import("std");
const sdk = @import("zigttp-sdk");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:url",
    .name = "url",
    .exports = &.{
        .{ .name = "urlParse", .module_func = urlParseImpl, .arg_count = 1, .returns = .object, .param_types = &.{.string}, .effect = .none, .return_labels = .{ .user_input = true }, .laws = &.{.pure} },
        .{ .name = "urlSearchParams", .module_func = urlSearchParamsImpl, .arg_count = 1, .returns = .object, .param_types = &.{.string}, .effect = .none, .return_labels = .{ .user_input = true }, .laws = &.{.pure} },
        .{ .name = "urlEncode", .module_func = urlEncodeImpl, .arg_count = 1, .returns = .string, .param_types = &.{.string}, .laws = &.{.pure} },
        .{ .name = "urlDecode", .module_func = urlDecodeImpl, .arg_count = 1, .returns = .string, .param_types = &.{.string}, .laws = &.{.pure} },
    },
};

const UrlComponents = struct {
    protocol: ?[]const u8 = null,
    host: ?[]const u8 = null,
    port: ?[]const u8 = null,
    path: []const u8 = "/",
    query: ?[]const u8 = null,
    hash: ?[]const u8 = null,
};

fn parseUrlComponents(input: []const u8) UrlComponents {
    var result = UrlComponents{};
    var rest = input;

    if (std.mem.indexOfScalar(u8, rest, '#')) |hash_pos| {
        result.hash = rest[hash_pos + 1 ..];
        rest = rest[0..hash_pos];
    }

    if (std.mem.indexOfScalar(u8, rest, '?')) |query_pos| {
        result.query = rest[query_pos + 1 ..];
        rest = rest[0..query_pos];
    }

    if (std.mem.indexOf(u8, rest, "://")) |scheme_end| {
        result.protocol = rest[0..scheme_end];
        rest = rest[scheme_end + 3 ..];

        const host_end = std.mem.indexOfScalar(u8, rest, '/') orelse rest.len;
        const authority = rest[0..host_end];

        if (std.mem.lastIndexOfScalar(u8, authority, ':')) |colon| {
            result.host = authority[0..colon];
            result.port = authority[colon + 1 ..];
        } else {
            result.host = authority;
        }

        rest = if (host_end < rest.len) rest[host_end..] else "/";
    }

    result.path = if (rest.len > 0) rest else "/";
    return result;
}

fn percentDecode(buf: []u8, input: []const u8) []u8 {
    var out: usize = 0;
    var i: usize = 0;
    while (i < input.len) {
        if (input[i] == '%' and i + 2 < input.len) {
            if (std.fmt.parseInt(u8, input[i + 1 ..][0..2], 16)) |byte| {
                buf[out] = byte;
                out += 1;
                i += 3;
                continue;
            } else |_| {}
        }
        buf[out] = if (input[i] == '+') ' ' else input[i];
        out += 1;
        i += 1;
    }
    return buf[0..out];
}

fn buildSearchParamsObject(handle: *sdk.ModuleHandle, query: []const u8) !sdk.JSValue {
    const allocator = sdk.getAllocator(handle);
    const obj = try sdk.createObject(handle);

    const scratch = allocator.alloc(u8, query.len) catch return obj;
    defer allocator.free(scratch);

    var pairs = std.mem.splitScalar(u8, query, '&');
    while (pairs.next()) |pair| {
        if (pair.len == 0) continue;

        const eq_pos = std.mem.indexOfScalar(u8, pair, '=');
        const raw_key = if (eq_pos) |pos| pair[0..pos] else pair;
        const raw_val = if (eq_pos) |pos| pair[pos + 1 ..] else "";

        const key = percentDecode(scratch, raw_key);
        if (key.len == 0) continue;

        const val_buf = scratch[key.len..];
        const val = percentDecode(val_buf, raw_val);
        const val_str = sdk.createString(handle, val) catch continue;

        sdk.objectSet(handle, obj, key, val_str) catch continue;
    }

    return obj;
}

fn urlParseImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const input = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const components = parseUrlComponents(input);
    const obj = try sdk.createObject(handle);

    try setOptionalProp(handle, obj, "protocol", components.protocol);
    try setOptionalProp(handle, obj, "host", components.host);
    try setOptionalProp(handle, obj, "port", components.port);
    try setOptionalProp(handle, obj, "query", components.query);
    try setOptionalProp(handle, obj, "hash", components.hash);

    try sdk.objectSet(handle, obj, "path", try sdk.createString(handle, components.path));

    const sp_val = if (components.query) |q|
        try buildSearchParamsObject(handle, q)
    else
        try sdk.createObject(handle);
    try sdk.objectSet(handle, obj, "searchParams", sp_val);

    return obj;
}

fn setOptionalProp(handle: *sdk.ModuleHandle, obj: sdk.JSValue, name: []const u8, val: ?[]const u8) !void {
    const js_val = if (val) |v| try sdk.createString(handle, v) else sdk.JSValue.undefined_val;
    try sdk.objectSet(handle, obj, name, js_val);
}

fn urlSearchParamsImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const input = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const query = if (input.len > 0 and input[0] == '?') input[1..] else input;
    return buildSearchParamsObject(handle, query);
}

fn urlEncodeImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const input = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const allocator = sdk.getAllocator(handle);
    const buf = allocator.alloc(u8, input.len * 3) catch return sdk.JSValue.undefined_val;
    defer allocator.free(buf);

    var out: usize = 0;
    for (input) |byte| {
        if (isUnreserved(byte)) {
            buf[out] = byte;
            out += 1;
        } else {
            buf[out] = '%';
            const hex = std.fmt.bytesToHex([_]u8{byte}, .upper);
            buf[out + 1] = hex[0];
            buf[out + 2] = hex[1];
            out += 3;
        }
    }

    return sdk.createString(handle, buf[0..out]) catch sdk.JSValue.undefined_val;
}

fn urlDecodeImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const input = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const allocator = sdk.getAllocator(handle);
    const buf = allocator.alloc(u8, input.len) catch return sdk.JSValue.undefined_val;
    defer allocator.free(buf);

    const decoded = percentDecode(buf, input);
    return sdk.createString(handle, decoded) catch sdk.JSValue.undefined_val;
}

fn isUnreserved(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '-' or c == '.' or c == '_' or c == '~';
}

test "parseUrlComponents: full URL" {
    const c = parseUrlComponents("https://example.com:8080/path/to?key=val&b=2#frag");
    try std.testing.expectEqualStrings("https", c.protocol.?);
    try std.testing.expectEqualStrings("example.com", c.host.?);
    try std.testing.expectEqualStrings("8080", c.port.?);
    try std.testing.expectEqualStrings("/path/to", c.path);
    try std.testing.expectEqualStrings("key=val&b=2", c.query.?);
    try std.testing.expectEqualStrings("frag", c.hash.?);
}

test "parseUrlComponents: path-only URL" {
    const c = parseUrlComponents("/users/123?page=2&limit=10");
    try std.testing.expect(c.protocol == null);
    try std.testing.expectEqualStrings("/users/123", c.path);
    try std.testing.expectEqualStrings("page=2&limit=10", c.query.?);
}

test "parseUrlComponents: scheme without port" {
    const c = parseUrlComponents("http://api.test/v1/items");
    try std.testing.expectEqualStrings("http", c.protocol.?);
    try std.testing.expectEqualStrings("api.test", c.host.?);
    try std.testing.expect(c.port == null);
}

test "parseUrlComponents: root path" {
    const c = parseUrlComponents("/");
    try std.testing.expectEqualStrings("/", c.path);
    try std.testing.expect(c.query == null);
}

test "percentDecode" {
    var buf: [64]u8 = undefined;
    try std.testing.expectEqualStrings("hello world", percentDecode(&buf, "hello%20world"));
    try std.testing.expectEqualStrings("hello world", percentDecode(&buf, "hello+world"));
    try std.testing.expectEqualStrings("a&b=c", percentDecode(&buf, "a%26b%3Dc"));
    try std.testing.expectEqualStrings("plain", percentDecode(&buf, "plain"));
}

test "isUnreserved" {
    try std.testing.expect(isUnreserved('a'));
    try std.testing.expect(isUnreserved('-'));
    try std.testing.expect(isUnreserved('~'));
    try std.testing.expect(!isUnreserved(' '));
    try std.testing.expect(!isUnreserved('&'));
    try std.testing.expect(!isUnreserved('/'));
}
