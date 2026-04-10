//! zigttp:url - URL parsing and encoding
//!
//! Exports:
//!   urlParse(url: string) -> object
//!     Parses a URL string into { protocol, host, port, path, query, hash, searchParams }.
//!     Handles both full URLs (https://host/path) and path-only (/path?query).
//!
//!   urlSearchParams(query: string) -> object
//!     Parses a query string ("key=val&key2=val2") into { key: "val", key2: "val2" }.
//!     Strips leading "?" if present. Percent-decodes keys and values.
//!
//!   urlEncode(str: string) -> string
//!     Percent-encodes a string for safe use in URL components.
//!
//!   urlDecode(str: string) -> string
//!     Percent-decodes a URL-encoded string.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const util = @import("util.zig");
const mb = @import("../module_binding.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:url",
    .name = "url",
    .exports = &.{
        .{ .name = "urlParse", .func = urlParseNative, .arg_count = 1, .returns = .object, .param_types = &.{.string}, .effect = .none, .return_labels = .{ .user_input = true } },
        .{ .name = "urlSearchParams", .func = urlSearchParamsNative, .arg_count = 1, .returns = .object, .param_types = &.{.string}, .effect = .none, .return_labels = .{ .user_input = true } },
        .{ .name = "urlEncode", .func = urlEncodeNative, .arg_count = 1, .returns = .string, .param_types = &.{.string} },
        .{ .name = "urlDecode", .func = urlDecodeNative, .arg_count = 1, .returns = .string, .param_types = &.{.string} },
    },
};

pub const exports = binding.toModuleExports();

// -------------------------------------------------------------------------
// URL component extraction (no-alloc, returns slices into the input)
// -------------------------------------------------------------------------

const UrlComponents = struct {
    protocol: ?[]const u8 = null,
    host: ?[]const u8 = null,
    port: ?[]const u8 = null,
    path: []const u8 = "/",
    query: ?[]const u8 = null,
    hash: ?[]const u8 = null,
};

/// Parse a URL string into its components. Handles both absolute and path-only URLs.
/// All returned slices point into the original input - no allocation needed.
fn parseUrlComponents(input: []const u8) UrlComponents {
    var result = UrlComponents{};
    var rest = input;

    // Extract fragment (#hash)
    if (std.mem.indexOfScalar(u8, rest, '#')) |hash_pos| {
        result.hash = rest[hash_pos + 1 ..];
        rest = rest[0..hash_pos];
    }

    // Extract query (?key=val)
    if (std.mem.indexOfScalar(u8, rest, '?')) |query_pos| {
        result.query = rest[query_pos + 1 ..];
        rest = rest[0..query_pos];
    }

    // Check for scheme (protocol)
    if (std.mem.indexOf(u8, rest, "://")) |scheme_end| {
        result.protocol = rest[0..scheme_end];
        rest = rest[scheme_end + 3 ..];

        // Extract host (and optional port)
        const host_end = std.mem.indexOfScalar(u8, rest, '/') orelse rest.len;
        const authority = rest[0..host_end];

        // Check for port
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

// -------------------------------------------------------------------------
// Query string parsing
// -------------------------------------------------------------------------

/// Percent-decode a slice in-place, returning the decoded subslice.
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
        if (input[i] == '+') {
            buf[out] = ' ';
        } else {
            buf[out] = input[i];
        }
        out += 1;
        i += 1;
    }
    return buf[0..out];
}

/// Build a JS object from a query string (without leading ?).
fn buildSearchParamsObject(ctx: *context.Context, query: []const u8) !value.JSValue {
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const obj = try ctx.createObject(null);

    // Allocate a scratch buffer for percent-decoding (decoded is always <= encoded)
    const scratch = ctx.allocator.alloc(u8, query.len) catch return obj.toValue();
    defer ctx.allocator.free(scratch);

    var pairs = std.mem.splitScalar(u8, query, '&');
    while (pairs.next()) |pair| {
        if (pair.len == 0) continue;

        const eq_pos = std.mem.indexOfScalar(u8, pair, '=');
        const raw_key = if (eq_pos) |pos| pair[0..pos] else pair;
        const raw_val = if (eq_pos) |pos| pair[pos + 1 ..] else "";

        const key = percentDecode(scratch, raw_key);
        if (key.len == 0) continue;
        const key_atom = ctx.atoms.intern(key) catch continue;

        // Decode value into the portion of scratch after the key
        const val_buf = scratch[key.len..];
        const val = percentDecode(val_buf, raw_val);
        const val_str = ctx.createString(val) catch continue;

        obj.setProperty(ctx.allocator, pool, key_atom, val_str) catch continue;
    }

    return obj.toValue();
}

// -------------------------------------------------------------------------
// Native function implementations
// -------------------------------------------------------------------------

/// urlParse(url) -> { protocol, host, port, path, query, hash, searchParams }
fn urlParseNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    const components = parseUrlComponents(input);
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const obj = try ctx.createObject(null);

    try setOptionalProp(ctx, obj, pool, "protocol", components.protocol);
    try setOptionalProp(ctx, obj, pool, "host", components.host);
    try setOptionalProp(ctx, obj, pool, "port", components.port);
    try setOptionalProp(ctx, obj, pool, "query", components.query);
    try setOptionalProp(ctx, obj, pool, "hash", components.hash);

    const path_atom = try ctx.atoms.intern("path");
    try obj.setProperty(ctx.allocator, pool, path_atom, try ctx.createString(components.path));

    const sp_atom = try ctx.atoms.intern("searchParams");
    const sp_val = if (components.query) |q|
        try buildSearchParamsObject(ctx, q)
    else
        (try ctx.createObject(null)).toValue();
    try obj.setProperty(ctx.allocator, pool, sp_atom, sp_val);

    return obj.toValue();
}

fn setOptionalProp(ctx: *context.Context, obj: anytype, pool: anytype, name: []const u8, val: ?[]const u8) !void {
    const atom = try ctx.atoms.intern(name);
    const js_val = if (val) |v| try ctx.createString(v) else value.JSValue.undefined_val;
    try obj.setProperty(ctx.allocator, pool, atom, js_val);
}

/// urlSearchParams(query) -> { key: "value", ... }
fn urlSearchParamsNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    // Strip leading ? if present
    const query = if (input.len > 0 and input[0] == '?') input[1..] else input;
    return buildSearchParamsObject(ctx, query);
}

/// urlEncode(str) -> percent-encoded string
fn urlEncodeNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    // Worst case: every byte becomes %XX (3x expansion)
    const buf = ctx.allocator.alloc(u8, input.len * 3) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(buf);

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

    return ctx.createString(buf[0..out]) catch value.JSValue.undefined_val;
}

/// urlDecode(str) -> percent-decoded string
fn urlDecodeNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.undefined_val;
    const input = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    // Decoded string is always <= encoded length
    const buf = ctx.allocator.alloc(u8, input.len) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(buf);

    const decoded = percentDecode(buf, input);
    return ctx.createString(decoded) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// Helpers
// -------------------------------------------------------------------------

/// RFC 3986 unreserved characters: ALPHA / DIGIT / "-" / "." / "_" / "~"
fn isUnreserved(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '-' or c == '.' or c == '_' or c == '~';
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

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
    try std.testing.expect(c.host == null);
    try std.testing.expect(c.port == null);
    try std.testing.expectEqualStrings("/users/123", c.path);
    try std.testing.expectEqualStrings("page=2&limit=10", c.query.?);
    try std.testing.expect(c.hash == null);
}

test "parseUrlComponents: scheme without port" {
    const c = parseUrlComponents("http://api.test/v1/items");
    try std.testing.expectEqualStrings("http", c.protocol.?);
    try std.testing.expectEqualStrings("api.test", c.host.?);
    try std.testing.expect(c.port == null);
    try std.testing.expectEqualStrings("/v1/items", c.path);
}

test "parseUrlComponents: no path after host" {
    const c = parseUrlComponents("https://example.com");
    try std.testing.expectEqualStrings("https", c.protocol.?);
    try std.testing.expectEqualStrings("example.com", c.host.?);
    try std.testing.expectEqualStrings("/", c.path);
}

test "parseUrlComponents: root path" {
    const c = parseUrlComponents("/");
    try std.testing.expectEqualStrings("/", c.path);
    try std.testing.expect(c.query == null);
}

test "percentDecode: basic encoding" {
    var buf: [64]u8 = undefined;
    const result = percentDecode(&buf, "hello%20world");
    try std.testing.expectEqualStrings("hello world", result);
}

test "percentDecode: plus as space" {
    var buf: [64]u8 = undefined;
    const result = percentDecode(&buf, "hello+world");
    try std.testing.expectEqualStrings("hello world", result);
}

test "percentDecode: mixed encoding" {
    var buf: [64]u8 = undefined;
    const result = percentDecode(&buf, "a%26b%3Dc");
    try std.testing.expectEqualStrings("a&b=c", result);
}

test "percentDecode: no encoding" {
    var buf: [64]u8 = undefined;
    const result = percentDecode(&buf, "plain");
    try std.testing.expectEqualStrings("plain", result);
}

test "isUnreserved" {
    try std.testing.expect(isUnreserved('a'));
    try std.testing.expect(isUnreserved('Z'));
    try std.testing.expect(isUnreserved('0'));
    try std.testing.expect(isUnreserved('-'));
    try std.testing.expect(isUnreserved('.'));
    try std.testing.expect(isUnreserved('_'));
    try std.testing.expect(isUnreserved('~'));
    try std.testing.expect(!isUnreserved(' '));
    try std.testing.expect(!isUnreserved('&'));
    try std.testing.expect(!isUnreserved('='));
    try std.testing.expect(!isUnreserved('/'));
}
