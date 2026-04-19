//! zigttp:http - HTTP request/response helpers
//!
//! Exports:
//!   parseCookies(header: string) -> object
//!     Parses a Cookie header string into { name: "value", ... }.
//!
//!   setCookie(name: string, value: string, opts?: object) -> string
//!     Builds a Set-Cookie header string. Options: path, domain, maxAge,
//!     expires, httpOnly, secure, sameSite.
//!
//!   negotiate(accept: string, available: string) -> string | undefined
//!     Content negotiation. Returns best match from comma-separated available types
//!     against an Accept header, or undefined if none match.
//!
//!   parseContentType(header: string) -> object
//!     Parses Content-Type header into { type, subtype, charset }.
//!
//!   cors(origin: string, opts?: object) -> object
//!     Builds CORS response headers object. Options: methods, headers, maxAge,
//!     credentials.

const std = @import("std");
const context = @import("../../context.zig");
const value = @import("../../value.zig");
const object = @import("../../object.zig");
const util = @import("../internal/util.zig");
const mb = @import("../../module_binding.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:http",
    .name = "http",
    .exports = &.{
        .{ .name = "parseCookies", .func = parseCookiesNative, .arg_count = 1, .returns = .object, .param_types = &.{.string}, .effect = .none, .return_labels = .{ .user_input = true, .credential = true } },
        .{ .name = "setCookie", .func = setCookieNative, .arg_count = 3, .returns = .string, .param_types = &.{ .string, .string, .object }, .effect = .none, .return_labels = .{ .internal = true }, .contract_extractions = &.{.{ .arg_position = 0, .category = .cookie_name }} },
        .{ .name = "negotiate", .func = negotiateNative, .arg_count = 2, .returns = .optional_string, .param_types = &.{ .string, .string }, .effect = .none, .failure_severity = .expected },
        .{ .name = "parseContentType", .func = parseContentTypeNative, .arg_count = 1, .returns = .object, .param_types = &.{.string} },
        .{ .name = "cors", .func = corsNative, .arg_count = 2, .returns = .object, .param_types = &.{ .string, .object }, .effect = .none, .contract_extractions = &.{.{ .arg_position = 0, .category = .cors_origin }} },
    },
};

pub const exports = binding.toModuleExports();

// -------------------------------------------------------------------------
// parseCookies
// -------------------------------------------------------------------------

/// Parse "name=value; name2=value2" into a JS object
fn parseCookiesNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.undefined_val;
    const header = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const obj = try ctx.createObject(null);

    var pairs = std.mem.splitScalar(u8, header, ';');
    while (pairs.next()) |pair| {
        const trimmed = std.mem.trim(u8, pair, " ");
        if (trimmed.len == 0) continue;

        const eq_pos = std.mem.indexOfScalar(u8, trimmed, '=') orelse continue;
        const name = std.mem.trim(u8, trimmed[0..eq_pos], " ");
        if (name.len == 0) continue;
        const val = trimmed[eq_pos + 1 ..];

        const name_atom = ctx.atoms.intern(name) catch continue;
        const val_str = ctx.createString(val) catch continue;
        obj.setProperty(ctx.allocator, pool, name_atom, val_str) catch continue;
    }

    return obj.toValue();
}

// -------------------------------------------------------------------------
// setCookie
// -------------------------------------------------------------------------

/// Build a Set-Cookie header string from name, value, and options object.
fn setCookieNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len < 2) return value.JSValue.undefined_val;
    const name = util.extractString(args[0]) orelse return value.JSValue.undefined_val;
    const val = util.extractString(args[1]) orelse return value.JSValue.undefined_val;

    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    // Start building: name=value
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(ctx.allocator);

    try buf.appendSlice(ctx.allocator, name);
    try buf.append(ctx.allocator, '=');
    try buf.appendSlice(ctx.allocator, val);

    // Process options object if provided
    if (args.len > 2 and args[2].isObject()) {
        const opts = args[2].toPtr(object.JSObject);

        // path
        if (getStringProp(opts, pool, &ctx.atoms, "path")) |path| {
            try buf.appendSlice(ctx.allocator, "; Path=");
            try buf.appendSlice(ctx.allocator, path);
        }

        // domain
        if (getStringProp(opts, pool, &ctx.atoms, "domain")) |domain| {
            try buf.appendSlice(ctx.allocator, "; Domain=");
            try buf.appendSlice(ctx.allocator, domain);
        }

        // maxAge (number of seconds)
        if (getNumberProp(opts, pool, &ctx.atoms, "maxAge")) |max_age| {
            try buf.appendSlice(ctx.allocator, "; Max-Age=");
            var num_buf: [20]u8 = undefined;
            const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{@as(i64, @intFromFloat(max_age))}) catch "";
            try buf.appendSlice(ctx.allocator, num_str);
        }

        // expires (string, e.g. HTTP date)
        if (getStringProp(opts, pool, &ctx.atoms, "expires")) |expires| {
            try buf.appendSlice(ctx.allocator, "; Expires=");
            try buf.appendSlice(ctx.allocator, expires);
        }

        // httpOnly
        if (getBoolProp(opts, pool, &ctx.atoms, "httpOnly")) |http_only| {
            if (http_only) try buf.appendSlice(ctx.allocator, "; HttpOnly");
        }

        // secure
        if (getBoolProp(opts, pool, &ctx.atoms, "secure")) |secure| {
            if (secure) try buf.appendSlice(ctx.allocator, "; Secure");
        }

        // sameSite
        if (getStringProp(opts, pool, &ctx.atoms, "sameSite")) |same_site| {
            try buf.appendSlice(ctx.allocator, "; SameSite=");
            try buf.appendSlice(ctx.allocator, same_site);
        }
    }

    return ctx.createString(buf.items) catch value.JSValue.undefined_val;
}

// -------------------------------------------------------------------------
// negotiate
// -------------------------------------------------------------------------

/// Simple content negotiation: find best match between Accept header and available types.
/// Returns the first available type that matches any Accept entry, or undefined.
fn negotiateNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len < 2) return value.JSValue.undefined_val;
    const accept_header = util.extractString(args[0]) orelse return value.JSValue.undefined_val;
    const available_str = util.extractString(args[1]) orelse return value.JSValue.undefined_val;

    // Parse available types (comma-separated)
    var available_types: [16][]const u8 = undefined;
    var available_count: usize = 0;
    {
        var iter = std.mem.splitScalar(u8, available_str, ',');
        while (iter.next()) |t| {
            if (available_count >= 16) break;
            const trimmed = std.mem.trim(u8, t, " ");
            if (trimmed.len > 0) {
                available_types[available_count] = trimmed;
                available_count += 1;
            }
        }
    }

    // Parse Accept header entries (comma-separated, may include quality)
    var accept_iter = std.mem.splitScalar(u8, accept_header, ',');
    // Collect and sort by quality (descending)
    var entries: [32]struct { media_type: []const u8, quality: f32 } = undefined;
    var entry_count: usize = 0;
    while (accept_iter.next()) |entry| {
        if (entry_count >= 32) break;
        const trimmed = std.mem.trim(u8, entry, " ");
        if (trimmed.len == 0) continue;

        // Split off quality parameter
        var quality: f32 = 1.0;
        var media_type = trimmed;
        if (std.mem.indexOf(u8, trimmed, ";q=")) |q_pos| {
            media_type = std.mem.trim(u8, trimmed[0..q_pos], " ");
            const q_str = trimmed[q_pos + 3 ..];
            quality = std.fmt.parseFloat(f32, q_str) catch 1.0;
        }

        entries[entry_count] = .{ .media_type = media_type, .quality = quality };
        entry_count += 1;
    }

    // Sort by quality descending (simple insertion sort for small arrays)
    for (1..entry_count) |i| {
        const key = entries[i];
        var j: usize = i;
        while (j > 0 and entries[j - 1].quality < key.quality) {
            entries[j] = entries[j - 1];
            j -= 1;
        }
        entries[j] = key;
    }

    // Find first match
    for (entries[0..entry_count]) |entry| {
        if (entry.quality <= 0) continue;
        for (available_types[0..available_count]) |avail| {
            if (mediaTypeMatches(entry.media_type, avail)) {
                return ctx.createString(avail) catch value.JSValue.undefined_val;
            }
        }
    }

    return value.JSValue.undefined_val;
}

/// Check if an accept media type matches an available type.
/// Handles wildcards: */* matches everything, text/* matches text/html, etc.
fn mediaTypeMatches(accept: []const u8, available: []const u8) bool {
    if (std.mem.eql(u8, accept, "*/*")) return true;
    if (std.ascii.eqlIgnoreCase(accept, available)) return true;

    // Check type/* wildcard
    if (std.mem.endsWith(u8, accept, "/*")) {
        const accept_type = accept[0 .. accept.len - 2];
        if (std.mem.indexOf(u8, available, "/")) |slash| {
            if (std.ascii.eqlIgnoreCase(accept_type, available[0..slash])) return true;
        }
    }

    return false;
}

// -------------------------------------------------------------------------
// parseContentType
// -------------------------------------------------------------------------

/// Parse "type/subtype; charset=utf-8" into { type, subtype, charset }
fn parseContentTypeNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.undefined_val;
    const header = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const obj = try ctx.createObject(null);

    // Split media type from parameters
    const semi_pos = std.mem.indexOfScalar(u8, header, ';');
    const media_type = std.mem.trim(u8, if (semi_pos) |p| header[0..p] else header, " ");

    // Split type/subtype
    if (std.mem.indexOfScalar(u8, media_type, '/')) |slash| {
        const type_atom = try ctx.atoms.intern("type");
        const type_str = try ctx.createString(media_type[0..slash]);
        try obj.setProperty(ctx.allocator, pool, type_atom, type_str);

        const subtype_atom = try ctx.atoms.intern("subtype");
        const subtype_str = try ctx.createString(media_type[slash + 1 ..]);
        try obj.setProperty(ctx.allocator, pool, subtype_atom, subtype_str);
    } else {
        const type_atom = try ctx.atoms.intern("type");
        const type_str = try ctx.createString(media_type);
        try obj.setProperty(ctx.allocator, pool, type_atom, type_str);
    }

    // Extract charset parameter if present
    if (semi_pos) |sp| {
        const params = header[sp + 1 ..];
        var param_iter = std.mem.splitScalar(u8, params, ';');
        while (param_iter.next()) |param| {
            const trimmed = std.mem.trim(u8, param, " ");
            if (std.ascii.startsWithIgnoreCase(trimmed, "charset=")) {
                const charset_val = std.mem.trim(u8, trimmed[8..], " \"");
                const charset_atom = try ctx.atoms.intern("charset");
                const charset_str = try ctx.createString(charset_val);
                try obj.setProperty(ctx.allocator, pool, charset_atom, charset_str);
                break;
            }
        }
    }

    return obj.toValue();
}

// -------------------------------------------------------------------------
// cors
// -------------------------------------------------------------------------

/// Build CORS response headers from origin and options.
fn corsNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.undefined_val;
    const origin = util.extractString(args[0]) orelse return value.JSValue.undefined_val;

    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const obj = try ctx.createObject(null);

    // Access-Control-Allow-Origin
    const origin_atom = try ctx.atoms.intern("Access-Control-Allow-Origin");
    const origin_str = try ctx.createString(origin);
    try obj.setProperty(ctx.allocator, pool, origin_atom, origin_str);

    // Process options
    if (args.len > 1 and args[1].isObject()) {
        const opts = args[1].toPtr(object.JSObject);

        // methods
        if (getStringProp(opts, pool, &ctx.atoms, "methods")) |methods| {
            const methods_atom = try ctx.atoms.intern("Access-Control-Allow-Methods");
            const methods_str = try ctx.createString(methods);
            try obj.setProperty(ctx.allocator, pool, methods_atom, methods_str);
        }

        // headers
        if (getStringProp(opts, pool, &ctx.atoms, "headers")) |headers| {
            const headers_atom = try ctx.atoms.intern("Access-Control-Allow-Headers");
            const headers_str = try ctx.createString(headers);
            try obj.setProperty(ctx.allocator, pool, headers_atom, headers_str);
        }

        // maxAge
        if (getNumberProp(opts, pool, &ctx.atoms, "maxAge")) |max_age| {
            const max_age_atom = try ctx.atoms.intern("Access-Control-Max-Age");
            var num_buf: [20]u8 = undefined;
            const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{@as(i64, @intFromFloat(max_age))}) catch "0";
            const max_age_str = try ctx.createString(num_str);
            try obj.setProperty(ctx.allocator, pool, max_age_atom, max_age_str);
        }

        // credentials
        if (getBoolProp(opts, pool, &ctx.atoms, "credentials")) |creds| {
            if (creds) {
                const creds_atom = try ctx.atoms.intern("Access-Control-Allow-Credentials");
                const creds_str = try ctx.createString("true");
                try obj.setProperty(ctx.allocator, pool, creds_atom, creds_str);
            }
        }
    }

    return obj.toValue();
}

// -------------------------------------------------------------------------
// Helpers for reading properties from JS objects
// -------------------------------------------------------------------------

fn getStringProp(obj: *object.JSObject, pool: *object.HiddenClassPool, atoms: *context.AtomTable, name: []const u8) ?[]const u8 {
    const atom = atoms.intern(name) catch return null;
    const prop_val = obj.getProperty(pool, atom) orelse return null;
    return util.extractString(prop_val);
}

fn getNumberProp(obj: *object.JSObject, pool: *object.HiddenClassPool, atoms: *context.AtomTable, name: []const u8) ?f64 {
    const atom = atoms.intern(name) catch return null;
    const prop_val = obj.getProperty(pool, atom) orelse return null;
    return util.extractFloat(prop_val);
}

fn getBoolProp(obj: *object.JSObject, pool: *object.HiddenClassPool, atoms: *context.AtomTable, name: []const u8) ?bool {
    const atom = atoms.intern(name) catch return null;
    const prop_val = obj.getProperty(pool, atom) orelse return null;
    if (prop_val.isTrue()) return true;
    if (prop_val.isFalse()) return false;
    return null;
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "mediaTypeMatches: exact match" {
    try std.testing.expect(mediaTypeMatches("text/html", "text/html"));
    try std.testing.expect(!mediaTypeMatches("text/html", "text/plain"));
}

test "mediaTypeMatches: wildcard" {
    try std.testing.expect(mediaTypeMatches("*/*", "text/html"));
    try std.testing.expect(mediaTypeMatches("*/*", "application/json"));
}

test "mediaTypeMatches: type wildcard" {
    try std.testing.expect(mediaTypeMatches("text/*", "text/html"));
    try std.testing.expect(mediaTypeMatches("text/*", "text/plain"));
    try std.testing.expect(!mediaTypeMatches("text/*", "application/json"));
}

test "mediaTypeMatches: case insensitive" {
    try std.testing.expect(mediaTypeMatches("Text/HTML", "text/html"));
}
