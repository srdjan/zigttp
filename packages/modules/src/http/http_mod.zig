//! zigttp:http - HTTP request/response helpers

const std = @import("std");
const sdk = @import("zigttp-sdk");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:http",
    .name = "http",
    .exports = &.{
        .{ .name = "parseCookies", .module_func = parseCookiesImpl, .arg_count = 1, .returns = .object, .param_types = &.{.string}, .effect = .none, .return_labels = .{ .user_input = true, .credential = true } },
        .{ .name = "setCookie", .module_func = setCookieImpl, .arg_count = 3, .returns = .string, .param_types = &.{ .string, .string, .object }, .effect = .none, .return_labels = .{ .internal = true }, .contract_extractions = &.{.{ .arg_position = 0, .category = .cookie_name }} },
        .{ .name = "negotiate", .module_func = negotiateImpl, .arg_count = 2, .returns = .optional_string, .param_types = &.{ .string, .string }, .effect = .none, .failure_severity = .expected },
        .{ .name = "parseContentType", .module_func = parseContentTypeImpl, .arg_count = 1, .returns = .object, .param_types = &.{.string} },
        .{ .name = "cors", .module_func = corsImpl, .arg_count = 2, .returns = .object, .param_types = &.{ .string, .object }, .effect = .none, .contract_extractions = &.{.{ .arg_position = 0, .category = .cors_origin }} },
    },
};

fn parseCookiesImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const header = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const obj = try sdk.createObject(handle);

    var pairs = std.mem.splitScalar(u8, header, ';');
    while (pairs.next()) |pair| {
        const trimmed = std.mem.trim(u8, pair, " ");
        if (trimmed.len == 0) continue;

        const eq_pos = std.mem.indexOfScalar(u8, trimmed, '=') orelse continue;
        const name = std.mem.trim(u8, trimmed[0..eq_pos], " ");
        if (name.len == 0) continue;
        const val = trimmed[eq_pos + 1 ..];

        const val_str = sdk.createString(handle, val) catch continue;
        sdk.objectSet(handle, obj, name, val_str) catch continue;
    }

    return obj;
}

fn setCookieImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.undefined_val;
    const name = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;
    const val = sdk.extractString(args[1]) orelse return sdk.JSValue.undefined_val;

    const allocator = sdk.getAllocator(handle);
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(allocator);

    try buf.appendSlice(allocator, name);
    try buf.append(allocator, '=');
    try buf.appendSlice(allocator, val);

    if (args.len > 2 and sdk.isObject(args[2])) {
        const opts = args[2];

        if (getStringProp(handle, opts, "path")) |path| {
            try buf.appendSlice(allocator, "; Path=");
            try buf.appendSlice(allocator, path);
        }
        if (getStringProp(handle, opts, "domain")) |domain| {
            try buf.appendSlice(allocator, "; Domain=");
            try buf.appendSlice(allocator, domain);
        }
        if (getNumberProp(handle, opts, "maxAge")) |max_age| {
            try buf.appendSlice(allocator, "; Max-Age=");
            var num_buf: [20]u8 = undefined;
            const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{@as(i64, @intFromFloat(max_age))}) catch "";
            try buf.appendSlice(allocator, num_str);
        }
        if (getStringProp(handle, opts, "expires")) |expires| {
            try buf.appendSlice(allocator, "; Expires=");
            try buf.appendSlice(allocator, expires);
        }
        if (getBoolProp(handle, opts, "httpOnly")) |http_only| {
            if (http_only) try buf.appendSlice(allocator, "; HttpOnly");
        }
        if (getBoolProp(handle, opts, "secure")) |secure| {
            if (secure) try buf.appendSlice(allocator, "; Secure");
        }
        if (getStringProp(handle, opts, "sameSite")) |same_site| {
            try buf.appendSlice(allocator, "; SameSite=");
            try buf.appendSlice(allocator, same_site);
        }
    }

    return sdk.createString(handle, buf.items) catch sdk.JSValue.undefined_val;
}

fn negotiateImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.undefined_val;
    const accept_header = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;
    const available_str = sdk.extractString(args[1]) orelse return sdk.JSValue.undefined_val;

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

    var entries: [32]struct { media_type: []const u8, quality: f32 } = undefined;
    var entry_count: usize = 0;
    var accept_iter = std.mem.splitScalar(u8, accept_header, ',');
    while (accept_iter.next()) |entry| {
        if (entry_count >= 32) break;
        const trimmed = std.mem.trim(u8, entry, " ");
        if (trimmed.len == 0) continue;

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

    for (1..entry_count) |i| {
        const key = entries[i];
        var j: usize = i;
        while (j > 0 and entries[j - 1].quality < key.quality) {
            entries[j] = entries[j - 1];
            j -= 1;
        }
        entries[j] = key;
    }

    for (entries[0..entry_count]) |entry| {
        if (entry.quality <= 0) continue;
        for (available_types[0..available_count]) |avail| {
            if (mediaTypeMatches(entry.media_type, avail)) {
                return sdk.createString(handle, avail) catch sdk.JSValue.undefined_val;
            }
        }
    }

    return sdk.JSValue.undefined_val;
}

fn mediaTypeMatches(accept: []const u8, available: []const u8) bool {
    if (std.mem.eql(u8, accept, "*/*")) return true;
    if (std.ascii.eqlIgnoreCase(accept, available)) return true;
    if (std.mem.endsWith(u8, accept, "/*")) {
        const accept_type = accept[0 .. accept.len - 2];
        if (std.mem.indexOf(u8, available, "/")) |slash| {
            if (std.ascii.eqlIgnoreCase(accept_type, available[0..slash])) return true;
        }
    }
    return false;
}

fn parseContentTypeImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const header = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const obj = try sdk.createObject(handle);

    const semi_pos = std.mem.indexOfScalar(u8, header, ';');
    const media_type = std.mem.trim(u8, if (semi_pos) |p| header[0..p] else header, " ");

    if (std.mem.indexOfScalar(u8, media_type, '/')) |slash| {
        try sdk.objectSet(handle, obj, "type", try sdk.createString(handle, media_type[0..slash]));
        try sdk.objectSet(handle, obj, "subtype", try sdk.createString(handle, media_type[slash + 1 ..]));
    } else {
        try sdk.objectSet(handle, obj, "type", try sdk.createString(handle, media_type));
    }

    if (semi_pos) |sp| {
        var param_iter = std.mem.splitScalar(u8, header[sp + 1 ..], ';');
        while (param_iter.next()) |param| {
            const trimmed = std.mem.trim(u8, param, " ");
            if (std.ascii.startsWithIgnoreCase(trimmed, "charset=")) {
                const charset_val = std.mem.trim(u8, trimmed[8..], " \"");
                try sdk.objectSet(handle, obj, "charset", try sdk.createString(handle, charset_val));
                break;
            }
        }
    }

    return obj;
}

fn corsImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const origin = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    const obj = try sdk.createObject(handle);
    try sdk.objectSet(handle, obj, "Access-Control-Allow-Origin", try sdk.createString(handle, origin));

    if (args.len > 1 and sdk.isObject(args[1])) {
        const opts = args[1];

        if (getStringProp(handle, opts, "methods")) |methods| {
            try sdk.objectSet(handle, obj, "Access-Control-Allow-Methods", try sdk.createString(handle, methods));
        }
        if (getStringProp(handle, opts, "headers")) |headers| {
            try sdk.objectSet(handle, obj, "Access-Control-Allow-Headers", try sdk.createString(handle, headers));
        }
        if (getNumberProp(handle, opts, "maxAge")) |max_age| {
            var num_buf: [20]u8 = undefined;
            const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{@as(i64, @intFromFloat(max_age))}) catch "0";
            try sdk.objectSet(handle, obj, "Access-Control-Max-Age", try sdk.createString(handle, num_str));
        }
        if (getBoolProp(handle, opts, "credentials")) |creds| {
            if (creds) {
                try sdk.objectSet(handle, obj, "Access-Control-Allow-Credentials", try sdk.createString(handle, "true"));
            }
        }
    }

    return obj;
}

fn getStringProp(handle: *sdk.ModuleHandle, obj: sdk.JSValue, name: []const u8) ?[]const u8 {
    const prop = sdk.objectGet(handle, obj, name) orelse return null;
    return sdk.extractString(prop);
}

fn getNumberProp(handle: *sdk.ModuleHandle, obj: sdk.JSValue, name: []const u8) ?f64 {
    const prop = sdk.objectGet(handle, obj, name) orelse return null;
    return sdk.extractFloat(prop);
}

fn getBoolProp(handle: *sdk.ModuleHandle, obj: sdk.JSValue, name: []const u8) ?bool {
    const prop = sdk.objectGet(handle, obj, name) orelse return null;
    if (prop.isTrue()) return true;
    if (prop.isFalse()) return false;
    return null;
}

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
