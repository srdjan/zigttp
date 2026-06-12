//! zigttp:decode - typed request ingress helpers

const std = @import("std");
const sdk = @import("zigttp-sdk");
const validate = @import("validate.zig");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:decode",
    .name = "decode",
    .stateful = false,
    .exports = &.{
        // decodeJson/decodeForm/decodeQuery delegate to the same in-process
        // validate machinery as validateJson/coerceJson (which are replay_pure):
        // argument-deterministic, no ambient state, and a `.validated` (not
        // secret/credential) return label. Mark them replay_pure so they run for
        // real under `serve --test` instead of returning undefined without an io
        // mock - matching their validate siblings.
        .{
            .name = "decodeJson",
            .module_func = decodeJsonImpl,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
            .laws = &.{.pure},
            .replay_pure = true,
        },
        .{
            .name = "decodeForm",
            .module_func = decodeFormImpl,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
            .laws = &.{.pure},
            .replay_pure = true,
        },
        .{
            .name = "decodeQuery",
            .module_func = decodeQueryImpl,
            .arg_count = 2,
            .returns = .result,
            .param_types = &.{ .string, .object },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
            .laws = &.{.pure},
            .replay_pure = true,
        },
        .{
            .name = "decodeFormMultipart",
            .module_func = decodeFormMultipartImpl,
            .arg_count = 3,
            .returns = .result,
            .param_types = &.{ .string, .string, .string },
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .category = .request_schema }},
            .return_labels = .{ .validated = true },
            .laws = &.{.pure},
            .replay_pure = true,
        },
    },
};

fn decodeJsonImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    const body = sdk.extractString(args[1]) orelse return sdk.resultErr(handle, "body must be a string");
    return validate.decodeJson(handle, name, body, false);
}

fn decodeFormImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    const body = sdk.extractString(args[1]) orelse return sdk.resultErr(handle, "body must be a string");
    const obj = try parseFormObject(handle, body);
    return validate.decodeObject(handle, name, obj, true);
}

fn decodeQueryImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    if (!sdk.isObject(args[1])) return sdk.resultErr(handle, "query must be an object");
    return validate.decodeObject(handle, name, args[1], true);
}

fn parseFormObject(handle: *sdk.ModuleHandle, raw: []const u8) !sdk.JSValue {
    const allocator = sdk.getAllocator(handle);
    const obj = try sdk.createObject(handle);
    var parts = std.mem.splitScalar(u8, raw, '&');
    while (parts.next()) |part| {
        if (part.len == 0) continue;

        const eq_idx = std.mem.indexOfScalar(u8, part, '=') orelse part.len;
        const key_raw = part[0..eq_idx];
        if (key_raw.len == 0) continue;
        const val_raw = if (eq_idx < part.len) part[eq_idx + 1 ..] else "";

        const key = try decodeFormComponent(allocator, key_raw);
        defer allocator.free(key);
        const val = try decodeFormComponent(allocator, val_raw);
        defer allocator.free(val);

        const js_val = try sdk.createString(handle, val);
        try sdk.objectSet(handle, obj, key, js_val);
    }
    return obj;
}

fn decodeFormComponent(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.ensureTotalCapacity(allocator, input.len);

    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        const c = input[i];
        if (c == '+') {
            out.appendAssumeCapacity(' ');
            continue;
        }
        if (c == '%' and i + 2 < input.len) {
            const hi = fromHex(input[i + 1]);
            const lo = fromHex(input[i + 2]);
            if (hi != null and lo != null) {
                out.appendAssumeCapacity((hi.? << 4) | lo.?);
                i += 2;
                continue;
            }
        }
        out.appendAssumeCapacity(c);
    }

    return out.toOwnedSlice(allocator);
}

fn fromHex(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

// decodeFormMultipart: parse multipart/form-data
//
// Arguments: (name: string, body: string, content_type: string)
//   name         - schema name passed to validate.decodeObject
//   body         - raw multipart body (binary-safe)
//   content_type - value of the Content-Type header (must include boundary=...)
//
// Returns a Result<object> where the decoded object maps field names to:
//   - string for plain text parts
//   - { filename: string, content: string, contentType: string } for file parts
fn decodeFormMultipartImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 3) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    const body = sdk.extractString(args[1]) orelse return sdk.resultErr(handle, "body must be a string");
    const content_type = sdk.extractString(args[2]) orelse return sdk.resultErr(handle, "content_type must be a string");

    const boundary = parseBoundary(content_type) orelse
        return sdk.resultErr(handle, "Content-Type missing boundary parameter");

    const obj = parseMultipartObject(handle, body, boundary) catch |err| switch (err) {
        error.MalformedMultipart => return sdk.resultErr(handle, "malformed multipart body"),
        else => return err,
    };
    return validate.decodeObject(handle, name, obj, true);
}

/// Extract the boundary token from a Content-Type header value.
/// Accepts: "multipart/form-data; boundary=<token>" (with or without quotes).
fn parseBoundary(content_type: []const u8) ?[]const u8 {
    const needle = "boundary=";
    const idx = std.mem.indexOf(u8, content_type, needle) orelse return null;
    var rest = content_type[idx + needle.len ..];
    // Strip optional surrounding semicolons/whitespace after value
    if (rest.len > 0 and rest[0] == '"') {
        rest = rest[1..];
        const end = std.mem.indexOfScalar(u8, rest, '"') orelse rest.len;
        return rest[0..end];
    }
    // Unquoted: ends at ';' or whitespace
    var end: usize = 0;
    while (end < rest.len and rest[end] != ';' and rest[end] != ' ' and rest[end] != '\t') : (end += 1) {}
    return rest[0..end];
}

/// Parse a multipart/form-data body into a JS object.
/// Each part maps its `name` disposition parameter to either a string (text
/// parts) or a file-descriptor object ({ filename, content, contentType }).
fn parseMultipartObject(handle: *sdk.ModuleHandle, body: []const u8, boundary: []const u8) !sdk.JSValue {
    const obj = try sdk.createObject(handle);

    // Build the delimiter: "--<boundary>" (RFC 2046: boundary <= 70 chars)
    var delim_buf: [72]u8 = undefined;
    delim_buf[0] = '-';
    delim_buf[1] = '-';
    @memcpy(delim_buf[2..2 + boundary.len], boundary);
    const delim = delim_buf[0..2 + boundary.len];

    var it = std.mem.splitSequence(u8, body, delim);
    // Skip the preamble (everything before the first delimiter)
    _ = it.next();

    while (it.next()) |raw_part| {
        // "--<boundary>--" is the closing delimiter; the remaining slice will
        // start with "--" after the split drops the delimiter itself.
        if (std.mem.startsWith(u8, raw_part, "--")) break;

        // Each part starts with CRLF after the delimiter line.
        const part = if (std.mem.startsWith(u8, raw_part, "\r\n")) raw_part[2..] else raw_part;

        // Separate headers from body: blank line (CRLF CRLF).
        const header_end = std.mem.indexOf(u8, part, "\r\n\r\n") orelse return error.MalformedMultipart;
        const headers_raw = part[0..header_end];
        var part_body = part[header_end + 4 ..];
        // Strip trailing CRLF that precedes the next delimiter.
        if (std.mem.endsWith(u8, part_body, "\r\n")) {
            part_body = part_body[0 .. part_body.len - 2];
        }

        // Parse Content-Disposition header.
        const disposition = findHeader(headers_raw, "content-disposition") orelse continue;
        const field_name = parseDispositionParam(disposition, "name") orelse continue;
        const filename = parseDispositionParam(disposition, "filename");
        const part_ct = findHeader(headers_raw, "content-type");

        if (filename != null) {
            // File part: return an object descriptor.
            const file_obj = try sdk.createObject(handle);
            const fn_val = try sdk.createString(handle, filename.?);
            try sdk.objectSet(handle, file_obj, "filename", fn_val);
            const content_val = try sdk.createString(handle, part_body);
            try sdk.objectSet(handle, file_obj, "content", content_val);
            const ct_str = part_ct orelse "application/octet-stream";
            const ct_val = try sdk.createString(handle, ct_str);
            try sdk.objectSet(handle, file_obj, "contentType", ct_val);
            try sdk.objectSet(handle, obj, field_name, file_obj);
        } else {
            // Text part: return the body string directly.
            const text_val = try sdk.createString(handle, part_body);
            try sdk.objectSet(handle, obj, field_name, text_val);
        }
    }

    return obj;
}

/// Find the value of a named header (case-insensitive) in a CRLF-separated
/// header block. Returns the trimmed value after the colon.
fn findHeader(headers: []const u8, name: []const u8) ?[]const u8 {
    var lines = std.mem.splitSequence(u8, headers, "\r\n");
    while (lines.next()) |line| {
        const colon = std.mem.indexOfScalar(u8, line, ':') orelse continue;
        const key = std.mem.trim(u8, line[0..colon], " \t");
        if (std.ascii.eqlIgnoreCase(key, name)) {
            return std.mem.trim(u8, line[colon + 1 ..], " \t");
        }
    }
    return null;
}

/// Extract a quoted or unquoted parameter value from a Content-Disposition
/// (or similar) header value string.
fn parseDispositionParam(header_value: []const u8, param: []const u8) ?[]const u8 {
    var start: usize = 0;
    while (start < header_value.len) {
        while (start < header_value.len and (header_value[start] == ';' or header_value[start] == ' ' or header_value[start] == '\t')) : (start += 1) {}
        const segment_start = start;

        var in_quote = false;
        while (start < header_value.len) {
            const c = header_value[start];
            if (c == '\\' and in_quote and start + 1 < header_value.len) {
                start += 2;
                continue;
            }
            if (c == '"') in_quote = !in_quote;
            if (c == ';' and !in_quote) break;
            start += 1;
        }

        const segment = std.mem.trim(u8, header_value[segment_start..start], " \t");
        if (start < header_value.len and header_value[start] == ';') start += 1;

        const eq = std.mem.indexOfScalar(u8, segment, '=') orelse continue;
        const key = std.mem.trim(u8, segment[0..eq], " \t");
        if (!std.mem.eql(u8, key, param)) continue;

        var value = std.mem.trim(u8, segment[eq + 1 ..], " \t");
        if (value.len > 0 and value[0] == '"') {
            value = value[1..];
            const end = std.mem.indexOfScalar(u8, value, '"') orelse value.len;
            return value[0..end];
        }
        var end: usize = 0;
        while (end < value.len and value[end] != ' ' and value[end] != '\t') : (end += 1) {}
        return value[0..end];
    }
    return null;
}

test "decodeFormComponent decodes plus and percent escapes" {
    const allocator = std.testing.allocator;
    const decoded = try decodeFormComponent(allocator, "display+name=zig%20ttp");
    defer allocator.free(decoded);
    try std.testing.expectEqualStrings("display name=zig ttp", decoded);
}

test "parseBoundary extracts unquoted boundary" {
    const boundary = parseBoundary("multipart/form-data; boundary=----WebKitFormBoundaryXYZ");
    try std.testing.expect(boundary != null);
    try std.testing.expectEqualStrings("----WebKitFormBoundaryXYZ", boundary.?);
}

test "parseBoundary extracts quoted boundary" {
    const boundary = parseBoundary("multipart/form-data; boundary=\"my boundary\"");
    try std.testing.expect(boundary != null);
    try std.testing.expectEqualStrings("my boundary", boundary.?);
}

test "parseBoundary returns null for missing boundary" {
    try std.testing.expect(parseBoundary("multipart/form-data") == null);
    try std.testing.expect(parseBoundary("application/json") == null);
}

test "parseDispositionParam extracts quoted name" {
    const disp = "form-data; name=\"username\"; filename=\"file.txt\"";
    const name = parseDispositionParam(disp, "name");
    try std.testing.expect(name != null);
    try std.testing.expectEqualStrings("username", name.?);
    const filename = parseDispositionParam(disp, "filename");
    try std.testing.expect(filename != null);
    try std.testing.expectEqualStrings("file.txt", filename.?);
}

test "parseDispositionParam matches parameter names by boundary" {
    const disp = "form-data; filename=\"a.txt\"; name=\"upload\"";
    const name = parseDispositionParam(disp, "name");
    try std.testing.expect(name != null);
    try std.testing.expectEqualStrings("upload", name.?);
    const filename = parseDispositionParam(disp, "filename");
    try std.testing.expect(filename != null);
    try std.testing.expectEqualStrings("a.txt", filename.?);
}

test "parseDispositionParam preserves semicolons in quoted values" {
    const disp = "form-data; filename=\"a;b.txt\"; name=\"upload\"";
    const filename = parseDispositionParam(disp, "filename");
    try std.testing.expect(filename != null);
    try std.testing.expectEqualStrings("a;b.txt", filename.?);
}

test "findHeader is case-insensitive" {
    const headers = "Content-Disposition: form-data; name=\"x\"\r\nContent-Type: text/plain";
    try std.testing.expect(findHeader(headers, "content-disposition") != null);
    try std.testing.expect(findHeader(headers, "Content-Disposition") != null);
    try std.testing.expect(findHeader(headers, "CONTENT-TYPE") != null);
    try std.testing.expect(findHeader(headers, "x-missing") == null);
}
