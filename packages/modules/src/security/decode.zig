//! zttp:decode - typed request ingress helpers

const std = @import("std");
const sdk = @import("zttp-sdk");
const validate = @import("validate.zig");

pub const binding = sdk.ModuleBinding{
    .specifier = "zttp:decode",
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
            .effect = .none,
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
            .effect = .none,
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
            .effect = .none,
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
            .effect = .none,
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
    const obj = parseFormObject(handle, body) catch |err| switch (err) {
        error.DuplicateField => return sdk.resultErr(handle, "duplicate field name in form body"),
        else => return err,
    };
    return validate.decodeObject(handle, name, obj, true);
}

fn decodeQueryImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.resultErr(handle, "missing arguments");
    const name = sdk.extractString(args[0]) orelse return sdk.resultErr(handle, "name must be a string");
    if (!sdk.isObject(args[1])) return sdk.resultErr(handle, "query must be an object");
    const query_copy = cloneValueForDecode(handle, args[1], 0) catch
        return sdk.resultErr(handle, "failed to copy query");
    return validate.decodeObject(handle, name, query_copy, true);
}

const MAX_DECODE_CLONE_DEPTH = 32;

fn cloneValueForDecode(handle: *sdk.ModuleHandle, val: sdk.JSValue, depth: u32) !sdk.JSValue {
    if (depth > MAX_DECODE_CLONE_DEPTH) return error.DecodeInputTooDeep;

    if (sdk.isArray(val)) {
        const out = try sdk.createArray(handle);
        const len = sdk.arrayLength(val) orelse return error.InvalidArray;
        var i: u32 = 0;
        while (i < len) : (i += 1) {
            if (sdk.arrayGet(handle, val, i)) |elem| {
                try sdk.arraySet(handle, out, i, try cloneValueForDecode(handle, elem, depth + 1));
            }
        }
        return out;
    }

    if (sdk.isObject(val)) {
        const out = try sdk.createObject(handle);
        const keys = try sdk.objectKeys(handle, val);
        const len = sdk.arrayLength(keys) orelse return error.InvalidObjectKeys;
        var i: u32 = 0;
        while (i < len) : (i += 1) {
            const key_val = sdk.arrayGet(handle, keys, i) orelse continue;
            const key = sdk.extractString(key_val) orelse continue;
            const prop = sdk.objectGet(handle, val, key) orelse sdk.JSValue.undefined_val;
            try sdk.objectSet(handle, out, key, try cloneValueForDecode(handle, prop, depth + 1));
        }
        return out;
    }

    return val;
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

        // Reject duplicate field names rather than silently last-wins. A
        // last-wins default is differential-prone (HTTP parameter pollution):
        // an upstream proxy/WAF that takes first-wins would disagree with this
        // layer. Fail closed instead.
        if (sdk.objectGet(handle, obj, key) != null) return error.DuplicateField;

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
        error.DuplicateField => return sdk.resultErr(handle, "duplicate field name in multipart body"),
        // A >70-char boundary is attacker-controllable via Content-Type; it must
        // surface as a Result.err, not escape as an unhandled engine exception
        // from this critical Result-returning function.
        error.BoundaryTooLong => return sdk.resultErr(handle, "boundary too long"),
        else => return err,
    };
    return validate.decodeObject(handle, name, obj, true);
}

/// Extract the boundary token from a Content-Type header value.
/// Accepts: "multipart/form-data; boundary=<token>" (with or without quotes).
fn parseBoundary(content_type: []const u8) ?[]const u8 {
    // Walk the ';'-separated parameters with the quote-aware tokenizer rather
    // than a blind indexOf("boundary="): a literal "boundary=" can appear inside
    // an earlier quoted parameter value (e.g. `name="boundary=evil"; boundary=real`),
    // and a substring scan would latch onto that and misparse the real boundary
    // (a differential-parsing surface vs RFC 7231-compliant intermediaries).
    // The media-type itself cannot contain ';', so parameters start after the first one.
    const params_start = (std.mem.indexOfScalar(u8, content_type, ';') orelse return null) + 1;
    return parseDispositionParam(content_type[params_start..], "boundary");
}

/// Parse a multipart/form-data body into a JS object.
/// Each part maps its `name` disposition parameter to either a string (text
/// parts) or a file-descriptor object ({ filename, content, contentType }).
fn parseMultipartObject(handle: *sdk.ModuleHandle, body: []const u8, boundary: []const u8) !sdk.JSValue {
    const obj = try sdk.createObject(handle);

    // Build the delimiters (RFC 2046: boundary <= 70 chars). The opening
    // "dash-boundary" is "--<boundary>"; every subsequent delimiter is anchored
    // to a line boundary as "CRLF--<boundary>". Splitting on the CRLF-anchored
    // form (rather than the bare "--<boundary>" substring) prevents a part's
    // content that happens to contain the boundary token from splitting the
    // part mid-value.
    if (boundary.len > 70) return error.BoundaryTooLong;
    var delim_buf: [74]u8 = undefined;
    delim_buf[0] = '\r';
    delim_buf[1] = '\n';
    delim_buf[2] = '-';
    delim_buf[3] = '-';
    @memcpy(delim_buf[4 .. 4 + boundary.len], boundary);
    const crlf_delim = delim_buf[0 .. 4 + boundary.len];
    const dash_boundary = delim_buf[2 .. 4 + boundary.len];

    // Find the opening dash-boundary, then split the remainder on the
    // CRLF-anchored delimiter so in-content boundary substrings are ignored.
    const first = std.mem.indexOf(u8, body, dash_boundary) orelse return error.MalformedMultipart;
    const after_first = body[first + dash_boundary.len ..];

    var it = std.mem.splitSequence(u8, after_first, crlf_delim);

    while (it.next()) |raw_part| {
        // "--<boundary>--" is the closing delimiter; the part after the opening
        // dash-boundary (or after a CRLF-anchored delimiter) starts with "--"
        // for the terminator.
        if (std.mem.startsWith(u8, raw_part, "--")) break;

        // Each part starts with CRLF after the delimiter line.
        const part = if (std.mem.startsWith(u8, raw_part, "\r\n")) raw_part[2..] else raw_part;

        // Separate headers from body: blank line (CRLF CRLF).
        const header_end = std.mem.indexOf(u8, part, "\r\n\r\n") orelse return error.MalformedMultipart;
        const headers_raw = part[0..header_end];
        // The split delimiter is the CRLF-anchored "\r\n--<boundary>", so the
        // separator CRLF that precedes the next delimiter was already consumed by
        // splitSequence. Whatever trailing bytes remain belong to the body, so do
        // NOT strip a trailing CRLF here — that would corrupt payloads ending in
        // CRLF (Windows text files, binary blobs whose last bytes are 0x0D 0x0A).
        const part_body = part[header_end + 4 ..];

        // Parse Content-Disposition header.
        const disposition = findHeader(headers_raw, "content-disposition") orelse continue;
        const field_name = parseDispositionParam(disposition, "name") orelse continue;
        const filename = parseDispositionParam(disposition, "filename");
        const part_ct = findHeader(headers_raw, "content-type");

        // Reject duplicate field names rather than silently last-wins; a
        // last-wins default is differential-prone (parameter pollution).
        if (sdk.objectGet(handle, obj, field_name) != null) return error.DuplicateField;

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
            // Find the closing quote honoring backslash escapes, matching the
            // segment tokenizer above. A plain indexOfScalar stops at the first
            // \"-escaped quote and truncates the value (e.g. "a\"b.txt" -> "a\").
            var end: usize = 0;
            while (end < value.len) : (end += 1) {
                if (value[end] == '\\' and end + 1 < value.len) {
                    end += 1;
                    continue;
                }
                if (value[end] == '"') break;
            }
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

test "cloneValueForDecode rejects excessive nesting before touching SDK object APIs" {
    const fake_handle: *sdk.ModuleHandle = @ptrFromInt(8);
    try std.testing.expectError(
        error.DecodeInputTooDeep,
        cloneValueForDecode(fake_handle, sdk.JSValue.undefined_val, MAX_DECODE_CLONE_DEPTH + 1),
    );
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

test "parseBoundary ignores boundary= inside an earlier quoted parameter value" {
    const boundary = parseBoundary("multipart/form-data; name=\"boundary=evil\"; boundary=real_boundary");
    try std.testing.expect(boundary != null);
    try std.testing.expectEqualStrings("real_boundary", boundary.?);
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
