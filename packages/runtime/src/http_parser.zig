//! HTTP parsing helpers extracted from server.zig.
//! Shared by streaming and buffer-based request parsing paths.

const std = @import("std");
const http_types = @import("http_types.zig");
const HttpHeader = http_types.HttpHeader;
const QueryParam = http_types.QueryParam;

/// Comptime lookup table for O(1) ASCII lowercase conversion.
/// Eliminates branch-per-character overhead in header normalization.
const LowerTable = blk: {
    var t: [256]u8 = undefined;
    for (&t, 0..) |*b, i| {
        b.* = if (i >= 'A' and i <= 'Z') @intCast(i + 32) else @intCast(i);
    }
    break :blk t;
};

/// Fast lowercase string conversion using comptime lookup table.
/// Returns slice into dest buffer.
fn lowerStringFast(dest: []u8, src: []const u8) []u8 {
    const len = @min(dest.len, src.len);
    for (dest[0..len], src[0..len]) |*d, s| {
        d.* = LowerTable[s];
    }
    return dest[0..len];
}

/// SIMD-accelerated search for HTTP header terminator (\r\n\r\n).
/// Returns offset to start of terminator, or null if not found.
/// Uses 16-byte vector operations when buffer is large enough.
pub fn findHeaderEnd(buf: []const u8) ?usize {
    if (buf.len < 4) return null;

    const Vec = @Vector(16, u8);
    const cr: Vec = @splat('\r');
    const lf: Vec = @splat('\n');
    var i: usize = 0;

    const asMask = struct {
        fn toU16(v: @Vector(16, bool)) u16 {
            const bits: @Vector(16, u1) = @bitCast(v);
            return @bitCast(bits);
        }
    }.toU16;

    const load16 = struct {
        fn at(data: []const u8, offset: usize) Vec {
            return @as(*align(1) const [16]u8, @ptrCast(data.ptr + offset)).*;
        }
    }.at;

    // SIMD bitmask path: evaluate 16 candidate start positions per iteration.
    // A candidate start at byte k matches when:
    //   buf[k] == '\r' && buf[k+1] == '\n' && buf[k+2] == '\r' && buf[k+3] == '\n'
    // This uses overlapping loads and bitwise AND on 16-bit masks.
    while (i + 19 <= buf.len) : (i += 16) {
        const m0 = asMask(load16(buf, i + 0) == cr);
        const m1 = asMask(load16(buf, i + 1) == lf);
        const m2 = asMask(load16(buf, i + 2) == cr);
        const m3 = asMask(load16(buf, i + 3) == lf);
        const candidates = m0 & m1 & m2 & m3;
        if (candidates != 0) {
            return i + @ctz(candidates);
        }
    }

    // Scalar tail for remaining bytes (<16 start positions).
    var j = i;
    while (j + 4 <= buf.len) : (j += 1) {
        if (buf[j] == '\r' and
            buf[j + 1] == '\n' and
            buf[j + 2] == '\r' and
            buf[j + 3] == '\n')
        {
            return j;
        }
    }

    return null;
}

pub const RequestLine = struct {
    method: []const u8,
    url: []const u8,
    path: []const u8,
    query_string: []const u8,
};

pub fn parseRequestLine(storage: []u8, offset: *usize, request_line: []const u8) !RequestLine {
    var parts = std.mem.splitScalar(u8, request_line, ' ');
    const method_slice = parts.next() orelse return error.InvalidRequest;
    const url_slice = parts.next() orelse return error.InvalidRequest;

    const method = try copyToStorage(storage, offset, method_slice);
    const url = try copyToStorage(storage, offset, url_slice);

    const query_start = std.mem.indexOf(u8, url, "?");
    const path = if (query_start) |idx| url[0..idx] else url;
    const query_string = if (query_start) |idx| url[idx + 1 ..] else "";

    return .{
        .method = method,
        .url = url,
        .path = path,
        .query_string = query_string,
    };
}

pub fn parseRequestLineBorrowed(request_line: []const u8) !RequestLine {
    var parts = std.mem.splitScalar(u8, request_line, ' ');
    const method = parts.next() orelse return error.InvalidRequest;
    const url = parts.next() orelse return error.InvalidRequest;

    const query_start = std.mem.indexOf(u8, url, "?");
    const path = if (query_start) |idx| url[0..idx] else url;
    const query_string = if (query_start) |idx| url[idx + 1 ..] else "";

    return .{
        .method = method,
        .url = url,
        .path = path,
        .query_string = query_string,
    };
}

pub fn parseHeadersFromLines(
    allocator: std.mem.Allocator,
    max_headers: usize,
    storage: []u8,
    offset: *usize,
    headers: *std.ArrayListUnmanaged(HttpHeader),
    fast_slots: *FastHeaderSlots,
    line_source: anytype,
) !void {
    var header_count: usize = 0;
    while (try line_source.next()) |line| {
        if (line.len == 0) break;
        if (header_count >= max_headers) return error.TooManyHeaders;
        try processHeaderLine(line, storage, offset, headers, allocator, fast_slots);
        header_count += 1;
    }
}

pub fn parseHeadersFromLinesBorrowed(
    allocator: std.mem.Allocator,
    max_headers: usize,
    headers: *std.ArrayListUnmanaged(HttpHeader),
    fast_slots: *FastHeaderSlots,
    line_source: anytype,
) !void {
    var header_count: usize = 0;
    while (try line_source.next()) |line| {
        if (line.len == 0) break;
        if (header_count >= max_headers) return error.TooManyHeaders;
        try processHeaderLineBorrowed(line, headers, allocator, fast_slots);
        header_count += 1;
    }
}

/// Copy a string into a pre-allocated batch storage buffer.
/// Returns a slice into the storage buffer.
fn copyToStorage(storage: []u8, offset: *usize, src: []const u8) ![]const u8 {
    if (offset.* + src.len > storage.len) return error.HeaderStorageExhausted;
    const dest = storage[offset.*..][0..src.len];
    @memcpy(dest, src);
    offset.* += src.len;
    return dest;
}

/// Result of query parameter parsing.
pub const QueryParseResult = struct {
    storage: ?[]QueryParam,
    params: []const QueryParam,
    /// Backing buffer for percent-decoded key/value strings.
    decoded_storage: ?[]u8,
};

/// Decode percent-encoded bytes and '+' (as space) from `src` into `dest`.
/// Returns the slice of `dest` actually written.
fn percentDecode(dest: []u8, src: []const u8) []u8 {
    var di: usize = 0;
    var si: usize = 0;
    while (si < src.len) {
        if (src[si] == '+') {
            dest[di] = ' ';
            di += 1;
            si += 1;
        } else if (src[si] == '%' and si + 2 < src.len) {
            const hi = hexVal(src[si + 1]);
            const lo = hexVal(src[si + 2]);
            if (hi != null and lo != null) {
                dest[di] = (@as(u8, hi.?) << 4) | @as(u8, lo.?);
                di += 1;
                si += 3;
            } else {
                dest[di] = src[si];
                di += 1;
                si += 1;
            }
        } else {
            dest[di] = src[si];
            di += 1;
            si += 1;
        }
    }
    return dest[0..di];
}

fn hexVal(c: u8) ?u4 {
    return switch (c) {
        '0'...'9' => @intCast(c - '0'),
        'a'...'f' => @intCast(c - 'a' + 10),
        'A'...'F' => @intCast(c - 'A' + 10),
        else => null,
    };
}

/// Parse query parameters from a query string (the part after '?').
/// Allocates storage for parameters; caller owns the returned storage.
pub fn parseQueryString(allocator: std.mem.Allocator, query_string: []const u8) !QueryParseResult {
    if (query_string.len == 0) return .{ .storage = null, .params = &.{}, .decoded_storage = null };

    // Count parameters first
    var param_count: usize = 1;
    for (query_string) |c| {
        if (c == '&') param_count += 1;
    }

    const qps = try allocator.alloc(QueryParam, param_count);
    errdefer allocator.free(qps);

    // Decoded strings are always <= original length; one buffer suffices.
    const decode_buf = try allocator.alloc(u8, query_string.len);
    errdefer allocator.free(decode_buf);
    var decode_offset: usize = 0;

    var qp_idx: usize = 0;
    var pairs = std.mem.splitScalar(u8, query_string, '&');
    while (pairs.next()) |pair| {
        if (std.mem.indexOf(u8, pair, "=")) |eq_idx| {
            const raw_key = pair[0..eq_idx];
            const raw_val = pair[eq_idx + 1 ..];

            const key_dest = decode_buf[decode_offset..];
            const decoded_key = percentDecode(key_dest, raw_key);
            decode_offset += decoded_key.len;

            const val_dest = decode_buf[decode_offset..];
            const decoded_val = percentDecode(val_dest, raw_val);
            decode_offset += decoded_val.len;

            qps[qp_idx] = .{
                .key = decoded_key,
                .value = decoded_val,
            };
            qp_idx += 1;
        }
    }
    return .{ .storage = qps, .params = qps[0..qp_idx], .decoded_storage = decode_buf };
}

/// Fast header slots populated during parsing to avoid O(n) lookups later.
pub const FastHeaderSlots = struct {
    connection: ?[]const u8 = null,
    content_length: ?usize = null,
    content_type: ?[]const u8 = null,
    has_chunked_encoding: bool = false,
};

/// Process a single header line: normalize key to lowercase, copy into storage,
/// append to header list, and update fast slots.
fn processHeaderLine(
    line: []const u8,
    storage: []u8,
    offset: *usize,
    headers: *std.ArrayListUnmanaged(HttpHeader),
    allocator: std.mem.Allocator,
    fast_slots: *FastHeaderSlots,
) !void {
    const header = splitHeaderLine(line) orelse return;
    const key = header.key;
    const value = header.value;

    // Normalize key to lowercase using comptime lookup table
    var key_lower_buf: [256]u8 = undefined;
    if (key.len > key_lower_buf.len) return error.HeaderKeyTooLong;
    const key_lower = lowerStringFast(key_lower_buf[0..key.len], key);
    const key_dup = try copyToStorage(storage, offset, key_lower);
    const value_dup = try copyToStorage(storage, offset, value);
    try headers.append(allocator, .{ .key = key_dup, .value = value_dup });

    // Populate fast header slots during parsing
    if (std.mem.eql(u8, key_lower, "content-length")) {
        const parsed = try parseContentLengthValue(value);
        if (fast_slots.content_length) |existing| {
            if (existing != parsed) return error.DuplicateContentLength;
        } else {
            fast_slots.content_length = parsed;
        }
    } else if (std.mem.eql(u8, key_lower, "connection")) {
        fast_slots.connection = value_dup;
    } else if (std.mem.eql(u8, key_lower, "content-type")) {
        fast_slots.content_type = value_dup;
    } else if (std.mem.eql(u8, key_lower, "transfer-encoding")) {
        if (std.ascii.indexOfIgnoreCase(value, "chunked") != null) {
            fast_slots.has_chunked_encoding = true;
        }
    }
}

/// Process a single header line without copying key/value slices.
fn processHeaderLineBorrowed(
    line: []const u8,
    headers: *std.ArrayListUnmanaged(HttpHeader),
    allocator: std.mem.Allocator,
    fast_slots: *FastHeaderSlots,
) !void {
    const header = splitHeaderLine(line) orelse return;
    const key = header.key;
    const value = header.value;
    try headers.append(allocator, .{ .key = key, .value = value });

    if (std.ascii.eqlIgnoreCase(key, "content-length")) {
        const parsed = try parseContentLengthValue(value);
        if (fast_slots.content_length) |existing| {
            if (existing != parsed) return error.DuplicateContentLength;
        } else {
            fast_slots.content_length = parsed;
        }
    } else if (std.ascii.eqlIgnoreCase(key, "connection")) {
        fast_slots.connection = value;
    } else if (std.ascii.eqlIgnoreCase(key, "content-type")) {
        fast_slots.content_type = value;
    } else if (std.ascii.eqlIgnoreCase(key, "transfer-encoding")) {
        if (std.ascii.indexOfIgnoreCase(value, "chunked") != null) {
            fast_slots.has_chunked_encoding = true;
        }
    }
}

pub fn splitHeaderLine(line: []const u8) ?struct { key: []const u8, value: []const u8 } {
    const idx = std.mem.indexOfScalar(u8, line, ':') orelse return null;
    const key = line[0..idx];
    const value = std.mem.trimStart(u8, line[idx + 1 ..], " \t");
    return .{ .key = key, .value = value };
}

pub fn parseContentLengthValue(value: []const u8) !usize {
    const trimmed = std.mem.trim(u8, value, " \t");
    if (trimmed.len == 0) return error.InvalidContentLength;
    for (trimmed) |c| {
        if (c < '0' or c > '9') return error.InvalidContentLength;
    }
    return std.fmt.parseInt(usize, trimmed, 10) catch return error.InvalidContentLength;
}

pub fn parseContentLength(header_section: []const u8) !?usize {
    var lines = std.mem.splitSequence(u8, header_section, "\r\n");
    _ = lines.next() orelse return null; // request line
    var found: ?usize = null;
    while (lines.next()) |line| {
        if (line.len == 0) break;
        const header = splitHeaderLine(line) orelse continue;
        if (std.ascii.eqlIgnoreCase(header.key, "content-length")) {
            const parsed = try parseContentLengthValue(header.value);
            if (found) |existing| {
                if (existing != parsed) return error.DuplicateContentLength;
            } else {
                found = parsed;
            }
        }
    }
    return found;
}

/// Check if headers contain Transfer-Encoding: chunked
pub fn hasTransferEncodingChunked(header_section: []const u8) bool {
    var lines = std.mem.splitSequence(u8, header_section, "\r\n");
    _ = lines.next() orelse return false; // skip request line
    while (lines.next()) |line| {
        if (line.len == 0) break;
        const header = splitHeaderLine(line) orelse continue;
        if (std.ascii.eqlIgnoreCase(header.key, "transfer-encoding")) {
            const val = std.mem.trim(u8, header.value, " \t");
            if (std.ascii.indexOfIgnoreCase(val, "chunked") != null) return true;
        }
    }
    return false;
}

const testing = std.testing;

test "findHeaderEnd: terminator at start of small buffer" {
    try testing.expectEqual(@as(?usize, 0), findHeaderEnd("\r\n\r\n"));
    try testing.expectEqual(@as(?usize, 0), findHeaderEnd("\r\n\r\nbody"));
}

test "findHeaderEnd: scalar tail path (buffer too small for SIMD)" {
    // Buffer < 19 bytes goes through the scalar-only loop.
    try testing.expectEqual(@as(?usize, 3), findHeaderEnd("GET\r\n\r\n"));
    try testing.expectEqual(@as(?usize, null), findHeaderEnd("no terminator"));
    try testing.expectEqual(@as(?usize, null), findHeaderEnd(""));
    try testing.expectEqual(@as(?usize, null), findHeaderEnd("abc")); // <4 early null
}

test "findHeaderEnd: SIMD path locates terminator" {
    // 32+ bytes forces at least one SIMD iteration.
    const buf = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\nbody after";
    // Compute expected offset: position of \r\n\r\n.
    const expected = std.mem.indexOf(u8, buf, "\r\n\r\n").?;
    try testing.expectEqual(@as(?usize, expected), findHeaderEnd(buf));
}

test "findHeaderEnd: terminator straddling SIMD/scalar boundary" {
    // Build a buffer where \r\n\r\n starts in the last few bytes of the SIMD
    // window (i.e. between 16 and 19 of remaining), forcing the scalar tail.
    var buf: [40]u8 = undefined;
    @memset(&buf, 'X');
    // Place terminator at offset 14 - inside first SIMD chunk's tail span.
    @memcpy(buf[14..18], "\r\n\r\n");
    try testing.expectEqual(@as(?usize, 14), findHeaderEnd(&buf));
}

test "parseRequestLine: GET with query string" {
    var storage: [256]u8 = undefined;
    var offset: usize = 0;
    const line = try parseRequestLine(&storage, &offset, "GET /api/v1/users?id=42 HTTP/1.1");
    try testing.expectEqualStrings("GET", line.method);
    try testing.expectEqualStrings("/api/v1/users?id=42", line.url);
    try testing.expectEqualStrings("/api/v1/users", line.path);
    try testing.expectEqualStrings("id=42", line.query_string);
}

test "parseRequestLine: no query string yields empty query" {
    var storage: [256]u8 = undefined;
    var offset: usize = 0;
    const line = try parseRequestLine(&storage, &offset, "POST /submit HTTP/1.1");
    try testing.expectEqualStrings("POST", line.method);
    try testing.expectEqualStrings("/submit", line.path);
    try testing.expectEqualStrings("", line.query_string);
}

test "parseRequestLine: storage exhaustion errors" {
    var storage: [4]u8 = undefined; // too small for "GET" + "/"
    var offset: usize = 0;
    try testing.expectError(error.HeaderStorageExhausted, parseRequestLine(&storage, &offset, "GET /path HTTP/1.1"));
}

test "parseRequestLine: malformed line missing url" {
    var storage: [128]u8 = undefined;
    var offset: usize = 0;
    try testing.expectError(error.InvalidRequest, parseRequestLine(&storage, &offset, "GET"));
}

test "parseRequestLineBorrowed: path and query slices point into input" {
    const input = "DELETE /resource?force=true HTTP/1.1";
    const line = try parseRequestLineBorrowed(input);
    try testing.expectEqualStrings("DELETE", line.method);
    try testing.expectEqualStrings("/resource?force=true", line.url);
    try testing.expectEqualStrings("/resource", line.path);
    try testing.expectEqualStrings("force=true", line.query_string);
    // Borrowed slices alias the input buffer.
    try testing.expect(line.method.ptr == input.ptr);
}

test "parseQueryString: empty input returns empty params, no storage" {
    const result = try parseQueryString(testing.allocator, "");
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqual(@as(usize, 0), result.params.len);
    try testing.expect(result.storage == null);
}

test "parseQueryString: simple key=value pairs" {
    const result = try parseQueryString(testing.allocator, "a=1&b=2&c=3");
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqual(@as(usize, 3), result.params.len);
    try testing.expectEqualStrings("a", result.params[0].key);
    try testing.expectEqualStrings("1", result.params[0].value);
    try testing.expectEqualStrings("c", result.params[2].key);
    try testing.expectEqualStrings("3", result.params[2].value);
}

test "parseQueryString: percent-decoding" {
    const result = try parseQueryString(testing.allocator, "name=John%20Doe&email=a%40b.com");
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqual(@as(usize, 2), result.params.len);
    try testing.expectEqualStrings("John Doe", result.params[0].value);
    try testing.expectEqualStrings("a@b.com", result.params[1].value);
}

test "parseQueryString: plus is decoded as space" {
    const result = try parseQueryString(testing.allocator, "q=hello+world");
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqualStrings("hello world", result.params[0].value);
}

test "parseQueryString: malformed percent-escape passes through" {
    // %XY is not valid hex; per the implementation, the literal '%' is kept.
    const result = try parseQueryString(testing.allocator, "k=%XY");
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqualStrings("%XY", result.params[0].value);
}

test "parseQueryString: pairs without '=' are skipped" {
    const result = try parseQueryString(testing.allocator, "a=1&orphan&b=2");
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqual(@as(usize, 2), result.params.len);
    try testing.expectEqualStrings("a", result.params[0].key);
    try testing.expectEqualStrings("b", result.params[1].key);
}

test "splitHeaderLine: key-value with single space" {
    const h = splitHeaderLine("Content-Type: application/json").?;
    try testing.expectEqualStrings("Content-Type", h.key);
    try testing.expectEqualStrings("application/json", h.value);
}

test "splitHeaderLine: trims leading whitespace from value" {
    const h = splitHeaderLine("X-Pad:\t  value").?;
    try testing.expectEqualStrings("X-Pad", h.key);
    try testing.expectEqualStrings("value", h.value);
}

test "splitHeaderLine: no colon returns null" {
    try testing.expect(splitHeaderLine("not a header") == null);
}

test "splitHeaderLine: empty value after colon" {
    const h = splitHeaderLine("X-Empty:").?;
    try testing.expectEqualStrings("X-Empty", h.key);
    try testing.expectEqualStrings("", h.value);
}

test "parseContentLengthValue: valid digits" {
    try testing.expectEqual(@as(usize, 0), try parseContentLengthValue("0"));
    try testing.expectEqual(@as(usize, 1024), try parseContentLengthValue("1024"));
    try testing.expectEqual(@as(usize, 42), try parseContentLengthValue("  42  "));
}

test "parseContentLengthValue: rejects non-digits, empty, signed" {
    try testing.expectError(error.InvalidContentLength, parseContentLengthValue(""));
    try testing.expectError(error.InvalidContentLength, parseContentLengthValue("   "));
    try testing.expectError(error.InvalidContentLength, parseContentLengthValue("12a"));
    try testing.expectError(error.InvalidContentLength, parseContentLengthValue("-5"));
    try testing.expectError(error.InvalidContentLength, parseContentLengthValue("+5"));
}

test "parseContentLength: extracts from header section" {
    const headers = "POST / HTTP/1.1\r\nContent-Length: 17\r\nHost: x\r\n\r\n";
    try testing.expectEqual(@as(?usize, 17), try parseContentLength(headers));
}

test "parseContentLength: missing returns null" {
    const headers = "GET / HTTP/1.1\r\nHost: x\r\n\r\n";
    try testing.expectEqual(@as(?usize, null), try parseContentLength(headers));
}

test "parseContentLength: case-insensitive header key" {
    const headers = "POST / HTTP/1.1\r\ncontent-length: 9\r\n\r\n";
    try testing.expectEqual(@as(?usize, 9), try parseContentLength(headers));
}

test "parseContentLength: duplicate same value is allowed" {
    const headers = "POST / HTTP/1.1\r\nContent-Length: 5\r\nContent-Length: 5\r\n\r\n";
    try testing.expectEqual(@as(?usize, 5), try parseContentLength(headers));
}

test "parseContentLength: duplicate different values errors" {
    const headers = "POST / HTTP/1.1\r\nContent-Length: 5\r\nContent-Length: 7\r\n\r\n";
    try testing.expectError(error.DuplicateContentLength, parseContentLength(headers));
}

test "hasTransferEncodingChunked: chunked detected" {
    const headers = "POST / HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n";
    try testing.expect(hasTransferEncodingChunked(headers));
}

test "hasTransferEncodingChunked: case-insensitive value" {
    const headers = "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n";
    try testing.expect(hasTransferEncodingChunked(headers));
}

test "hasTransferEncodingChunked: comma-list containing chunked" {
    const headers = "POST / HTTP/1.1\r\nTransfer-Encoding: gzip, chunked\r\n\r\n";
    try testing.expect(hasTransferEncodingChunked(headers));
}

test "hasTransferEncodingChunked: absent or other encoding returns false" {
    try testing.expect(!hasTransferEncodingChunked("GET / HTTP/1.1\r\nHost: x\r\n\r\n"));
    try testing.expect(!hasTransferEncodingChunked("POST / HTTP/1.1\r\nTransfer-Encoding: gzip\r\n\r\n"));
}
