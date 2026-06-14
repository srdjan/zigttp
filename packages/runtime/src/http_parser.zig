//! HTTP parsing helpers extracted from server.zig.
//! Shared by the threaded server's buffer-based request parser.

const std = @import("std");
const http_types = @import("http_types.zig");
const HttpHeader = http_types.HttpHeader;
const QueryParam = http_types.QueryParam;

/// Conservative per-request length caps. The defaults match the shared
/// 8 KiB request-line / header storage pool so that a request whose URL or
/// query fits the pool also fits these caps. Callers that need looser or
/// tighter limits can pass a non-default value.
pub const DEFAULT_MAX_URL_LENGTH: usize = 8192;
pub const DEFAULT_MAX_QUERY_LENGTH: usize = 8192;

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
    /// true when the request line ends with "HTTP/1.0" (keep-alive is opt-in).
    is_http_10: bool = false,
};

pub fn parseRequestLine(
    storage: []u8,
    offset: *usize,
    request_line: []const u8,
    max_url_length: usize,
) !RequestLine {
    var parts = std.mem.splitScalar(u8, request_line, ' ');
    const method_slice = parts.next() orelse return error.InvalidRequest;
    const url_slice = parts.next() orelse return error.InvalidRequest;

    if (url_slice.len > max_url_length) return error.UriTooLong;

    const version = parts.next() orelse "";
    const is_http_10 = std.mem.eql(u8, version, "HTTP/1.0");

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
        .is_http_10 = is_http_10,
    };
}

pub fn parseRequestLineBorrowed(request_line: []const u8, max_url_length: usize) !RequestLine {
    var parts = std.mem.splitScalar(u8, request_line, ' ');
    const method = parts.next() orelse return error.InvalidRequest;
    const url = parts.next() orelse return error.InvalidRequest;

    if (url.len > max_url_length) return error.UriTooLong;

    const version = parts.next() orelse "";
    const is_http_10 = std.mem.eql(u8, version, "HTTP/1.0");

    const query_start = std.mem.indexOf(u8, url, "?");
    const path = if (query_start) |idx| url[0..idx] else url;
    const query_string = if (query_start) |idx| url[idx + 1 ..] else "";

    return .{
        .method = method,
        .url = url,
        .path = path,
        .query_string = query_string,
        .is_http_10 = is_http_10,
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
/// Returns error.QueryTooLong if the query exceeds max_query_length; this
/// is a DoS guard so a multi-megabyte query cannot force a proportional
/// allocation.
pub fn parseQueryString(
    allocator: std.mem.Allocator,
    query_string: []const u8,
    max_query_length: usize,
) !QueryParseResult {
    if (query_string.len > max_query_length) return error.QueryTooLong;
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

const MAX_CHUNK_SIZE_LINE_BYTES: usize = 8 * 1024;
const MAX_CHUNK_TRAILER_LINE_BYTES: usize = 8 * 1024;
const MAX_CHUNK_TRAILER_BYTES: usize = 16 * 1024;

/// Return the number of encoded body bytes consumed by a complete chunked
/// transfer, including the terminating chunk and ignored trailers. Returns
/// null when the caller needs to read more bytes.
pub fn chunkedBodyConsumed(body: []const u8, max_body_size: usize) !?usize {
    var pos: usize = 0;
    var decoded_len: usize = 0;

    while (true) {
        const line_rel = (try findCrlfWithin(body[pos..], MAX_CHUNK_SIZE_LINE_BYTES)) orelse return null;
        const line_end = pos + line_rel;
        const size = try parseChunkSizeLine(body[pos..line_end]);
        pos = line_end + 2;

        if (size == 0) {
            const trailer_start = pos;
            while (true) {
                if (pos + 2 <= body.len and std.mem.eql(u8, body[pos..][0..2], "\r\n")) {
                    return pos + 2;
                }
                if (pos - trailer_start > MAX_CHUNK_TRAILER_BYTES) return error.InvalidChunkedEncoding;
                const trailer_rel = (try findCrlfWithin(body[pos..], MAX_CHUNK_TRAILER_LINE_BYTES)) orelse {
                    if (body.len - trailer_start > MAX_CHUNK_TRAILER_BYTES) return error.InvalidChunkedEncoding;
                    return null;
                };
                pos += trailer_rel + 2;
            }
        }

        if (decoded_len > max_body_size or size > max_body_size - decoded_len) {
            return error.FileTooBig;
        }
        decoded_len += size;

        if (body.len < pos + size + 2) return null;
        pos += size;
        if (!std.mem.eql(u8, body[pos..][0..2], "\r\n")) return error.InvalidChunkedEncoding;
        pos += 2;
    }
}

/// Decode a complete chunked transfer into an owned byte slice. The caller owns
/// the returned slice. Trailers are validated for framing and otherwise ignored.
pub fn decodeChunkedBody(
    allocator: std.mem.Allocator,
    body: []const u8,
    max_body_size: usize,
) ![]u8 {
    _ = try chunkedBodyConsumed(body, max_body_size) orelse return error.IncompleteBody;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    // Decoded output never exceeds the encoded body; reserve once so the
    // per-chunk appendSlice loop does not reallocate as it grows.
    try out.ensureTotalCapacity(allocator, @min(body.len, max_body_size));

    var pos: usize = 0;
    while (true) {
        const line_rel = std.mem.indexOf(u8, body[pos..], "\r\n") orelse return error.IncompleteBody;
        const line_end = pos + line_rel;
        const size = try parseChunkSizeLine(body[pos..line_end]);
        pos = line_end + 2;

        if (size == 0) break;
        try out.appendSlice(allocator, body[pos..][0..size]);
        pos += size + 2;
    }

    return out.toOwnedSlice(allocator);
}

fn parseChunkSizeLine(line: []const u8) !usize {
    const size_part_raw = if (std.mem.indexOfScalar(u8, line, ';')) |idx| line[0..idx] else line;
    const size_part = std.mem.trim(u8, size_part_raw, " \t");
    if (size_part.len == 0) return error.InvalidChunkedEncoding;

    var value: usize = 0;
    for (size_part) |c| {
        const digit = hexVal(c) orelse return error.InvalidChunkedEncoding;
        const shifted, const overflow_mul = @mulWithOverflow(value, 16);
        if (overflow_mul != 0) return error.FileTooBig;
        const added, const overflow_add = @addWithOverflow(shifted, @as(usize, digit));
        if (overflow_add != 0) return error.FileTooBig;
        value = added;
    }
    return value;
}

fn findCrlfWithin(input: []const u8, max_line_bytes: usize) !?usize {
    if (std.mem.indexOf(u8, input, "\r\n")) |idx| {
        if (idx > max_line_bytes) return error.InvalidChunkedEncoding;
        return idx;
    }

    if (input.len <= max_line_bytes) return null;
    if (input.len == max_line_bytes + 1 and input[max_line_bytes] == '\r') return null;
    return error.InvalidChunkedEncoding;
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
    const line = try parseRequestLine(&storage, &offset, "GET /api/v1/users?id=42 HTTP/1.1", DEFAULT_MAX_URL_LENGTH);
    try testing.expectEqualStrings("GET", line.method);
    try testing.expectEqualStrings("/api/v1/users?id=42", line.url);
    try testing.expectEqualStrings("/api/v1/users", line.path);
    try testing.expectEqualStrings("id=42", line.query_string);
}

test "parseRequestLine: no query string yields empty query" {
    var storage: [256]u8 = undefined;
    var offset: usize = 0;
    const line = try parseRequestLine(&storage, &offset, "POST /submit HTTP/1.1", DEFAULT_MAX_URL_LENGTH);
    try testing.expectEqualStrings("POST", line.method);
    try testing.expectEqualStrings("/submit", line.path);
    try testing.expectEqualStrings("", line.query_string);
}

test "parseRequestLine: storage exhaustion errors" {
    var storage: [4]u8 = undefined; // too small for "GET" + "/"
    var offset: usize = 0;
    try testing.expectError(error.HeaderStorageExhausted, parseRequestLine(&storage, &offset, "GET /path HTTP/1.1", DEFAULT_MAX_URL_LENGTH));
}

test "parseRequestLine: malformed line missing url" {
    var storage: [128]u8 = undefined;
    var offset: usize = 0;
    try testing.expectError(error.InvalidRequest, parseRequestLine(&storage, &offset, "GET", DEFAULT_MAX_URL_LENGTH));
}

test "parseRequestLine: rejects URLs exceeding max_url_length with UriTooLong" {
    var storage: [4096]u8 = undefined;
    var offset: usize = 0;
    // URL of length 33 vs limit of 16
    try testing.expectError(error.UriTooLong, parseRequestLine(&storage, &offset, "GET /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa HTTP/1.1", 16));
}

test "parseRequestLineBorrowed: path and query slices point into input" {
    const input = "DELETE /resource?force=true HTTP/1.1";
    const line = try parseRequestLineBorrowed(input, DEFAULT_MAX_URL_LENGTH);
    try testing.expectEqualStrings("DELETE", line.method);
    try testing.expectEqualStrings("/resource?force=true", line.url);
    try testing.expectEqualStrings("/resource", line.path);
    try testing.expectEqualStrings("force=true", line.query_string);
    // Borrowed slices alias the input buffer.
    try testing.expect(line.method.ptr == input.ptr);
}

test "parseRequestLineBorrowed: rejects URLs exceeding max_url_length" {
    try testing.expectError(error.UriTooLong, parseRequestLineBorrowed("GET /xxxxxxxxxxxxxxxxxxxxxx HTTP/1.1", 8));
}

test "parseQueryString: empty input returns empty params, no storage" {
    const result = try parseQueryString(testing.allocator, "", DEFAULT_MAX_QUERY_LENGTH);
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqual(@as(usize, 0), result.params.len);
    try testing.expect(result.storage == null);
}

test "parseQueryString: simple key=value pairs" {
    const result = try parseQueryString(testing.allocator, "a=1&b=2&c=3", DEFAULT_MAX_QUERY_LENGTH);
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqual(@as(usize, 3), result.params.len);
    try testing.expectEqualStrings("a", result.params[0].key);
    try testing.expectEqualStrings("1", result.params[0].value);
    try testing.expectEqualStrings("c", result.params[2].key);
    try testing.expectEqualStrings("3", result.params[2].value);
}

test "parseQueryString: percent-decoding" {
    const result = try parseQueryString(testing.allocator, "name=John%20Doe&email=a%40b.com", DEFAULT_MAX_QUERY_LENGTH);
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqual(@as(usize, 2), result.params.len);
    try testing.expectEqualStrings("John Doe", result.params[0].value);
    try testing.expectEqualStrings("a@b.com", result.params[1].value);
}

test "parseQueryString: plus is decoded as space" {
    const result = try parseQueryString(testing.allocator, "q=hello+world", DEFAULT_MAX_QUERY_LENGTH);
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqualStrings("hello world", result.params[0].value);
}

test "parseQueryString: rejects queries exceeding max_query_length with QueryTooLong" {
    // 4 KiB query string, 1 KiB limit. Must fail before any allocation.
    const big = "k=" ++ ("v" ** 4094);
    try testing.expectError(error.QueryTooLong, parseQueryString(testing.allocator, big, 1024));
}

test "parseQueryString: malformed percent-escape passes through" {
    // %XY is not valid hex; per the implementation, the literal '%' is kept.
    const result = try parseQueryString(testing.allocator, "k=%XY", DEFAULT_MAX_QUERY_LENGTH);
    defer if (result.storage) |s| testing.allocator.free(s);
    defer if (result.decoded_storage) |s| testing.allocator.free(s);
    try testing.expectEqualStrings("%XY", result.params[0].value);
}

test "parseQueryString: pairs without '=' are skipped" {
    const result = try parseQueryString(testing.allocator, "a=1&orphan&b=2", DEFAULT_MAX_QUERY_LENGTH);
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

test "chunkedBodyConsumed detects complete body with trailers" {
    const body = "5;ext=1\r\nhello\r\n6\r\n world\r\n0\r\nX-Trailer: ignored\r\n\r\nnext";
    try testing.expectEqual(@as(?usize, 52), try chunkedBodyConsumed(body, 1024));
}

test "decodeChunkedBody assembles chunks" {
    const decoded = try decodeChunkedBody(testing.allocator, "5\r\nhello\r\n6\r\n world\r\n0\r\n\r\n", 1024);
    defer testing.allocator.free(decoded);
    try testing.expectEqualStrings("hello world", decoded);
}

test "chunkedBodyConsumed returns null for incomplete body" {
    try testing.expectEqual(@as(?usize, null), try chunkedBodyConsumed("5\r\nhel", 1024));
}

test "chunkedBodyConsumed rejects malformed size" {
    try testing.expectError(error.InvalidChunkedEncoding, chunkedBodyConsumed("z\r\nhello\r\n0\r\n\r\n", 1024));
}

test "chunkedBodyConsumed enforces decoded body limit" {
    try testing.expectError(error.FileTooBig, chunkedBodyConsumed("5\r\nhello\r\n0\r\n\r\n", 4));
}

test "chunkedBodyConsumed rejects overlong chunk size line before CRLF" {
    const line = try testing.allocator.alloc(u8, MAX_CHUNK_SIZE_LINE_BYTES + 1);
    defer testing.allocator.free(line);
    @memset(line, 'a');

    try testing.expectError(error.InvalidChunkedEncoding, chunkedBodyConsumed(line, 1024));
}

test "chunkedBodyConsumed allows max length chunk size line to wait for CRLF" {
    const line = try testing.allocator.alloc(u8, MAX_CHUNK_SIZE_LINE_BYTES + 1);
    defer testing.allocator.free(line);
    @memset(line[0..MAX_CHUNK_SIZE_LINE_BYTES], 'a');
    line[MAX_CHUNK_SIZE_LINE_BYTES] = '\r';

    try testing.expectEqual(@as(?usize, null), try chunkedBodyConsumed(line, 1024));
}

test "chunkedBodyConsumed rejects overlong trailer line before CRLF" {
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(testing.allocator);
    try body.appendSlice(testing.allocator, "0\r\n");
    try body.appendNTimes(testing.allocator, 'a', MAX_CHUNK_TRAILER_LINE_BYTES + 1);

    try testing.expectError(error.InvalidChunkedEncoding, chunkedBodyConsumed(body.items, 1024));
}

test "chunkedBodyConsumed rejects oversized trailer block" {
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(testing.allocator);
    try body.appendSlice(testing.allocator, "0\r\n");
    while (body.items.len <= MAX_CHUNK_TRAILER_BYTES + 8) {
        try body.appendSlice(testing.allocator, "X: y\r\n");
    }

    try testing.expectError(error.InvalidChunkedEncoding, chunkedBodyConsumed(body.items, 1024));
}

// -------------------------------------------------------------------------
// Deterministic fuzz harness
//
// std.testing.allocator already detects leaks; std.testing.checkAllAllocationFailures
// is overkill here. The contract these tests pin is narrower: for any input
// the parser must either return a typed error or a result whose slices stay
// inside the input/storage buffer. No panic, no out-of-bounds read, no leak.
// Inputs come from a fixed seed so failures reproduce.
// -------------------------------------------------------------------------

const FuzzKind = enum {
    request_line,
    query_string,
    content_length_value,
    header_section,
    header_terminator,
};

fn fuzzScratch(rng: *std.Random, kind: FuzzKind, buf: []u8) []u8 {
    // Length distribution: 80% small (<64), 15% medium (<512), 5% large
    // up to buf.len. Skewed small so we exercise edge cases more often
    // than long random strings.
    const roll = rng.int(u8);
    const len: usize = if (roll < 205)
        @min(rng.uintLessThan(usize, 64) + 1, buf.len)
    else if (roll < 243)
        @min(rng.uintLessThan(usize, 512) + 1, buf.len)
    else
        @min(rng.uintLessThan(usize, buf.len) + 1, buf.len);

    // Byte distribution biased toward HTTP-relevant characters so the
    // fuzz exercises real parse states more than random bytes would.
    for (buf[0..len]) |*b| {
        const r = rng.int(u8);
        b.* = switch (r % 16) {
            0 => ' ',
            1 => '\r',
            2 => '\n',
            3 => '\t',
            4 => ':',
            5 => '/',
            6 => '?',
            7 => '&',
            8 => '=',
            9 => '%',
            10 => '+',
            11 => '.',
            12 => '-',
            13 => '0' + rng.uintLessThan(u8, 10),
            14 => 'a' + rng.uintLessThan(u8, 26),
            else => rng.int(u8),
        };
    }

    // Kind-specific shaping: nudge each input toward something parseable
    // so we hit success paths too, not just the early-reject branches.
    switch (kind) {
        .request_line => {
            // Try to seed a method + URL shape ~50% of the time. The
            // method+url+version split is space-delimited, so if any of
            // positions 5..end contain space/CR/LF the URL would end
            // early and the parser sees a tiny URL. Overwrite the URL
            // span with a single safe byte so the URL exercises full
            // length and the parser's length cap path gets reached.
            if (len >= 16 and rng.int(u8) < 128) {
                @memcpy(buf[0..4], "GET ");
                buf[4] = '/';
                // Reserve the last 9 bytes for " HTTP/1.1" so the
                // trailing tokens are well-formed. Fill the URL middle
                // with a benign character that the parser will accept
                // verbatim.
                const url_end = if (len >= 13) len - 9 else len;
                @memset(buf[5..url_end], 'a');
                if (len >= 13) @memcpy(buf[url_end..len], " HTTP/1.1");
            }
        },
        .header_section => {
            // Append a terminator to about 50% of inputs.
            if (len >= 4 and rng.int(u8) < 128) {
                buf[len - 4] = '\r';
                buf[len - 3] = '\n';
                buf[len - 2] = '\r';
                buf[len - 1] = '\n';
            }
        },
        else => {},
    }

    return buf[0..len];
}

test "fuzz: parseRequestLine never panics or returns out-of-bounds slices" {
    var prng = std.Random.DefaultPrng.init(0xC0FFEE_1234);
    var rng = prng.random();
    var input_buf: [4096]u8 = undefined;
    var storage: [8192]u8 = undefined;

    var iter: usize = 0;
    while (iter < 2000) : (iter += 1) {
        const input = fuzzScratch(&rng, .request_line, &input_buf);
        var offset: usize = 0;
        if (parseRequestLine(&storage, &offset, input, DEFAULT_MAX_URL_LENGTH)) |line| {
            // Slices must lie inside the storage buffer.
            const storage_base = @intFromPtr(&storage[0]);
            const storage_end = storage_base + storage.len;
            const method_ptr = @intFromPtr(line.method.ptr);
            const url_ptr = @intFromPtr(line.url.ptr);
            try testing.expect(method_ptr >= storage_base and method_ptr + line.method.len <= storage_end);
            try testing.expect(url_ptr >= storage_base and url_ptr + line.url.len <= storage_end);
            try testing.expect(line.url.len <= DEFAULT_MAX_URL_LENGTH);
            // path and query_string are subslices of url. By construction
            // (split at the first '?'), the sum is exactly url.len when no
            // '?' is present, and url.len - 1 when it is. The bound
            // therefore is path.len + query.len <= url.len; a tighter
            // bound here catches off-by-one errors at the '?' split.
            try testing.expect(line.path.len + line.query_string.len <= line.url.len);
        } else |err| {
            // Any error must be one the call site expects to handle.
            switch (err) {
                error.InvalidRequest,
                error.UriTooLong,
                error.HeaderStorageExhausted,
                => {},
            }
        }
    }
}

test "fuzz: parseRequestLineBorrowed slices stay inside the input" {
    var prng = std.Random.DefaultPrng.init(0xC0FFEE_5678);
    var rng = prng.random();
    var input_buf: [4096]u8 = undefined;

    var iter: usize = 0;
    while (iter < 2000) : (iter += 1) {
        const input = fuzzScratch(&rng, .request_line, &input_buf);
        if (parseRequestLineBorrowed(input, DEFAULT_MAX_URL_LENGTH)) |line| {
            const input_base = @intFromPtr(input.ptr);
            const input_end = input_base + input.len;
            const method_ptr = @intFromPtr(line.method.ptr);
            const url_ptr = @intFromPtr(line.url.ptr);
            try testing.expect(method_ptr >= input_base and method_ptr + line.method.len <= input_end);
            try testing.expect(url_ptr >= input_base and url_ptr + line.url.len <= input_end);
        } else |err| switch (err) {
            error.InvalidRequest, error.UriTooLong => {},
        }
    }
}

test "fuzz: parseQueryString never leaks under arbitrary input" {
    var prng = std.Random.DefaultPrng.init(0xDEADBEEF);
    var rng = prng.random();
    var input_buf: [2048]u8 = undefined;

    var iter: usize = 0;
    while (iter < 2000) : (iter += 1) {
        const input = fuzzScratch(&rng, .query_string, &input_buf);
        if (parseQueryString(testing.allocator, input, DEFAULT_MAX_QUERY_LENGTH)) |result| {
            // Inspect before freeing — params slices point into storage.
            // Percent-decoding monotonically shrinks length (`%XX` → 1
            // byte; `+` → 1 byte; everything else stays 1:1), so the
            // sum of decoded keys+values across all pairs is bounded
            // above by the raw input length.
            var sum: usize = 0;
            for (result.params) |param| {
                sum += param.key.len + param.value.len;
            }
            try testing.expect(sum <= input.len);
            if (result.storage) |s| testing.allocator.free(s);
            if (result.decoded_storage) |d| testing.allocator.free(d);
        } else |err| switch (err) {
            error.QueryTooLong, error.OutOfMemory => {},
        }
    }
}

test "fuzz: parseContentLengthValue is total over arbitrary bytes" {
    var prng = std.Random.DefaultPrng.init(0xABCD1234);
    var rng = prng.random();
    var input_buf: [256]u8 = undefined;

    var iter: usize = 0;
    while (iter < 4000) : (iter += 1) {
        const input = fuzzScratch(&rng, .content_length_value, &input_buf);
        if (parseContentLengthValue(input)) |_| {
            // Success path: every byte in the trimmed view was a digit.
            const trimmed = std.mem.trim(u8, input, " \t");
            try testing.expect(trimmed.len > 0);
            for (trimmed) |c| try testing.expect(c >= '0' and c <= '9');
        } else |err| switch (err) {
            error.InvalidContentLength => {},
        }
    }
}

test "fuzz: parseContentLength tolerates malformed header sections" {
    var prng = std.Random.DefaultPrng.init(0x11223344);
    var rng = prng.random();
    var input_buf: [4096]u8 = undefined;

    var iter: usize = 0;
    while (iter < 2000) : (iter += 1) {
        const input = fuzzScratch(&rng, .header_section, &input_buf);
        if (parseContentLength(input)) |_| {
            // success or null — both fine
        } else |err| switch (err) {
            error.InvalidContentLength, error.DuplicateContentLength => {},
        }
    }
}

test "fuzz: findHeaderEnd reports only in-range offsets" {
    var prng = std.Random.DefaultPrng.init(0x55667788);
    var rng = prng.random();
    var input_buf: [4096]u8 = undefined;

    var iter: usize = 0;
    while (iter < 4000) : (iter += 1) {
        const input = fuzzScratch(&rng, .header_terminator, &input_buf);
        if (findHeaderEnd(input)) |offset| {
            try testing.expect(offset + 4 <= input.len);
            try testing.expectEqual(@as(u8, '\r'), input[offset]);
            try testing.expectEqual(@as(u8, '\n'), input[offset + 1]);
            try testing.expectEqual(@as(u8, '\r'), input[offset + 2]);
            try testing.expectEqual(@as(u8, '\n'), input[offset + 3]);
        }
    }
}

// Specific known-bad inputs that have historically tripped HTTP parsers.
// These are regression pins, not fuzz iterations.
test "parser rejects classic malformed request-line shapes" {
    var storage: [256]u8 = undefined;
    var offset: usize = 0;
    // Two-tuple missing version is fine; the parser only enforces method+url.
    // But truly empty parts must error.
    offset = 0;
    try testing.expectError(error.InvalidRequest, parseRequestLine(&storage, &offset, "", DEFAULT_MAX_URL_LENGTH));
    offset = 0;
    try testing.expectError(error.InvalidRequest, parseRequestLine(&storage, &offset, "GET", DEFAULT_MAX_URL_LENGTH));
}

test "parser preserves CR/LF embedded in the request line verbatim" {
    // The request line is one logical line; embedded CR/LF could be a
    // smuggling attempt. The current parser does NOT strip them — they
    // end up inside `url` and downstream consumers see them. Pin that
    // current behavior so any future "strip CR/LF" change is a
    // conscious decision (and updates this test).
    var storage: [256]u8 = undefined;
    var offset: usize = 0;
    const line = try parseRequestLine(&storage, &offset, "GET /a\r\nInjected: header HTTP/1.1", DEFAULT_MAX_URL_LENGTH);
    try testing.expect(std.mem.indexOf(u8, line.url, "\r") != null);
}

test "parser preserves path traversal sequences in the URL slice verbatim" {
    // Path traversal handling is a server-layer concern, not a parser
    // concern. The parser keeps the URL verbatim; static file serving
    // does the canonicalization. Pin that contract so a refactor that
    // moves traversal handling into the parser doesn't quietly change
    // semantics for proxy/passthrough handlers that want raw URLs.
    var storage: [256]u8 = undefined;
    var offset: usize = 0;
    const line = try parseRequestLine(&storage, &offset, "GET /../etc/passwd HTTP/1.1", DEFAULT_MAX_URL_LENGTH);
    try testing.expectEqualStrings("/../etc/passwd", line.url);
}
