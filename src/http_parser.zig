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
