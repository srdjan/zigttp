//! HTTP response framing helpers extracted from server.zig.
//!
//! Pure functions over byte buffers — no Server, ConnectionPool, or
//! Runtime coupling. Used by both the threaded and evented backends.
//! Split out so server.zig can focus on the request lifecycle and
//! dispatch; the framing primitives have no shared mutable state and
//! make a clean sibling.

const std = @import("std");
const Io = std.Io;
const http_types = @import("http_types.zig");
const attest_header_strings = @import("attest/header_strings.zig");
const attest_well_known = @import("attest/well_known.zig");

const HttpResponse = http_types.HttpResponse;

/// Format an ETag from file mtime + size. Output is a fixed-width
/// 34-byte quoted hex string written into `out`.
pub fn formatETag(mtime: Io.Timestamp, size: u64, out: *[34]u8) []const u8 {
    const ns_bytes = std.mem.asBytes(&mtime.nanoseconds);
    const size_bytes = std.mem.asBytes(&size);
    var bytes: [16]u8 = undefined;
    @memcpy(bytes[0..8], ns_bytes[0..8]);
    @memcpy(bytes[8..16], size_bytes[0..8]);

    const hex_chars = "0123456789abcdef";
    out[0] = '"';
    for (bytes, 0..) |byte, i| {
        out[1 + i * 2] = hex_chars[byte >> 4];
        out[1 + i * 2 + 1] = hex_chars[byte & 0x0f];
    }
    out[33] = '"';
    return out[0..34];
}

pub fn connectionValue(keep_alive: bool) []const u8 {
    return if (keep_alive) "keep-alive" else "close";
}

pub fn formatStaticError(buf: []u8, status: u16, body: []const u8, keep_alive: bool) ![]const u8 {
    return formatHttpError(buf, status, body, keep_alive, null);
}

/// Single source of truth for HTTP error response framing. Callers pick the
/// sink (buffer, fd, Writer interface) and pass the formatted bytes through.
pub fn formatHttpError(
    buf: []u8,
    status: u16,
    body: []const u8,
    keep_alive: bool,
    content_type: ?[]const u8,
) ![]const u8 {
    if (content_type) |ct| {
        return std.fmt.bufPrint(
            buf,
            "HTTP/1.1 {d} {s}\r\nContent-Length: {d}\r\nContent-Type: {s}\r\nConnection: {s}\r\n\r\n{s}",
            .{ status, getStatusText(status), body.len, ct, connectionValue(keep_alive), body },
        );
    }
    return std.fmt.bufPrint(
        buf,
        "HTTP/1.1 {d} {s}\r\nContent-Length: {d}\r\nConnection: {s}\r\n\r\n{s}",
        .{ status, getStatusText(status), body.len, connectionValue(keep_alive), body },
    );
}

pub fn formatStaticNotModified(buf: []u8, etag: []const u8, keep_alive: bool) ![]const u8 {
    return std.fmt.bufPrint(
        buf,
        "HTTP/1.1 304 Not Modified\r\nETag: {s}\r\nConnection: {s}\r\n\r\n",
        .{ etag, connectionValue(keep_alive) },
    );
}

pub fn formatStaticOkHeader(
    buf: []u8,
    size: usize,
    content_type: []const u8,
    etag: []const u8,
    keep_alive: bool,
) ![]const u8 {
    return std.fmt.bufPrint(
        buf,
        "HTTP/1.1 200 OK\r\nContent-Length: {d}\r\nContent-Type: {s}\r\nETag: {s}\r\nConnection: {s}\r\n\r\n",
        .{ size, content_type, etag, connectionValue(keep_alive) },
    );
}

pub const DynamicHeaderOrder = enum {
    /// Historical threaded path order: handler headers, attestation,
    /// Content-Length, then Connection.
    sync,
    /// Historical evented path order: Content-Length, Connection,
    /// filtered handler headers, then attestation.
    evented,
};

pub fn appendStatusLine(buf: []u8, pos: *usize, status: u16, prefer_precomputed: bool) !void {
    if (prefer_precomputed) {
        if (getStatusLine(status)) |precomputed| {
            if (pos.* + precomputed.len > buf.len) return error.BufferOverflow;
            @memcpy(buf[pos.*..][0..precomputed.len], precomputed);
            pos.* += precomputed.len;
            return;
        }
    }

    const status_line = std.fmt.bufPrint(
        buf[pos.*..],
        "HTTP/1.1 {d} {s}\r\n",
        .{ status, getStatusText(status) },
    ) catch return error.BufferOverflow;
    pos.* += status_line.len;
}

pub fn appendHeaderLine(buf: []u8, pos: *usize, key: []const u8, value: []const u8) !void {
    const line = std.fmt.bufPrint(buf[pos.*..], "{s}: {s}\r\n", .{ key, value }) catch return error.BufferOverflow;
    pos.* += line.len;
}

/// Handler-supplied response headers must not duplicate framing headers the
/// server emits itself. Returns true when the header should NOT be relayed.
pub fn isFramingHeader(name: []const u8) bool {
    return std.ascii.eqlIgnoreCase(name, "Content-Length") or
        std.ascii.eqlIgnoreCase(name, "Connection") or
        std.ascii.eqlIgnoreCase(name, "Transfer-Encoding");
}

pub fn appendContentLengthHeader(buf: []u8, pos: *usize, body_len: usize) !void {
    const line = std.fmt.bufPrint(buf[pos.*..], "Content-Length: {d}\r\n", .{body_len}) catch return error.BufferOverflow;
    pos.* += line.len;
}

pub fn appendConnectionHeader(buf: []u8, pos: *usize, keep_alive: bool) !void {
    const line = if (keep_alive) "Connection: keep-alive\r\n" else "Connection: close\r\n";
    if (pos.* + line.len > buf.len) return error.BufferOverflow;
    @memcpy(buf[pos.*..][0..line.len], line);
    pos.* += line.len;
}

pub fn appendHeaderTerminator(buf: []u8, pos: *usize) !void {
    if (pos.* + 2 > buf.len) return error.BufferOverflow;
    @memcpy(buf[pos.*..][0..2], "\r\n");
    pos.* += 2;
}

pub fn buildDynamicResponseHeader(
    buf: []u8,
    response: *const HttpResponse,
    keep_alive: bool,
    attestation_headers: ?attest_header_strings.HeaderStrings,
    order: DynamicHeaderOrder,
) !usize {
    var pos: usize = 0;
    try appendStatusLine(buf, &pos, response.status, order == .sync);

    switch (order) {
        .sync => {
            for (response.headers.items) |header| {
                if (isFramingHeader(header.key)) continue;
                try appendHeaderLine(buf, &pos, header.key, header.value);
            }
            pos = try appendAttestationHeaders(attestation_headers, buf, pos);
            try appendContentLengthHeader(buf, &pos, response.body.len);
            try appendConnectionHeader(buf, &pos, keep_alive);
        },
        .evented => {
            try appendContentLengthHeader(buf, &pos, response.body.len);
            try appendConnectionHeader(buf, &pos, keep_alive);
            for (response.headers.items) |header| {
                if (isFramingHeader(header.key)) continue;
                try appendHeaderLine(buf, &pos, header.key, header.value);
            }
            pos = try appendAttestationHeaders(attestation_headers, buf, pos);
        },
    }

    try appendHeaderTerminator(buf, &pos);
    return pos;
}

/// Format the response headers for `GET /.well-known/zigttp-attest`. The
/// well-known endpoint is hot and deterministic: when cached, return 304
/// with the precomputed ETag; otherwise 200 with Content-Type, ETag, and a
/// precomputed body. Returns the prefix length (headers); the caller writes
/// the body separately.
pub fn formatWellKnownHeaders(
    doc: *const attest_well_known.Doc,
    cached: bool,
    keep_alive: bool,
    header_buf: []u8,
) !usize {
    const conn = if (keep_alive) "keep-alive" else "close";
    if (cached) {
        const out = try std.fmt.bufPrint(
            header_buf,
            "HTTP/1.1 304 Not Modified\r\nETag: \"{s}\"\r\nCache-Control: public, max-age={d}\r\nConnection: {s}\r\nContent-Length: 0\r\n\r\n",
            .{ doc.etag_hex, attest_well_known.cache_max_age_seconds, conn },
        );
        return out.len;
    }
    const out = try std.fmt.bufPrint(
        header_buf,
        "HTTP/1.1 200 OK\r\nContent-Type: {s}\r\nETag: \"{s}\"\r\nCache-Control: public, max-age={d}\r\nConnection: {s}\r\nContent-Length: {d}\r\n\r\n",
        .{ attest_well_known.content_type, doc.etag_hex, attest_well_known.cache_max_age_seconds, conn, doc.body.len },
    );
    return out.len;
}

/// Append the precomputed `Zigttp-Proofs` and `Zigttp-Attest` header lines.
/// Slice 1 of proof receipts: per-request work is two `bufPrint`s of static
/// strings, never a parse or signature operation. `proofs_value.len == 0`
/// signals "no chip is proven; skip the line so the header is never empty."
pub fn appendAttestationHeaders(
    headers: ?attest_header_strings.HeaderStrings,
    buf: []u8,
    start: usize,
) !usize {
    const hs = headers orelse return start;
    var pos = start;
    if (hs.proofs_value.len > 0) {
        const line = std.fmt.bufPrint(buf[pos..], "{s}: {s}\r\n", .{ attest_header_strings.header_name_proofs, hs.proofs_value }) catch return error.BufferOverflow;
        pos += line.len;
    }
    const attest_line = std.fmt.bufPrint(buf[pos..], "{s}: {s}\r\n", .{ attest_header_strings.header_name_attest, hs.attest_value }) catch return error.BufferOverflow;
    pos += attest_line.len;
    return pos;
}

pub fn getStatusText(status: u16) []const u8 {
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
        413 => "Payload Too Large",
        414 => "URI Too Long",
        422 => "Unprocessable Entity",
        429 => "Too Many Requests",
        500 => "Internal Server Error",
        501 => "Not Implemented",
        502 => "Bad Gateway",
        503 => "Service Unavailable",
        504 => "Gateway Timeout",
        else => "Unknown",
    };
}

/// Pre-computed status lines for common status codes.
/// Avoids fmt.bufPrint overhead in hot path.
pub fn getStatusLine(status: u16) ?[]const u8 {
    return switch (status) {
        200 => "HTTP/1.1 200 OK\r\n",
        201 => "HTTP/1.1 201 Created\r\n",
        204 => "HTTP/1.1 204 No Content\r\n",
        301 => "HTTP/1.1 301 Moved Permanently\r\n",
        302 => "HTTP/1.1 302 Found\r\n",
        304 => "HTTP/1.1 304 Not Modified\r\n",
        400 => "HTTP/1.1 400 Bad Request\r\n",
        404 => "HTTP/1.1 404 Not Found\r\n",
        500 => "HTTP/1.1 500 Internal Server Error\r\n",
        503 => "HTTP/1.1 503 Service Unavailable\r\n",
        else => null,
    };
}

// -------------------------------------------------------------------------
// Tests (moved from server.zig)
// -------------------------------------------------------------------------

test "appendAttestationHeaders: null headers is a no-op" {
    var buf: [256]u8 = undefined;
    const out = try appendAttestationHeaders(null, &buf, 7);
    try std.testing.expectEqual(@as(usize, 7), out);
}

test "appendAttestationHeaders: empty proofs writes only Zigttp-Attest" {
    var buf: [256]u8 = undefined;
    const hs = attest_header_strings.HeaderStrings{
        .proofs_value = "",
        .attest_value = "abc.def.ghi",
    };
    const out = try appendAttestationHeaders(hs, &buf, 0);
    try std.testing.expectEqualStrings("Zigttp-Attest: abc.def.ghi\r\n", buf[0..out]);
}

test "appendAttestationHeaders: both lines written in order" {
    var buf: [256]u8 = undefined;
    const hs = attest_header_strings.HeaderStrings{
        .proofs_value = "pure, injection_safe",
        .attest_value = "h.p.s",
    };
    const out = try appendAttestationHeaders(hs, &buf, 0);
    try std.testing.expectEqualStrings(
        "Zigttp-Proofs: pure, injection_safe\r\nZigttp-Attest: h.p.s\r\n",
        buf[0..out],
    );
}

test "appendAttestationHeaders: BufferOverflow when buf too small" {
    var buf: [10]u8 = undefined;
    const hs = attest_header_strings.HeaderStrings{
        .proofs_value = "x",
        .attest_value = "y",
    };
    try std.testing.expectError(error.BufferOverflow, appendAttestationHeaders(hs, &buf, 0));
}

test "get status text" {
    try std.testing.expectEqualStrings("OK", getStatusText(200));
    try std.testing.expectEqualStrings("Not Found", getStatusText(404));
    try std.testing.expectEqualStrings("Internal Server Error", getStatusText(500));
}
