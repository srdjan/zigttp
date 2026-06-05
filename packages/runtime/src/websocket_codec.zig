//! RFC 6455 WebSocket codec helpers.
//!
//! W1-c scope: handshake-time pieces only. Frame encode/decode is deferred
//! until W1-d when ws_gateway has an active connection to exercise it
//! against. The frame-level types (`Opcode`, `Header0`, `Header1`) are
//! re-exported from `std.http.Server.WebSocket` so W1-d can either drive
//! the stdlib's `readSmallMessage`/`writeMessage` directly against our
//! socket or drop in our own reader/writer adapters without redefining
//! the wire format.
//!
//! What lives here:
//!   - `computeAcceptKey(out, client_key)` — base64-encoded SHA1 of the
//!     client key + the RFC 6455 magic GUID, written into a fixed 28-byte
//!     buffer. Matches `std.http.Server.Request.respondWebSocket`'s inline
//!     computation so clients can't tell the difference.
//!   - `validateUpgrade(headers) !UpgradeRequest` — scans the request
//!     headers for a valid RFC 6455 upgrade: correct Upgrade / Connection
//!     tokens, Sec-WebSocket-Version: 13, and a non-empty
//!     Sec-WebSocket-Key. Returns the client key (borrowed from the
//!     headers) or a typed error.
//!   - `writeHandshakeResponse(writer, accept_key)` — emits the 101
//!     response required to complete the handshake.
//!
//! The validator is permissive by design: `Connection` may contain the
//! `Upgrade` token among other values (some proxies add `keep-alive`),
//! and all header-name comparisons are case-insensitive per RFC 7230.

const std = @import("std");
const ascii = std.ascii;
const http_types = @import("http_types.zig");

/// Re-exports so downstream callers don't need to import std.http.Server
/// directly and can swap this module's adapters in and out uniformly.
pub const Opcode = std.http.Server.WebSocket.Opcode;
pub const Header0 = std.http.Server.WebSocket.Header0;
pub const Header1 = std.http.Server.WebSocket.Header1;

pub const CloseMetadata = struct {
    /// RFC 6455 "No Status Received" sentinel. This value is never valid on
    /// the wire, but is the right local representation for an empty close body.
    code: u16 = 1005,
    reason: []const u8 = "",
    had_code: bool = false,
};

pub const SmallMessage = struct {
    opcode: Opcode,
    data: []u8,
};

pub const ReadResult = union(enum) {
    message: SmallMessage,
    close: CloseMetadata,
};

pub const ReadSmallMessageError = error{
    ConnectionClose,
    UnexpectedOpCode,
    MessageOversize,
    MissingMaskBit,
    ReadFailed,
    EndOfStream,
    InvalidCloseFrame,
    InvalidCloseCode,
    InvalidCloseReason,
};

/// Magic GUID appended to the client key during accept-key computation.
/// Defined in RFC 6455 §4.2.2.
const accept_key_guid: []const u8 = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

/// Accept-key output length: base64(SHA1) of a fixed 60-byte input is
/// always exactly 28 bytes.
pub const accept_key_len: usize = 28;

/// Write the RFC 6455 Sec-WebSocket-Accept value (base64-encoded SHA1 of
/// client_key + magic GUID) into `out`. `out` must be exactly
/// `accept_key_len` bytes long.
pub fn computeAcceptKey(out: *[accept_key_len]u8, client_key: []const u8) void {
    var sha1 = std.crypto.hash.Sha1.init(.{});
    sha1.update(client_key);
    sha1.update(accept_key_guid);
    var digest: [std.crypto.hash.Sha1.digest_length]u8 = undefined;
    sha1.final(&digest);
    const encoded = std.base64.standard.Encoder.encode(out, &digest);
    std.debug.assert(encoded.len == accept_key_len);
}

pub const UpgradeError = error{
    MissingUpgradeHeader,
    MissingConnectionHeader,
    MissingWebSocketVersion,
    UnsupportedWebSocketVersion,
    MissingWebSocketKey,
    InvalidWebSocketKey,
};

pub const UpgradeRequest = struct {
    /// Borrowed from the request headers; lifetime matches the caller's
    /// `HttpRequestView`.
    client_key: []const u8,
};

/// Inspect request headers and decide whether they constitute a valid
/// RFC 6455 upgrade. Returns the client key (borrowed) on success.
pub fn validateUpgrade(headers: []const http_types.HttpHeader) UpgradeError!UpgradeRequest {
    var upgrade_header: ?[]const u8 = null;
    var connection_header: ?[]const u8 = null;
    var version_header: ?[]const u8 = null;
    var key_header: ?[]const u8 = null;

    for (headers) |h| {
        if (ascii.eqlIgnoreCase(h.key, "upgrade")) upgrade_header = h.value;
        if (ascii.eqlIgnoreCase(h.key, "connection")) connection_header = h.value;
        if (ascii.eqlIgnoreCase(h.key, "sec-websocket-version")) version_header = h.value;
        if (ascii.eqlIgnoreCase(h.key, "sec-websocket-key")) key_header = h.value;
    }

    const upgrade = upgrade_header orelse return error.MissingUpgradeHeader;
    if (!tokenListContains(upgrade, "websocket")) return error.MissingUpgradeHeader;

    const connection = connection_header orelse return error.MissingConnectionHeader;
    if (!tokenListContains(connection, "upgrade")) return error.MissingConnectionHeader;

    const version = version_header orelse return error.MissingWebSocketVersion;
    if (!std.mem.eql(u8, std.mem.trim(u8, version, " \t"), "13")) {
        return error.UnsupportedWebSocketVersion;
    }

    const key = key_header orelse return error.MissingWebSocketKey;
    const key_trimmed = std.mem.trim(u8, key, " \t");
    if (key_trimmed.len == 0) return error.MissingWebSocketKey;
    // The client MUST send a 24-character base64 value; we don't decode
    // it because the accept-key hash treats it as opaque bytes, but we
    // do enforce non-empty and reasonable length as a sanity check.
    if (key_trimmed.len > 256) return error.InvalidWebSocketKey;

    return .{ .client_key = key_trimmed };
}

/// Write the 101 Switching Protocols response for a successful upgrade.
/// `accept_key` must be the 28-byte base64 output of `computeAcceptKey`.
/// The caller is responsible for flushing the writer.
pub fn writeHandshakeResponse(writer: anytype, accept_key: *const [accept_key_len]u8) !void {
    try writer.writeAll("HTTP/1.1 101 Switching Protocols\r\n");
    try writer.writeAll("Upgrade: websocket\r\n");
    try writer.writeAll("Connection: Upgrade\r\n");
    try writer.writeAll("Sec-WebSocket-Accept: ");
    try writer.writeAll(accept_key);
    try writer.writeAll("\r\n\r\n");
}

/// Read a single non-fragmented client frame. This mirrors
/// std.http.Server.WebSocket.readSmallMessage but returns parsed close-frame
/// metadata instead of collapsing every close into error.ConnectionClose.
pub fn readSmallMessage(ws: *std.http.Server.WebSocket) ReadSmallMessageError!ReadResult {
    const in = ws.input;
    while (true) {
        const header = try in.takeArray(2);
        const h0: Header0 = @bitCast(header[0]);
        const h1: Header1 = @bitCast(header[1]);

        switch (h0.opcode) {
            .text, .binary, .pong, .ping, .connection_close => {},
            .continuation => return error.UnexpectedOpCode,
            _ => return error.UnexpectedOpCode,
        }

        if (!h0.fin) return error.MessageOversize;
        if (!h1.mask) return error.MissingMaskBit;

        const len: usize = switch (h1.payload_len) {
            .len16 => try in.takeInt(u16, .big),
            .len64 => std.math.cast(usize, try in.takeInt(u64, .big)) orelse return error.MessageOversize,
            else => @intFromEnum(h1.payload_len),
        };
        if (len > in.buffer.len) return error.MessageOversize;
        if (isControlOpcode(h0.opcode) and len > 125) return error.MessageOversize;

        const mask: u32 = @bitCast((try in.takeArray(4)).*);
        const payload = try in.take(len);
        unmaskPayload(payload, mask);

        switch (h0.opcode) {
            .pong => continue,
            .connection_close => return .{ .close = try parseClosePayload(payload) },
            .text, .binary, .ping => return .{ .message = .{ .opcode = h0.opcode, .data = payload } },
            else => return error.UnexpectedOpCode,
        }
    }
}

pub fn parseClosePayload(payload: []const u8) ReadSmallMessageError!CloseMetadata {
    if (payload.len == 0) return .{};
    if (payload.len == 1) return error.InvalidCloseFrame;

    const code = std.mem.readInt(u16, payload[0..2], .big);
    if (!validCloseCode(code)) return error.InvalidCloseCode;

    const reason = payload[2..];
    if (!std.unicode.utf8ValidateSlice(reason)) return error.InvalidCloseReason;
    return .{ .code = code, .reason = reason, .had_code = true };
}

fn validCloseCode(code: u16) bool {
    return switch (code) {
        1000, 1001, 1002, 1003, 1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014 => true,
        3000...4999 => true,
        else => false,
    };
}

fn isControlOpcode(opcode: Opcode) bool {
    return switch (opcode) {
        .connection_close, .ping, .pong => true,
        else => false,
    };
}

fn unmaskPayload(payload: []u8, mask: u32) void {
    const floored_len = (payload.len / 4) * 4;
    const u32_payload: []align(1) u32 = @ptrCast(payload[0..floored_len]);
    for (u32_payload) |*elem| elem.* ^= mask;
    const mask_bytes: []const u8 = @ptrCast(&mask);
    for (payload[floored_len..], mask_bytes[0 .. payload.len - floored_len]) |*leftover, m| {
        leftover.* ^= m;
    }
}

/// Check whether a comma-separated HTTP header field-value contains a
/// given token (case-insensitive, token-boundary-aware).
fn tokenListContains(header_value: []const u8, needle: []const u8) bool {
    var it = std.mem.splitScalar(u8, header_value, ',');
    while (it.next()) |token| {
        const trimmed = std.mem.trim(u8, token, " \t");
        if (ascii.eqlIgnoreCase(trimmed, needle)) return true;
    }
    return false;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "computeAcceptKey matches the RFC 6455 test vector" {
    // RFC 6455 §1.3: client key "dGhlIHNhbXBsZSBub25jZQ==" produces
    // accept key "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=".
    var out: [accept_key_len]u8 = undefined;
    computeAcceptKey(&out, "dGhlIHNhbXBsZSBub25jZQ==");
    try testing.expectEqualStrings("s3pPLMBiTxaQ9kYGzzhZRbK+xOo=", &out);
}

test "validateUpgrade accepts a well-formed request" {
    const headers = [_]http_types.HttpHeader{
        .{ .key = "Host", .value = "example.com" },
        .{ .key = "Upgrade", .value = "websocket" },
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "Sec-WebSocket-Version", .value = "13" },
    };
    const upgrade = try validateUpgrade(&headers);
    try testing.expectEqualStrings("dGhlIHNhbXBsZSBub25jZQ==", upgrade.client_key);
}

test "validateUpgrade tolerates multi-token Connection header" {
    // Some proxies append keep-alive, upgrade to the Connection header.
    const headers = [_]http_types.HttpHeader{
        .{ .key = "Upgrade", .value = "websocket" },
        .{ .key = "Connection", .value = "keep-alive, Upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "Sec-WebSocket-Version", .value = "13" },
    };
    const upgrade = try validateUpgrade(&headers);
    try testing.expectEqualStrings("dGhlIHNhbXBsZSBub25jZQ==", upgrade.client_key);
}

test "validateUpgrade is case-insensitive on header names" {
    const headers = [_]http_types.HttpHeader{
        .{ .key = "UPGRADE", .value = "WebSocket" },
        .{ .key = "connection", .value = "upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "sec-websocket-version", .value = "13" },
    };
    const upgrade = try validateUpgrade(&headers);
    try testing.expectEqualStrings("dGhlIHNhbXBsZSBub25jZQ==", upgrade.client_key);
}

test "validateUpgrade rejects missing Upgrade header" {
    const headers = [_]http_types.HttpHeader{
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "Sec-WebSocket-Version", .value = "13" },
    };
    try testing.expectError(error.MissingUpgradeHeader, validateUpgrade(&headers));
}

test "validateUpgrade rejects non-websocket Upgrade token" {
    const headers = [_]http_types.HttpHeader{
        .{ .key = "Upgrade", .value = "h2c" },
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "Sec-WebSocket-Version", .value = "13" },
    };
    try testing.expectError(error.MissingUpgradeHeader, validateUpgrade(&headers));
}

test "validateUpgrade rejects wrong websocket version" {
    const headers = [_]http_types.HttpHeader{
        .{ .key = "Upgrade", .value = "websocket" },
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Key", .value = "dGhlIHNhbXBsZSBub25jZQ==" },
        .{ .key = "Sec-WebSocket-Version", .value = "8" },
    };
    try testing.expectError(error.UnsupportedWebSocketVersion, validateUpgrade(&headers));
}

test "validateUpgrade rejects missing Sec-WebSocket-Key" {
    const headers = [_]http_types.HttpHeader{
        .{ .key = "Upgrade", .value = "websocket" },
        .{ .key = "Connection", .value = "Upgrade" },
        .{ .key = "Sec-WebSocket-Version", .value = "13" },
    };
    try testing.expectError(error.MissingWebSocketKey, validateUpgrade(&headers));
}

test "writeHandshakeResponse emits the RFC 6455 101 response" {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);

    var accept: [accept_key_len]u8 = undefined;
    computeAcceptKey(&accept, "dGhlIHNhbXBsZSBub25jZQ==");

    try writeHandshakeResponse(&aw.writer, &accept);
    buf = aw.toArrayList();

    const expected =
        "HTTP/1.1 101 Switching Protocols\r\n" ++
        "Upgrade: websocket\r\n" ++
        "Connection: Upgrade\r\n" ++
        "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n" ++
        "\r\n";
    try testing.expectEqualStrings(expected, buf.items);
}

test "tokenListContains splits on comma and trims whitespace" {
    try testing.expect(tokenListContains("Upgrade", "upgrade"));
    try testing.expect(tokenListContains("keep-alive, Upgrade", "upgrade"));
    try testing.expect(tokenListContains("Upgrade , keep-alive", "upgrade"));
    try testing.expect(!tokenListContains("keep-alive", "upgrade"));
    try testing.expect(!tokenListContains("upgrade-other", "upgrade"));
}

test "Opcode re-export matches stdlib shape" {
    // If stdlib renumbers opcodes, our code that imports these breaks.
    try testing.expectEqual(@as(u4, 1), @intFromEnum(Opcode.text));
    try testing.expectEqual(@as(u4, 2), @intFromEnum(Opcode.binary));
    try testing.expectEqual(@as(u4, 8), @intFromEnum(Opcode.connection_close));
    try testing.expectEqual(@as(u4, 9), @intFromEnum(Opcode.ping));
    try testing.expectEqual(@as(u4, 10), @intFromEnum(Opcode.pong));
}

test "parseClosePayload accepts code and reason" {
    const close = try parseClosePayload("\x03\xe8normal");
    try testing.expect(close.had_code);
    try testing.expectEqual(@as(u16, 1000), close.code);
    try testing.expectEqualStrings("normal", close.reason);
}

test "parseClosePayload uses 1005 sentinel for empty payload" {
    const close = try parseClosePayload("");
    try testing.expect(!close.had_code);
    try testing.expectEqual(@as(u16, 1005), close.code);
    try testing.expectEqualStrings("", close.reason);
}

test "parseClosePayload rejects one byte payload" {
    try testing.expectError(error.InvalidCloseFrame, parseClosePayload(&[_]u8{0x03}));
}

test "parseClosePayload rejects reserved wire code" {
    try testing.expectError(error.InvalidCloseCode, parseClosePayload(&[_]u8{ 0x03, 0xee }));
}

test "parseClosePayload rejects invalid utf8 reason" {
    try testing.expectError(error.InvalidCloseReason, parseClosePayload(&[_]u8{ 0x03, 0xe8, 0xff }));
}
