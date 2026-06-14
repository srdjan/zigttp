//! Shared mapping from a model-provider HTTP error response to a typed error.
//!
//! Both the Anthropic and OpenAI wire layers call `classify`; the catch sites
//! turn each typed error into one-line remediation via
//! `loop.providerErrorRemediation`. Keeping the status/body interpretation in
//! one place means the two providers cannot drift in how they report auth,
//! credit, rate-limit, model, overload, and prompt-too-long failures.

const std = @import("std");

/// Map an HTTP error status (and, for ambiguous 4xx, the error body) to a typed
/// provider error. The body substrings cover both providers' conventions:
/// Anthropic returns 400 "credit balance is too low"; OpenAI returns 429
/// "insufficient_quota" and 400 "context_length_exceeded".
pub fn classify(status_code: u16, body: []const u8) anyerror {
    return switch (status_code) {
        401, 403 => error.AuthFailed,
        404 => error.ModelNotFound,
        429 => if (containsAny(body, &.{ "insufficient_quota", "credit balance", "quota" }))
            error.InsufficientCredit
        else
            error.RateLimited,
        529 => error.ProviderOverloaded,
        else => blk: {
            if (status_code >= 500) break :blk error.ProviderServerError;
            if (status_code == 400) {
                if (containsAny(body, &.{"credit balance"})) break :blk error.InsufficientCredit;
                if (containsAny(body, &.{ "prompt is too long", "context_length_exceeded", "maximum context", "too long" }))
                    break :blk error.PromptTooLong;
            }
            break :blk error.HttpNotOk;
        },
    };
}

fn containsAny(haystack: []const u8, needles: []const []const u8) bool {
    for (needles) |n| {
        if (std.mem.indexOf(u8, haystack, n) != null) return true;
    }
    return false;
}

/// TCP connect + TLS handshake budget. Bounds a captive-portal / unreachable
/// endpoint so a request fails fast instead of hanging on connect.
pub const connect_timeout_ms: u64 = 15_000;

/// Idle-read budget for the streamed response. A reasoning response streams
/// tokens continuously, so this many milliseconds with zero bytes is a genuine
/// stall (overloaded endpoint, dropped-but-not-reset connection), not a slow
/// answer. Bounded via SO_RCVTIMEO so a stalled stream cannot hang forever.
pub const read_idle_timeout_ms: u64 = 120_000;

/// Send budget for the request body. A half-open / stalled peer that accepts
/// the connection but never drains its receive window would otherwise block the
/// body write (writeAll + flush) forever. Bounded via SO_SNDTIMEO.
pub const write_idle_timeout_ms: u64 = 120_000;

/// The connect/handshake timeout to pass to `connectTcpOptions`.
pub fn connectTimeout() std.Io.Timeout {
    return .{ .duration = .{ .raw = std.Io.Duration.fromMilliseconds(connect_timeout_ms), .clock = .awake } };
}

/// Best-effort SO_RCVTIMEO on a connection socket, so a blocking read returns an
/// error once an idle gap exceeds `read_idle_timeout_ms` instead of blocking
/// until the peer closes the stream. Mirrors the runtime server's slow-client
/// guard. Failures are ignored: the connect timeout still bounds the request.
pub fn setReadTimeout(fd: std.posix.fd_t) void {
    const tv = std.posix.timeval{
        .sec = @intCast(read_idle_timeout_ms / 1000),
        .usec = @intCast((read_idle_timeout_ms % 1000) * 1000),
    };
    std.posix.setsockopt(fd, std.posix.SOL.SOCKET, std.posix.SO.RCVTIMEO, std.mem.asBytes(&tv)) catch {};
}

/// Best-effort SO_SNDTIMEO on a connection socket, so the body write returns an
/// error once a stalled peer stops draining its receive window instead of
/// blocking forever. Mirrors the runtime server's slow-client write guard.
pub fn setWriteTimeout(fd: std.posix.fd_t) void {
    const tv = std.posix.timeval{
        .sec = @intCast(write_idle_timeout_ms / 1000),
        .usec = @intCast((write_idle_timeout_ms % 1000) * 1000),
    };
    std.posix.setsockopt(fd, std.posix.SOL.SOCKET, std.posix.SO.SNDTIMEO, std.mem.asBytes(&tv)) catch {};
}

/// True for the connect/read errors that mean a network stall, so the wire
/// layer can return a clean `error.RequestTimedOut`.
pub fn isTimeout(err: anyerror) bool {
    return err == error.Timeout or err == error.WouldBlock;
}

/// Open a provider connection with the shared connect timeout and idle-read
/// guard, mapping a connect timeout to a clean `error.RequestTimedOut`. Both
/// the Anthropic and OpenAI clients go through this so the timeout policy and
/// the socket-option dance live in exactly one place. `explicit_port` is the
/// URI's port, if any; otherwise the protocol default is used.
pub fn connect(
    client: *std.http.Client,
    host: std.Io.net.HostName,
    explicit_port: ?u16,
    protocol: std.http.Client.Protocol,
) !*std.http.Client.Connection {
    const connection = client.connectTcpOptions(.{
        .host = host,
        .port = explicit_port orelse switch (protocol) {
            .plain => 80,
            .tls => 443,
        },
        .protocol = protocol,
        .timeout = connectTimeout(),
    }) catch |err| {
        if (isTimeout(err)) return error.RequestTimedOut;
        return err;
    };
    // Bound a stalled stream so a hung response can't freeze the CLI forever,
    // on both the read side (SO_RCVTIMEO) and the body-write side (SO_SNDTIMEO).
    const sock_handle = connection.stream_reader.stream.socket.handle;
    setReadTimeout(sock_handle);
    setWriteTimeout(sock_handle);
    return connection;
}

/// Read the full response body, mapping an idle-read timeout to a clean
/// `error.RequestTimedOut` (shared by both provider clients).
pub fn readBody(reader: anytype, arena: std.mem.Allocator, limit_bytes: usize) ![]u8 {
    return reader.allocRemaining(arena, .limited(limit_bytes)) catch |err| {
        if (isTimeout(err)) return error.RequestTimedOut;
        return err;
    };
}

const testing = std.testing;

test "classify maps common provider statuses to typed errors" {
    try testing.expectEqual(error.AuthFailed, classify(401, ""));
    try testing.expectEqual(error.AuthFailed, classify(403, ""));
    try testing.expectEqual(error.ModelNotFound, classify(404, ""));
    try testing.expectEqual(error.RateLimited, classify(429, "rate_limit_error"));
    try testing.expectEqual(error.InsufficientCredit, classify(429, "insufficient_quota"));
    try testing.expectEqual(error.InsufficientCredit, classify(400, "your credit balance is too low"));
    try testing.expectEqual(error.PromptTooLong, classify(400, "context_length_exceeded"));
    try testing.expectEqual(error.PromptTooLong, classify(400, "prompt is too long for the model"));
    try testing.expectEqual(error.ProviderOverloaded, classify(529, ""));
    try testing.expectEqual(error.ProviderServerError, classify(500, ""));
    try testing.expectEqual(error.ProviderServerError, classify(503, ""));
    try testing.expectEqual(error.HttpNotOk, classify(400, "some other bad request"));
}
