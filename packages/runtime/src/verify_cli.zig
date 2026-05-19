//! `zigttp verify <url>` - slice 1 of proof receipts. Third-party verifier
//! that anyone (CI, security scanner, partner integration, MCP host) can run
//! against any deployed zigttp endpoint to confirm the contract claims it
//! returns are signed by whoever holds the build's private key.
//!
//! Trust posture for slice 1: the JWS protected header carries the full
//! Ed25519 public key. The verifier validates the signature against that
//! embedded key, prints the key fingerprint, and (with `--trust-key <hex>`)
//! can pin against a known fingerprint. Identity binding lands in slice 2
//! (well-known endpoint plus transparency log).

const std = @import("std");
const envelope = @import("attest/envelope.zig");
const attest_header_strings = @import("attest/header_strings.zig");

pub const exit_ok: u8 = 0;
pub const exit_arg_error: u8 = 1;
pub const exit_not_attested: u8 = 2;
pub const exit_signature_invalid: u8 = 3;
pub const exit_key_mismatch: u8 = 4;
pub const exit_http_error: u8 = 5;

pub const Options = struct {
    url: []const u8,
    trust_key_fingerprint_hex: ?[]const u8 = null,
    json_output: bool = false,
};

pub fn parseArgs(argv: []const []const u8) !Options {
    var url: ?[]const u8 = null;
    var trust_key: ?[]const u8 = null;
    var json_output = false;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--trust-key")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            trust_key = argv[i];
        } else if (std.mem.eql(u8, arg, "--json")) {
            json_output = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return error.HelpRequested;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            if (url != null) return error.TooManyArguments;
            url = arg;
        } else {
            return error.UnknownArgument;
        }
    }

    if (url == null) return error.MissingArgument;
    if (trust_key) |k| {
        if (k.len != 64) return error.InvalidTrustKey;
        for (k) |c| {
            if (!std.ascii.isHex(c)) return error.InvalidTrustKey;
        }
    }
    return .{
        .url = url.?,
        .trust_key_fingerprint_hex = trust_key,
        .json_output = json_output,
    };
}

pub fn printHelp() void {
    const help =
        \\zigttp verify <url> [--trust-key <hex>] [--json]
        \\
        \\Fetches the URL, reads the Zigttp-Attest response header, and validates
        \\its Ed25519 signature against the embedded public key. Prints the proven
        \\contract claims on success.
        \\
        \\Options:
        \\  --trust-key <hex>  64-char SHA-256 fingerprint of the expected public
        \\                     key. Fails (exit 4) on mismatch.
        \\  --json             Emit the claims as a single JSON object on stdout.
        \\
        \\Exit codes:
        \\  0  verified
        \\  1  argument error
        \\  2  endpoint did not return Zigttp-Attest header
        \\  3  signature does not validate
        \\  4  key fingerprint does not match --trust-key
        \\  5  HTTP-level error (DNS, connect, status >= 400)
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

pub fn run(allocator: std.mem.Allocator, opts: Options) !u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();

    const attest_jws = fetchAttestHeader(allocator, io_backend.io(), opts.url) catch |err| {
        switch (err) {
            error.UnsupportedScheme => writeStderr("verify: only http and https URLs are supported\n"),
            error.NotAttested => writeStderr("verify: endpoint did not return Zigttp-Attest header\n"),
            error.HttpStatus => writeStderr("verify: HTTP request returned non-2xx status\n"),
            else => writeStderrFmt("verify: request failed: {t}\n", .{err}),
        }
        return switch (err) {
            error.UnsupportedScheme => exit_arg_error,
            error.NotAttested => exit_not_attested,
            else => exit_http_error,
        };
    };
    defer allocator.free(attest_jws);

    var result = envelope.verify(allocator, attest_jws) catch |err| {
        writeStderrFmt("verify: signature verification failed: {t}\n", .{err});
        return exit_signature_invalid;
    };
    defer result.deinit();

    if (opts.trust_key_fingerprint_hex) |expected| {
        if (!std.mem.eql(u8, expected, &result.fingerprint_hex)) {
            writeStderrFmt(
                "verify: key fingerprint does not match --trust-key\n  expected: {s}\n  actual:   {s}\n",
                .{ expected, result.fingerprint_hex },
            );
            return exit_key_mismatch;
        }
    }

    if (opts.json_output) {
        writeClaimsJson(opts.url, &result);
    } else {
        writeClaimsHuman(opts.url, &result);
    }
    return exit_ok;
}

const FetchError = error{
    UnsupportedScheme,
    NotAttested,
    HttpStatus,
    OutOfMemory,
};

/// Fetch the URL, extract the Zigttp-Attest header value, return an owned
/// copy. The response body is discarded; only headers matter for slice 1.
fn fetchAttestHeader(
    allocator: std.mem.Allocator,
    io: std.Io,
    url: []const u8,
) ![]u8 {
    const uri = std.Uri.parse(url) catch return error.UnsupportedScheme;
    if (!std.mem.eql(u8, uri.scheme, "http") and !std.mem.eql(u8, uri.scheme, "https")) {
        return error.UnsupportedScheme;
    }

    var client = std.http.Client{ .allocator = allocator, .io = io };
    defer client.deinit();

    var req = try client.request(.GET, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
    });
    defer req.deinit();

    try req.sendBodiless();
    var response = try req.receiveHead(&.{});

    const status = @intFromEnum(response.head.status);
    if (status < 200 or status >= 300) return error.HttpStatus;

    var it = response.head.iterateHeaders();
    while (it.next()) |header| {
        if (std.ascii.eqlIgnoreCase(header.name, attest_header_strings.header_name_attest)) {
            const owned = try allocator.dupe(u8, header.value);
            // Drain body so the connection is left clean; we ignore the bytes.
            const reader = response.reader(&.{});
            _ = reader.discardRemaining() catch {};
            return owned;
        }
    }
    return error.NotAttested;
}

// TODO(slice 2): add a fixture-based integration test for `run()` that spins
// up a fake HTTP server emitting both header forms (with chips, empty chips,
// signature-tampered, key-mismatched) and asserts on the exit codes. Slice 1
// relies on the `parseArgs` unit tests, the `envelope.verify` unit tests, and
// manual end-to-end runs documented in the slice-1 spec.

fn writeClaimsHuman(url: []const u8, result: *const envelope.VerifyResult) void {
    var buf: [4096]u8 = undefined;
    const out = std.fmt.bufPrint(
        &buf,
        \\Verified: {s}
        \\  key fingerprint:  {s}
        \\  compiler version: {s}
        \\  signed at:        {d} (unix)
        \\  contract sha256:  {s}
        \\  bytecode sha256:  {s}
        \\  policy sha256:    {s}
        \\  capability hash:  {s}
        \\  routes count:     {d}
        \\  proven chips:     {s}
        \\
        ,
        .{
            url,
            &result.fingerprint_hex,
            result.claims.compiler_version,
            result.claims.signed_at_unix,
            result.claims.contract_sha256,
            result.claims.bytecode_sha256,
            result.claims.policy_sha256,
            result.claims.capability_hash,
            result.claims.routes_count,
            if (result.claims.property_summary.len > 0) result.claims.property_summary else "(none)",
        },
    ) catch return;
    _ = std.c.write(std.c.STDOUT_FILENO, out.ptr, out.len);
}

fn writeClaimsJson(url: []const u8, result: *const envelope.VerifyResult) void {
    var buf: [4096]u8 = undefined;
    const out = std.fmt.bufPrint(
        &buf,
        "{{\"url\":\"{s}\",\"keyFingerprint\":\"{s}\",\"compilerVersion\":\"{s}\",\"signedAt\":{d}," ++
            "\"contractSha256\":\"{s}\",\"bytecodeSha256\":\"{s}\",\"policySha256\":\"{s}\"," ++
            "\"capabilityHash\":\"{s}\",\"routesCount\":{d},\"propertySummary\":\"{s}\"}}\n",
        .{
            url,
            &result.fingerprint_hex,
            result.claims.compiler_version,
            result.claims.signed_at_unix,
            result.claims.contract_sha256,
            result.claims.bytecode_sha256,
            result.claims.policy_sha256,
            result.claims.capability_hash,
            result.claims.routes_count,
            result.claims.property_summary,
        },
    ) catch return;
    _ = std.c.write(std.c.STDOUT_FILENO, out.ptr, out.len);
}

fn writeStderr(msg: []const u8) void {
    _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
}

fn writeStderrFmt(comptime fmt: []const u8, args: anytype) void {
    var buf: [512]u8 = undefined;
    const line = std.fmt.bufPrint(&buf, fmt, args) catch return;
    _ = std.c.write(std.c.STDERR_FILENO, line.ptr, line.len);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "parseArgs: url only" {
    const opts = try parseArgs(&.{"http://example.com/"});
    try std.testing.expectEqualStrings("http://example.com/", opts.url);
    try std.testing.expect(opts.trust_key_fingerprint_hex == null);
    try std.testing.expect(!opts.json_output);
}

test "parseArgs: url with --json and --trust-key" {
    const opts = try parseArgs(&.{
        "https://example.com/api",
        "--json",
        "--trust-key",
        "a" ** 64,
    });
    try std.testing.expectEqualStrings("https://example.com/api", opts.url);
    try std.testing.expect(opts.json_output);
    try std.testing.expectEqualStrings("a" ** 64, opts.trust_key_fingerprint_hex.?);
}

test "parseArgs: missing url errors" {
    try std.testing.expectError(error.MissingArgument, parseArgs(&.{}));
    try std.testing.expectError(error.MissingArgument, parseArgs(&.{"--json"}));
}

test "parseArgs: trust-key must be 64 hex chars" {
    try std.testing.expectError(error.InvalidTrustKey, parseArgs(&.{ "http://x", "--trust-key", "tooShort" }));
    try std.testing.expectError(error.InvalidTrustKey, parseArgs(&.{ "http://x", "--trust-key", "z" ** 64 }));
}

test "parseArgs: --help signals via error" {
    try std.testing.expectError(error.HelpRequested, parseArgs(&.{"--help"}));
}

test "parseArgs: rejects multiple positional args" {
    try std.testing.expectError(error.TooManyArguments, parseArgs(&.{ "http://a", "http://b" }));
}
