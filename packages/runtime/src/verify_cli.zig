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
        } else if (std.mem.startsWith(u8, arg, "--trust-key=")) {
            // Accept the `--trust-key=<hex>` form for parity with expert/deploy.
            trust_key = arg["--trust-key=".len..];
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
        // Hex is case-insensitive, so compare case-insensitively: an uppercase
        // or mixed-case --trust-key copy of the (lowercase) fingerprint must
        // still match. A genuinely different key still fails closed.
        if (!std.ascii.eqlIgnoreCase(expected, &result.fingerprint_hex)) {
            writeStderrFmt(
                "verify: key fingerprint does not match --trust-key\n  expected: {s}\n  actual:   {s}\n",
                .{ expected, result.fingerprint_hex },
            );
            return exit_key_mismatch;
        }
    }

    if (opts.json_output) {
        try writeClaimsJson(allocator, opts.url, &result);
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
        \\  durable workflow: proof={s}, retrySafe={s}, idempotent={s}, faultCovered={s}
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
            result.claims.durable_workflow_proof_level,
            boolString(result.claims.durable_workflow_retry_safe),
            boolString(result.claims.durable_workflow_idempotent),
            boolString(result.claims.durable_workflow_fault_covered),
        },
    ) catch return;
    _ = std.c.write(std.c.STDOUT_FILENO, out.ptr, out.len);
}

fn boolString(value: bool) []const u8 {
    return if (value) "true" else "false";
}

fn renderClaimsJson(allocator: std.mem.Allocator, url: []const u8, result: *const envelope.VerifyResult) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };

    try json.beginObject();
    try json.objectField("url");
    try json.write(url);
    try json.objectField("keyFingerprint");
    try json.write(&result.fingerprint_hex);
    try json.objectField("compilerVersion");
    try json.write(result.claims.compiler_version);
    try json.objectField("signedAt");
    try json.write(result.claims.signed_at_unix);
    try json.objectField("contractSha256");
    try json.write(result.claims.contract_sha256);
    try json.objectField("bytecodeSha256");
    try json.write(result.claims.bytecode_sha256);
    try json.objectField("policySha256");
    try json.write(result.claims.policy_sha256);
    try json.objectField("capabilityHash");
    try json.write(result.claims.capability_hash);
    try json.objectField("routesCount");
    try json.write(result.claims.routes_count);
    try json.objectField("propertySummary");
    try json.write(result.claims.property_summary);
    try json.objectField("durableWorkflowProofLevel");
    try json.write(result.claims.durable_workflow_proof_level);
    try json.objectField("durableWorkflowRetrySafe");
    try json.write(result.claims.durable_workflow_retry_safe);
    try json.objectField("durableWorkflowIdempotent");
    try json.write(result.claims.durable_workflow_idempotent);
    try json.objectField("durableWorkflowFaultCovered");
    try json.write(result.claims.durable_workflow_fault_covered);
    try json.endObject();
    try aw.writer.writeByte('\n');

    return try allocator.dupe(u8, aw.writer.buffered());
}

fn writeClaimsJson(allocator: std.mem.Allocator, url: []const u8, result: *const envelope.VerifyResult) !void {
    const out = try renderClaimsJson(allocator, url, result);
    defer allocator.free(out);
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

test "renderClaimsJson uses JSON string escaping" {
    const testing = std.testing;

    var result = envelope.VerifyResult{
        .claims = .{
            .contract_sha256 = "a" ** 64,
            .bytecode_sha256 = "b" ** 64,
            .policy_sha256 = "c" ** 64,
            .capability_hash = "d" ** 64,
            .compiler_version = "zigttp\"dev\\test",
            .signed_at_unix = 1_700_000_000,
            .property_summary = "quote\" slash\\ newline\n",
            .routes_count = 2,
            .durable_workflow_proof_level = "partial",
            .durable_workflow_retry_safe = false,
            .durable_workflow_idempotent = true,
            .durable_workflow_fault_covered = false,
        },
        .public_key = [_]u8{0} ** std.crypto.sign.Ed25519.PublicKey.encoded_length,
        .fingerprint_hex = [_]u8{'f'} ** 64,
        .arena = std.heap.ArenaAllocator.init(testing.allocator),
    };
    defer result.deinit();

    const out = try renderClaimsJson(testing.allocator, "https://example.test/a?x=\"y\"", &result);
    defer testing.allocator.free(out);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, out, .{});
    defer parsed.deinit();
    const root = parsed.value.object;

    try testing.expectEqualStrings("https://example.test/a?x=\"y\"", root.get("url").?.string);
    try testing.expectEqualStrings("zigttp\"dev\\test", root.get("compilerVersion").?.string);
    try testing.expectEqualStrings("quote\" slash\\ newline\n", root.get("propertySummary").?.string);
    try testing.expectEqualStrings("partial", root.get("durableWorkflowProofLevel").?.string);
    try testing.expect(!root.get("durableWorkflowRetrySafe").?.bool);
    try testing.expect(root.get("durableWorkflowIdempotent").?.bool);
    try testing.expect(!root.get("durableWorkflowFaultCovered").?.bool);
    try testing.expectEqual(@as(i64, 1_700_000_000), root.get("signedAt").?.integer);
    try testing.expectEqual(@as(i64, 2), root.get("routesCount").?.integer);
    try testing.expect(std.mem.indexOf(u8, out, "\\\"") != null);
    try testing.expect(std.mem.endsWith(u8, out, "\n"));
}
