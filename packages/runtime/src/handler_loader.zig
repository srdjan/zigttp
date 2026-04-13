//! Shared loader for `ServerConfig.handler` sources.
//!
//! Replay, handler-test, and durable-recovery runners all need to turn a
//! `HandlerSource` union into raw JS code plus a filename label. Previously
//! each runner duplicated the switch, which meant the three `not yet supported`
//! branches had to be kept in sync by hand. This module is the single source
//! of truth.

const std = @import("std");
const builtin = @import("builtin");
const zq = @import("zigts");
const server = @import("server.zig");

/// Loaded handler code plus a display filename for diagnostics.
pub const LoadedHandler = struct {
    /// Owned buffer holding the handler source code. Free with `allocator.free`.
    code: []const u8,
    /// Filename for error messages. Points into the config for `file_path`,
    /// or the static string "eval" for inline code.
    filename: []const u8,
};

/// Load handler source from a `server.HandlerSource` variant.
///
/// `label` is a comptime caller name ("Replay", "Testing", "Durable recovery")
/// used in diagnostics when an unsupported variant is encountered.
pub fn load(
    allocator: std.mem.Allocator,
    handler: server.HandlerSource,
    comptime label: []const u8,
) !LoadedHandler {
    return switch (handler) {
        .file_path => |path| .{
            .code = zq.file_io.readFile(allocator, path, 100 * 1024 * 1024) catch |err| {
                if (!builtin.is_test) std.log.err("Failed to read handler '{s}': {}", .{ path, err });
                return err;
            },
            .filename = path,
        },
        .inline_code => |code| .{
            .code = try allocator.dupe(u8, code),
            .filename = "eval",
        },
        .embedded_bytecode => {
            if (!builtin.is_test) std.log.err(label ++ " with embedded bytecode not yet supported", .{});
            return error.UnsupportedHandlerSource;
        },
        .appended_payload => {
            if (!builtin.is_test) std.log.err(label ++ " with appended payload not yet supported", .{});
            return error.UnsupportedHandlerSource;
        },
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "load inline_code returns duped buffer and eval filename" {
    const allocator = std.testing.allocator;
    const source = "function handler(r) { return Response.json({ok:true}); }";

    const loaded = try load(allocator, .{ .inline_code = source }, "Testing");
    defer allocator.free(loaded.code);

    try std.testing.expectEqualStrings(source, loaded.code);
    try std.testing.expectEqualStrings("eval", loaded.filename);
    // The returned buffer must be a separate allocation, not an alias into the
    // input slice — callers free it unconditionally.
    try std.testing.expect(loaded.code.ptr != source.ptr);
}

// The file_path happy path is exercised end-to-end by every example handler
// run via scripts/test-examples.sh. We skip a unit test for it here to avoid
// chasing std.fs.Dir API drift across Zig nightlies; the control flow is
// covered by the other three cases and the wire-up by integration.

test "load embedded_bytecode returns UnsupportedHandlerSource" {
    const allocator = std.testing.allocator;
    const stub = [_]u8{ 0x00, 0x01, 0x02 };

    const result = load(allocator, .{ .embedded_bytecode = &stub }, "Replay");
    try std.testing.expectError(error.UnsupportedHandlerSource, result);
}

test "load appended_payload returns UnsupportedHandlerSource" {
    const allocator = std.testing.allocator;
    const payload: server.AppendedPayload = .{
        .bytecode = &.{},
        .dep_bytecodes = &.{},
        .contract_json = null,
    };

    const result = load(allocator, .{ .appended_payload = payload }, "Durable recovery");
    try std.testing.expectError(error.UnsupportedHandlerSource, result);
}
