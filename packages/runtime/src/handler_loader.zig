//! Shared loader for `ServerConfig.handler` sources.
//!
//! Replay, handler-test, and durable-recovery runners share this loader so
//! the embedded_bytecode and appended_payload branches are declared once.
//! Callers own their diagnostics on the error path.

const std = @import("std");
const zq = @import("zigts");
const server = @import("server.zig");

pub const LoadedHandler = struct {
    /// Owned buffer holding the handler source code. Free with `allocator.free`.
    code: []const u8,
    /// Filename for error messages. Points into the config for `file_path`,
    /// or the static string "eval" for inline code.
    filename: []const u8,
};

pub fn load(
    allocator: std.mem.Allocator,
    handler: server.HandlerSource,
) !LoadedHandler {
    return switch (handler) {
        .file_path => |path| .{
            .code = try zq.file_io.readFile(allocator, path, 100 * 1024 * 1024),
            .filename = path,
        },
        .inline_code => |code| .{
            .code = try allocator.dupe(u8, code),
            .filename = "eval",
        },
        .embedded_bytecode, .appended_payload => error.UnsupportedHandlerSource,
    };
}

test "load inline_code returns duped buffer and eval filename" {
    const allocator = std.testing.allocator;
    const source = "function handler(r) { return Response.json({ok:true}); }";

    const loaded = try load(allocator, .{ .inline_code = source });
    defer allocator.free(loaded.code);

    try std.testing.expectEqualStrings(source, loaded.code);
    try std.testing.expectEqualStrings("eval", loaded.filename);
    // Returned buffer is a separate allocation; callers free unconditionally.
    try std.testing.expect(loaded.code.ptr != source.ptr);
}

test "load embedded_bytecode returns UnsupportedHandlerSource" {
    const stub = [_]u8{ 0x00, 0x01, 0x02 };
    const result = load(std.testing.allocator, .{ .embedded_bytecode = &stub });
    try std.testing.expectError(error.UnsupportedHandlerSource, result);
}

test "load appended_payload returns UnsupportedHandlerSource" {
    const payload: server.AppendedPayload = .{
        .bytecode = &.{},
        .dep_bytecodes = &.{},
        .contract_json = null,
    };
    const result = load(std.testing.allocator, .{ .appended_payload = payload });
    try std.testing.expectError(error.UnsupportedHandlerSource, result);
}
