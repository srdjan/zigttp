//! Shared `verify-paths` v1 envelope writer. See docs/zigts-expert-contract.md.
//!
//! Diagnostics from `precompile.runCheckOnly` are serialized inside the loop
//! while the per-file CheckResult is still alive, so no cross-iteration
//! borrowed strings survive into the envelope.

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;
const writeJsonString = zigts.handler_contract.writeJsonString;
const precompile = @import("precompile.zig");
const json_diag = precompile.json_diag;
const expert_meta = @import("expert_meta.zig");

pub const VerifyPathsOutcome = struct {
    ok: bool,
};

/// Run full analysis on `paths` and write the v1 JSON envelope to `writer`.
/// `scratch` is used for per-file CheckResult allocations and an internal
/// violations buffer; neither outlives this call. The writer's bytes are
/// the authoritative contract — see docs/zigts-expert-contract.md.
pub fn writeJsonEnvelope(
    scratch: std.mem.Allocator,
    writer: anytype,
    paths: []const []const u8,
) !VerifyPathsOutcome {
    const hash = rule_registry.policyHash();

    var violations_buf: std.ArrayList(u8) = .empty;
    defer violations_buf.deinit(scratch);
    var vio_aw: std.Io.Writer.Allocating = .fromArrayList(scratch, &violations_buf);
    const vw = &vio_aw.writer;

    var has_errors = false;
    var first_violation = true;

    for (paths) |path| {
        var result = precompile.runCheckOnly(scratch, path, null, true, null) catch |err| {
            if (!first_violation) try vw.writeByte(',');
            first_violation = false;
            const fake: json_diag.JsonDiagnostic = .{
                .code = "ZTS000",
                .severity = "error",
                .message = @errorName(err),
                .file = path,
                .line = 0,
                .column = 0,
                .suggestion = null,
            };
            try json_diag.writeDiagnosticJson(vw, &fake);
            has_errors = true;
            continue;
        };
        defer result.deinit(scratch);

        if (result.totalErrors() > 0) has_errors = true;

        for (result.json_diagnostics.items) |diag| {
            if (!first_violation) try vw.writeByte(',');
            first_violation = false;
            // Normalize `file` to the original path; the checker may substitute
            // a scratch path internally (see edit-simulate's temp file path).
            var with_file = diag;
            with_file.file = path;
            try json_diag.writeDiagnosticJson(vw, &with_file);
        }
    }

    violations_buf = vio_aw.toArrayList();

    try writer.print(
        "{{\"ok\":{s},\"policy_version\":\"{s}\",\"policy_hash\":\"{s}\",\"checked_files\":[",
        .{
            if (has_errors) "false" else "true",
            expert_meta.policy_version,
            hash,
        },
    );
    for (paths, 0..) |path, i| {
        if (i > 0) try writer.writeByte(',');
        try writeJsonString(writer, path);
    }
    try writer.writeAll("],\"violations\":[");
    try writer.writeAll(violations_buf.items);
    try writer.writeAll("]}\n");

    return .{ .ok = !has_errors };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "writeJsonEnvelope with empty paths emits an ok v1 envelope" {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);

    const outcome = try writeJsonEnvelope(std.testing.allocator, &aw.writer, &.{});

    buf = aw.toArrayList();
    const s = buf.items;

    try std.testing.expect(outcome.ok);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"ok\":true") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"checked_files\":[]") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"violations\":[]") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"policy_version\":\"2026.04.2\"") != null);
    try std.testing.expectEqual(@as(u8, '\n'), s[s.len - 1]);
}

test "writeJsonEnvelope on a non-existent path emits a ZTS000 envelope" {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);

    const paths = [_][]const u8{"/tmp/zigts-verify-paths-core-not-real.ts"};
    const outcome = try writeJsonEnvelope(std.testing.allocator, &aw.writer, &paths);

    buf = aw.toArrayList();
    const s = buf.items;

    try std.testing.expect(!outcome.ok);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"ok\":false") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"ZTS000\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "/tmp/zigts-verify-paths-core-not-real.ts") != null);
}
