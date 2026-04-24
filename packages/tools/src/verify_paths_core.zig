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

pub const Diagnostic = json_diag.JsonDiagnostic;

/// Walk `paths`, run the full analysis per file, and invoke `ctx.emit(*const
/// Diagnostic)` for every violation (including synthesized ZTS000 entries for
/// files that fail to load). The diagnostic's string fields are only
/// guaranteed valid for the duration of the `emit` call; consumers that need
/// to persist them must copy inside the callback. `file` is normalized to the
/// caller-supplied argv path so temp-file checker paths stay invisible.
pub fn collect(
    scratch: std.mem.Allocator,
    paths: []const []const u8,
    ctx: anytype,
) !VerifyPathsOutcome {
    var has_errors = false;
    for (paths) |path| {
        var result = precompile.runCheckOnly(scratch, path, null, true, null) catch |err| {
            const fake: Diagnostic = .{
                .code = "ZTS000",
                .severity = "error",
                .message = @errorName(err),
                .file = path,
                .line = 0,
                .column = 0,
                .suggestion = null,
            };
            try ctx.emit(&fake);
            has_errors = true;
            continue;
        };
        defer result.deinit(scratch);

        if (result.totalErrors() > 0) has_errors = true;

        for (result.json_diagnostics.items) |diag| {
            var with_file = diag;
            with_file.file = path;
            try ctx.emit(&with_file);
        }
    }
    return .{ .ok = !has_errors };
}

/// Run full analysis on `paths` and write the v1 JSON envelope to `writer`.
/// `scratch` is used for per-file CheckResult allocations and an internal
/// violations buffer; neither outlives this call. The writer's bytes are
/// the authoritative contract — see docs/zigts-expert-contract.md.
pub fn writeJsonEnvelope(
    scratch: std.mem.Allocator,
    writer: anytype,
    paths: []const []const u8,
) !VerifyPathsOutcome {
    var violations_buf: std.ArrayList(u8) = .empty;
    defer violations_buf.deinit(scratch);
    var vio_aw: std.Io.Writer.Allocating = .fromArrayList(scratch, &violations_buf);

    var emitter = JsonEmitter{ .writer = &vio_aw.writer, .first = true };
    const outcome = try collect(scratch, paths, &emitter);

    violations_buf = vio_aw.toArrayList();

    try writer.print(
        "{{\"ok\":{s},\"policy_version\":\"{s}\",\"policy_hash\":\"{s}\",\"checked_files\":[",
        .{
            if (outcome.ok) "true" else "false",
            expert_meta.policy_version,
            rule_registry.policyHash(),
        },
    );
    for (paths, 0..) |path, i| {
        if (i > 0) try writer.writeByte(',');
        try writeJsonString(writer, path);
    }
    try writer.writeAll("],\"violations\":[");
    try writer.writeAll(violations_buf.items);
    try writer.writeAll("]}\n");

    return outcome;
}

const JsonEmitter = struct {
    writer: *std.Io.Writer,
    first: bool,

    pub fn emit(self: *JsonEmitter, diag: *const Diagnostic) !void {
        if (!self.first) try self.writer.writeByte(',');
        self.first = false;
        try json_diag.writeDiagnosticJson(self.writer, diag);
    }
};

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

test "writeJsonEnvelope surfaces ZTS400 for a flow violation" {
    // The agent-facing `verify-paths` surface must include flow_checker
    // diagnostics, not only the bool/type/verifier cascade. Before flow
    // diagnostics were wired into json_diagnostics, this same handler
    // would produce `"ok":true,"violations":[]`.
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const fixture =
        \\import { env } from "zigttp:env";
        \\function handler(req) {
        \\  const secret = env("SECRET_KEY");
        \\  if (secret) {
        \\    return Response.json({ leaked: secret });
        \\  }
        \\  return Response.json({ ok: true });
        \\}
    ;
    try tmp_dir.dir.writeFile(.{ .sub_path = "leak.ts", .data = fixture });

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = try tmp_dir.dir.realpath("leak.ts", &path_buf);
    const paths = [_][]const u8{abs_path};

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const outcome = try writeJsonEnvelope(allocator, &aw.writer, &paths);
    buf = aw.toArrayList();
    const s = buf.items;

    try std.testing.expect(!outcome.ok);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"ok\":false") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "\"ZTS400\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "secret data flows into response body") != null);
}
