//! Edit simulation engine for diff-aware violation analysis.
//!
//! Runs the full analysis pipeline on proposed file content and optionally
//! compares against the original to determine which violations are new.
//! Used by the `zigts edit-simulate` CLI subcommand and Claude Code hooks.

const std = @import("std");
const zigts = @import("zigts");
const precompile = @import("precompile.zig");
const json_diag = precompile.json_diag;
const rule_error = zigts.rule_error;
const file_io = zigts.file_io;
const writeJsonString = zigts.handler_contract.writeJsonString;

pub const EditSimulateInput = struct {
    file: []const u8,
    content: []const u8,
    before: ?[]const u8 = null,
};

pub const SimulatedViolation = struct {
    code: []const u8,
    severity: []const u8,
    message: []const u8,
    help: ?[]const u8,
    line: u32,
    column: u16,
    introduced_by_patch: bool,
};

pub const SimulateResult = struct {
    violations: std.ArrayListUnmanaged(SimulatedViolation) = .empty,
    total: u32 = 0,
    new_count: u32 = 0,
    preexisting_count: u32 = 0,

    pub fn deinit(self: *SimulateResult, allocator: std.mem.Allocator) void {
        self.violations.deinit(allocator);
    }
};

/// Run analysis on proposed content and optionally diff against before content.
pub fn simulate(
    allocator: std.mem.Allocator,
    input: EditSimulateInput,
) !SimulateResult {
    const tmp_path = try writeTempFile(allocator, input.file, input.content);
    defer {
        deleteTempFile(allocator, tmp_path);
        allocator.free(tmp_path);
    }

    var new_check = try precompile.runCheckOnly(allocator, tmp_path, null, true);
    defer new_check.deinit(allocator);

    var baseline_keys: ?std.AutoHashMapUnmanaged(ViolationKey, void) = null;
    defer if (baseline_keys) |*bk| bk.deinit(allocator);

    if (input.before) |before_content| {
        const before_path = try writeTempFile(allocator, input.file, before_content);
        defer {
            deleteTempFile(allocator, before_path);
            allocator.free(before_path);
        }

        var old_check = try precompile.runCheckOnly(allocator, before_path, null, true);
        defer old_check.deinit(allocator);

        baseline_keys = .empty;
        for (old_check.json_diagnostics.items) |diag| {
            try baseline_keys.?.put(allocator, violationKey(&diag), {});
        }
    }

    var result = SimulateResult{};
    errdefer result.deinit(allocator);

    for (new_check.json_diagnostics.items) |diag| {
        const is_new = if (baseline_keys) |bk|
            !bk.contains(violationKey(&diag))
        else
            true;

        try result.violations.append(allocator, .{
            .code = diag.code,
            .severity = diag.severity,
            .message = diag.message,
            .help = diag.suggestion,
            .line = diag.line,
            .column = diag.column,
            .introduced_by_patch = is_new,
        });

        result.total += 1;
        if (is_new) {
            result.new_count += 1;
        } else {
            result.preexisting_count += 1;
        }
    }

    return result;
}

/// Read EditSimulateInput from stdin JSON.
pub fn readStdinJson(allocator: std.mem.Allocator) rule_error.RuleError!EditSimulateInput {
    const stdin_data = readAllStdin(allocator) catch return error.StdinReadFailed;
    defer allocator.free(stdin_data);

    if (stdin_data.len == 0) return error.StdinReadFailed;

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, stdin_data, .{}) catch
        return error.JsonParseError;
    defer parsed.deinit();

    if (parsed.value != .object) return error.JsonParseError;
    const obj = parsed.value.object;

    const file_val = obj.get("file") orelse return error.InvalidInput;
    const content_val = obj.get("content") orelse return error.InvalidInput;
    if (file_val != .string or content_val != .string) return error.InvalidInput;

    const before_val = obj.get("before");
    const before: ?[]const u8 = if (before_val) |bv|
        (if (bv == .string) bv.string else null)
    else
        null;

    return .{
        .file = file_val.string,
        .content = content_val.string,
        .before = before,
    };
}

/// Write SimulateResult as JSON to writer.
pub fn writeResultJson(writer: anytype, result: *const SimulateResult) !void {
    try writer.writeAll("{\"violations\":[");
    for (result.violations.items, 0..) |v, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("{\"code\":");
        try writeJsonString(writer, v.code);
        try writer.writeAll(",\"severity\":");
        try writeJsonString(writer, v.severity);
        try writer.writeAll(",\"message\":");
        try writeJsonString(writer, v.message);
        try writer.writeAll(",\"line\":");
        try writer.print("{d}", .{v.line});
        try writer.writeAll(",\"column\":");
        try writer.print("{d}", .{v.column});
        try writer.writeAll(",\"introduced_by_patch\":");
        try writer.writeAll(if (v.introduced_by_patch) "true" else "false");
        if (v.help) |h| {
            try writer.writeAll(",\"suggestion\":");
            try writeJsonString(writer, h);
        }
        try writer.writeAll("}");
    }
    try writer.writeAll("],\"summary\":{\"total\":");
    try writer.print("{d}", .{result.total});
    try writer.writeAll(",\"new\":");
    try writer.print("{d}", .{result.new_count});
    try writer.writeAll(",\"preexisting\":");
    try writer.print("{d}", .{result.preexisting_count});
    try writer.writeAll("}}\n");
}

/// CLI entry point for `zigts edit-simulate`.
pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var stdin_json = false;
    var handler_path: ?[]const u8 = null;
    var before_path: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--stdin-json")) {
            stdin_json = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--before")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            before_path = argv[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
        if (!std.mem.startsWith(u8, arg, "-") and handler_path == null) {
            handler_path = arg;
            continue;
        }
        return error.InvalidArgument;
    }

    var owned_content: ?[]u8 = null;
    defer if (owned_content) |c| allocator.free(c);
    var owned_before: ?[]u8 = null;
    defer if (owned_before) |b| allocator.free(b);

    const input: EditSimulateInput = if (stdin_json)
        try readStdinJson(allocator)
    else blk: {
        const path = handler_path orelse return error.MissingArgument;
        owned_content = try file_io.readFile(allocator, path, 10 * 1024 * 1024);
        if (before_path) |bp| {
            owned_before = try file_io.readFile(allocator, bp, 10 * 1024 * 1024);
        }
        break :blk .{
            .file = path,
            .content = owned_content.?,
            .before = owned_before,
        };
    };

    var result = try simulate(allocator, input);
    defer result.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try writeResultJson(&aw.writer, &result);

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Value-type composite key for comparing violations.
/// Uses Wyhash for fast non-cryptographic hashing. Line numbers are excluded
/// because edits shift them.
/// TODO(v0.1): This is a heuristic. Identical messages at different locations
/// will be treated as the same violation.
const ViolationKey = struct {
    code_hash: u64,
    msg_hash: u64,
};

fn violationKey(diag: *const json_diag.JsonDiagnostic) ViolationKey {
    return .{
        .code_hash = std.hash.Wyhash.hash(0, diag.code),
        .msg_hash = std.hash.Wyhash.hash(0, diag.message),
    };
}

fn deleteTempFile(allocator: std.mem.Allocator, path: []const u8) void {
    const path_z = allocator.dupeZ(u8, path) catch return;
    defer allocator.free(path_z);
    _ = std.c.unlink(path_z);
}

fn writeTempFile(allocator: std.mem.Allocator, original_name: []const u8, content: []const u8) ![]u8 {
    const ext = blk: {
        if (std.mem.lastIndexOf(u8, original_name, ".")) |dot_idx| {
            break :blk original_name[dot_idx..];
        }
        break :blk ".ts";
    };

    var uniquifier: u8 = undefined;
    const addr = @intFromPtr(&uniquifier);
    const tmp_path = try std.fmt.allocPrint(allocator, "/tmp/zigts-edit-sim-{x}{s}", .{
        addr,
        ext,
    });
    errdefer allocator.free(tmp_path);

    try file_io.writeFile(allocator, tmp_path, content);
    return tmp_path;
}

fn readAllStdin(allocator: std.mem.Allocator) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);

    var read_buf: [4096]u8 = undefined;
    while (true) {
        const n = std.posix.read(std.posix.STDIN_FILENO, &read_buf) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) break;
        try buf.appendSlice(allocator, read_buf[0..n]);
    }
    return buf.toOwnedSlice(allocator);
}

fn printHelp() void {
    const help =
        \\zigts edit-simulate - simulate an edit and report violations
        \\
        \\Usage:
        \\  zigts edit-simulate [handler.ts] [--before old.ts] [--stdin-json]
        \\
        \\Options:
        \\  --stdin-json    Read input as JSON from stdin: {"file", "content", "before"?}
        \\  --before FILE   Original file for diff-aware analysis
        \\
        \\Output: JSON with violations array and summary (total, new, preexisting).
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "writeResultJson empty result" {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);

    const result = SimulateResult{};
    try writeResultJson(&aw.writer, &result);

    buf = aw.toArrayList();
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"total\":0") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"violations\":[]") != null);
}

test "violationKey excludes line numbers" {
    const diag1 = json_diag.JsonDiagnostic{
        .code = "ZTS303",
        .severity = "error",
        .message = "unchecked result",
        .file = "handler.ts",
        .line = 10,
        .column = 4,
        .suggestion = null,
    };
    const diag2 = json_diag.JsonDiagnostic{
        .code = "ZTS303",
        .severity = "error",
        .message = "unchecked result",
        .file = "handler.ts",
        .line = 20,
        .column = 4,
        .suggestion = null,
    };

    const key1 = violationKey(&diag1);
    const key2 = violationKey(&diag2);

    try std.testing.expectEqual(key1.code_hash, key2.code_hash);
    try std.testing.expectEqual(key1.msg_hash, key2.msg_hash);
}
