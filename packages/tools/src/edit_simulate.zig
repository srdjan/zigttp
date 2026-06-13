//! Edit simulation engine for diff-aware violation analysis.
//!
//! Runs the full analysis pipeline on proposed file content and optionally
//! compares against the original to determine which violations are new.
//! Used by the `zigts edit-simulate` CLI subcommand and the interactive
//! expert loop.

const std = @import("std");
const zigts = @import("zigts");
const precompile = @import("precompile.zig");
const project_config_mod = @import("project_config");
const json_diag = precompile.json_diag;
const rule_error = zigts.rule_error;
const file_io = zigts.file_io;
const writeJsonString = zigts.handler_contract.writeJsonString;
const HandlerProperties = zigts.handler_contract.HandlerProperties;

pub const EditSimulateInput = struct {
    file: []const u8,
    content: []const u8,
    before: ?[]const u8 = null,
    /// SQL schema for zigttp:sql query validation. Boundary callers that own
    /// project context (the expert veto, the edit-simulate CLI) resolve it
    /// via `discoverProjectSqlSchemaPath`; when null, zigttp:sql edits fail
    /// analysis with MissingSqlSchema exactly like a schema-less `check`.
    sql_schema_path: ?[]const u8 = null,
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
    /// Behavioral properties from contract extraction. Present only when the
    /// analysis pipeline reached the contract phase (no parse or type errors).
    properties: ?HandlerProperties = null,

    pub fn deinit(self: *SimulateResult, allocator: std.mem.Allocator) void {
        for (self.violations.items) |v| {
            allocator.free(v.code);
            allocator.free(v.severity);
            allocator.free(v.message);
            if (v.help) |help| allocator.free(help);
        }
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

    var new_check = try precompile.runCheckOnly(allocator, tmp_path, input.sql_schema_path, true, null);
    defer new_check.deinit(allocator);

    var baseline_keys: ?std.AutoHashMapUnmanaged(ViolationKey, void) = null;
    defer if (baseline_keys) |*bk| bk.deinit(allocator);

    if (input.before) |before_content| {
        const before_path = try writeTempFile(allocator, input.file, before_content);
        defer {
            deleteTempFile(allocator, before_path);
            allocator.free(before_path);
        }

        var old_check = try precompile.runCheckOnly(allocator, before_path, input.sql_schema_path, true, null);
        defer old_check.deinit(allocator);

        baseline_keys = .empty;
        for (old_check.json_diagnostics.items) |diag| {
            try baseline_keys.?.put(allocator, violationKey(&diag), {});
        }
    }

    var result = SimulateResult{
        .properties = new_check.properties,
    };
    errdefer result.deinit(allocator);

    for (new_check.json_diagnostics.items) |diag| {
        const is_new = if (baseline_keys) |bk|
            !bk.contains(violationKey(&diag))
        else
            true;

        const code = try allocator.dupe(u8, diag.code);
        errdefer allocator.free(code);
        const severity = try allocator.dupe(u8, diag.severity);
        errdefer allocator.free(severity);
        const message = try allocator.dupe(u8, diag.message);
        errdefer allocator.free(message);
        const help = if (diag.suggestion) |suggestion| try allocator.dupe(u8, suggestion) else null;
        errdefer if (help) |h| allocator.free(h);

        try result.violations.append(allocator, .{
            .code = code,
            .severity = severity,
            .message = message,
            .help = help,
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
pub fn readStdinJson(allocator: std.mem.Allocator) !EditSimulateInput {
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

    const file = try allocator.dupe(u8, file_val.string);
    errdefer allocator.free(file);
    const content = try allocator.dupe(u8, content_val.string);
    errdefer allocator.free(content);
    const owned_before = if (before) |b| try allocator.dupe(u8, b) else null;
    errdefer if (owned_before) |b| allocator.free(b);

    return .{
        .file = file,
        .content = content,
        .before = owned_before,
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
    try writer.writeAll("}");
    if (result.properties) |p| {
        try writer.print(",\"properties\":{{\"pure\":{},\"read_only\":{},\"deterministic\":{},\"retry_safe\":{},\"idempotent\":{},\"state_isolated\":{},\"injection_safe\":{},\"fault_covered\":{}}}", .{
            p.pure,
            p.read_only,
            p.deterministic,
            p.retry_safe,
            p.idempotent,
            p.state_isolated,
            p.injection_safe,
            p.fault_covered,
        });
    }
    try writer.writeAll("}\n");
}

/// CLI entry point for `zigts edit-simulate`.
pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try runWithArgsWriter(allocator, argv, &aw.writer);

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }
}

fn runWithArgsWriter(allocator: std.mem.Allocator, argv: []const []const u8, writer: *std.Io.Writer) !void {
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
        if (std.mem.eql(u8, arg, "--json")) {
            // edit-simulate is JSON-by-default; accept a redundant --json so
            // IDE/CI callers that always pass it do not hit InvalidArgument.
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

    var owned_content: ?[]const u8 = null;
    defer if (owned_content) |c| allocator.free(c);
    var owned_before: ?[]const u8 = null;
    defer if (owned_before) |b| allocator.free(b);
    var owned_file: ?[]const u8 = null;
    defer if (owned_file) |f| allocator.free(f);

    var input: EditSimulateInput = if (stdin_json) blk: {
        const parsed = try readStdinJson(allocator);
        owned_file = parsed.file;
        owned_content = parsed.content;
        if (parsed.before) |b| owned_before = b;
        break :blk parsed;
    } else blk: {
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

    // CLI boundary: resolve the project's SQL schema once per invocation so
    // zigttp:sql edits validate the way `zigttp dev`/`test` validate them.
    const discovered_schema = discoverProjectSqlSchemaPath(allocator, null);
    defer if (discovered_schema) |p| allocator.free(p);
    input.sql_schema_path = discovered_schema;

    var result = try simulate(allocator, input);
    defer result.deinit(allocator);

    try writeResultJson(writer, &result);
}

/// Resolve the project's SQL schema for analysis: the `sqlite` entry in the
/// nearest `zigttp.json` walking up from `start_path` (or cwd when null),
/// resolved against the project root. This is the same source `zigttp dev`,
/// `zigttp test`, and `zigttp doctor` pass to the analyzer. Returns null when
/// there is no project, no `sqlite` entry, or the manifest cannot be read:
/// a broken zigttp.json degrades to schema-less analysis rather than failing
/// the edit. Caller frees the returned slice.
pub fn discoverProjectSqlSchemaPath(allocator: std.mem.Allocator, start_path: ?[]const u8) ?[]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var project = project_config_mod.discover(allocator, io, start_path) catch return null;
    defer if (project) |*p| p.deinit(allocator);
    if (project) |*cfg| {
        return cfg.resolvedSqlitePath(allocator) catch null;
    }
    return null;
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

    var rand_bytes: [8]u8 = undefined;
    fillRandom(&rand_bytes);
    const rand_int = std.mem.readInt(u64, &rand_bytes, .little);
    const tmp_path = try std.fmt.allocPrint(allocator, "/tmp/zigts-edit-sim-{x:0>16}{s}", .{
        rand_int,
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
        \\zigttp:sql queries validate against the project's SQL schema, discovered
        \\from the "sqlite" entry in the nearest zigttp.json.
        \\
        \\Output: JSON with violations array and summary (total, new, preexisting).
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

/// Fill `buf` with cryptographically random bytes via /dev/urandom.
/// Falls back to leaving `buf` zeroed only if /dev/urandom is unavailable
/// (which should never happen on any supported platform).
fn fillRandom(buf: []u8) void {
    const fd = std.c.open("/dev/urandom", .{ .ACCMODE = .RDONLY }, @as(std.c.mode_t, 0));
    if (fd < 0) return;
    defer _ = std.c.close(fd);
    var filled: usize = 0;
    while (filled < buf.len) {
        const n = std.c.read(fd, buf[filled..].ptr, buf.len - filled);
        if (n < 0) {
            if (std.c.errno(n) == .INTR) continue;
            return;
        }
        if (n == 0) return;
        filled += @intCast(n);
    }
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
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"properties\"") == null);
}

test "writeResultJson includes properties when present" {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);

    const result = SimulateResult{
        .properties = .{
            .pure = false,
            .read_only = true,
            .stateless = false,
            .retry_safe = true,
            .deterministic = true,
            .has_egress = false,
            .injection_safe = true,
            .state_isolated = true,
            .fault_covered = false,
        },
    };
    try writeResultJson(&aw.writer, &result);

    buf = aw.toArrayList();
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"properties\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"deterministic\":true") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"injection_safe\":true") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"state_isolated\":true") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"fault_covered\":false") != null);
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

test "runWithArgs accepts redundant --json flag" {
    const allocator = std.testing.allocator;

    const handler =
        \\function handler(req: Request): Response & Spec<"state_isolated"> {
        \\  return Response.json({ ok: true });
        \\}
    ;
    var uniquifier: u8 = undefined;
    const addr = @intFromPtr(&uniquifier);
    const path = try std.fmt.allocPrint(allocator, "/tmp/zigts-edit-sim-json-{x}.ts", .{addr});
    defer allocator.free(path);
    try file_io.writeFile(allocator, path, handler);
    defer deleteTempFile(allocator, path);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    // The redundant --json must be accepted (not error.InvalidArgument).
    try runWithArgsWriter(allocator, &.{ path, "--json" }, &aw.writer);

    buf = aw.toArrayList();
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"violations\"") != null);
}

test "simulate flags newly introduced canonical diagnostics" {
    const before =
        \\function parse(x: number): number { return x; }
        \\function handler(req: Request): Response & Spec<"state_isolated"> {
        \\  const a = parse(1);
        \\  const b = parse(2);
        \\  return Response.json({ a, b });
        \\}
    ;
    const after =
        \\const parse = (x: number): number => x;
        \\function handler(req: Request): Response & Spec<"state_isolated"> {
        \\  const a = parse(1);
        \\  const b = parse(2);
        \\  return Response.json({ a, b });
        \\}
    ;

    var result = try simulate(std.testing.allocator, .{
        .file = "handler.ts",
        .content = after,
        .before = before,
    });
    defer result.deinit(std.testing.allocator);

    var saw_608 = false;
    for (result.violations.items) |v| {
        if (std.mem.eql(u8, v.code, "ZTS608")) {
            saw_608 = true;
            try std.testing.expect(v.introduced_by_patch);
        }
    }
    try std.testing.expect(saw_608);
    try std.testing.expect(result.new_count > 0);
}
