//! Executes each `IntentAssertion` extracted by `intent_extractor.zig`
//! against the live handler bytecode and reports pass/fail per assertion.
//! Lives outside the proof boundary - assertions are runnable examples,
//! not new compiler obligations.
//!
//! Subset semantics for `expectedBodyJson`:
//! - JSON objects match if every key in the expected value appears in
//!   the actual value with a matching value (recursive).
//! - JSON arrays must match exactly (length + per-element).
//! - Primitives must be equal.
//!
//! Header semantics: each expected header must appear in the response
//! with case-insensitive name match and exact value match.

const std = @import("std");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const precompile = zigts_cli.precompile;
const Runtime = @import("zruntime.zig").Runtime;
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;
const HttpRequestView = @import("http_types.zig").HttpRequestView;
const HttpHeader = @import("http_types.zig").HttpHeader;
const HttpResponse = @import("http_types.zig").HttpResponse;
const handler_loader = @import("handler_loader.zig");
const runtime_natives = @import("runtime_natives.zig");
const trace = zigts.trace;

const HandlerContract = zigts.handler_contract.HandlerContract;
const IntentAssertion = zigts.handler_contract.IntentAssertion;

pub const RunSummary = struct {
    total: usize,
    passed: usize,
    failed: usize,
};

pub const RunError = error{
    NoIntentDeclared,
    DynamicIntent,
    AssertionsFailed,
};

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len == 0) {
        try writeStderr("usage: zigttp assert-intent <handler.ts>\n");
        return error.MissingArgument;
    }

    var handler_path: ?[]const u8 = null;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            const help =
                \\zigttp assert-intent - run author-declared intent assertions
                \\
                \\Usage: zigttp assert-intent <handler.ts>
                \\
                \\Executes each `intent.assertions` entry declared in the handler
                \\against the live bytecode. Subset matching on response body.
                \\Exit code: 0 = all assertions pass, 1 = at least one failed,
                \\2 = no intent declared, 3 = intent extraction was dynamic.
                \\
            ;
            try writeStdout(help);
            return;
        }
        if (!std.mem.startsWith(u8, arg, "-") and handler_path == null) {
            handler_path = arg;
            continue;
        }
        try writeStderr("invalid argument\n");
        return error.InvalidArgument;
    }

    const path = handler_path orelse {
        try writeStderr("missing handler path\n");
        return error.MissingArgument;
    };

    const summary = run(allocator, path) catch |err| switch (err) {
        error.NoIntentDeclared => {
            try writeStderr("no intent declared in handler\n");
            std.process.exit(2);
        },
        error.DynamicIntent => {
            try writeStderr("intent extraction was dynamic; rewrite as a static literal\n");
            std.process.exit(3);
        },
        else => return err,
    };

    var buf: [256]u8 = undefined;
    const line = std.fmt.bufPrint(&buf, "\n{d} passed, {d} failed, {d} total\n", .{
        summary.passed, summary.failed, summary.total,
    }) catch return;
    try writeStdout(line);

    if (summary.failed > 0) std.process.exit(1);
}

/// Programmatic entry. Returns the per-run summary or one of the
/// `RunError` variants. Prints per-assertion pass/fail lines to stdout.
pub fn run(allocator: std.mem.Allocator, handler_path: []const u8) !RunSummary {
    var check = try precompile.runCheckOnly(allocator, handler_path, null, false, null);
    defer check.deinit(allocator);

    const contract = if (check.contract) |c| c else return error.NoIntentDeclared;
    const intent = contract.intent orelse return error.NoIntentDeclared;
    if (intent.dynamic) return error.DynamicIntent;
    if (intent.assertions.items.len == 0) return error.NoIntentDeclared;

    const loaded = try handler_loader.load(allocator, .{ .file_path = handler_path });
    defer allocator.free(loaded.code);

    var passed: usize = 0;
    var failed: usize = 0;

    for (intent.assertions.items) |*assertion| {
        const result = runOne(allocator, loaded.code, loaded.filename, assertion) catch |err| AssertionResult{
            .pass = false,
            .errors = .empty,
            .runtime_error = err,
        };
        var r = result;
        defer r.deinit(allocator);

        if (r.pass) {
            passed += 1;
            var buf: [256]u8 = undefined;
            const line = std.fmt.bufPrint(&buf, "  PASS  {s}\n", .{assertion.name}) catch continue;
            try writeStdout(line);
        } else {
            failed += 1;
            var buf: [256]u8 = undefined;
            const line = std.fmt.bufPrint(&buf, "  FAIL  {s}\n", .{assertion.name}) catch continue;
            try writeStdout(line);
            if (r.runtime_error) |err| {
                var ebuf: [128]u8 = undefined;
                const eline = std.fmt.bufPrint(&ebuf, "        error: {}\n", .{err}) catch continue;
                try writeStdout(eline);
            }
            for (r.errors.items) |msg| {
                try writeStdout("        ");
                try writeStdout(msg);
                try writeStdout("\n");
            }
        }
    }

    return .{
        .total = intent.assertions.items.len,
        .passed = passed,
        .failed = failed,
    };
}

const AssertionResult = struct {
    pass: bool,
    errors: std.ArrayList([]u8),
    runtime_error: ?anyerror = null,

    fn deinit(self: *AssertionResult, allocator: std.mem.Allocator) void {
        for (self.errors.items) |e| allocator.free(e);
        self.errors.deinit(allocator);
    }
};

fn runOne(
    allocator: std.mem.Allocator,
    handler_code: []const u8,
    handler_filename: []const u8,
    assertion: *const IntentAssertion,
) !AssertionResult {
    var errors: std.ArrayList([]u8) = .empty;
    errdefer {
        for (errors.items) |e| allocator.free(e);
        errors.deinit(allocator);
    }

    var config: RuntimeConfig = .{};
    config.replay_file_path = "intent";
    config.enforce_arena_escape = false;

    const rt = try Runtime.init(allocator, config);
    defer rt.deinit();

    // Empty replay state - intent assertions execute virtual modules for
    // real where possible (env, crypto, etc are stubbed by the runtime in
    // replay mode; assertions that depend on external I/O without
    // matching stubs simply observe whatever the replay layer returns).
    var replay_state = trace.ReplayState{
        .io_calls = &.{},
        .cursor = 0,
        .divergences = 0,
    };
    rt.ctx.setModuleState(
        trace.REPLAY_STATE_SLOT,
        @ptrCast(&replay_state),
        &trace.ReplayState.deinitOpaque,
    );
    defer rt.ctx.module_state[trace.REPLAY_STATE_SLOT] = null;

    try rt.loadCode(handler_code, handler_filename);

    var headers_list: std.ArrayListUnmanaged(HttpHeader) = .empty;
    defer headers_list.deinit(allocator);

    if (assertion.request_body_json) |_| {
        try headers_list.append(allocator, .{ .key = "content-type", .value = "application/json" });
    }

    // assertion.path includes any query string; split it so req.path and
    // req.query match what the live server would produce for the assertion.
    const target = runtime_natives.parseRequestTarget(allocator, assertion.path);
    defer target.deinit(allocator);

    var response = try rt.executeHandler(.{
        .method = assertion.method,
        .url = assertion.path,
        .path = target.path,
        .query_params = target.params,
        .headers = headers_list,
        .body = assertion.request_body_json,
    });
    defer response.deinit();

    if (assertion.expected_status) |expected| {
        if (response.status != expected) {
            const msg = try std.fmt.allocPrint(allocator, "expected status {d}, got {d}", .{ expected, response.status });
            try errors.append(allocator, msg);
        }
    }

    if (assertion.expected_body_json) |expected_body| {
        const mismatch = compareJsonSubset(allocator, expected_body, response.body) catch |err| {
            const msg = try std.fmt.allocPrint(allocator, "body comparison error: {}", .{err});
            try errors.append(allocator, msg);
            return .{ .pass = errors.items.len == 0, .errors = errors };
        };
        if (mismatch) |m| {
            try errors.append(allocator, m);
        }
    }

    for (assertion.expected_headers.items) |h| {
        var found = false;
        for (response.headers.items) |actual| {
            if (std.ascii.eqlIgnoreCase(actual.key, h.name) and std.mem.eql(u8, actual.value, h.value)) {
                found = true;
                break;
            }
        }
        if (!found) {
            const msg = try std.fmt.allocPrint(allocator, "missing header: {s}: {s}", .{ h.name, h.value });
            try errors.append(allocator, msg);
        }
    }

    return .{ .pass = errors.items.len == 0, .errors = errors };
}

// ---------------------------------------------------------------------------
// JSON subset comparison
//
// Returns an owned error message on mismatch, or null when the expected
// value is a subset of the actual value.
// ---------------------------------------------------------------------------

const CompareError = error{ OutOfMemory, NoSpaceLeft };

fn compareJsonSubset(allocator: std.mem.Allocator, expected: []const u8, actual: []const u8) CompareError!?[]u8 {
    var expected_parsed = std.json.parseFromSlice(std.json.Value, allocator, expected, .{}) catch {
        return try std.fmt.allocPrint(allocator, "expected body is not valid JSON: {s}", .{truncate(expected, 80)});
    };
    defer expected_parsed.deinit();
    var actual_parsed = std.json.parseFromSlice(std.json.Value, allocator, actual, .{}) catch {
        return try std.fmt.allocPrint(allocator, "actual body is not valid JSON: {s}", .{truncate(actual, 80)});
    };
    defer actual_parsed.deinit();

    return try compareValues(allocator, expected_parsed.value, actual_parsed.value, "");
}

fn displayPath(path: []const u8) []const u8 {
    return if (path.len == 0) "(root)" else path;
}

fn compareValues(allocator: std.mem.Allocator, exp: std.json.Value, act: std.json.Value, path: []const u8) CompareError!?[]u8 {
    const where = displayPath(path);
    if (@as(std.meta.Tag(std.json.Value), exp) != @as(std.meta.Tag(std.json.Value), act)) {
        return try std.fmt.allocPrint(allocator, "type mismatch at {s}: expected {s}, got {s}", .{ where, @tagName(exp), @tagName(act) });
    }

    return switch (exp) {
        .null => null,
        .bool => |b| if (b == act.bool) null else try std.fmt.allocPrint(allocator, "value mismatch at {s}: expected {}, got {}", .{ where, b, act.bool }),
        .integer => |i| if (i == act.integer) null else try std.fmt.allocPrint(allocator, "value mismatch at {s}: expected {d}, got {d}", .{ where, i, act.integer }),
        .float => |f| if (f == act.float) null else try std.fmt.allocPrint(allocator, "value mismatch at {s}: expected {d}, got {d}", .{ where, f, act.float }),
        .number_string => |s| if (std.mem.eql(u8, s, act.number_string)) null else try std.fmt.allocPrint(allocator, "value mismatch at {s}: expected {s}, got {s}", .{ where, s, act.number_string }),
        .string => |s| if (std.mem.eql(u8, s, act.string)) null else try std.fmt.allocPrint(allocator, "value mismatch at {s}: expected \"{s}\", got \"{s}\"", .{ where, s, act.string }),
        .array => |arr| try compareArray(allocator, arr, act.array, path),
        .object => |obj| try compareObject(allocator, obj, act.object, path),
    };
}

fn compareArray(allocator: std.mem.Allocator, exp: std.json.Array, act: std.json.Array, path: []const u8) CompareError!?[]u8 {
    if (exp.items.len != act.items.len) {
        return try std.fmt.allocPrint(allocator, "array length mismatch at {s}: expected {d} items, got {d}", .{ displayPath(path), exp.items.len, act.items.len });
    }
    for (exp.items, 0..) |e, i| {
        const sub = try std.fmt.allocPrint(allocator, "{s}[{d}]", .{ path, i });
        defer allocator.free(sub);
        if (try compareValues(allocator, e, act.items[i], sub)) |msg| return msg;
    }
    return null;
}

fn compareObject(allocator: std.mem.Allocator, exp: std.json.ObjectMap, act: std.json.ObjectMap, path: []const u8) CompareError!?[]u8 {
    var it = exp.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        const sub = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ path, key });
        defer allocator.free(sub);

        const actual_value = act.get(key) orelse {
            return try std.fmt.allocPrint(allocator, "missing key at {s}", .{sub});
        };
        if (try compareValues(allocator, entry.value_ptr.*, actual_value, sub)) |msg| return msg;
    }
    return null;
}

// ---------------------------------------------------------------------------
// Local IO helpers - the dev CLI writes directly via `std.c.write`. The
// runner mirrors that pattern so output is unbuffered and predictable.
// ---------------------------------------------------------------------------

fn writeStdout(s: []const u8) !void {
    _ = std.c.write(std.c.STDOUT_FILENO, s.ptr, s.len);
}

fn writeStderr(s: []const u8) !void {
    _ = std.c.write(std.c.STDERR_FILENO, s.ptr, s.len);
}

fn truncate(s: []const u8, max: usize) []const u8 {
    return if (s.len > max) s[0..max] else s;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "compareJsonSubset accepts exact match" {
    const allocator = std.testing.allocator;
    const result = try compareJsonSubset(allocator, "{\"ok\":true}", "{\"ok\":true}");
    try std.testing.expect(result == null);
}

test "compareJsonSubset accepts subset object" {
    const allocator = std.testing.allocator;
    const result = try compareJsonSubset(allocator, "{\"ok\":true}", "{\"ok\":true,\"extra\":42}");
    try std.testing.expect(result == null);
}

test "compareJsonSubset rejects missing key" {
    const allocator = std.testing.allocator;
    const maybe = try compareJsonSubset(allocator, "{\"ok\":true}", "{\"other\":1}");
    try std.testing.expect(maybe != null);
    if (maybe) |m| {
        defer allocator.free(m);
        try std.testing.expect(std.mem.indexOf(u8, m, "missing key") != null);
    }
}

test "compareJsonSubset rejects mismatched value" {
    const allocator = std.testing.allocator;
    const maybe = try compareJsonSubset(allocator, "{\"ok\":true}", "{\"ok\":false}");
    try std.testing.expect(maybe != null);
    if (maybe) |m| {
        defer allocator.free(m);
        try std.testing.expect(std.mem.indexOf(u8, m, "value mismatch") != null);
    }
}

test "compareJsonSubset enforces strict array length" {
    const allocator = std.testing.allocator;
    const maybe = try compareJsonSubset(allocator, "[1,2]", "[1,2,3]");
    try std.testing.expect(maybe != null);
    if (maybe) |m| {
        defer allocator.free(m);
        try std.testing.expect(std.mem.indexOf(u8, m, "array length mismatch") != null);
    }
}

test "compareJsonSubset recurses into nested objects" {
    const allocator = std.testing.allocator;
    const maybe = try compareJsonSubset(allocator, "{\"a\":{\"b\":1}}", "{\"a\":{\"b\":2}}");
    try std.testing.expect(maybe != null);
    if (maybe) |m| {
        defer allocator.free(m);
        try std.testing.expect(std.mem.indexOf(u8, m, ".a.b") != null);
    }
}
