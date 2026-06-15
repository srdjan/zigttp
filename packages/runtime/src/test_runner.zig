//! Handler Test Runner
//!
//! Runs declarative handler tests defined as JSONL test cases.
//! Leverages the deterministic replay infrastructure: because virtual modules
//! are the ONLY I/O boundary, handlers are pure functions of
//! (Request, VirtualModuleResponses).
//!
//! Test file format (JSONL):
//!   {"type":"test","name":"descriptive name"}
//!   {"type":"request","method":"GET","url":"/path","headers":{},"body":null}
//!   {"type":"io","seq":0,"module":"env","fn":"env","args":["KEY"],"result":"value"}
//!   {"type":"expect","status":200,"body":"{\"ok\":true}","bodyContains":null}
//!
//! Usage:
//!   zig build run -- handler.ts --test tests.jsonl

const std = @import("std");
const builtin = @import("builtin");
const zq = @import("zigts");
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;
const Runtime = @import("zruntime.zig").Runtime;
const HttpRequestView = @import("http_types.zig").HttpRequestView;
const HttpHeader = @import("http_types.zig").HttpHeader;
const HttpResponse = @import("http_types.zig").HttpResponse;
const ServerConfig = @import("server.zig").ServerConfig;
const handler_loader = @import("handler_loader.zig");
const runtime_natives = @import("runtime_natives.zig");

const trace = zq.trace;
const parseHeadersFromJson = @import("trace_helpers.zig").parseHeadersFromJson;

const TestAssertions = struct {
    status: ?u16 = null,
    body: ?[]const u8 = null,
    body_contains: ?[]const u8 = null,
    headers_json: ?[]const u8 = null,
};

const TestCase = struct {
    name: []const u8,
    request: ?trace.RequestTrace = null,
    io_calls: []const trace.IoEntry = &.{},
    assertions: TestAssertions = .{},
};

pub fn run(allocator: std.mem.Allocator, config: ServerConfig) !void {
    const test_path = config.runtime_config.test_file_path orelse return error.NoTestFile;

    const test_source = readFile(allocator, test_path) catch |err| {
        std.log.err("Failed to read test file '{s}': {}", .{ test_path, err });
        return err;
    };
    defer allocator.free(test_source);

    const tests = parseTestFile(allocator, test_source) catch |err| {
        if (err == error.InvalidTestFixture) {
            std.log.err("Invalid test fixture '{s}'. Expected JSONL rows with type=test/request/io/expect.", .{test_path});
        }
        return err;
    };
    defer {
        for (tests) |t| allocator.free(t.io_calls);
        allocator.free(tests);
    }

    if (tests.len == 0) {
        std.log.info("No tests found in '{s}'\n", .{test_path});
        return;
    }

    const loaded = handler_loader.load(allocator, config.handler) catch |err| {
        switch (err) {
            error.UnsupportedHandlerSource => std.log.err("Handler tests require a file_path or inline_code handler source", .{}),
            else => std.log.err("Handler tests failed to load handler: {}", .{err}),
        }
        return err;
    };
    const handler_code = loaded.code;
    const handler_filename = loaded.filename;
    defer allocator.free(handler_code);

    // Set replay_file_path sentinel so Runtime installs replay stubs
    // instead of real virtual module functions.
    var test_config = config.runtime_config;
    test_config.trace_file_path = null;
    test_config.replay_file_path = "test";
    // Disable arena escape enforcement: each test creates an isolated runtime
    // that is destroyed after the test, so arena objects can't outlive their scope.
    test_config.enforce_arena_escape = false;

    std.debug.print("\nTesting {s} ({d} tests)\n\n", .{ handler_filename, tests.len });

    var passed: u32 = 0;
    var failed: u32 = 0;

    for (tests) |test_case| {
        const result = runOneTest(allocator, test_config, handler_code, handler_filename, &test_case);
        if (result.pass) {
            passed += 1;
            std.debug.print("  PASS  {s}\n", .{result.name});
        } else {
            failed += 1;
            std.debug.print("  FAIL  {s}\n", .{result.name});
            if (result.err) |err| {
                std.debug.print("        error: {}\n", .{err});
            }
            for (result.failures.items) |msg| {
                std.debug.print("        {s}\n", .{msg});
            }
            result.deinitFailures(allocator);
        }
    }

    std.debug.print("\nResults: {d} passed, {d} failed, {d} total\n", .{
        passed, failed, passed + failed,
    });

    if (failed > 0) {
        return error.TestsFailed;
    }
}

const TestResult = struct {
    pass: bool,
    name: []const u8,
    failures: std.ArrayList([]const u8),
    err: ?anyerror = null,

    fn deinitFailures(self: TestResult, allocator: std.mem.Allocator) void {
        for (self.failures.items) |msg| allocator.free(msg);
        var f = self.failures;
        f.deinit(allocator);
    }
};

fn runOneTest(
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    handler_code: []const u8,
    handler_filename: []const u8,
    test_case: *const TestCase,
) TestResult {
    var failures: std.ArrayList([]const u8) = .empty;

    const request = test_case.request orelse {
        const msg = std.fmt.allocPrint(allocator, "no request defined", .{}) catch
            return .{ .pass = false, .name = test_case.name, .failures = failures, .err = error.OutOfMemory };
        failures.append(allocator, msg) catch {};
        return .{ .pass = false, .name = test_case.name, .failures = failures, .err = null };
    };

    const rt = Runtime.init(allocator, config) catch |err| {
        return .{ .pass = false, .name = test_case.name, .failures = failures, .err = err };
    };
    defer rt.deinit();

    var replay_state = trace.ReplayState{
        .io_calls = test_case.io_calls,
        .cursor = 0,
        .divergences = 0,
    };
    rt.ctx.setModuleState(
        trace.REPLAY_STATE_SLOT,
        @ptrCast(&replay_state),
        &trace.ReplayState.deinitOpaque,
    );
    defer rt.ctx.module_state[trace.REPLAY_STATE_SLOT] = null;

    rt.loadCode(handler_code, handler_filename) catch |err| {
        return .{ .pass = false, .name = test_case.name, .failures = failures, .err = err };
    };

    var headers_list: std.ArrayListUnmanaged(HttpHeader) = .empty;
    defer headers_list.deinit(allocator);
    parseHeadersFromJson(allocator, request.headers_json, &headers_list) catch {};

    // Split req.path and parse req.query exactly as the live server does, via
    // the shared helper every recorded-request consumer uses. Without it,
    // zruntime falls back to the full url for req.path (keeping the "?query"
    // suffix) and a handler reading req.query.* would see an empty object under
    // `serve --test`.
    const target = runtime_natives.parseRequestTarget(allocator, request.url);
    defer target.deinit(allocator);

    // Unescape the request body (shared with the replay runner) so a JSONL body
    // like "{\"title\":\"x\"}" reaches the handler as real JSON rather than the
    // backslash-escaped literal.
    const ub = trace.unescapeBody(allocator, request.body);
    defer if (ub.owned) |owned| allocator.free(owned);

    var response = rt.executeHandler(.{
        .method = request.method,
        .url = request.url,
        .path = target.path,
        .query_params = target.params,
        .headers = headers_list,
        .body = ub.slice,
    }) catch |err| {
        return .{ .pass = false, .name = test_case.name, .failures = failures, .err = err };
    };
    defer response.deinit();

    checkAssertions(allocator, &response, &test_case.assertions, &failures);

    // NOTE (RS1/RS2): serve --test deliberately does NOT fail on
    // replay_state.divergences. The counter conflates real drift (a reordered
    // effectful call) with benign noise - hand-written fixtures whose arg JSON
    // is non-canonical (argsDrifted byte-compares), and pure modules that run
    // for real under --test without consuming their recorded entry. Failing on
    // it here false-positives on legitimate example fixtures. Making the signal
    // usable requires precise divergence accounting (distinguish name-mismatch
    // from arg-formatting); until then the strict consumed-all/divergence gate
    // lives only in the replay verifier (replay_runner), which uses
    // recorder-produced capsules.

    return .{
        .pass = failures.items.len == 0,
        .name = test_case.name,
        .failures = failures,
        .err = null,
    };
}

fn checkAssertions(
    allocator: std.mem.Allocator,
    response: *const HttpResponse,
    assertions: *const TestAssertions,
    failures: *std.ArrayList([]const u8),
) void {
    if (assertions.status) |expected_status| {
        if (response.status != expected_status) {
            const msg = std.fmt.allocPrint(allocator, "expected status: {d}, actual status: {d}", .{
                expected_status, response.status,
            }) catch return;
            failures.append(allocator, msg) catch {};
        }
    }

    if (assertions.body) |expected_body| {
        const unescaped = trace.unescapeJson(allocator, expected_body) catch expected_body;
        defer if (unescaped.ptr != expected_body.ptr) allocator.free(unescaped);

        if (!std.mem.eql(u8, response.body, unescaped)) {
            const msg = std.fmt.allocPrint(allocator, "body mismatch:\n        expected: {s}\n        actual:   {s}", .{
                truncate(unescaped, 200), truncate(response.body, 200),
            }) catch return;
            failures.append(allocator, msg) catch {};
        }
    }

    if (assertions.body_contains) |needle| {
        const unescaped = trace.unescapeJson(allocator, needle) catch needle;
        defer if (unescaped.ptr != needle.ptr) allocator.free(unescaped);

        if (std.mem.indexOf(u8, response.body, unescaped) == null) {
            const msg = std.fmt.allocPrint(allocator, "body does not contain: {s}\n        actual body: {s}", .{
                truncate(unescaped, 100), truncate(response.body, 200),
            }) catch return;
            failures.append(allocator, msg) catch {};
        }
    }

    // Subset match: each expected header must appear in the response
    if (assertions.headers_json) |expected_headers_json| {
        var expected_headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
        defer expected_headers.deinit(allocator);
        parseHeadersFromJson(allocator, expected_headers_json, &expected_headers) catch return;

        for (expected_headers.items) |expected| {
            var found = false;
            for (response.headers.items) |actual| {
                if (std.ascii.eqlIgnoreCase(actual.key, expected.key) and
                    std.mem.eql(u8, actual.value, expected.value))
                {
                    found = true;
                    break;
                }
            }
            if (!found) {
                const msg = std.fmt.allocPrint(allocator, "missing header: {s}: {s}", .{
                    expected.key, expected.value,
                }) catch return;
                failures.append(allocator, msg) catch {};
            }
        }
    }
}

fn truncate(s: []const u8, max: usize) []const u8 {
    return if (s.len > max) s[0..max] else s;
}

fn parseTestFile(allocator: std.mem.Allocator, source: []const u8) ![]TestCase {
    var tests: std.ArrayList(TestCase) = .empty;
    errdefer tests.deinit(allocator);

    var current_name: ?[]const u8 = null;
    var current_request: ?trace.RequestTrace = null;
    var current_io: std.ArrayList(trace.IoEntry) = .empty;
    defer current_io.deinit(allocator);
    var current_assertions: TestAssertions = .{};

    var lines = std.mem.splitScalar(u8, source, '\n');
    var line_no: usize = 0;
    while (lines.next()) |line| {
        line_no += 1;
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;

        const type_str = trace.findJsonStringValue(trimmed, "\"type\"") orelse {
            if (!builtin.is_test) {
                std.log.err("Invalid test fixture line {d}: missing string field \"type\"", .{line_no});
            }
            return error.InvalidTestFixture;
        };

        if (std.mem.eql(u8, type_str, "test")) {
            if (current_name != null) {
                try tests.append(allocator, .{
                    .name = current_name.?,
                    .request = current_request,
                    .io_calls = try current_io.toOwnedSlice(allocator),
                    .assertions = current_assertions,
                });
                current_request = null;
                current_assertions = .{};
            }
            current_name = trace.findJsonStringValue(trimmed, "\"name\"") orelse "unnamed";
        } else if (std.mem.eql(u8, type_str, "request")) {
            current_request = .{
                .method = trace.findJsonStringValue(trimmed, "\"method\"") orelse "GET",
                .url = trace.findJsonStringValue(trimmed, "\"url\"") orelse "/",
                .headers_json = trace.findJsonObjectValue(trimmed, "\"headers\"") orelse "{}",
                .body = trace.findJsonStringValue(trimmed, "\"body\""),
            };
        } else if (std.mem.eql(u8, type_str, "io")) {
            // findJsonIntValue returns ?i64 and accepts negatives; seq is u32.
            // An unguarded @intCast panics (safe builds) or wraps (ReleaseFast)
            // on a hostile fixture. Mirror the status field's guarded cast.
            const seq_i = trace.findJsonIntValue(trimmed, "\"seq\"") orelse 0;
            const seq = std.math.cast(u32, seq_i) orelse return error.InvalidTestFixture;
            try current_io.append(allocator, .{
                .seq = seq,
                .module = trace.findJsonStringValue(trimmed, "\"module\"") orelse "",
                .func = trace.findJsonStringValue(trimmed, "\"fn\"") orelse "",
                .args_json = trace.findJsonArrayValue(trimmed, "\"args\"") orelse "[]",
                .result_json = trace.findJsonAnyValue(trimmed, "\"result\"") orelse "null",
            });
        } else if (std.mem.eql(u8, type_str, "expect")) {
            const status_val = trace.findJsonIntValue(trimmed, "\"status\"");
            current_assertions.status = if (status_val) |s| @intCast(@max(0, @min(999, s))) else null;
            current_assertions.body = trace.findJsonStringValue(trimmed, "\"body\"");
            current_assertions.body_contains = trace.findJsonStringValue(trimmed, "\"bodyContains\"");
            current_assertions.headers_json = trace.findJsonObjectValue(trimmed, "\"headers\"");
        } else {
            if (!builtin.is_test) {
                std.log.err("Invalid test fixture line {d}: unknown type \"{s}\"", .{ line_no, type_str });
            }
            return error.InvalidTestFixture;
        }
    }

    if (current_name != null) {
        try tests.append(allocator, .{
            .name = current_name.?,
            .request = current_request,
            .io_calls = try current_io.toOwnedSlice(allocator),
            .assertions = current_assertions,
        });
    }

    return try tests.toOwnedSlice(allocator);
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    return zq.file_io.readFile(allocator, path, 100 * 1024 * 1024);
}

// ============================================================================
// Tests
// ============================================================================

test "parseTestFile: empty input" {
    const tests = try parseTestFile(std.testing.allocator, "");
    defer std.testing.allocator.free(tests);
    try std.testing.expectEqual(@as(usize, 0), tests.len);
}

test "parseTestFile: invalid non-empty row fails" {
    try std.testing.expectError(error.InvalidTestFixture, parseTestFile(std.testing.allocator, "not json"));
    try std.testing.expectError(error.InvalidTestFixture, parseTestFile(std.testing.allocator, "{\"type\":\"bogus\"}"));
}

test "parseTestFile: single test with status assertion" {
    const source =
        \\{"type":"test","name":"health check"}
        \\{"type":"request","method":"GET","url":"/health","headers":{},"body":null}
        \\{"type":"expect","status":200}
    ;
    const tests = try parseTestFile(std.testing.allocator, source);
    defer {
        for (tests) |t| std.testing.allocator.free(t.io_calls);
        std.testing.allocator.free(tests);
    }

    try std.testing.expectEqual(@as(usize, 1), tests.len);
    try std.testing.expectEqualStrings("health check", tests[0].name);
    try std.testing.expectEqualStrings("GET", tests[0].request.?.method);
    try std.testing.expectEqualStrings("/health", tests[0].request.?.url);
    try std.testing.expectEqual(@as(u16, 200), tests[0].assertions.status.?);
}

test "parseTestFile: multiple tests" {
    const source =
        \\{"type":"test","name":"first"}
        \\{"type":"request","method":"GET","url":"/a","headers":{},"body":null}
        \\{"type":"expect","status":200}
        \\{"type":"test","name":"second"}
        \\{"type":"request","method":"POST","url":"/b","headers":{},"body":null}
        \\{"type":"expect","status":404}
    ;
    const tests = try parseTestFile(std.testing.allocator, source);
    defer {
        for (tests) |t| std.testing.allocator.free(t.io_calls);
        std.testing.allocator.free(tests);
    }

    try std.testing.expectEqual(@as(usize, 2), tests.len);
    try std.testing.expectEqualStrings("first", tests[0].name);
    try std.testing.expectEqualStrings("second", tests[1].name);
    try std.testing.expectEqual(@as(u16, 404), tests[1].assertions.status.?);
}

test "parseTestFile: test with io stubs" {
    const source =
        \\{"type":"test","name":"with io"}
        \\{"type":"request","method":"GET","url":"/","headers":{},"body":null}
        \\{"type":"io","seq":0,"module":"env","fn":"env","args":["KEY"],"result":"val"}
        \\{"type":"io","seq":1,"module":"crypto","fn":"sha256","args":["x"],"result":"abc"}
        \\{"type":"expect","status":200}
    ;
    const tests = try parseTestFile(std.testing.allocator, source);
    defer {
        for (tests) |t| std.testing.allocator.free(t.io_calls);
        std.testing.allocator.free(tests);
    }

    try std.testing.expectEqual(@as(usize, 1), tests.len);
    try std.testing.expectEqual(@as(usize, 2), tests[0].io_calls.len);
    try std.testing.expectEqualStrings("env", tests[0].io_calls[0].module);
    try std.testing.expectEqualStrings("crypto", tests[0].io_calls[1].module);
}

test "parseTestFile: bodyContains assertion" {
    const source =
        \\{"type":"test","name":"contains"}
        \\{"type":"request","method":"GET","url":"/","headers":{},"body":null}
        \\{"type":"expect","status":200,"bodyContains":"hello"}
    ;
    const tests = try parseTestFile(std.testing.allocator, source);
    defer {
        for (tests) |t| std.testing.allocator.free(t.io_calls);
        std.testing.allocator.free(tests);
    }

    try std.testing.expectEqual(@as(usize, 1), tests.len);
    try std.testing.expectEqualStrings("hello", tests[0].assertions.body_contains.?);
}

test "checkAssertions: status match passes" {
    const assertions = TestAssertions{ .status = 200 };
    var response = HttpResponse.init(std.testing.allocator);
    response.status = 200;
    var failures: std.ArrayList([]const u8) = .empty;
    checkAssertions(std.testing.allocator, &response, &assertions, &failures);
    try std.testing.expectEqual(@as(usize, 0), failures.items.len);
}

test "checkAssertions: status mismatch fails" {
    const assertions = TestAssertions{ .status = 404 };
    var response = HttpResponse.init(std.testing.allocator);
    response.status = 200;
    var failures: std.ArrayList([]const u8) = .empty;
    checkAssertions(std.testing.allocator, &response, &assertions, &failures);
    defer {
        for (failures.items) |msg| std.testing.allocator.free(msg);
        failures.deinit(std.testing.allocator);
    }
    try std.testing.expectEqual(@as(usize, 1), failures.items.len);
    try std.testing.expect(std.mem.indexOf(u8, failures.items[0], "expected status: 404") != null);
}

test "checkAssertions: bodyContains match passes" {
    const assertions = TestAssertions{ .body_contains = "world" };
    var response = HttpResponse.init(std.testing.allocator);
    response.body = "{\"greeting\":\"hello world\"}";
    var failures: std.ArrayList([]const u8) = .empty;
    checkAssertions(std.testing.allocator, &response, &assertions, &failures);
    try std.testing.expectEqual(@as(usize, 0), failures.items.len);
}

test "checkAssertions: bodyContains mismatch fails" {
    const assertions = TestAssertions{ .body_contains = "missing" };
    var response = HttpResponse.init(std.testing.allocator);
    response.body = "{\"greeting\":\"hello\"}";
    var failures: std.ArrayList([]const u8) = .empty;
    checkAssertions(std.testing.allocator, &response, &assertions, &failures);
    defer {
        for (failures.items) |msg| std.testing.allocator.free(msg);
        failures.deinit(std.testing.allocator);
    }
    try std.testing.expectEqual(@as(usize, 1), failures.items.len);
    try std.testing.expect(std.mem.indexOf(u8, failures.items[0], "body does not contain") != null);
}

test "runOneTest: request url with query string yields path without query (CLI-1 scaffold)" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Handler matches req.path === "/" (mirrors the default `zigttp init` basic scaffold).
    const handler_code =
        \\function handler(req) {
        \\  if (req.method === "GET" && req.path === "/") {
        \\    return Response.text("Hello,");
        \\  }
        \\  return Response.text("Not Found", { status: 404 });
        \\}
    ;

    // Mirror run(): replay sentinel installs stub modules, arena escape off.
    const config = RuntimeConfig{
        .replay_file_path = "test",
        .enforce_arena_escape = false,
    };

    // The scaffold fixture sends GET /?probe=1; before the fix req.path became
    // "/?probe=1" and the handler returned 404.
    const test_case = TestCase{
        .name = "scaffold probe",
        .request = .{
            .method = "GET",
            .url = "/?probe=1",
            .headers_json = "{}",
            .body = null,
        },
        .assertions = .{ .status = 200 },
    };

    const result = runOneTest(allocator, config, handler_code, "<scaffold>", &test_case);
    defer result.deinitFailures(allocator);

    try std.testing.expect(result.err == null);
    try std.testing.expect(result.pass);
}

test "runOneTest: req.query is populated from the url query string" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Echo a query param back. Mirrors the weather app, which reads
    // req.query.latitude/longitude. Without query_params plumbed into the test
    // request, req.query would be empty and this would 400.
    const handler_code =
        \\function handler(req) {
        \\  const lat = req.query.latitude;
        \\  if (lat === undefined) {
        \\    return Response.text("missing", { status: 400 });
        \\  }
        \\  return Response.text("lat=" + lat);
        \\}
    ;

    const config = RuntimeConfig{
        .replay_file_path = "test",
        .enforce_arena_escape = false,
    };

    const test_case = TestCase{
        .name = "query echo",
        .request = .{
            .method = "GET",
            .url = "/forecast?latitude=48.85&longitude=2.35",
            .headers_json = "{}",
            .body = null,
        },
        .assertions = .{ .status = 200, .body_contains = "lat=48.85" },
    };

    const result = runOneTest(allocator, config, handler_code, "<query>", &test_case);
    defer result.deinitFailures(allocator);

    try std.testing.expect(result.err == null);
    try std.testing.expect(result.pass);
}
