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
const zq = @import("zts");
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;
const Runtime = @import("zruntime.zig").Runtime;
const HttpRequestView = @import("http_types.zig").HttpRequestView;
const HttpHeader = @import("http_types.zig").HttpHeader;
const HttpResponse = @import("http_types.zig").HttpResponse;
const ServerConfig = @import("server.zig").ServerConfig;

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

    const tests = try parseTestFile(allocator, test_source);
    defer {
        for (tests) |t| allocator.free(t.io_calls);
        allocator.free(tests);
    }

    if (tests.len == 0) {
        std.log.info("No tests found in '{s}'\n", .{test_path});
        return;
    }

    const handler_code = switch (config.handler) {
        .file_path => |path| readFile(allocator, path) catch |err| {
            std.log.err("Failed to read handler '{s}': {}", .{ path, err });
            return err;
        },
        .inline_code => |code| try allocator.dupe(u8, code),
        .embedded_bytecode => {
            std.log.err("Testing with embedded bytecode not yet supported", .{});
            return error.UnsupportedTestSource;
        },
    };
    defer allocator.free(handler_code);

    const handler_filename = switch (config.handler) {
        .file_path => |path| path,
        else => "eval",
    };

    // Set replay_file_path sentinel so Runtime installs replay stubs
    // instead of real virtual module functions.
    var test_config = config.runtime_config;
    test_config.trace_file_path = null;
    test_config.replay_file_path = "test";

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

    var response = rt.executeHandler(.{
        .method = request.method,
        .url = request.url,
        .headers = headers_list,
        .body = request.body,
    }) catch |err| {
        return .{ .pass = false, .name = test_case.name, .failures = failures, .err = err };
    };
    defer response.deinit();

    checkAssertions(allocator, &response, &test_case.assertions, &failures);

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
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const type_str = trace.findJsonStringValue(line, "\"type\"") orelse continue;

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
            current_name = trace.findJsonStringValue(line, "\"name\"") orelse "unnamed";
        } else if (std.mem.eql(u8, type_str, "request")) {
            current_request = .{
                .method = trace.findJsonStringValue(line, "\"method\"") orelse "GET",
                .url = trace.findJsonStringValue(line, "\"url\"") orelse "/",
                .headers_json = trace.findJsonObjectValue(line, "\"headers\"") orelse "{}",
                .body = trace.findJsonStringValue(line, "\"body\""),
            };
        } else if (std.mem.eql(u8, type_str, "io")) {
            try current_io.append(allocator, .{
                .seq = @intCast(trace.findJsonIntValue(line, "\"seq\"") orelse 0),
                .module = trace.findJsonStringValue(line, "\"module\"") orelse "",
                .func = trace.findJsonStringValue(line, "\"fn\"") orelse "",
                .args_json = trace.findJsonArrayValue(line, "\"args\"") orelse "[]",
                .result_json = trace.findJsonAnyValue(line, "\"result\"") orelse "null",
            });
        } else if (std.mem.eql(u8, type_str, "expect")) {
            const status_val = trace.findJsonIntValue(line, "\"status\"");
            current_assertions.status = if (status_val) |s| @intCast(@max(0, @min(999, s))) else null;
            current_assertions.body = trace.findJsonStringValue(line, "\"body\"");
            current_assertions.body_contains = trace.findJsonStringValue(line, "\"bodyContains\"");
            current_assertions.headers_json = trace.findJsonObjectValue(line, "\"headers\"");
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
