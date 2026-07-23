//! Build-time replay verification + handler-test execution.

const std = @import("std");
const zts = @import("zts");

// ============================================================================
// Build-Time Replay Verification
// ============================================================================

pub const BuildReplayResult = struct {
    total: u32,
    pass: u32,
    fail: u32,
};

/// Run replay verification at build time.
/// Creates a fresh zts context per trace for isolation (the interpreter mutates
/// context state during execution). This is O(N*parse) but acceptable at build
/// time since trace counts are typically small.
pub fn runBuildTimeReplay(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    groups: []const zts.trace.RequestTraceGroup,
) !BuildReplayResult {
    var pass: u32 = 0;
    var fail: u32 = 0;

    for (groups, 0..) |*group, idx| {
        const ok = replayOneBuildTime(allocator, handler_source, handler_filename, group) catch |err| {
            std.debug.print("  Trace #{d}: error - {}\n", .{ idx, err });
            fail += 1;
            continue;
        };
        if (ok) {
            pass += 1;
        } else {
            fail += 1;
        }
    }

    return .{ .total = pass + fail, .pass = pass, .fail = fail };
}

/// Result of executing a handler at build time.
const HandlerExecResult = struct {
    status: u16,
    body: []const u8,
    divergences: u32,
};

/// Execute a handler against a request with stubbed I/O at build time.
/// Shared by both replay verification and handler tests.
fn executeBuildTimeHandler(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    request: zts.trace.RequestTrace,
    io_calls: []const zts.trace.IoEntry,
) !HandlerExecResult {
    const ctx = try zts.createContext(allocator, .{ .nursery_size = 64 * 1024 });
    defer zts.destroyContext(ctx);
    try zts.builtins.initBuiltins(ctx);

    inline for (zts.builtin_modules.all) |binding| {
        try zts.modules.registerVirtualModuleReplay(binding, ctx, allocator);
    }

    var replay_state = zts.trace.ReplayState{
        .io_calls = io_calls,
        .cursor = 0,
        .divergences = 0,
    };
    ctx.setModuleState(
        zts.trace.REPLAY_STATE_SLOT,
        @ptrCast(&replay_state),
        &zts.trace.ReplayState.deinitOpaque,
    );
    defer ctx.module_state[zts.trace.REPLAY_STATE_SLOT] = null;

    var strings = zts.StringTable.init(allocator);
    defer strings.deinit();

    var source_to_parse: []const u8 = handler_source;
    var strip_result: ?zts.StripResult = null;
    defer if (strip_result) |*sr| sr.deinit();

    const is_ts = std.mem.endsWith(u8, handler_filename, ".ts");
    const is_tsx = std.mem.endsWith(u8, handler_filename, ".tsx");
    if (is_ts or is_tsx) {
        var strip_diag: ?zts.StripDiagnostic = null;
        strip_result = zts.strip(allocator, handler_source, .{
            .tsx_mode = is_tsx,
            .diagnostic_out = &strip_diag,
        }) catch |err| {
            if (strip_diag) |d| {
                std.debug.print("{s}:{d}:{d}: {s}\n", .{ handler_filename, d.line, d.column, d.kind.message() });
            } else {
                std.debug.print("TypeScript strip error in {s}: {}\n", .{ handler_filename, err });
            }
            return error.StripFailed;
        };
        source_to_parse = strip_result.?.code;
    }

    var p = zts.Parser.init(allocator, source_to_parse, &strings, &ctx.atoms);
    defer p.deinit();
    if (std.mem.endsWith(u8, handler_filename, ".jsx") or is_tsx) {
        p.enableJsx();
    }

    const bytecode_data = p.parse() catch return error.ParseFailed;

    const shapes = p.getShapes();
    if (shapes.len > 0) {
        try ctx.materializeShapes(shapes);
    }

    const func = zts.bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = bytecode_data,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = zts.Interpreter.init(ctx);
    _ = try interp.run(&func);

    const handler_val = ctx.getGlobal(zts.Atom.handler) orelse return error.NoHandler;
    if (!handler_val.isObject()) return error.NoHandler;
    const handler_obj = zts.JSObject.fromValue(handler_val);
    if (handler_obj.class_id != .function or !handler_obj.flags.is_callable) return error.NoHandler;

    const hc_pool_req = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const req_obj = try ctx.createObject(null);

    const method_val = try ctx.createString(request.method);
    try req_obj.setProperty(allocator, hc_pool_req, zts.Atom.method, method_val);

    const url_val = try ctx.createString(request.url);
    try req_obj.setProperty(allocator, hc_pool_req, zts.Atom.url, url_val);

    if (request.body) |body| {
        const body_val = try ctx.createString(body);
        try req_obj.setProperty(allocator, hc_pool_req, zts.Atom.body, body_val);
    }

    const args = [_]zts.JSValue{req_obj.toValue()};
    const bc_data = handler_obj.getBytecodeFunctionData() orelse return error.NotCallable;
    const result = interp.callBytecodeFunction(
        handler_obj.toValue(),
        bc_data.bytecode,
        zts.JSValue.undefined_val,
        &args,
    ) catch return error.HandlerError;

    if (!result.isObject()) return error.InvalidResponse;
    const result_obj = zts.JSObject.fromValue(result);
    const hc_pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    const status_val = result_obj.getProperty(hc_pool, zts.Atom.status) orelse zts.JSValue.fromInt(200);
    const actual_status: u16 = if (status_val.isInt())
        @intCast(@max(0, @min(999, status_val.getInt())))
    else
        200;

    const body_val = result_obj.getProperty(hc_pool, zts.Atom.body) orelse zts.JSValue.undefined_val;

    return .{
        .status = actual_status,
        .body = zts.trace.extractStringData(body_val) orelse "",
        .divergences = replay_state.divergences,
    };
}

/// Replay a single trace at build time.
fn replayOneBuildTime(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    group: *const zts.trace.RequestTraceGroup,
) !bool {
    const r = try executeBuildTimeHandler(allocator, handler_source, handler_filename, group.request, group.io_calls);

    const expected = group.response orelse return true;
    const status_ok = r.status == expected.status;

    // Unescape before comparing (trace stores JSON-escaped body strings).
    const expected_body = zts.trace.unescapeJson(allocator, expected.body) catch expected.body;
    defer if (expected_body.ptr != expected.body.ptr) allocator.free(expected_body);

    return status_ok and std.mem.eql(u8, r.body, expected_body) and r.divergences == 0;
}

// ============================================================================
// Build-Time Handler Tests
// ============================================================================

const BuildTestCase = struct {
    name: []const u8,
    request: ?zts.trace.RequestTrace = null,
    io_calls: []const zts.trace.IoEntry = &.{},
    expected_status: ?u16 = null,
    expected_body: ?[]const u8 = null,
    expected_body_contains: ?[]const u8 = null,
};

pub fn runBuildTimeTests(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    test_source: []const u8,
) !BuildReplayResult {
    const tests = try parseBuildTestFile(allocator, test_source);
    defer {
        for (tests) |t| allocator.free(t.io_calls);
        allocator.free(tests);
    }

    var pass: u32 = 0;
    var fail: u32 = 0;

    for (tests) |*tc| {
        const request = tc.request orelse {
            std.debug.print("  FAIL  {s} (no request defined)\n", .{tc.name});
            fail += 1;
            continue;
        };

        const r = executeBuildTimeHandler(allocator, handler_source, handler_filename, request, tc.io_calls) catch |err| {
            std.debug.print("  FAIL  {s} (error: {})\n", .{ tc.name, err });
            fail += 1;
            continue;
        };

        var ok = true;
        if (tc.expected_status) |expected_status| {
            if (r.status != expected_status) {
                std.debug.print("  FAIL  {s}\n        expected status: {d}, actual status: {d}\n", .{
                    tc.name, expected_status, r.status,
                });
                ok = false;
            }
        }
        if (tc.expected_body) |expected| {
            const unescaped = zts.trace.unescapeJson(allocator, expected) catch expected;
            defer if (unescaped.ptr != expected.ptr) allocator.free(unescaped);
            if (!std.mem.eql(u8, r.body, unescaped)) {
                std.debug.print("  FAIL  {s}\n        body mismatch\n        expected: {s}\n        actual:   {s}\n", .{
                    tc.name, truncate(unescaped, 200), truncate(r.body, 200),
                });
                ok = false;
            }
        }
        if (tc.expected_body_contains) |needle| {
            const unescaped = zts.trace.unescapeJson(allocator, needle) catch needle;
            defer if (unescaped.ptr != needle.ptr) allocator.free(unescaped);
            if (std.mem.indexOf(u8, r.body, unescaped) == null) {
                std.debug.print("  FAIL  {s}\n        body does not contain: {s}\n", .{ tc.name, unescaped });
                ok = false;
            }
        }

        if (ok) {
            std.debug.print("  PASS  {s}\n", .{tc.name});
            pass += 1;
        } else {
            fail += 1;
        }
    }

    return .{ .total = pass + fail, .pass = pass, .fail = fail };
}

fn truncate(s: []const u8, max: usize) []const u8 {
    return if (s.len > max) s[0..max] else s;
}
fn parseBuildTestFile(allocator: std.mem.Allocator, source: []const u8) ![]BuildTestCase {
    var tests: std.ArrayList(BuildTestCase) = .empty;
    errdefer tests.deinit(allocator);

    var current_name: ?[]const u8 = null;
    var current_request: ?zts.trace.RequestTrace = null;
    var current_io: std.ArrayList(zts.trace.IoEntry) = .empty;
    defer current_io.deinit(allocator);
    var current_status: ?u16 = null;
    var current_body: ?[]const u8 = null;
    var current_body_contains: ?[]const u8 = null;

    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const type_str = zts.trace.findJsonStringValue(line, "\"type\"") orelse continue;

        if (std.mem.eql(u8, type_str, "test")) {
            if (current_name != null) {
                try tests.append(allocator, .{
                    .name = current_name.?,
                    .request = current_request,
                    .io_calls = try current_io.toOwnedSlice(allocator),
                    .expected_status = current_status,
                    .expected_body = current_body,
                    .expected_body_contains = current_body_contains,
                });
                current_request = null;
                current_status = null;
                current_body = null;
                current_body_contains = null;
            }
            current_name = zts.trace.findJsonStringValue(line, "\"name\"") orelse "unnamed";
        } else if (std.mem.eql(u8, type_str, "request")) {
            current_request = .{
                .method = zts.trace.findJsonStringValue(line, "\"method\"") orelse "GET",
                .url = zts.trace.findJsonStringValue(line, "\"url\"") orelse "/",
                .headers_json = zts.trace.findJsonObjectValue(line, "\"headers\"") orelse "{}",
                .body = zts.trace.findJsonStringValue(line, "\"body\""),
            };
        } else if (std.mem.eql(u8, type_str, "io")) {
            try current_io.append(allocator, .{
                .seq = @intCast(zts.trace.findJsonIntValue(line, "\"seq\"") orelse 0),
                .module = zts.trace.findJsonStringValue(line, "\"module\"") orelse "",
                .func = zts.trace.findJsonStringValue(line, "\"fn\"") orelse "",
                .args_json = zts.trace.findJsonArrayValue(line, "\"args\"") orelse "[]",
                .result_json = zts.trace.findJsonAnyValue(line, "\"result\"") orelse "null",
            });
        } else if (std.mem.eql(u8, type_str, "expect")) {
            const status_val = zts.trace.findJsonIntValue(line, "\"status\"");
            current_status = if (status_val) |s| @intCast(@max(0, @min(999, s))) else null;
            current_body = zts.trace.findJsonStringValue(line, "\"body\"");
            current_body_contains = zts.trace.findJsonStringValue(line, "\"bodyContains\"");
        }
    }

    if (current_name != null) {
        try tests.append(allocator, .{
            .name = current_name.?,
            .request = current_request,
            .io_calls = try current_io.toOwnedSlice(allocator),
            .expected_status = current_status,
            .expected_body = current_body,
            .expected_body_contains = current_body_contains,
        });
    }

    return try tests.toOwnedSlice(allocator);
}
