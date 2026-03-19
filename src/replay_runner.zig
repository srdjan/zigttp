//! Replay Runner
//!
//! Replays recorded JSONL traces against a handler and reports pass/fail.
//! In diff mode (when a different handler is specified), compares outputs
//! and produces a structured regression report.
//!
//! Usage:
//!   zig build run -- --replay traces.jsonl handler.ts
//!   zig build run -- --replay traces.jsonl handler_v2.ts   (diff mode)

const std = @import("std");
const zq = @import("zts");
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;
const Runtime = @import("zruntime.zig").Runtime;
const HttpRequestView = @import("http_types.zig").HttpRequestView;
const HttpHeader = @import("http_types.zig").HttpHeader;
const HttpResponse = @import("http_types.zig").HttpResponse;
const ServerConfig = @import("server.zig").ServerConfig;

const trace = zq.trace;

/// Run the replay engine.
pub fn run(allocator: std.mem.Allocator, config: ServerConfig) !void {
    const replay_path = config.runtime_config.replay_file_path orelse return error.NoReplayFile;

    // Read trace file
    const trace_source = readFile(allocator, replay_path) catch |err| {
        std.log.err("Failed to read trace file '{s}': {}", .{ replay_path, err });
        return err;
    };
    defer allocator.free(trace_source);

    // Parse traces
    const groups = try trace.parseTraceFile(allocator, trace_source);
    defer {
        for (groups) |g| allocator.free(g.io_calls);
        allocator.free(groups);
    }

    if (groups.len == 0) {
        log("No traces found in '{s}'\n", .{replay_path});
        return;
    }

    // Resolve handler source
    const handler_code = switch (config.handler) {
        .file_path => |path| readFile(allocator, path) catch |err| {
            std.log.err("Failed to read handler '{s}': {}", .{ path, err });
            return err;
        },
        .inline_code => |code| try allocator.dupe(u8, code),
        .embedded_bytecode => {
            std.log.err("Replay with embedded bytecode not yet supported", .{});
            return error.UnsupportedReplaySource;
        },
    };
    defer allocator.free(handler_code);

    const handler_filename = switch (config.handler) {
        .file_path => |path| path,
        else => "eval",
    };

    // Build replay runtime config: keep replay_file_path set so Runtime
    // installs replay stubs instead of real virtual module functions.
    var replay_config = config.runtime_config;
    replay_config.trace_file_path = null;
    // replay_file_path stays set to signal replay mode

    var summary = ReplaySummary{};
    var results: std.ArrayList(ReplayResult) = .empty;
    defer {
        for (results.items) |r| r.deinit(allocator);
        results.deinit(allocator);
    }

    log("Replaying {d} traces against {s}...\n", .{ groups.len, handler_filename });

    for (groups, 0..) |group, trace_idx| {
        const result = replayOne(allocator, replay_config, handler_code, handler_filename, &group, trace_idx) catch |err| {
            try results.append(allocator, .{
                .match = false,
                .expected_status = if (group.response) |r| r.status else 200,
                .actual_status = 0,
                .body_match = false,
                .expected_body = if (group.response) |r| r.body else "",
                .actual_body_owned = &.{},
                .io_divergences = 0,
                .io_total = @intCast(group.io_calls.len),
                .err = err,
            });
            summary.diverged += 1;
            summary.total += 1;
            continue;
        };
        try results.append(allocator, result);
        summary.total += 1;
        if (result.match) {
            summary.identical += 1;
        } else if (result.expected_status != result.actual_status) {
            summary.status_changed += 1;
        } else if (!result.body_match) {
            summary.body_changed += 1;
        } else {
            summary.diverged += 1;
        }
    }

    // Print summary
    log("\nReplay results for {s}:\n", .{handler_filename});
    log("  {d}/{d} identical\n", .{ summary.identical, summary.total });
    if (summary.status_changed > 0) {
        log("  {d} status code changes\n", .{summary.status_changed});
    }
    if (summary.body_changed > 0) {
        log("  {d} body changes\n", .{summary.body_changed});
    }
    if (summary.diverged > 0) {
        log("  {d} diverged (I/O mismatch or runtime error)\n", .{summary.diverged});
    }

    // Print diffs for non-matching traces (up to 10)
    var diff_count: u32 = 0;
    for (results.items, 0..) |result, idx| {
        if (result.match) continue;
        if (diff_count >= 10) {
            log("\n  ... and {d} more differences\n", .{summary.total - summary.identical - diff_count});
            break;
        }
        diff_count += 1;

        if (result.err) |err| {
            log("\nTrace #{d}: error - {}\n", .{ idx, err });
            continue;
        }

        if (result.expected_status != result.actual_status) {
            log("\nTrace #{d}: status changed {d} -> {d}\n", .{ idx, result.expected_status, result.actual_status });
        }
        if (!result.body_match) {
            log("\nTrace #{d}: body changed\n", .{idx});
            if (result.expected_body.len > 0 and result.expected_body.len <= 200) {
                // Unescape the expected body for display (trace stores JSON-escaped)
                const display = trace.unescapeJson(allocator, result.expected_body) catch result.expected_body;
                defer if (display.ptr != result.expected_body.ptr) allocator.free(display);
                log("  - {s}\n", .{display});
            }
            if (result.actual_body_owned.len > 0) {
                log("  + {s}\n", .{result.actual_body_owned});
            }
        }
    }

    if (summary.identical == summary.total) {
        log("\nAll {d} traces passed.\n", .{summary.total});
    }
}

/// Replay a single trace against the handler.
fn replayOne(
    allocator: std.mem.Allocator,
    config: RuntimeConfig,
    handler_code: []const u8,
    handler_filename: []const u8,
    group: *const trace.RequestTraceGroup,
    _: usize,
) !ReplayResult {
    // Create a fresh runtime for each replay.
    // Config has replay_file_path set, so Runtime.init registers replay stubs
    // for virtual modules and fetchSync instead of real implementations.
    const rt = try Runtime.init(allocator, config);
    defer rt.deinit();

    // Set up per-request replay state in module_state.
    // The replay stubs (generated by makeReplayStub) read from this.
    var replay_state = trace.ReplayState{
        .io_calls = group.io_calls,
        .cursor = 0,
        .divergences = 0,
    };
    rt.ctx.setModuleState(
        trace.REPLAY_STATE_SLOT,
        @ptrCast(&replay_state),
        &trace.ReplayState.deinitOpaque,
    );
    // Clear module_state before replay_state goes out of scope
    defer rt.ctx.module_state[trace.REPLAY_STATE_SLOT] = null;

    // Load handler code
    try rt.loadCode(handler_code, handler_filename);

    // Build request from trace
    var headers_list: std.ArrayListUnmanaged(HttpHeader) = .{};
    defer headers_list.deinit(allocator);

    try parseHeadersFromJson(allocator, group.request.headers_json, &headers_list);

    const request = HttpRequestView{
        .method = group.request.method,
        .url = group.request.url,
        .headers = headers_list,
        .body = group.request.body,
    };

    // Execute handler
    var response = rt.executeHandler(request) catch |err| {
        return ReplayResult{
            .match = false,
            .expected_status = if (group.response) |r| r.status else 200,
            .actual_status = 0,
            .body_match = false,
            .expected_body = if (group.response) |r| r.body else "",
            .actual_body_owned = &.{},
            .io_divergences = replay_state.divergences,
            .io_total = @intCast(group.io_calls.len),
            .err = err,
        };
    };
    defer response.deinit();

    const expected = group.response orelse {
        return ReplayResult{
            .match = true,
            .expected_status = 200,
            .actual_status = response.status,
            .body_match = true,
            .expected_body = "",
            .actual_body_owned = &.{},
            .io_divergences = replay_state.divergences,
            .io_total = @intCast(group.io_calls.len),
            .err = null,
        };
    };

    const status_match = response.status == expected.status;

    // The trace stores body as a JSON string value, so it contains escape
    // sequences like \" for embedded quotes. Unescape before comparing.
    const expected_body_unescaped = trace.unescapeJson(allocator, expected.body) catch expected.body;
    defer if (expected_body_unescaped.ptr != expected.body.ptr) allocator.free(expected_body_unescaped);
    const body_match = std.mem.eql(u8, response.body, expected_body_unescaped);

    // Dupe the body before response.deinit() frees it
    const body_copy = if (!body_match and response.body.len > 0 and response.body.len <= 200)
        allocator.dupe(u8, response.body) catch &.{}
    else
        &.{};

    return ReplayResult{
        .match = status_match and body_match and replay_state.divergences == 0,
        .expected_status = expected.status,
        .actual_status = response.status,
        .body_match = body_match,
        .expected_body = expected.body,
        .actual_body_owned = body_copy,
        .io_divergences = replay_state.divergences,
        .io_total = @intCast(group.io_calls.len),
        .err = null,
    };
}

// ============================================================================
// Result Types
// ============================================================================

pub const ReplayResult = struct {
    match: bool,
    expected_status: u16,
    actual_status: u16,
    body_match: bool,
    expected_body: []const u8,
    /// Owned copy of the actual response body (must be freed).
    actual_body_owned: []const u8,
    io_divergences: u32,
    io_total: u32,
    err: ?anyerror,

    pub fn deinit(self: *const ReplayResult, allocator: std.mem.Allocator) void {
        if (self.actual_body_owned.len > 0) {
            allocator.free(self.actual_body_owned);
        }
    }
};

pub const ReplaySummary = struct {
    total: u32 = 0,
    identical: u32 = 0,
    status_changed: u32 = 0,
    body_changed: u32 = 0,
    diverged: u32 = 0,
};

// ============================================================================
// Helpers
// ============================================================================

fn log(comptime fmt: []const u8, args: anytype) void {
    std.log.info(fmt, args);
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0) catch {
        return error.FileNotFound;
    };
    defer std.Io.Threaded.closeFd(fd);

    const max_size = 100 * 1024 * 1024; // 100MB limit
    var buffer: std.ArrayList(u8) = .empty;
    errdefer buffer.deinit(allocator);

    var chunk: [8192]u8 = undefined;
    while (true) {
        const bytes_read = std.posix.read(fd, &chunk) catch {
            return error.InputOutput;
        };
        if (bytes_read == 0) break;
        if (buffer.items.len + bytes_read > max_size) {
            return error.FileTooBig;
        }
        try buffer.appendSlice(allocator, chunk[0..bytes_read]);
    }

    return try buffer.toOwnedSlice(allocator);
}

/// Parse a simple JSON object {"key":"value",...} into header list.
fn parseHeadersFromJson(
    allocator: std.mem.Allocator,
    json: []const u8,
    headers: *std.ArrayListUnmanaged(HttpHeader),
) !void {
    if (json.len < 2 or json[0] != '{') return;

    var pos: usize = 1;
    while (pos < json.len) {
        // Skip whitespace and commas
        while (pos < json.len and (json[pos] == ' ' or json[pos] == ',' or json[pos] == '\n')) : (pos += 1) {}
        if (pos >= json.len or json[pos] == '}') break;

        // Parse key
        if (json[pos] != '"') break;
        pos += 1;
        const key_start = pos;
        while (pos < json.len and json[pos] != '"') : (pos += 1) {
            if (json[pos] == '\\') pos += 1;
        }
        const key = json[key_start..pos];
        if (pos < json.len) pos += 1; // skip closing quote

        // Skip colon
        while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}

        // Parse value
        if (pos >= json.len or json[pos] != '"') break;
        pos += 1;
        const val_start = pos;
        while (pos < json.len and json[pos] != '"') : (pos += 1) {
            if (json[pos] == '\\') pos += 1;
        }
        const val = json[val_start..pos];
        if (pos < json.len) pos += 1; // skip closing quote

        try headers.append(allocator, .{ .key = key, .value = val });
    }
}
