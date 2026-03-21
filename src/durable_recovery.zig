//! Durable Execution Recovery
//!
//! On server startup, scans the durable directory for incomplete oplog files
//! and re-executes their handlers. The handler's run(key, ...) call reuses the
//! recorded oplog, replays deterministic effects, and continues live for any
//! remaining work.
//!
//! An oplog is incomplete if it contains a request entry but no "complete" marker.
//! Completed oplogs are kept so duplicate durable keys can reuse the recorded
//! response.

const std = @import("std");
const zq = @import("zts");
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;
const Runtime = @import("zruntime.zig").Runtime;
const HttpRequestView = @import("http_types.zig").HttpRequestView;
const HttpHeader = @import("http_types.zig").HttpHeader;
const ServerConfig = @import("server.zig").ServerConfig;

const c = @cImport({
    @cInclude("dirent.h");
});

const trace = zq.trace;

/// Scan the durable directory and recover any incomplete runs.
/// Returns the number of runs recovered.
pub fn recoverIncompleteOplogs(allocator: std.mem.Allocator, config: ServerConfig) !u32 {
    const oplog_dir = config.runtime_config.durable_oplog_dir orelse return 0;

    const dir_path_z = try allocator.dupeZ(u8, oplog_dir);
    defer allocator.free(dir_path_z);

    const dir = c.opendir(dir_path_z) orelse {
        // Directory doesn't exist yet - nothing to recover
        return 0;
    };
    defer _ = c.closedir(dir);

    // Collect oplog filenames
    var oplog_files: std.ArrayList([]const u8) = .empty;
    defer {
        for (oplog_files.items) |name| allocator.free(name);
        oplog_files.deinit(allocator);
    }

    while (c.readdir(dir)) |entry| {
        const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
        const name = std.mem.sliceTo(name_ptr, 0);
        if (!std.mem.startsWith(u8, name, "durable-")) continue;
        if (!std.mem.endsWith(u8, name, ".jsonl")) continue;
        try oplog_files.append(allocator, try allocator.dupe(u8, name));
    }

    if (oplog_files.items.len == 0) return 0;

    var recovered: u32 = 0;

    for (oplog_files.items) |filename| {
        // Build full path
        var path_buf: [512]u8 = undefined;
        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ oplog_dir, filename }) catch continue;

        // Read oplog file
        const source = readFile(allocator, full_path) catch |err| {
            std.log.err("Failed to read oplog '{s}': {}", .{ full_path, err });
            continue;
        };
        defer allocator.free(source);

        if (!trace.isIncompleteOplog(source)) {
            // Completed durable runs remain on disk for duplicate-key reuse.
            continue;
        }

        // Parse the incomplete oplog
        var parsed = trace.parseIncompleteOplog(allocator, source) catch |err| {
            std.log.err("Failed to parse oplog '{s}': {}", .{ full_path, err });
            continue;
        };
        defer parsed.deinit();

        const run_key = parsed.run_key orelse {
            std.log.err("Skipping oplog without durable run key: {s}", .{full_path});
            continue;
        };

        std.log.info("Recovering durable run: {s} {s} {s} ({d} recorded events)", .{
            run_key,
            parsed.request.method,
            parsed.request.url,
            parsed.events.len,
        });

        const success = recoverOne(allocator, config, &parsed, full_path) catch |err| {
            std.log.err("Recovery failed for '{s}': {}", .{ full_path, err });
            continue;
        };

        if (success) {
            recovered += 1;
        }
    }

    if (recovered > 0) {
        std.log.info("Recovered {d} incomplete request(s)", .{recovered});
    }

    return recovered;
}

fn recoverOne(
    allocator: std.mem.Allocator,
    config: ServerConfig,
    parsed: *const trace.IncompleteOplog,
    oplog_path: []const u8,
) !bool {
    const recovery_config = config.runtime_config;

    const rt = try Runtime.init(allocator, recovery_config);
    defer rt.deinit();

    // Load handler code
    const handler_code = switch (config.handler) {
        .file_path => |path| readFile(allocator, path) catch |err| {
            std.log.err("Failed to read handler '{s}': {}", .{ path, err });
            return err;
        },
        .inline_code => |code| try allocator.dupe(u8, code),
        .embedded_bytecode => {
            std.log.err("Durable recovery with embedded bytecode not yet supported", .{});
            return error.UnsupportedRecoverySource;
        },
    };
    defer allocator.free(handler_code);

    const handler_filename = switch (config.handler) {
        .file_path => |path| path,
        else => "eval",
    };

    try rt.loadCode(handler_code, handler_filename);
    rt.setPendingDurableRecovery(
        parsed.run_key orelse return error.MissingRunKey,
        oplog_path,
        parsed.events,
    );

    // Build request from oplog
    var headers_list: std.ArrayListUnmanaged(HttpHeader) = .empty;
    defer headers_list.deinit(allocator);
    try parseHeadersFromJson(allocator, parsed.request.headers_json, &headers_list);
    const body = if (parsed.request.body) |raw|
        try trace.unescapeJson(allocator, raw)
    else
        null;
    defer if (body) |owned| allocator.free(owned);

    const request = HttpRequestView{
        .method = parsed.request.method,
        .url = parsed.request.url,
        .headers = headers_list,
        .body = body,
    };

    // Execute handler - durable wrappers will replay from oplog, then continue live
    var response = rt.executeHandler(request) catch |err| {
        std.log.err("Recovery handler execution failed: {}", .{err});
        return false;
    };
    defer response.deinit();

    const updated = readFile(allocator, oplog_path) catch return false;
    defer allocator.free(updated);
    if (trace.isIncompleteOplog(updated)) return false;

    std.log.info("Recovered: {s} {s} -> {d}", .{ parsed.request.method, parsed.request.url, response.status });
    return true;
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    return zq.file_io.readFile(allocator, path, 100 * 1024 * 1024);
}

const parseHeadersFromJson = @import("trace_helpers.zig").parseHeadersFromJson;
