//! Durable Execution Recovery
//!
//! On server startup, scans the oplog directory for incomplete oplog files
//! and re-executes their handlers with the recorded I/O entries. The durable
//! wrappers replay from the oplog, then continue live for any remaining I/O.
//!
//! An oplog is incomplete if it contains a request entry but no "complete" marker.
//! After successful recovery, the oplog is deleted.

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

/// Scan the oplog directory and recover any incomplete requests.
/// Returns the number of requests recovered.
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
        if (!std.mem.startsWith(u8, name, "oplog-")) continue;
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

        // Check if incomplete (has request but no complete marker)
        if (!trace.isIncompleteOplog(source)) {
            // Already complete - safe to delete
            deleteFile(allocator, full_path);
            continue;
        }

        // Parse the incomplete oplog
        var parsed = trace.parseIncompleteOplog(allocator, source) catch |err| {
            std.log.err("Failed to parse oplog '{s}': {}", .{ full_path, err });
            continue;
        };
        defer parsed.deinit();

        std.log.info("Recovering request: {s} {s} ({d} I/O entries recorded)", .{
            parsed.request.method,
            parsed.request.url,
            parsed.io_calls.len,
        });

        // Re-execute with durable state containing the recorded entries
        const success = recoverOne(allocator, config, &parsed, full_path) catch |err| {
            std.log.err("Recovery failed for '{s}': {}", .{ full_path, err });
            continue;
        };

        if (success) {
            recovered += 1;
            // Oplog was marked complete inside recoverOne; now delete it
            deleteFile(allocator, full_path);
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
    // Create a runtime with durable mode enabled
    const recovery_config = config.runtime_config;
    // Keep durable_oplog_dir set so installVirtualModules picks durable wrappers
    // but we'll manage the DurableState ourselves

    const rt = try Runtime.init(allocator, recovery_config);
    defer rt.deinit();

    // Open the oplog file for appending (to continue writing I/O entries)
    const oplog_fd = openOplogForAppend(allocator, oplog_path) catch |err| {
        std.log.err("Failed to reopen oplog for recovery: {}", .{err});
        return err;
    };
    defer std.Io.Threaded.closeFd(oplog_fd);

    // Create DurableState with the recorded I/O entries for replay
    var durable_state = trace.DurableState.init(
        allocator,
        parsed.io_calls,
        oplog_fd,
    );
    defer durable_state.deinit();

    rt.ctx.setModuleState(
        trace.DURABLE_STATE_SLOT,
        @ptrCast(&durable_state),
        &trace.DurableState.deinitOpaque,
    );
    defer rt.ctx.module_state[trace.DURABLE_STATE_SLOT] = null;

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

    // Build request from oplog
    var headers_list: std.ArrayListUnmanaged(HttpHeader) = .{};
    defer headers_list.deinit(allocator);
    try parseHeadersFromJson(allocator, parsed.request.headers_json, &headers_list);

    const request = HttpRequestView{
        .method = parsed.request.method,
        .url = parsed.request.url,
        .headers = headers_list,
        .body = parsed.request.body,
    };

    // Execute handler - durable wrappers will replay from oplog, then continue live
    var response = rt.executeHandler(request) catch |err| {
        std.log.err("Recovery handler execution failed: {}", .{err});
        return false;
    };
    defer response.deinit();

    // The response was persisted and marked complete by traceRecordResponse
    // (via the DurableState in module_state). Log the result.
    std.log.info("Recovered: {s} {s} -> {d}", .{
        parsed.request.method,
        parsed.request.url,
        response.status,
    });

    return true;
}

fn openOplogForAppend(allocator: std.mem.Allocator, path: []const u8) !std.c.fd_t {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .APPEND = true },
        0,
    ) catch return error.FileOpenFailed;

    return fd;
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0) catch {
        return error.FileNotFound;
    };
    defer std.Io.Threaded.closeFd(fd);

    const max_size = 100 * 1024 * 1024;
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

fn deleteFile(allocator: std.mem.Allocator, path: []const u8) void {
    const path_z = allocator.dupeZ(u8, path) catch return;
    defer allocator.free(path_z);
    _ = std.c.unlink(path_z);
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
        while (pos < json.len and (json[pos] == ' ' or json[pos] == ',' or json[pos] == '\n')) : (pos += 1) {}
        if (pos >= json.len or json[pos] == '}') break;

        if (json[pos] != '"') break;
        pos += 1;
        const key_start = pos;
        while (pos < json.len and json[pos] != '"') : (pos += 1) {
            if (json[pos] == '\\') pos += 1;
        }
        const key = json[key_start..pos];
        if (pos < json.len) pos += 1;

        while (pos < json.len and (json[pos] == ':' or json[pos] == ' ')) : (pos += 1) {}

        if (pos >= json.len or json[pos] != '"') break;
        pos += 1;
        const val_start = pos;
        while (pos < json.len and json[pos] != '"') : (pos += 1) {
            if (json[pos] == '\\') pos += 1;
        }
        const val = json[val_start..pos];
        if (pos < json.len) pos += 1;

        try headers.append(allocator, .{ .key = key, .value = val });
    }
}
