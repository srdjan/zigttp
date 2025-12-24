//! Native API Bindings for JS
//!
//! Implements Deno-style APIs:
//! - fetch(url, options?) -> Promise<Response>
//! - Deno.readTextFile(path) -> Promise<string>
//! - Deno.writeTextFile(path, content) -> Promise<void>
//! - Deno.readFile(path) -> Promise<Uint8Array>
//! - Deno.writeFile(path, data) -> Promise<void>
//! - setTimeout(callback, ms) -> number
//! - clearTimeout(id) -> void
//! - console.log/error/warn/info

const std = @import("std");
const mq = @import("mquickjs.zig");
const EventLoop = @import("event_loop.zig").EventLoop;
const PendingOp = @import("event_loop.zig").PendingOp;
const OpKind = @import("event_loop.zig").OpKind;
const OpState = @import("event_loop.zig").OpState;
const OpData = @import("event_loop.zig").OpData;

// ============================================================================
// Thread-local Event Loop Reference
// ============================================================================

threadlocal var current_loop: ?*EventLoop = null;
threadlocal var current_allocator: ?std.mem.Allocator = null;

pub fn setCurrentLoop(loop: *EventLoop, allocator: std.mem.Allocator) void {
    current_loop = loop;
    current_allocator = allocator;
}

pub fn clearCurrentLoop() void {
    current_loop = null;
    current_allocator = null;
}

// ============================================================================
// Binding Installation
// ============================================================================

pub fn installAllBindings(ctx: *mq.JSContext) !void {
    const global = mq.getGlobalObject(ctx);

    // Console
    try installConsole(ctx, global);

    // Fetch API
    try installFetch(ctx, global);

    // Deno namespace
    try installDeno(ctx, global);

    // Timers
    try installTimers(ctx, global);

    // Promise polyfill (if needed - mquickjs may have limited Promise support)
    try installPromiseHelpers(ctx, global);
}

// ============================================================================
// Console API
// ============================================================================

fn installConsole(ctx: *mq.JSContext, global: mq.JSValue) !void {
    const console = switch (mq.newObject(ctx)) {
        .ok => |c| c,
        .err => return error.BindingFailed,
    };

    inline for (.{
        .{ "log", consoleLog },
        .{ "error", consoleError },
        .{ "warn", consoleWarn },
        .{ "info", consoleInfo },
        .{ "debug", consoleDebug },
        .{ "time", consoleTime },
        .{ "timeEnd", consoleTimeEnd },
    }) |binding| {
        const func = mq.newCFunction(ctx, binding[1], binding[0], 1);
        if (func == .ok) {
            _ = mq.setPropertyStr(ctx, console, binding[0], func.ok);
        }
    }

    _ = mq.setPropertyStr(ctx, global, "console", console);
}

fn consoleLog(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    printToStream(ctx, argc, argv, std.fs.File.stdout().deprecatedWriter(), "");
    return mq.undefined_();
}

fn consoleError(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    printToStream(ctx, argc, argv, std.fs.File.stderr().deprecatedWriter(), "\x1b[31m[ERROR]\x1b[0m ");
    return mq.undefined_();
}

fn consoleWarn(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    printToStream(ctx, argc, argv, std.fs.File.stderr().deprecatedWriter(), "\x1b[33m[WARN]\x1b[0m ");
    return mq.undefined_();
}

fn consoleInfo(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    printToStream(ctx, argc, argv, std.fs.File.stdout().deprecatedWriter(), "\x1b[34m[INFO]\x1b[0m ");
    return mq.undefined_();
}

fn consoleDebug(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    printToStream(ctx, argc, argv, std.fs.File.stdout().deprecatedWriter(), "\x1b[90m[DEBUG]\x1b[0m ");
    return mq.undefined_();
}

var console_timers: std.StringHashMap(i64) = undefined;
var console_timers_init = false;

fn consoleTime(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    if (!console_timers_init) {
        console_timers = std.StringHashMap(i64).init(std.heap.page_allocator);
        console_timers_init = true;
    }

    const label = if (argc > 0) (mq.toCString(ctx, argv[0]).unwrapOr("default")) else "default";
    console_timers.put(label, std.time.milliTimestamp()) catch {};
    return mq.undefined_();
}

fn consoleTimeEnd(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    if (!console_timers_init) return mq.undefined_();

    const label = if (argc > 0) (mq.toCString(ctx, argv[0]).unwrapOr("default")) else "default";
    if (console_timers.get(label)) |start| {
        const elapsed = std.time.milliTimestamp() - start;
        const writer = std.fs.File.stdout().deprecatedWriter();
        writer.print("{s}: {d}ms\n", .{ label, elapsed }) catch {};
        _ = console_timers.remove(label);
    }
    return mq.undefined_();
}

fn printToStream(ctx: *mq.JSContext, argc: c_int, argv: [*c]mq.JSValue, writer: anytype, prefix: []const u8) void {
    if (prefix.len > 0) writer.writeAll(prefix) catch {};

    var i: c_int = 0;
    while (i < argc) : (i += 1) {
        if (i > 0) writer.writeAll(" ") catch {};
        printValue(ctx, argv[@intCast(i)], writer);
    }
    writer.writeAll("\n") catch {};
}

fn printValue(ctx: *mq.JSContext, val: mq.JSValue, writer: anytype) void {
    if (mq.isUndefined(val)) {
        writer.writeAll("undefined") catch {};
    } else if (mq.isNull(val)) {
        writer.writeAll("null") catch {};
    } else if (mq.isBool(val)) {
        writer.writeAll(if (mq.toBool(ctx, val)) "true" else "false") catch {};
    } else if (mq.isString(ctx, val)) {
        const str = mq.toCString(ctx, val).unwrapOr("");
        writer.writeAll(str) catch {};
    } else if (mq.isNumber(ctx, val)) {
        const int_result = mq.toInt32(ctx, val);
        if (int_result == .ok) {
            writer.print("{d}", .{int_result.ok}) catch {};
            return;
        }
        const f = mq.toFloat64(ctx, val).unwrapOr(0.0);
        writer.print("{d}", .{f}) catch {};
    } else if (mq.isArray(ctx, val)) {
        // JSON stringify arrays
        stringifyAndPrint(ctx, val, writer);
    } else if (mq.isFunction(ctx, val)) {
        writer.writeAll("[Function]") catch {};
    } else if (mq.isObject(ctx, val)) {
        stringifyAndPrint(ctx, val, writer);
    } else {
        writer.writeAll("[unknown]") catch {};
    }
}

fn stringifyAndPrint(ctx: *mq.JSContext, val: mq.JSValue, writer: anytype) void {
    const global = mq.getGlobalObject(ctx);
    const json = mq.getPropertyStr(ctx, global, "JSON");
    if (json == .ok) {
        const stringify = mq.getPropertyStr(ctx, json.ok, "stringify");
        if (stringify == .ok) {
            const args = [_]mq.JSValue{val};
            const result = mq.call(ctx, stringify.ok, mq.undefined_(), &args);
            if (result == .ok) {
                const str = mq.toCString(ctx, result.ok).unwrapOr("[Object]");
                writer.writeAll(str) catch {};
                return;
            }
        }
    }
    writer.writeAll("[Object]") catch {};
}

// ============================================================================
// Fetch API
// ============================================================================

/// Validate URL is allowed (SSRF prevention)
/// Blocks: file://, localhost, 127.0.0.1, private IP ranges
fn isUrlAllowed(url: []const u8) bool {
    // Must start with http:// or https://
    const http_prefix = "http://";
    const https_prefix = "https://";

    var host_start: usize = 0;
    if (std.mem.startsWith(u8, url, https_prefix)) {
        host_start = https_prefix.len;
    } else if (std.mem.startsWith(u8, url, http_prefix)) {
        host_start = http_prefix.len;
    } else {
        return false; // Block non-http schemes (file://, ftp://, etc.)
    }

    if (host_start >= url.len) return false;

    // Extract host portion (up to :, /, ?, or #)
    const rest = url[host_start..];
    var host_end: usize = rest.len;
    for (rest, 0..) |char, i| {
        if (char == ':' or char == '/' or char == '?' or char == '#') {
            host_end = i;
            break;
        }
    }
    const host = rest[0..host_end];

    if (host.len == 0) return false;

    // Block localhost variants (case-insensitive)
    if (std.ascii.eqlIgnoreCase(host, "localhost")) return false;

    // Block IPv4 loopback
    if (std.mem.eql(u8, host, "127.0.0.1")) return false;

    // Block IPv6 loopback
    if (std.mem.eql(u8, host, "::1")) return false;
    if (std.mem.eql(u8, host, "[::1]")) return false;

    // Block private IP ranges
    if (std.mem.startsWith(u8, host, "10.")) return false; // 10.0.0.0/8
    if (std.mem.startsWith(u8, host, "192.168.")) return false; // 192.168.0.0/16
    if (std.mem.startsWith(u8, host, "169.254.")) return false; // Link-local
    if (std.mem.startsWith(u8, host, "0.")) return false; // 0.0.0.0/8

    // Block 172.16.0.0 - 172.31.255.255
    if (std.mem.startsWith(u8, host, "172.")) {
        if (host.len > 4) {
            const dot_pos = std.mem.indexOf(u8, host[4..], ".") orelse return true;
            const second_octet = std.fmt.parseInt(u8, host[4 .. 4 + dot_pos], 10) catch return true;
            if (second_octet >= 16 and second_octet <= 31) return false;
        }
    }

    return true;
}

fn installFetch(ctx: *mq.JSContext, global: mq.JSValue) !void {
    const fetch_fn = mq.newCFunction(ctx, nativeFetch, "fetch", 2);
    if (fetch_fn == .ok) {
        _ = mq.setPropertyStr(ctx, global, "fetch", fetch_fn.ok);
    }
}

fn nativeFetch(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    const loop = current_loop orelse return mq.throwError(ctx, "No event loop");
    const allocator = current_allocator orelse return mq.throwError(ctx, "No allocator");

    if (argc < 1) {
        return mq.throwError(ctx, "fetch requires a URL argument");
    }

    // Get URL
    const url_str = mq.toCString(ctx, argv[0]);
    if (url_str == .err) {
        return mq.throwTypeError(ctx, "URL must be a string");
    }

    // Security: validate URL (SSRF prevention)
    if (!isUrlAllowed(url_str.ok)) {
        return mq.throwError(ctx, "URL not allowed: blocked protocol or host");
    }

    // Copy URL to owned memory
    const url = allocator.dupe(u8, url_str.ok) catch {
        return mq.throwError(ctx, "Out of memory");
    };

    // Parse options (method, body, headers)
    var method: std.http.Method = .GET;
    var body: ?[]const u8 = null;

    if (argc > 1 and mq.isObject(ctx, argv[1])) {
        const opts = argv[1];

        // Method
        const method_val = mq.getPropertyStr(ctx, opts, "method");
        if (method_val == .ok and mq.isString(ctx, method_val.ok)) {
            const method_str = mq.toCString(ctx, method_val.ok).unwrapOr("GET");
            method = parseHttpMethod(method_str);
        }

        // Body
        const body_val = mq.getPropertyStr(ctx, opts, "body");
        if (body_val == .ok and mq.isString(ctx, body_val.ok)) {
            const body_str = mq.toCString(ctx, body_val.ok).unwrapOr("");
            body = allocator.dupe(u8, body_str) catch null;
        }
    }

    // Create Promise
    return createPromiseWithOp(ctx, loop, .{
        .kind = .fetch,
        .state = .pending,
        .resolve_ref = null,
        .reject_ref = null,
        .data = .{ .fetch = .{
            .url = url,
            .method = method,
            .body = body,
            .request = null,
            .header_buf = undefined,
        } },
    }) catch {
        return mq.throwError(ctx, "Failed to create Promise");
    };
}

fn parseHttpMethod(s: []const u8) std.http.Method {
    if (std.ascii.eqlIgnoreCase(s, "GET")) return .GET;
    if (std.ascii.eqlIgnoreCase(s, "POST")) return .POST;
    if (std.ascii.eqlIgnoreCase(s, "PUT")) return .PUT;
    if (std.ascii.eqlIgnoreCase(s, "DELETE")) return .DELETE;
    if (std.ascii.eqlIgnoreCase(s, "PATCH")) return .PATCH;
    if (std.ascii.eqlIgnoreCase(s, "HEAD")) return .HEAD;
    if (std.ascii.eqlIgnoreCase(s, "OPTIONS")) return .OPTIONS;
    return .GET;
}

// ============================================================================
// Deno Namespace
// ============================================================================

/// Validate file path is safe (no traversal attacks)
fn isFilePathSafe(path: []const u8) bool {
    // Reject empty paths
    if (path.len == 0) return false;

    // Reject absolute paths
    if (path[0] == '/') return false;

    // Reject Windows absolute paths
    if (path.len >= 2 and path[1] == ':') return false;

    // Check for directory traversal
    var iter = std.mem.splitAny(u8, path, "/\\");
    while (iter.next()) |component| {
        if (std.mem.eql(u8, component, "..")) return false;
    }

    return true;
}

fn installDeno(ctx: *mq.JSContext, global: mq.JSValue) !void {
    const deno = switch (mq.newObject(ctx)) {
        .ok => |d| d,
        .err => return error.BindingFailed,
    };

    // Version info
    const version = switch (mq.newObject(ctx)) {
        .ok => |v| v,
        .err => return error.BindingFailed,
    };
    _ = mq.setPropertyStr(ctx, version, "deno", mq.fromString(ctx, "mqjs-0.1.0").unwrapOr(mq.undefined_()));
    _ = mq.setPropertyStr(ctx, version, "v8", mq.fromString(ctx, "mquickjs").unwrapOr(mq.undefined_()));
    _ = mq.setPropertyStr(ctx, version, "typescript", mq.fromString(ctx, "N/A").unwrapOr(mq.undefined_()));
    _ = mq.setPropertyStr(ctx, deno, "version", version);

    // File system functions
    inline for (.{
        .{ "readTextFile", denoReadTextFile },
        .{ "writeTextFile", denoWriteTextFile },
        .{ "readFile", denoReadFile },
        .{ "writeFile", denoWriteFile },
        .{ "remove", denoRemove },
        .{ "mkdir", denoMkdir },
        .{ "stat", denoStat },
        .{ "readDir", denoReadDir },
        .{ "cwd", denoCwd },
        .{ "exit", denoExit },
    }) |binding| {
        const func = mq.newCFunction(ctx, binding[1], binding[0], 2);
        if (func == .ok) {
            _ = mq.setPropertyStr(ctx, deno, binding[0], func.ok);
        }
    }

    // Environment
    const env = switch (mq.newObject(ctx)) {
        .ok => |e| e,
        .err => return error.BindingFailed,
    };
    const env_get = mq.newCFunction(ctx, denoEnvGet, "get", 1);
    if (env_get == .ok) _ = mq.setPropertyStr(ctx, env, "get", env_get.ok);
    const env_set = mq.newCFunction(ctx, denoEnvSet, "set", 2);
    if (env_set == .ok) _ = mq.setPropertyStr(ctx, env, "set", env_set.ok);
    _ = mq.setPropertyStr(ctx, deno, "env", env);

    _ = mq.setPropertyStr(ctx, global, "Deno", deno);
}

fn denoReadTextFile(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    const loop = current_loop orelse return mq.throwError(ctx, "No event loop");

    if (argc < 1) {
        return mq.throwError(ctx, "readTextFile requires a path argument");
    }

    const path_str = mq.toCString(ctx, argv[0]);
    if (path_str == .err) {
        return mq.throwTypeError(ctx, "Path must be a string");
    }

    // Security: validate path
    if (!isFilePathSafe(path_str.ok)) {
        return mq.throwError(ctx, "Invalid path: traversal not allowed");
    }

    return createPromiseWithOp(ctx, loop, .{
        .kind = .read_file,
        .state = .pending,
        .resolve_ref = null,
        .reject_ref = null,
        .data = .{ .read_file = .{ .path = path_str.ok } },
    }) catch {
        return mq.throwError(ctx, "Failed to create Promise");
    };
}

fn denoWriteTextFile(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    const loop = current_loop orelse return mq.throwError(ctx, "No event loop");

    if (argc < 2) {
        return mq.throwError(ctx, "writeTextFile requires path and content arguments");
    }

    const path_str = mq.toCString(ctx, argv[0]);
    const content_str = mq.toCString(ctx, argv[1]);

    if (path_str == .err or content_str == .err) {
        return mq.throwTypeError(ctx, "Arguments must be strings");
    }

    // Security: validate path
    if (!isFilePathSafe(path_str.ok)) {
        return mq.throwError(ctx, "Invalid path: traversal not allowed");
    }

    return createPromiseWithOp(ctx, loop, .{
        .kind = .write_file,
        .state = .pending,
        .resolve_ref = null,
        .reject_ref = null,
        .data = .{ .write_file = .{
            .path = path_str.ok,
            .content = content_str.ok,
        } },
    }) catch {
        return mq.throwError(ctx, "Failed to create Promise");
    };
}

fn denoReadFile(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    // For now, same as readTextFile - would return Uint8Array in full impl
    return denoReadTextFile(ctx, mq.undefined_(), argc, argv);
}

fn denoWriteFile(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    // For now, same as writeTextFile
    return denoWriteTextFile(ctx, mq.undefined_(), argc, argv);
}

fn denoRemove(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    if (argc < 1) return mq.throwError(ctx, "remove requires a path");

    const path = mq.toCString(ctx, argv[0]).unwrapOr("");

    // Security: validate path
    if (!isFilePathSafe(path)) {
        return mq.throwError(ctx, "Invalid path: traversal not allowed");
    }

    std.fs.cwd().deleteFile(path) catch |err| {
        if (err == error.IsDir) {
            std.fs.cwd().deleteDir(path) catch {
                return mq.throwError(ctx, "Failed to remove");
            };
        } else {
            return mq.throwError(ctx, "Failed to remove");
        }
    };
    return mq.undefined_();
}

fn denoMkdir(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    if (argc < 1) return mq.throwError(ctx, "mkdir requires a path");

    const path = mq.toCString(ctx, argv[0]).unwrapOr("");

    // Security: validate path
    if (!isFilePathSafe(path)) {
        return mq.throwError(ctx, "Invalid path: traversal not allowed");
    }

    std.fs.cwd().makeDir(path) catch {
        return mq.throwError(ctx, "Failed to create directory");
    };
    return mq.undefined_();
}

fn denoStat(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    if (argc < 1) return mq.throwError(ctx, "stat requires a path");

    const path = mq.toCString(ctx, argv[0]).unwrapOr("");

    // Security: validate path
    if (!isFilePathSafe(path)) {
        return mq.throwError(ctx, "Invalid path: traversal not allowed");
    }

    const stat = std.fs.cwd().statFile(path) catch {
        return mq.throwError(ctx, "Failed to stat");
    };

    const result = mq.newObject(ctx).unwrapOr(mq.undefined_());
    _ = mq.setPropertyStr(ctx, result, "size", mq.fromInt(ctx, @intCast(stat.size)));
    _ = mq.setPropertyStr(ctx, result, "isFile", mq.fromBool(stat.kind == .file));
    _ = mq.setPropertyStr(ctx, result, "isDirectory", mq.fromBool(stat.kind == .directory));
    _ = mq.setPropertyStr(ctx, result, "isSymlink", mq.fromBool(stat.kind == .sym_link));

    return result;
}

fn denoReadDir(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    if (argc < 1) return mq.throwError(ctx, "readDir requires a path");

    const path = mq.toCString(ctx, argv[0]).unwrapOr(".");

    // Security: validate path (allow "." for current directory)
    if (path.len > 0 and !std.mem.eql(u8, path, ".") and !isFilePathSafe(path)) {
        return mq.throwError(ctx, "Invalid path: traversal not allowed");
    }

    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch {
        return mq.throwError(ctx, "Failed to open directory");
    };
    defer dir.close();

    const result = mq.newArray(ctx).unwrapOr(mq.undefined_());
    var idx: u32 = 0;

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        const item = mq.newObject(ctx).unwrapOr(continue);
        _ = mq.setPropertyStr(ctx, item, "name", mq.fromString(ctx, entry.name).unwrapOr(mq.undefined_()));
        _ = mq.setPropertyStr(ctx, item, "isFile", mq.fromBool(entry.kind == .file));
        _ = mq.setPropertyStr(ctx, item, "isDirectory", mq.fromBool(entry.kind == .directory));
        _ = mq.setPropertyStr(ctx, item, "isSymlink", mq.fromBool(entry.kind == .sym_link));
        _ = mq.setPropertyUint32(ctx, result, idx, item);
        idx += 1;
    }

    return result;
}

fn denoCwd(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, _: c_int, _: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = std.fs.cwd().realpath(".", &buf) catch {
        return mq.throwError(ctx, "Failed to get cwd");
    };
    return mq.fromString(ctx, cwd).unwrapOr(mq.undefined_());
}

fn denoExit(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    const code: u8 = if (argc > 0) @intCast(mq.toInt32(ctx, argv[0]).unwrapOr(0)) else 0;
    std.process.exit(code);
}

fn denoEnvGet(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    if (argc < 1) return mq.undefined_();

    const key = mq.toCString(ctx, argv[0]).unwrapOr("");
    const value = std.posix.getenv(key) orelse return mq.undefined_();
    return mq.fromString(ctx, value).unwrapOr(mq.undefined_());
}

fn denoEnvSet(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    _ = ctx_;
    _ = argc;
    _ = argv;
    // Setting env vars is tricky - skip for now
    return mq.undefined_();
}

// ============================================================================
// Timers
// ============================================================================

var next_timer_id: u32 = 1;

fn installTimers(ctx: *mq.JSContext, global: mq.JSValue) !void {
    const set_timeout = mq.newCFunction(ctx, nativeSetTimeout, "setTimeout", 2);
    if (set_timeout == .ok) _ = mq.setPropertyStr(ctx, global, "setTimeout", set_timeout.ok);

    const clear_timeout = mq.newCFunction(ctx, nativeClearTimeout, "clearTimeout", 1);
    if (clear_timeout == .ok) _ = mq.setPropertyStr(ctx, global, "clearTimeout", clear_timeout.ok);

    const set_interval = mq.newCFunction(ctx, nativeSetInterval, "setInterval", 2);
    if (set_interval == .ok) _ = mq.setPropertyStr(ctx, global, "setInterval", set_interval.ok);

    const clear_interval = mq.newCFunction(ctx, nativeClearInterval, "clearInterval", 1);
    if (clear_interval == .ok) _ = mq.setPropertyStr(ctx, global, "clearInterval", clear_interval.ok);
}

fn nativeSetTimeout(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, argc: c_int, argv: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    const loop = current_loop orelse return mq.throwError(ctx, "No event loop");

    if (argc < 1) return mq.throwError(ctx, "setTimeout requires a callback");

    const callback = argv[0];
    if (!mq.isFunction(ctx, callback)) {
        return mq.throwTypeError(ctx, "First argument must be a function");
    }

    const delay_ms: i64 = if (argc > 1) mq.toInt32(ctx, argv[1]).unwrapOr(0) else 0;
    const deadline = std.time.milliTimestamp() + delay_ms;

    const timer_id = next_timer_id;
    next_timer_id +%= 1;

    // Schedule timeout - we need to store callback
    loop.scheduleOp(.{
        .kind = .timeout,
        .state = .pending,
        .resolve_ref = null, // Would need to store callback ref
        .reject_ref = null,
        .data = .{ .timeout = .{ .deadline_ms = deadline } },
    }) catch {
        return mq.throwError(ctx, "Failed to schedule timeout");
    };

    return mq.fromInt(ctx, @intCast(timer_id));
}

fn nativeClearTimeout(_: ?*mq.JSContext, _: [*c]mq.JSValue, _: c_int, _: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    // TODO: Implement timer cancellation
    return mq.undefined_();
}

fn nativeSetInterval(ctx_: ?*mq.JSContext, _: [*c]mq.JSValue, _: c_int, _: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    const ctx = ctx_.?;
    // TODO: Implement setInterval
    return mq.throwError(ctx, "setInterval not yet implemented");
}

fn nativeClearInterval(_: ?*mq.JSContext, _: [*c]mq.JSValue, _: c_int, _: [*c]mq.JSValue) callconv(.c) mq.JSValue {
    return mq.undefined_();
}

// ============================================================================
// Promise Helpers
// ============================================================================

fn installPromiseHelpers(ctx: *mq.JSContext, global: mq.JSValue) !void {
    // Check if Promise exists
    const promise_check = mq.getPropertyStr(ctx, global, "Promise");
    if (promise_check == .ok and mq.isFunction(ctx, promise_check.ok)) {
        // Promise already exists
        return;
    }

    // If mquickjs doesn't have Promise, we'd need to polyfill it
    // For now, assume it does (ES5+ subset typically includes Promise)
    std.log.warn("Promise may not be available in this mquickjs build", .{});
}

fn createPromiseWithOp(ctx: *mq.JSContext, loop: *EventLoop, op: PendingOp) !mq.JSValue {
    // Create a new Promise using the Promise constructor
    const global = mq.getGlobalObject(ctx);
    const promise_ctor = mq.getPropertyStr(ctx, global, "Promise");

    if (promise_ctor != .ok or !mq.isFunction(ctx, promise_ctor.ok)) {
        return error.NoPromise;
    }

    // We need to create an executor function that captures resolve/reject
    // This is tricky without closures in C - use a different approach

    // Create Promise.resolve() style for now (sync resolution during loop tick)
    // Real implementation would need executor pattern

    const new_op = op;
    try loop.scheduleOp(new_op);

    // Return a pending Promise placeholder
    // In full impl, would use new Promise((resolve, reject) => ...)
    const placeholder = mq.newObject(ctx);
    if (placeholder == .ok) {
        _ = mq.setPropertyStr(ctx, placeholder.ok, "__isPending", mq.fromBool(true));
    }
    return placeholder.unwrapOr(mq.undefined_());
}
