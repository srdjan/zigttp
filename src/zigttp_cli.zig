const std = @import("std");
const builtin = @import("builtin");

const Server = @import("server.zig").Server;
const ServerConfig = @import("server.zig").ServerConfig;
const HandlerSource = @import("server.zig").HandlerSource;
const RuntimeConfig = @import("zruntime.zig").RuntimeConfig;
const replay_runner = @import("replay_runner.zig");
const test_runner = @import("test_runner.zig");
const durable_recovery = @import("durable_recovery.zig");
const durable_scheduler = @import("durable_scheduler.zig");
const project_config_mod = @import("project_config");
const ProjectConfig = project_config_mod.ProjectConfig;
const zts = @import("zts");

const embedded_handler = @import("embedded_handler");

pub fn main(init: std.process.Init.Minimal) !void {
    var debug_alloc: if (builtin.mode == .Debug) std.heap.DebugAllocator(.{}) else void =
        if (builtin.mode == .Debug) .init else {};
    defer if (builtin.mode == .Debug) {
        _ = debug_alloc.deinit();
    };
    const allocator = if (builtin.mode == .Debug) debug_alloc.allocator() else std.heap.smp_allocator;

    const args = try collectArgs(allocator, init.args);
    defer {
        for (args) |arg| allocator.free(arg);
        allocator.free(args);
    }

    const user_args = args[1..];
    const command = if (user_args.len == 0) "" else user_args[0];

    if (user_args.len == 0) {
        if (embedded_handler.bytecode.len > 0) {
            try serveCommand(allocator, &.{});
            return;
        }
        printHelp();
        return;
    }

    if (std.mem.eql(u8, command, "serve")) {
        try serveCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "init")) {
        try initCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "dev")) {
        try devCommand(allocator, args[0], user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "doctor")) {
        try doctorCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "help")) {
        printHelp();
        return;
    }

    // Backward-compatible default: treat old `zigttp <handler>` usage as `serve`.
    try serveCommand(allocator, user_args);
}

fn serveCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const config = try parseServeArgs(allocator, argv);

    if (config.runtime_config.replay_file_path != null) {
        replay_runner.run(allocator, config) catch |err| {
            std.log.err("Replay error: {}", .{err});
            return;
        };
        return;
    }

    if (config.runtime_config.test_file_path != null) {
        test_runner.run(allocator, config) catch |err| {
            if (err != error.TestsFailed) {
                std.log.err("Test error: {}", .{err});
            }
            std.process.exit(if (err == error.TestsFailed) 1 else 2);
        };
        return;
    }

    var scheduler: ?durable_scheduler.DurableScheduler = null;
    defer if (scheduler) |*worker| worker.deinit();

    if (config.runtime_config.durable_oplog_dir != null) {
        _ = durable_recovery.recoverIncompleteOplogs(allocator, config) catch |err| {
            std.log.err("Durable recovery error: {}", .{err});
            return;
        };
        scheduler = .{ .config = config };
        scheduler.?.start() catch |err| {
            std.log.err("Durable scheduler error: {}", .{err});
            return;
        };
    }

    var server = Server.init(allocator, config) catch |err| {
        std.log.err("Failed to initialize server: {}", .{err});
        return;
    };
    defer server.deinit();

    server.run() catch |err| {
        std.log.err("Server error: {}", .{err});
        return;
    };
}

fn initCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len == 0) return error.MissingProjectName;

    var template_name: []const u8 = "basic";
    var project_name: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--template")) {
            i += 1;
            if (i >= argv.len) return error.MissingTemplate;
            template_name = argv[i];
            continue;
        }
        if (project_name == null) {
            project_name = arg;
            continue;
        }
        return error.InvalidArgument;
    }

    const name = project_name orelse return error.MissingProjectName;
    const template = parseTemplate(template_name) orelse return error.InvalidTemplate;

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, name);
    const src_dir = try std.fmt.allocPrint(allocator, "{s}/src", .{name});
    defer allocator.free(src_dir);
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, src_dir);
    const tests_dir = try std.fmt.allocPrint(allocator, "{s}/tests", .{name});
    defer allocator.free(tests_dir);
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, tests_dir);
    const public_dir = try std.fmt.allocPrint(allocator, "{s}/public", .{name});
    defer allocator.free(public_dir);
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, public_dir);

    try writeProjectFile(allocator, name, "zigttp.json", switch (template) {
        .basic, .api => defaultManifest,
        .htmx => htmxManifest,
    });
    try writeProjectFile(allocator, name, "src/handler.ts", switch (template) {
        .basic => basicHandler,
        .api => apiHandler,
        .htmx => htmxHandler,
    });
    try writeProjectFile(allocator, name, "tests/handler.test.jsonl", switch (template) {
        .basic => basicTests,
        .api => apiTests,
        .htmx => htmxTests,
    });
    try writeProjectFile(allocator, name, ".gitignore", gitignoreSource);
    try writeProjectFile(allocator, name, "README.md", switch (template) {
        .basic => basicReadme,
        .api => apiReadme,
        .htmx => htmxReadme,
    });
    try writeProjectFile(allocator, name, "public/.keep", "");

    std.debug.print("Initialized zigttp project in {s}\n", .{name});
    std.debug.print("Next steps:\n", .{});
    std.debug.print("  cd {s}\n", .{name});
    std.debug.print("  zigttp dev\n", .{});
    std.debug.print("  zts check\n", .{});
}

fn devCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
    try runDevPreflight(allocator, argv);

    var child_args = std.ArrayList([]const u8).empty;
    defer child_args.deinit(allocator);
    try child_args.append(allocator, program_path);
    try child_args.append(allocator, "serve");
    try child_args.appendSlice(allocator, argv);

    var watch = try buildWatchSet(allocator, argv);
    defer watch.deinit(allocator);

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var previous_stamp = try watch.computeStamp(io);

    while (true) {
        var child = try std.process.spawn(io, .{
            .argv = child_args.items,
            .stdin = .inherit,
            .stdout = .inherit,
            .stderr = .inherit,
        });

        while (true) {
            try std.Io.sleep(io, .fromMilliseconds(250), .awake);
            const next_stamp = try watch.computeStamp(io);
            if (next_stamp != previous_stamp) {
                previous_stamp = next_stamp;
                child.kill(io);
                std.debug.print("\nChange detected. Restarting zigttp serve...\n", .{});
                try runDevPreflight(allocator, argv);
                break;
            }
        }
    }
}

fn doctorCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const start_path = if (argv.len > 0) argv[0] else null;
    var project = try project_config_mod.discover(allocator, io, start_path);
    defer if (project) |*p| p.deinit(allocator);

    if (project) |*cfg| {
        std.debug.print("Manifest: {s}\n", .{cfg.manifest_path});
        const entry = try cfg.resolvedEntry(allocator);
        defer allocator.free(entry);
        std.debug.print("Entry:    {s}\n", .{entry});
        try std.Io.Dir.accessAbsolute(io, entry, .{});

        if (try cfg.resolvedStaticDir(allocator)) |static_dir| {
            defer allocator.free(static_dir);
            std.debug.print("Static:   {s}\n", .{static_dir});
            try std.Io.Dir.accessAbsolute(io, static_dir, .{});
        }
        if (try cfg.resolvedSqlitePath(allocator)) |sqlite_path| {
            defer allocator.free(sqlite_path);
            std.debug.print("SQLite:   {s}\n", .{sqlite_path});
        }
        if (try cfg.resolvedDurableDir(allocator)) |durable_dir| {
            defer allocator.free(durable_dir);
            std.debug.print("Durable:  {s}\n", .{durable_dir});
        }
        if (cfg.outbound_hosts.len > 1) {
            return error.UnsupportedMultipleOutboundHosts;
        }
        std.debug.print("Doctor: OK\n", .{});
        return;
    }

    if (start_path) |path| {
        std.debug.print("No zigttp.json found. Treating '{s}' as ad hoc source.\n", .{path});
        try std.Io.Dir.access(std.Io.Dir.cwd(), io, path, .{});
        std.debug.print("Doctor: OK\n", .{});
        return;
    }

    return error.NoProjectConfig;
}

fn parseServeArgs(allocator: std.mem.Allocator, argv: []const []const u8) !ServerConfig {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const explicit_path = findPositionalPath(argv);
    var project = try project_config_mod.discover(allocator, io, explicit_path);
    defer if (project) |*p| p.deinit(allocator);

    const has_embedded = embedded_handler.bytecode.len > 0;

    var config = ServerConfig{
        .handler = if (has_embedded)
            .{ .embedded_bytecode = &embedded_handler.bytecode }
        else if (project) |*cfg|
            .{ .file_path = cfg.entry }
        else
            .{ .inline_code = "" },
        .runtime_config = .{},
    };

    if (project) |*cfg| {
        config.port = cfg.port;
        config.host = cfg.host;
        config.enable_cors = cfg.cors;
        config.static_dir = cfg.static_dir;
        config.runtime_config.sqlite_path = cfg.sqlite;
        config.runtime_config.durable_oplog_dir = cfg.durable_dir;
        config.runtime_config.outbound_http_enabled = cfg.outbound_http;
        if (cfg.outbound_hosts.len > 1) return error.UnsupportedMultipleOutboundHosts;
        if (cfg.outbound_hosts.len == 1) {
            config.runtime_config.outbound_allow_host = cfg.outbound_hosts[0];
        }
    }

    var handler_set = has_embedded or project != null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help")) {
            printServeHelp();
            return error.HelpRequested;
        } else if (std.mem.eql(u8, arg, "-p") or std.mem.eql(u8, arg, "--port")) {
            i += 1;
            if (i >= argv.len) return error.MissingPortValue;
            config.port = std.fmt.parseInt(u16, argv[i], 10) catch return error.InvalidPort;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--host")) {
            i += 1;
            if (i >= argv.len) return error.MissingHostValue;
            config.host = argv[i];
        } else if (std.mem.eql(u8, arg, "-e") or std.mem.eql(u8, arg, "--eval")) {
            i += 1;
            if (i >= argv.len) return error.MissingEvalCode;
            config.handler = .{ .inline_code = argv[i] };
            handler_set = true;
        } else if (std.mem.eql(u8, arg, "-m") or std.mem.eql(u8, arg, "--memory")) {
            i += 1;
            if (i >= argv.len) return error.MissingMemoryValue;
            config.runtime_config.memory_limit = parseSize(argv[i]) catch return error.InvalidMemorySize;
        } else if (std.mem.eql(u8, arg, "-n") or std.mem.eql(u8, arg, "--pool")) {
            i += 1;
            if (i >= argv.len) return error.MissingPoolValue;
            config.pool_size = std.fmt.parseInt(usize, argv[i], 10) catch return error.InvalidPoolSize;
        } else if (std.mem.eql(u8, arg, "-q") or std.mem.eql(u8, arg, "--quiet")) {
            config.log_requests = false;
        } else if (std.mem.eql(u8, arg, "--cors")) {
            config.enable_cors = true;
        } else if (std.mem.eql(u8, arg, "--static")) {
            i += 1;
            if (i >= argv.len) return error.MissingStaticDir;
            config.static_dir = argv[i];
        } else if (std.mem.eql(u8, arg, "--outbound-http")) {
            config.runtime_config.outbound_http_enabled = true;
        } else if (std.mem.eql(u8, arg, "--outbound-host")) {
            i += 1;
            if (i >= argv.len) return error.MissingOutboundHost;
            config.runtime_config.outbound_http_enabled = true;
            config.runtime_config.outbound_allow_host = argv[i];
        } else if (std.mem.eql(u8, arg, "--outbound-timeout-ms")) {
            i += 1;
            if (i >= argv.len) return error.MissingOutboundTimeout;
            config.runtime_config.outbound_http_enabled = true;
            config.runtime_config.outbound_timeout_ms = std.fmt.parseInt(u32, argv[i], 10) catch return error.InvalidOutboundTimeout;
        } else if (std.mem.eql(u8, arg, "--outbound-max-response")) {
            i += 1;
            if (i >= argv.len) return error.MissingOutboundMaxResponse;
            config.runtime_config.outbound_http_enabled = true;
            config.runtime_config.outbound_max_response_bytes = parseSize(argv[i]) catch return error.InvalidOutboundMaxResponse;
        } else if (std.mem.eql(u8, arg, "--sqlite")) {
            i += 1;
            if (i >= argv.len) return error.MissingSqlitePath;
            config.runtime_config.sqlite_path = argv[i];
        } else if (std.mem.eql(u8, arg, "--trace")) {
            i += 1;
            if (i >= argv.len) return error.MissingTraceFile;
            config.runtime_config.trace_file_path = argv[i];
        } else if (std.mem.eql(u8, arg, "--replay")) {
            i += 1;
            if (i >= argv.len) return error.MissingReplayFile;
            config.runtime_config.replay_file_path = argv[i];
        } else if (std.mem.eql(u8, arg, "--test")) {
            i += 1;
            if (i >= argv.len) return error.MissingTestFile;
            config.runtime_config.test_file_path = argv[i];
        } else if (std.mem.eql(u8, arg, "--durable")) {
            i += 1;
            if (i >= argv.len) return error.MissingDurableDir;
            config.runtime_config.durable_oplog_dir = argv[i];
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            config.handler = .{ .file_path = arg };
            handler_set = true;
        } else {
            return error.UnknownOption;
        }
    }

    if (!handler_set) {
        std.log.err("No handler specified. Use `zigttp init`, `zigttp serve <file>`, or build with -Dhandler=<path>.", .{});
        return error.NoHandler;
    }

    return config;
}

fn runDevPreflight(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const zts_cli = @import("zts_cli");

    var check_args = std.ArrayList([]const u8).empty;
    defer check_args.deinit(allocator);
    try check_args.append(allocator, "check");

    const path = findPositionalPath(argv);
    if (path) |p| {
        try check_args.append(allocator, p);
    }

    try zts_cli.run(allocator, check_args.items);
}

const WatchSet = struct {
    paths: []const []const u8,

    fn deinit(self: *WatchSet, allocator: std.mem.Allocator) void {
        for (self.paths) |path| allocator.free(path);
        allocator.free(self.paths);
    }

    fn computeStamp(self: *const WatchSet, io: std.Io) !u64 {
        var hash = std.hash.Wyhash.init(0);
        for (self.paths) |path| {
            try foldPathIntoHash(io, &hash, path);
        }
        return hash.final();
    }
};

fn buildWatchSet(allocator: std.mem.Allocator, argv: []const []const u8) !WatchSet {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const explicit_path = findPositionalPath(argv);
    var project = try project_config_mod.discover(allocator, io, explicit_path);
    defer if (project) |*p| p.deinit(allocator);

    var paths = std.ArrayList([]const u8).empty;
    errdefer {
        for (paths.items) |path| allocator.free(path);
        paths.deinit(allocator);
    }

    if (project) |*cfg| {
        try paths.append(allocator, try allocator.dupe(u8, cfg.manifest_path));
        try paths.append(allocator, try std.fs.path.resolve(allocator, &.{ cfg.root_dir, "src" }));
        if (try cfg.resolvedStaticDir(allocator)) |static_dir| {
            try paths.append(allocator, static_dir);
        }
    } else if (explicit_path) |path| {
        if (looksLikeHandlerFile(path)) {
            const abs = try std.fs.path.resolve(allocator, &.{path});
            const dir_name = std.fs.path.dirname(abs) orelse ".";
            defer allocator.free(abs);
            try paths.append(allocator, try allocator.dupe(u8, dir_name));
        }
    }

    if (paths.items.len == 0) {
        try paths.append(allocator, try std.fs.path.resolve(allocator, &.{"."}));
    }

    return .{ .paths = try paths.toOwnedSlice(allocator) };
}

fn foldPathIntoHash(io: std.Io, hash: *std.hash.Wyhash, path: []const u8) !void {
    const stat = std.Io.Dir.statFile(std.Io.Dir.cwd(), io, path, .{}) catch |err| switch (err) {
        error.FileNotFound, error.NotDir => return,
        else => return err,
    };

    hash.update(path);
    hash.update(std.mem.asBytes(&stat.mtime.nanoseconds));
    hash.update(std.mem.asBytes(&stat.size));

    if (stat.kind != .directory) return;

    var dir = try std.Io.Dir.openDir(std.Io.Dir.cwd(), io, path, .{ .iterate = true });
    defer dir.close(io);

    var walker = try dir.walk(std.heap.smp_allocator);
    defer walker.deinit();

    while (try walker.next(io)) |entry| {
        const entry_stat = try std.Io.Dir.statFile(entry.dir, io, entry.basename, .{});
        hash.update(entry.path);
        hash.update(std.mem.asBytes(&entry_stat.mtime.nanoseconds));
        hash.update(std.mem.asBytes(&entry_stat.size));
    }
}

fn collectArgs(allocator: std.mem.Allocator, args_vector: std.process.Args) ![]const []const u8 {
    var args_iter = std.process.Args.Iterator.init(args_vector);
    defer args_iter.deinit();

    var args = std.ArrayList([]const u8).empty;
    errdefer args.deinit(allocator);

    while (args_iter.next()) |arg| {
        try args.append(allocator, try allocator.dupe(u8, arg));
    }

    return args.toOwnedSlice(allocator);
}

fn findPositionalPath(argv: []const []const u8) ?[]const u8 {
    var skip_next = false;
    for (argv) |arg| {
        if (skip_next) {
            skip_next = false;
            continue;
        }
        if (std.mem.eql(u8, arg, "-p") or
            std.mem.eql(u8, arg, "--port") or
            std.mem.eql(u8, arg, "-h") or
            std.mem.eql(u8, arg, "--host") or
            std.mem.eql(u8, arg, "-e") or
            std.mem.eql(u8, arg, "--eval") or
            std.mem.eql(u8, arg, "-m") or
            std.mem.eql(u8, arg, "--memory") or
            std.mem.eql(u8, arg, "-n") or
            std.mem.eql(u8, arg, "--pool") or
            std.mem.eql(u8, arg, "--static") or
            std.mem.eql(u8, arg, "--outbound-host") or
            std.mem.eql(u8, arg, "--outbound-timeout-ms") or
            std.mem.eql(u8, arg, "--outbound-max-response") or
            std.mem.eql(u8, arg, "--sqlite") or
            std.mem.eql(u8, arg, "--trace") or
            std.mem.eql(u8, arg, "--replay") or
            std.mem.eql(u8, arg, "--test") or
            std.mem.eql(u8, arg, "--durable"))
        {
            skip_next = true;
            continue;
        }
        if (std.mem.startsWith(u8, arg, "-")) continue;
        return arg;
    }
    return null;
}

fn looksLikeHandlerFile(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".js") or
        std.mem.endsWith(u8, path, ".jsx") or
        std.mem.endsWith(u8, path, ".ts") or
        std.mem.endsWith(u8, path, ".tsx");
}

fn parseSize(str: []const u8) !usize {
    var num_end: usize = 0;
    for (str, 0..) |c, i| {
        if (c >= '0' and c <= '9') {
            num_end = i + 1;
        } else {
            break;
        }
    }

    if (num_end == 0) return error.InvalidSize;

    const num = try std.fmt.parseInt(usize, str[0..num_end], 10);
    const suffix = str[num_end..];

    const multiplier: usize = if (suffix.len == 0)
        1
    else if (std.ascii.eqlIgnoreCase(suffix, "k") or std.ascii.eqlIgnoreCase(suffix, "kb"))
        1024
    else if (std.ascii.eqlIgnoreCase(suffix, "m") or std.ascii.eqlIgnoreCase(suffix, "mb"))
        1024 * 1024
    else if (std.ascii.eqlIgnoreCase(suffix, "g") or std.ascii.eqlIgnoreCase(suffix, "gb"))
        1024 * 1024 * 1024
    else
        return error.InvalidSizeSuffix;

    return std.math.mul(usize, num, multiplier) catch return error.InvalidSize;
}

fn printHelp() void {
    const help =
        \\zigttp - runtime and project workflow for zigttp handlers
        \\
        \\Usage:
        \\  zigttp init <name> [--template basic|api|htmx]
        \\  zigttp dev [handler-or-project]
        \\  zigttp serve [options] [handler.js]
        \\  zigttp doctor [path]
        \\
        \\Commands:
        \\  init    Create a starter zigttp project
        \\  dev     Run with restart-on-change
        \\  serve   Run the HTTP server
        \\  doctor  Validate project files and resolved paths
        \\
        \\Use `zts check` to run compiler checks and capability summaries.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printServeHelp() void {
    const help =
        \\zigttp serve [options] <handler.js>
        \\zigttp serve -e "function handler(req) { return Response.json({ok:true}) }"
        \\
        \\Options:
        \\  -p, --port <PORT>     Port to listen on
        \\  -h, --host <HOST>     Host to bind to
        \\  -e, --eval <CODE>     Evaluate inline JavaScript handler
        \\  -m, --memory <SIZE>   JS runtime memory limit
        \\  -n, --pool <N>        Runtime pool size
        \\  -q, --quiet           Disable request logging
        \\  --cors                Enable CORS headers
        \\  --static <DIR>        Serve static files from directory
        \\  --outbound-http       Enable native outbound HTTP bridge
        \\  --outbound-host <H>   Restrict outbound bridge to exact host H
        \\  --outbound-timeout-ms Connect timeout for outbound bridge in ms
        \\  --outbound-max-response <SIZE>
        \\  --sqlite <FILE>       SQLite database path for zigttp:sql
        \\  --trace <FILE>        Record handler I/O traces to JSONL file
        \\  --replay <FILE>       Replay recorded traces and verify handler output
        \\  --test <FILE>         Run declarative handler tests from JSONL file
        \\  --durable <DIR>       Enable durable execution with write-ahead oplog
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

const Template = enum { basic, api, htmx };

fn parseTemplate(name: []const u8) ?Template {
    if (std.mem.eql(u8, name, "basic")) return .basic;
    if (std.mem.eql(u8, name, "api")) return .api;
    if (std.mem.eql(u8, name, "htmx")) return .htmx;
    return null;
}

fn writeProjectFile(allocator: std.mem.Allocator, project_name: []const u8, relative_path: []const u8, data: []const u8) !void {
    const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ project_name, relative_path });
    defer allocator.free(full_path);
    try zts.file_io.writeFile(allocator, full_path, data);
}

const defaultManifest =
    \\{
    \\  "entry": "src/handler.ts",
    \\  "port": 3000
    \\}
;

const htmxManifest =
    \\{
    \\  "entry": "src/handler.ts",
    \\  "port": 3000,
    \\  "staticDir": "public"
    \\}
;

const basicHandler =
    \\function handler(req) {
    \\    if (req.method === "GET" && req.path === "/") {
    \\        return Response.text("hello from zigttp");
    \\    }
    \\
    \\    return Response.text("Not Found", { status: 404 });
    \\}
;

const apiHandler =
    \\function handler(req) {
    \\    if (req.method === "GET" && req.path === "/health") {
    \\        return Response.json({ ok: true });
    \\    }
    \\
    \\    if (req.method === "POST" && req.path === "/echo") {
    \\        return Response.json({
    \\            method: req.method,
    \\            body: req.body ?? ""
    \\        });
    \\    }
    \\
    \\    return Response.json({ error: "not found" }, { status: 404 });
    \\}
;

const htmxHandler =
    \\function Page() {
    \\    return (
    \\        <html>
    \\            <body>
    \\                <h1>zigttp</h1>
    \\                <button hx-get="/fragment" hx-target="#slot" hx-swap="innerHTML">
    \\                    Load fragment
    \\                </button>
    \\                <div id="slot">ready</div>
    \\            </body>
    \\        </html>
    \\    );
    \\}
    \\
    \\function handler(req) {
    \\    if (req.method === "GET" && req.path === "/") {
    \\        return Response.html(renderToString(<Page />));
    \\    }
    \\
    \\    if (req.method === "GET" && req.path === "/fragment") {
    \\        return Response.html("<p>loaded</p>");
    \\    }
    \\
    \\    return Response.text("Not Found", { status: 404 });
    \\}
;

const basicTests =
    \\{"type":"test","name":"root returns hello"}
    \\{"type":"request","method":"GET","url":"/","headers":{},"body":null}
    \\{"type":"expect","status":200,"body":"hello from zigttp"}
;

const apiTests =
    \\{"type":"test","name":"health returns ok"}
    \\{"type":"request","method":"GET","url":"/health","headers":{},"body":null}
    \\{"type":"expect","status":200,"body":"{\"ok\":true}"}
;

const htmxTests =
    \\{"type":"test","name":"root renders html"}
    \\{"type":"request","method":"GET","url":"/","headers":{},"body":null}
    \\{"type":"expect","status":200,"bodyContains":"zigttp"}
;

const gitignoreSource =
    \\.zig-cache/
    \\zig-out/
    \\.zigttp/
;

const basicReadme =
    \\# zigttp app
    \\
    \\## Commands
    \\- `zigttp dev`
    \\- `zigttp serve`
    \\- `zts check`
;

const apiReadme = basicReadme;
const htmxReadme = basicReadme;

test "parse size" {
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1k"));
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1K"));
    try std.testing.expectEqual(@as(usize, 1024), try parseSize("1kb"));
    try std.testing.expectEqual(@as(usize, 256 * 1024), try parseSize("256k"));
    try std.testing.expectEqual(@as(usize, 1024 * 1024), try parseSize("1m"));
    try std.testing.expectEqual(@as(usize, 1024 * 1024), try parseSize("1mb"));
    try std.testing.expectEqual(@as(usize, 100), try parseSize("100"));
}
