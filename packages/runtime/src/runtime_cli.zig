const std = @import("std");
const builtin = @import("builtin");

const Server = @import("server.zig").Server;
const ServerConfig = @import("server.zig").ServerConfig;
const edge_server = @import("edge_server.zig");
const contract_runtime = @import("contract_runtime.zig");
const replay_runner = @import("replay_runner.zig");
const test_runner = @import("test_runner.zig");
const durable_recovery = @import("durable_recovery.zig");
const durable_scheduler = @import("durable_scheduler.zig");
const feature_options = @import("runtime_feature_options");
const project_config_mod = @import("project_config");
const self_extract = @import("self_extract.zig");
const live_reload_mod = @import("runtime_features.zig").live_reload;
const shared = @import("cli_shared.zig");

const embedded_handler = @import("embedded_handler");

pub fn main(init: std.process.Init.Minimal) !void {
    var debug_alloc: if (builtin.mode == .Debug) std.heap.DebugAllocator(.{}) else void =
        if (builtin.mode == .Debug) .init else {};
    defer if (builtin.mode == .Debug) {
        _ = debug_alloc.deinit();
    };
    const allocator = if (builtin.mode == .Debug) debug_alloc.allocator() else std.heap.smp_allocator;

    // Check for self-extracting binary payload before anything else
    const self_payload = self_extract.detect(allocator) catch null;

    const args = try shared.collectArgs(allocator, init.args);
    defer {
        for (args) |arg| allocator.free(arg);
        allocator.free(args);
    }

    const user_args = args[1..];
    const command = if (user_args.len == 0) "" else user_args[0];

    // Self-extracting binary: serve the appended handler
    if (self_payload) |payload| {
        defer payload.deinit(allocator);
        try serveAppended(allocator, &payload, user_args);
        return;
    }

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
    if (std.mem.eql(u8, command, "edge")) {
        try edgeCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "attest")) {
        try attestCommand(allocator);
        return;
    }
    if (std.mem.eql(u8, command, "version") or std.mem.eql(u8, command, "--version")) {
        shared.printVersion();
        return;
    }
    if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "help")) {
        printHelp();
        return;
    }

    // Backward-compatible default: treat bare `zigttp <handler>` usage as `serve`.
    try serveCommand(allocator, user_args);
}

pub fn edgeCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const config_path = parseEdgeConfigPath(argv) catch |err| {
        if (err == error.HelpRequested) return;
        return err;
    };

    var config = try edge_server.loadConfig(allocator, config_path);
    var edge = edge_server.EdgeServer.init(allocator, config) catch |err| {
        config.deinit(allocator);
        std.log.err("Failed to initialize edge runtime: {}", .{err});
        std.process.exit(1);
    };
    defer edge.deinit();

    edge.run() catch |err| {
        if (err == error.TlsTerminationNotImplemented) {
            std.log.err("HTTPS listener config is parsed but TLS termination is not wired yet; use protocol \"http\" for this edge runtime milestone.", .{});
            std.process.exit(1);
        }
        std.log.err("Edge runtime error: {}", .{err});
        std.process.exit(1);
    };
}

fn parseEdgeConfigPath(argv: []const []const u8) ![]const u8 {
    var path: ?[]const u8 = null;
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help")) {
            printEdgeHelp();
            return error.HelpRequested;
        }
        if (std.mem.eql(u8, arg, "-c") or std.mem.eql(u8, arg, "--config")) {
            i += 1;
            if (i >= argv.len) return error.MissingEdgeConfig;
            path = argv[i];
            continue;
        }
        if (!std.mem.startsWith(u8, arg, "-")) {
            path = arg;
            continue;
        }
        return error.UnknownOption;
    }
    return path orelse "zigttp.edge.json";
}

fn attestCommand(allocator: std.mem.Allocator) !void {
    var payload = (try self_extract.detect(allocator)) orelse {
        shared.writeStdoutLine("{\"error\":\"no embedded payload; attest is only available on self-extracting binaries\"}");
        std.process.exit(1);
    };
    defer payload.deinit(allocator);

    const contract_json = payload.contract_json orelse {
        shared.writeStdoutLine("{\"error\":\"embedded payload has no contract\"}");
        std.process.exit(1);
    };

    var raw = contract_runtime.parseContractJson(allocator, contract_json) catch {
        shared.writeStdoutLine("{\"error\":\"failed to parse contract\"}");
        std.process.exit(1);
    };
    defer raw.deinit();
    const contract = raw.rawView();

    var actual: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(payload.bytecode, &actual, .{});

    const policy_hex = std.fmt.bytesToHex(contract.policy_hash, .lower);
    const artifact_hex = std.fmt.bytesToHex(contract.artifact_sha256, .lower);
    const actual_hex = std.fmt.bytesToHex(actual, .lower);
    const cap_hex = if (contract.capabilities) |caps|
        std.fmt.bytesToHex(caps.hash, .lower)
    else
        [_]u8{'0'} ** 64;

    var buf: [1024]u8 = undefined;
    const line = try std.fmt.bufPrint(
        &buf,
        "{{\"policyHash\":\"{s}\",\"artifactSha256\":\"{s}\",\"artifactActual\":\"{s}\",\"capabilityHash\":\"{s}\"}}\n",
        .{ &policy_hex, &artifact_hex, &actual_hex, &cap_hex },
    );
    _ = std.c.write(std.c.STDOUT_FILENO, line.ptr, line.len);
}

pub fn serveCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const feature_flags = parseServeFeatureFlags(argv);
    if (feature_flags.studio_enabled) {
        if (!feature_options.enable_studio) {
            std.log.err("--studio is not available in zigttp-runtime; use the zigttp developer CLI", .{});
            std.process.exit(1);
        }
    }
    if (feature_flags.watch_enabled and !feature_options.enable_live_reload) {
        std.log.err("--watch is not available in zigttp-runtime; use the zigttp developer CLI", .{});
        std.process.exit(1);
    }

    var config = parseServeArgs(allocator, argv) catch |err| {
        if (err == error.HelpRequested) return;
        return err;
    };
    if (feature_flags.studio_enabled) config.studio = true;
    if (feature_flags.demo_enabled) {
        config.studio = true;
        config.studio_demo_root = ".";
    }

    if (config.runtime_config.replay_file_path != null) {
        replay_runner.run(allocator, config) catch |err| {
            std.log.err("Replay error: {}", .{err});
            return;
        };
        return;
    }

    if (config.runtime_config.test_file_path != null) {
        test_runner.run(allocator, config) catch |err| {
            if (err != error.TestsFailed and err != error.InvalidTestFixture) {
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

    // Start with proven live reload if --watch was requested
    if (feature_flags.watch_enabled) {
        const handler_path = switch (config.handler) {
            .file_path => |path| path,
            else => {
                std.log.err("--watch requires a file-based handler (not --eval or embedded)", .{});
                return;
            },
        };

        var watch_set = shared.buildWatchSet(allocator, argv) catch |err| {
            std.log.err("Failed to build watch set: {}", .{err});
            return;
        };
        defer watch_set.deinit(allocator);

        var live_reload = live_reload_mod.LiveReloadState.init(
            allocator,
            &server,
            handler_path,
            watch_set.paths,
            .{
                .prove = feature_flags.prove_enabled,
                .force_swap = feature_flags.force_swap,
                .quest = .{ .enabled = feature_flags.quest_enabled, .explicit = feature_flags.quest_explicit },
            },
        );
        defer live_reload.deinit();

        server.runWithBackgroundWork(&live_reload, watcherThread) catch |err| {
            std.log.err("Server error: {}", .{err});
            return;
        };
    } else {
        server.run() catch |err| {
            std.log.err("Server error: {}", .{err});
            return;
        };
    }
}

const ServeFeatureFlags = struct {
    watch_enabled: bool = false,
    prove_enabled: bool = false,
    force_swap: bool = false,
    studio_enabled: bool = false,
    demo_enabled: bool = false,
    quest_enabled: bool = false,
    quest_explicit: bool = false,
};

fn parseServeFeatureFlags(argv: []const []const u8) ServeFeatureFlags {
    var flags = ServeFeatureFlags{};
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--watch")) flags.watch_enabled = true;
        if (std.mem.eql(u8, arg, "--prove")) flags.prove_enabled = true;
        if (std.mem.eql(u8, arg, "--force-swap")) flags.force_swap = true;
        if (std.mem.eql(u8, arg, "--studio")) flags.studio_enabled = true;
        if (std.mem.eql(u8, arg, "--demo")) flags.demo_enabled = true;
        if (std.mem.eql(u8, arg, "--quest")) {
            flags.quest_enabled = true;
            flags.quest_explicit = true;
        }
        if (std.mem.eql(u8, arg, "--quest-default")) flags.quest_enabled = true;
    }

    if (flags.demo_enabled) flags.studio_enabled = true;
    if (flags.studio_enabled or flags.quest_enabled) {
        flags.watch_enabled = true;
        flags.prove_enabled = true;
    }
    return flags;
}

/// Parse a single flag shared between `serve` and `serve --appended`.
/// Returns true if `arg` (at index `i.*`) was recognized. Advances `i.*`
/// past consumed value tokens. Returns false if the flag is not one of the
/// shared set, leaving `i.*` unchanged so the caller can handle it.
fn parseCommonServeFlag(
    arg: []const u8,
    i: *usize,
    argv: []const []const u8,
    config: *ServerConfig,
) !bool {
    if (std.mem.eql(u8, arg, "-p") or std.mem.eql(u8, arg, "--port")) {
        i.* += 1;
        if (i.* >= argv.len) return error.MissingPortValue;
        config.port = std.fmt.parseInt(u16, argv[i.*], 10) catch return error.InvalidPort;
        return true;
    }
    if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--host")) {
        i.* += 1;
        if (i.* >= argv.len) return error.MissingHostValue;
        config.host = argv[i.*];
        return true;
    }
    if (std.mem.eql(u8, arg, "--cors")) {
        config.enable_cors = true;
        return true;
    }
    if (std.mem.eql(u8, arg, "-q") or std.mem.eql(u8, arg, "--quiet")) {
        config.log_requests = false;
        return true;
    }
    if (std.mem.eql(u8, arg, "--system")) {
        i.* += 1;
        if (i.* >= argv.len) return error.MissingSystemFile;
        config.runtime_config.system_config_path = argv[i.*];
        return true;
    }
    if (std.mem.eql(u8, arg, "--durable")) {
        i.* += 1;
        if (i.* >= argv.len) return error.MissingDurableDir;
        config.runtime_config.durable_oplog_dir = argv[i.*];
        return true;
    }
    if (std.mem.eql(u8, arg, "--outbound-http")) {
        config.runtime_config.outbound_http_enabled = true;
        return true;
    }
    if (std.mem.eql(u8, arg, "--outbound-host")) {
        i.* += 1;
        if (i.* >= argv.len) return error.MissingOutboundHost;
        config.runtime_config.outbound_http_enabled = true;
        config.runtime_config.outbound_allow_host = argv[i.*];
        return true;
    }
    if (std.mem.eql(u8, arg, "--no-env-check")) {
        config.skip_env_check = true;
        return true;
    }
    if (std.mem.eql(u8, arg, "--security-log")) {
        i.* += 1;
        if (i.* >= argv.len) return error.MissingSecurityLogPath;
        config.security_log_path = argv[i.*];
        return true;
    }
    if (std.mem.eql(u8, arg, "--lifecycle")) {
        i.* += 1;
        if (i.* >= argv.len) return error.MissingLifecycleValue;
        config.lifecycle_override = parseLifecycle(argv[i.*]) orelse return error.InvalidLifecycleValue;
        return true;
    }
    return false;
}

fn parseServeArgs(allocator: std.mem.Allocator, argv: []const []const u8) !ServerConfig {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const explicit_path = shared.findPositionalPath(argv);
    var project = try project_config_mod.discover(allocator, io, explicit_path);
    // ServerConfig borrows strings (host, entry, static_dir, ...) directly
    // from `project`. Those references must outlive parseServeArgs, so we
    // only deinit on error - on success the project is intentionally leaked
    // for the rest of the process. serveCommand runs until the process exits.
    errdefer if (project) |*p| p.deinit(allocator);

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
        config.runtime_config.system_config_path = cfg.system;
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
        if (try parseCommonServeFlag(arg, &i, argv, &config)) continue;
        if (std.mem.eql(u8, arg, "--help")) {
            printServeHelp();
            return error.HelpRequested;
        } else if (std.mem.eql(u8, arg, "-e") or std.mem.eql(u8, arg, "--eval")) {
            i += 1;
            if (i >= argv.len) return error.MissingEvalCode;
            config.handler = .{ .inline_code = argv[i] };
            handler_set = true;
        } else if (std.mem.eql(u8, arg, "-m") or std.mem.eql(u8, arg, "--memory")) {
            i += 1;
            if (i >= argv.len) return error.MissingMemoryValue;
            config.runtime_config.memory_limit = shared.parseSize(argv[i]) catch return error.InvalidMemorySize;
        } else if (std.mem.eql(u8, arg, "--max-body-size")) {
            i += 1;
            if (i >= argv.len) return error.MissingMaxBodySize;
            config.max_body_size = shared.parseSize(argv[i]) catch return error.InvalidMaxBodySize;
        } else if (std.mem.eql(u8, arg, "-n") or std.mem.eql(u8, arg, "--pool")) {
            i += 1;
            if (i >= argv.len) return error.MissingPoolValue;
            config.pool_size = std.fmt.parseInt(usize, argv[i], 10) catch return error.InvalidPoolSize;
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
            config.runtime_config.outbound_max_response_bytes = shared.parseSize(argv[i]) catch return error.InvalidOutboundMaxResponse;
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
        } else if (std.mem.eql(u8, arg, "--watch") or
            std.mem.eql(u8, arg, "--prove") or
            std.mem.eql(u8, arg, "--force-swap") or
            std.mem.eql(u8, arg, "--studio") or
            std.mem.eql(u8, arg, "--demo") or
            std.mem.eql(u8, arg, "--quest") or
            std.mem.eql(u8, arg, "--quest-default"))
        {
            // Handled by serveCommand before parseServeArgs is called
            continue;
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

fn watcherThread(live_reload: *live_reload_mod.LiveReloadState) void {
    var io_backend = std.Io.Threaded.init(live_reload.allocator, .{ .environ = .empty });
    defer io_backend.deinit();

    live_reload.run(io_backend.io()) catch |err| {
        std.log.err("Live reload error: {}", .{err});
    };
}

fn parseLifecycle(str: []const u8) ?contract_runtime.PoolingPolicy {
    if (std.mem.eql(u8, str, "ephemeral")) return .ephemeral;
    if (std.mem.eql(u8, str, "bounded")) return .reuse_bounded_by_count;
    if (std.mem.eql(u8, str, "ttl")) return .reuse_bounded_by_ttl;
    if (std.mem.eql(u8, str, "reuse")) return .reuse_unbounded;
    return null;
}

fn serveAppended(allocator: std.mem.Allocator, payload: *const self_extract.Payload, argv: []const []const u8) !void {
    // Matches the curl hint printed by `zigttp deploy`; -p PORT still overrides.
    var config = ServerConfig{
        .handler = .{ .appended_payload = .{
            .bytecode = payload.bytecode,
            .dep_bytecodes = payload.dep_bytecodes,
        } },
        .contract_json = payload.contract_json,
        .attestation_jws = payload.attestation_jws,
        .port = 3000,
    };

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (try parseCommonServeFlag(arg, &i, argv, &config)) continue;
        if (std.mem.eql(u8, arg, "--help")) {
            const help = "Usage: <binary> [-p PORT] [-h HOST] [--cors] [-q] [--no-env-check] [--security-log FILE] [--lifecycle ephemeral|bounded|reuse] [--system FILE]\n";
            _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
            return;
        }
        // Silently ignore unknown flags for forward compatibility
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

fn printHelp() void {
    const help =
        \\zigttp - serverless runtime
        \\
        \\Usage:
        \\  zigttp serve [options] [handler.ts]    Run handler
        \\  zigttp edge [--config FILE]            Run in-process edge runtime
        \\  zigttp attest                           Inspect embedded proof artifact
        \\  zigttp version                          Show version
        \\  zigttp help                             Show this help
        \\
        \\For compile, prove, and expert commands, use `zigts`.
        \\For init, dev, and deploy commands, use `zigttp`.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printEdgeHelp() void {
    const help =
        \\zigttp edge [options]
        \\
        \\Options:
        \\  -c, --config <FILE>  Edge config JSON (default: zigttp.edge.json)
        \\
        \\Config shape:
        \\  {
        \\    "listener": {"host":"127.0.0.1","port":8080,"protocol":"http"},
        \\    "handlers": [{"name":"api","entry":"src/handler.ts","pool":8}],
        \\    "routes": [{"host":"*","method":"*","pathPrefix":"/","target":"api"}]
        \\  }
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
        \\  --system <FILE>       System registry for zigttp:service
        \\  --watch               Auto-reload on source change
        \\  --prove               With --watch, gate swaps on upgrade verdict
        \\  --force-swap          With --watch, apply breaking swaps anyway
        \\  --studio              Serve the local proof workbench
        \\  --quest               Run the first-run proof quest
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

test "parseCommonServeFlag: -p consumes next arg as port" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{ "-p", "4242" };
    var i: usize = 0;
    try std.testing.expect(try parseCommonServeFlag(argv[0], &i, &argv, &config));
    try std.testing.expectEqual(@as(u16, 4242), config.port);
    try std.testing.expectEqual(@as(usize, 1), i);
}

test "parseServeFeatureFlags: quest implies watch and prove" {
    const quest = parseServeFeatureFlags(&.{"--quest"});
    try std.testing.expect(quest.quest_enabled);
    try std.testing.expect(quest.quest_explicit);
    try std.testing.expect(quest.watch_enabled);
    try std.testing.expect(quest.prove_enabled);

    const quest_default = parseServeFeatureFlags(&.{"--quest-default"});
    try std.testing.expect(quest_default.quest_enabled);
    try std.testing.expect(!quest_default.quest_explicit);
    try std.testing.expect(quest_default.watch_enabled);
    try std.testing.expect(quest_default.prove_enabled);
}

test "parseServeFeatureFlags: studio and demo imply proof watch loop" {
    const studio = parseServeFeatureFlags(&.{"--studio"});
    try std.testing.expect(studio.studio_enabled);
    try std.testing.expect(studio.watch_enabled);
    try std.testing.expect(studio.prove_enabled);

    const demo = parseServeFeatureFlags(&.{"--demo"});
    try std.testing.expect(demo.demo_enabled);
    try std.testing.expect(demo.studio_enabled);
    try std.testing.expect(demo.watch_enabled);
    try std.testing.expect(demo.prove_enabled);
}

test "parseCommonServeFlag: --cors toggles enable_cors, no value consumed" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{"--cors"};
    var i: usize = 0;
    try std.testing.expect(try parseCommonServeFlag(argv[0], &i, &argv, &config));
    try std.testing.expect(config.enable_cors);
    try std.testing.expectEqual(@as(usize, 0), i);
}

test "parseCommonServeFlag: returns false for unrelated flag" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{"--eval"};
    var i: usize = 0;
    try std.testing.expect(!try parseCommonServeFlag(argv[0], &i, &argv, &config));
    try std.testing.expectEqual(@as(usize, 0), i);
}

test "parseCommonServeFlag: missing value returns error" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{"-p"};
    var i: usize = 0;
    try std.testing.expectError(error.MissingPortValue, parseCommonServeFlag(argv[0], &i, &argv, &config));
}

test "parseCommonServeFlag: --lifecycle parses known value" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{ "--lifecycle", "ephemeral" };
    var i: usize = 0;
    try std.testing.expect(try parseCommonServeFlag(argv[0], &i, &argv, &config));
    try std.testing.expectEqual(contract_runtime.PoolingPolicy.ephemeral, config.lifecycle_override.?);
}

test "parseCommonServeFlag: --lifecycle rejects unknown value" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{ "--lifecycle", "nonsense" };
    var i: usize = 0;
    try std.testing.expectError(error.InvalidLifecycleValue, parseCommonServeFlag(argv[0], &i, &argv, &config));
}
