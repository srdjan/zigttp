const std = @import("std");
const builtin = @import("builtin");

const Server = @import("server.zig").Server;
const ServerConfig = @import("server.zig").ServerConfig;
const edge_server = @import("runtime_features.zig").edge;
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
const workflow_queue_cli = @import("workflow_queue_cli.zig");
const durable_dead_runs_cli = @import("durable_dead_runs_cli.zig");

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

    if (std.mem.eql(u8, command, "workflow-queue")) {
        try workflowQueueCommand(allocator, user_args[1..]);
        return;
    }

    if (std.mem.eql(u8, command, "durable")) {
        try durableCommand(allocator, user_args[1..]);
        return;
    }

    if (self_payload) |payload| {
        defer payload.deinit(allocator);
        switch (classifyAppendedInvocation(user_args)) {
            .serve => {
                serveAppended(allocator, &payload, user_args) catch |err| {
                    if (err == error.HelpRequested) return;
                    if (err == error.UnknownArgument or err == error.UnknownOption) std.process.exit(1);
                    return err;
                };
                return;
            },
            .attest => {
                try attestPayload(allocator, &payload);
                return;
            },
            .version => {
                shared.printVersion();
                return;
            },
            .help => {
                printAppendedHelp();
                return;
            },
            .unknown_positional => |arg| {
                std.debug.print("Unknown argument for self-contained binary: {s}\n\n", .{arg});
                printAppendedHelp();
                std.process.exit(1);
            },
        }
        return;
    }

    if (user_args.len == 0) {
        if (embedded_handler.bytecode.len > 0) {
            try serveCommandWithEnviron(allocator, &.{}, init.environ);
            return;
        }
        printHelp();
        return;
    }

    if (std.mem.eql(u8, command, "serve")) {
        try serveCommandWithEnviron(allocator, user_args[1..], init.environ);
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
    try serveCommandWithEnviron(allocator, user_args, init.environ);
}

pub fn workflowQueueCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    workflow_queue_cli.run(allocator, argv) catch |err| {
        if (workflow_queue_cli.isExpectedUserError(err)) std.process.exit(1);
        return err;
    };
}

/// Dispatches `zigttp durable <sub-verb> ...`. Only `dead-runs` exists today;
/// structured as its own top-level command (rather than folding straight
/// into `dead-runs` verbs) so other durable-run operator surfaces have a
/// natural home under `durable` later without a breaking rename.
pub fn durableCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len == 0 or std.mem.eql(u8, argv[0], "--help") or std.mem.eql(u8, argv[0], "help")) {
        const usage =
            \\zigttp durable - durable-run operator commands
            \\
            \\Usage:
            \\  zigttp durable dead-runs list --durable <DIR>
            \\  zigttp durable dead-runs show --durable <DIR> <ID>
            \\  zigttp durable dead-runs replay --durable <DIR> <ID>
            \\  zigttp durable dead-runs discard --durable <DIR> <ID>
            \\
        ;
        _ = std.c.write(std.c.STDOUT_FILENO, usage.ptr, usage.len);
        return;
    }
    if (std.mem.eql(u8, argv[0], "dead-runs")) {
        durable_dead_runs_cli.run(allocator, argv[1..]) catch |err| {
            if (durable_dead_runs_cli.isExpectedUserError(err)) std.process.exit(1);
            return err;
        };
        return;
    }
    std.debug.print("zigttp durable: unknown subcommand '{s}'. Run `zigttp durable --help` for usage.\n", .{argv[0]});
    std.process.exit(1);
}

pub fn edgeCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const config_path = parseEdgeConfigPath(argv) catch |err| {
        switch (err) {
            // --help printed usage and is not an error.
            error.HelpRequested => return,
            // On a default (edge-compiled-out) build, the more useful signal is
            // that edge is unavailable; let that win over an arg-parse error.
            error.UnknownOption, error.MissingEdgeConfig => {
                if (!feature_options.enable_edge) shared.featureCompiledOut("edge", "edge");
                std.log.err("zigttp edge: invalid arguments", .{});
                std.process.exit(1);
            },
            else => {
                std.log.err("zigttp edge: {}", .{err});
                std.process.exit(1);
            },
        }
    };

    if (!feature_options.enable_edge) shared.featureCompiledOut("edge", "edge");

    var config = edge_server.loadConfig(allocator, config_path) catch |err| {
        std.log.err("zigttp edge: failed to load config '{s}': {}", .{ config_path, err });
        std.process.exit(1);
    };
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
            path = try shared.takeArg(&i, argv, error.MissingEdgeConfig);
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

    try attestPayload(allocator, &payload);
}

fn attestPayload(allocator: std.mem.Allocator, payload: *const self_extract.Payload) !void {
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

const AppendedInvocation = union(enum) {
    serve,
    attest,
    version,
    help,
    unknown_positional: []const u8,
};

fn classifyAppendedInvocation(argv: []const []const u8) AppendedInvocation {
    if (argv.len == 0) return .serve;
    const command = argv[0];
    if (std.mem.eql(u8, command, "attest")) return appendedCommandNoArgs(argv, .attest);
    if (std.mem.eql(u8, command, "version") or std.mem.eql(u8, command, "--version")) return appendedCommandNoArgs(argv, .version);
    if (std.mem.eql(u8, command, "help") or std.mem.eql(u8, command, "--help")) return appendedCommandNoArgs(argv, .help);
    if (!std.mem.startsWith(u8, command, "-")) return .{ .unknown_positional = command };
    return .serve;
}

fn appendedCommandNoArgs(argv: []const []const u8, tag: std.meta.Tag(AppendedInvocation)) AppendedInvocation {
    if (argv.len > 1) return .{ .unknown_positional = argv[1] };
    return switch (tag) {
        .attest => .attest,
        .version => .version,
        .help => .help,
        .serve,
        .unknown_positional,
        => unreachable,
    };
}

pub fn serveCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    return serveCommandWithDebugPanicPath(allocator, argv, null);
}

pub fn serveCommandWithEnviron(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    environ: std.process.Environ,
) !void {
    return serveCommandWithDebugPanicPath(allocator, argv, environ.getPosix("ZIGTTP_DEBUG_PANIC_PATH"));
}

fn applyDebugPanicPathDefault(config: *ServerConfig, debug_panic_path: ?[]const u8) void {
    if (debug_panic_path) |path| {
        if (config.runtime_config.debug_panic_path == null) {
            config.runtime_config.debug_panic_path = path;
        }
    }
}

fn serveCommandWithDebugPanicPath(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    debug_panic_path: ?[]const u8,
) !void {
    const feature_flags = parseServeFeatureFlags(argv);
    if (feature_flags.studio_enabled) {
        if (!feature_options.enable_studio) {
            // `zigttp dev/serve --studio` is dispatched through the developer
            // CLI, so pointing at "the developer CLI" misdirects. The actionable
            // fix is to rebuild with -Dstudio, matching the dedicated `studio`
            // command's wording.
            shared.featureCompiledOut("studio", "studio");
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
    applyDebugPanicPathDefault(&config, debug_panic_path);
    warnDangerousServeFlags(feature_flags.force_swap, config.skip_env_check);
    if (feature_flags.studio_enabled) config.studio = true;
    if (feature_flags.demo_enabled) {
        config.studio = true;
        config.studio_demo_root = ".";
    }

    if (config.runtime_config.replay_file_path != null) {
        replay_runner.run(allocator, config) catch |err| {
            if (err != error.ReplayVerificationFailed) {
                std.log.err("Replay error: {}", .{err});
            }
            std.process.exit(replayExitCode(err));
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
        std.process.exit(1);
    };
    defer server.deinit();

    // Start with proven live reload if --watch was requested
    if (feature_flags.watch_enabled) {
        const handler_path = switch (config.handler) {
            .file_path => |path| path,
            else => {
                std.log.err("--watch requires a file-based handler (not --eval or embedded)", .{});
                std.process.exit(1);
            },
        };

        var watch_set = shared.buildWatchSet(allocator, argv) catch |err| {
            std.log.err("Failed to build watch set: {}", .{err});
            std.process.exit(1);
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
            reportServerError(err, config.port);
            std.process.exit(1);
        };
    } else {
        server.run() catch |err| {
            reportServerError(err, config.port);
            std.process.exit(1);
        };
    }
}

fn replayExitCode(err: anyerror) u8 {
    return if (err == error.ReplayVerificationFailed) 1 else 2;
}

/// Print a server-run failure. A port already in use is the most common
/// first-run failure, so it gets an actionable line naming the port and the
/// remedy instead of the bare `error.AddressInUse`, which prints after the
/// green proof card and reads like success followed by a cryptic crash.
fn reportServerError(err: anyerror, port: u16) void {
    if (err == error.AddressInUse) {
        if (!builtin.is_test) {
            var buf: [160]u8 = undefined;
            const line = formatAddressInUse(&buf, port) catch return;
            _ = std.c.write(std.c.STDERR_FILENO, line.ptr, line.len);
        }
        return;
    }
    std.log.err("Server error: {}", .{err});
}

/// Emit one warning line per active safety-disabling flag. These flags are
/// opt-in and legitimate, but a user (often in a script) should get a visible
/// signal rather than silently bypassing a guard. Gated out of tests to match
/// `reportServerError`; the spawning path is exercised by the CLI E2E smokes.
fn warnDangerousServeFlags(force_swap: bool, skip_env_check: bool) void {
    if (builtin.is_test) return;
    if (force_swap) std.log.warn(
        "--force-swap: breaking contract changes will be applied on reload without confirmation",
        .{},
    );
    if (skip_env_check) std.log.warn(
        "--no-env-check: startup environment validation is disabled",
        .{},
    );
}

/// Render the actionable port-in-use line into `buf`. Split out from
/// `reportServerError` so the message (port number + remedy) is unit-testable
/// without binding a socket.
fn formatAddressInUse(buf: []u8, port: u16) ![]const u8 {
    return std.fmt.bufPrint(
        buf,
        "Port {d} is already in use. Pass a different port: zigttp dev -p <PORT>\n",
        .{port},
    );
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
        const value = try shared.takeArg(i, argv, error.MissingPortValue);
        config.port = std.fmt.parseInt(u16, value, 10) catch return error.InvalidPort;
        return true;
    }
    if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--host")) {
        config.host = try shared.takeArg(i, argv, error.MissingHostValue);
        return true;
    }
    if (std.mem.eql(u8, arg, "-q") or std.mem.eql(u8, arg, "--quiet")) {
        config.log_requests = false;
        return true;
    }
    if (std.mem.eql(u8, arg, "--max-websocket-connections")) {
        const value = try shared.takeArg(i, argv, error.MissingMaxWebSocketConnections);
        config.max_websocket_connections = std.fmt.parseInt(usize, value, 10) catch return error.InvalidMaxWebSocketConnections;
        return true;
    }
    if (std.mem.eql(u8, arg, "--system")) {
        config.runtime_config.system_config_path = try shared.takeArg(i, argv, error.MissingSystemFile);
        return true;
    }
    if (std.mem.eql(u8, arg, "--durable")) {
        config.runtime_config.durable_oplog_dir = try shared.takeArg(i, argv, error.MissingDurableDir);
        return true;
    }
    if (std.mem.eql(u8, arg, "--workflow-queue")) {
        config.runtime_config.workflow_queue_enabled = true;
        return true;
    }
    if (std.mem.eql(u8, arg, "--actor-queue")) {
        config.runtime_config.queue_actor_enabled = true;
        return true;
    }
    if (std.mem.eql(u8, arg, "--outbound-http")) {
        config.runtime_config.outbound_http_enabled = true;
        return true;
    }
    if (std.mem.eql(u8, arg, "--outbound-host")) {
        config.runtime_config.outbound_http_enabled = true;
        config.runtime_config.outbound_allow_host = try shared.takeArg(i, argv, error.MissingOutboundHost);
        return true;
    }
    if (std.mem.eql(u8, arg, "--no-env-check")) {
        config.skip_env_check = true;
        return true;
    }
    if (std.mem.eql(u8, arg, "--security-log")) {
        config.security_log_path = try shared.takeArg(i, argv, error.MissingSecurityLogPath);
        return true;
    }
    if (std.mem.eql(u8, arg, "--lifecycle")) {
        const value = try shared.takeArg(i, argv, error.MissingLifecycleValue);
        config.lifecycle_override = parseLifecycle(value) orelse return error.InvalidLifecycleValue;
        return true;
    }
    return false;
}

fn parseServeArgs(allocator: std.mem.Allocator, argv: []const []const u8) !ServerConfig {
    var io_backend = shared.threadedIo(allocator);
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
            const raw_eval = try shared.takeArg(&i, argv, error.MissingEvalCode);
            config.handler = .{ .inline_code = try shared.stripInlineSource(allocator, raw_eval) };
            handler_set = true;
        } else if (std.mem.eql(u8, arg, "-m") or std.mem.eql(u8, arg, "--memory")) {
            const value = try shared.takeArg(&i, argv, error.MissingMemoryValue);
            config.runtime_config.memory_limit = shared.parseSize(value) catch return error.InvalidMemorySize;
        } else if (std.mem.eql(u8, arg, "--max-body-size")) {
            const value = try shared.takeArg(&i, argv, error.MissingMaxBodySize);
            config.max_body_size = shared.parseSize(value) catch return error.InvalidMaxBodySize;
        } else if (std.mem.eql(u8, arg, "-n") or std.mem.eql(u8, arg, "--pool")) {
            const value = try shared.takeArg(&i, argv, error.MissingPoolValue);
            config.pool_size = std.fmt.parseInt(usize, value, 10) catch return error.InvalidPoolSize;
        } else if (std.mem.eql(u8, arg, "--static")) {
            config.static_dir = try shared.takeArg(&i, argv, error.MissingStaticDir);
        } else if (std.mem.eql(u8, arg, "--outbound-http")) {
            config.runtime_config.outbound_http_enabled = true;
        } else if (std.mem.eql(u8, arg, "--outbound-host")) {
            config.runtime_config.outbound_http_enabled = true;
            config.runtime_config.outbound_allow_host = try shared.takeArg(&i, argv, error.MissingOutboundHost);
        } else if (std.mem.eql(u8, arg, "--outbound-timeout-ms")) {
            const value = try shared.takeArg(&i, argv, error.MissingOutboundTimeout);
            config.runtime_config.outbound_http_enabled = true;
            config.runtime_config.outbound_timeout_ms = std.fmt.parseInt(u32, value, 10) catch return error.InvalidOutboundTimeout;
        } else if (std.mem.eql(u8, arg, "--outbound-max-response")) {
            const value = try shared.takeArg(&i, argv, error.MissingOutboundMaxResponse);
            config.runtime_config.outbound_http_enabled = true;
            config.runtime_config.outbound_max_response_bytes = shared.parseSize(value) catch return error.InvalidOutboundMaxResponse;
        } else if (std.mem.eql(u8, arg, "--sqlite")) {
            config.runtime_config.sqlite_path = try shared.takeArg(&i, argv, error.MissingSqlitePath);
        } else if (std.mem.eql(u8, arg, "--trace")) {
            config.runtime_config.trace_file_path = try shared.takeArg(&i, argv, error.MissingTraceFile);
        } else if (std.mem.eql(u8, arg, "--incident-log")) {
            config.runtime_config.incident_log_path = try shared.takeArg(&i, argv, error.MissingIncidentLog);
        } else if (std.mem.eql(u8, arg, "--_debug-panic-path")) {
            config.runtime_config.debug_panic_path = try shared.takeArg(&i, argv, error.MissingDebugPanicPath);
        } else if (std.mem.eql(u8, arg, "--replay")) {
            config.runtime_config.replay_file_path = try shared.takeArg(&i, argv, error.MissingReplayFile);
        } else if (std.mem.eql(u8, arg, "--test")) {
            config.runtime_config.test_file_path = try shared.takeArg(&i, argv, error.MissingTestFile);
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
    var io_backend = shared.threadedIo(live_reload.allocator);
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

/// Build the base ServerConfig for a self-contained (deployed) binary from its
/// appended payload. The embedded section-4 capability policy is wired as the
/// enforcement source (`dev_capability_policy`) so the deployed binary restricts
/// egress/env/cache/sql to the proven allowlist, matching dev/serve and the
/// AOT-embedded path. Without this wiring a deployed handler fell back to the
/// allow-all stub policy and ran effectively unsandboxed.
fn appendedServerConfig(payload: *const self_extract.Payload) ServerConfig {
    // Matches the curl hint printed by `zigttp deploy`; -p PORT still overrides.
    return .{
        .handler = .{ .appended_payload = .{
            .bytecode = payload.bytecode,
            .dep_bytecodes = payload.dep_bytecodes,
        } },
        .contract_json = payload.contract_json,
        .attestation_jws = payload.attestation_jws,
        .policy_section_sha256 = payload.policy_section_sha256,
        .port = 3000,
        // The deployed binary runs the interpreter against the appended bytecode,
        // so (like dev/serve) it needs the contract-derived allowlist supplied as
        // `dev_capability_policy`; otherwise applyEmbeddedCapabilityPolicy keeps
        // the allow-all stub and egress/env/cache/sql go unenforced.
        .runtime_config = .{ .dev_capability_policy = payload.policy },
    };
}

fn serveAppended(allocator: std.mem.Allocator, payload: *const self_extract.Payload, argv: []const []const u8) !void {
    var config = appendedServerConfig(payload);

    try parseAppendedServeArgs(argv, &config, true);

    var server = Server.init(allocator, config) catch |err| {
        std.log.err("Failed to initialize server: {}", .{err});
        std.process.exit(1);
    };
    defer server.deinit();

    server.run() catch |err| {
        // Same chokepoint as serveCommand: a deployed binary is the path most
        // likely to hit AddressInUse on a fresh host, so surface the actionable
        // port message and exit non-zero rather than logging a bare error and
        // returning 0 (which supervisors read as a clean start).
        reportServerError(err, config.port);
        std.process.exit(1);
    };
}

fn parseAppendedServeArgs(argv: []const []const u8, config: *ServerConfig, emit_diagnostics: bool) !void {
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (try parseCommonServeFlag(arg, &i, argv, config)) continue;
        if (std.mem.eql(u8, arg, "--help")) {
            if (emit_diagnostics) printAppendedHelp();
            return error.HelpRequested;
        }
        if (!std.mem.startsWith(u8, arg, "-")) {
            if (emit_diagnostics) {
                std.debug.print("Unknown argument for self-contained binary: {s}\n\n", .{arg});
                printAppendedHelp();
            }
            return error.UnknownArgument;
        }
        if (emit_diagnostics) {
            std.debug.print("Unknown option for self-contained binary: {s}\n\n", .{arg});
            printAppendedHelp();
        }
        return error.UnknownOption;
    }
}

fn printAppendedHelp() void {
    const help =
        \\Usage: <binary> [options]
        \\       <binary> attest
        \\       <binary> version
        \\       <binary> help
        \\
        \\Options:
        \\  -p, --port <PORT>     Port to listen on
        \\  -h, --host <HOST>     Host to bind to
        \\  -q, --quiet           Disable request logging
        \\  --max-websocket-connections <N> Live WebSocket limit (default 1024; 0 disables)
        \\  --no-env-check        Skip required environment checks
        \\  --security-log <FILE> Write security decisions to FILE
        \\  --lifecycle <MODE>    ephemeral, bounded, ttl, or reuse
        \\  --system <FILE>       Handler bundle for service and workflow
        \\  --durable <DIR>       Enable durable execution with write-ahead oplog
        \\  --workflow-queue      Queue durable workflow dispatch; requires --system and --durable
        \\  --actor-queue         Enable in-memory zigttp:queue actor mailboxes
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printHelp() void {
    const help =
        \\zigttp - serverless runtime
        \\
        \\Usage:
        \\  zigttp serve [options] [handler.ts]    Run handler
        \\  zigttp edge [--config FILE]            Run in-process edge runtime
        \\  zigttp workflow-queue <cmd> --durable <DIR>  Inspect workflow queue dead letters
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
        \\Requires a binary built with: zig build -Dedge
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
        \\  --max-body-size <SIZE> Request body limit (default 1m); oversize returns 413
        \\  --max-websocket-connections <N> Live WebSocket limit (default 1024; 0 disables)
        \\  -n, --pool <N>        Runtime pool size
        \\  -q, --quiet           Disable request logging
        \\  --static <DIR>        Serve static files from directory
        \\  --outbound-http       Enable native outbound HTTP bridge
        \\  --outbound-host <H>   Restrict outbound bridge to exact host H (host only, not port)
        \\  --outbound-timeout-ms Connect timeout for outbound bridge in ms
        \\  --outbound-max-response <SIZE>
        \\  --sqlite <FILE>       SQLite database path for zigttp:sql
        \\  --trace <FILE>        Record handler I/O traces to JSONL file
        \\  --incident-log <FILE> Append runtime soundness incidents as JSONL
        \\  --replay <FILE>       Replay recorded traces and verify handler output
        \\  --test <FILE>         Run declarative handler tests from JSONL file
        \\  --durable <DIR>       Enable durable execution with write-ahead oplog
        \\  --system <FILE>       Handler bundle for zigttp:service/workflow
        \\  --workflow-queue      Queue durable workflow dispatch; requires --system and --durable
        \\  --actor-queue         Enable in-memory zigttp:queue actor mailboxes
        \\  --no-env-check        Skip startup env var validation
        \\  --security-log <FILE> Append security events to a JSONL file
        \\  --lifecycle <MODE>    Runtime lifecycle mode
        \\  --watch               Auto-reload on source change
        \\  --prove               With --watch, gate swaps on upgrade verdict
        \\  --force-swap          With --watch, apply breaking swaps anyway
        \\  --studio              Serve the local proof workbench
        \\  --quest               Run the first-run proof quest
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

test "appendedServerConfig wires the embedded capability policy for enforcement" {
    // Regression for the deploy fail-open: a self-contained binary must enforce
    // the proven egress/env/cache/sql allowlist embedded in its payload (section
    // 4), not fall back to the allow-all stub. The egress allowlist proves a
    // single host; the deployed config must restrict to exactly that host.
    const payload = self_extract.Payload{
        .bytecode = &.{},
        .dep_bytecodes = &.{},
        .contract_json = null,
        .policy = .{ .egress = .{ .enabled = true, .values = &[_][]const u8{"api.allowed.example"} } },
        .policy_strings = &.{},
        .policy_section_sha256 = [_]u8{0x5a} ** 32,
        .attestation_jws = null,
    };
    const config = appendedServerConfig(&payload);
    const policy = config.runtime_config.dev_capability_policy orelse return error.EmbeddedPolicyNotWired;
    try std.testing.expect(policy.allowsEgressHost("api.allowed.example"));
    try std.testing.expect(!policy.allowsEgressHost("evil.example"));
    try std.testing.expectEqualSlices(u8, &payload.policy_section_sha256, &config.policy_section_sha256.?);
}

test "parseCommonServeFlag: -p consumes next arg as port" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{ "-p", "4242" };
    var i: usize = 0;
    try std.testing.expect(try parseCommonServeFlag(argv[0], &i, &argv, &config));
    try std.testing.expectEqual(@as(u16, 4242), config.port);
    try std.testing.expectEqual(@as(usize, 1), i);
}

test "parseCommonServeFlag: WebSocket capacity has explicit zero semantics" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{ "--max-websocket-connections", "0" };
    var i: usize = 0;
    try std.testing.expect(try parseCommonServeFlag(argv[0], &i, &argv, &config));
    try std.testing.expectEqual(@as(usize, 0), config.max_websocket_connections);
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

test "debug panic env default does not override explicit serve flag" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };

    applyDebugPanicPathDefault(&config, "/env");
    try std.testing.expectEqualStrings("/env", config.runtime_config.debug_panic_path.?);

    config.runtime_config.debug_panic_path = "/flag";
    applyDebugPanicPathDefault(&config, "/env");
    try std.testing.expectEqualStrings("/flag", config.runtime_config.debug_panic_path.?);

    applyDebugPanicPathDefault(&config, null);
    try std.testing.expectEqualStrings("/flag", config.runtime_config.debug_panic_path.?);
}

test "parseServeArgs parses internal debug panic path flag" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    const config = try parseServeArgs(arena.allocator(), &.{ "handler.ts", "--_debug-panic-path", "/flag" });
    try std.testing.expectEqualStrings("/flag", config.runtime_config.debug_panic_path.?);
}

test "parseCommonServeFlag: returns false for unrelated flag" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{"--eval"};
    var i: usize = 0;
    try std.testing.expect(!try parseCommonServeFlag(argv[0], &i, &argv, &config));
    try std.testing.expectEqual(@as(usize, 0), i);
}

test "classifyAppendedInvocation dispatches metadata commands before serve" {
    try std.testing.expectEqual(@as(std.meta.Tag(AppendedInvocation), .attest), std.meta.activeTag(classifyAppendedInvocation(&.{"attest"})));
    try std.testing.expectEqual(@as(std.meta.Tag(AppendedInvocation), .version), std.meta.activeTag(classifyAppendedInvocation(&.{"version"})));
    try std.testing.expectEqual(@as(std.meta.Tag(AppendedInvocation), .version), std.meta.activeTag(classifyAppendedInvocation(&.{"--version"})));
    try std.testing.expectEqual(@as(std.meta.Tag(AppendedInvocation), .help), std.meta.activeTag(classifyAppendedInvocation(&.{"help"})));
    try std.testing.expectEqual(@as(std.meta.Tag(AppendedInvocation), .help), std.meta.activeTag(classifyAppendedInvocation(&.{"--help"})));
    try std.testing.expectEqual(@as(std.meta.Tag(AppendedInvocation), .serve), std.meta.activeTag(classifyAppendedInvocation(&.{})));
    try std.testing.expectEqual(@as(std.meta.Tag(AppendedInvocation), .serve), std.meta.activeTag(classifyAppendedInvocation(&.{ "-p", "3001" })));

    switch (classifyAppendedInvocation(&.{"serve"})) {
        .unknown_positional => |arg| try std.testing.expectEqualStrings("serve", arg),
        else => return error.ExpectedUnknownPositional,
    }
    switch (classifyAppendedInvocation(&.{ "attest", "extra" })) {
        .unknown_positional => |arg| try std.testing.expectEqualStrings("extra", arg),
        else => return error.ExpectedUnknownPositional,
    }
}

test "serveAppended argv rejects unknown positional args" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    try std.testing.expectError(error.UnknownArgument, parseAppendedServeArgs(&.{ "-p", "3001", "extra" }, &config, false));
    try std.testing.expectEqual(@as(u16, 3001), config.port);
}

test "serveAppended argv rejects unknown options" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    try std.testing.expectError(error.UnknownOption, parseAppendedServeArgs(&.{"--bogus"}, &config, false));
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

test "parseCommonServeFlag: --actor-queue enables queue runtime" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{"--actor-queue"};
    var i: usize = 0;
    try std.testing.expect(try parseCommonServeFlag(argv[0], &i, &argv, &config));
    try std.testing.expect(config.runtime_config.queue_actor_enabled);
}

test "parseCommonServeFlag: --lifecycle rejects unknown value" {
    var config = ServerConfig{ .handler = .{ .inline_code = "" }, .runtime_config = .{} };
    const argv = [_][]const u8{ "--lifecycle", "nonsense" };
    var i: usize = 0;
    try std.testing.expectError(error.InvalidLifecycleValue, parseCommonServeFlag(argv[0], &i, &argv, &config));
}

test "formatAddressInUse names the port and the remedy" {
    var buf: [160]u8 = undefined;
    const line = try formatAddressInUse(&buf, 3000);
    // Must name the actual port and tell the user how to recover, instead of
    // the bare error.AddressInUse that read like a crash after the proof card.
    try std.testing.expect(std.mem.indexOf(u8, line, "3000") != null);
    try std.testing.expect(std.mem.indexOf(u8, line, "already in use") != null);
    try std.testing.expect(std.mem.indexOf(u8, line, "zigttp dev -p") != null);
}

test "replay exit codes distinguish verification failures from invalid fixtures" {
    try std.testing.expectEqual(@as(u8, 1), replayExitCode(error.ReplayVerificationFailed));
    try std.testing.expectEqual(@as(u8, 2), replayExitCode(error.InvalidReplayFixture));
    try std.testing.expectEqual(@as(u8, 2), replayExitCode(error.InvalidTraceJson));
    try std.testing.expectEqual(@as(u8, 2), replayExitCode(error.InvalidTraceEntry));
    try std.testing.expectEqual(@as(u8, 2), replayExitCode(error.FileNotFound));
}
