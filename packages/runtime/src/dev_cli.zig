const std = @import("std");
const builtin = @import("builtin");

const project_config_mod = @import("project_config");
const zigts = @import("zigts");
const self_extract = @import("self_extract.zig");
const zigts_cli = @import("zigts_cli");
const deploy = @import("deploy.zig");
const proofs_cli = @import("proofs_cli.zig");
const witnesses_cli = @import("witnesses_cli.zig");
const precompile = zigts_cli.precompile;
const shared = @import("cli_shared.zig");
const runtime_cli = @import("runtime_cli.zig");
const embedded_handler = @import("embedded_handler");
const proof_ledger = @import("proof_ledger.zig");
const live_reload = @import("live_reload.zig");

const deploy_exit_drift: u8 = 2;
const deploy_exit_ready_timeout: u8 = 3;
const deploy_exit_did_not_start: u8 = 4;

pub fn main(init: std.process.Init.Minimal) !void {
    var debug_alloc: if (builtin.mode == .Debug) std.heap.DebugAllocator(.{}) else void =
        if (builtin.mode == .Debug) .init else {};
    defer if (builtin.mode == .Debug) {
        _ = debug_alloc.deinit();
    };
    const allocator = if (builtin.mode == .Debug) debug_alloc.allocator() else std.heap.smp_allocator;

    const args = try shared.collectArgs(allocator, init.args);
    defer {
        for (args) |arg| allocator.free(arg);
        allocator.free(args);
    }

    const user_args = args[1..];
    const command = if (user_args.len == 0) "" else user_args[0];

    if (user_args.len == 0) {
        if (embedded_handler.bytecode.len > 0) {
            try runtime_cli.serveCommand(allocator, &.{});
            return;
        }
        printHelp();
        return;
    }

    if (std.mem.eql(u8, command, "init")) {
        try initCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "dev")) {
        devCommand(allocator, args[0], user_args[1..]) catch |err| {
            if (handlePreflightError(err, command)) std.process.exit(1);
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "studio")) {
        studioCommand(allocator, args[0], user_args[1..]) catch |err| {
            if (handlePreflightError(err, command)) std.process.exit(1);
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "serve")) {
        // Convenience: dev CLI can also serve a handler locally for quick testing.
        try runtime_cli.serveCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "edge")) {
        try runtime_cli.edgeCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "doctor")) {
        doctorCommand(allocator, user_args[1..]) catch |err| {
            if (err == error.NoProjectConfig) {
                printNoProjectConfigDiagnostic(command);
                std.process.exit(1);
            }
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "check") or
        std.mem.eql(u8, command, "prove") or
        std.mem.eql(u8, command, "mock") or
        std.mem.eql(u8, command, "link") or
        std.mem.eql(u8, command, "gen-tests"))
    {
        zigts_cli.run(allocator, user_args) catch |err| {
            if (err == error.NoProjectConfig) {
                printNoProjectConfigDiagnostic(command);
                std.process.exit(1);
            }
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "compile")) {
        compileCommand(allocator, user_args[1..]) catch |err| {
            if (err == error.NoProjectConfig) {
                printNoProjectConfigDiagnostic(command);
                std.process.exit(1);
            }
            if (err == error.MissingArgument) {
                std.process.exit(1);
            }
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "build")) {
        buildCommand(allocator, user_args[1..]) catch |err| {
            if (err == error.NoProjectConfig) {
                printNoProjectConfigDiagnostic(command);
                std.process.exit(1);
            }
            if (err == error.MissingArgument or err == error.UnknownOption) {
                std.process.exit(1);
            }
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "expert")) {
        if (hasHelpFlag(user_args[1..])) {
            printExpertAliasHelp();
            return;
        }
        if (user_args.len > 1) {
            std.debug.print("zigttp expert does not accept arguments. Use direct commands like `zigts meta` or `zigts verify-paths`.\n\n", .{});
            printExpertAliasHelp();
            return;
        }
        std.debug.print("zigttp expert is deprecated; use `zigts expert`.\n", .{});
        const pi_app = @import("pi_app");
        const witness_replay_lib = @import("witness_replay_lib.zig");
        pi_app.witness_replay.setReplayFn(witness_replay_lib.replayWitnessJsonl);
        try pi_app.run(allocator);
        return;
    }
    if (std.mem.eql(u8, command, "login")) {
        deploy.login(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                printLoginHelp();
                return;
            }
            if (err == error.UnknownOption) {
                std.debug.print("zigttp login only accepts --token-stdin and --device.\n\n", .{});
                printLoginHelp();
                return;
            }
            if (err == error.InvalidOptionCombination) {
                std.debug.print("Choose either --token-stdin or --device, not both.\n\n", .{});
                printLoginHelp();
                return;
            }
            if (err == error.TokenPromptUnavailable) {
                std.debug.print("Interactive token entry requires a TTY. Use `zigttp login --token-stdin` or `zigttp login --device`.\n", .{});
                return;
            }
            if (err == error.EmptyToken) {
                std.debug.print("No token was provided.\n", .{});
                return;
            }
            if (err == error.InvalidToken) {
                std.debug.print("Token rejected by the control plane.\n", .{});
                return;
            }
            std.log.err("Login failed: {}", .{err});
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "deploy")) {
        if (deployArgsRequestLocal(user_args[1..])) {
            localDeployCommand(allocator, user_args[1..]) catch |err| {
                if (err == error.NoProjectConfig) {
                    printNoProjectConfigDiagnostic(command);
                    std.process.exit(1);
                }
                if (err == error.UnknownOption) {
                    std.process.exit(1);
                }
                return err;
            };
            return;
        }
        deploy.run(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                printDeployHelp();
                return;
            }
            if (err == error.UnknownOption) {
                std.debug.print("zigttp deploy only accepts --confirm, --region <name>, --wait, and --no-wait.\n\n", .{});
                printDeployHelp();
                return;
            }
            if (err == error.MissingOptionValue) {
                std.debug.print("--region requires a value, for example: zigttp deploy --region us-east\n\n", .{});
                printDeployHelp();
                return;
            }
            if (err == error.HandlerNotFound) {
                std.debug.print("Could not find a handler file. Create one of handler.ts, handler.tsx, handler.jsx, or handler.js (optionally under src/) and try again.\n", .{});
                return;
            }
            if (err == error.NameUndetectable) {
                std.debug.print("Could not determine a service name from package.json, git remote, or directory name.\n", .{});
                return;
            }
            if (err == error.ContractUnavailable) {
                std.debug.print("Could not extract a handler contract for this deploy.\n", .{});
                return;
            }
            if (err == error.ControlPlaneReviewRequired) {
                return;
            }
            if (err == error.DeployDrift) std.process.exit(deploy_exit_drift);
            if (err == error.ServiceReadyTimeout) std.process.exit(deploy_exit_ready_timeout);
            if (err == error.ServiceDidNotStart) std.process.exit(deploy_exit_did_not_start);
            std.log.err("Deploy failed: {}", .{err});
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "review")) {
        deploy.review(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                printReviewHelp();
                return;
            }
            if (err == error.UnknownOption) {
                std.debug.print("zigttp review accepts <plan-id> and optional --approve|--reject, with --grant only for approvals.\n\n", .{});
                printReviewHelp();
                return;
            }
            if (err == error.MissingPlanId) {
                std.debug.print("zigttp review requires a plan id.\n\n", .{});
                printReviewHelp();
                return;
            }
            if (err == error.InvalidOptionCombination) {
                std.debug.print("Use exactly one of --approve or --reject. --grant only works with --approve.\n\n", .{});
                printReviewHelp();
                return;
            }
            if (err == error.PlanNotFound) {
                std.debug.print("Deploy plan not found.\n", .{});
                return;
            }
            if (err == error.PlanConflict) {
                std.debug.print("Deploy plan cannot be changed in its current state.\n", .{});
                return;
            }
            if (err == error.PlanExpired) {
                std.debug.print("Deploy plan has expired.\n", .{});
                return;
            }
            std.log.err("Review failed: {}", .{err});
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "grants")) {
        deploy.grants(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                printGrantsHelp();
                return;
            }
            if (err == error.UnknownOption or err == error.InvalidOptionCombination) {
                std.debug.print("zigttp grants accepts at most one optional project name.\n\n", .{});
                printGrantsHelp();
                return;
            }
            std.log.err("Grant listing failed: {}", .{err});
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "revoke-grant")) {
        deploy.revokeGrant(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                printRevokeGrantHelp();
                return;
            }
            if (err == error.UnknownOption) {
                std.debug.print("zigttp revoke-grant only accepts a grant id.\n\n", .{});
                printRevokeGrantHelp();
                return;
            }
            if (err == error.MissingGrantId) {
                std.debug.print("zigttp revoke-grant requires a grant id.\n\n", .{});
                printRevokeGrantHelp();
                return;
            }
            if (err == error.InvalidOptionCombination) {
                std.debug.print("zigttp revoke-grant accepts exactly one grant id.\n\n", .{});
                printRevokeGrantHelp();
                return;
            }
            if (err == error.GrantNotFound) {
                std.debug.print("Capability grant not found.\n", .{});
                return;
            }
            std.log.err("Grant revoke failed: {}", .{err});
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "logout")) {
        try deploy.logout(allocator);
        return;
    }
    if (std.mem.eql(u8, command, "proofs")) {
        // Expected user-input errors are explained on stderr by proofs_cli
        // itself; only unexpected ones (allocator, etc.) bubble.
        proofs_cli.run(allocator, user_args[1..]) catch |err| {
            if (proofs_cli.isExpectedUserError(err)) return;
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "witnesses")) {
        witnesses_cli.run(allocator, user_args[1..]) catch |err| {
            if (witnesses_cli.isExpectedUserError(err)) return;
            return err;
        };
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

    std.debug.print("Unknown command: {s}\n\n", .{command});
    printHelp();
    std.process.exit(1);
}

fn hasHelpFlag(argv: []const []const u8) bool {
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "help")) return true;
    }
    return false;
}

fn deployArgsRequestLocal(argv: []const []const u8) bool {
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        if (std.mem.eql(u8, argv[i], "--local")) return true;
        if (std.mem.eql(u8, argv[i], "--target")) {
            if (i + 1 < argv.len and std.mem.eql(u8, argv[i + 1], "local")) return true;
        }
    }
    return false;
}

fn printNoProjectConfigDiagnostic(command: []const u8) void {
    std.debug.print(
        \\No zigttp.json found in the current directory or any parent.
        \\
        \\Run `zigttp init <name>` to scaffold a new project, then `cd <name>`
        \\and re-run `zigttp {s}`. Or pass an explicit handler path as an argument.
        \\
    , .{command});
}

/// Convert preflight errors from `dev`/`studio` (which run `zigts check`
/// before launching the runtime child) into clean exit-1 messages instead
/// of panic-style stack-trace dumps. The readable line was already printed
/// upstream by `zigts check` itself.
///
/// Returns `true` if the caller should `std.process.exit(1)`.
fn handlePreflightError(err: anyerror, command: []const u8) bool {
    if (err == error.NoProjectConfig) {
        printNoProjectConfigDiagnostic(command);
        return true;
    }
    if (err == error.FileNotFound) {
        // zigts check has already printed `Error reading handler file 'X': error.FileNotFound`.
        // Add a remediation hint and swallow the stack trace.
        std.debug.print(
            \\
            \\Check the `entry` path in zigttp.json or pass --help for usage.
            \\
        , .{});
        return true;
    }
    return false;
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
    std.debug.print("  zigttp studio            # browser proof workbench\n", .{});
    std.debug.print("  zigttp dev               # terminal proof HUD\n", .{});
    std.debug.print("  zigttp proofs badge      # write a verdict badge for your README\n", .{});
}

/// Path of the marker file that records "first-run tour was shown."
/// Once this file exists, `zigttp dev` skips the tour on subsequent runs.
fn tourMarkerPath() []const u8 {
    return ".zigttp/tour-shown";
}

/// First-run tour copy. One screen, no prompts, dismissed by writing the
/// marker file the moment we render. Designed to land just before the HUD
/// from `serve --watch --prove` starts streaming, so the author reads what
/// the four properties mean and then sees them flip live.
const tour_text =
    \\
    \\  zigttp dev   proof-aware live reload
    \\  ----------------------------------------------------------------------
    \\  every save is recompiled, then proven. the hud frame below shows your
    \\  handler's proof surface in real time. four properties to watch:
    \\
    \\    pure              no virtual-module calls
    \\    read_only         no state mutations
    \\    deterministic     no Date.now() / Math.random()
    \\    injection_safe    user input never reaches sensitive sinks
    \\
    \\  try it: drop a `Date.now()` into your handler and watch -deterministic
    \\  light up. revert it and watch +deterministic come back. the proof card
    \\  streams below.
    \\
    \\
;

fn stderrIsTty() bool {
    return switch (builtin.os.tag) {
        .windows => false,
        else => std.posix.system.isatty(std.posix.STDERR_FILENO) != 0,
    };
}

fn tourMarkerExistsAt(allocator: std.mem.Allocator, base_dir: []const u8) bool {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const path = std.fs.path.join(allocator, &.{ base_dir, tourMarkerPath() }) catch return false;
    defer allocator.free(path);
    std.Io.Dir.access(std.Io.Dir.cwd(), io, path, .{}) catch return false;
    return true;
}

fn touchTourMarkerAt(allocator: std.mem.Allocator, base_dir: []const u8) void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const state_dir = std.fs.path.join(allocator, &.{ base_dir, ".zigttp" }) catch return;
    defer allocator.free(state_dir);
    std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, state_dir) catch return;
    const path = std.fs.path.join(allocator, &.{ base_dir, tourMarkerPath() }) catch return;
    defer allocator.free(path);
    var file = std.Io.Dir.createFile(std.Io.Dir.cwd(), io, path, .{}) catch return;
    file.close(io);
}

fn tourMarkerExists(allocator: std.mem.Allocator) bool {
    return tourMarkerExistsAt(allocator, ".");
}

fn touchTourMarker(allocator: std.mem.Allocator) void {
    touchTourMarkerAt(allocator, ".");
}

/// Render the first-run tour exactly once, the first time `zigttp dev` is
/// invoked in a project. Dismissal is durable (marker file). Skipped when
/// stderr is not a TTY (CI, redirected logs) or `--no-tour` is passed.
fn maybeShowFirstRunTour(allocator: std.mem.Allocator, argv: []const []const u8) void {
    if (shared.hasFlag(argv, "--no-tour")) return;
    if (!stderrIsTty()) return;
    if (tourMarkerExists(allocator)) return;
    _ = std.c.write(std.c.STDERR_FILENO, tour_text.ptr, tour_text.len);
    touchTourMarker(allocator);
}

fn devCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
    maybeShowFirstRunTour(allocator, argv);
    try runDevPreflight(allocator, argv);

    const runtime_binary = try resolveRuntimeBinary(allocator, program_path);
    defer allocator.free(runtime_binary);

    // `zigttp dev` is the magnet surface: it implies `--watch --prove` so the
    // proof HUD lights up on the first save. `--no-prove` opts out of proof
    // verification but still watches; both flags can be passed explicitly to
    // be a no-op (idempotent).
    const user_no_prove = shared.hasFlag(argv, "--no-prove");
    const user_has_watch = shared.hasFlag(argv, "--watch");
    const user_has_prove = shared.hasFlag(argv, "--prove");

    var child_args = std.ArrayList([]const u8).empty;
    defer child_args.deinit(allocator);
    try child_args.append(allocator, runtime_binary);
    try child_args.append(allocator, "serve");
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--no-prove")) continue;
        if (std.mem.eql(u8, arg, "--no-tour")) continue;
        try child_args.append(allocator, arg);
    }
    if (!user_has_watch) try child_args.append(allocator, "--watch");
    if (!user_has_prove and !user_no_prove) try child_args.append(allocator, "--prove");

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var child = try std.process.spawn(io, .{
        .argv = child_args.items,
        .stdin = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
    });
    _ = child.wait(io) catch {};
}

fn studioCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
    try runDevPreflight(allocator, argv);

    const runtime_binary = try resolveRuntimeBinary(allocator, program_path);
    defer allocator.free(runtime_binary);

    const user_has_watch = shared.hasFlag(argv, "--watch");
    const user_has_prove = shared.hasFlag(argv, "--prove");

    var child_args = std.ArrayList([]const u8).empty;
    defer child_args.deinit(allocator);
    try child_args.append(allocator, runtime_binary);
    try child_args.append(allocator, "serve");
    for (argv) |arg| try child_args.append(allocator, arg);
    if (!shared.hasFlag(argv, "--studio")) try child_args.append(allocator, "--studio");
    if (!user_has_watch) try child_args.append(allocator, "--watch");
    if (!user_has_prove) try child_args.append(allocator, "--prove");

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var child = try std.process.spawn(io, .{
        .argv = child_args.items,
        .stdin = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
    });
    _ = child.wait(io) catch {};
}

/// Find the `zigttp` runtime binary.
/// Strategy: adjacent to this CLI binary (same directory), then PATH fallback.
fn resolveRuntimeBinary(allocator: std.mem.Allocator, program_path: []const u8) ![]const u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    // program_path may be absolute or cwd-relative depending on how the user
    // invoked us. Dir.access handles both, so we don't need to canonicalize.
    const dir_name = std.fs.path.dirname(program_path) orelse ".";
    const cwd = std.Io.Dir.cwd();
    const candidates = [_][]const u8{ "zigttp-runtime", "zigttp" };
    for (candidates) |name| {
        const candidate = try std.fs.path.join(allocator, &.{ dir_name, name });
        errdefer allocator.free(candidate);
        if (std.mem.eql(u8, candidate, program_path)) {
            allocator.free(candidate);
            continue;
        }
        cwd.access(io, candidate, .{}) catch {
            allocator.free(candidate);
            continue;
        };
        return candidate;
    }

    return try allocator.dupe(u8, "zigttp-runtime");
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

fn runDevPreflight(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var check_args = std.ArrayList([]const u8).empty;
    defer check_args.deinit(allocator);
    try check_args.append(allocator, "check");

    const path = shared.findPositionalPath(argv);
    if (path) |p| {
        try check_args.append(allocator, p);
    }

    try zigts_cli.run(allocator, check_args.items);
}

fn compileCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var handler_path: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            i += 1;
            if (i >= argv.len) {
                std.log.err("-o requires an output path", .{});
                return error.MissingArgument;
            }
            output_path = argv[i];
        } else if (std.mem.eql(u8, arg, "--help")) {
            printCompileHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            handler_path = arg;
        }
    }

    if (handler_path == null) {
        std.log.err("handler file path required", .{});
        printCompileHelp();
        return error.MissingArgument;
    }
    if (output_path == null) {
        std.log.err("-o <output> required", .{});
        printCompileHelp();
        return error.MissingArgument;
    }

    try buildArtifact(allocator, handler_path.?, output_path.?, null);
}

const ProjectArtifact = struct {
    project: project_config_mod.ProjectConfig,
    handler_path: []u8,
    output_path: []u8,
    project_name: []const u8,

    fn deinit(self: *ProjectArtifact, allocator: std.mem.Allocator) void {
        allocator.free(self.handler_path);
        allocator.free(self.output_path);
        self.project.deinit(allocator);
    }
};

/// Discover `zigttp.json`, resolve the handler entry, and compute the artifact
/// output path under `<root>/.zigttp/<subdir>/<project-name>`. Creates the
/// parent dir for the default path; for an explicit override, trusts the
/// caller (avoids macOS symlink quirks like `/tmp` → `/private/tmp`).
fn prepareProjectArtifact(
    allocator: std.mem.Allocator,
    io: std.Io,
    subdir: []const u8,
    output_override: ?[]const u8,
) !ProjectArtifact {
    var project_opt = try project_config_mod.discover(allocator, io, null);
    errdefer if (project_opt) |*p| p.deinit(allocator);
    var project = project_opt orelse return error.NoProjectConfig;
    errdefer project.deinit(allocator);

    const handler_path = try project.resolvedEntry(allocator);
    errdefer allocator.free(handler_path);

    const project_name = std.fs.path.basename(project.root_dir);

    const output_path = if (output_override) |p|
        try allocator.dupe(u8, p)
    else blk: {
        const path = try std.fs.path.resolve(allocator, &.{ project.root_dir, ".zigttp", subdir, project_name });
        errdefer allocator.free(path);
        if (std.fs.path.dirname(path)) |parent| {
            std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, parent) catch |err| switch (err) {
                error.PathAlreadyExists => {},
                else => return err,
            };
        }
        break :blk path;
    };

    return .{
        .project = project,
        .handler_path = handler_path,
        .output_path = output_path,
        .project_name = project_name,
    };
}

fn buildCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var output_override: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            i += 1;
            if (i >= argv.len) {
                std.log.err("-o requires an output path", .{});
                return error.MissingArgument;
            }
            output_override = argv[i];
        } else if (std.mem.eql(u8, arg, "--help")) {
            printBuildHelp();
            return;
        } else {
            std.log.err("Unknown argument: {s}", .{arg});
            printBuildHelp();
            return error.UnknownOption;
        }
    }

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();

    var artifact = try prepareProjectArtifact(allocator, io_backend.io(), "build", output_override);
    defer artifact.deinit(allocator);

    try buildArtifact(allocator, artifact.handler_path, artifact.output_path, null);

    std.debug.print(
        \\
        \\Built: {s}
        \\Run:   {s}
        \\
    , .{ artifact.output_path, artifact.output_path });
}

fn localDeployCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--region") or
            std.mem.eql(u8, arg, "--confirm") or
            std.mem.eql(u8, arg, "--wait") or
            std.mem.eql(u8, arg, "--no-wait"))
        {
            std.debug.print(
                \\zigttp deploy --local does not accept {s}.
                \\Cloud-only flags (--region, --confirm, --wait, --no-wait) apply
                \\only to the hosted control-plane deploy.
                \\
            , .{arg});
            return error.UnknownOption;
        }
        if (std.mem.eql(u8, arg, "--local")) continue;
        if (std.mem.eql(u8, arg, "--target")) continue;
        if (std.mem.eql(u8, arg, "local")) continue;
        if (std.mem.eql(u8, arg, "--help")) {
            printLocalDeployHelp();
            return;
        }
        std.debug.print("Unknown argument for `zigttp deploy --local`: {s}\n\n", .{arg});
        printLocalDeployHelp();
        return error.UnknownOption;
    }

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();

    var artifact = try prepareProjectArtifact(allocator, io_backend.io(), "deploy", null);
    defer artifact.deinit(allocator);

    // proof_ledger.appendEvent writes the relative path `.zigttp/proofs.jsonl`,
    // so anchor CWD at the project root before buildArtifact emits the ledger
    // row. The summary block below advertises the ledger; failure here must
    // surface, not silently warn.
    try std.Io.Threaded.chdir(artifact.project.root_dir);

    try buildArtifact(allocator, artifact.handler_path, artifact.output_path, artifact.project_name);

    std.debug.print(
        \\
        \\Local deploy ready.
        \\
        \\  Artifact: {s}
        \\  Run:      {s} -p {d}
        \\  Verify:   curl http://{s}:{d}/
        \\  Ledger:   .zigttp/proofs.jsonl (kind=deploy)
        \\
    , .{ artifact.output_path, artifact.output_path, artifact.project.port, artifact.project.host, artifact.project.port });
}

fn buildArtifact(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    output_path: []const u8,
    ledger_service_name: ?[]const u8,
) !void {
    const source = zigts.file_io.readFile(allocator, handler_path, 10 * 1024 * 1024) catch |err| {
        std.log.err("Failed to read handler '{s}': {}", .{ handler_path, err });
        return err;
    };
    defer allocator.free(source);

    std.log.info("Compiling {s}...", .{handler_path});

    var compiled = precompile.compileHandler(
        allocator,
        source,
        handler_path,
        false,
        true,
        true,
        null,
        null,
        false,
        null,
    ) catch |err| {
        std.log.err("Compilation failed: {}", .{err});
        return err;
    };
    defer compiled.deinit(allocator);

    if (compiled.verify_failed) {
        std.log.err("Verification failed - binary not created", .{});
        if (compiled.violations_summary) |summary| {
            _ = std.c.write(std.c.STDERR_FILENO, summary.ptr, summary.len);
        }
        return error.VerificationFailed;
    }

    if (compiled.bytecode.len == 0) {
        std.log.err("No bytecode generated", .{});
        return error.NoBytecode;
    }

    // The compile subcommand splices bytecode onto the runtime binary, not
    // the dev CLI. Locate the runtime binary adjacent to this executable.
    const dev_self_path = self_extract.getSelfExePath(allocator) catch |err| {
        std.log.err("Failed to determine own executable path: {}", .{err});
        return err;
    };
    defer allocator.free(dev_self_path);

    const runtime_binary = try resolveRuntimeBinary(allocator, dev_self_path);
    defer allocator.free(runtime_binary);

    var contract_json: ?[]const u8 = null;
    if (compiled.contract) |*contract| {
        var json_output: std.ArrayList(u8) = .empty;
        defer json_output.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &json_output);
        zigts.writeContractJson(contract, &aw.writer) catch {};
        json_output = aw.toArrayList();
        if (json_output.items.len > 0) {
            contract_json = try json_output.toOwnedSlice(allocator);
        }
    }
    defer if (contract_json) |cj| allocator.free(cj);

    const dep_bytecodes: []const []const u8 = compiled.dep_bytecodes orelse &.{};

    const policy = zigts.handler_policy.RuntimePolicy{};
    self_extract.create(
        allocator,
        runtime_binary,
        output_path,
        compiled.bytecode,
        dep_bytecodes,
        contract_json,
        &policy,
    ) catch |err| {
        std.log.err("Failed to create output binary: {}", .{err});
        return err;
    };

    if (builtin.os.tag == .macos) {
        codesignAdHoc(allocator, output_path);
    }

    if (ledger_service_name) |service_name| {
        if (compiled.contract) |*contract| {
            var sha_digest: [std.crypto.hash.sha2.Sha256.digest_length]u8 = undefined;
            std.crypto.hash.sha2.Sha256.hash(source, &sha_digest, .{});
            const sha_hex = std.fmt.bytesToHex(sha_digest, .lower);
            try live_reload.appendLedgerEntry(allocator, .deploy, contract, handler_path, &sha_hex, service_name);
        }
    }

    std.log.info("Compiled: {s} -> {s} (bytecode {d} bytes)", .{
        handler_path, output_path, compiled.bytecode.len,
    });
}

fn codesignAdHoc(allocator: std.mem.Allocator, path: []const u8) void {
    const path_z = allocator.dupeZ(u8, path) catch return;
    defer allocator.free(path_z);

    const pid = std.c.fork();
    if (pid == 0) {
        const codesign: [*:0]const u8 = "/usr/bin/codesign";
        const argv = [_:null]?[*:0]const u8{
            codesign,
            "--force",
            "--sign",
            "-",
            path_z,
            null,
        };
        _ = std.c.execve(codesign, &argv, std.c.environ);
        std.c._exit(1);
    } else if (pid > 0) {
        var status: i32 = 0;
        _ = std.c.waitpid(pid, &status, 0);
        if (status != 0) {
            std.log.warn("Ad-hoc code signing returned non-zero status", .{});
        }
    }
}

fn printCompileHelp() void {
    const help =
        \\zigttp compile <handler.ts> -o <output>
        \\
        \\Compile a handler into a self-contained binary.
        \\Verification is mandatory: the handler must pass all checks.
        \\The output binary wraps the zigttp runtime template, which must be
        \\installed alongside zigttp as `zigttp-runtime`.
        \\
        \\Options:
        \\  -o, --output <PATH>   Output binary path (required)
        \\  --help                Show this help
        \\
        \\For a no-args version that auto-detects from zigttp.json, use
        \\`zigttp build` instead.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printBuildHelp() void {
    const help =
        \\zigttp build [-o <output>]
        \\
        \\Verify the handler in this project and emit a self-contained binary.
        \\Reads zigttp.json from the current directory or any parent.
        \\Default output path is `.zigttp/build/<project-name>`.
        \\
        \\Options:
        \\  -o, --output <PATH>   Override the output binary path
        \\  --help                Show this help
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printLocalDeployHelp() void {
    const help =
        \\zigttp deploy --local
        \\
        \\Build a self-contained binary for the handler in this project and
        \\record the deploy in the local proof ledger. No cloud credentials,
        \\Docker, or network access required.
        \\
        \\Reads zigttp.json from the current directory or any parent.
        \\Output: .zigttp/deploy/<project-name>
        \\Ledger: .zigttp/proofs.jsonl (appends a kind=deploy row)
        \\
        \\Options:
        \\  --local         Use the local target (this command)
        \\  --target local  Same as --local
        \\  --help          Show this help
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printDeployHelp() void {
    const help =
        \\zigttp deploy [--local]
        \\
        \\Deploy the handler in this directory.
        \\
        \\With --local: build a self-contained binary at .zigttp/deploy/<name>
        \\and record the deploy in .zigttp/proofs.jsonl. No credentials needed.
        \\
        \\Without --local: deploy to the zigttp runtime via the hosted control
        \\plane. If credentials are missing, Zigttp first prompts for an access
        \\token. Submit an empty token to fall back to browser-based device
        \\login.
        \\
        \\The command auto-detects everything by default. Optional flags:
        \\  --local          Build a local artifact, no network, no credentials
        \\  --confirm        Allow replace-like updates after showing the drift warning
        \\  --region <name>  Override the deployment region for this run
        \\  --wait           Block until the service reports ready (default)
        \\  --no-wait        Return immediately after the deploy is accepted
        \\
        \\Exit codes:
        \\  0  success
        \\  2  drift detected, re-run with --confirm
        \\  3  timed out waiting for the service to become ready
        \\  4  service failed to start
        \\
        \\Related:
        \\  zigttp login    Store deploy credentials explicitly
        \\  zigttp logout   Forget the saved sign-in credentials
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printLoginHelp() void {
    const help =
        \\zigttp login
        \\
        \\Store a Zigttp access token for future deploys.
        \\
        \\Default behavior prompts for a token in the terminal. Submit an empty
        \\line to fall back to browser-based device login.
        \\
        \\Options:
        \\  --token-stdin   Read the token from stdin
        \\  --device        Skip token entry and use browser-based device login
        \\
        \\Related:
        \\  zigttp deploy   Deploy using saved credentials or prompt if needed
        \\  zigttp logout   Forget the saved sign-in credentials
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printReviewHelp() void {
    const help =
        \\zigttp review <plan-id>
        \\zigttp review <plan-id> --approve [--grant]
        \\zigttp review <plan-id> --reject
        \\
        \\Inspect, approve, or reject a deploy capability review.
        \\
        \\Options:
        \\  <plan-id>       Show the current plan status and risky additions
        \\  --approve       Approve the plan for one use
        \\  --grant         When approving, also create a reusable capability grant
        \\  --reject        Reject the plan
        \\
        \\Related:
        \\  zigttp deploy   Trigger a deploy and surface plan-required reviews
        \\  zigttp login    Store deploy credentials explicitly
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printGrantsHelp() void {
    const help =
        \\zigttp grants [project-name]
        \\
        \\List reusable capability grants visible to the current account.
        \\
        \\Arguments:
        \\  project-name    Optional project filter
        \\
        \\Related:
        \\  zigttp revoke-grant <grant-id>   Revoke a reusable capability grant
        \\  zigttp review <plan-id>          Inspect or approve a pending review
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printRevokeGrantHelp() void {
    const help =
        \\zigttp revoke-grant <grant-id>
        \\
        \\Revoke a reusable capability grant so future risky deploys must be reviewed again.
        \\
        \\Related:
        \\  zigttp grants                    List reusable capability grants
        \\  zigttp review <plan-id>         Approve a new review or grant
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printHelp() void {
    const help =
        \\zigttp - developer tool for zigttp projects
        \\
        \\Usage:
        \\  zigttp init <name> [--template ...]    Create project
        \\  zigttp studio [handler-or-project]      Browser proof workbench
        \\  zigttp dev [handler-or-project]         Watch + proven hot reload (HUD on)
        \\  zigttp serve [options] [handler.ts]    Run handler locally
        \\  zigttp edge [--config FILE]            Run in-process edge runtime
        \\  zigttp expert                          Deprecated alias for `zigts expert`
        \\  zigttp check [handler.ts] [--contract]  Verify handler
        \\  zigttp build [-o <bin>]                 Verify and emit self-contained binary
        \\  zigttp compile <handler.ts> -o <bin>    Build self-contained binary (explicit args)
        \\  zigttp login                            Store deploy credentials
        \\  zigttp deploy                           Deploy the handler in this directory
        \\  zigttp review <plan-id>                 Inspect, approve, or reject a deploy plan
        \\  zigttp grants [project-name]            List reusable capability grants
        \\  zigttp revoke-grant <grant-id>          Revoke a reusable capability grant
        \\  zigttp logout                           Forget saved sign-in credentials
        \\  zigttp proofs [list|show|diff|watch|export]  Browse the proof ledger
        \\  zigttp prove <old.json> <new.json>      Upgrade safety check
        \\  zigttp mock <tests.jsonl> [--port N]    Mock server from tests
        \\  zigttp link <system.json>               Cross-handler linking
        \\  zigttp doctor [path]                    Validate project
        \\  zigttp version                          Show version
        \\
        \\The internal runtime template is installed as `zigttp-runtime`.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printExpertAliasHelp() void {
    const help =
        \\zigttp expert - deprecated interactive expert alias
        \\
        \\Usage:
        \\  zigttp expert
        \\
        \\This alias will be removed in a future release.
        \\Use `zigts expert` for the interactive coding-agent workflow.
        \\Use direct `zigts` commands such as `zigts meta` and
        \\`zigts verify-paths` for machine-facing tooling.
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
    try zigts.file_io.writeFile(allocator, full_path, data);
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
    \\import { logInfo } from "zigttp:log";
    \\
    \\function handler(req) {
    \\    logInfo("request received", { path: req.path });
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
    \\## Quick start
    \\
    \\1. Open the proof workbench in your browser:
    \\
    \\       zigttp studio
    \\
    \\2. Edit `src/handler.ts` in your editor. Studio re-verifies on save and
    \\   shows the verdict, proven surface, witnesses, and any spec diagnostics.
    \\
    \\3. When you are happy with the verdict, build a self-contained binary:
    \\
    \\       zigttp build
    \\       ./.zigttp/build/<this-app-name>
    \\
    \\4. Or do a verified local deploy that also records the proof in the
    \\   ledger at `.zigttp/proofs.jsonl`:
    \\
    \\       zigttp deploy --local
    \\       ./.zigttp/deploy/<this-app-name>
    \\
    \\## Other useful commands
    \\
    \\- `zigttp check` - run the analyzer once and exit
    \\- `zigttp doctor` - validate the project layout
    \\- `zigttp proofs list` - browse the proof ledger
;

const apiReadme = basicReadme;
const htmxReadme = basicReadme;

test "first-run tour marker is durable: absent then created then detected" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const base = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    try testing.expect(!tourMarkerExistsAt(allocator, base));
    touchTourMarkerAt(allocator, base);
    try testing.expect(tourMarkerExistsAt(allocator, base));
    // Idempotent: a second touch is harmless.
    touchTourMarkerAt(allocator, base);
    try testing.expect(tourMarkerExistsAt(allocator, base));
}

test "prepareProjectArtifact default path: <root>/.zigttp/<subdir>/<basename>" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try tmp.dir.writeFile(testing.io, .{
        .sub_path = "zigttp.json",
        .data = "{\"entry\":\"src/handler.ts\",\"port\":3000}",
    });
    try std.Io.Dir.createDirPath(tmp.dir, io, "src");
    try tmp.dir.writeFile(testing.io, .{
        .sub_path = "src/handler.ts",
        .data = "function handler(r) { return Response.text('ok') }\n",
    });

    var artifact = try prepareProjectArtifact(testing.allocator, io, "build", null);
    defer artifact.deinit(testing.allocator);

    // Output path ends in `.zigttp/build/<basename(root)>`.
    try testing.expect(std.mem.indexOf(u8, artifact.output_path, ".zigttp/build/") != null);
    try testing.expectEqualStrings(artifact.project_name, std.fs.path.basename(artifact.output_path));

    // Parent dir exists (createDirPath ran).
    const parent = std.fs.path.dirname(artifact.output_path).?;
    try std.Io.Dir.accessAbsolute(io, parent, .{});

    // Handler path resolves to the manifest entry.
    try testing.expectStringEndsWith(artifact.handler_path, "src/handler.ts");
}

test "prepareProjectArtifact override path: passes through unchanged, no parent created" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try tmp.dir.writeFile(testing.io, .{
        .sub_path = "zigttp.json",
        .data = "{\"entry\":\"src/handler.ts\"}",
    });
    try std.Io.Dir.createDirPath(tmp.dir, io, "src");
    try tmp.dir.writeFile(testing.io, .{
        .sub_path = "src/handler.ts",
        .data = "function handler(r) { return Response.text('ok') }\n",
    });

    var artifact = try prepareProjectArtifact(testing.allocator, io, "deploy", "/tmp/explicit-name");
    defer artifact.deinit(testing.allocator);

    try testing.expectEqualStrings("/tmp/explicit-name", artifact.output_path);
}

test "prepareProjectArtifact returns NoProjectConfig when no zigttp.json on or above CWD" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    // No zigttp.json present anywhere up the tree from this tmp dir.
    try testing.expectError(error.NoProjectConfig, prepareProjectArtifact(testing.allocator, io, "build", null));
}
