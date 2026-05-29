const std = @import("std");
const builtin = @import("builtin");

const project_config_mod = @import("project_config");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const proofs_cli = @import("proofs_cli.zig");
const proof_cli = @import("proof_cli.zig");
const witnesses_cli = @import("witnesses_cli.zig");
const precompile = zigts_cli.precompile;
const shared = @import("cli_shared.zig");
const runtime_cli = @import("runtime_cli.zig");
const embedded_handler = @import("embedded_handler");
const proof_ledger = @import("proof_ledger.zig");
const ratchet_command = @import("ratchet_command.zig");
// Hosted cloud deploy is deferred from v0.1.0-beta. The control-plane module
// stays imported so it keeps compiling, ready to re-enable in a later release;
// only the `deploy --cloud` rejection in `main` remains user-visible.
const deploy = @import("deploy.zig");
const live_reload = @import("live_reload.zig");
const pi_app = @import("pi_app");
const verify_cli = @import("verify_cli.zig");
const cli_args = @import("cli_args.zig");
const cli_tour = @import("cli_tour.zig");
const maybeShowFirstRunTour = cli_tour.maybeShowFirstRunTour;
const cli_release_check = @import("cli_release_check.zig");
const cli_paths = @import("cli_paths.zig");
const resolveDeveloperServeBinary = cli_paths.resolveDeveloperServeBinary;
const resolveReentryBinaryAfterChdir = cli_paths.resolveReentryBinaryAfterChdir;
const cli_doctor = @import("cli_doctor.zig");
const doctorCommand = cli_doctor.doctorCommand;
const printDoctorHelp = cli_doctor.printDoctorHelp;
const cli_auth = @import("cli_auth.zig");
const doctorPathExists = cli_doctor.doctorPathExists;
const runDoctorAnalyzerForProject = cli_doctor.runDoctorAnalyzerForProject;
const printCheckStageFailures = cli_doctor.printCheckStageFailures;
const hasHelpFlag = cli_args.hasHelpFlag;
const hasLongHelpFlag = cli_args.hasLongHelpFlag;
const deployArgsRequestCloud = cli_args.deployArgsRequestCloud;
const printNoProjectConfigDiagnostic = cli_args.printNoProjectConfigDiagnostic;
const handlePreflightError = cli_args.handlePreflightError;
const cloud_only_deploy_flags = cli_args.cloud_only_deploy_flags;
const template_choices = cli_args.template_choices;
const cli_templates = @import("cli_templates.zig");
const Template = cli_templates.Template;
const parseTemplate = cli_templates.parseTemplate;
const demo_command = @import("demo_command.zig");
const test_command = @import("test_command.zig");
const init_command = @import("init_command.zig");
const build_command = @import("build_command.zig");

test {
    // Command modules are reached only through `main`'s dispatch, which the
    // test build does not analyze. Reference them here so their own test
    // blocks (and the sibling files they transitively pull in, e.g. the
    // analyzer/serve paths) are collected by `zig build test-cli`.
    _ = @import("cli_templates.zig");
    _ = @import("demo_command.zig");
    _ = @import("test_command.zig");
    _ = @import("init_command.zig");
    _ = @import("build_command.zig");
}

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

    if (std.mem.eql(u8, command, "auth")) {
        cli_auth.authCommand(allocator, user_args[1..]) catch |err| switch (err) {
            error.InvalidArgument, error.NotATty, error.EmptyKey, error.InvalidKey => std.process.exit(1),
            else => return err,
        };
        return;
    }

    // Commands that hit a model backend or read handler env get the
    // stored provider keys injected before dispatch, so users do not
    // have to `export ANTHROPIC_API_KEY=...` in their shell. Shell
    // values always win; injection only fills gaps.
    if (std.mem.eql(u8, command, "dev") or
        std.mem.eql(u8, command, "serve") or
        std.mem.eql(u8, command, "expert"))
    {
        cli_auth.injectStoredProvidersIntoEnv(allocator);
    }

    if (std.mem.eql(u8, command, "init")) {
        init_command.initCommand(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                init_command.printInitHelp();
                return;
            }
            if (err == error.MissingProjectName) {
                std.debug.print("zigttp init requires a project name.\n\n", .{});
                init_command.printInitHelp();
                std.process.exit(1);
            }
            if (err == error.MissingTemplate) {
                std.debug.print("--template requires one of: " ++ template_choices ++ ".\n\n", .{});
                init_command.printInitHelp();
                std.process.exit(1);
            }
            if (err == error.InvalidTemplate) {
                std.debug.print("Unknown template. Choose one of: " ++ template_choices ++ ".\n\n", .{});
                init_command.printInitHelp();
                std.process.exit(1);
            }
            if (err == error.InvalidProjectName) {
                std.debug.print("Invalid project name. Use letters, numbers, '-' or '_', starting with a letter or number.\n\n", .{});
                init_command.printInitHelp();
                std.process.exit(1);
            }
            if (err == error.MissingExtensionName) {
                std.debug.print("zigttp init --extension requires a name.\n\n", .{});
                init_command.printInitHelp();
                std.process.exit(1);
            }
            if (err == error.InvalidArgument or err == error.UnknownOption) {
                std.debug.print("Invalid init arguments.\n\n", .{});
                init_command.printInitHelp();
                std.process.exit(1);
            }
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "dev")) {
        if (hasLongHelpFlag(user_args[1..])) {
            printDevHelp();
            return;
        }
        devCommand(allocator, args[0], user_args[1..]) catch |err| {
            if (handlePreflightError(err, command)) std.process.exit(1);
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "studio")) {
        if (hasLongHelpFlag(user_args[1..])) {
            printStudioHelp();
            return;
        }
        studioCommand(allocator, args[0], user_args[1..]) catch |err| {
            if (handlePreflightError(err, command)) std.process.exit(1);
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "demo")) {
        demo_command.demoCommand(allocator, args[0], user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                demo_command.printDemoHelp();
                return;
            }
            if (err == error.MissingOptionValue) {
                std.debug.print("demo option requires a value.\n\n", .{});
                demo_command.printDemoHelp();
                std.process.exit(1);
            }
            if (err == error.InvalidPort) {
                std.debug.print("--port requires a number from 1 to 65535.\n\n", .{});
                demo_command.printDemoHelp();
                std.process.exit(1);
            }
            if (err == error.OutputExists) {
                std.debug.print("--out target already exists. Pick a new directory; zigttp demo will not overwrite files.\n", .{});
                std.process.exit(1);
            }
            if (err == error.InvalidOutputPath) {
                std.debug.print("--out must end in a simple directory name using letters, numbers, '-' or '_'.\n", .{});
                std.process.exit(1);
            }
            if (err == error.UnknownOption) {
                std.debug.print("Unknown demo option.\n\n", .{});
                demo_command.printDemoHelp();
                std.process.exit(1);
            }
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
        if (hasHelpFlag(user_args[1..])) {
            printDoctorHelp();
            return;
        }
        doctorCommand(allocator, user_args[1..]) catch |err| {
            if (err == error.NoProjectConfig) {
                printNoProjectConfigDiagnostic(command);
                std.process.exit(1);
            }
            if (err == error.FileNotFound or
                err == error.UnsupportedMultipleOutboundHosts or
                err == error.CheckFailed or
                err == error.DoctorFailed or
                err == error.InvalidArgument)
            {
                std.process.exit(1);
            }
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "test")) {
        test_command.testCommand(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                test_command.printTestHelp();
                return;
            }
            if (err == error.NoProjectConfig) {
                printNoProjectConfigDiagnostic(command);
                std.process.exit(1);
            }
            if (err == error.FileNotFound) {
                std.process.exit(1);
            }
            if (err == error.UnknownOption or err == error.TooManyArguments) {
                if (err == error.UnknownOption) {
                    std.debug.print("zigttp test accepts a single optional tests.jsonl path; flags are not supported here.\n\n", .{});
                } else {
                    std.debug.print("zigttp test accepts at most one tests.jsonl path.\n\n", .{});
                }
                test_command.printTestHelp();
                std.process.exit(1);
            }
            if (err == error.CheckFailed or err == error.UnsupportedMultipleOutboundHosts) {
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
        std.mem.eql(u8, command, "gen-tests") or
        std.mem.eql(u8, command, "rollout") or
        std.mem.eql(u8, command, "features") or
        std.mem.eql(u8, command, "modules") or
        std.mem.eql(u8, command, "restrictions") or
        std.mem.eql(u8, command, "meta") or
        std.mem.eql(u8, command, "verify-paths") or
        std.mem.eql(u8, command, "verify-modules") or
        std.mem.eql(u8, command, "verify-module-manifest") or
        std.mem.eql(u8, command, "extension-status") or
        std.mem.eql(u8, command, "edit-simulate") or
        std.mem.eql(u8, command, "describe-rule") or
        std.mem.eql(u8, command, "search") or
        std.mem.eql(u8, command, "review-patch") or
        std.mem.eql(u8, command, "prove-behavior"))
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
        build_command.compileCommand(allocator, user_args[1..]) catch |err| {
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
        build_command.buildCommand(allocator, user_args[1..]) catch |err| {
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
    if (std.mem.eql(u8, command, "ratchet")) {
        ratchet_command.run(allocator, user_args[1..]) catch |err| switch (err) {
            error.MissingArgument,
            error.UnknownSubcommand,
            error.UnknownFlag,
            error.TooManyArguments,
            error.BaselineFlagRemoved,
            error.NonRatchetableSpec,
            => std.process.exit(1),
            error.HandlerCompileFailed, error.Regression => std.process.exit(1),
            else => return err,
        };
        return;
    }
    if (std.mem.eql(u8, command, "expert")) {
        if (hasHelpFlag(user_args[1..])) {
            printExpertHelp();
            return;
        }
        switch (validateExpertArgs(user_args[1..])) {
            .ok => {},
            .unknown_flag => |flag| {
                std.debug.print("zigttp expert does not accept flag '{s}'. See `zigttp expert --help`.\n", .{flag});
                std.process.exit(1);
            },
            .unexpected_arg => |arg| {
                std.debug.print(
                    "zigttp expert does not accept subcommand or positional argument '{s}'. See `zigttp expert --help`.\n",
                    .{arg},
                );
                std.process.exit(1);
            },
        }
        _ = pi_app.parseExpertFlags(user_args[1..]) catch |err| {
            std.debug.print("{s}", .{pi_app.flagErrorMessage(err)});
            std.process.exit(2);
        };
        if (!pi_app.envHasModelBackend()) {
            std.debug.print(
                \\zigttp expert needs a model backend.
                \\
                \\Quickest path:
                \\  zigttp auth claude   # paste your key once, stored at ~/.zigttp/providers.json
                \\
                \\Or set one of these environment variables and run `zigttp expert` again:
                \\  ANTHROPIC_API_KEY   (recommended)  https://console.anthropic.com/
                \\  OPENAI_API_KEY
                \\
                \\See `zigttp expert --help` for details.
                \\
            , .{});
            std.process.exit(1);
        }
        const witness_replay_lib = @import("witness_replay_lib.zig");
        const perf_probe_lib = @import("perf_probe_lib.zig");
        const equivalence_probe_lib = @import("equivalence_probe_lib.zig");
        pi_app.setInvocationArgv(user_args[1..]);
        pi_app.witness_replay.setReplayFn(witness_replay_lib.replayWitnessJsonl);
        pi_app.perf_probe.setProbeFn(perf_probe_lib.recordPerfReceipt);
        pi_app.equivalence_probe.setProbeFn(equivalence_probe_lib.recordEquivalenceReceipt);
        pi_app.capsule_probe.setProbeFn(capsuleReplayProbe);
        try pi_app.run(allocator);
        return;
    }
    if (std.mem.eql(u8, command, "deploy")) {
        if (deployArgsRequestCloud(user_args[1..])) |flag| {
            std.debug.print(
                "zigttp deploy with `{s}` selects hosted cloud deploy, which is not available in v0.1.0-beta.\n" ++
                    "Run `zigttp deploy` without the flag to build a self-contained binary you can run anywhere.\n",
                .{flag},
            );
            std.process.exit(1);
        }
        build_command.localDeployCommand(allocator, user_args[1..]) catch |err| {
            if (err == error.NoProjectConfig) {
                printNoProjectConfigDiagnostic(command);
                std.process.exit(1);
            }
            if (err == error.UnknownOption) {
                std.process.exit(1);
            }
            // buildArtifact already printed a remediation line for each
            // of these; exit cleanly so the user does not also see a
            // Zig panic-style stack trace.
            switch (err) {
                error.ParseError,
                error.VerificationFailed,
                error.NoBytecode,
                error.FileNotFound,
                error.AccessDenied,
                => std.process.exit(1),
                else => return err,
            }
        };
        return;
    }
    if (std.mem.eql(u8, command, "verify")) {
        const opts = verify_cli.parseArgs(user_args[1..]) catch |err| switch (err) {
            error.HelpRequested => {
                verify_cli.printHelp();
                return;
            },
            error.MissingArgument => {
                std.debug.print("zigttp verify: <url> is required\n\n", .{});
                verify_cli.printHelp();
                std.process.exit(verify_cli.exit_arg_error);
            },
            error.UnknownArgument, error.TooManyArguments, error.InvalidTrustKey => {
                std.debug.print("zigttp verify: invalid arguments\n\n", .{});
                verify_cli.printHelp();
                std.process.exit(verify_cli.exit_arg_error);
            },
        };
        const code = try verify_cli.run(allocator, opts);
        if (code != 0) std.process.exit(code);
        return;
    }
    if (std.mem.eql(u8, command, "proofs")) {
        // Expected user-input errors are explained on stderr by proofs_cli
        // itself; only unexpected ones (allocator, etc.) bubble.
        proofs_cli.run(allocator, user_args[1..]) catch |err| {
            if (proofs_cli.isExpectedUserError(err)) std.process.exit(1);
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "proof")) {
        // Proof Flight Recorder capsule replay. Expected user errors (missing
        // capsule, policy mismatch, regression) are explained on stderr;
        // only unexpected ones bubble.
        proof_cli.run(allocator, user_args[1..]) catch |err| {
            if (proof_cli.isExpectedUserError(err)) std.process.exit(1);
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "witnesses")) {
        witnesses_cli.run(allocator, user_args[1..]) catch |err| {
            if (witnesses_cli.isExpectedUserError(err)) std.process.exit(1);
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "version") or std.mem.eql(u8, command, "--version")) {
        shared.printVersion();
        return;
    }
    if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "help")) {
        if (hasAllFlag(user_args[1..])) printHelpAll() else printHelp();
        return;
    }

    std.debug.print("Unknown command: {s}\n\n", .{command});
    printHelp();
    std.process.exit(1);
}

fn devCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
    try runDevPreflight(allocator, argv, "dev");

    const serve_binary = try resolveDeveloperServeBinary(allocator, program_path);
    defer allocator.free(serve_binary);

    // `zigttp dev` is the magnet surface: it implies `--watch --prove` so the
    // proof HUD lights up on the first save. `--no-prove` opts out of proof
    // verification but still watches; both flags can be passed explicitly to
    // be a no-op (idempotent).
    const user_no_prove = shared.hasFlag(argv, "--no-prove");
    const user_has_watch = shared.hasFlag(argv, "--watch");
    const user_has_prove = shared.hasFlag(argv, "--prove");
    const user_has_quest = shared.hasFlag(argv, "--quest");
    const user_no_tour = shared.hasFlag(argv, "--no-tour");
    const user_record_proof = shared.hasFlag(argv, "--record-proof");
    const default_quest = !user_no_tour and !user_has_quest and shared.stderrIsTty() and !cli_tour.tourMarkerExists(allocator);

    // `--record-proof` desugars to the existing live `--trace` capture aimed
    // inside a capsule directory, plus a manifest written once the session
    // ends. The session trace path must outlive the child process.
    const capsule_name = "default";
    const record_trace_path: ?[]u8 = if (user_record_proof)
        proof_cli.sessionTracePathAlloc(allocator, capsule_name) catch |err| blk: {
            std.debug.print("zigttp dev: --record-proof could not prepare a capsule: {}\n", .{err});
            break :blk null;
        }
    else
        null;
    defer if (record_trace_path) |p| allocator.free(p);

    var child_args = std.ArrayList([]const u8).empty;
    defer child_args.deinit(allocator);
    try child_args.append(allocator, serve_binary);
    try child_args.append(allocator, "serve");
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--no-prove")) continue;
        if (std.mem.eql(u8, arg, "--no-tour")) continue;
        // `--record-proof` is a dev-layer flag; the runtime sees only --trace.
        if (std.mem.eql(u8, arg, "--record-proof")) continue;
        try child_args.append(allocator, arg);
    }
    if (!user_has_watch) try child_args.append(allocator, "--watch");
    if (!user_has_prove and !user_no_prove) try child_args.append(allocator, "--prove");
    if (default_quest and !user_no_prove) try child_args.append(allocator, "--quest-default");
    if (record_trace_path) |trace_path| {
        try child_args.append(allocator, "--trace");
        try child_args.append(allocator, trace_path);
        // Write the manifest up front: it describes the handler (its hashes,
        // contract, policy), not the recorded traces, and `dev` is normally
        // ended with Ctrl+C — which signals the whole process group, so any
        // post-shutdown code here would not reliably run. Writing now means
        // the capsule is complete the moment the first request is captured.
        recordProofManifest(allocator, argv, capsule_name) catch |err| {
            std.debug.print("zigttp dev: --record-proof could not write the capsule manifest: {}\n", .{err});
        };
        std.debug.print("Recording a proof capsule to .zigttp/capsules/{s}/ — requests this session are captured.\n", .{capsule_name});
        std.debug.print("After an edit, replay it with: zigttp proof replay {s}\n", .{capsule_name});
    }

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

/// Adapter registered into the `pi_app.capsule_probe` seam: replays the
/// workspace's active capsule against just-applied content and maps the
/// runtime tally onto the PI seam's tally type. Best-effort by contract.
fn capsuleReplayProbe(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    handler_path: []const u8,
    after_bytes: []const u8,
) anyerror!pi_app.capsule_probe.ReplayTally {
    const tally = proof_cli.replayActiveCapsule(allocator, workspace_root, handler_path, after_bytes) catch
        return pi_app.capsule_probe.ReplayTally{};
    return .{ .total = tally.total, .regressed = tally.regressed };
}

/// Resolve the handler + system path the recording session used, then write
/// the capsule manifest. Split out so `devCommand` stays readable.
fn recordProofManifest(allocator: std.mem.Allocator, argv: []const []const u8, capsule_name: []const u8) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const explicit_path = shared.findPositionalPath(argv);
    var project = try project_config_mod.discover(allocator, io, explicit_path);
    defer if (project) |*p| p.deinit(allocator);

    const handler_path = if (explicit_path) |path|
        try allocator.dupe(u8, path)
    else if (project) |*cfg|
        try cfg.resolvedEntry(allocator)
    else
        return error.NoProjectConfig;
    defer allocator.free(handler_path);

    const system_path = if (explicit_path == null and project != null)
        try project.?.resolvedSystemPath(allocator)
    else
        null;
    defer if (system_path) |p| allocator.free(p);

    try proof_cli.writeManifest(allocator, capsule_name, handler_path, system_path);
}

fn studioCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
    const parsed = try extractTemplateFlag(allocator, argv);
    defer allocator.free(parsed.filtered);

    try studioPreflight(allocator, parsed.template);
    try runDevPreflight(allocator, parsed.filtered, "studio");

    const serve_binary = try resolveDeveloperServeBinary(allocator, program_path);
    defer allocator.free(serve_binary);

    const user_has_watch = shared.hasFlag(parsed.filtered, "--watch");
    const user_has_prove = shared.hasFlag(parsed.filtered, "--prove");

    var child_args = std.ArrayList([]const u8).empty;
    defer child_args.deinit(allocator);
    try child_args.append(allocator, serve_binary);
    try child_args.append(allocator, "serve");
    for (parsed.filtered) |arg| try child_args.append(allocator, arg);
    if (!shared.hasFlag(parsed.filtered, "--studio")) try child_args.append(allocator, "--studio");
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

/// Lets `mkdir myapp && cd myapp && zigttp studio` work as a single demo
/// gesture. If the cwd (or any ancestor) already has a `zigttp.json`, this
/// is a no-op and the existing project loads. If the cwd is empty (only
/// dotfiles like `.git` allowed), we scaffold the selected template
/// in-place. If the cwd has user files but no manifest, we leave the
/// existing `error.NoProjectConfig` path to print the standard hint.
fn studioPreflight(allocator: std.mem.Allocator, template: Template) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var project_opt = try project_config_mod.discover(allocator, io, null);
    if (project_opt) |*p| {
        p.deinit(allocator);
        return;
    }

    if (!directoryIsEffectivelyEmpty(io)) return;

    try init_command.scaffoldProject(allocator, ".", template);
    if (!builtin.is_test) {
        std.debug.print("Scaffolded {s} template in current directory. Opening studio.\n\n", .{@tagName(template)});
    }
}

/// Empty enough to scaffold over: no non-dotfile entries. `.git`,
/// `.DS_Store`, IDE configs are all tolerated. A pre-existing
/// `zigttp.json` would have been caught by `discover` upstream, so this
/// only inspects the surface of the cwd.
fn directoryIsEffectivelyEmpty(io: std.Io) bool {
    var dir = std.Io.Dir.openDir(std.Io.Dir.cwd(), io, ".", .{ .iterate = true }) catch return false;
    defer dir.close(io);

    var iter = dir.iterate();
    while (iter.next(io) catch return false) |entry| {
        if (entry.name.len == 0) continue;
        if (entry.name[0] == '.') continue;
        return false;
    }
    return true;
}

const StudioArgs = struct {
    template: Template,
    /// Owned allocation; caller frees once.
    filtered: [][]const u8,
};

/// Parse and consume `--template <name>` from `argv`. Everything else is
/// forwarded to `serve`, which would reject `--template` as
/// `error.UnknownOption`. Default template is `basic`.
fn extractTemplateFlag(allocator: std.mem.Allocator, argv: []const []const u8) !StudioArgs {
    var template: Template = .basic;
    var filtered = try allocator.alloc([]const u8, argv.len);
    errdefer allocator.free(filtered);

    var n: usize = 0;
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        if (std.mem.eql(u8, argv[i], "--template")) {
            if (i + 1 >= argv.len) return error.MissingTemplate;
            template = parseTemplate(argv[i + 1]) orelse return error.InvalidTemplate;
            i += 1;
            continue;
        }
        filtered[n] = argv[i];
        n += 1;
    }
    const resized = try allocator.realloc(filtered, n);
    return .{ .template = template, .filtered = resized };
}

fn runDevPreflight(allocator: std.mem.Allocator, argv: []const []const u8, command: []const u8) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const explicit_path = shared.findPositionalPath(argv);
    var project = try project_config_mod.discover(allocator, io, explicit_path);
    defer if (project) |*p| p.deinit(allocator);

    const target = if (explicit_path) |path|
        path
    else if (project) |*cfg|
        try cfg.resolvedEntry(allocator)
    else
        return error.NoProjectConfig;
    defer if (explicit_path == null) allocator.free(target);

    if (!doctorPathExists(io, target)) {
        if (!builtin.is_test) {
            std.debug.print("zigttp {s} preflight failed: handler not found: {s}\n", .{ command, target });
            std.debug.print("Next: update `entry` in zigttp.json or pass an explicit handler path.\n", .{});
        }
        return error.FileNotFound;
    }

    const explicit_sqlite = optionValue(argv, "--sqlite");
    const explicit_system = optionValue(argv, "--system");

    const discovered_sqlite = if (explicit_sqlite == null and project != null)
        try project.?.resolvedSqlitePath(allocator)
    else
        null;
    defer if (discovered_sqlite) |path| allocator.free(path);

    const discovered_system = if (explicit_system == null and project != null)
        try project.?.resolvedSystemPath(allocator)
    else
        null;
    defer if (discovered_system) |path| allocator.free(path);

    const sqlite_path = explicit_sqlite orelse discovered_sqlite;
    const system_path = explicit_system orelse discovered_system;

    var check = precompile.runCheckOnly(allocator, target, sqlite_path, false, system_path) catch |err| {
        if (!builtin.is_test) {
            std.debug.print("zigttp {s} preflight could not run for {s}: {}\n", .{ command, target, err });
            std.debug.print("Next: run `zigttp check {s}` for full diagnostics.\n", .{target});
        }
        return error.CheckFailed;
    };
    defer check.deinit(allocator);

    if (check.totalErrors() > 0) {
        if (!builtin.is_test) {
            std.debug.print("zigttp {s} preflight failed for {s}: {d} error(s)\n", .{ command, target, check.totalErrors() });
            printCheckStageFailures(&check, "  ");
            std.debug.print("Next: fix the errors above, then rerun `zigttp {s}`.\n", .{command});
        }
        return error.CheckFailed;
    }

    if (!builtin.is_test) {
        var card_buf: std.ArrayList(u8) = .empty;
        defer card_buf.deinit(allocator);
        var card_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &card_buf);
        precompile.formatProofCard(&card_aw.writer, &check, target);
        card_buf = card_aw.toArrayList();
        if (card_buf.items.len > 0) {
            _ = std.c.write(std.c.STDERR_FILENO, card_buf.items.ptr, card_buf.items.len);
        }
    }
}

fn optionValue(argv: []const []const u8, name: []const u8) ?[]const u8 {
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        if (!std.mem.eql(u8, argv[i], name)) continue;
        return if (i + 1 < argv.len) argv[i + 1] else null;
    }
    return null;
}

fn printDevHelp() void {
    const help =
        \\zigttp dev [options] [handler.ts]
        \\
        \\Start the local edit loop. The command runs the analyzer, then
        \\serves the handler with watch mode enabled. By default it also
        \\proves each change before hot-swapping the running handler.
        \\
        \\Common options:
        \\  -p, --port <PORT>     Port to listen on (project default: 3000)
        \\  -h, --host <HOST>     Host to bind to (project default: 127.0.0.1)
        \\  --studio              Also serve the optional /_zigttp/studio mirror
        \\  --no-prove            Watch and reload without contract proof gating
        \\  --record-proof        Capture this session's requests into a replayable
        \\                        proof capsule (.zigttp/capsules/default/)
        \\  --no-tour             Skip the first-run proof tour
        \\  --quest               Replay the guided proof quest
        \\  --outbound-http       Enable native outbound HTTP bridge
        \\  --sqlite <FILE>       SQLite database for zigttp:sql
        \\  --system <FILE>       System registry for zigttp:service
        \\  --help                Show this help
        \\
        \\If no handler path is passed, the entry in zigttp.json is used.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printStudioHelp() void {
    const help =
        \\zigttp studio [options] [handler.ts]
        \\
        \\Open the optional browser proof workbench at /_zigttp/studio. In an
        \\empty directory, this command scaffolds a project in place before
        \\launching.
        \\
        \\Options:
        \\  --template basic|api|htmx  Template used for empty-dir scaffolding
        \\  -p, --port <PORT>          Port to listen on (default: 3000)
        \\  -h, --host <HOST>          Host to bind to (default: 127.0.0.1)
        \\  --no-env-check             Skip startup env validation
        \\  --help                     Show this help
        \\
        \\Studio implies --watch --prove and keeps the old handler running when
        \\a save fails verification.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn hasAllFlag(argv: []const []const u8) bool {
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--all") or std.mem.eql(u8, arg, "all")) return true;
    }
    return false;
}

const ExpertArgValidation = union(enum) {
    ok,
    unknown_flag: []const u8,
    unexpected_arg: []const u8,
};

fn validateExpertArgs(argv: []const []const u8) ExpertArgValidation {
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help") or
            std.mem.eql(u8, arg, "-h") or
            std.mem.eql(u8, arg, "help"))
        {
            continue;
        }
        if (isExpertBareFlag(arg)) continue;
        if (std.mem.startsWith(u8, arg, "--")) {
            if (isExpertValueTakingFlag(arg)) {
                if (i + 1 < argv.len) i += 1;
                continue;
            }
            if (isExpertValueTakingFlagEq(arg)) continue;
            return .{ .unknown_flag = arg };
        }
        return .{ .unexpected_arg = arg };
    }
    return .ok;
}

fn isExpertBareFlag(arg: []const u8) bool {
    return std.mem.eql(u8, arg, "--yes") or
        std.mem.eql(u8, arg, "--no-edit") or
        std.mem.eql(u8, arg, "--no-session") or
        std.mem.eql(u8, arg, "--no-persist-tool-output") or
        std.mem.eql(u8, arg, "--no-context-files") or
        std.mem.eql(u8, arg, "--perf-receipt") or
        std.mem.eql(u8, arg, "--no-perf-receipt") or
        std.mem.eql(u8, arg, "--resume") or
        std.mem.eql(u8, arg, "--continue");
}

fn isExpertValueTakingFlag(arg: []const u8) bool {
    for (pi_app.value_taking_flags) |name| {
        if (std.mem.eql(u8, arg, name)) return true;
    }
    return false;
}

fn isExpertValueTakingFlagEq(arg: []const u8) bool {
    for (pi_app.value_taking_flags) |name| {
        if (arg.len > name.len and
            std.mem.startsWith(u8, arg, name) and
            arg[name.len] == '=')
        {
            return true;
        }
    }
    return false;
}

/// The default `zigttp --help` surface: only the five core verbs. Everything
/// else lives behind `zigttp help --all` (`core_help_all`).
const core_help =
    \\zigttp - serverless JavaScript runtime
    \\
    \\Commands:
    \\  zigttp init <name>     Create a project
    \\  zigttp dev             Run locally, watch and prove on every save
    \\  zigttp test            Run handler tests
    \\  zigttp expert          Interactive compiler-in-the-loop agent
    \\  zigttp deploy          Build, prove, and deploy (local by default)
    \\
    \\Get started:
    \\  zigttp init my-app && cd my-app
    \\  zigttp dev
    \\
    \\Run `zigttp help --all` for advanced commands.
    \\
;

fn printHelp() void {
    _ = std.c.write(std.c.STDOUT_FILENO, core_help.ptr, core_help.len);
}

const core_help_all =
    \\zigttp - serverless JavaScript runtime
    \\
    \\Core commands:
    \\  zigttp init <name> [--template basic|api|htmx]  Create a project
    \\  zigttp dev [handler.ts]                Run locally, watch and prove on save
    \\  zigttp test [tests.jsonl]              Run handler tests
    \\  zigttp expert                          Interactive compiler-in-the-loop agent
    \\  zigttp deploy                          Build, prove, deploy (local default)
    \\
    \\Analyze:
    \\  zigttp check [handler.ts]              Run the analyzer once
    \\  zigttp prove <old.json> <new.json>     Contract upgrade safety check
    \\  zigttp mock <tests.jsonl>              Mock server from test fixtures
    \\  zigttp link <system.json>              Cross-handler system linking
    \\  zigttp gen-tests [handler.ts]          Generate a starter test fixture
    \\
    \\Run and inspect:
    \\  zigttp serve [handler.ts]              Run a handler without watch or proof
    \\  zigttp doctor [path]                   Check project readiness
    \\  zigttp studio [handler.ts]             Optional browser proof workbench
    \\  zigttp demo                            Guided local proof theater
    \\  zigttp edge [--config FILE]            Run the in-process edge runtime
    \\
    \\Package:
    \\  zigttp build [-o <bin>]                Emit a self-contained binary
    \\  zigttp compile <handler.ts> -o <bin>   Build a binary from an explicit path
    \\
    \\Proof ledger:
    \\  zigttp proofs [list|show|diff|watch|export|badge|bundle|verify]
    \\  zigttp proof replay <capsule>          Replay a recorded capsule against the current handler
    \\  zigttp verify <url>                    Verify a deployed proof receipt
    \\
    \\Credentials:
    \\  zigttp auth claude                     Store an Anthropic API key for expert
    \\  zigttp auth status                     Show which provider keys are configured
    \\  zigttp auth revoke <provider>          Remove a stored key (claude | openai)
    \\
    \\Machine tools (JSON output for IDE and review-bot integrations):
    \\  zigttp features                        List supported language features
    \\  zigttp modules                         List virtual module exports
    \\  zigttp restrictions                    Show language restrictions and the proofs they unlock
    \\  zigttp meta                            Compiler and policy metadata
    \\  zigttp describe-rule [name|code]       Look up a diagnostic rule
    \\  zigttp search <keyword>                Search rules by keyword
    \\  zigttp verify-paths <file>...          Behavior-path verification
    \\  zigttp verify-modules <file>...        Module-contract verification
    \\  zigttp edit-simulate [handler.ts]      Simulate an edit and report violations
    \\  zigttp review-patch <file>             Review a patch for new violations
    \\  zigttp prove-behavior <before> <after> Behavioral-equivalence verdict between two handler versions
    \\  zigttp rollout <old-system> <new>      System-level deployment manifest
    \\
    \\Advanced:
    \\  zigttp ratchet [show|check]            Property-regression gate
    \\  zigttp witnesses [list|pin|unpin|prune|synthesize]  Falsifying-input corpus
    \\  zigttp version                         Show version
    \\
    \\Every command keeps its own `--help`.
    \\
;

fn printHelpAll() void {
    _ = std.c.write(std.c.STDOUT_FILENO, core_help_all.ptr, core_help_all.len);
}

const expert_help =
    \\zigttp expert - interactive compiler-in-the-loop agent
    \\
    \\Usage:
    \\  zigttp expert [--yes | --no-edit] [--no-session] [--no-persist-tool-output]
    \\                [--session-id <id> | --resume | --continue | --fork <id>]
    \\                [--tools minimal|full] [--no-context-files]
    \\  zigttp expert --print <prompt> [--mode json]
    \\  zigttp expert --mode rpc
    \\  zigttp expert --handler <handler.ts> --goal <goals> [--max-iters N]
    \\
    \\Flags:
    \\  --yes                      auto-approve every verified edit
    \\  --no-edit                  auto-reject every verified edit
    \\  --no-session               disable session persistence for this run
    \\  --no-persist-tool-output   omit tool output bodies from persisted session
    \\  --no-context-files         skip AGENTS.md / CLAUDE.md project context
    \\  --no-perf-receipt          do not sign a perf receipt on applied edits
    \\  --session-id <id>          resume or create a session with this id
    \\  --resume, --continue       resume the newest session for this cwd
    \\  --fork <session-id>        branch from an existing session
    \\  --tools minimal|full       select workspace-read-only or full tool preset
    \\  --print <prompt>           run a single non-interactive turn and exit
    \\  --mode json                with --print, emit NDJSON transcript events
    \\  --mode rpc                 run line-delimited JSON-RPC 2.0 over stdio
    \\  --handler <path>           handler path for autoloop repair
    \\  --goal <csv>               property goals for autoloop repair
    \\  --max-iters <N>            autoloop iteration budget
    \\
    \\Examples:
    \\  zigttp expert --resume
    \\  zigttp expert --print "add a GET /health route" --mode json
    \\  zigttp expert --handler handler.ts --goal no_secret_leakage
    \\
    \\Model backend:
    \\  Run `zigttp auth claude` once to store an Anthropic key at
    \\  ~/.zigttp/providers.json (mode 0600). `zigttp expert` reads it
    \\  on launch. Alternatively, export one of these variables yourself:
    \\    ANTHROPIC_API_KEY   (recommended)  https://console.anthropic.com/
    \\    OPENAI_API_KEY
    \\  An empty value counts as missing; the command exits with a setup
    \\  message instead of launching against an unconfigured backend.
    \\
    \\For machine-facing compiler tooling, use direct commands such as:
    \\  zigttp meta
    \\  zigttp verify-paths <file>...
    \\  zigttp verify-modules --builtins --strict --json
    \\  zigttp proofs export --session <id> --out <path>
    \\
;

fn printExpertHelp() void {
    _ = std.c.write(std.c.STDOUT_FILENO, expert_help.ptr, expert_help.len);
}

test "default help advertises only the five core commands" {
    const has = struct {
        fn f(haystack: []const u8, needle: []const u8) bool {
            return std.mem.indexOf(u8, haystack, needle) != null;
        }
    }.f;
    inline for (.{
        "zigttp init", "zigttp dev", "zigttp test", "zigttp expert", "zigttp deploy",
    }) |verb| {
        try std.testing.expect(has(core_help, verb));
    }
    inline for (.{
        "zigttp check", "zigttp serve", "zigttp compile", "zigttp proofs", "zigttp doctor",
    }) |hidden| {
        try std.testing.expect(!has(core_help, hidden));
    }
}

test "help --all surfaces the advanced commands" {
    const has = struct {
        fn f(haystack: []const u8, needle: []const u8) bool {
            return std.mem.indexOf(u8, haystack, needle) != null;
        }
    }.f;
    inline for (.{
        "zigttp serve",        "zigttp build",          "zigttp compile",
        "zigttp doctor",       "zigttp proofs",         "zigttp check",
        "zigttp prove",        "zigttp features",       "zigttp modules",
        "zigttp restrictions", "zigttp meta",           "zigttp describe-rule",
        "zigttp verify-paths", "zigttp verify-modules", "zigttp edit-simulate",
        "zigttp review-patch", "zigttp rollout",
    }) |cmd| {
        try std.testing.expect(has(core_help_all, cmd));
    }
}

test "help --all surfaces the optional browser studio workbench" {
    try std.testing.expect(std.mem.indexOf(u8, core_help_all, "zigttp studio") != null);
}

test "hasAllFlag detects the --all escape hatch" {
    try std.testing.expect(hasAllFlag(&.{"--all"}));
    try std.testing.expect(hasAllFlag(&.{ "foo", "all" }));
    try std.testing.expect(!hasAllFlag(&.{"--help"}));
    try std.testing.expect(!hasAllFlag(&.{}));
}

test "validateExpertArgs accepts documented expert launch forms" {
    const ok = struct {
        fn expect(argv: []const []const u8) !void {
            switch (validateExpertArgs(argv)) {
                .ok => {},
                else => return error.ExpectedValidExpertArgs,
            }
        }
    }.expect;

    try ok(&.{});
    try ok(&.{"--resume"});
    try ok(&.{"--continue"});
    try ok(&.{ "--session-id", "abc" });
    try ok(&.{"--session-id=abc"});
    try ok(&.{ "--fork", "abc" });
    try ok(&.{"--fork=abc"});
    try ok(&.{ "--print", "add a GET /health route", "--mode", "json" });
    try ok(&.{"--mode=rpc"});
    try ok(&.{ "--handler", "handler.ts", "--goal", "no_secret_leakage", "--max-iters", "4" });
    try ok(&.{ "--tools", "minimal", "--yes", "--no-context-files" });
    try ok(&.{ "--no-session", "--no-persist-tool-output", "--no-edit" });
    try ok(&.{"--no-perf-receipt"});
    try ok(&.{"--perf-receipt"});
}

test "validateExpertArgs rejects unknown expert flags and subcommands" {
    switch (validateExpertArgs(&.{"--bogus"})) {
        .unknown_flag => |flag| try std.testing.expectEqualStrings("--bogus", flag),
        else => return error.ExpectedUnknownExpertFlag,
    }
    switch (validateExpertArgs(&.{"diagnose"})) {
        .unexpected_arg => |arg| try std.testing.expectEqualStrings("diagnose", arg),
        else => return error.ExpectedUnexpectedExpertArg,
    }
}

test "expert help advertises documented modes" {
    inline for (.{
        "zigttp expert --resume",
        "zigttp expert --print <prompt> [--mode json]",
        "zigttp expert --mode rpc",
        "zigttp expert --handler <handler.ts> --goal <goals>",
        "--tools minimal|full",
        "--no-context-files",
    }) |needle| {
        try std.testing.expect(std.mem.indexOf(u8, expert_help, needle) != null);
    }
}

test "hasLongHelpFlag preserves -h for host flags" {
    try std.testing.expect(hasLongHelpFlag(&.{"--help"}));
    try std.testing.expect(hasLongHelpFlag(&.{"help"}));
    try std.testing.expect(!hasLongHelpFlag(&.{"-h"}));
    try std.testing.expect(!hasLongHelpFlag(&.{ "-h", "0.0.0.0" }));
}

test "deployArgsRequestCloud requires explicit opt-in and reports the triggering flag" {
    try std.testing.expectEqual(@as(?[]const u8, null), deployArgsRequestCloud(&.{}));
    try std.testing.expectEqual(@as(?[]const u8, null), deployArgsRequestCloud(&.{"--local"}));
    try std.testing.expectEqual(@as(?[]const u8, null), deployArgsRequestCloud(&.{ "--target", "local" }));

    try std.testing.expectEqualStrings("--cloud", deployArgsRequestCloud(&.{"--cloud"}).?);
    try std.testing.expectEqualStrings("--region", deployArgsRequestCloud(&.{ "--region", "us-east" }).?);
    try std.testing.expectEqualStrings("--confirm", deployArgsRequestCloud(&.{"--confirm"}).?);
    try std.testing.expectEqualStrings("--wait", deployArgsRequestCloud(&.{"--wait"}).?);
    try std.testing.expectEqualStrings("--no-wait", deployArgsRequestCloud(&.{"--no-wait"}).?);
}

test "help --all no longer advertises hosted cloud deploy" {
    const has = struct {
        fn f(haystack: []const u8, needle: []const u8) bool {
            return std.mem.indexOf(u8, haystack, needle) != null;
        }
    }.f;
    // Trailing space avoids spurious substring matches with longer
    // command names (e.g. `zigttp review` would otherwise match
    // `zigttp review-patch`).
    inline for (.{
        "zigttp login ",  "zigttp logout ",       "zigttp review ",
        "zigttp grants ", "zigttp revoke-grant ", "zigttp assert-intent ",
        "--cloud",
    }) |hidden| {
        try std.testing.expect(!has(core_help_all, hidden));
    }
}

test "resolveDeveloperServeBinary re-enters developer CLI for studio and dev" {
    const path = try resolveDeveloperServeBinary(std.testing.allocator, "/tmp/bin/zigttp");
    defer std.testing.allocator.free(path);
    try std.testing.expectEqualStrings("/tmp/bin/zigttp", path);

    const fallback = try resolveDeveloperServeBinary(std.testing.allocator, "");
    defer std.testing.allocator.free(fallback);
    try std.testing.expectEqualStrings("zigttp", fallback);
}

test "resolveReentryBinaryAfterChdir preserves PATH lookup for bare names" {
    const bare = try resolveReentryBinaryAfterChdir(std.testing.allocator, "zigttp", "/repo");
    defer std.testing.allocator.free(bare);
    try std.testing.expectEqualStrings("zigttp", bare);

    const relative = try resolveReentryBinaryAfterChdir(std.testing.allocator, "./zig-out/bin/zigttp", "/repo");
    defer std.testing.allocator.free(relative);
    try std.testing.expect(std.mem.endsWith(u8, relative, "/repo/zig-out/bin/zigttp"));

    const absolute = try resolveReentryBinaryAfterChdir(std.testing.allocator, "/usr/local/bin/zigttp", "/repo");
    defer std.testing.allocator.free(absolute);
    try std.testing.expectEqualStrings("/usr/local/bin/zigttp", absolute);
}

test "extractTemplateFlag defaults to basic and strips the flag pair" {
    const out = try extractTemplateFlag(std.testing.allocator, &.{ "--watch", "--studio" });
    defer std.testing.allocator.free(out.filtered);
    try std.testing.expectEqual(Template.basic, out.template);
    try std.testing.expectEqual(@as(usize, 2), out.filtered.len);
    try std.testing.expectEqualStrings("--watch", out.filtered[0]);
    try std.testing.expectEqualStrings("--studio", out.filtered[1]);
}

test "extractTemplateFlag consumes --template <name> and forwards the rest" {
    const out = try extractTemplateFlag(std.testing.allocator, &.{ "--template", "htmx", "--watch" });
    defer std.testing.allocator.free(out.filtered);
    try std.testing.expectEqual(Template.htmx, out.template);
    try std.testing.expectEqual(@as(usize, 1), out.filtered.len);
    try std.testing.expectEqualStrings("--watch", out.filtered[0]);
}

test "extractTemplateFlag rejects missing or unknown templates" {
    try std.testing.expectError(error.MissingTemplate, extractTemplateFlag(std.testing.allocator, &.{"--template"}));
    try std.testing.expectError(error.InvalidTemplate, extractTemplateFlag(std.testing.allocator, &.{ "--template", "react" }));
}

test "studioPreflight scaffolds in an empty cwd" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try studioPreflight(testing.allocator, .basic);

    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "zigttp.json", .{});
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "src/handler.ts", .{});
}

test "studioPreflight is a no-op when zigttp.json already exists" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    // Seed an existing project marker. Body content is intentionally bogus;
    // discover() only cares that the file exists.
    try zigts.file_io.writeFile(testing.allocator, "zigttp.json", "{}");

    try studioPreflight(testing.allocator, .basic);

    // No `src/handler.ts` should have been scaffolded over the existing project.
    try std.testing.expectError(error.FileNotFound, std.Io.Dir.access(std.Io.Dir.cwd(), io, "src/handler.ts", .{}));
}

test "studioPreflight refuses to scaffold over user files" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try zigts.file_io.writeFile(testing.allocator, "notes.txt", "not a zigttp project");

    try studioPreflight(testing.allocator, .basic);

    // No scaffold happened: zigttp.json must not exist.
    try std.testing.expectError(error.FileNotFound, std.Io.Dir.access(std.Io.Dir.cwd(), io, "zigttp.json", .{}));
}

test "studioPreflight tolerates dotfile-only cwd (e.g. .git)" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, ".git");

    try studioPreflight(testing.allocator, .basic);

    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "zigttp.json", .{});
}

test "runDevPreflight reports analyzer failure before starting dev loop" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try tmp.dir.createDirPath(io, "src");
    try tmp.dir.writeFile(io, .{
        .sub_path = "zigttp.json",
        .data =
        \\{
        \\  "entry": "src/handler.ts"
        \\}
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "src/handler.ts",
        .data =
        \\function handler(req) {
        \\    return Response.text("ok");
        \\}
        ,
    });

    try testing.expectError(error.CheckFailed, runDevPreflight(testing.allocator, &.{}, "dev"));
}

test "doctorPathExists accepts relative paths" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try tmp.dir.writeFile(io, .{
        .sub_path = "zigttp.json",
        .data =
        \\{
        \\  "entry": "examples/handler/handler.ts"
        \\}
        ,
    });
    try tmp.dir.createDirPath(io, "examples/handler");
    try tmp.dir.writeFile(io, .{
        .sub_path = "examples/handler/handler.ts",
        .data =
        \\function handler(req: Request): Response {
        \\    return Response.text("ok");
        \\}
        ,
    });

    try testing.expect(doctorPathExists(io, "examples/handler/handler.ts"));
    try testing.expect(!doctorPathExists(io, "examples/handler/missing.ts"));
}

test "release doctor options parse json and out path" {
    const opts = try cli_release_check.parseReleaseDoctorOptions(&.{ "--json", "--out", "docs/releases/passport.json" });
    try std.testing.expect(opts.json);
    try std.testing.expectEqualStrings("docs/releases/passport.json", opts.out_path.?);
    try std.testing.expectError(error.InvalidArgument, cli_release_check.parseReleaseDoctorOptions(&.{"--out"}));
    try std.testing.expectError(error.InvalidArgument, cli_release_check.parseReleaseDoctorOptions(&.{"--bad"}));
}

test "release passport reports ready for a complete fixture" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try writeReleaseDoctorFixture(io, &tmp, .{});

    var passport = try cli_release_check.collectReleasePassport(testing.allocator);
    defer passport.deinit(testing.allocator);
    try testing.expectEqual(cli_release_check.ReleaseVerdict.ready, passport.verdict());

    const json = try cli_release_check.renderReleasePassportJson(testing.allocator, &passport);
    defer testing.allocator.free(json);
    try testing.expect(std.mem.indexOf(u8, json, "\"verdict\":\"ready\"") != null);
    try testing.expect(std.mem.indexOf(u8, json, "\"release\":\"0.1.0-beta\"") != null);
}

test "release passport blocks stale public claims" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try writeReleaseDoctorFixture(io, &tmp, .{ .stale_readme = true });

    var passport = try cli_release_check.collectReleasePassport(testing.allocator);
    defer passport.deinit(testing.allocator);
    try testing.expectEqual(cli_release_check.ReleaseVerdict.blocked, passport.verdict());
}

test "release passport warns for documented reliability gap" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try writeReleaseDoctorFixture(io, &tmp, .{ .document_413_gap = true });

    var passport = try cli_release_check.collectReleasePassport(testing.allocator);
    defer passport.deinit(testing.allocator);
    try testing.expectEqual(cli_release_check.ReleaseVerdict.ready_with_known_issues, passport.verdict());
}

test "doctorCommand passes configured sqlite path into analyzer" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try tmp.dir.createDirPath(io, "src");
    try tmp.dir.writeFile(io, .{
        .sub_path = "zigttp.json",
        .data =
        \\{
        \\  "entry": "src/handler.ts",
        \\  "sqlite": "schema.sql"
        \\}
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "schema.sql",
        .data =
        \\CREATE TABLE users (
        \\    id INTEGER PRIMARY KEY,
        \\    name TEXT NOT NULL
        \\);
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "src/handler.ts",
        .data =
        \\import { sql, sqlMany } from "zigttp:sql";
        \\
        \\sql("listUsers", "SELECT id, name FROM users ORDER BY id ASC");
        \\
        \\function handler(req: Request): Response {
        \\    return Response.json({ users: sqlMany("listUsers", {}) });
        \\}
        ,
    });

    var project = try project_config_mod.discover(testing.allocator, io, null);
    defer if (project) |*cfg| cfg.deinit(testing.allocator);

    if (project) |*cfg| {
        const entry = try cfg.resolvedEntry(testing.allocator);
        defer testing.allocator.free(entry);
        const sqlite_path = try cfg.resolvedSqlitePath(testing.allocator);
        defer if (sqlite_path) |path| testing.allocator.free(path);

        var check = try runDoctorAnalyzerForProject(testing.allocator, cfg, entry, sqlite_path);
        defer check.deinit(testing.allocator);
        try testing.expectEqual(@as(usize, 0), check.totalErrors());
    } else {
        return error.NoProjectConfig;
    }
}

const ReleaseDoctorFixtureOptions = struct {
    stale_readme: bool = false,
    document_413_gap: bool = false,
};

fn writeReleaseDoctorFixture(io: std.Io, tmp: *std.testing.TmpDir, opts: ReleaseDoctorFixtureOptions) !void {
    try tmp.dir.createDirPath(io, "packages/zigts/src");
    try tmp.dir.createDirPath(io, "packages/runtime/src");
    try tmp.dir.createDirPath(io, "docs/releases");
    try tmp.dir.createDirPath(io, "docs");
    try tmp.dir.createDirPath(io, "scripts");
    try tmp.dir.createDirPath(io, ".github/workflows");

    try tmp.dir.writeFile(io, .{
        .sub_path = "build.zig.zon",
        .data =
        \\.{
        \\    .name = .zigttp,
        \\    .version = "0.1.0-beta",
        \\}
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "packages/zigts/src/root.zig",
        .data =
        \\pub const version = struct {
        \\    pub const string = "0.1.0-beta";
        \\};
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "build.zig",
        .data =
        \\// smoke-v1
        \\// test-module-governance
        \\// test-capability-audit
        ,
    });
    try tmp.dir.writeFile(io, .{ .sub_path = "scripts/smoke-v1.sh", .data = "#!/bin/sh\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "scripts/test-examples.sh", .data = "#!/bin/sh\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = ".github/workflows/ci.yml", .data = "name: ci\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = ".github/workflows/release.yml", .data = "name: release\n" });
    try tmp.dir.writeFile(io, .{
        .sub_path = "docs/releases/v0.1.0-beta-checklist.md",
        .data =
        \\# Checklist
        \\
        \\| Item | Domain | Owner | Disposition |
        \\|------|--------|-------|-------------|
        \\| No blockers | Release | srdjan | ship |
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "docs/releases/v0.1.0-beta-benchmarks.md",
        .data =
        \\# Benchmarks
        \\zigttp 112,393 req/s.
        \\RSS 13.4 MB.
        \\Cold start 7.3 ms.
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "README.md",
        .data = if (opts.stale_readme)
            "Numbers: 3ms runtime init. 1.2MB binary. 4MB memory baseline.\n"
        else
            "Numbers: cold start 7.3 ms, RSS 13.4 MB, throughput 112,393 req/s.\n",
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "docs/performance.md",
        .data = "Performance numbers match v0.1.0-beta benchmark evidence.\n",
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "docs/reliability.md",
        .data = if (opts.document_413_gap)
            "Request bodies over the cap closes the connection without a response; 413 is tracked.\n"
        else
            "No release-blocking reliability gaps are documented.\n",
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "packages/runtime/src/dev_cli.zig",
        .data = "zigttp verify <url>\nproofs\n--no-attest\n",
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "packages/runtime/src/proofs_cli.zig",
        .data = "badge\nbundle\nverify\n",
    });
}

test "first-run tour marker is durable: absent then created then detected" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const base = try std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp_dir.sub_path});

    try testing.expect(!cli_tour.tourMarkerExistsAt(allocator, base));
    cli_tour.touchTourMarkerAt(allocator, base);
    try testing.expect(cli_tour.tourMarkerExistsAt(allocator, base));
    // Idempotent: a second touch is harmless.
    cli_tour.touchTourMarkerAt(allocator, base);
    try testing.expect(cli_tour.tourMarkerExistsAt(allocator, base));
}

