const std = @import("std");
const builtin = @import("builtin");

const project_config_mod = @import("project_config");
const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const proofs_cli = @import("proofs_cli.zig");
const proof_cli = @import("proof_cli.zig");
const witnesses_cli = @import("witnesses_cli.zig");
const shared = @import("cli_shared.zig");
const runtime_cli = @import("runtime_cli.zig");
const feature_options = @import("runtime_feature_options");
const embedded_handler = @import("embedded_handler");
const proof_ledger = @import("proof_ledger.zig");
const ratchet_command = @import("ratchet_command.zig");
const pi_app = @import("pi_app");
const verify_cli = @import("verify_cli.zig");
const cli_args = @import("cli_args.zig");
const cli_tour = @import("cli_tour.zig");
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
const hasHelpFlag = cli_args.hasHelpFlag;
const hasLongHelpFlag = cli_args.hasLongHelpFlag;
const deployArgsRequestCloud = cli_args.deployArgsRequestCloud;
const printNoProjectConfigDiagnostic = cli_args.printNoProjectConfigDiagnostic;
const handlePreflightError = cli_args.handlePreflightError;
const template_choices = cli_args.template_choices;
const demo_command = @import("demo_command.zig");
const test_command = @import("test_command.zig");
const init_command = @import("init_command.zig");
const build_command = @import("build_command.zig");
const dev_command = @import("dev_command.zig");
const cli_help = @import("cli_help.zig");

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
    _ = @import("dev_command.zig");
    _ = @import("cli_help.zig");
    // cli_auth and verify_cli are likewise only reached via main's dispatch;
    // reference them so their tests (API-key store 0600 perms/masking, and the
    // Ed25519 verifier arg parsing incl. trust-key validation) actually run.
    _ = @import("cli_auth.zig");
    _ = @import("verify_cli.zig");
}

/// Validate, configure, and launch the expert agent. Shared by the `expert`
/// command and by `init --expert` (which scaffolds, chdir's into the project,
/// then calls this with no extra args).
fn dispatchExpert(allocator: std.mem.Allocator, expert_args: []const []const u8) !void {
    if (hasHelpFlag(expert_args)) {
        cli_help.printExpertHelp();
        return;
    }
    switch (cli_args.validateExpertArgs(expert_args)) {
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
    _ = pi_app.parseExpertFlags(expert_args) catch |err| {
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
    pi_app.setInvocationArgv(expert_args);
    pi_app.witness_replay.setReplayFn(witness_replay_lib.replayWitnessJsonl);
    pi_app.perf_probe.setProbeFn(perf_probe_lib.recordPerfReceipt);
    pi_app.equivalence_probe.setProbeFn(equivalence_probe_lib.recordEquivalenceReceipt);
    pi_app.capsule_probe.setProbeFn(dev_command.capsuleReplayProbe);
    try pi_app.run(allocator);
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
        cli_help.printHelp();
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
        const outcome = init_command.initCommand(allocator, user_args[1..]) catch |err| {
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
        if (outcome.enter_expert) {
            if (outcome.project_name) |proj| {
                // Stored provider keys are injected for the `expert` command;
                // do the same before handing off from `init --expert`.
                cli_auth.injectStoredProvidersIntoEnv(allocator);
                std.Io.Threaded.chdir(proj) catch |e| {
                    std.debug.print("init --expert: could not enter '{s}': {s}\n", .{ proj, @errorName(e) });
                    std.process.exit(1);
                };
                return dispatchExpert(allocator, &.{});
            }
        }
        return;
    }
    if (std.mem.eql(u8, command, "dev")) {
        if (hasLongHelpFlag(user_args[1..])) {
            dev_command.printDevHelp();
            return;
        }
        dev_command.devCommand(allocator, args[0], user_args[1..]) catch |err| {
            if (handlePreflightError(err, command)) std.process.exit(1);
            return err;
        };
        return;
    }
    if (std.mem.eql(u8, command, "studio")) {
        if (hasLongHelpFlag(user_args[1..])) {
            dev_command.printStudioHelp();
            return;
        }
        if (!feature_options.enable_studio) shared.featureCompiledOut("studio", "studio");
        dev_command.studioCommand(allocator, args[0], user_args[1..]) catch |err| {
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
        // Map parse/usage failures to a clean nonzero exit with a hint instead of
        // propagating a raw Zig error (which prints a stack trace).
        runtime_cli.serveCommand(allocator, user_args[1..]) catch |err| {
            std.debug.print("serve: {s}. Run `zigttp serve --help` for usage.\n", .{@errorName(err)});
            std.process.exit(1);
        };
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
    // Delegate the full shared analyzer surface to the same code path `zigts`
    // uses. Membership lives in one place (`zigts_cli.commands`), so the two
    // binaries can never expose a different command set. `compile` is handled
    // separately below: it builds a binary here but precompiles to .zig in
    // `zigts`, so it is deliberately excluded from the shared registry.
    if (zigts_cli.isAnalyzerCommand(command)) {
        zigts_cli.run(allocator, user_args) catch |err| {
            if (err == error.NoProjectConfig) {
                printNoProjectConfigDiagnostic(command);
                std.process.exit(1);
            }
            // Usage errors (bad/unknown/missing flags or args) must not escape
            // as a raw Zig stack trace: IDE/CI cannot read those. Map them to a
            // clean one-line message and exit 1. Generic across every analyzer
            // command routed through this dispatch.
            if (err == error.InvalidArgument or
                err == error.InvalidArguments or
                err == error.MissingArgument or
                err == error.UnknownArgument or
                err == error.UnknownOption or
                err == error.UnknownFlag or
                err == error.TooManyArguments)
            {
                std.debug.print(
                    "zigttp {s}: invalid arguments ({s}). Run `zigttp {s} --help` for usage.\n",
                    .{ command, @errorName(err), command },
                );
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
    if (std.mem.eql(u8, command, "ledger")) {
        // Session ledger management (list, resume, export, replay). Lives only
        // in the developer CLI; the pi-free `zigts` analyzer binary does not
        // carry it.
        try pi_app.runLedgerCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "expert")) {
        return dispatchExpert(allocator, user_args[1..]);
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
        if (cli_help.hasAllFlag(user_args[1..])) cli_help.printHelpAll() else cli_help.printHelp();
        return;
    }

    std.debug.print("Unknown command: {s}\n\n", .{command});
    cli_help.printHelp();
    std.process.exit(1);
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
    const opts = try cli_release_check.parseReleaseDoctorOptions(&.{ "--json", "--out", ".zigttp/release-passport.json" });
    try std.testing.expect(opts.json);
    try std.testing.expectEqualStrings(".zigttp/release-passport.json", opts.out_path.?);
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
        \\function handler(req: Request): Response & Spec<"state_isolated"> {
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
    try tmp.dir.createDirPath(io, "docs/virtual-modules");
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
        .sub_path = "docs/README.md",
        .data =
        \\# Documentation
        \\Current docs use one user guide and one roadmap.
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "docs/user-guide.md",
        .data =
        \\# User Guide
        \\Current user flow.
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "docs/roadmap.md",
        .data =
        \\# Roadmap
        \\Current support boundary.
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "docs/virtual-modules/README.md",
        .data =
        \\# Virtual Modules
        \\| Module | Exports | Capabilities |
        \\|---|---|---|
        \\| `zigttp:env` | `env` | `env`, `policy_check` |
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "README.md",
        .data = if (opts.stale_readme)
            "Numbers: 3ms runtime init. 1.2MB binary. 4MB memory baseline.\n"
        else
            "Numbers: cold-start floor 3.5 ms, typical 7-15 ms, RSS 13 MB, throughput 112k req/s.\n",
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "docs/performance.md",
        .data = "Performance: 3.5 ms floor, 7-15 ms typical, 13 MB RSS, 112k req/s.\n",
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
