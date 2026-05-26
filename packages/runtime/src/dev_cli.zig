const std = @import("std");
const builtin = @import("builtin");

const project_config_mod = @import("project_config");
const zigts = @import("zigts");
const self_extract = @import("self_extract.zig");
const zigts_cli = @import("zigts_cli");
const proofs_cli = @import("proofs_cli.zig");
const witnesses_cli = @import("witnesses_cli.zig");
const intent_runner = @import("intent_runner.zig");
const precompile = zigts_cli.precompile;
const shared = @import("cli_shared.zig");
const runtime_cli = @import("runtime_cli.zig");
const embedded_handler = @import("embedded_handler");
const proof_ledger = @import("proof_ledger.zig");
const ratchet_command = @import("ratchet_command.zig");
// Hosted cloud deploy is deferred from v0.1.0-beta: the CLI entry points are
// gated in `main` (see `rejectCloudCommand`). The control-plane module stays
// imported so it keeps compiling, ready to re-enable in a later release.
const deploy = @import("deploy.zig");
const live_reload = @import("live_reload.zig");
const demo = @import("demo.zig");
const pi_app = @import("pi_app");
const attest_build_receipt = @import("attest/build_receipt.zig");
const verify_cli = @import("verify_cli.zig");
const cli_args = @import("cli_args.zig");
const cli_tour = @import("cli_tour.zig");
const maybeShowFirstRunTour = cli_tour.maybeShowFirstRunTour;
const cli_release_check = @import("cli_release_check.zig");
const cli_paths = @import("cli_paths.zig");
const resolveDeveloperServeBinary = cli_paths.resolveDeveloperServeBinary;
const resolveReentryBinaryAfterChdir = cli_paths.resolveReentryBinaryAfterChdir;
const resolveRuntimeBinary = cli_paths.resolveRuntimeBinary;
const hasHelpFlag = cli_args.hasHelpFlag;
const hasLongHelpFlag = cli_args.hasLongHelpFlag;
const containsString = cli_args.containsString;
const deployArgsRequestCloud = cli_args.deployArgsRequestCloud;
const rejectCloudCommand = cli_args.rejectCloudCommand;
const printNoProjectConfigDiagnostic = cli_args.printNoProjectConfigDiagnostic;
const handlePreflightError = cli_args.handlePreflightError;
const cloud_only_deploy_flags = cli_args.cloud_only_deploy_flags;
const template_choices = cli_args.template_choices;

/// Slice 1 placeholder. Replace with a build-injected constant (short git sha
/// plus stable tag) when the `build.zig` wiring lands later in slice 1.
/// Legacy alias. Slice 1 introduced `--attest` as an opt-in flag; slice 2
/// flipped attestation default-on, so the spelling now warns once and is a
/// no-op. `--no-attest` is the explicit opt-out.
const attest_flag_legacy: []const u8 = "--attest";
const no_attest_flag: []const u8 = "--no-attest";

var attest_legacy_warned: bool = false;

/// Shared help text describing `--no-attest`. Three command help printers
/// all advertise the same flag; one source of truth prevents drift.
const no_attest_help_block: []const u8 =
    \\  --no-attest           Skip proof-receipt signing for this build.
    \\                        Default is to sign with the persistent
    \\                        identity at ~/.zigttp/attest/keypair.bin.
    \\
;

fn warnAttestLegacyOnce() void {
    if (attest_legacy_warned) return;
    attest_legacy_warned = true;
    const msg = "note: --attest is now the default; drop the flag or pass --no-attest to opt out.\n";
    _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
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

    if (std.mem.eql(u8, command, "init")) {
        initCommand(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                printInitHelp();
                return;
            }
            if (err == error.MissingProjectName) {
                std.debug.print("zigttp init requires a project name.\n\n", .{});
                printInitHelp();
                std.process.exit(1);
            }
            if (err == error.MissingTemplate) {
                std.debug.print("--template requires one of: " ++ template_choices ++ ".\n\n", .{});
                printInitHelp();
                std.process.exit(1);
            }
            if (err == error.InvalidTemplate) {
                std.debug.print("Unknown template. Choose one of: " ++ template_choices ++ ".\n\n", .{});
                printInitHelp();
                std.process.exit(1);
            }
            if (err == error.InvalidProjectName) {
                std.debug.print("Invalid project name. Use letters, numbers, '-' or '_', starting with a letter or number.\n\n", .{});
                printInitHelp();
                std.process.exit(1);
            }
            if (err == error.MissingExtensionName) {
                std.debug.print("zigttp init --extension requires a name.\n\n", .{});
                printInitHelp();
                std.process.exit(1);
            }
            if (err == error.InvalidArgument or err == error.UnknownOption) {
                std.debug.print("Invalid init arguments.\n\n", .{});
                printInitHelp();
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
        demoCommand(allocator, args[0], user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                printDemoHelp();
                return;
            }
            if (err == error.MissingOptionValue) {
                std.debug.print("demo option requires a value.\n\n", .{});
                printDemoHelp();
                std.process.exit(1);
            }
            if (err == error.InvalidPort) {
                std.debug.print("--port requires a number from 1 to 65535.\n\n", .{});
                printDemoHelp();
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
                printDemoHelp();
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
        testCommand(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                printTestHelp();
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
                printTestHelp();
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
        if (user_args.len > 1) {
            std.debug.print("zigttp expert does not accept arguments.\n\n", .{});
            printExpertHelp();
            return;
        }
        if (!pi_app.envHasModelBackend()) {
            std.debug.print(
                \\zigttp expert needs a model backend.
                \\
                \\Set one of these environment variables, then run `zigttp expert` again:
                \\  ANTHROPIC_API_KEY   (recommended)  https://console.anthropic.com/
                \\  OPENAI_API_KEY
                \\
                \\See `zigttp expert --help` for details.
                \\
            , .{});
            std.process.exit(1);
        }
        const witness_replay_lib = @import("witness_replay_lib.zig");
        pi_app.witness_replay.setReplayFn(witness_replay_lib.replayWitnessJsonl);
        try pi_app.run(allocator);
        return;
    }
    if (std.mem.eql(u8, command, "login")) rejectCloudCommand("login");
    if (std.mem.eql(u8, command, "deploy")) {
        if (deployArgsRequestCloud(user_args[1..])) |flag| {
            std.debug.print(
                "zigttp deploy with `{s}` selects hosted cloud deploy, which is not available in v0.1.0-beta.\n" ++
                    "Run `zigttp deploy` without the flag to build a self-contained binary you can run anywhere.\n",
                .{flag},
            );
            std.process.exit(1);
        }
        localDeployCommand(allocator, user_args[1..]) catch |err| {
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
    if (std.mem.eql(u8, command, "review")) rejectCloudCommand("review");
    if (std.mem.eql(u8, command, "grants")) rejectCloudCommand("grants");
    if (std.mem.eql(u8, command, "revoke-grant")) rejectCloudCommand("revoke-grant");
    if (std.mem.eql(u8, command, "logout")) rejectCloudCommand("logout");
    if (std.mem.eql(u8, command, "proofs")) {
        // Expected user-input errors are explained on stderr by proofs_cli
        // itself; only unexpected ones (allocator, etc.) bubble.
        proofs_cli.run(allocator, user_args[1..]) catch |err| {
            if (proofs_cli.isExpectedUserError(err)) std.process.exit(1);
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
    if (std.mem.eql(u8, command, "assert-intent")) {
        try intent_runner.runWithArgs(allocator, user_args[1..]);
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

fn initCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len == 0) return error.MissingProjectName;

    var template_name: []const u8 = "basic";
    var project_name: ?[]const u8 = null;
    var extension_name: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return error.HelpRequested;
        }
        if (std.mem.eql(u8, arg, "--template")) {
            i += 1;
            if (i >= argv.len) return error.MissingTemplate;
            template_name = argv[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "--extension")) {
            i += 1;
            if (i >= argv.len) return error.MissingExtensionName;
            extension_name = argv[i];
            continue;
        }
        if (std.mem.startsWith(u8, arg, "-")) {
            return error.UnknownOption;
        }
        if (project_name == null) {
            project_name = arg;
            continue;
        }
        return error.InvalidArgument;
    }

    if (extension_name) |name| {
        try validateProjectName(name);
        try scaffoldExtension(allocator, name);
        printInitExtensionNextSteps(name);
        return;
    }

    const name = project_name orelse return error.MissingProjectName;
    try validateProjectName(name);
    const template = parseTemplate(template_name) orelse return error.InvalidTemplate;

    try scaffoldProject(allocator, name, template);
    printInitNextSteps(name, template);
}

fn validateProjectName(name: []const u8) !void {
    if (name.len == 0) return error.InvalidProjectName;
    if (std.fs.path.isAbsolute(name)) return error.InvalidProjectName;

    for (name, 0..) |c, i| {
        const is_letter = (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
        const is_digit = c >= '0' and c <= '9';
        const is_separator = c == '-' or c == '_';

        if (i == 0 and !is_letter and !is_digit) return error.InvalidProjectName;
        if (!is_letter and !is_digit and !is_separator) return error.InvalidProjectName;
    }
}

fn scaffoldProject(allocator: std.mem.Allocator, name: []const u8, template: Template) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    // Refuse to scaffold over an existing project. `zigttp.json` is the
    // marker; any other contents in the directory are left alone.
    const manifest_path = try std.fmt.allocPrint(allocator, "{s}/zigttp.json", .{name});
    defer allocator.free(manifest_path);
    if (std.Io.Dir.access(std.Io.Dir.cwd(), io, manifest_path, .{})) {
        std.debug.print(
            "error: '{s}/zigttp.json' already exists. Pick a different name or remove the existing project.\n",
            .{name},
        );
        std.process.exit(1);
    } else |_| {}

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
    try writeProjectFile(allocator, name, handlerPathForTemplate(template), switch (template) {
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
}

fn scaffoldExtension(allocator: std.mem.Allocator, name: []const u8) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const marker_path = try std.fmt.allocPrint(allocator, "{s}/zigttp-module.json", .{name});
    defer allocator.free(marker_path);
    if (std.Io.Dir.access(std.Io.Dir.cwd(), io, marker_path, .{})) {
        std.debug.print(
            "error: '{s}/zigttp-module.json' already exists. Pick a different name or remove the existing extension.\n",
            .{name},
        );
        std.process.exit(1);
    } else |_| {}

    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, name);
    const src_dir = try std.fmt.allocPrint(allocator, "{s}/src", .{name});
    defer allocator.free(src_dir);
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io, src_dir);

    const manifest = try renderExtensionTemplate(allocator, io, extension_manifest_template, name);
    defer allocator.free(manifest);
    try writeProjectFile(allocator, name, "zigttp-module.json", manifest);

    const root_zig = try renderExtensionTemplate(allocator, io, extension_root_zig_template, name);
    defer allocator.free(root_zig);
    try writeProjectFile(allocator, name, "src/root.zig", root_zig);

    const build_zig = try renderExtensionTemplate(allocator, io, extension_build_zig_template, name);
    defer allocator.free(build_zig);
    try writeProjectFile(allocator, name, "build.zig", build_zig);

    const build_zon = try renderExtensionTemplate(allocator, io, extension_build_zon_template, name);
    defer allocator.free(build_zon);
    try writeProjectFile(allocator, name, "build.zig.zon", build_zon);

    const handler_ts = try renderExtensionTemplate(allocator, io, extension_handler_ts_template, name);
    defer allocator.free(handler_ts);
    try writeProjectFile(allocator, name, "handler.ts", handler_ts);

    const readme = try renderExtensionTemplate(allocator, io, extension_readme_template, name);
    defer allocator.free(readme);
    try writeProjectFile(allocator, name, "README.md", readme);

    try writeProjectFile(allocator, name, ".gitignore", gitignoreSource);
}

/// Substitute the placeholder `{{name}}` with the supplied extension name.
/// Caller owns the returned slice.
fn renderExtensionTemplate(
    allocator: std.mem.Allocator,
    io: std.Io,
    template: []const u8,
    name: []const u8,
) ![]u8 {
    const package_name = try extensionPackageName(allocator, name);
    defer allocator.free(package_name);
    const fingerprint = extensionPackageFingerprint(io, package_name);
    const fingerprint_text = try std.fmt.allocPrint(allocator, "0x{x}", .{fingerprint});
    defer allocator.free(fingerprint_text);

    const named = try std.mem.replaceOwned(u8, allocator, template, "{{name}}", name);
    defer allocator.free(named);
    const packaged = try std.mem.replaceOwned(u8, allocator, named, "{{package_name}}", package_name);
    defer allocator.free(packaged);
    return std.mem.replaceOwned(u8, allocator, packaged, "{{fingerprint}}", fingerprint_text);
}

fn extensionPackageName(allocator: std.mem.Allocator, name: []const u8) ![]u8 {
    const prefix = "zigttp_ext_";
    const max_package_name_len = 32;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);

    try out.appendSlice(allocator, prefix);
    for (name) |c| {
        try out.append(allocator, normalizeExtensionPackageChar(c));
    }
    if (out.items.len <= max_package_name_len) return out.toOwnedSlice(allocator);

    out.clearRetainingCapacity();
    try out.appendSlice(allocator, prefix);

    const suffix_len = 9; // "_" plus eight lowercase CRC32 hex digits.
    const base_budget = max_package_name_len - prefix.len - suffix_len;
    for (name[0..@min(name.len, base_budget)]) |c| {
        try out.append(allocator, normalizeExtensionPackageChar(c));
    }

    var checksum_bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &checksum_bytes, std.hash.Crc32.hash(name), .big);
    const checksum_hex = std.fmt.bytesToHex(checksum_bytes, .lower);
    try out.append(allocator, '_');
    try out.appendSlice(allocator, &checksum_hex);

    return out.toOwnedSlice(allocator);
}

fn normalizeExtensionPackageChar(c: u8) u8 {
    return switch (c) {
        '-' => '_',
        'A'...'Z' => std.ascii.toLower(c),
        else => c,
    };
}

fn extensionPackageFingerprint(io: std.Io, package_name: []const u8) u64 {
    var id: u32 = undefined;
    io.randomSecure(std.mem.asBytes(&id)) catch io.random(std.mem.asBytes(&id));
    id = 1 + (id % (std.math.maxInt(u32) - 1));
    const checksum = std.hash.Crc32.hash(package_name);
    return (@as(u64, checksum) << 32) | id;
}

fn printInitExtensionNextSteps(name: []const u8) void {
    std.debug.print("Initialized zigttp extension in {s}\n", .{name});
    std.debug.print("Next steps:\n", .{});
    std.debug.print("  cd {s}\n", .{name});
    std.debug.print("  zigts verify-module-manifest zigttp-module.json\n", .{});
    std.debug.print("  zigts extension-status --module-manifest zigttp-module.json\n", .{});
    std.debug.print("  # adjust build.zig.zon to point at the version of zigttp-sdk you depend on,\n", .{});
    std.debug.print("  # then `zig build` to compile the native binding.\n", .{});
}

/// Print the post-init welcome panel. `zigttp dev` owns the first-run tour so
/// the tour marker is written inside the project on the first real dev run.
fn printInitNextSteps(name: []const u8, template: Template) void {
    const tty = shared.stderrIsTty();
    const c = shared.palette(tty);

    std.debug.print("\n", .{});
    std.debug.print("  {s}+{s} initialized zigttp project in {s}{s}{s}\n", .{ c.green, c.reset, c.bold, name, c.reset });
    std.debug.print("  template: {s}\n", .{@tagName(template)});
    std.debug.print("  {s}----------------------------------------------------------------------{s}\n", .{ c.dim, c.reset });
    std.debug.print("\n", .{});
    std.debug.print("  created:\n", .{});
    std.debug.print("    zigttp.json\n", .{});
    std.debug.print("    {s}\n", .{handlerPathForTemplate(template)});
    std.debug.print("    tests/handler.test.jsonl\n", .{});
    std.debug.print("    public/.keep\n", .{});
    std.debug.print("    README.md\n", .{});
    std.debug.print("    .gitignore\n", .{});
    std.debug.print("\n", .{});
    std.debug.print("  {s}edit your handler. watch the proof flip live.{s}\n", .{ c.bold, c.reset });
    std.debug.print("\n", .{});
    std.debug.print("  try it:\n", .{});
    std.debug.print("\n", .{});
    std.debug.print("    cd {s}\n", .{name});
    std.debug.print("    {s}zigttp dev{s}          {s}# watch, prove, and start the Proof Quest{s}\n", .{ c.cyan, c.reset, c.dim, c.reset });
    std.debug.print("    curl http://127.0.0.1:3000{s}\n", .{starterPathForTemplate(template)});
    std.debug.print("\n", .{});
    std.debug.print("    {s}zigttp test{s}         {s}# run tests/handler.test.jsonl{s}\n", .{ c.cyan, c.reset, c.dim, c.reset });
    std.debug.print("    {s}zigttp deploy{s}       {s}# build, prove, and deploy to .zigttp/deploy/{s}{s}\n", .{ c.cyan, c.reset, c.dim, name, c.reset });
    std.debug.print("\n", .{});
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
    const default_quest = !user_no_tour and !user_has_quest and shared.stderrIsTty() and !cli_tour.tourMarkerExists(allocator);

    var child_args = std.ArrayList([]const u8).empty;
    defer child_args.deinit(allocator);
    try child_args.append(allocator, serve_binary);
    try child_args.append(allocator, "serve");
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--no-prove")) continue;
        if (std.mem.eql(u8, arg, "--no-tour")) continue;
        try child_args.append(allocator, arg);
    }
    if (!user_has_watch) try child_args.append(allocator, "--watch");
    if (!user_has_prove and !user_no_prove) try child_args.append(allocator, "--prove");
    if (default_quest and !user_no_prove) try child_args.append(allocator, "--quest-default");

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

const DemoArgs = struct {
    no_open: bool = false,
    port: u16 = 3000,
    out_dir: ?[]const u8 = null,
};

fn parseDemoArgs(argv: []const []const u8) !DemoArgs {
    var parsed = DemoArgs{};
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) return error.HelpRequested;
        if (std.mem.eql(u8, arg, "--no-open")) {
            parsed.no_open = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--port")) {
            i += 1;
            if (i >= argv.len) return error.MissingOptionValue;
            parsed.port = std.fmt.parseInt(u16, argv[i], 10) catch return error.InvalidPort;
            if (parsed.port == 0) return error.InvalidPort;
            continue;
        }
        if (std.mem.eql(u8, arg, "--out")) {
            i += 1;
            if (i >= argv.len) return error.MissingOptionValue;
            parsed.out_dir = argv[i];
            continue;
        }
        return error.UnknownOption;
    }
    return parsed;
}

fn demoCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
    const parsed = try parseDemoArgs(argv);

    var workspace = try demo.createWorkspace(allocator, parsed.out_dir, parsed.port);
    defer workspace.deinit(allocator);
    defer workspace.cleanup(allocator);
    var passport = try pi_app.demo_passport.resetToBaseline(allocator, workspace.root);
    defer passport.deinit(allocator);

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const old_cwd = try std.process.currentPathAlloc(io, allocator);
    defer allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const serve_binary = try resolveDeveloperServeBinary(allocator, program_path);
    defer allocator.free(serve_binary);
    const serve_binary_for_workspace = try resolveReentryBinaryAfterChdir(allocator, serve_binary, old_cwd);
    defer allocator.free(serve_binary_for_workspace);

    const url = try std.fmt.allocPrint(
        allocator,
        "http://127.0.0.1:{d}/_zigttp/studio",
        .{parsed.port},
    );
    defer allocator.free(url);
    const verify_url = try std.fmt.allocPrint(allocator, "http://127.0.0.1:{d}/", .{parsed.port});
    defer allocator.free(verify_url);
    const well_known_url = try std.fmt.allocPrint(allocator, "http://127.0.0.1:{d}/.well-known/zigttp-attest", .{parsed.port});
    defer allocator.free(well_known_url);

    std.debug.print(
        \\
        \\zigttp proof theater
        \\Workspace: {s}
        \\Studio:    {s}
        \\TUI:       {s}
        \\Verify:    zigttp verify {s}
        \\Well-known:{s}
        \\
        \\Flow: baseline -> unsafe edit -> witness -> repair -> local deploy receipt
        \\
    , .{ workspace.root, url, passport.tui_command, verify_url, well_known_url });

    if (!parsed.no_open) openBrowser(allocator, url);

    var child_args: std.ArrayList([]const u8) = .empty;
    defer child_args.deinit(allocator);
    try child_args.append(allocator, serve_binary_for_workspace);
    try child_args.append(allocator, "serve");
    try child_args.append(allocator, "--studio");
    try child_args.append(allocator, "--watch");
    try child_args.append(allocator, "--prove");
    try child_args.append(allocator, "--demo");
    try child_args.append(allocator, "--port");
    const port_text = try std.fmt.allocPrint(allocator, "{d}", .{parsed.port});
    defer allocator.free(port_text);
    try child_args.append(allocator, port_text);

    try std.Io.Threaded.chdir(workspace.root);

    var child = try std.process.spawn(io, .{
        .argv = child_args.items,
        .stdin = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
    });
    _ = child.wait(io) catch {};
}

fn openBrowser(allocator: std.mem.Allocator, url: []const u8) void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const argv: []const []const u8 = switch (builtin.os.tag) {
        .macos => &.{ "/usr/bin/open", url },
        .linux => &.{ "xdg-open", url },
        else => return,
    };
    var child = std.process.spawn(io, .{
        .argv = argv,
        .stdin = .ignore,
        .stdout = .ignore,
        .stderr = .ignore,
    }) catch return;
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

    try scaffoldProject(allocator, ".", template);
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

fn doctorCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len > 0 and std.mem.eql(u8, argv[0], "--release")) {
        try cli_release_check.releaseDoctorCommand(allocator, argv[1..], printDoctorHelp);
        return;
    }

    if (argv.len > 1) {
        std.debug.print("zigttp doctor accepts at most one path.\n\n", .{});
        printDoctorHelp();
        return error.InvalidArgument;
    }

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const start_path = if (argv.len > 0) argv[0] else null;
    var project = try project_config_mod.discover(allocator, io, start_path);
    defer if (project) |*p| p.deinit(allocator);

    if (project) |*cfg| {
        var failures: usize = 0;

        std.debug.print("zigttp doctor\n", .{});
        std.debug.print("Project root: {s}\n\n", .{cfg.root_dir});

        printDoctorOk("manifest", cfg.manifest_path);
        printDoctorPlatform();
        printDoctorRuntimeTemplate(allocator);

        const entry = try cfg.resolvedEntry(allocator);
        defer allocator.free(entry);
        const entry_ok = doctorPathExists(io, entry);
        if (!entry_ok) failures += 1;
        printDoctorPath("entry", entry, entry_ok);

        const sqlite_path = try cfg.resolvedSqlitePath(allocator);
        defer if (sqlite_path) |path| allocator.free(path);

        if (entry_ok) {
            var check = runDoctorAnalyzerForProject(allocator, cfg, entry, sqlite_path) catch |err| {
                failures += 1;
                printDoctorAnalyzerError(err);
                return error.CheckFailed;
            };
            defer check.deinit(allocator);
            if (check.totalErrors() > 0) {
                failures += 1;
                printDoctorCheckFailure(&check);
            } else {
                std.debug.print("[ok]   check    handler passes analyzer\n", .{});
            }
        } else {
            printDoctorSkip("check", "entry missing");
        }

        if (try cfg.resolvedStaticDir(allocator)) |static_dir| {
            defer allocator.free(static_dir);
            const ok = doctorPathExists(io, static_dir);
            if (!ok) failures += 1;
            printDoctorPath("static", static_dir, ok);
        } else {
            printDoctorSkip("static", "not configured");
        }

        if (sqlite_path) |path| {
            std.debug.print("[info] sqlite   {s}\n", .{path});
        } else {
            printDoctorSkip("sqlite", "not configured");
        }

        if (try cfg.resolvedDurableDir(allocator)) |durable_dir| {
            defer allocator.free(durable_dir);
            std.debug.print("[info] durable  {s}\n", .{durable_dir});
        } else {
            printDoctorSkip("durable", "not configured");
        }

        if (try cfg.resolvedSystemPath(allocator)) |system_path| {
            defer allocator.free(system_path);
            const ok = doctorPathExists(io, system_path);
            if (!ok) failures += 1;
            printDoctorPath("system", system_path, ok);
        } else {
            printDoctorSkip("system", "not configured");
        }

        if (cfg.outbound_hosts.len > 1) {
            std.debug.print("[fail] outbound multiple outboundHosts are configured; current runtime accepts one\n", .{});
            return error.UnsupportedMultipleOutboundHosts;
        }
        if (cfg.outbound_http) {
            if (cfg.outbound_hosts.len == 1) {
                std.debug.print("[ok]   outbound host allowlist: {s}\n", .{cfg.outbound_hosts[0]});
            } else {
                std.debug.print("[warn] outbound enabled without host allowlist\n", .{});
            }
        } else {
            printDoctorSkip("outbound", "not enabled");
        }

        const tests_path = try std.fs.path.resolve(allocator, &.{ cfg.root_dir, "tests", "handler.test.jsonl" });
        defer allocator.free(tests_path);
        printDoctorOptionalPath("tests", tests_path, doctorPathExists(io, tests_path));

        std.debug.print("\n", .{});
        if (failures > 0) {
            std.debug.print("Doctor: {d} required check{s} failed\n", .{ failures, if (failures == 1) @as([]const u8, "") else "s" });
            std.debug.print("Next: fix the failed row above, then run `zigttp doctor` again.\n", .{});
            return error.DoctorFailed;
        }
        std.debug.print("Doctor: OK\n", .{});
        std.debug.print("Next: zigttp dev\n", .{});
        return;
    }

    if (start_path) |path| {
        std.debug.print("No zigttp.json found. Treating '{s}' as ad hoc source.\n", .{path});
        std.Io.Dir.access(std.Io.Dir.cwd(), io, path, .{}) catch |err| {
            std.debug.print("[fail] source   cannot read {s}: {}\n", .{ path, err });
            std.debug.print("Next: pass a readable handler path or run inside a project with zigttp.json.\n", .{});
            return error.FileNotFound;
        };
        std.debug.print("Doctor: OK\n", .{});
        return;
    }

    return error.NoProjectConfig;
}

fn runDoctorAnalyzerForProject(
    allocator: std.mem.Allocator,
    cfg: *const project_config_mod.ProjectConfig,
    entry: []const u8,
    sqlite_path: ?[]const u8,
) !precompile.CheckResult {
    const system_for_check = try cfg.resolvedSystemPath(allocator);
    defer if (system_for_check) |path| allocator.free(path);
    return try precompile.runCheckOnly(allocator, entry, sqlite_path, false, system_for_check);
}

fn printDoctorAnalyzerError(err: anyerror) void {
    std.debug.print("[fail] check    handler analyzer could not run: {}\n", .{err});
    std.debug.print("       next     run `zigttp check` for full diagnostics after fixing the project paths\n", .{});
}

fn printDoctorCheckFailure(check: *const precompile.CheckResult) void {
    std.debug.print("[fail] check    handler analyzer found {d} error(s)\n", .{check.totalErrors()});
    printCheckStageFailures(check, "       ");
    std.debug.print("       next     run `zigttp check` for full diagnostics\n", .{});
}

fn printCheckStageFailures(check: *const precompile.CheckResult, prefix: []const u8) void {
    if (check.parse_errors > 0) std.debug.print("{s}parse    {d} error(s)\n", .{ prefix, check.parse_errors });
    if (check.bool_errors > 0) std.debug.print("{s}sound    {d} error(s)\n", .{ prefix, check.bool_errors });
    if (check.type_errors > 0) std.debug.print("{s}types    {d} error(s)\n", .{ prefix, check.type_errors });
    if (check.strict_errors > 0) std.debug.print("{s}strict   {d} error(s)\n", .{ prefix, check.strict_errors });
    if (check.verify_errors > 0) std.debug.print("{s}verify   {d} error(s)\n", .{ prefix, check.verify_errors });
    if (check.flow_errors > 0) std.debug.print("{s}flow     {d} error(s)\n", .{ prefix, check.flow_errors });
    const spec_errors = check.totalErrors() -|
        (check.parse_errors + check.bool_errors + check.type_errors + check.strict_errors + check.verify_errors + check.flow_errors);
    if (spec_errors > 0) std.debug.print("{s}spec     {d} error(s)\n", .{ prefix, spec_errors });
}

fn printDoctorRuntimeTemplate(allocator: std.mem.Allocator) void {
    const self_path = self_extract.getSelfExePath(allocator) catch {
        printDoctorSkip("runtime", "could not locate current executable");
        return;
    };
    defer allocator.free(self_path);

    const runtime_path = resolveRuntimeBinary(allocator, self_path) catch {
        printDoctorSkip("runtime", "zigttp-runtime not found beside zigttp");
        return;
    };
    defer allocator.free(runtime_path);

    if (std.mem.endsWith(u8, runtime_path, "zigttp-runtime")) {
        printDoctorOk("runtime", runtime_path);
    } else {
        std.debug.print("[warn] runtime  using fallback template: {s}\n", .{runtime_path});
    }
}

fn printDoctorPlatform() void {
    const os = builtin.os.tag;
    const arch = builtin.cpu.arch;
    const supported = (os == .macos or os == .linux) and
        (arch == .x86_64 or arch == .aarch64);
    if (supported) {
        std.debug.print("[ok]   {s:<8} {s}-{s}\n", .{ "platform", @tagName(os), @tagName(arch) });
    } else {
        std.debug.print(
            "[warn] platform {s}-{s} is not an officially supported target (macOS/Linux, x86-64/ARM64)\n",
            .{ @tagName(os), @tagName(arch) },
        );
    }
}

fn doctorPathExists(io: std.Io, path: []const u8) bool {
    if (std.fs.path.isAbsolute(path)) {
        std.Io.Dir.accessAbsolute(io, path, .{}) catch return false;
    } else {
        std.Io.Dir.access(std.Io.Dir.cwd(), io, path, .{}) catch return false;
    }
    return true;
}

fn printDoctorOk(label: []const u8, detail: []const u8) void {
    std.debug.print("[ok]   {s:<8} {s}\n", .{ label, detail });
}

fn printDoctorSkip(label: []const u8, reason: []const u8) void {
    std.debug.print("[skip] {s:<8} {s}\n", .{ label, reason });
}

fn printDoctorPath(label: []const u8, path: []const u8, ok: bool) void {
    if (ok) {
        printDoctorOk(label, path);
    } else {
        std.debug.print("[fail] {s:<8} missing: {s}\n", .{ label, path });
    }
}

fn printDoctorOptionalPath(label: []const u8, path: []const u8, ok: bool) void {
    if (ok) {
        printDoctorOk(label, path);
    } else {
        std.debug.print("[warn] {s:<8} missing: {s}\n", .{ label, path });
    }
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

fn testCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var explicit_test_path: ?[]const u8 = null;

    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return error.HelpRequested;
        }
        if (std.mem.startsWith(u8, arg, "-")) return error.UnknownOption;
        if (explicit_test_path != null) return error.TooManyArguments;
        explicit_test_path = arg;
    }

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var project = try project_config_mod.discover(allocator, io, null);
    defer if (project) |*p| p.deinit(allocator);
    const cfg = if (project) |*p| p else return error.NoProjectConfig;
    if (cfg.outbound_hosts.len > 1) {
        std.debug.print("zigttp test cannot run with multiple outboundHosts; current runtime accepts one.\n", .{});
        return error.UnsupportedMultipleOutboundHosts;
    }

    const test_path = if (explicit_test_path) |path|
        try allocator.dupe(u8, path)
    else
        try std.fs.path.resolve(allocator, &.{ cfg.root_dir, "tests", "handler.test.jsonl" });
    defer allocator.free(test_path);

    if (!doctorPathExists(io, test_path)) {
        std.debug.print("Test fixture not found: {s}\n", .{test_path});
        std.debug.print("Run `zigttp gen-tests` or create tests/handler.test.jsonl.\n", .{});
        return error.FileNotFound;
    }

    const entry = try cfg.resolvedEntry(allocator);
    defer allocator.free(entry);
    if (!doctorPathExists(io, entry)) {
        std.debug.print("Handler not found: {s}\n", .{entry});
        std.debug.print("Next: update `entry` in zigttp.json or create the handler file.\n", .{});
        return error.FileNotFound;
    }

    const sqlite_path = try cfg.resolvedSqlitePath(allocator);
    defer if (sqlite_path) |path| allocator.free(path);
    var check = runDoctorAnalyzerForProject(allocator, cfg, entry, sqlite_path) catch |err| {
        if (!builtin.is_test) {
            std.debug.print("Pre-test check could not run: {}\n", .{err});
            std.debug.print("Next: run `zigttp check` for full diagnostics.\n", .{});
        }
        return error.CheckFailed;
    };
    defer check.deinit(allocator);
    if (check.totalErrors() > 0) {
        if (!builtin.is_test) {
            std.debug.print("Pre-test check failed for {s}: {d} error(s)\n", .{ entry, check.totalErrors() });
            printCheckStageFailures(&check, "  ");
            std.debug.print("Next: fix the errors above, then rerun `zigttp test`.\n", .{});
        }
        return error.CheckFailed;
    }

    var serve_args = [_][]const u8{ "--test", test_path };
    var serve_arena: std.heap.ArenaAllocator = .init(allocator);
    defer serve_arena.deinit();
    try runtime_cli.serveCommand(serve_arena.allocator(), &serve_args);
}

const CompileCommandOptions = struct {
    handler_path: []const u8,
    output_path: []const u8,
    attest_requested: bool,
    legacy_attest_seen: bool,
};

const BuildCommandOptions = struct {
    output_override: ?[]const u8,
    attest_requested: bool,
    legacy_attest_seen: bool,
};

const CommandArgError = union(enum) {
    missing_output_value,
    missing_handler_path,
    missing_output_flag,
    unknown_arg: []const u8,
};

const CommandArgFailure = struct {
    err: CommandArgError,
    legacy_attest_seen: bool,
};

const CompileCommandParse = union(enum) {
    help: bool,
    ok: CompileCommandOptions,
    err: CommandArgFailure,
};

const BuildCommandParse = union(enum) {
    help: bool,
    ok: BuildCommandOptions,
    err: CommandArgFailure,
};

fn parseCompileCommandArgs(argv: []const []const u8) CompileCommandParse {
    var handler_path: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;
    var attest_requested = true;
    var legacy_attest_seen = false;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            i += 1;
            if (i >= argv.len) {
                return .{ .err = .{ .err = .missing_output_value, .legacy_attest_seen = legacy_attest_seen } };
            }
            output_path = argv[i];
        } else if (std.mem.eql(u8, arg, attest_flag_legacy)) {
            // Kept as a no-op for parsing. The command emits the legacy warning
            // after parse succeeds, so parse-only tests stay side-effect free.
            legacy_attest_seen = true;
        } else if (std.mem.eql(u8, arg, no_attest_flag)) {
            attest_requested = false;
        } else if (std.mem.eql(u8, arg, "--help")) {
            return .{ .help = legacy_attest_seen };
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            handler_path = arg;
        } else {
            // Reject unknown `-`-prefixed flags rather than silently
            // dropping them. Symmetric with buildCommand at :2106-2110.
            // Without this branch, a typo like `--ouptut` was swallowed
            // and the build proceeded against the wrong (or default)
            // state with no diagnostic.
            return .{ .err = .{ .err = .{ .unknown_arg = arg }, .legacy_attest_seen = legacy_attest_seen } };
        }
    }

    if (handler_path == null) {
        return .{ .err = .{ .err = .missing_handler_path, .legacy_attest_seen = legacy_attest_seen } };
    }
    if (output_path == null) {
        return .{ .err = .{ .err = .missing_output_flag, .legacy_attest_seen = legacy_attest_seen } };
    }

    return .{ .ok = .{
        .handler_path = handler_path.?,
        .output_path = output_path.?,
        .attest_requested = attest_requested,
        .legacy_attest_seen = legacy_attest_seen,
    } };
}

fn failCompileCommandArgs(err: CommandArgError) !void {
    switch (err) {
        .missing_output_value => {
            std.log.err("-o requires an output path", .{});
            return error.MissingArgument;
        },
        .missing_handler_path => {
            std.log.err("handler file path required", .{});
            printCompileHelp();
            return error.MissingArgument;
        },
        .missing_output_flag => {
            std.log.err("-o <output> required", .{});
            printCompileHelp();
            return error.MissingArgument;
        },
        .unknown_arg => |arg| {
            std.log.err("Unknown argument: {s}", .{arg});
            printCompileHelp();
            return error.UnknownOption;
        },
    }
}

fn compileCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const opts = switch (parseCompileCommandArgs(argv)) {
        .help => |legacy_attest_seen| {
            if (legacy_attest_seen) warnAttestLegacyOnce();
            printCompileHelp();
            return;
        },
        .ok => |opts| opts,
        .err => |failure| {
            if (failure.legacy_attest_seen) warnAttestLegacyOnce();
            return failCompileCommandArgs(failure.err);
        },
    };

    if (opts.legacy_attest_seen) warnAttestLegacyOnce();

    try buildArtifact(allocator, .{
        .handler_path = opts.handler_path,
        .output_path = opts.output_path,
        .attest_requested = opts.attest_requested,
    });
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
                error.AccessDenied => {
                    std.debug.print(
                        \\
                        \\Aborted: cannot create '{s}': permission denied.
                        \\Check write permissions on the project root.
                        \\
                    , .{parent});
                    return err;
                },
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

fn parseBuildCommandArgs(argv: []const []const u8) BuildCommandParse {
    var output_override: ?[]const u8 = null;
    var attest_requested = true;
    var legacy_attest_seen = false;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            i += 1;
            if (i >= argv.len) {
                return .{ .err = .{ .err = .missing_output_value, .legacy_attest_seen = legacy_attest_seen } };
            }
            output_override = argv[i];
        } else if (std.mem.eql(u8, arg, attest_flag_legacy)) {
            // Kept as a no-op for parsing. The command emits the legacy warning
            // after parse succeeds, so parse-only tests stay side-effect free.
            legacy_attest_seen = true;
        } else if (std.mem.eql(u8, arg, no_attest_flag)) {
            attest_requested = false;
        } else if (std.mem.eql(u8, arg, "--help")) {
            return .{ .help = legacy_attest_seen };
        } else {
            return .{ .err = .{ .err = .{ .unknown_arg = arg }, .legacy_attest_seen = legacy_attest_seen } };
        }
    }

    return .{ .ok = .{
        .output_override = output_override,
        .attest_requested = attest_requested,
        .legacy_attest_seen = legacy_attest_seen,
    } };
}

fn failBuildCommandArgs(err: CommandArgError) !void {
    switch (err) {
        .missing_output_value => {
            std.log.err("-o requires an output path", .{});
            return error.MissingArgument;
        },
        .unknown_arg => |arg| {
            std.log.err("Unknown argument: {s}", .{arg});
            printBuildHelp();
            return error.UnknownOption;
        },
        .missing_handler_path,
        .missing_output_flag,
        => unreachable,
    }
}

fn buildCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const opts = switch (parseBuildCommandArgs(argv)) {
        .help => |legacy_attest_seen| {
            if (legacy_attest_seen) warnAttestLegacyOnce();
            printBuildHelp();
            return;
        },
        .ok => |opts| opts,
        .err => |failure| {
            if (failure.legacy_attest_seen) warnAttestLegacyOnce();
            return failBuildCommandArgs(failure.err);
        },
    };

    if (opts.legacy_attest_seen) warnAttestLegacyOnce();

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();

    var artifact = try prepareProjectArtifact(allocator, io_backend.io(), "build", opts.output_override);
    defer artifact.deinit(allocator);

    try buildArtifact(allocator, .{
        .handler_path = artifact.handler_path,
        .output_path = artifact.output_path,
        .attest_requested = opts.attest_requested,
    });

    std.debug.print(
        \\
        \\Built: {s}
        \\Run:   {s}
        \\
    , .{ artifact.output_path, artifact.output_path });
}

const local_deploy_accepted_tokens = [_][]const u8{ "--local", "--target", "local", attest_flag_legacy, no_attest_flag };

fn localDeployCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var attest_requested = true;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--help")) {
            printLocalDeployHelp();
            return;
        }
        if (std.mem.eql(u8, arg, attest_flag_legacy)) {
            warnAttestLegacyOnce();
            continue;
        }
        if (std.mem.eql(u8, arg, no_attest_flag)) {
            attest_requested = false;
            continue;
        }
        if (containsString(&local_deploy_accepted_tokens, arg)) continue;

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

    try buildArtifact(allocator, .{
        .handler_path = artifact.handler_path,
        .output_path = artifact.output_path,
        .ledger_service_name = artifact.project_name,
        .attest_requested = attest_requested,
    });

    std.debug.print(
        \\
        \\Deployed: {s}
        \\Run:      ./{s}
        \\Try:      curl http://{s}:{d}/
        \\Ledger:   .zigttp/proofs.jsonl (kind=deploy)
        \\
        \\Inspect the proof ledger: zigttp proofs list
        \\
    , .{ artifact.output_path, artifact.output_path, artifact.project.host, artifact.project.port });
}

/// Slice 1 of proof receipts (docs/roadmap/attest-slice-1.md). Produces a
/// compact JWS committing to (contract_json, bytecode, rule-registry policy,
/// capability matrix) for the current build. Caller owns the returned bytes.
/// Returns null when the compile did not yield a HandlerContract; we cannot
/// sign chips we never derived.
fn buildAttestationJws(
    allocator: std.mem.Allocator,
    contract_json: []const u8,
    bytecode: []const u8,
    contract: *const zigts.HandlerContract,
) !?[]u8 {
    return try attest_build_receipt.buildJws(allocator, contract_json, bytecode, contract);
}

/// Inputs to `buildArtifact`. Bundled so the three call sites
/// (`compileCommand`, `buildCommand`, `localDeployCommand`) name what they
/// pass rather than relying on positional order across a 5-param signature.
const ArtifactBuildInput = struct {
    handler_path: []const u8,
    output_path: []const u8,
    /// Service name to record in the proof ledger entry. Null when the
    /// build is not part of a named project (`compile`/`build` paths);
    /// set to the project name on the `deploy --local` path.
    ledger_service_name: ?[]const u8 = null,
    /// Whether the caller asked for an embedded attestation JWS.
    attest_requested: bool,
};

fn buildArtifact(allocator: std.mem.Allocator, input: ArtifactBuildInput) !void {
    const handler_path = input.handler_path;
    const output_path = input.output_path;
    const ledger_service_name = input.ledger_service_name;
    const attest_requested = input.attest_requested;

    const source = zigts.file_io.readFile(allocator, handler_path, 10 * 1024 * 1024) catch |err| {
        std.log.err("Failed to read handler '{s}': {}", .{ handler_path, err });
        return err;
    };
    defer allocator.free(source);

    std.log.info("Compiling {s}...", .{handler_path});
    shared.step("Compiling handler...");

    var compiled = precompile.compileHandler(allocator, source, handler_path, .{
        .emit_verify = true,
        .emit_contract = true,
    }) catch |err| {
        // precompile already prints per-error lines to stderr; only surface
        // the remediation hint so the dev knows where to look.
        std.debug.print(
            \\
            \\Aborted: handler did not compile. Run `zigttp check` to inspect.
            \\
        , .{});
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

    const attestation_jws: ?[]u8 = blk: {
        if (!attest_requested) break :blk null;
        const cj = contract_json orelse {
            std.log.warn("--attest requested but no contract was emitted; skipping attestation", .{});
            break :blk null;
        };
        const contract_ptr = if (compiled.contract) |*c| c else break :blk null;
        break :blk try buildAttestationJws(allocator, cj, compiled.bytecode, contract_ptr);
    };
    defer if (attestation_jws) |a| allocator.free(a);

    shared.step("Writing binary...");
    const policy = zigts.handler_policy.RuntimePolicy{};
    self_extract.create(
        allocator,
        runtime_binary,
        output_path,
        compiled.bytecode,
        dep_bytecodes,
        contract_json,
        &policy,
        attestation_jws,
    ) catch |err| {
        if (err == error.FileNotFound) {
            // self_extract opens both the runtime template and the output
            // path; the output parent was created by prepareProjectArtifact,
            // so FileNotFound here almost always means the runtime template
            // is missing alongside the dev CLI.
            std.debug.print(
                \\
                \\Aborted: zigttp-runtime template not found at '{s}'.
                \\Install zigttp-runtime alongside zigttp, or rebuild via `zig build`.
                \\
            , .{runtime_binary});
        } else {
            std.log.err("Failed to create output binary: {}", .{err});
        }
        return err;
    };

    if (builtin.os.tag == .macos) {
        codesignAdHoc(allocator, output_path);
    }

    if (ledger_service_name) |service_name| {
        if (compiled.contract) |*contract| {
            shared.step("Recording proof ledger...");
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

fn printInitHelp() void {
    const help =
        \\zigttp init <name> [--template basic|api|htmx]
        \\zigttp init --extension <name>
        \\
        \\Create a new zigttp project or extension directory.
        \\Names may contain letters, numbers, '-' and '_', and must start with
        \\a letter or number.
        \\
        \\Project files (default):
        \\  zigttp.json
        \\  src/handler.ts     (basic/api)
        \\  src/handler.tsx    (htmx)
        \\  tests/handler.test.jsonl
        \\  public/
        \\  .gitignore
        \\  README.md
        \\
        \\Project templates:
        \\  basic   Proof-focused starter for the default dev loop
        \\  api     JSON health and echo endpoints
        \\  htmx    TSX page and HTMX fragment
        \\
        \\Extension files (--extension):
        \\  zigttp-module.json     proof metadata for the partner module
        \\  src/root.zig           native Zig binding against zigttp-sdk
        \\  build.zig
        \\  build.zig.zon
        \\  handler.ts             sample handler importing zigttp-ext:<name>
        \\  .gitignore
        \\  README.md
        \\
        \\Examples:
        \\  zigttp init my-app
        \\  zigttp init --extension my-partner
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
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
        \\  --studio              Also serve /_zigttp/studio
        \\  --no-prove            Watch and reload without contract proof gating
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
        \\Open the browser proof workbench at /_zigttp/studio. In an empty
        \\directory, this command scaffolds a project in place before launching.
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

fn printDoctorHelp() void {
    const help =
        \\zigttp doctor [path]
        \\zigttp doctor --release [--json] [--out FILE]
        \\
        \\Validate the project discovered from the current directory, a handler
        \\path, or a zigttp.json path. Prints a checklist for the files and
        \\runtime options that affect local development.
        \\
        \\With --release, validates the v0.1.0-beta release evidence and prints
        \\a release proof passport. The release check reads existing files only;
        \\it does not run the benchmark or test suite.
        \\
        \\Checks:
        \\  manifest, entry, static directory, system file, tests fixture,
        \\  sqlite/durable settings, and outbound HTTP configuration.
        \\
        \\Examples:
        \\  zigttp doctor
        \\  zigttp doctor src/handler.ts
        \\  zigttp doctor --release --json
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printTestHelp() void {
    const help =
        \\zigttp test [tests.jsonl]
        \\
        \\Run declarative handler tests for the current project. If no test
        \\file is passed, zigttp reads tests/handler.test.jsonl under the
        \\project root discovered from zigttp.json.
        \\
        \\Examples:
        \\  zigttp test
        \\  zigttp test tests/handler.test.jsonl
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
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
        \\
    ++ no_attest_help_block ++
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
        \\zigttp build [-o <output>] [--no-attest]
        \\
        \\Verify the handler in this project and emit a self-contained binary.
        \\Reads zigttp.json from the current directory or any parent.
        \\Default output path is `.zigttp/build/<project-name>`.
        \\
        \\Options:
        \\  -o, --output <PATH>   Override the output binary path
        \\
    ++ no_attest_help_block ++
        \\  --help                Show this help
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printLocalDeployHelp() void {
    const help =
        \\zigttp deploy --local [--no-attest]
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
        \\  --local               Use the local target (this command)
        \\  --target local        Same as --local
        \\
    ++ no_attest_help_block ++
        \\  --help                Show this help
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printDemoHelp() void {
    const help =
        \\zigttp demo [--no-open] [--port N] [--out DIR]
        \\
        \\Create a self-contained Proof Theater workspace and launch Studio.
        \\The demo runs fully local: no cloud credentials, API keys, or
        \\network services are required.
        \\
        \\Options:
        \\  --no-open       Do not try to open the browser
        \\  --port <PORT>   Studio port (default: 3000)
        \\  --out <DIR>     Write the demo project to DIR. Refuses to overwrite.
        \\  --help          Show this help
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
    \\  zigttp demo                            Guided local proof theater
    \\  zigttp studio [handler.ts]             Browser proof workbench
    \\  zigttp edge [--config FILE]            Run the in-process edge runtime
    \\
    \\Package:
    \\  zigttp build [-o <bin>]                Emit a self-contained binary
    \\  zigttp compile <handler.ts> -o <bin>   Build a binary from an explicit path
    \\
    \\Proof ledger:
    \\  zigttp proofs [list|show|diff|watch|export|badge|bundle|verify]
    \\  zigttp verify <url>                    Verify a deployed proof receipt
    \\
    \\Advanced:
    \\  zigttp ratchet [show|check]            Property-regression gate
    \\  zigttp witnesses [list|pin|unpin|prune|synthesize]  Falsifying-input corpus
    \\  zigttp version                         Show version
    \\
    \\Every command keeps its own `--help`.
    \\
    \\The internal runtime template is installed as `zigttp-runtime`.
    \\
;

fn printHelpAll() void {
    _ = std.c.write(std.c.STDOUT_FILENO, core_help_all.ptr, core_help_all.len);
}

fn printExpertHelp() void {
    const help =
        \\zigttp expert - interactive compiler-in-the-loop agent
        \\
        \\Usage:
        \\  zigttp expert
        \\
        \\Starts the interactive coding agent. It runs the same analyzers the
        \\compiler uses, so it can verify edits, explain diagnostics, and propose
        \\fixes against your handler as you work.
        \\
        \\Model backend:
        \\  Set one of these environment variables before launching:
        \\    ANTHROPIC_API_KEY   (recommended)  https://console.anthropic.com/
        \\    OPENAI_API_KEY
        \\  An empty value counts as missing; the command exits with a setup
        \\  message instead of launching against an unconfigured backend.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

const Template = enum { basic, api, htmx };

fn handlerPathForTemplate(template: Template) []const u8 {
    return switch (template) {
        .basic, .api => "src/handler.ts",
        .htmx => "src/handler.tsx",
    };
}

fn parseTemplate(name: []const u8) ?Template {
    if (std.mem.eql(u8, name, "basic")) return .basic;
    if (std.mem.eql(u8, name, "api")) return .api;
    if (std.mem.eql(u8, name, "htmx")) return .htmx;
    return null;
}

test "parseTemplate accepts v1 templates only" {
    try std.testing.expectEqual(Template.basic, parseTemplate("basic").?);
    try std.testing.expectEqual(Template.api, parseTemplate("api").?);
    try std.testing.expectEqual(Template.htmx, parseTemplate("htmx").?);
    try std.testing.expect(parseTemplate("react") == null);
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
        "zigttp serve",  "zigttp build",  "zigttp compile", "zigttp doctor",
        "zigttp proofs", "zigttp studio", "zigttp check",   "zigttp prove",
    }) |cmd| {
        try std.testing.expect(has(core_help_all, cmd));
    }
}

test "hasAllFlag detects the --all escape hatch" {
    try std.testing.expect(hasAllFlag(&.{"--all"}));
    try std.testing.expect(hasAllFlag(&.{ "foo", "all" }));
    try std.testing.expect(!hasAllFlag(&.{"--help"}));
    try std.testing.expect(!hasAllFlag(&.{}));
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
    inline for (.{
        "zigttp login",  "zigttp logout",       "zigttp review",
        "zigttp grants", "zigttp revoke-grant", "zigttp assert-intent",
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

test "initCommand validates arguments before writing files" {
    try std.testing.expectError(error.MissingProjectName, initCommand(std.testing.allocator, &.{}));
    try std.testing.expectError(error.HelpRequested, initCommand(std.testing.allocator, &.{"--help"}));
    try std.testing.expectError(error.MissingTemplate, initCommand(std.testing.allocator, &.{ "demo", "--template" }));
    try std.testing.expectError(error.InvalidTemplate, initCommand(std.testing.allocator, &.{ "demo", "--template", "react" }));
    try std.testing.expectError(error.UnknownOption, initCommand(std.testing.allocator, &.{ "demo", "--bad" }));
}

test "validateProjectName accepts simple safe names" {
    try validateProjectName("demo");
    try validateProjectName("demo-app");
    try validateProjectName("demo_app_1");
    try validateProjectName("123-demo");
}

test "validateProjectName rejects paths and shell-confusing names" {
    const invalid = [_][]const u8{
        "",
        ".",
        "..",
        "../demo",
        "demo/app",
        "demo\\app",
        "-demo",
        "_demo",
        "demo app",
        "demo.app",
    };
    for (invalid) |name| {
        try std.testing.expectError(error.InvalidProjectName, validateProjectName(name));
    }
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

test "init --extension scaffolds a manifest the parser accepts" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try scaffoldExtension(testing.allocator, "demoext");

    const expected = [_][]const u8{
        "demoext/zigttp-module.json",
        "demoext/src/root.zig",
        "demoext/build.zig",
        "demoext/build.zig.zon",
        "demoext/handler.ts",
        "demoext/.gitignore",
        "demoext/README.md",
    };
    for (expected) |path| {
        try std.Io.Dir.access(std.Io.Dir.cwd(), io, path, .{});
    }

    const manifest_bytes = try zigts.file_io.readFile(testing.allocator, "demoext/zigttp-module.json", 256 * 1024);
    defer testing.allocator.free(manifest_bytes);
    var manifest = try zigts.module_manifest.parse(testing.allocator, manifest_bytes);
    defer manifest.deinit(testing.allocator);
    try testing.expectEqualStrings("zigttp-ext:demoext", manifest.specifier);
    try testing.expectEqual(@as(usize, 1), manifest.exports.items.len);
    try testing.expectEqualStrings("greet", manifest.exports.items[0].name);
}

test "init --extension sanitizes build zon package name for hyphenated names" {
    const testing = std.testing;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try scaffoldExtension(testing.allocator, "demo-ext");

    const build_zon = try zigts.file_io.readFile(testing.allocator, "demo-ext/build.zig.zon", 256 * 1024);
    defer testing.allocator.free(build_zon);
    try testing.expect(std.mem.indexOf(u8, build_zon, ".name = .zigttp_ext_demo_ext") != null);
    try testing.expect(std.mem.indexOf(u8, build_zon, ".fingerprint = 0x") != null);
}

test "init --extension package fingerprint uses package name checksum" {
    const testing = std.testing;
    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const package_name = try extensionPackageName(testing.allocator, "Demo-Ext");
    defer testing.allocator.free(package_name);

    try testing.expectEqualStrings("zigttp_ext_demo_ext", package_name);
    const fingerprint = extensionPackageFingerprint(io, package_name);
    try testing.expectEqual(std.hash.Crc32.hash(package_name), @as(u32, @truncate(fingerprint >> 32)));
    try testing.expect(@as(u32, @truncate(fingerprint)) != 0);
}

test "init --extension package name stays within Zig manifest limit" {
    const testing = std.testing;
    const name = "Very-Long-Extension-Name-That-Still-Scaffolds";
    const package_name = try extensionPackageName(testing.allocator, name);
    defer testing.allocator.free(package_name);

    try testing.expect(package_name.len <= 32);
    try testing.expect(std.mem.startsWith(u8, package_name, "zigttp_ext_very"));
    try testing.expect(std.mem.indexOfScalar(u8, package_name, '-') == null);
}

test "init --extension refuses to overwrite an existing extension" {
    // Sanity: the scaffolder exits the process when zigttp-module.json
    // already exists. The exit-on-conflict path itself can't be tested
    // in-process without exiting the test runner, so we only assert that
    // the first scaffold succeeds and the marker is present afterwards.
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try scaffoldExtension(testing.allocator, "alpha");
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "alpha/zigttp-module.json", .{});
}

test "initCommand scaffolds the v1 project layout" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try scaffoldProject(testing.allocator, "demo", .api);

    const expected = [_][]const u8{
        "demo/zigttp.json",
        "demo/src/handler.ts",
        "demo/tests/handler.test.jsonl",
        "demo/public/.keep",
        "demo/.gitignore",
        "demo/README.md",
    };
    for (expected) |path| {
        try std.Io.Dir.access(std.Io.Dir.cwd(), io, path, .{});
    }

    const handler = try zigts.file_io.readFile(testing.allocator, "demo/src/handler.ts", 64 * 1024);
    defer testing.allocator.free(handler);
    try testing.expect(std.mem.indexOf(u8, handler, "function handler(req: Request): Response") != null);
}

test "initCommand writes template-specific starter README files" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try scaffoldProject(testing.allocator, "api-demo", .api);
    try scaffoldProject(testing.allocator, "htmx-demo", .htmx);

    const api_readme = try zigts.file_io.readFile(testing.allocator, "api-demo/README.md", 64 * 1024);
    defer testing.allocator.free(api_readme);
    const htmx_readme = try zigts.file_io.readFile(testing.allocator, "htmx-demo/README.md", 64 * 1024);
    defer testing.allocator.free(htmx_readme);
    const htmx_manifest = try zigts.file_io.readFile(testing.allocator, "htmx-demo/zigttp.json", 64 * 1024);
    defer testing.allocator.free(htmx_manifest);

    try testing.expect(std.mem.indexOf(u8, api_readme, "# zigttp API app") != null);
    try testing.expect(std.mem.indexOf(u8, api_readme, "curl http://127.0.0.1:3000/health") != null);
    try testing.expect(std.mem.indexOf(u8, htmx_readme, "# zigttp HTMX app") != null);
    try testing.expect(std.mem.indexOf(u8, htmx_readme, "src/handler.tsx") != null);
    try testing.expect(std.mem.indexOf(u8, htmx_readme, "zigttp test") != null);
    try testing.expect(std.mem.indexOf(u8, htmx_manifest, "\"entry\": \"src/handler.tsx\"") != null);
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "htmx-demo/src/handler.tsx", .{});
    try testing.expectError(error.FileNotFound, std.Io.Dir.access(std.Io.Dir.cwd(), io, "htmx-demo/src/handler.ts", .{}));
}

test "testCommand validates arguments before project discovery" {
    try std.testing.expectError(error.HelpRequested, testCommand(std.testing.allocator, &.{"--help"}));
    try std.testing.expectError(error.UnknownOption, testCommand(std.testing.allocator, &.{"--watch"}));
    try std.testing.expectError(error.TooManyArguments, testCommand(std.testing.allocator, &.{ "a.jsonl", "b.jsonl" }));
}

test "testCommand runs analyzer before runtime tests" {
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
    try tmp.dir.createDirPath(io, "tests");
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
    try tmp.dir.writeFile(io, .{
        .sub_path = "tests/handler.test.jsonl",
        .data =
        \\{"type":"test","name":"root"}
        \\{"type":"request","method":"GET","url":"/","headers":{},"body":null}
        \\{"type":"expect","status":200,"bodyContains":"ok"}
        ,
    });

    try testing.expectError(error.CheckFailed, testCommand(testing.allocator, &.{}));
}

test "testCommand accepts relative explicit fixture path" {
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
    try tmp.dir.createDirPath(io, "tests");
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
        \\function handler(req: Request): Response {
        \\    return Response.text("ok");
        \\}
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "tests/custom.test.jsonl",
        .data =
        \\{"type":"test","name":"root"}
        \\{"type":"request","method":"GET","url":"/","headers":{},"body":null}
        \\{"type":"expect","status":200,"bodyContains":"ok"}
        ,
    });

    try testCommand(testing.allocator, &.{"tests/custom.test.jsonl"});
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
    \\  "entry": "src/handler.tsx",
    \\  "port": 3000,
    \\  "staticDir": "public"
    \\}
;

const basicHandler = zigts_cli.proof_quest_fixture.starter_source;

const apiHandler =
    \\function handler(req: Request): Response {
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

fn starterPathForTemplate(template: Template) []const u8 {
    return switch (template) {
        .api => "/health",
        .basic, .htmx => "/",
    };
}

const htmxHandler =
    \\function Page(): JSX.Element {
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
    \\function handler(req: Request): Response {
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
    \\{"type":"test","name":"root renders greeting html"}
    \\{"type":"request","method":"GET","url":"/?probe=1","headers":{},"body":null}
    \\{"type":"expect","status":200,"bodyContains":"Hello,"}
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
    \\zig-cache/
    \\zig-out/
    \\.zigttp/
    \\.DS_Store
;

const basicReadme =
    \\# zigttp app
    \\
    \\## Quick start
    \\
    \\1. Start the terminal proof loop:
    \\
    \\       zigttp dev
    \\
    \\   On a fresh scaffold, the Proof Quest runs once. Press `b` to preview
    \\   a tiny edit that breaks `deterministic`, `y` to apply it, then `r`
    \\   and `y` to repair it. Use `zigttp dev --quest` to replay it later.
    \\
    \\2. Edit `src/handler.ts` in your editor. The HUD re-verifies on save and
    \\   shows the verdict, proven surface, proof deltas, and spec diagnostics.
    \\   Press `Tab` or `l` to rotate proof lenses.
    \\
    \\3. Run the fixture:
    \\
    \\       zigttp test
    \\
    \\4. When you are happy with the verdict, do a verified local deploy. It
    \\   builds a self-contained binary and records the proof in the ledger at
    \\   `.zigttp/proofs.jsonl`:
    \\
    \\       zigttp deploy
    \\       ./.zigttp/deploy/<this-app-name>
    \\
    \\## More
    \\
    \\- `zigttp expert` - interactive compiler-in-the-loop agent
    \\- `zigttp help --all` - advanced commands
;

const apiReadme =
    \\# zigttp API app
    \\
    \\## Quick start
    \\
    \\1. Start the local dev loop:
    \\
    \\       zigttp dev
    \\
    \\2. Try the starter endpoints:
    \\
    \\       curl http://127.0.0.1:3000/health
    \\       curl -X POST http://127.0.0.1:3000/echo -d 'hello'
    \\
    \\3. Run the fixture:
    \\
    \\       zigttp test
    \\
    \\4. Deploy locally (builds the binary and records the proof ledger entry):
    \\
    \\       zigttp deploy
    \\
    \\## More
    \\
    \\- `zigttp expert` - interactive compiler-in-the-loop agent
    \\- `zigttp help --all` - advanced commands
;

const htmxReadme =
    \\# zigttp HTMX app
    \\
    \\## Quick start
    \\
    \\1. Start the TSX/HTMX dev loop:
    \\
    \\       zigttp dev
    \\
    \\2. Edit `src/handler.tsx`, then open the page:
    \\
    \\       http://127.0.0.1:3000/
    \\
    \\3. Run the fixture:
    \\
    \\       zigttp test
    \\
    \\4. Deploy locally (builds the binary and records the proof ledger entry):
    \\
    \\       zigttp deploy
    \\
    \\## More
    \\
    \\- `zigttp expert` - interactive compiler-in-the-loop agent
    \\- `zigttp help --all` - advanced commands
;

// `{{name}}` is replaced by the extension name at scaffold time.
const extension_manifest_template =
    \\{
    \\  "schemaVersion": 1,
    \\  "specifier": "zigttp-ext:{{name}}",
    \\  "backend": "native-zig",
    \\  "stateModel": "none",
    \\  "requiredCapabilities": [],
    \\  "exports": [
    \\    {
    \\      "name": "greet",
    \\      "params": ["string"],
    \\      "returns": "string",
    \\      "effect": "read",
    \\      "traceable": true
    \\    }
    \\  ]
    \\}
    \\
;

const extension_root_zig_template =
    \\const sdk = @import("zigttp-sdk");
    \\
    \\pub const binding = sdk.ModuleBinding{
    \\    .specifier = "zigttp-ext:{{name}}",
    \\    .name = "ext_{{name}}",
    \\    .required_capabilities = &.{},
    \\    .exports = &.{
    \\        .{
    \\            .name = "greet",
    \\            .module_func = greetNative,
    \\            .arg_count = 1,
    \\            .effect = .read,
    \\            .returns = .string,
    \\            .param_types = &.{.string},
    \\            .traceable = true,
    \\        },
    \\    },
    \\};
    \\
    \\comptime {
    \\    sdk.validateBindings(&.{binding});
    \\}
    \\
    \\fn greetNative(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    \\    const subject = if (args.len > 0) sdk.extractString(args[0]) orelse "world" else "world";
    \\    const prefix = "hello, ";
    \\    var buf: [256]u8 = undefined;
    \\    const subject_len = @min(subject.len, buf.len - prefix.len);
    \\    @memcpy(buf[0..prefix.len], prefix);
    \\    @memcpy(buf[prefix.len..][0..subject_len], subject[0..subject_len]);
    \\    return sdk.createString(handle, buf[0 .. prefix.len + subject_len]);
    \\}
    \\
;

const extension_build_zig_template =
    \\const std = @import("std");
    \\
    \\pub fn build(b: *std.Build) void {
    \\    const target = b.standardTargetOptions(.{});
    \\    const optimize = b.standardOptimizeOption(.{});
    \\    const sdk_dep = b.dependency("zigttp_sdk", .{
    \\        .target = target,
    \\        .optimize = optimize,
    \\    });
    \\
    \\    _ = b.addModule("{{name}}", .{
    \\        .root_source_file = b.path("src/root.zig"),
    \\        .target = target,
    \\        .optimize = optimize,
    \\        .imports = &.{
    \\            .{ .name = "zigttp-sdk", .module = sdk_dep.module("zigttp-sdk") },
    \\        },
    \\    });
    \\}
    \\
;

const extension_build_zon_template =
    \\.{
    \\    .name = .{{package_name}},
    \\    .version = "0.0.0",
    \\    .fingerprint = {{fingerprint}}, // Changing this has security and trust implications.
    \\    .minimum_zig_version = "0.16.0",
    \\    .dependencies = .{
    \\        // Replace this placeholder with the version of zigttp-sdk you depend on.
    \\        // For monorepo use, set `.path = "../zigttp-sdk"`. For external use,
    \\        // run `zig fetch --save <url>` to pin a release tarball.
    \\        .zigttp_sdk = .{
    \\            .path = "../zigttp-sdk",
    \\        },
    \\    },
    \\    .paths = .{
    \\        "build.zig",
    \\        "build.zig.zon",
    \\        "src",
    \\        "zigttp-module.json",
    \\    },
    \\}
    \\
;

const extension_handler_ts_template =
    \\import { greet } from "zigttp-ext:{{name}}";
    \\
    \\function handler(req) {
    \\    if (req.method === "GET" && req.path === "/") {
    \\        return Response.text(greet("zigttp"));
    \\    }
    \\    return Response.text("Not Found", { status: 404 });
    \\}
    \\
;

const extension_readme_template =
    \\# zigttp-ext:{{name}}
    \\
    \\A minimal zigttp extension scaffolded by `zigttp init --extension {{name}}`.
    \\
    \\## Files
    \\
    \\- `zigttp-module.json` - proof metadata for tools like
    \\  `zigts verify-module-manifest` and `zigts extension-status`.
    \\- `src/root.zig` - the native Zig binding that implements the export.
    \\- `build.zig` / `build.zig.zon` - Zig build wiring against `zigttp-sdk`.
    \\- `handler.ts` - a sibling handler that imports `zigttp-ext:{{name}}` so
    \\  you can verify end-to-end with `zigts check` once the manifest is
    \\  registered via `--module-manifest`.
    \\
    \\## Validate the manifest
    \\
    \\    zigts verify-module-manifest zigttp-module.json
    \\    zigts extension-status --module-manifest zigttp-module.json
    \\
    \\## Wire up `zigttp-sdk`
    \\
    \\`build.zig.zon` ships with a `path = "../zigttp-sdk"` placeholder. For
    \\external use, swap it for a `zig fetch --save <url>` line that pins a
    \\released tarball. Then run `zig build` to compile the binding.
    \\
;

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

// ---------------------------------------------------------------------------
// compileCommand / buildCommand argv-parsing error paths. The happy paths
// reach buildArtifact, which requires a real handler on disk and a runtime
// binary alongside the test executable — out of scope for unit tests. The
// argv parser is the part the user trips on most often, and it's testable
// in isolation.
// ---------------------------------------------------------------------------

test "compileCommand requires a handler positional" {
    const testing = std.testing;
    try testing.expectEqual(CommandArgError.missing_handler_path, parseCompileCommandArgs(&.{}).err.err);
    try testing.expectEqual(
        CommandArgError.missing_handler_path,
        parseCompileCommandArgs(&.{ "-o", "out.bin" }).err.err,
    );
}

test "compileCommand requires -o" {
    const testing = std.testing;
    // Handler positional present, but -o missing.
    try testing.expectEqual(
        CommandArgError.missing_output_flag,
        parseCompileCommandArgs(&.{"handler.ts"}).err.err,
    );
}

test "compileCommand rejects bare -o without a follow-up path" {
    const testing = std.testing;
    // `-o` is the last token; the inner `i += 1; if (i >= argv.len)` guard
    // must fire as MissingArgument rather than silently leaving output null.
    try testing.expectEqual(
        CommandArgError.missing_output_value,
        parseCompileCommandArgs(&.{ "handler.ts", "-o" }).err.err,
    );
}

test "buildCommand rejects bare -o without a follow-up path" {
    const testing = std.testing;
    try testing.expectEqual(
        CommandArgError.missing_output_value,
        parseBuildCommandArgs(&.{"-o"}).err.err,
    );
}

test "buildCommand rejects unknown flags as UnknownOption" {
    const testing = std.testing;
    // Anything past the known set (`-o`, `--no-attest`, legacy `--attest`,
    // `--help`) hits the catch-all branch and exits with UnknownOption so
    // the user knows their flag was not recognised.
    try testing.expectEqualStrings("--unknown-flag", parseBuildCommandArgs(&.{"--unknown-flag"}).err.err.unknown_arg);
    try testing.expectEqualStrings("unexpected_positional", parseBuildCommandArgs(&.{"unexpected_positional"}).err.err.unknown_arg);
}

test "compileCommand rejects unknown -prefixed flags symmetrically with buildCommand" {
    const testing = std.testing;
    // The argv loop used to silently drop `-`-prefixed unknowns via the
    // non-matching path between `--help` and the `!startsWith("-")`
    // positional branch. A typo like `--ouptut` would be ignored and the
    // build would proceed with the default state. The new else arm
    // returns UnknownOption — same behaviour as buildCommand.
    try testing.expectEqualStrings("--ouptut", parseCompileCommandArgs(&.{ "--ouptut", "out.bin", "handler.ts" }).err.err.unknown_arg);
    try testing.expectEqualStrings("--json", parseCompileCommandArgs(&.{ "--json", "handler.ts", "-o", "out.bin" }).err.err.unknown_arg);
}
