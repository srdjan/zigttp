const std = @import("std");
const builtin = @import("builtin");

const project_config_mod = @import("project_config");
const zigts = @import("zigts");
const self_extract = @import("self_extract.zig");
const zigts_cli = @import("zigts_cli");
const deploy = @import("deploy.zig");
const precompile = zigts_cli.precompile;
const shared = @import("cli_shared.zig");
const runtime_cli = @import("runtime_cli.zig");

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
        printHelp();
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
    if (std.mem.eql(u8, command, "serve")) {
        // Convenience: dev CLI can also serve a handler locally for quick testing.
        try runtime_cli.serveCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "doctor")) {
        try doctorCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "check")) {
        try zigts_cli.run(allocator, user_args);
        return;
    }
    if (std.mem.eql(u8, command, "prove")) {
        try zigts_cli.run(allocator, user_args);
        return;
    }
    if (std.mem.eql(u8, command, "mock")) {
        try zigts_cli.run(allocator, user_args);
        return;
    }
    if (std.mem.eql(u8, command, "link")) {
        try zigts_cli.run(allocator, user_args);
        return;
    }
    if (std.mem.eql(u8, command, "compile")) {
        try compileCommand(allocator, user_args[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "expert")) {
        const pi_app = @import("pi_app");
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
                std.debug.print("zigttp-cli login only accepts --token-stdin and --device.\n\n", .{});
                printLoginHelp();
                return;
            }
            if (err == error.InvalidOptionCombination) {
                std.debug.print("Choose either --token-stdin or --device, not both.\n\n", .{});
                printLoginHelp();
                return;
            }
            if (err == error.TokenPromptUnavailable) {
                std.debug.print("Interactive token entry requires a TTY. Use `zigttp-cli login --token-stdin` or `zigttp-cli login --device`.\n", .{});
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
        deploy.run(allocator, user_args[1..]) catch |err| {
            if (err == error.HelpRequested) {
                printDeployHelp();
                return;
            }
            if (err == error.UnknownOption) {
                std.debug.print("zigttp-cli deploy only accepts --confirm, --region <name>, --wait, and --no-wait.\n\n", .{});
                printDeployHelp();
                return;
            }
            if (err == error.MissingOptionValue) {
                std.debug.print("--region requires a value, for example: zigttp-cli deploy --region us-east\n\n", .{});
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
                std.debug.print("zigttp-cli review accepts <plan-id> and optional --approve|--reject, with --grant only for approvals.\n\n", .{});
                printReviewHelp();
                return;
            }
            if (err == error.MissingPlanId) {
                std.debug.print("zigttp-cli review requires a plan id.\n\n", .{});
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
                std.debug.print("zigttp-cli grants accepts at most one optional project name.\n\n", .{});
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
                std.debug.print("zigttp-cli revoke-grant only accepts a grant id.\n\n", .{});
                printRevokeGrantHelp();
                return;
            }
            if (err == error.MissingGrantId) {
                std.debug.print("zigttp-cli revoke-grant requires a grant id.\n\n", .{});
                printRevokeGrantHelp();
                return;
            }
            if (err == error.InvalidOptionCombination) {
                std.debug.print("zigttp-cli revoke-grant accepts exactly one grant id.\n\n", .{});
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
    std.debug.print("  zigttp-cli dev\n", .{});
    std.debug.print("  zigttp-cli check\n", .{});
}

fn devCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
    try runDevPreflight(allocator, argv);

    const runtime_binary = try resolveRuntimeBinary(allocator, program_path);
    defer allocator.free(runtime_binary);

    var child_args = std.ArrayList([]const u8).empty;
    defer child_args.deinit(allocator);
    try child_args.append(allocator, runtime_binary);
    try child_args.append(allocator, "serve");
    try child_args.appendSlice(allocator, argv);

    var watch = try shared.buildWatchSet(allocator, argv);
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

/// Find the `zigttp` runtime binary.
/// Strategy: adjacent to this CLI binary (same directory), then PATH fallback.
fn resolveRuntimeBinary(allocator: std.mem.Allocator, program_path: []const u8) ![]const u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const abs_self = try std.fs.path.resolve(allocator, &.{program_path});
    defer allocator.free(abs_self);

    const dir_name = std.fs.path.dirname(abs_self) orelse ".";
    const candidate = try std.fs.path.join(allocator, &.{ dir_name, "zigttp" });
    errdefer allocator.free(candidate);

    std.Io.Dir.accessAbsolute(io, candidate, .{}) catch {
        allocator.free(candidate);
        // Fall back to PATH resolution
        return try allocator.dupe(u8, "zigttp");
    };
    return candidate;
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

    const source = zigts.file_io.readFile(allocator, handler_path.?, 10 * 1024 * 1024) catch |err| {
        std.log.err("Failed to read handler '{s}': {}", .{ handler_path.?, err });
        return err;
    };
    defer allocator.free(source);

    std.log.info("Compiling {s}...", .{handler_path.?});

    var compiled = precompile.compileHandler(
        allocator,
        source,
        handler_path.?,
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
        output_path.?,
        compiled.bytecode,
        dep_bytecodes,
        contract_json,
        &policy,
    ) catch |err| {
        std.log.err("Failed to create output binary: {}", .{err});
        return err;
    };

    if (builtin.os.tag == .macos) {
        codesignAdHoc(allocator, output_path.?);
    }

    std.log.info("Compiled: {s} -> {s} (bytecode {d} bytes)", .{
        handler_path.?, output_path.?, compiled.bytecode.len,
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
        \\zigttp-cli compile <handler.ts> -o <output>
        \\
        \\Compile a handler into a self-contained binary.
        \\Verification is mandatory: the handler must pass all checks.
        \\The output binary wraps the zigttp runtime, which must be installed
        \\adjacent to zigttp-cli.
        \\
        \\Options:
        \\  -o, --output <PATH>   Output binary path (required)
        \\  --help                Show this help
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printDeployHelp() void {
    const help =
        \\zigttp-cli deploy
        \\
        \\Deploy the handler in this directory to the zigttp runtime.
        \\
        \\If credentials are missing, Zigttp first prompts for an access token.
        \\Submit an empty token to fall back to browser-based device login.
        \\
        \\The command auto-detects everything by default. Optional flags:
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
        \\  zigttp-cli login    Store deploy credentials explicitly
        \\  zigttp-cli logout   Forget the saved sign-in credentials
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printLoginHelp() void {
    const help =
        \\zigttp-cli login
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
        \\  zigttp-cli deploy   Deploy using saved credentials or prompt if needed
        \\  zigttp-cli logout   Forget the saved sign-in credentials
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printReviewHelp() void {
    const help =
        \\zigttp-cli review <plan-id>
        \\zigttp-cli review <plan-id> --approve [--grant]
        \\zigttp-cli review <plan-id> --reject
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
        \\  zigttp-cli deploy   Trigger a deploy and surface plan-required reviews
        \\  zigttp-cli login    Store deploy credentials explicitly
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printGrantsHelp() void {
    const help =
        \\zigttp-cli grants [project-name]
        \\
        \\List reusable capability grants visible to the current account.
        \\
        \\Arguments:
        \\  project-name    Optional project filter
        \\
        \\Related:
        \\  zigttp-cli revoke-grant <grant-id>   Revoke a reusable capability grant
        \\  zigttp-cli review <plan-id>          Inspect or approve a pending review
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printRevokeGrantHelp() void {
    const help =
        \\zigttp-cli revoke-grant <grant-id>
        \\
        \\Revoke a reusable capability grant so future risky deploys must be reviewed again.
        \\
        \\Related:
        \\  zigttp-cli grants                    List reusable capability grants
        \\  zigttp-cli review <plan-id>         Approve a new review or grant
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printHelp() void {
    const help =
        \\zigttp-cli - developer tool for zigttp projects
        \\
        \\Usage:
        \\  zigttp-cli init <name> [--template ...]    Create project
        \\  zigttp-cli dev [handler-or-project]         Watch-restart loop
        \\  zigttp-cli serve [options] [handler.ts]    Run handler locally
        \\  zigttp-cli expert                          Run the interactive compiler agent
        \\  zigttp-cli check [handler.ts] [--contract]  Verify handler
        \\  zigttp-cli compile <handler.ts> -o <bin>    Build self-contained binary
        \\  zigttp-cli login                            Store deploy credentials
        \\  zigttp-cli deploy                           Deploy the handler in this directory
        \\  zigttp-cli review <plan-id>                 Inspect, approve, or reject a deploy plan
        \\  zigttp-cli grants [project-name]            List reusable capability grants
        \\  zigttp-cli revoke-grant <grant-id>          Revoke a reusable capability grant
        \\  zigttp-cli logout                           Forget saved sign-in credentials
        \\  zigttp-cli prove <old.json> <new.json>      Upgrade safety check
        \\  zigttp-cli mock <tests.jsonl> [--port N]    Mock server from tests
        \\  zigttp-cli link <system.json>               Cross-handler linking
        \\  zigttp-cli doctor [path]                    Validate project
        \\  zigttp-cli version                          Show version
        \\
        \\The `zigttp` runtime binary is separate and is what ships with deployed apps.
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
    \\- `zigttp-cli dev`
    \\- `zigttp serve`
    \\- `zigttp-cli check`
;

const apiReadme = basicReadme;
const htmxReadme = basicReadme;
