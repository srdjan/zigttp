//! `zigttp compile`, `zigttp build`, and `zigttp deploy --local` — the three
//! commands that turn a handler into a self-contained binary. They share the
//! artifact pipeline (prepareProjectArtifact + buildArtifact), so they live
//! together. Dispatch and error-to-exit-code translation stay in dev_cli.main;
//! the help printers are internal because only these commands invoke them.

const std = @import("std");
const builtin = @import("builtin");

const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const precompile = zigts_cli.precompile;
const shared = @import("cli_shared.zig");
const self_extract = @import("self_extract.zig");
const attest_build_receipt = @import("attest/build_receipt.zig");
const live_reload = @import("live_reload.zig");
const project_config_mod = @import("project_config");
const cli_paths = @import("cli_paths.zig");
const resolveRuntimeBinary = cli_paths.resolveRuntimeBinary;
const cli_args = @import("cli_args.zig");
const containsString = cli_args.containsString;

const no_attest_flag: []const u8 = "--no-attest";

/// Shared help text describing `--no-attest`. Three command help printers
/// all advertise the same flag; one source of truth prevents drift.
const no_attest_help_block: []const u8 =
    \\  --no-attest           Skip proof-receipt signing for this build.
    \\                        Default is to sign with the persistent
    \\                        identity at ~/.zigttp/attest/keypair.bin.
    \\
;

const CompileCommandOptions = struct {
    handler_path: []const u8,
    output_path: []const u8,
    attest_requested: bool,
};

const BuildCommandOptions = struct {
    output_override: ?[]const u8,
    attest_requested: bool,
};

const CommandArgError = union(enum) {
    missing_output_value,
    missing_handler_path,
    missing_output_flag,
    unknown_arg: []const u8,
};

const CompileCommandParse = union(enum) {
    help,
    ok: CompileCommandOptions,
    err: CommandArgError,
};

const BuildCommandParse = union(enum) {
    help,
    ok: BuildCommandOptions,
    err: CommandArgError,
};

fn parseCompileCommandArgs(argv: []const []const u8) CompileCommandParse {
    var handler_path: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;
    var attest_requested = true;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            i += 1;
            if (i >= argv.len) {
                return .{ .err = .missing_output_value };
            }
            output_path = argv[i];
        } else if (std.mem.eql(u8, arg, no_attest_flag)) {
            attest_requested = false;
        } else if (std.mem.eql(u8, arg, "--help")) {
            return .help;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            handler_path = arg;
        } else {
            // Reject unknown `-`-prefixed flags rather than silently
            // dropping them. Symmetric with buildCommand at :2106-2110.
            // Without this branch, a typo like `--ouptut` was swallowed
            // and the build proceeded against the wrong (or default)
            // state with no diagnostic.
            return .{ .err = .{ .unknown_arg = arg } };
        }
    }

    if (handler_path == null) {
        return .{ .err = .missing_handler_path };
    }
    if (output_path == null) {
        return .{ .err = .missing_output_flag };
    }

    return .{ .ok = .{
        .handler_path = handler_path.?,
        .output_path = output_path.?,
        .attest_requested = attest_requested,
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

pub fn compileCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const opts = switch (parseCompileCommandArgs(argv)) {
        .help => {
            printCompileHelp();
            return;
        },
        .ok => |opts| opts,
        .err => |err| return failCompileCommandArgs(err),
    };

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

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            i += 1;
            if (i >= argv.len) {
                return .{ .err = .missing_output_value };
            }
            output_override = argv[i];
        } else if (std.mem.eql(u8, arg, no_attest_flag)) {
            attest_requested = false;
        } else if (std.mem.eql(u8, arg, "--help")) {
            return .help;
        } else {
            return .{ .err = .{ .unknown_arg = arg } };
        }
    }

    return .{ .ok = .{
        .output_override = output_override,
        .attest_requested = attest_requested,
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

pub fn buildCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    const opts = switch (parseBuildCommandArgs(argv)) {
        .help => {
            printBuildHelp();
            return;
        },
        .ok => |opts| opts,
        .err => |err| return failBuildCommandArgs(err),
    };

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

const local_deploy_accepted_tokens = [_][]const u8{ "--local", "--target", "local", no_attest_flag };

pub fn localDeployCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var attest_requested = true;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--help")) {
            printLocalDeployHelp();
            return;
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
        \\Run:      {s}
        \\Try:      curl http://{s}:{d}/
        \\Ledger:   .zigttp/proofs.jsonl (kind=deploy)
        \\
        \\Inspect the proof ledger: zigttp proofs list
        \\
    , .{ artifact.output_path, artifact.output_path, artifact.project.host, artifact.project.port });
}

/// Produces a compact JWS committing to (contract_json, bytecode,
/// rule-registry policy, capability matrix) for the current build. Caller owns
/// the returned bytes.
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
            std.log.warn("attestation requested but no contract was emitted; skipping attestation", .{});
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
        // The appended bytecode makes the signed binary fail codesign's strict
        // validation, so codesign prints scary-but-expected diagnostics to
        // stdout/stderr. Silence them: the signature is still applied and the
        // binary still runs. Redirect both streams to /dev/null before exec.
        const devnull_fd = std.c.open("/dev/null", .{ .ACCMODE = .WRONLY }, @as(std.c.mode_t, 0));
        if (devnull_fd >= 0) {
            _ = std.c.dup2(devnull_fd, std.c.STDOUT_FILENO);
            _ = std.c.dup2(devnull_fd, std.c.STDERR_FILENO);
        }
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
        // A non-zero status is expected (strict validation fails on the appended
        // bytecode) and unactionable, so it is deliberately not surfaced.
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
    try testing.expectEqual(CommandArgError.missing_handler_path, parseCompileCommandArgs(&.{}).err);
    try testing.expectEqual(
        CommandArgError.missing_handler_path,
        parseCompileCommandArgs(&.{ "-o", "out.bin" }).err,
    );
}

test "compileCommand requires -o" {
    const testing = std.testing;
    // Handler positional present, but -o missing.
    try testing.expectEqual(
        CommandArgError.missing_output_flag,
        parseCompileCommandArgs(&.{"handler.ts"}).err,
    );
}

test "compileCommand rejects bare -o without a follow-up path" {
    const testing = std.testing;
    // `-o` is the last token; the inner `i += 1; if (i >= argv.len)` guard
    // must fire as MissingArgument rather than silently leaving output null.
    try testing.expectEqual(
        CommandArgError.missing_output_value,
        parseCompileCommandArgs(&.{ "handler.ts", "-o" }).err,
    );
}

test "buildCommand rejects bare -o without a follow-up path" {
    const testing = std.testing;
    try testing.expectEqual(
        CommandArgError.missing_output_value,
        parseBuildCommandArgs(&.{"-o"}).err,
    );
}

test "buildCommand rejects unknown flags as UnknownOption" {
    const testing = std.testing;
    // Anything past the known set (`-o`, `--no-attest`, `--help`) hits
    // the catch-all branch and exits with UnknownOption so
    // the user knows their flag was not recognised.
    try testing.expectEqualStrings("--unknown-flag", parseBuildCommandArgs(&.{"--unknown-flag"}).err.unknown_arg);
    try testing.expectEqualStrings("unexpected_positional", parseBuildCommandArgs(&.{"unexpected_positional"}).err.unknown_arg);
}

test "compileCommand rejects unknown -prefixed flags symmetrically with buildCommand" {
    const testing = std.testing;
    // The argv loop used to silently drop `-`-prefixed unknowns via the
    // non-matching path between `--help` and the `!startsWith("-")`
    // positional branch. A typo like `--ouptut` would be ignored and the
    // build would proceed with the default state. The new else arm
    // returns UnknownOption — same behaviour as buildCommand.
    try testing.expectEqualStrings("--ouptut", parseCompileCommandArgs(&.{ "--ouptut", "out.bin", "handler.ts" }).err.unknown_arg);
    try testing.expectEqualStrings("--json", parseCompileCommandArgs(&.{ "--json", "handler.ts", "-o", "out.bin" }).err.unknown_arg);
}
