//! `zigttp doctor` and its diagnostic printers, extracted from dev_cli.zig.
//!
//! Validates the project discovered from the current directory (or an
//! explicit path / handler file) and prints a checklist for the files and
//! runtime options that affect local development. The `--release` branch
//! delegates to cli_release_check.releaseDoctorCommand; otherwise this
//! module owns the entire flow.

const std = @import("std");
const builtin = @import("builtin");
const project_config_mod = @import("project_config");
const zigts_cli = @import("zigts_cli");
const precompile = zigts_cli.precompile;
const self_extract = @import("self_extract.zig");
const cli_release_check = @import("cli_release_check.zig");
const cli_paths = @import("cli_paths.zig");

pub fn doctorCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
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

pub fn printDoctorHelp() void {
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

pub fn runDoctorAnalyzerForProject(
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

pub fn printCheckStageFailures(check: *const precompile.CheckResult, prefix: []const u8) void {
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

    const runtime_path = cli_paths.resolveRuntimeBinary(allocator, self_path) catch {
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

pub fn doctorPathExists(io: std.Io, path: []const u8) bool {
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
