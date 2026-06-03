//! `zigttp dev` and `zigttp studio` — the local edit loop. `dev` is the magnet
//! surface (implies --watch --prove, optional proof-capsule recording and the
//! first-run quest); `studio` adds the browser workbench and scaffolds an empty
//! cwd in place. Both share runDevPreflight (analyzer gate before serving).
//! Dispatch and error-to-exit-code translation stay in dev_cli.main.

const std = @import("std");
const builtin = @import("builtin");

const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const precompile = zigts_cli.precompile;
const shared = @import("cli_shared.zig");
const cli_tour = @import("cli_tour.zig");
const proof_cli = @import("proof_cli.zig");
const pi_app = @import("pi_app");
const project_config_mod = @import("project_config");
const cli_paths = @import("cli_paths.zig");
const resolveDeveloperServeBinary = cli_paths.resolveDeveloperServeBinary;
const cli_doctor = @import("cli_doctor.zig");
const doctorPathExists = cli_doctor.doctorPathExists;
const printCheckStageFailures = cli_doctor.printCheckStageFailures;
const init_command = @import("init_command.zig");
const cli_templates = @import("cli_templates.zig");
const Template = cli_templates.Template;
const parseTemplate = cli_templates.parseTemplate;

pub fn devCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
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
    const user_no_tour = cli_tour.skipRequested(argv);
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
        if (std.mem.eql(u8, arg, "--no-quest")) continue;
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

    // Render the four-property explanation once, just before the serve child's
    // proof-card HUD (and the guided quest) start streaming, so a first-time
    // user reads what a proof / deterministic / injection_safe mean before the
    // quest drops them into "[quest] press b to preview...". Touches the marker;
    // `default_quest` above already captured the pre-touch (first-run) state, so
    // the tour and the quest appear together exactly once.
    cli_tour.maybeShowFirstRunTour(allocator, argv);

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
pub fn capsuleReplayProbe(
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

pub fn studioCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
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

pub fn printDevHelp() void {
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
        \\  --no-quest            Skip the first-run proof tour and guided quest
        \\                        (alias: --no-tour)
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

pub fn printStudioHelp() void {
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
