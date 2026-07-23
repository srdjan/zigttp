//! `zttp init` — scaffold a new project or partner extension. This module
//! owns all scaffolding: the project templates, the extension templates, and
//! the file writers. The studio/dev preflight reuses `scaffoldProject` to
//! scaffold an empty cwd in place, so that helper is public. Dispatch and
//! error-to-exit-code translation stay in dev_cli.main.

const std = @import("std");
const builtin = @import("builtin");

const zts = @import("zts");
const zts_cli = @import("zts_cli");
const shared = @import("cli_shared.zig");
const cli_templates = @import("cli_templates.zig");
const Template = cli_templates.Template;
const parseTemplate = cli_templates.parseTemplate;

/// What `initCommand` decided after scaffolding. `enter_expert` asks the
/// dispatcher to chdir into the new project and launch `zttp expert` (the
/// `--expert` flag); `project_name` borrows from argv.
pub const InitOutcome = struct {
    enter_expert: bool = false,
    project_name: ?[]const u8 = null,
};

/// Parsed `zttp init` arguments, before any scaffolding side effect. Shared by
/// `initCommand` (which scaffolds) and `willEnterExpert` (which only inspects the
/// decision) so the two can never diverge on what `--expert` means.
const ParsedInitArgs = struct {
    template_name: []const u8 = "basic",
    project_name: ?[]const u8 = null,
    extension_name: ?[]const u8 = null,
    enter_expert: bool = false,
};

fn parseInitArgs(argv: []const []const u8) !ParsedInitArgs {
    if (argv.len == 0) return error.MissingProjectName;

    var parsed: ParsedInitArgs = .{};
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return error.HelpRequested;
        }
        if (std.mem.eql(u8, arg, "--template")) {
            parsed.template_name = try shared.takeArg(&i, argv, error.MissingTemplate);
            continue;
        }
        if (std.mem.eql(u8, arg, "--extension")) {
            parsed.extension_name = try shared.takeArg(&i, argv, error.MissingExtensionName);
            continue;
        }
        if (std.mem.eql(u8, arg, "--expert")) {
            parsed.enter_expert = true;
            continue;
        }
        if (std.mem.startsWith(u8, arg, "-")) {
            return error.UnknownOption;
        }
        if (parsed.project_name == null) {
            parsed.project_name = arg;
            continue;
        }
        return error.InvalidArgument;
    }
    return parsed;
}

/// Whether `zttp init <argv>` would launch the expert agent after scaffolding.
/// The dispatcher uses this to run the model-backend check before scaffolding,
/// without re-deriving `--expert` from a raw token scan: this honors `--help`
/// (returns false), ignores `--expert` when `--extension` wins or when
/// `--template` consumes `--expert` as its value, exactly as the real flow does.
pub fn willEnterExpert(argv: []const []const u8) bool {
    const parsed = parseInitArgs(argv) catch return false;
    return parsed.enter_expert and parsed.extension_name == null;
}

pub fn initCommand(allocator: std.mem.Allocator, argv: []const []const u8) !InitOutcome {
    const parsed = try parseInitArgs(argv);

    if (parsed.extension_name) |name| {
        try validateProjectName(name);
        scaffoldExtension(allocator, name) catch |err| {
            if (err == error.ScaffoldTargetExists) std.process.exit(1);
            return err;
        };
        printInitExtensionNextSteps(name);
        return .{};
    }

    const name = parsed.project_name orelse return error.MissingProjectName;
    try validateProjectName(name);
    const template = parseTemplate(parsed.template_name) orelse return error.InvalidTemplate;

    scaffoldProject(allocator, name, template) catch |err| {
        if (err == error.ScaffoldTargetExists) std.process.exit(1);
        return err;
    };
    if (parsed.enter_expert) {
        printInitExpertHandoff(name);
        return .{ .enter_expert = true, .project_name = name };
    }
    printInitNextSteps(name, template);
    return .{};
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

fn scaffoldPaths(allocator: std.mem.Allocator, name: []const u8, relative_paths: []const []const u8) ![][]u8 {
    const paths = try allocator.alloc([]u8, relative_paths.len);
    errdefer allocator.free(paths);

    var initialized: usize = 0;
    errdefer for (paths[0..initialized]) |path| allocator.free(path);
    for (relative_paths, 0..) |relative_path, i| {
        paths[i] = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ name, relative_path });
        initialized += 1;
    }
    return paths;
}

fn freeScaffoldPaths(allocator: std.mem.Allocator, paths: [][]u8) void {
    for (paths) |path| allocator.free(path);
    allocator.free(paths);
}

fn refuseExistingTargets(io: std.Io, paths: []const []const u8, preserved_path: ?[]const u8) !void {
    for (paths) |path| {
        if (preserved_path) |preserved| {
            if (std.mem.eql(u8, path, preserved)) continue;
        }
        std.Io.Dir.access(std.Io.Dir.cwd(), io, path, .{}) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => return err,
        };
        reportScaffoldConflict(path);
        return error.ScaffoldTargetExists;
    }
}

fn createScaffoldDir(io: std.Io, path: []const u8) !bool {
    return try std.Io.Dir.cwd().createDirPathStatus(io, path, .default_dir) == .created;
}

fn rollbackScaffoldFiles(io: std.Io, paths: []const []const u8, preserved_path: ?[]const u8) void {
    var i = paths.len;
    while (i > 0) {
        i -= 1;
        if (preserved_path) |preserved| {
            if (std.mem.eql(u8, paths[i], preserved)) continue;
        }
        std.Io.Dir.cwd().deleteFile(io, paths[i]) catch {};
    }
}

fn reportScaffoldConflict(path: []const u8) void {
    std.debug.print(
        "error: '{s}' already exists. Pick a different name or remove the conflicting file; zttp init will not overwrite it.\n",
        .{path},
    );
}

pub fn scaffoldProject(allocator: std.mem.Allocator, name: []const u8, template: Template) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    // Named init scaffolds refuse every existing target before writing. The
    // in-place studio preflight preserves a caller-owned .gitignore.
    const relative_paths = [_][]const u8{
        "zttp.json",
        handlerPathForTemplate(template),
        "tests/handler.test.jsonl",
        ".gitignore",
        "README.md",
        "public/.keep",
    };
    const paths = try scaffoldPaths(allocator, name, &relative_paths);
    defer freeScaffoldPaths(allocator, paths);
    const in_place_preflight = std.mem.eql(u8, name, ".");
    const preserve_gitignore = if (in_place_preflight) blk: {
        std.Io.Dir.access(std.Io.Dir.cwd(), io, paths[3], .{}) catch |err| switch (err) {
            error.FileNotFound => break :blk false,
            else => return err,
        };
        break :blk true;
    } else false;
    const preserved_path: ?[]const u8 = if (preserve_gitignore) paths[3] else null;
    try refuseExistingTargets(io, paths, preserved_path);

    const project_dir_created = try createScaffoldDir(io, name);
    errdefer if (project_dir_created) std.Io.Dir.cwd().deleteDir(io, name) catch {};
    const src_dir = try std.fmt.allocPrint(allocator, "{s}/src", .{name});
    defer allocator.free(src_dir);
    const src_dir_created = try createScaffoldDir(io, src_dir);
    errdefer if (src_dir_created) std.Io.Dir.cwd().deleteDir(io, src_dir) catch {};
    const tests_dir = try std.fmt.allocPrint(allocator, "{s}/tests", .{name});
    defer allocator.free(tests_dir);
    const tests_dir_created = try createScaffoldDir(io, tests_dir);
    errdefer if (tests_dir_created) std.Io.Dir.cwd().deleteDir(io, tests_dir) catch {};
    const public_dir = try std.fmt.allocPrint(allocator, "{s}/public", .{name});
    defer allocator.free(public_dir);
    const public_dir_created = try createScaffoldDir(io, public_dir);
    errdefer if (public_dir_created) std.Io.Dir.cwd().deleteDir(io, public_dir) catch {};

    var files_written: usize = 0;
    errdefer rollbackScaffoldFiles(io, paths[0..files_written], preserved_path);

    try writeProjectFile(allocator, paths[0], switch (template) {
        .basic, .api => defaultManifest,
        .htmx => htmxManifest,
    });
    files_written += 1;
    try writeProjectFile(allocator, paths[1], switch (template) {
        .basic => basicHandler,
        .api => apiHandler,
        .htmx => htmxHandler,
    });
    files_written += 1;
    try writeProjectFile(allocator, paths[2], switch (template) {
        .basic => basicTests,
        .api => apiTests,
        .htmx => htmxTests,
    });
    files_written += 1;
    if (!preserve_gitignore) try writeProjectFile(allocator, paths[3], gitignoreSource);
    files_written += 1;
    try writeProjectFile(allocator, paths[4], switch (template) {
        .basic => basicReadme,
        .api => apiReadme,
        .htmx => htmxReadme,
    });
    files_written += 1;
    try writeProjectFile(allocator, paths[5], "");
    files_written += 1;
}

fn scaffoldExtension(allocator: std.mem.Allocator, name: []const u8) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const relative_paths = [_][]const u8{
        "zttp-module.json",
        "src/root.zig",
        "build.zig",
        "build.zig.zon",
        "handler.ts",
        "README.md",
        ".gitignore",
    };
    const paths = try scaffoldPaths(allocator, name, &relative_paths);
    defer freeScaffoldPaths(allocator, paths);
    try refuseExistingTargets(io, paths, null);

    const extension_dir_created = try createScaffoldDir(io, name);
    errdefer if (extension_dir_created) std.Io.Dir.cwd().deleteDir(io, name) catch {};
    const src_dir = try std.fmt.allocPrint(allocator, "{s}/src", .{name});
    defer allocator.free(src_dir);
    const src_dir_created = try createScaffoldDir(io, src_dir);
    errdefer if (src_dir_created) std.Io.Dir.cwd().deleteDir(io, src_dir) catch {};

    var files_written: usize = 0;
    errdefer rollbackScaffoldFiles(io, paths[0..files_written], null);

    const manifest = try renderExtensionTemplate(allocator, io, extension_manifest_template, name);
    defer allocator.free(manifest);
    try writeProjectFile(allocator, paths[0], manifest);
    files_written += 1;

    const root_zig = try renderExtensionTemplate(allocator, io, extension_root_zig_template, name);
    defer allocator.free(root_zig);
    try writeProjectFile(allocator, paths[1], root_zig);
    files_written += 1;

    const build_zig = try renderExtensionTemplate(allocator, io, extension_build_zig_template, name);
    defer allocator.free(build_zig);
    try writeProjectFile(allocator, paths[2], build_zig);
    files_written += 1;

    const build_zon = try renderExtensionTemplate(allocator, io, extension_build_zon_template, name);
    defer allocator.free(build_zon);
    try writeProjectFile(allocator, paths[3], build_zon);
    files_written += 1;

    const handler_ts = try renderExtensionTemplate(allocator, io, extension_handler_ts_template, name);
    defer allocator.free(handler_ts);
    try writeProjectFile(allocator, paths[4], handler_ts);
    files_written += 1;

    const readme = try renderExtensionTemplate(allocator, io, extension_readme_template, name);
    defer allocator.free(readme);
    try writeProjectFile(allocator, paths[5], readme);
    files_written += 1;

    try writeProjectFile(allocator, paths[6], gitignoreSource);
    files_written += 1;
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
    const prefix = "zttp_ext_";
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
    std.debug.print("Initialized zttp extension in {s}\n", .{name});
    std.debug.print("Next steps:\n", .{});
    std.debug.print("  cd {s}\n", .{name});
    std.debug.print("  zts verify-module-manifest zttp-module.json\n", .{});
    std.debug.print("  zts extension-status --module-manifest zttp-module.json\n", .{});
    std.debug.print("  # adjust build.zig.zon to point at the version of zttp-sdk you depend on,\n", .{});
    std.debug.print("  # then `zig build` to compile the native binding.\n", .{});
}

/// Print the post-init welcome panel. `zttp dev` owns the first-run tour so
/// the tour marker is written inside the project on the first real dev run.
fn printInitNextSteps(name: []const u8, template: Template) void {
    const tty = shared.stderrIsTty();
    const c = shared.palette(tty);

    std.debug.print("\n", .{});
    std.debug.print("  {s}+{s} initialized zttp project in {s}{s}{s}\n", .{ c.green, c.reset, c.bold, name, c.reset });
    std.debug.print("  template: {s}\n", .{@tagName(template)});
    std.debug.print("  {s}----------------------------------------------------------------------{s}\n", .{ c.dim, c.reset });
    std.debug.print("\n", .{});
    std.debug.print("  created:\n", .{});
    std.debug.print("    zttp.json\n", .{});
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
    std.debug.print("    {s}zttp dev{s}          {s}# watch and prove on every save{s}\n", .{ c.cyan, c.reset, c.dim, c.reset });
    std.debug.print("    curl http://127.0.0.1:3000{s}\n", .{starterPathForTemplate(template)});
    std.debug.print("\n", .{});
    std.debug.print("    {s}zttp test{s}         {s}# run tests/handler.test.jsonl{s}\n", .{ c.cyan, c.reset, c.dim, c.reset });
    std.debug.print("    {s}zttp deploy{s}       {s}# build, prove, and deploy to .zttp/deploy/{s}{s}\n", .{ c.cyan, c.reset, c.dim, name, c.reset });
    std.debug.print("\n", .{});
}

/// `init --expert` scaffolds, then hands off to the expert agent. Keep this
/// short: the expert REPL paints its own banner right after.
fn printInitExpertHandoff(name: []const u8) void {
    const tty = shared.stderrIsTty();
    const c = shared.palette(tty);
    std.debug.print("\n", .{});
    std.debug.print("  {s}+{s} initialized zttp project in {s}{s}{s}\n", .{ c.green, c.reset, c.bold, name, c.reset });
    std.debug.print("  {s}entering expert mode - describe the handler you want{s}\n", .{ c.dim, c.reset });
    std.debug.print("\n", .{});
}

pub fn printInitHelp() void {
    const help =
        \\zttp init <name> [--template basic|api|htmx] [--expert]
        \\zttp init --extension <name>
        \\
        \\Create a new zttp project or extension directory.
        \\With --expert, scaffold the project and then enter `zttp expert`
        \\(the interactive compiler-in-the-loop agent) inside it.
        \\Names may contain letters, numbers, '-' and '_', and must start with
        \\a letter or number.
        \\
        \\Project files (default):
        \\  zttp.json
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
        \\  zttp-module.json     proof metadata for the partner module
        \\  src/root.zig           native Zig binding against zttp-sdk
        \\  build.zig
        \\  build.zig.zon
        \\  handler.ts             sample handler importing zttp-ext:<name>
        \\  .gitignore
        \\  README.md
        \\
        \\Examples:
        \\  zttp init my-app
        \\  zttp init my-app --expert
        \\  zttp init --extension my-partner
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn handlerPathForTemplate(template: Template) []const u8 {
    return switch (template) {
        .basic, .api => "src/handler.ts",
        .htmx => "src/handler.tsx",
    };
}

fn starterPathForTemplate(template: Template) []const u8 {
    return switch (template) {
        .api => "/health",
        .basic, .htmx => "/",
    };
}

fn writeProjectFile(allocator: std.mem.Allocator, path: []const u8, data: []const u8) !void {
    zts.file_io.writeFileCreateExclusive(allocator, path, data) catch |err| {
        if (err == error.PathAlreadyExists) {
            reportScaffoldConflict(path);
            return error.ScaffoldTargetExists;
        }
        return err;
    };
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

const basicHandler = zts_cli.proof_quest_fixture.starter_source;

const apiHandler =
    \\import type { Spec } from "zttp:types";
    \\
    \\// Author-declared guardrails. Declaring an explicit Spec scopes the proof
    \\// to these properties; without it every supported spec is active, and the
    \\// /echo route below (which reflects the request body) cannot discharge
    \\// pii_contained. Run `zttp check` to see them proven at compile time.
    \\type Guardrails = Spec<
    \\    | "deterministic"
    \\    | "no_secret_leakage"
    \\    | "injection_safe"
    \\>;
    \\
    \\function handler(req: Request): Response & Guardrails {
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
    \\import type { Spec } from "zttp:types";
    \\
    \\// Author-declared guardrails. Declaring an explicit Spec scopes the proof
    \\// to these properties; without it every supported spec is active and the
    \\// proof cannot be discharged. Run `zttp check` to see them proven at
    \\// compile time.
    \\type Guardrails = Spec<
    \\    | "deterministic"
    \\    | "no_secret_leakage"
    \\    | "injection_safe"
    \\>;
    \\
    \\function Page(): JSX.Element {
    \\    return (
    \\        <html>
    \\            <body>
    \\                <h1>zttp</h1>
    \\                <button hx-get="/fragment" hx-target="#slot" hx-swap="innerHTML">
    \\                    Load fragment
    \\                </button>
    \\                <div id="slot">ready</div>
    \\            </body>
    \\        </html>
    \\    );
    \\}
    \\
    \\function handler(req: Request): Response & Guardrails {
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
    \\{"type":"expect","status":200,"bodyContains":"zttp"}
;

const gitignoreSource =
    \\.zig-cache/
    \\zig-cache/
    \\zig-out/
    \\.zttp/
    \\.DS_Store
;

const basicReadme =
    \\# zttp app
    \\
    \\## Quick start
    \\
    \\1. Start the terminal proof loop:
    \\
    \\       zttp dev
    \\
    \\   On a fresh scaffold, the Proof Quest runs once. Press `b` to preview
    \\   a tiny edit that breaks `deterministic`, `y` to apply it, then `r`
    \\   and `y` to repair it. Use `zttp dev --quest` to replay it later.
    \\
    \\2. Edit `src/handler.ts` in your editor. The HUD re-verifies on save and
    \\   shows the verdict, proven surface, proof deltas, and spec diagnostics.
    \\   Press `Tab` or `l` to rotate proof lenses.
    \\
    \\3. Run the fixture:
    \\
    \\       zttp test
    \\
    \\4. When you are happy with the verdict, do a verified local deploy. It
    \\   builds a self-contained binary and records the proof in the ledger at
    \\   `.zttp/proofs.jsonl`:
    \\
    \\       zttp deploy
    \\       ./.zttp/deploy/<this-app-name>
    \\
    \\## More
    \\
    \\- `zttp expert` - interactive compiler-in-the-loop agent
    \\- `zttp help --all` - advanced commands
;

const apiReadme =
    \\# zttp API app
    \\
    \\## Quick start
    \\
    \\1. Start the local dev loop:
    \\
    \\       zttp dev
    \\
    \\2. Try the starter endpoints:
    \\
    \\       curl http://127.0.0.1:3000/health
    \\       curl -X POST http://127.0.0.1:3000/echo -d 'hello'
    \\
    \\3. Run the fixture:
    \\
    \\       zttp test
    \\
    \\4. Deploy locally (builds the binary and records the proof ledger entry):
    \\
    \\       zttp deploy
    \\
    \\## More
    \\
    \\- `zttp expert` - interactive compiler-in-the-loop agent
    \\- `zttp help --all` - advanced commands
;

const htmxReadme =
    \\# zttp HTMX app
    \\
    \\## Quick start
    \\
    \\1. Start the TSX/HTMX dev loop:
    \\
    \\       zttp dev
    \\
    \\2. Edit `src/handler.tsx`, then open the page:
    \\
    \\       http://127.0.0.1:3000/
    \\
    \\3. Run the fixture:
    \\
    \\       zttp test
    \\
    \\4. Deploy locally (builds the binary and records the proof ledger entry):
    \\
    \\       zttp deploy
    \\
    \\## More
    \\
    \\- `zttp expert` - interactive compiler-in-the-loop agent
    \\- `zttp help --all` - advanced commands
;

// `{{name}}` is replaced by the extension name at scaffold time.
const extension_manifest_template =
    \\{
    \\  "schemaVersion": 1,
    \\  "specifier": "zttp-ext:{{name}}",
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
    \\const sdk = @import("zttp-sdk");
    \\
    \\pub const binding = sdk.ModuleBinding{
    \\    .specifier = "zttp-ext:{{name}}",
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
    \\    const sdk_dep = b.dependency("zttp_sdk", .{
    \\        .target = target,
    \\        .optimize = optimize,
    \\    });
    \\
    \\    _ = b.addModule("{{name}}", .{
    \\        .root_source_file = b.path("src/root.zig"),
    \\        .target = target,
    \\        .optimize = optimize,
    \\        .imports = &.{
    \\            .{ .name = "zttp-sdk", .module = sdk_dep.module("zttp-sdk") },
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
    \\        // Replace this placeholder with the version of zttp-sdk you depend on.
    \\        // For monorepo use, set `.path = "../zttp-sdk"`. For external use,
    \\        // run `zig fetch --save <url>` to pin a release tarball.
    \\        .zttp_sdk = .{
    \\            .path = "../zttp-sdk",
    \\        },
    \\    },
    \\    .paths = .{
    \\        "build.zig",
    \\        "build.zig.zon",
    \\        "src",
    \\        "zttp-module.json",
    \\    },
    \\}
    \\
;

const extension_handler_ts_template =
    \\import { greet } from "zttp-ext:{{name}}";
    \\
    \\function handler(req) {
    \\    if (req.method === "GET" && req.path === "/") {
    \\        return Response.text(greet("zttp"));
    \\    }
    \\    return Response.text("Not Found", { status: 404 });
    \\}
    \\
;

const extension_readme_template =
    \\# zttp-ext:{{name}}
    \\
    \\A minimal zttp extension scaffolded by `zttp init --extension {{name}}`.
    \\
    \\## Files
    \\
    \\- `zttp-module.json` - proof metadata for tools like
    \\  `zts verify-module-manifest` and `zts extension-status`.
    \\- `src/root.zig` - the native Zig binding that implements the export.
    \\- `build.zig` / `build.zig.zon` - Zig build wiring against `zttp-sdk`.
    \\- `handler.ts` - a sibling handler that imports `zttp-ext:{{name}}` so
    \\  you can verify end-to-end with `zts check` once the manifest is
    \\  registered via `--module-manifest`.
    \\
    \\## Validate the manifest
    \\
    \\    zts verify-module-manifest zttp-module.json
    \\    zts extension-status --module-manifest zttp-module.json
    \\
    \\## Wire up `zttp-sdk`
    \\
    \\`build.zig.zon` ships with a `path = "../zttp-sdk"` placeholder. For
    \\external use, swap it for a `zig fetch --save <url>` line that pins a
    \\released tarball. Then run `zig build` to compile the binding.
    \\
;

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
        "demoext/zttp-module.json",
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

    const manifest_bytes = try zts.file_io.readFile(testing.allocator, "demoext/zttp-module.json", 256 * 1024);
    defer testing.allocator.free(manifest_bytes);
    var manifest = try zts.module_manifest.parse(testing.allocator, manifest_bytes);
    defer manifest.deinit(testing.allocator);
    try testing.expectEqualStrings("zttp-ext:demoext", manifest.specifier);
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

    const build_zon = try zts.file_io.readFile(testing.allocator, "demo-ext/build.zig.zon", 256 * 1024);
    defer testing.allocator.free(build_zon);
    try testing.expect(std.mem.indexOf(u8, build_zon, ".name = .zttp_ext_demo_ext") != null);
    try testing.expect(std.mem.indexOf(u8, build_zon, ".fingerprint = 0x") != null);
}

test "init --extension package fingerprint uses package name checksum" {
    const testing = std.testing;
    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    const package_name = try extensionPackageName(testing.allocator, "Demo-Ext");
    defer testing.allocator.free(package_name);

    try testing.expectEqualStrings("zttp_ext_demo_ext", package_name);
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
    try testing.expect(std.mem.startsWith(u8, package_name, "zttp_ext_very"));
    try testing.expect(std.mem.indexOfScalar(u8, package_name, '-') == null);
}

test "init --extension preserves a pre-existing build.zig" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try std.Io.Dir.cwd().createDirPath(io, "alpha");
    try zts.file_io.writeFileCreateExclusive(testing.allocator, "alpha/build.zig", "user build\n");

    try testing.expectError(error.ScaffoldTargetExists, scaffoldExtension(testing.allocator, "alpha"));
    const build_zig = try zts.file_io.readFile(testing.allocator, "alpha/build.zig", 1024);
    defer testing.allocator.free(build_zig);
    try testing.expectEqualStrings("user build\n", build_zig);
    try testing.expectError(error.FileNotFound, std.Io.Dir.access(std.Io.Dir.cwd(), io, "alpha/zttp-module.json", .{}));
}

test "init preserves a pre-existing README" {
    const testing = std.testing;
    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try std.Io.Dir.cwd().createDirPath(io, "demo");
    try zts.file_io.writeFileCreateExclusive(testing.allocator, "demo/README.md", "user readme\n");

    try testing.expectError(error.ScaffoldTargetExists, scaffoldProject(testing.allocator, "demo", .basic));
    const readme = try zts.file_io.readFile(testing.allocator, "demo/README.md", 1024);
    defer testing.allocator.free(readme);
    try testing.expectEqualStrings("user readme\n", readme);
    try testing.expectError(error.FileNotFound, std.Io.Dir.access(std.Io.Dir.cwd(), io, "demo/zttp.json", .{}));
}

test "studio scaffold preserves a pre-existing gitignore in a dotfile-only cwd" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try zts.file_io.writeFileCreateExclusive(testing.allocator, ".gitignore", "user rules\n");
    try scaffoldProject(testing.allocator, ".", .basic);

    const contents = try zts.file_io.readFile(testing.allocator, ".gitignore", 1024);
    defer testing.allocator.free(contents);
    try testing.expectEqualStrings("user rules\n", contents);
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "zttp.json", .{});
}

test "init refuses a pre-existing gitignore in a named project" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try std.Io.Dir.cwd().createDirPath(io, "demo");
    try zts.file_io.writeFileCreateExclusive(testing.allocator, "demo/.gitignore", "user rules\n");
    try testing.expectError(error.ScaffoldTargetExists, scaffoldProject(testing.allocator, "demo", .basic));

    const contents = try zts.file_io.readFile(testing.allocator, "demo/.gitignore", 1024);
    defer testing.allocator.free(contents);
    try testing.expectEqualStrings("user rules\n", contents);
}

test "init preserves a pre-existing handler" {
    const testing = std.testing;
    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try std.Io.Dir.cwd().createDirPath(io, "demo/src");
    try zts.file_io.writeFileCreateExclusive(testing.allocator, "demo/src/handler.ts", "user handler\n");

    try testing.expectError(error.ScaffoldTargetExists, scaffoldProject(testing.allocator, "demo", .basic));
    const handler = try zts.file_io.readFile(testing.allocator, "demo/src/handler.ts", 1024);
    defer testing.allocator.free(handler);
    try testing.expectEqualStrings("user handler\n", handler);
    try testing.expectError(error.FileNotFound, std.Io.Dir.access(std.Io.Dir.cwd(), io, "demo/zttp.json", .{}));
}

test "init scaffolds into a clean existing directory" {
    const testing = std.testing;
    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    try std.Io.Dir.cwd().createDirPath(io, "demo");
    try scaffoldProject(testing.allocator, "demo", .basic);

    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "demo/zttp.json", .{});
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "demo/src/handler.ts", .{});
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "demo/README.md", .{});
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
        "demo/zttp.json",
        "demo/src/handler.ts",
        "demo/tests/handler.test.jsonl",
        "demo/public/.keep",
        "demo/.gitignore",
        "demo/README.md",
    };
    for (expected) |path| {
        try std.Io.Dir.access(std.Io.Dir.cwd(), io, path, .{});
    }

    const handler = try zts.file_io.readFile(testing.allocator, "demo/src/handler.ts", 64 * 1024);
    defer testing.allocator.free(handler);
    try testing.expect(std.mem.indexOf(u8, handler, "function handler(req: Request): Response") != null);
}

test "initCommand --expert scaffolds and asks the dispatcher to enter expert mode" {
    const testing = std.testing;

    var io_backend = std.Io.Threaded.init(testing.allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try @import("proof_ledger.zig").chdirTmpForTest(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const outcome = try initCommand(testing.allocator, &.{ "demo", "--expert" });
    try testing.expect(outcome.enter_expert);
    try testing.expectEqualStrings("demo", outcome.project_name.?);

    // --expert still scaffolds the project before the handoff.
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "demo/src/handler.ts", .{});
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "demo/tests/handler.test.jsonl", .{});

    // Without --expert there is no handoff.
    const plain = try initCommand(testing.allocator, &.{"demo2"});
    try testing.expect(!plain.enter_expert);
    try testing.expect(plain.project_name == null);
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

    const api_readme = try zts.file_io.readFile(testing.allocator, "api-demo/README.md", 64 * 1024);
    defer testing.allocator.free(api_readme);
    const htmx_readme = try zts.file_io.readFile(testing.allocator, "htmx-demo/README.md", 64 * 1024);
    defer testing.allocator.free(htmx_readme);
    const htmx_manifest = try zts.file_io.readFile(testing.allocator, "htmx-demo/zttp.json", 64 * 1024);
    defer testing.allocator.free(htmx_manifest);

    try testing.expect(std.mem.indexOf(u8, api_readme, "# zttp API app") != null);
    try testing.expect(std.mem.indexOf(u8, api_readme, "curl http://127.0.0.1:3000/health") != null);
    try testing.expect(std.mem.indexOf(u8, htmx_readme, "# zttp HTMX app") != null);
    try testing.expect(std.mem.indexOf(u8, htmx_readme, "src/handler.tsx") != null);
    try testing.expect(std.mem.indexOf(u8, htmx_readme, "zttp test") != null);
    try testing.expect(std.mem.indexOf(u8, htmx_manifest, "\"entry\": \"src/handler.tsx\"") != null);
    try std.Io.Dir.access(std.Io.Dir.cwd(), io, "htmx-demo/src/handler.tsx", .{});
    try testing.expectError(error.FileNotFound, std.Io.Dir.access(std.Io.Dir.cwd(), io, "htmx-demo/src/handler.ts", .{}));
}
