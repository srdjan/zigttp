//! `zigttp init` — scaffold a new project or partner extension. This module
//! owns all scaffolding: the project templates, the extension templates, and
//! the file writers. The studio/dev preflight reuses `scaffoldProject` to
//! scaffold an empty cwd in place, so that helper is public. Dispatch and
//! error-to-exit-code translation stay in dev_cli.main.

const std = @import("std");
const builtin = @import("builtin");

const zigts = @import("zigts");
const zigts_cli = @import("zigts_cli");
const shared = @import("cli_shared.zig");
const cli_templates = @import("cli_templates.zig");
const Template = cli_templates.Template;
const parseTemplate = cli_templates.parseTemplate;

/// What `initCommand` decided after scaffolding. `enter_expert` asks the
/// dispatcher to chdir into the new project and launch `zigttp expert` (the
/// `--expert` flag); `project_name` borrows from argv.
pub const InitOutcome = struct {
    enter_expert: bool = false,
    project_name: ?[]const u8 = null,
};

pub fn initCommand(allocator: std.mem.Allocator, argv: []const []const u8) !InitOutcome {
    if (argv.len == 0) return error.MissingProjectName;

    var template_name: []const u8 = "basic";
    var project_name: ?[]const u8 = null;
    var extension_name: ?[]const u8 = null;
    var enter_expert = false;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return error.HelpRequested;
        }
        if (std.mem.eql(u8, arg, "--template")) {
            template_name = try shared.takeArg(&i, argv, error.MissingTemplate);
            continue;
        }
        if (std.mem.eql(u8, arg, "--extension")) {
            extension_name = try shared.takeArg(&i, argv, error.MissingExtensionName);
            continue;
        }
        if (std.mem.eql(u8, arg, "--expert")) {
            enter_expert = true;
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
        return .{};
    }

    const name = project_name orelse return error.MissingProjectName;
    try validateProjectName(name);
    const template = parseTemplate(template_name) orelse return error.InvalidTemplate;

    try scaffoldProject(allocator, name, template);
    if (enter_expert) {
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

pub fn scaffoldProject(allocator: std.mem.Allocator, name: []const u8, template: Template) !void {
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
    std.debug.print("    {s}zigttp dev{s}          {s}# watch and prove on every save{s}\n", .{ c.cyan, c.reset, c.dim, c.reset });
    std.debug.print("    curl http://127.0.0.1:3000{s}\n", .{starterPathForTemplate(template)});
    std.debug.print("\n", .{});
    std.debug.print("    {s}zigttp test{s}         {s}# run tests/handler.test.jsonl{s}\n", .{ c.cyan, c.reset, c.dim, c.reset });
    std.debug.print("    {s}zigttp deploy{s}       {s}# build, prove, and deploy to .zigttp/deploy/{s}{s}\n", .{ c.cyan, c.reset, c.dim, name, c.reset });
    std.debug.print("\n", .{});
}

/// `init --expert` scaffolds, then hands off to the expert agent. Keep this
/// short: the expert REPL paints its own banner right after.
fn printInitExpertHandoff(name: []const u8) void {
    const tty = shared.stderrIsTty();
    const c = shared.palette(tty);
    std.debug.print("\n", .{});
    std.debug.print("  {s}+{s} initialized zigttp project in {s}{s}{s}\n", .{ c.green, c.reset, c.bold, name, c.reset });
    std.debug.print("  {s}entering expert mode - describe the handler you want{s}\n", .{ c.dim, c.reset });
    std.debug.print("\n", .{});
}

pub fn printInitHelp() void {
    const help =
        \\zigttp init <name> [--template basic|api|htmx] [--expert]
        \\zigttp init --extension <name>
        \\
        \\Create a new zigttp project or extension directory.
        \\With --expert, scaffold the project and then enter `zigttp expert`
        \\(the interactive compiler-in-the-loop agent) inside it.
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
        \\  zigttp init my-app --expert
        \\  zigttp init --extension my-partner
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
    \\import type { Spec } from "zigttp:types";
    \\
    \\// Author-declared guardrails. Declaring an explicit Spec scopes the proof
    \\// to these properties; without it every supported spec is active, and the
    \\// /echo route below (which reflects the request body) cannot discharge
    \\// pii_contained. Run `zigttp check` to see them proven at compile time.
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
    \\import type { Spec } from "zigttp:types";
    \\
    \\// Author-declared guardrails. Declaring an explicit Spec scopes the proof
    \\// to these properties; without it every supported spec is active and the
    \\// proof cannot be discharged. Run `zigttp check` to see them proven at
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
