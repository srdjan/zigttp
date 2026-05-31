//! `zigttp demo` — build a self-contained Proof Theater workspace and either
//! launch Studio interactively or run the scripted baseline -> witness ->
//! repair -> deploy flow and exit. Split out of dev_cli.zig; the dispatch and
//! error-to-exit-code translation stay in dev_cli.main.

const std = @import("std");
const builtin = @import("builtin");

const shared = @import("cli_shared.zig");
const demo = @import("demo.zig");
const pi_app = @import("pi_app");
const cli_paths = @import("cli_paths.zig");
const resolveDeveloperServeBinary = cli_paths.resolveDeveloperServeBinary;
const resolveReentryBinaryAfterChdir = cli_paths.resolveReentryBinaryAfterChdir;

const DemoArgs = struct {
    no_open: bool = false,
    scripted: bool = false,
    port: u16 = 3000,
    out_dir: ?[]const u8 = null,
    export_dir: ?[]const u8 = null,
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
        if (std.mem.eql(u8, arg, "--scripted")) {
            parsed.scripted = true;
            parsed.no_open = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--port")) {
            const value = try shared.takeArg(&i, argv, error.MissingOptionValue);
            parsed.port = std.fmt.parseInt(u16, value, 10) catch return error.InvalidPort;
            if (parsed.port == 0) return error.InvalidPort;
            continue;
        }
        if (std.mem.eql(u8, arg, "--out")) {
            parsed.out_dir = try shared.takeArg(&i, argv, error.MissingOptionValue);
            continue;
        }
        if (std.mem.eql(u8, arg, "--export")) {
            parsed.export_dir = try shared.takeArg(&i, argv, error.MissingOptionValue);
            continue;
        }
        return error.UnknownOption;
    }
    return parsed;
}

pub fn demoCommand(allocator: std.mem.Allocator, program_path: []const u8, argv: []const []const u8) !void {
    const parsed = try parseDemoArgs(argv);

    var workspace = try demo.createWorkspace(allocator, parsed.out_dir, parsed.port);
    defer workspace.deinit(allocator);
    defer if (!parsed.scripted) workspace.cleanup(allocator);
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
        \\Expert:    {s}
        \\Verify:    zigttp verify {s}
        \\Well-known:{s}
        \\
        \\Flow: baseline -> unsafe edit -> witness -> repair -> local deploy receipt
        \\
    , .{ workspace.root, url, passport.expert_command, verify_url, well_known_url });

    if (parsed.scripted) {
        const workspace_root = try resolveDemoPathFrom(allocator, old_cwd, workspace.root);
        defer allocator.free(workspace_root);
        const handler_path = try std.fs.path.join(allocator, &.{ workspace_root, "src", "handler.tsx" });
        defer allocator.free(handler_path);
        const export_dir = if (parsed.export_dir) |path|
            try resolveDemoPathFrom(allocator, old_cwd, path)
        else
            null;
        defer if (export_dir) |path| allocator.free(path);

        try std.Io.Threaded.chdir(workspace_root);
        const config: demo.Config = .{
            .workspace_root = workspace_root,
            .handler_path = handler_path,
        };
        _ = try demo.applyAction(allocator, config, .introduce_bug);
        _ = try demo.applyAction(allocator, config, .repair_bug);
        const final_step = try demo.applyAction(allocator, config, .deploy);
        if (export_dir) |path| {
            try demo.exportPassport(allocator, config, .{
                .out_dir = path,
                .step = final_step,
            });
            std.debug.print("Passport:  {s}\n", .{path});
        }
        std.debug.print("Scripted:  complete ({s})\n", .{final_step.toString()});
        return;
    }

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

fn resolveDemoPathFrom(allocator: std.mem.Allocator, base_dir: []const u8, path: []const u8) ![]u8 {
    if (std.fs.path.isAbsolute(path)) return try allocator.dupe(u8, path);
    return try std.fs.path.resolve(allocator, &.{ base_dir, path });
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

pub fn printDemoHelp() void {
    const help =
        \\zigttp demo [--no-open] [--port N] [--out DIR]
        \\zigttp demo --scripted --out DIR [--export DIR]
        \\
        \\Create a self-contained Proof Theater workspace and launch Studio.
        \\The demo runs fully local: no cloud credentials, API keys, or
        \\network services are required.
        \\
        \\Options:
        \\  --no-open       Do not try to open the browser
        \\  --port <PORT>   Studio port (default: 3000)
        \\  --out <DIR>     Write the demo project to DIR. Refuses to overwrite.
        \\  --scripted      Run baseline -> witness -> repair -> deploy and exit.
        \\  --export <DIR>  Write an offline Proof Passport export.
        \\  --help          Show this help
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
