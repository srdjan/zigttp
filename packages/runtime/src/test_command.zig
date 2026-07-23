//! `zttp test` — run declarative handler tests for the current project after
//! a mandatory pre-test analyzer pass. Split out of dev_cli.zig; the dispatch
//! and error-to-exit-code translation stay in dev_cli.main.

const std = @import("std");
const builtin = @import("builtin");

const project_config_mod = @import("project_config");
const runtime_cli = @import("runtime_cli.zig");
const cli_doctor = @import("cli_doctor.zig");
const doctorPathExists = cli_doctor.doctorPathExists;
const runDoctorAnalyzerForProject = cli_doctor.runDoctorAnalyzerForProject;
const printCheckStageFailures = cli_doctor.printCheckStageFailures;

pub fn testCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
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
        std.debug.print("zttp test cannot run with multiple outboundHosts; current runtime accepts one.\n", .{});
        return error.UnsupportedMultipleOutboundHosts;
    }

    const test_path = if (explicit_test_path) |path|
        try allocator.dupe(u8, path)
    else
        try std.fs.path.resolve(allocator, &.{ cfg.root_dir, "tests", "handler.test.jsonl" });
    defer allocator.free(test_path);

    if (!doctorPathExists(io, test_path)) {
        std.debug.print("Test fixture not found: {s}\n", .{test_path});
        std.debug.print("Run `zttp gen-tests` or create tests/handler.test.jsonl.\n", .{});
        return error.FileNotFound;
    }

    const entry = try cfg.resolvedEntry(allocator);
    defer allocator.free(entry);
    if (!doctorPathExists(io, entry)) {
        std.debug.print("Handler not found: {s}\n", .{entry});
        std.debug.print("Next: update `entry` in zttp.json or create the handler file.\n", .{});
        return error.FileNotFound;
    }

    const sqlite_path = try cfg.resolvedSqlitePath(allocator);
    defer if (sqlite_path) |path| allocator.free(path);
    var check = runDoctorAnalyzerForProject(allocator, cfg, entry, sqlite_path) catch |err| {
        if (!builtin.is_test) {
            std.debug.print("Pre-test check could not run: {}\n", .{err});
            std.debug.print("Next: run `zttp check` for full diagnostics.\n", .{});
        }
        return error.CheckFailed;
    };
    defer check.deinit(allocator);
    if (check.totalErrors() > 0) {
        if (!builtin.is_test) {
            std.debug.print("Pre-test check failed for {s}: {d} error(s)\n", .{ entry, check.totalErrors() });
            printCheckStageFailures(&check, "  ");
            std.debug.print("Next: fix the errors above, then rerun `zttp test`.\n", .{});
        }
        return error.CheckFailed;
    }

    var serve_args = [_][]const u8{ "--test", test_path };
    var serve_arena: std.heap.ArenaAllocator = .init(allocator);
    defer serve_arena.deinit();
    try runtime_cli.serveCommand(serve_arena.allocator(), &serve_args);
}

pub fn printTestHelp() void {
    const help =
        \\zttp test [tests.jsonl]
        \\
        \\Run declarative handler tests for the current project. If no test
        \\file is passed, zttp reads tests/handler.test.jsonl under the
        \\project root discovered from zttp.json.
        \\
        \\Examples:
        \\  zttp test
        \\  zttp test tests/handler.test.jsonl
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
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
        .sub_path = "zttp.json",
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
        .sub_path = "zttp.json",
        .data =
        \\{
        \\  "entry": "src/handler.ts"
        \\}
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "src/handler.ts",
        .data =
        \\function handler(req: Request): Response & Spec<"deterministic"> {
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
