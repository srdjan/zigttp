const std = @import("std");

const precompile = @import("precompile.zig");
const prove = @import("prove.zig");
const mock = @import("mock_server.zig");
const project_config_mod = @import("project_config");

pub fn main(init: std.process.Init.Minimal) !void {
    const allocator = std.heap.smp_allocator;
    const argv = try collectArgs(allocator, init.args);
    defer {
        for (argv) |arg| allocator.free(arg);
        allocator.free(argv);
    }
    try run(allocator, argv[1..]);
}

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len == 0 or std.mem.eql(u8, argv[0], "--help") or std.mem.eql(u8, argv[0], "help")) {
        printHelp();
        return;
    }

    const command = argv[0];
    if (std.mem.eql(u8, command, "compile")) {
        try precompile.runCompileWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "check")) {
        try runCheckCommand(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "prove")) {
        try prove.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "mock")) {
        try mock.runWithArgs(allocator, argv[1..]);
        return;
    }

    printHelp();
    return error.UnknownCommand;
}

fn runCheckCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var sql_schema_path: ?[]const u8 = null;
    var handler_path: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--sql-schema")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            sql_schema_path = argv[i];
            continue;
        }
        if (!std.mem.startsWith(u8, arg, "-") and handler_path == null) {
            handler_path = arg;
            continue;
        }
        return error.InvalidArgument;
    }

    const target = if (handler_path) |path|
        path
    else
        try defaultProjectEntry(allocator);
    defer if (handler_path == null) allocator.free(target);

    try precompile.runCheck(allocator, target, sql_schema_path);
}

fn defaultProjectEntry(allocator: std.mem.Allocator) ![]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var project = try project_config_mod.discover(allocator, io, null);
    defer if (project) |*p| p.deinit(allocator);

    if (project) |*cfg| {
        return try cfg.resolvedEntry(allocator);
    }
    return error.NoProjectConfig;
}

fn collectArgs(allocator: std.mem.Allocator, args_vector: std.process.Args) ![]const []const u8 {
    var args_iter = std.process.Args.Iterator.init(args_vector);
    defer args_iter.deinit();

    var args = std.ArrayList([]const u8).empty;
    errdefer args.deinit(allocator);
    while (args_iter.next()) |arg| {
        try args.append(allocator, try allocator.dupe(u8, arg));
    }
    return args.toOwnedSlice(allocator);
}

fn printHelp() void {
    const help =
        \\zts - compiler and analysis tools for zigttp handlers
        \\
        \\Usage:
        \\  zts check [handler.ts] [--sql-schema path]
        \\  zts compile [precompile flags] <handler.ts> <output.zig>
        \\  zts prove <old-contract.json> <new-contract.json> [output-dir/]
        \\  zts mock <tests.jsonl> [--port PORT]
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
