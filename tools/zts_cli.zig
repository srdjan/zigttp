const std = @import("std");

pub const precompile = @import("precompile.zig");
const prove = @import("prove.zig");
const mock = @import("mock_server.zig");
const system_build = @import("system_build.zig");
const project_config_mod = @import("project_config");
const zts = @import("zts");
const zts_file_io = zts.file_io;
const writeContractJson = zts.handler_contract.writeContractJson;

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
    if (std.mem.eql(u8, command, "link")) {
        try system_build.runWithArgs(allocator, argv[1..]);
        return;
    }

    printHelp();
    return error.UnknownCommand;
}

fn runCheckCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var sql_schema_path: ?[]const u8 = null;
    var handler_path: ?[]const u8 = null;
    var emit_contract = false;
    var emit_types = false;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--sql-schema")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            sql_schema_path = argv[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "--contract")) {
            emit_contract = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--types")) {
            emit_types = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--help")) {
            printCheckHelp();
            return;
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

    var result = try precompile.runCheckOnly(allocator, target, sql_schema_path);
    defer result.deinit(allocator);

    {
        var card_buf: std.ArrayList(u8) = .empty;
        defer card_buf.deinit(allocator);
        var card_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &card_buf);
        precompile.formatProofCard(&card_aw.writer, &result, target);
        card_buf = card_aw.toArrayList();
        if (card_buf.items.len > 0) {
            _ = std.c.write(std.c.STDERR_FILENO, card_buf.items.ptr, card_buf.items.len);
        }
    }

    if (emit_contract) {
        if (result.contract) |contract| {
            var json_output: std.ArrayList(u8) = .empty;
            defer json_output.deinit(allocator);
            var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &json_output);
            writeContractJson(&contract, &aw.writer) catch |err| {
                std.debug.print("Error serializing contract: {}\n", .{err});
                return err;
            };
            json_output = aw.toArrayList();
            zts_file_io.writeFile(allocator, "contract.json", json_output.items) catch |err| {
                std.debug.print("Error writing contract.json: {}\n", .{err});
                return err;
            };
            std.debug.print("Wrote contract.json\n", .{});
        }
    }

    if (emit_types) {
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
        precompile.generateTypeDefs(&aw.writer);
        buf = aw.toArrayList();
        zts_file_io.writeFile(allocator, "zigttp.d.ts", buf.items) catch |err| {
            std.debug.print("Error writing zigttp.d.ts: {}\n", .{err});
            return err;
        };
        std.debug.print("Wrote zigttp.d.ts\n", .{});
    }

    if (result.totalErrors() > 0) std.process.exit(1);
    if (result.totalWarnings() > 0) std.process.exit(2);
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

pub fn collectArgs(allocator: std.mem.Allocator, args_vector: std.process.Args) ![]const []const u8 {
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
        \\  zts check [handler.ts] [--contract] [--types] [--sql-schema path]
        \\  zts compile [precompile flags] <handler.ts> <output.zig>
        \\  zts prove <old-contract.json> <new-contract.json> [output-dir/]
        \\  zts mock <tests.jsonl> [--port PORT]
        \\  zts link <system.json> [--output-dir <dir>]
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printCheckHelp() void {
    const help =
        \\zts check - verify handler and show proof card
        \\
        \\Usage: zts check [handler.ts] [options]
        \\
        \\Options:
        \\  --contract       Emit contract.json in current directory
        \\  --types          Emit zigttp.d.ts type definitions for IDE autocomplete
        \\  --sql-schema P   SQLite schema file for query validation
        \\
        \\If no handler is specified, uses the entry from zigttp.json.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
