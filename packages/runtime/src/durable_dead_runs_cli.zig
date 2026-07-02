//! CLI verbs for inspecting and acting on durable-run dead-run records.
//! Mirrors `workflow_queue_cli.zig`'s shape exactly (Command/Options/
//! parseOptions/runWith), covering a sibling dead-letter surface: durable
//! runs that permanently fail crash recovery, rather than queued child
//! dispatch. See `durable_dead_runs.zig` for the on-disk record format.

const std = @import("std");

const shared = @import("cli_shared.zig");
const dead_runs = @import("durable_dead_runs.zig");

const Command = enum {
    list,
    show,
    replay,
    discard,
    help,
};

const Options = struct {
    command: Command = .help,
    durable_dir: ?[]const u8 = null,
    id: ?[]const u8 = null,
};

pub fn isExpectedUserError(err: anyerror) bool {
    return switch (err) {
        error.MissingArgValue,
        error.MissingDurableDir,
        error.MissingId,
        error.InvalidDeadRunId,
        error.NotFound,
        error.TooManyArguments,
        error.UnknownArgument,
        error.UnknownSubcommand,
        error.DeadRunMissing,
        error.DeadRunAlreadyDiscarded,
        error.InvalidDeadRunRecord,
        error.DeleteFailed,
        => true,
        else => false,
    };
}

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(allocator);
    defer err.deinit();

    runWith(allocator, argv, &out.writer, &err.writer) catch |e| {
        writeFd(std.c.STDOUT_FILENO, out.writer.buffered());
        writeFd(std.c.STDERR_FILENO, err.writer.buffered());
        return e;
    };
    writeFd(std.c.STDOUT_FILENO, out.writer.buffered());
    writeFd(std.c.STDERR_FILENO, err.writer.buffered());
}

pub fn runWith(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    const opts = parseOptions(argv) catch |err| {
        try stderr.writeAll("zigttp durable dead-runs: invalid arguments\n\n");
        try writeHelp(stderr);
        return err;
    };

    if (opts.command == .help) {
        try writeHelp(stdout);
        return;
    }

    const durable_dir = opts.durable_dir orelse {
        try stderr.writeAll("zigttp durable dead-runs: --durable <DIR> is required\n");
        return error.MissingDurableDir;
    };
    if (opts.id) |id| {
        if (!dead_runs.isValidDeadRunId(id)) {
            try stderr.writeAll("zigttp durable dead-runs: id must use letters, numbers, '-' or '_'\n");
            return error.InvalidDeadRunId;
        }
    }

    switch (opts.command) {
        .help => unreachable,
        .list => try listCommand(allocator, durable_dir, stdout),
        .show => try showCommand(allocator, durable_dir, opts.id orelse return missingId(stderr), stdout, stderr),
        .replay => try replayCommand(allocator, durable_dir, opts.id orelse return missingId(stderr), stdout, stderr),
        .discard => try discardCommand(allocator, durable_dir, opts.id orelse return missingId(stderr), stdout, stderr),
    }
}

fn parseOptions(argv: []const []const u8) !Options {
    var opts: Options = .{};
    var command_seen = false;
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "help")) {
            opts.command = .help;
            return opts;
        }
        if (std.mem.eql(u8, arg, "--durable")) {
            opts.durable_dir = try shared.takeArg(&i, argv, error.MissingArgValue);
            continue;
        }
        if (std.mem.startsWith(u8, arg, "-")) return error.UnknownArgument;
        if (!command_seen) {
            opts.command = parseCommand(arg) orelse return error.UnknownSubcommand;
            command_seen = true;
            continue;
        }
        if (opts.id != null) return error.TooManyArguments;
        opts.id = arg;
    }

    switch (opts.command) {
        .show, .replay, .discard => if (opts.id == null) return error.MissingId,
        .list, .help => if (opts.id != null) return error.TooManyArguments,
    }

    return opts;
}

fn parseCommand(arg: []const u8) ?Command {
    if (std.mem.eql(u8, arg, "list")) return .list;
    if (std.mem.eql(u8, arg, "show")) return .show;
    if (std.mem.eql(u8, arg, "replay")) return .replay;
    if (std.mem.eql(u8, arg, "discard")) return .discard;
    return null;
}

fn missingId(stderr: *std.Io.Writer) !void {
    try stderr.writeAll("zigttp durable dead-runs: id is required\n");
    return error.MissingId;
}

fn listCommand(allocator: std.mem.Allocator, durable_dir: []const u8, stdout: *std.Io.Writer) !void {
    const ids = try dead_runs.listDeadRunIds(allocator, durable_dir);
    defer {
        for (ids) |id| allocator.free(id);
        allocator.free(ids);
    }

    if (ids.len == 0) {
        try stdout.writeAll("(none)\n");
        return;
    }
    for (ids) |id| try stdout.print("{s}\n", .{id});
}

fn showCommand(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    id: []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    const payload = (try dead_runs.readDeadRun(allocator, durable_dir, id)) orelse {
        try stderr.print("zigttp durable dead-runs: not found: {s}\n", .{id});
        return error.NotFound;
    };
    defer allocator.free(payload);
    try stdout.print("{s}\n", .{payload});
}

fn replayCommand(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    id: []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    dead_runs.replayDeadRun(allocator, durable_dir, id) catch |err| {
        try writeActionError(stderr, id, err);
        return err;
    };
    try stdout.print("replayed {s}\n", .{id});
}

fn discardCommand(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    id: []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    dead_runs.discardDeadRun(allocator, durable_dir, id) catch |err| {
        try writeActionError(stderr, id, err);
        return err;
    };
    try stdout.print("discarded {s}\n", .{id});
}

fn writeActionError(stderr: *std.Io.Writer, id: []const u8, err: anyerror) !void {
    switch (err) {
        error.DeadRunMissing => {
            try stderr.print("zigttp durable dead-runs: not found: {s}\n", .{id});
        },
        error.DeadRunAlreadyDiscarded => {
            try stderr.print("zigttp durable dead-runs: already discarded, cannot replay: {s}\n", .{id});
        },
        error.InvalidDeadRunRecord => {
            try stderr.print("zigttp durable dead-runs: record is corrupt: {s}\n", .{id});
        },
        else => {},
    }
}

fn writeHelp(writer: *std.Io.Writer) !void {
    try writer.writeAll(
        \\zigttp durable dead-runs - inspect durable runs that permanently failed recovery
        \\
        \\Usage:
        \\  zigttp durable dead-runs list --durable <DIR>
        \\  zigttp durable dead-runs show --durable <DIR> <ID>
        \\  zigttp durable dead-runs replay --durable <DIR> <ID>
        \\  zigttp durable dead-runs discard --durable <DIR> <ID>
        \\
    );
}

fn writeFd(fd: c_int, bytes: []const u8) void {
    if (bytes.len == 0) return;
    _ = std.c.write(fd, bytes.ptr, bytes.len);
}

test "durable dead-runs cli lists shows and discards records" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = dir_buf[0..dir_len];

    try dead_runs.writeDeadRun(allocator, durable_dir, "durable-cli1", "job:cli", "durable-cli1.jsonl", "handler error", 1000, 10);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(allocator);
    defer err.deinit();
    try runWith(allocator, &.{ "list", "--durable", durable_dir }, &out.writer, &err.writer);
    try std.testing.expectEqualStrings("durable-cli1\n", out.writer.buffered());

    out.clearRetainingCapacity();
    err.clearRetainingCapacity();
    try runWith(allocator, &.{ "show", "--durable", durable_dir, "durable-cli1" }, &out.writer, &err.writer);
    try std.testing.expect(std.mem.indexOf(u8, out.writer.buffered(), "handler error") != null);

    out.clearRetainingCapacity();
    err.clearRetainingCapacity();
    try runWith(allocator, &.{ "discard", "--durable", durable_dir, "durable-cli1" }, &out.writer, &err.writer);
    try std.testing.expectEqualStrings("discarded durable-cli1\n", out.writer.buffered());

    out.clearRetainingCapacity();
    err.clearRetainingCapacity();
    try runWith(allocator, &.{ "list", "--durable", durable_dir }, &out.writer, &err.writer);
    try std.testing.expectEqualStrings("(none)\n", out.writer.buffered());
}

test "durable dead-runs cli replays a quarantined record" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = dir_buf[0..dir_len];

    try dead_runs.writeDeadRun(allocator, durable_dir, "durable-cli2", "job:replay", "durable-cli2.jsonl", "boom", 2000, 10);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(allocator);
    defer err.deinit();
    try runWith(allocator, &.{ "replay", "--durable", durable_dir, "durable-cli2" }, &out.writer, &err.writer);
    try std.testing.expectEqualStrings("replayed durable-cli2\n", out.writer.buffered());

    try std.testing.expect(!(try dead_runs.hasDeadRun(allocator, durable_dir, "durable-cli2")));
}

test "durable dead-runs cli rejects unsafe ids" {
    const allocator = std.testing.allocator;
    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(allocator);
    defer err.deinit();

    try std.testing.expectError(
        error.InvalidDeadRunId,
        runWith(allocator, &.{ "show", "--durable", "/tmp/zigttp-durable", "../x" }, &out.writer, &err.writer),
    );
    try std.testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "id must use") != null);
}

test "durable dead-runs cli reports missing record on replay" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = dir_buf[0..dir_len];

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(allocator);
    defer err.deinit();
    try std.testing.expectError(
        error.DeadRunMissing,
        runWith(allocator, &.{ "replay", "--durable", durable_dir, "durable-missing" }, &out.writer, &err.writer),
    );
    try std.testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "not found") != null);
}
