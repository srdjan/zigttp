const std = @import("std");

const http_types = @import("http_types.zig");
const shared = @import("cli_shared.zig");
const workflow_queue = @import("workflow_queue.zig");

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
        error.InvalidWorkflowQueueItemId,
        error.NotFound,
        error.TooManyArguments,
        error.UnknownArgument,
        error.UnknownSubcommand,
        error.WorkflowQueueDeadLetterMissing,
        error.WorkflowQueueDeadLetterNotReplayable,
        error.WorkflowQueueItemAlreadyLeased,
        error.WorkflowQueueItemAlreadyPending,
        error.WorkflowQueueResultAlreadyDone,
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
        try stderr.writeAll("zigttp workflow-queue: invalid arguments\n\n");
        try writeHelp(stderr);
        return err;
    };

    if (opts.command == .help) {
        try writeHelp(stdout);
        return;
    }

    const durable_dir = opts.durable_dir orelse {
        try stderr.writeAll("zigttp workflow-queue: --durable <DIR> is required\n");
        return error.MissingDurableDir;
    };
    if (opts.id) |id| {
        if (!workflow_queue.isValidItemId(id)) {
            try stderr.writeAll("zigttp workflow-queue: item id must use letters, numbers, '-' or '_'\n");
            return error.InvalidWorkflowQueueItemId;
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
    try stderr.writeAll("zigttp workflow-queue: item id is required\n");
    return error.MissingId;
}

fn listCommand(allocator: std.mem.Allocator, durable_dir: []const u8, stdout: *std.Io.Writer) !void {
    const ids = try workflow_queue.listDeadIds(allocator, durable_dir);
    defer {
        for (ids) |id| allocator.free(id);
        allocator.free(ids);
    }

    const orphaned = try workflow_queue.listOrphanedReclaimIds(allocator, durable_dir);
    defer {
        for (orphaned) |id| allocator.free(id);
        allocator.free(orphaned);
    }

    if (ids.len == 0 and orphaned.len == 0) {
        try stdout.writeAll("(none)\n");
        return;
    }
    for (ids) |id| try stdout.print("{s}\n", .{id});
    if (orphaned.len > 0) {
        try stdout.writeAll("orphaned (stray reclaim, recovers on next claim attempt):\n");
        for (orphaned) |id| try stdout.print("  {s}\n", .{id});
    }
}

fn showCommand(
    allocator: std.mem.Allocator,
    durable_dir: []const u8,
    id: []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    const payload = (try workflow_queue.readDead(allocator, durable_dir, id)) orelse {
        try stderr.print("zigttp workflow-queue: dead letter not found: {s}\n", .{id});
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
    workflow_queue.replayDead(allocator, durable_dir, id) catch |err| {
        try writeReplayError(stderr, id, err);
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
    workflow_queue.discardDead(allocator, durable_dir, id) catch |err| switch (err) {
        error.WorkflowQueueDeadLetterMissing => {
            try stderr.print("zigttp workflow-queue: dead letter not found: {s}\n", .{id});
            return error.NotFound;
        },
        else => return err,
    };
    try stdout.print("discarded {s}\n", .{id});
}

fn writeReplayError(stderr: *std.Io.Writer, id: []const u8, err: anyerror) !void {
    switch (err) {
        error.WorkflowQueueDeadLetterMissing => {
            try stderr.print("zigttp workflow-queue: dead letter not found: {s}\n", .{id});
        },
        error.WorkflowQueueDeadLetterNotReplayable => {
            try stderr.print("zigttp workflow-queue: dead letter is not replayable: {s}\n", .{id});
        },
        error.WorkflowQueueResultAlreadyDone => {
            try stderr.print("zigttp workflow-queue: item already has a done result: {s}\n", .{id});
        },
        error.WorkflowQueueItemAlreadyPending => {
            try stderr.print("zigttp workflow-queue: item is already pending: {s}\n", .{id});
        },
        error.WorkflowQueueItemAlreadyLeased => {
            try stderr.print("zigttp workflow-queue: item is currently leased: {s}\n", .{id});
        },
        else => {},
    }
}

fn writeHelp(writer: *std.Io.Writer) !void {
    try writer.writeAll(
        \\zigttp workflow-queue - inspect durable workflow queue dead letters
        \\
        \\Usage:
        \\  zigttp workflow-queue list --durable <DIR>
        \\  zigttp workflow-queue show --durable <DIR> <ID>
        \\  zigttp workflow-queue replay --durable <DIR> <ID>
        \\  zigttp workflow-queue discard --durable <DIR> <ID>
        \\
    );
}

fn writeFd(fd: c_int, bytes: []const u8) void {
    if (bytes.len == 0) return;
    _ = std.c.write(fd, bytes.ptr, bytes.len);
}

test "workflow-queue cli lists shows and discards dead letters" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = dir_buf[0..dir_len];

    try workflow_queue.markDead(allocator, durable_dir, "item-cli", "manual review");

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(allocator);
    defer err.deinit();
    try runWith(allocator, &.{ "list", "--durable", durable_dir }, &out.writer, &err.writer);
    try std.testing.expectEqualStrings("item-cli\n", out.writer.buffered());

    out.clearRetainingCapacity();
    err.clearRetainingCapacity();
    try runWith(allocator, &.{ "show", "--durable", durable_dir, "item-cli" }, &out.writer, &err.writer);
    try std.testing.expect(std.mem.indexOf(u8, out.writer.buffered(), "manual review") != null);

    out.clearRetainingCapacity();
    err.clearRetainingCapacity();
    try runWith(allocator, &.{ "discard", "--durable", durable_dir, "item-cli" }, &out.writer, &err.writer);
    try std.testing.expectEqualStrings("discarded item-cli\n", out.writer.buffered());

    out.clearRetainingCapacity();
    err.clearRetainingCapacity();
    try runWith(allocator, &.{ "list", "--durable", durable_dir }, &out.writer, &err.writer);
    try std.testing.expectEqualStrings("(none)\n", out.writer.buffered());
}

test "workflow-queue cli replays replayable dead letter" {
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dir_len = try tmp.dir.realPath(std.testing.io, &dir_buf);
    const durable_dir = dir_buf[0..dir_len];

    const view: http_types.HttpRequestView = .{
        .method = "GET",
        .path = "/",
        .url = "/",
        .query_params = &.{},
        .headers = .empty,
        .body = null,
    };
    try workflow_queue.enqueueRequest(allocator, durable_dir, "item-replay", "child", view);
    var first = try workflow_queue.tryClaim(allocator, durable_dir, "item-replay", 0, 1);
    defer first.deinit(allocator);
    var second = try workflow_queue.tryClaim(allocator, durable_dir, "item-replay", 2, 1);
    defer second.deinit(allocator);
    var third = try workflow_queue.tryClaim(allocator, durable_dir, "item-replay", 4, 1);
    defer third.deinit(allocator);
    var dead = try workflow_queue.tryClaim(allocator, durable_dir, "item-replay", 6, 1);
    defer dead.deinit(allocator);
    try std.testing.expect(dead == .dead);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(allocator);
    defer err.deinit();
    try runWith(allocator, &.{ "replay", "--durable", durable_dir, "item-replay" }, &out.writer, &err.writer);
    try std.testing.expectEqualStrings("replayed item-replay\n", out.writer.buffered());

    var replayed = try workflow_queue.tryClaim(allocator, durable_dir, "item-replay", 10, 1);
    defer replayed.deinit(allocator);
    const request = switch (replayed) {
        .claimed => |*req| req,
        else => return error.ExpectedClaimedQueueItem,
    };
    try std.testing.expectEqualStrings("child", request.target);
    try std.testing.expectEqual(@as(u32, 1), request.attempts);
}

test "workflow-queue cli rejects unsafe ids" {
    const allocator = std.testing.allocator;
    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    var err = std.Io.Writer.Allocating.init(allocator);
    defer err.deinit();

    try std.testing.expectError(
        error.InvalidWorkflowQueueItemId,
        runWith(allocator, &.{ "show", "--durable", "/tmp/zigttp-durable", "../x" }, &out.writer, &err.writer),
    );
    try std.testing.expect(std.mem.indexOf(u8, err.writer.buffered(), "item id must use") != null);
}
