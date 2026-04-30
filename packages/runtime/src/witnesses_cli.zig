//! `zigttp witnesses` subcommand. Surfaces the on-disk witness corpus to
//! authors and CI: list persisted falsifying inputs per handler, pin entries
//! that should never be pruned, and prune unpinned entries by age.
//!
//! Subcommands:
//!     zigttp witnesses list [<handler>]
//!     zigttp witnesses pin <handler> <key|prefix>
//!     zigttp witnesses unpin <handler> <key|prefix>
//!     zigttp witnesses prune <handler> [--older-than <seconds>]
//!
//! With no handler argument, `list` reports a one-line summary per handler
//! corpus discovered under `.zigttp/witnesses/`.

const std = @import("std");
const zigts = @import("zigts");
const witness_corpus = zigts.witness_corpus;
const printer_mod = @import("deploy/printer.zig");
const io_util = @import("deploy/io_util.zig");

const Subcommand = enum {
    list,
    pin,
    unpin,
    prune,
    help,
};

/// Errors that witnesses_cli explained on stderr. Callers like dev_cli
/// swallow these so the shell exit stays clean.
pub fn isExpectedUserError(err: anyerror) bool {
    return switch (err) {
        error.UnknownSubcommand,
        error.MissingHandlerArgument,
        error.MissingKeyArgument,
        error.WitnessNotFound,
        error.WitnessAmbiguous,
        error.WitnessCorpusMissing,
        error.MissingArgValue,
        error.InvalidArgument,
        error.UnknownArgument,
        => true,
        else => false,
    };
}

pub fn run(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var stdout_buf: [4096]u8 = undefined;
    var stderr_buf: [1024]u8 = undefined;
    var stdout_writer = printer_mod.FdWriter.init(std.c.STDOUT_FILENO, stdout_buf[0..]);
    var stderr_writer = printer_mod.FdWriter.init(std.c.STDERR_FILENO, stderr_buf[0..]);
    defer stdout_writer.interface.flush() catch {};
    defer stderr_writer.interface.flush() catch {};

    try runWith(allocator, argv, &stdout_writer.interface, &stderr_writer.interface);
}

pub fn runWith(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    if (argv.len == 0) {
        try printHelp(stdout);
        return;
    }

    const sub = parseSub(argv[0]) orelse {
        try stderr.print("Unknown witnesses subcommand: {s}\n", .{argv[0]});
        return error.UnknownSubcommand;
    };
    const rest = argv[1..];

    switch (sub) {
        .list => try cmdList(allocator, rest, stdout, stderr),
        .pin => try cmdPin(allocator, rest, stdout, stderr, true),
        .unpin => try cmdPin(allocator, rest, stdout, stderr, false),
        .prune => try cmdPrune(allocator, rest, stdout, stderr),
        .help => try printHelp(stdout),
    }
}

fn parseSub(s: []const u8) ?Subcommand {
    if (std.mem.eql(u8, s, "list")) return .list;
    if (std.mem.eql(u8, s, "pin")) return .pin;
    if (std.mem.eql(u8, s, "unpin")) return .unpin;
    if (std.mem.eql(u8, s, "prune")) return .prune;
    if (std.mem.eql(u8, s, "help") or std.mem.eql(u8, s, "--help") or std.mem.eql(u8, s, "-h")) return .help;
    return null;
}

fn printHelp(stdout: *std.Io.Writer) !void {
    try stdout.writeAll(
        \\zigttp witnesses - inspect and manage the on-disk witness corpus.
        \\
        \\Usage:
        \\  zigttp witnesses list [<handler>]
        \\  zigttp witnesses pin <handler> <key|prefix>
        \\  zigttp witnesses unpin <handler> <key|prefix>
        \\  zigttp witnesses prune <handler> [--older-than <seconds>]
        \\
        \\Each handler accumulates a corpus of compiler-discovered falsifying
        \\inputs ("witnesses") under .zigttp/witnesses/<short_hash>/. Pinned
        \\witnesses are never pruned. With no <handler> argument, `list`
        \\summarises every corpus directory discovered.
        \\
    );
}

fn cmdList(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    if (argv.len == 0) {
        return summariseAll(allocator, stdout, stderr);
    }
    const handler = argv[0];
    const dir = try witness_corpus.corpusDir(allocator, handler);
    defer allocator.free(dir);

    const entries = witness_corpus.loadEntries(allocator, dir) catch |err| switch (err) {
        error.WitnessCorpusMissing => {
            try stdout.print("No witnesses recorded for {s}\n", .{handler});
            return;
        },
        else => return err,
    };
    defer witness_corpus.freeEntries(allocator, entries);

    if (entries.len == 0) {
        try stdout.print("No witnesses recorded for {s}\n", .{handler});
        return;
    }

    try stdout.print("{d} witness(es) for {s}:\n", .{ entries.len, handler });
    for (entries) |e| {
        const pin_marker: []const u8 = if (e.pinned) "[pinned]" else "        ";
        try stdout.print("  {s} {s}  {s}  {s}\n", .{
            pin_marker,
            shortKey(e.key),
            e.property,
            e.summary,
        });
    }
}

fn summariseAll(
    allocator: std.mem.Allocator,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    _ = stderr;
    var io_backend = io_util.threadedIo(allocator);
    defer io_backend.deinit();
    const io = io_backend.io();

    var root = std.Io.Dir.cwd().openDir(io, witness_corpus.corpus_root_relative, .{ .iterate = true }) catch {
        try stdout.writeAll("No witnesses recorded yet (.zigttp/witnesses/ is empty).\n");
        return;
    };
    defer root.close(io);

    var iter = root.iterate();
    var any = false;
    while (try iter.next(io)) |child| {
        if (child.kind != .directory) continue;
        any = true;
        const child_dir = try std.fmt.allocPrint(
            allocator,
            "{s}/{s}",
            .{ witness_corpus.corpus_root_relative, child.name },
        );
        defer allocator.free(child_dir);

        const handler_path = readHandlerPath(allocator, io, child_dir) catch null;
        defer if (handler_path) |p| allocator.free(p);

        var total: usize = 0;
        const counts = witness_corpus.countByProperty(allocator, child_dir) catch null;
        if (counts) |c_slice| {
            defer witness_corpus.freeCounts(allocator, c_slice);
            for (c_slice) |c| total += c.count;
        }

        const path_str = if (handler_path) |p| p else "<unknown handler>";
        try stdout.print("  {s}  {d} witness(es)\n", .{ path_str, total });
    }
    if (!any) {
        try stdout.writeAll("No witnesses recorded yet (.zigttp/witnesses/ is empty).\n");
    }
}

fn readHandlerPath(
    allocator: std.mem.Allocator,
    io: std.Io,
    corpus_dir: []const u8,
) !?[]u8 {
    const path = try std.fmt.allocPrint(allocator, "{s}/handler.path", .{corpus_dir});
    defer allocator.free(path);

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const n = std.Io.Dir.cwd().readFile(io, path, &buf) catch return null;
    return try allocator.dupe(u8, n);
}

fn cmdPin(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    set: bool,
) !void {
    if (argv.len < 1) {
        try stderr.writeAll("Missing handler argument.\n");
        return error.MissingHandlerArgument;
    }
    if (argv.len < 2) {
        try stderr.writeAll("Missing key argument.\n");
        return error.MissingKeyArgument;
    }
    const handler = argv[0];
    const key_prefix = argv[1];

    const dir = try witness_corpus.corpusDir(allocator, handler);
    defer allocator.free(dir);

    const resolved_key = try resolveKey(allocator, dir, key_prefix, stderr);
    defer allocator.free(resolved_key);

    try witness_corpus.pin(allocator, dir, resolved_key, set);

    const verb: []const u8 = if (set) "Pinned" else "Unpinned";
    try stdout.print("{s} {s}  for {s}\n", .{ verb, shortKey(resolved_key), handler });
}

fn cmdPrune(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !void {
    if (argv.len == 0) {
        try stderr.writeAll("Missing handler argument.\n");
        return error.MissingHandlerArgument;
    }
    const handler = argv[0];

    var older_than_seconds: i64 = 90 * 24 * 60 * 60;
    var i: usize = 1;
    while (i < argv.len) : (i += 1) {
        const a = argv[i];
        if (std.mem.eql(u8, a, "--older-than")) {
            i += 1;
            if (i >= argv.len) {
                try stderr.writeAll("--older-than requires a value (seconds).\n");
                return error.MissingArgValue;
            }
            older_than_seconds = std.fmt.parseInt(i64, argv[i], 10) catch {
                try stderr.print("Invalid --older-than value: {s}\n", .{argv[i]});
                return error.InvalidArgument;
            };
        } else {
            try stderr.print("Unknown argument: {s}\n", .{a});
            return error.UnknownArgument;
        }
    }

    var ts: std.posix.timespec = undefined;
    _ = std.posix.system.clock_gettime(.REALTIME, &ts);
    const now: i64 = @intCast(ts.sec);
    const cutoff = now - older_than_seconds;

    const dir = try witness_corpus.corpusDir(allocator, handler);
    defer allocator.free(dir);

    const removed = try witness_corpus.prune(allocator, dir, cutoff);
    try stdout.print("Pruned {d} witness(es) older than {d}s for {s}.\n", .{ removed, older_than_seconds, handler });
}

/// Resolve a key prefix to exactly one persisted witness key. Errors out
/// on zero or multiple matches with a helpful message.
fn resolveKey(
    allocator: std.mem.Allocator,
    corpus_dir: []const u8,
    key_prefix: []const u8,
    stderr: *std.Io.Writer,
) ![]u8 {
    const entries = try witness_corpus.loadEntries(allocator, corpus_dir);
    defer witness_corpus.freeEntries(allocator, entries);

    var match: ?[]const u8 = null;
    var ambiguous = false;
    for (entries) |e| {
        if (!std.mem.startsWith(u8, e.key, key_prefix)) continue;
        if (match != null) {
            ambiguous = true;
            break;
        }
        match = e.key;
    }

    if (ambiguous) {
        try stderr.print("Key prefix {s} matches multiple witnesses. Use a longer prefix.\n", .{key_prefix});
        return error.WitnessAmbiguous;
    }
    if (match) |m| return try allocator.dupe(u8, m);

    try stderr.print("No witness found for key prefix {s}.\n", .{key_prefix});
    return error.WitnessNotFound;
}

fn shortKey(key: []const u8) []const u8 {
    return key[0..@min(key.len, 12)];
}
