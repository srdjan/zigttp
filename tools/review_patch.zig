//! `zigts review-patch` subcommand.
//!
//! Analyzes a file (optionally against a before version) and reports violations.
//! With `--diff-only`, only violations introduced by the patch are shown.

const std = @import("std");
const zigts = @import("zigts");
const edit_simulate = @import("edit_simulate.zig");
const file_io = zigts.file_io;

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    var diff_only = false;
    var stdin_json = false;
    var handler_path: ?[]const u8 = null;
    var before_path: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--diff-only")) {
            diff_only = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--stdin-json")) {
            stdin_json = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--before")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            before_path = argv[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
        if (!std.mem.startsWith(u8, arg, "-") and handler_path == null) {
            handler_path = arg;
            continue;
        }
        return error.InvalidArgument;
    }

    var owned_content: ?[]u8 = null;
    defer if (owned_content) |c| allocator.free(c);
    var owned_before: ?[]u8 = null;
    defer if (owned_before) |b| allocator.free(b);

    const input: edit_simulate.EditSimulateInput = if (stdin_json)
        try edit_simulate.readStdinJson(allocator)
    else blk: {
        const path = handler_path orelse return error.MissingArgument;
        owned_content = try file_io.readFile(allocator, path, 10 * 1024 * 1024);
        if (before_path) |bp| {
            owned_before = try file_io.readFile(allocator, bp, 10 * 1024 * 1024);
        }
        break :blk .{
            .file = path,
            .content = owned_content.?,
            .before = owned_before,
        };
    };

    var result = try edit_simulate.simulate(allocator, input);
    defer result.deinit(allocator);

    if (diff_only) {
        // Filter in-place: keep only violations introduced by the patch.
        var write_idx: usize = 0;
        var new_count: u32 = 0;
        for (result.violations.items) |v| {
            if (v.introduced_by_patch) {
                result.violations.items[write_idx] = v;
                write_idx += 1;
                new_count += 1;
            }
        }
        result.violations.items.len = write_idx;
        result.total = new_count;
        result.new_count = new_count;
        result.preexisting_count = 0;
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    if (json_mode) {
        try edit_simulate.writeResultJson(&aw.writer, &result);
    } else {
        try writeTextReport(&aw.writer, &result);
    }

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }

    if (result.total > 0) std.process.exit(1);
}

fn writeTextReport(writer: anytype, result: *const edit_simulate.SimulateResult) !void {
    if (result.total == 0) {
        try writer.writeAll("No violations found.\n");
        return;
    }

    for (result.violations.items) |v| {
        const patch_marker: []const u8 = if (v.introduced_by_patch) " [NEW]" else "";
        try writer.print("{s} line {d}:{d}{s}: {s}\n", .{
            v.code,
            v.line,
            v.column,
            patch_marker,
            v.message,
        });
        if (v.help) |h| {
            try writer.print("  help: {s}\n", .{h});
        }
    }

    try writer.print("\nTotal: {d} ({d} new, {d} preexisting)\n", .{
        result.total,
        result.new_count,
        result.preexisting_count,
    });
}

fn printHelp() void {
    const help =
        \\zigts review-patch - review a file for violations
        \\
        \\Usage:
        \\  zigts review-patch <file> [--before <old-file>] [--diff-only] [--json] [--stdin-json]
        \\
        \\Options:
        \\  --before FILE    Original file for diff-aware analysis
        \\  --diff-only      Only show violations introduced by the patch
        \\  --json           Output as JSON
        \\  --stdin-json     Read input as JSON from stdin
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
