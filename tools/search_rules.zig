//! `zigts search` subcommand.
//!
//! Searches rules by keyword (substring match against name, description, help).

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;
const describe_rule = @import("describe_rule.zig");

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    var keyword: ?[]const u8 = null;

    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            keyword = arg;
        }
    }

    const search_term = keyword orelse {
        printHelp();
        return;
    };

    const indices = rule_registry.search(search_term);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    if (json_mode) {
        try aw.writer.writeAll("[");
        var first = true;
        for (indices.constSlice()) |idx| {
            if (!first) try aw.writer.writeAll(",");
            first = false;
            try describe_rule.writeRuleJson(&aw.writer, &rule_registry.all_rules[idx]);
        }
        try aw.writer.writeAll("]\n");
    } else {
        if (indices.len == 0) {
            try aw.writer.print("No rules matching '{s}'\n", .{search_term});
        } else {
            try aw.writer.print("{d} rule(s) matching '{s}':\n\n", .{ indices.len, search_term });
            for (indices.constSlice()) |idx| {
                const entry = &rule_registry.all_rules[idx];
                try aw.writer.print("  {s:<30} {s:<8} {s}\n", .{
                    entry.name,
                    entry.code,
                    entry.description,
                });
            }
        }
    }

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }
}

fn printHelp() void {
    const help =
        \\zigts search - search diagnostic rules by keyword
        \\
        \\Usage: zigts search <keyword> [--json]
        \\
        \\Searches rule names, descriptions, and help text for substring matches.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
