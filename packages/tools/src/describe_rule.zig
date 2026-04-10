//! `zigts describe-rule` subcommand.
//!
//! Lists all rules, describes a specific rule by name or code, or outputs
//! the policy hash for CI assertions.

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;
const writeJsonString = zigts.handler_contract.writeJsonString;

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    var hash_mode = false;
    var rule_name: ?[]const u8 = null;

    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (std.mem.eql(u8, arg, "--hash")) {
            hash_mode = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            rule_name = arg;
        }
    }

    if (hash_mode) {
        const hash = rule_registry.policyHash();
        _ = std.c.write(std.c.STDOUT_FILENO, &hash, hash.len);
        _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
        return;
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    if (rule_name) |name| {
        const entry = rule_registry.findByName(name) orelse
            rule_registry.findByCode(name) orelse {
            try aw.writer.print("Unknown rule: {s}\n", .{name});
            buf = aw.toArrayList();
            _ = std.c.write(std.c.STDERR_FILENO, buf.items.ptr, buf.items.len);
            std.process.exit(1);
        };

        if (json_mode) {
            try writeRuleJson(&aw.writer, entry);
            try aw.writer.writeAll("\n");
        } else {
            try writeRuleText(&aw.writer, entry);
        }
    } else if (json_mode) {
        try aw.writer.writeAll("[");
        for (&rule_registry.all_rules, 0..) |*entry, i| {
            if (i > 0) try aw.writer.writeAll(",");
            try writeRuleJson(&aw.writer, entry);
        }
        try aw.writer.writeAll("]\n");
    } else {
        try aw.writer.print("{s:<30} {s:<8} {s:<10} {s}\n", .{ "NAME", "CODE", "CATEGORY", "DESCRIPTION" });
        try aw.writer.writeAll("---\n");
        for (&rule_registry.all_rules) |*entry| {
            try aw.writer.print("{s:<30} {s:<8} {s:<10} {s}\n", .{
                entry.name,
                entry.code,
                entry.category.label(),
                entry.description,
            });
        }
    }

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }
}

pub fn writeRuleJson(writer: anytype, entry: *const rule_registry.RuleEntry) !void {
    try writer.writeAll("{\"name\":");
    try writeJsonString(writer, entry.name);
    try writer.writeAll(",\"code\":");
    try writeJsonString(writer, entry.code);
    try writer.writeAll(",\"category\":");
    try writeJsonString(writer, entry.category.label());
    try writer.writeAll(",\"description\":");
    try writeJsonString(writer, entry.description);
    if (entry.example) |ex| {
        try writer.writeAll(",\"example\":");
        try writeJsonString(writer, ex);
    }
    try writer.writeAll(",\"help\":");
    try writeJsonString(writer, entry.help);
    try writer.writeAll("}");
}

fn writeRuleText(writer: anytype, entry: *const rule_registry.RuleEntry) !void {
    try writer.print("Rule: {s}\n", .{entry.name});
    try writer.print("Code: {s}\n", .{entry.code});
    try writer.print("Category: {s}\n", .{entry.category.label()});
    try writer.print("Description: {s}\n", .{entry.description});
    if (entry.example) |ex| {
        try writer.print("Example: {s}\n", .{ex});
    }
    try writer.print("Help: {s}\n", .{entry.help});
}

fn printHelp() void {
    const help =
        \\zigts describe-rule - describe diagnostic rules
        \\
        \\Usage:
        \\  zigts describe-rule [rule-name|code] [--json] [--hash]
        \\
        \\Options:
        \\  --json    Output as JSON
        \\  --hash    Output policy hash (SHA-256 of all rule metadata)
        \\
        \\With no arguments, lists all rules.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
