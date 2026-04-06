//! `zigts expert` subcommand namespace.
//!
//! Provides a stable, versioned CLI surface for Claude Code hooks and CI.
//! Delegates to existing command implementations; adds `meta` and `verify-paths`.

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;
const writeJsonString = zigts.handler_contract.writeJsonString;
const precompile = @import("precompile.zig");
const json_diag = precompile.json_diag;
const edit_simulate = @import("edit_simulate.zig");
const describe_rule = @import("describe_rule.zig");
const search_rules = @import("search_rules.zig");
const review_patch = @import("review_patch.zig");

const compiler_version = "0.14.0";
const policy_version = "2026.04.1";

const CategoryCounts = struct { verifier: usize, policy: usize, property: usize };
const category_counts: CategoryCounts = blk: {
    var v: usize = 0;
    var p: usize = 0;
    var pr: usize = 0;
    for (&rule_registry.all_rules) |*rule| {
        switch (rule.category) {
            .verifier => v += 1,
            .policy => p += 1,
            .property => pr += 1,
        }
    }
    break :blk .{ .verifier = v, .policy = p, .property = pr };
};

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (argv.len == 0 or std.mem.eql(u8, argv[0], "--help") or std.mem.eql(u8, argv[0], "help")) {
        printExpertHelp();
        return;
    }

    const sub = argv[0];
    const rest = argv[1..];

    if (std.mem.eql(u8, sub, "meta")) {
        try runMeta(allocator, rest);
        return;
    }
    if (std.mem.eql(u8, sub, "verify-paths")) {
        try runVerifyPaths(allocator, rest);
        return;
    }
    if (std.mem.eql(u8, sub, "review-patch")) {
        try review_patch.runWithArgs(allocator, rest);
        return;
    }
    if (std.mem.eql(u8, sub, "edit-simulate")) {
        try edit_simulate.runWithArgs(allocator, rest);
        return;
    }
    if (std.mem.eql(u8, sub, "describe-rule")) {
        try describe_rule.runWithArgs(allocator, rest);
        return;
    }
    if (std.mem.eql(u8, sub, "search")) {
        try search_rules.runWithArgs(allocator, rest);
        return;
    }

    printExpertHelp();
    return error.UnknownSubcommand;
}

// ---------------------------------------------------------------------------
// meta
// ---------------------------------------------------------------------------

fn runMeta(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            printMetaHelp();
            return;
        }
    }

    const hash = rule_registry.policyHash();
    const counts = category_counts;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    if (json_mode) {
        try w.print(
            "{{\"compiler_version\":\"{s}\",\"policy_version\":\"{s}\",\"policy_hash\":\"{s}\",\"rule_count\":{d},\"categories\":{{\"verifier\":{d},\"policy\":{d},\"property\":{d}}},\"mode\":\"embedded\"}}\n",
            .{ compiler_version, policy_version, hash, rule_registry.all_rules.len, counts.verifier, counts.policy, counts.property },
        );
    } else {
        try w.print(
            \\zigts expert policy
            \\  compiler: {s}
            \\  policy:   {s}
            \\  hash:     {s}
            \\  rules:    {d} ({d} verifier, {d} policy, {d} property)
            \\  mode:     embedded
            \\
        , .{ compiler_version, policy_version, hash, rule_registry.all_rules.len, counts.verifier, counts.policy, counts.property });
    }

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }
}

// ---------------------------------------------------------------------------
// verify-paths
// ---------------------------------------------------------------------------

fn runVerifyPaths(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    var paths: std.ArrayListUnmanaged([]const u8) = .empty;
    defer paths.deinit(allocator);

    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            printVerifyHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            try paths.append(allocator, arg);
        }
    }

    if (paths.items.len == 0) {
        printVerifyHelp();
        return error.MissingArgument;
    }

    const hash = rule_registry.policyHash();

    var all_diagnostics: std.ArrayList(json_diag.JsonDiagnostic) = .empty;
    defer all_diagnostics.deinit(allocator);
    var has_errors = false;

    for (paths.items) |path| {
        var result = precompile.runCheckOnly(allocator, path, null, true) catch |err| {
            if (json_mode) {
                // Report file-level error as a diagnostic
                try all_diagnostics.append(allocator, .{
                    .code = "ZTS000",
                    .severity = "error",
                    .message = @errorName(err),
                    .file = path,
                    .line = 0,
                    .column = 0,
                    .suggestion = null,
                });
                has_errors = true;
                continue;
            }
            return err;
        };
        defer result.deinit(allocator);

        if (result.totalErrors() > 0) has_errors = true;

        for (result.json_diagnostics.items) |diag| {
            try all_diagnostics.append(allocator, .{
                .code = diag.code,
                .severity = diag.severity,
                .message = diag.message,
                .file = path,
                .line = diag.line,
                .column = diag.column,
                .suggestion = diag.suggestion,
            });
        }
    }

    const ok = !has_errors;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    if (json_mode) {
        try w.print("{{\"ok\":{s},\"policy_version\":\"{s}\",\"policy_hash\":\"{s}\",\"checked_files\":[", .{
            if (ok) "true" else "false",
            policy_version,
            hash,
        });
        for (paths.items, 0..) |path, i| {
            if (i > 0) try w.writeAll(",");
            try writeJsonString(w, path);
        }
        try w.writeAll("],\"violations\":[");
        for (all_diagnostics.items, 0..) |*diag, i| {
            if (i > 0) try w.writeAll(",");
            try json_diag.writeDiagnosticJson(w, diag);
        }
        try w.writeAll("]}\n");
    } else {
        if (ok) {
            try w.print("Verified {d} file(s): no violations\n", .{paths.items.len});
        } else {
            try w.print("Verified {d} file(s): {d} violation(s)\n", .{
                paths.items.len,
                all_diagnostics.items.len,
            });
            for (all_diagnostics.items) |diag| {
                try w.print("  {s} {s}:{d}:{d}: {s}\n", .{
                    diag.code,
                    diag.file,
                    diag.line,
                    diag.column,
                    diag.message,
                });
            }
        }
    }

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }

    if (has_errors) std.process.exit(1);
}

// ---------------------------------------------------------------------------
// Help
// ---------------------------------------------------------------------------

fn printExpertHelp() void {
    const help =
        \\zigts expert - compiler-embedded policy oracle
        \\
        \\Usage:
        \\  zigts expert meta [--json]
        \\  zigts expert verify-paths <file>... [--json]
        \\  zigts expert review-patch <file> [--before <old>] [--diff-only] [--json] [--stdin-json]
        \\  zigts expert edit-simulate [handler.ts] [--before old.ts] [--stdin-json]
        \\  zigts expert describe-rule [rule-name|code] [--json] [--hash]
        \\  zigts expert search <keyword> [--json]
        \\
        \\Subcommands:
        \\  meta           Show policy metadata (version, hash, rule count)
        \\  verify-paths   Run full analysis on files and report violations
        \\  review-patch   Analyze a file for violations (diff-aware with --before)
        \\  edit-simulate  Simulate an edit and report new vs preexisting violations
        \\  describe-rule  Describe a specific rule or list all rules
        \\  search         Search rules by keyword
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printMetaHelp() void {
    const help =
        \\zigts expert meta - show policy metadata
        \\
        \\Usage: zigts expert meta [--json]
        \\
        \\Outputs compiler version, policy version, policy hash, rule count,
        \\and category breakdown.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn printVerifyHelp() void {
    const help =
        \\zigts expert verify-paths - verify handler files
        \\
        \\Usage: zigts expert verify-paths <file>... [--json]
        \\
        \\Runs the full analysis pipeline on each file and reports violations.
        \\Exit code 1 if any errors found.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
