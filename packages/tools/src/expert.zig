//! `zigts expert` subcommand namespace.
//!
//! Provides a stable, versioned CLI surface for the interactive expert app
//! and CI.
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
const module_audit = @import("module_audit.zig");
const expert_meta = @import("expert_meta.zig");
const verify_paths_core = @import("verify_paths_core.zig");

const compiler_version = expert_meta.compiler_version;
const policy_version = expert_meta.policy_version;
const category_counts = expert_meta.category_counts;

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
    if (std.mem.eql(u8, sub, "verify-modules")) {
        try runVerifyModules(allocator, rest);
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

    const info = expert_meta.compute();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    if (json_mode) {
        try expert_meta.writeJson(w, &info);
    } else {
        try expert_meta.writeText(w, &info);
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

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    var has_errors = false;

    if (json_mode) {
        const outcome = try verify_paths_core.writeJsonEnvelope(allocator, w, paths.items);
        has_errors = !outcome.ok;
    } else {
        // Text path still collects diagnostics into a local list so the
        // summary line can count them up-front. Not part of the v1 JSON
        // contract; free to diverge from the streaming helper.
        var all_diagnostics: std.ArrayList(json_diag.JsonDiagnostic) = .empty;
        defer all_diagnostics.deinit(allocator);

        for (paths.items) |path| {
            var result = precompile.runCheckOnly(allocator, path, null, true, null) catch |err| return err;
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

        if (!has_errors) {
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

fn runVerifyModules(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var parsed = parseVerifyModulesArgs(allocator, argv) catch |err| switch (err) {
        error.InvalidArguments, error.UnknownArgument => {
            printVerifyModulesHelp();
            return err;
        },
        else => return err,
    };
    defer parsed.paths.deinit(allocator);

    if (parsed.help) {
        printVerifyModulesHelp();
        return;
    }

    const hash = rule_registry.policyHash();
    var result = if (parsed.builtins)
        try module_audit.verifyBuiltins(allocator, .{ .strict = parsed.strict })
    else blk: {
        if (parsed.paths.items.len == 0) {
            printVerifyModulesHelp();
            return error.MissingArgument;
        }
        break :blk try module_audit.verifyPaths(allocator, parsed.paths.items, .{ .strict = parsed.strict });
    };
    defer result.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    if (parsed.json_mode) {
        try module_audit.writeJsonEnvelope(w, &result, hash);
    } else if (result.diagnostics.items.len == 0) {
        try w.print("Verified {d} virtual module file(s): no issues\n", .{result.checked_files.items.len});
    } else {
        try w.print("Verified {d} virtual module file(s): {d} issue(s)\n", .{
            result.checked_files.items.len,
            result.diagnostics.items.len,
        });
        for (result.diagnostics.items) |diag| {
            try w.print("  {s} {s} {s}:{d}:{d}: {s}\n", .{
                diag.diag.code,
                diag.diag.severity,
                diag.diag.file,
                diag.diag.line,
                diag.diag.column,
                diag.diag.message,
            });
        }
    }

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }

    if (result.hasErrors()) std.process.exit(1);
}

const VerifyModulesArgs = struct {
    help: bool = false,
    json_mode: bool = false,
    builtins: bool = false,
    strict: bool = false,
    paths: std.ArrayListUnmanaged([]const u8) = .empty,
};

fn parseVerifyModulesArgs(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
) !VerifyModulesArgs {
    var parsed = VerifyModulesArgs{};
    errdefer parsed.paths.deinit(allocator);

    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            parsed.json_mode = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            parsed.help = true;
        } else if (std.mem.eql(u8, arg, "--builtins")) {
            parsed.builtins = true;
        } else if (std.mem.eql(u8, arg, "--strict")) {
            parsed.strict = true;
        } else if (std.mem.startsWith(u8, arg, "-")) {
            return error.UnknownArgument;
        } else {
            try parsed.paths.append(allocator, arg);
        }
    }

    if (parsed.builtins and parsed.paths.items.len > 0) {
        return error.InvalidArguments;
    }

    return parsed;
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
        \\  zigts expert verify-modules <file>... [--strict] [--json]
        \\  zigts expert verify-modules --builtins [--strict] [--json]
        \\  zigts expert review-patch <file> [--before <old>] [--diff-only] [--json] [--stdin-json]
        \\  zigts expert edit-simulate [handler.ts] [--before old.ts] [--stdin-json]
        \\  zigts expert describe-rule [rule-name|code] [--json] [--hash]
        \\  zigts expert search <keyword> [--json]
        \\
        \\Subcommands:
        \\  meta           Show policy metadata (version, hash, rule count)
        \\  verify-paths   Run full analysis on files and report violations
        \\  verify-modules Audit built-in virtual module files and specs
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

fn printVerifyModulesHelp() void {
    const help =
        \\zigts expert verify-modules - audit built-in virtual module files
        \\
        \\Usage:
        \\  zigts expert verify-modules <file>... [--strict] [--json]
        \\  zigts expert verify-modules --builtins [--strict] [--json]
        \\
        \\`--builtins` audits the authoritative public built-in module/spec set.
        \\Positional paths audit individual built-in module/spec files.
        \\Paths under packages/zigts/src/modules/ that are not public built-ins
        \\are ignored so editor hooks stay advisory and low-noise.
        \\
        \\Checks for forbidden direct effect usage, helper/capability drift,
        \\and mismatches between the Zig binding and the module spec artifact.
        \\`--strict` upgrades missing-spec governance warnings into errors.
        \\Exit code 1 if any errors found.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

// ---------------------------------------------------------------------------
// v1 contract tripwire
//
// `docs/zigts-expert-contract.md` freezes `zigts expert meta` output as v1.
// These tests fail loudly if a drive-by edit changes a pinned value. Any real
// change must also update the contract doc.
// ---------------------------------------------------------------------------

test "v1 contract: compiler_version pinned" {
    try std.testing.expectEqualStrings("0.16.0", compiler_version);
}

test "v1 contract: policy_version pinned" {
    try std.testing.expectEqualStrings("2026.04.2", policy_version);
}

test "v1 contract: every rule has a category" {
    const sum = category_counts.verifier + category_counts.policy + category_counts.property;
    try std.testing.expectEqual(rule_registry.all_rules.len, sum);
}

test "verify-modules arg parser accepts builtins and strict flags" {
    var parsed = try parseVerifyModulesArgs(std.testing.allocator, &.{ "--builtins", "--strict", "--json" });
    defer parsed.paths.deinit(std.testing.allocator);

    try std.testing.expect(parsed.builtins);
    try std.testing.expect(parsed.strict);
    try std.testing.expect(parsed.json_mode);
    try std.testing.expectEqual(@as(usize, 0), parsed.paths.items.len);
}

test "verify-modules arg parser rejects mixing builtins and positional paths" {
    try std.testing.expectError(
        error.InvalidArguments,
        parseVerifyModulesArgs(std.testing.allocator, &.{ "--builtins", "packages/zigts/src/modules/env.zig" }),
    );
}
