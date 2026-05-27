//! Shared helper implementations for the direct `zigts meta`,
//! `zigts verify-paths`, and `zigts verify-modules` commands.
//!
//! Exit contract: `help`/`--help`/`-h` in any position returns cleanly (0);
//! missing args or unknown flags propagate an error (1).

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;
const module_audit = @import("module_audit.zig");
const expert_meta = @import("expert_meta.zig");
const verify_paths_core = @import("verify_paths_core.zig");

const compiler_version = expert_meta.compiler_version;
const policy_version = expert_meta.policy_version;
const category_counts = expert_meta.category_counts;

fn hasHelpFlag(argv: []const []const u8) bool {
    for (argv) |a| {
        if (std.mem.eql(u8, a, "--help") or std.mem.eql(u8, a, "-h") or std.mem.eql(u8, a, "help")) return true;
    }
    return false;
}

fn hasFlag(argv: []const []const u8, flag: []const u8) bool {
    for (argv) |a| {
        if (std.mem.eql(u8, a, flag)) return true;
    }
    return false;
}

fn writeOwnedToStdout(bytes: []const u8) void {
    _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
}

/// Persisted shape for the text formatter: only the fields it prints, duped
/// into the caller-supplied arena so they outlive the per-file CheckResult
/// that owns the original bytes.
const TextDiag = struct {
    code: []const u8,
    file: []const u8,
    message: []const u8,
    line: u32,
    column: u16,
};

const TextEmitter = struct {
    arena: std.mem.Allocator,
    list: std.ArrayListUnmanaged(TextDiag),

    pub fn emit(self: *TextEmitter, diag: *const verify_paths_core.Diagnostic) !void {
        try self.list.append(self.arena, .{
            .code = try self.arena.dupe(u8, diag.code),
            .file = try self.arena.dupe(u8, diag.file),
            .message = try self.arena.dupe(u8, diag.message),
            .line = diag.line,
            .column = diag.column,
        });
    }
};

pub fn runMeta(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (hasHelpFlag(argv)) {
        printMetaHelp();
        return;
    }
    const json_mode = hasFlag(argv, "--json");

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
    writeOwnedToStdout(buf.items);
}

pub fn runVerifyPaths(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (hasHelpFlag(argv)) {
        printVerifyHelp();
        return;
    }
    var json_mode = false;
    var paths: std.ArrayListUnmanaged([]const u8) = .empty;
    defer paths.deinit(allocator);

    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            try paths.append(allocator, arg);
        } else {
            printVerifyHelp();
            std.process.exit(1);
        }
    }

    if (paths.items.len == 0) {
        printVerifyHelp();
        std.process.exit(1);
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
        // Text path shares the same traversal as the JSON path; the duping
        // emitter copies string fields into an arena so they outlive the
        // per-file CheckResult that owns them.
        var arena_state = std.heap.ArenaAllocator.init(allocator);
        defer arena_state.deinit();
        var emitter = TextEmitter{
            .arena = arena_state.allocator(),
            .list = .empty,
        };

        const outcome = try verify_paths_core.collect(allocator, paths.items, &emitter);
        has_errors = !outcome.ok;

        if (emitter.list.items.len == 0) {
            try w.print("Verified {d} file(s): no violations\n", .{paths.items.len});
        } else {
            try w.print("Verified {d} file(s): {d} violation(s)\n", .{
                paths.items.len,
                emitter.list.items.len,
            });
            for (emitter.list.items) |diag| {
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
    writeOwnedToStdout(buf.items);

    if (has_errors) std.process.exit(1);
}

pub fn runVerifyModules(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (hasHelpFlag(argv)) {
        printVerifyModulesHelp();
        return;
    }
    var parsed = parseVerifyModulesArgs(allocator, argv) catch |err| switch (err) {
        error.InvalidArguments, error.UnknownArgument => {
            printVerifyModulesHelp();
            return err;
        },
        else => return err,
    };
    defer parsed.paths.deinit(allocator);

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
    writeOwnedToStdout(buf.items);

    if (result.hasErrors()) std.process.exit(1);
}

pub fn runExtensionStatus(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (hasHelpFlag(argv)) {
        printExtensionStatusHelp();
        return;
    }

    var parsed = parseExtensionStatusArgs(allocator, argv) catch |err| switch (err) {
        error.InvalidArguments, error.MissingArgument => {
            printExtensionStatusHelp();
            return err;
        },
        else => return err,
    };
    defer parsed.paths.deinit(allocator);

    if (parsed.paths.items.len == 0) {
        printExtensionStatusHelp();
        return error.MissingArgument;
    }

    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();

    var entries: std.ArrayListUnmanaged(ExtensionStatusEntry) = .empty;
    defer entries.deinit(allocator);

    var any_invalid = false;
    for (parsed.paths.items) |path| {
        var entry = try loadExtensionStatusEntry(allocator, path);
        errdefer entry.deinit(allocator);
        if (entry.invalid) any_invalid = true;
        try entries.append(allocator, entry);
    }
    defer for (entries.items) |*e| e.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    if (parsed.json_mode) {
        try writeExtensionStatusJson(w, entries.items, !any_invalid);
    } else {
        try writeExtensionStatusText(w, entries.items);
    }

    buf = aw.toArrayList();
    writeOwnedToStdout(buf.items);

    if (any_invalid) std.process.exit(1);
}

const ExtensionStatusEntry = struct {
    path: []const u8,
    invalid: bool,
    /// Non-null when the manifest parsed cleanly.
    manifest: ?zigts.module_manifest.Manifest,
    audit: module_audit.VerifyResult,

    pub fn deinit(self: *ExtensionStatusEntry, allocator: std.mem.Allocator) void {
        if (self.manifest) |*m| m.deinit(allocator);
        self.audit.deinit(allocator);
    }
};

fn loadExtensionStatusEntry(
    allocator: std.mem.Allocator,
    path: []const u8,
) !ExtensionStatusEntry {
    var audit = try module_audit.verifyManifestPath(allocator, path);
    errdefer audit.deinit(allocator);

    var manifest_opt: ?zigts.module_manifest.Manifest = null;
    if (!audit.hasErrors()) {
        const bytes = zigts.file_io.readFile(allocator, path, 256 * 1024) catch null;
        if (bytes) |content| {
            defer allocator.free(content);
            if (zigts.module_manifest.parse(allocator, content)) |parsed_manifest| {
                manifest_opt = parsed_manifest;
            } else |_| {}
        }
    }

    return .{
        .path = path,
        .invalid = audit.hasErrors(),
        .manifest = manifest_opt,
        .audit = audit,
    };
}

pub fn runVerifyModuleManifest(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    if (hasHelpFlag(argv)) {
        printVerifyModuleManifestHelp();
        return;
    }

    var json_mode = false;
    var manifest_path: ?[]const u8 = null;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (!std.mem.startsWith(u8, arg, "-") and manifest_path == null) {
            manifest_path = arg;
        } else {
            printVerifyModuleManifestHelp();
            return error.InvalidArguments;
        }
    }

    const path = manifest_path orelse {
        printVerifyModuleManifestHelp();
        return error.MissingArgument;
    };

    const hash = rule_registry.policyHash();
    var result = try module_audit.verifyManifestPath(allocator, path);
    defer result.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    if (json_mode) {
        try module_audit.writeJsonEnvelope(w, &result, hash);
    } else if (result.diagnostics.items.len == 0) {
        try w.print("Verified module manifest: {s}\n", .{path});
    } else {
        try w.print("Verified module manifest: {d} issue(s)\n", .{result.diagnostics.items.len});
        for (result.diagnostics.items) |diag| {
            try w.print("  {s} {s}:{d}:{d}: {s}\n", .{
                diag.diag.code,
                diag.diag.file,
                diag.diag.line,
                diag.diag.column,
                diag.diag.message,
            });
        }
    }

    buf = aw.toArrayList();
    writeOwnedToStdout(buf.items);

    if (result.hasErrors()) std.process.exit(1);
}

const ExtensionStatusArgs = struct {
    json_mode: bool = false,
    paths: std.ArrayListUnmanaged([]const u8) = .empty,
};

fn parseExtensionStatusArgs(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
) !ExtensionStatusArgs {
    var parsed = ExtensionStatusArgs{};
    errdefer parsed.paths.deinit(allocator);

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--json")) {
            parsed.json_mode = true;
        } else if (std.mem.eql(u8, arg, "--module-manifest")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            try parsed.paths.append(allocator, argv[i]);
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            try parsed.paths.append(allocator, arg);
        } else {
            return error.InvalidArguments;
        }
    }
    return parsed;
}

fn writeExtensionStatusText(
    w: anytype,
    entries: []const ExtensionStatusEntry,
) !void {
    for (entries, 0..) |entry, idx| {
        if (idx > 0) try w.writeByte('\n');
        try w.print("Extension manifest: {s}\n", .{entry.path});
        if (entry.manifest) |manifest| {
            try w.print("  specifier: {s}\n", .{manifest.specifier});
            try w.print("  status: {s}\n", .{if (entry.invalid) "invalid" else "valid"});
            if (manifest.backend) |backend| {
                try w.print("  backend: {s}\n", .{@tagName(backend)});
            }
            if (manifest.state_model) |state| {
                try w.print("  state_model: {s}\n", .{@tagName(state)});
            }
            try w.writeAll("  capabilities:");
            if (manifest.required_capabilities.items.len == 0) {
                try w.writeAll(" (none)\n");
            } else {
                for (manifest.required_capabilities.items, 0..) |decl, i| {
                    try w.writeAll(if (i == 0) " " else ", ");
                    if (decl.partner_name) |name| {
                        try w.print("{s} (inherits {s})", .{ name, @tagName(decl.effective) });
                    } else {
                        try w.writeAll(@tagName(decl.effective));
                    }
                }
                try w.writeByte('\n');
            }
            if (manifest.contract_section) |section| {
                try w.print("  contract_section: {s}\n", .{section});
            }
            try w.print("  exports ({d}):\n", .{manifest.exports.items.len});
            for (manifest.exports.items) |exp| {
                try w.print("    {s}\n", .{exp.name});
                try w.print("      effect: {s}   returns: {s}   failure: {s}   traceable: {s}\n", .{
                    @tagName(exp.effect),
                    @tagName(exp.returns),
                    @tagName(exp.failure_severity),
                    if (exp.traceable) "yes" else "no",
                });
                try writeLabelSetText(w, exp.return_labels);
                if (exp.contract_extractions.items.len > 0) {
                    try w.print("      contract extractions: {d}\n", .{exp.contract_extractions.items.len});
                    for (exp.contract_extractions.items) |rule| {
                        try w.writeAll("        - ");
                        try writeContractRuleText(w, rule);
                        try w.writeByte('\n');
                    }
                }
            }
        } else {
            try w.print("  status: invalid (parse failed)\n", .{});
        }
        if (entry.audit.diagnostics.items.len > 0) {
            try w.writeAll("  validation issues:\n");
            for (entry.audit.diagnostics.items) |diag| {
                try w.print("    {s} {s} {s}:{d}:{d}: {s}\n", .{
                    diag.diag.code,
                    diag.diag.severity,
                    diag.diag.file,
                    diag.diag.line,
                    diag.diag.column,
                    diag.diag.message,
                });
            }
        }
    }
}

fn writeLabelSetText(w: anytype, labels: zigts.module_binding.LabelSet) !void {
    if (labels.isEmpty()) return;
    try w.writeAll("      return labels:");
    var first = true;
    inline for (@typeInfo(zigts.module_binding.DataLabel).@"enum".fields) |field| {
        const label: zigts.module_binding.DataLabel = @enumFromInt(field.value);
        if (labels.has(label)) {
            try w.print("{s}{s}", .{ if (first) " " else ", ", field.name });
            first = false;
        }
    }
    try w.writeByte('\n');
}

fn writeContractRuleText(w: anytype, rule: zigts.module_manifest.ContractExtractionRule) !void {
    try w.writeAll(@tagName(rule.category));
    if (rule.extension_category) |tag| try w.print("[{s}]", .{tag});
    try w.print(" arg={d}", .{rule.arg_position});
    if (rule.transform) |t| try w.print(" transform={s}", .{@tagName(t)});
    if (rule.flag_only) try w.writeAll(" flag_only");
}

fn writeExtensionStatusJson(
    w: anytype,
    entries: []const ExtensionStatusEntry,
    ok: bool,
) !void {
    try w.print("{{\"ok\":{s},\"manifests\":[", .{if (ok) "true" else "false"});
    for (entries, 0..) |entry, idx| {
        if (idx > 0) try w.writeByte(',');
        try w.writeAll("{\"path\":");
        try zigts.handler_contract.writeJsonString(w, entry.path);
        try w.print(",\"valid\":{s}", .{if (entry.invalid) "false" else "true"});
        if (entry.manifest) |manifest| {
            try w.writeAll(",\"specifier\":");
            try zigts.handler_contract.writeJsonString(w, manifest.specifier);
            if (manifest.backend) |backend| {
                try w.writeAll(",\"backend\":");
                try zigts.handler_contract.writeJsonString(w, @tagName(backend));
            }
            if (manifest.state_model) |state| {
                try w.writeAll(",\"stateModel\":");
                try zigts.handler_contract.writeJsonString(w, @tagName(state));
            }
            try w.writeAll(",\"requiredCapabilities\":[");
            for (manifest.required_capabilities.items, 0..) |decl, i| {
                if (i > 0) try w.writeByte(',');
                if (decl.partner_name) |name| {
                    try w.writeAll("{\"name\":");
                    try zigts.handler_contract.writeJsonString(w, name);
                    try w.writeAll(",\"inherits\":");
                    try zigts.handler_contract.writeJsonString(w, @tagName(decl.effective));
                    try w.writeByte('}');
                } else {
                    try zigts.handler_contract.writeJsonString(w, @tagName(decl.effective));
                }
            }
            try w.writeAll("]");
            if (manifest.contract_section) |section| {
                try w.writeAll(",\"contractSection\":");
                try zigts.handler_contract.writeJsonString(w, section);
            }
            try w.writeAll(",\"exports\":[");
            for (manifest.exports.items, 0..) |exp, i| {
                if (i > 0) try w.writeByte(',');
                try writeExportJson(w, exp);
            }
            try w.writeByte(']');
        }
        try w.writeAll(",\"issues\":[");
        for (entry.audit.diagnostics.items, 0..) |*diag, i| {
            if (i > 0) try w.writeByte(',');
            try @import("json_diagnostics.zig").writeDiagnosticJson(w, &diag.diag);
        }
        try w.writeAll("]}");
    }
    try w.writeAll("]}\n");
}

fn writeExportJson(w: anytype, exp: zigts.module_manifest.Export) !void {
    try w.writeAll("{\"name\":");
    try zigts.handler_contract.writeJsonString(w, exp.name);
    try w.writeAll(",\"effect\":");
    try zigts.handler_contract.writeJsonString(w, @tagName(exp.effect));
    try w.writeAll(",\"returns\":");
    try zigts.handler_contract.writeJsonString(w, @tagName(exp.returns));
    try w.writeAll(",\"failureSeverity\":");
    try zigts.handler_contract.writeJsonString(w, @tagName(exp.failure_severity));
    try w.print(",\"traceable\":{s}", .{if (exp.traceable) "true" else "false"});
    try w.writeAll(",\"returnLabels\":[");
    var first = true;
    inline for (@typeInfo(zigts.module_binding.DataLabel).@"enum".fields) |field| {
        const label: zigts.module_binding.DataLabel = @enumFromInt(field.value);
        if (exp.return_labels.has(label)) {
            if (!first) try w.writeByte(',');
            try zigts.handler_contract.writeJsonString(w, field.name);
            first = false;
        }
    }
    try w.writeAll("],\"contractExtractions\":[");
    for (exp.contract_extractions.items, 0..) |rule, i| {
        if (i > 0) try w.writeByte(',');
        try w.print("{{\"category\":\"{s}\",\"argPosition\":{d},\"flagOnly\":{s}", .{
            @tagName(rule.category),
            rule.arg_position,
            if (rule.flag_only) "true" else "false",
        });
        if (rule.transform) |t| {
            try w.writeAll(",\"transform\":");
            try zigts.handler_contract.writeJsonString(w, @tagName(t));
        }
        if (rule.extension_category) |tag| {
            try w.writeAll(",\"extensionCategory\":");
            try zigts.handler_contract.writeJsonString(w, tag);
        }
        try w.writeByte('}');
    }
    try w.writeAll("]}");
}

fn printExtensionStatusHelp() void {
    const help =
        \\zigts extension-status - report registered partner module manifests
        \\
        \\Usage:
        \\  zigts extension-status --module-manifest <path> [--module-manifest <path>...] [--json]
        \\  zigts extension-status <path> [<path>...] [--json]
        \\
        \\Loads each manifest, validates it via the same audit pass used by
        \\`zigts verify-module-manifest`, and prints the parsed specifier,
        \\backend, state model, capabilities, exports, effects, return kinds,
        \\failure severity, return labels, and contract extraction rules.
        \\Exit code 1 if any manifest fails validation.
        \\
    ;
    writeOwnedToStdout(help);
}

const VerifyModulesArgs = struct {
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

fn printMetaHelp() void {
    const help =
        \\zigts meta - show policy metadata
        \\
        \\Usage: zigts meta [--json]
        \\
        \\Outputs compiler version, policy version, policy hash, rule count,
        \\and category breakdown.
        \\
    ;
    writeOwnedToStdout(help);
}

fn printVerifyHelp() void {
    const help =
        \\zigts verify-paths - verify handler files
        \\
        \\Usage: zigts verify-paths <file>... [--json]
        \\
        \\Runs the full analysis pipeline on each file and reports violations.
        \\Exit code 1 if any errors found.
        \\
    ;
    writeOwnedToStdout(help);
}

fn printVerifyModulesHelp() void {
    const help =
        \\zigts verify-modules - audit built-in virtual module files
        \\
        \\Usage:
        \\  zigts verify-modules <file>... [--strict] [--json]
        \\  zigts verify-modules --builtins [--strict] [--json]
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
    writeOwnedToStdout(help);
}

fn printVerifyModuleManifestHelp() void {
    const help =
        \\zigts verify-module-manifest - validate a proof-carrying virtual module manifest
        \\
        \\Usage: zigts verify-module-manifest <manifest.json> [--json]
        \\
        \\Validates schema version, specifier, backend/state model, capabilities,
        \\exports, effects, return kinds, labels, contract extraction rules, and laws.
        \\Exit code 1 if any errors are found.
        \\
    ;
    writeOwnedToStdout(help);
}

// ---------------------------------------------------------------------------
// v1 contract tripwire
//
// `docs/zigts-expert-contract.md` freezes `zigts meta` output as v1.
// These tests fail loudly if a drive-by edit changes a pinned value. Any real
// change must also update the contract doc.
// ---------------------------------------------------------------------------

test "v1 contract: compiler_version pinned" {
    try std.testing.expectEqualStrings(zigts.version.string, compiler_version);
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

test "extension-status arg parser collects paths and --json" {
    var parsed = try parseExtensionStatusArgs(std.testing.allocator, &.{
        "--module-manifest",
        "a.json",
        "b.json",
        "--json",
    });
    defer parsed.paths.deinit(std.testing.allocator);

    try std.testing.expect(parsed.json_mode);
    try std.testing.expectEqual(@as(usize, 2), parsed.paths.items.len);
    try std.testing.expectEqualStrings("a.json", parsed.paths.items[0]);
    try std.testing.expectEqualStrings("b.json", parsed.paths.items[1]);
}

test "extension-status arg parser rejects --module-manifest without path" {
    try std.testing.expectError(
        error.MissingArgument,
        parseExtensionStatusArgs(std.testing.allocator, &.{"--module-manifest"}),
    );
}

test "extension-status arg parser rejects unknown flags" {
    try std.testing.expectError(
        error.InvalidArguments,
        parseExtensionStatusArgs(std.testing.allocator, &.{"--bogus"}),
    );
}

test "verify-modules arg parser rejects mixing builtins and positional paths" {
    try std.testing.expectError(
        error.InvalidArguments,
        parseVerifyModulesArgs(std.testing.allocator, &.{ "--builtins", "packages/zigts/src/modules/env.zig" }),
    );
}
