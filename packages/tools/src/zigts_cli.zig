const std = @import("std");

pub const precompile = @import("precompile.zig");
pub const deploy_manifest = @import("deploy_manifest.zig");
pub const upgrade_verifier = @import("upgrade_verifier.zig");
// Re-exports for the pi package, which consumes shared tool cores through
// the `zigts_cli` named module instead of reaching into tools/src/ directly.
pub const expert_meta = @import("expert_meta.zig");
pub const verify_paths_core = @import("verify_paths_core.zig");
pub const describe_rule = @import("describe_rule.zig");
pub const edit_simulate = @import("edit_simulate.zig");
pub const canonicalize = @import("canonicalize.zig");
pub const module_audit = @import("module_audit.zig");
pub const json_diagnostics = @import("json_diagnostics.zig");
pub const system_analysis = @import("system_analysis.zig");
pub const proof_quest_fixture = @import("proof_quest_fixture.zig");
const json_diag = precompile.json_diag;
const prove = @import("prove.zig");
const mock = @import("mock_server.zig");
pub const system_build = @import("system_build.zig");
const system_rollout = @import("system_rollout.zig");
const search_rules = @import("search_rules.zig");
const review_patch = @import("review_patch.zig");
const prove_behavior = @import("prove_behavior.zig");
const expert = @import("expert.zig");
const project_config_mod = @import("project_config");
const zigts = @import("zigts");
const zigts_file_io = zigts.file_io;
const writeContractJson = zigts.handler_contract.writeContractJson;

/// Help-grouping for the canonical analyzer surface. `analyze` commands act on
/// a handler/system; `machine` commands emit JSON metadata for IDE and
/// review-bot integrations.
pub const Category = enum { analyze, machine };

/// One dispatchable analyzer command. This table is the single source of truth
/// for the surface shared by the `zigts` binary and the `zigttp` developer CLI:
/// both dispatch from it (`run` / `isAnalyzerCommand`) and both render help from
/// it (`printHelp` / `writeCommandLines`). `compile` is intentionally absent -
/// it names different operations in each binary (binary output in `zigttp`,
/// `.zig` output in `zigts`) and stays a per-binary special case.
pub const Command = struct {
    name: []const u8,
    run: *const fn (std.mem.Allocator, []const []const u8) anyerror!void,
    /// Full usage line shown by `zigts --help` (without the leading `zigts `).
    usage: []const u8,
    /// Short argument hint shown by `zigttp help --all`.
    args: []const u8,
    /// One-line description shown by `zigttp help --all`.
    blurb: []const u8,
    category: Category,
};

pub const commands = [_]Command{
    .{ .name = "check", .run = runCheckCommand, .category = .analyze, .args = "[handler.ts]", .blurb = "Run the analyzer once", .usage = "check [handler.ts] [--json] [--contract] [--types] [--sql-schema path] [--system path]" },
    .{ .name = "prove", .run = prove.runWithArgs, .category = .analyze, .args = "<old.json> <new.json>", .blurb = "Contract upgrade safety check", .usage = "prove <old-contract.json> <new-contract.json> [output-dir/]" },
    .{ .name = "mock", .run = mock.runWithArgs, .category = .analyze, .args = "<tests.jsonl>", .blurb = "Mock server from test fixtures", .usage = "mock <tests.jsonl> [--port PORT]" },
    .{ .name = "link", .run = system_build.runWithArgs, .category = .analyze, .args = "<system.json>", .blurb = "Cross-handler system linking", .usage = "link <system.json> [--output-dir <dir>]" },
    .{ .name = "gen-tests", .run = runGenTestsCommand, .category = .analyze, .args = "[handler.ts]", .blurb = "Generate a starter test fixture", .usage = "gen-tests [handler.ts] [-o output.jsonl]" },
    .{ .name = "prove-behavior", .run = prove_behavior.runWithArgs, .category = .analyze, .args = "<before> <after>", .blurb = "Behavioral-equivalence verdict between two handler versions", .usage = "prove-behavior <before.ts> <after.ts> [--json] [--sql-schema path]" },
    .{ .name = "edit-simulate", .run = edit_simulate.runWithArgs, .category = .analyze, .args = "[handler.ts]", .blurb = "Simulate an edit and report violations", .usage = "edit-simulate [handler.ts] [--before old.ts] [--stdin-json]" },
    .{ .name = "review-patch", .run = review_patch.runWithArgs, .category = .analyze, .args = "<file>", .blurb = "Review a patch for new violations", .usage = "review-patch <file> [--before <old>] [--diff-only] [--json] [--stdin-json]" },
    .{ .name = "rollout", .run = system_rollout.runWithArgs, .category = .analyze, .args = "<old-system> <new>", .blurb = "System-level deployment manifest", .usage = "rollout <old-system.json> <new-system.json> [--output-dir <dir>]" },
    .{ .name = "canonicalize", .run = canonicalize.runWithArgs, .category = .analyze, .args = "<file> --json", .blurb = "Canonicalize a handler and report refactors", .usage = "canonicalize <file> --json [--simulate]" },
    .{ .name = "normalize", .run = canonicalize.runNormalizeWithArgs, .category = .analyze, .args = "<file>", .blurb = "Rewrite a handler into Canonical Normal Form", .usage = "normalize <file> [--write] [--check] [--json]" },
    .{ .name = "features", .run = runFeaturesCommand, .category = .machine, .args = "", .blurb = "List supported language features", .usage = "features [--json]" },
    .{ .name = "modules", .run = runModulesCommand, .category = .machine, .args = "", .blurb = "List virtual module exports", .usage = "modules [--json]" },
    .{ .name = "restrictions", .run = runRestrictionsCommand, .category = .machine, .args = "", .blurb = "Show language restrictions and the proofs they unlock", .usage = "restrictions [--json] [--by proof|class]" },
    .{ .name = "meta", .run = expert.runMeta, .category = .machine, .args = "", .blurb = "Compiler and policy metadata", .usage = "meta [--json]" },
    .{ .name = "describe-rule", .run = describe_rule.runWithArgs, .category = .machine, .args = "[name|code]", .blurb = "Look up a diagnostic rule", .usage = "describe-rule [rule-name|code] [--json] [--hash]" },
    .{ .name = "search", .run = search_rules.runWithArgs, .category = .machine, .args = "<keyword>", .blurb = "Search rules by keyword", .usage = "search <keyword> [--json]" },
    .{ .name = "spec-check", .run = runSpecCheckCommand, .category = .machine, .args = "", .blurb = "Check the semantics registry against the IR/bytecode tables", .usage = "spec-check [--json]" },
    .{ .name = "spec-hash", .run = runSpecHashCommand, .category = .machine, .args = "", .blurb = "Print the semantics-registry hash", .usage = "spec-hash" },
    .{ .name = "verify-paths", .run = expert.runVerifyPaths, .category = .machine, .args = "<file>...", .blurb = "Behavior-path verification", .usage = "verify-paths <file>... [--json]" },
    .{ .name = "verify-modules", .run = expert.runVerifyModules, .category = .machine, .args = "<file>...", .blurb = "Module-contract verification", .usage = "verify-modules <file>... [--strict] [--json] | --builtins" },
    .{ .name = "verify-module-manifest", .run = expert.runVerifyModuleManifest, .category = .machine, .args = "<manifest.json>", .blurb = "Verify a module manifest", .usage = "verify-module-manifest <manifest.json> [--json]" },
    .{ .name = "extension-status", .run = expert.runExtensionStatus, .category = .machine, .args = "--module-manifest <path>...", .blurb = "Report extension module status", .usage = "extension-status --module-manifest <path>... [--json]" },
};

/// True for every command in the shared registry. `compile` is excluded by
/// design (see `Command`), so callers route it separately.
pub fn isAnalyzerCommand(name: []const u8) bool {
    for (commands) |c| {
        if (std.mem.eql(u8, name, c.name)) return true;
    }
    return false;
}

/// Render the `zigttp help --all` lines for one category from the registry, so
/// the developer CLI's analyzer/machine sections cannot drift from the surface
/// `zigts` actually dispatches. Aligns the description column at byte 41.
pub fn writeCommandLines(w: *std.Io.Writer, category: Category) std.Io.Writer.Error!void {
    const prefix = "  zigttp ";
    const desc_col = 41;
    for (commands) |c| {
        if (c.category != category) continue;
        var width: usize = prefix.len + c.name.len;
        try w.writeAll(prefix);
        try w.writeAll(c.name);
        if (c.args.len > 0) {
            try w.writeByte(' ');
            try w.writeAll(c.args);
            width += 1 + c.args.len;
        }
        try w.splatByteAll(' ', if (width < desc_col) desc_col - width else 1);
        try w.writeAll(c.blurb);
        try w.writeByte('\n');
    }
}

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

    if (std.mem.eql(u8, command, "version") or
        std.mem.eql(u8, command, "--version") or
        std.mem.eql(u8, command, "-v"))
    {
        std.debug.print("zigts {s}\n", .{expert_meta.compiler_version});
        return;
    }

    // Note: `expert`/`ledger` are intercepted with a developer-CLI pointer in
    // zigts_main.zig before this runs, so they never reach the dispatch here.

    // `compile` names different operations in `zigts` (precompile to .zig) and
    // `zigttp` (build a binary), so it stays out of the shared registry.
    if (std.mem.eql(u8, command, "compile")) {
        try precompile.runCompileWithArgs(allocator, argv[1..]);
        return;
    }
    for (commands) |c| {
        if (std.mem.eql(u8, command, c.name)) {
            try c.run(allocator, argv[1..]);
            return;
        }
    }

    // Unknown command: a clean message and non-zero exit, not a Zig error trace
    // that IDE/CI integrations parsing stderr cannot read.
    std.debug.print("Unknown command: {s}\nRun `zigts --help` for the command list.\n", .{command});
    std.process.exit(1);
}

fn runCheckCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var sql_schema_path: ?[]const u8 = null;
    var explicit_system_path: ?[]const u8 = null;
    var handler_path: ?[]const u8 = null;
    var emit_contract = false;
    var emit_types = false;
    var json_mode = false;
    var require_export_capsules = false;

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
        if (std.mem.eql(u8, arg, "--system")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            explicit_system_path = argv[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "--types")) {
            emit_types = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--require-export-capsules")) {
            require_export_capsules = true;
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

    const discovered_system = if (explicit_system_path == null)
        try discoverProjectSystemPath(allocator, if (handler_path) |path| path else null)
    else
        null;
    defer if (discovered_system) |path| allocator.free(path);

    const system_path = explicit_system_path orelse discovered_system;

    var result = precompile.runCheckOnlyWithOptions(allocator, target, .{
        .sql_schema_path = sql_schema_path,
        .json_mode = json_mode,
        .system_path = system_path,
    }) catch |err| switch (err) {
        error.MissingSqlSchema => {
            if (json_mode) try writeMissingSqlSchemaJson(allocator, target);
            std.process.exit(1);
        },
        else => return err,
    };
    defer result.deinit(allocator);

    // Opt-in docs mode: ask exported helpers to carry explicit capsules
    // (ZTS507 / ZTS508). Warning-only; never changes the exit code.
    if (require_export_capsules) {
        precompile.appendExportCapsuleDiagnostics(allocator, &result, target);
    }

    if (json_mode) {
        // JSON output to stdout
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

        // Compute the witnesses summary for this handler from the on-disk
        // corpus. The block is inserted under proof.witnesses when present.
        // A missing corpus produces a {"total":0,"by_property":{}} block.
        var witnesses_buf: std.ArrayList(u8) = .empty;
        defer witnesses_buf.deinit(allocator);
        const witnesses_block: ?[]const u8 = blk: {
            const corpus_dir = zigts.witness_corpus.corpusDir(allocator, target) catch break :blk null;
            defer allocator.free(corpus_dir);
            var wbuf_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &witnesses_buf);
            zigts.witness_corpus.writeProofEnvelopeBlock(allocator, &wbuf_aw.writer, corpus_dir) catch {
                witnesses_buf = wbuf_aw.toArrayList();
                break :blk null;
            };
            witnesses_buf = wbuf_aw.toArrayList();
            break :blk if (witnesses_buf.items.len > 0) witnesses_buf.items else null;
        };

        if (result.totalErrors() > 0) {
            const contract_ptr: ?*const zigts.handler_contract.HandlerContract = if (result.contract) |*c| c else null;
            json_diag.writeErrorJson(&aw.writer, contract_ptr, result.json_diagnostics.items, witnesses_block, result.proof_trace_json) catch {};
        } else {
            const contract_ptr: ?*const zigts.handler_contract.HandlerContract = if (result.contract) |*c| c else null;
            json_diag.writeSuccessJson(&aw.writer, contract_ptr, result.json_diagnostics.items, witnesses_block, result.proof_trace_json) catch {};
        }

        buf = aw.toArrayList();
        if (buf.items.len > 0) {
            _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
        }

        if (result.totalErrors() > 0) std.process.exit(1);
        return;
    }

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

    if (require_export_capsules) {
        for (result.json_diagnostics.items) |d| {
            if (!std.mem.eql(u8, d.code, "ZTS507") and !std.mem.eql(u8, d.code, "ZTS508")) continue;
            std.debug.print("  {s} (warning) {s}:{d}  {s}\n", .{ d.code, d.file, d.line, d.message });
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
            zigts_file_io.writeFile(allocator, "contract.json", json_output.items) catch |err| {
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
        zigts_file_io.writeFile(allocator, "zigttp.d.ts", buf.items) catch |err| {
            std.debug.print("Error writing zigttp.d.ts: {}\n", .{err});
            return err;
        };
        std.debug.print("Wrote zigttp.d.ts\n", .{});
    }

    if (result.totalErrors() > 0) std.process.exit(1);
    if (result.totalWarnings() > 0) std.process.exit(2);
}

fn writeMissingSqlSchemaJson(allocator: std.mem.Allocator, target: []const u8) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try writeMissingSqlSchemaJsonToWriter(&aw.writer, target);

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }
}

fn writeMissingSqlSchemaJsonToWriter(writer: anytype, target: []const u8) !void {
    const diagnostics = [_]json_diag.JsonDiagnostic{.{
        .code = "ZTS700",
        .severity = "error",
        .message = "zigttp:sql queries require --sql-schema <schema.sql|schema.sqlite>",
        .file = target,
        .line = 1,
        .column = 1,
        .suggestion = "pass --sql-schema <schema.sql|schema.sqlite> or configure sqlite in zigttp.json",
    }};
    try json_diag.writeErrorJson(writer, null, diagnostics[0..], null, null);
}

/// A standard help token (`--help`, `-h`, or bare `help`), accepted by the
/// machine commands whose arg loops otherwise reject unknown flags.
fn isHelpToken(arg: []const u8) bool {
    return std.mem.eql(u8, arg, "--help") or
        std.mem.eql(u8, arg, "-h") or
        std.mem.eql(u8, arg, "help");
}

fn runFeaturesCommand(_: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (isHelpToken(arg)) {
            // Recognized so it is not rejected; this machine command has no
            // separate help screen, so it falls through to the normal catalog.
        } else {
            // Match restrictions/check: an unknown flag is a loud error, not a
            // silently-ignored arg that yields wrong output for tools and CI.
            return error.InvalidArgument;
        }
    }

    var buf: std.ArrayList(u8) = .empty;
    const allocator = std.heap.smp_allocator;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    if (json_mode) {
        json_diag.writeFeaturesJson(&aw.writer) catch return;
    } else {
        json_diag.writeFeaturesText(&aw.writer) catch return;
    }

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }
}

fn appendFmt(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), comptime fmt: []const u8, args: anytype) !void {
    const s = try std.fmt.allocPrint(allocator, fmt, args);
    defer allocator.free(s);
    try buf.appendSlice(allocator, s);
}

fn runSpecHashCommand(_: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (isHelpToken(arg)) {} else {
            return error.InvalidArgument;
        }
    }
    const hash = zigts.semantics.semanticsHash();
    if (json_mode) {
        const allocator = std.heap.smp_allocator;
        const line = try std.fmt.allocPrint(allocator, "{{\"semanticsHash\":\"{s}\"}}\n", .{hash});
        defer allocator.free(line);
        _ = std.c.write(std.c.STDOUT_FILENO, line.ptr, line.len);
    } else {
        _ = std.c.write(std.c.STDOUT_FILENO, &hash, hash.len);
        _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
    }
}

fn appendJsonString(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), s: []const u8) !void {
    try buf.append(allocator, '"');
    for (s) |c| {
        switch (c) {
            '"' => try buf.appendSlice(allocator, "\\\""),
            '\\' => try buf.appendSlice(allocator, "\\\\"),
            '\n' => try buf.appendSlice(allocator, "\\n"),
            else => try buf.append(allocator, c),
        }
    }
    try buf.append(allocator, '"');
}

fn runSpecCheckCommand(_: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (isHelpToken(arg)) {
            // no separate help screen
        } else {
            return error.InvalidArgument;
        }
    }

    const allocator = std.heap.smp_allocator;
    var result = zigts.semantics_check.runCheck(allocator) catch {
        const msg = "spec-check: internal error\n";
        _ = std.c.write(std.c.STDERR_FILENO, msg, msg.len);
        std.process.exit(2);
    };
    defer result.deinit();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    if (json_mode) {
        try writeSpecCheckJson(allocator, &buf, &result);
    } else {
        try writeSpecCheckText(allocator, &buf, &result);
    }
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }

    // exit 0 conform, 1 divergence (2 for internal error handled above).
    if (!result.ok()) std.process.exit(1);
}

fn writeSpecCheckText(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), r: *const zigts.semantics_check.CheckResult) !void {
    const cov = zigts.semantics.coverage();
    const sh = zigts.semantics.semanticsHash();
    try appendFmt(allocator, buf, "zigts semantics spec-check\n", .{});
    try appendFmt(allocator, buf, "  semantics hash:    {s}\n", .{sh});
    try appendFmt(allocator, buf, "  nodes specified:   {d}/{d}\n", .{ cov.nodes_specified, cov.nodes_total });
    try appendFmt(allocator, buf, "  opcodes specified: {d}/{d}\n", .{ cov.opcodes_specified, cov.opcodes_total });
    try appendFmt(allocator, buf, "  value proofs:      {d} (binop x{d}, unop x{d})\n", .{ r.nodes_proven, r.binop_instances, r.unop_instances });
    try appendFmt(allocator, buf, "  refinements:       {d}\n", .{r.refinements_proven});
    try appendFmt(allocator, buf, "  structural-only:   {d}\n", .{r.nodes_structural});
    try appendFmt(allocator, buf, "  stack-effect:      {d} checks\n", .{r.stack_effect_checked});
    if (r.ok()) {
        try appendFmt(allocator, buf, "  result:            PASS\n", .{});
    } else {
        try appendFmt(allocator, buf, "  result:            FAIL ({d} divergence)\n", .{r.failures.items.len});
        for (r.failures.items) |f| {
            try appendFmt(allocator, buf, "    {s} {s}: {s}\n", .{ f.code.code(), f.where, f.message });
        }
    }
}

fn writeSpecCheckJson(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), r: *const zigts.semantics_check.CheckResult) !void {
    const cov = zigts.semantics.coverage();
    const sh = zigts.semantics.semanticsHash();
    const ir_h = zigts.semantics.irTableHash();
    const op_h = zigts.semantics.opcodeTableHash();
    try appendFmt(allocator, buf, "{{\"semanticsHash\":\"{s}\",\"irTableHash\":\"{s}\",\"opcodeTableHash\":\"{s}\",", .{ sh, ir_h, op_h });
    try appendFmt(allocator, buf, "\"nodes\":{{\"specified\":{d},\"total\":{d}}},\"opcodes\":{{\"specified\":{d},\"total\":{d}}},", .{ cov.nodes_specified, cov.nodes_total, cov.opcodes_specified, cov.opcodes_total });
    try appendFmt(allocator, buf, "\"valueProofs\":{d},\"binopInstances\":{d},\"unopInstances\":{d},\"refinements\":{d},\"structural\":{d},\"stackEffectChecks\":{d},", .{ r.nodes_proven, r.binop_instances, r.unop_instances, r.refinements_proven, r.nodes_structural, r.stack_effect_checked });
    try appendFmt(allocator, buf, "\"ok\":{s},\"failures\":[", .{if (r.ok()) "true" else "false"});
    for (r.failures.items, 0..) |f, i| {
        if (i != 0) try buf.append(allocator, ',');
        try appendFmt(allocator, buf, "{{\"code\":\"{s}\",\"where\":\"{s}\",\"message\":", .{ f.code.code(), f.where });
        try appendJsonString(allocator, buf, f.message);
        try buf.append(allocator, '}');
    }
    try appendFmt(allocator, buf, "]}}\n", .{});
}

fn runRestrictionsCommand(_: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    var group_by: json_diag.RestrictionGrouping = .none;
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--by")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            const value = argv[i];
            if (std.mem.eql(u8, value, "proof")) {
                group_by = .proof;
            } else if (std.mem.eql(u8, value, "class") or std.mem.eql(u8, value, "failure_class")) {
                group_by = .failure_class;
            } else {
                return error.InvalidArgument;
            }
            continue;
        }
        if (std.mem.eql(u8, arg, "--help")) {
            const help =
                \\zigts restrictions - language cuts mapped to the proofs they unlock
                \\
                \\Usage: zigts restrictions [--json] [--by proof|class]
                \\
                \\Options:
                \\  --json            Emit structured JSON
                \\  --by proof        Group entries by the proof they unlock
                \\  --by class        Group entries by the failure class they prevent
                \\
            ;
            _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
            return;
        }
        return error.InvalidArgument;
    }

    var buf: std.ArrayList(u8) = .empty;
    const allocator = std.heap.smp_allocator;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    if (json_mode) {
        json_diag.writeRestrictionsJson(&aw.writer) catch return;
    } else {
        json_diag.writeRestrictionsText(&aw.writer, group_by) catch return;
    }

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }
}

fn runModulesCommand(_: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (isHelpToken(arg)) {
            // Recognized so it is not rejected; this machine command has no
            // separate help screen, so it falls through to the normal catalog.
        } else {
            return error.InvalidArgument;
        }
    }

    var buf: std.ArrayList(u8) = .empty;
    const allocator = std.heap.smp_allocator;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    if (json_mode) {
        json_diag.writeModulesJson(&aw.writer) catch return;
    } else {
        json_diag.writeModulesText(&aw.writer) catch return;
    }

    buf = aw.toArrayList();
    if (buf.items.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }
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

fn discoverProjectSystemPath(allocator: std.mem.Allocator, start_path: ?[]const u8) !?[]u8 {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var project = try project_config_mod.discover(allocator, io, start_path);
    defer if (project) |*p| p.deinit(allocator);

    if (project) |*cfg| {
        return try cfg.resolvedSystemPath(allocator);
    }
    return null;
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

const help_head =
    \\zigts - compiler and analysis tools for zigttp handlers
    \\
    \\Usage:
    \\  zigts compile [precompile flags] <handler.ts> <output.zig>
    \\
;

const help_tail =
    \\
    \\The interactive `expert` agent and session `ledger` commands live in the
    \\developer CLI: run `zigttp expert` / `zigttp ledger`.
    \\
;

fn printHelp() void {
    var buf: [4096]u8 = undefined;
    var w = std.Io.Writer.fixed(&buf);
    // The usage lines come from the shared registry so `zigts` and `zigttp`
    // advertise exactly the surface they dispatch.
    w.writeAll(help_head) catch {};
    for (commands) |c| {
        w.print("  zigts {s}\n", .{c.usage}) catch {};
    }
    w.writeAll(help_tail) catch {};
    const out = w.buffered();
    _ = std.c.write(std.c.STDOUT_FILENO, out.ptr, out.len);
}

fn printCheckHelp() void {
    const help =
        \\zigts check - verify handler and show proof card
        \\
        \\Usage: zigts check [handler.ts] [options]
        \\
        \\Options:
        \\  --json           Emit structured JSON to stdout (for tool integration)
        \\  --contract       Emit contract.json in current directory
        \\  --types          Emit zigttp.d.ts type definitions for IDE autocomplete
        \\  --sql-schema P   SQLite schema file for query validation
        \\  --system P       system.json for internal serviceCall typing
        \\  --require-export-capsules
        \\                   Docs mode: warn (ZTS507/ZTS508) when an exported
        \\                   helper carries no Effects<...> / Proof<...> capsule
        \\
        \\If no handler is specified, uses the entry from zigttp.json.
        \\
        \\Exit codes:
        \\  0  ok (no errors)
        \\  1  one or more errors
        \\  2  warnings only (no errors)
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

fn runGenTestsCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var handler_path: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            output_path = argv[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "--help")) {
            printGenTestsHelp();
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

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const count = try precompile.runGenTests(allocator, target, &aw.writer);
    buf = aw.toArrayList();

    if (output_path) |out_path| {
        try zigts_file_io.writeFile(allocator, out_path, buf.items);
        const msg = try std.fmt.allocPrint(allocator, "Wrote {d} test{s} to {s}\n", .{
            count,
            if (count == 1) @as([]const u8, "") else "s",
            out_path,
        });
        defer allocator.free(msg);
        _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
    } else {
        if (buf.items.len > 0) {
            _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
        }
    }
}

fn printGenTestsHelp() void {
    const help =
        \\zigts gen-tests - generate JSONL tests from proven behavioral paths
        \\
        \\Usage: zigts gen-tests [handler.ts] [-o output.jsonl]
        \\
        \\Options:
        \\  -o, --output <file>  Write tests to file instead of stdout
        \\
        \\Enumerates all execution paths through the handler and emits a
        \\declarative JSONL test case for each path. Output is immediately
        \\runnable via `--test` or `zigttp mock`.
        \\
        \\If no handler is specified, uses the entry from zigttp.json.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

test "missing sql schema json uses diagnostic envelope" {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);

    try writeMissingSqlSchemaJsonToWriter(&aw.writer, "handler.ts");

    buf = aw.toArrayList();
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"success\":false") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "\"code\":\"ZTS700\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, buf.items, "--sql-schema") != null);
}

test {
    // Collect the command modules reached only through this dispatcher. A plain
    // `@import` (above) does not pull a file's `test {}` blocks into the test
    // binary; this explicit reference does. Mirrors dev_cli.zig's anchor block
    // (see MEMORY.md "CLI test-collection closure").
    _ = describe_rule;
    _ = search_rules;
}
