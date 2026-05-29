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
const system_build = @import("system_build.zig");
const system_rollout = @import("system_rollout.zig");
const search_rules = @import("search_rules.zig");
const review_patch = @import("review_patch.zig");
const prove_behavior = @import("prove_behavior.zig");
const canonicalize_cmd = @import("canonicalize.zig");
const expert = @import("expert.zig");
const project_config_mod = @import("project_config");
const zigts = @import("zigts");
const zigts_file_io = zigts.file_io;
const writeContractJson = zigts.handler_contract.writeContractJson;

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
    if (std.mem.eql(u8, command, "compile")) {
        try precompile.runCompileWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "check")) {
        try runCheckCommand(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "prove")) {
        try prove.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "mock")) {
        try mock.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "link")) {
        try system_build.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "rollout")) {
        try system_rollout.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "features")) {
        try runFeaturesCommand(argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "modules")) {
        try runModulesCommand(argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "restrictions")) {
        try runRestrictionsCommand(argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "meta")) {
        try expert.runMeta(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "verify-paths")) {
        try expert.runVerifyPaths(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "verify-modules")) {
        try expert.runVerifyModules(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "verify-module-manifest")) {
        try expert.runVerifyModuleManifest(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "extension-status")) {
        try expert.runExtensionStatus(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "edit-simulate")) {
        try edit_simulate.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "canonicalize")) {
        try canonicalize_cmd.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "describe-rule")) {
        try describe_rule.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "search")) {
        try search_rules.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "review-patch")) {
        try review_patch.runWithArgs(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "gen-tests")) {
        try runGenTestsCommand(allocator, argv[1..]);
        return;
    }
    if (std.mem.eql(u8, command, "prove-behavior")) {
        try prove_behavior.runWithArgs(allocator, argv[1..]);
        return;
    }
    printHelp();
    return error.UnknownCommand;
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

    var result = try precompile.runCheckOnlyWithOptions(allocator, target, .{
        .sql_schema_path = sql_schema_path,
        .json_mode = json_mode,
        .system_path = system_path,
    });
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

fn runFeaturesCommand(argv: []const []const u8) !void {
    var json_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) json_mode = true;
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

fn runRestrictionsCommand(argv: []const []const u8) !void {
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

fn runModulesCommand(argv: []const []const u8) !void {
    var json_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) json_mode = true;
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

fn printHelp() void {
    const help =
        \\zigts - compiler and analysis tools for zigttp handlers
        \\
        \\Usage:
        \\  zigts check [handler.ts] [--json] [--contract] [--types] [--sql-schema path] [--system path]
        \\  zigts compile [precompile flags] <handler.ts> <output.zig>
        \\  zigts prove <old-contract.json> <new-contract.json> [output-dir/]
        \\  zigts mock <tests.jsonl> [--port PORT]
        \\  zigts link <system.json> [--output-dir <dir>]
        \\  zigts rollout <old-system.json> <new-system.json> [--output-dir <dir>]
        \\  zigts features [--json]
        \\  zigts modules [--json]
        \\  zigts restrictions [--json] [--by proof|class]
        \\  zigts meta [--json]
        \\  zigts verify-paths <file>... [--json]
        \\  zigts verify-modules <file>... [--strict] [--json]
        \\  zigts verify-modules --builtins [--strict] [--json]
        \\  zigts verify-module-manifest <manifest.json> [--json]
        \\  zigts extension-status --module-manifest <path>... [--json]
        \\  zigts edit-simulate [handler.ts] [--before old.ts] [--stdin-json]
        \\  zigts canonicalize <file> --json [--simulate]
        \\  zigts describe-rule [rule-name|code] [--json] [--hash]
        \\  zigts search <keyword> [--json]
        \\  zigts review-patch <file> [--before <old>] [--diff-only] [--json] [--stdin-json]
        \\  zigts prove-behavior <before.ts> <after.ts> [--json]
        \\  zigts gen-tests [handler.ts] [-o output.jsonl]
        \\  zigts expert                          (interactive coding agent)
        \\  zigts ledger export --session <id> --out <path>
        \\  zigts ledger replay --input <path> --onto <git-ref>
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
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
