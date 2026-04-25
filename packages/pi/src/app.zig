//! Top-level entrypoint for `zigts expert`.

const std = @import("std");
const registry_mod = @import("registry/registry.zig");
const repl = @import("repl.zig");
const tui_app = @import("tui/app.zig");
const loop = @import("loop.zig");
const print_mode = @import("print_mode.zig");
const rpc_mode = @import("rpc_mode.zig");
const ledger = @import("ledger.zig");
const autoloop = @import("autoloop.zig");
const transcript_mod = @import("transcript.zig");
const session_state = @import("session_state.zig");
const property_goals = @import("property_goals.zig");
const tools_common = @import("tools/common.zig");

const meta_tool = @import("tools/zigts_expert_meta.zig");
const verify_paths_tool = @import("tools/zigts_expert_verify_paths.zig");
const describe_rule_tool = @import("tools/zigts_expert_describe_rule.zig");
const search_tool = @import("tools/zigts_expert_search.zig");
const edit_simulate_tool = @import("tools/zigts_expert_edit_simulate.zig");
const review_patch_tool = @import("tools/zigts_expert_review_patch.zig");
const prove_patch_tool = @import("tools/zigts_expert_prove_patch.zig");
const system_proof_tool = @import("tools/zigts_expert_system_proof.zig");
const features_tool = @import("tools/zigts_expert_features.zig");
const modules_tool = @import("tools/zigts_expert_modules.zig");
const verify_modules_tool = @import("tools/zigts_expert_verify_modules.zig");
const workspace_list_files_tool = @import("tools/workspace_list_files.zig");
const workspace_read_file_tool = @import("tools/workspace_read_file.zig");
const workspace_search_text_tool = @import("tools/workspace_search_text.zig");
const zigts_check_tool = @import("tools/zigts_check.zig");
const zig_build_step_tool = @import("tools/zig_build_step.zig");
const zig_test_step_tool = @import("tools/zig_test_step.zig");
const gen_tests_tool = @import("tools/gen_tests.zig");
const pi_goal_check_tool = @import("tools/pi_goal_check.zig");
const pi_repair_plan_tool = @import("tools/pi_repair_plan.zig");
const pi_apply_repair_plan_tool = @import("tools/pi_apply_repair_plan.zig");

const Registry = registry_mod.Registry;

pub fn buildMinimalRegistry(allocator: std.mem.Allocator) !Registry {
    var reg: Registry = .{};
    errdefer reg.deinit(allocator);
    try reg.register(allocator, workspace_read_file_tool.tool);
    try reg.register(allocator, workspace_list_files_tool.tool);
    try reg.register(allocator, workspace_search_text_tool.tool);
    return reg;
}

pub fn buildRegistry(allocator: std.mem.Allocator) !Registry {
    var reg: Registry = .{};
    errdefer reg.deinit(allocator);

    try reg.register(allocator, meta_tool.tool);
    try reg.register(allocator, verify_paths_tool.tool);
    try reg.register(allocator, describe_rule_tool.tool);
    try reg.register(allocator, search_tool.tool);
    try reg.register(allocator, edit_simulate_tool.tool);
    try reg.register(allocator, review_patch_tool.tool);
    try reg.register(allocator, prove_patch_tool.tool);
    try reg.register(allocator, system_proof_tool.tool);
    try reg.register(allocator, features_tool.tool);
    try reg.register(allocator, modules_tool.tool);
    try reg.register(allocator, verify_modules_tool.tool);
    try reg.register(allocator, workspace_list_files_tool.tool);
    try reg.register(allocator, workspace_read_file_tool.tool);
    try reg.register(allocator, workspace_search_text_tool.tool);
    try reg.register(allocator, zigts_check_tool.tool);
    try reg.register(allocator, zig_build_step_tool.tool);
    try reg.register(allocator, zig_test_step_tool.tool);
    try reg.register(allocator, gen_tests_tool.tool);
    try reg.register(allocator, pi_goal_check_tool.tool);
    try reg.register(allocator, pi_repair_plan_tool.tool);
    try reg.register(allocator, pi_apply_repair_plan_tool.tool);

    return reg;
}

/// Long flags whose next token is a value. `zigts_main.zig` consults this
/// list so it can skip the value while scanning for stray positional args.
pub const value_taking_flags = [_][]const u8{ "--session-id", "--print", "--mode", "--tools", "--fork", "--goal", "--max-iters", "--handler" };

var captured_argv: ?[]const []const u8 = null;

pub fn setInvocationArgv(argv: []const []const u8) void {
    captured_argv = argv;
}

pub fn run(allocator: std.mem.Allocator) !void {
    const argv = captured_argv orelse &[_][]const u8{};
    const flags = parseExpertFlags(argv) catch |err| exitWithMessage(flagErrorMessage(err), 2);

    var registry = switch (flags.tools_preset) {
        .full => try buildRegistry(allocator),
        .minimal => try buildMinimalRegistry(allocator),
    };
    defer registry.deinit(allocator);

    if (flags.goals != null) {
        try runAutoloop(allocator, &registry, flags);
        return;
    }

    if (flags.rpc_mode) {
        try rpc_mode.run(allocator, &registry, flags, flags.policy orelse .auto_reject);
        return;
    }

    if (flags.print != null) {
        try print_mode.run(allocator, &registry, flags, flags.policy orelse .auto_reject);
        return;
    }

    const interactive_policy = flags.policy orelse .ask;
    const is_tty = std.c.isatty(std.c.STDIN_FILENO) != 0;
    if (is_tty) {
        try tui_app.run(allocator, &registry, flags, interactive_policy);
    } else {
        try repl.run(allocator, &registry, flags, interactive_policy);
    }
}

pub fn runLedgerCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    try ledger.runWithArgs(allocator, argv);
}

fn runAutoloop(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    flags: ExpertFlags,
) !void {
    const goals_csv = flags.goals orelse return;
    const handler = flags.handler orelse return;

    const goals = try splitCsv(allocator, goals_csv);
    defer {
        for (goals) |g| allocator.free(g);
        allocator.free(goals);
    }
    if (goals.len == 0) exitWithMessage("error: --goal list is empty\n", 2);
    for (goals) |goal| {
        if (!property_goals.isGoalDriveable(goal)) {
            var stderr: [256]u8 = undefined;
            const line = std.fmt.bufPrint(
                &stderr,
                "error: unsupported autoloop goal '{s}' in --goal; try one of: {s}\n",
                .{ goal, property_goals.supported_goal_list },
            ) catch "error: unsupported autoloop goal in --goal\n";
            exitWithMessage(line, 2);
        }
    }

    const workspace_root = try tools_common.workspaceRoot(allocator);
    defer allocator.free(workspace_root);

    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(allocator);

    var budget: autoloop.Budget = .{};
    if (flags.max_iters) |n| budget.max_iterations = n;

    const goal_slices = try allocator.alloc([]const u8, goals.len);
    defer allocator.free(goal_slices);
    for (goals, 0..) |g, i| goal_slices[i] = g;

    const outcome = autoloop.drive(allocator, registry, &transcript, .{
        .workspace_root = workspace_root,
        .file = handler,
        .goals = goal_slices,
        .budget = budget,
    }) catch |err| {
        var stderr: [256]u8 = undefined;
        const line = std.fmt.bufPrint(&stderr, "autoloop error: {s}\n", .{@errorName(err)}) catch "autoloop error\n";
        exitWithMessage(line, 1);
    };

    try printAutoloopOutcome(allocator, outcome, &transcript, handler, goal_slices);
    if (outcome.verdict != .achieved) std.process.exit(1);
}

fn printAutoloopOutcome(
    allocator: std.mem.Allocator,
    outcome: autoloop.Outcome,
    transcript: *const transcript_mod.Transcript,
    file: []const u8,
    goals: []const []const u8,
) !void {
    const props = session_state.currentProperties(transcript, file);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.print("autoloop verdict: {s}\n", .{@tagName(outcome.verdict)});
    try w.print("iterations: {d}\n", .{outcome.iterations});
    try w.writeAll("goals:\n");
    for (goals) |goal| {
        // .achieved means pi_goal_check reported ok for every requested goal
        // on this handler, even when no patch was needed and the transcript
        // holds no VerifiedPatch snapshot to derive properties from.
        const met = outcome.verdict == .achieved or
            (if (props) |p| session_state.propertyByName(p, goal) else false);
        try w.print("  {s} {s}\n", .{ if (met) "[x]" else "[ ]", goal });
    }
    if (outcome.final_patch_hash) |hash| {
        const hex = std.fmt.bytesToHex(hash, .lower);
        try w.writeAll("final_patch_hash: ");
        try w.writeAll(&hex);
        try w.writeByte('\n');
    }

    buf = aw.toArrayList();
    _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
}

fn splitCsv(allocator: std.mem.Allocator, csv: []const u8) ![][]u8 {
    var out: std.ArrayList([]u8) = .empty;
    errdefer {
        for (out.items) |s| allocator.free(s);
        out.deinit(allocator);
    }
    var it = std.mem.splitScalar(u8, csv, ',');
    while (it.next()) |part| {
        const trimmed = std.mem.trim(u8, part, " \t");
        if (trimmed.len == 0) continue;
        try out.append(allocator, try allocator.dupe(u8, trimmed));
    }
    return out.toOwnedSlice(allocator);
}

pub const ToolsPreset = enum { full, minimal };

fn parseToolsPreset(val: []const u8) !ToolsPreset {
    if (std.mem.eql(u8, val, "minimal")) return .minimal;
    if (std.mem.eql(u8, val, "full")) return .full;
    return error.UnsupportedToolsPreset;
}

fn exitWithMessage(msg: []const u8, code: u8) noreturn {
    _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
    std.process.exit(code);
}

fn flagErrorMessage(err: anyerror) []const u8 {
    return switch (err) {
        error.MutuallyExclusiveApprovalFlags => "error: --yes and --no-edit are mutually exclusive\n",
        error.MissingSessionId => "error: --session-id requires a value\n",
        error.MutuallyExclusiveResumeFlags => "error: --resume and --session-id are mutually exclusive\n",
        error.MissingPrintPrompt => "error: --print requires a value\n",
        error.MissingModeValue => "error: --mode requires a value (json|rpc)\n",
        error.UnsupportedMode => "error: --mode only accepts 'json' or 'rpc'\n",
        error.JsonModeRequiresPrint => "error: --mode json requires --print <prompt>\n",
        error.RpcModeConflictsWithPrint => "error: --mode rpc cannot be combined with --print\n",
        error.MissingToolsPreset => "error: --tools requires a value (full, minimal)\n",
        error.UnsupportedToolsPreset => "error: --tools only accepts 'full' or 'minimal'\n",
        error.MissingForkSessionId => "error: --fork requires a session id value\n",
        error.MutuallyExclusiveForkFlags => "error: --fork is mutually exclusive with --resume, --continue, and --session-id\n",
        error.MissingGoalValue => "error: --goal requires a comma-separated list of property tags\n",
        error.MissingMaxItersValue => "error: --max-iters requires a positive integer\n",
        error.InvalidMaxIters => "error: --max-iters must be a positive integer\n",
        error.MissingHandlerValue => "error: --handler requires a path value\n",
        error.GoalRequiresHandler => "error: --goal requires --handler <path>\n",
        error.GoalConflictsWithPrintOrRpc => "error: --goal cannot be combined with --print or --mode rpc\n",
        else => "error: unexpected flag parse failure\n",
    };
}

fn parseMaxIters(val: []const u8) !u32 {
    const n = std.fmt.parseInt(u32, val, 10) catch return error.InvalidMaxIters;
    if (n == 0) return error.InvalidMaxIters;
    return n;
}

fn setMode(out: *ExpertFlags, val: []const u8) !void {
    if (std.mem.eql(u8, val, "json")) {
        out.json_mode = true;
        return;
    }
    if (std.mem.eql(u8, val, "rpc")) {
        out.rpc_mode = true;
        return;
    }
    return error.UnsupportedMode;
}

/// Flags parsed from `zigts expert` argv. `policy == null` means the user
/// did not pass `--yes` or `--no-edit`; callers pick an appropriate default
/// (`.ask` for interactive, `.auto_reject` for `--print`).
pub const ExpertFlags = struct {
    policy: ?loop.ApprovalPolicy = null,
    no_session: bool = false,
    no_persist_tool_output: bool = false,
    /// Skip the AGENTS.md / CLAUDE.md project-context walk. System prompt
    /// still ships full persona + live snapshots; only the appended
    /// project-context section is suppressed.
    no_context_files: bool = false,
    session_id: ?[]const u8 = null,
    resume_latest: bool = false,
    fork_session_id: ?[]const u8 = null,
    print: ?[]const u8 = null,
    json_mode: bool = false,
    /// Line-delimited JSON-RPC 2.0 over stdio. Long-lived session; mutually
    /// exclusive with --print.
    rpc_mode: bool = false,
    tools_preset: ToolsPreset = .full,
    /// Comma-separated property tags to drive convergence against. When
    /// non-null, `zigts expert` short-circuits the conversational run and
    /// invokes the autoloop orchestrator end-to-end.
    goals: ?[]const u8 = null,
    /// Iteration budget for the autoloop. When null, the orchestrator's
    /// default (8) applies.
    max_iters: ?u32 = null,
    /// Handler path the autoloop operates on. Required whenever `goals` is
    /// set; ignored otherwise.
    handler: ?[]const u8 = null,
};

/// Scan argv for the expert launch flags. Unknown `--*` tokens are ignored so
/// future slices can add their own without breaking this parser. `--yes` and
/// `--no-edit` together return an error so the caller can report a clear
/// diagnostic. Order-independent; repetition is idempotent.
pub fn parseExpertFlags(argv: []const []const u8) !ExpertFlags {
    var out: ExpertFlags = .{};
    var saw_yes = false;
    var saw_no_edit = false;
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--yes")) saw_yes = true;
        if (std.mem.eql(u8, arg, "--no-edit")) saw_no_edit = true;
        if (std.mem.eql(u8, arg, "--no-session")) out.no_session = true;
        if (std.mem.eql(u8, arg, "--no-persist-tool-output")) out.no_persist_tool_output = true;
        if (std.mem.eql(u8, arg, "--no-context-files")) out.no_context_files = true;
        if (std.mem.eql(u8, arg, "--resume") or std.mem.eql(u8, arg, "--continue")) out.resume_latest = true;
        if (std.mem.eql(u8, arg, "--fork")) {
            if (i + 1 >= argv.len) return error.MissingForkSessionId;
            i += 1;
            out.fork_session_id = argv[i];
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--fork=")) {
            out.fork_session_id = arg["--fork=".len..];
        }
        if (std.mem.eql(u8, arg, "--tools")) {
            if (i + 1 >= argv.len) return error.MissingToolsPreset;
            i += 1;
            out.tools_preset = try parseToolsPreset(argv[i]);
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--tools=")) {
            out.tools_preset = try parseToolsPreset(arg["--tools=".len..]);
        }
        if (std.mem.eql(u8, arg, "--session-id")) {
            if (i + 1 >= argv.len) return error.MissingSessionId;
            i += 1;
            out.session_id = argv[i];
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--session-id=")) {
            out.session_id = arg["--session-id=".len..];
        }
        if (std.mem.eql(u8, arg, "--print")) {
            if (i + 1 >= argv.len) return error.MissingPrintPrompt;
            i += 1;
            out.print = argv[i];
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--print=")) {
            out.print = arg["--print=".len..];
            continue;
        }
        if (std.mem.eql(u8, arg, "--mode")) {
            if (i + 1 >= argv.len) return error.MissingModeValue;
            i += 1;
            try setMode(&out, argv[i]);
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--mode=")) {
            try setMode(&out, arg["--mode=".len..]);
        }
        if (std.mem.eql(u8, arg, "--goal")) {
            if (i + 1 >= argv.len) return error.MissingGoalValue;
            i += 1;
            out.goals = argv[i];
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--goal=")) {
            out.goals = arg["--goal=".len..];
        }
        if (std.mem.eql(u8, arg, "--max-iters")) {
            if (i + 1 >= argv.len) return error.MissingMaxItersValue;
            i += 1;
            out.max_iters = try parseMaxIters(argv[i]);
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--max-iters=")) {
            out.max_iters = try parseMaxIters(arg["--max-iters=".len..]);
        }
        if (std.mem.eql(u8, arg, "--handler")) {
            if (i + 1 >= argv.len) return error.MissingHandlerValue;
            i += 1;
            out.handler = argv[i];
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--handler=")) {
            out.handler = arg["--handler=".len..];
        }
    }
    if (saw_yes and saw_no_edit) return error.MutuallyExclusiveApprovalFlags;
    if (out.resume_latest and out.session_id != null) return error.MutuallyExclusiveResumeFlags;
    if (out.fork_session_id != null and out.resume_latest) return error.MutuallyExclusiveForkFlags;
    if (out.fork_session_id != null and out.session_id != null) return error.MutuallyExclusiveForkFlags;
    if (out.json_mode and out.print == null) return error.JsonModeRequiresPrint;
    if (out.rpc_mode and out.print != null) return error.RpcModeConflictsWithPrint;
    if (out.goals != null and out.handler == null) return error.GoalRequiresHandler;
    if (out.goals != null and (out.print != null or out.rpc_mode)) return error.GoalConflictsWithPrintOrRpc;
    if (saw_yes) {
        out.policy = .auto_approve;
    } else if (saw_no_edit) {
        out.policy = .auto_reject;
    }
    return out;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

// When adding new tools to buildRegistry, extend the expected_names list below.
// Do not reintroduce a hardcoded count: the assertion is name-based, not numeric.
test "buildRegistry registers every first-party compiler primitive" {
    var reg = try buildRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    const expected_names = [_][]const u8{
        "zigts_expert_meta",
        "zigts_expert_verify_paths",
        "zigts_expert_describe_rule",
        "zigts_expert_search",
        "zigts_expert_edit_simulate",
        "zigts_expert_review_patch",
        "zigts_expert_prove_patch",
        "zigts_expert_system_proof",
        "zigts_expert_features",
        "zigts_expert_modules",
        "zigts_expert_verify_modules",
        "workspace_list_files",
        "workspace_read_file",
        "workspace_search_text",
        "zigts_check",
        "zig_build_step",
        "zig_test_step",
        "workspace_gen_tests",
        "pi_goal_check",
        "pi_repair_plan",
        "pi_apply_repair_plan",
    };

    for (expected_names) |expected| {
        if (reg.findByName(expected) == null) {
            std.debug.print("missing tool: {s}\n", .{expected});
            return error.TestFailed;
        }
    }
    try testing.expect(reg.count() >= expected_names.len);
}

fn expectOkContains(outcome: *repl.DispatchOutcome, allocator: std.mem.Allocator, needle: []const u8) !void {
    switch (outcome.*) {
        .result => |*r| {
            defer r.deinit(allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.llm_text, needle) != null);
        },
        else => return error.TestFailed,
    }
}

test "parseExpertFlags: empty argv yields defaults" {
    const flags = try parseExpertFlags(&.{});
    try testing.expect(flags.policy == null);
    try testing.expectEqual(false, flags.no_session);
    try testing.expectEqual(false, flags.no_persist_tool_output);
}

test "parseExpertFlags: --yes yields auto_approve" {
    const argv = [_][]const u8{ "zigts", "expert", "--yes" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.auto_approve, flags.policy.?);
    try testing.expectEqual(false, flags.no_session);
    try testing.expectEqual(false, flags.no_persist_tool_output);
}

test "parseExpertFlags: --no-edit yields auto_reject" {
    const argv = [_][]const u8{ "zigts", "expert", "--no-edit" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.auto_reject, flags.policy.?);
}

test "parseExpertFlags: --no-session alone flips only that field" {
    const argv = [_][]const u8{ "zigts", "expert", "--no-session" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expect(flags.policy == null);
    try testing.expectEqual(true, flags.no_session);
    try testing.expectEqual(false, flags.no_persist_tool_output);
}

test "parseExpertFlags: --no-persist-tool-output alone flips only that field" {
    const argv = [_][]const u8{ "zigts", "expert", "--no-persist-tool-output" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expect(flags.policy == null);
    try testing.expectEqual(false, flags.no_session);
    try testing.expectEqual(true, flags.no_persist_tool_output);
}

test "parseExpertFlags: --yes --no-session --no-persist-tool-output combine" {
    const argv = [_][]const u8{ "zigts", "expert", "--yes", "--no-session", "--no-persist-tool-output" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.auto_approve, flags.policy.?);
    try testing.expectEqual(true, flags.no_session);
    try testing.expectEqual(true, flags.no_persist_tool_output);
}

test "parseExpertFlags: --yes + --no-edit still errors regardless of session flags" {
    const argv = [_][]const u8{ "zigts", "expert", "--yes", "--no-session", "--no-edit" };
    try testing.expectError(error.MutuallyExclusiveApprovalFlags, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --no-context-files flips only that field" {
    const argv = [_][]const u8{ "zigts", "expert", "--no-context-files" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(true, flags.no_context_files);
    try testing.expectEqual(false, flags.no_session);
    try testing.expectEqual(false, flags.no_persist_tool_output);
}

test "parseExpertFlags: --no-context-files defaults to false" {
    const argv = [_][]const u8{ "zigts", "expert" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(false, flags.no_context_files);
}

test "parseExpertFlags: --mode rpc sets rpc_mode" {
    const argv = [_][]const u8{ "zigts", "expert", "--mode", "rpc" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(true, flags.rpc_mode);
    try testing.expectEqual(false, flags.json_mode);
}

test "parseExpertFlags: --mode=rpc inline form" {
    const argv = [_][]const u8{ "zigts", "expert", "--mode=rpc" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(true, flags.rpc_mode);
}

test "parseExpertFlags: --mode rpc with --print errors" {
    const argv = [_][]const u8{ "zigts", "expert", "--mode", "rpc", "--print", "hi" };
    try testing.expectError(error.RpcModeConflictsWithPrint, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --mode bogus still rejected" {
    const argv = [_][]const u8{ "zigts", "expert", "--mode", "xml" };
    try testing.expectError(error.UnsupportedMode, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: unknown --frobnicate is ignored, defaults preserved" {
    const argv = [_][]const u8{ "zigts", "expert", "--frobnicate" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expect(flags.policy == null);
    try testing.expectEqual(false, flags.no_session);
    try testing.expectEqual(false, flags.no_persist_tool_output);
}

test "parseExpertFlags: repeated --no-session is idempotent" {
    const argv = [_][]const u8{ "zigts", "expert", "--no-session", "--no-session" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(true, flags.no_session);
    try testing.expect(flags.policy == null);
}

test "parseExpertFlags: --session-id <id> two-argv form captures value" {
    const argv = [_][]const u8{ "zigts", "expert", "--session-id", "abc123" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expect(flags.session_id != null);
    try testing.expectEqualStrings("abc123", flags.session_id.?);
    try testing.expectEqual(false, flags.resume_latest);
}

test "parseExpertFlags: --session-id=<id> one-argv form captures value" {
    const argv = [_][]const u8{ "zigts", "expert", "--session-id=xyz" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expect(flags.session_id != null);
    try testing.expectEqualStrings("xyz", flags.session_id.?);
}

test "parseExpertFlags: --session-id without a value errors" {
    const argv = [_][]const u8{ "zigts", "expert", "--session-id" };
    try testing.expectError(error.MissingSessionId, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --resume alone sets the flag" {
    const argv = [_][]const u8{ "zigts", "expert", "--resume" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(true, flags.resume_latest);
    try testing.expect(flags.session_id == null);
}

test "parseExpertFlags: --resume + --session-id is an error" {
    const argv = [_][]const u8{ "zigts", "expert", "--resume", "--session-id", "x" };
    try testing.expectError(error.MutuallyExclusiveResumeFlags, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --print \"hello\" captures prompt" {
    const argv = [_][]const u8{ "zigts", "expert", "--print", "hello" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expect(flags.print != null);
    try testing.expectEqualStrings("hello", flags.print.?);
    try testing.expectEqual(false, flags.json_mode);
}

test "parseExpertFlags: --print=hello inline form captures prompt" {
    const argv = [_][]const u8{ "zigts", "expert", "--print=hello" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expect(flags.print != null);
    try testing.expectEqualStrings("hello", flags.print.?);
}

test "parseExpertFlags: --print without a value errors MissingPrintPrompt" {
    const argv = [_][]const u8{ "zigts", "expert", "--print" };
    try testing.expectError(error.MissingPrintPrompt, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --mode json without --print errors JsonModeRequiresPrint" {
    const argv = [_][]const u8{ "zigts", "expert", "--mode", "json" };
    try testing.expectError(error.JsonModeRequiresPrint, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --mode json with --print sets json_mode" {
    const argv = [_][]const u8{ "zigts", "expert", "--print", "x", "--mode", "json" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(true, flags.json_mode);
    try testing.expect(flags.print != null);
}

test "parseExpertFlags: --mode=json inline form sets json_mode" {
    const argv = [_][]const u8{ "zigts", "expert", "--print", "x", "--mode=json" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(true, flags.json_mode);
}

test "parseExpertFlags: --mode bogus errors UnsupportedMode" {
    const argv = [_][]const u8{ "zigts", "expert", "--print", "x", "--mode", "bogus" };
    try testing.expectError(error.UnsupportedMode, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --print + --yes combines policy with print" {
    const argv = [_][]const u8{ "zigts", "expert", "--print", "hello", "--yes" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.auto_approve, flags.policy.?);
    try testing.expect(flags.print != null);
    try testing.expectEqualStrings("hello", flags.print.?);
}

test "parseExpertFlags: --continue sets resume_latest like --resume" {
    const argv = [_][]const u8{ "zigts", "expert", "--continue" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(true, flags.resume_latest);
    try testing.expect(flags.fork_session_id == null);
}

test "parseExpertFlags: --fork two-argv form captures id" {
    const argv = [_][]const u8{ "zigts", "expert", "--fork", "abc123" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expect(flags.fork_session_id != null);
    try testing.expectEqualStrings("abc123", flags.fork_session_id.?);
    try testing.expectEqual(false, flags.resume_latest);
}

test "parseExpertFlags: --fork=id inline form captures id" {
    const argv = [_][]const u8{ "zigts", "expert", "--fork=xyz" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqualStrings("xyz", flags.fork_session_id.?);
}

test "parseExpertFlags: --fork without value errors MissingForkSessionId" {
    const argv = [_][]const u8{ "zigts", "expert", "--fork" };
    try testing.expectError(error.MissingForkSessionId, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --fork + --resume errors MutuallyExclusiveForkFlags" {
    const argv = [_][]const u8{ "zigts", "expert", "--fork", "x", "--resume" };
    try testing.expectError(error.MutuallyExclusiveForkFlags, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --fork + --session-id errors MutuallyExclusiveForkFlags" {
    const argv = [_][]const u8{ "zigts", "expert", "--fork", "x", "--session-id", "y" };
    try testing.expectError(error.MutuallyExclusiveForkFlags, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --tools minimal sets preset" {
    const argv = [_][]const u8{ "zigts", "expert", "--tools", "minimal" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(ToolsPreset.minimal, flags.tools_preset);
}

test "parseExpertFlags: --tools=full sets preset" {
    const argv = [_][]const u8{ "zigts", "expert", "--tools=full" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(ToolsPreset.full, flags.tools_preset);
}

test "parseExpertFlags: --tools without value errors MissingToolsPreset" {
    const argv = [_][]const u8{ "zigts", "expert", "--tools" };
    try testing.expectError(error.MissingToolsPreset, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --tools bad-value errors UnsupportedToolsPreset" {
    const argv = [_][]const u8{ "zigts", "expert", "--tools", "quantum" };
    try testing.expectError(error.UnsupportedToolsPreset, parseExpertFlags(argv[0..]));
}

test "buildRegistry + dispatchLine end-to-end against every tool" {
    var reg = try buildRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var meta_outcome = try repl.dispatchLine(testing.allocator, &reg, "zigts_expert_meta");
    try expectOkContains(&meta_outcome, testing.allocator, "\"compiler_version\"");

    var rule_outcome = try repl.dispatchLine(testing.allocator, &reg, "zigts_expert_describe_rule ZTS303");
    try expectOkContains(&rule_outcome, testing.allocator, "\"ZTS303\"");

    var search_outcome = try repl.dispatchLine(testing.allocator, &reg, "zigts_expert_search result");
    try expectOkContains(&search_outcome, testing.allocator, "\"code\":");
}

test "parseExpertFlags: --goal sets goals csv" {
    const argv = [_][]const u8{ "zigts", "expert", "--handler", "handler.ts", "--goal", "no_secret_leakage,injection_safe" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqualStrings("no_secret_leakage,injection_safe", flags.goals.?);
    try testing.expectEqualStrings("handler.ts", flags.handler.?);
}

test "parseExpertFlags: --goal= and --handler= attached forms" {
    const argv = [_][]const u8{ "zigts", "expert", "--handler=h.ts", "--goal=no_secret_leakage" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqualStrings("no_secret_leakage", flags.goals.?);
    try testing.expectEqualStrings("h.ts", flags.handler.?);
}

test "parseExpertFlags: --max-iters parses a positive integer" {
    const argv = [_][]const u8{ "zigts", "expert", "--handler", "h.ts", "--goal", "no_secret_leakage", "--max-iters", "12" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(@as(u32, 12), flags.max_iters.?);
}

test "parseExpertFlags: --max-iters rejects zero" {
    const argv = [_][]const u8{ "zigts", "expert", "--max-iters", "0" };
    try testing.expectError(error.InvalidMaxIters, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --max-iters rejects non-numeric" {
    const argv = [_][]const u8{ "zigts", "expert", "--max-iters", "abc" };
    try testing.expectError(error.InvalidMaxIters, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --goal without --handler is rejected" {
    const argv = [_][]const u8{ "zigts", "expert", "--goal", "no_secret_leakage" };
    try testing.expectError(error.GoalRequiresHandler, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --goal conflicts with --print" {
    const argv = [_][]const u8{ "zigts", "expert", "--handler", "h.ts", "--goal", "no_secret_leakage", "--print", "hi" };
    try testing.expectError(error.GoalConflictsWithPrintOrRpc, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: --goal conflicts with --mode rpc" {
    const argv = [_][]const u8{ "zigts", "expert", "--handler", "h.ts", "--goal", "no_secret_leakage", "--mode", "rpc" };
    try testing.expectError(error.GoalConflictsWithPrintOrRpc, parseExpertFlags(argv[0..]));
}

test "splitCsv trims whitespace and drops empty entries" {
    const parts = try splitCsv(testing.allocator, " a , b,, c ");
    defer {
        for (parts) |p| testing.allocator.free(p);
        testing.allocator.free(parts);
    }
    try testing.expectEqual(@as(usize, 3), parts.len);
    try testing.expectEqualStrings("a", parts[0]);
    try testing.expectEqualStrings("b", parts[1]);
    try testing.expectEqualStrings("c", parts[2]);
}
