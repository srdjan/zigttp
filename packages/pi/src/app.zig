//! Top-level entrypoint for `zigts expert`.
//!
//! This file lives at src/ level (not inside pi/) so its module path anchors
//! at packages/tools/src/ — the same anchoring used by pi_tests.zig. That
//! lets the pi subtree's sibling imports (`../../expert_meta.zig` etc.) stay
//! inside the module sandbox Zig 0.16 enforces.

const std = @import("std");
const registry_mod = @import("registry/registry.zig");
const repl = @import("repl.zig");
const tui_app = @import("tui/app.zig");
const loop = @import("loop.zig");

const meta_tool = @import("tools/zigts_expert_meta.zig");
const verify_paths_tool = @import("tools/zigts_expert_verify_paths.zig");
const describe_rule_tool = @import("tools/zigts_expert_describe_rule.zig");
const search_tool = @import("tools/zigts_expert_search.zig");
const edit_simulate_tool = @import("tools/zigts_expert_edit_simulate.zig");
const review_patch_tool = @import("tools/zigts_expert_review_patch.zig");
const features_tool = @import("tools/zigts_expert_features.zig");
const modules_tool = @import("tools/zigts_expert_modules.zig");
const verify_modules_tool = @import("tools/zigts_expert_verify_modules.zig");
const workspace_list_files_tool = @import("tools/workspace_list_files.zig");
const workspace_read_file_tool = @import("tools/workspace_read_file.zig");
const workspace_search_text_tool = @import("tools/workspace_search_text.zig");
const zigts_check_tool = @import("tools/zigts_check.zig");
const zig_build_step_tool = @import("tools/zig_build_step.zig");
const zig_test_step_tool = @import("tools/zig_test_step.zig");

const Registry = registry_mod.Registry;

pub fn buildRegistry(allocator: std.mem.Allocator) !Registry {
    var reg: Registry = .{};
    errdefer reg.deinit(allocator);

    try reg.register(allocator, meta_tool.tool);
    try reg.register(allocator, verify_paths_tool.tool);
    try reg.register(allocator, describe_rule_tool.tool);
    try reg.register(allocator, search_tool.tool);
    try reg.register(allocator, edit_simulate_tool.tool);
    try reg.register(allocator, review_patch_tool.tool);
    try reg.register(allocator, features_tool.tool);
    try reg.register(allocator, modules_tool.tool);
    try reg.register(allocator, verify_modules_tool.tool);
    try reg.register(allocator, workspace_list_files_tool.tool);
    try reg.register(allocator, workspace_read_file_tool.tool);
    try reg.register(allocator, workspace_search_text_tool.tool);
    try reg.register(allocator, zigts_check_tool.tool);
    try reg.register(allocator, zig_build_step_tool.tool);
    try reg.register(allocator, zig_test_step_tool.tool);

    return reg;
}

/// Module-level argv snapshot. Callers that can supply argv (currently
/// `zigts_main.zig` for the `zigts expert` path) set this immediately before
/// invoking `run`. Callers without argv access (the deprecated
/// `zigttp expert` path in `dev_cli.zig`) leave it null; `run` then treats
/// the policy as `.ask`. Kept module-local so the public `run(allocator)`
/// signature remains stable.
var captured_argv: ?[]const []const u8 = null;

pub fn setInvocationArgv(argv: []const []const u8) void {
    captured_argv = argv;
}

pub fn run(allocator: std.mem.Allocator) !void {
    const argv = captured_argv orelse &[_][]const u8{};
    const flags = parseExpertFlags(argv) catch |err| switch (err) {
        error.MutuallyExclusiveApprovalFlags => {
            const msg = "error: --yes and --no-edit are mutually exclusive\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            std.process.exit(2);
        },
    };

    var registry = try buildRegistry(allocator);
    defer registry.deinit(allocator);

    const is_tty = std.c.isatty(std.c.STDIN_FILENO) != 0;
    if (is_tty) {
        try tui_app.run(allocator, &registry, flags.policy, flags.no_session, flags.no_persist_tool_output);
    } else {
        try repl.run(allocator, &registry, flags.policy, flags.no_session, flags.no_persist_tool_output);
    }
}

/// Flags parsed from `zigts expert` argv. `policy` folds `--yes`/`--no-edit`
/// into an `ApprovalPolicy`; `no_session` and `no_persist_tool_output` are
/// plumbing for Batch 4's events.jsonl writer and are merely carried through
/// `run` today.
pub const ExpertFlags = struct {
    policy: loop.ApprovalPolicy = .ask,
    no_session: bool = false,
    no_persist_tool_output: bool = false,
};

/// Scan argv for the expert launch flags. Unknown `--*` tokens are ignored so
/// future slices can add their own without breaking this parser. `--yes` and
/// `--no-edit` together return an error so the caller can report a clear
/// diagnostic. Order-independent; repetition is idempotent.
pub fn parseExpertFlags(argv: []const []const u8) !ExpertFlags {
    var out: ExpertFlags = .{};
    var saw_yes = false;
    var saw_no_edit = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--yes")) saw_yes = true;
        if (std.mem.eql(u8, arg, "--no-edit")) saw_no_edit = true;
        if (std.mem.eql(u8, arg, "--no-session")) out.no_session = true;
        if (std.mem.eql(u8, arg, "--no-persist-tool-output")) out.no_persist_tool_output = true;
    }
    if (saw_yes and saw_no_edit) return error.MutuallyExclusiveApprovalFlags;
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
        "zigts_expert_features",
        "zigts_expert_modules",
        "zigts_expert_verify_modules",
        "workspace_list_files",
        "workspace_read_file",
        "workspace_search_text",
        "zigts_check",
        "zig_build_step",
        "zig_test_step",
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
            try testing.expect(std.mem.indexOf(u8, r.body, needle) != null);
        },
        else => return error.TestFailed,
    }
}

test "parseExpertFlags: empty argv yields defaults" {
    const flags = try parseExpertFlags(&.{});
    try testing.expectEqual(loop.ApprovalPolicy.ask, flags.policy);
    try testing.expectEqual(false, flags.no_session);
    try testing.expectEqual(false, flags.no_persist_tool_output);
}

test "parseExpertFlags: --yes yields auto_approve" {
    const argv = [_][]const u8{ "zigts", "expert", "--yes" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.auto_approve, flags.policy);
    try testing.expectEqual(false, flags.no_session);
    try testing.expectEqual(false, flags.no_persist_tool_output);
}

test "parseExpertFlags: --no-edit yields auto_reject" {
    const argv = [_][]const u8{ "zigts", "expert", "--no-edit" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.auto_reject, flags.policy);
}

test "parseExpertFlags: --no-session alone flips only that field" {
    const argv = [_][]const u8{ "zigts", "expert", "--no-session" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.ask, flags.policy);
    try testing.expectEqual(true, flags.no_session);
    try testing.expectEqual(false, flags.no_persist_tool_output);
}

test "parseExpertFlags: --no-persist-tool-output alone flips only that field" {
    const argv = [_][]const u8{ "zigts", "expert", "--no-persist-tool-output" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.ask, flags.policy);
    try testing.expectEqual(false, flags.no_session);
    try testing.expectEqual(true, flags.no_persist_tool_output);
}

test "parseExpertFlags: --yes --no-session --no-persist-tool-output combine" {
    const argv = [_][]const u8{ "zigts", "expert", "--yes", "--no-session", "--no-persist-tool-output" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.auto_approve, flags.policy);
    try testing.expectEqual(true, flags.no_session);
    try testing.expectEqual(true, flags.no_persist_tool_output);
}

test "parseExpertFlags: --yes + --no-edit still errors regardless of session flags" {
    const argv = [_][]const u8{ "zigts", "expert", "--yes", "--no-session", "--no-edit" };
    try testing.expectError(error.MutuallyExclusiveApprovalFlags, parseExpertFlags(argv[0..]));
}

test "parseExpertFlags: unknown --frobnicate is ignored, defaults preserved" {
    const argv = [_][]const u8{ "zigts", "expert", "--frobnicate" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(loop.ApprovalPolicy.ask, flags.policy);
    try testing.expectEqual(false, flags.no_session);
    try testing.expectEqual(false, flags.no_persist_tool_output);
}

test "parseExpertFlags: repeated --no-session is idempotent" {
    const argv = [_][]const u8{ "zigts", "expert", "--no-session", "--no-session" };
    const flags = try parseExpertFlags(argv[0..]);
    try testing.expectEqual(true, flags.no_session);
    try testing.expectEqual(loop.ApprovalPolicy.ask, flags.policy);
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
