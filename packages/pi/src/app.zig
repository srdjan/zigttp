//! Top-level entrypoint for `zigts expert`.

const std = @import("std");
const registry_mod = @import("registry/registry.zig");
const repl = @import("repl.zig");
const tui_app = @import("tui/app.zig");
const loop = @import("loop.zig");
const print_mode = @import("print_mode.zig");

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

/// Long flags whose next token is a value. `zigts_main.zig` consults this
/// list so it can skip the value while scanning for stray positional args.
pub const value_taking_flags = [_][]const u8{ "--session-id", "--print", "--mode" };

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
        error.MissingSessionId => {
            const msg = "error: --session-id requires a value\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            std.process.exit(2);
        },
        error.MutuallyExclusiveResumeFlags => {
            const msg = "error: --resume and --session-id are mutually exclusive\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            std.process.exit(2);
        },
        error.MissingPrintPrompt => {
            const msg = "error: --print requires a value\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            std.process.exit(2);
        },
        error.MissingModeValue => {
            const msg = "error: --mode requires a value (json)\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            std.process.exit(2);
        },
        error.UnsupportedMode => {
            const msg = "error: --mode only accepts 'json'\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            std.process.exit(2);
        },
        error.JsonModeRequiresPrint => {
            const msg = "error: --mode json requires --print <prompt>\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            std.process.exit(2);
        },
    };

    var registry = try buildRegistry(allocator);
    defer registry.deinit(allocator);

    if (flags.print != null) {
        try print_mode.run(allocator, &registry, flags, flags.policy orelse .auto_reject);
        return;
    }

    const interactive_policy = flags.policy orelse .ask;
    const is_tty = std.c.isatty(std.c.STDIN_FILENO) != 0;
    if (is_tty) {
        try tui_app.run(allocator, &registry, interactive_policy, flags.no_session, flags.no_persist_tool_output, flags.session_id, flags.resume_latest);
    } else {
        try repl.run(allocator, &registry, interactive_policy, flags.no_session, flags.no_persist_tool_output, flags.session_id, flags.resume_latest);
    }
}

/// Flags parsed from `zigts expert` argv. `policy == null` means the user
/// did not pass `--yes` or `--no-edit`; callers pick an appropriate default
/// (`.ask` for interactive, `.auto_reject` for `--print`).
pub const ExpertFlags = struct {
    policy: ?loop.ApprovalPolicy = null,
    no_session: bool = false,
    no_persist_tool_output: bool = false,
    session_id: ?[]const u8 = null,
    resume_latest: bool = false,
    print: ?[]const u8 = null,
    json_mode: bool = false,
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
        if (std.mem.eql(u8, arg, "--resume")) out.resume_latest = true;
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
            const val = argv[i];
            if (!std.mem.eql(u8, val, "json")) return error.UnsupportedMode;
            out.json_mode = true;
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--mode=")) {
            const val = arg["--mode=".len..];
            if (!std.mem.eql(u8, val, "json")) return error.UnsupportedMode;
            out.json_mode = true;
        }
    }
    if (saw_yes and saw_no_edit) return error.MutuallyExclusiveApprovalFlags;
    if (out.resume_latest and out.session_id != null) return error.MutuallyExclusiveResumeFlags;
    if (out.json_mode and out.print == null) return error.JsonModeRequiresPrint;
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
