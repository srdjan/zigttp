//! CLI argument helpers extracted from dev_cli.zig.
//!
//! Pure functions over argv slices and small CLI-vocabulary constants
//! that the command dispatchers consult before doing any I/O. Sibling
//! to dev_cli.zig; dev_cli imports these and uses them throughout
//! main() and the per-command preflight paths.

const std = @import("std");
const pi_app = @import("pi_app");

/// `zigttp deploy` flags that opt into hosted cloud deploy. They are
/// intercepted by `deployArgsRequestCloud` before `localDeployCommand`
/// runs and rejected while cloud deploy is deferred from the beta.
pub const cloud_only_deploy_flags = [_][]const u8{ "--region", "--confirm", "--wait", "--no-wait" };

/// Comma-separated list of template names accepted by `zigttp init
/// --template`. Used in both the help text and the preflight error
/// message so the two stay in sync.
pub const template_choices = "basic, api, htmx";

pub fn hasHelpFlag(argv: []const []const u8) bool {
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "help")) return true;
    }
    return false;
}

pub fn hasLongHelpFlag(argv: []const []const u8) bool {
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "help")) return true;
    }
    return false;
}

pub fn containsString(haystack: []const []const u8, needle: []const u8) bool {
    for (haystack) |entry| {
        if (std.mem.eql(u8, entry, needle)) return true;
    }
    return false;
}

/// Returns the first flag that selects the hosted control-plane deploy, or
/// null when bare `zigttp deploy` should fall through to the local path.
/// Cloud-only flags (`--region`, `--confirm`, `--wait`, `--no-wait`) imply
/// `--cloud` so existing scripts keep working through v1.x. Callers use the
/// returned literal to label the rejection so the user sees the flag they
/// actually typed, not a hardcoded `--cloud`.
pub fn deployArgsRequestCloud(argv: []const []const u8) ?[]const u8 {
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--cloud")) return "--cloud";
        // `--target <value>` (and `--target=<value>`) with any non-local target
        // means cloud, which is deferred this beta: route it to the same clean
        // rejection as --cloud rather than a confusing "unknown argument".
        if (std.mem.eql(u8, arg, "--target")) {
            if (i + 1 < argv.len and !std.mem.eql(u8, argv[i + 1], "local")) return "--target";
        }
        if (std.mem.startsWith(u8, arg, "--target=")) {
            if (!std.mem.eql(u8, arg["--target=".len..], "local")) return "--target";
        }
        if (containsString(&cloud_only_deploy_flags, arg)) return arg;
    }
    return null;
}

/// Commands that accept an explicit handler path positional (they call
/// `findPositionalPath`). Only these get the "Or pass an explicit handler path"
/// remediation; `deploy`/`test`/`build` etc. take no handler argument, so that
/// sentence would be misleading for them.
fn commandAcceptsHandlerPath(command: []const u8) bool {
    return std.mem.eql(u8, command, "dev") or std.mem.eql(u8, command, "studio");
}

pub fn printNoProjectConfigDiagnostic(command: []const u8) void {
    if (commandAcceptsHandlerPath(command)) {
        std.debug.print(
            \\No zigttp.json found in the current directory or any parent.
            \\
            \\Run `zigttp init <name>` to scaffold a new project, then `cd <name>`
            \\and re-run `zigttp {s}`. Or pass an explicit handler path as an argument.
            \\
        , .{command});
    } else {
        std.debug.print(
            \\No zigttp.json found in the current directory or any parent.
            \\
            \\Run `zigttp init <name>` to scaffold a new project, then `cd <name>`
            \\and re-run `zigttp {s}`.
            \\
        , .{command});
    }
}

/// Convert preflight errors from `dev`/`studio` (which run `zigts check`
/// before launching the runtime child) into clean exit-1 messages instead
/// of panic-style stack-trace dumps. The readable line was already printed
/// upstream by `zigts check` itself.
///
/// Returns `true` if the caller should `std.process.exit(1)`.
pub fn handlePreflightError(err: anyerror, command: []const u8) bool {
    if (err == error.NoProjectConfig) {
        printNoProjectConfigDiagnostic(command);
        return true;
    }
    if (err == error.MissingTemplate) {
        std.debug.print("--template requires one of: " ++ template_choices ++ ".\n", .{});
        return true;
    }
    if (err == error.InvalidTemplate) {
        std.debug.print("Unknown template. Choose one of: " ++ template_choices ++ ".\n", .{});
        return true;
    }
    if (err == error.FileNotFound) {
        // zigts check has already printed `Error reading handler file 'X': error.FileNotFound`.
        // Add a remediation hint and swallow the stack trace.
        std.debug.print(
            \\
            \\Check the `entry` path in zigttp.json or pass --help for usage.
            \\
        , .{});
        return true;
    }
    if (err == error.CheckFailed) {
        return true;
    }
    return false;
}

pub const ExpertArgValidation = union(enum) {
    ok,
    unknown_flag: []const u8,
    unexpected_arg: []const u8,
};

/// Validate `zigttp expert` argv against the documented flag set before the
/// agent loop starts, so a typo'd flag or stray subcommand fails fast with a
/// clear message instead of being silently ignored. Value-taking flags are
/// resolved against `pi_app.value_taking_flags` so this list stays in sync
/// with the expert CLI's own parser.
pub fn validateExpertArgs(argv: []const []const u8) ExpertArgValidation {
    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--help") or
            std.mem.eql(u8, arg, "-h") or
            std.mem.eql(u8, arg, "help"))
        {
            continue;
        }
        if (isExpertBareFlag(arg)) continue;
        if (std.mem.startsWith(u8, arg, "--")) {
            if (isExpertValueTakingFlag(arg)) {
                if (i + 1 < argv.len) i += 1;
                continue;
            }
            if (isExpertValueTakingFlagEq(arg)) continue;
            return .{ .unknown_flag = arg };
        }
        return .{ .unexpected_arg = arg };
    }
    return .ok;
}

fn isExpertBareFlag(arg: []const u8) bool {
    return std.mem.eql(u8, arg, "--yes") or
        std.mem.eql(u8, arg, "--no-edit") or
        std.mem.eql(u8, arg, "--no-session") or
        std.mem.eql(u8, arg, "--no-persist-tool-output") or
        std.mem.eql(u8, arg, "--no-context-files") or
        std.mem.eql(u8, arg, "--perf-receipt") or
        std.mem.eql(u8, arg, "--no-perf-receipt") or
        std.mem.eql(u8, arg, "--equivalence-receipt") or
        std.mem.eql(u8, arg, "--no-equivalence-receipt") or
        std.mem.eql(u8, arg, "--resume") or
        std.mem.eql(u8, arg, "--continue");
}

fn isExpertValueTakingFlag(arg: []const u8) bool {
    for (pi_app.value_taking_flags) |name| {
        if (std.mem.eql(u8, arg, name)) return true;
    }
    return false;
}

fn isExpertValueTakingFlagEq(arg: []const u8) bool {
    for (pi_app.value_taking_flags) |name| {
        if (arg.len > name.len and
            std.mem.startsWith(u8, arg, name) and
            arg[name.len] == '=')
        {
            return true;
        }
    }
    return false;
}

test "commandAcceptsHandlerPath only includes the positional-path commands" {
    try std.testing.expect(commandAcceptsHandlerPath("dev"));
    try std.testing.expect(commandAcceptsHandlerPath("studio"));
    try std.testing.expect(!commandAcceptsHandlerPath("deploy"));
    try std.testing.expect(!commandAcceptsHandlerPath("test"));
    try std.testing.expect(!commandAcceptsHandlerPath("build"));
    try std.testing.expect(!commandAcceptsHandlerPath("doctor"));
    try std.testing.expect(!commandAcceptsHandlerPath("compile"));
}

test "validateExpertArgs accepts documented expert launch forms" {
    const ok = struct {
        fn expect(argv: []const []const u8) !void {
            switch (validateExpertArgs(argv)) {
                .ok => {},
                else => return error.ExpectedValidExpertArgs,
            }
        }
    }.expect;

    try ok(&.{});
    try ok(&.{"--resume"});
    try ok(&.{"--continue"});
    try ok(&.{ "--session-id", "abc" });
    try ok(&.{"--session-id=abc"});
    try ok(&.{ "--fork", "abc" });
    try ok(&.{"--fork=abc"});
    try ok(&.{ "--print", "add a GET /health route", "--mode", "json" });
    try ok(&.{"--mode=rpc"});
    try ok(&.{ "--handler", "handler.ts", "--goal", "no_secret_leakage", "--max-iters", "4" });
    try ok(&.{ "--tools", "minimal", "--yes", "--no-context-files" });
    try ok(&.{ "--no-session", "--no-persist-tool-output", "--no-edit" });
    try ok(&.{"--no-perf-receipt"});
    try ok(&.{"--perf-receipt"});
    try ok(&.{"--no-equivalence-receipt"});
    try ok(&.{"--equivalence-receipt"});
}

test "validateExpertArgs rejects unknown expert flags and subcommands" {
    switch (validateExpertArgs(&.{"--bogus"})) {
        .unknown_flag => |flag| try std.testing.expectEqualStrings("--bogus", flag),
        else => return error.ExpectedUnknownExpertFlag,
    }
    switch (validateExpertArgs(&.{"diagnose"})) {
        .unexpected_arg => |arg| try std.testing.expectEqualStrings("diagnose", arg),
        else => return error.ExpectedUnexpectedExpertArg,
    }
}
