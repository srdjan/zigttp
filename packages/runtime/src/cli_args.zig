//! CLI argument helpers extracted from dev_cli.zig.
//!
//! Pure functions over argv slices and small CLI-vocabulary constants
//! that the command dispatchers consult before doing any I/O. Sibling
//! to dev_cli.zig; dev_cli imports these and uses them throughout
//! main() and the per-command preflight paths.

const std = @import("std");

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
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--cloud")) return "--cloud";
        if (containsString(&cloud_only_deploy_flags, arg)) return arg;
    }
    return null;
}

pub fn printNoProjectConfigDiagnostic(command: []const u8) void {
    std.debug.print(
        \\No zigttp.json found in the current directory or any parent.
        \\
        \\Run `zigttp init <name>` to scaffold a new project, then `cd <name>`
        \\and re-run `zigttp {s}`. Or pass an explicit handler path as an argument.
        \\
    , .{command});
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
