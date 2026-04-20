const std = @import("std");
const zigts_cli = @import("zigts_cli");
const pi_app = @import("pi_app");

pub fn main(init: std.process.Init.Minimal) !void {
    const allocator = std.heap.smp_allocator;
    const argv = try zigts_cli.collectArgs(allocator, init.args);
    defer {
        for (argv) |arg| allocator.free(arg);
        allocator.free(argv);
    }

    const user_args = argv[1..];
    if (user_args.len > 0 and std.mem.eql(u8, user_args[0], "expert")) {
        try runExpertCommand(user_args[1..], allocator);
        return;
    }

    try zigts_cli.run(allocator, user_args);
}

fn runExpertCommand(argv: []const []const u8, allocator: std.mem.Allocator) !void {
    // Unknown long flags are reserved for future slices (--print, --mode, ...)
    // and flow through to pi_app unchanged. Bare positional tokens remain
    // unsupported: `zigts expert` does not take subcommands.
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "help")) {
            printExpertHelp();
            return;
        }
        if (std.mem.startsWith(u8, arg, "--")) continue;
        std.debug.print("zigts expert does not accept subcommands; use direct commands like `zigts meta` or `zigts verify-paths`.\n", .{});
        return error.InvalidArgument;
    }

    pi_app.setInvocationArgv(argv);
    try pi_app.run(allocator);
}

fn printExpertHelp() void {
    const help =
        \\zigts expert - interactive coding agent for zigttp
        \\
        \\Usage:
        \\  zigts expert [--yes | --no-edit] [--no-session] [--no-persist-tool-output]
        \\               [--session-id <id> | --resume]
        \\
        \\Flags:
        \\  --yes                      auto-approve every verified edit (non-interactive)
        \\  --no-edit                  auto-reject every verified edit (veto-only)
        \\  --no-session               disable session persistence for this run
        \\  --no-persist-tool-output   omit tool output bodies from persisted session
        \\  --session-id <id>          resume or create a session with this id
        \\  --resume                   resume the newest session for this cwd
        \\
        \\Launches the interactive compiler-in-the-loop expert session.
        \\For machine-facing tooling, use direct commands such as:
        \\  zigts meta
        \\  zigts verify-paths <file>...
        \\  zigts verify-modules --builtins --strict --json
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}
