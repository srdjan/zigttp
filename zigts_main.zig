const std = @import("std");
const zigts_cli = @import("zigts_cli");

// `zigts` is the pi-free compiler/analyzer CLI installed for IDE and CI
// integrations that call the analyzer directly. Every command here is also
// reachable as `zigttp <command>` with identical output. The interactive
// `expert` agent and session `ledger` commands live only in the developer
// `zigttp` binary, so the agent's network/credential surface is never linked
// into this binary.
pub fn main(init: std.process.Init.Minimal) !void {
    const allocator = std.heap.smp_allocator;
    const argv = try zigts_cli.collectArgs(allocator, init.args);
    defer {
        for (argv) |arg| allocator.free(arg);
        allocator.free(argv);
    }

    const user_args = argv[1..];
    if (user_args.len > 0) {
        const command = user_args[0];
        if (std.mem.eql(u8, command, "expert") or std.mem.eql(u8, command, "ledger")) {
            std.debug.print(
                "`zigts {s}` moved to the developer CLI. Run `zigttp {s}` instead.\n",
                .{ command, command },
            );
            std.process.exit(1);
        }
    }

    // Map usage errors (bad/unknown/missing flags or args) to a clean one-line
    // message + exit 1 instead of a raw Zig stack trace, mirroring the `zigttp`
    // developer binary's analyzer dispatch so the two surfaces stay identical
    // for IDE/CI integrations that call `zigts` directly.
    zigts_cli.run(allocator, user_args) catch |err| switch (err) {
        error.InvalidArgument,
        error.InvalidArguments,
        error.MissingArgument,
        error.UnknownArgument,
        error.UnknownOption,
        error.UnknownFlag,
        error.TooManyArguments,
        => {
            const cmd = if (user_args.len > 0) user_args[0] else "";
            std.debug.print(
                "zigts {s}: invalid arguments ({s}). Run `zigts {s} --help` for usage.\n",
                .{ cmd, @errorName(err), cmd },
            );
            std.process.exit(1);
        },
        else => return err,
    };
}
