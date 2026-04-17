const std = @import("std");
const cli = @import("runtime_cli.zig");

pub fn main(init: std.process.Init.Minimal) !void {
    return cli.main(init);
}

test {
    _ = @import("runtime_cli.zig");
    _ = @import("cli_shared.zig");
    _ = @import("zruntime.zig");
    _ = @import("server.zig");
    _ = @import("proof_adapter.zig");
}
