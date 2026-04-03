const std = @import("std");
const cli = @import("zigttp_cli.zig");

pub fn main(init: std.process.Init.Minimal) !void {
    return cli.main(init);
}

test {
    _ = @import("zigttp_cli.zig");
    _ = @import("project_config");
    _ = @import("zruntime.zig");
    _ = @import("server.zig");
}
