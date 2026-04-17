const std = @import("std");
const cli = @import("dev_cli.zig");

pub fn main(init: std.process.Init.Minimal) !void {
    return cli.main(init);
}

test {
    _ = @import("dev_cli.zig");
}
