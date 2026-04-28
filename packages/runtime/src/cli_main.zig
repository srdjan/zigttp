const std = @import("std");
const cli = @import("dev_cli.zig");

pub fn main(init: std.process.Init.Minimal) !void {
    return cli.main(init);
}

test {
    _ = @import("dev_cli.zig");
    _ = @import("deploy/state.zig");
    _ = @import("deploy/review.zig");
    _ = @import("proof_ledger.zig");
    _ = @import("proofs_cli.zig");
    _ = @import("proof_card_tui.zig");
    _ = @import("proof_audit_ring.zig");
}
