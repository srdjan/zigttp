const std = @import("std");
const cli = @import("dev_cli.zig");

pub fn main(init: std.process.Init.Minimal) !void {
    return cli.main(init);
}

test {
    _ = @import("dev_cli.zig");

    // Force-link the deploy submodules whose tests should run as part of
    // the developer-CLI test target. Mirrors the set enumerated in
    // packages/deploy/src/test_root.zig — keep the two lists in sync.
    // Lazy analysis through the package boundary won't reach decls that
    // are only declared but never referenced; the explicit refs below
    // ensure every relevant deploy file is compiled and its `test`
    // blocks collected.
    const deploy = @import("zigttp_deploy");
    _ = deploy.printer;
    _ = deploy.review;
    _ = deploy.northflank_adapter;
    _ = deploy.http;
    _ = deploy.state;
    _ = deploy.oci.layer;
    _ = deploy.oci.image;
    _ = deploy.oci.registry;
    _ = deploy.oci.tar;

    _ = @import("proof_ledger.zig");
    _ = @import("proofs_cli.zig");
    _ = @import("proof_card_tui.zig");
    _ = @import("proof_audit_ring.zig");
    _ = @import("demo.zig");
    _ = @import("ratchet_command.zig");
}
