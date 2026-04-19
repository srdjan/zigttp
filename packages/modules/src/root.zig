//! zigttp-modules
//!
//! Peer package hosting the built-in virtual modules that ship with
//! zigttp. Depends only on zigttp-sdk; zigts imports this package back
//! to register the bindings.
//!
//! Module groups match docs/virtual-modules/:
//!   security/ data/ net/ http/ workflow/ platform/
//!
//! Ports land group by group. Until a group is fully ported, zigts
//! continues to source its modules from packages/zigts/src/modules/.

pub const sdk = @import("zigttp-sdk");

pub const internal = struct {
    pub const util = @import("internal/util.zig");
};

pub const security = struct {
    pub const crypto = @import("security/crypto.zig");
};

comptime {
    sdk.validateBindings(&.{security.crypto.binding});
}

test {
    _ = internal.util;
    _ = security.crypto;
}
