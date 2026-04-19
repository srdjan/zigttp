//! zigttp-modules — built-in virtual modules, depending only on
//! zigttp-sdk. Zigts imports this package to register bindings.

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
