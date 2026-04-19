//! zigttp-modules — built-in virtual modules, depending only on
//! zigttp-sdk. Zigts imports this package to register bindings.

pub const sdk = @import("zigttp-sdk");

pub const internal = struct {
    pub const util = @import("internal/util.zig");
};

pub const security = struct {
    pub const crypto = @import("security/crypto.zig");
    pub const auth = @import("security/auth.zig");
    pub const validate = @import("security/validate.zig");
    pub const decode = @import("security/decode.zig");
};

comptime {
    sdk.validateBindings(&.{
        security.crypto.binding,
        security.auth.binding,
        security.validate.binding,
        security.decode.binding,
    });
}

test {
    _ = internal.util;
    _ = security.crypto;
    _ = security.auth;
    _ = security.validate;
    _ = security.decode;
}
