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

pub const platform = struct {
    pub const env = @import("platform/env.zig");
    pub const id = @import("platform/id.zig");
    pub const text = @import("platform/text.zig");
    pub const time = @import("platform/time.zig");
    pub const log = @import("platform/log.zig");
};

pub const http = struct {
    pub const router = @import("http/router.zig");
    pub const url = @import("http/url.zig");
    pub const http_mod = @import("http/http_mod.zig");
};

comptime {
    sdk.validateBindings(&.{
        security.crypto.binding,
        security.auth.binding,
        security.validate.binding,
        security.decode.binding,
        platform.env.binding,
        platform.id.binding,
        platform.text.binding,
        platform.time.binding,
        platform.log.binding,
        http.router.binding,
        http.url.binding,
        http.http_mod.binding,
    });
}

test {
    _ = internal.util;
    _ = security.crypto;
    _ = security.auth;
    _ = security.validate;
    _ = security.decode;
    _ = platform.env;
    _ = platform.id;
    _ = platform.text;
    _ = platform.time;
    _ = platform.log;
    _ = http.router;
    _ = http.url;
    _ = http.http_mod;
}
