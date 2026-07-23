//! zttp-modules — built-in virtual modules, depending only on
//! zttp-sdk. Zigts imports this package to register bindings.

pub const sdk = @import("zttp-sdk");

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

pub const data = struct {
    pub const ratelimit = @import("data/ratelimit.zig");
    pub const cache = @import("data/cache.zig");
    pub const sql = @import("data/sql.zig");
};

pub const net = struct {
    pub const fetch = @import("net/fetch.zig");
    pub const service = @import("net/service.zig");
    pub const websocket = @import("net/websocket.zig");
};

pub const workflow = struct {
    pub const compose = @import("workflow/compose.zig");
};

pub const catalog = struct {
    pub const crypto = security.crypto.binding;
    pub const auth = security.auth.binding;
    pub const validate = security.validate.binding;
    pub const decode = security.decode.binding;

    pub const env = platform.env.binding;
    pub const id = platform.id.binding;
    pub const text = platform.text.binding;
    pub const time = platform.time.binding;
    pub const log = platform.log.binding;

    pub const router = http.router.binding;
    pub const url = http.url.binding;
    pub const http_mod = http.http_mod.binding;

    pub const ratelimit = data.ratelimit.binding;
    pub const cache = data.cache.binding;
    pub const sql = data.sql.binding;

    pub const fetch = net.fetch.binding;
    pub const service = net.service.binding;
    pub const websocket = net.websocket.binding;

    pub const compose = workflow.compose.binding;
};

pub const all_bindings = [_]sdk.ModuleBinding{
    catalog.crypto,
    catalog.auth,
    catalog.validate,
    catalog.decode,
    catalog.env,
    catalog.id,
    catalog.text,
    catalog.time,
    catalog.log,
    catalog.router,
    catalog.url,
    catalog.http_mod,
    catalog.ratelimit,
    catalog.cache,
    catalog.sql,
    catalog.fetch,
    catalog.service,
    catalog.websocket,
    catalog.compose,
};

comptime {
    sdk.validateBindings(&all_bindings);
}

test {
    _ = internal.util;
    _ = catalog;
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
    _ = data.ratelimit;
    _ = data.cache;
    _ = data.sql;
    _ = net.fetch;
    _ = net.service;
    _ = net.websocket;
    _ = workflow.compose;
}
