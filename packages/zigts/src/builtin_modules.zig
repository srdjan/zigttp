//! Built-in Module Registry
//!
//! Lists all built-in virtual module bindings. This is the single source of
//! truth for which modules exist - all consumers (resolver, type checker,
//! verifier, contract builder) read from this list.
//!
//! Third-party modules are appended to this list via build.zig options.

const mb = @import("module_binding.zig");
const ModuleBinding = mb.ModuleBinding;
const extension_bindings = @import("extension_bindings.zig");
const file_io = @import("file_io.zig");

const env_mod = @import("modules/env.zig");
const crypto_mod = @import("modules/crypto.zig");
const router_mod = @import("modules/router.zig");
const auth_mod = @import("modules/auth.zig");
const validate_mod = @import("modules/validate.zig");
const decode_mod = @import("modules/decode.zig");
const cache_mod = @import("modules/cache.zig");
const sql_mod = @import("modules/sql.zig");
const io_mod = @import("modules/io.zig");
const scope_mod = @import("modules/scope.zig");
const compose_mod = @import("modules/compose.zig");
const durable_mod = @import("modules/durable.zig");
const url_mod = @import("modules/url.zig");
const id_mod = @import("modules/id.zig");
const http_mod = @import("modules/http_mod.zig");
const log_mod = @import("modules/log.zig");
const text_mod = @import("modules/text.zig");
const time_mod = @import("modules/time.zig");
const ratelimit_mod = @import("modules/ratelimit.zig");
const service_mod = @import("modules/service.zig");

/// All in-tree virtual module bindings, in registration order.
pub const builtins = [_]ModuleBinding{
    env_mod.binding,
    crypto_mod.binding,
    router_mod.binding,
    auth_mod.binding,
    validate_mod.binding,
    decode_mod.binding,
    cache_mod.binding,
    sql_mod.binding,
    io_mod.binding,
    scope_mod.binding,
    compose_mod.binding,
    durable_mod.binding,
    url_mod.binding,
    id_mod.binding,
    http_mod.binding,
    log_mod.binding,
    text_mod.binding,
    time_mod.binding,
    ratelimit_mod.binding,
    service_mod.binding,
};

/// Unified module registry: core built-ins plus explicitly registered extensions.
pub const all = builtins ++ extension_bindings.all;

/// Number of built-in modules.
pub const count = all.len;

const builtin_spec_paths = [_][]const u8{
    "../module-specs/env.json",
    "../module-specs/crypto.json",
    "../module-specs/router.json",
    "../module-specs/auth.json",
    "../module-specs/validate.json",
    "../module-specs/decode.json",
    "../module-specs/cache.json",
    "../module-specs/sql.json",
    "../module-specs/io.json",
    "../module-specs/scope.json",
    "../module-specs/compose.json",
    "../module-specs/durable.json",
    "../module-specs/url.json",
    "../module-specs/id.json",
    "../module-specs/http-mod.json",
    "../module-specs/log.json",
    "../module-specs/text.json",
    "../module-specs/time.json",
    "../module-specs/ratelimit.json",
    "../module-specs/service.json",
};

comptime {
    if (builtin_spec_paths.len != builtins.len) {
        @compileError("builtin_spec_paths must stay in sync with builtin_modules.builtins");
    }
}

// Validate all bindings at compile time. Produces compile errors for
// duplicate specifiers, duplicate function names, or state inconsistency.
comptime {
    mb.validateBindings(&all);
}

/// Look up a built-in module by specifier string.
pub fn fromSpecifier(specifier: []const u8) ?*const ModuleBinding {
    for (&all) |*b| {
        if (std.mem.eql(u8, b.specifier, specifier)) return b;
    }
    return null;
}

/// Get the binding for a specific export from a specific module.
pub fn findExport(
    specifier: []const u8,
    func_name: []const u8,
) ?struct { binding: *const ModuleBinding, func: *const mb.FunctionBinding } {
    const binding = fromSpecifier(specifier) orelse return null;
    for (binding.exports) |*f| {
        if (std.mem.eql(u8, f.name, func_name)) {
            return .{ .binding = binding, .func = f };
        }
    }
    return null;
}

/// Get the binding for a function name (searches all modules).
pub fn findFunction(func_name: []const u8) ?struct { binding: *const ModuleBinding, func: *const mb.FunctionBinding } {
    for (&all) |*b| {
        for (b.exports) |*f| {
            if (std.mem.eql(u8, f.name, func_name)) {
                return .{ .binding = b, .func = f };
            }
        }
    }
    return null;
}

const std = @import("std");

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

// Uniqueness of specifiers and function names is enforced at compile time
// by the validateBindings() call above. Only behavioral tests below.

test "fromSpecifier finds known modules" {
    try std.testing.expect(fromSpecifier("zigttp:env") != null);
    try std.testing.expect(fromSpecifier("zigttp:crypto") != null);
    try std.testing.expect(fromSpecifier("zigttp:cache") != null);
    try std.testing.expect(fromSpecifier("zigttp:url") != null);
    try std.testing.expect(fromSpecifier("zigttp:id") != null);
    try std.testing.expect(fromSpecifier("zigttp-ext:math") != null);
    try std.testing.expect(fromSpecifier("zigttp:unknown") == null);
}

test "findFunction finds known functions" {
    const result = findFunction("sha256");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("zigttp:crypto", result.?.binding.specifier);
    try std.testing.expectEqual(mb.ReturnKind.string, result.?.func.returns);
}

test "findExport finds known module export" {
    const result = findExport("zigttp:crypto", "sha256");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("zigttp:crypto", result.?.binding.specifier);
    try std.testing.expectEqual(mb.ReturnKind.string, result.?.func.returns);
}

test "findExport finds extension module export" {
    const result = findExport("zigttp-ext:math", "double");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("zigttp-ext:math", result.?.binding.specifier);
    try std.testing.expectEqual(mb.ReturnKind.number, result.?.func.returns);
}

test "findFunction finds extension functions" {
    const result = findFunction("double");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("zigttp-ext:math", result.?.binding.specifier);
    try std.testing.expectEqual(mb.ReturnKind.number, result.?.func.returns);
}

test "findFunction finds result-producing functions" {
    const result = findFunction("jwtVerify");
    try std.testing.expect(result != null);
    try std.testing.expectEqual(mb.ReturnKind.result, result.?.func.returns);
}

test "findFunction finds optional-producing functions" {
    const env_fn = findFunction("env");
    try std.testing.expect(env_fn != null);
    try std.testing.expectEqual(mb.ReturnKind.optional_string, env_fn.?.func.returns);

    const cache_fn = findFunction("cacheGet");
    try std.testing.expect(cache_fn != null);
    try std.testing.expectEqual(mb.ReturnKind.optional_string, cache_fn.?.func.returns);

    const router_fn = findFunction("routerMatch");
    try std.testing.expect(router_fn != null);
    try std.testing.expectEqual(mb.ReturnKind.optional_object, router_fn.?.func.returns);
}

test "failure_severity annotations on failable functions" {
    // Critical: auth and validation failures
    try std.testing.expectEqual(mb.FailureSeverity.critical, findFunction("jwtVerify").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.critical, findFunction("validateJson").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.critical, findFunction("validateObject").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.critical, findFunction("coerceJson").?.func.failure_severity);

    // Expected: cache miss, missing config, route not matched
    try std.testing.expectEqual(mb.FailureSeverity.expected, findFunction("env").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.expected, findFunction("cacheGet").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.expected, findFunction("routerMatch").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.expected, findFunction("parseBearer").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.expected, findFunction("sqlOne").?.func.failure_severity);

    // None: functions that always succeed
    try std.testing.expectEqual(mb.FailureSeverity.none, findFunction("sha256").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.none, findFunction("cacheSet").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.none, findFunction("urlParse").?.func.failure_severity);
    try std.testing.expectEqual(mb.FailureSeverity.none, findFunction("urlEncode").?.func.failure_severity);
}

fn bindingHasCapability(binding: *const ModuleBinding, capability: mb.ModuleCapability) bool {
    for (binding.required_capabilities) |item| {
        if (item == capability) return true;
    }
    return false;
}

test "module capability annotations cover audited helper-enforced built-ins" {
    const env_binding = fromSpecifier("zigttp:env").?;
    try std.testing.expect(bindingHasCapability(env_binding, .env));
    try std.testing.expect(bindingHasCapability(env_binding, .policy_check));

    const crypto_binding = fromSpecifier("zigttp:crypto").?;
    try std.testing.expect(bindingHasCapability(crypto_binding, .crypto));

    const auth_binding = fromSpecifier("zigttp:auth").?;
    try std.testing.expect(bindingHasCapability(auth_binding, .crypto));
    try std.testing.expect(bindingHasCapability(auth_binding, .clock));

    const id_binding = fromSpecifier("zigttp:id").?;
    try std.testing.expect(bindingHasCapability(id_binding, .random));
    try std.testing.expect(bindingHasCapability(id_binding, .clock));

    const log_binding = fromSpecifier("zigttp:log").?;
    try std.testing.expect(bindingHasCapability(log_binding, .stderr));
    try std.testing.expect(bindingHasCapability(log_binding, .clock));

    const cache_binding = fromSpecifier("zigttp:cache").?;
    try std.testing.expect(bindingHasCapability(cache_binding, .clock));
    try std.testing.expect(bindingHasCapability(cache_binding, .policy_check));

    const sql_binding = fromSpecifier("zigttp:sql").?;
    try std.testing.expect(bindingHasCapability(sql_binding, .sqlite));
    try std.testing.expect(bindingHasCapability(sql_binding, .policy_check));

    const io_binding = fromSpecifier("zigttp:io").?;
    try std.testing.expect(bindingHasCapability(io_binding, .runtime_callback));

    const durable_binding = fromSpecifier("zigttp:durable").?;
    try std.testing.expect(bindingHasCapability(durable_binding, .runtime_callback));

    const ratelimit_binding = fromSpecifier("zigttp:ratelimit").?;
    try std.testing.expect(bindingHasCapability(ratelimit_binding, .clock));

    const service_binding = fromSpecifier("zigttp:service").?;
    try std.testing.expect(bindingHasCapability(service_binding, .filesystem));
    try std.testing.expect(bindingHasCapability(service_binding, .runtime_callback));

    const url_binding = fromSpecifier("zigttp:url").?;
    try std.testing.expectEqual(@as(usize, 0), url_binding.required_capabilities.len);
}

test "every in-tree builtin module has a module spec artifact" {
    inline for (builtin_spec_paths) |path| {
        const full_path = "packages/zigts/" ++ path[3..];
        try std.testing.expect(file_io.fileExists(std.testing.allocator, full_path));
        const contents = try file_io.readFile(std.testing.allocator, full_path, 64 * 1024);
        defer std.testing.allocator.free(contents);
        try std.testing.expect(contents.len > 0);
    }
}
