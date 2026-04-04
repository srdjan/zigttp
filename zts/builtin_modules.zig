//! Built-in Module Registry
//!
//! Lists all built-in virtual module bindings. This is the single source of
//! truth for which modules exist - all consumers (resolver, type checker,
//! verifier, contract builder) read from this list.
//!
//! Third-party modules are appended to this list via build.zig options.

const mb = @import("module_binding.zig");
const ModuleBinding = mb.ModuleBinding;
const extension_bindings = @import("generated/extension_bindings.zig");

const env_mod = @import("modules/env.zig");
const crypto_mod = @import("modules/crypto.zig");
const router_mod = @import("modules/router.zig");
const auth_mod = @import("modules/auth.zig");
const validate_mod = @import("modules/validate.zig");
const decode_mod = @import("modules/decode.zig");
const cache_mod = @import("modules/cache.zig");
const sql_mod = @import("modules/sql.zig");
const io_mod = @import("modules/io.zig");
const compose_mod = @import("modules/compose.zig");
const durable_mod = @import("modules/durable.zig");

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
    compose_mod.binding,
    durable_mod.binding,
};

/// Unified module registry: core built-ins plus explicitly registered extensions.
pub const all = builtins ++ extension_bindings.all;

/// Number of built-in modules.
pub const count = all.len;

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
    try std.testing.expect(fromSpecifier("zigttp-ext:math") != null);
    try std.testing.expect(fromSpecifier("zigttp:unknown") == null);
}

test "findFunction finds known functions" {
    const result = findFunction("sha256");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("zigttp:crypto", result.?.binding.specifier);
    try std.testing.expectEqual(mb.ReturnKind.string, result.?.func.returns);
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
}
