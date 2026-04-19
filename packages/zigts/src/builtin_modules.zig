//! Built-in Module Registry
//!
//! Lists all built-in virtual module bindings. This is the single source of
//! truth for which modules exist - all consumers (resolver, type checker,
//! verifier, contract builder) read from this list.
//!
//! Third-party modules are appended to this list via build.zig options.

const mb = @import("module_binding.zig");
const adapter = @import("module_binding_adapter.zig");
const ModuleBinding = mb.ModuleBinding;
const extension_bindings = @import("extension_bindings.zig");
const file_io = @import("file_io.zig");
const std = @import("std");
const modules = @import("zigttp-modules");

// Ported modules are imported directly from the peer package. Bindings
// are adapted (ordinal-safe cast + required_capabilities conversion) at
// comptime, yielding zigts-internal ModuleBinding values grouped under
// a single namespace so their top-level names don't shadow locals in
// the assertion tests below.
const ported = struct {
    const env = adapter.adaptModuleBinding(modules.platform.env.binding);
    const crypto = adapter.adaptModuleBinding(modules.security.crypto.binding);
    const router = adapter.adaptModuleBinding(modules.http.router.binding);
    const auth = adapter.adaptModuleBinding(modules.security.auth.binding);
    const validate = adapter.adaptModuleBinding(modules.security.validate.binding);
    const decode = adapter.adaptModuleBinding(modules.security.decode.binding);
    const cache = adapter.adaptModuleBinding(modules.data.cache.binding);
    const ratelimit = adapter.adaptModuleBinding(modules.data.ratelimit.binding);
    const url = adapter.adaptModuleBinding(modules.http.url.binding);
    const id = adapter.adaptModuleBinding(modules.platform.id.binding);
    const http = adapter.adaptModuleBinding(modules.http.http_mod.binding);
    const log = adapter.adaptModuleBinding(modules.platform.log.binding);
    const text = adapter.adaptModuleBinding(modules.platform.text.binding);
    const time = adapter.adaptModuleBinding(modules.platform.time.binding);
    const compose = adapter.adaptModuleBinding(modules.workflow.compose.binding);
};

// Install-having stubs stay on the zigts side because their installState
// helpers are called from runtime bootstrap outside any module invocation.
const sql_mod = @import("modules/data/sql.zig");
const service_mod = @import("modules/net/service.zig");
const fetch_mod = @import("modules/net/fetch.zig");
const websocket_mod = @import("modules/net/websocket.zig");

// Genuinely-in-tree modules: io keeps a threadlocal read by fetchSync;
// scope manipulates GC roots directly; durable's callback wrapper has a
// function-pointer layout issue that needs further investigation.
const io_mod = @import("modules/workflow/io.zig");
const scope_mod = @import("modules/workflow/scope.zig");
const durable_mod = @import("modules/workflow/durable.zig");

/// All in-tree virtual module bindings, in registration order.
pub const builtins = [_]ModuleBinding{
    ported.env,
    ported.crypto,
    ported.router,
    ported.auth,
    ported.validate,
    ported.decode,
    ported.cache,
    sql_mod.binding,
    io_mod.binding,
    scope_mod.binding,
    ported.compose,
    durable_mod.binding,
    ported.url,
    ported.id,
    ported.http,
    ported.log,
    ported.text,
    ported.time,
    ported.ratelimit,
    service_mod.binding,
    fetch_mod.binding,
    websocket_mod.binding,
};

/// Unified module registry: core built-ins plus explicitly registered extensions.
pub const all = builtins ++ extension_bindings.all;

/// Number of built-in modules.
pub const count = all.len;

pub const BuiltinGovernanceEntry = struct {
    specifier: []const u8,
    module_path: []const u8,
    spec_path: []const u8,
};

pub const builtin_governance_entries = [_]BuiltinGovernanceEntry{
    .{ .specifier = "zigttp:env", .module_path = "packages/modules/src/platform/env.zig", .spec_path = "packages/zigts/module-specs/platform/env.json" },
    .{ .specifier = "zigttp:crypto", .module_path = "packages/modules/src/security/crypto.zig", .spec_path = "packages/zigts/module-specs/security/crypto.json" },
    .{ .specifier = "zigttp:router", .module_path = "packages/modules/src/http/router.zig", .spec_path = "packages/zigts/module-specs/http/router.json" },
    .{ .specifier = "zigttp:auth", .module_path = "packages/modules/src/security/auth.zig", .spec_path = "packages/zigts/module-specs/security/auth.json" },
    .{ .specifier = "zigttp:validate", .module_path = "packages/modules/src/security/validate.zig", .spec_path = "packages/zigts/module-specs/security/validate.json" },
    .{ .specifier = "zigttp:decode", .module_path = "packages/modules/src/security/decode.zig", .spec_path = "packages/zigts/module-specs/security/decode.json" },
    .{ .specifier = "zigttp:cache", .module_path = "packages/modules/src/data/cache.zig", .spec_path = "packages/zigts/module-specs/data/cache.json" },
    .{ .specifier = "zigttp:sql", .module_path = "packages/modules/src/data/sql.zig", .spec_path = "packages/zigts/module-specs/data/sql.json" },
    .{ .specifier = "zigttp:io", .module_path = "packages/zigts/src/modules/workflow/io.zig", .spec_path = "packages/zigts/module-specs/workflow/io.json" },
    .{ .specifier = "zigttp:scope", .module_path = "packages/zigts/src/modules/workflow/scope.zig", .spec_path = "packages/zigts/module-specs/workflow/scope.json" },
    .{ .specifier = "zigttp:compose", .module_path = "packages/modules/src/workflow/compose.zig", .spec_path = "packages/zigts/module-specs/workflow/compose.json" },
    .{ .specifier = "zigttp:durable", .module_path = "packages/zigts/src/modules/workflow/durable.zig", .spec_path = "packages/zigts/module-specs/workflow/durable.json" },
    .{ .specifier = "zigttp:url", .module_path = "packages/modules/src/http/url.zig", .spec_path = "packages/zigts/module-specs/http/url.json" },
    .{ .specifier = "zigttp:id", .module_path = "packages/modules/src/platform/id.zig", .spec_path = "packages/zigts/module-specs/platform/id.json" },
    .{ .specifier = "zigttp:http", .module_path = "packages/modules/src/http/http_mod.zig", .spec_path = "packages/zigts/module-specs/http/http-mod.json" },
    .{ .specifier = "zigttp:log", .module_path = "packages/modules/src/platform/log.zig", .spec_path = "packages/zigts/module-specs/platform/log.json" },
    .{ .specifier = "zigttp:text", .module_path = "packages/modules/src/platform/text.zig", .spec_path = "packages/zigts/module-specs/platform/text.json" },
    .{ .specifier = "zigttp:time", .module_path = "packages/modules/src/platform/time.zig", .spec_path = "packages/zigts/module-specs/platform/time.json" },
    .{ .specifier = "zigttp:ratelimit", .module_path = "packages/modules/src/data/ratelimit.zig", .spec_path = "packages/zigts/module-specs/data/ratelimit.json" },
    .{ .specifier = "zigttp:service", .module_path = "packages/modules/src/net/service.zig", .spec_path = "packages/zigts/module-specs/net/service.json" },
    .{ .specifier = "zigttp:fetch", .module_path = "packages/modules/src/net/fetch.zig", .spec_path = "packages/zigts/module-specs/net/fetch.json" },
    .{ .specifier = "zigttp:websocket", .module_path = "packages/modules/src/net/websocket.zig", .spec_path = "packages/zigts/module-specs/net/websocket.json" },
};

comptime {
    if (builtin_governance_entries.len != builtins.len) {
        @compileError("builtin_governance_entries must stay in sync with builtin_modules.builtins");
    }
    for (builtin_governance_entries, builtins) |entry, binding| {
        if (!std.mem.eql(u8, entry.specifier, binding.specifier)) {
            @compileError("builtin_governance_entries specifier drift: " ++ entry.specifier ++ " vs " ++ binding.specifier);
        }
    }
}

pub fn governanceEntries() []const BuiltinGovernanceEntry {
    return &builtin_governance_entries;
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
    inline for (builtin_governance_entries) |entry| {
        try std.testing.expect(file_io.fileExists(std.testing.allocator, entry.spec_path));
        const contents = try file_io.readFile(std.testing.allocator, entry.spec_path, 64 * 1024);
        defer std.testing.allocator.free(contents);
        try std.testing.expect(contents.len > 0);
    }
}

test "governance entries stay aligned with public built-ins" {
    const entries = governanceEntries();
    try std.testing.expectEqual(builtins.len, entries.len);
    try std.testing.expectEqualStrings("zigttp:env", entries[0].specifier);
    try std.testing.expectEqualStrings("packages/modules/src/platform/env.zig", entries[0].module_path);
    try std.testing.expectEqualStrings("packages/zigts/module-specs/platform/env.json", entries[0].spec_path);
    try std.testing.expectEqualStrings("zigttp:websocket", entries[entries.len - 1].specifier);
}
