//! Virtual Module Resolver
//!
//! Resolves module specifiers at compile time:
//! - `zigttp:env`    -> native env var functions
//! - `zigttp:crypto` -> native crypto functions (sha256, hmac, base64)
//! - `zigttp:router` -> native pattern-matching router
//! - `./path`        -> relative file import (future)
//!
//! Resolution happens before codegen. Each virtual module registers
//! its native functions as globals in the JS context, so import bindings
//! resolve through the normal global variable lookup.

const std = @import("std");
const object = @import("../object.zig");
const context = @import("../context.zig");

const env_mod = @import("env.zig");
const crypto_mod = @import("crypto.zig");
const router_mod = @import("router.zig");
const auth_mod = @import("auth.zig");
const validate_mod = @import("validate.zig");
const cache_mod = @import("cache.zig");

/// A single exported function from a virtual module
pub const ModuleExport = struct {
    name: []const u8,
    func: object.NativeFn,
    arg_count: u8,
};

/// Known virtual module identifiers
pub const VirtualModule = enum {
    env,
    crypto,
    router,
    auth,
    validate,
    cache,

    pub fn fromSpecifier(specifier: []const u8) ?VirtualModule {
        if (std.mem.eql(u8, specifier, "zigttp:env")) return .env;
        if (std.mem.eql(u8, specifier, "zigttp:crypto")) return .crypto;
        if (std.mem.eql(u8, specifier, "zigttp:router")) return .router;
        if (std.mem.eql(u8, specifier, "zigttp:auth")) return .auth;
        if (std.mem.eql(u8, specifier, "zigttp:validate")) return .validate;
        if (std.mem.eql(u8, specifier, "zigttp:cache")) return .cache;
        return null;
    }

    pub fn getExports(self: VirtualModule) []const ModuleExport {
        return switch (self) {
            .env => &env_mod.exports,
            .crypto => &crypto_mod.exports,
            .router => &router_mod.exports,
            .auth => &auth_mod.exports,
            .validate => &validate_mod.exports,
            .cache => &cache_mod.exports,
        };
    }
};

/// Resolution result
pub const ResolveResult = union(enum) {
    /// Virtual module with native implementations
    virtual: VirtualModule,
    /// Relative file import (specifier is the path)
    file: []const u8,
    /// Unknown module - compile error
    unknown: void,
};

/// Resolve a module specifier
pub fn resolve(specifier: []const u8) ResolveResult {
    // Check virtual modules first
    if (VirtualModule.fromSpecifier(specifier)) |vm| {
        return .{ .virtual = vm };
    }

    // Check for relative file imports
    if (specifier.len > 0 and (specifier[0] == '.' or specifier[0] == '/')) {
        return .{ .file = specifier };
    }

    return .unknown;
}

/// Register all exports from a virtual module into a JS context.
/// Called once per import at compile time (before codegen runs).
/// Each export becomes a global native function accessible by name.
pub fn registerVirtualModule(ctx: *context.Context, module: VirtualModule, allocator: std.mem.Allocator) !void {
    const module_exports = module.getExports();
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    for (module_exports) |exp| {
        // Intern the function name as an atom
        const name_atom = try ctx.atoms.intern(exp.name);

        // Create native function object and register as global
        const fn_obj = try object.JSObject.createNativeFunction(
            allocator,
            pool,
            ctx.root_class_idx,
            exp.func,
            name_atom,
            exp.arg_count,
        );
        try ctx.builtin_objects.append(allocator, fn_obj);
        try ctx.setGlobal(name_atom, fn_obj.toValue());
    }
}

/// Validate that all import specifiers from a module are actually exported.
/// Returns the first unresolved specifier name, or null if all resolve.
pub fn validateImports(module: VirtualModule, specifier_names: []const []const u8) ?[]const u8 {
    const module_exports = module.getExports();
    for (specifier_names) |name| {
        var found = false;
        for (module_exports) |exp| {
            if (std.mem.eql(u8, exp.name, name)) {
                found = true;
                break;
            }
        }
        if (!found) return name;
    }
    return null;
}

test "validateImports accepts known exports" {
    const names = [_][]const u8{ "sha256", "base64Encode" };
    try std.testing.expect(validateImports(.crypto, &names) == null);
}

test "validateImports reports first missing export" {
    const names = [_][]const u8{ "env", "missing" };
    const missing = validateImports(.env, &names) orelse return error.ExpectedMissingExport;
    try std.testing.expectEqualStrings("missing", missing);
}
