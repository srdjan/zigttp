//! Virtual Module Resolver
//!
//! Resolves module specifiers at compile time:
//! - `zigttp:env`    -> native env var functions
//! - `zigttp:crypto` -> native crypto functions (sha256, hmac, base64)
//! - `zigttp:router` -> native pattern-matching router
//! - `zigttp:io`     -> structured concurrent I/O (parallel, race)
//! - `./path`        -> relative file import (future)
//!
//! Resolution happens before codegen. Each virtual module registers
//! its native functions as globals in the JS context, so import bindings
//! resolve through the normal global variable lookup.

const std = @import("std");
const object = @import("../object.zig");
const context = @import("../context.zig");
const trace = @import("../trace.zig");

const env_mod = @import("env.zig");
const crypto_mod = @import("crypto.zig");
const router_mod = @import("router.zig");
const auth_mod = @import("auth.zig");
const validate_mod = @import("validate.zig");
const decode_mod = @import("decode.zig");
const cache_mod = @import("cache.zig");
const sql_mod = @import("sql.zig");
const io_mod = @import("io.zig");
const compose_mod = @import("compose.zig");
const durable_mod = @import("durable.zig");

/// Effect classification for virtual module functions.
/// Used by the contract builder to derive handler-level properties
/// (pure, read_only, retry_safe) at compile time.
pub const EffectClass = enum {
    /// Does not modify external state
    read,
    /// Modifies external state
    write,
    /// Compile-time only, no runtime effect (e.g. guard)
    none,

    pub fn toString(self: EffectClass) []const u8 {
        return switch (self) {
            .read => "read",
            .write => "write",
            .none => "none",
        };
    }
};

/// A single exported function from a virtual module
pub const ModuleExport = struct {
    name: []const u8,
    func: object.NativeFn,
    arg_count: u8,
    effect: EffectClass = .read,
};

/// Known virtual module identifiers
pub const VirtualModule = enum {
    env,
    crypto,
    router,
    auth,
    validate,
    decode,
    cache,
    sql,
    io,
    compose,
    durable,

    pub fn fromSpecifier(specifier: []const u8) ?VirtualModule {
        if (std.mem.eql(u8, specifier, "zigttp:env")) return .env;
        if (std.mem.eql(u8, specifier, "zigttp:crypto")) return .crypto;
        if (std.mem.eql(u8, specifier, "zigttp:router")) return .router;
        if (std.mem.eql(u8, specifier, "zigttp:auth")) return .auth;
        if (std.mem.eql(u8, specifier, "zigttp:validate")) return .validate;
        if (std.mem.eql(u8, specifier, "zigttp:decode")) return .decode;
        if (std.mem.eql(u8, specifier, "zigttp:cache")) return .cache;
        if (std.mem.eql(u8, specifier, "zigttp:sql")) return .sql;
        if (std.mem.eql(u8, specifier, "zigttp:io")) return .io;
        if (std.mem.eql(u8, specifier, "zigttp:compose")) return .compose;
        if (std.mem.eql(u8, specifier, "zigttp:durable")) return .durable;
        return null;
    }

    pub fn getExports(self: VirtualModule) []const ModuleExport {
        return switch (self) {
            .env => &env_mod.exports,
            .crypto => &crypto_mod.exports,
            .router => &router_mod.exports,
            .auth => &auth_mod.exports,
            .validate => &validate_mod.exports,
            .decode => &decode_mod.exports,
            .cache => &cache_mod.exports,
            .sql => &sql_mod.exports,
            .io => &io_mod.exports,
            .compose => &compose_mod.exports,
            .durable => &durable_mod.exports,
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

/// Register virtual module exports with trace-recording wrappers.
/// Each NativeFn is wrapped to record its arguments and return value
/// to the TraceRecorder in module_state slot 7 (if present).
/// The module enum must be comptime-known so we can generate wrappers.
pub fn registerVirtualModuleTraced(comptime module: VirtualModule, ctx: *context.Context, allocator: std.mem.Allocator) !void {
    if (module == .durable) {
        return registerVirtualModule(ctx, module, allocator);
    }
    const module_exports = comptime module.getExports();
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const module_name = comptime moduleEnumName(module);

    inline for (module_exports) |exp| {
        const wrapped = if (comptime shouldWrapExport(module, exp.name))
            comptime trace.makeTracingWrapper(module_name, exp.name, exp.func)
        else
            exp.func;
        const name_atom = try ctx.atoms.intern(exp.name);
        const fn_obj = try object.JSObject.createNativeFunction(
            allocator,
            pool,
            ctx.root_class_idx,
            wrapped,
            name_atom,
            exp.arg_count,
        );
        try ctx.builtin_objects.append(allocator, fn_obj);
        try ctx.setGlobal(name_atom, fn_obj.toValue());
    }
}

/// Get the string name for a VirtualModule enum at compile time.
fn moduleEnumName(comptime module: VirtualModule) []const u8 {
    return switch (module) {
        .env => "env",
        .crypto => "crypto",
        .router => "router",
        .auth => "auth",
        .validate => "validate",
        .decode => "decode",
        .cache => "cache",
        .sql => "sql",
        .io => "io",
        .compose => "compose",
        .durable => "durable",
    };
}

/// Register virtual module exports with replay stubs.
/// Each NativeFn is replaced with a stub that reads recorded return values
/// from the ReplayState in module_state slot 3.
pub fn registerVirtualModuleReplay(comptime module: VirtualModule, ctx: *context.Context, allocator: std.mem.Allocator) !void {
    if (module == .durable) {
        return registerVirtualModule(ctx, module, allocator);
    }
    const module_exports = comptime module.getExports();
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const module_name = comptime moduleEnumName(module);

    inline for (module_exports) |exp| {
        const func = if (comptime shouldWrapExport(module, exp.name))
            comptime trace.makeReplayStub(module_name, exp.name)
        else
            exp.func;
        const name_atom = try ctx.atoms.intern(exp.name);
        const fn_obj = try object.JSObject.createNativeFunction(
            allocator,
            pool,
            ctx.root_class_idx,
            func,
            name_atom,
            exp.arg_count,
        );
        try ctx.builtin_objects.append(allocator, fn_obj);
        try ctx.setGlobal(name_atom, fn_obj.toValue());
    }
}

/// Register virtual module exports with durable execution wrappers.
/// Each NativeFn is wrapped to replay from oplog, then record with write-ahead
/// persistence once the oplog is exhausted. Uses DurableState in module_state slot 3.
pub fn registerVirtualModuleDurable(comptime module: VirtualModule, ctx: *context.Context, allocator: std.mem.Allocator) !void {
    if (module == .durable) {
        return registerVirtualModule(ctx, module, allocator);
    }
    const module_exports = comptime module.getExports();
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const module_name = comptime moduleEnumName(module);

    inline for (module_exports) |exp| {
        const wrapped = if (comptime shouldWrapExport(module, exp.name))
            comptime trace.makeDurableWrapper(module_name, exp.name, exp.func)
        else
            exp.func;
        const name_atom = try ctx.atoms.intern(exp.name);
        const fn_obj = try object.JSObject.createNativeFunction(
            allocator,
            pool,
            ctx.root_class_idx,
            wrapped,
            name_atom,
            exp.arg_count,
        );
        try ctx.builtin_objects.append(allocator, fn_obj);
        try ctx.setGlobal(name_atom, fn_obj.toValue());
    }
}

fn shouldWrapExport(comptime module: VirtualModule, comptime export_name: []const u8) bool {
    return switch (module) {
        .validate => !std.mem.eql(u8, export_name, "schemaCompile") and !std.mem.eql(u8, export_name, "schemaDrop"),
        .decode => true,
        .sql => !std.mem.eql(u8, export_name, "sql"),
        // routerMatch is a pure pattern matcher with no I/O - run it directly
        .router => false,
        else => true,
    };
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
