//! Virtual Module Resolver
//!
//! Resolves module specifiers at compile time against the unified module
//! binding registry. Core built-ins remain in-tree under `zigttp:*`, while
//! explicitly registered packaged extensions live under `zigttp-ext:*`.

const std = @import("std");
const object = @import("../object.zig");
const context = @import("../context.zig");
const trace = @import("../trace.zig");
const mb = @import("../module_binding.zig");
const builtin_modules = @import("../builtin_modules.zig");

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

/// A single exported function from a virtual module.
pub const ModuleExport = struct {
    name: []const u8,
    func: object.NativeFn,
    arg_count: u8,
    effect: EffectClass = .read,
};

/// Resolution result
pub const ResolveResult = union(enum) {
    /// Virtual module with native implementations
    virtual: *const mb.ModuleBinding,
    /// Relative file import (specifier is the path)
    file: []const u8,
    /// Unknown module - compile error
    unknown: void,
};

/// Resolve a module specifier.
pub fn resolve(specifier: []const u8) ResolveResult {
    if (builtin_modules.fromSpecifier(specifier)) |binding| {
        return .{ .virtual = binding };
    }

    if (specifier.len > 0 and (specifier[0] == '.' or specifier[0] == '/')) {
        return .{ .file = specifier };
    }

    return .unknown;
}

/// Register all exports from a virtual module into a JS context.
/// Called once per import at compile time (before codegen runs).
pub fn registerVirtualModule(comptime binding: mb.ModuleBinding, ctx: *context.Context, allocator: std.mem.Allocator) !void {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    inline for (binding.exports) |exp| {
        const base_func = comptime wrappedExportFn(binding, exp);
        const name_atom = try ctx.atoms.intern(exp.name);
        const fn_obj = try object.JSObject.createNativeFunction(
            allocator,
            pool,
            ctx.root_class_idx,
            base_func,
            name_atom,
            exp.arg_count,
        );
        try ctx.builtin_objects.append(allocator, fn_obj);
        try ctx.setGlobal(name_atom, fn_obj.toValue());
    }
}

/// Register module exports with trace-recording wrappers.
pub fn registerVirtualModuleTraced(comptime binding: mb.ModuleBinding, ctx: *context.Context, allocator: std.mem.Allocator) !void {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    inline for (binding.exports) |exp| {
        const base_func = comptime wrappedExportFn(binding, exp);
        const func = if (comptime shouldWrapExport(binding, exp))
            comptime trace.makeTracingWrapper(binding.name, exp.name, base_func)
        else
            base_func;
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

/// Register module exports with replay stubs.
pub fn registerVirtualModuleReplay(comptime binding: mb.ModuleBinding, ctx: *context.Context, allocator: std.mem.Allocator) !void {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    inline for (binding.exports) |exp| {
        const base_func = comptime wrappedExportFn(binding, exp);
        const func = if (comptime shouldWrapExport(binding, exp))
            comptime trace.makeReplayStub(binding.name, exp.name)
        else
            base_func;
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

/// Register module exports with durable execution wrappers.
pub fn registerVirtualModuleDurable(comptime binding: mb.ModuleBinding, ctx: *context.Context, allocator: std.mem.Allocator) !void {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    inline for (binding.exports) |exp| {
        const base_func = comptime wrappedExportFn(binding, exp);
        const func = if (comptime shouldWrapExport(binding, exp))
            comptime trace.makeDurableWrapper(binding.name, exp.name, base_func)
        else
            base_func;
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

fn shouldWrapExport(comptime binding: mb.ModuleBinding, comptime func_binding: mb.FunctionBinding) bool {
    return !binding.comptime_only and !binding.self_managed_io and func_binding.traceable;
}

fn wrappedExportFn(comptime binding: mb.ModuleBinding, comptime func_binding: mb.FunctionBinding) object.NativeFn {
    if (func_binding.func) |native_fn| {
        if (binding.required_capabilities.len == 0) return native_fn;
        return comptime mb.wrapNativeFnWithCapabilities(native_fn, binding.specifier, binding.required_capabilities);
    }
    return func_binding.getNativeFn();
}

/// Validate that all import specifiers from a module are actually exported.
/// Returns the first unresolved specifier name, or null if all resolve.
pub fn validateImports(binding: *const mb.ModuleBinding, specifier_names: []const []const u8) ?[]const u8 {
    for (specifier_names) |name| {
        var found = false;
        for (binding.exports) |exp| {
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
    const binding = builtin_modules.fromSpecifier("zigttp:crypto") orelse return error.ExpectedBinding;
    const names = [_][]const u8{ "sha256", "base64Encode" };
    try std.testing.expect(validateImports(binding, &names) == null);
}

test "validateImports accepts extension exports" {
    const binding = builtin_modules.fromSpecifier("zigttp-ext:math") orelse return error.ExpectedBinding;
    const names = [_][]const u8{ "double", "isEven" };
    try std.testing.expect(validateImports(binding, &names) == null);
}

test "validateImports reports first missing export" {
    const binding = builtin_modules.fromSpecifier("zigttp:env") orelse return error.ExpectedBinding;
    const names = [_][]const u8{ "env", "missing" };
    const missing = validateImports(binding, &names) orelse return error.ExpectedMissingExport;
    try std.testing.expectEqualStrings("missing", missing);
}
