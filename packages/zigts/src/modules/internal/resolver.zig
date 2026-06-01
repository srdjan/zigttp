//! Virtual Module Resolver
//!
//! Resolves module specifiers at compile time against the unified module
//! binding registry. Core built-ins remain in-tree under `zigttp:*`, while
//! explicitly registered packaged extensions live under `zigttp-ext:*`.

const std = @import("std");
const object = @import("../../object.zig");
const context = @import("../../context.zig");
const trace = @import("../../trace.zig");
const mb = @import("../../module_binding.zig");
const builtin_modules = @import("../../builtin_modules.zig");

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
        try registerNativeExport(ctx, allocator, pool, name_atom, base_func, exp.arg_count);
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
        try registerNativeExport(ctx, allocator, pool, name_atom, func, exp.arg_count);
    }
}

/// Register module exports with replay stubs.
pub fn registerVirtualModuleReplay(comptime binding: mb.ModuleBinding, ctx: *context.Context, allocator: std.mem.Allocator) !void {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    inline for (binding.exports) |exp| {
        const base_func = comptime wrappedExportFn(binding, exp);
        const func = if (comptime isFetchExport(binding, exp))
            base_func
        else if (comptime shouldWrapExport(binding, exp))
            comptime trace.makeReplayStub(binding.name, exp.name)
        else
            base_func;
        const name_atom = try ctx.atoms.intern(exp.name);
        try registerNativeExport(ctx, allocator, pool, name_atom, func, exp.arg_count);
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
        try registerNativeExport(ctx, allocator, pool, name_atom, func, exp.arg_count);
    }
}

fn registerNativeExport(
    ctx: *context.Context,
    allocator: std.mem.Allocator,
    pool: *object.HiddenClassPool,
    name_atom: object.Atom,
    func: object.NativeFn,
    arg_count: u8,
) !void {
    const fn_obj = try object.JSObject.createNativeFunction(
        allocator,
        pool,
        ctx.root_class_idx,
        func,
        name_atom,
        arg_count,
    );
    var fn_obj_unowned = true;
    errdefer if (fn_obj_unowned) fn_obj.destroyBuiltin(allocator, pool);
    try ctx.builtin_objects.append(allocator, fn_obj);
    fn_obj_unowned = false;
    try ctx.setGlobal(name_atom, fn_obj.toValue());
}

fn shouldWrapExport(comptime binding: mb.ModuleBinding, comptime func_binding: mb.FunctionBinding) bool {
    return !binding.comptime_only and !binding.self_managed_io and func_binding.traceable;
}

fn isFetchExport(comptime binding: mb.ModuleBinding, comptime func_binding: mb.FunctionBinding) bool {
    return std.mem.eql(u8, binding.specifier, "zigttp:fetch") and std.mem.eql(u8, func_binding.name, "fetch");
}

fn wrappedExportFn(comptime binding: mb.ModuleBinding, comptime func_binding: mb.FunctionBinding) object.NativeFn {
    // Both branches install the active-module context so `sdk.requireCapability`
    // sees the binding's declared `required_capabilities`. Skipping the wrap on
    // the `module_func` path would silently leave every sandbox SDK call with
    // an empty active context — that was the gap from Wave 1B/2D P0 #3 in the
    // 2026-05-23 review.
    if (func_binding.func) |native_fn| {
        if (binding.required_capabilities.len == 0) return native_fn;
        return comptime mb.wrapNativeFnWithCapabilities(native_fn, binding.specifier, binding.required_capabilities);
    }
    if (func_binding.module_func) |module_fn| {
        return comptime mb.wrapModuleFnWithCapabilities(module_fn, binding.specifier, binding.required_capabilities);
    }
    @compileError("FunctionBinding must set either func or module_func: " ++ func_binding.name);
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

test "validateImports reports first missing export" {
    const binding = builtin_modules.fromSpecifier("zigttp:env") orelse return error.ExpectedBinding;
    const names = [_][]const u8{ "env", "missing" };
    const missing = validateImports(binding, &names) orelse return error.ExpectedMissingExport;
    try std.testing.expectEqualStrings("missing", missing);
}

// ---------------------------------------------------------------------------
// wrappedExportFn — capability propagation on the module_func path.
//
// Wave 1B/2D P0 #3 (2026-05-23 review) flagged that this branch previously
// short-circuited through `getNativeFn()`, which wraps with empty
// `required_capabilities`. Any sandbox module declaring caps would then run
// with an empty active-module context, so every `sdk.requireCapability` call
// — including the ones inside `sdk.hmacSha256`, `sdk.fillRandom`, and the
// JWT verifier — would refuse the operation. The test below pins the fixed
// shape: a `module_func` binding with `.crypto` declared in its parent
// `ModuleBinding` must observe `.crypto` in the active context.
// ---------------------------------------------------------------------------

const value = @import("../../value.zig");

test "wrappedExportFn propagates required_capabilities through module_func" {
    const probe = struct {
        fn run(_: *mb.ModuleHandle, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            // Asserts execute against the active context installed by the
            // resolver's wrapper, not the test's outer scope.
            const fake_handle: *mb.ModuleHandle = @ptrFromInt(0x1);
            if (!mb.hasCapability(fake_handle, .crypto)) return error.MissingCrypto;
            if (mb.hasCapability(fake_handle, .clock)) return error.UnexpectedClock;
            return value.JSValue.true_val;
        }
    }.run;

    const fb = mb.FunctionBinding{
        .name = "probe",
        .module_func = probe,
        .arg_count = 0,
    };
    const binding = mb.ModuleBinding{
        .specifier = "zigttp:test-cap-probe",
        .name = "test-cap-probe",
        .required_capabilities = &.{.crypto},
        .exports = &.{fb},
    };

    const wrapped = comptime wrappedExportFn(binding, fb);
    const result = try wrapped(@ptrFromInt(0x1), value.JSValue.undefined_val, &.{});
    try std.testing.expect(result.isTrue());
}

test "wrappedExportFn leaves func path unwrapped when no capabilities declared" {
    const native = struct {
        fn run(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            return value.JSValue.true_val;
        }
    }.run;

    const fb = mb.FunctionBinding{
        .name = "bare",
        .func = native,
        .arg_count = 0,
    };
    const binding = mb.ModuleBinding{
        .specifier = "zigttp:test-bare",
        .name = "test-bare",
        .exports = &.{fb},
    };

    const wrapped = comptime wrappedExportFn(binding, fb);
    // No wrapping means the wrapped pointer is literally the original.
    try std.testing.expectEqual(@as(object.NativeFn, native), wrapped);
}
