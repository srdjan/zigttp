//! Virtual Module Binding Specification
//!
//! A single comptime struct that captures every fact about a virtual module
//! needed by all consumers: resolver, type checker, verifier, bool checker,
//! contract builder, state manager, and trace/replay system.
//!
//! Built-in modules declare `pub const binding: ModuleBinding` alongside
//! their existing `exports` array. Third-party modules depend on zigttp-sdk
//! and write functions using ModuleFn (opaque handle) instead of NativeFn.
//!
//! The ModuleHandle opaque type provides a capability-based sandbox:
//! third-party modules cannot dereference the handle or access Context
//! internals. All interaction goes through free functions in this file.

const std = @import("std");
const object = @import("object.zig");
const value = @import("value.zig");
const context = @import("context.zig");
const resolver = @import("modules/resolver.zig");

// Re-export EffectClass from resolver for backward compatibility
pub const EffectClass = resolver.EffectClass;

// -------------------------------------------------------------------------
// Opaque handle for third-party module sandbox
// -------------------------------------------------------------------------

/// Opaque handle passed to third-party module functions.
/// Cannot be dereferenced - modules interact with the runtime exclusively
/// through the free functions below. Internally this is a *Context, but
/// that fact is hidden from module authors.
pub const ModuleHandle = opaque {};

/// Function signature for third-party (sandboxed) module functions.
/// Receives an opaque handle instead of raw *anyopaque.
pub const ModuleFn = *const fn (
    handle: *ModuleHandle,
    this: value.JSValue,
    args: []const value.JSValue,
) anyerror!value.JSValue;

/// Generate a NativeFn wrapper around a ModuleFn.
/// The wrapper casts *anyopaque to *ModuleHandle (a no-op pointer cast)
/// so the module receives the opaque handle it expects.
pub fn wrapModuleFn(comptime user_fn: ModuleFn) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            return user_fn(@ptrCast(ctx_ptr), this, args);
        }
    }.call;
}

// -------------------------------------------------------------------------
// ModuleHandle free functions (the sandbox API)
// -------------------------------------------------------------------------

/// Cast a ModuleHandle to the underlying Context. Internal use only -
/// this function is called by the SDK free functions below, never by
/// module authors directly.
fn handleToContext(handle: *ModuleHandle) *context.Context {
    return @ptrCast(@alignCast(handle));
}

/// Create a new JS string value. Ownership transfers to the GC.
pub fn createString(handle: *ModuleHandle, data: []const u8) !value.JSValue {
    const ctx = handleToContext(handle);
    return ctx.createString(data);
}

/// Extract a borrowed string slice from a JSValue.
/// The returned slice is valid only during the current function call.
pub fn extractString(val: value.JSValue) ?[]const u8 {
    const str = val.toStringStruct() orelse return null;
    return str.asSlice();
}

/// Extract an integer from a JSValue.
pub fn extractInt(val: value.JSValue) ?i32 {
    return val.toInt();
}

/// Extract a float from a JSValue.
pub fn extractFloat(val: value.JSValue) ?f64 {
    return val.toFloat();
}

/// Create a Result object: { ok: true, value: payload }
pub fn resultOk(handle: *ModuleHandle, payload: value.JSValue) !value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/util.zig");
    return util.createPlainResultOk(ctx, payload);
}

/// Create a Result object: { ok: false, error: message }
pub fn resultErr(handle: *ModuleHandle, message: []const u8) !value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/util.zig");
    return util.createPlainResultErr(ctx, message);
}

/// Throw a JS error. Sets ctx.exception and returns exception_val.
pub fn throwError(handle: *ModuleHandle, name: []const u8, message: []const u8) value.JSValue {
    const ctx = handleToContext(handle);
    const util = @import("modules/util.zig");
    return util.throwError(ctx, name, message);
}

/// Get typed module state from a slot. Returns null if not initialized.
pub fn getState(handle: *ModuleHandle, comptime T: type, slot: usize) ?*T {
    const ctx = handleToContext(handle);
    return ctx.getModuleState(T, slot);
}

/// Set module state in a slot with a cleanup callback.
pub fn setState(
    handle: *ModuleHandle,
    slot: usize,
    ptr: *anyopaque,
    deinit_fn: *const fn (*anyopaque, std.mem.Allocator) void,
) void {
    const ctx = handleToContext(handle);
    ctx.setModuleState(slot, ptr, deinit_fn);
}

/// Get the runtime allocator for persistent allocations.
pub fn getAllocator(handle: *ModuleHandle) std.mem.Allocator {
    const ctx = handleToContext(handle);
    return ctx.allocator;
}

// -------------------------------------------------------------------------
// Return type classification
// -------------------------------------------------------------------------

/// Unified return type for verification, type checking, and bool checking.
/// Each consumer maps this to its own internal representation.
pub const ReturnKind = enum {
    /// Plain value types - no caller-side check required
    boolean,
    number,
    string,
    object,
    undefined,
    unknown,

    /// Optional types - verifier requires narrowing before use
    optional_string,
    optional_object,

    /// Result type ({ok, value, error}) - verifier requires .ok check
    result,
};

// -------------------------------------------------------------------------
// Contract extraction rules
// -------------------------------------------------------------------------

/// Category for extracted literals in the contract JSON.
pub const ContractCategory = enum {
    env,
    cache_namespace,
    sql_registration,
    durable_key,
    durable_step,
    durable_signal,
    durable_producer_key,
    schema_compile,
    request_schema,
    route_pattern,
    /// Escape hatch for multi-arg or complex extraction.
    custom,
};

/// Transform applied to extracted literal before storing.
pub const ContractTransform = enum {
    /// Extract hostname from URL string.
    extract_host,
    /// No transform, use raw literal.
    identity,
};

/// Declarative rule for extracting a literal from a function argument.
pub const ContractExtraction = struct {
    /// Which argument position holds the literal (0-indexed).
    arg_position: u8 = 0,
    /// What category this literal belongs to in the contract.
    category: ContractCategory,
    /// Optional transform applied to the raw string.
    transform: ?ContractTransform = null,
    /// When true, the call sets a boolean flag rather than extracting a literal.
    flag_only: bool = false,
};

/// Boolean flags set on the contract when a function is imported or called.
pub const ContractFlags = struct {
    sets_durable_used: bool = false,
    sets_durable_timers: bool = false,
    sets_bearer_auth: bool = false,
    sets_jwt_auth: bool = false,
};

// -------------------------------------------------------------------------
// Function binding
// -------------------------------------------------------------------------

/// Complete metadata for a single exported function.
pub const FunctionBinding = struct {
    /// Function name as visible from JS import.
    name: []const u8,

    /// Native implementation (for built-in modules using raw Context access).
    /// Exactly one of func or module_func must be set.
    func: ?object.NativeFn = null,

    /// Sandboxed implementation (for third-party modules using ModuleHandle).
    /// The registration system wraps this into a NativeFn automatically.
    module_func: ?ModuleFn = null,

    /// Argument count (for JS runtime).
    arg_count: u8,

    /// Effect classification for handler property derivation.
    effect: EffectClass = .read,

    /// Return type classification. Drives verifier, bool checker, and type checker.
    returns: ReturnKind = .unknown,

    /// Type signature for parameter types (mapped to TypeIndex during type init).
    param_types: []const ReturnKind = &.{},

    /// Whether trace/replay/durable should wrap this function.
    /// false for setup-only functions (schemaCompile, schemaDrop, sql register).
    traceable: bool = true,

    /// Contract extraction rules for this function's arguments.
    contract_extractions: []const ContractExtraction = &.{},

    /// Flags set on the contract when this function is called.
    contract_flags: ContractFlags = .{},

    /// Get the NativeFn for this binding, wrapping ModuleFn if needed.
    pub fn getNativeFn(comptime self: FunctionBinding) object.NativeFn {
        if (self.func) |f| return f;
        if (self.module_func) |mf| return wrapModuleFn(mf);
        @compileError("FunctionBinding must set either func or module_func");
    }

    /// Whether the return value is a Result object ({ok, value, error}).
    pub fn isResult(self: FunctionBinding) bool {
        return self.returns == .result;
    }

    /// Convert to a legacy ModuleExport for backward compatibility.
    pub fn toModuleExport(comptime self: FunctionBinding) resolver.ModuleExport {
        return .{
            .name = self.name,
            .func = self.getNativeFn(),
            .arg_count = self.arg_count,
            .effect = self.effect,
        };
    }
};

// -------------------------------------------------------------------------
// Module binding
// -------------------------------------------------------------------------

/// Complete declaration for a virtual module.
/// One per module - the single source of truth for all consumers.
pub const ModuleBinding = struct {
    /// Module specifier as used in JS imports: "zigttp:crypto", "zigttp:redis".
    specifier: []const u8,

    /// Short name for trace/replay JSON keys and enum references.
    name: []const u8,

    /// All exported functions with full metadata.
    exports: []const FunctionBinding,

    /// Whether this module needs per-runtime state.
    stateful: bool = false,

    /// State initialization callback (called during runtime init).
    state_init: ?*const fn (*anyopaque, std.mem.Allocator) anyerror!void = null,

    /// State cleanup callback (called during context deinit).
    state_deinit: ?*const fn (*anyopaque, std.mem.Allocator) void = null,

    /// Contract section name for contract.json output.
    /// When non-null, the module gets its own top-level section.
    contract_section: ?[]const u8 = null,

    /// Whether the contract should feed into RuntimePolicy sandboxing.
    sandboxable: bool = false,

    /// Whether this module is compile-time only (skip trace/replay/durable).
    comptime_only: bool = false,

    /// Generate a legacy exports array from this binding.
    pub fn toModuleExports(comptime self: ModuleBinding) [self.exports.len]resolver.ModuleExport {
        var result: [self.exports.len]resolver.ModuleExport = undefined;
        for (self.exports, 0..) |exp, i| {
            result[i] = exp.toModuleExport();
        }
        return result;
    }
};

// -------------------------------------------------------------------------
// Registry validation
// -------------------------------------------------------------------------

/// Validate a set of module bindings at compile time.
/// Produces clear compile errors for:
///   - duplicate specifiers
///   - duplicate function names across modules
///   - state lifecycle inconsistency (stateful without init/deinit)
///   - specifier format (must start with "zigttp:")
///   - function bindings missing both func and module_func
pub fn validateBindings(comptime bindings: []const ModuleBinding) void {
    @setEvalBranchQuota(5000);
    // Check specifier format and state consistency per module
    for (bindings) |b| {
        if (b.specifier.len < 7 or !std.mem.eql(u8, b.specifier[0..7], "zigttp:")) {
            @compileError("module specifier must start with 'zigttp:': " ++ b.specifier);
        }
        // state_init and state_deinit must be set together or not at all
        if (b.state_init != null and b.state_deinit == null) {
            @compileError("module has state_init but missing state_deinit: " ++ b.specifier);
        }
        if (b.state_init == null and b.state_deinit != null) {
            @compileError("module has state_deinit but missing state_init: " ++ b.specifier);
        }
        for (b.exports) |f| {
            if (f.func == null and f.module_func == null) {
                @compileError("function binding missing both func and module_func: " ++ f.name);
            }
        }
    }
    // Check unique specifiers
    for (bindings, 0..) |a, i| {
        for (bindings[i + 1 ..]) |b| {
            if (std.mem.eql(u8, a.specifier, b.specifier)) {
                @compileError("duplicate module specifier: " ++ a.specifier);
            }
        }
    }
    // Check unique function names across all modules
    for (bindings, 0..) |a, ai| {
        for (a.exports, 0..) |af, afi| {
            // Check within same module (later exports)
            for (a.exports[afi + 1 ..]) |af2| {
                if (std.mem.eql(u8, af.name, af2.name)) {
                    @compileError("duplicate function name within " ++ a.specifier ++ ": " ++ af.name);
                }
            }
            // Check across later modules
            for (bindings[ai + 1 ..]) |b| {
                for (b.exports) |bf| {
                    if (std.mem.eql(u8, af.name, bf.name)) {
                        @compileError("duplicate function name '" ++ af.name ++ "' in " ++ a.specifier ++ " and " ++ b.specifier);
                    }
                }
            }
        }
    }
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "FunctionBinding toModuleExport preserves fields" {
    const fb = FunctionBinding{
        .name = "testFn",
        .func = struct {
            fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                return value.JSValue.undefined_val;
            }
        }.f,
        .arg_count = 2,
        .effect = .write,
        .returns = .boolean,
    };

    const me = comptime fb.toModuleExport();
    try std.testing.expectEqualStrings("testFn", me.name);
    try std.testing.expectEqual(@as(u8, 2), me.arg_count);
    try std.testing.expectEqual(EffectClass.write, me.effect);
}

test "ModuleBinding toModuleExports generates correct array" {
    const binding = ModuleBinding{
        .specifier = "zigttp:test",
        .name = "test",
        .exports = &.{
            .{ .name = "fn1", .func = struct {
                fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                    return value.JSValue.undefined_val;
                }
            }.f, .arg_count = 1 },
            .{ .name = "fn2", .func = struct {
                fn f(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                    return value.JSValue.false_val;
                }
            }.f, .arg_count = 0, .effect = .write },
        },
    };

    const exports = comptime binding.toModuleExports();
    try std.testing.expectEqual(@as(usize, 2), exports.len);
    try std.testing.expectEqualStrings("fn1", exports[0].name);
    try std.testing.expectEqualStrings("fn2", exports[1].name);
    try std.testing.expectEqual(EffectClass.read, exports[0].effect);
    try std.testing.expectEqual(EffectClass.write, exports[1].effect);
}

test "wrapModuleFn generates valid NativeFn" {
    const module_fn: ModuleFn = struct {
        fn f(_: *ModuleHandle, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            return value.JSValue.true_val;
        }
    }.f;

    const native_fn = comptime wrapModuleFn(module_fn);
    // Verify the wrapper compiles and has the right type
    const ptr_info = @typeInfo(@TypeOf(native_fn));
    try std.testing.expect(ptr_info == .pointer);
}

test "FunctionBinding with module_func wraps correctly" {
    const fb = FunctionBinding{
        .name = "sandboxedFn",
        .module_func = struct {
            fn f(_: *ModuleHandle, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
                return value.JSValue.true_val;
            }
        }.f,
        .arg_count = 1,
    };

    const me = comptime fb.toModuleExport();
    try std.testing.expectEqualStrings("sandboxedFn", me.name);
    try std.testing.expect(me.func != null);
}
