const std = @import("std");
pub const compat = @import("../compat.zig");
pub const value = @import("../value.zig");
pub const object = @import("../object.zig");
pub const context = @import("../context.zig");
pub const string = @import("../string.zig");
pub const heap = @import("../heap.zig");
pub const http = @import("../http.zig");
pub const trace_mod = @import("../trace.zig");

/// Built-in class IDs
pub const ClassId = enum(u8) {
    object = 0,
    array = 1,
    function = 2,
    string_obj = 3,
    number = 4,
    boolean = 5,
    symbol = 6,
    @"error" = 7,
    array_buffer = 8,
    typed_array = 9,
    data_view = 10,
    promise = 11,
    map = 12,
    set = 13,
    weak_map = 14,
    weak_set = 15,
    regexp = 16,
    date = 17,
    proxy = 18,
    // Add more as needed
};

pub const ImplFn = *const fn (*context.Context, value.JSValue, []const value.JSValue) value.JSValue;

/// Create a native function wrapper from an implementation function.
/// Eliminates boilerplate wrapper functions by generating them at compile time.
/// Example: `wrap(jsonParse)` instead of `wrapJsonParse`
pub fn wrap(comptime impl: ImplFn) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
            const result = impl(ctx, this, args);
            if (ctx.hasException()) {
                return error.NativeFunctionError;
            }
            return result;
        }
    }.call;
}

pub fn getObject(val: value.JSValue) ?*object.JSObject {
    if (!val.isObject()) return null;
    return val.toPtr(object.JSObject);
}

/// Helper to get atom name as string
pub fn atomName(atom: object.Atom, ctx: *context.Context) ?[]const u8 {
    if (atom.isPredefined()) return atom.toPredefinedName();
    return ctx.atoms.getName(atom);
}

/// Helper to create Result.ok(value)
pub fn createResultOk(ctx: *context.Context, val: value.JSValue) value.JSValue {
    return createResult(ctx, true, val, null);
}

/// Helper to create Result.err(error)
pub fn createResultErr(ctx: *context.Context, err_val: value.JSValue) value.JSValue {
    return createResult(ctx, false, err_val, .@"error");
}

/// Helper to create Result err payload with a specific field name.
pub fn createResultErrWithField(ctx: *context.Context, err_val: value.JSValue, field_atom: object.Atom) value.JSValue {
    return createResult(ctx, false, err_val, field_atom);
}

/// Shared constructor for native Result objects.
pub fn createResult(
    ctx: *context.Context,
    is_ok: bool,
    payload: value.JSValue,
    error_field: ?object.Atom,
) value.JSValue {
    const result_obj = ctx.createObject(ctx.result_prototype) catch return value.JSValue.undefined_val;
    result_obj.class_id = .result;
    result_obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK] = value.JSValue.fromBool(is_ok);
    result_obj.inline_slots[object.JSObject.Slots.RESULT_VALUE] = payload;
    result_obj.inline_slots[object.JSObject.Slots.RESULT_ERROR_FIELD] = if (error_field) |atom|
        value.JSValue.fromInt(@intCast(@intFromEnum(atom)))
    else
        value.JSValue.undefined_val;
    return result_obj.toValue();
}

/// Simple value-to-string conversion for non-object types.
pub fn valueToStringSimple(allocator: std.mem.Allocator, val: value.JSValue) !*string.JSString {
    if (val.isString()) {
        return val.toPtr(string.JSString);
    }
    if (val.isInt()) {
        var buf: [32]u8 = undefined;
        const slice = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return try string.createString(allocator, "0");
        return try string.createString(allocator, slice);
    }
    if (val.isNull()) return try string.createString(allocator, "null");
    if (val.isUndefined()) return try string.createString(allocator, "undefined");
    if (val.isTrue()) return try string.createString(allocator, "true");
    if (val.isFalse()) return try string.createString(allocator, "false");
    if (val.isObject()) return try string.createString(allocator, "[object Object]");
    return try string.createString(allocator, "");
}

pub fn getCallFn() ?http.CallFunctionFn {
    return http.call_function_callback;
}

/// Invoke a JS callback via a cached call function pointer.
pub fn invokeCallback(call_fn: http.CallFunctionFn, func_obj: *object.JSObject, call_args: []const value.JSValue) ?value.JSValue {
    return call_fn(func_obj, call_args) catch return null;
}

/// Create an array with the proper prototype set (enables method chaining).
pub fn createArrayWithPrototype(ctx: *context.Context) ?*object.JSObject {
    const arr = ctx.createArray() catch return null;
    arr.prototype = ctx.array_prototype;
    return arr;
}

/// Extract callback function object from first argument.
pub fn getCallbackArg(args: []const value.JSValue) ?*object.JSObject {
    if (args.len == 0) return null;
    if (!args[0].isCallable()) return null;
    return args[0].toPtr(object.JSObject);
}

pub fn getJSString(val: value.JSValue) ?*string.JSString {
    if (val.isString()) {
        return val.toPtr(string.JSString);
    }
    // Slices are not JSStrings - they have different layout
    // Callers need to use getStringData() instead
    return null;
}

/// Get string data from any string type (flat string, slice, or rope).
/// For concat ropes, flattens using the context's arena (if available) or heap allocator.
/// The flattened result is cached in the rope node for subsequent accesses.
pub fn getStringData(val: value.JSValue) ?[]const u8 {
    return getStringDataImpl(val, null);
}

/// Context-aware variant that uses the arena for flattening concat ropes.
pub fn getStringDataCtx(val: value.JSValue, ctx: *context.Context) ?[]const u8 {
    return getStringDataImpl(val, ctx);
}

pub fn getStringDataImpl(val: value.JSValue, ctx: ?*context.Context) ?[]const u8 {
    if (val.isString()) {
        return val.toPtr(string.JSString).data();
    }
    if (val.isStringSlice()) {
        return val.toPtr(string.SliceString).data();
    }
    if (val.isRope()) {
        const rope = val.toPtr(string.RopeNode);
        if (rope.kind == .leaf) {
            return rope.payload.leaf.data();
        }
        // Concat rope: flatten and cache by converting to leaf
        if (ctx) |c| {
            if (c.hybrid) |h| {
                const flat = rope.flattenWithArena(h.arena) orelse return null;
                rope.kind = .leaf;
                rope.payload = .{ .leaf = flat };
                return flat.data();
            }
        }
        const flat = rope.flatten(std.heap.c_allocator) catch return null;
        rope.kind = .leaf;
        rope.payload = .{ .leaf = flat };
        return flat.data();
    }
    return null;
}

/// Get the parent JSString, handling both flat strings and slices.
/// For slices, returns the parent so substring operations work correctly.
pub fn getStringParent(val: value.JSValue) ?*string.JSString {
    if (val.isString()) {
        return val.toPtr(string.JSString);
    }
    if (val.isStringSlice()) {
        return val.toPtr(string.SliceString).parent;
    }
    return null;
}

pub fn toNumber(val: value.JSValue) ?f64 {
    return val.toNumber();
}

/// Helper to create float result (allocation-free with NaN-boxing)
pub fn allocFloat(ctx: *context.Context, v: f64) value.JSValue {
    _ = ctx;
    // Check if result is a safe integer (optimization)
    if (!std.math.isNan(v) and !std.math.isInf(v) and @floor(v) == v and v >= -2147483648 and v <= 2147483647) {
        return value.JSValue.fromInt(@intFromFloat(v));
    }
    // NaN-boxing: ALL f64 values are stored inline - no heap allocation!
    // This eliminates the 41.6x performance gap in mathOps benchmark.
    return value.JSValue.fromFloat(v);
}

/// Helper to add a method using a dynamic atom name
pub fn addMethodDynamic(
    ctx: *context.Context,
    obj: *object.JSObject,
    name: []const u8,
    func: object.NativeFn,
    arg_count: u8,
) !void {
    return addMethodDynamicWithId(ctx, obj, name, func, arg_count, .none);
}

/// Helper to add a method with builtin ID for fast dispatch
pub fn addMethodDynamicWithId(
    ctx: *context.Context,
    obj: *object.JSObject,
    name: []const u8,
    func: object.NativeFn,
    arg_count: u8,
    builtin_id: object.BuiltinId,
) !void {
    const allocator = ctx.allocator;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const root_class_idx = ctx.root_class_idx;
    if (object.lookupPredefinedAtom(name)) |atom| {
        const func_obj = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, func, atom, arg_count, builtin_id);
        try ctx.setPropertyChecked(obj, atom, func_obj.toValue());
        return;
    }
    const atom = try ctx.atoms.intern(name);
    const func_obj = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, func, atom, arg_count, builtin_id);
    try ctx.setPropertyChecked(obj, atom, func_obj.toValue());
}

/// Create a native function and add it as a property on an object
pub fn addMethod(
    ctx: *context.Context,
    allocator: std.mem.Allocator,
    pool: *object.HiddenClassPool,
    obj: *object.JSObject,
    root_class_idx: object.HiddenClassIndex,
    name: object.Atom,
    func: object.NativeFn,
    arg_count: u8,
) !void {
    const func_obj = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, func, name, arg_count);
    try ctx.setPropertyChecked(obj, name, func_obj.toValue());
}

/// Create a native function with builtin ID for fast dispatch
pub fn addMethodWithId(
    ctx: *context.Context,
    allocator: std.mem.Allocator,
    pool: *object.HiddenClassPool,
    obj: *object.JSObject,
    root_class_idx: object.HiddenClassIndex,
    name: object.Atom,
    func: object.NativeFn,
    arg_count: u8,
    builtin_id: object.BuiltinId,
) !void {
    const func_obj = try object.JSObject.createNativeFunctionWithId(allocator, pool, root_class_idx, func, name, arg_count, builtin_id);
    try ctx.setPropertyChecked(obj, name, func_obj.toValue());
}
