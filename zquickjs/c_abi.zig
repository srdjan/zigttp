//! C ABI compatibility layer
//!
//! Exports C-compatible functions matching mquickjs.h API for drop-in replacement.

const std = @import("std");
const value = @import("value.zig");
const context = @import("context.zig");
const gc = @import("gc.zig");
const pool = @import("pool.zig");
const bytecode = @import("bytecode.zig");
const interpreter = @import("interpreter.zig");

// Re-export JSValue as C-compatible type
pub const JSValue = value.JSValue;
pub const JSContext = context.Context;

/// Global allocator for C API (set during init)
var global_allocator: ?std.mem.Allocator = null;

// ============================================================================
// Context Management
// ============================================================================

/// Create a new JavaScript context
export fn JS_NewContext(mem_start: ?[*]u8, mem_size: usize) ?*JSContext {
    const allocator = global_allocator orelse std.heap.c_allocator;
    _ = mem_start;
    _ = mem_size;

    var gc_state = allocator.create(gc.GC) catch return null;
    gc_state.* = gc.GC.init(allocator, .{}) catch {
        allocator.destroy(gc_state);
        return null;
    };

    const ctx = context.Context.init(allocator, gc_state, .{}) catch {
        gc_state.deinit();
        allocator.destroy(gc_state);
        return null;
    };

    return ctx;
}

/// Free a JavaScript context
export fn JS_FreeContext(ctx: ?*JSContext) void {
    if (ctx) |c| {
        const allocator = c.allocator;
        const gc_state = c.gc_state;
        c.deinit();
        gc_state.deinit();
        allocator.destroy(gc_state);
    }
}

/// Run garbage collection
export fn JS_GC(ctx: ?*JSContext) void {
    if (ctx) |c| {
        c.gc_state.minorGC();
    }
}

// ============================================================================
// Value Creation
// ============================================================================

/// Create integer value
export fn JS_NewInt32(ctx: ?*JSContext, val: i32) JSValue {
    _ = ctx;
    return JSValue.fromInt(val);
}

/// Create float value
export fn JS_NewFloat64(ctx: ?*JSContext, val: f64) JSValue {
    _ = ctx;
    _ = val;
    // TODO: Implement float boxing
    return JSValue.undefined_val;
}

/// Create string value
export fn JS_NewString(ctx: ?*JSContext, buf: ?[*:0]const u8) JSValue {
    _ = ctx;
    _ = buf;
    // TODO: Implement string creation
    return JSValue.undefined_val;
}

/// Create new object
export fn JS_NewObject(ctx: ?*JSContext) JSValue {
    _ = ctx;
    // TODO: Implement object creation
    return JSValue.undefined_val;
}

/// Create new array
export fn JS_NewArray(ctx: ?*JSContext, initial_len: i32) JSValue {
    _ = ctx;
    _ = initial_len;
    // TODO: Implement array creation
    return JSValue.undefined_val;
}

// ============================================================================
// Value Conversion
// ============================================================================

/// Convert to 32-bit integer
export fn JS_ToInt32(ctx: ?*JSContext, pres: ?*i32, val: JSValue) i32 {
    _ = ctx;
    if (val.isInt()) {
        if (pres) |p| p.* = val.getInt();
        return 0; // Success
    }
    return -1; // Error
}

/// Convert to number
export fn JS_ToNumber(ctx: ?*JSContext, pres: ?*f64, val: JSValue) i32 {
    _ = ctx;
    if (val.isInt()) {
        if (pres) |p| p.* = @floatFromInt(val.getInt());
        return 0;
    }
    return -1;
}

/// Check if value is integer
export fn JS_IsInt(val: JSValue) bool {
    return val.isInt();
}

/// Check if value is null
export fn JS_IsNull(val: JSValue) bool {
    return val.isNull();
}

/// Check if value is undefined
export fn JS_IsUndefined(val: JSValue) bool {
    return val.isUndefined();
}

/// Check if value is boolean
export fn JS_IsBool(val: JSValue) bool {
    return val.isBool();
}

/// Check if value is exception
export fn JS_IsException(val: JSValue) bool {
    return val.isException();
}

// ============================================================================
// Code Execution
// ============================================================================

/// Evaluate JavaScript code
export fn JS_Eval(ctx: ?*JSContext, input: ?[*]const u8, input_len: usize, filename: ?[*:0]const u8, eval_flags: i32) JSValue {
    _ = ctx;
    _ = input;
    _ = input_len;
    _ = filename;
    _ = eval_flags;
    // TODO: Implement evaluation
    return JSValue.undefined_val;
}

/// Call a function
export fn JS_Call(ctx: ?*JSContext, func_val: JSValue, this_val: JSValue, argc: i32, argv: ?[*]const JSValue) JSValue {
    _ = ctx;
    _ = func_val;
    _ = this_val;
    _ = argc;
    _ = argv;
    // TODO: Implement function calls
    return JSValue.undefined_val;
}

// ============================================================================
// Property Access
// ============================================================================

/// Get property by string name
export fn JS_GetPropertyStr(ctx: ?*JSContext, obj: JSValue, prop: ?[*:0]const u8) JSValue {
    _ = ctx;
    _ = obj;
    _ = prop;
    // TODO: Implement property access
    return JSValue.undefined_val;
}

/// Set property by string name
export fn JS_SetPropertyStr(ctx: ?*JSContext, obj: JSValue, prop: ?[*:0]const u8, val: JSValue) i32 {
    _ = ctx;
    _ = obj;
    _ = prop;
    _ = val;
    // TODO: Implement property setting
    return -1;
}

// ============================================================================
// Global Object
// ============================================================================

/// Get the global object
export fn JS_GetGlobalObject(ctx: ?*JSContext) JSValue {
    if (ctx) |c| {
        return c.global;
    }
    return JSValue.undefined_val;
}

// ============================================================================
// GC Reference Management
// ============================================================================

/// GC reference guard (for protecting values during GC)
pub const JSGCRef = extern struct {
    val: JSValue,
    prev: ?*JSGCRef,
};

/// Push GC reference
export fn JS_PushGCRef(ctx: ?*JSContext, ref: ?*JSGCRef) ?*JSValue {
    _ = ctx;
    if (ref) |r| {
        return &r.val;
    }
    return null;
}

/// Pop GC reference
export fn JS_PopGCRef(ctx: ?*JSContext, ref: ?*JSGCRef) JSValue {
    _ = ctx;
    if (ref) |r| {
        return r.val;
    }
    return JSValue.undefined_val;
}

// ============================================================================
// Initialization
// ============================================================================

/// Initialize the ZQuickJS C ABI layer
export fn ZQ_Init(allocator_ptr: ?*anyopaque) void {
    _ = allocator_ptr;
    // Use C allocator by default
    global_allocator = std.heap.c_allocator;
}

/// Set custom allocator
pub fn setAllocator(allocator: std.mem.Allocator) void {
    global_allocator = allocator;
}

// ============================================================================
// Tests
// ============================================================================

test "C ABI integer creation" {
    const val = JS_NewInt32(null, 42);
    try std.testing.expect(JS_IsInt(val));

    var result: i32 = 0;
    try std.testing.expectEqual(@as(i32, 0), JS_ToInt32(null, &result, val));
    try std.testing.expectEqual(@as(i32, 42), result);
}

test "C ABI special values" {
    try std.testing.expect(JS_IsNull(JSValue.null_val));
    try std.testing.expect(JS_IsUndefined(JSValue.undefined_val));
    try std.testing.expect(JS_IsBool(JSValue.true_val));
    try std.testing.expect(JS_IsBool(JSValue.false_val));
    try std.testing.expect(JS_IsException(JSValue.exception_val));
}
