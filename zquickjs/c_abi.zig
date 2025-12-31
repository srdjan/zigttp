//! C ABI compatibility layer
//!
//! Exports C-compatible functions matching mquickjs.h API for drop-in replacement.

const std = @import("std");
const value = @import("value.zig");
const context = @import("context.zig");
const gc = @import("gc.zig");
const object = @import("object.zig");
const string = @import("string.zig");
const heap = @import("heap.zig");
const parser = @import("parser/root.zig");
const interpreter = @import("interpreter.zig");
const bytecode = @import("bytecode.zig");

// Re-export types as C-compatible
pub const JSValue = value.JSValue;
pub const JSContext = context.Context;

/// Class IDs matching mquickjs.h
pub const JSObjectClassEnum = enum(c_int) {
    JS_CLASS_OBJECT = 0,
    JS_CLASS_ARRAY = 1,
    JS_CLASS_C_FUNCTION = 2,
    JS_CLASS_CLOSURE = 3,
    JS_CLASS_NUMBER = 4,
    JS_CLASS_BOOLEAN = 5,
    JS_CLASS_STRING = 6,
    JS_CLASS_DATE = 7,
    JS_CLASS_REGEXP = 8,
    JS_CLASS_ERROR = 9,
    JS_CLASS_EVAL_ERROR = 10,
    JS_CLASS_RANGE_ERROR = 11,
    JS_CLASS_REFERENCE_ERROR = 12,
    JS_CLASS_SYNTAX_ERROR = 13,
    JS_CLASS_TYPE_ERROR = 14,
    JS_CLASS_URI_ERROR = 15,
    JS_CLASS_INTERNAL_ERROR = 16,
    JS_CLASS_ARRAY_BUFFER = 17,
    JS_CLASS_TYPED_ARRAY = 18,
    _,
};

/// Eval flags matching mquickjs.h
pub const JS_EVAL_RETVAL: c_int = 1 << 0;
pub const JS_EVAL_REPL: c_int = 1 << 1;
pub const JS_EVAL_STRIP_COL: c_int = 1 << 2;
pub const JS_EVAL_JSON: c_int = 1 << 3;

/// GC reference guard (for protecting values during GC)
pub const JSGCRef = extern struct {
    val: JSValue,
    prev: ?*JSGCRef,
};

/// C string buffer for temporary conversions
pub const JSCStringBuf = extern struct {
    buf: [5]u8,
};

/// Global allocator for C API (set during init)
var global_allocator: ?std.mem.Allocator = null;

/// Global hidden class cache (for C API created objects)
var global_root_class: ?*object.HiddenClass = null;

// ============================================================================
// Context Management
// ============================================================================

/// Create a new JavaScript context
export fn JS_NewContext(mem_start: ?[*]u8, mem_size: usize) ?*JSContext {
    const allocator = global_allocator orelse std.heap.c_allocator;
    _ = mem_start;
    _ = mem_size;

    // Initialize root hidden class if needed
    if (global_root_class == null) {
        global_root_class = object.HiddenClass.init(allocator) catch return null;
    }

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

/// Set context opaque pointer
export fn JS_SetContextOpaque(ctx: ?*JSContext, opaque_ptr: ?*anyopaque) void {
    _ = ctx;
    _ = opaque_ptr;
    // TODO: Store opaque in context
}

/// Get context opaque pointer
export fn JS_GetContextOpaque(ctx: ?*JSContext) ?*anyopaque {
    _ = ctx;
    return null;
}

// ============================================================================
// Value Creation
// ============================================================================

/// Create integer value (31-bit signed)
export fn JS_NewInt32(ctx: ?*JSContext, val: i32) JSValue {
    _ = ctx;
    return JSValue.fromInt(val);
}

/// Create unsigned 32-bit integer value
export fn JS_NewUint32(ctx: ?*JSContext, val: u32) JSValue {
    // For values that fit in 31 bits, use int tag
    if (val <= std.math.maxInt(i32)) {
        return JSValue.fromInt(@intCast(val));
    }
    // Otherwise needs float boxing
    return JS_NewFloat64(ctx, @floatFromInt(val));
}

/// Create 64-bit integer value
export fn JS_NewInt64(ctx: ?*JSContext, val: i64) JSValue {
    // For values that fit in 31 bits, use int tag
    if (val >= std.math.minInt(i32) and val <= std.math.maxInt(i32)) {
        return JSValue.fromInt(@intCast(val));
    }
    // Otherwise needs float boxing
    return JS_NewFloat64(ctx, @floatFromInt(val));
}

/// Create float64 value
export fn JS_NewFloat64(ctx: ?*JSContext, d: f64) JSValue {
    // Allocate a float box
    const allocator = if (ctx) |c| c.allocator else (global_allocator orelse return JSValue.undefined_val);

    const float_box = allocator.create(value.JSValue.Float64Box) catch return JSValue.undefined_val;
    float_box.* = .{
        .header = heap.MemBlockHeader.init(.float64, @sizeOf(value.JSValue.Float64Box)),
        ._pad = 0,
        .value = d,
    };

    return JSValue.fromPtr(float_box);
}

/// Create boolean value
export fn JS_NewBool(val: c_int) JSValue {
    return if (val != 0) JSValue.true_val else JSValue.false_val;
}

/// Create string value from null-terminated C string
export fn JS_NewString(ctx: ?*JSContext, buf: ?[*:0]const u8) JSValue {
    if (buf == null) return JSValue.null_val;
    const s = std.mem.span(buf.?);
    return JS_NewStringLen(ctx, buf, s.len);
}

/// Create string value with explicit length
export fn JS_NewStringLen(ctx: ?*JSContext, buf: ?[*]const u8, buf_len: usize) JSValue {
    if (buf == null or buf_len == 0) {
        // Return empty string
        const allocator = if (ctx) |c| c.allocator else (global_allocator orelse return JSValue.undefined_val);
        const empty_str = string.createString(allocator, "") catch return JSValue.undefined_val;
        return JSValue.fromPtr(empty_str);
    }

    const allocator = if (ctx) |c| c.allocator else (global_allocator orelse return JSValue.undefined_val);
    const s = buf.?[0..buf_len];
    const js_str = string.createString(allocator, s) catch return JSValue.undefined_val;
    return JSValue.fromPtr(js_str);
}

/// Create new object
export fn JS_NewObject(ctx: ?*JSContext) JSValue {
    const allocator = if (ctx) |c| c.allocator else (global_allocator orelse return JSValue.undefined_val);

    // Ensure we have a root class
    if (global_root_class == null) {
        global_root_class = object.HiddenClass.init(allocator) catch return JSValue.undefined_val;
    }

    const obj = object.JSObject.create(allocator, global_root_class.?, null) catch return JSValue.undefined_val;
    return obj.toValue();
}

/// Create new array
export fn JS_NewArray(ctx: ?*JSContext, initial_len: c_int) JSValue {
    const allocator = if (ctx) |c| c.allocator else (global_allocator orelse return JSValue.undefined_val);

    // Ensure we have a root class
    if (global_root_class == null) {
        global_root_class = object.HiddenClass.init(allocator) catch return JSValue.undefined_val;
    }

    const obj = object.JSObject.createArray(allocator, global_root_class.?) catch return JSValue.undefined_val;
    obj.setArrayLength(@intCast(@max(initial_len, 0)));

    return obj.toValue();
}

/// Create object with specific class ID
export fn JS_NewObjectClassUser(ctx: ?*JSContext, class_id: c_int) JSValue {
    const obj_val = JS_NewObject(ctx);
    if (obj_val.isPtr()) {
        const obj = obj_val.toPtr(object.JSObject);
        obj.class_id = @enumFromInt(@as(u8, @intCast(class_id)));
    }
    return obj_val;
}

// ============================================================================
// Value Conversion
// ============================================================================

/// Convert to 32-bit integer
export fn JS_ToInt32(ctx: ?*JSContext, pres: ?*i32, val: JSValue) c_int {
    _ = ctx;
    if (val.isInt()) {
        if (pres) |p| p.* = val.getInt();
        return 0;
    }
    if (val.isFloat64()) {
        if (pres) |p| p.* = @intFromFloat(val.getFloat64());
        return 0;
    }
    return -1;
}

/// Convert to unsigned 32-bit integer
export fn JS_ToUint32(ctx: ?*JSContext, pres: ?*u32, val: JSValue) c_int {
    var i: i32 = 0;
    const result = JS_ToInt32(ctx, &i, val);
    if (result == 0 and pres != null) {
        pres.?.* = @bitCast(i);
    }
    return result;
}

/// Convert to number (f64)
export fn JS_ToNumber(ctx: ?*JSContext, pres: ?*f64, val: JSValue) c_int {
    _ = ctx;
    if (val.isInt()) {
        if (pres) |p| p.* = @floatFromInt(val.getInt());
        return 0;
    }
    if (val.isFloat64()) {
        if (pres) |p| p.* = val.getFloat64();
        return 0;
    }
    return -1;
}

/// Convert value to C string (returns pointer to static or allocated buffer)
export fn JS_ToCString(ctx: ?*JSContext, val: JSValue, buf: ?*JSCStringBuf) ?[*:0]const u8 {
    var len: usize = 0;
    return JS_ToCStringLen(ctx, &len, val, buf);
}

/// Convert value to C string with length
export fn JS_ToCStringLen(ctx: ?*JSContext, plen: ?*usize, val: JSValue, buf: ?*JSCStringBuf) ?[*:0]const u8 {
    _ = ctx;
    _ = buf;

    if (!val.isString()) {
        if (plen) |p| p.* = 0;
        return null;
    }

    // Get the string
    const js_str = val.toPtr(string.JSString);
    const data = js_str.data();
    if (plen) |p| p.* = data.len;

    // Return pointer to string data (null-terminated assumption - may need fix)
    return @ptrCast(data.ptr);
}

/// Convert value to string (returns JSValue string)
export fn JS_ToString(ctx: ?*JSContext, val: JSValue) JSValue {
    if (val.isString()) {
        return val; // Already a string
    }

    // Convert based on type
    if (val.isUndefined()) {
        return JS_NewString(ctx, "undefined");
    }
    if (val.isNull()) {
        return JS_NewString(ctx, "null");
    }
    if (val.isBool()) {
        return if (val.getBool()) JS_NewString(ctx, "true") else JS_NewString(ctx, "false");
    }
    if (val.isInt()) {
        const allocator = if (ctx) |c| c.allocator else (global_allocator orelse return JSValue.undefined_val);
        var buf: [32]u8 = undefined;
        const s = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return JSValue.undefined_val;
        const js_str = string.createString(allocator, s) catch return JSValue.undefined_val;
        return JSValue.fromPtr(js_str);
    }

    return JSValue.undefined_val;
}

// ============================================================================
// Type Checks
// ============================================================================

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

/// Check if value is uninitialized (not used in this implementation)
export fn JS_IsUninitialized(val: JSValue) bool {
    // We don't have an uninitialized state - treat undefined as closest
    return val.isUndefined();
}

/// Check if value is a number (int or boxed float)
export fn JS_IsNumber(ctx: ?*JSContext, val: JSValue) bool {
    _ = ctx;
    return val.isNumber();
}

/// Check if value is a string
export fn JS_IsString(ctx: ?*JSContext, val: JSValue) bool {
    _ = ctx;
    return val.isString();
}

/// Check if value is a pointer (object, array, function, etc.)
export fn JS_IsPtr(val: JSValue) bool {
    return val.isPtr();
}

/// Check if value is an error
export fn JS_IsError(ctx: ?*JSContext, val: JSValue) bool {
    _ = ctx;
    if (!val.isObject()) return false;
    const obj = val.toPtr(object.JSObject);
    return obj.class_id == .@"error";
}

/// Check if value is a function
export fn JS_IsFunction(ctx: ?*JSContext, val: JSValue) bool {
    _ = ctx;
    if (!val.isObject()) return false;
    const obj = val.toPtr(object.JSObject);
    return obj.flags.is_callable;
}

/// Get class ID of object
export fn JS_GetClassID(ctx: ?*JSContext, val: JSValue) c_int {
    _ = ctx;
    if (!val.isObject()) return -1;
    const obj = val.toPtr(object.JSObject);
    return @intFromEnum(obj.class_id);
}

// ============================================================================
// Property Access
// ============================================================================

/// Get property by string name
export fn JS_GetPropertyStr(ctx: ?*JSContext, obj_val: JSValue, prop: ?[*:0]const u8) JSValue {
    if (prop == null) return JSValue.undefined_val;
    if (!obj_val.isObject()) return JSValue.undefined_val;

    const obj = obj_val.toPtr(object.JSObject);

    // Look up atom for property name
    const allocator = if (ctx) |c| c.allocator else (global_allocator orelse return JSValue.undefined_val);
    const prop_str = std.mem.span(prop.?);

    // Try to match predefined atoms first
    const atom = lookupAtom(prop_str) orelse {
        // Dynamic atom lookup via context
        if (ctx) |c| {
            const dynamic_atom = c.atoms.intern(prop_str) catch return JSValue.undefined_val;
            if (obj.getProperty(dynamic_atom)) |v| return v;
        }
        return JSValue.undefined_val;
    };
    _ = allocator;

    if (obj.getProperty(atom)) |v| return v;
    return JSValue.undefined_val;
}

/// Get property by uint32 index
export fn JS_GetPropertyUint32(ctx: ?*JSContext, obj_val: JSValue, idx: u32) JSValue {
    _ = ctx;
    if (!obj_val.isObject()) return JSValue.undefined_val;

    const obj = obj_val.toPtr(object.JSObject);

    // For arrays, access by index (simplified - needs proper array support)
    // This is a basic implementation - real arrays need dense storage
    if (obj.class_id == .array) {
        // Create numeric atom (simplified)
        const atom: object.Atom = @enumFromInt(object.Atom.FIRST_DYNAMIC + idx);
        if (obj.getOwnProperty(atom)) |v| return v;
    }

    return JSValue.undefined_val;
}

/// Set property by string name
export fn JS_SetPropertyStr(ctx: ?*JSContext, obj_val: JSValue, prop: ?[*:0]const u8, val: JSValue) JSValue {
    if (prop == null) return JSValue.exception_val;
    if (!obj_val.isObject()) return JSValue.exception_val;

    const obj = obj_val.toPtr(object.JSObject);

    const allocator = if (ctx) |c| c.allocator else (global_allocator orelse return JSValue.exception_val);
    const prop_str = std.mem.span(prop.?);

    // Try to match predefined atoms first
    const atom = lookupAtom(prop_str) orelse blk: {
        // Dynamic atom lookup via context
        if (ctx) |c| {
            break :blk c.atoms.intern(prop_str) catch return JSValue.exception_val;
        }
        return JSValue.exception_val;
    };

    obj.setProperty(allocator, atom, val) catch return JSValue.exception_val;
    return val;
}

/// Set property by uint32 index
export fn JS_SetPropertyUint32(ctx: ?*JSContext, obj_val: JSValue, idx: u32, val: JSValue) JSValue {
    if (!obj_val.isObject()) return JSValue.exception_val;

    const obj = obj_val.toPtr(object.JSObject);

    // For arrays, set by index (simplified)
    if (obj.class_id == .array) {
        const allocator = if (ctx) |c| c.allocator else (global_allocator orelse return JSValue.exception_val);
        const atom: object.Atom = @enumFromInt(object.Atom.FIRST_DYNAMIC + idx);
        obj.setProperty(allocator, atom, val) catch return JSValue.exception_val;

        // Update length if needed
        if (obj.getOwnProperty(.length)) |len_val| {
            if (len_val.isInt()) {
                const len: u32 = @intCast(len_val.getInt());
                if (idx >= len) {
                    obj.setProperty(allocator, .length, JSValue.fromInt(@intCast(idx + 1))) catch {};
                }
            }
        }
    }

    return val;
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
// Exception Handling
// ============================================================================

/// Throw an exception
export fn JS_Throw(ctx: ?*JSContext, exception: JSValue) JSValue {
    if (ctx) |c| {
        c.throwException(exception);
    }
    return JSValue.exception_val;
}

/// Throw out of memory exception
export fn JS_ThrowOutOfMemory(ctx: ?*JSContext) JSValue {
    _ = ctx;
    return JSValue.exception_val;
}

/// Get error string
export fn JS_GetErrorStr(ctx: ?*JSContext, buf: ?[*]u8, buf_size: usize) ?[*]u8 {
    if (ctx == null or buf == null or buf_size == 0) return null;
    const c = ctx.?;

    if (!c.hasException()) {
        buf.?[0] = 0;
        return buf;
    }

    // Get exception message
    const err_str = "Error";
    const copy_len = @min(err_str.len, buf_size - 1);
    @memcpy(buf.?[0..copy_len], err_str[0..copy_len]);
    buf.?[copy_len] = 0;

    return buf;
}

// ============================================================================
// Code Execution
// ============================================================================

/// Evaluate JavaScript code
export fn JS_Eval(ctx: ?*JSContext, input: ?[*]const u8, input_len: usize, filename: ?[*:0]const u8, eval_flags: c_int) JSValue {
    _ = filename;
    _ = eval_flags;

    const c = ctx orelse return JSValue.exception_val;
    const src = input orelse return JSValue.exception_val;
    if (input_len == 0) return JSValue.undefined_val;

    const source = src[0..input_len];
    const allocator = global_allocator orelse c.allocator;

    // Create string table for interning
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    // Parse the source code
    var p = parser.Parser.init(allocator, source, &strings, null);
    defer p.deinit();

    const code = p.parse() catch |err| {
        std.log.err("Parse error: {}", .{err});
        return JSValue.exception_val;
    };

    // Create function bytecode from parser output
    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    // Run the bytecode
    var interp = interpreter.Interpreter.init(c);
    const result = interp.run(&func) catch |err| {
        std.log.err("Runtime error: {}", .{err});
        return JSValue.exception_val;
    };

    return result;
}

/// Parse JavaScript code
export fn JS_Parse(ctx: ?*JSContext, input: ?[*]const u8, input_len: usize, filename: ?[*:0]const u8, eval_flags: c_int) JSValue {
    _ = ctx;
    _ = input;
    _ = input_len;
    _ = filename;
    _ = eval_flags;
    // TODO: Implement parsing
    return JSValue.undefined_val;
}

/// Run parsed code
export fn JS_Run(ctx: ?*JSContext, val: JSValue) JSValue {
    _ = ctx;
    _ = val;
    // TODO: Implement running
    return JSValue.undefined_val;
}

/// Push argument for function call
export fn JS_PushArg(ctx: ?*JSContext, val: JSValue) void {
    if (ctx) |c| {
        c.push(val) catch {};
    }
}

/// Call a function
export fn JS_Call(ctx: ?*JSContext, call_flags: c_int) JSValue {
    _ = ctx;
    _ = call_flags;
    // TODO: Implement function calls via interpreter
    return JSValue.undefined_val;
}

// ============================================================================
// GC Reference Management
// ============================================================================

/// Push GC reference (protect value from collection)
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

/// Add GC reference (for list-based management)
export fn JS_AddGCRef(ctx: ?*JSContext, ref: ?*JSGCRef) ?*JSValue {
    return JS_PushGCRef(ctx, ref);
}

/// Delete GC reference
export fn JS_DeleteGCRef(ctx: ?*JSContext, ref: ?*JSGCRef) void {
    _ = ctx;
    _ = ref;
}

// ============================================================================
// Object Opaque Data
// ============================================================================

/// Set opaque pointer on object
export fn JS_SetOpaque(ctx: ?*JSContext, val: JSValue, opaque_ptr: ?*anyopaque) void {
    _ = ctx;
    _ = val;
    _ = opaque_ptr;
    // TODO: Store opaque in object
}

/// Get opaque pointer from object
export fn JS_GetOpaque(ctx: ?*JSContext, val: JSValue) ?*anyopaque {
    _ = ctx;
    _ = val;
    return null;
}

// ============================================================================
// Stack Management
// ============================================================================

/// Check if stack has enough space
export fn JS_StackCheck(ctx: ?*JSContext, len: u32) c_int {
    if (ctx) |c| {
        c.ensureStack(len) catch return -1;
        return 0;
    }
    return -1;
}

// ============================================================================
// Debug Functions
// ============================================================================

/// Set log function
export fn JS_SetLogFunc(ctx: ?*JSContext, write_func: ?*const fn (?*anyopaque, [*]const u8, usize) void) void {
    _ = ctx;
    _ = write_func;
}

/// Print value
export fn JS_PrintValue(ctx: ?*JSContext, val: JSValue) void {
    _ = ctx;
    _ = val;
    // TODO: Implement printing
}

// ============================================================================
// Initialization
// ============================================================================

/// Initialize the ZQuickJS C ABI layer
export fn ZQ_Init(allocator_ptr: ?*anyopaque) void {
    _ = allocator_ptr;
    global_allocator = std.heap.c_allocator;
}

/// Set custom allocator (Zig API)
pub fn setAllocator(allocator: std.mem.Allocator) void {
    global_allocator = allocator;
}

/// Cleanup global state
pub fn cleanup() void {
    if (global_root_class) |class| {
        if (global_allocator) |allocator| {
            class.deinit(allocator);
        }
        global_root_class = null;
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Look up predefined atom by name
fn lookupAtom(name: []const u8) ?object.Atom {
    // Common property names
    const atom_map = .{
        .{ "length", object.Atom.length },
        .{ "prototype", object.Atom.prototype },
        .{ "constructor", object.Atom.constructor },
        .{ "toString", object.Atom.toString },
        .{ "valueOf", object.Atom.valueOf },
        .{ "name", object.Atom.name },
        .{ "message", object.Atom.message },
        .{ "push", object.Atom.push },
        .{ "pop", object.Atom.pop },
        .{ "shift", object.Atom.shift },
        .{ "unshift", object.Atom.unshift },
        .{ "slice", object.Atom.slice },
        .{ "concat", object.Atom.concat },
        .{ "join", object.Atom.join },
        .{ "indexOf", object.Atom.indexOf },
        .{ "includes", object.Atom.includes },
        .{ "map", object.Atom.map },
        .{ "filter", object.Atom.filter },
        .{ "reduce", object.Atom.reduce },
        .{ "forEach", object.Atom.forEach },
        .{ "charAt", object.Atom.charAt },
        .{ "charCodeAt", object.Atom.charCodeAt },
        .{ "split", object.Atom.split },
        .{ "trim", object.Atom.trim },
        .{ "toLowerCase", object.Atom.toLowerCase },
        .{ "toUpperCase", object.Atom.toUpperCase },
        .{ "replace", object.Atom.replace },
        .{ "Math", object.Atom.Math },
        .{ "JSON", object.Atom.JSON },
        .{ "console", object.Atom.console },
        .{ "Object", object.Atom.Object },
        .{ "Array", object.Atom.Array },
        .{ "String", object.Atom.String },
        .{ "Number", object.Atom.Number },
        .{ "Boolean", object.Atom.Boolean },
        .{ "Function", object.Atom.Function },
        .{ "Error", object.Atom.Error },
        .{ "TypeError", object.Atom.TypeError },
        .{ "RangeError", object.Atom.RangeError },
    };

    inline for (atom_map) |entry| {
        if (std.mem.eql(u8, name, entry[0])) {
            return entry[1];
        }
    }

    return null;
}

// ============================================================================
// Tests
// ============================================================================

test "C ABI integer creation" {
    const val = JS_NewInt32(null, 42);
    try std.testing.expect(JS_IsInt(val));

    var result: i32 = 0;
    try std.testing.expectEqual(@as(c_int, 0), JS_ToInt32(null, &result, val));
    try std.testing.expectEqual(@as(i32, 42), result);
}

test "C ABI special values" {
    try std.testing.expect(JS_IsNull(JSValue.null_val));
    try std.testing.expect(JS_IsUndefined(JSValue.undefined_val));
    try std.testing.expect(JS_IsBool(JSValue.true_val));
    try std.testing.expect(JS_IsBool(JSValue.false_val));
    try std.testing.expect(JS_IsException(JSValue.exception_val));
}

test "C ABI uint32 creation" {
    const small = JS_NewUint32(null, 100);
    try std.testing.expect(JS_IsInt(small));

    var result: i32 = 0;
    _ = JS_ToInt32(null, &result, small);
    try std.testing.expectEqual(@as(i32, 100), result);
}

test "C ABI bool creation" {
    const t = JS_NewBool(1);
    const f = JS_NewBool(0);

    try std.testing.expect(JS_IsBool(t));
    try std.testing.expect(JS_IsBool(f));
    try std.testing.expect(t.getBool());
    try std.testing.expect(!f.getBool());
}

test "C ABI float creation" {
    global_allocator = std.testing.allocator;
    defer global_allocator = null;

    // Test float (boxed as Float64Box)
    const float_val = JS_NewFloat64(null, 3.14);
    defer {
        if (float_val.isPtr()) {
            const float_box = float_val.toPtr(value.JSValue.Float64Box);
            std.testing.allocator.destroy(float_box);
        }
    }
    try std.testing.expect(JS_IsNumber(null, float_val));

    var result: f64 = 0;
    _ = JS_ToNumber(null, &result, float_val);
    try std.testing.expect(@abs(result - 3.14) < 0.001);
}

test "C ABI string creation" {
    global_allocator = std.testing.allocator;
    defer global_allocator = null;

    const str_val = JS_NewString(null, "hello");
    defer {
        if (str_val.isPtr()) {
            const js_str = str_val.toPtr(string.JSString);
            string.freeString(std.testing.allocator, js_str);
        }
    }

    try std.testing.expect(JS_IsString(null, str_val));

    var len: usize = 0;
    const cstr = JS_ToCStringLen(null, &len, str_val, null);
    try std.testing.expect(cstr != null);
    try std.testing.expectEqual(@as(usize, 5), len);
}

test "C ABI object creation" {
    global_allocator = std.testing.allocator;
    defer {
        cleanup();
        global_allocator = null;
    }

    const obj = JS_NewObject(null);
    defer {
        if (obj.isPtr()) {
            const js_obj = obj.toPtr(object.JSObject);
            js_obj.destroy(std.testing.allocator);
        }
    }

    try std.testing.expect(JS_IsPtr(obj));
    try std.testing.expectEqual(@as(c_int, 0), JS_GetClassID(null, obj));
}

test "C ABI array creation" {
    global_allocator = std.testing.allocator;
    defer {
        cleanup();
        global_allocator = null;
    }

    const arr = JS_NewArray(null, 5);
    defer {
        if (arr.isPtr()) {
            const js_obj = arr.toPtr(object.JSObject);
            // Clean up hidden class if it was transitioned
            if (js_obj.hidden_class != global_root_class.?) {
                std.testing.allocator.free(js_obj.hidden_class.properties);
                js_obj.hidden_class.deinit(std.testing.allocator);
            }
            js_obj.destroy(std.testing.allocator);
        }
    }

    try std.testing.expect(JS_IsPtr(arr));
    try std.testing.expectEqual(@as(c_int, 1), JS_GetClassID(null, arr)); // array = 1
}

test "C ABI property access" {
    global_allocator = std.testing.allocator;
    defer {
        cleanup();
        global_allocator = null;
    }

    const obj = JS_NewObject(null);
    defer {
        if (obj.isPtr()) {
            const js_obj = obj.toPtr(object.JSObject);
            // Clean up hidden class if it was transitioned
            if (js_obj.hidden_class != global_root_class.?) {
                std.testing.allocator.free(js_obj.hidden_class.properties);
                js_obj.hidden_class.deinit(std.testing.allocator);
            }
            js_obj.destroy(std.testing.allocator);
        }
    }

    // Set property
    const set_result = JS_SetPropertyStr(null, obj, "length", JSValue.fromInt(42));
    try std.testing.expect(!set_result.isException());

    // Get property
    const get_result = JS_GetPropertyStr(null, obj, "length");
    try std.testing.expect(get_result.isInt());
    try std.testing.expectEqual(@as(i32, 42), get_result.getInt());
}

test "C ABI lookupAtom" {
    try std.testing.expectEqual(object.Atom.length, lookupAtom("length").?);
    try std.testing.expectEqual(object.Atom.toString, lookupAtom("toString").?);
    try std.testing.expect(lookupAtom("nonexistent") == null);
}
