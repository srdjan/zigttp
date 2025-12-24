//! Zig bindings for MicroQuickJS (mquickjs) - Bellard's embedded JS engine
//! 
//! Key differences from QuickJS:
//! - Compacting GC: object addresses can move after any allocation
//! - No JS_FreeValue: GC handles memory automatically  
//! - Must use GCRef for values that survive allocations
//! - Context takes a fixed memory buffer (no malloc)

const std = @import("std");
pub const c = @cImport({
    @cDefine("MQUICKJS_ZIG", "1");
    @cInclude("stddef.h");
    @cInclude("mquickjs.h");
    @cInclude("mqjs_stdlib_decl.h");
});

threadlocal var cstring_buf: c.JSCStringBuf = undefined;

// ============================================================================
// Core Types
// ============================================================================

/// Opaque JS value - word-sized (32 or 64 bit)
/// WARNING: Address may change after any JS allocation!
pub const JSValue = c.JSValue;

/// GC reference holder - keeps JSValue stable across allocations
pub const JSGCRef = c.JSGCRef;

/// JS execution context
pub const JSContext = c.JSContext;

/// Standard library definition (ROM-resident)
pub const JSStdLibraryDef = c.JSSTDLibraryDef;

// ============================================================================
// Result Type (Functional Error Handling)
// ============================================================================

pub fn Result(comptime T: type) type {
    return union(enum) {
        ok: T,
        err: Error,

        const Self = @This();

        pub fn map(self: Self, f: anytype) Result(@typeInfo(@TypeOf(f)).Fn.return_type.?) {
            return switch (self) {
                .ok => |v| .{ .ok = f(v) },
                .err => |e| .{ .err = e },
            };
        }

        pub fn flatMap(self: Self, f: anytype) @TypeOf(f(undefined)) {
            return switch (self) {
                .ok => |v| f(v),
                .err => |e| .{ .err = e },
            };
        }

        pub fn unwrap(self: Self) !T {
            return switch (self) {
                .ok => |v| v,
                .err => |e| e.toZigError(),
            };
        }

        pub fn unwrapOr(self: Self, default: T) T {
            return switch (self) {
                .ok => |v| v,
                .err => default,
            };
        }
    };
}

// ============================================================================
// Error Types
// ============================================================================

pub const Error = struct {
    kind: ErrorKind,
    message: ?[]const u8 = null,

    pub const ErrorKind = enum {
        exception,
        out_of_memory,
        syntax_error,
        type_error,
        reference_error,
        range_error,
        internal_error,
        invalid_context,
        gc_ref_overflow,
    };

    pub fn toZigError(self: Error) anyerror {
        return switch (self.kind) {
            .exception => error.JSException,
            .out_of_memory => error.OutOfMemory,
            .syntax_error => error.SyntaxError,
            .type_error => error.TypeError,
            .reference_error => error.ReferenceError,
            .range_error => error.RangeError,
            .internal_error => error.InternalError,
            .invalid_context => error.InvalidContext,
            .gc_ref_overflow => error.GCRefOverflow,
        };
    }
};

// ============================================================================
// Value Type Tags
// ============================================================================

pub const ValueTag = enum {
    undefined,
    null_,
    bool_,
    int,
    float,
    string,
    object,
    function,
    array,
    exception,
    symbol,
    unknown,
};

// ============================================================================+
// Standard Library (custom C function table)
// ============================================================================+

const extra_c_function_capacity: usize = 64;
var stdlib_initialized = false;
var custom_stdlib: c.JSSTDLibraryDef = undefined;
var custom_c_function_table: []c.JSCFunctionDef = &.{};
var custom_c_function_table_len: usize = 0;
var custom_c_function_table_capacity: usize = 0;

fn initStdlib() void {
    if (stdlib_initialized) return;

    const base_len: usize = @intCast(c.mqjs_c_function_table_len);
    custom_c_function_table_capacity = base_len + extra_c_function_capacity;
    custom_c_function_table = std.heap.page_allocator.alloc(c.JSCFunctionDef, custom_c_function_table_capacity) catch {
        @panic("Out of memory allocating c_function_table");
    };
    std.mem.copyForwards(c.JSCFunctionDef, custom_c_function_table[0..base_len], c.mqjs_c_function_table[0..base_len]);
    custom_c_function_table_len = base_len;

    custom_stdlib = c.mqjs_stdlib.*;
    custom_stdlib.c_function_table = custom_c_function_table.ptr;
    stdlib_initialized = true;
}

fn registerCFunction(func: c.JSCFunction, arg_count: u8) !usize {
    initStdlib();

    for (custom_c_function_table[0..custom_c_function_table_len], 0..) |entry, idx| {
        if (entry.def_type == c.JS_CFUNC_generic and entry.func.generic == func and entry.arg_count == arg_count) {
            return idx;
        }
    }

    if (custom_c_function_table_len >= custom_c_function_table_capacity) {
        return error.OutOfMemory;
    }

    custom_c_function_table[custom_c_function_table_len] = .{
        .func = .{ .generic = func },
        .name = c.JS_UNDEFINED,
        .def_type = c.JS_CFUNC_generic,
        .arg_count = arg_count,
        .magic = 0,
    };
    custom_c_function_table_len += 1;
    return custom_c_function_table_len - 1;
}

fn defaultStdlib() *const c.JSSTDLibraryDef {
    initStdlib();
    return &custom_stdlib;
}

// ============================================================================
// Context Management
// ============================================================================

/// Create a new JS context with a fixed memory buffer
/// The buffer must remain valid for the lifetime of the context
pub fn newContext(mem_buf: []u8, stdlib: ?*const JSStdLibraryDef) Result(*JSContext) {
    const stdlib_ptr = stdlib orelse defaultStdlib();
    const ctx = c.JS_NewContext(mem_buf.ptr, mem_buf.len, stdlib_ptr);
    if (ctx == null) {
        return .{ .err = .{ .kind = .out_of_memory } };
    }
    return .{ .ok = ctx.? };
}

/// Free context (only needed to call finalizers, no system memory freed)
pub fn freeContext(ctx: *JSContext) void {
    c.JS_FreeContext(ctx);
}

// ============================================================================
// GC Reference Management
// ============================================================================

/// RAII wrapper for GC references
/// Automatically pops the reference when scope exits
pub const GCRefGuard = struct {
    ctx: *JSContext,
    ref: JSGCRef,
    value_ptr: *JSValue,

    /// Get the current value (may have moved due to GC)
    pub fn get(self: *GCRefGuard) JSValue {
        return self.value_ptr.*;
    }

    /// Set a new value
    pub fn set(self: *GCRefGuard, val: JSValue) void {
        self.value_ptr.* = val;
    }

    /// Release the GC reference
    pub fn deinit(self: *GCRefGuard) void {
        c.JS_PopGCRef(self.ctx, &self.ref);
    }
};

/// Push a GC reference - returns a guard that auto-pops on scope exit
pub fn pushGCRef(ctx: *JSContext) Result(GCRefGuard) {
    var guard: GCRefGuard = undefined;
    guard.ctx = ctx;
    guard.value_ptr = c.JS_PushGCRef(ctx, &guard.ref);
    if (guard.value_ptr == null) {
        return .{ .err = .{ .kind = .gc_ref_overflow } };
    }
    return .{ .ok = guard };
}

// ============================================================================
// Value Creation
// ============================================================================

pub fn undefined_() JSValue {
    return c.JS_UNDEFINED;
}

pub fn null_() JSValue {
    return c.JS_NULL;
}

pub fn fromBool(val: bool) JSValue {
    return if (val) c.JS_TRUE else c.JS_FALSE;
}

pub fn fromInt(ctx: *JSContext, val: i32) JSValue {
    return c.JS_NewInt32(ctx, val);
}

pub fn fromFloat(ctx: *JSContext, val: f64) JSValue {
    return c.JS_NewFloat64(ctx, val);
}

pub fn fromString(ctx: *JSContext, str: []const u8) Result(JSValue) {
    const val = c.JS_NewStringLen(ctx, str.ptr, str.len);
    if (isException(val)) {
        return .{ .err = .{ .kind = .out_of_memory } };
    }
    return .{ .ok = val };
}

pub fn newObject(ctx: *JSContext) Result(JSValue) {
    const val = c.JS_NewObject(ctx);
    if (isException(val)) {
        return .{ .err = .{ .kind = .out_of_memory } };
    }
    return .{ .ok = val };
}

pub fn newArray(ctx: *JSContext) Result(JSValue) {
    const val = c.JS_NewArray(ctx, 0);
    if (isException(val)) {
        return .{ .err = .{ .kind = .out_of_memory } };
    }
    return .{ .ok = val };
}

// ============================================================================
// Value Inspection
// ============================================================================

pub fn isUndefined(val: JSValue) bool {
    return c.JS_IsUndefined(val) != 0;
}

pub fn isNull(val: JSValue) bool {
    return c.JS_IsNull(val) != 0;
}

pub fn isBool(val: JSValue) bool {
    return c.JS_IsBool(val) != 0;
}

pub fn isNumber(ctx: *JSContext, val: JSValue) bool {
    return c.JS_IsNumber(ctx, val) != 0;
}

pub fn isString(ctx: *JSContext, val: JSValue) bool {
    return c.JS_IsString(ctx, val) != 0;
}

pub fn isObject(ctx: *JSContext, val: JSValue) bool {
    return c.JS_GetClassID(ctx, val) >= 0;
}

pub fn isFunction(ctx: *JSContext, val: JSValue) bool {
    return c.JS_IsFunction(ctx, val) != 0;
}

pub fn isArray(ctx: *JSContext, val: JSValue) bool {
    return c.JS_GetClassID(ctx, val) == c.JS_CLASS_ARRAY;
}

pub fn isException(val: JSValue) bool {
    return c.JS_IsException(val) != 0;
}

pub fn getTag(ctx: *JSContext, val: JSValue) ValueTag {
    if (isUndefined(val)) return .undefined;
    if (isNull(val)) return .null_;
    if (isBool(val)) return .bool_;
    if (isString(ctx, val)) return .string;
    if (isNumber(ctx, val)) {
        // Check if it's an integer
        var i: i32 = undefined;
        if (c.JS_ToInt32(ctx, &i, val) == 0) return .int;
        return .float;
    }
    if (isException(val)) return .exception;
    if (isArray(ctx, val)) return .array;
    if (isFunction(ctx, val)) return .function;
    if (isObject(ctx, val)) return .object;
    return .unknown;
}

// ============================================================================
// Value Conversion (from JS to Zig)
// ============================================================================

pub fn toBool(ctx: *JSContext, val: JSValue) bool {
    var result: i32 = 0;
    if (c.JS_ToInt32(ctx, &result, val) == 0) {
        return result != 0;
    }
    return false;
}

pub fn toInt32(ctx: *JSContext, val: JSValue) Result(i32) {
    var result: i32 = undefined;
    if (c.JS_ToInt32(ctx, &result, val) < 0) {
        return .{ .err = .{ .kind = .type_error } };
    }
    return .{ .ok = result };
}

pub fn toInt64(ctx: *JSContext, val: JSValue) Result(i64) {
    var result: f64 = undefined;
    if (c.JS_ToNumber(ctx, &result, val) < 0) {
        return .{ .err = .{ .kind = .type_error } };
    }
    return .{ .ok = @as(i64, @intFromFloat(result)) };
}

pub fn toFloat64(ctx: *JSContext, val: JSValue) Result(f64) {
    var result: f64 = undefined;
    if (c.JS_ToNumber(ctx, &result, val) < 0) {
        return .{ .err = .{ .kind = .type_error } };
    }
    return .{ .ok = result };
}

/// Get string as a slice - valid until next GC
/// WARNING: The returned slice may become invalid after any JS allocation!
pub fn toCString(ctx: *JSContext, val: JSValue) Result([]const u8) {
    var len: usize = undefined;
    const ptr = c.JS_ToCStringLen(ctx, &len, val, &cstring_buf);
    if (ptr == null) {
        return .{ .err = .{ .kind = .type_error } };
    }
    return .{ .ok = ptr[0..len] };
}

// ============================================================================
// Object Property Access
// ============================================================================

pub fn getPropertyStr(ctx: *JSContext, obj: JSValue, prop: [:0]const u8) Result(JSValue) {
    const val = c.JS_GetPropertyStr(ctx, obj, prop.ptr);
    if (isException(val)) {
        return .{ .err = .{ .kind = .exception } };
    }
    return .{ .ok = val };
}

pub fn setPropertyStr(ctx: *JSContext, obj: JSValue, prop: [:0]const u8, val: JSValue) Result(void) {
    const res = c.JS_SetPropertyStr(ctx, obj, prop.ptr, val);
    if (isException(res)) {
        return .{ .err = .{ .kind = .exception } };
    }
    return .{ .ok = {} };
}

pub fn getPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32) Result(JSValue) {
    const val = c.JS_GetPropertyUint32(ctx, obj, idx);
    if (isException(val)) {
        return .{ .err = .{ .kind = .exception } };
    }
    return .{ .ok = val };
}

pub fn setPropertyUint32(ctx: *JSContext, obj: JSValue, idx: u32, val: JSValue) Result(void) {
    const res = c.JS_SetPropertyUint32(ctx, obj, idx, val);
    if (isException(res)) {
        return .{ .err = .{ .kind = .exception } };
    }
    return .{ .ok = {} };
}

// ============================================================================
// Code Evaluation
// ============================================================================

pub const EvalFlags = packed struct {
    return_value: bool = true,
    repl: bool = false,
    strip_col: bool = false,
    json: bool = false,
    _padding: u4 = 0,
};

fn evalFlagsToC(flags: EvalFlags) c_int {
    var result: c_int = 0;
    if (flags.return_value) result |= c.JS_EVAL_RETVAL;
    if (flags.repl) result |= c.JS_EVAL_REPL;
    if (flags.strip_col) result |= c.JS_EVAL_STRIP_COL;
    if (flags.json) result |= c.JS_EVAL_JSON;
    return result;
}

/// Evaluate JavaScript code
pub fn eval(ctx: *JSContext, code: []const u8, filename: [:0]const u8, flags: EvalFlags) Result(JSValue) {
    const val = c.JS_Eval(ctx, code.ptr, code.len, filename.ptr, evalFlagsToC(flags));
    if (isException(val)) {
        return .{ .err = extractException(ctx) };
    }
    return .{ .ok = val };
}

/// Call a JS function
pub fn call(ctx: *JSContext, func: JSValue, this: JSValue, args: []const JSValue) Result(JSValue) {
    if (c.JS_StackCheck(ctx, @intCast(args.len + 2)) != 0) {
        return .{ .err = .{ .kind = .internal_error } };
    }
    var i: usize = args.len;
    while (i > 0) {
        i -= 1;
        c.JS_PushArg(ctx, args[i]);
    }
    c.JS_PushArg(ctx, func);
    c.JS_PushArg(ctx, this);
    const val = c.JS_Call(ctx, @intCast(args.len));
    if (isException(val)) {
        return .{ .err = extractException(ctx) };
    }
    return .{ .ok = val };
}

// ============================================================================
// Exception Handling
// ============================================================================

fn extractException(ctx: *JSContext) Error {
    _ = ctx;
    return .{ .kind = .exception };
}

/// Get the current exception value (if any)
pub fn getException(_: *JSContext) JSValue {
    return c.JS_EXCEPTION;
}

/// Throw a new error
pub fn throwError(ctx: *JSContext, message: [:0]const u8) JSValue {
    return c.JS_ThrowError(ctx, c.JS_CLASS_INTERNAL_ERROR, "%s", message.ptr);
}

pub fn throwTypeError(ctx: *JSContext, message: [:0]const u8) JSValue {
    return c.JS_ThrowError(ctx, c.JS_CLASS_TYPE_ERROR, "%s", message.ptr);
}

pub fn throwRangeError(ctx: *JSContext, message: [:0]const u8) JSValue {
    return c.JS_ThrowError(ctx, c.JS_CLASS_RANGE_ERROR, "%s", message.ptr);
}

// ============================================================================
// Global Object
// ============================================================================

pub fn getGlobalObject(ctx: *JSContext) JSValue {
    return c.JS_GetGlobalObject(ctx);
}

// ============================================================================
// C Function Binding
// ============================================================================

pub const JSCFunction = c.JSCFunction;

/// Create a new C function callable from JS
pub fn newCFunction(ctx: *JSContext, func: JSCFunction, name: [:0]const u8, arg_count: c_int) Result(JSValue) {
    _ = name;
    const func_idx = registerCFunction(func, @intCast(arg_count)) catch {
        return .{ .err = .{ .kind = .out_of_memory } };
    };
    const val = c.JS_NewCFunctionParams(ctx, @intCast(func_idx), c.JS_UNDEFINED);
    if (isException(val)) {
        return .{ .err = .{ .kind = .out_of_memory } };
    }
    return .{ .ok = val };
}

/// Helper to wrap a Zig function as a C function for JS
pub fn wrapFunction(comptime zigFn: anytype) JSCFunction {
    const Wrapper = struct {
        fn call(ctx: ?*JSContext, _: [*c]JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
            const real_ctx = ctx.?;
            const args = if (argc > 0) argv[0..@intCast(argc)] else &[_]JSValue{};
            return zigFn(real_ctx, args);
        }
    };
    return Wrapper.call;
}

// ============================================================================
// Memory Statistics
// ============================================================================

pub const MemoryUsage = struct {
    malloc_size: usize,
    malloc_limit: usize,
    memory_used: usize,
};

// ============================================================================
// Stdlib C function shims (referenced by mqjs_stdlib.h)
// ============================================================================

pub export fn js_print(ctx_: ?*JSContext, _: [*c]JSValue, argc: c_int, argv: [*c]JSValue) callconv(.c) JSValue {
    const ctx = ctx_ orelse return c.JS_EXCEPTION;
    var writer = std.fs.File.stdout().deprecatedWriter();
    var i: c_int = 0;
    while (i < argc) : (i += 1) {
        if (i > 0) writer.writeAll(" ") catch {};
        const str = toCString(ctx, argv[@intCast(i)]).unwrapOr("");
        writer.writeAll(str) catch {};
    }
    writer.writeAll("\n") catch {};
    return c.JS_UNDEFINED;
}

pub export fn js_gc(ctx_: ?*JSContext, _: [*c]JSValue, _: c_int, _: [*c]JSValue) callconv(.c) JSValue {
    const ctx = ctx_ orelse return c.JS_EXCEPTION;
    c.JS_GC(ctx);
    return c.JS_UNDEFINED;
}

pub export fn js_date_now(ctx_: ?*JSContext, _: [*c]JSValue, _: c_int, _: [*c]JSValue) callconv(.c) JSValue {
    const ctx = ctx_ orelse return c.JS_EXCEPTION;
    const now_ms: i64 = std.time.milliTimestamp();
    return c.JS_NewInt64(ctx, now_ms);
}

pub export fn js_performance_now(ctx_: ?*JSContext, _: [*c]JSValue, _: c_int, _: [*c]JSValue) callconv(.c) JSValue {
    const ctx = ctx_ orelse return c.JS_EXCEPTION;
    const now_ns: i128 = std.time.nanoTimestamp();
    const now_ms: f64 = @as(f64, @floatFromInt(now_ns)) / 1_000_000.0;
    return c.JS_NewFloat64(ctx, now_ms);
}

pub export fn js_load(ctx_: ?*JSContext, _: [*c]JSValue, _: c_int, _: [*c]JSValue) callconv(.c) JSValue {
    const ctx = ctx_ orelse return c.JS_EXCEPTION;
    return c.JS_ThrowError(ctx, c.JS_CLASS_INTERNAL_ERROR, "%s", "load not implemented");
}

pub export fn js_setTimeout(_: ?*JSContext, _: [*c]JSValue, _: c_int, _: [*c]JSValue) callconv(.c) JSValue {
    return c.JS_UNDEFINED;
}

pub export fn js_clearTimeout(_: ?*JSContext, _: [*c]JSValue, _: c_int, _: [*c]JSValue) callconv(.c) JSValue {
    return c.JS_UNDEFINED;
}

pub fn getMemoryUsage(ctx: *JSContext) MemoryUsage {
    var stats: c.JSMemoryUsage = undefined;
    c.JS_ComputeMemoryUsage(ctx, &stats);
    return .{
        .malloc_size = @intCast(stats.malloc_size),
        .malloc_limit = @intCast(stats.malloc_limit),
        .memory_used = @intCast(stats.memory_used_size),
    };
}

// ============================================================================
// Bytecode (Persistent Storage)
// ============================================================================

/// Load bytecode from ROM/file
pub fn loadBytecode(ctx: *JSContext, bytecode: []const u8) Result(JSValue) {
    const val = c.JS_LoadBytecode(ctx, bytecode.ptr, bytecode.len);
    if (isException(val)) {
        return .{ .err = .{ .kind = .exception } };
    }
    return .{ .ok = val };
}

/// Run a loaded bytecode module
pub fn run(ctx: *JSContext, module: JSValue) Result(JSValue) {
    const val = c.JS_Run(ctx, module);
    if (isException(val)) {
        return .{ .err = extractException(ctx) };
    }
    return .{ .ok = val };
}

// ============================================================================
// Tests
// ============================================================================

test "create context" {
    var mem_buf: [64 * 1024]u8 = undefined;
    const ctx_result = newContext(&mem_buf, null);
    try std.testing.expect(ctx_result == .ok);
    const ctx = ctx_result.ok;
    defer freeContext(ctx);
}

test "basic evaluation" {
    var mem_buf: [64 * 1024]u8 = undefined;
    const ctx = (try newContext(&mem_buf, null).unwrap());
    defer freeContext(ctx);

    const result = eval(ctx, "1 + 2", "<test>", .{});
    try std.testing.expect(result == .ok);

    const int_result = toInt32(ctx, result.ok);
    try std.testing.expect(int_result == .ok);
    try std.testing.expectEqual(@as(i32, 3), int_result.ok);
}
