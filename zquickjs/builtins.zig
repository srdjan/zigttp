//! Built-in JavaScript objects and methods
//!
//! Standard library implementation for Object, Array, String, etc.

const std = @import("std");
const value = @import("value.zig");
const object = @import("object.zig");
const context = @import("context.zig");
const string = @import("string.zig");
const http = @import("http.zig");
const regex = @import("regex.zig");

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

/// Native function signature - matches object.NativeFn
pub const NativeFunc = *const fn (*context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue;

/// Built-in function descriptor
pub const BuiltinFunc = struct {
    name: object.Atom,
    func: object.NativeFn,
    arg_count: u8,
};

// ============================================================================
// Object methods
// ============================================================================

/// Get object from JSValue, returns null if not an object
fn getObject(val: value.JSValue) ?*object.JSObject {
    if (!val.isPtr()) return null;
    // In a full implementation, we'd check the header type
    return val.toPtr(object.JSObject);
}

/// Object.keys(obj) - Returns array of own enumerable property names
pub fn objectKeys(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const keys = obj.getOwnEnumerableKeys(ctx.allocator) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    // Create array to hold results
    // For now, return the count as an integer (proper array creation requires more infrastructure)
    return value.JSValue.fromInt(@intCast(keys.len));
}

/// Object.values(obj) - Returns array of own enumerable property values
pub fn objectValues(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const keys = obj.getOwnEnumerableKeys(ctx.allocator) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    // For now, return count (proper array creation requires more infrastructure)
    return value.JSValue.fromInt(@intCast(keys.len));
}

/// Object.entries(obj) - Returns array of [key, value] pairs
pub fn objectEntries(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const keys = obj.getOwnEnumerableKeys(ctx.allocator) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    // For now, return count
    return value.JSValue.fromInt(@intCast(keys.len));
}

/// Object.assign(target, ...sources) - Copy properties from sources to target
pub fn objectAssign(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    // Return target (in full implementation, would copy properties)
    return args[0];
}

/// Object.hasOwnProperty(prop) - Check if object has own property
pub fn objectHasOwn(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len < 2) return value.JSValue.false_val;

    const obj = getObject(args[0]) orelse return value.JSValue.false_val;

    // For string property names, we'd need to intern them first
    // For now, return false as a stub
    _ = obj;
    return value.JSValue.false_val;
}

/// Object.freeze(obj) - Freeze an object
pub fn objectFreeze(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    if (getObject(args[0])) |obj| {
        obj.preventExtensions();
        // In full implementation, would also make all properties non-writable/non-configurable
    }
    return args[0];
}

/// Object.isFrozen(obj) - Check if object is frozen
pub fn objectIsFrozen(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.true_val;

    if (getObject(args[0])) |obj| {
        return if (obj.isExtensible()) value.JSValue.false_val else value.JSValue.true_val;
    }
    return value.JSValue.true_val;
}

pub const object_methods = [_]BuiltinFunc{
    .{ .name = "keys", .func = objectKeys, .arg_count = 1 },
    .{ .name = "values", .func = objectValues, .arg_count = 1 },
    .{ .name = "entries", .func = objectEntries, .arg_count = 1 },
    .{ .name = "assign", .func = objectAssign, .arg_count = 2 },
    .{ .name = "hasOwn", .func = objectHasOwn, .arg_count = 2 },
    .{ .name = "freeze", .func = objectFreeze, .arg_count = 1 },
    .{ .name = "isFrozen", .func = objectIsFrozen, .arg_count = 1 },
};

// ============================================================================
// JSON methods
// ============================================================================

/// JSON.parse(text) - Parse JSON string to JS value
pub fn jsonParse(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    // Get the string content
    const str_val = args[0];
    if (!str_val.isString()) return value.JSValue.undefined_val;

    const js_str = str_val.toPtr(string.JSString);
    const text = js_str.data();

    // Parse the JSON
    return parseJsonValue(ctx, text) catch value.JSValue.undefined_val;
}

/// JSON.stringify(value) - Convert JS value to JSON string
pub fn jsonStringify(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const val = args[0];

    // Build JSON string
    var buffer = std.ArrayList(u8).init(ctx.allocator);
    defer buffer.deinit();

    stringifyValue(&buffer, val) catch return value.JSValue.undefined_val;

    // Create JS string from result
    const result = string.createString(ctx.allocator, buffer.items) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// Wrapper for jsonParse that converts context pointer
fn wrapJsonParse(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return jsonParse(ctx, this, args);
}

/// Wrapper for jsonStringify that converts context pointer
fn wrapJsonStringify(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return jsonStringify(ctx, this, args);
}

/// Wrappers for Object methods
fn wrapObjectKeys(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return objectKeys(ctx, this, args);
}

fn wrapObjectValues(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return objectValues(ctx, this, args);
}

fn wrapObjectEntries(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return objectEntries(ctx, this, args);
}

fn wrapObjectAssign(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return objectAssign(ctx, this, args);
}

fn wrapObjectHasOwn(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return objectHasOwn(ctx, this, args);
}

fn wrapObjectFreeze(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return objectFreeze(ctx, this, args);
}

fn wrapObjectIsFrozen(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return objectIsFrozen(ctx, this, args);
}

// ============================================================================
// Error methods
// ============================================================================

/// Create a new Error object with message
pub fn errorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "Error");
}

/// Create a new TypeError object
pub fn typeErrorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "TypeError");
}

/// Create a new RangeError object
pub fn rangeErrorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "RangeError");
}

/// Create a new SyntaxError object
pub fn syntaxErrorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "SyntaxError");
}

/// Create a new ReferenceError object
pub fn referenceErrorConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    return createErrorObject(ctx, this, args, "ReferenceError");
}

/// Helper to create error objects
fn createErrorObject(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue, error_name: []const u8) value.JSValue {
    // If called with 'new', use the provided 'this' object
    // Otherwise create a new error object
    const obj: *object.JSObject = if (this.isObject() and !this.isUndefined())
        object.JSObject.fromValue(this)
    else blk: {
        const root_class = ctx.root_class orelse return value.JSValue.undefined_val;
        break :blk object.JSObject.create(ctx.allocator, root_class, ctx.object_prototype) catch return value.JSValue.undefined_val;
    };

    // Set the 'name' property
    const name_str = string.createString(ctx.allocator, error_name) catch return value.JSValue.undefined_val;
    obj.setProperty(ctx.allocator, .name, value.JSValue.fromPtr(name_str)) catch {};

    // Set the 'message' property
    if (args.len > 0 and args[0].isString()) {
        obj.setProperty(ctx.allocator, .message, args[0]) catch {};
    } else if (args.len > 0) {
        // Convert to string
        const msg_str = valueToStringSimple(ctx.allocator, args[0]) catch {
            const empty = string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val;
            obj.setProperty(ctx.allocator, .message, value.JSValue.fromPtr(empty)) catch {};
            return obj.toValue();
        };
        obj.setProperty(ctx.allocator, .message, value.JSValue.fromPtr(msg_str)) catch {};
    } else {
        const empty = string.createString(ctx.allocator, "") catch return value.JSValue.undefined_val;
        obj.setProperty(ctx.allocator, .message, value.JSValue.fromPtr(empty)) catch {};
    }

    obj.class_id = .@"error";
    return obj.toValue();
}

/// Error.prototype.toString
pub fn errorToString(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;

    if (!this.isObject()) {
        return value.JSValue.undefined_val;
    }

    const obj = object.JSObject.fromValue(this);

    // Get name
    var name_text: []const u8 = "Error";
    if (obj.getProperty(.name)) |name_val| {
        if (name_val.isString()) {
            name_text = name_val.toPtr(string.JSString).data();
        }
    }

    // Get message
    var msg_text: []const u8 = "";
    if (obj.getProperty(.message)) |msg_val| {
        if (msg_val.isString()) {
            msg_text = msg_val.toPtr(string.JSString).data();
        }
    }

    // Format as "Name: message" or just "Name" if no message
    if (msg_text.len == 0) {
        const result = string.createString(ctx.allocator, name_text) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(result);
    }

    // Concatenate: name + ": " + message
    var buffer = std.ArrayList(u8).init(ctx.allocator);
    defer buffer.deinit();
    buffer.appendSlice(name_text) catch return value.JSValue.undefined_val;
    buffer.appendSlice(": ") catch return value.JSValue.undefined_val;
    buffer.appendSlice(msg_text) catch return value.JSValue.undefined_val;

    const result = string.createString(ctx.allocator, buffer.items) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(result);
}

/// Simple value to string conversion
fn valueToStringSimple(allocator: std.mem.Allocator, val: value.JSValue) !*string.JSString {
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

/// Wrapper for Error constructor
fn wrapErrorConstructor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return errorConstructor(ctx, this, args);
}

fn wrapTypeErrorConstructor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return typeErrorConstructor(ctx, this, args);
}

fn wrapRangeErrorConstructor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return rangeErrorConstructor(ctx, this, args);
}

fn wrapSyntaxErrorConstructor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return syntaxErrorConstructor(ctx, this, args);
}

fn wrapReferenceErrorConstructor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return referenceErrorConstructor(ctx, this, args);
}

fn wrapErrorToString(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return errorToString(ctx, this, args);
}

// ============================================================================
// Promise implementation
// ============================================================================

/// Promise state
pub const PromiseState = enum(u8) {
    pending = 0,
    fulfilled = 1,
    rejected = 2,
};

/// Promise data stored in object's extra data
pub const PromiseData = struct {
    state: PromiseState,
    result: value.JSValue,
    then_handlers: ?*object.JSObject, // Array of {onFulfilled, onRejected} pairs
    catch_handler: value.JSValue,
};

/// Promise constructor: new Promise((resolve, reject) => {...})
pub fn promiseConstructor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;

    // Create the promise object
    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;
    const promise_obj = object.JSObject.create(ctx.allocator, root_class, null) catch return value.JSValue.undefined_val;
    promise_obj.class_id = .promise;

    // Store initial state (pending)
    const state_atom = ctx.atoms.intern("__state") catch return value.JSValue.undefined_val;
    promise_obj.setProperty(ctx.allocator, state_atom, value.JSValue.fromInt(0)) catch {};

    const result_atom = ctx.atoms.intern("__result") catch return value.JSValue.undefined_val;
    promise_obj.setProperty(ctx.allocator, result_atom, value.JSValue.undefined_val) catch {};

    // If executor function provided, call it with resolve/reject
    if (args.len > 0 and args[0].isObject()) {
        // For now, immediately resolve with undefined
        // A full implementation would create resolve/reject functions and call the executor
        promise_obj.setProperty(ctx.allocator, state_atom, value.JSValue.fromInt(1)) catch {};
    }

    return promise_obj.toValue();
}

/// Promise.resolve(value) - Create fulfilled promise
pub fn promiseResolve(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;

    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;
    const promise_obj = object.JSObject.create(ctx.allocator, root_class, null) catch return value.JSValue.undefined_val;
    promise_obj.class_id = .promise;

    const state_atom = ctx.atoms.intern("__state") catch return value.JSValue.undefined_val;
    const result_atom = ctx.atoms.intern("__result") catch return value.JSValue.undefined_val;

    // Set state to fulfilled
    promise_obj.setProperty(ctx.allocator, state_atom, value.JSValue.fromInt(1)) catch {};

    // Set result
    const result_val = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    promise_obj.setProperty(ctx.allocator, result_atom, result_val) catch {};

    return promise_obj.toValue();
}

/// Promise.reject(reason) - Create rejected promise
pub fn promiseReject(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;

    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;
    const promise_obj = object.JSObject.create(ctx.allocator, root_class, null) catch return value.JSValue.undefined_val;
    promise_obj.class_id = .promise;

    const state_atom = ctx.atoms.intern("__state") catch return value.JSValue.undefined_val;
    const result_atom = ctx.atoms.intern("__result") catch return value.JSValue.undefined_val;

    // Set state to rejected
    promise_obj.setProperty(ctx.allocator, state_atom, value.JSValue.fromInt(2)) catch {};

    // Set reason
    const reason_val = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    promise_obj.setProperty(ctx.allocator, result_atom, reason_val) catch {};

    return promise_obj.toValue();
}

/// promise.then(onFulfilled, onRejected) - Add callbacks
pub fn promiseThen(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;

    const promise = object.JSObject.fromValue(this);
    if (promise.class_id != .promise) return value.JSValue.undefined_val;

    const state_atom = ctx.atoms.intern("__state") catch return value.JSValue.undefined_val;
    const result_atom = ctx.atoms.intern("__result") catch return value.JSValue.undefined_val;

    // Get current state
    const state_val = promise.getProperty(state_atom) orelse return value.JSValue.undefined_val;
    const state: PromiseState = if (state_val.isInt()) @enumFromInt(@as(u8, @intCast(state_val.getInt()))) else .pending;

    // Get result
    const result = promise.getProperty(result_atom) orelse value.JSValue.undefined_val;

    // Get callbacks
    const on_fulfilled = if (args.len > 0 and args[0].isObject()) args[0] else value.JSValue.undefined_val;
    const on_rejected = if (args.len > 1 and args[1].isObject()) args[1] else value.JSValue.undefined_val;

    // If already settled, we could call the appropriate callback
    // For this simplified implementation, return a new resolved promise
    switch (state) {
        .fulfilled => {
            if (on_fulfilled.isObject()) {
                // Would call on_fulfilled(result) and return new promise with result
                // For now, just return Promise.resolve(result)
                return promiseResolve(ctx, this, &[_]value.JSValue{result});
            }
            return promiseResolve(ctx, this, &[_]value.JSValue{result});
        },
        .rejected => {
            if (on_rejected.isObject()) {
                return promiseResolve(ctx, this, &[_]value.JSValue{result});
            }
            return promiseReject(ctx, this, &[_]value.JSValue{result});
        },
        .pending => {
            // Would queue handlers for later execution
            // For now, return a pending promise
            return promiseConstructor(ctx, this, &[_]value.JSValue{});
        },
    }
}

/// promise.catch(onRejected) - Add rejection handler (shorthand for then(undefined, onRejected))
pub fn promiseCatch(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const on_rejected = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    return promiseThen(ctx, this, &[_]value.JSValue{ value.JSValue.undefined_val, on_rejected });
}

/// Wrapper for Promise constructor
fn wrapPromiseConstructor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return promiseConstructor(ctx, this, args);
}

fn wrapPromiseResolve(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return promiseResolve(ctx, this, args);
}

fn wrapPromiseReject(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return promiseReject(ctx, this, args);
}

fn wrapPromiseThen(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return promiseThen(ctx, this, args);
}

fn wrapPromiseCatch(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return promiseCatch(ctx, this, args);
}

// ============================================================================
// Number methods
// ============================================================================

/// Number.isInteger(value) - Returns true if value is an integer
pub fn numberIsInteger(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(false);
    const val = args[0];

    // Check if it's an int32
    if (val.isInt()) return value.JSValue.fromBool(true);

    // Check if it's a float that's an integer
    if (val.isFloat()) {
        const n = val.getFloat();
        if (std.math.isNan(n) or std.math.isInf(n)) return value.JSValue.fromBool(false);
        return value.JSValue.fromBool(@floor(n) == n);
    }

    return value.JSValue.fromBool(false);
}

/// Number.isNaN(value) - Returns true if value is NaN
pub fn numberIsNaN(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(false);
    const val = args[0];

    if (!val.isFloat()) return value.JSValue.fromBool(false);
    return value.JSValue.fromBool(std.math.isNan(val.getFloat()));
}

/// Number.isFinite(value) - Returns true if value is a finite number
pub fn numberIsFinite(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(false);
    const val = args[0];

    if (val.isInt()) return value.JSValue.fromBool(true);

    if (val.isFloat()) {
        const n = val.getFloat();
        return value.JSValue.fromBool(!std.math.isNan(n) and !std.math.isInf(n));
    }

    return value.JSValue.fromBool(false);
}

/// Number.parseFloat(string) - Parse string as float
pub fn numberParseFloat(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.nan_val;
    const val = args[0];

    if (!val.isString()) return value.JSValue.nan_val;

    const str = val.toPtr(string.JSString);
    const text = str.data();

    // Skip leading whitespace
    var i: usize = 0;
    while (i < text.len and (text[i] == ' ' or text[i] == '\t' or text[i] == '\n' or text[i] == '\r')) : (i += 1) {}

    if (i >= text.len) return value.JSValue.nan_val;

    // Parse sign
    var sign: f64 = 1.0;
    if (text[i] == '-') {
        sign = -1.0;
        i += 1;
    } else if (text[i] == '+') {
        i += 1;
    }

    // Parse number
    var result: f64 = 0.0;
    var has_digits = false;

    // Integer part
    while (i < text.len and text[i] >= '0' and text[i] <= '9') {
        result = result * 10.0 + @as(f64, @floatFromInt(text[i] - '0'));
        has_digits = true;
        i += 1;
    }

    // Fractional part
    if (i < text.len and text[i] == '.') {
        i += 1;
        var frac: f64 = 0.1;
        while (i < text.len and text[i] >= '0' and text[i] <= '9') {
            result += @as(f64, @floatFromInt(text[i] - '0')) * frac;
            frac *= 0.1;
            has_digits = true;
            i += 1;
        }
    }

    if (!has_digits) return value.JSValue.nan_val;

    // Exponent part
    if (i < text.len and (text[i] == 'e' or text[i] == 'E')) {
        i += 1;
        var exp_sign: i32 = 1;
        if (i < text.len and text[i] == '-') {
            exp_sign = -1;
            i += 1;
        } else if (i < text.len and text[i] == '+') {
            i += 1;
        }

        var exp: i32 = 0;
        while (i < text.len and text[i] >= '0' and text[i] <= '9') {
            exp = exp * 10 + @as(i32, @intCast(text[i] - '0'));
            i += 1;
        }
        result *= std.math.pow(f64, 10.0, @as(f64, @floatFromInt(exp * exp_sign)));
    }

    result *= sign;
    const float_box = ctx.gc_state.allocFloat(result) catch return value.JSValue.nan_val;
    return value.JSValue.fromPtr(float_box);
}

/// Number.parseInt(string, radix) - Parse string as integer
pub fn numberParseInt(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.nan_val;
    const val = args[0];

    if (!val.isString()) return value.JSValue.nan_val;

    const str = val.toPtr(string.JSString);
    const text = str.data();

    // Get radix (default 10)
    var radix: u8 = 10;
    if (args.len > 1 and args[1].isInt()) {
        const r = args[1].getInt();
        if (r < 2 or r > 36) return value.JSValue.nan_val;
        radix = @intCast(r);
    }

    // Skip leading whitespace
    var i: usize = 0;
    while (i < text.len and (text[i] == ' ' or text[i] == '\t' or text[i] == '\n' or text[i] == '\r')) : (i += 1) {}

    if (i >= text.len) return value.JSValue.nan_val;

    // Parse sign
    var negative = false;
    if (text[i] == '-') {
        negative = true;
        i += 1;
    } else if (text[i] == '+') {
        i += 1;
    }

    // Handle 0x prefix for hex
    if (radix == 16 and i + 1 < text.len and text[i] == '0' and (text[i + 1] == 'x' or text[i + 1] == 'X')) {
        i += 2;
    } else if (radix == 10 and i + 1 < text.len and text[i] == '0' and (text[i + 1] == 'x' or text[i + 1] == 'X')) {
        radix = 16;
        i += 2;
    }

    // Parse digits
    var result: i64 = 0;
    var has_digits = false;

    while (i < text.len) {
        const c = text[i];
        var digit: ?u8 = null;

        if (c >= '0' and c <= '9') {
            digit = c - '0';
        } else if (c >= 'a' and c <= 'z') {
            digit = c - 'a' + 10;
        } else if (c >= 'A' and c <= 'Z') {
            digit = c - 'A' + 10;
        }

        if (digit == null or digit.? >= radix) break;

        result = result * radix + digit.?;
        has_digits = true;
        i += 1;
    }

    if (!has_digits) return value.JSValue.nan_val;

    if (negative) result = -result;

    // Check if it fits in i32
    if (result >= std.math.minInt(i32) and result <= std.math.maxInt(i32)) {
        return value.JSValue.fromInt(@intCast(result));
    }

    // Return as float for large values
    const float_box = ctx.gc_state.allocFloat(@floatFromInt(result)) catch return value.JSValue.nan_val;
    return value.JSValue.fromPtr(float_box);
}

/// Number.prototype.toFixed(digits) - Format number with fixed decimal places
pub fn numberToFixed(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Get the number value
    var n: f64 = undefined;
    if (this.isInt()) {
        n = @floatFromInt(this.getInt());
    } else if (this.isFloat()) {
        n = this.getFloat();
    } else {
        return value.JSValue.undefined_val;
    }

    // Get digits (default 0)
    var digits: u8 = 0;
    if (args.len > 0 and args[0].isInt()) {
        const d = args[0].getInt();
        if (d < 0 or d > 100) return value.JSValue.undefined_val;
        digits = @intCast(d);
    }

    // Format the number
    var buf: [64]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{d:.[1]}", .{ n, digits }) catch return value.JSValue.undefined_val;

    const str = string.createString(ctx.allocator, result) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(str);
}

/// Number.prototype.toString(radix) - Convert number to string
pub fn numberToString(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Get radix (default 10)
    var radix: u8 = 10;
    if (args.len > 0 and args[0].isInt()) {
        const r = args[0].getInt();
        if (r < 2 or r > 36) return value.JSValue.undefined_val;
        radix = @intCast(r);
    }

    // Get the number value
    var n: i64 = undefined;
    var is_float = false;
    var float_val: f64 = undefined;

    if (this.isInt()) {
        n = this.getInt();
    } else if (this.isFloat()) {
        float_val = this.getFloat();
        if (@floor(float_val) == float_val and float_val >= @as(f64, @floatFromInt(std.math.minInt(i64))) and float_val <= @as(f64, @floatFromInt(std.math.maxInt(i64)))) {
            n = @intFromFloat(float_val);
        } else {
            is_float = true;
        }
    } else {
        return value.JSValue.undefined_val;
    }

    // For floats or base 10, use standard formatting
    if (is_float or radix == 10) {
        var buf: [64]u8 = undefined;
        const result = if (is_float)
            std.fmt.bufPrint(&buf, "{d}", .{float_val}) catch return value.JSValue.undefined_val
        else
            std.fmt.bufPrint(&buf, "{d}", .{n}) catch return value.JSValue.undefined_val;

        const str = string.createString(ctx.allocator, result) catch return value.JSValue.undefined_val;
        return value.JSValue.fromPtr(str);
    }

    // Handle negative numbers
    var abs_n: u64 = if (n < 0) @intCast(-n) else @intCast(n);
    var buf: [65]u8 = undefined;
    var pos: usize = buf.len;

    if (abs_n == 0) {
        pos -= 1;
        buf[pos] = '0';
    } else {
        const digits = "0123456789abcdefghijklmnopqrstuvwxyz";
        while (abs_n > 0) {
            pos -= 1;
            buf[pos] = digits[@intCast(abs_n % radix)];
            abs_n /= radix;
        }
    }

    if (n < 0) {
        pos -= 1;
        buf[pos] = '-';
    }

    const str = string.createString(ctx.allocator, buf[pos..]) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(str);
}

fn wrapNumberIsInteger(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return numberIsInteger(ctx, this, args);
}

fn wrapNumberIsNaN(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return numberIsNaN(ctx, this, args);
}

fn wrapNumberIsFinite(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return numberIsFinite(ctx, this, args);
}

fn wrapNumberParseFloat(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return numberParseFloat(ctx, this, args);
}

fn wrapNumberParseInt(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return numberParseInt(ctx, this, args);
}

fn wrapNumberToFixed(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return numberToFixed(ctx, this, args);
}

fn wrapNumberToString(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return numberToString(ctx, this, args);
}

/// Global isNaN - coerces argument to number first (unlike Number.isNaN)
pub fn globalIsNaN(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(true); // isNaN(undefined) = true

    const val = args[0];

    // Try to get as number
    if (val.isInt()) return value.JSValue.fromBool(false);
    if (val.isFloat()) return value.JSValue.fromBool(std.math.isNan(val.getFloat()));
    if (val.isNull()) return value.JSValue.fromBool(false); // null coerces to 0
    if (val.isUndefined()) return value.JSValue.fromBool(true);
    if (val.isBool()) return value.JSValue.fromBool(false); // true->1, false->0

    // Strings and objects would need toNumber coercion - for now return true
    return value.JSValue.fromBool(true);
}

/// Global isFinite - coerces argument to number first (unlike Number.isFinite)
pub fn globalIsFinite(_: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.fromBool(false); // isFinite(undefined) = false

    const val = args[0];

    if (val.isInt()) return value.JSValue.fromBool(true);
    if (val.isFloat()) {
        const n = val.getFloat();
        return value.JSValue.fromBool(!std.math.isNan(n) and !std.math.isInf(n));
    }
    if (val.isNull()) return value.JSValue.fromBool(true); // null coerces to 0
    if (val.isUndefined()) return value.JSValue.fromBool(false);
    if (val.isBool()) return value.JSValue.fromBool(true); // true->1, false->0

    return value.JSValue.fromBool(false);
}

fn wrapGlobalIsNaN(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return globalIsNaN(ctx, this, args);
}

fn wrapGlobalIsFinite(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return globalIsFinite(ctx, this, args);
}

// ============================================================================
// Map implementation
// ============================================================================

/// Map constructor - new Map() or new Map(iterable)
pub fn mapConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;

    // Create the Map object
    const map_obj = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;

    // Internal storage: _keys and _values arrays
    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.undefined_val;
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    // Create internal arrays
    const keys_arr = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;
    keys_arr.setProperty(allocator, .length, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    const values_arr = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;
    values_arr.setProperty(allocator, .length, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    map_obj.setProperty(allocator, keys_atom, keys_arr.toValue()) catch return value.JSValue.undefined_val;
    map_obj.setProperty(allocator, values_atom, values_arr.toValue()) catch return value.JSValue.undefined_val;
    map_obj.setProperty(allocator, size_atom, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    // If iterable provided, add entries
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Handle iterable initialization
    }

    return map_obj.toValue();
}

/// Map.prototype.set(key, value)
pub fn mapSet(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 2) return this;

    const allocator = ctx.allocator;
    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];
    const val = args[1];

    const keys_atom = ctx.atoms.intern("_keys") catch return this;
    const values_atom = ctx.atoms.intern("_values") catch return this;
    const size_atom = ctx.atoms.intern("size") catch return this;

    const keys_val = map_obj.getProperty(keys_atom) orelse return this;
    const values_val = map_obj.getProperty(values_atom) orelse return this;

    if (!keys_val.isObject() or !values_val.isObject()) return this;

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    const len_val = keys_arr.getProperty(.length) orelse value.JSValue.fromInt(0);
    const len = if (len_val.isInt()) len_val.getInt() else 0;

    // Check if key already exists
    var i: i32 = 0;
    while (i < len) : (i += 1) {
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)));
        if (keys_arr.getProperty(idx_atom)) |existing_key| {
            if (value.strictEqual(existing_key, key)) {
                // Update existing value
                values_arr.setProperty(allocator, idx_atom, val) catch return this;
                return this;
            }
        }
    }

    // Add new entry
    const new_idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(len)));
    keys_arr.setProperty(allocator, new_idx_atom, key) catch return this;
    values_arr.setProperty(allocator, new_idx_atom, val) catch return this;
    keys_arr.setProperty(allocator, .length, value.JSValue.fromInt(len + 1)) catch return this;
    values_arr.setProperty(allocator, .length, value.JSValue.fromInt(len + 1)) catch return this;
    map_obj.setProperty(allocator, size_atom, value.JSValue.fromInt(len + 1)) catch return this;

    return this;
}

/// Map.prototype.get(key)
pub fn mapGet(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.undefined_val;

    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.undefined_val;
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;

    const keys_val = map_obj.getProperty(keys_atom) orelse return value.JSValue.undefined_val;
    const values_val = map_obj.getProperty(values_atom) orelse return value.JSValue.undefined_val;

    if (!keys_val.isObject() or !values_val.isObject()) return value.JSValue.undefined_val;

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    const len_val = keys_arr.getProperty(.length) orelse value.JSValue.fromInt(0);
    const len = if (len_val.isInt()) len_val.getInt() else 0;

    var i: i32 = 0;
    while (i < len) : (i += 1) {
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)));
        if (keys_arr.getProperty(idx_atom)) |existing_key| {
            if (value.strictEqual(existing_key, key)) {
                return values_arr.getProperty(idx_atom) orelse value.JSValue.undefined_val;
            }
        }
    }

    return value.JSValue.undefined_val;
}

/// Map.prototype.has(key)
pub fn mapHas(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.fromBool(false);
    const keys_val = map_obj.getProperty(keys_atom) orelse return value.JSValue.fromBool(false);

    if (!keys_val.isObject()) return value.JSValue.fromBool(false);

    const keys_arr = object.JSObject.fromValue(keys_val);
    const len_val = keys_arr.getProperty(.length) orelse value.JSValue.fromInt(0);
    const len = if (len_val.isInt()) len_val.getInt() else 0;

    var i: i32 = 0;
    while (i < len) : (i += 1) {
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)));
        if (keys_arr.getProperty(idx_atom)) |existing_key| {
            if (value.strictEqual(existing_key, key)) {
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Map.prototype.delete(key)
pub fn mapDelete(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const allocator = ctx.allocator;
    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.fromBool(false);
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.fromBool(false);
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.fromBool(false);

    const keys_val = map_obj.getProperty(keys_atom) orelse return value.JSValue.fromBool(false);
    const values_val = map_obj.getProperty(values_atom) orelse return value.JSValue.fromBool(false);

    if (!keys_val.isObject() or !values_val.isObject()) return value.JSValue.fromBool(false);

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    const len_val = keys_arr.getProperty(.length) orelse value.JSValue.fromInt(0);
    const len = if (len_val.isInt()) len_val.getInt() else 0;

    var i: i32 = 0;
    while (i < len) : (i += 1) {
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)));
        if (keys_arr.getProperty(idx_atom)) |existing_key| {
            if (value.strictEqual(existing_key, key)) {
                // Shift remaining elements
                var j = i;
                while (j < len - 1) : (j += 1) {
                    const curr_atom: object.Atom = @enumFromInt(@as(u32, @intCast(j)));
                    const next_atom: object.Atom = @enumFromInt(@as(u32, @intCast(j + 1)));
                    if (keys_arr.getProperty(next_atom)) |next_key| {
                        keys_arr.setProperty(allocator, curr_atom, next_key) catch {};
                    }
                    if (values_arr.getProperty(next_atom)) |next_val| {
                        values_arr.setProperty(allocator, curr_atom, next_val) catch {};
                    }
                }
                keys_arr.setProperty(allocator, .length, value.JSValue.fromInt(len - 1)) catch {};
                values_arr.setProperty(allocator, .length, value.JSValue.fromInt(len - 1)) catch {};
                map_obj.setProperty(allocator, size_atom, value.JSValue.fromInt(len - 1)) catch {};
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Map.prototype.clear()
pub fn mapClear(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;

    const allocator = ctx.allocator;
    const map_obj = object.JSObject.fromValue(this);

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.undefined_val;
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    const keys_val = map_obj.getProperty(keys_atom) orelse return value.JSValue.undefined_val;
    const values_val = map_obj.getProperty(values_atom) orelse return value.JSValue.undefined_val;

    if (!keys_val.isObject() or !values_val.isObject()) return value.JSValue.undefined_val;

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    keys_arr.setProperty(allocator, .length, value.JSValue.fromInt(0)) catch {};
    values_arr.setProperty(allocator, .length, value.JSValue.fromInt(0)) catch {};
    map_obj.setProperty(allocator, size_atom, value.JSValue.fromInt(0)) catch {};

    return value.JSValue.undefined_val;
}

fn wrapMapConstructor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mapConstructor(ctx, this, args);
}

fn wrapMapSet(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mapSet(ctx, this, args);
}

fn wrapMapGet(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mapGet(ctx, this, args);
}

fn wrapMapHas(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mapHas(ctx, this, args);
}

fn wrapMapDelete(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mapDelete(ctx, this, args);
}

fn wrapMapClear(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mapClear(ctx, this, args);
}

// ============================================================================
// Set implementation
// ============================================================================

/// Set constructor - new Set() or new Set(iterable)
pub fn setConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;

    // Create the Set object
    const set_obj = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;

    // Internal storage: _values array
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    // Create internal array
    const values_arr = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;
    values_arr.setProperty(allocator, .length, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    set_obj.setProperty(allocator, values_atom, values_arr.toValue()) catch return value.JSValue.undefined_val;
    set_obj.setProperty(allocator, size_atom, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    // If iterable provided, add values
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Handle iterable initialization
    }

    return set_obj.toValue();
}

/// Set.prototype.add(value)
pub fn setAdd(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return this;

    const allocator = ctx.allocator;
    const set_obj = object.JSObject.fromValue(this);
    const val = args[0];

    const values_atom = ctx.atoms.intern("_values") catch return this;
    const size_atom = ctx.atoms.intern("size") catch return this;

    const values_val = set_obj.getProperty(values_atom) orelse return this;
    if (!values_val.isObject()) return this;

    const values_arr = object.JSObject.fromValue(values_val);
    const len_val = values_arr.getProperty(.length) orelse value.JSValue.fromInt(0);
    const len = if (len_val.isInt()) len_val.getInt() else 0;

    // Check if value already exists
    var i: i32 = 0;
    while (i < len) : (i += 1) {
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)));
        if (values_arr.getProperty(idx_atom)) |existing| {
            if (value.strictEqual(existing, val)) {
                return this; // Already exists
            }
        }
    }

    // Add new value
    const new_idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(len)));
    values_arr.setProperty(allocator, new_idx_atom, val) catch return this;
    values_arr.setProperty(allocator, .length, value.JSValue.fromInt(len + 1)) catch return this;
    set_obj.setProperty(allocator, size_atom, value.JSValue.fromInt(len + 1)) catch return this;

    return this;
}

/// Set.prototype.has(value)
pub fn setHas(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const set_obj = object.JSObject.fromValue(this);
    const val = args[0];

    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.fromBool(false);
    const values_val = set_obj.getProperty(values_atom) orelse return value.JSValue.fromBool(false);

    if (!values_val.isObject()) return value.JSValue.fromBool(false);

    const values_arr = object.JSObject.fromValue(values_val);
    const len_val = values_arr.getProperty(.length) orelse value.JSValue.fromInt(0);
    const len = if (len_val.isInt()) len_val.getInt() else 0;

    var i: i32 = 0;
    while (i < len) : (i += 1) {
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)));
        if (values_arr.getProperty(idx_atom)) |existing| {
            if (value.strictEqual(existing, val)) {
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Set.prototype.delete(value)
pub fn setDelete(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const allocator = ctx.allocator;
    const set_obj = object.JSObject.fromValue(this);
    const val = args[0];

    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.fromBool(false);
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.fromBool(false);

    const values_val = set_obj.getProperty(values_atom) orelse return value.JSValue.fromBool(false);
    if (!values_val.isObject()) return value.JSValue.fromBool(false);

    const values_arr = object.JSObject.fromValue(values_val);
    const len_val = values_arr.getProperty(.length) orelse value.JSValue.fromInt(0);
    const len = if (len_val.isInt()) len_val.getInt() else 0;

    var i: i32 = 0;
    while (i < len) : (i += 1) {
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)));
        if (values_arr.getProperty(idx_atom)) |existing| {
            if (value.strictEqual(existing, val)) {
                // Shift remaining elements
                var j = i;
                while (j < len - 1) : (j += 1) {
                    const curr_atom: object.Atom = @enumFromInt(@as(u32, @intCast(j)));
                    const next_atom: object.Atom = @enumFromInt(@as(u32, @intCast(j + 1)));
                    if (values_arr.getProperty(next_atom)) |next_val| {
                        values_arr.setProperty(allocator, curr_atom, next_val) catch {};
                    }
                }
                values_arr.setProperty(allocator, .length, value.JSValue.fromInt(len - 1)) catch {};
                set_obj.setProperty(allocator, size_atom, value.JSValue.fromInt(len - 1)) catch {};
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Set.prototype.clear()
pub fn setClear(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;

    const allocator = ctx.allocator;
    const set_obj = object.JSObject.fromValue(this);

    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    const values_val = set_obj.getProperty(values_atom) orelse return value.JSValue.undefined_val;
    if (!values_val.isObject()) return value.JSValue.undefined_val;

    const values_arr = object.JSObject.fromValue(values_val);
    values_arr.setProperty(allocator, .length, value.JSValue.fromInt(0)) catch {};
    set_obj.setProperty(allocator, size_atom, value.JSValue.fromInt(0)) catch {};

    return value.JSValue.undefined_val;
}

fn wrapSetConstructor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return setConstructor(ctx, this, args);
}

fn wrapSetAdd(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return setAdd(ctx, this, args);
}

fn wrapSetHas(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return setHas(ctx, this, args);
}

fn wrapSetDelete(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return setDelete(ctx, this, args);
}

fn wrapSetClear(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return setClear(ctx, this, args);
}

/// Parse a JSON value from text
fn parseJsonValue(ctx: *context.Context, text: []const u8) !value.JSValue {
    var pos: usize = 0;
    return parseJsonValueAt(ctx, text, &pos);
}

/// Parse JSON value at position
fn parseJsonValueAt(ctx: *context.Context, text: []const u8, pos: *usize) !value.JSValue {
    // Skip whitespace
    while (pos.* < text.len and (text[pos.*] == ' ' or text[pos.*] == '\n' or text[pos.*] == '\r' or text[pos.*] == '\t')) {
        pos.* += 1;
    }

    if (pos.* >= text.len) return error.UnexpectedEof;

    const c = text[pos.*];

    // Object
    if (c == '{') {
        return parseJsonObject(ctx, text, pos);
    }

    // Array
    if (c == '[') {
        return parseJsonArray(ctx, text, pos);
    }

    // String
    if (c == '"') {
        return parseJsonString(ctx, text, pos);
    }

    // Number
    if (c == '-' or (c >= '0' and c <= '9')) {
        return parseJsonNumber(text, pos);
    }

    // true
    if (text.len >= pos.* + 4 and std.mem.eql(u8, text[pos.*..][0..4], "true")) {
        pos.* += 4;
        return value.JSValue.true_val;
    }

    // false
    if (text.len >= pos.* + 5 and std.mem.eql(u8, text[pos.*..][0..5], "false")) {
        pos.* += 5;
        return value.JSValue.false_val;
    }

    // null
    if (text.len >= pos.* + 4 and std.mem.eql(u8, text[pos.*..][0..4], "null")) {
        pos.* += 4;
        return value.JSValue.null_val;
    }

    return error.InvalidJson;
}

/// Parse JSON object
fn parseJsonObject(ctx: *context.Context, text: []const u8, pos: *usize) !value.JSValue {
    pos.* += 1; // skip '{'

    const root_class = ctx.root_class orelse return error.NoRootClass;
    const obj = try object.JSObject.create(ctx.allocator, root_class, null);

    // Skip whitespace
    while (pos.* < text.len and (text[pos.*] == ' ' or text[pos.*] == '\n' or text[pos.*] == '\r' or text[pos.*] == '\t')) {
        pos.* += 1;
    }

    if (pos.* < text.len and text[pos.*] == '}') {
        pos.* += 1;
        return obj.toValue();
    }

    while (pos.* < text.len) {
        // Skip whitespace
        while (pos.* < text.len and (text[pos.*] == ' ' or text[pos.*] == '\n' or text[pos.*] == '\r' or text[pos.*] == '\t')) {
            pos.* += 1;
        }

        // Parse key (must be string)
        if (pos.* >= text.len or text[pos.*] != '"') return error.InvalidJson;
        const key_val = try parseJsonString(ctx, text, pos);
        const key_str = key_val.toPtr(string.JSString);

        // Skip whitespace
        while (pos.* < text.len and (text[pos.*] == ' ' or text[pos.*] == '\n' or text[pos.*] == '\r' or text[pos.*] == '\t')) {
            pos.* += 1;
        }

        // Expect ':'
        if (pos.* >= text.len or text[pos.*] != ':') return error.InvalidJson;
        pos.* += 1;

        // Parse value
        const val = try parseJsonValueAt(ctx, text, pos);

        // Store in object using dynamic atom
        const atom = try ctx.atoms.intern(key_str.data());
        try obj.setProperty(ctx.allocator, atom, val);

        // Skip whitespace
        while (pos.* < text.len and (text[pos.*] == ' ' or text[pos.*] == '\n' or text[pos.*] == '\r' or text[pos.*] == '\t')) {
            pos.* += 1;
        }

        // Check for ',' or '}'
        if (pos.* >= text.len) return error.InvalidJson;
        if (text[pos.*] == '}') {
            pos.* += 1;
            return obj.toValue();
        }
        if (text[pos.*] == ',') {
            pos.* += 1;
            continue;
        }
        return error.InvalidJson;
    }

    return error.InvalidJson;
}

/// Parse JSON array
fn parseJsonArray(ctx: *context.Context, text: []const u8, pos: *usize) !value.JSValue {
    pos.* += 1; // skip '['

    const root_class = ctx.root_class orelse return error.NoRootClass;
    const arr = try object.JSObject.create(ctx.allocator, root_class, ctx.array_prototype);
    arr.class_id = .array;

    // Skip whitespace
    while (pos.* < text.len and (text[pos.*] == ' ' or text[pos.*] == '\n' or text[pos.*] == '\r' or text[pos.*] == '\t')) {
        pos.* += 1;
    }

    if (pos.* < text.len and text[pos.*] == ']') {
        pos.* += 1;
        try arr.setProperty(ctx.allocator, .length, value.JSValue.fromInt(0));
        return arr.toValue();
    }

    var index: i32 = 0;
    while (pos.* < text.len) {
        // Parse element
        const elem = try parseJsonValueAt(ctx, text, pos);
        arr.setSlot(@intCast(index), elem);
        index += 1;

        // Skip whitespace
        while (pos.* < text.len and (text[pos.*] == ' ' or text[pos.*] == '\n' or text[pos.*] == '\r' or text[pos.*] == '\t')) {
            pos.* += 1;
        }

        // Check for ',' or ']'
        if (pos.* >= text.len) return error.InvalidJson;
        if (text[pos.*] == ']') {
            pos.* += 1;
            try arr.setProperty(ctx.allocator, .length, value.JSValue.fromInt(index));
            return arr.toValue();
        }
        if (text[pos.*] == ',') {
            pos.* += 1;
            continue;
        }
        return error.InvalidJson;
    }

    return error.InvalidJson;
}

/// Parse JSON string
fn parseJsonString(ctx: *context.Context, text: []const u8, pos: *usize) !value.JSValue {
    pos.* += 1; // skip opening '"'

    var buffer = std.ArrayList(u8).init(ctx.allocator);
    defer buffer.deinit();

    while (pos.* < text.len) {
        const c = text[pos.*];
        if (c == '"') {
            pos.* += 1;
            const result = try string.createString(ctx.allocator, buffer.items);
            return value.JSValue.fromPtr(result);
        }
        if (c == '\\') {
            pos.* += 1;
            if (pos.* >= text.len) return error.InvalidJson;
            const escaped = text[pos.*];
            pos.* += 1;
            switch (escaped) {
                '"' => try buffer.append('"'),
                '\\' => try buffer.append('\\'),
                '/' => try buffer.append('/'),
                'n' => try buffer.append('\n'),
                'r' => try buffer.append('\r'),
                't' => try buffer.append('\t'),
                'b' => try buffer.append(0x08),
                'f' => try buffer.append(0x0C),
                'u' => {
                    // Unicode escape \uXXXX
                    if (pos.* + 4 > text.len) return error.InvalidJson;
                    const hex = text[pos.*..][0..4];
                    pos.* += 4;
                    const code = std.fmt.parseInt(u16, hex, 16) catch return error.InvalidJson;
                    // Simple ASCII handling for now
                    if (code < 128) {
                        try buffer.append(@intCast(code));
                    } else {
                        // UTF-8 encoding for BMP characters
                        if (code < 0x80) {
                            try buffer.append(@intCast(code));
                        } else if (code < 0x800) {
                            try buffer.append(@intCast(0xC0 | (code >> 6)));
                            try buffer.append(@intCast(0x80 | (code & 0x3F)));
                        } else {
                            try buffer.append(@intCast(0xE0 | (code >> 12)));
                            try buffer.append(@intCast(0x80 | ((code >> 6) & 0x3F)));
                            try buffer.append(@intCast(0x80 | (code & 0x3F)));
                        }
                    }
                },
                else => try buffer.append(escaped),
            }
        } else {
            try buffer.append(c);
            pos.* += 1;
        }
    }

    return error.InvalidJson;
}

/// Parse JSON number
fn parseJsonNumber(text: []const u8, pos: *usize) !value.JSValue {
    const start = pos.*;

    // Optional minus
    if (pos.* < text.len and text[pos.*] == '-') {
        pos.* += 1;
    }

    // Integer part
    if (pos.* < text.len and text[pos.*] == '0') {
        pos.* += 1;
    } else {
        while (pos.* < text.len and text[pos.*] >= '0' and text[pos.*] <= '9') {
            pos.* += 1;
        }
    }

    // Fractional part
    var is_float = false;
    if (pos.* < text.len and text[pos.*] == '.') {
        is_float = true;
        pos.* += 1;
        while (pos.* < text.len and text[pos.*] >= '0' and text[pos.*] <= '9') {
            pos.* += 1;
        }
    }

    // Exponent part
    if (pos.* < text.len and (text[pos.*] == 'e' or text[pos.*] == 'E')) {
        is_float = true;
        pos.* += 1;
        if (pos.* < text.len and (text[pos.*] == '+' or text[pos.*] == '-')) {
            pos.* += 1;
        }
        while (pos.* < text.len and text[pos.*] >= '0' and text[pos.*] <= '9') {
            pos.* += 1;
        }
    }

    const num_str = text[start..pos.*];

    if (is_float) {
        const f = std.fmt.parseFloat(f64, num_str) catch return error.InvalidJson;
        // For now, truncate to int if it fits
        if (f == @trunc(f) and f >= -2147483648 and f <= 2147483647) {
            return value.JSValue.fromInt(@intFromFloat(f));
        }
        // Would need heap allocation for float - return as int truncated
        return value.JSValue.fromInt(@intFromFloat(@trunc(f)));
    } else {
        const i = std.fmt.parseInt(i32, num_str, 10) catch {
            // Try as float if integer overflow
            const f = std.fmt.parseFloat(f64, num_str) catch return error.InvalidJson;
            return value.JSValue.fromInt(@intFromFloat(@trunc(f)));
        };
        return value.JSValue.fromInt(i);
    }
}

/// Stringify a JS value to JSON
fn stringifyValue(buffer: *std.ArrayList(u8), val: value.JSValue) !void {
    if (val.isNull()) {
        try buffer.appendSlice("null");
        return;
    }

    if (val.isUndefined()) {
        try buffer.appendSlice("null"); // undefined becomes null in JSON
        return;
    }

    if (val.isTrue()) {
        try buffer.appendSlice("true");
        return;
    }

    if (val.isFalse()) {
        try buffer.appendSlice("false");
        return;
    }

    if (val.isInt()) {
        var num_buf: [32]u8 = undefined;
        const slice = std.fmt.bufPrint(&num_buf, "{d}", .{val.getInt()}) catch return error.OutOfMemory;
        try buffer.appendSlice(slice);
        return;
    }

    if (val.isString()) {
        const str = val.toPtr(string.JSString);
        try buffer.append('"');
        for (str.data()) |c| {
            switch (c) {
                '"' => try buffer.appendSlice("\\\""),
                '\\' => try buffer.appendSlice("\\\\"),
                '\n' => try buffer.appendSlice("\\n"),
                '\r' => try buffer.appendSlice("\\r"),
                '\t' => try buffer.appendSlice("\\t"),
                0x08 => try buffer.appendSlice("\\b"),
                0x0C => try buffer.appendSlice("\\f"),
                else => {
                    if (c < 0x20) {
                        // Control characters
                        try buffer.appendSlice("\\u00");
                        const hex = "0123456789abcdef";
                        try buffer.append(hex[c >> 4]);
                        try buffer.append(hex[c & 0xF]);
                    } else {
                        try buffer.append(c);
                    }
                },
            }
        }
        try buffer.append('"');
        return;
    }

    if (val.isObject()) {
        const obj = object.JSObject.fromValue(val);

        // Check if it's an array
        if (obj.class_id == .array) {
            try buffer.append('[');
            const len = getArrayLength(obj);
            var i: i32 = 0;
            while (i < len) : (i += 1) {
                if (i > 0) try buffer.append(',');
                const elem = obj.getSlot(@intCast(i));
                try stringifyValue(buffer, elem);
            }
            try buffer.append(']');
            return;
        }

        // Regular object
        try buffer.append('{');
        var first = true;

        // Get property slots
        const hidden_class = obj.hidden_class;
        for (hidden_class.transitions.items) |transition| {
            if (!first) try buffer.append(',');
            first = false;

            // Get property name from atom (simplified - just use index)
            var atom_buf: [32]u8 = undefined;
            const atom_name = std.fmt.bufPrint(&atom_buf, "{d}", .{@intFromEnum(transition.atom)}) catch continue;
            try buffer.append('"');
            try buffer.appendSlice(atom_name);
            try buffer.append('"');
            try buffer.append(':');

            if (obj.getProperty(transition.atom)) |prop_val| {
                try stringifyValue(buffer, prop_val);
            } else {
                try buffer.appendSlice("null");
            }
        }
        try buffer.append('}');
        return;
    }

    // Unknown type - output null
    try buffer.appendSlice("null");
}

// ============================================================================
// Array methods
// ============================================================================

/// Get array length from object
fn getArrayLength(obj: *object.JSObject) i32 {
    if (obj.getOwnProperty(.length)) |len_val| {
        if (len_val.isInt()) return len_val.getInt();
    }
    return 0;
}

/// Set array length on object
fn setArrayLength(obj: *object.JSObject, allocator: std.mem.Allocator, len: i32) void {
    obj.setProperty(allocator, .length, value.JSValue.fromInt(len)) catch {};
}

/// Array.isArray(value) - Check if value is an array
pub fn arrayIsArray(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    if (args.len == 0) return value.JSValue.false_val;

    if (getObject(args[0])) |obj| {
        return if (obj.class_id == .array) value.JSValue.true_val else value.JSValue.false_val;
    }
    return value.JSValue.false_val;
}

/// Array.from(arrayLike, mapFn?, thisArg?) - Create array from array-like or iterable
pub fn arrayFrom(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (args.len == 0) return value.JSValue.undefined_val;

    const source = args[0];
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;

    // Create result array
    const result = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;
    result.class_id = .array;

    // Handle string - convert to array of characters
    if (source.isString()) {
        const str = source.toPtr(string.JSString);
        const data = str.data();
        var idx: i32 = 0;

        var i: usize = 0;
        while (i < data.len) {
            const char_len = string.utf8CodepointByteLen(data[i]);
            const char_slice = data[i..@min(i + char_len, data.len)];
            const char_str = string.createString(allocator, char_slice) catch return result.toValue();
            const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(idx)));
            result.setProperty(allocator, idx_atom, value.JSValue.fromPtr(char_str)) catch {};
            idx += 1;
            i += char_len;
        }
        result.setProperty(allocator, .length, value.JSValue.fromInt(idx)) catch {};
        return result.toValue();
    }

    // Handle array-like object with length property
    if (source.isObject()) {
        const src_obj = object.JSObject.fromValue(source);
        if (src_obj.getProperty(.length)) |len_val| {
            if (len_val.isInt()) {
                const len = len_val.getInt();
                var idx: i32 = 0;
                while (idx < len) : (idx += 1) {
                    const src_atom: object.Atom = @enumFromInt(@as(u32, @intCast(idx)));
                    const elem = src_obj.getProperty(src_atom) orelse value.JSValue.undefined_val;
                    result.setProperty(allocator, src_atom, elem) catch {};
                }
                result.setProperty(allocator, .length, value.JSValue.fromInt(len)) catch {};
                return result.toValue();
            }
        }
    }

    result.setProperty(allocator, .length, value.JSValue.fromInt(0)) catch {};
    return result.toValue();
}

/// Array.of(...elements) - Create array from arguments
pub fn arrayOf(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;

    const result = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;
    result.class_id = .array;

    for (args, 0..) |arg, i| {
        const idx_atom: object.Atom = @enumFromInt(@as(u32, @intCast(i)));
        result.setProperty(allocator, idx_atom, arg) catch {};
    }
    result.setProperty(allocator, .length, value.JSValue.fromInt(@intCast(args.len))) catch {};
    return result.toValue();
}

fn wrapArrayFrom(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayFrom(ctx, this, args);
}

fn wrapArrayOf(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayOf(ctx, this, args);
}

/// Array.prototype.push(...items) - Add elements to end, return new length
pub fn arrayPush(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    var len = getArrayLength(obj);

    // Add each argument
    for (args) |arg| {
        // Set element at index
        const idx_atom = ctx.atoms.intern(std.fmt.allocPrint(ctx.allocator, "{d}", .{len}) catch return value.JSValue.undefined_val) catch return value.JSValue.undefined_val;
        obj.setProperty(ctx.allocator, idx_atom, arg) catch return value.JSValue.undefined_val;
        len += 1;
    }

    setArrayLength(obj, ctx.allocator, len);
    return value.JSValue.fromInt(len);
}

/// Array.prototype.pop() - Remove and return last element
pub fn arrayPop(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    var len = getArrayLength(obj);

    if (len <= 0) return value.JSValue.undefined_val;

    len -= 1;
    // Get last element (in full impl, would use numeric index)
    const idx_atom = ctx.atoms.intern(std.fmt.allocPrint(ctx.allocator, "{d}", .{len}) catch return value.JSValue.undefined_val) catch return value.JSValue.undefined_val;
    const result = obj.getOwnProperty(idx_atom) orelse value.JSValue.undefined_val;

    // Delete and update length
    _ = obj.deleteProperty(idx_atom);
    setArrayLength(obj, ctx.allocator, len);

    return result;
}

/// Array.prototype.shift() - Remove and return first element
pub fn arrayShift(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = args;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj);

    if (len <= 0) return value.JSValue.undefined_val;

    // Get first element
    const zero_atom = ctx.atoms.intern("0") catch return value.JSValue.undefined_val;
    const result = obj.getOwnProperty(zero_atom) orelse value.JSValue.undefined_val;

    // Shift all elements down (simplified - full impl would iterate)
    setArrayLength(obj, ctx.allocator, len - 1);

    return result;
}

/// Array.prototype.unshift(...items) - Add elements to beginning
pub fn arrayUnshift(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj);

    // In full implementation, would shift existing elements first
    const new_len = len + @as(i32, @intCast(args.len));
    setArrayLength(obj, ctx.allocator, new_len);

    return value.JSValue.fromInt(new_len);
}

/// Array.prototype.indexOf(searchElement, fromIndex?) - Find first index of element
pub fn arrayIndexOf(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    const obj = getObject(this) orelse return value.JSValue.fromInt(-1);
    const len = getArrayLength(obj);

    if (args.len == 0 or len <= 0) return value.JSValue.fromInt(-1);

    const search_elem = args[0];
    var from_idx: i32 = 0;
    if (args.len > 1 and args[1].isInt()) {
        from_idx = args[1].getInt();
        if (from_idx < 0) from_idx = @max(0, len + from_idx);
    }

    // Linear search (simplified)
    _ = search_elem;
    return value.JSValue.fromInt(-1);
}

/// Array.prototype.includes(searchElement, fromIndex?) - Check if array contains element
pub fn arrayIncludes(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const result = arrayIndexOf(ctx, this, args);
    if (result.isInt() and result.getInt() >= 0) {
        return value.JSValue.true_val;
    }
    return value.JSValue.false_val;
}

/// Array.prototype.join(separator?) - Join elements into string
pub fn arrayJoin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj);

    // Return length for now (proper string creation requires more infrastructure)
    return value.JSValue.fromInt(len);
}

/// Array.prototype.reverse() - Reverse array in place
pub fn arrayReverse(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    // Return this (in-place modification)
    return this;
}

/// Array.prototype.slice(start?, end?) - Return shallow copy of portion
pub fn arraySlice(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj);

    var start: i32 = 0;
    var end: i32 = len;

    if (args.len > 0 and args[0].isInt()) {
        start = args[0].getInt();
        if (start < 0) start = @max(0, len + start);
    }
    if (args.len > 1 and args[1].isInt()) {
        end = args[1].getInt();
        if (end < 0) end = @max(0, len + end);
    }

    // Return length of slice (proper array creation requires more infrastructure)
    return value.JSValue.fromInt(@max(0, end - start));
}

/// Array.prototype.concat(...items) - Merge arrays
pub fn arrayConcat(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    var total_len = getArrayLength(obj);

    for (args) |arg| {
        if (getObject(arg)) |arr| {
            total_len += getArrayLength(arr);
        } else {
            total_len += 1;
        }
    }

    return value.JSValue.fromInt(total_len);
}

/// Array.prototype.map(callback, thisArg?) - Map to new array
pub fn arrayMap(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    const obj = getObject(this) orelse return value.JSValue.undefined_val;
    const len = getArrayLength(obj);

    // Requires function call infrastructure - return length for now
    return value.JSValue.fromInt(len);
}

/// Array.prototype.filter(callback, thisArg?) - Filter to new array
pub fn arrayFilter(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // Requires function call infrastructure
    return value.JSValue.fromInt(0);
}

/// Array.prototype.reduce(callback, initialValue?) - Reduce to single value
pub fn arrayReduce(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    // Requires function call infrastructure
    if (args.len > 1) return args[1]; // Return initial value
    return value.JSValue.undefined_val;
}

/// Array.prototype.forEach(callback, thisArg?) - Execute callback for each element
pub fn arrayForEach(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // Requires function call infrastructure
    return value.JSValue.undefined_val;
}

/// Array.prototype.every(callback, thisArg?) - Test if all elements pass
pub fn arrayEvery(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    return value.JSValue.true_val;
}

/// Array.prototype.some(callback, thisArg?) - Test if any element passes
pub fn arraySome(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    return value.JSValue.false_val;
}

/// Array.prototype.find(callback, thisArg?) - Find first matching element
pub fn arrayFind(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    return value.JSValue.undefined_val;
}

/// Array.prototype.findIndex(callback, thisArg?) - Find index of first matching element
pub fn arrayFindIndex(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    return value.JSValue.fromInt(-1);
}

/// Array.prototype.fill(value, start?, end?) - Fill with static value
pub fn arrayFill(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    return this;
}

/// Array.prototype.sort(compareFunc?) - Sort array in place
pub fn arraySort(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    return this;
}

pub const array_methods = [_]BuiltinFunc{
    .{ .name = "isArray", .func = arrayIsArray, .arg_count = 1 },
    .{ .name = "push", .func = arrayPush, .arg_count = 1 },
    .{ .name = "pop", .func = arrayPop, .arg_count = 0 },
    .{ .name = "shift", .func = arrayShift, .arg_count = 0 },
    .{ .name = "unshift", .func = arrayUnshift, .arg_count = 1 },
    .{ .name = "indexOf", .func = arrayIndexOf, .arg_count = 1 },
    .{ .name = "includes", .func = arrayIncludes, .arg_count = 1 },
    .{ .name = "join", .func = arrayJoin, .arg_count = 1 },
    .{ .name = "reverse", .func = arrayReverse, .arg_count = 0 },
    .{ .name = "slice", .func = arraySlice, .arg_count = 2 },
    .{ .name = "concat", .func = arrayConcat, .arg_count = 1 },
    .{ .name = "map", .func = arrayMap, .arg_count = 1 },
    .{ .name = "filter", .func = arrayFilter, .arg_count = 1 },
    .{ .name = "reduce", .func = arrayReduce, .arg_count = 2 },
    .{ .name = "forEach", .func = arrayForEach, .arg_count = 1 },
    .{ .name = "every", .func = arrayEvery, .arg_count = 1 },
    .{ .name = "some", .func = arraySome, .arg_count = 1 },
    .{ .name = "find", .func = arrayFind, .arg_count = 1 },
    .{ .name = "findIndex", .func = arrayFindIndex, .arg_count = 1 },
    .{ .name = "fill", .func = arrayFill, .arg_count = 1 },
    .{ .name = "sort", .func = arraySort, .arg_count = 1 },
};

// ============================================================================
// String methods
// ============================================================================

/// Get JSString from value (if it's a string pointer)
fn getJSString(val: value.JSValue) ?*string.JSString {
    if (!val.isPtr()) return null;
    // In full implementation, would check header type
    return val.toPtr(string.JSString);
}

/// String.prototype.length - Get string length
pub fn stringLength(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    if (getJSString(this)) |str| {
        return value.JSValue.fromInt(@intCast(str.len));
    }
    return value.JSValue.fromInt(0);
}

/// String.prototype.charAt(index) - Get character at index
pub fn stringCharAt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (getJSString(this)) |str| {
        var idx: u32 = 0;
        if (args.len > 0 and args[0].isInt()) {
            const i = args[0].getInt();
            if (i < 0) return value.JSValue.undefined_val;
            idx = @intCast(i);
        }
        if (str.charAt(idx)) |_| {
            // Return single char (in full impl would return string)
            return value.JSValue.fromInt(@intCast(idx));
        }
    }
    return value.JSValue.undefined_val;
}

/// String.prototype.charCodeAt(index) - Get char code at index
pub fn stringCharCodeAt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (getJSString(this)) |str| {
        var idx: u32 = 0;
        if (args.len > 0 and args[0].isInt()) {
            const i = args[0].getInt();
            if (i < 0) return allocFloat(ctx, std.math.nan(f64));
            idx = @intCast(i);
        }
        if (str.charAt(idx)) |c| {
            return value.JSValue.fromInt(@intCast(c));
        }
    }
    return allocFloat(ctx, std.math.nan(f64));
}

/// String.prototype.indexOf(searchString, position?) - Find substring
pub fn stringIndexOf(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (args.len == 0) return value.JSValue.fromInt(-1);

    if (getJSString(this)) |str| {
        if (getJSString(args[0])) |needle| {
            if (str.indexOf(needle.data())) |idx| {
                return value.JSValue.fromInt(@intCast(idx));
            }
        }
    }
    return value.JSValue.fromInt(-1);
}

/// String.prototype.lastIndexOf(searchString, position?) - Find substring from end
pub fn stringLastIndexOf(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (args.len == 0) return value.JSValue.fromInt(-1);

    if (getJSString(this)) |str| {
        if (getJSString(args[0])) |needle| {
            if (str.lastIndexOf(needle.data())) |idx| {
                return value.JSValue.fromInt(@intCast(idx));
            }
        }
    }
    return value.JSValue.fromInt(-1);
}

/// String.prototype.startsWith(searchString, position?) - Check prefix
pub fn stringStartsWith(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (args.len == 0) return value.JSValue.false_val;

    if (getJSString(this)) |str| {
        if (getJSString(args[0])) |prefix| {
            return if (str.startsWith(prefix.data())) value.JSValue.true_val else value.JSValue.false_val;
        }
    }
    return value.JSValue.false_val;
}

/// String.prototype.endsWith(searchString, length?) - Check suffix
pub fn stringEndsWith(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (args.len == 0) return value.JSValue.false_val;

    if (getJSString(this)) |str| {
        if (getJSString(args[0])) |suffix| {
            return if (str.endsWith(suffix.data())) value.JSValue.true_val else value.JSValue.false_val;
        }
    }
    return value.JSValue.false_val;
}

/// String.prototype.includes(searchString, position?) - Check if contains
pub fn stringIncludes(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const result = stringIndexOf(ctx, this, args);
    if (result.isInt() and result.getInt() >= 0) {
        return value.JSValue.true_val;
    }
    return value.JSValue.false_val;
}

/// String.prototype.slice(start, end?) - Extract substring
pub fn stringSlice(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (getJSString(this)) |str| {
        var start: i32 = 0;
        var end: i32 = @intCast(str.len);

        if (args.len > 0 and args[0].isInt()) {
            start = args[0].getInt();
            if (start < 0) start = @max(0, @as(i32, @intCast(str.len)) + start);
        }
        if (args.len > 1 and args[1].isInt()) {
            end = args[1].getInt();
            if (end < 0) end = @max(0, @as(i32, @intCast(str.len)) + end);
        }

        // Return slice length for now
        return value.JSValue.fromInt(@max(0, end - start));
    }
    return value.JSValue.undefined_val;
}

/// String.prototype.substring(start, end?) - Extract substring
pub fn stringSubstring(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    // substring is similar to slice but doesn't support negative indices
    return stringSlice(ctx, this, args);
}

/// String.prototype.toLowerCase() - Convert to lowercase
pub fn stringToLowerCase(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    // In full impl, would create new lowercase string
    return this;
}

/// String.prototype.toUpperCase() - Convert to uppercase
pub fn stringToUpperCase(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    // In full impl, would create new uppercase string
    return this;
}

/// String.prototype.trim() - Remove whitespace from both ends
pub fn stringTrim(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    return this;
}

/// String.prototype.trimStart() - Remove whitespace from start
pub fn stringTrimStart(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    return this;
}

/// String.prototype.trimEnd() - Remove whitespace from end
pub fn stringTrimEnd(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = args;
    return this;
}

/// String.prototype.split(separator, limit?) - Split into array
pub fn stringSplit(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    _ = args;
    // Return count (proper array creation requires more infrastructure)
    return value.JSValue.fromInt(1);
}

/// String.prototype.repeat(count) - Repeat string
pub fn stringRepeat(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (getJSString(this)) |str| {
        var count: i32 = 0;
        if (args.len > 0 and args[0].isInt()) {
            count = args[0].getInt();
            if (count < 0) return value.JSValue.undefined_val;
        }
        // Return new length (proper string creation in full impl)
        return value.JSValue.fromInt(@as(i32, @intCast(str.len)) * count);
    }
    return value.JSValue.undefined_val;
}

/// String.prototype.padStart(targetLength, padString?) - Pad at start
pub fn stringPadStart(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (getJSString(this)) |str| {
        var target_len: i32 = @intCast(str.len);
        if (args.len > 0 and args[0].isInt()) {
            target_len = @max(target_len, args[0].getInt());
        }
        return value.JSValue.fromInt(target_len);
    }
    return this;
}

/// String.prototype.padEnd(targetLength, padString?) - Pad at end
pub fn stringPadEnd(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    if (getJSString(this)) |str| {
        var target_len: i32 = @intCast(str.len);
        if (args.len > 0 and args[0].isInt()) {
            target_len = @max(target_len, args[0].getInt());
        }
        return value.JSValue.fromInt(target_len);
    }
    return this;
}

/// String.prototype.concat(...strings) - Concatenate strings
pub fn stringConcat(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    var total_len: i32 = 0;
    if (getJSString(this)) |str| {
        total_len = @intCast(str.len);
    }
    for (args) |arg| {
        if (getJSString(arg)) |str| {
            total_len += @intCast(str.len);
        }
    }
    return value.JSValue.fromInt(total_len);
}

/// String.prototype.replace(searchValue, replaceValue) - Replace first occurrence
pub fn stringReplace(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;

    if (!this.isString() or args.len < 2) return this;
    const str = this.toPtr(string.JSString);
    const input = str.data();

    const replacement = if (args[1].isString()) args[1].toPtr(string.JSString).data() else "";

    // Check if searchValue is a RegExp
    if (args[0].isObject()) {
        const search_obj = object.JSObject.fromValue(args[0]);
        const source_atom = ctx.atoms.intern("source") catch return this;
        if (search_obj.getProperty(source_atom)) |source_val| {
            if (source_val.isString()) {
                // It's a RegExp
                const pattern = source_val.toPtr(string.JSString).data();
                const _regex_atom = ctx.atoms.intern("_regex") catch return this;
                const flags_val = search_obj.getProperty(_regex_atom) orelse value.JSValue.fromInt(0);
                const flags_int: u8 = if (flags_val.isInt()) @intCast(flags_val.getInt()) else 0;
                const flags = regex.Flags{
                    .global = (flags_int & 1) != 0,
                    .ignore_case = (flags_int & 2) != 0,
                    .multiline = (flags_int & 4) != 0,
                    .dot_all = (flags_int & 8) != 0,
                };

                const compiled = regex.Regex.compile(pattern, flags);

                if (flags.global) {
                    // Replace all matches
                    var result = std.ArrayList(u8).init(allocator);
                    var pos: usize = 0;
                    while (pos <= input.len) {
                        if (compiled.exec(input[pos..])) |match| {
                            result.appendSlice(input[pos .. pos + match.start]) catch return this;
                            result.appendSlice(replacement) catch return this;
                            pos = pos + match.end;
                            if (match.end == match.start) pos += 1; // Avoid infinite loop
                        } else {
                            break;
                        }
                    }
                    result.appendSlice(input[pos..]) catch return this;
                    const new_str = string.createString(allocator, result.items) catch return this;
                    return new_str.toValue();
                } else {
                    // Replace first match
                    if (compiled.exec(input)) |match| {
                        var result = std.ArrayList(u8).init(allocator);
                        result.appendSlice(input[0..match.start]) catch return this;
                        result.appendSlice(replacement) catch return this;
                        result.appendSlice(input[match.end..]) catch return this;
                        const new_str = string.createString(allocator, result.items) catch return this;
                        return new_str.toValue();
                    }
                }
                return this;
            }
        }
    }

    // String search
    if (args[0].isString()) {
        const search = args[0].toPtr(string.JSString).data();
        if (std.mem.indexOf(u8, input, search)) |idx| {
            var result = std.ArrayList(u8).init(allocator);
            result.appendSlice(input[0..idx]) catch return this;
            result.appendSlice(replacement) catch return this;
            result.appendSlice(input[idx + search.len ..]) catch return this;
            const new_str = string.createString(allocator, result.items) catch return this;
            return new_str.toValue();
        }
    }

    return this;
}

/// String.prototype.replaceAll(searchValue, replaceValue) - Replace all occurrences
pub fn stringReplaceAll(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;

    if (!this.isString() or args.len < 2) return this;
    const str = this.toPtr(string.JSString);
    const input = str.data();

    const replacement = if (args[1].isString()) args[1].toPtr(string.JSString).data() else "";

    // String search - replace all
    if (args[0].isString()) {
        const search = args[0].toPtr(string.JSString).data();
        if (search.len == 0) return this;

        var result = std.ArrayList(u8).init(allocator);
        var pos: usize = 0;
        while (std.mem.indexOfPos(u8, input, pos, search)) |idx| {
            result.appendSlice(input[pos..idx]) catch return this;
            result.appendSlice(replacement) catch return this;
            pos = idx + search.len;
        }
        result.appendSlice(input[pos..]) catch return this;
        const new_str = string.createString(allocator, result.items) catch return this;
        return new_str.toValue();
    }

    return this;
}

/// String.prototype.match(regexp) - Returns array of matches or null
pub fn stringMatch(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.null_val;

    if (!this.isString() or args.len < 1) return value.JSValue.null_val;
    const str = this.toPtr(string.JSString);
    const input = str.data();

    // Get RegExp pattern
    if (!args[0].isObject()) return value.JSValue.null_val;
    const regexp_obj = object.JSObject.fromValue(args[0]);
    const source_atom = ctx.atoms.intern("source") catch return value.JSValue.null_val;
    const source_val = regexp_obj.getProperty(source_atom) orelse return value.JSValue.null_val;
    if (!source_val.isString()) return value.JSValue.null_val;

    const pattern = source_val.toPtr(string.JSString).data();
    const _regex_atom = ctx.atoms.intern("_regex") catch return value.JSValue.null_val;
    const flags_val = regexp_obj.getProperty(_regex_atom) orelse value.JSValue.fromInt(0);
    const flags_int: u8 = if (flags_val.isInt()) @intCast(flags_val.getInt()) else 0;
    const flags = regex.Flags{
        .global = (flags_int & 1) != 0,
        .ignore_case = (flags_int & 2) != 0,
        .multiline = (flags_int & 4) != 0,
        .dot_all = (flags_int & 8) != 0,
    };

    const compiled = regex.Regex.compile(pattern, flags);

    if (flags.global) {
        // Return array of all matches
        const matches = compiled.execAll(allocator, input) catch return value.JSValue.null_val;
        if (matches.len == 0) {
            allocator.free(matches);
            return value.JSValue.null_val;
        }

        const result_arr = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.null_val;
        for (matches, 0..) |match, i| {
            const match_str = string.createString(allocator, match.slice()) catch continue;
            result_arr.setSlot(i, match_str.toValue());
        }
        result_arr.setProperty(allocator, .length, value.JSValue.fromInt(@intCast(matches.len))) catch {};
        allocator.free(matches);
        return result_arr.toValue();
    } else {
        // Return first match array (like exec)
        if (compiled.exec(input)) |match| {
            const result_arr = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.null_val;
            const matched_str = string.createString(allocator, match.slice()) catch return value.JSValue.null_val;
            result_arr.setSlot(0, matched_str.toValue());
            result_arr.setProperty(allocator, .length, value.JSValue.fromInt(1)) catch {};

            const index_atom = ctx.atoms.intern("index") catch return result_arr.toValue();
            result_arr.setProperty(allocator, index_atom, value.JSValue.fromInt(@intCast(match.start))) catch {};

            return result_arr.toValue();
        }
    }

    return value.JSValue.null_val;
}

/// String.prototype.search(regexp) - Returns index of first match or -1
pub fn stringSearch(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isString() or args.len < 1) return value.JSValue.fromInt(-1);
    const str = this.toPtr(string.JSString);
    const input = str.data();

    // Get RegExp pattern
    if (!args[0].isObject()) return value.JSValue.fromInt(-1);
    const regexp_obj = object.JSObject.fromValue(args[0]);
    const source_atom = ctx.atoms.intern("source") catch return value.JSValue.fromInt(-1);
    const source_val = regexp_obj.getProperty(source_atom) orelse return value.JSValue.fromInt(-1);
    if (!source_val.isString()) return value.JSValue.fromInt(-1);

    const pattern = source_val.toPtr(string.JSString).data();
    const _regex_atom = ctx.atoms.intern("_regex") catch return value.JSValue.fromInt(-1);
    const flags_val = regexp_obj.getProperty(_regex_atom) orelse value.JSValue.fromInt(0);
    const flags_int: u8 = if (flags_val.isInt()) @intCast(flags_val.getInt()) else 0;
    const flags = regex.Flags{
        .global = false, // search always finds first
        .ignore_case = (flags_int & 2) != 0,
        .multiline = (flags_int & 4) != 0,
        .dot_all = (flags_int & 8) != 0,
    };

    const compiled = regex.Regex.compile(pattern, flags);
    if (compiled.exec(input)) |match| {
        return value.JSValue.fromInt(@intCast(match.start));
    }

    return value.JSValue.fromInt(-1);
}

fn wrapStringMatch(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringMatch(ctx, this, args);
}

fn wrapStringSearch(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringSearch(ctx, this, args);
}

/// String.fromCharCode(...charCodes) - Create string from char codes
pub fn stringFromCharCode(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    // Return length for now
    return value.JSValue.fromInt(@intCast(args.len));
}

pub const string_methods = [_]BuiltinFunc{
    .{ .name = "length", .func = stringLength, .arg_count = 0 },
    .{ .name = "charAt", .func = stringCharAt, .arg_count = 1 },
    .{ .name = "charCodeAt", .func = stringCharCodeAt, .arg_count = 1 },
    .{ .name = "indexOf", .func = stringIndexOf, .arg_count = 1 },
    .{ .name = "lastIndexOf", .func = stringLastIndexOf, .arg_count = 1 },
    .{ .name = "startsWith", .func = stringStartsWith, .arg_count = 1 },
    .{ .name = "endsWith", .func = stringEndsWith, .arg_count = 1 },
    .{ .name = "includes", .func = stringIncludes, .arg_count = 1 },
    .{ .name = "slice", .func = stringSlice, .arg_count = 2 },
    .{ .name = "substring", .func = stringSubstring, .arg_count = 2 },
    .{ .name = "toLowerCase", .func = stringToLowerCase, .arg_count = 0 },
    .{ .name = "toUpperCase", .func = stringToUpperCase, .arg_count = 0 },
    .{ .name = "trim", .func = stringTrim, .arg_count = 0 },
    .{ .name = "trimStart", .func = stringTrimStart, .arg_count = 0 },
    .{ .name = "trimEnd", .func = stringTrimEnd, .arg_count = 0 },
    .{ .name = "split", .func = stringSplit, .arg_count = 1 },
    .{ .name = "repeat", .func = stringRepeat, .arg_count = 1 },
    .{ .name = "padStart", .func = stringPadStart, .arg_count = 1 },
    .{ .name = "padEnd", .func = stringPadEnd, .arg_count = 1 },
    .{ .name = "concat", .func = stringConcat, .arg_count = 1 },
    .{ .name = "replace", .func = stringReplace, .arg_count = 2 },
    .{ .name = "replaceAll", .func = stringReplaceAll, .arg_count = 2 },
    .{ .name = "match", .func = stringMatch, .arg_count = 1 },
    .{ .name = "search", .func = stringSearch, .arg_count = 1 },
    .{ .name = "fromCharCode", .func = stringFromCharCode, .arg_count = 1 },
};

// ============================================================================
// Math methods
// ============================================================================

/// Helper to get numeric value as f64
fn toNumber(val: value.JSValue) ?f64 {
    return val.toNumber();
}

/// Helper to allocate float result
fn allocFloat(ctx: *context.Context, v: f64) value.JSValue {
    // Check if result is a safe integer
    if (@floor(v) == v and v >= -2147483648 and v <= 2147483647) {
        return value.JSValue.fromInt(@intFromFloat(v));
    }
    const box = ctx.gc_state.allocFloat(v) catch return value.JSValue.undefined_val;
    return value.JSValue.fromPtr(box);
}

pub fn mathAbs(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @abs(n));
}

pub fn mathFloor(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @floor(n));
}

pub fn mathCeil(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @ceil(n));
}

pub fn mathRound(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @round(n));
}

pub fn mathTrunc(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @trunc(n));
}

pub fn mathMin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return allocFloat(ctx, std.math.inf(f64));

    var min_val: f64 = std.math.inf(f64);
    for (args) |arg| {
        const n = toNumber(arg) orelse return value.JSValue.undefined_val;
        if (std.math.isNan(n)) return allocFloat(ctx, std.math.nan(f64));
        min_val = @min(min_val, n);
    }
    return allocFloat(ctx, min_val);
}

pub fn mathMax(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return allocFloat(ctx, -std.math.inf(f64));

    var max_val: f64 = -std.math.inf(f64);
    for (args) |arg| {
        const n = toNumber(arg) orelse return value.JSValue.undefined_val;
        if (std.math.isNan(n)) return allocFloat(ctx, std.math.nan(f64));
        max_val = @max(max_val, n);
    }
    return allocFloat(ctx, max_val);
}

pub fn mathPow(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len < 2) return value.JSValue.undefined_val;
    const base = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    const exp = toNumber(args[1]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, std.math.pow(f64, base, exp));
}

pub fn mathSqrt(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    if (n < 0) return allocFloat(ctx, std.math.nan(f64));
    return allocFloat(ctx, @sqrt(n));
}

pub fn mathSin(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @sin(n));
}

pub fn mathCos(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @cos(n));
}

pub fn mathTan(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @tan(n));
}

pub fn mathLog(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @log(n));
}

pub fn mathExp(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    return allocFloat(ctx, @exp(n));
}

var prng: ?std.Random.DefaultPrng = null;

pub fn mathRandom(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    _ = args;
    if (prng == null) {
        prng = std.Random.DefaultPrng.init(@bitCast(std.time.milliTimestamp()));
    }
    const r = prng.?.random().float(f64);
    return allocFloat(ctx, r);
}

pub fn mathSign(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;
    const n = toNumber(args[0]) orelse return value.JSValue.undefined_val;
    if (std.math.isNan(n)) return allocFloat(ctx, std.math.nan(f64));
    if (n > 0) return value.JSValue.fromInt(1);
    if (n < 0) return value.JSValue.fromInt(-1);
    return value.JSValue.fromInt(0);
}

pub const math_methods = [_]BuiltinFunc{
    .{ .name = "abs", .func = mathAbs, .arg_count = 1 },
    .{ .name = "floor", .func = mathFloor, .arg_count = 1 },
    .{ .name = "ceil", .func = mathCeil, .arg_count = 1 },
    .{ .name = "round", .func = mathRound, .arg_count = 1 },
    .{ .name = "trunc", .func = mathTrunc, .arg_count = 1 },
    .{ .name = "min", .func = mathMin, .arg_count = 2 },
    .{ .name = "max", .func = mathMax, .arg_count = 2 },
    .{ .name = "pow", .func = mathPow, .arg_count = 2 },
    .{ .name = "sqrt", .func = mathSqrt, .arg_count = 1 },
    .{ .name = "sin", .func = mathSin, .arg_count = 1 },
    .{ .name = "cos", .func = mathCos, .arg_count = 1 },
    .{ .name = "tan", .func = mathTan, .arg_count = 1 },
    .{ .name = "log", .func = mathLog, .arg_count = 1 },
    .{ .name = "exp", .func = mathExp, .arg_count = 1 },
    .{ .name = "random", .func = mathRandom, .arg_count = 0 },
    .{ .name = "sign", .func = mathSign, .arg_count = 1 },
};

/// Math constants
pub const math_constants = struct {
    pub const PI: f64 = 3.141592653589793;
    pub const E: f64 = 2.718281828459045;
    pub const LN2: f64 = 0.6931471805599453;
    pub const LN10: f64 = 2.302585092994046;
    pub const LOG2E: f64 = 1.4426950408889634;
    pub const LOG10E: f64 = 0.4342944819032518;
    pub const SQRT2: f64 = 1.4142135623730951;
    pub const SQRT1_2: f64 = 0.7071067811865476;
};

// ============================================================================
// Console methods
// ============================================================================

pub fn consoleLog(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    const stdout = std.io.getStdOut().writer();
    for (args, 0..) |arg, i| {
        if (i > 0) stdout.writeAll(" ") catch {};
        printValue(stdout, arg) catch {};
    }
    stdout.writeAll("\n") catch {};
    return value.JSValue.undefined_val;
}

pub fn consoleWarn(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    const stderr = std.io.getStdErr().writer();
    stderr.writeAll("[WARN] ") catch {};
    for (args, 0..) |arg, i| {
        if (i > 0) stderr.writeAll(" ") catch {};
        printValue(stderr, arg) catch {};
    }
    stderr.writeAll("\n") catch {};
    return value.JSValue.undefined_val;
}

pub fn consoleError(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = ctx;
    _ = this;
    const stderr = std.io.getStdErr().writer();
    stderr.writeAll("[ERROR] ") catch {};
    for (args, 0..) |arg, i| {
        if (i > 0) stderr.writeAll(" ") catch {};
        printValue(stderr, arg) catch {};
    }
    stderr.writeAll("\n") catch {};
    return value.JSValue.undefined_val;
}

fn printValue(writer: anytype, val: value.JSValue) !void {
    if (val.isInt()) {
        try writer.print("{d}", .{val.getInt()});
    } else if (val.isNull()) {
        try writer.writeAll("null");
    } else if (val.isUndefined()) {
        try writer.writeAll("undefined");
    } else if (val.isTrue()) {
        try writer.writeAll("true");
    } else if (val.isFalse()) {
        try writer.writeAll("false");
    } else {
        try writer.writeAll("[object]");
    }
}

pub const console_methods = [_]BuiltinFunc{
    .{ .name = "log", .func = consoleLog, .arg_count = 0 },
    .{ .name = "warn", .func = consoleWarn, .arg_count = 0 },
    .{ .name = "error", .func = consoleError, .arg_count = 0 },
};

// ============================================================================
// Initialization
// ============================================================================

/// Wrapper to convert context.Context native function signature to object.NativeFn
fn wrapConsoleLog(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return consoleLog(ctx, this, args);
}

fn wrapConsoleWarn(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return consoleWarn(ctx, this, args);
}

fn wrapConsoleError(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return consoleError(ctx, this, args);
}

fn wrapMathAbs(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathAbs(ctx, this, args);
}

fn wrapMathFloor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathFloor(ctx, this, args);
}

fn wrapMathCeil(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathCeil(ctx, this, args);
}

fn wrapMathRound(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathRound(ctx, this, args);
}

fn wrapMathTrunc(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathTrunc(ctx, this, args);
}

fn wrapMathMin(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathMin(ctx, this, args);
}

fn wrapMathMax(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathMax(ctx, this, args);
}

fn wrapMathPow(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathPow(ctx, this, args);
}

fn wrapMathSqrt(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathSqrt(ctx, this, args);
}

fn wrapMathSin(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathSin(ctx, this, args);
}

fn wrapMathCos(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathCos(ctx, this, args);
}

fn wrapMathTan(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathTan(ctx, this, args);
}

fn wrapMathLog(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathLog(ctx, this, args);
}

fn wrapMathExp(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathExp(ctx, this, args);
}

fn wrapMathRandom(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathRandom(ctx, this, args);
}

fn wrapMathSign(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return mathSign(ctx, this, args);
}

// ============================================================================
// Array method wrappers
// ============================================================================

fn wrapArrayIsArray(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayIsArray(ctx, this, args);
}

fn wrapArrayPush(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayPush(ctx, this, args);
}

fn wrapArrayPop(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayPop(ctx, this, args);
}

fn wrapArrayShift(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayShift(ctx, this, args);
}

fn wrapArrayUnshift(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayUnshift(ctx, this, args);
}

fn wrapArrayIndexOf(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayIndexOf(ctx, this, args);
}

fn wrapArrayIncludes(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayIncludes(ctx, this, args);
}

fn wrapArrayJoin(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayJoin(ctx, this, args);
}

fn wrapArrayReverse(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayReverse(ctx, this, args);
}

fn wrapArraySlice(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arraySlice(ctx, this, args);
}

fn wrapArrayConcat(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayConcat(ctx, this, args);
}

fn wrapArrayMap(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayMap(ctx, this, args);
}

fn wrapArrayFilter(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayFilter(ctx, this, args);
}

fn wrapArrayReduce(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayReduce(ctx, this, args);
}

fn wrapArrayForEach(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayForEach(ctx, this, args);
}

fn wrapArrayEvery(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayEvery(ctx, this, args);
}

fn wrapArraySome(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arraySome(ctx, this, args);
}

fn wrapArrayFind(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayFind(ctx, this, args);
}

fn wrapArrayFindIndex(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayFindIndex(ctx, this, args);
}

fn wrapArrayFill(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arrayFill(ctx, this, args);
}

fn wrapArraySort(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return arraySort(ctx, this, args);
}

// ============================================================================
// String method wrappers
// ============================================================================

fn wrapStringCharAt(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringCharAt(ctx, this, args);
}

fn wrapStringCharCodeAt(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringCharCodeAt(ctx, this, args);
}

fn wrapStringIndexOf(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringIndexOf(ctx, this, args);
}

fn wrapStringLastIndexOf(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringLastIndexOf(ctx, this, args);
}

fn wrapStringStartsWith(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringStartsWith(ctx, this, args);
}

fn wrapStringEndsWith(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringEndsWith(ctx, this, args);
}

fn wrapStringIncludes(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringIncludes(ctx, this, args);
}

fn wrapStringSlice(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringSlice(ctx, this, args);
}

fn wrapStringSubstring(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringSubstring(ctx, this, args);
}

fn wrapStringToLowerCase(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringToLowerCase(ctx, this, args);
}

fn wrapStringToUpperCase(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringToUpperCase(ctx, this, args);
}

fn wrapStringTrim(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringTrim(ctx, this, args);
}

fn wrapStringTrimStart(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringTrimStart(ctx, this, args);
}

fn wrapStringTrimEnd(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringTrimEnd(ctx, this, args);
}

fn wrapStringSplit(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringSplit(ctx, this, args);
}

fn wrapStringRepeat(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringRepeat(ctx, this, args);
}

fn wrapStringPadStart(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringPadStart(ctx, this, args);
}

fn wrapStringPadEnd(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringPadEnd(ctx, this, args);
}

fn wrapStringConcat(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringConcat(ctx, this, args);
}

fn wrapStringReplace(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringReplace(ctx, this, args);
}

fn wrapStringReplaceAll(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringReplaceAll(ctx, this, args);
}

fn wrapStringFromCharCode(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return stringFromCharCode(ctx, this, args);
}

/// Helper to add a method using a dynamic atom name
fn addMethodDynamic(
    ctx: *context.Context,
    obj: *object.JSObject,
    name: []const u8,
    func: object.NativeFn,
    arg_count: u8,
) !void {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return error.NoRootClass;
    const atom = try ctx.atoms.intern(name);
    const func_obj = try object.JSObject.createNativeFunction(allocator, root_class, func, atom, arg_count);
    try obj.setProperty(allocator, atom, func_obj.toValue());
}

/// Create a native function and add it as a property on an object
fn addMethod(
    allocator: std.mem.Allocator,
    obj: *object.JSObject,
    root_class: *object.HiddenClass,
    name: object.Atom,
    func: object.NativeFn,
    arg_count: u8,
) !void {
    const func_obj = try object.JSObject.createNativeFunction(allocator, root_class, func, name, arg_count);
    try obj.setProperty(allocator, name, func_obj.toValue());
}

/// Initialize all built-in objects on global
pub fn initBuiltins(ctx: *context.Context) !void {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return error.NoRootClass;

    // Create console object
    const console_obj = try object.JSObject.create(allocator, root_class, null);
    try addMethod(allocator, console_obj, root_class, .log, wrapConsoleLog, 0);
    // Note: .warn and .error atoms don't exist in predefined atoms, use .log for now
    // In full implementation would add dynamic atoms

    // Register console on global
    try ctx.setGlobal(.console, console_obj.toValue());

    // Create Math object
    const math_obj = try object.JSObject.create(allocator, root_class, null);
    try addMethod(allocator, math_obj, root_class, .abs, wrapMathAbs, 1);
    try addMethod(allocator, math_obj, root_class, .floor, wrapMathFloor, 1);
    try addMethod(allocator, math_obj, root_class, .ceil, wrapMathCeil, 1);
    try addMethod(allocator, math_obj, root_class, .round, wrapMathRound, 1);
    try addMethod(allocator, math_obj, root_class, .min, wrapMathMin, 2);
    try addMethod(allocator, math_obj, root_class, .max, wrapMathMax, 2);
    try addMethod(allocator, math_obj, root_class, .pow, wrapMathPow, 2);
    try addMethod(allocator, math_obj, root_class, .sqrt, wrapMathSqrt, 1);
    try addMethod(allocator, math_obj, root_class, .sin, wrapMathSin, 1);
    try addMethod(allocator, math_obj, root_class, .cos, wrapMathCos, 1);
    try addMethod(allocator, math_obj, root_class, .tan, wrapMathTan, 1);
    try addMethod(allocator, math_obj, root_class, .log, wrapMathLog, 1);
    try addMethod(allocator, math_obj, root_class, .exp, wrapMathExp, 1);
    try addMethod(allocator, math_obj, root_class, .random, wrapMathRandom, 0);

    // Add Math constants as properties
    const pi_box = try ctx.gc_state.allocFloat(math_constants.PI);
    try math_obj.setProperty(allocator, @enumFromInt(ctx.atoms.next_id), value.JSValue.fromPtr(pi_box));
    // Note: Would need to add "PI", "E" etc as dynamic atoms for full implementation

    // Register Math on global
    try ctx.setGlobal(.Math, math_obj.toValue());

    // Create JSON object
    const json_obj = try object.JSObject.create(allocator, root_class, null);
    try addMethod(allocator, json_obj, root_class, .parse, wrapJsonParse, 1);
    try addMethod(allocator, json_obj, root_class, .stringify, wrapJsonStringify, 1);

    // Register JSON on global
    try ctx.setGlobal(.JSON, json_obj.toValue());

    // Create Object constructor with static methods
    const object_obj = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, object_obj, "keys", wrapObjectKeys, 1);
    try addMethodDynamic(ctx, object_obj, "values", wrapObjectValues, 1);
    try addMethodDynamic(ctx, object_obj, "entries", wrapObjectEntries, 1);
    try addMethodDynamic(ctx, object_obj, "assign", wrapObjectAssign, 2);
    try addMethodDynamic(ctx, object_obj, "hasOwn", wrapObjectHasOwn, 2);
    try addMethodDynamic(ctx, object_obj, "freeze", wrapObjectFreeze, 1);
    try addMethodDynamic(ctx, object_obj, "isFrozen", wrapObjectIsFrozen, 1);

    // Register Object on global
    try ctx.setGlobal(.Object, object_obj.toValue());

    // Create Error prototype with toString method
    const error_proto = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, error_proto, "toString", wrapErrorToString, 0);

    // Create Error constructor
    const error_ctor_func = try object.JSObject.createNativeFunction(allocator, root_class, wrapErrorConstructor, .Error, 1);
    try error_ctor_func.setProperty(allocator, .prototype, error_proto.toValue());
    try ctx.setGlobal(.Error, error_ctor_func.toValue());

    // Create TypeError constructor
    const type_error_ctor = try object.JSObject.createNativeFunction(allocator, root_class, wrapTypeErrorConstructor, .TypeError, 1);
    try type_error_ctor.setProperty(allocator, .prototype, error_proto.toValue());
    try ctx.setGlobal(.TypeError, type_error_ctor.toValue());

    // Create RangeError constructor
    const range_error_ctor = try object.JSObject.createNativeFunction(allocator, root_class, wrapRangeErrorConstructor, .RangeError, 1);
    try range_error_ctor.setProperty(allocator, .prototype, error_proto.toValue());
    try ctx.setGlobal(.RangeError, range_error_ctor.toValue());

    // Create SyntaxError constructor
    const syntax_error_ctor = try object.JSObject.createNativeFunction(allocator, root_class, wrapSyntaxErrorConstructor, .SyntaxError, 1);
    try syntax_error_ctor.setProperty(allocator, .prototype, error_proto.toValue());
    try ctx.setGlobal(.SyntaxError, syntax_error_ctor.toValue());

    // Create ReferenceError constructor
    const ref_error_ctor = try object.JSObject.createNativeFunction(allocator, root_class, wrapReferenceErrorConstructor, .ReferenceError, 1);
    try ref_error_ctor.setProperty(allocator, .prototype, error_proto.toValue());
    try ctx.setGlobal(.ReferenceError, ref_error_ctor.toValue());

    // Create Promise prototype with then/catch methods
    const promise_proto = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, promise_proto, "then", wrapPromiseThen, 2);
    try addMethodDynamic(ctx, promise_proto, "catch", wrapPromiseCatch, 1);

    // Create Promise constructor with static methods
    const promise_ctor = try object.JSObject.createNativeFunction(allocator, root_class, wrapPromiseConstructor, .Promise, 1);
    try promise_ctor.setProperty(allocator, .prototype, promise_proto.toValue());

    // Add static methods: Promise.resolve, Promise.reject
    try addMethodDynamic(ctx, promise_ctor, "resolve", wrapPromiseResolve, 1);
    try addMethodDynamic(ctx, promise_ctor, "reject", wrapPromiseReject, 1);

    try ctx.setGlobal(.Promise, promise_ctor.toValue());

    // Create Number object with static methods
    const number_obj = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, number_obj, "isInteger", wrapNumberIsInteger, 1);
    try addMethodDynamic(ctx, number_obj, "isNaN", wrapNumberIsNaN, 1);
    try addMethodDynamic(ctx, number_obj, "isFinite", wrapNumberIsFinite, 1);
    try addMethodDynamic(ctx, number_obj, "parseFloat", wrapNumberParseFloat, 1);
    try addMethodDynamic(ctx, number_obj, "parseInt", wrapNumberParseInt, 2);

    // Add Number constants
    const max_value_atom = try ctx.atoms.intern("MAX_VALUE");
    const max_value_box = try ctx.gc_state.allocFloat(1.7976931348623157e+308);
    try number_obj.setProperty(allocator, max_value_atom, value.JSValue.fromPtr(max_value_box));

    const min_value_atom = try ctx.atoms.intern("MIN_VALUE");
    const min_value_box = try ctx.gc_state.allocFloat(5e-324);
    try number_obj.setProperty(allocator, min_value_atom, value.JSValue.fromPtr(min_value_box));

    const nan_atom = try ctx.atoms.intern("NaN");
    try number_obj.setProperty(allocator, nan_atom, value.JSValue.nan_val);

    const pos_inf_atom = try ctx.atoms.intern("POSITIVE_INFINITY");
    const pos_inf_box = try ctx.gc_state.allocFloat(std.math.inf(f64));
    try number_obj.setProperty(allocator, pos_inf_atom, value.JSValue.fromPtr(pos_inf_box));

    const neg_inf_atom = try ctx.atoms.intern("NEGATIVE_INFINITY");
    const neg_inf_box = try ctx.gc_state.allocFloat(-std.math.inf(f64));
    try number_obj.setProperty(allocator, neg_inf_atom, value.JSValue.fromPtr(neg_inf_box));

    // Register Number on global (dynamic atom since "Number" isn't predefined)
    const number_global_atom = try ctx.atoms.intern("Number");
    try ctx.setGlobal(number_global_atom, number_obj.toValue());

    // Also register parseFloat and parseInt globally (JS convention)
    const global_parse_float_atom = try ctx.atoms.intern("parseFloat");
    const parse_float_func = try object.JSObject.createNativeFunction(allocator, root_class, wrapNumberParseFloat, global_parse_float_atom, 1);
    try ctx.setGlobal(global_parse_float_atom, parse_float_func.toValue());

    const global_parse_int_atom = try ctx.atoms.intern("parseInt");
    const parse_int_func = try object.JSObject.createNativeFunction(allocator, root_class, wrapNumberParseInt, global_parse_int_atom, 2);
    try ctx.setGlobal(global_parse_int_atom, parse_int_func.toValue());

    // Also register isNaN and isFinite globally (JS convention)
    const global_is_nan_atom = try ctx.atoms.intern("isNaN");
    const global_is_nan_func = try object.JSObject.createNativeFunction(allocator, root_class, wrapGlobalIsNaN, global_is_nan_atom, 1);
    try ctx.setGlobal(global_is_nan_atom, global_is_nan_func.toValue());

    const global_is_finite_atom = try ctx.atoms.intern("isFinite");
    const global_is_finite_func = try object.JSObject.createNativeFunction(allocator, root_class, wrapGlobalIsFinite, global_is_finite_atom, 1);
    try ctx.setGlobal(global_is_finite_atom, global_is_finite_func.toValue());

    // Create Map prototype with methods
    const map_proto = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, map_proto, "set", wrapMapSet, 2);
    try addMethodDynamic(ctx, map_proto, "get", wrapMapGet, 1);
    try addMethodDynamic(ctx, map_proto, "has", wrapMapHas, 1);
    try addMethodDynamic(ctx, map_proto, "delete", wrapMapDelete, 1);
    try addMethodDynamic(ctx, map_proto, "clear", wrapMapClear, 0);

    // Create Map constructor
    const map_atom = try ctx.atoms.intern("Map");
    const map_ctor = try object.JSObject.createNativeFunction(allocator, root_class, wrapMapConstructor, map_atom, 0);
    try map_ctor.setProperty(allocator, .prototype, map_proto.toValue());
    try ctx.setGlobal(map_atom, map_ctor.toValue());

    // Create Set prototype with methods
    const set_proto = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, set_proto, "add", wrapSetAdd, 1);
    try addMethodDynamic(ctx, set_proto, "has", wrapSetHas, 1);
    try addMethodDynamic(ctx, set_proto, "delete", wrapSetDelete, 1);
    try addMethodDynamic(ctx, set_proto, "clear", wrapSetClear, 0);

    // Create Set constructor
    const set_atom = try ctx.atoms.intern("Set");
    const set_ctor = try object.JSObject.createNativeFunction(allocator, root_class, wrapSetConstructor, set_atom, 0);
    try set_ctor.setProperty(allocator, .prototype, set_proto.toValue());
    try ctx.setGlobal(set_atom, set_ctor.toValue());

    // Create Response object with static methods
    const response_obj = try object.JSObject.create(allocator, root_class, null);

    // Register Response static methods using dynamic atoms
    const json_atom = try ctx.atoms.intern("json");
    const text_atom = try ctx.atoms.intern("text");
    const html_atom = try ctx.atoms.intern("html");
    const redirect_atom = try ctx.atoms.intern("redirect");

    const json_func = try object.JSObject.createNativeFunction(allocator, root_class, http.responseJson, json_atom, 1);
    try response_obj.setProperty(allocator, json_atom, json_func.toValue());

    const text_func = try object.JSObject.createNativeFunction(allocator, root_class, http.responseText, text_atom, 1);
    try response_obj.setProperty(allocator, text_atom, text_func.toValue());

    const html_func = try object.JSObject.createNativeFunction(allocator, root_class, http.responseHtml, html_atom, 1);
    try response_obj.setProperty(allocator, html_atom, html_func.toValue());

    const redirect_func = try object.JSObject.createNativeFunction(allocator, root_class, http.responseRedirect, redirect_atom, 1);
    try response_obj.setProperty(allocator, redirect_atom, redirect_func.toValue());

    // Register Response on global (dynamic atom since "Response" isn't predefined)
    const response_atom = try ctx.atoms.intern("Response");
    try ctx.setGlobal(response_atom, response_obj.toValue());

    // Register h() - hyperscript function for JSX
    const h_atom = try ctx.atoms.intern("h");
    const h_func = try object.JSObject.createNativeFunction(allocator, root_class, http.h, h_atom, 2);
    try ctx.setGlobal(h_atom, h_func.toValue());

    // Register renderToString() for SSR
    const render_atom = try ctx.atoms.intern("renderToString");
    const render_func = try object.JSObject.createNativeFunction(allocator, root_class, http.renderToString, render_atom, 1);
    try ctx.setGlobal(render_atom, render_func.toValue());

    // Register Fragment constant for JSX
    const fragment_atom = try ctx.atoms.intern("Fragment");
    const fragment_str = try string.createString(allocator, http.FRAGMENT_MARKER);
    try ctx.setGlobal(fragment_atom, value.JSValue.fromPtr(fragment_str));

    // ========================================================================
    // Array.prototype
    // ========================================================================
    const array_proto = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, array_proto, "push", wrapArrayPush, 1);
    try addMethodDynamic(ctx, array_proto, "pop", wrapArrayPop, 0);
    try addMethodDynamic(ctx, array_proto, "shift", wrapArrayShift, 0);
    try addMethodDynamic(ctx, array_proto, "unshift", wrapArrayUnshift, 1);
    try addMethodDynamic(ctx, array_proto, "indexOf", wrapArrayIndexOf, 1);
    try addMethodDynamic(ctx, array_proto, "includes", wrapArrayIncludes, 1);
    try addMethodDynamic(ctx, array_proto, "join", wrapArrayJoin, 1);
    try addMethodDynamic(ctx, array_proto, "reverse", wrapArrayReverse, 0);
    try addMethodDynamic(ctx, array_proto, "slice", wrapArraySlice, 2);
    try addMethodDynamic(ctx, array_proto, "concat", wrapArrayConcat, 1);
    try addMethodDynamic(ctx, array_proto, "map", wrapArrayMap, 1);
    try addMethodDynamic(ctx, array_proto, "filter", wrapArrayFilter, 1);
    try addMethodDynamic(ctx, array_proto, "reduce", wrapArrayReduce, 2);
    try addMethodDynamic(ctx, array_proto, "forEach", wrapArrayForEach, 1);
    try addMethodDynamic(ctx, array_proto, "every", wrapArrayEvery, 1);
    try addMethodDynamic(ctx, array_proto, "some", wrapArraySome, 1);
    try addMethodDynamic(ctx, array_proto, "find", wrapArrayFind, 1);
    try addMethodDynamic(ctx, array_proto, "findIndex", wrapArrayFindIndex, 1);
    try addMethodDynamic(ctx, array_proto, "fill", wrapArrayFill, 3);
    try addMethodDynamic(ctx, array_proto, "sort", wrapArraySort, 1);
    ctx.array_prototype = array_proto;

    // Create Array constructor function on global
    const array_atom = try ctx.atoms.intern("Array");
    const array_ctor = try object.JSObject.create(allocator, root_class, null);
    // Array static methods
    try addMethodDynamic(ctx, array_ctor, "isArray", wrapArrayIsArray, 1);
    try addMethodDynamic(ctx, array_ctor, "from", wrapArrayFrom, 1);
    try addMethodDynamic(ctx, array_ctor, "of", wrapArrayOf, 0);
    try ctx.setGlobal(array_atom, array_ctor.toValue());

    // ========================================================================
    // String.prototype
    // ========================================================================
    const string_proto = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, string_proto, "charAt", wrapStringCharAt, 1);
    try addMethodDynamic(ctx, string_proto, "charCodeAt", wrapStringCharCodeAt, 1);
    try addMethodDynamic(ctx, string_proto, "indexOf", wrapStringIndexOf, 1);
    try addMethodDynamic(ctx, string_proto, "lastIndexOf", wrapStringLastIndexOf, 1);
    try addMethodDynamic(ctx, string_proto, "startsWith", wrapStringStartsWith, 1);
    try addMethodDynamic(ctx, string_proto, "endsWith", wrapStringEndsWith, 1);
    try addMethodDynamic(ctx, string_proto, "includes", wrapStringIncludes, 1);
    try addMethodDynamic(ctx, string_proto, "slice", wrapStringSlice, 2);
    try addMethodDynamic(ctx, string_proto, "substring", wrapStringSubstring, 2);
    try addMethodDynamic(ctx, string_proto, "toLowerCase", wrapStringToLowerCase, 0);
    try addMethodDynamic(ctx, string_proto, "toUpperCase", wrapStringToUpperCase, 0);
    try addMethodDynamic(ctx, string_proto, "trim", wrapStringTrim, 0);
    try addMethodDynamic(ctx, string_proto, "trimStart", wrapStringTrimStart, 0);
    try addMethodDynamic(ctx, string_proto, "trimEnd", wrapStringTrimEnd, 0);
    try addMethodDynamic(ctx, string_proto, "split", wrapStringSplit, 2);
    try addMethodDynamic(ctx, string_proto, "repeat", wrapStringRepeat, 1);
    try addMethodDynamic(ctx, string_proto, "padStart", wrapStringPadStart, 2);
    try addMethodDynamic(ctx, string_proto, "padEnd", wrapStringPadEnd, 2);
    try addMethodDynamic(ctx, string_proto, "concat", wrapStringConcat, 1);
    try addMethodDynamic(ctx, string_proto, "replace", wrapStringReplace, 2);
    try addMethodDynamic(ctx, string_proto, "replaceAll", wrapStringReplaceAll, 2);
    ctx.string_prototype = string_proto;

    // Create String constructor function on global
    const string_atom = try ctx.atoms.intern("String");
    const string_ctor = try object.JSObject.create(allocator, root_class, null);
    // String.fromCharCode static method
    try addMethodDynamic(ctx, string_ctor, "fromCharCode", wrapStringFromCharCode, 1);
    try ctx.setGlobal(string_atom, string_ctor.toValue());

    // Create RegExp prototype with test/exec methods
    const regexp_proto = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, regexp_proto, "test", wrapRegExpTest, 1);
    try addMethodDynamic(ctx, regexp_proto, "exec", wrapRegExpExec, 1);

    // Create RegExp constructor
    const regexp_atom = try ctx.atoms.intern("RegExp");
    const regexp_ctor = try object.JSObject.createNativeFunction(allocator, root_class, wrapRegExpConstructor, regexp_atom, 2);
    try regexp_ctor.setProperty(allocator, .prototype, regexp_proto.toValue());
    try ctx.setGlobal(regexp_atom, regexp_ctor.toValue());

    // Create Generator prototype with next/return/throw methods
    const generator_proto = try object.JSObject.create(allocator, root_class, null);
    try addMethodDynamic(ctx, generator_proto, "next", wrapGeneratorNext, 1);
    try addMethodDynamic(ctx, generator_proto, "return", wrapGeneratorReturn, 1);
    try addMethodDynamic(ctx, generator_proto, "throw", wrapGeneratorThrow, 1);
    ctx.generator_prototype = generator_proto;
}

// ============================================================================
// RegExp implementation
// ============================================================================

/// RegExp constructor - new RegExp(pattern, flags) or RegExp(pattern, flags)
pub fn regExpConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;

    // Get pattern string
    var pattern: []const u8 = "";
    if (args.len > 0) {
        if (args[0].isString()) {
            const str = args[0].toPtr(string.JSString);
            pattern = str.data();
        }
    }

    // Get flags string
    var flags_str: []const u8 = "";
    if (args.len > 1) {
        if (args[1].isString()) {
            const str = args[1].toPtr(string.JSString);
            flags_str = str.data();
        }
    }

    // Create RegExp object
    const regexp_obj = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;

    // Store pattern and flags as properties
    const source_atom = ctx.atoms.intern("source") catch return value.JSValue.undefined_val;
    const flags_atom = ctx.atoms.intern("flags") catch return value.JSValue.undefined_val;
    const global_atom = ctx.atoms.intern("global") catch return value.JSValue.undefined_val;
    const ignore_case_atom = ctx.atoms.intern("ignoreCase") catch return value.JSValue.undefined_val;
    const multiline_atom = ctx.atoms.intern("multiline") catch return value.JSValue.undefined_val;
    const last_index_atom = ctx.atoms.intern("lastIndex") catch return value.JSValue.undefined_val;
    const _regex_atom = ctx.atoms.intern("_regex") catch return value.JSValue.undefined_val;

    // Create JS strings for source and flags
    const source_js = string.createString(allocator, pattern) catch return value.JSValue.undefined_val;
    const flags_js = string.createString(allocator, flags_str) catch return value.JSValue.undefined_val;

    regexp_obj.setProperty(allocator, source_atom, source_js.toValue()) catch return value.JSValue.undefined_val;
    regexp_obj.setProperty(allocator, flags_atom, flags_js.toValue()) catch return value.JSValue.undefined_val;

    // Parse flags
    const flags = regex.parseFlags(flags_str);
    regexp_obj.setProperty(allocator, global_atom, if (flags.global) value.JSValue.true_val else value.JSValue.false_val) catch return value.JSValue.undefined_val;
    regexp_obj.setProperty(allocator, ignore_case_atom, if (flags.ignore_case) value.JSValue.true_val else value.JSValue.false_val) catch return value.JSValue.undefined_val;
    regexp_obj.setProperty(allocator, multiline_atom, if (flags.multiline) value.JSValue.true_val else value.JSValue.false_val) catch return value.JSValue.undefined_val;
    regexp_obj.setProperty(allocator, last_index_atom, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    // Store compiled regex as internal property
    // For now we just store the pattern and flags separately, compile on demand
    regexp_obj.setProperty(allocator, _regex_atom, value.JSValue.fromInt(@as(i32, @intCast(@intFromEnum(flags.global) | (@as(u8, @intFromEnum(flags.ignore_case)) << 1) | (@as(u8, @intFromEnum(flags.multiline)) << 2) | (@as(u8, @intFromEnum(flags.dot_all)) << 3))))) catch return value.JSValue.undefined_val;

    return regexp_obj.toValue();
}

fn wrapRegExpConstructor(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return regExpConstructor(ctx, this, args);
}

/// RegExp.prototype.test(string) - Returns true if pattern matches
pub fn regExpTest(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.false_val;

    // Get pattern from this
    const regexp_obj = object.JSObject.fromValue(this);
    const source_atom = ctx.atoms.intern("source") catch return value.JSValue.false_val;
    const source_val = regexp_obj.getProperty(source_atom) orelse return value.JSValue.false_val;
    if (!source_val.isString()) return value.JSValue.false_val;

    const pattern = source_val.toPtr(string.JSString).data();

    // Get flags
    const _regex_atom = ctx.atoms.intern("_regex") catch return value.JSValue.false_val;
    const flags_val = regexp_obj.getProperty(_regex_atom) orelse value.JSValue.fromInt(0);
    const flags_int: u8 = if (flags_val.isInt()) @intCast(flags_val.getInt()) else 0;
    const flags = regex.Flags{
        .global = (flags_int & 1) != 0,
        .ignore_case = (flags_int & 2) != 0,
        .multiline = (flags_int & 4) != 0,
        .dot_all = (flags_int & 8) != 0,
    };

    // Get input string
    if (!args[0].isString()) return value.JSValue.false_val;
    const input = args[0].toPtr(string.JSString).data();

    // Compile and test
    const compiled = regex.Regex.compile(pattern, flags);
    return if (compiled.match(input)) value.JSValue.true_val else value.JSValue.false_val;
}

fn wrapRegExpTest(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return regExpTest(ctx, this, args);
}

/// RegExp.prototype.exec(string) - Returns match array or null
pub fn regExpExec(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.null_val;

    if (!this.isObject() or args.len < 1) return value.JSValue.null_val;

    // Get pattern from this
    const regexp_obj = object.JSObject.fromValue(this);
    const source_atom = ctx.atoms.intern("source") catch return value.JSValue.null_val;
    const source_val = regexp_obj.getProperty(source_atom) orelse return value.JSValue.null_val;
    if (!source_val.isString()) return value.JSValue.null_val;

    const pattern = source_val.toPtr(string.JSString).data();

    // Get flags
    const _regex_atom = ctx.atoms.intern("_regex") catch return value.JSValue.null_val;
    const flags_val = regexp_obj.getProperty(_regex_atom) orelse value.JSValue.fromInt(0);
    const flags_int: u8 = if (flags_val.isInt()) @intCast(flags_val.getInt()) else 0;
    const flags = regex.Flags{
        .global = (flags_int & 1) != 0,
        .ignore_case = (flags_int & 2) != 0,
        .multiline = (flags_int & 4) != 0,
        .dot_all = (flags_int & 8) != 0,
    };

    // Get input string
    if (!args[0].isString()) return value.JSValue.null_val;
    const input = args[0].toPtr(string.JSString).data();

    // Compile and exec
    const compiled = regex.Regex.compile(pattern, flags);
    const match_result = compiled.exec(input);

    if (match_result) |match| {
        // Create result array
        const result_arr = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.null_val;

        // Set matched string as index 0
        const matched_str = string.createString(allocator, match.slice()) catch return value.JSValue.null_val;
        result_arr.setSlot(0, matched_str.toValue());
        result_arr.setProperty(allocator, .length, value.JSValue.fromInt(1)) catch return value.JSValue.null_val;

        // Set index property
        const index_atom = ctx.atoms.intern("index") catch return result_arr.toValue();
        result_arr.setProperty(allocator, index_atom, value.JSValue.fromInt(@intCast(match.start))) catch return result_arr.toValue();

        // Set input property
        const input_atom = ctx.atoms.intern("input") catch return result_arr.toValue();
        result_arr.setProperty(allocator, input_atom, args[0]) catch return result_arr.toValue();

        return result_arr.toValue();
    }

    return value.JSValue.null_val;
}

fn wrapRegExpExec(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return regExpExec(ctx, this, args);
}

/// Create a RegExp object from pattern and flags strings (used by parser for literals)
pub fn createRegExp(ctx: *context.Context, pattern: []const u8, flags_str: []const u8) !*object.JSObject {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return error.NoRootClass;

    const regexp_obj = try object.JSObject.create(allocator, root_class, null);

    const source_atom = try ctx.atoms.intern("source");
    const flags_atom = try ctx.atoms.intern("flags");
    const global_atom = try ctx.atoms.intern("global");
    const ignore_case_atom = try ctx.atoms.intern("ignoreCase");
    const multiline_atom = try ctx.atoms.intern("multiline");
    const last_index_atom = try ctx.atoms.intern("lastIndex");
    const _regex_atom = try ctx.atoms.intern("_regex");

    const source_js = try string.createString(allocator, pattern);
    const flags_js = try string.createString(allocator, flags_str);

    try regexp_obj.setProperty(allocator, source_atom, source_js.toValue());
    try regexp_obj.setProperty(allocator, flags_atom, flags_js.toValue());

    const flags = regex.parseFlags(flags_str);
    try regexp_obj.setProperty(allocator, global_atom, if (flags.global) value.JSValue.true_val else value.JSValue.false_val);
    try regexp_obj.setProperty(allocator, ignore_case_atom, if (flags.ignore_case) value.JSValue.true_val else value.JSValue.false_val);
    try regexp_obj.setProperty(allocator, multiline_atom, if (flags.multiline) value.JSValue.true_val else value.JSValue.false_val);
    try regexp_obj.setProperty(allocator, last_index_atom, value.JSValue.fromInt(0));

    const flags_packed: u8 = @intFromBool(flags.global) | (@as(u8, @intFromBool(flags.ignore_case)) << 1) | (@as(u8, @intFromBool(flags.multiline)) << 2) | (@as(u8, @intFromBool(flags.dot_all)) << 3);
    try regexp_obj.setProperty(allocator, _regex_atom, value.JSValue.fromInt(@as(i32, flags_packed)));

    return regexp_obj;
}

// ============================================================================
// Generator implementation
// ============================================================================

const interpreter = @import("interpreter.zig");

/// Generator.prototype.next(value) - Advance generator to next yield
pub fn generatorNext(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;

    if (!this.isObject()) return value.JSValue.undefined_val;

    const gen_obj = object.JSObject.fromValue(this);
    if (gen_obj.class_id != .generator) return value.JSValue.undefined_val;

    // Create an interpreter instance for running the generator
    var interp = interpreter.Interpreter.init(ctx);

    const result = interp.runGenerator(gen_obj) catch {
        // Return {value: undefined, done: true} on error
        const result_obj = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;
        result_obj.setProperty(allocator, .value, value.JSValue.undefined_val) catch return value.JSValue.undefined_val;
        const done_atom = ctx.atoms.intern("done") catch return value.JSValue.undefined_val;
        result_obj.setProperty(allocator, done_atom, value.JSValue.true_val) catch return value.JSValue.undefined_val;
        return result_obj.toValue();
    };

    // Create iterator result object {value, done}
    const result_obj = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;
    result_obj.setProperty(allocator, .value, result.value) catch return value.JSValue.undefined_val;
    const done_atom = ctx.atoms.intern("done") catch return value.JSValue.undefined_val;
    result_obj.setProperty(allocator, done_atom, if (result.done) value.JSValue.true_val else value.JSValue.false_val) catch return value.JSValue.undefined_val;

    return result_obj.toValue();
}

fn wrapGeneratorNext(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return generatorNext(ctx, this, args);
}

/// Generator.prototype.return(value) - Force generator to return
pub fn generatorReturn(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;

    if (!this.isObject()) return value.JSValue.undefined_val;

    const gen_obj = object.JSObject.fromValue(this);
    if (gen_obj.class_id != .generator) return value.JSValue.undefined_val;

    // Mark generator as completed
    const gen_data = gen_obj.getGeneratorData() orelse return value.JSValue.undefined_val;
    gen_data.state = .completed;

    // Return {value, done: true}
    const return_val = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    const result_obj = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;
    result_obj.setProperty(allocator, .value, return_val) catch return value.JSValue.undefined_val;
    const done_atom = ctx.atoms.intern("done") catch return value.JSValue.undefined_val;
    result_obj.setProperty(allocator, done_atom, value.JSValue.true_val) catch return value.JSValue.undefined_val;

    return result_obj.toValue();
}

fn wrapGeneratorReturn(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return generatorReturn(ctx, this, args);
}

/// Generator.prototype.throw(error) - Throw error into generator
pub fn generatorThrow(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    const allocator = ctx.allocator;
    const root_class = ctx.root_class orelse return value.JSValue.undefined_val;

    if (!this.isObject()) return value.JSValue.undefined_val;

    const gen_obj = object.JSObject.fromValue(this);
    if (gen_obj.class_id != .generator) return value.JSValue.undefined_val;

    // Mark generator as completed (simple implementation - doesn't handle try/catch in generator)
    const gen_data = gen_obj.getGeneratorData() orelse return value.JSValue.undefined_val;
    gen_data.state = .completed;

    // Return {value: undefined, done: true}
    const result_obj = object.JSObject.create(allocator, root_class, null) catch return value.JSValue.undefined_val;
    result_obj.setProperty(allocator, .value, value.JSValue.undefined_val) catch return value.JSValue.undefined_val;
    const done_atom = ctx.atoms.intern("done") catch return value.JSValue.undefined_val;
    result_obj.setProperty(allocator, done_atom, value.JSValue.true_val) catch return value.JSValue.undefined_val;

    return result_obj.toValue();
}

fn wrapGeneratorThrow(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));
    return generatorThrow(ctx, this, args);
}

test "Math.abs" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Integer abs
    const result = mathAbs(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(-42)});
    try std.testing.expectEqual(@as(i32, 42), result.getInt());

    // Positive stays positive
    const result2 = mathAbs(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(10)});
    try std.testing.expectEqual(@as(i32, 10), result2.getInt());
}

test "Math.min/max" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const args = [_]value.JSValue{
        value.JSValue.fromInt(5),
        value.JSValue.fromInt(3),
        value.JSValue.fromInt(8),
    };

    const min_result = mathMin(ctx, value.JSValue.undefined_val, &args);
    try std.testing.expectEqual(@as(i32, 3), min_result.getInt());

    const max_result = mathMax(ctx, value.JSValue.undefined_val, &args);
    try std.testing.expectEqual(@as(i32, 8), max_result.getInt());
}

test "Math.floor/ceil/round" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // floor(5) = 5
    const floor_result = mathFloor(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(5)});
    try std.testing.expectEqual(@as(i32, 5), floor_result.getInt());

    // ceil(5) = 5
    const ceil_result = mathCeil(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(5)});
    try std.testing.expectEqual(@as(i32, 5), ceil_result.getInt());

    // round(5) = 5
    const round_result = mathRound(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(5)});
    try std.testing.expectEqual(@as(i32, 5), round_result.getInt());
}

test "Math.pow" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // 2^3 = 8
    const result = mathPow(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(2),
        value.JSValue.fromInt(3),
    });
    try std.testing.expectEqual(@as(i32, 8), result.getInt());

    // 3^2 = 9
    const result2 = mathPow(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(3),
        value.JSValue.fromInt(2),
    });
    try std.testing.expectEqual(@as(i32, 9), result2.getInt());
}

test "Math.sqrt" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // sqrt(16) = 4
    const result = mathSqrt(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(16)});
    try std.testing.expectEqual(@as(i32, 4), result.getInt());

    // sqrt(9) = 3
    const result2 = mathSqrt(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(9)});
    try std.testing.expectEqual(@as(i32, 3), result2.getInt());
}

test "Math.sign" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // sign(10) = 1
    const pos = mathSign(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(10)});
    try std.testing.expectEqual(@as(i32, 1), pos.getInt());

    // sign(-5) = -1
    const neg = mathSign(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(-5)});
    try std.testing.expectEqual(@as(i32, -1), neg.getInt());

    // sign(0) = 0
    const zero = mathSign(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(0)});
    try std.testing.expectEqual(@as(i32, 0), zero.getInt());
}

test "Math constants" {
    // PI
    try std.testing.expect(math_constants.PI > 3.14 and math_constants.PI < 3.15);
    // E
    try std.testing.expect(math_constants.E > 2.71 and math_constants.E < 2.72);
    // SQRT2
    try std.testing.expect(math_constants.SQRT2 > 1.41 and math_constants.SQRT2 < 1.42);
}

test "Object.isFrozen" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var root_class = try object.HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try object.JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    // Object is extensible by default
    const obj_val = obj.toValue();
    const not_frozen = objectIsFrozen(ctx, value.JSValue.undefined_val, &[_]value.JSValue{obj_val});
    try std.testing.expect(not_frozen.isFalse());

    // Freeze the object
    _ = objectFreeze(ctx, value.JSValue.undefined_val, &[_]value.JSValue{obj_val});
    const is_frozen = objectIsFrozen(ctx, value.JSValue.undefined_val, &[_]value.JSValue{obj_val});
    try std.testing.expect(is_frozen.isTrue());
}

test "Array.isArray" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Regular object is not an array
    var root_class = try object.HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var obj = try object.JSObject.create(allocator, root_class, null);
    defer obj.destroy(allocator);

    const not_array = arrayIsArray(ctx, value.JSValue.undefined_val, &[_]value.JSValue{obj.toValue()});
    try std.testing.expect(not_array.isFalse());

    // Non-object values
    const int_not_array = arrayIsArray(ctx, value.JSValue.undefined_val, &[_]value.JSValue{value.JSValue.fromInt(42)});
    try std.testing.expect(int_not_array.isFalse());

    // Empty args
    const empty = arrayIsArray(ctx, value.JSValue.undefined_val, &[_]value.JSValue{});
    try std.testing.expect(empty.isFalse());
}

test "Array.slice" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var root_class = try object.HiddenClass.init(allocator);
    defer root_class.deinit(allocator);

    var arr = try object.JSObject.create(allocator, root_class, null);
    defer arr.destroy(allocator);

    // Set length = 10
    try arr.setProperty(allocator, .length, value.JSValue.fromInt(10));
    defer allocator.free(arr.hidden_class.properties);
    defer arr.hidden_class.deinit(allocator);

    // slice(2, 5) should return length 3
    const result = arraySlice(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.fromInt(2),
        value.JSValue.fromInt(5),
    });
    try std.testing.expectEqual(@as(i32, 3), result.getInt());

    // slice(-3) from end
    const from_end = arraySlice(ctx, arr.toValue(), &[_]value.JSValue{
        value.JSValue.fromInt(-3),
    });
    try std.testing.expectEqual(@as(i32, 3), from_end.getInt());
}

test "String.slice" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create a test string
    const str = try string.createString(allocator, "hello world");
    defer string.freeString(allocator, str);

    const str_val = value.JSValue.fromPtr(str);

    // slice(0, 5) = "hello" (length 5)
    const result = stringSlice(ctx, str_val, &[_]value.JSValue{
        value.JSValue.fromInt(0),
        value.JSValue.fromInt(5),
    });
    try std.testing.expectEqual(@as(i32, 5), result.getInt());

    // slice(6) = "world" (length 5)
    const from_6 = stringSlice(ctx, str_val, &[_]value.JSValue{
        value.JSValue.fromInt(6),
    });
    try std.testing.expectEqual(@as(i32, 5), from_6.getInt());
}

test "String.charCodeAt" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "ABC");
    defer string.freeString(allocator, str);

    const str_val = value.JSValue.fromPtr(str);

    // charCodeAt(0) = 65 ('A')
    const a_code = stringCharCodeAt(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(0)});
    try std.testing.expectEqual(@as(i32, 65), a_code.getInt());

    // charCodeAt(1) = 66 ('B')
    const b_code = stringCharCodeAt(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(1)});
    try std.testing.expectEqual(@as(i32, 66), b_code.getInt());

    // charCodeAt(2) = 67 ('C')
    const c_code = stringCharCodeAt(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(2)});
    try std.testing.expectEqual(@as(i32, 67), c_code.getInt());
}

test "String.repeat" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const str = try string.createString(allocator, "ab");
    defer string.freeString(allocator, str);

    const str_val = value.JSValue.fromPtr(str);

    // repeat(3) = "ababab" (length 6)
    const result = stringRepeat(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(3)});
    try std.testing.expectEqual(@as(i32, 6), result.getInt());

    // repeat(0) = "" (length 0)
    const empty = stringRepeat(ctx, str_val, &[_]value.JSValue{value.JSValue.fromInt(0)});
    try std.testing.expectEqual(@as(i32, 0), empty.getInt());
}

test "initBuiltins registers console and Math" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Initialize built-ins
    try initBuiltins(ctx);

    // Check console is registered on global
    const console_val = ctx.getGlobal(.console);
    try std.testing.expect(console_val != null);
    try std.testing.expect(console_val.?.isObject());

    // Check Math is registered on global
    const math_val = ctx.getGlobal(.Math);
    try std.testing.expect(math_val != null);
    try std.testing.expect(math_val.?.isObject());

    // Check Math has abs method
    const math_obj = object.JSObject.fromValue(math_val.?);
    const abs_val = math_obj.getProperty(.abs);
    try std.testing.expect(abs_val != null);
    try std.testing.expect(abs_val.?.isCallable());

    // Check Response is registered
    const response_atom = try ctx.atoms.intern("Response");
    const response_val = ctx.getGlobal(response_atom);
    try std.testing.expect(response_val != null);
    try std.testing.expect(response_val.?.isObject());

    // Check Response.json exists
    const response_obj = object.JSObject.fromValue(response_val.?);
    const json_atom = try ctx.atoms.intern("json");
    const json_method = response_obj.getProperty(json_atom);
    try std.testing.expect(json_method != null);
    try std.testing.expect(json_method.?.isCallable());

    // Check h() is registered
    const h_atom = try ctx.atoms.intern("h");
    const h_val = ctx.getGlobal(h_atom);
    try std.testing.expect(h_val != null);
    try std.testing.expect(h_val.?.isCallable());

    // Check renderToString is registered
    const render_atom = try ctx.atoms.intern("renderToString");
    const render_val = ctx.getGlobal(render_atom);
    try std.testing.expect(render_val != null);
    try std.testing.expect(render_val.?.isCallable());

    // Check Fragment is registered
    const fragment_atom = try ctx.atoms.intern("Fragment");
    const fragment_val = ctx.getGlobal(fragment_atom);
    try std.testing.expect(fragment_val != null);
    try std.testing.expect(fragment_val.?.isString());
}

test "Number.isInteger" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Integer value should return true
    const int_result = numberIsInteger(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });
    try std.testing.expect(int_result.isBool());
    try std.testing.expect(int_result.getBool() == true);

    // Float with fractional part should return false
    const float_box = try gc_state.allocFloat(3.14);
    const float_result = numberIsInteger(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(float_box),
    });
    try std.testing.expect(float_result.isBool());
    try std.testing.expect(float_result.getBool() == false);

    // Float that equals integer should return true
    const int_float_box = try gc_state.allocFloat(42.0);
    const int_float_result = numberIsInteger(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(int_float_box),
    });
    try std.testing.expect(int_float_result.isBool());
    try std.testing.expect(int_float_result.getBool() == true);
}

test "Number.isNaN" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // NaN should return true
    const nan_result = numberIsNaN(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.nan_val,
    });
    try std.testing.expect(nan_result.isBool());
    try std.testing.expect(nan_result.getBool() == true);

    // Regular number should return false
    const num_result = numberIsNaN(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });
    try std.testing.expect(num_result.isBool());
    try std.testing.expect(num_result.getBool() == false);
}

test "Number.isFinite" {
    const gc = @import("gc.zig");
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Regular integer should return true
    const int_result = numberIsFinite(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromInt(42),
    });
    try std.testing.expect(int_result.isBool());
    try std.testing.expect(int_result.getBool() == true);

    // Infinity should return false
    const inf_box = try gc_state.allocFloat(std.math.inf(f64));
    const inf_result = numberIsFinite(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.fromPtr(inf_box),
    });
    try std.testing.expect(inf_result.isBool());
    try std.testing.expect(inf_result.getBool() == false);

    // NaN should return false
    const nan_result = numberIsFinite(ctx, value.JSValue.undefined_val, &[_]value.JSValue{
        value.JSValue.nan_val,
    });
    try std.testing.expect(nan_result.isBool());
    try std.testing.expect(nan_result.getBool() == false);
}
