const std = @import("std");
const h = @import("helpers.zig");
const value = h.value;
const object = h.object;
const context = h.context;
const string = h.string;

const valueToStringSimple = h.valueToStringSimple;

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
pub fn createErrorObject(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue, error_name: []const u8) value.JSValue {
    // If called with 'new', use the provided 'this' object
    // Otherwise create a new error object
    const obj: *object.JSObject = if (this.isObject() and !this.isUndefined())
        object.JSObject.fromValue(this)
    else blk: {
        break :blk ctx.createObject(ctx.object_prototype) catch return value.JSValue.undefined_val;
    };

    // Set the 'name' property (return undefined on allocation failure)
    const name_val = ctx.createString(error_name) catch return value.JSValue.undefined_val;
    ctx.setPropertyChecked(obj, .name, name_val) catch return value.JSValue.undefined_val;

    // Set the 'message' property
    if (args.len > 0 and args[0].isString()) {
        ctx.setPropertyChecked(obj, .message, args[0]) catch return value.JSValue.undefined_val;
    } else if (args.len > 0) {
        // Convert to string
        const msg_str = valueToStringSimple(ctx.allocator, args[0]) catch {
            const empty_val = ctx.createString("") catch return value.JSValue.undefined_val;
            ctx.setPropertyChecked(obj, .message, empty_val) catch return value.JSValue.undefined_val;
            return obj.toValue();
        };
        ctx.setPropertyChecked(obj, .message, value.JSValue.fromPtr(msg_str)) catch return value.JSValue.undefined_val;
    } else {
        const empty_val = ctx.createString("") catch return value.JSValue.undefined_val;
        ctx.setPropertyChecked(obj, .message, empty_val) catch return value.JSValue.undefined_val;
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
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    // Get name
    var name_text: []const u8 = "Error";
    if (obj.getProperty(pool, .name)) |name_val| {
        if (name_val.isString()) {
            name_text = name_val.toPtr(string.JSString).data();
        }
    }

    // Get message
    var msg_text: []const u8 = "";
    if (obj.getProperty(pool, .message)) |msg_val| {
        if (msg_val.isString()) {
            msg_text = msg_val.toPtr(string.JSString).data();
        }
    }

    // Format as "Name: message" or just "Name" if no message
    if (msg_text.len == 0) {
        return ctx.createString(name_text) catch return value.JSValue.undefined_val;
    }

    // Concatenate: name + ": " + message
    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(ctx.allocator);
    buffer.appendSlice(ctx.allocator, name_text) catch return value.JSValue.undefined_val;
    buffer.appendSlice(ctx.allocator, ": ") catch return value.JSValue.undefined_val;
    buffer.appendSlice(ctx.allocator, msg_text) catch return value.JSValue.undefined_val;

    return ctx.createString(buffer.items) catch return value.JSValue.undefined_val;
}
