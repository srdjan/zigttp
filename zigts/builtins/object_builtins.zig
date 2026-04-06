const h = @import("helpers.zig");
const value = h.value;
const object = h.object;
const context = h.context;

const getObject = h.getObject;
const atomName = h.atomName;
const createArrayWithPrototype = h.createArrayWithPrototype;

/// Object.keys(obj) - Returns array of own enumerable property names
pub fn objectKeys(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const keys = obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    const result = createArrayWithPrototype(ctx) orelse return value.JSValue.undefined_val;
    for (keys) |key| {
        const name = atomName(key, ctx) orelse continue;
        const str_val = ctx.createString(name) catch continue;
        result.arrayPush(ctx.allocator, str_val) catch continue;
    }
    return result.toValue();
}

/// Object.values(obj) - Returns array of own enumerable property values
pub fn objectValues(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const keys = obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    const result = createArrayWithPrototype(ctx) orelse return value.JSValue.undefined_val;
    for (keys) |key| {
        const val = obj.getProperty(pool, key) orelse value.JSValue.undefined_val;
        result.arrayPush(ctx.allocator, val) catch continue;
    }
    return result.toValue();
}

/// Object.entries(obj) - Returns array of [key, value] pairs
pub fn objectEntries(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const keys = obj.getOwnEnumerableKeys(ctx.allocator, pool) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    const result = createArrayWithPrototype(ctx) orelse return value.JSValue.undefined_val;
    for (keys) |key| {
        const name = atomName(key, ctx) orelse continue;
        const pair = createArrayWithPrototype(ctx) orelse continue;
        const str_val = ctx.createString(name) catch continue;
        pair.arrayPush(ctx.allocator, str_val) catch continue;
        const val = obj.getProperty(pool, key) orelse value.JSValue.undefined_val;
        pair.arrayPush(ctx.allocator, val) catch continue;
        result.arrayPush(ctx.allocator, pair.toValue()) catch continue;
    }
    return result.toValue();
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
