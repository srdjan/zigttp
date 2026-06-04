const std = @import("std");
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

/// Object.hasOwn(obj, prop) - Check if object has own property
pub fn objectHasOwn(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len < 2) return value.JSValue.false_val;

    const obj = getObject(args[0]) orelse return value.JSValue.false_val;
    const pool = ctx.hidden_class_pool orelse return value.JSValue.false_val;

    const key = h.getStringDataCtx(args[1], ctx) orelse return value.JSValue.false_val;
    const atom = ctx.atoms.intern(key) catch return value.JSValue.false_val;

    return value.JSValue.fromBool(obj.hasOwnProperty(pool, atom));
}

test "objectHasOwn reports own properties ENG6" {
    // Regression for ENG-6: Object.hasOwn was a stub that always returned false.
    // It must intern the property-name argument and report real ownership.
    var dbg: std.heap.DebugAllocator(.{}) = .init;
    const allocator = dbg.allocator();
    const gc_mod = @import("../gc.zig");
    const heap_mod = @import("../heap.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const obj = try ctx.createObject(null);
    const a_atom = try ctx.atoms.intern("a");
    try ctx.setPropertyChecked(obj, a_atom, value.JSValue.fromInt(1));

    const a_key = try ctx.createString("a");
    const b_key = try ctx.createString("b");

    const has_a = objectHasOwn(ctx, value.JSValue.undefined_val, &.{ obj.toValue(), a_key });
    const has_b = objectHasOwn(ctx, value.JSValue.undefined_val, &.{ obj.toValue(), b_key });

    try std.testing.expect(has_a.isTrue());
    try std.testing.expect(has_b.isFalse());
}
