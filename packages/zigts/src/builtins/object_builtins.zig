const std = @import("std");
const h = @import("helpers.zig");
const value = h.value;
const object = h.object;
const context = h.context;

const getObject = h.getObject;
const atomName = h.atomName;
const createArrayWithPrototype = h.createArrayWithPrototype;

const IndexedKey = struct {
    atom: object.Atom,
    index: u32,
};

fn canonicalArrayIndex(name: []const u8) ?u32 {
    if (name.len == 0) return null;
    if (name.len > 1 and name[0] == '0') return null;
    for (name) |c| {
        if (c < '0' or c > '9') return null;
    }
    const index = std.fmt.parseInt(u32, name, 10) catch return null;
    if (index == std.math.maxInt(u32)) return null;
    return index;
}

fn indexedKeyLessThan(_: void, a: IndexedKey, b: IndexedKey) bool {
    return a.index < b.index;
}

fn appendArrayIndexKey(ctx: *context.Context, index_keys: *std.ArrayList(IndexedKey), index: u32) !void {
    var buf: [10]u8 = undefined;
    const name = try std.fmt.bufPrint(&buf, "{d}", .{index});
    const atom = try ctx.atoms.intern(name);
    try index_keys.append(ctx.allocator, .{ .atom = atom, .index = index });
}

fn appendDenseArrayIndexKeys(ctx: *context.Context, obj: *object.JSObject, index_keys: *std.ArrayList(IndexedKey)) !void {
    if (!obj.isArray()) return;
    const len = obj.getArrayLength();

    const inline_start = object.JSObject.Slots.ARRAY_ELEMENTS_START;
    const inline_count = @min(
        object.JSObject.INLINE_SLOT_COUNT - inline_start,
        @as(usize, @intCast(len)),
    );
    for (0..inline_count) |offset| {
        const slot = inline_start + offset;
        const val = obj.inline_slots[slot];
        if (!val.isUndefined()) {
            try appendArrayIndexKey(ctx, index_keys, @intCast(offset));
        }
    }

    const overflow_base_index: u32 = object.JSObject.INLINE_SLOT_COUNT - object.JSObject.Slots.ARRAY_ELEMENTS_START;
    if (len <= overflow_base_index) return;
    const overflow_len = @min(
        @as(usize, obj.overflow_capacity),
        @as(usize, @intCast(len - overflow_base_index)),
    );
    if (obj.overflow_slots) |slots| {
        for (0..overflow_len) |offset| {
            const val = slots[offset];
            if (!val.isUndefined()) {
                try appendArrayIndexKey(ctx, index_keys, overflow_base_index + @as(u32, @intCast(offset)));
            }
        }
    }
}

fn getOwnEnumerableKeysForBuiltins(ctx: *context.Context, obj: *object.JSObject, pool: *const object.HiddenClassPool) ![]object.Atom {
    var index_keys = std.ArrayList(IndexedKey).empty;
    defer index_keys.deinit(ctx.allocator);
    var string_keys = std.ArrayList(object.Atom).empty;
    defer string_keys.deinit(ctx.allocator);

    try appendDenseArrayIndexKeys(ctx, obj, &index_keys);

    const own_keys = try obj.getOwnEnumerableKeys(ctx.allocator, pool);
    defer ctx.allocator.free(own_keys);
    for (own_keys) |key| {
        const name = atomName(key, ctx) orelse continue;
        if (canonicalArrayIndex(name)) |index| {
            if (obj.isArray() and obj.getIndex(index) != null) continue;
            try index_keys.append(ctx.allocator, .{ .atom = key, .index = index });
        } else {
            try string_keys.append(ctx.allocator, key);
        }
    }

    std.mem.sort(IndexedKey, index_keys.items, {}, indexedKeyLessThan);

    const result = try ctx.allocator.alloc(object.Atom, index_keys.items.len + string_keys.items.len);
    var out_i: usize = 0;
    for (index_keys.items) |key| {
        result[out_i] = key.atom;
        out_i += 1;
    }
    for (string_keys.items) |key| {
        result[out_i] = key;
        out_i += 1;
    }
    return result;
}

fn getEnumerableValue(ctx: *context.Context, obj: *object.JSObject, pool: *const object.HiddenClassPool, key: object.Atom) value.JSValue {
    if (obj.isArray()) {
        if (atomName(key, ctx)) |name| {
            if (canonicalArrayIndex(name)) |index| {
                if (obj.getIndex(index)) |val| return val;
            }
        }
    }
    return obj.getProperty(pool, key) orelse value.JSValue.undefined_val;
}

/// Object.keys(obj) - Returns array of own enumerable property names
pub fn objectKeys(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    _ = this;
    if (args.len == 0) return value.JSValue.undefined_val;

    const obj = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const keys = getOwnEnumerableKeysForBuiltins(ctx, obj, pool) catch return value.JSValue.undefined_val;
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
    const keys = getOwnEnumerableKeysForBuiltins(ctx, obj, pool) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    const result = createArrayWithPrototype(ctx) orelse return value.JSValue.undefined_val;
    for (keys) |key| {
        const val = getEnumerableValue(ctx, obj, pool, key);
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
    const keys = getOwnEnumerableKeysForBuiltins(ctx, obj, pool) catch return value.JSValue.undefined_val;
    defer ctx.allocator.free(keys);

    const result = createArrayWithPrototype(ctx) orelse return value.JSValue.undefined_val;
    for (keys) |key| {
        const name = atomName(key, ctx) orelse continue;
        const pair = createArrayWithPrototype(ctx) orelse continue;
        const str_val = ctx.createString(name) catch continue;
        pair.arrayPush(ctx.allocator, str_val) catch continue;
        const val = getEnumerableValue(ctx, obj, pool, key);
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

test "Object enumeration orders array indexes before string keys" {
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
    try ctx.setPropertyChecked(obj, try ctx.atoms.intern("b"), value.JSValue.fromInt(3));
    try ctx.setPropertyChecked(obj, try ctx.atoms.intern("2"), value.JSValue.fromInt(20));
    try ctx.setPropertyChecked(obj, try ctx.atoms.intern("1"), value.JSValue.fromInt(10));
    try ctx.setPropertyChecked(obj, try ctx.atoms.intern("a"), value.JSValue.fromInt(4));

    const keys = objectKeys(ctx, value.JSValue.undefined_val, &.{obj.toValue()});
    const key_arr = getObject(keys).?;
    try std.testing.expectEqual(@as(u32, 4), key_arr.getArrayLength());
    try std.testing.expectEqualStrings("1", h.getStringDataCtx(key_arr.getIndexUnchecked(0), ctx).?);
    try std.testing.expectEqualStrings("2", h.getStringDataCtx(key_arr.getIndexUnchecked(1), ctx).?);
    try std.testing.expectEqualStrings("b", h.getStringDataCtx(key_arr.getIndexUnchecked(2), ctx).?);
    try std.testing.expectEqualStrings("a", h.getStringDataCtx(key_arr.getIndexUnchecked(3), ctx).?);

    const values = objectValues(ctx, value.JSValue.undefined_val, &.{obj.toValue()});
    const value_arr = getObject(values).?;
    try std.testing.expectEqual(@as(i32, 10), value_arr.getIndexUnchecked(0).getInt());
    try std.testing.expectEqual(@as(i32, 20), value_arr.getIndexUnchecked(1).getInt());
    try std.testing.expectEqual(@as(i32, 3), value_arr.getIndexUnchecked(2).getInt());
    try std.testing.expectEqual(@as(i32, 4), value_arr.getIndexUnchecked(3).getInt());
}

test "Object enumeration includes dense array indexes" {
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

    const arr = try ctx.createArray();
    arr.prototype = ctx.array_prototype;
    try arr.arrayPush(allocator, value.JSValue.fromInt(7));
    try arr.arrayPush(allocator, value.JSValue.fromInt(8));

    const keys = objectKeys(ctx, value.JSValue.undefined_val, &.{arr.toValue()});
    const key_arr = getObject(keys).?;
    try std.testing.expectEqual(@as(u32, 2), key_arr.getArrayLength());
    try std.testing.expectEqualStrings("0", h.getStringDataCtx(key_arr.getIndexUnchecked(0), ctx).?);
    try std.testing.expectEqualStrings("1", h.getStringDataCtx(key_arr.getIndexUnchecked(1), ctx).?);

    const values = objectValues(ctx, value.JSValue.undefined_val, &.{arr.toValue()});
    const value_arr = getObject(values).?;
    try std.testing.expectEqual(@as(i32, 7), value_arr.getIndexUnchecked(0).getInt());
    try std.testing.expectEqual(@as(i32, 8), value_arr.getIndexUnchecked(1).getInt());
}

test "Object enumeration skips array holes and length-only slots" {
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

    const length_only = try ctx.createArray();
    length_only.prototype = ctx.array_prototype;
    length_only.setArrayLength(1_000_000);
    const empty_keys = objectKeys(ctx, value.JSValue.undefined_val, &.{length_only.toValue()});
    try std.testing.expectEqual(@as(u32, 0), getObject(empty_keys).?.getArrayLength());

    const sparse = try ctx.createArray();
    sparse.prototype = ctx.array_prototype;
    try ctx.setIndexChecked(sparse, 2, value.JSValue.fromInt(42));
    const keys = objectKeys(ctx, value.JSValue.undefined_val, &.{sparse.toValue()});
    const key_arr = getObject(keys).?;
    try std.testing.expectEqual(@as(u32, 1), key_arr.getArrayLength());
    try std.testing.expectEqualStrings("2", h.getStringDataCtx(key_arr.getIndexUnchecked(0), ctx).?);

    const values = objectValues(ctx, value.JSValue.undefined_val, &.{sparse.toValue()});
    const value_arr = getObject(values).?;
    try std.testing.expectEqual(@as(u32, 1), value_arr.getArrayLength());
    try std.testing.expectEqual(@as(i32, 42), value_arr.getIndexUnchecked(0).getInt());
}
