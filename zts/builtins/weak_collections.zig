const std = @import("std");
const h = @import("helpers.zig");
const value = h.value;
const object = h.object;
const context = h.context;

const wrap = h.wrap;
const addMethodDynamic = h.addMethodDynamic;

// ============================================================================
// WeakMap implementation
// ============================================================================

/// WeakMap uses object identity as keys with weak references
/// Simplified implementation - uses a regular map but with object pointer keys
pub const WeakMapData = struct {
    entries: std.AutoHashMap(usize, value.JSValue),

    pub fn init(allocator: std.mem.Allocator) WeakMapData {
        return .{
            .entries = std.AutoHashMap(usize, value.JSValue).init(allocator),
        };
    }

    pub fn deinit(self: *WeakMapData) void {
        self.entries.deinit();
    }

    pub fn get(self: *WeakMapData, key: value.JSValue) ?value.JSValue {
        if (!key.isObject()) return null;
        const ptr_key = key.getPtrAddress();
        return self.entries.get(ptr_key);
    }

    pub fn set(self: *WeakMapData, key: value.JSValue, val: value.JSValue) !void {
        if (!key.isObject()) return error.InvalidKey;
        const ptr_key = key.getPtrAddress();
        try self.entries.put(ptr_key, val);
    }

    pub fn has(self: *WeakMapData, key: value.JSValue) bool {
        if (!key.isObject()) return false;
        const ptr_key = key.getPtrAddress();
        return self.entries.contains(ptr_key);
    }

    pub fn delete(self: *WeakMapData, key: value.JSValue) bool {
        if (!key.isObject()) return false;
        const ptr_key = key.getPtrAddress();
        return self.entries.remove(ptr_key);
    }
};

/// WeakMap constructor - new WeakMap()
pub fn weakMapConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Create WeakMap object
    const weak_map = ctx.createObject(null) catch return value.JSValue.undefined_val;
    weak_map.class_id = .weak_map;

    // Allocate and store WeakMapData
    const data_allocator = if (ctx.hybrid) |hybrid| hybrid.arena.allocator() else ctx.allocator;
    const data = if (ctx.hybrid) |hybrid|
        hybrid.arena.create(WeakMapData) orelse return value.JSValue.undefined_val
    else
        ctx.allocator.create(WeakMapData) catch return value.JSValue.undefined_val;
    data.* = WeakMapData.init(data_allocator);
    weak_map.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA] = value.JSValue.fromExternPtr(data);

    // If iterable argument provided, add entries
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Iterate over argument and add entries
    }

    return weak_map.toValue();
}

/// WeakMap.prototype.get(key)
pub fn weakMapGet(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    if (args.len == 0) return value.JSValue.undefined_val;

    const map_obj = object.JSObject.fromValue(this);
    if (map_obj.class_id != .weak_map) return value.JSValue.undefined_val;

    const data_val = map_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.undefined_val;
    const data: *WeakMapData = @ptrCast(@alignCast(data_val.toExternPtr(WeakMapData)));

    return data.get(args[0]) orelse value.JSValue.undefined_val;
}

/// WeakMap.prototype.set(key, value)
pub fn weakMapSet(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    if (args.len < 2) return this;

    const map_obj = object.JSObject.fromValue(this);
    if (map_obj.class_id != .weak_map) return value.JSValue.undefined_val;

    const data_val = map_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.undefined_val;
    const data: *WeakMapData = @ptrCast(@alignCast(data_val.toExternPtr(WeakMapData)));

    data.set(args[0], args[1]) catch return value.JSValue.undefined_val;
    return this; // Return the WeakMap for chaining
}

/// WeakMap.prototype.has(key)
pub fn weakMapHas(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    if (args.len == 0) return value.JSValue.false_val;

    const map_obj = object.JSObject.fromValue(this);
    if (map_obj.class_id != .weak_map) return value.JSValue.false_val;

    const data_val = map_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.false_val;
    const data: *WeakMapData = @ptrCast(@alignCast(data_val.toExternPtr(WeakMapData)));

    return if (data.has(args[0])) value.JSValue.true_val else value.JSValue.false_val;
}

/// WeakMap.prototype.delete(key)
pub fn weakMapDelete(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    if (args.len == 0) return value.JSValue.false_val;

    const map_obj = object.JSObject.fromValue(this);
    if (map_obj.class_id != .weak_map) return value.JSValue.false_val;

    const data_val = map_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.false_val;
    const data: *WeakMapData = @ptrCast(@alignCast(data_val.toExternPtr(WeakMapData)));

    return if (data.delete(args[0])) value.JSValue.true_val else value.JSValue.false_val;
}

// ============================================================================
// WeakSet implementation
// ============================================================================

/// WeakSet uses object identity with weak references
pub const WeakSetData = struct {
    entries: std.AutoHashMap(usize, void),

    pub fn init(allocator: std.mem.Allocator) WeakSetData {
        return .{
            .entries = std.AutoHashMap(usize, void).init(allocator),
        };
    }

    pub fn deinit(self: *WeakSetData) void {
        self.entries.deinit();
    }

    pub fn add(self: *WeakSetData, val: value.JSValue) !void {
        if (!val.isObject()) return error.InvalidValue;
        const ptr_key = val.getPtrAddress();
        try self.entries.put(ptr_key, {});
    }

    pub fn has(self: *WeakSetData, val: value.JSValue) bool {
        if (!val.isObject()) return false;
        const ptr_key = val.getPtrAddress();
        return self.entries.contains(ptr_key);
    }

    pub fn delete(self: *WeakSetData, val: value.JSValue) bool {
        if (!val.isObject()) return false;
        const ptr_key = val.getPtrAddress();
        return self.entries.remove(ptr_key);
    }
};

/// WeakSet constructor - new WeakSet()
pub fn weakSetConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Create WeakSet object
    const weak_set = ctx.createObject(null) catch return value.JSValue.undefined_val;
    weak_set.class_id = .weak_set;

    // Allocate and store WeakSetData
    const data_allocator = if (ctx.hybrid) |hybrid| hybrid.arena.allocator() else ctx.allocator;
    const data = if (ctx.hybrid) |hybrid|
        hybrid.arena.create(WeakSetData) orelse return value.JSValue.undefined_val
    else
        ctx.allocator.create(WeakSetData) catch return value.JSValue.undefined_val;
    data.* = WeakSetData.init(data_allocator);
    weak_set.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA] = value.JSValue.fromExternPtr(data);

    // If iterable argument provided, add entries
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Iterate over argument and add entries
    }

    return weak_set.toValue();
}

/// WeakSet.prototype.add(value)
pub fn weakSetAdd(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    if (args.len == 0) return this;

    const set_obj = object.JSObject.fromValue(this);
    if (set_obj.class_id != .weak_set) return value.JSValue.undefined_val;

    const data_val = set_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.undefined_val;
    const data: *WeakSetData = @ptrCast(@alignCast(data_val.toExternPtr(WeakSetData)));

    data.add(args[0]) catch return value.JSValue.undefined_val;
    return this; // Return the WeakSet for chaining
}

/// WeakSet.prototype.has(value)
pub fn weakSetHas(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    if (args.len == 0) return value.JSValue.false_val;

    const set_obj = object.JSObject.fromValue(this);
    if (set_obj.class_id != .weak_set) return value.JSValue.false_val;

    const data_val = set_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.false_val;
    const data: *WeakSetData = @ptrCast(@alignCast(data_val.toExternPtr(WeakSetData)));

    return if (data.has(args[0])) value.JSValue.true_val else value.JSValue.false_val;
}

/// WeakSet.prototype.delete(value)
pub fn weakSetDelete(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    if (args.len == 0) return value.JSValue.false_val;

    const set_obj = object.JSObject.fromValue(this);
    if (set_obj.class_id != .weak_set) return value.JSValue.false_val;

    const data_val = set_obj.inline_slots[object.JSObject.Slots.WEAK_COLLECTION_DATA];
    if (!data_val.isExternPtr()) return value.JSValue.false_val;
    const data: *WeakSetData = @ptrCast(@alignCast(data_val.toExternPtr(WeakSetData)));

    return if (data.delete(args[0])) value.JSValue.true_val else value.JSValue.false_val;
}

/// Initialize WeakMap and WeakSet built-ins
pub fn initWeakCollections(ctx: *context.Context) !void {
    const allocator = ctx.allocator;
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const root_class_idx = ctx.root_class_idx;

    // Create WeakMap prototype
    const weak_map_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, weak_map_proto, "get", wrap(weakMapGet), 1);
    try addMethodDynamic(ctx, weak_map_proto, "set", wrap(weakMapSet), 2);
    try addMethodDynamic(ctx, weak_map_proto, "has", wrap(weakMapHas), 1);
    try addMethodDynamic(ctx, weak_map_proto, "delete", wrap(weakMapDelete), 1);

    // Create WeakMap constructor
    const weak_map_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(weakMapConstructor), .WeakMap, 0);
    try ctx.setPropertyChecked(weak_map_ctor, .prototype, weak_map_proto.toValue());
    try ctx.setGlobal(.WeakMap, weak_map_ctor.toValue());

    // Create WeakSet prototype
    const weak_set_proto = try object.JSObject.create(allocator, root_class_idx, null, pool);
    try addMethodDynamic(ctx, weak_set_proto, "add", wrap(weakSetAdd), 1);
    try addMethodDynamic(ctx, weak_set_proto, "has", wrap(weakSetHas), 1);
    try addMethodDynamic(ctx, weak_set_proto, "delete", wrap(weakSetDelete), 1);

    // Create WeakSet constructor
    const weak_set_ctor = try object.JSObject.createNativeFunction(allocator, pool, root_class_idx, wrap(weakSetConstructor), .WeakSet, 0);
    try ctx.setPropertyChecked(weak_set_ctor, .prototype, weak_set_proto.toValue());
    try ctx.setGlobal(.WeakSet, weak_set_ctor.toValue());
}
