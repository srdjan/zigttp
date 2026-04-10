const h = @import("helpers.zig");
const context = h.context;
const value = h.value;
const object = h.object;

// Aliased helpers for use in this module
const getObject = h.getObject;
const getStringData = h.getStringData;

// ============================================================================
// Map implementation
// ============================================================================

/// Map constructor - new Map() or new Map(iterable)
pub fn mapConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Create the Map object
    const map_obj = ctx.createObject(null) catch return value.JSValue.undefined_val;

    // Internal storage: _keys and _values arrays
    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.undefined_val;
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    // Create internal arrays (proper arrays for efficient indexed access)
    const keys_arr = ctx.createArray() catch return value.JSValue.undefined_val;
    const values_arr = ctx.createArray() catch return value.JSValue.undefined_val;

    ctx.setPropertyChecked(map_obj, keys_atom, keys_arr.toValue()) catch return value.JSValue.undefined_val;
    ctx.setPropertyChecked(map_obj, values_atom, values_arr.toValue()) catch return value.JSValue.undefined_val;
    ctx.setPropertyChecked(map_obj, size_atom, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    // If iterable provided, add entries
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Handle iterable initialization
    }

    return map_obj.toValue();
}

/// Map.prototype.set(key, value)
pub fn mapSet(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 2) return this;

    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];
    const val = args[1];
    const pool = ctx.hidden_class_pool orelse return this;

    const keys_atom = ctx.atoms.intern("_keys") catch return this;
    const values_atom = ctx.atoms.intern("_values") catch return this;
    const size_atom = ctx.atoms.intern("size") catch return this;

    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return this;
    const values_val = map_obj.getProperty(pool, values_atom) orelse return this;

    if (!keys_val.isObject() or !values_val.isObject()) return this;

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    const len = keys_arr.getArrayLength();

    // Check if key already exists
    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (keys_arr.getIndex(i)) |existing_key| {
            if (existing_key.strictEquals(key)) {
                // Update existing value
                ctx.setIndexChecked(values_arr, i, val) catch return this;
                return this;
            }
        }
    }

    // Add new entry
    ctx.setIndexChecked(keys_arr, len, key) catch return this;
    ctx.setIndexChecked(values_arr, len, val) catch return this;
    ctx.setPropertyChecked(map_obj, size_atom, value.JSValue.fromInt(@as(i32, @intCast(len)) + 1)) catch return this;

    return this;
}

/// Map.prototype.get(key)
pub fn mapGet(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.undefined_val;

    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.undefined_val;
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;

    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return value.JSValue.undefined_val;
    const values_val = map_obj.getProperty(pool, values_atom) orelse return value.JSValue.undefined_val;

    if (!keys_val.isObject() or !values_val.isObject()) return value.JSValue.undefined_val;

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    const len = keys_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (keys_arr.getIndex(i)) |existing_key| {
            if (existing_key.strictEquals(key)) {
                return values_arr.getIndex(i) orelse value.JSValue.undefined_val;
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
    const pool = ctx.hidden_class_pool orelse return value.JSValue.fromBool(false);

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.fromBool(false);
    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return value.JSValue.fromBool(false);

    if (!keys_val.isObject()) return value.JSValue.fromBool(false);

    const keys_arr = object.JSObject.fromValue(keys_val);
    const len = keys_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (keys_arr.getIndex(i)) |existing_key| {
            if (existing_key.strictEquals(key)) {
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Map.prototype.delete(key)
pub fn mapDelete(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const map_obj = object.JSObject.fromValue(this);
    const key = args[0];
    const pool = ctx.hidden_class_pool orelse return value.JSValue.fromBool(false);

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.fromBool(false);
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.fromBool(false);
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.fromBool(false);

    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return value.JSValue.fromBool(false);
    const values_val = map_obj.getProperty(pool, values_atom) orelse return value.JSValue.fromBool(false);

    if (!keys_val.isObject() or !values_val.isObject()) return value.JSValue.fromBool(false);

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    const len = keys_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (keys_arr.getIndex(i)) |existing_key| {
            if (existing_key.strictEquals(key)) {
                // Shift remaining elements
                var j = i;
                while (j < len - 1) : (j += 1) {
                    if (keys_arr.getIndex(j + 1)) |next_key| {
                        ctx.setIndexChecked(keys_arr, j, next_key) catch {};
                    }
                    if (values_arr.getIndex(j + 1)) |next_val| {
                        ctx.setIndexChecked(values_arr, j, next_val) catch {};
                    }
                }
                keys_arr.setArrayLength(len - 1);
                values_arr.setArrayLength(len - 1);
                ctx.setPropertyChecked(map_obj, size_atom, value.JSValue.fromInt(@as(i32, @intCast(len)) - 1)) catch {};
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Map.prototype.clear()
pub fn mapClear(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;

    const map_obj = object.JSObject.fromValue(this);
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    const keys_atom = ctx.atoms.intern("_keys") catch return value.JSValue.undefined_val;
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    const keys_val = map_obj.getProperty(pool, keys_atom) orelse return value.JSValue.undefined_val;
    const values_val = map_obj.getProperty(pool, values_atom) orelse return value.JSValue.undefined_val;

    if (!keys_val.isObject() or !values_val.isObject()) return value.JSValue.undefined_val;

    const keys_arr = object.JSObject.fromValue(keys_val);
    const values_arr = object.JSObject.fromValue(values_val);

    keys_arr.setArrayLength(0);
    values_arr.setArrayLength(0);
    ctx.setPropertyChecked(map_obj, size_atom, value.JSValue.fromInt(0)) catch {};

    return value.JSValue.undefined_val;
}
