const h = @import("helpers.zig");
const context = h.context;
const value = h.value;
const object = h.object;

// Aliased helpers for use in this module
const getObject = h.getObject;
const getStringData = h.getStringData;

// ============================================================================
// Set implementation
// ============================================================================

/// Set constructor - new Set() or new Set(iterable)
pub fn setConstructor(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    // Create the Set object
    const set_obj = ctx.createObject(null) catch return value.JSValue.undefined_val;

    // Internal storage: _values array
    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    // Create internal array (proper array for efficient indexed access)
    const values_arr = ctx.createArray() catch return value.JSValue.undefined_val;

    ctx.setPropertyChecked(set_obj, values_atom, values_arr.toValue()) catch return value.JSValue.undefined_val;
    ctx.setPropertyChecked(set_obj, size_atom, value.JSValue.fromInt(0)) catch return value.JSValue.undefined_val;

    // If iterable provided, add values
    if (args.len > 0 and args[0].isObject()) {
        // TODO: Handle iterable initialization
    }

    return set_obj.toValue();
}

/// Set.prototype.add(value)
pub fn setAdd(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return this;

    const set_obj = object.JSObject.fromValue(this);
    const val = args[0];
    const pool = ctx.hidden_class_pool orelse return this;

    const values_atom = ctx.atoms.intern("_values") catch return this;
    const size_atom = ctx.atoms.intern("size") catch return this;

    const values_val = set_obj.getProperty(pool, values_atom) orelse return this;
    if (!values_val.isObject()) return this;

    const values_arr = object.JSObject.fromValue(values_val);
    const len = values_arr.getArrayLength();

    // Check if value already exists
    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (values_arr.getIndex(i)) |existing| {
            if (existing.strictEquals(val)) {
                return this; // Already exists
            }
        }
    }

    // Add new value
    ctx.setIndexChecked(values_arr, len, val) catch return this;
    ctx.setPropertyChecked(set_obj, size_atom, value.JSValue.fromInt(@as(i32, @intCast(len)) + 1)) catch return this;

    return this;
}

/// Set.prototype.has(value)
pub fn setHas(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const set_obj = object.JSObject.fromValue(this);
    const val = args[0];
    const pool = ctx.hidden_class_pool orelse return value.JSValue.fromBool(false);

    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.fromBool(false);
    const values_val = set_obj.getProperty(pool, values_atom) orelse return value.JSValue.fromBool(false);

    if (!values_val.isObject()) return value.JSValue.fromBool(false);

    const values_arr = object.JSObject.fromValue(values_val);
    const len = values_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (values_arr.getIndex(i)) |existing| {
            if (existing.strictEquals(val)) {
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Set.prototype.delete(value)
pub fn setDelete(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject() or args.len < 1) return value.JSValue.fromBool(false);

    const set_obj = object.JSObject.fromValue(this);
    const val = args[0];
    const pool = ctx.hidden_class_pool orelse return value.JSValue.fromBool(false);

    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.fromBool(false);
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.fromBool(false);

    const values_val = set_obj.getProperty(pool, values_atom) orelse return value.JSValue.fromBool(false);
    if (!values_val.isObject()) return value.JSValue.fromBool(false);

    const values_arr = object.JSObject.fromValue(values_val);
    const len = values_arr.getArrayLength();

    var i: u32 = 0;
    while (i < len) : (i += 1) {
        if (values_arr.getIndex(i)) |existing| {
            if (existing.strictEquals(val)) {
                // Shift remaining elements
                var j = i;
                while (j < len - 1) : (j += 1) {
                    if (values_arr.getIndex(j + 1)) |next_val| {
                        ctx.setIndexChecked(values_arr, j, next_val) catch {};
                    }
                }
                values_arr.setArrayLength(len - 1);
                ctx.setPropertyChecked(set_obj, size_atom, value.JSValue.fromInt(@as(i32, @intCast(len)) - 1)) catch {};
                return value.JSValue.fromBool(true);
            }
        }
    }

    return value.JSValue.fromBool(false);
}

/// Set.prototype.clear()
pub fn setClear(ctx: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;

    const set_obj = object.JSObject.fromValue(this);
    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    const values_atom = ctx.atoms.intern("_values") catch return value.JSValue.undefined_val;
    const size_atom = ctx.atoms.intern("size") catch return value.JSValue.undefined_val;

    const values_val = set_obj.getProperty(pool, values_atom) orelse return value.JSValue.undefined_val;
    if (!values_val.isObject()) return value.JSValue.undefined_val;

    const values_arr = object.JSObject.fromValue(values_val);
    values_arr.setArrayLength(0);
    ctx.setPropertyChecked(set_obj, size_atom, value.JSValue.fromInt(0)) catch {};

    return value.JSValue.undefined_val;
}
