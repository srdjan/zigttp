const h = @import("helpers.zig");
const value = h.value;
const object = h.object;
const context = h.context;

const createResultOk = h.createResultOk;
const createResultErr = h.createResultErr;

// ============================================================================
// Result type implementation - functional error handling
// ============================================================================

/// Result slot layout:
/// slot[0] = isOk (boolean)
/// slot[1] = value (the ok or err value)
/// Create a Result.ok(value)
pub fn resultOk(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const val = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    return createResultOk(ctx, val);
}

/// Create a Result.err(error)
pub fn resultErr(ctx: *context.Context, _: value.JSValue, args: []const value.JSValue) value.JSValue {
    const err_val = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    return createResultErr(ctx, err_val);
}

/// Result.prototype.isOk() - returns true if result is ok
pub fn resultIsOk(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.false_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.false_val;
    return obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK];
}

/// Result.prototype.isErr() - returns true if result is err
pub fn resultIsErr(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.true_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.true_val;
    // Return opposite of isOk
    return if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) value.JSValue.false_val else value.JSValue.true_val;
}

/// Result.prototype.unwrap() - returns value if ok, undefined if err
pub fn resultUnwrap(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.undefined_val;

    if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) {
        return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    }
    return value.JSValue.undefined_val;
}

/// Result.prototype.unwrapOr(default) - returns value if ok, default if err
pub fn resultUnwrapOr(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    if (!this.isObject()) {
        return if (args.len > 0) args[0] else value.JSValue.undefined_val;
    }
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) {
        return if (args.len > 0) args[0] else value.JSValue.undefined_val;
    }

    if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) {
        return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    }
    return if (args.len > 0) args[0] else value.JSValue.undefined_val;
}

/// Result.prototype.unwrapErr() - returns error if err, undefined if ok
pub fn resultUnwrapErr(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.undefined_val;

    if (!obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) {
        return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    }
    return value.JSValue.undefined_val;
}

/// Result.prototype.map(fn) - transform ok value, pass through err
/// Note: Requires callback infrastructure - returns self for now
pub fn resultMap(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    // TODO: Implement when callback infrastructure is available
    return this;
}

/// Result.prototype.mapErr(fn) - transform err value, pass through ok
/// Note: Requires callback infrastructure - returns self for now
pub fn resultMapErr(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    // TODO: Implement when callback infrastructure is available
    return this;
}

/// Result.prototype.match({ok: fn, err: fn}) - pattern match on result
/// Note: Requires callback infrastructure - returns value for now
pub fn resultMatch(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    if (!this.isObject()) return value.JSValue.undefined_val;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return value.JSValue.undefined_val;
    // TODO: Implement when callback infrastructure is available
    // For now just return the inner value
    return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
}
