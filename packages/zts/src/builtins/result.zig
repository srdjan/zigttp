const std = @import("std");
const gc = @import("../gc.zig");
const h = @import("helpers.zig");
const value = h.value;
const object = h.object;
const context = h.context;
const http = h.http;
const string = h.string;

const createResultOk = h.createResultOk;
const createResultErr = h.createResultErr;
const createResultErrWithField = h.createResultErrWithField;
const getCallFn = h.getCallFn;
const getCallbackArg = h.getCallbackArg;
const getObject = h.getObject;
const invokeCallback = h.invokeCallback;

// ============================================================================
// Result type implementation - functional error handling
// ============================================================================

/// Result slot layout:
/// slot[0] = isOk (boolean)
/// slot[1] = payload (the ok or err value)
/// slot[2] = error field atom (`error`/`errors`) or undefined for ok
fn getNativeResult(this: value.JSValue) ?*object.JSObject {
    if (!this.isObject()) return null;
    const obj = object.JSObject.fromValue(this);
    if (obj.class_id != .result) return null;
    return obj;
}

fn resultErrorFieldAtom(obj: *const object.JSObject) object.Atom {
    return obj.resultErrorFieldAtom() orelse .@"error";
}

fn coerceResultLike(ctx: *context.Context, val: value.JSValue) ?value.JSValue {
    const pool = ctx.hidden_class_pool orelse return null;
    const obj = getObject(val) orelse return null;
    if (obj.class_id == .result) return val;

    const ok_val = obj.getProperty(pool, .ok) orelse return null;
    if (!ok_val.isBool()) return null;

    if (ok_val.getBool()) {
        const payload = obj.getProperty(pool, .value) orelse value.JSValue.undefined_val;
        return createResultOk(ctx, payload);
    }

    if (obj.getProperty(pool, .@"error")) |err_val| {
        return createResultErr(ctx, err_val);
    }
    if (obj.getProperty(pool, .errors)) |err_val| {
        return createResultErrWithField(ctx, err_val, .errors);
    }
    return createResultErr(ctx, value.JSValue.undefined_val);
}

fn invokeResultCallback(
    callback: *object.JSObject,
    arg: value.JSValue,
) ?value.JSValue {
    const call_fn = getCallFn() orelse return null;
    const call_args = [_]value.JSValue{arg};
    return invokeCallback(call_fn, callback, &call_args);
}

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
    const obj = getNativeResult(this) orelse return value.JSValue.false_val;
    return obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK];
}

/// Result.prototype.isErr() - returns true if result is err
pub fn resultIsErr(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    const obj = getNativeResult(this) orelse return value.JSValue.true_val;
    // Return opposite of isOk
    return if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) value.JSValue.false_val else value.JSValue.true_val;
}

/// Result.prototype.unwrap() - returns value if ok, undefined if err
pub fn resultUnwrap(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    const obj = getNativeResult(this) orelse return value.JSValue.undefined_val;

    if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) {
        return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    }
    return value.JSValue.undefined_val;
}

/// Result.prototype.unwrapOr(default) - returns value if ok, default if err
pub fn resultUnwrapOr(_: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getNativeResult(this) orelse return if (args.len > 0) args[0] else value.JSValue.undefined_val;

    if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) {
        return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    }
    return if (args.len > 0) args[0] else value.JSValue.undefined_val;
}

/// Result.prototype.unwrapErr() - returns error if err, undefined if ok
pub fn resultUnwrapErr(_: *context.Context, this: value.JSValue, _: []const value.JSValue) value.JSValue {
    const obj = getNativeResult(this) orelse return value.JSValue.undefined_val;

    if (!obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) {
        return obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    }
    return value.JSValue.undefined_val;
}

/// Result.prototype.map(fn) - transform ok value, pass through err
pub fn resultMap(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getNativeResult(this) orelse return this;
    if (!obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) return this;
    const callback = getCallbackArg(args) orelse return this;
    const payload = obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    const mapped = invokeResultCallback(callback, payload) orelse return value.JSValue.undefined_val;
    return createResultOk(ctx, mapped);
}

/// Result.prototype.mapErr(fn) - transform err value, pass through ok
pub fn resultMapErr(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getNativeResult(this) orelse return this;
    if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) return this;
    const callback = getCallbackArg(args) orelse return this;
    const payload = obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    const mapped = invokeResultCallback(callback, payload) orelse return value.JSValue.undefined_val;
    return createResultErrWithField(ctx, mapped, resultErrorFieldAtom(obj));
}

/// Result.prototype.andThen(fn) - transform ok value to another Result
pub fn resultAndThen(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getNativeResult(this) orelse return this;
    if (!obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue()) return this;
    const callback = getCallbackArg(args) orelse return this;
    const payload = obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];
    const next = invokeResultCallback(callback, payload) orelse return value.JSValue.undefined_val;
    return coerceResultLike(ctx, next) orelse value.JSValue.undefined_val;
}

/// Result.prototype.match({ok: fn, err: fn}) - pattern match on result
pub fn resultMatch(ctx: *context.Context, this: value.JSValue, args: []const value.JSValue) value.JSValue {
    const obj = getNativeResult(this) orelse return value.JSValue.undefined_val;
    if (args.len == 0) return value.JSValue.undefined_val;

    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
    const cases = getObject(args[0]) orelse return value.JSValue.undefined_val;
    const payload = obj.inline_slots[object.JSObject.Slots.RESULT_VALUE];

    const case_val = if (obj.inline_slots[object.JSObject.Slots.RESULT_IS_OK].isTrue())
        cases.getProperty(pool, .ok)
    else if (cases.getProperty(pool, .err)) |err_case|
        err_case
    else if (cases.getProperty(pool, .@"error")) |err_case|
        err_case
    else
        cases.getProperty(pool, .errors);

    const callback = case_val orelse return value.JSValue.undefined_val;
    if (!callback.isCallable()) return value.JSValue.undefined_val;
    return invokeResultCallback(callback.toPtr(object.JSObject), payload) orelse value.JSValue.undefined_val;
}

var active_test_ctx: ?*context.Context = null;

fn dispatchTestCallback(func: *object.JSObject, args: []const value.JSValue) anyerror!value.JSValue {
    const data = func.getNativeFunctionData() orelse return error.InvalidFunction;
    return data.func(@ptrFromInt(1), value.JSValue.undefined_val, args);
}

fn testAddOne(_: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    if (args.len == 0 or !args[0].isInt()) return value.JSValue.undefined_val;
    return value.JSValue.fromInt(args[0].getInt() + 1);
}

fn testWrapErr(_: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = active_test_ctx orelse return value.JSValue.undefined_val;
    const payload = if (args.len > 0) args[0] else value.JSValue.undefined_val;
    return createResultErr(ctx, payload);
}

fn testReturnPayload(_: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    return if (args.len > 0) args[0] else value.JSValue.undefined_val;
}

test "native Result exposes compatibility fields" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const pool = ctx.hidden_class_pool.?;
    const ok_result = createResultOk(ctx, value.JSValue.fromInt(7));
    defer object.JSObject.fromValue(ok_result).destroy(allocator);
    const ok_obj = object.JSObject.fromValue(ok_result);
    try std.testing.expect(ok_obj.getProperty(pool, .ok).?.isTrue());
    try std.testing.expectEqual(@as(i32, 7), ok_obj.getProperty(pool, .value).?.getInt());

    const err_payload = try ctx.createString("boom");
    defer string.freeString(allocator, err_payload.toPtr(string.JSString));
    const err_result = createResultErrWithField(ctx, err_payload, .errors);
    defer object.JSObject.fromValue(err_result).destroy(allocator);
    const err_obj = object.JSObject.fromValue(err_result);
    try std.testing.expect(err_obj.getProperty(pool, .ok).?.isFalse());
    try std.testing.expect(err_obj.getProperty(pool, .value).?.isUndefined());
    try std.testing.expect(err_obj.getProperty(pool, .errors).?.isString());

    const keys = try err_obj.getOwnEnumerableKeys(allocator, pool);
    defer allocator.free(keys);
    try std.testing.expectEqual(@as(usize, 2), keys.len);
    try std.testing.expectEqual(object.Atom.ok, keys[0]);
    try std.testing.expectEqual(object.Atom.errors, keys[1]);
}

test "native Result map mapErr and andThen use callback support" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const pool = ctx.hidden_class_pool.?;
    active_test_ctx = ctx;
    defer active_test_ctx = null;
    http.setCallFunctionCallback(dispatchTestCallback);
    defer http.clearCallFunctionCallback();

    const add_one = try object.JSObject.createNativeFunction(allocator, pool, ctx.root_class_idx, testAddOne, .map, 1);
    defer add_one.destroyFull(allocator);
    const wrap_err = try object.JSObject.createNativeFunction(allocator, pool, ctx.root_class_idx, testWrapErr, .andThen, 1);
    defer wrap_err.destroyFull(allocator);
    const return_payload = try object.JSObject.createNativeFunction(allocator, pool, ctx.root_class_idx, testReturnPayload, .match, 1);
    defer return_payload.destroyFull(allocator);

    const ok_result = createResultOk(ctx, value.JSValue.fromInt(41));
    defer object.JSObject.fromValue(ok_result).destroy(allocator);
    const mapped = resultMap(ctx, ok_result, &.{add_one.toValue()});
    defer object.JSObject.fromValue(mapped).destroy(allocator);
    try std.testing.expect(resultIsOk(ctx, mapped, &.{}).isTrue());
    try std.testing.expectEqual(@as(i32, 42), resultUnwrap(ctx, mapped, &.{}).getInt());

    const err_payload = try ctx.createString("bad");
    defer string.freeString(allocator, err_payload.toPtr(string.JSString));
    const err_result = createResultErr(ctx, err_payload);
    defer object.JSObject.fromValue(err_result).destroy(allocator);
    const mapped_err = resultMapErr(ctx, err_result, &.{return_payload.toValue()});
    defer object.JSObject.fromValue(mapped_err).destroy(allocator);
    try std.testing.expect(resultIsErr(ctx, mapped_err, &.{}).isTrue());
    try std.testing.expect(mapped_err.toPtr(object.JSObject).getProperty(pool, .@"error").?.isString());

    const chained = resultAndThen(ctx, ok_result, &.{wrap_err.toValue()});
    defer object.JSObject.fromValue(chained).destroy(allocator);
    try std.testing.expect(resultIsErr(ctx, chained, &.{}).isTrue());

    const cases = try ctx.createObject(null);
    defer cases.destroy(allocator);
    try ctx.setPropertyChecked(cases, .ok, return_payload.toValue());
    try ctx.setPropertyChecked(cases, .err, return_payload.toValue());
    const matched = resultMatch(ctx, chained, &.{cases.toValue()});
    try std.testing.expect(matched.isInt());
    try std.testing.expectEqual(@as(i32, 41), matched.getInt());
}
