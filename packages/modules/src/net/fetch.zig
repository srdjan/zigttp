//! zigttp:fetch - web-standard outbound HTTP.
//!
//! The native fn only validates arguments, fetches the installed runtime
//! callback, and delegates. The callback is populated during runtime
//! bootstrap from zigts side (see packages/zigts/src/modules/net/fetch.zig).

const sdk = @import("zigttp-sdk");
const util = @import("../internal/util.zig");

pub const MODULE_STATE_SLOT: usize = 11; // module_slots.Slot.fetch

pub const FetchCallFn = *const fn (
    runtime_ptr: *anyopaque,
    handle: *sdk.ModuleHandle,
    args: []const sdk.JSValue,
) anyerror!sdk.JSValue;

pub const FetchState = struct {
    runtime_ptr: *anyopaque,
    call_fn: FetchCallFn,
};

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:fetch",
    .name = "fetch",
    .required_capabilities = &.{ .network, .runtime_callback },
    .stateful = true,
    .exports = &.{
        .{
            .name = "fetch",
            .module_func = fetchImpl,
            .arg_count = 2,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .object },
            .return_labels = .{ .external = true },
            .contract_extractions = &.{
                .{ .arg_position = 0, .category = .fetch_host, .transform = .extract_host },
            },
        },
        .{
            .name = "get",
            .module_func = getImpl,
            .arg_count = 2,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .object },
            .return_labels = .{ .external = true },
            .contract_extractions = &.{
                .{ .arg_position = 0, .category = .fetch_host, .transform = .extract_host },
            },
        },
        .{
            .name = "post",
            .module_func = postImpl,
            .arg_count = 2,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .object },
            .return_labels = .{ .external = true },
            .contract_extractions = &.{
                .{ .arg_position = 0, .category = .fetch_host, .transform = .extract_host },
            },
        },
        .{
            .name = "put",
            .module_func = putImpl,
            .arg_count = 2,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .object },
            .return_labels = .{ .external = true },
            .contract_extractions = &.{
                .{ .arg_position = 0, .category = .fetch_host, .transform = .extract_host },
            },
        },
        .{
            .name = "patch",
            .module_func = patchImpl,
            .arg_count = 2,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .object },
            .return_labels = .{ .external = true },
            .contract_extractions = &.{
                .{ .arg_position = 0, .category = .fetch_host, .transform = .extract_host },
            },
        },
        .{
            .name = "delete",
            .module_func = deleteImpl,
            .arg_count = 2,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .object },
            .return_labels = .{ .external = true },
            .contract_extractions = &.{
                .{ .arg_position = 0, .category = .fetch_host, .transform = .extract_host },
            },
        },
    },
};

fn fetchImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    const state = sdk.getModuleState(handle, FetchState, MODULE_STATE_SLOT) orelse {
        return sdk.throwError(handle, "Error", "fetch() requires runtime installation (no runtime callback wired)");
    };
    if (args.len == 0) return util.throwTypeError(handle, "fetch() requires a URL string or init object");
    try sdk.requireCapability(handle, .runtime_callback);
    return state.call_fn(state.runtime_ptr, handle, args);
}

fn getImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    return fetchWithMethod(handle, args, "GET");
}

fn postImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    return fetchWithMethod(handle, args, "POST");
}

fn putImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    return fetchWithMethod(handle, args, "PUT");
}

fn patchImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    return fetchWithMethod(handle, args, "PATCH");
}

fn deleteImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    return fetchWithMethod(handle, args, "DELETE");
}

fn fetchWithMethod(handle: *sdk.ModuleHandle, args: []const sdk.JSValue, method: []const u8) anyerror!sdk.JSValue {
    const state = sdk.getModuleState(handle, FetchState, MODULE_STATE_SLOT) orelse {
        return sdk.throwError(handle, "Error", "fetch helpers require runtime installation (no runtime callback wired)");
    };
    if (args.len == 0) return util.throwTypeError(handle, "fetch helper requires a URL string");
    const url = args[0];
    if (sdk.extractString(url) == null) return util.throwTypeError(handle, "fetch helper url must be a string");

    const init = if (args.len > 1) args[1] else sdk.JSValue.undefined_val;
    if (!init.isUndefined() and !init.isNull() and !sdk.isObject(init)) {
        return util.throwTypeError(handle, "fetch helper init must be an object");
    }

    const final_init = if (sdk.isObject(init)) try cloneWithMethod(handle, init, method) else blk: {
        const obj = try sdk.createObject(handle);
        try sdk.objectSet(handle, obj, "method", try sdk.createString(handle, method));
        break :blk obj;
    };

    const forwarded = [_]sdk.JSValue{ url, final_init };
    try sdk.requireCapability(handle, .runtime_callback);
    return state.call_fn(state.runtime_ptr, handle, &forwarded);
}

fn cloneWithMethod(handle: *sdk.ModuleHandle, init: sdk.JSValue, method: []const u8) !sdk.JSValue {
    const obj = try sdk.createObject(handle);
    const keys = try sdk.objectKeys(handle, init);
    const key_count = sdk.arrayLength(keys) orelse 0;
    var i: u32 = 0;
    while (i < key_count) : (i += 1) {
        const key_val = sdk.arrayGet(handle, keys, i) orelse continue;
        const key = sdk.extractString(key_val) orelse continue;
        const val = sdk.objectGet(handle, init, key) orelse continue;
        try sdk.objectSet(handle, obj, key, val);
    }
    try sdk.objectSet(handle, obj, "method", try sdk.createString(handle, method));
    return obj;
}
