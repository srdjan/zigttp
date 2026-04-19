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
