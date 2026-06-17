//! zigttp:fetch - web-standard outbound HTTP.
//!
//! The native fn only validates arguments, fetches the installed runtime
//! callback, and delegates. The callback is populated during runtime
//! bootstrap from zigts side (see packages/zigts/src/modules/net/fetch.zig).

const std = @import("std");
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
            .name = "fetchWithRetry",
            .module_func = fetchWithRetryImpl,
            .arg_count = 3,
            .required_arg_count = 1,
            .effect = .write,
            .returns = .object,
            .param_types = &.{ .string, .object, .object },
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

// Default retry-on status codes: rate-limited + server errors.
const DEFAULT_RETRY_STATUSES = [_]i32{ 429, 500, 502, 503, 504 };
const DEFAULT_MAX_RETRIES: i32 = 3;
const DEFAULT_BASE_DELAY_MS: i64 = 100;
const DEFAULT_MAX_DELAY_MS: i64 = 5000;
const MAX_ALLOWED_RETRIES: i32 = 10;
const MAX_ALLOWED_BASE_DELAY_MS: i64 = 5000;
const MAX_ALLOWED_DELAY_MS: i64 = 30000;

/// fetchWithRetry(url, init?, retryOptions?)
///
/// retryOptions shape (all optional):
///   maxRetries    - number of additional attempts after the first (default 3)
///   baseDelayMs   - base delay in ms for exponential backoff (default 100)
///   maxDelayMs    - cap on computed delay (default 5000)
///   retryOn       - array of status codes to retry on (default [429,500,502,503,504])
///
/// Safety limits: maxRetries <= 10, baseDelayMs <= 5000, maxDelayMs <= 30000.
/// Negative retry/delay values are normalized to 0.
///
/// Backoff formula: delay = min(baseDelayMs * 2^attempt, maxDelayMs)
/// Delays are implemented with std.time.sleep; this is safe because the JS
/// runtime is single-threaded and handlers are fully synchronous.
fn fetchWithRetryImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    const state = sdk.getModuleState(handle, FetchState, MODULE_STATE_SLOT) orelse {
        return sdk.throwError(handle, "Error", "fetchWithRetry() requires runtime installation (no runtime callback wired)");
    };
    if (args.len == 0) return util.throwTypeError(handle, "fetchWithRetry() requires a URL string");
    try sdk.requireCapability(handle, .runtime_callback);

    // Build the args slice for the underlying fetch call: (url, init).
    const fetch_args = args[0..@min(args.len, 2)];

    // Parse retryOptions from the third argument, if present.
    var max_retries: i32 = DEFAULT_MAX_RETRIES;
    var base_delay_ms: i64 = DEFAULT_BASE_DELAY_MS;
    var max_delay_ms: i64 = DEFAULT_MAX_DELAY_MS;
    // retry_on_custom: null means use DEFAULT_RETRY_STATUSES
    var retry_on_custom: ?sdk.JSValue = null;

    if (args.len >= 3 and sdk.isObject(args[2])) {
        const opts = args[2];
        if (sdk.objectGet(handle, opts, "maxRetries")) |v| {
            if (sdk.extractInt(v)) |n| max_retries = n;
        }
        if (sdk.objectGet(handle, opts, "baseDelayMs")) |v| {
            if (sdk.extractInt(v)) |n| base_delay_ms = @intCast(n);
        }
        if (sdk.objectGet(handle, opts, "maxDelayMs")) |v| {
            if (sdk.extractInt(v)) |n| max_delay_ms = @intCast(n);
        }
        if (sdk.objectGet(handle, opts, "retryOn")) |v| {
            if (sdk.isArray(v)) retry_on_custom = v;
        }
    }

    if (normalizeRetryOptions(&max_retries, &base_delay_ms, &max_delay_ms)) |message| {
        return util.throwTypeError(handle, message);
    }

    var last_response: sdk.JSValue = undefined;
    var attempt: i32 = 0;
    while (true) {
        last_response = try state.call_fn(state.runtime_ptr, handle, fetch_args);

        // Inside a parallel()/race() thunk the underlying fetch is only RECORDED
        // (a descriptor) and returns the `undefined` placeholder, not a real
        // response. Retrying here would re-register the same fetch maxRetries+1
        // times and nanosleep between each during the synchronous collection
        // phase. Return the placeholder immediately; the real fetch runs once in
        // the parallel execute phase.
        if (last_response.isUndefined()) return last_response;

        // If the response has ok=true, return immediately.
        if (sdk.isObject(last_response)) {
            if (sdk.objectGet(handle, last_response, "ok")) |ok_val| {
                if (ok_val.isTrue()) return last_response;
            }
            // Check whether the status code is in the retryOn list.
            const status_val = sdk.objectGet(handle, last_response, "status");
            const status: i32 = if (status_val) |sv| sdk.extractInt(sv) orelse 0 else 0;
            const should_retry = blk: {
                if (retry_on_custom) |arr| {
                    const len_u32 = sdk.arrayLength(arr) orelse break :blk false;
                    var i: u32 = 0;
                    while (i < len_u32) : (i += 1) {
                        const elem = sdk.arrayGet(handle, arr, i) orelse continue;
                        if (sdk.extractInt(elem)) |code| {
                            if (code == status) break :blk true;
                        }
                    }
                    break :blk false;
                } else {
                    for (DEFAULT_RETRY_STATUSES) |code| {
                        if (code == status) break :blk true;
                    }
                    break :blk false;
                }
            };
            if (!should_retry) return last_response;
        } else {
            // Non-object response (network error path): always retry if attempts remain.
        }

        if (attempt >= max_retries) return last_response;

        // Compute exponential delay: base * 2^attempt, capped at max.
        const shift: u6 = @intCast(@min(attempt, 62));
        const multiplier: i64 = @as(i64, 1) << shift;
        const delay = @min(base_delay_ms *| multiplier, max_delay_ms);
        if (delay > 0) {
            const delay_ns: u64 = @intCast(delay * std.time.ns_per_ms);
            const ts = std.c.timespec{
                .sec = @intCast(delay_ns / std.time.ns_per_s),
                .nsec = @intCast(delay_ns % std.time.ns_per_s),
            };
            _ = std.c.nanosleep(&ts, null);
        }
        attempt += 1;
    }
}

fn normalizeRetryOptions(max_retries: *i32, base_delay_ms: *i64, max_delay_ms: *i64) ?[]const u8 {
    if (max_retries.* < 0) max_retries.* = 0;
    if (base_delay_ms.* < 0) base_delay_ms.* = 0;
    if (max_delay_ms.* < 0) max_delay_ms.* = 0;

    if (max_retries.* > MAX_ALLOWED_RETRIES) return "fetchWithRetry() maxRetries must be <= 10";
    if (base_delay_ms.* > MAX_ALLOWED_BASE_DELAY_MS) return "fetchWithRetry() baseDelayMs must be <= 5000";
    if (max_delay_ms.* > MAX_ALLOWED_DELAY_MS) return "fetchWithRetry() maxDelayMs must be <= 30000";
    return null;
}

const testing = std.testing;

test "fetchWithRetry retry options reject unbounded sleeps" {
    var retries: i32 = 11;
    var base_delay_ms: i64 = DEFAULT_BASE_DELAY_MS;
    var max_delay_ms: i64 = DEFAULT_MAX_DELAY_MS;
    try testing.expectEqualStrings("fetchWithRetry() maxRetries must be <= 10", normalizeRetryOptions(&retries, &base_delay_ms, &max_delay_ms).?);

    retries = DEFAULT_MAX_RETRIES;
    base_delay_ms = 5001;
    max_delay_ms = DEFAULT_MAX_DELAY_MS;
    try testing.expectEqualStrings("fetchWithRetry() baseDelayMs must be <= 5000", normalizeRetryOptions(&retries, &base_delay_ms, &max_delay_ms).?);

    retries = DEFAULT_MAX_RETRIES;
    base_delay_ms = DEFAULT_BASE_DELAY_MS;
    max_delay_ms = 30001;
    try testing.expectEqualStrings("fetchWithRetry() maxDelayMs must be <= 30000", normalizeRetryOptions(&retries, &base_delay_ms, &max_delay_ms).?);
}

test "fetchWithRetry retry options normalize negative values" {
    var retries: i32 = -1;
    var base_delay_ms: i64 = -10;
    var max_delay_ms: i64 = -20;

    try testing.expect(normalizeRetryOptions(&retries, &base_delay_ms, &max_delay_ms) == null);
    try testing.expectEqual(@as(i32, 0), retries);
    try testing.expectEqual(@as(i64, 0), base_delay_ms);
    try testing.expectEqual(@as(i64, 0), max_delay_ms);
}
