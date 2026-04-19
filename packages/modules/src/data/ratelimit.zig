//! zigttp:ratelimit - In-memory rate limiting
//!
//! Exports:
//!   rateCheck(key: string, limit: number, windowSec: number) -> Result
//!     Fixed-window per-key counter. Returns
//!     { ok: true, value: { remaining, resetAt } } or
//!     { ok: false, error: { tag: "rate_exceeded", retryAfter } }.
//!
//!   rateReset(key: string) -> boolean
//!     Clears the counter for the given key. Returns true if present.

const std = @import("std");
const sdk = @import("zigttp-sdk");

const MODULE_STATE_SLOT: usize = 8; // module_slots.Slot.ratelimit

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:ratelimit",
    .name = "ratelimit",
    .required_capabilities = &.{.clock},
    .stateful = true,
    .exports = &.{
        .{
            .name = "rateCheck",
            .module_func = rateCheckImpl,
            .arg_count = 3,
            .returns = .result,
            .param_types = &.{ .string, .number, .number },
            .effect = .write,
            .failure_severity = .critical,
            .contract_extractions = &.{.{ .arg_position = 0, .category = .rate_limit_key }},
            .return_labels = .{ .internal = true },
        },
        .{
            .name = "rateReset",
            .module_func = rateResetImpl,
            .arg_count = 1,
            .returns = .boolean,
            .param_types = &.{.string},
            .effect = .write,
        },
    },
};

const EVICT_INTERVAL: u32 = 256;

const RateEntry = struct {
    count: u32,
    window_start: i64,
    window_sec: i64,
};

pub const RateStore = struct {
    allocator: std.mem.Allocator,
    entries: std.StringHashMap(RateEntry),
    check_count: u32 = 0,

    fn init(allocator: std.mem.Allocator) RateStore {
        return .{
            .allocator = allocator,
            .entries = std.StringHashMap(RateEntry).init(allocator),
        };
    }

    fn deinitSelf(self: *RateStore) void {
        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.entries.deinit();
    }

    fn sdkDeinit(ptr: *anyopaque) callconv(.c) void {
        const store: *RateStore = @ptrCast(@alignCast(ptr));
        const allocator = store.allocator;
        store.deinitSelf();
        allocator.destroy(store);
    }

    fn evictExpired(self: *RateStore, now: i64) void {
        var expired: [64][]const u8 = undefined;
        var expired_count: usize = 0;

        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            if (now >= entry.value_ptr.window_start + entry.value_ptr.window_sec) {
                if (expired_count < 64) {
                    expired[expired_count] = entry.key_ptr.*;
                    expired_count += 1;
                }
            }
        }

        for (expired[0..expired_count]) |key| {
            if (self.entries.fetchRemove(key)) |_| {
                self.allocator.free(key);
            }
        }
    }

    const CheckResult = struct { allowed: bool, remaining: u32, reset_at_ms: i64 };

    fn check(self: *RateStore, key: []const u8, limit: u32, window_sec: i64, now: i64) !CheckResult {
        self.check_count +%= 1;
        if (self.check_count % EVICT_INTERVAL == 0) {
            self.evictExpired(now);
        }

        if (self.entries.getPtr(key)) |entry| {
            const window_end = entry.window_start + entry.window_sec;

            if (now >= window_end) {
                entry.count = 1;
                entry.window_start = now;
                entry.window_sec = window_sec;
                return .{
                    .allowed = true,
                    .remaining = if (limit > 1) limit - 1 else 0,
                    .reset_at_ms = (now + window_sec) * 1000,
                };
            }

            if (entry.count >= limit) {
                return .{
                    .allowed = false,
                    .remaining = 0,
                    .reset_at_ms = window_end * 1000,
                };
            }

            entry.count += 1;
            return .{
                .allowed = true,
                .remaining = limit - entry.count,
                .reset_at_ms = window_end * 1000,
            };
        }

        const owned_key = try self.allocator.dupe(u8, key);
        try self.entries.put(owned_key, .{
            .count = 1,
            .window_start = now,
            .window_sec = window_sec,
        });
        return .{
            .allowed = true,
            .remaining = if (limit > 1) limit - 1 else 0,
            .reset_at_ms = (now + window_sec) * 1000,
        };
    }

    fn reset(self: *RateStore, key: []const u8) bool {
        if (self.entries.fetchRemove(key)) |kv| {
            self.allocator.free(kv.key);
            return true;
        }
        return false;
    }
};

fn getOrCreateStore(handle: *sdk.ModuleHandle) !*RateStore {
    if (sdk.getModuleState(handle, RateStore, MODULE_STATE_SLOT)) |store| return store;
    const allocator = sdk.getAllocator(handle);
    const store = try allocator.create(RateStore);
    store.* = RateStore.init(allocator);
    try sdk.setModuleState(handle, MODULE_STATE_SLOT, @ptrCast(store), RateStore.sdkDeinit);
    return store;
}

fn rateCheckImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 3) return sdk.JSValue.undefined_val;
    const key = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;
    const limit_i = sdk.extractInt(args[1]) orelse return sdk.JSValue.undefined_val;
    if (limit_i < 1) return sdk.JSValue.undefined_val;
    const limit: u32 = @intCast(limit_i);
    const window_f = sdk.extractFloat(args[2]) orelse return sdk.JSValue.undefined_val;
    if (window_f < 1) return sdk.JSValue.undefined_val;
    const window_sec: i64 = @intFromFloat(window_f);

    const now_ms = sdk.nowMs(handle) catch return sdk.JSValue.undefined_val;
    const now_s = @divTrunc(now_ms, 1000);

    const store = getOrCreateStore(handle) catch return sdk.JSValue.undefined_val;
    const result = store.check(key, limit, window_sec, now_s) catch return sdk.JSValue.undefined_val;

    if (result.allowed) {
        const val_obj = try sdk.createObject(handle);
        try sdk.objectSet(handle, val_obj, "remaining", sdk.JSValue.fromInt(@intCast(result.remaining)));
        try sdk.objectSet(handle, val_obj, "resetAt", sdk.numberFromF64(@floatFromInt(result.reset_at_ms)));
        return sdk.resultOk(handle, val_obj);
    }

    const err_obj = try sdk.createObject(handle);
    const tag_str = try sdk.createString(handle, "rate_exceeded");
    try sdk.objectSet(handle, err_obj, "tag", tag_str);

    const retry_after_ms = result.reset_at_ms - now_ms;
    const retry_sec = @divTrunc(@max(@as(i64, 0), retry_after_ms), 1000);
    try sdk.objectSet(handle, err_obj, "retryAfter", sdk.numberFromF64(@floatFromInt(retry_sec)));
    return sdk.resultErrValue(handle, err_obj);
}

fn rateResetImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.false_val;
    const key = sdk.extractString(args[0]) orelse return sdk.JSValue.false_val;
    const store = getOrCreateStore(handle) catch return sdk.JSValue.false_val;
    return if (store.reset(key)) sdk.JSValue.true_val else sdk.JSValue.false_val;
}

test "RateStore: basic check" {
    var store = RateStore.init(std.testing.allocator);
    defer store.deinitSelf();

    const r1 = try store.check("test-key", 3, 60, 1000);
    try std.testing.expect(r1.allowed);
    try std.testing.expectEqual(@as(u32, 2), r1.remaining);

    const r2 = try store.check("test-key", 3, 60, 1001);
    try std.testing.expect(r2.allowed);
    try std.testing.expectEqual(@as(u32, 1), r2.remaining);

    const r3 = try store.check("test-key", 3, 60, 1002);
    try std.testing.expect(r3.allowed);
    try std.testing.expectEqual(@as(u32, 0), r3.remaining);

    const r4 = try store.check("test-key", 3, 60, 1003);
    try std.testing.expect(!r4.allowed);
}

test "RateStore: reset" {
    var store = RateStore.init(std.testing.allocator);
    defer store.deinitSelf();

    _ = try store.check("test-key", 3, 60, 1000);
    try std.testing.expect(store.reset("test-key"));
    try std.testing.expect(!store.reset("nonexistent"));
}

test "RateStore: independent keys" {
    var store = RateStore.init(std.testing.allocator);
    defer store.deinitSelf();

    const r1 = try store.check("key-a", 1, 60, 1000);
    try std.testing.expect(r1.allowed);

    const r2 = try store.check("key-b", 1, 60, 1000);
    try std.testing.expect(r2.allowed);

    const r3 = try store.check("key-a", 1, 60, 1001);
    try std.testing.expect(!r3.allowed);

    const r4 = try store.check("key-b", 1, 60, 1001);
    try std.testing.expect(!r4.allowed);
}
