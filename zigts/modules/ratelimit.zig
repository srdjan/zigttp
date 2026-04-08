//! zigttp:ratelimit - In-memory rate limiting
//!
//! Exports:
//!   rateCheck(key: string, limit: number, windowSec: number) -> Result
//!     Check and increment the rate counter for the given key.
//!     Returns { ok: true, value: { remaining: number, resetAt: number } }
//!     or { ok: false, error: { tag: "rate_exceeded", retryAfter: number } }.
//!     Uses a fixed-window algorithm with per-key counters.
//!
//!   rateReset(key: string) -> boolean
//!     Resets the counter for the given key. Returns true if the key existed.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const builtins_helpers = @import("../builtins/helpers.zig");
const util = @import("util.zig");
const mb = @import("../module_binding.zig");

const MODULE_STATE_SLOT = @intFromEnum(@import("../module_slots.zig").Slot.ratelimit);

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:ratelimit",
    .name = "ratelimit",
    .required_capabilities = &.{.clock},
    .stateful = true,
    .exports = &.{
        .{ .name = "rateCheck", .func = rateCheckNative, .arg_count = 3, .returns = .result, .param_types = &.{ .string, .number, .number }, .effect = .write, .failure_severity = .critical, .contract_extractions = &.{.{ .arg_position = 0, .category = .rate_limit_key }}, .return_labels = .{ .internal = true } },
        .{ .name = "rateReset", .func = rateResetNative, .arg_count = 1, .returns = .boolean, .param_types = &.{.string}, .effect = .write },
    },
};

pub const exports = binding.toModuleExports();

// -------------------------------------------------------------------------
// Rate limit state
// -------------------------------------------------------------------------

const RateEntry = struct {
    count: u32,
    window_start: i64, // epoch seconds when this window began
    window_sec: i64, // window duration in seconds
};

const EVICT_INTERVAL: u32 = 256;

const RateStore = struct {
    allocator: std.mem.Allocator,
    entries: std.StringHashMap(RateEntry),
    check_count: u32 = 0,

    fn init(allocator: std.mem.Allocator) RateStore {
        return .{
            .allocator = allocator,
            .entries = std.StringHashMap(RateEntry).init(allocator),
        };
    }

    fn deinit(self: *RateStore) void {
        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.entries.deinit();
    }

    fn deinitOpaque(ptr: *anyopaque, _: std.mem.Allocator) void {
        const store: *RateStore = @ptrCast(@alignCast(ptr));
        store.deinit();
    }

    /// Remove entries whose windows have expired.
    fn evictExpired(self: *RateStore, now: i64) void {
        // Collect expired keys first to avoid modifying during iteration
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

    /// Check rate limit. Returns (allowed, remaining, reset_at).
    fn check(self: *RateStore, key: []const u8, limit: u32, window_sec: i64) !struct { allowed: bool, remaining: u32, reset_at: i64 } {
        const now = nowSeconds();

        self.check_count +%= 1;
        if (self.check_count % EVICT_INTERVAL == 0) {
            self.evictExpired(now);
        }

        if (self.entries.getPtr(key)) |entry| {
            const window_end = entry.window_start + entry.window_sec;

            if (now >= window_end) {
                // Window expired, start new window
                entry.count = 1;
                entry.window_start = now;
                entry.window_sec = window_sec;
                return .{
                    .allowed = true,
                    .remaining = if (limit > 1) limit - 1 else 0,
                    .reset_at = (now + window_sec) * 1000,
                };
            }

            if (entry.count >= limit) {
                // Rate exceeded
                return .{
                    .allowed = false,
                    .remaining = 0,
                    .reset_at = window_end * 1000,
                };
            }

            entry.count += 1;
            return .{
                .allowed = true,
                .remaining = limit - entry.count,
                .reset_at = window_end * 1000,
            };
        }

        // New key
        const owned_key = try self.allocator.dupe(u8, key);
        try self.entries.put(owned_key, .{
            .count = 1,
            .window_start = now,
            .window_sec = window_sec,
        });
        return .{
            .allowed = true,
            .remaining = if (limit > 1) limit - 1 else 0,
            .reset_at = (now + window_sec) * 1000,
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

fn nowSeconds() i64 {
    const ms = mb.clockNowMsChecked();
    return @divTrunc(ms, 1000);
}

fn getOrCreateStore(ctx: *context.Context) !*RateStore {
    if (ctx.getModuleState(RateStore, MODULE_STATE_SLOT)) |store| {
        return store;
    }

    const store = try ctx.allocator.create(RateStore);
    store.* = RateStore.init(ctx.allocator);
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(store), &RateStore.deinitOpaque);
    return store;
}

// -------------------------------------------------------------------------
// Native function implementations
// -------------------------------------------------------------------------

/// rateCheck(key, limit, windowSec) -> Result
fn rateCheckNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len < 3) return value.JSValue.undefined_val;
    const key = util.extractString(args[0]) orelse return value.JSValue.undefined_val;
    const limit_i = util.extractInt(args[1]) orelse return value.JSValue.undefined_val;
    if (limit_i < 1) return value.JSValue.undefined_val;
    const limit: u32 = @intCast(limit_i);
    const window_f = util.extractFloat(args[2]) orelse return value.JSValue.undefined_val;
    if (window_f < 1) return value.JSValue.undefined_val;
    const window_sec: i64 = @intFromFloat(window_f);

    const store = getOrCreateStore(ctx) catch return value.JSValue.undefined_val;
    const result = store.check(key, limit, window_sec) catch return value.JSValue.undefined_val;

    const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;

    if (result.allowed) {
        // Build { remaining, resetAt } value object
        const val_obj = try ctx.createObject(null);
        const remaining_atom = try ctx.atoms.intern("remaining");
        const reset_atom = try ctx.atoms.intern("resetAt");
        try val_obj.setProperty(ctx.allocator, pool, remaining_atom, value.JSValue.fromInt(@intCast(result.remaining)));
        try val_obj.setProperty(ctx.allocator, pool, reset_atom, builtins_helpers.allocFloat(ctx, @floatFromInt(result.reset_at)));
        return util.createPlainResultOk(ctx, val_obj.toValue());
    } else {
        // Build { tag: "rate_exceeded", retryAfter } error object
        const err_obj = try ctx.createObject(null);
        const tag_atom = try ctx.atoms.intern("tag");
        const retry_atom = try ctx.atoms.intern("retryAfter");
        const tag_str = try ctx.createString("rate_exceeded");
        try err_obj.setProperty(ctx.allocator, pool, tag_atom, tag_str);

        const now_ms = mb.clockNowMsChecked();
        const retry_after_ms = result.reset_at - now_ms;
        const retry_sec = @divTrunc(@max(0, retry_after_ms), 1000);
        try err_obj.setProperty(ctx.allocator, pool, retry_atom, builtins_helpers.allocFloat(ctx, @floatFromInt(retry_sec)));
        return util.createPlainResultErrValue(ctx, err_obj.toValue());
    }
}

fn pushRateLimitTestContext() mb.ActiveModuleToken {
    return mb.pushActiveModuleContext(binding.specifier, binding.required_capabilities);
}

/// rateReset(key) -> boolean
fn rateResetNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx = util.castContext(ctx_ptr);

    if (args.len == 0) return value.JSValue.false_val;
    const key = util.extractString(args[0]) orelse return value.JSValue.false_val;

    const store = getOrCreateStore(ctx) catch return value.JSValue.false_val;
    return if (store.reset(key)) value.JSValue.true_val else value.JSValue.false_val;
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "RateStore: basic check" {
    const token = pushRateLimitTestContext();
    defer mb.popActiveModuleContext(token);

    const store_obj = RateStore.init(std.testing.allocator);
    var store = store_obj;
    defer store.deinit();

    const r1 = try store.check("test-key", 3, 60);
    try std.testing.expect(r1.allowed);
    try std.testing.expectEqual(@as(u32, 2), r1.remaining);

    const r2 = try store.check("test-key", 3, 60);
    try std.testing.expect(r2.allowed);
    try std.testing.expectEqual(@as(u32, 1), r2.remaining);

    const r3 = try store.check("test-key", 3, 60);
    try std.testing.expect(r3.allowed);
    try std.testing.expectEqual(@as(u32, 0), r3.remaining);

    const r4 = try store.check("test-key", 3, 60);
    try std.testing.expect(!r4.allowed);
}

test "RateStore: reset" {
    const token = pushRateLimitTestContext();
    defer mb.popActiveModuleContext(token);

    const store_obj = RateStore.init(std.testing.allocator);
    var store = store_obj;
    defer store.deinit();

    _ = try store.check("test-key", 3, 60);
    try std.testing.expect(store.reset("test-key"));
    try std.testing.expect(!store.reset("nonexistent"));
}

test "RateStore: independent keys" {
    const token = pushRateLimitTestContext();
    defer mb.popActiveModuleContext(token);

    const store_obj = RateStore.init(std.testing.allocator);
    var store = store_obj;
    defer store.deinit();

    const r1 = try store.check("key-a", 1, 60);
    try std.testing.expect(r1.allowed);

    const r2 = try store.check("key-b", 1, 60);
    try std.testing.expect(r2.allowed);

    const r3 = try store.check("key-a", 1, 60);
    try std.testing.expect(!r3.allowed);

    const r4 = try store.check("key-b", 1, 60);
    try std.testing.expect(!r4.allowed);
}
