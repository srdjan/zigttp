//! zigttp:cache - In-memory key-value cache with TTL and LRU eviction.
//!
//! Exports:
//!   cacheGet(namespace, key) -> string | null
//!   cacheSet(namespace, key, value, ttl?) -> boolean
//!   cacheDelete(namespace, key) -> boolean
//!   cacheIncr(namespace, key, delta?, ttl?) -> number
//!   cacheStats(namespace?) -> { hits, misses, entries, bytes }
//!
//! Namespaces share the LRU list and global byte/entry budget; each
//! namespace has independent hit/miss counters. TTL is expressed in
//! seconds; 0 or negative means "already expired", null means "no expiry".

const std = @import("std");
const sdk = @import("zigttp-sdk");
const util = @import("../internal/util.zig");

const MODULE_STATE_SLOT: usize = 5; // module_slots.Slot.cache

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:cache",
    .name = "cache",
    .required_capabilities = &.{ .clock, .policy_check },
    .stateful = true,
    .contract_section = "cache",
    .sandboxable = true,
    .exports = &.{
        .{
            .name = "cacheGet",
            .module_func = cacheGetImpl,
            .arg_count = 2,
            .effect = .read,
            .returns = .optional_string,
            .param_types = &.{.string},
            .failure_severity = .expected,
            .contract_extractions = &.{.{ .category = .cache_namespace }},
            .return_labels = .{ .internal = true },
        },
        .{
            .name = "cacheSet",
            .module_func = cacheSetImpl,
            .arg_count = 4,
            .effect = .write,
            .returns = .boolean,
            .param_types = &.{ .string, .string },
            .contract_extractions = &.{.{ .category = .cache_namespace }},
            .laws = &.{.idempotent_call},
        },
        .{
            .name = "cacheDelete",
            .module_func = cacheDeleteImpl,
            .arg_count = 2,
            .effect = .write,
            .returns = .boolean,
            .param_types = &.{.string},
            .contract_extractions = &.{.{ .category = .cache_namespace }},
            .laws = &.{.idempotent_call},
        },
        .{
            .name = "cacheIncr",
            .module_func = cacheIncrImpl,
            .arg_count = 4,
            .effect = .write,
            .returns = .number,
            .param_types = &.{.string},
            .contract_extractions = &.{.{ .category = .cache_namespace }},
        },
        .{
            .name = "cacheStats",
            .module_func = cacheStatsImpl,
            .arg_count = 1,
            .effect = .read,
            .returns = .object,
            .param_types = &.{},
        },
    },
};

const CacheEntry = struct {
    key: []const u8,
    ns: []const u8,
    cache_value: []const u8,
    expires_at: ?i64,
    byte_size: usize,
    prev: ?*CacheEntry,
    next: ?*CacheEntry,
};

const NamespaceCache = struct {
    entries: std.StringHashMap(*CacheEntry),
    hits: u64,
    misses: u64,
};

pub const CacheStore = struct {
    namespaces: std.StringHashMap(*NamespaceCache),
    allocator: std.mem.Allocator,
    lru_head: ?*CacheEntry,
    lru_tail: ?*CacheEntry,
    total_entries: usize,
    total_bytes: usize,
    max_entries: usize,
    max_bytes: usize,

    fn init(allocator: std.mem.Allocator) CacheStore {
        return .{
            .namespaces = std.StringHashMap(*NamespaceCache).init(allocator),
            .allocator = allocator,
            .lru_head = null,
            .lru_tail = null,
            .total_entries = 0,
            .total_bytes = 0,
            .max_entries = 10_000,
            .max_bytes = 16 * 1024 * 1024,
        };
    }

    fn deinitSelf(self: *CacheStore) void {
        var ns_it = self.namespaces.iterator();
        while (ns_it.next()) |ns_entry| {
            var entry_it = ns_entry.value_ptr.*.entries.iterator();
            while (entry_it.next()) |e| {
                self.freeEntry(e.value_ptr.*);
            }
            ns_entry.value_ptr.*.entries.deinit();
            self.allocator.destroy(ns_entry.value_ptr.*);
            self.allocator.free(ns_entry.key_ptr.*);
        }
        self.namespaces.deinit();
    }

    fn sdkDeinit(ptr: *anyopaque) callconv(.c) void {
        const store: *CacheStore = @ptrCast(@alignCast(ptr));
        const allocator = store.allocator;
        store.deinitSelf();
        allocator.destroy(store);
    }

    fn getOrCreateNamespace(self: *CacheStore, ns: []const u8) !*NamespaceCache {
        if (self.namespaces.get(ns)) |existing| return existing;
        const ns_cache = try self.allocator.create(NamespaceCache);
        ns_cache.* = .{
            .entries = std.StringHashMap(*CacheEntry).init(self.allocator),
            .hits = 0,
            .misses = 0,
        };
        const ns_owned = try self.allocator.dupe(u8, ns);
        try self.namespaces.put(ns_owned, ns_cache);
        return ns_cache;
    }

    fn get(self: *CacheStore, ns: []const u8, key: []const u8, now_s: i64) ?[]const u8 {
        const ns_cache = self.namespaces.get(ns) orelse return null;
        const entry = ns_cache.entries.get(key) orelse {
            ns_cache.misses += 1;
            return null;
        };

        if (entry.expires_at) |exp| {
            if (now_s > exp) {
                self.removeEntry(ns_cache, entry);
                ns_cache.misses += 1;
                return null;
            }
        }

        ns_cache.hits += 1;
        self.promoteToHead(entry);
        return entry.cache_value;
    }

    fn set(self: *CacheStore, ns: []const u8, key: []const u8, val: []const u8, ttl: ?i64, now_s: i64) !void {
        const ns_cache = try self.getOrCreateNamespace(ns);

        if (ns_cache.entries.get(key)) |existing| {
            self.total_bytes -= existing.byte_size;
            self.allocator.free(existing.cache_value);
            existing.cache_value = try self.allocator.dupe(u8, val);
            existing.byte_size = existing.key.len + existing.ns.len + existing.cache_value.len;
            existing.expires_at = if (ttl) |t| now_s + t else null;
            self.total_bytes += existing.byte_size;
            self.promoteToHead(existing);
            return;
        }

        while (self.total_entries >= self.max_entries or self.total_bytes >= self.max_bytes) {
            if (!self.evictLru()) break;
        }

        const entry = try self.allocator.create(CacheEntry);
        errdefer self.allocator.destroy(entry);

        const key_owned = try self.allocator.dupe(u8, key);
        errdefer self.allocator.free(key_owned);
        const ns_owned = try self.allocator.dupe(u8, ns);
        errdefer self.allocator.free(ns_owned);
        const val_owned = try self.allocator.dupe(u8, val);
        errdefer self.allocator.free(val_owned);

        entry.* = .{
            .key = key_owned,
            .ns = ns_owned,
            .cache_value = val_owned,
            .expires_at = if (ttl) |t| now_s + t else null,
            .byte_size = key_owned.len + ns_owned.len + val_owned.len,
            .prev = null,
            .next = null,
        };

        try ns_cache.entries.put(key_owned, entry);
        self.total_entries += 1;
        self.total_bytes += entry.byte_size;
        self.promoteToHead(entry);
    }

    fn delete(self: *CacheStore, ns: []const u8, key: []const u8) bool {
        const ns_cache = self.namespaces.get(ns) orelse return false;
        const entry = ns_cache.entries.get(key) orelse return false;
        self.removeEntry(ns_cache, entry);
        return true;
    }

    fn removeEntry(self: *CacheStore, ns_cache: *NamespaceCache, entry: *CacheEntry) void {
        self.unlinkFromLru(entry);
        _ = ns_cache.entries.fetchRemove(entry.key);
        self.total_entries -= 1;
        self.total_bytes -= entry.byte_size;
        self.freeEntry(entry);
    }

    fn freeEntry(self: *CacheStore, entry: *CacheEntry) void {
        self.allocator.free(entry.key);
        self.allocator.free(entry.ns);
        self.allocator.free(entry.cache_value);
        self.allocator.destroy(entry);
    }

    fn evictLru(self: *CacheStore) bool {
        const tail = self.lru_tail orelse return false;
        const ns_cache = self.namespaces.get(tail.ns) orelse return false;
        self.removeEntry(ns_cache, tail);
        return true;
    }

    fn promoteToHead(self: *CacheStore, entry: *CacheEntry) void {
        if (self.lru_head == entry) return;
        self.unlinkFromLru(entry);
        entry.prev = null;
        entry.next = self.lru_head;
        if (self.lru_head) |old_head| old_head.prev = entry;
        self.lru_head = entry;
        if (self.lru_tail == null) self.lru_tail = entry;
    }

    fn unlinkFromLru(self: *CacheStore, entry: *CacheEntry) void {
        if (entry.prev) |prev| {
            prev.next = entry.next;
        } else if (self.lru_head == entry) {
            self.lru_head = entry.next;
        }
        if (entry.next) |next_entry| {
            next_entry.prev = entry.prev;
        } else if (self.lru_tail == entry) {
            self.lru_tail = entry.prev;
        }
        entry.prev = null;
        entry.next = null;
    }
};

fn getOrCreateStore(handle: *sdk.ModuleHandle) !*CacheStore {
    if (sdk.getModuleState(handle, CacheStore, MODULE_STATE_SLOT)) |store| return store;
    const allocator = sdk.getAllocator(handle);
    const store = try allocator.create(CacheStore);
    store.* = CacheStore.init(allocator);
    try sdk.setModuleState(handle, MODULE_STATE_SLOT, @ptrCast(store), CacheStore.sdkDeinit);
    return store;
}

fn nowSeconds(handle: *sdk.ModuleHandle) !i64 {
    const ms = try sdk.nowMs(handle);
    return @divTrunc(ms, 1000);
}

/// Returns the thrown exception JSValue if the namespace is denied, null
/// if allowed. Callers propagate the exception by returning it directly.
fn denyIfNamespaceBlocked(handle: *sdk.ModuleHandle, ns: []const u8) ?sdk.JSValue {
    if (sdk.allowsCacheNamespace(handle, ns)) return null;
    return util.throwCapabilityPolicyError(handle, "cache namespace", ns);
}

fn cacheGetImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.undefined_val;
    const ns = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;
    if (denyIfNamespaceBlocked(handle, ns)) |exc| return exc;
    const key = sdk.extractString(args[1]) orelse return sdk.JSValue.undefined_val;

    const now_s = nowSeconds(handle) catch return sdk.JSValue.undefined_val;
    const store = getOrCreateStore(handle) catch return sdk.JSValue.undefined_val;
    const result = store.get(ns, key, now_s) orelse return sdk.JSValue.undefined_val;
    return sdk.createString(handle, result) catch sdk.JSValue.undefined_val;
}

fn cacheSetImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 3) return sdk.JSValue.false_val;
    const ns = sdk.extractString(args[0]) orelse return sdk.JSValue.false_val;
    if (denyIfNamespaceBlocked(handle, ns)) |exc| return exc;
    const key = sdk.extractString(args[1]) orelse return sdk.JSValue.false_val;
    const val_str = sdk.extractString(args[2]) orelse return sdk.JSValue.false_val;

    const ttl: ?i64 = if (args.len >= 4) blk: {
        if (sdk.extractInt(args[3])) |t| break :blk @intCast(t);
        break :blk null;
    } else null;

    const now_s = nowSeconds(handle) catch return sdk.JSValue.false_val;
    const store = getOrCreateStore(handle) catch return sdk.JSValue.false_val;
    store.set(ns, key, val_str, ttl, now_s) catch return sdk.JSValue.false_val;
    return sdk.JSValue.true_val;
}

fn cacheDeleteImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.false_val;
    const ns = sdk.extractString(args[0]) orelse return sdk.JSValue.false_val;
    if (denyIfNamespaceBlocked(handle, ns)) |exc| return exc;
    const key = sdk.extractString(args[1]) orelse return sdk.JSValue.false_val;

    const store = getOrCreateStore(handle) catch return sdk.JSValue.false_val;
    return if (store.delete(ns, key)) sdk.JSValue.true_val else sdk.JSValue.false_val;
}

fn cacheIncrImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len < 2) return sdk.JSValue.undefined_val;
    const ns = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;
    if (denyIfNamespaceBlocked(handle, ns)) |exc| return exc;
    const key = sdk.extractString(args[1]) orelse return sdk.JSValue.undefined_val;

    const delta: i64 = if (args.len >= 3) blk: {
        if (sdk.extractInt(args[2])) |d| break :blk @intCast(d);
        break :blk 1;
    } else 1;

    const ttl: ?i64 = if (args.len >= 4) blk: {
        if (sdk.extractInt(args[3])) |t| break :blk @intCast(t);
        break :blk null;
    } else null;

    const now_s = nowSeconds(handle) catch return sdk.JSValue.undefined_val;
    const store = getOrCreateStore(handle) catch return sdk.JSValue.undefined_val;

    const current_str = store.get(ns, key, now_s) orelse "0";
    const current = std.fmt.parseInt(i64, current_str, 10) catch 0;
    const new_val = current + delta;

    var buf: [32]u8 = undefined;
    const new_str = std.fmt.bufPrint(&buf, "{d}", .{new_val}) catch return sdk.JSValue.undefined_val;
    store.set(ns, key, new_str, ttl, now_s) catch return sdk.JSValue.undefined_val;

    return sdk.numberFromF64(@floatFromInt(new_val));
}

fn cacheStatsImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    const store_opt = sdk.getModuleState(handle, CacheStore, MODULE_STATE_SLOT);

    if (args.len >= 1) {
        if (sdk.extractString(args[0])) |ns| {
            if (denyIfNamespaceBlocked(handle, ns)) |exc| return exc;
            if (store_opt) |store| {
                if (store.namespaces.get(ns)) |ns_cache| {
                    return makeStatsObject(handle, ns_cache.hits, ns_cache.misses, ns_cache.entries.count(), 0);
                }
            }
            return makeStatsObject(handle, 0, 0, 0, 0);
        }
    }

    const store = store_opt orelse return makeStatsObject(handle, 0, 0, 0, 0);

    var total_hits: u64 = 0;
    var total_misses: u64 = 0;
    var ns_it = store.namespaces.iterator();
    while (ns_it.next()) |entry| {
        total_hits += entry.value_ptr.*.hits;
        total_misses += entry.value_ptr.*.misses;
    }

    return makeStatsObject(handle, total_hits, total_misses, store.total_entries, store.total_bytes);
}

fn makeStatsObject(handle: *sdk.ModuleHandle, hits: u64, misses: u64, entries: usize, bytes: usize) !sdk.JSValue {
    const obj = try sdk.createObject(handle);
    try sdk.objectSet(handle, obj, "hits", sdk.numberFromF64(@floatFromInt(hits)));
    try sdk.objectSet(handle, obj, "misses", sdk.numberFromF64(@floatFromInt(misses)));
    try sdk.objectSet(handle, obj, "entries", sdk.numberFromF64(@floatFromInt(entries)));
    try sdk.objectSet(handle, obj, "bytes", sdk.numberFromF64(@floatFromInt(bytes)));
    return obj;
}

test "CacheStore: set and get" {
    var store = CacheStore.init(std.testing.allocator);
    defer store.deinitSelf();

    try store.set("ns", "key1", "value1", null, 1000);
    const result = store.get("ns", "key1", 1000) orelse return error.TestFailed;
    try std.testing.expectEqualStrings("value1", result);
}

test "CacheStore: get missing key" {
    var store = CacheStore.init(std.testing.allocator);
    defer store.deinitSelf();
    try std.testing.expect(store.get("ns", "nonexistent", 1000) == null);
}

test "CacheStore: delete" {
    var store = CacheStore.init(std.testing.allocator);
    defer store.deinitSelf();

    try store.set("ns", "key1", "value1", null, 1000);
    try std.testing.expect(store.delete("ns", "key1"));
    try std.testing.expect(store.get("ns", "key1", 1000) == null);
}

test "CacheStore: TTL expiry" {
    var store = CacheStore.init(std.testing.allocator);
    defer store.deinitSelf();

    try store.set("ns", "key1", "value1", -1, 1000);
    try std.testing.expect(store.get("ns", "key1", 1000) == null);
}

test "CacheStore: LRU eviction" {
    var store = CacheStore.init(std.testing.allocator);
    defer store.deinitSelf();
    store.max_entries = 3;

    try store.set("ns", "a", "1", null, 1000);
    try store.set("ns", "b", "2", null, 1000);
    try store.set("ns", "c", "3", null, 1000);
    try store.set("ns", "d", "4", null, 1000);

    try std.testing.expect(store.get("ns", "a", 1000) == null);
    try std.testing.expect(store.get("ns", "b", 1000) != null);
    try std.testing.expect(store.get("ns", "c", 1000) != null);
    try std.testing.expect(store.get("ns", "d", 1000) != null);
}

test "CacheStore: LRU promotes on access" {
    var store = CacheStore.init(std.testing.allocator);
    defer store.deinitSelf();
    store.max_entries = 3;

    try store.set("ns", "a", "1", null, 1000);
    try store.set("ns", "b", "2", null, 1000);
    try store.set("ns", "c", "3", null, 1000);
    _ = store.get("ns", "a", 1000);
    try store.set("ns", "d", "4", null, 1000);

    try std.testing.expect(store.get("ns", "a", 1000) != null);
    try std.testing.expect(store.get("ns", "b", 1000) == null);
}

test "CacheStore: namespace isolation" {
    var store = CacheStore.init(std.testing.allocator);
    defer store.deinitSelf();

    try store.set("ns1", "key", "value1", null, 1000);
    try store.set("ns2", "key", "value2", null, 1000);
    try std.testing.expectEqualStrings("value1", store.get("ns1", "key", 1000).?);
    try std.testing.expectEqualStrings("value2", store.get("ns2", "key", 1000).?);
}

test "CacheStore: overwrite existing key" {
    var store = CacheStore.init(std.testing.allocator);
    defer store.deinitSelf();

    try store.set("ns", "key", "old", null, 1000);
    try store.set("ns", "key", "new", null, 1000);
    try std.testing.expectEqualStrings("new", store.get("ns", "key", 1000).?);
    try std.testing.expectEqual(@as(usize, 1), store.total_entries);
}

test "CacheStore: stats tracking" {
    var store = CacheStore.init(std.testing.allocator);
    defer store.deinitSelf();

    try store.set("ns", "key", "val", null, 1000);
    _ = store.get("ns", "key", 1000);
    _ = store.get("ns", "miss", 1000);

    const ns_cache = store.namespaces.get("ns") orelse return error.TestFailed;
    try std.testing.expectEqual(@as(u64, 1), ns_cache.hits);
    try std.testing.expectEqual(@as(u64, 1), ns_cache.misses);
    try std.testing.expectEqual(@as(usize, 1), store.total_entries);
}
