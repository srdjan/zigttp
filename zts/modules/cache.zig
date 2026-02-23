//! zigttp:cache - In-memory key-value cache with TTL and LRU eviction
//!
//! Exports:
//!   cacheGet(namespace: string, key: string) -> string | null
//!     Retrieve a cached value. Returns null if not found or expired.
//!
//!   cacheSet(namespace: string, key: string, val: string, ttl?: number) -> boolean
//!     Store a value. Optional TTL in seconds (0 = no expiry).
//!     Evicts LRU entries if over capacity.
//!
//!   cacheDelete(namespace: string, key: string) -> boolean
//!     Remove a cached entry.
//!
//!   cacheIncr(namespace: string, key: string, delta?: number, ttl?: number) -> number
//!     Atomic increment. Default delta=1. Creates entry at 0 if missing.
//!
//!   cacheStats(namespace?: string) -> { hits, misses, entries, bytes }
//!     Per-namespace or aggregate statistics.
//!
//! Cache persists across requests within the same runtime pool slot.
//! Namespace isolation: separate keyspaces, but global LRU eviction.

const std = @import("std");
const context = @import("../context.zig");
const value = @import("../value.zig");
const object = @import("../object.zig");
const compat = @import("../compat.zig");
const resolver = @import("resolver.zig");
const util = @import("util.zig");

/// Module state slot index (must match VirtualModule enum ordinal for 'cache')
const MODULE_STATE_SLOT = 5;

/// Get current time in seconds (epoch)
fn nowSeconds() i64 {
    const ms = compat.realtimeNowMs() catch return 0;
    return @divTrunc(ms, 1000);
}

/// Module exports
pub const exports = [_]resolver.ModuleExport{
    .{ .name = "cacheGet", .func = cacheGetNative, .arg_count = 2 },
    .{ .name = "cacheSet", .func = cacheSetNative, .arg_count = 4 },
    .{ .name = "cacheDelete", .func = cacheDeleteNative, .arg_count = 2 },
    .{ .name = "cacheIncr", .func = cacheIncrNative, .arg_count = 4 },
    .{ .name = "cacheStats", .func = cacheStatsNative, .arg_count = 1 },
};

// ============================================================================
// Internal data structures
// ============================================================================

const CacheEntry = struct {
    key: []const u8, // owned
    ns: []const u8, // owned (namespace)
    cache_value: []const u8, // owned
    expires_at: ?i64, // epoch seconds, null = no expiry
    byte_size: usize,
    // LRU doubly-linked list
    prev: ?*CacheEntry,
    next: ?*CacheEntry,
};

const NamespaceCache = struct {
    entries: std.StringHashMap(*CacheEntry),
    hits: u64,
    misses: u64,
};

const CacheStore = struct {
    namespaces: std.StringHashMap(*NamespaceCache),
    allocator: std.mem.Allocator,
    // LRU: head = most recently accessed, tail = eviction candidate
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
            .max_bytes = 16 * 1024 * 1024, // 16MB
        };
    }

    fn deinitSelf(self: *CacheStore) void {
        // Free all entries through namespaces
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

    fn deinitOpaque(ptr: *anyopaque, _: std.mem.Allocator) void {
        const store: *CacheStore = @ptrCast(@alignCast(ptr));
        store.deinitSelf();
        store.allocator.destroy(store);
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

    fn get(self: *CacheStore, ns: []const u8, key: []const u8) ?[]const u8 {
        const ns_cache = self.namespaces.get(ns) orelse {
            return null;
        };

        const entry = ns_cache.entries.get(key) orelse {
            ns_cache.misses += 1;
            return null;
        };

        // Lazy TTL check
        if (entry.expires_at) |exp| {
            if (nowSeconds() > exp) {
                // Expired: remove
                self.removeEntry(ns_cache, entry);
                ns_cache.misses += 1;
                return null;
            }
        }

        ns_cache.hits += 1;
        self.promoteToHead(entry);
        return entry.cache_value;
    }

    fn set(self: *CacheStore, ns: []const u8, key: []const u8, val: []const u8, ttl: ?i64) !void {
        const ns_cache = try self.getOrCreateNamespace(ns);

        // If key already exists, update in place
        if (ns_cache.entries.get(key)) |existing| {
            self.total_bytes -= existing.byte_size;
            self.allocator.free(existing.cache_value);
            existing.cache_value = try self.allocator.dupe(u8, val);
            existing.byte_size = existing.key.len + existing.ns.len + existing.cache_value.len;
            existing.expires_at = if (ttl) |t| nowSeconds() + t else null;
            self.total_bytes += existing.byte_size;
            self.promoteToHead(existing);
            return;
        }

        // Evict if needed
        while (self.total_entries >= self.max_entries or self.total_bytes >= self.max_bytes) {
            if (!self.evictLru()) break;
        }

        // Create new entry
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
            .expires_at = if (ttl) |t| nowSeconds() + t else null,
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
        // Remove from LRU list
        self.unlinkFromLru(entry);

        // Remove from namespace hashmap
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
        // Find the namespace for this entry
        const ns_cache = self.namespaces.get(tail.ns) orelse return false;
        self.removeEntry(ns_cache, tail);
        return true;
    }

    fn promoteToHead(self: *CacheStore, entry: *CacheEntry) void {
        if (self.lru_head == entry) return; // Already head

        // Unlink from current position
        self.unlinkFromLru(entry);

        // Insert at head
        entry.prev = null;
        entry.next = self.lru_head;
        if (self.lru_head) |old_head| {
            old_head.prev = entry;
        }
        self.lru_head = entry;
        if (self.lru_tail == null) {
            self.lru_tail = entry;
        }
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

/// Get or create the per-runtime CacheStore
fn getOrCreateStore(ctx: *context.Context) !*CacheStore {
    if (ctx.getModuleState(CacheStore, MODULE_STATE_SLOT)) |store| {
        return store;
    }

    const store = try ctx.allocator.create(CacheStore);
    store.* = CacheStore.init(ctx.allocator);
    ctx.setModuleState(MODULE_STATE_SLOT, @ptrCast(store), &CacheStore.deinitOpaque);
    return store;
}

// ============================================================================
// Native function implementations
// ============================================================================

/// cacheGet(namespace, key) -> string | null
fn cacheGetNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len < 2) return value.JSValue.null_val;
    const ns = util.extractString(args[0]) orelse return value.JSValue.null_val;
    const key = util.extractString(args[1]) orelse return value.JSValue.null_val;

    const store = getOrCreateStore(ctx) catch return value.JSValue.null_val;
    const result = store.get(ns, key) orelse return value.JSValue.null_val;

    return ctx.createString(result) catch value.JSValue.null_val;
}

/// cacheSet(namespace, key, value, ttl?) -> boolean
fn cacheSetNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len < 3) return value.JSValue.false_val;
    const ns = util.extractString(args[0]) orelse return value.JSValue.false_val;
    const key = util.extractString(args[1]) orelse return value.JSValue.false_val;
    const val_str = util.extractString(args[2]) orelse return value.JSValue.false_val;

    // Optional TTL in seconds
    const ttl: ?i64 = if (args.len >= 4) blk: {
        if (util.extractInt(args[3])) |t| {
            break :blk @intCast(t);
        }
        break :blk null;
    } else null;

    const store = getOrCreateStore(ctx) catch return value.JSValue.false_val;
    store.set(ns, key, val_str, ttl) catch return value.JSValue.false_val;

    return value.JSValue.true_val;
}

/// cacheDelete(namespace, key) -> boolean
fn cacheDeleteNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len < 2) return value.JSValue.false_val;
    const ns = util.extractString(args[0]) orelse return value.JSValue.false_val;
    const key = util.extractString(args[1]) orelse return value.JSValue.false_val;

    const store = getOrCreateStore(ctx) catch return value.JSValue.false_val;
    return if (store.delete(ns, key)) value.JSValue.true_val else value.JSValue.false_val;
}

/// cacheIncr(namespace, key, delta?, ttl?) -> number
fn cacheIncrNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    if (args.len < 2) return value.JSValue.undefined_val;
    const ns = util.extractString(args[0]) orelse return value.JSValue.undefined_val;
    const key = util.extractString(args[1]) orelse return value.JSValue.undefined_val;

    const delta: i64 = if (args.len >= 3) blk: {
        if (util.extractInt(args[2])) |d| break :blk @intCast(d);
        break :blk 1;
    } else 1;

    const ttl: ?i64 = if (args.len >= 4) blk: {
        if (util.extractInt(args[3])) |t| break :blk @intCast(t);
        break :blk null;
    } else null;

    const store = getOrCreateStore(ctx) catch return value.JSValue.undefined_val;

    // Get current value, default to 0
    const current_str = store.get(ns, key) orelse "0";
    const current = std.fmt.parseInt(i64, current_str, 10) catch 0;
    const new_val = current + delta;

    // Store back as string
    var buf: [32]u8 = undefined;
    const new_str = std.fmt.bufPrint(&buf, "{d}", .{new_val}) catch return value.JSValue.undefined_val;
    store.set(ns, key, new_str, ttl) catch return value.JSValue.undefined_val;

    // Return as JS integer (i32 range check)
    if (new_val >= std.math.minInt(i32) and new_val <= std.math.maxInt(i32)) {
        return value.JSValue.fromInt(@intCast(new_val));
    }
    return value.JSValue.fromFloat(@floatFromInt(new_val));
}

/// cacheStats(namespace?) -> { hits, misses, entries, bytes }
fn cacheStatsNative(ctx_ptr: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    const ctx: *context.Context = @ptrCast(@alignCast(ctx_ptr));

    const store = ctx.getModuleState(CacheStore, MODULE_STATE_SLOT) orelse {
        // No cache initialized yet - return zeros
        return makeStatsObject(ctx, 0, 0, 0, 0);
    };

    // If namespace specified, return stats for that namespace
    if (args.len >= 1) {
        if (util.extractString(args[0])) |ns| {
            if (store.namespaces.get(ns)) |ns_cache| {
                return makeStatsObject(ctx, ns_cache.hits, ns_cache.misses, ns_cache.entries.count(), 0);
            }
            return makeStatsObject(ctx, 0, 0, 0, 0);
        }
    }

    // Aggregate stats across all namespaces
    var total_hits: u64 = 0;
    var total_misses: u64 = 0;
    var ns_it = store.namespaces.iterator();
    while (ns_it.next()) |entry| {
        total_hits += entry.value_ptr.*.hits;
        total_misses += entry.value_ptr.*.misses;
    }

    return makeStatsObject(ctx, total_hits, total_misses, store.total_entries, store.total_bytes);
}

fn makeStatsObject(ctx: *context.Context, hits: u64, misses: u64, entries: usize, bytes: usize) !value.JSValue {
    const pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const obj = try ctx.createObject(null);

    const hits_atom = try ctx.atoms.intern("hits");
    const misses_atom = try ctx.atoms.intern("misses");
    const entries_atom = try ctx.atoms.intern("entries");
    const bytes_atom = try ctx.atoms.intern("bytes");

    const hits_val = intToJsValue(hits);
    const misses_val = intToJsValue(misses);
    const entries_val = intToJsValue(entries);
    const bytes_val = intToJsValue(bytes);

    try obj.setProperty(ctx.allocator, pool, hits_atom, hits_val);
    try obj.setProperty(ctx.allocator, pool, misses_atom, misses_val);
    try obj.setProperty(ctx.allocator, pool, entries_atom, entries_val);
    try obj.setProperty(ctx.allocator, pool, bytes_atom, bytes_val);

    return obj.toValue();
}

fn intToJsValue(n: anytype) value.JSValue {
    const i: i64 = @intCast(n);
    if (i >= std.math.minInt(i32) and i <= std.math.maxInt(i32)) {
        return value.JSValue.fromInt(@intCast(i));
    }
    return value.JSValue.fromFloat(@floatFromInt(i));
}

// ============================================================================
// Tests
// ============================================================================

test "CacheStore: set and get" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    try store.set("ns", "key1", "value1", null);
    const result = store.get("ns", "key1") orelse return error.TestFailed;
    try std.testing.expectEqualStrings("value1", result);
}

test "CacheStore: get missing key" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    try std.testing.expect(store.get("ns", "nonexistent") == null);
}

test "CacheStore: delete" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    try store.set("ns", "key1", "value1", null);
    try std.testing.expect(store.delete("ns", "key1"));
    try std.testing.expect(store.get("ns", "key1") == null);
}

test "CacheStore: delete missing key" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    try std.testing.expect(!store.delete("ns", "missing"));
}

test "CacheStore: TTL expiry" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    // Set with TTL of 0 (already expired relative to now)
    try store.set("ns", "key1", "value1", -1);
    // Should be expired
    try std.testing.expect(store.get("ns", "key1") == null);
}

test "CacheStore: LRU eviction" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    store.max_entries = 3;

    try store.set("ns", "a", "1", null);
    try store.set("ns", "b", "2", null);
    try store.set("ns", "c", "3", null);
    // This should evict "a" (least recently used)
    try store.set("ns", "d", "4", null);

    try std.testing.expect(store.get("ns", "a") == null);
    try std.testing.expect(store.get("ns", "b") != null);
    try std.testing.expect(store.get("ns", "c") != null);
    try std.testing.expect(store.get("ns", "d") != null);
}

test "CacheStore: LRU promotes on access" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    store.max_entries = 3;

    try store.set("ns", "a", "1", null);
    try store.set("ns", "b", "2", null);
    try store.set("ns", "c", "3", null);

    // Access "a" to promote it
    _ = store.get("ns", "a");

    // This should evict "b" (now least recently used)
    try store.set("ns", "d", "4", null);

    try std.testing.expect(store.get("ns", "a") != null); // was promoted
    try std.testing.expect(store.get("ns", "b") == null); // evicted
}

test "CacheStore: namespace isolation" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    try store.set("ns1", "key", "value1", null);
    try store.set("ns2", "key", "value2", null);

    const v1 = store.get("ns1", "key") orelse return error.TestFailed;
    const v2 = store.get("ns2", "key") orelse return error.TestFailed;

    try std.testing.expectEqualStrings("value1", v1);
    try std.testing.expectEqualStrings("value2", v2);
}

test "CacheStore: overwrite existing key" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    try store.set("ns", "key", "old", null);
    try store.set("ns", "key", "new", null);

    const result = store.get("ns", "key") orelse return error.TestFailed;
    try std.testing.expectEqualStrings("new", result);
    try std.testing.expect(store.total_entries == 1);
}

test "CacheStore: increment" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    // Increment non-existent key (starts at 0)
    const v1 = store.get("ns", "counter") orelse "0";
    const c1 = std.fmt.parseInt(i64, v1, 10) catch 0;
    try store.set("ns", "counter", "1", null);
    try std.testing.expect(c1 == 0);

    // Increment again
    const v2 = store.get("ns", "counter") orelse "0";
    const c2 = std.fmt.parseInt(i64, v2, 10) catch 0;
    try std.testing.expect(c2 == 1);

    const new_val = c2 + 5;
    var buf: [32]u8 = undefined;
    const s = try std.fmt.bufPrint(&buf, "{d}", .{new_val});
    try store.set("ns", "counter", s, null);

    const v3 = store.get("ns", "counter") orelse return error.TestFailed;
    try std.testing.expectEqualStrings("6", v3);
}

test "CacheStore: stats tracking" {
    const allocator = std.testing.allocator;
    var store = CacheStore.init(allocator);
    defer store.deinitSelf();

    try store.set("ns", "key", "val", null);
    _ = store.get("ns", "key"); // hit
    _ = store.get("ns", "miss"); // miss

    const ns_cache = store.namespaces.get("ns") orelse return error.TestFailed;
    try std.testing.expect(ns_cache.hits == 1);
    try std.testing.expect(ns_cache.misses == 1);
    try std.testing.expect(store.total_entries == 1);
}
