//! Proof-driven runtime adapter: compile-time proofs drive runtime behavior.
//!
//! When the handler contract proves properties like `deterministic` and `read_only`,
//! the runtime can safely optimize request handling without configuration. This module
//! implements the first such optimization: response memoization for proven-deterministic
//! handlers. A GET request that produced a given response will produce the same response
//! on every subsequent call - the compiler proved it - so we cache and serve directly
//! from Zig without entering the JS runtime.
//!
//! Activation: automatic when contract proves `pure` OR (`deterministic` AND `read_only`).
//! No configuration required. The proof IS the configuration.

const std = @import("std");
const compat = @import("compat.zig");
const http_types = @import("http_types.zig");
const contract_runtime = @import("contract_runtime.zig");

const HttpResponse = http_types.HttpResponse;
const HttpHeader = http_types.HttpHeader;
const Wyhash = std.hash.Wyhash;

// ============================================================================
// Types
// ============================================================================

const OwnedHeader = struct {
    key: []u8,
    value: []u8,
};

const CacheEntry = struct {
    status: u16,
    headers: []OwnedHeader,
    body: []u8,
    created_ns: u64,
    byte_size: usize,
};

// ============================================================================
// ProofCache
// ============================================================================

pub const ProofCache = struct {
    allocator: std.mem.Allocator,
    entries: std.AutoHashMapUnmanaged(u64, CacheEntry),
    insertion_order: std.ArrayListUnmanaged(u64),
    rw_lock: compat.RwLock,
    max_entries: u32,
    ttl_ns: u64,
    max_body_size: usize,
    hits: std.atomic.Value(u64),
    misses: std.atomic.Value(u64),
    enabled: bool,
    total_bytes: std.atomic.Value(usize),

    pub const Config = struct {
        max_entries: u32 = 1024,
        ttl_seconds: u32 = 300,
        max_body_size: usize = 256 * 1024,
    };

    pub const Stats = struct {
        hits: u64,
        misses: u64,
        entries: u32,
        total_bytes: usize,
    };

    /// Create a ProofCache. Enabled only when properties prove the handler is
    /// deterministic and side-effect-free (pure, or deterministic + read_only).
    pub fn init(
        allocator: std.mem.Allocator,
        properties: contract_runtime.Properties,
        config: Config,
    ) ProofCache {
        const cacheable = properties.pure or (properties.deterministic and properties.read_only);
        return .{
            .allocator = allocator,
            .entries = .{},
            .insertion_order = .empty,
            .rw_lock = .{},
            .max_entries = config.max_entries,
            .ttl_ns = @as(u64, config.ttl_seconds) * std.time.ns_per_s,
            .max_body_size = config.max_body_size,
            .hits = std.atomic.Value(u64).init(0),
            .misses = std.atomic.Value(u64).init(0),
            .enabled = cacheable,
            .total_bytes = std.atomic.Value(usize).init(0),
        };
    }

    pub fn deinit(self: *ProofCache) void {
        var it = self.entries.iterator();
        while (it.next()) |kv| {
            self.freeEntry(kv.value_ptr);
        }
        self.entries.deinit(self.allocator);
        self.insertion_order.deinit(self.allocator);
    }

    /// Check if a request method and headers qualify for caching.
    /// Only GET and HEAD are cached. Cache-Control: no-cache bypasses the cache.
    pub fn shouldCache(method: []const u8, headers: std.ArrayListUnmanaged(HttpHeader)) bool {
        // Only cache GET and HEAD
        if (!std.ascii.eqlIgnoreCase(method, "GET") and
            !std.ascii.eqlIgnoreCase(method, "HEAD"))
        {
            return false;
        }

        // Respect Cache-Control: no-cache / no-store
        for (headers.items) |header| {
            if (std.ascii.eqlIgnoreCase(header.key, "cache-control")) {
                if (std.mem.indexOf(u8, header.value, "no-cache") != null or
                    std.mem.indexOf(u8, header.value, "no-store") != null)
                {
                    return false;
                }
            }
        }

        return true;
    }

    /// Compute the cache key from a request's method and URL.
    /// The URL includes path and query string, which fully identifies a GET request.
    pub fn computeKey(method: []const u8, url: []const u8) u64 {
        var hasher = Wyhash.init(0);
        hasher.update(method);
        hasher.update("\x00");
        hasher.update(url);
        return hasher.final();
    }

    /// Look up a cached response. Returns an owned HttpResponse on hit (caller must
    /// call deinit on it), or null on miss or TTL expiry.
    pub fn get(self: *ProofCache, key: u64, resp_allocator: std.mem.Allocator) ?HttpResponse {
        if (!self.enabled) return null;

        self.rw_lock.lockShared();
        defer self.rw_lock.unlockShared();

        const entry = self.entries.get(key) orelse {
            _ = self.misses.fetchAdd(1, .monotonic);
            return null;
        };

        // Check TTL
        const now_ns = compat.monotonicNowNs() catch 0;
        if (self.ttl_ns > 0 and now_ns > entry.created_ns and
            now_ns - entry.created_ns > self.ttl_ns)
        {
            _ = self.misses.fetchAdd(1, .monotonic);
            return null;
        }

        // Clone into an owned HttpResponse
        const response = cloneToResponse(&entry, resp_allocator) catch {
            _ = self.misses.fetchAdd(1, .monotonic);
            return null;
        };

        _ = self.hits.fetchAdd(1, .monotonic);
        return response;
    }

    /// Store a response in the cache. Deep-copies all data from the response.
    /// Silently drops entries whose body exceeds max_body_size, or if the cache
    /// is not enabled.
    pub fn put(self: *ProofCache, key: u64, response: *const HttpResponse) void {
        if (!self.enabled) return;
        if (response.body.len > self.max_body_size) return;

        // Deep-copy outside the lock. Manual cleanup on failure since errdefer
        // does not fire in void functions (no error return path).
        const body_copy = self.allocator.dupe(u8, response.body) catch return;

        const headers_copy = self.allocator.alloc(OwnedHeader, response.headers.items.len) catch {
            self.allocator.free(body_copy);
            return;
        };
        var headers_initialized: usize = 0;

        for (response.headers.items, 0..) |h, i| {
            const key_dup = self.allocator.dupe(u8, h.key) catch {
                self.freePartialCopy(headers_copy, headers_initialized, body_copy);
                return;
            };
            const val_dup = self.allocator.dupe(u8, h.value) catch {
                self.allocator.free(key_dup);
                self.freePartialCopy(headers_copy, headers_initialized, body_copy);
                return;
            };
            headers_copy[i] = .{ .key = key_dup, .value = val_dup };
            headers_initialized = i + 1;
        }

        var byte_size: usize = body_copy.len + headers_copy.len * @sizeOf(OwnedHeader);
        for (headers_copy) |h| {
            byte_size += h.key.len + h.value.len;
        }

        const now_ns = compat.monotonicNowNs() catch 0;

        const new_entry = CacheEntry{
            .status = response.status,
            .headers = headers_copy,
            .body = body_copy,
            .created_ns = now_ns,
            .byte_size = byte_size,
        };

        // Acquire exclusive lock for mutation
        self.rw_lock.lock();
        defer self.rw_lock.unlock();

        // Single lookup: getOrPut avoids double hash probe on miss
        const gop = self.entries.getOrPut(self.allocator, key) catch {
            var copy = new_entry;
            self.freeEntry(&copy);
            return;
        };

        if (gop.found_existing) {
            _ = self.total_bytes.fetchSub(gop.value_ptr.byte_size, .monotonic);
            self.freeEntry(gop.value_ptr);
        } else {
            // Evict if at capacity (getOrPut already inserted the slot)
            if (self.entries.count() > self.max_entries and self.insertion_order.items.len > 0) {
                const oldest_key = self.insertion_order.swapRemove(0);
                if (self.entries.fetchRemove(oldest_key)) |removed| {
                    _ = self.total_bytes.fetchSub(removed.value.byte_size, .monotonic);
                    var entry = removed.value;
                    self.freeEntry(&entry);
                }
            }
            self.insertion_order.append(self.allocator, key) catch {};
        }

        gop.value_ptr.* = new_entry;
        _ = self.total_bytes.fetchAdd(byte_size, .monotonic);
    }

    fn freePartialCopy(self: *ProofCache, headers: []OwnedHeader, count: usize, body: []u8) void {
        for (headers[0..count]) |h| {
            self.allocator.free(h.key);
            self.allocator.free(h.value);
        }
        self.allocator.free(headers);
        self.allocator.free(body);
    }

    pub fn getStats(self: *const ProofCache) Stats {
        return .{
            .hits = self.hits.load(.monotonic),
            .misses = self.misses.load(.monotonic),
            .entries = @intCast(self.entries.count()),
            .total_bytes = self.total_bytes.load(.monotonic),
        };
    }

    // -- Private helpers --

    fn freeEntry(self: *ProofCache, entry: *CacheEntry) void {
        for (entry.headers) |h| {
            self.allocator.free(h.key);
            self.allocator.free(h.value);
        }
        self.allocator.free(entry.headers);
        self.allocator.free(entry.body);
    }

    fn cloneToResponse(entry: *const CacheEntry, resp_allocator: std.mem.Allocator) !HttpResponse {
        var response = HttpResponse.init(resp_allocator);
        errdefer response.deinit();

        response.status = entry.status;

        // Deep-copy body
        if (entry.body.len > 0) {
            const body_copy = try resp_allocator.dupe(u8, entry.body);
            response.setBodyOwned(body_copy);
        }

        // Deep-copy headers (putHeader dupes internally)
        for (entry.headers) |h| {
            try response.putHeader(h.key, h.value);
        }

        // Mark as cache hit
        try response.putHeaderBorrowed("X-Zigttp-Proof-Cache", "hit");

        return response;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "computeKey deterministic" {
    const k1 = ProofCache.computeKey("GET", "/api/health");
    const k2 = ProofCache.computeKey("GET", "/api/health");
    const k3 = ProofCache.computeKey("GET", "/api/users");
    const k4 = ProofCache.computeKey("POST", "/api/health");

    try std.testing.expectEqual(k1, k2);
    try std.testing.expect(k1 != k3);
    try std.testing.expect(k1 != k4);
}

test "computeKey includes query string" {
    const k1 = ProofCache.computeKey("GET", "/api/users?page=1");
    const k2 = ProofCache.computeKey("GET", "/api/users?page=2");
    const k3 = ProofCache.computeKey("GET", "/api/users?page=1");

    try std.testing.expect(k1 != k2);
    try std.testing.expectEqual(k1, k3);
}

test "shouldCache GET only" {
    const empty_headers: std.ArrayListUnmanaged(HttpHeader) = .empty;

    try std.testing.expect(ProofCache.shouldCache("GET", empty_headers));
    try std.testing.expect(ProofCache.shouldCache("HEAD", empty_headers));
    try std.testing.expect(ProofCache.shouldCache("get", empty_headers));
    try std.testing.expect(!ProofCache.shouldCache("POST", empty_headers));
    try std.testing.expect(!ProofCache.shouldCache("PUT", empty_headers));
    try std.testing.expect(!ProofCache.shouldCache("DELETE", empty_headers));
    try std.testing.expect(!ProofCache.shouldCache("PATCH", empty_headers));
}

test "shouldCache respects no-cache" {
    const allocator = std.testing.allocator;

    var headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
    defer headers.deinit(allocator);
    try headers.append(allocator, .{ .key = "cache-control", .value = "no-cache" });

    try std.testing.expect(!ProofCache.shouldCache("GET", headers));
}

test "shouldCache respects no-store" {
    const allocator = std.testing.allocator;

    var headers: std.ArrayListUnmanaged(HttpHeader) = .empty;
    defer headers.deinit(allocator);
    try headers.append(allocator, .{ .key = "cache-control", .value = "no-store, max-age=0" });

    try std.testing.expect(!ProofCache.shouldCache("GET", headers));
}

test "disabled when properties not met" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{}, .{});
    defer cache.deinit();

    try std.testing.expect(!cache.enabled);

    // get and put are no-ops when disabled
    const key = ProofCache.computeKey("GET", "/test");
    try std.testing.expect(cache.get(key, allocator) == null);

    var resp = HttpResponse.init(allocator);
    defer resp.deinit();
    resp.status = 200;
    cache.put(key, &resp); // should not crash

    try std.testing.expect(cache.get(key, allocator) == null);
}

test "enabled for pure handler" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .pure = true }, .{});
    defer cache.deinit();

    try std.testing.expect(cache.enabled);
}

test "enabled for deterministic + read_only handler" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .deterministic = true, .read_only = true }, .{});
    defer cache.deinit();

    try std.testing.expect(cache.enabled);
}

test "not enabled for deterministic-only handler" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .deterministic = true }, .{});
    defer cache.deinit();

    try std.testing.expect(!cache.enabled);
}

test "put and get round-trip" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .pure = true }, .{});
    defer cache.deinit();

    // Build a response to cache
    var resp = HttpResponse.init(allocator);
    defer resp.deinit();
    resp.status = 200;
    const body = try allocator.dupe(u8, "{\"ok\":true}");
    resp.setBodyOwned(body);
    try resp.putHeader("content-type", "application/json");

    const key = ProofCache.computeKey("GET", "/api/health");
    cache.put(key, &resp);

    // Retrieve it
    var cached = cache.get(key, allocator) orelse return error.TestUnexpectedResult;
    defer cached.deinit();

    try std.testing.expectEqual(@as(u16, 200), cached.status);
    try std.testing.expectEqualStrings("{\"ok\":true}", cached.body);

    // Should have original header + proof-cache header
    var found_content_type = false;
    var found_proof_cache = false;
    for (cached.headers.items) |h| {
        if (std.ascii.eqlIgnoreCase(h.key, "content-type")) {
            try std.testing.expectEqualStrings("application/json", h.value);
            found_content_type = true;
        }
        if (std.ascii.eqlIgnoreCase(h.key, "X-Zigttp-Proof-Cache")) {
            try std.testing.expectEqualStrings("hit", h.value);
            found_proof_cache = true;
        }
    }
    try std.testing.expect(found_content_type);
    try std.testing.expect(found_proof_cache);
}

test "get returns null on miss" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .pure = true }, .{});
    defer cache.deinit();

    const key = ProofCache.computeKey("GET", "/nonexistent");
    try std.testing.expect(cache.get(key, allocator) == null);
}

test "capacity eviction" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .pure = true }, .{ .max_entries = 2 });
    defer cache.deinit();

    // Insert 3 entries - first should be evicted
    const key1 = ProofCache.computeKey("GET", "/one");
    const key2 = ProofCache.computeKey("GET", "/two");
    const key3 = ProofCache.computeKey("GET", "/three");

    var resp = HttpResponse.init(allocator);
    resp.status = 200;
    cache.put(key1, &resp);

    resp.status = 201;
    cache.put(key2, &resp);

    resp.status = 202;
    cache.put(key3, &resp);
    resp.deinit();

    // key1 should be evicted
    try std.testing.expect(cache.get(key1, allocator) == null);

    // key2 and key3 should still be present
    var c2 = cache.get(key2, allocator) orelse return error.TestUnexpectedResult;
    defer c2.deinit();
    try std.testing.expectEqual(@as(u16, 201), c2.status);

    var c3 = cache.get(key3, allocator) orelse return error.TestUnexpectedResult;
    defer c3.deinit();
    try std.testing.expectEqual(@as(u16, 202), c3.status);
}

test "max_body_size enforcement" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .pure = true }, .{ .max_body_size = 16 });
    defer cache.deinit();

    var resp = HttpResponse.init(allocator);
    defer resp.deinit();
    resp.status = 200;
    // Body larger than max_body_size
    const big_body = try allocator.dupe(u8, "this body exceeds the sixteen byte limit");
    resp.setBodyOwned(big_body);

    const key = ProofCache.computeKey("GET", "/big");
    cache.put(key, &resp);

    // Should not be cached
    try std.testing.expect(cache.get(key, allocator) == null);
}

test "deep copy independence" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .pure = true }, .{});
    defer cache.deinit();

    const key = ProofCache.computeKey("GET", "/test");

    // Build and cache a response, then destroy the original
    {
        var resp = HttpResponse.init(allocator);
        defer resp.deinit();
        resp.status = 200;
        const body = try allocator.dupe(u8, "original");
        resp.setBodyOwned(body);
        try resp.putHeader("x-test", "value");

        cache.put(key, &resp);
    }
    // Original is now freed

    // Cached copy should still be valid
    var cached = cache.get(key, allocator) orelse return error.TestUnexpectedResult;
    defer cached.deinit();

    try std.testing.expectEqual(@as(u16, 200), cached.status);
    try std.testing.expectEqualStrings("original", cached.body);
}

test "stats tracking" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .pure = true }, .{});
    defer cache.deinit();

    const key = ProofCache.computeKey("GET", "/test");

    // Miss
    _ = cache.get(key, allocator);

    // Put + hit
    var resp = HttpResponse.init(allocator);
    resp.status = 200;
    cache.put(key, &resp);
    resp.deinit();

    var cached = cache.get(key, allocator) orelse return error.TestUnexpectedResult;
    cached.deinit();

    const stats = cache.getStats();
    try std.testing.expectEqual(@as(u64, 1), stats.hits);
    try std.testing.expectEqual(@as(u64, 1), stats.misses);
    try std.testing.expectEqual(@as(u32, 1), stats.entries);
}

test "replacing existing key" {
    const allocator = std.testing.allocator;
    var cache = ProofCache.init(allocator, .{ .pure = true }, .{});
    defer cache.deinit();

    const key = ProofCache.computeKey("GET", "/test");

    // Insert first version
    var resp1 = HttpResponse.init(allocator);
    resp1.status = 200;
    const body1 = try allocator.dupe(u8, "v1");
    resp1.setBodyOwned(body1);
    cache.put(key, &resp1);
    resp1.deinit();

    // Replace with second version
    var resp2 = HttpResponse.init(allocator);
    resp2.status = 201;
    const body2 = try allocator.dupe(u8, "v2");
    resp2.setBodyOwned(body2);
    cache.put(key, &resp2);
    resp2.deinit();

    // Should get the second version
    var cached = cache.get(key, allocator) orelse return error.TestUnexpectedResult;
    defer cached.deinit();

    try std.testing.expectEqual(@as(u16, 201), cached.status);
    try std.testing.expectEqualStrings("v2", cached.body);
}
