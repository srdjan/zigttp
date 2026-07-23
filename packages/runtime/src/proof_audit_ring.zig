//! Proof Audit Ring
//!
//! A bounded, mutex-guarded ring of "proof did something" events emitted by
//! the request path. The ring lets the live HUD's request pane show that the
//! compiler-proven facts are actually doing work at runtime, without forcing
//! request handlers through any new dependency.
//!
//! Today the ring captures two events:
//!   - `cache_hit`: the proof cache served a response without entering JS,
//!     activated by a `pure` or (`deterministic` + `read_only`) handler.
//!   - `route_blocked`: a request was rejected at the HTTP layer because the
//!     handler's proven route table did not include the method+path pair.
//!
//! The ring is a process-global singleton because both emitters (`server.zig`)
//! and the consumer (`proof_card_tui.zig`'s request-pane builder) live in the
//! same runtime process. Tests call `clear()` to reset state between cases.
//!
//! No allocations: events store method (8 bytes) and path (64 bytes) in
//! fixed inline arrays so the ring can be statically sized and the emit path
//! is signal-safe in the common case.

const std = @import("std");
const compat = @import("zts").compat;

pub const EventKind = enum(u8) {
    cache_hit,
    route_blocked,

    pub fn label(self: EventKind) []const u8 {
        return switch (self) {
            .cache_hit => "cache hit",
            .route_blocked => "route blocked",
        };
    }
};

pub const AuditEvent = struct {
    kind: EventKind,
    method: [8]u8 = std.mem.zeroes([8]u8),
    method_len: u8 = 0,
    path: [64]u8 = std.mem.zeroes([64]u8),
    path_len: u8 = 0,

    pub fn methodSlice(self: *const AuditEvent) []const u8 {
        return self.method[0..self.method_len];
    }

    pub fn pathSlice(self: *const AuditEvent) []const u8 {
        return self.path[0..self.path_len];
    }
};

pub const RING_CAPACITY: usize = 32;

var ring: [RING_CAPACITY]AuditEvent = undefined;
var ring_head: usize = 0;
var ring_count: usize = 0;
var ring_mutex: compat.Mutex = .{};

pub fn pushCacheHit(method: []const u8, path: []const u8) void {
    push(buildEvent(.cache_hit, method, path));
}

pub fn pushRouteBlocked(method: []const u8, path: []const u8) void {
    push(buildEvent(.route_blocked, method, path));
}

/// Copy the most recent events (up to `out.len`) into `out` in chronological
/// order: `out[0]` is the oldest of the kept window, `out[len-1]` is the
/// newest. Returns the number of events written.
pub fn snapshot(out: []AuditEvent) usize {
    ring_mutex.lock();
    defer ring_mutex.unlock();
    const n = @min(out.len, ring_count);
    if (n == 0) return 0;
    var idx: usize = (ring_head + RING_CAPACITY - n) % RING_CAPACITY;
    var i: usize = 0;
    while (i < n) : (i += 1) {
        out[i] = ring[idx];
        idx = (idx + 1) % RING_CAPACITY;
    }
    return n;
}

/// Reset ring state. Test-only.
pub fn clear() void {
    ring_mutex.lock();
    defer ring_mutex.unlock();
    ring_head = 0;
    ring_count = 0;
}

fn push(event: AuditEvent) void {
    ring_mutex.lock();
    defer ring_mutex.unlock();
    ring[ring_head] = event;
    ring_head = (ring_head + 1) % RING_CAPACITY;
    if (ring_count < RING_CAPACITY) ring_count += 1;
}

fn buildEvent(kind: EventKind, method: []const u8, path: []const u8) AuditEvent {
    var event: AuditEvent = .{ .kind = kind };
    const m = @min(method.len, event.method.len);
    @memcpy(event.method[0..m], method[0..m]);
    event.method_len = @intCast(m);
    const p = @min(path.len, event.path.len);
    @memcpy(event.path[0..p], path[0..p]);
    event.path_len = @intCast(p);
    return event;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "push and snapshot in chronological order" {
    clear();
    pushCacheHit("GET", "/healthz");
    pushRouteBlocked("POST", "/admin");
    pushCacheHit("GET", "/api/items");

    var buf: [4]AuditEvent = undefined;
    const n = snapshot(&buf);
    try std.testing.expectEqual(@as(usize, 3), n);

    try std.testing.expectEqual(EventKind.cache_hit, buf[0].kind);
    try std.testing.expectEqualStrings("GET", buf[0].methodSlice());
    try std.testing.expectEqualStrings("/healthz", buf[0].pathSlice());

    try std.testing.expectEqual(EventKind.route_blocked, buf[1].kind);
    try std.testing.expectEqualStrings("/admin", buf[1].pathSlice());

    try std.testing.expectEqual(EventKind.cache_hit, buf[2].kind);
    try std.testing.expectEqualStrings("/api/items", buf[2].pathSlice());
}

test "snapshot returns only the newest window when ring overflows" {
    clear();
    var i: usize = 0;
    while (i < RING_CAPACITY + 5) : (i += 1) {
        var path_buf: [16]u8 = undefined;
        const path = std.fmt.bufPrint(&path_buf, "/p/{d}", .{i}) catch unreachable;
        pushCacheHit("GET", path);
    }

    var buf: [RING_CAPACITY]AuditEvent = undefined;
    const n = snapshot(&buf);
    try std.testing.expectEqual(RING_CAPACITY, n);

    // Oldest kept is iteration index 5; newest is iteration index CAP+4.
    var first_path_buf: [16]u8 = undefined;
    const first = std.fmt.bufPrint(&first_path_buf, "/p/{d}", .{5}) catch unreachable;
    try std.testing.expectEqualStrings(first, buf[0].pathSlice());

    var last_path_buf: [16]u8 = undefined;
    const last = std.fmt.bufPrint(&last_path_buf, "/p/{d}", .{RING_CAPACITY + 4}) catch unreachable;
    try std.testing.expectEqualStrings(last, buf[n - 1].pathSlice());
}

test "long path is truncated to fixed buffer" {
    clear();
    const long = "/" ++ "a" ** 200;
    pushCacheHit("GET", long);

    var buf: [1]AuditEvent = undefined;
    const n = snapshot(&buf);
    try std.testing.expectEqual(@as(usize, 1), n);
    try std.testing.expectEqual(@as(usize, 64), buf[0].pathSlice().len);
}

test "EventKind.label gives stable strings" {
    try std.testing.expectEqualStrings("cache hit", EventKind.cache_hit.label());
    try std.testing.expectEqualStrings("route blocked", EventKind.route_blocked.label());
}
