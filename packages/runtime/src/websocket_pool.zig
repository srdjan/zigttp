//! In-memory registry of live WebSocket connections.
//!
//! W1-d's first building block. The pool owns per-connection metadata
//! (fd, room membership, lifecycle state, last-frame timestamp) and
//! exposes room-scoped iteration so handlers can broadcast via
//! `getWebSockets(roomKey)`. Connection state and I/O continue to live
//! on the gateway and codec layers respectively — this module is a pure
//! index.
//!
//! Concurrency: a single coarse mutex guards all mutations. Accept-path
//! registration, per-frame state updates, and broadcast-path reads all
//! pass through the same lock. The critical sections are small (hash
//! map insert/lookup, array append, pointer copy) so contention stays
//! low until measured data says otherwise. A finer scheme (per-room
//! locks, lock-free lookups) is deliberately deferred.
//!
//! Lifetime: the pool does not close sockets or free handler state.
//! The caller that registered a connection is responsible for calling
//! `unregister` before the fd is closed. `unregister` is idempotent.

const std = @import("std");

pub const ConnectionId = u64;

pub const ConnectionState = enum {
    /// Handler currently executing against this connection.
    live,
    /// Idle but the owning runtime is still hot. Default after onOpen
    /// and between messages.
    parked,
    /// Idle long enough that the runtime has been released. The fd is
    /// still registered in the evented loop; next inbound frame
    /// triggers rehydrate. Set by W3.
    dormant,
};

pub const Connection = struct {
    id: ConnectionId,
    fd: std.posix.fd_t,
    /// Allocator-owned copy of the room key. Borrowed from argv or
    /// URL slices is not safe — the pool outlives request buffers.
    room: []const u8,
    state: ConnectionState,
    /// Wall-clock ms of the last inbound frame. Hibernation eligibility
    /// is `now - last_frame_at_ms > idle_threshold_ms` (W3 uses this).
    last_frame_at_ms: i64,
};

pub const Pool = struct {
    allocator: std.mem.Allocator,
    mutex: std.atomic.Mutex = .unlocked,
    by_id: std.AutoHashMapUnmanaged(ConnectionId, *Connection) = .empty,
    by_room: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(ConnectionId)) = .empty,
    next_id: std.atomic.Value(u64) = std.atomic.Value(u64).init(1),

    pub fn init(allocator: std.mem.Allocator) Pool {
        return .{ .allocator = allocator };
    }

    /// Acquire the pool's spin-lock. Critical sections are tiny
    /// (hash-map insert/lookup, slice copy), so spinning costs less
    /// than setting up a blocking-mutex backend. If contention ever
    /// matters, revisit.
    fn lock(self: *Pool) void {
        while (!self.mutex.tryLock()) std.atomic.spinLoopHint();
    }

    fn unlockLock(self: *Pool) void {
        self.mutex.unlock();
    }

    pub fn deinit(self: *Pool) void {
        self.lock();
        defer self.unlockLock();

        var id_it = self.by_id.iterator();
        while (id_it.next()) |entry| {
            const conn = entry.value_ptr.*;
            self.allocator.free(conn.room);
            self.allocator.destroy(conn);
        }
        self.by_id.deinit(self.allocator);

        var room_it = self.by_room.iterator();
        while (room_it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
            self.allocator.free(entry.key_ptr.*);
        }
        self.by_room.deinit(self.allocator);
    }

    /// Register a new connection under `room_key` and return a stable
    /// ConnectionId. The room key is duped into pool-owned memory; the
    /// caller may free or reuse the source slice after this call
    /// returns.
    pub fn register(
        self: *Pool,
        fd: std.posix.fd_t,
        room_key: []const u8,
        now_ms: i64,
    ) !ConnectionId {
        self.lock();
        defer self.unlockLock();

        const room_owned = try self.allocator.dupe(u8, room_key);
        errdefer self.allocator.free(room_owned);

        const conn = try self.allocator.create(Connection);
        errdefer self.allocator.destroy(conn);

        const id = self.next_id.fetchAdd(1, .monotonic);
        conn.* = .{
            .id = id,
            .fd = fd,
            .room = room_owned,
            .state = .parked,
            .last_frame_at_ms = now_ms,
        };

        try self.by_id.put(self.allocator, id, conn);
        errdefer _ = self.by_id.remove(id);

        try self.appendRoomLocked(room_owned, id);
        return id;
    }

    /// Remove a connection. Idempotent: double-unregister is a no-op.
    /// Does not close the fd — the caller owns the socket lifecycle.
    pub fn unregister(self: *Pool, id: ConnectionId) void {
        self.lock();
        defer self.unlockLock();

        const conn_entry = self.by_id.fetchRemove(id) orelse return;
        const conn = conn_entry.value;

        self.removeFromRoomLocked(conn.room, id);
        self.allocator.free(conn.room);
        self.allocator.destroy(conn);
    }

    /// Snapshot the connection data for a given id. Returns a value,
    /// not a pointer, because the internal Connection may be destroyed
    /// by a concurrent unregister; callers reading via this helper are
    /// safe because the snapshot is stable.
    pub fn snapshot(self: *Pool, id: ConnectionId) ?Connection {
        self.lock();
        defer self.unlockLock();
        const conn = self.by_id.get(id) orelse return null;
        return conn.*;
    }

    /// Write the set of connection ids in a room into `out`, which must
    /// have capacity for `countInRoom(room)` entries. Returns the slice
    /// of ids actually written. The ids remain valid for the duration
    /// of the caller's frame — a concurrent unregister may invalidate
    /// them, so broadcast loops should tolerate `snapshot(id) == null`.
    pub fn collectRoom(self: *Pool, room_key: []const u8, out: []ConnectionId) []ConnectionId {
        self.lock();
        defer self.unlockLock();

        const list = self.by_room.getPtr(room_key) orelse return out[0..0];
        const n = @min(list.items.len, out.len);
        std.mem.copyForwards(ConnectionId, out[0..n], list.items[0..n]);
        return out[0..n];
    }

    pub fn countInRoom(self: *Pool, room_key: []const u8) usize {
        self.lock();
        defer self.unlockLock();
        const list = self.by_room.getPtr(room_key) orelse return 0;
        return list.items.len;
    }

    /// Update lifecycle metadata for a single connection. Returns
    /// `false` if the id no longer exists.
    pub fn touch(self: *Pool, id: ConnectionId, state: ConnectionState, now_ms: i64) bool {
        self.lock();
        defer self.unlockLock();
        const conn = self.by_id.get(id) orelse return false;
        conn.state = state;
        conn.last_frame_at_ms = now_ms;
        return true;
    }

    fn appendRoomLocked(self: *Pool, room_key_owned: []const u8, id: ConnectionId) !void {
        if (self.by_room.getPtr(room_key_owned)) |list| {
            try list.append(self.allocator, id);
            return;
        }
        // First member of the room: put a fresh list under a newly
        // duped key so the map owns its keys independently of the
        // connection structs.
        const key_copy = try self.allocator.dupe(u8, room_key_owned);
        errdefer self.allocator.free(key_copy);

        var list: std.ArrayListUnmanaged(ConnectionId) = .empty;
        errdefer list.deinit(self.allocator);
        try list.append(self.allocator, id);

        try self.by_room.put(self.allocator, key_copy, list);
    }

    fn removeFromRoomLocked(self: *Pool, room_key: []const u8, id: ConnectionId) void {
        const list_ptr = self.by_room.getPtr(room_key) orelse return;
        var i: usize = 0;
        while (i < list_ptr.items.len) : (i += 1) {
            if (list_ptr.items[i] == id) {
                _ = list_ptr.swapRemove(i);
                break;
            }
        }
        if (list_ptr.items.len == 0) {
            // Last member of the room: free the room's entry entirely
            // so the map doesn't grow monotonically as rooms open and
            // close.
            list_ptr.deinit(self.allocator);
            if (self.by_room.fetchRemove(room_key)) |entry| {
                self.allocator.free(entry.key);
            }
        }
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "register returns incrementing ids and snapshot reflects state" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    const a = try pool.register(100, "alpha", 1000);
    const b = try pool.register(101, "alpha", 1001);
    try testing.expect(b > a);

    const snap = pool.snapshot(a) orelse return error.TestFailed;
    try testing.expectEqual(@as(std.posix.fd_t, 100), snap.fd);
    try testing.expectEqualStrings("alpha", snap.room);
    try testing.expectEqual(ConnectionState.parked, snap.state);
}

test "collectRoom returns all members of a room" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    const a = try pool.register(100, "alpha", 1000);
    const b = try pool.register(101, "alpha", 1001);
    _ = try pool.register(102, "beta", 1002);

    var buf: [8]ConnectionId = undefined;
    const alpha = pool.collectRoom("alpha", &buf);
    try testing.expectEqual(@as(usize, 2), alpha.len);
    // Order isn't part of the contract; just assert membership.
    const has_a = alpha[0] == a or alpha[1] == a;
    const has_b = alpha[0] == b or alpha[1] == b;
    try testing.expect(has_a);
    try testing.expect(has_b);

    try testing.expectEqual(@as(usize, 1), pool.countInRoom("beta"));
    try testing.expectEqual(@as(usize, 0), pool.countInRoom("gamma"));
}

test "unregister removes from both indices and is idempotent" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    const id = try pool.register(100, "alpha", 1000);
    pool.unregister(id);
    try testing.expectEqual(@as(usize, 0), pool.countInRoom("alpha"));
    try testing.expect(pool.snapshot(id) == null);

    // Idempotent.
    pool.unregister(id);
    try testing.expectEqual(@as(usize, 0), pool.countInRoom("alpha"));
}

test "room entry is pruned when the last member leaves" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    const a = try pool.register(100, "alpha", 1000);
    const b = try pool.register(101, "alpha", 1001);

    pool.unregister(a);
    try testing.expectEqual(@as(usize, 1), pool.countInRoom("alpha"));
    pool.unregister(b);
    try testing.expectEqual(@as(usize, 0), pool.countInRoom("alpha"));

    // The internal map key+list should be freed at this point; the
    // deinit below verifies that by not leaking.
}

test "touch updates state and timestamp, returns false for missing id" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    const id = try pool.register(100, "alpha", 1000);
    try testing.expect(pool.touch(id, .dormant, 2000));

    const snap = pool.snapshot(id) orelse return error.TestFailed;
    try testing.expectEqual(ConnectionState.dormant, snap.state);
    try testing.expectEqual(@as(i64, 2000), snap.last_frame_at_ms);

    try testing.expect(!pool.touch(9999, .live, 3000));
}

test "collectRoom truncates to output buffer capacity" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    for (0..10) |i| {
        _ = try pool.register(@intCast(100 + i), "alpha", 1000);
    }

    var buf: [4]ConnectionId = undefined;
    const got = pool.collectRoom("alpha", &buf);
    try testing.expectEqual(@as(usize, 4), got.len);
    try testing.expectEqual(@as(usize, 10), pool.countInRoom("alpha"));
}
