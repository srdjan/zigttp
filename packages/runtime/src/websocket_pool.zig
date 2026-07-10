//! In-memory registry of live WebSocket connections.
//!
//! W1-d's first building block. The pool owns per-connection metadata
//! (fd, room membership, lifecycle state, last-frame timestamp) and
//! exposes room-scoped iteration so handlers can broadcast via
//! `getWebSockets(roomKey)`. It also owns the retained per-connection
//! outbound state that serializes complete frame writes. The gateway and
//! frame loop own protocol flow; websocket_codec owns wire encoding.
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
const zq = @import("zigts");
const websocket_codec = @import("websocket_codec.zig");

const c = @cImport({
    @cInclude("dirent.h");
});

pub const ConnectionId = u64;

/// Refcounted connection-local send boundary. The pool owns one reference
/// while a connection is registered; each OutboundLease pins another while it
/// waits for or holds the mutex. This lets unregister remove and destroy the
/// Connection immediately without freeing the lock under a concurrent sender.
const OutboundState = struct {
    allocator: std.mem.Allocator,
    refs: std.atomic.Value(usize) = .init(1),
    mutex: std.Io.Mutex = .init,

    fn retain(self: *OutboundState) void {
        _ = self.refs.fetchAdd(1, .monotonic);
    }

    fn release(self: *OutboundState) void {
        if (self.refs.fetchSub(1, .acq_rel) == 1) self.allocator.destroy(self);
    }

    fn lock(self: *OutboundState) void {
        self.mutex.lockUncancelable(std.Options.debug_io);
    }

    fn unlock(self: *OutboundState) void {
        self.mutex.unlock(std.Options.debug_io);
    }
};

pub const OutboundLease = struct {
    state: ?*OutboundState,
    fd: std.posix.fd_t,

    pub fn deinit(self: *OutboundLease) void {
        const state = self.state orelse return;
        self.state = null;
        std.Io.Threaded.closeFd(self.fd);
        state.release();
    }

    pub fn writeFrame(self: *OutboundLease, opcode: websocket_codec.Opcode, payload: []const u8) !void {
        const state = self.state orelse return error.LeaseReleased;
        state.lock();
        defer state.unlock();
        try websocket_codec.writeServerFrame(self.fd, opcode, payload);
    }

    pub fn writeFrameAndShutdown(self: *OutboundLease, opcode: websocket_codec.Opcode, payload: []const u8) !void {
        const state = self.state orelse return error.LeaseReleased;
        state.lock();
        defer state.unlock();
        const result = websocket_codec.writeServerFrame(self.fd, opcode, payload);
        _ = std.c.shutdown(self.fd, std.c.SHUT.WR);
        return result;
    }
};

/// Cap on a single attachment's bytes when reading from disk. Attachments
/// are per-connection scratch space, not blob storage; 1 MiB is already
/// well beyond any realistic handler use.
const max_attachment_bytes: usize = 1 * 1024 * 1024;

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
    /// Allocator-owned per-connection attachment bytes, set by the
    /// handler via `serializeAttachment` and read back via
    /// `deserializeAttachment`. null until first write.
    attachment: ?[]u8 = null,
    /// Registered request/response pair for codec-level auto-reply.
    /// When an inbound text frame's bytes match `auto_request` exactly,
    /// the frame loop writes `auto_response` back without waking the
    /// JS handler. Both are allocator-owned; null when unset.
    auto_request: ?[]u8 = null,
    auto_response: ?[]u8 = null,
    outbound: *OutboundState,
};

pub const ConnectionSnapshot = struct {
    id: ConnectionId,
    fd: std.posix.fd_t,
    room: []const u8,
    state: ConnectionState,
    last_frame_at_ms: i64,
};

pub const Pool = struct {
    allocator: std.mem.Allocator,
    max_connections: usize,
    mutex: std.atomic.Mutex = .unlocked,
    by_id: std.AutoHashMapUnmanaged(ConnectionId, *Connection) = .empty,
    by_room: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(ConnectionId)) = .empty,
    next_id: std.atomic.Value(u64) = std.atomic.Value(u64).init(1),
    /// Directory where per-connection attachment bytes are persisted as
    /// `<id>.att`. When null the pool is in-memory only.
    attachments_dir: ?[]u8 = null,

    pub fn init(allocator: std.mem.Allocator) Pool {
        return initWithLimit(allocator, 1024);
    }

    pub fn initWithLimit(allocator: std.mem.Allocator, max_connections: usize) Pool {
        return .{ .allocator = allocator, .max_connections = max_connections };
    }

    /// Enable disk persistence of attachments under `dir`. The pool
    /// dupes the path into its own allocator so the caller can free the
    /// source slice immediately. Call before the first `setAttachment`
    /// to ensure every write lands on disk. Calling with a new path
    /// replaces the prior one and does not migrate existing files.
    pub fn setAttachmentsDir(self: *Pool, dir: []const u8) !void {
        self.lock();
        defer self.unlock();

        const owned = try self.allocator.dupe(u8, dir);
        if (self.attachments_dir) |prev| self.allocator.free(prev);
        self.attachments_dir = owned;
    }

    /// Acquire the pool's spin-lock. Critical sections are tiny
    /// (hash-map insert/lookup, slice copy), so spinning costs less
    /// than setting up a blocking-mutex backend. If contention ever
    /// matters, revisit.
    fn lock(self: *Pool) void {
        while (!self.mutex.tryLock()) std.atomic.spinLoopHint();
    }

    fn unlock(self: *Pool) void {
        self.mutex.unlock();
    }

    pub fn deinit(self: *Pool) void {
        self.lock();
        defer self.unlock();

        var id_it = self.by_id.iterator();
        while (id_it.next()) |entry| {
            const conn = entry.value_ptr.*;
            self.allocator.free(conn.room);
            if (conn.attachment) |bytes| self.allocator.free(bytes);
            if (conn.auto_request) |bytes| self.allocator.free(bytes);
            if (conn.auto_response) |bytes| self.allocator.free(bytes);
            conn.outbound.release();
            self.allocator.destroy(conn);
        }
        self.by_id.deinit(self.allocator);

        var room_it = self.by_room.iterator();
        while (room_it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
            self.allocator.free(entry.key_ptr.*);
        }
        self.by_room.deinit(self.allocator);

        if (self.attachments_dir) |dir| self.allocator.free(dir);
        self.attachments_dir = null;
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
        defer self.unlock();

        if (self.by_id.count() >= self.max_connections) return error.CapacityReached;

        const room_owned = try self.allocator.dupe(u8, room_key);
        errdefer self.allocator.free(room_owned);

        const conn = try self.allocator.create(Connection);
        errdefer self.allocator.destroy(conn);

        const outbound = try self.allocator.create(OutboundState);
        outbound.* = .{ .allocator = self.allocator };
        errdefer outbound.release();

        const id = self.next_id.fetchAdd(1, .monotonic);
        conn.* = .{
            .id = id,
            .fd = fd,
            .room = room_owned,
            .state = .parked,
            .last_frame_at_ms = now_ms,
            .attachment = null,
            .auto_request = null,
            .auto_response = null,
            .outbound = outbound,
        };

        try self.by_id.put(self.allocator, id, conn);
        errdefer _ = self.by_id.remove(id);

        try self.appendRoomLocked(room_owned, id);
        return id;
    }

    /// Remove a connection. Idempotent: double-unregister is a no-op.
    /// Does not close the fd — the caller owns the socket lifecycle.
    /// Clean shutdown: the on-disk attachment (if any) is also removed
    /// so a subsequent `listPersistedAttachmentIds` only reports ids
    /// that crashed rather than exited cleanly.
    pub fn unregister(self: *Pool, id: ConnectionId) void {
        self.lock();
        defer self.unlock();

        const conn_entry = self.by_id.fetchRemove(id) orelse return;
        const conn = conn_entry.value;

        self.removeFromRoomLocked(conn.room, id);
        self.allocator.free(conn.room);
        if (conn.attachment) |bytes| self.allocator.free(bytes);
        if (conn.auto_request) |bytes| self.allocator.free(bytes);
        if (conn.auto_response) |bytes| self.allocator.free(bytes);
        conn.outbound.release();
        self.allocator.destroy(conn);

        if (self.attachments_dir) |dir| deleteAttachmentFile(dir, id);
    }

    /// Snapshot the connection data for a given id. Returns a value,
    /// not a pointer, because the internal Connection may be destroyed
    /// by a concurrent unregister; callers reading via this helper are
    /// safe because the snapshot is stable.
    pub fn snapshot(self: *Pool, id: ConnectionId) ?ConnectionSnapshot {
        self.lock();
        defer self.unlock();
        const conn = self.by_id.get(id) orelse return null;
        return .{
            .id = conn.id,
            .fd = conn.fd,
            .room = conn.room,
            .state = conn.state,
            .last_frame_at_ms = conn.last_frame_at_ms,
        };
    }

    /// Acquire a stable connection-local send lease. The refcount and fd dup are
    /// captured under the pool lock, but no network I/O holds that coarse lock.
    pub fn acquireOutbound(self: *Pool, id: ConnectionId) !OutboundLease {
        self.lock();
        defer self.unlock();
        const conn = self.by_id.get(id) orelse return error.ConnectionNotFound;
        conn.outbound.retain();
        const new_fd = std.c.dup(conn.fd);
        if (new_fd < 0) {
            conn.outbound.release();
            return error.DuplicateSocketFailed;
        }
        return .{ .state = conn.outbound, .fd = new_fd };
    }

    pub fn sendFrame(self: *Pool, id: ConnectionId, opcode: websocket_codec.Opcode, payload: []const u8) !void {
        var lease = try self.acquireOutbound(id);
        defer lease.deinit();
        try lease.writeFrame(opcode, payload);
    }

    pub fn sendFrameAndShutdown(self: *Pool, id: ConnectionId, opcode: websocket_codec.Opcode, payload: []const u8) !void {
        var lease = try self.acquireOutbound(id);
        defer lease.deinit();
        try lease.writeFrameAndShutdown(opcode, payload);
    }

    /// Return an allocator-owned snapshot containing every connection id in a
    /// room. Registration is itself bounded by max_connections, so this cannot
    /// allocate beyond the configured server-wide WebSocket limit. The pool
    /// lock is released before callers create JS values or send frames.
    pub fn snapshotRoom(self: *Pool, room_key: []const u8, out_allocator: std.mem.Allocator) ![]ConnectionId {
        self.lock();
        defer self.unlock();

        const list = self.by_room.getPtr(room_key) orelse return try out_allocator.alloc(ConnectionId, 0);
        return try out_allocator.dupe(ConnectionId, list.items);
    }

    pub fn countInRoom(self: *Pool, room_key: []const u8) usize {
        self.lock();
        defer self.unlock();
        const list = self.by_room.getPtr(room_key) orelse return 0;
        return list.items.len;
    }

    /// Store an allocator-owned copy of `bytes` as the connection's
    /// attachment. Overwrites any prior value. Returns `false` if the
    /// id no longer exists. When `attachments_dir` is set, the bytes
    /// also land atomically at `<dir>/<id>.att` via tmp-rename so a
    /// subsequent crash leaves the last-successful write on disk.
    pub fn setAttachment(self: *Pool, id: ConnectionId, bytes: []const u8) !bool {
        self.lock();
        defer self.unlock();
        const conn = self.by_id.get(id) orelse return false;

        const owned = try self.allocator.dupe(u8, bytes);
        errdefer self.allocator.free(owned);

        if (self.attachments_dir) |dir| {
            try writeAttachmentFileAtomic(self.allocator, dir, id, bytes);
        }

        if (conn.attachment) |prev| self.allocator.free(prev);
        conn.attachment = owned;
        return true;
    }

    /// Return an allocator-owned copy of the connection's attachment.
    /// The caller takes ownership of the returned slice. Returns `null`
    /// when the connection is missing or has no attachment yet.
    pub fn copyAttachment(
        self: *Pool,
        id: ConnectionId,
        out_allocator: std.mem.Allocator,
    ) !?[]u8 {
        self.lock();
        defer self.unlock();
        const conn = self.by_id.get(id) orelse return null;
        const bytes = conn.attachment orelse return null;
        return try out_allocator.dupe(u8, bytes);
    }

    /// Register a codec-level auto-reply for this connection: when an
    /// inbound text frame's bytes exactly match `request_bytes`, the
    /// frame loop writes `response_bytes` back without dispatching to
    /// the JS handler. Calling again replaces the prior pair.
    /// Returns `false` if the id no longer exists. Pass empty slices
    /// to clear (the pool retains null-state).
    pub fn setAutoResponse(
        self: *Pool,
        id: ConnectionId,
        request_bytes: []const u8,
        response_bytes: []const u8,
    ) !bool {
        self.lock();
        defer self.unlock();
        const conn = self.by_id.get(id) orelse return false;

        if (request_bytes.len == 0 and response_bytes.len == 0) {
            if (conn.auto_request) |prev| self.allocator.free(prev);
            if (conn.auto_response) |prev| self.allocator.free(prev);
            conn.auto_request = null;
            conn.auto_response = null;
            return true;
        }

        const req_owned = try self.allocator.dupe(u8, request_bytes);
        errdefer self.allocator.free(req_owned);
        const resp_owned = try self.allocator.dupe(u8, response_bytes);
        errdefer self.allocator.free(resp_owned);

        if (conn.auto_request) |prev| self.allocator.free(prev);
        if (conn.auto_response) |prev| self.allocator.free(prev);
        conn.auto_request = req_owned;
        conn.auto_response = resp_owned;
        return true;
    }

    /// If the connection has a registered auto-response matching
    /// `data`, return an allocator-owned copy of the stored response
    /// bytes. The copy is so the caller can drop the pool lock before
    /// doing the socket write — the live slice would otherwise be
    /// invalidated if another thread called `setAutoResponse` or
    /// `unregister` concurrently.
    pub fn tryAutoResponse(
        self: *Pool,
        id: ConnectionId,
        data: []const u8,
        out_allocator: std.mem.Allocator,
    ) !?[]u8 {
        self.lock();
        defer self.unlock();
        const conn = self.by_id.get(id) orelse return null;
        const req = conn.auto_request orelse return null;
        const resp = conn.auto_response orelse return null;
        if (!std.mem.eql(u8, req, data)) return null;
        return try out_allocator.dupe(u8, resp);
    }

    /// Update lifecycle metadata for a single connection. Returns
    /// `false` if the id no longer exists.
    pub fn touch(self: *Pool, id: ConnectionId, state: ConnectionState, now_ms: i64) bool {
        self.lock();
        defer self.unlock();
        const conn = self.by_id.get(id) orelse return false;
        conn.state = state;
        conn.last_frame_at_ms = now_ms;
        return true;
    }

    /// Mark a connection as actively dispatching to the JS handler.
    /// Paired with `endDispatch` so the pool can report whether any
    /// given connection is mid-flight vs. idle. Does not update the
    /// last-frame timestamp — that belongs to inbound frame receipt,
    /// not handler execution.
    pub fn beginDispatch(self: *Pool, id: ConnectionId) bool {
        self.lock();
        defer self.unlock();
        const conn = self.by_id.get(id) orelse return false;
        conn.state = .live;
        return true;
    }

    /// Mark a connection as parked after handler execution returns.
    /// Paired with `beginDispatch`. A parked connection that stays
    /// parked past the idle threshold becomes a hibernation candidate
    /// via `scanIdle`.
    pub fn endDispatch(self: *Pool, id: ConnectionId) bool {
        self.lock();
        defer self.unlock();
        const conn = self.by_id.get(id) orelse return false;
        conn.state = .parked;
        return true;
    }

    /// Sweep parked connections whose last inbound frame was longer
    /// than `idle_threshold_ms` ago and transition them to `dormant`.
    /// Returns the subset of ids that crossed the boundary — callers
    /// can feed those into an evented reader registration once the
    /// thread-per-connection model is replaced. `live` connections
    /// are never swept (a handler that blocks indefinitely is a
    /// handler bug, not a hibernation case). Returns an allocator-
    /// owned slice; caller frees.
    pub fn scanIdle(
        self: *Pool,
        out_allocator: std.mem.Allocator,
        idle_threshold_ms: i64,
        now_ms: i64,
    ) ![]ConnectionId {
        self.lock();
        defer self.unlock();
        var out: std.ArrayList(ConnectionId) = .empty;
        errdefer out.deinit(out_allocator);

        var it = self.by_id.iterator();
        while (it.next()) |entry| {
            const conn = entry.value_ptr.*;
            if (conn.state != .parked) continue;
            if (now_ms - conn.last_frame_at_ms <= idle_threshold_ms) continue;
            conn.state = .dormant;
            try out.append(out_allocator, conn.id);
        }
        return try out.toOwnedSlice(out_allocator);
    }

    /// Rehydrate a crashed connection's attachment onto a freshly
    /// registered one. Reads `<dir>/<prior_id>.att`, installs the bytes
    /// on `new_id` (rewriting to `<dir>/<new_id>.att`), and removes the
    /// prior file. Returns `false` if no persisted file exists for
    /// `prior_id`, if `new_id` is not live, or if persistence is off.
    ///
    /// The caller is expected to validate that the client genuinely
    /// owns `prior_id` (cookie, signed token) before invoking this —
    /// the pool treats the id as trusted input.
    pub fn adoptPersistedAttachment(
        self: *Pool,
        new_id: ConnectionId,
        prior_id: ConnectionId,
    ) !bool {
        self.lock();
        defer self.unlock();
        const dir = self.attachments_dir orelse return false;
        const conn = self.by_id.get(new_id) orelse return false;

        const bytes = (try readAttachmentFile(self.allocator, dir, prior_id)) orelse return false;
        defer self.allocator.free(bytes);

        try writeAttachmentFileAtomic(self.allocator, dir, new_id, bytes);
        const owned = try self.allocator.dupe(u8, bytes);
        if (conn.attachment) |prev| self.allocator.free(prev);
        conn.attachment = owned;

        if (prior_id != new_id) deleteAttachmentFile(dir, prior_id);
        return true;
    }

    /// Scan the attachments directory and return every persisted
    /// connection id. Useful for startup recovery: a process that
    /// crashed leaves `<id>.att` files behind, and callers can decide
    /// whether to surface them, adopt them, or clear them. Returns an
    /// empty slice when persistence is disabled or the directory is
    /// empty. Caller owns the returned slice.
    pub fn listPersistedAttachmentIds(
        self: *Pool,
        out_allocator: std.mem.Allocator,
    ) ![]ConnectionId {
        self.lock();
        const dir = self.attachments_dir;
        self.unlock();
        if (dir == null) return &.{};

        return listAttachmentIds(out_allocator, dir.?);
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
// Static disk helpers.
// ---------------------------------------------------------------------------

fn allocAttachmentPath(
    allocator: std.mem.Allocator,
    dir: []const u8,
    id: ConnectionId,
    suffix: []const u8,
) ![]u8 {
    return std.fmt.allocPrint(allocator, "{s}/{d}.att{s}", .{ dir, id, suffix });
}

fn writeAttachmentFileAtomic(
    allocator: std.mem.Allocator,
    dir: []const u8,
    id: ConnectionId,
    bytes: []const u8,
) !void {
    const tmp_path = try allocAttachmentPath(allocator, dir, id, ".tmp");
    defer allocator.free(tmp_path);
    const final_path = try allocAttachmentPath(allocator, dir, id, "");
    defer allocator.free(final_path);

    try zq.file_io.writeFile(allocator, tmp_path, bytes);

    // rename(2) is atomic on POSIX; readers never observe a partial file.
    const tmp_z = try allocator.dupeZ(u8, tmp_path);
    defer allocator.free(tmp_z);
    const final_z = try allocator.dupeZ(u8, final_path);
    defer allocator.free(final_z);
    if (std.c.rename(tmp_z, final_z) != 0) {
        _ = std.c.unlink(tmp_z);
        return error.AttachmentRenameFailed;
    }
}

fn readAttachmentFile(
    allocator: std.mem.Allocator,
    dir: []const u8,
    id: ConnectionId,
) !?[]u8 {
    const path = try allocAttachmentPath(allocator, dir, id, "");
    defer allocator.free(path);
    return zq.file_io.readFile(allocator, path, max_attachment_bytes) catch |err| switch (err) {
        error.FileNotFound => null,
        else => err,
    };
}

fn deleteAttachmentFile(dir: []const u8, id: ConnectionId) void {
    const path = allocAttachmentPath(std.heap.c_allocator, dir, id, "") catch return;
    defer std.heap.c_allocator.free(path);
    const path_z = std.heap.c_allocator.dupeZ(u8, path) catch return;
    defer std.heap.c_allocator.free(path_z);
    _ = std.c.unlink(path_z);
}

fn listAttachmentIds(
    allocator: std.mem.Allocator,
    dir: []const u8,
) ![]ConnectionId {
    const dir_z = try allocator.dupeZ(u8, dir);
    defer allocator.free(dir_z);

    const dh = c.opendir(dir_z) orelse return &.{};
    defer _ = c.closedir(dh);

    var out: std.ArrayList(ConnectionId) = .empty;
    errdefer out.deinit(allocator);

    while (c.readdir(dh)) |entry| {
        const name_ptr: [*:0]const u8 = @ptrCast(&entry.*.d_name);
        const name = std.mem.sliceTo(name_ptr, 0);
        if (!std.mem.endsWith(u8, name, ".att")) continue;
        const stem = name[0 .. name.len - 4];
        const id = std.fmt.parseInt(ConnectionId, stem, 10) catch continue;
        try out.append(allocator, id);
    }
    return try out.toOwnedSlice(allocator);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "outbound lease pins state and fd across unregister and pool teardown" {
    const fds = try @import("server_io.zig").createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[1]);

    var pool = Pool.init(testing.allocator);
    const id = try pool.register(fds[0], "lease", 0);
    var lease = try pool.acquireOutbound(id);
    defer lease.deinit();

    pool.unregister(id);
    pool.deinit();
    std.Io.Threaded.closeFd(fds[0]);

    try lease.writeFrame(.text, "still-pinned");
    var frame: [14]u8 = undefined;
    try readExactFd(fds[1], &frame);
    try testing.expectEqual(@as(u8, 0x81), frame[0]);
    try testing.expectEqual(@as(u8, 12), frame[1]);
    try testing.expectEqualStrings("still-pinned", frame[2..]);
}

test "a blocked peer does not delay an independent connection" {
    const a_fds = try @import("server_io.zig").createUnixSocketPair();
    defer std.Io.Threaded.closeFd(a_fds[0]);
    defer std.Io.Threaded.closeFd(a_fds[1]);
    const b_fds = try @import("server_io.zig").createUnixSocketPair();
    defer std.Io.Threaded.closeFd(b_fds[0]);
    defer std.Io.Threaded.closeFd(b_fds[1]);

    var pool = Pool.init(testing.allocator);
    defer pool.deinit();
    const a = try pool.register(a_fds[0], "a", 0);
    const b = try pool.register(b_fds[0], "b", 0);
    const send_buffer: c_int = 256;
    std.posix.setsockopt(a_fds[0], std.posix.SOL.SOCKET, std.posix.SO.SNDBUF, std.mem.asBytes(&send_buffer)) catch {};

    const blocked_payload: [32768]u8 = @splat('A');
    var blocked = SendTestContext{ .pool = &pool, .id = a, .payload = &blocked_payload };
    const blocked_thread = try std.Thread.spawn(.{}, sendTestFrame, .{&blocked});
    var blocked_joined = false;
    defer if (!blocked_joined) {
        _ = std.c.shutdown(a_fds[0], std.c.SHUT.RDWR);
        blocked_thread.join();
    };
    while (!blocked.started.load(.acquire)) std.atomic.spinLoopHint();

    const peer_payload = "peer-b";
    var peer = SendTestContext{ .pool = &pool, .id = b, .payload = peer_payload };
    const peer_thread = try std.Thread.spawn(.{}, sendTestFrame, .{&peer});
    var waited_ms: usize = 0;
    while (!peer.finished.load(.acquire) and waited_ms < 1000) : (waited_ms += 1) {
        const ts = std.c.timespec{ .sec = 0, .nsec = std.time.ns_per_ms };
        _ = std.c.nanosleep(&ts, null);
    }
    if (!peer.finished.load(.acquire)) {
        _ = std.c.shutdown(a_fds[0], std.c.SHUT.RDWR);
        blocked_thread.join();
        blocked_joined = true;
        peer_thread.join();
        return error.IndependentPeerBlocked;
    }
    peer_thread.join();
    try testing.expect(!peer.failed.load(.acquire));

    var frame: [8]u8 = undefined;
    try readExactFd(b_fds[1], &frame);
    try testing.expectEqualSlices(u8, &.{ 0x81, 0x06 }, frame[0..2]);
    try testing.expectEqualStrings(peer_payload, frame[2..]);

    _ = std.c.shutdown(a_fds[0], std.c.SHUT.RDWR);
    blocked_thread.join();
    blocked_joined = true;
}

test "a close frame shuts the outbound boundary before later sends" {
    const fds = try @import("server_io.zig").createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);
    defer std.Io.Threaded.closeFd(fds[1]);

    var pool = Pool.init(testing.allocator);
    defer pool.deinit();
    const id = try pool.register(fds[0], "closing", 0);
    try pool.sendFrameAndShutdown(id, .connection_close, &.{ 0x03, 0xe8 });
    try testing.expectError(error.WriteFailed, pool.sendFrame(id, .text, "late"));

    var frame: [4]u8 = undefined;
    try readExactFd(fds[1], &frame);
    try testing.expectEqualSlices(u8, &.{ 0x88, 0x02, 0x03, 0xe8 }, &frame);
    var byte: [1]u8 = undefined;
    try testing.expectEqual(@as(isize, 0), std.c.read(fds[1], &byte, 1));
}

test "concurrent large sends to one peer remain complete frames" {
    const fds = try @import("server_io.zig").createUnixSocketPair();
    defer std.Io.Threaded.closeFd(fds[0]);
    defer std.Io.Threaded.closeFd(fds[1]);
    const send_buffer: c_int = 256;
    std.posix.setsockopt(fds[0], std.posix.SOL.SOCKET, std.posix.SO.SNDBUF, std.mem.asBytes(&send_buffer)) catch {};

    var pool = Pool.init(testing.allocator);
    defer pool.deinit();
    const id = try pool.register(fds[0], "concurrent", 0);
    const payload_a: [32768]u8 = @splat('A');
    const payload_b: [32768]u8 = @splat('B');
    var ctx_a = SendTestContext{ .pool = &pool, .id = id, .payload = &payload_a };
    var ctx_b = SendTestContext{ .pool = &pool, .id = id, .payload = &payload_b };
    const thread_a = try std.Thread.spawn(.{}, sendTestFrame, .{&ctx_a});
    const thread_b = try std.Thread.spawn(.{}, sendTestFrame, .{&ctx_b});
    var joined = false;
    defer if (!joined) {
        _ = std.c.shutdown(fds[0], std.c.SHUT.RDWR);
        thread_a.join();
        thread_b.join();
    };

    var bytes: [2 * (4 + 32768)]u8 = undefined;
    try readExactFd(fds[1], &bytes);
    thread_a.join();
    thread_b.join();
    joined = true;
    try testing.expect(!ctx_a.failed.load(.acquire));
    try testing.expect(!ctx_b.failed.load(.acquire));

    var offset: usize = 0;
    const first = try decodeServerTextFrame(&bytes, &offset);
    const second = try decodeServerTextFrame(&bytes, &offset);
    try testing.expectEqual(bytes.len, offset);
    const first_is_a = allByte(first, 'A');
    const first_is_b = allByte(first, 'B');
    try testing.expect(first_is_a or first_is_b);
    try testing.expect(if (first_is_a) allByte(second, 'B') else allByte(second, 'A'));
}

const SendTestContext = struct {
    pool: *Pool,
    id: ConnectionId,
    payload: []const u8,
    started: std.atomic.Value(bool) = .init(false),
    finished: std.atomic.Value(bool) = .init(false),
    failed: std.atomic.Value(bool) = .init(false),
};

fn sendTestFrame(ctx: *SendTestContext) void {
    ctx.started.store(true, .release);
    ctx.pool.sendFrame(ctx.id, .text, ctx.payload) catch ctx.failed.store(true, .release);
    ctx.finished.store(true, .release);
}

/// Blocking exact read, shared with the WebSocket callback tests.
pub fn readExactFd(fd: std.posix.fd_t, out: []u8) !void {
    var offset: usize = 0;
    while (offset < out.len) {
        const n = std.c.read(fd, out[offset..].ptr, out.len - offset);
        if (n < 0) {
            if (std.posix.errno(n) == .INTR) continue;
            return error.ReadFailed;
        }
        if (n == 0) return error.EndOfStream;
        offset += @intCast(n);
    }
}

fn decodeServerTextFrame(bytes: []const u8, offset: *usize) ![]const u8 {
    if (offset.* + 2 > bytes.len or bytes[offset.*] != 0x81) return error.InvalidFrame;
    const len_tag = bytes[offset.* + 1];
    if ((len_tag & 0x80) != 0) return error.InvalidFrame;
    offset.* += 2;
    const len: usize = if (len_tag < 126)
        len_tag
    else if (len_tag == 126) blk: {
        if (offset.* + 2 > bytes.len) return error.InvalidFrame;
        const value = std.mem.readInt(u16, bytes[offset.*..][0..2], .big);
        offset.* += 2;
        break :blk value;
    } else blk: {
        if (offset.* + 8 > bytes.len) return error.InvalidFrame;
        const value = std.math.cast(usize, std.mem.readInt(u64, bytes[offset.*..][0..8], .big)) orelse return error.InvalidFrame;
        offset.* += 8;
        break :blk value;
    };
    if (offset.* + len > bytes.len) return error.InvalidFrame;
    const payload = bytes[offset.*..][0..len];
    offset.* += len;
    return payload;
}

fn allByte(bytes: []const u8, expected: u8) bool {
    for (bytes) |byte| if (byte != expected) return false;
    return true;
}

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

test "snapshotRoom returns all members of a room" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    const a = try pool.register(100, "alpha", 1000);
    const b = try pool.register(101, "alpha", 1001);
    _ = try pool.register(102, "beta", 1002);

    const alpha = try pool.snapshotRoom("alpha", testing.allocator);
    defer testing.allocator.free(alpha);
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

test "snapshotRoom never truncates live members" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    for (0..10) |i| {
        _ = try pool.register(@intCast(100 + i), "alpha", 1000);
    }

    const got = try pool.snapshotRoom("alpha", testing.allocator);
    defer testing.allocator.free(got);
    try testing.expectEqual(@as(usize, 10), got.len);
    try testing.expectEqual(@as(usize, 10), pool.countInRoom("alpha"));
}

test "registration enforces the pool connection limit" {
    var pool = Pool.initWithLimit(testing.allocator, 1);
    defer pool.deinit();
    _ = try pool.register(100, "alpha", 0);
    try testing.expectError(error.CapacityReached, pool.register(101, "alpha", 0));
}

test "attachment round-trip owns bytes independent of caller" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    const id = try pool.register(100, "alpha", 1000);

    // Nothing stored yet.
    try testing.expect((try pool.copyAttachment(id, testing.allocator)) == null);

    // Caller-owned buffer: we mutate it after storing to prove the
    // pool doesn't just retain the caller's slice.
    var src_buf: [16]u8 = .{0} ** 16;
    @memcpy(src_buf[0..5], "hello");
    try testing.expect(try pool.setAttachment(id, src_buf[0..5]));
    @memset(&src_buf, 'X');

    const copied = (try pool.copyAttachment(id, testing.allocator)) orelse return error.TestFailed;
    defer testing.allocator.free(copied);
    try testing.expectEqualStrings("hello", copied);

    // Overwrite replaces the prior attachment cleanly.
    try testing.expect(try pool.setAttachment(id, "world!"));
    const copied2 = (try pool.copyAttachment(id, testing.allocator)) orelse return error.TestFailed;
    defer testing.allocator.free(copied2);
    try testing.expectEqualStrings("world!", copied2);

    // Unregister frees the attachment (verified by testing.allocator
    // not leaking at deinit time).
    pool.unregister(id);
}

test "setAttachment reports false for missing id" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();
    try testing.expect(!(try pool.setAttachment(9999, "x")));
}

// ---------------------------------------------------------------------------
// W2-b: disk persistence
// ---------------------------------------------------------------------------

fn testAttachmentsDir(arena: std.mem.Allocator, sub_path: []const u8) ![]u8 {
    const dir = try std.fmt.allocPrint(arena, ".zig-cache/tmp/{s}/ws", .{sub_path});
    const dir_z = try arena.dupeZ(u8, dir);
    // The parent (.zig-cache/tmp/<sub>) is created by tmpDir; we just
    // need to add the `ws` child.
    switch (std.posix.errno(std.posix.system.mkdir(dir_z, 0o755))) {
        .SUCCESS, .EXIST => {},
        else => return error.MakeDirFailed,
    }
    return dir;
}

test "setAttachment writes a file under attachments_dir" {
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const dir = try testAttachmentsDir(arena, tmp_dir.sub_path[0..]);

    var pool = Pool.init(testing.allocator);
    defer pool.deinit();
    try pool.setAttachmentsDir(dir);

    const id = try pool.register(100, "alpha", 1000);
    try testing.expect(try pool.setAttachment(id, "hello"));

    const on_disk = (try readAttachmentFile(testing.allocator, dir, id)) orelse
        return error.TestFailed;
    defer testing.allocator.free(on_disk);
    try testing.expectEqualStrings("hello", on_disk);

    pool.unregister(id);
    try testing.expect((try readAttachmentFile(testing.allocator, dir, id)) == null);
}

test "listPersistedAttachmentIds surfaces crash-leftover files" {
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const dir = try testAttachmentsDir(arena, tmp_dir.sub_path[0..]);

    // First pool "crashes" without unregistering the connection.
    var crashed_id: ConnectionId = 0;
    {
        var pool = Pool.init(testing.allocator);
        defer pool.deinit();
        try pool.setAttachmentsDir(dir);
        crashed_id = try pool.register(100, "alpha", 1000);
        try testing.expect(try pool.setAttachment(crashed_id, "survived"));
        // No unregister — simulates a crash.
    }

    // Fresh pool over the same dir sees the orphaned file.
    var fresh = Pool.init(testing.allocator);
    defer fresh.deinit();
    try fresh.setAttachmentsDir(dir);

    const ids = try fresh.listPersistedAttachmentIds(testing.allocator);
    defer testing.allocator.free(ids);
    try testing.expectEqual(@as(usize, 1), ids.len);
    try testing.expectEqual(crashed_id, ids[0]);
}

test "adoptPersistedAttachment migrates bytes onto a new connection" {
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const dir = try testAttachmentsDir(arena, tmp_dir.sub_path[0..]);

    // Seed a crashed attachment.
    var prior_id: ConnectionId = 0;
    {
        var pool = Pool.init(testing.allocator);
        defer pool.deinit();
        try pool.setAttachmentsDir(dir);
        prior_id = try pool.register(100, "alpha", 1000);
        try testing.expect(try pool.setAttachment(prior_id, "remember-me"));
    }

    // New pool, new connection; adopt the prior attachment.
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();
    try pool.setAttachmentsDir(dir);

    const new_id = try pool.register(200, "alpha", 2000);
    try testing.expect(try pool.adoptPersistedAttachment(new_id, prior_id));

    const copied = (try pool.copyAttachment(new_id, testing.allocator)) orelse
        return error.TestFailed;
    defer testing.allocator.free(copied);
    try testing.expectEqualStrings("remember-me", copied);

    // The prior file is removed; only the new id's file remains.
    const ids = try pool.listPersistedAttachmentIds(testing.allocator);
    defer testing.allocator.free(ids);
    try testing.expectEqual(@as(usize, 1), ids.len);
    try testing.expectEqual(new_id, ids[0]);

    // Adoption of a missing prior id is a safe `false`.
    try testing.expect(!(try pool.adoptPersistedAttachment(new_id, 9999)));
}

test "setAutoResponse stores and tryAutoResponse matches exactly" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    const id = try pool.register(100, "alpha", 1000);

    // No auto-response yet.
    try testing.expect((try pool.tryAutoResponse(id, "ping", testing.allocator)) == null);

    try testing.expect(try pool.setAutoResponse(id, "ping", "pong"));

    // Exact match returns a caller-owned copy.
    const resp = (try pool.tryAutoResponse(id, "ping", testing.allocator)) orelse
        return error.TestFailed;
    defer testing.allocator.free(resp);
    try testing.expectEqualStrings("pong", resp);

    // Mismatched input: nothing returned.
    try testing.expect((try pool.tryAutoResponse(id, "other", testing.allocator)) == null);

    // Replacing with a fresh pair works and frees the old bytes.
    try testing.expect(try pool.setAutoResponse(id, "heartbeat", "alive"));
    try testing.expect((try pool.tryAutoResponse(id, "ping", testing.allocator)) == null);
    const resp2 = (try pool.tryAutoResponse(id, "heartbeat", testing.allocator)) orelse
        return error.TestFailed;
    defer testing.allocator.free(resp2);
    try testing.expectEqualStrings("alive", resp2);

    // Clearing with empty pair removes the auto-response.
    try testing.expect(try pool.setAutoResponse(id, "", ""));
    try testing.expect((try pool.tryAutoResponse(id, "heartbeat", testing.allocator)) == null);
}

test "setAutoResponse returns false for missing id" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();
    try testing.expect(!(try pool.setAutoResponse(9999, "a", "b")));
}

// ---------------------------------------------------------------------------
// W3: connection state machine + idle scanning
// ---------------------------------------------------------------------------

test "begin/endDispatch toggle state between live and parked" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    const id = try pool.register(100, "alpha", 1000);
    try testing.expectEqual(ConnectionState.parked, (pool.snapshot(id) orelse return error.TestFailed).state);

    try testing.expect(pool.beginDispatch(id));
    try testing.expectEqual(ConnectionState.live, (pool.snapshot(id) orelse return error.TestFailed).state);

    try testing.expect(pool.endDispatch(id));
    try testing.expectEqual(ConnectionState.parked, (pool.snapshot(id) orelse return error.TestFailed).state);

    try testing.expect(!pool.beginDispatch(9999));
    try testing.expect(!pool.endDispatch(9999));
}

test "scanIdle transitions stale parked connections to dormant" {
    var pool = Pool.init(testing.allocator);
    defer pool.deinit();

    // Three connections registered at the same moment.
    const a = try pool.register(100, "alpha", 1_000);
    const b = try pool.register(101, "alpha", 1_000);
    const c_live = try pool.register(102, "alpha", 1_000);

    // `c_live` is actively dispatching and must never be swept.
    try testing.expect(pool.beginDispatch(c_live));

    // Only `b` has received a recent frame.
    _ = pool.touch(b, .parked, 9_000);

    const idle = try pool.scanIdle(testing.allocator, 1_000, 10_000);
    defer testing.allocator.free(idle);

    // `a`: last frame at 1000, now 10000, threshold 1000 → idle 9000ms > 1000 → dormant.
    // `b`: last frame at 9000, idle 1000ms = threshold → NOT past threshold → stays parked.
    // `c_live`: in .live, never scanned regardless of timestamp.
    try testing.expectEqual(@as(usize, 1), idle.len);
    try testing.expectEqual(a, idle[0]);

    try testing.expectEqual(ConnectionState.dormant, (pool.snapshot(a) orelse return error.TestFailed).state);
    try testing.expectEqual(ConnectionState.parked, (pool.snapshot(b) orelse return error.TestFailed).state);
    try testing.expectEqual(ConnectionState.live, (pool.snapshot(c_live) orelse return error.TestFailed).state);

    // A second scan finds no new candidates — the already-dormant connection
    // isn't re-reported.
    const idle2 = try pool.scanIdle(testing.allocator, 1_000, 10_000);
    defer testing.allocator.free(idle2);
    try testing.expectEqual(@as(usize, 0), idle2.len);
}
