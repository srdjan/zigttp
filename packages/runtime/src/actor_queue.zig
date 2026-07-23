//! In-process actor mailboxes for opt-in handler-to-handler messaging.
//!
//! The queue owns every payload byte it accepts. JavaScript runtimes only see
//! reconstructed values, so a runtime reset, panic quarantine, or GC cycle does
//! not invalidate retained messages.

const std = @import("std");
const compat = @import("zts").compat;

/// Hard cap on the number of distinct actor mailboxes a single ActorQueue
/// will create. Mailboxes are never evicted (a live pointer may be cached by
/// an in-flight send()), so without a cap a handler that derives target
/// names from request-controlled data (session/user ids) grows this set
/// without bound for the life of the process.
const MAX_MAILBOXES: usize = 4096;

pub const MessageId = u64;

pub const MessagePriority = enum(u2) {
    high = 0,
    normal = 1,
    low = 2,
};

pub const LeaseInfo = struct {
    attempt: u32,
    leased_until_ms: i64,
};

pub const DeadInfo = struct {
    attempt: u32,
    reason: []const u8,
};

pub const DeliveryState = union(enum) {
    pending,
    leased: LeaseInfo,
    done,
    dead: DeadInfo,
};

pub const MessageEnvelope = struct {
    id: MessageId,
    source: []u8,
    target: []u8,
    payload_json: []u8,
    correlation_id: ?MessageId = null,
    reply_to: ?[]u8 = null,
    priority: MessagePriority = .normal,
    attempt: u32 = 0,
    max_attempts: u32 = 3,
    enqueued_ms: i64 = 0,
    state: DeliveryState = .pending,

    pub fn deinit(self: *MessageEnvelope, allocator: std.mem.Allocator) void {
        allocator.free(self.source);
        allocator.free(self.target);
        allocator.free(self.payload_json);
        if (self.reply_to) |reply_to| allocator.free(reply_to);
        switch (self.state) {
            .dead => |dead| allocator.free(dead.reason),
            else => {},
        }
        allocator.destroy(self);
    }
};

pub const SendOptions = struct {
    source: []const u8 = "system",
    reply_to: ?[]const u8 = null,
    correlation_id: ?MessageId = null,
    priority: MessagePriority = .normal,
    max_attempts: u32 = 3,
};

const Ring = struct {
    items: []*MessageEnvelope,
    head: usize = 0,
    tail: usize = 0,
    len: usize = 0,

    fn init(allocator: std.mem.Allocator, capacity: usize) !Ring {
        return .{ .items = try allocator.alloc(*MessageEnvelope, capacity) };
    }

    fn deinit(self: *Ring, allocator: std.mem.Allocator) void {
        allocator.free(self.items);
        self.* = .{ .items = &.{} };
    }

    fn push(self: *Ring, item: *MessageEnvelope) bool {
        if (self.len >= self.items.len) return false;
        self.items[self.tail] = item;
        self.tail = (self.tail + 1) % self.items.len;
        self.len += 1;
        return true;
    }

    fn pop(self: *Ring) ?*MessageEnvelope {
        if (self.len == 0) return null;
        const item = self.items[self.head];
        self.head = (self.head + 1) % self.items.len;
        self.len -= 1;
        return item;
    }
};

pub const Mailbox = struct {
    capacity: usize,
    rings: [3]Ring,
    /// Retained messages for this actor: pending in a ring or leased
    /// in-flight. Released on ack, or when a message is dead-lettered.
    count: std.atomic.Value(usize),
    mutex: compat.Mutex = .{},

    pub fn init(allocator: std.mem.Allocator, capacity: usize) !Mailbox {
        if (capacity == 0) return error.InvalidQueueCapacity;

        var rings: [3]Ring = undefined;
        var initialized: usize = 0;
        errdefer {
            for (rings[0..initialized]) |*ring| ring.deinit(allocator);
        }
        for (&rings) |*ring| {
            ring.* = try Ring.init(allocator, capacity);
            initialized += 1;
        }

        return .{
            .capacity = capacity,
            .rings = rings,
            .count = std.atomic.Value(usize).init(0),
        };
    }

    pub fn deinit(self: *Mailbox, allocator: std.mem.Allocator) void {
        self.mutex.lock();
        while (self.popLocked()) |msg| {
            msg.deinit(allocator);
        }
        self.mutex.unlock();

        for (&self.rings) |*ring| ring.deinit(allocator);
    }

    pub fn pushNew(self: *Mailbox, item: *MessageEnvelope) bool {
        if (self.count.load(.acquire) >= self.capacity) return false;

        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.count.load(.acquire) >= self.capacity) return false;
        const idx = priorityIndex(item.priority);
        if (!self.rings[idx].push(item)) return false;
        _ = self.count.fetchAdd(1, .release);
        return true;
    }

    pub fn pushRetained(self: *Mailbox, item: *MessageEnvelope) bool {
        self.mutex.lock();
        defer self.mutex.unlock();

        const idx = priorityIndex(item.priority);
        return self.rings[idx].push(item);
    }

    pub fn tryPop(self: *Mailbox) ?*MessageEnvelope {
        self.mutex.lock();
        defer self.mutex.unlock();

        return self.popLocked();
    }

    pub fn releaseRetained(self: *Mailbox) void {
        const old = self.count.fetchSub(1, .release);
        if (std.debug.runtime_safety and old == 0) {
            std.debug.panic("actor queue mailbox retained count underflow", .{});
        }
    }

    pub fn retainedCount(self: *const Mailbox) usize {
        return self.count.load(.acquire);
    }

    fn popLocked(self: *Mailbox) ?*MessageEnvelope {
        inline for (0..3) |idx| {
            if (self.rings[idx].pop()) |item| {
                return item;
            }
        }
        return null;
    }
};

pub const ActorQueue = struct {
    allocator: std.mem.Allocator,
    mailbox_capacity: usize,
    lease_ms: i64,
    mailboxes: std.StringHashMapUnmanaged(*Mailbox) = .empty,
    inflight: std.AutoHashMapUnmanaged(MessageId, *MessageEnvelope) = .empty,
    dead_letters: std.ArrayListUnmanaged(*MessageEnvelope) = .empty,
    next_id: std.atomic.Value(MessageId) = std.atomic.Value(MessageId).init(1),
    mutex: compat.Mutex = .{},

    pub fn init(allocator: std.mem.Allocator, mailbox_capacity: usize, lease_ms: i64) ActorQueue {
        return .{
            .allocator = allocator,
            .mailbox_capacity = @max(mailbox_capacity, 1),
            .lease_ms = @max(lease_ms, 0),
        };
    }

    pub fn deinit(self: *ActorQueue) void {
        self.mutex.lock();

        var inflight_it = self.inflight.valueIterator();
        while (inflight_it.next()) |msg_ptr| {
            msg_ptr.*.deinit(self.allocator);
        }
        self.inflight.deinit(self.allocator);

        for (self.dead_letters.items) |msg| {
            msg.deinit(self.allocator);
        }
        self.dead_letters.deinit(self.allocator);

        var mailbox_it = self.mailboxes.iterator();
        while (mailbox_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.mailboxes.deinit(self.allocator);

        self.mutex.unlock();
    }

    pub fn send(
        self: *ActorQueue,
        target: []const u8,
        payload_json: []const u8,
        options: SendOptions,
    ) !MessageId {
        if (target.len == 0) return error.InvalidActor;

        const message = try self.createMessage(target, payload_json, options);
        errdefer message.deinit(self.allocator);

        const mailbox = try self.ensureMailbox(target);
        if (!mailbox.pushNew(message)) return error.QueueFull;
        return message.id;
    }

    pub fn receive(self: *ActorQueue, actor: []const u8) !?*MessageEnvelope {
        if (actor.len == 0) return error.InvalidActor;

        _ = try self.reclaimExpired(unixMillis());

        const mailbox = self.lookupMailbox(actor) orelse return null;
        const message = mailbox.tryPop() orelse return null;
        message.attempt += 1;
        message.state = .{
            .leased = .{
                .attempt = message.attempt,
                .leased_until_ms = unixMillis() + self.lease_ms,
            },
        };

        self.mutex.lock();
        self.inflight.put(self.allocator, message.id, message) catch |err| {
            self.mutex.unlock();
            message.state = .pending;
            message.attempt -= 1;
            if (!mailbox.pushRetained(message)) {
                mailbox.releaseRetained();
                message.deinit(self.allocator);
            }
            return err;
        };
        self.mutex.unlock();
        return message;
    }

    pub fn reclaimExpired(self: *ActorQueue, now_ms: i64) !usize {
        var expired_ids: std.ArrayListUnmanaged(MessageId) = .empty;
        defer expired_ids.deinit(self.allocator);

        {
            self.mutex.lock();
            defer self.mutex.unlock();

            var it = self.inflight.iterator();
            while (it.next()) |entry| {
                const msg = entry.value_ptr.*;
                switch (msg.state) {
                    .leased => |lease| {
                        if (lease.leased_until_ms <= now_ms) {
                            try expired_ids.append(self.allocator, entry.key_ptr.*);
                        }
                    },
                    else => {},
                }
            }
        }

        var reclaimed: usize = 0;
        for (expired_ids.items) |id| {
            self.mutex.lock();
            const removed = self.inflight.fetchRemove(id);
            self.mutex.unlock();
            const message = if (removed) |entry| entry.value else continue;

            message.state = .pending;
            const mailbox = try self.ensureMailbox(message.target);
            if (!mailbox.pushRetained(message)) {
                // Preserve the message rather than dropping it when a lease
                // expires into a saturated mailbox. It can be reclaimed again
                // on the next receive attempt.
                message.state = .{
                    .leased = .{
                        .attempt = message.attempt,
                        .leased_until_ms = now_ms,
                    },
                };
                self.mutex.lock();
                self.inflight.put(self.allocator, message.id, message) catch {
                    self.mutex.unlock();
                    mailbox.releaseRetained();
                    message.deinit(self.allocator);
                    return error.OutOfMemory;
                };
                self.mutex.unlock();
                continue;
            }
            reclaimed += 1;
        }
        return reclaimed;
    }

    pub fn ack(self: *ActorQueue, id: MessageId) bool {
        self.mutex.lock();
        const removed = self.inflight.fetchRemove(id);
        self.mutex.unlock();

        if (removed) |entry| {
            entry.value.state = .done;
            if (self.lookupMailbox(entry.value.target)) |mailbox| {
                mailbox.releaseRetained();
            }
            entry.value.deinit(self.allocator);
            return true;
        }
        return false;
    }

    pub fn nack(self: *ActorQueue, id: MessageId, reason: []const u8) !NackOutcome {
        self.mutex.lock();
        const message = self.inflight.get(id) orelse {
            self.mutex.unlock();
            return .not_found;
        };

        if (message.attempt >= message.max_attempts) {
            const reason_owned = self.allocator.dupe(u8, reason) catch |err| {
                self.mutex.unlock();
                return err;
            };
            errdefer self.allocator.free(reason_owned);

            self.dead_letters.append(self.allocator, message) catch |err| {
                self.mutex.unlock();
                return err;
            };
            _ = self.inflight.remove(id);
            self.mutex.unlock();

            message.state = .{
                .dead = .{
                    .attempt = message.attempt,
                    .reason = reason_owned,
                },
            };
            // Dead letters move out of the mailbox's pending/in-flight
            // accounting into `dead_letters`, which has no capacity cap of
            // its own; release the retained slot so a run of dead-lettered
            // messages doesn't permanently exhaust mailbox_capacity for
            // this actor the way ack() already avoids for completed ones.
            if (self.lookupMailbox(message.target)) |mailbox| {
                mailbox.releaseRetained();
            }
            return .dead_lettered;
        }

        const mailbox = self.mailboxes.get(message.target) orelse {
            self.mutex.unlock();
            return error.InvalidActor;
        };
        const old_state = message.state;
        _ = self.inflight.remove(id);
        self.mutex.unlock();

        message.state = .pending;
        if (!mailbox.pushRetained(message)) {
            message.state = old_state;
            self.mutex.lock();
            self.inflight.put(self.allocator, message.id, message) catch |err| {
                self.mutex.unlock();
                mailbox.releaseRetained();
                message.deinit(self.allocator);
                return err;
            };
            self.mutex.unlock();
            return error.QueueFull;
        }
        return .requeued;
    }

    pub fn reply(
        self: *ActorQueue,
        request_id: MessageId,
        payload_json: []const u8,
        max_attempts: u32,
    ) !MessageId {
        self.mutex.lock();
        const original = self.inflight.get(request_id) orelse {
            self.mutex.unlock();
            return error.MessageNotInFlight;
        };
        const target_owned = self.allocator.dupe(u8, original.reply_to orelse original.source) catch |err| {
            self.mutex.unlock();
            return err;
        };
        errdefer self.allocator.free(target_owned);
        const source_owned = self.allocator.dupe(u8, original.target) catch |err| {
            self.mutex.unlock();
            return err;
        };
        errdefer self.allocator.free(source_owned);
        self.mutex.unlock();

        defer self.allocator.free(target_owned);
        defer self.allocator.free(source_owned);
        return self.send(target_owned, payload_json, .{
            .source = source_owned,
            .correlation_id = request_id,
            .priority = .high,
            .max_attempts = max_attempts,
        });
    }

    pub fn deadLetterCount(self: *ActorQueue) usize {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.dead_letters.items.len;
    }

    fn createMessage(
        self: *ActorQueue,
        target: []const u8,
        payload_json: []const u8,
        options: SendOptions,
    ) !*MessageEnvelope {
        const message = try self.allocator.create(MessageEnvelope);
        errdefer self.allocator.destroy(message);

        const source_owned = try self.allocator.dupe(u8, options.source);
        errdefer self.allocator.free(source_owned);
        const target_owned = try self.allocator.dupe(u8, target);
        errdefer self.allocator.free(target_owned);
        const payload_owned = try self.allocator.dupe(u8, payload_json);
        errdefer self.allocator.free(payload_owned);
        const reply_to_owned = if (options.reply_to) |reply_to|
            try self.allocator.dupe(u8, reply_to)
        else
            null;
        errdefer if (reply_to_owned) |reply_to| self.allocator.free(reply_to);

        message.* = .{
            .id = self.next_id.fetchAdd(1, .acq_rel),
            .source = source_owned,
            .target = target_owned,
            .payload_json = payload_owned,
            .correlation_id = options.correlation_id,
            .reply_to = reply_to_owned,
            .priority = options.priority,
            .max_attempts = @max(options.max_attempts, 1),
            .enqueued_ms = unixMillis(),
        };
        return message;
    }

    fn ensureMailbox(self: *ActorQueue, actor: []const u8) !*Mailbox {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.mailboxes.get(actor)) |mailbox| return mailbox;

        if (self.mailboxes.count() >= MAX_MAILBOXES) return error.TooManyMailboxes;

        const key = try self.allocator.dupe(u8, actor);
        errdefer self.allocator.free(key);
        const mailbox = try self.allocator.create(Mailbox);
        errdefer self.allocator.destroy(mailbox);
        mailbox.* = try Mailbox.init(self.allocator, self.mailbox_capacity);
        errdefer mailbox.deinit(self.allocator);
        try self.mailboxes.put(self.allocator, key, mailbox);
        return mailbox;
    }

    fn lookupMailbox(self: *ActorQueue, actor: []const u8) ?*Mailbox {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.mailboxes.get(actor);
    }
};

pub const NackOutcome = enum {
    not_found,
    requeued,
    dead_lettered,
};

fn priorityIndex(priority: MessagePriority) usize {
    return @intFromEnum(priority);
}

fn unixMillis() i64 {
    return @import("zts").trace.unixMillis();
}

test "actor queue delivers higher priority first and retains messages until ack" {
    const allocator = std.testing.allocator;
    var queue = ActorQueue.init(allocator, 8, 30_000);
    defer queue.deinit();

    const low_id = try queue.send("worker", "{\"p\":\"low\"}", .{ .source = "main", .priority = .low });
    const high_id = try queue.send("worker", "{\"p\":\"high\"}", .{ .source = "main", .priority = .high });

    const first = (try queue.receive("worker")).?;
    try std.testing.expectEqual(high_id, first.id);
    try std.testing.expectEqual(@as(u32, 1), first.attempt);
    try std.testing.expect(queue.ack(first.id));

    const second = (try queue.receive("worker")).?;
    try std.testing.expectEqual(low_id, second.id);
    try std.testing.expect(queue.ack(second.id));
    try std.testing.expectEqual(@as(?*MessageEnvelope, null), try queue.receive("worker"));
}

test "actor queue nacks requeue until max attempts then dead-letters" {
    const allocator = std.testing.allocator;
    var queue = ActorQueue.init(allocator, 8, 30_000);
    defer queue.deinit();

    const id = try queue.send("worker", "{\"n\":1}", .{ .source = "main", .max_attempts = 2 });

    const first = (try queue.receive("worker")).?;
    try std.testing.expectEqual(id, first.id);
    try std.testing.expectEqual(NackOutcome.requeued, try queue.nack(id, "retry"));

    const second = (try queue.receive("worker")).?;
    try std.testing.expectEqual(id, second.id);
    try std.testing.expectEqual(@as(u32, 2), second.attempt);
    try std.testing.expectEqual(NackOutcome.dead_lettered, try queue.nack(id, "boom"));
    try std.testing.expectEqual(@as(usize, 1), queue.deadLetterCount());
    try std.testing.expectEqual(@as(?*MessageEnvelope, null), try queue.receive("worker"));
}

test "actor queue replies to reply_to actor with correlation id" {
    const allocator = std.testing.allocator;
    var queue = ActorQueue.init(allocator, 8, 30_000);
    defer queue.deinit();

    const request_id = try queue.send("worker", "{\"job\":1}", .{
        .source = "main",
        .reply_to = "main",
    });
    _ = (try queue.receive("worker")).?;

    const reply_id = try queue.reply(request_id, "{\"ok\":true}", 3);
    const reply_msg = (try queue.receive("main")).?;
    try std.testing.expectEqual(reply_id, reply_msg.id);
    try std.testing.expectEqual(request_id, reply_msg.correlation_id.?);
    try std.testing.expectEqualStrings("worker", reply_msg.source);
    try std.testing.expect(queue.ack(reply_msg.id));
    try std.testing.expect(queue.ack(request_id));
}

test "actor queue reclaims expired leases for redelivery" {
    const allocator = std.testing.allocator;
    var queue = ActorQueue.init(allocator, 8, 0);
    defer queue.deinit();

    const id = try queue.send("worker", "{\"job\":1}", .{ .source = "main", .max_attempts = 3 });
    const first = (try queue.receive("worker")).?;
    try std.testing.expectEqual(id, first.id);
    try std.testing.expectEqual(@as(u32, 1), first.attempt);

    const second = (try queue.receive("worker")).?;
    try std.testing.expectEqual(id, second.id);
    try std.testing.expectEqual(@as(u32, 2), second.attempt);
    try std.testing.expect(queue.ack(second.id));
}

test "actor queue counts leased messages against mailbox capacity" {
    const allocator = std.testing.allocator;
    var queue = ActorQueue.init(allocator, 1, 30_000);
    defer queue.deinit();

    const id = try queue.send("worker", "{\"job\":1}", .{ .source = "main" });
    const mailbox = queue.lookupMailbox("worker").?;
    try std.testing.expectEqual(@as(usize, 1), mailbox.retainedCount());

    const leased = (try queue.receive("worker")).?;
    try std.testing.expectEqual(id, leased.id);
    try std.testing.expectEqual(@as(usize, 1), mailbox.retainedCount());
    try std.testing.expectError(error.QueueFull, queue.send("worker", "{\"job\":2}", .{ .source = "main" }));

    try std.testing.expect(queue.ack(id));
    try std.testing.expectEqual(@as(usize, 0), mailbox.retainedCount());
    const next_id = try queue.send("worker", "{\"job\":2}", .{ .source = "main" });
    const next = (try queue.receive("worker")).?;
    try std.testing.expectEqual(next_id, next.id);
    try std.testing.expect(queue.ack(next_id));
}

test "actor queue releases mailbox capacity on dead letter" {
    const allocator = std.testing.allocator;
    var queue = ActorQueue.init(allocator, 1, 30_000);
    defer queue.deinit();

    const id = try queue.send("worker", "{\"job\":1}", .{ .source = "main", .max_attempts = 1 });
    _ = (try queue.receive("worker")).?;
    try std.testing.expectEqual(NackOutcome.dead_lettered, try queue.nack(id, "boom"));
    try std.testing.expectEqual(@as(usize, 1), queue.deadLetterCount());
    // Dead-lettering releases the mailbox slot (dead_letters tracks the
    // message separately) so a run of permanently-failing messages cannot
    // exhaust mailbox_capacity for this actor forever.
    try std.testing.expectEqual(@as(usize, 0), queue.lookupMailbox("worker").?.retainedCount());
    _ = try queue.send("worker", "{\"job\":2}", .{ .source = "main" });
}
