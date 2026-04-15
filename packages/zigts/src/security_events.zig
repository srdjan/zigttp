//! Process-wide ring-buffered security event stream. Sandbox-boundary
//! events (capability/policy denials, arena leaks, OOM) are emitted from
//! engine code and drained by the runtime to JSONL.

const std = @import("std");
const compat = @import("compat.zig");
const json_utils = @import("json_utils.zig");

pub const SecurityEventKind = enum {
    capability_denied,
    policy_denied_env,
    policy_denied_cache,
    policy_denied_sql,
    policy_denied_egress,
    arena_audit_failure,
    persistent_string_escape,
    request_timeout,
    oom,
};

pub const max_module_len: usize = 32;
pub const max_detail_len: usize = 128;

pub const SecurityEvent = struct {
    kind: SecurityEventKind,
    timestamp_ns: u64 = 0,
    module_buf: [max_module_len]u8 = undefined,
    module_len: u8 = 0,
    detail_buf: [max_detail_len]u8 = undefined,
    detail_len: u8 = 0,

    pub fn init(
        kind: SecurityEventKind,
        module_name: []const u8,
        detail_text: []const u8,
    ) SecurityEvent {
        var event: SecurityEvent = .{ .kind = kind };
        const mn = @min(module_name.len, max_module_len);
        @memcpy(event.module_buf[0..mn], module_name[0..mn]);
        event.module_len = @intCast(mn);
        const dn = @min(detail_text.len, max_detail_len);
        @memcpy(event.detail_buf[0..dn], detail_text[0..dn]);
        event.detail_len = @intCast(dn);
        return event;
    }

    pub fn moduleSlice(self: *const SecurityEvent) []const u8 {
        return self.module_buf[0..self.module_len];
    }

    pub fn detailSlice(self: *const SecurityEvent) []const u8 {
        return self.detail_buf[0..self.detail_len];
    }
};

pub const Stats = struct {
    emitted: u64,
    dropped: u64,
    buffered: usize,
};

/// Mutex-guarded ring buffer. Drops oldest on overflow.
pub const Stream = struct {
    allocator: std.mem.Allocator,
    buffer: []SecurityEvent,
    head: usize = 0,
    count: usize = 0,
    emitted: u64 = 0,
    dropped: u64 = 0,
    mutex: compat.Mutex = .{},

    pub fn init(allocator: std.mem.Allocator, capacity: usize) !*Stream {
        std.debug.assert(capacity > 0);
        const self = try allocator.create(Stream);
        errdefer allocator.destroy(self);
        const buffer = try allocator.alloc(SecurityEvent, capacity);
        self.* = .{
            .allocator = allocator,
            .buffer = buffer,
        };
        return self;
    }

    pub fn deinit(self: *Stream) void {
        self.allocator.free(self.buffer);
        self.allocator.destroy(self);
    }

    pub fn emit(self: *Stream, event: SecurityEvent) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        const slot = (self.head + self.count) % self.buffer.len;
        self.buffer[slot] = event;
        if (event.timestamp_ns == 0) {
            self.buffer[slot].timestamp_ns = compat.realtimeNowNs() catch 0;
        }

        if (self.count < self.buffer.len) {
            self.count += 1;
        } else {
            self.head = (self.head + 1) % self.buffer.len;
            self.dropped += 1;
        }
        self.emitted += 1;
    }

    /// Copy up to `out.len` events from the ring into `out`, oldest first,
    /// and remove them from the ring. Returns the number of events copied.
    pub fn drain(self: *Stream, out: []SecurityEvent) usize {
        self.mutex.lock();
        defer self.mutex.unlock();

        const n = @min(out.len, self.count);
        for (0..n) |i| {
            const slot = (self.head + i) % self.buffer.len;
            out[i] = self.buffer[slot];
        }
        self.head = (self.head + n) % self.buffer.len;
        self.count -= n;
        return n;
    }

    pub fn stats(self: *Stream) Stats {
        self.mutex.lock();
        defer self.mutex.unlock();
        return .{
            .emitted = self.emitted,
            .dropped = self.dropped,
            .buffered = self.count,
        };
    }
};

// -------------------------------------------------------------------------
// Global singleton
// -------------------------------------------------------------------------

// Atomic pointer so hot-path emits stay lock-free. Init/deinit still hold a
// small mutex to coordinate creation and prevent double-free.
var global_stream: std.atomic.Value(?*Stream) = .init(null);
var init_mutex: compat.Mutex = .{};

pub fn initGlobal(allocator: std.mem.Allocator, capacity: usize) !void {
    init_mutex.lock();
    defer init_mutex.unlock();
    if (global_stream.load(.acquire) != null) return;
    const s = try Stream.init(allocator, capacity);
    global_stream.store(s, .release);
}

pub fn deinitGlobal() void {
    init_mutex.lock();
    defer init_mutex.unlock();
    if (global_stream.load(.acquire)) |s| {
        global_stream.store(null, .release);
        s.deinit();
    }
}

pub fn getGlobal() ?*Stream {
    return global_stream.load(.acquire);
}

pub fn emitGlobal(event: SecurityEvent) void {
    const stream = global_stream.load(.acquire) orelse return;
    stream.emit(event);
}

// -------------------------------------------------------------------------
// JSONL serialization
// -------------------------------------------------------------------------

/// Write an event as a single JSONL line (including trailing newline).
pub fn writeJsonLine(event: *const SecurityEvent, writer: anytype) !void {
    try writer.print(
        "{{\"kind\":\"{s}\",\"ts\":{d},\"module\":\"",
        .{ @tagName(event.kind), event.timestamp_ns },
    );
    try json_utils.writeJsonStringContent(writer, event.moduleSlice());
    try writer.writeAll("\",\"detail\":\"");
    try json_utils.writeJsonStringContent(writer, event.detailSlice());
    try writer.writeAll("\"}\n");
}

// =========================================================================
// Tests
// =========================================================================

test "Stream emit and drain roundtrip" {
    const allocator = std.testing.allocator;
    var stream = try Stream.init(allocator, 4);
    defer stream.deinit();

    stream.emit(SecurityEvent.init(.policy_denied_env, "zigttp:env", "SECRET_KEY"));
    stream.emit(SecurityEvent.init(.arena_audit_failure, "handler", "leak"));

    var buf: [8]SecurityEvent = undefined;
    const n = stream.drain(&buf);
    try std.testing.expectEqual(@as(usize, 2), n);
    try std.testing.expectEqual(SecurityEventKind.policy_denied_env, buf[0].kind);
    try std.testing.expectEqualStrings("zigttp:env", buf[0].moduleSlice());
    try std.testing.expectEqualStrings("SECRET_KEY", buf[0].detailSlice());
    try std.testing.expectEqual(SecurityEventKind.arena_audit_failure, buf[1].kind);

    // Drained ring is empty
    try std.testing.expectEqual(@as(usize, 0), stream.drain(&buf));
    try std.testing.expectEqual(@as(u64, 0), stream.stats().dropped);
}

test "Stream drops oldest on overflow and tracks drop count" {
    const allocator = std.testing.allocator;
    var stream = try Stream.init(allocator, 3);
    defer stream.deinit();

    // Emit 5 events tagged via the detail field so we can identify survivors
    const details = [_][]const u8{ "e0", "e1", "e2", "e3", "e4" };
    for (details) |d| {
        stream.emit(SecurityEvent.init(.capability_denied, "m", d));
    }

    try std.testing.expectEqual(@as(u64, 2), stream.stats().dropped);
    try std.testing.expectEqual(@as(u64, 5), stream.stats().emitted);

    var buf: [8]SecurityEvent = undefined;
    const n = stream.drain(&buf);
    try std.testing.expectEqual(@as(usize, 3), n);
    try std.testing.expectEqualStrings("e2", buf[0].detailSlice());
    try std.testing.expectEqualStrings("e3", buf[1].detailSlice());
    try std.testing.expectEqualStrings("e4", buf[2].detailSlice());
}

test "SecurityEvent.init truncates over-long strings" {
    const long_module = "zigttp:this-is-a-very-long-specifier-name-that-exceeds-the-inline-limit";
    const long_detail = "a" ** 256;
    const event = SecurityEvent.init(.policy_denied_sql, long_module, long_detail);
    try std.testing.expectEqual(max_module_len, event.moduleSlice().len);
    try std.testing.expectEqual(max_detail_len, event.detailSlice().len);
}

test "writeJsonLine emits valid JSONL" {
    const allocator = std.testing.allocator;
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    var event = SecurityEvent.init(.policy_denied_env, "zigttp:env", "KEY\"with\\quotes");
    event.timestamp_ns = 12345;
    try writeJsonLine(&event, &aw.writer);
    output = aw.toArrayList();

    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"kind\":\"policy_denied_env\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"ts\":12345") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"module\":\"zigttp:env\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "KEY\\\"with\\\\quotes") != null);
    try std.testing.expectEqual(@as(u8, '\n'), output.items[output.items.len - 1]);
}

test "global stream init/deinit/emit" {
    try initGlobal(std.testing.allocator, 16);
    defer deinitGlobal();

    emitGlobal(SecurityEvent.init(.oom, "handler", "heap exhausted"));

    const stream = getGlobal() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 1), stream.stats().emitted);

    var buf: [4]SecurityEvent = undefined;
    const n = stream.drain(&buf);
    try std.testing.expectEqual(@as(usize, 1), n);
    try std.testing.expectEqual(SecurityEventKind.oom, buf[0].kind);
}
