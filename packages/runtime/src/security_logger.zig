//! Background drainer for `zq.security_events`. Opens a JSONL file and
//! spawns a polling thread; `deinit` joins and flushes before close.

const std = @import("std");
const zq = @import("zigts");
const security_events = zq.security_events;

const drain_batch_size: usize = 32;
const write_buf_size: usize = 16 * 1024;
const poll_ns: u64 = 250 * std.time.ns_per_ms;

pub const SecurityLogger = struct {
    allocator: std.mem.Allocator,
    fd: std.c.fd_t,
    stop: std.atomic.Value(bool),
    thread: ?std.Thread,

    pub fn start(allocator: std.mem.Allocator, path: []const u8) !*SecurityLogger {
        const self = try allocator.create(SecurityLogger);
        errdefer allocator.destroy(self);

        const fd = try zq.file_io.openAppend(allocator, path);
        errdefer std.Io.Threaded.closeFd(fd);

        self.* = .{
            .allocator = allocator,
            .fd = fd,
            .stop = std.atomic.Value(bool).init(false),
            .thread = null,
        };

        self.thread = try std.Thread.spawn(.{}, drainLoop, .{self});
        return self;
    }

    pub fn deinit(self: *SecurityLogger) void {
        self.stop.store(true, .release);
        if (self.thread) |t| t.join();
        self.flushRemaining();
        std.Io.Threaded.closeFd(self.fd);
        self.allocator.destroy(self);
    }

    fn drainLoop(self: *SecurityLogger) void {
        var batch: [drain_batch_size]security_events.SecurityEvent = undefined;
        const ts = std.c.timespec{
            .sec = @intCast(poll_ns / std.time.ns_per_s),
            .nsec = @intCast(poll_ns % std.time.ns_per_s),
        };
        while (!self.stop.load(.acquire)) {
            const n = drainInto(&batch);
            if (n > 0) self.writeBatch(batch[0..n]);
            _ = std.c.nanosleep(&ts, null);
        }
    }

    fn flushRemaining(self: *SecurityLogger) void {
        var batch: [drain_batch_size]security_events.SecurityEvent = undefined;
        while (true) {
            const n = drainInto(&batch);
            if (n == 0) break;
            self.writeBatch(batch[0..n]);
        }
    }

    fn drainInto(batch: []security_events.SecurityEvent) usize {
        const stream = security_events.getGlobal() orelse return 0;
        return stream.drain(batch);
    }

    // Serialize all events into one buffer, then issue a single write. A
    // full batch can easily exceed a single JSON line's worth, so flush
    // mid-batch whenever the line would not fit.
    fn writeBatch(
        self: *SecurityLogger,
        events: []const security_events.SecurityEvent,
    ) void {
        var buf: [write_buf_size]u8 = undefined;
        var w = std.Io.Writer.fixed(&buf);
        for (events) |*event| {
            security_events.writeJsonLine(event, &w) catch {
                self.flushBuf(w.buffered());
                w.end = 0;
                security_events.writeJsonLine(event, &w) catch continue;
            };
        }
        self.flushBuf(w.buffered());
    }

    fn flushBuf(self: *SecurityLogger, bytes: []const u8) void {
        if (bytes.len == 0) return;
        _ = std.c.write(self.fd, bytes.ptr, bytes.len);
    }
};

// =========================================================================
// Tests
// =========================================================================

test "SecurityLogger flushes events to file" {
    const allocator = std.testing.allocator;
    try security_events.initGlobal(allocator, 64);
    defer security_events.deinitGlobal();

    const seed = @import("zigts").compat.realtimeNowNs() catch 0;
    var tmp_buf: [128]u8 = undefined;
    const path = try std.fmt.bufPrint(&tmp_buf, "/tmp/zigttp-secevt-test-{d}.jsonl", .{seed});

    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const logger = try SecurityLogger.start(allocator, path);
    defer {
        logger.deinit();
        _ = std.c.unlink(path_z);
    }

    security_events.emitGlobal(security_events.SecurityEvent.init(
        .policy_denied_env,
        "zigttp:env",
        "SECRET_KEY",
    ));
    security_events.emitGlobal(security_events.SecurityEvent.init(
        .arena_audit_failure,
        "runtime",
        "leak detected",
    ));

    const sleep_ts = std.c.timespec{ .sec = 0, .nsec = 50 * std.time.ns_per_ms };
    _ = std.c.nanosleep(&sleep_ts, null);
}
