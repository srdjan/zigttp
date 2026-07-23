const std = @import("std");

pub const ClockError = error{ClockGettimeFailed};

fn clockGetTime(clock_id: std.posix.CLOCK) ClockError!std.posix.timespec {
    var ts: std.posix.timespec = undefined;
    const rc = std.c.clock_gettime(
        @as(std.c.clockid_t, @enumFromInt(@intFromEnum(clock_id))),
        &ts,
    );
    if (rc != 0) return error.ClockGettimeFailed;
    return ts;
}

pub fn realtimeNowNs() ClockError!u64 {
    const ts = try clockGetTime(.REALTIME);
    const sec: u64 = @intCast(ts.sec);
    const nsec: u64 = @intCast(ts.nsec);
    return sec * std.time.ns_per_s + nsec;
}

pub fn realtimeNowMs() ClockError!i64 {
    const ts = try clockGetTime(.REALTIME);
    return ts.sec * 1000 + @divTrunc(ts.nsec, 1_000_000);
}

pub fn monotonicNowNs() ClockError!u64 {
    const ts = try clockGetTime(.MONOTONIC);
    const sec: u64 = @intCast(ts.sec);
    const nsec: u64 = @intCast(ts.nsec);
    return sec * std.time.ns_per_s + nsec;
}

pub const Instant = struct {
    ns: u64,

    pub fn now() ClockError!Instant {
        return .{ .ns = try monotonicNowNs() };
    }

    pub fn since(self: Instant, start: Instant) u64 {
        return self.ns -| start.ns;
    }
};

pub const Timer = struct {
    start_ns: u64,

    pub fn start() ClockError!Timer {
        return .{ .start_ns = try monotonicNowNs() };
    }

    pub fn read(self: *const Timer) u64 {
        const now = monotonicNowNs() catch return 0;
        return now -| self.start_ns;
    }
};

pub const Mutex = struct {
    inner: std.Io.Mutex = .init,

    pub fn lock(self: *Mutex) void {
        self.inner.lockUncancelable(std.Options.debug_io);
    }

    pub fn unlock(self: *Mutex) void {
        self.inner.unlock(std.Options.debug_io);
    }
};

pub const RwLock = struct {
    inner: std.Io.RwLock = .init,

    pub fn lock(self: *RwLock) void {
        self.inner.lockUncancelable(std.Options.debug_io);
    }

    pub fn unlock(self: *RwLock) void {
        self.inner.unlock(std.Options.debug_io);
    }

    pub fn lockShared(self: *RwLock) void {
        self.inner.lockSharedUncancelable(std.Options.debug_io);
    }

    pub fn unlockShared(self: *RwLock) void {
        self.inner.unlockShared(std.Options.debug_io);
    }
};
