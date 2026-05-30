const std = @import("std");

// Printer owns nothing. It holds borrowed pointers to two std.Io.Writer
// instances so that deploy code paths can emit output without hard-coding a
// destination. Production callers point it at FdWriter-backed writers that
// round-trip through std.c.write (matching the rest of the deploy module),
// while tests point it at std.Io.Writer.Allocating buffers via BufferedPrinter.
//
// Write errors are swallowed on purpose: the deploy CLI never checked broken
// pipes before, and this refactor must be behaviour-preserving.
pub const Printer = struct {
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,

    pub fn line(self: Printer, prefix: []const u8, value: []const u8) void {
        self.stdout.writeAll(prefix) catch {};
        if (value.len > 0) self.stdout.writeAll(value) catch {};
        self.stdout.writeAll("\n") catch {};
        self.stdout.flush() catch {};
    }

    pub fn warn(self: Printer, text: []const u8) void {
        self.stderr.writeAll(text) catch {};
        self.stderr.writeAll("\n") catch {};
        self.stderr.flush() catch {};
    }

    // write emits text to stderr without appending a newline. Used by
    // first_run.zig for multi-line messages that already carry their own
    // newlines.
    pub fn write(self: Printer, text: []const u8) void {
        self.stderr.writeAll(text) catch {};
        self.stderr.flush() catch {};
    }
};

// Progress wraps a Printer with a monotonic-clock rate limiter so long-running
// phases (OCI push, provider provisioning) can stream "still working" lines to
// stderr without flooding the user. Phase-granularity only; nothing here knows
// about bytes or percentages. Wave 5.
pub const Progress = struct {
    printer: Printer,
    // Sentinel value ensures the first emit always fires regardless of clock.
    last_emit_ms: i64 = std.math.minInt(i64) / 2,
    // Minimum milliseconds between emissions. Callers can set to 0 to force
    // every call through; production uses 500ms.
    min_interval_ms: i64 = 500,
    // Injectable clock for tests. Returns monotonic milliseconds.
    now_fn: *const fn () i64 = defaultNowMs,

    pub fn emit(self: *Progress, text: []const u8) void {
        const now = self.now_fn();
        if (now - self.last_emit_ms < self.min_interval_ms) return;
        self.last_emit_ms = now;
        self.printer.warn(text);
    }

    pub fn force(self: *Progress, text: []const u8) void {
        self.last_emit_ms = self.now_fn();
        self.printer.warn(text);
    }
};

fn defaultNowMs() i64 {
    var ts: std.posix.timespec = undefined;
    switch (std.posix.errno(std.posix.system.clock_gettime(.MONOTONIC, &ts))) {
        .SUCCESS => {
            const seconds: i64 = @intCast(ts.sec);
            const nanos: i64 = @intCast(ts.nsec);
            return seconds * std.time.ms_per_s + @divTrunc(nanos, std.time.ns_per_ms);
        },
        else => return 0,
    }
}

// FdWriter wraps a raw POSIX file descriptor (STDOUT_FILENO / STDERR_FILENO)
// in the std.Io.Writer interface. We use this instead of std.Io.File.writer
// because the deploy module already writes via std.c.write throughout (see
// deploy/config.zig, zruntime.zig, benchmark.zig) and we do not want to
// introduce an std.Io dependency here.
pub const FdWriter = struct {
    interface: std.Io.Writer,
    fd: c_int,

    pub fn init(fd: c_int, buffer: []u8) FdWriter {
        return .{
            .interface = .{
                .vtable = &vtable,
                .buffer = buffer,
            },
            .fd = fd,
        };
    }

    const vtable: std.Io.Writer.VTable = .{
        .drain = drain,
    };

    fn drain(
        w: *std.Io.Writer,
        data: []const []const u8,
        splat: usize,
    ) std.Io.Writer.Error!usize {
        const self: *FdWriter = @fieldParentPtr("interface", w);

        // Flush pre-buffered bytes first. The Io.Writer contract lets us
        // fully consume buffer[0..end] before the data slices.
        const buffered_bytes = w.buffer[0..w.end];
        if (buffered_bytes.len > 0) {
            writeFd(self.fd, buffered_bytes);
            w.end = 0;
        }

        if (data.len == 0) return 0;

        // Write all but the last element once.
        var written: usize = 0;
        var i: usize = 0;
        while (i + 1 < data.len) : (i += 1) {
            writeFd(self.fd, data[i]);
            written += data[i].len;
        }

        // The last element is repeated `splat` times.
        const tail = data[data.len - 1];
        var remaining = splat;
        while (remaining > 0) : (remaining -= 1) {
            writeFd(self.fd, tail);
            written += tail.len;
        }
        return written;
    }

    fn writeFd(fd: c_int, bytes: []const u8) void {
        if (bytes.len == 0) return;
        _ = std.c.write(fd, bytes.ptr, bytes.len);
    }
};

// BufferedPrinter is a test helper. It owns two Allocating writers and hands
// out a Printer that routes writes into them. After the code under test
// returns, use stdoutSlice / stderrSlice to assert on the captured transcript.
pub const BufferedPrinter = struct {
    stdout_writer: std.Io.Writer.Allocating,
    stderr_writer: std.Io.Writer.Allocating,

    pub fn init(allocator: std.mem.Allocator) BufferedPrinter {
        return .{
            .stdout_writer = std.Io.Writer.Allocating.init(allocator),
            .stderr_writer = std.Io.Writer.Allocating.init(allocator),
        };
    }

    pub fn deinit(self: *BufferedPrinter) void {
        self.stdout_writer.deinit();
        self.stderr_writer.deinit();
    }

    pub fn printer(self: *BufferedPrinter) Printer {
        return .{
            .stdout = &self.stdout_writer.writer,
            .stderr = &self.stderr_writer.writer,
        };
    }

    pub fn stdoutSlice(self: *BufferedPrinter) []const u8 {
        return self.stdout_writer.writer.buffered();
    }

    pub fn stderrSlice(self: *BufferedPrinter) []const u8 {
        return self.stderr_writer.writer.buffered();
    }
};

test "Printer.line writes prefix and value then newline" {
    var buffered = BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    buffered.printer().line("  Handler:    ", "handler.ts");

    try std.testing.expectEqualStrings("  Handler:    handler.ts\n", buffered.stdoutSlice());
    try std.testing.expectEqualStrings("", buffered.stderrSlice());
}

test "Printer.line writes prefix only when value is empty" {
    var buffered = BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    buffered.printer().line("  Verifying.. ok", "");

    try std.testing.expectEqualStrings("  Verifying.. ok\n", buffered.stdoutSlice());
}

test "Printer.warn appends newline" {
    var buffered = BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    buffered.printer().warn("boom");

    try std.testing.expectEqualStrings("", buffered.stdoutSlice());
    try std.testing.expectEqualStrings("boom\n", buffered.stderrSlice());
}

test "Printer.write does not append newline" {
    var buffered = BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    buffered.printer().write("Signed in as a@b.\n");

    try std.testing.expectEqualStrings("Signed in as a@b.\n", buffered.stderrSlice());
}

// FakeClock scaffolding for Progress tests. Returns successive values from a
// slice so tests can simulate a wall clock without touching std.time.
const FakeClock = struct {
    var values: []const i64 = &.{};
    var idx: usize = 0;

    fn now() i64 {
        const v = values[idx];
        idx += 1;
        return v;
    }

    fn reset(v: []const i64) void {
        values = v;
        idx = 0;
    }
};

test "Progress.emit rate-limits within the window" {
    var buffered = BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    FakeClock.reset(&.{ 0, 100, 200, 600 });
    var progress = Progress{
        .printer = buffered.printer(),
        .min_interval_ms = 500,
        .now_fn = FakeClock.now,
    };

    // First call at t=0 fires (last_emit_ms sentinel is minInt/2), second at
    // t=100 and third at t=200 are inside the 500ms window, fourth at t=600
    // fires again.
    progress.emit("a");
    progress.emit("b");
    progress.emit("c");
    progress.emit("d");

    try std.testing.expectEqualStrings("a\nd\n", buffered.stderrSlice());
}

test "Progress.force emits regardless and updates window" {
    var buffered = BufferedPrinter.init(std.testing.allocator);
    defer buffered.deinit();

    FakeClock.reset(&.{ 100, 200 });
    var progress = Progress{
        .printer = buffered.printer(),
        .min_interval_ms = 500,
        .now_fn = FakeClock.now,
    };

    progress.force("first");
    // emit at t=200 should be suppressed because force just set last=100.
    progress.emit("second");

    try std.testing.expectEqualStrings("first\n", buffered.stderrSlice());
    // last_emit_ms was updated by force to 100.
    try std.testing.expectEqual(@as(i64, 100), progress.last_emit_ms);
}
