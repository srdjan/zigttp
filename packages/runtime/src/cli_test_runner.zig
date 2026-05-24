//! Simple test runner for the developer CLI test target.
//!
//! The default Zig build runner uses server mode, which gives good timeout
//! accounting but buffers per-test stderr until failure. CLI tests are the
//! target most likely to exercise process-like command paths, so run them with
//! a small watchdog that keeps CI and local shells informed while a test runs.

const builtin = @import("builtin");
const std = @import("std");
const testing = std.testing;

const heartbeat_ns: i128 = 3 * std.time.ns_per_s;
const test_timeout_ns: i128 = 30 * std.time.ns_per_s;
const runner_io = std.Io.Threaded.global_single_threaded.io();
const TestStatus = enum { pass, skip, fail };

pub const std_options: std.Options = .{
    .logFn = log,
};

var log_err_count: usize = 0;

const WatchState = struct {
    mutex: std.Io.Mutex = .init,
    done: bool = false,
    active_name: []const u8 = "",
    active_index: usize = 0,
    total: usize = 0,
    active_started_ns: i128 = 0,
    last_report_ns: i128 = 0,
    passed: usize = 0,
    skipped: usize = 0,
    failed: usize = 0,
    leaks: usize = 0,
    log_errors: usize = 0,
};

var watch_state: WatchState = .{};

pub fn main(init: std.process.Init.Minimal) !void {
    @disableInstrumentation();

    const test_fns = builtin.test_functions;
    {
        watch_state.mutex.lockUncancelable(runner_io);
        defer watch_state.mutex.unlock(runner_io);
        watch_state.total = test_fns.len;
    }

    std.debug.print(
        "test-cli: running {d} tests (heartbeat {d}s, per-test timeout {d}s)\n",
        .{ test_fns.len, heartbeat_ns / std.time.ns_per_s, test_timeout_ns / std.time.ns_per_s },
    );

    var watchdog = try std.Thread.spawn(.{}, watchdogMain, .{});
    defer {
        watch_state.mutex.lockUncancelable(runner_io);
        watch_state.done = true;
        watch_state.mutex.unlock(runner_io);
        watchdog.join();
    }

    var ok_count: usize = 0;
    var skip_count: usize = 0;
    var fail_count: usize = 0;
    var leak_count: usize = 0;
    var log_error_count: usize = 0;

    for (test_fns, 0..) |test_fn, index| {
        beginTest(test_fn.name, index, test_fns.len);

        testing.allocator_instance = .{};
        testing.io_instance = .init(testing.allocator, .{
            .argv0 = .init(init.args),
            .environ = init.environ,
        });
        testing.log_level = .warn;
        testing.environ = init.environ;
        log_err_count = 0;

        const status: TestStatus = if (test_fn.func()) |_| .pass else |err| switch (err) {
            error.SkipZigTest => .skip,
            else => fail: {
                if (@errorReturnTrace()) |trace| {
                    std.debug.dumpErrorReturnTrace(trace);
                }
                break :fail .fail;
            },
        };

        testing.io_instance.deinit();
        const test_leaks = testing.allocator_instance.detectLeaks();
        testing.allocator_instance.deinitWithoutLeakChecks();
        const test_log_errors = log_err_count;

        switch (status) {
            .pass => ok_count += 1,
            .skip => skip_count += 1,
            .fail => fail_count += 1,
        }
        leak_count += test_leaks;
        log_error_count += test_log_errors;

        finishTest(status, test_leaks, test_log_errors);
    }

    {
        watch_state.mutex.lockUncancelable(runner_io);
        watch_state.active_name = "";
        watch_state.mutex.unlock(runner_io);
    }

    std.debug.print(
        "test-cli: done {d} passed; {d} skipped; {d} failed; {d} leaked; {d} logged errors\n",
        .{ ok_count, skip_count, fail_count, leak_count, log_error_count },
    );

    if (fail_count != 0 or leak_count != 0 or log_error_count != 0) {
        std.process.exit(1);
    }
}

fn beginTest(name: []const u8, index: usize, total: usize) void {
    const now = nowNs();
    watch_state.mutex.lockUncancelable(runner_io);
    defer watch_state.mutex.unlock(runner_io);
    watch_state.active_name = name;
    watch_state.active_index = index + 1;
    watch_state.total = total;
    watch_state.active_started_ns = now;
    watch_state.last_report_ns = now;
}

fn finishTest(status: TestStatus, leaks: usize, log_errors: usize) void {
    watch_state.mutex.lockUncancelable(runner_io);
    defer watch_state.mutex.unlock(runner_io);
    switch (status) {
        .pass => watch_state.passed += 1,
        .skip => watch_state.skipped += 1,
        .fail => watch_state.failed += 1,
    }
    watch_state.leaks += leaks;
    watch_state.log_errors += log_errors;
    watch_state.active_name = "";
}

fn watchdogMain() void {
    while (true) {
        runner_io.sleep(.fromNanoseconds(std.time.ns_per_s), .awake) catch {};

        const snapshot = snapshotWatchState();
        if (snapshot.done) return;
        if (snapshot.active_name.len == 0) continue;

        const now = nowNs();
        const elapsed_ns = now - snapshot.active_started_ns;
        if (elapsed_ns >= test_timeout_ns) {
            std.debug.print(
                "test-cli: TIMEOUT after {d}s [{d}/{d}] {s} (passed {d}, skipped {d}, failed {d})\n",
                .{
                    @divTrunc(elapsed_ns, std.time.ns_per_s),
                    snapshot.active_index,
                    snapshot.total,
                    snapshot.active_name,
                    snapshot.passed,
                    snapshot.skipped,
                    snapshot.failed,
                },
            );
            std.process.exit(1);
        }

        if (now - snapshot.last_report_ns >= heartbeat_ns) {
            markReported(now);
            std.debug.print(
                "test-cli: running {d}s [{d}/{d}] {s} (passed {d}, skipped {d}, failed {d})\n",
                .{
                    @divTrunc(elapsed_ns, std.time.ns_per_s),
                    snapshot.active_index,
                    snapshot.total,
                    snapshot.active_name,
                    snapshot.passed,
                    snapshot.skipped,
                    snapshot.failed,
                },
            );
        }
    }
}

const WatchSnapshot = struct {
    done: bool,
    active_name: []const u8,
    active_index: usize,
    total: usize,
    active_started_ns: i128,
    last_report_ns: i128,
    passed: usize,
    skipped: usize,
    failed: usize,
};

fn snapshotWatchState() WatchSnapshot {
    watch_state.mutex.lockUncancelable(runner_io);
    defer watch_state.mutex.unlock(runner_io);
    return .{
        .done = watch_state.done,
        .active_name = watch_state.active_name,
        .active_index = watch_state.active_index,
        .total = watch_state.total,
        .active_started_ns = watch_state.active_started_ns,
        .last_report_ns = watch_state.last_report_ns,
        .passed = watch_state.passed,
        .skipped = watch_state.skipped,
        .failed = watch_state.failed,
    };
}

fn markReported(now: i128) void {
    watch_state.mutex.lockUncancelable(runner_io);
    defer watch_state.mutex.unlock(runner_io);
    watch_state.last_report_ns = now;
}

fn nowNs() i128 {
    return std.Io.Timestamp.now(runner_io, .awake).nanoseconds;
}

pub fn log(
    comptime message_level: std.log.Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    @disableInstrumentation();
    if (@intFromEnum(message_level) <= @intFromEnum(std.log.Level.err)) {
        log_err_count +|= 1;
    }
    if (@intFromEnum(message_level) <= @intFromEnum(testing.log_level)) {
        std.debug.print(
            "[" ++ @tagName(scope) ++ "] (" ++ @tagName(message_level) ++ "): " ++ format ++ "\n",
            args,
        );
    }
}
