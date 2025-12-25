//! MicroQuickJS Performance Benchmark Runner
//!
//! Measures performance across key areas:
//! - Value operations (int/float arithmetic)
//! - String operations (concat, methods)
//! - Object creation and property access
//! - Function calls and closures
//! - GC pressure
//! - Realistic HTTP handler simulation
//!
//! Usage: zig build bench
//!        zig build bench -- --iterations 100000

const std = @import("std");
const mq = @import("mquickjs.zig");
const Runtime = @import("runtime.zig").Runtime;
const RuntimeConfig = @import("runtime.zig").RuntimeConfig;
const bindings = @import("bindings.zig");

const BenchmarkResult = struct {
    name: []const u8,
    iterations: u32,
    time_ns: u64,
    ops_per_sec: f64,
    result_value: i64,
};

const BenchmarkSuite = struct {
    results: std.ArrayList(BenchmarkResult),
    allocator: std.mem.Allocator,
    runtime: Runtime,
    iterations: u32,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, iterations: u32) !Self {
        const config = RuntimeConfig{
            .memory_limit = 4 * 1024 * 1024, // 4MB for benchmarks
            .enable_jsx = false,
            .enable_fetch = false,
            .enable_fs = false,
        };

        var runtime = try Runtime.init(allocator, config);
        errdefer runtime.deinit();

        // Load benchmark code
        const bench_code = @embedFile("benchmarks.js");
        const eval_result = mq.eval(runtime.ctx, bench_code, "benchmarks.js", .{});
        switch (eval_result) {
            .ok => {},
            .err => |e| {
                std.log.err("Failed to load benchmarks: {s}", .{e.message orelse "unknown error"});
                return error.BenchmarkLoadFailed;
            },
        }

        return Self{
            .results = .empty,
            .allocator = allocator,
            .runtime = runtime,
            .iterations = iterations,
        };
    }

    pub fn deinit(self: *Self) void {
        self.results.deinit(self.allocator);
        self.runtime.deinit();
    }

    pub fn runBenchmark(self: *Self, name: []const u8, iters: ?u32) !void {
        const iterations = iters orelse self.iterations;

        // Get the runBenchmark function
        const global = mq.getGlobalObject(self.runtime.ctx);
        const run_fn_result = mq.getPropertyStr(self.runtime.ctx, global, "runBenchmark");
        const run_fn = switch (run_fn_result) {
            .ok => |v| v,
            .err => return error.BenchmarkNotFound,
        };

        if (!mq.isFunction(self.runtime.ctx, run_fn)) {
            return error.BenchmarkNotFound;
        }

        // Create arguments
        const name_str_result = mq.fromString(self.runtime.ctx, name);
        const name_str = switch (name_str_result) {
            .ok => |v| v,
            .err => return error.StringCreationFailed,
        };
        const iter_val = mq.fromInt(self.runtime.ctx, @intCast(iterations));

        // Timed run
        const start = std.time.nanoTimestamp();
        const call_result = mq.call(self.runtime.ctx, run_fn, global, &[_]mq.JSValue{ name_str, iter_val });

        const end = std.time.nanoTimestamp();
        const elapsed_ns: u64 = @intCast(end - start);

        // Extract result
        var result_value: i64 = 0;
        switch (call_result) {
            .ok => |result_obj| {
                if (mq.isObject(self.runtime.ctx, result_obj)) {
                    const result_prop = mq.getPropertyStr(self.runtime.ctx, result_obj, "result");
                    switch (result_prop) {
                        .ok => |v| {
                            if (mq.isNumber(self.runtime.ctx, v)) {
                                const num_result = mq.toFloat64(self.runtime.ctx, v);
                                switch (num_result) {
                                    .ok => |n| result_value = @intFromFloat(n),
                                    .err => {},
                                }
                            }
                        },
                        .err => {},
                    }
                }
            },
            .err => |e| {
                std.log.err("Benchmark '{s}' failed: {s}", .{ name, e.message orelse "unknown" });
                return error.BenchmarkFailed;
            },
        }

        const ops_per_sec = @as(f64, @floatFromInt(iterations)) / (@as(f64, @floatFromInt(elapsed_ns)) / 1_000_000_000.0);

        try self.results.append(self.allocator, .{
            .name = name,
            .iterations = iterations,
            .time_ns = elapsed_ns,
            .ops_per_sec = ops_per_sec,
            .result_value = result_value,
        });
    }

    pub fn runAll(self: *Self) !void {
        const benchmarks = [_]struct { name: []const u8, iters: ?u32 }{
            .{ .name = "intArithmetic", .iters = null },
            .{ .name = "floatArithmetic", .iters = null },
            .{ .name = "stringConcat", .iters = null },
            .{ .name = "stringOps", .iters = null },
            .{ .name = "objectCreate", .iters = null },
            .{ .name = "propertyAccess", .iters = null },
            .{ .name = "arrayOps", .iters = null },
            .{ .name = "functionCalls", .iters = null },
            .{ .name = "recursion", .iters = 25 }, // Fibonacci depth
            .{ .name = "closures", .iters = null },
            .{ .name = "jsonOps", .iters = self.iterations / 10 }, // Slower
            .{ .name = "gcPressure", .iters = null },
            .{ .name = "httpHandler", .iters = self.iterations / 10 },
            .{ .name = "forOfLoop", .iters = null },
            .{ .name = "typedArrays", .iters = null },
        };

        for (benchmarks) |b| {
            self.runBenchmark(b.name, b.iters) catch |err| {
                std.log.warn("Skipping benchmark '{s}': {}", .{ b.name, err });
            };
        }
    }

    pub fn printResults(self: *Self) void {
        const stdout = std.fs.File.stdout().deprecatedWriter();

        stdout.print("\n", .{}) catch {};
        stdout.print("╔══════════════════════════════════════════════════════════════════════════╗\n", .{}) catch {};
        stdout.print("║                    MicroQuickJS Performance Benchmarks                   ║\n", .{}) catch {};
        stdout.print("╠══════════════════════════════════════════════════════════════════════════╣\n", .{}) catch {};
        stdout.print("║ {s:<20} │ {s:>12} │ {s:>12} │ {s:>15} ║\n", .{ "Benchmark", "Iterations", "Time (ms)", "Ops/sec" }) catch {};
        stdout.print("╠══════════════════════════════════════════════════════════════════════════╣\n", .{}) catch {};

        var total_time_ns: u64 = 0;
        for (self.results.items) |r| {
            const time_ms = @as(f64, @floatFromInt(r.time_ns)) / 1_000_000.0;
            stdout.print("║ {s:<20} │ {d:>12} │ {d:>12.2} │ {d:>15.0} ║\n", .{
                r.name,
                r.iterations,
                time_ms,
                r.ops_per_sec,
            }) catch {};
            total_time_ns += r.time_ns;
        }

        stdout.print("╠══════════════════════════════════════════════════════════════════════════╣\n", .{}) catch {};
        const total_time_ms = @as(f64, @floatFromInt(total_time_ns)) / 1_000_000.0;
        stdout.print("║ {s:<20} │ {s:>12} │ {d:>12.2} │ {s:>15} ║\n", .{ "TOTAL", "", total_time_ms, "" }) catch {};
        stdout.print("╚══════════════════════════════════════════════════════════════════════════╝\n", .{}) catch {};
        stdout.print("\n", .{}) catch {};
    }

    pub fn exportJson(self: *Self) ![]u8 {
        var json: std.ArrayList(u8) = .empty;
        errdefer json.deinit(self.allocator);

        try json.appendSlice(self.allocator, "{\n  \"benchmarks\": [\n");
        for (self.results.items, 0..) |r, i| {
            const time_ms = @as(f64, @floatFromInt(r.time_ns)) / 1_000_000.0;

            var buf: [512]u8 = undefined;
            const entry = std.fmt.bufPrint(&buf,
                \\    {{
                \\      "name": "{s}",
                \\      "iterations": {d},
                \\      "time_ms": {d:.3},
                \\      "ops_per_sec": {d:.0}
                \\    }}
            , .{ r.name, r.iterations, time_ms, r.ops_per_sec }) catch return error.OutOfMemory;

            try json.appendSlice(self.allocator, entry);
            if (i < self.results.items.len - 1) {
                try json.appendSlice(self.allocator, ",\n");
            } else {
                try json.appendSlice(self.allocator, "\n");
            }
        }
        try json.appendSlice(self.allocator, "  ]\n}\n");

        return json.toOwnedSlice(self.allocator);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse arguments
    var iterations: u32 = 100_000;
    var json_output = false;

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = args.next(); // Skip program name

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--iterations") or std.mem.eql(u8, arg, "-i")) {
            if (args.next()) |val| {
                iterations = std.fmt.parseInt(u32, val, 10) catch 100_000;
            }
        } else if (std.mem.eql(u8, arg, "--json") or std.mem.eql(u8, arg, "-j")) {
            json_output = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            const stdout = std.fs.File.stdout().deprecatedWriter();
            try stdout.writeAll(
                \\MicroQuickJS Benchmark Suite
                \\
                \\Usage: zigttp-bench [options]
                \\
                \\Options:
                \\  -i, --iterations N   Number of iterations per benchmark (default: 100000)
                \\  -j, --json           Output results as JSON
                \\  -h, --help           Show this help
                \\
                \\Benchmarks:
                \\  intArithmetic    - Integer math operations
                \\  floatArithmetic  - Float math operations
                \\  stringConcat     - String concatenation
                \\  stringOps        - String method calls
                \\  objectCreate     - Object allocation
                \\  propertyAccess   - Property get/set
                \\  arrayOps         - Array operations
                \\  functionCalls    - Function call overhead
                \\  recursion        - Recursive calls (fibonacci)
                \\  closures         - Closure creation
                \\  jsonOps          - JSON parse/stringify
                \\  gcPressure       - GC stress test
                \\  httpHandler      - HTTP handler simulation
                \\  forOfLoop        - ES6 for-of iteration
                \\  typedArrays      - TypedArray operations
                \\
            );
            return;
        }
    }

    // Run benchmarks
    var suite = try BenchmarkSuite.init(allocator, iterations);
    defer suite.deinit();

    try suite.runAll();

    if (json_output) {
        const json = try suite.exportJson();
        defer allocator.free(json);
        const stdout = std.fs.File.stdout().deprecatedWriter();
        try stdout.writeAll(json);
    } else {
        suite.printResults();
    }
}

test "benchmark suite initialization" {
    const allocator = std.testing.allocator;
    var suite = try BenchmarkSuite.init(allocator, 1000);
    defer suite.deinit();
}
