//! zts Performance Benchmark Runner
//!
//! Runs JavaScript benchmarks and times them individually from Zig side.
//!
//! Usage: zig build bench

const std = @import("std");
const zruntime = @import("zruntime.zig");
const Runtime = zruntime.Runtime;
const RuntimeConfig = zruntime.RuntimeConfig;

const BenchmarkResult = struct {
    name: []const u8,
    iterations: u32,
    time_ms: f64,
    ops_per_sec: u64,
};

fn println(msg: []const u8) void {
    std.fs.File.stdout().writeAll(msg) catch {};
    std.fs.File.stdout().writeAll("\n") catch {};
}

fn printFmt(comptime fmt: []const u8, args: anytype) void {
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, fmt, args) catch return;
    println(msg);
}

const ITERATIONS: u32 = 50000;

// Each benchmark as a separate code string
const benchmarks = [_]struct { name: []const u8, iterations: u32, code: []const u8 }{
    .{
        .name = "intArithmetic",
        .iterations = ITERATIONS,
        .code =
        \\function runIntArithmetic(iterations) {
        \\    let sum = 0;
        \\    for (let i of range(iterations)) {
        \\        sum = (sum + i) % 1000000;
        \\        sum = (sum - (i % 1000) + 1000000) % 1000000;
        \\        sum = (sum * 2) % 1000000;
        \\        sum = (sum >> 1);
        \\    }
        \\    return sum;
        \\}
        \\runIntArithmetic(50000);
        ,
    },
    .{
        .name = "stringConcat",
        .iterations = ITERATIONS,
        .code =
        \\function runStringConcat(iterations) {
        \\    let result = '';
        \\    for (let i of range(iterations)) {
        \\        result = result + 'x';
        \\        if (result.length > 1000) {
        \\            result = '';
        \\        }
        \\    }
        \\    return result.length;
        \\}
        \\runStringConcat(50000);
        ,
    },
    .{
        .name = "stringOps",
        .iterations = ITERATIONS,
        .code =
        \\function runStringOps(iterations) {
        \\    let str = 'The quick brown fox jumps over the lazy dog';
        \\    let count = 0;
        \\    for (let i of range(iterations)) {
        \\        count = (count + str.indexOf('fox')) % 1000000;
        \\        count = (count + str.length) % 1000000;
        \\    }
        \\    return count;
        \\}
        \\runStringOps(50000);
        ,
    },
    .{
        .name = "objectCreate",
        .iterations = ITERATIONS,
        .code =
        \\function runObjectCreate(iterations) {
        \\    let objects = [];
        \\    for (let i of range(iterations)) {
        \\        objects.push({ id: i, name: 'item' });
        \\        if (objects.length > 100) {
        \\            objects = [];
        \\        }
        \\    }
        \\    return objects.length;
        \\}
        \\runObjectCreate(50000);
        ,
    },
    .{
        .name = "propertyAccess",
        .iterations = ITERATIONS,
        .code =
        \\function runPropertyAccess(iterations) {
        \\    let obj = { a: 1, b: 2, c: 3, d: 4, e: 5 };
        \\    let sum = 0;
        \\    for (let i of range(iterations)) {
        \\        sum = (sum + obj.a + obj.b + obj.c + obj.d + obj.e) % 1000000;
        \\        obj.a = i % 100;
        \\    }
        \\    return sum;
        \\}
        \\runPropertyAccess(50000);
        ,
    },
    .{
        .name = "arrayOps",
        .iterations = ITERATIONS,
        .code =
        \\function runArrayOps(iterations) {
        \\    let arr = [];
        \\    let sum = 0;
        \\    for (let i of range(iterations)) {
        \\        arr.push(i % 1000);
        \\        if (arr.length > 100) {
        \\            for (let val of arr) {
        \\                sum = (sum + val) % 1000000;
        \\            }
        \\            arr = [];
        \\        }
        \\    }
        \\    return sum;
        \\}
        \\runArrayOps(50000);
        ,
    },
    .{
        .name = "functionCalls",
        .iterations = ITERATIONS,
        .code =
        \\function add(a, b) { return (a + b) % 1000000; }
        \\function compute(x, y) { return add(x, y); }
        \\function runFunctionCalls(iterations) {
        \\    let result = 0;
        \\    for (let i of range(iterations)) {
        \\        result = compute(i % 1000, result);
        \\    }
        \\    return result;
        \\}
        \\runFunctionCalls(50000);
        ,
    },
    .{
        .name = "recursion",
        .iterations = 25,
        .code =
        \\function fib(n) {
        \\    if (n <= 1) return n;
        \\    return fib(n - 1) + fib(n - 2);
        \\}
        \\fib(25);
        ,
    },
    .{
        .name = "jsonOps",
        .iterations = 5000,
        .code =
        \\function runJsonOps(iterations) {
        \\    let obj = { users: [{ id: 1 }, { id: 2 }] };
        \\    let count = 0;
        \\    for (let i of range(iterations)) {
        \\        let json = JSON.stringify(obj);
        \\        let parsed = JSON.parse(json);
        \\        count = (count + parsed.users.length) % 1000000;
        \\    }
        \\    return count;
        \\}
        \\runJsonOps(5000);
        ,
    },
    .{
        .name = "gcPressure",
        .iterations = ITERATIONS,
        .code =
        \\function runGcPressure(iterations) {
        \\    let count = 0;
        \\    for (let i of range(iterations)) {
        \\        let obj = { a: i % 100, b: 'str' };
        \\        let str = JSON.stringify(obj);
        \\        count = (count + str.length) % 1000000;
        \\    }
        \\    return count;
        \\}
        \\runGcPressure(50000);
        ,
    },
    .{
        .name = "httpHandler",
        .iterations = 5000,
        .code =
        \\function runHttpHandler(iterations) {
        \\    let responses = 0;
        \\    for (let i of range(iterations)) {
        \\        let response = {
        \\            status: 200,
        \\            body: JSON.stringify({ id: i % 100, name: 'User' })
        \\        };
        \\        responses = (responses + response.body.length) % 1000000;
        \\    }
        \\    return responses;
        \\}
        \\runHttpHandler(5000);
        ,
    },
    .{
        .name = "forOfLoop",
        .iterations = ITERATIONS,
        .code =
        \\function runForOfLoop(iterations) {
        \\    let arr = [];
        \\    for (let i of range(100)) {
        \\        arr.push(i);
        \\    }
        \\    let sum = 0;
        \\    let loops = (iterations / 100) >> 0;
        \\    for (let j of range(loops)) {
        \\        for (let val of arr) {
        \\            sum = (sum + val) % 1000000;
        \\        }
        \\    }
        \\    return sum;
        \\}
        \\runForOfLoop(50000);
        ,
    },
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const config = RuntimeConfig{
        .memory_limit = 8 * 1024 * 1024, // 8MB for benchmarks
        .enable_jsx = false,
        .enable_fetch = false,
        .enable_fs = false,
    };

    println("");
    println("=== zts JavaScript Engine Benchmarks ===");
    println("");

    var results: [benchmarks.len]BenchmarkResult = undefined;
    var total_time_ns: u64 = 0;

    for (benchmarks, 0..) |bench, i| {
        // Create fresh runtime for each benchmark
        const runtime = try Runtime.init(allocator, config);
        defer runtime.deinit();

        const start = std.time.Instant.now() catch {
            println("Timer not available");
            return;
        };

        runtime.loadCode(bench.code, bench.name) catch |err| {
            printFmt("{s}: ERROR - {}", .{ bench.name, err });
            continue;
        };

        const end = std.time.Instant.now() catch continue;
        const elapsed_ns = end.since(start);
        total_time_ns += elapsed_ns;

        const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
        const ops_per_sec: u64 = if (elapsed_ns > 0)
            @intFromFloat(@as(f64, @floatFromInt(bench.iterations)) * 1_000_000_000.0 / @as(f64, @floatFromInt(elapsed_ns)))
        else
            0;

        results[i] = .{
            .name = bench.name,
            .iterations = bench.iterations,
            .time_ms = elapsed_ms,
            .ops_per_sec = ops_per_sec,
        };

        printFmt("{s}: {d:.3}ms ({} ops/sec)", .{ bench.name, elapsed_ms, ops_per_sec });
    }

    println("");
    println("=== Comparison with Baseline (mquickjs) ===");
    println("");

    // Baseline values from bench-baseline.json
    const baseline = [_]struct { name: []const u8, ops_per_sec: u64 }{
        .{ .name = "intArithmetic", .ops_per_sec = 16134237 },
        .{ .name = "stringConcat", .ops_per_sec = 6187353 },
        .{ .name = "stringOps", .ops_per_sec = 258402 },
        .{ .name = "objectCreate", .ops_per_sec = 1696526 },
        .{ .name = "propertyAccess", .ops_per_sec = 3357733 },
        .{ .name = "arrayOps", .ops_per_sec = 6589352 },
        .{ .name = "functionCalls", .ops_per_sec = 5145092 },
        .{ .name = "recursion", .ops_per_sec = 2244 },
        .{ .name = "jsonOps", .ops_per_sec = 70761 },
        .{ .name = "gcPressure", .ops_per_sec = 229426 },
        .{ .name = "httpHandler", .ops_per_sec = 332469 },
        .{ .name = "forOfLoop", .ops_per_sec = 54824561 },
    };

    printFmt("{s:<20} {s:>15} {s:>15} {s:>10}", .{ "Benchmark", "zts", "mquickjs", "Ratio" });
    println("------------------------------------------------------------");

    for (results, 0..) |result, i| {
        if (i < baseline.len) {
            const base_ops = baseline[i].ops_per_sec;
            const ratio = if (base_ops > 0)
                @as(f64, @floatFromInt(result.ops_per_sec)) / @as(f64, @floatFromInt(base_ops))
            else
                0.0;
            const indicator: []const u8 = if (ratio >= 1.0) " " else " ";
            printFmt("{s:<20} {d:>12}/s {d:>12}/s {d:>7.2}x{s}", .{
                result.name,
                result.ops_per_sec,
                base_ops,
                ratio,
                indicator,
            });
        }
    }

    println("");
    const total_ms = @as(f64, @floatFromInt(total_time_ns)) / 1_000_000.0;
    printFmt("Total time: {d:.1}ms", .{total_ms});
    println("");
}
