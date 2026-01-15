//! zts Performance Benchmark Runner
//!
//! Runs JavaScript benchmarks and times them individually from Zig side.
//!
//! Usage: zig build bench

const std = @import("std");
const zruntime = @import("zruntime.zig");
const zq = @import("zts");
const Runtime = zruntime.Runtime;
const RuntimeConfig = zruntime.RuntimeConfig;

pub const std_options: std.Options = .{
    .log_level = .err,
};

const BenchmarkResult = struct {
    name: []const u8,
    iterations: u32,
    time_ms: f64,
    ops_per_sec: u64,
};

const Options = struct {
    json: bool = false,
    quiet: bool = false,
    compare: bool = true,
    script_path: ?[]const u8 = null,
    bench: bool = false,
    bench_fn: []const u8 = "run",
    bench_iterations: u32 = 200000,
    warmup_rounds: u32 = 120,
    warmup_iterations: u32 = 200,
};

const usage =
    "Usage: zigttp-bench [--json] [--quiet] [--no-compare] [--script <path>] [--bench]\n" ++
    "                   [--bench-fn <name>] [--iterations <n>] [--warmup <n>] [--warmup-iters <n>]\n";

fn writeStdout(data: []const u8) void {
    _ = std.c.write(std.c.STDOUT_FILENO, data.ptr, data.len);
}

fn parseU32(arg: []const u8, flag: []const u8) u32 {
    return std.fmt.parseInt(u32, arg, 10) catch {
        writeStdout("Invalid value for ");
        writeStdout(flag);
        writeStdout("\n");
        writeStdout(usage);
        std.process.exit(1);
    };
}

// Global to store args from main
var g_args: std.process.Args = undefined;

fn parseOptions() Options {
    var options = Options{};
    var args = std.process.Args.Iterator.init(g_args);
    defer args.deinit();
    _ = args.skip();

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            options.json = true;
        } else if (std.mem.eql(u8, arg, "--quiet")) {
            options.quiet = true;
        } else if (std.mem.eql(u8, arg, "--no-compare")) {
            options.compare = false;
        } else if (std.mem.eql(u8, arg, "--script")) {
            const path = args.next() orelse {
                writeStdout("Missing value for --script\n");
                writeStdout(usage);
                std.process.exit(1);
            };
            options.script_path = path;
        } else if (std.mem.eql(u8, arg, "--bench")) {
            options.bench = true;
        } else if (std.mem.eql(u8, arg, "--bench-fn")) {
            const name = args.next() orelse {
                writeStdout("Missing value for --bench-fn\n");
                writeStdout(usage);
                std.process.exit(1);
            };
            options.bench_fn = name;
        } else if (std.mem.eql(u8, arg, "--iterations")) {
            const value = args.next() orelse {
                writeStdout("Missing value for --iterations\n");
                writeStdout(usage);
                std.process.exit(1);
            };
            options.bench_iterations = parseU32(value, "--iterations");
        } else if (std.mem.eql(u8, arg, "--warmup")) {
            const value = args.next() orelse {
                writeStdout("Missing value for --warmup\n");
                writeStdout(usage);
                std.process.exit(1);
            };
            options.warmup_rounds = parseU32(value, "--warmup");
        } else if (std.mem.eql(u8, arg, "--warmup-iters")) {
            const value = args.next() orelse {
                writeStdout("Missing value for --warmup-iters\n");
                writeStdout(usage);
                std.process.exit(1);
            };
            options.warmup_iterations = parseU32(value, "--warmup-iters");
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            writeStdout(usage);
            std.process.exit(0);
        }
    }

    return options;
}

fn println(msg: []const u8) void {
    writeStdout(msg);
    writeStdout("\n");
}

/// Read file using POSIX syscalls (before Io is initialized)
fn readFilePosix(allocator: std.mem.Allocator, path: []const u8, max_size: usize) ![]u8 {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.posix.close(fd);

    const stat = try std.posix.fstat(fd);
    const file_size: usize = @intCast(@max(0, stat.size));

    if (file_size > max_size) return error.FileTooBig;

    const buffer = try allocator.alloc(u8, file_size);
    errdefer allocator.free(buffer);

    var total_read: usize = 0;
    while (total_read < file_size) {
        const bytes_read = try std.posix.read(fd, buffer[total_read..]);
        if (bytes_read == 0) break;
        total_read += bytes_read;
    }

    return buffer[0..total_read];
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
        .name = "httpHandlerHeavy",
        .iterations = 2000,
        .code =
        \\function runHttpHandlerHeavy(iterations) {
        \\    let responses = 0;
        \\    let baseHeaders = { 'content-type': 'application/json', 'cache-control': 'no-store' };
        \\    let payload = JSON.stringify({ id: 1, name: 'User1', tags: ['alpha','beta','gamma'] });
        \\    for (let i of range(iterations)) {
        \\        let reqPath = (i % 3 === 0) ? '/api/users' : '/api/users/42';
        \\        let query = (i % 2 === 0) ? '?limit=10&offset=5' : '?limit=25&offset=0';
        \\        let limit = (query.indexOf('limit=25') !== -1) ? 25 : 10;
        \\        let offset = (query.indexOf('offset=5') !== -1) ? 5 : 0;
        \\        let bodyObj = null;
        \\        if (reqPath.indexOf('/api/users') === 0) {
        \\            bodyObj = JSON.parse(payload);
        \\            bodyObj.limit = limit;
        \\            bodyObj.offset = offset;
        \\        }
        \\        let headers = {
        \\            'content-type': baseHeaders['content-type'],
        \\            'cache-control': baseHeaders['cache-control'],
        \\            'x-request-id': 'req-' + (i % 1000)
        \\        };
        \\        let response = {
        \\            status: (reqPath === '/api/users') ? 200 : 201,
        \\            headers: headers,
        \\            body: JSON.stringify({ ok: true, path: reqPath, data: bodyObj })
        \\        };
        \\        responses = (responses + response.body.length + response.status) % 1000000;
        \\    }
        \\    return responses;
        \\}
        \\runHttpHandlerHeavy(2000);
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

pub fn main(init: std.process.Init.Minimal) !void {
    g_args = init.args;
    const options = parseOptions();
    // Use c_allocator (libc malloc/free) for benchmarks
    // - Proper memory freeing (unlike page_allocator)
    // - No tracking overhead (unlike GPA which is 20x slower)
    const allocator = std.heap.c_allocator;

    const config = RuntimeConfig{
        .memory_limit = 64 * 1024 * 1024, // 64MB for benchmarks
        .enable_jsx = false,
        .enable_fetch = false,
        .enable_fs = false,
        .use_hybrid_allocation = true, // Keep arena for performance
        .enforce_arena_escape = false, // Allow arena escapes - script lifetime matches arena
    };

    if (options.script_path) |script_path| {
        const code = readFilePosix(allocator, script_path, 10 * 1024 * 1024) catch |err| {
            std.log.err("Failed to read script {s}: {}", .{ script_path, err });
            return;
        };
        defer allocator.free(code);

        const runtime = try Runtime.init(allocator, config);
        // Note: Skip runtime.deinit() - page_allocator doesn't support individual frees
        // All memory is released when the process exits

        runtime.loadCode(code, script_path) catch |err| {
            std.log.err("Failed to execute script {s}: {}", .{ script_path, err });
            return;
        };

        if (options.bench) {
            const max_i32 = std.math.maxInt(i32);
            if (options.bench_iterations > max_i32 or options.warmup_iterations > max_i32) {
                std.log.err("Iterations exceed i32 range", .{});
                return;
            }

            const warmup_arg = zq.JSValue.fromInt(@intCast(options.warmup_iterations));
            const warmup_args = [_]zq.JSValue{warmup_arg};
            var warm_idx: u32 = 0;
            while (warm_idx < options.warmup_rounds) : (warm_idx += 1) {
                _ = runtime.callGlobalFunction(options.bench_fn, &warmup_args) catch |err| {
                    std.log.err("Warmup call failed: {}", .{err});
                    return;
                };
            }

            const run_arg = zq.JSValue.fromInt(@intCast(options.bench_iterations));
            const run_args = [_]zq.JSValue{run_arg};

            const start = std.time.Instant.now() catch {
                std.log.err("Timer not available", .{});
                return;
            };
            _ = runtime.callGlobalFunction(options.bench_fn, &run_args) catch |err| {
                std.log.err("Benchmark call failed: {}", .{err});
                return;
            };
            const end = std.time.Instant.now() catch {
                std.log.err("Timer not available", .{});
                return;
            };

            const elapsed_ns = end.since(start);
            const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
            const ops_per_sec: u64 = if (elapsed_ns > 0)
                @intFromFloat(@as(f64, @floatFromInt(options.bench_iterations)) * 1_000_000_000.0 / @as(f64, @floatFromInt(elapsed_ns)))
            else
                0;

            const name = std.fs.path.basename(script_path);
            printFmt("bench {s}::{s} iters={} warmup={} ms={d:.3} ops/s={}", .{
                name,
                options.bench_fn,
                options.bench_iterations,
                options.warmup_rounds,
                elapsed_ms,
                ops_per_sec,
            });
            return;
        }
        return;
    }

    if (!options.quiet and !options.json) {
        println("");
        println("=== zts JavaScript Engine Benchmarks ===");
        println("");
    }

    var results: [benchmarks.len]BenchmarkResult = undefined;
    var total_time_ns: u64 = 0;

    for (benchmarks, 0..) |bench, i| {
        // Create fresh runtime for each benchmark
        const runtime = try Runtime.init(allocator, config);
        // Note: Skip runtime.deinit() - page_allocator doesn't support individual frees

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

        if (!options.quiet and !options.json) {
            printFmt("{s}: {d:.3}ms ({} ops/sec)", .{ bench.name, elapsed_ms, ops_per_sec });
        }
    }

    if (options.json) {
        writeStdout("{\n  \"benchmarks\": [\n");
        for (results, 0..) |result, i| {
            var line_buf: [512]u8 = undefined;
            const line = std.fmt.bufPrint(
                &line_buf,
                "    {{\"name\":\"{s}\",\"iterations\":{},\"time_ms\":{d:.3},\"ops_per_sec\":{}}}",
                .{ result.name, result.iterations, result.time_ms, result.ops_per_sec },
            ) catch continue;
            writeStdout(line);
            if (i + 1 < results.len) {
                writeStdout(",\n");
            } else {
                writeStdout("\n");
            }
        }
        const total_ms = @as(f64, @floatFromInt(total_time_ns)) / 1_000_000.0;
        var tail_buf: [128]u8 = undefined;
        const tail = std.fmt.bufPrint(&tail_buf, "  ],\n  \"total_ms\":{d:.3}\n}}\n", .{total_ms}) catch return;
        writeStdout(tail);
        return;
    }

    if (!options.quiet and options.compare) {
        println("");
        println("=== Comparison with Baseline (2026-01-10) ===");
        println("");
    }

    // Baseline values from benchmarks/2026-01-10-mod-const-optimization.json
    const baseline = [_]struct { name: []const u8, ops_per_sec: u64 }{
        .{ .name = "intArithmetic", .ops_per_sec = 17225787 },
        .{ .name = "stringConcat", .ops_per_sec = 10323733 },
        .{ .name = "stringOps", .ops_per_sec = 22449208 },
        .{ .name = "objectCreate", .ops_per_sec = 9923588 },
        .{ .name = "propertyAccess", .ops_per_sec = 17985611 },
        .{ .name = "arrayOps", .ops_per_sec = 13231594 },
        .{ .name = "functionCalls", .ops_per_sec = 16688684 },
        .{ .name = "recursion", .ops_per_sec = 2855 },
        .{ .name = "jsonOps", .ops_per_sec = 85355 },
        .{ .name = "gcPressure", .ops_per_sec = 274482 },
        .{ .name = "httpHandler", .ops_per_sec = 1131466 },
        .{ .name = "forOfLoop", .ops_per_sec = 53210339 },
    };

    if (!options.quiet and options.compare) {
        printFmt("{s:<20} {s:>15} {s:>15} {s:>10}", .{ "Benchmark", "zts", "baseline", "Ratio" });
        println("------------------------------------------------------------");
    }

    const getBaseline = struct {
        fn find(name: []const u8) ?u64 {
            for (baseline) |entry| {
                if (std.mem.eql(u8, entry.name, name)) return entry.ops_per_sec;
            }
            return null;
        }
    }.find;

    if (!options.quiet and options.compare) {
        for (results) |result| {
            const base_ops = getBaseline(result.name) orelse continue;
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
        println("");
    }

    const total_ms = @as(f64, @floatFromInt(total_time_ns)) / 1_000_000.0;
    printFmt("Total time: {d:.1}ms", .{total_ms});
    println("");
}
