//! zigts Compile-Time Microbenchmark
//!
//! Measures parse + codegen wall-clock time, bytes allocated, and IR-node
//! count per compile across a small synthesized corpus of handler-shaped
//! programs. Scaffolding only: this harness collects the numbers that
//! Phase 8 of the perf plan uses to tune
//! `packages/zigts/src/parser/codegen.zig#reserveCapacity` and
//! `packages/zigts/src/intern_pool.zig` capacity hints. No tuning here.
//!
//! Usage: `zig build compile-bench -Doptimize=ReleaseFast -- [flags]`
//! Flags:
//!   --json            Emit JSON to stdout.
//!   --quiet           Suppress human-readable output.
//!   --iterations N    Iterations per fixture (default 50).

const std = @import("std");
const zq = @import("zigts");
const compat = zq.compat;

pub const std_options: std.Options = .{
    .log_level = .err,
};

// -- Fixtures -----------------------------------------------------------------
//
// Inline ES5+ subset fixtures. Kept in-file to avoid coupling the harness to
// any on-disk example that might change shape over time. Four sizes span the
// spectrum the tuner cares about: tiny (hello), small (arith loop), medium
// (struct/array work), large (module-style with many functions).

const Fixture = struct {
    name: []const u8,
    source: []const u8,
};

const fixtures = [_]Fixture{
    .{
        .name = "tiny_hello",
        .source =
        \\function handler(req) {
        \\  return Response.json({ ok: true, msg: "hello" });
        \\}
        ,
    },
    .{
        .name = "small_arith",
        .source =
        \\function handler(req) {
        \\  let sum = 0;
        \\  for (const n of [1, 2, 3, 4, 5, 6, 7, 8]) {
        \\    sum = sum + n * 2;
        \\  }
        \\  const doubled = sum + sum;
        \\  const tripled = doubled + sum;
        \\  return Response.json({ sum: sum, doubled: doubled, tripled: tripled });
        \\}
        ,
    },
    .{
        .name = "medium_object_ops",
        .source =
        \\function buildUser(id, name) {
        \\  return { id: id, name: name, active: true, score: id * 10 };
        \\}
        \\
        \\function scoreFor(user) {
        \\  const bonus = user.active ? 100 : 0;
        \\  return user.score + bonus;
        \\}
        \\
        \\function handler(req) {
        \\  const users = [
        \\    buildUser(1, "alice"),
        \\    buildUser(2, "bob"),
        \\    buildUser(3, "carol"),
        \\    buildUser(4, "dave"),
        \\  ];
        \\  let total = 0;
        \\  for (const u of users) {
        \\    total = total + scoreFor(u);
        \\  }
        \\  const first = users[0];
        \\  const summary = {
        \\    count: 4,
        \\    total: total,
        \\    lead: first.name,
        \\    status: total > 100 ? "ok" : "low",
        \\  };
        \\  return Response.json(summary);
        \\}
        ,
    },
    .{
        .name = "large_module",
        .source =
        \\function greet(name) { return "hello, " + name; }
        \\function shout(s) { return s + "!"; }
        \\function wrap(s) { return "<" + s + ">"; }
        \\function pair(a, b) { return { a: a, b: b }; }
        \\function triple(a, b, c) { return { a: a, b: b, c: c }; }
        \\function sumArr(xs) {
        \\  let s = 0;
        \\  for (const x of xs) { s = s + x; }
        \\  return s;
        \\}
        \\function mulArr(xs) {
        \\  let m = 1;
        \\  for (const x of xs) { m = m * x; }
        \\  return m;
        \\}
        \\function maxArr(xs) {
        \\  let m = xs[0];
        \\  for (const x of xs) { if (x > m) { m = x; } }
        \\  return m;
        \\}
        \\function buildReport(xs) {
        \\  const s = sumArr(xs);
        \\  const m = mulArr(xs);
        \\  const top = maxArr(xs);
        \\  return { sum: s, product: m, max: top, count: xs.length };
        \\}
        \\function handler(req) {
        \\  const xs = [2, 3, 5, 7, 11, 13];
        \\  const ys = [1, 4, 9, 16, 25];
        \\  const r1 = buildReport(xs);
        \\  const r2 = buildReport(ys);
        \\  const greeting = shout(wrap(greet("world")));
        \\  const out = {
        \\    greeting: greeting,
        \\    reports: pair(r1, r2),
        \\    meta: triple("v1", "compile-bench", xs.length + ys.length),
        \\  };
        \\  return Response.json(out);
        \\}
        ,
    },
};

// -- Counting allocator -------------------------------------------------------
//
// Honest bytes-per-compile accounting. Wraps an underlying allocator and
// totals every successful allocation and resize grow. We explicitly do not
// subtract frees: "peak live bytes" would require a different shape and
// the tuner wants a churn proxy, which this gives.

const CountingAlloc = struct {
    backing: std.mem.Allocator,
    total_bytes: u64 = 0,

    fn allocator(self: *CountingAlloc) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = vtAlloc,
                .resize = vtResize,
                .remap = vtRemap,
                .free = vtFree,
            },
        };
    }

    fn vtAlloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *CountingAlloc = @ptrCast(@alignCast(ctx));
        const result = self.backing.rawAlloc(len, alignment, ret_addr);
        if (result != null) self.total_bytes += len;
        return result;
    }

    fn vtResize(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *CountingAlloc = @ptrCast(@alignCast(ctx));
        const ok = self.backing.rawResize(buf, alignment, new_len, ret_addr);
        if (ok and new_len > buf.len) {
            self.total_bytes += new_len - buf.len;
        }
        return ok;
    }

    fn vtRemap(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *CountingAlloc = @ptrCast(@alignCast(ctx));
        const result = self.backing.rawRemap(buf, alignment, new_len, ret_addr);
        if (result != null and new_len > buf.len) {
            self.total_bytes += new_len - buf.len;
        }
        return result;
    }

    fn vtFree(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const self: *CountingAlloc = @ptrCast(@alignCast(ctx));
        self.backing.rawFree(buf, alignment, ret_addr);
    }
};

// -- Compile once ------------------------------------------------------------
//
// Runs the same path hot compilation takes: parse -> IR -> codegen. We do
// NOT run BoolChecker / TypeChecker / bytecode optimizer passes from
// zruntime.loadCode on purpose: the target under tune is codegen buffer
// sizing (`reserveCapacity`) and IR-node -> bytecode ratio.

const CompileStats = struct {
    /// Total bytes across tokenizer/parser/IR/codegen. Matches the prior
    /// single-counter number so baselines remain comparable.
    bytes: u64,
    /// Subset of `bytes` attributable to CodeGen (reserveCapacity growth,
    /// constants pool, labels, pending_jumps, bytecode array). This is the
    /// number the Phase 8 tuning pass uses to calibrate `reserveCapacity`.
    codegen_bytes: u64,
    ir_nodes: u64,
    bytecode_len: u64,
};

fn compileOnce(backing: std.mem.Allocator, source: []const u8) !CompileStats {
    var parser_counting = CountingAlloc{ .backing = backing };
    var codegen_counting = CountingAlloc{ .backing = backing };
    const parser_alloc = parser_counting.allocator();
    const codegen_alloc = codegen_counting.allocator();

    var strings = zq.string.StringTable.init(parser_alloc);
    defer strings.deinit();
    var atoms = zq.context.AtomTable.init(parser_alloc);
    defer atoms.deinit();

    var p = zq.Parser.init(parser_alloc, source, &strings, &atoms);
    defer p.deinit();

    const bytecode_data = try p.parseWithCodegenAllocator(codegen_alloc);
    const ir_node_count: u64 = @intCast(p.js_parser.nodes.tags.items.len);

    return .{
        .bytes = parser_counting.total_bytes + codegen_counting.total_bytes,
        .codegen_bytes = codegen_counting.total_bytes,
        .ir_nodes = ir_node_count,
        .bytecode_len = @intCast(bytecode_data.len),
    };
}

// -- Driver ------------------------------------------------------------------

const Options = struct {
    json: bool = false,
    quiet: bool = false,
    iterations: u32 = 50,
};

const usage =
    "Usage: compile-bench [--json] [--quiet] [--iterations N]\n";

fn writeStdout(data: []const u8) void {
    _ = std.c.write(std.c.STDOUT_FILENO, data.ptr, data.len);
}

fn printFmt(comptime fmt: []const u8, args: anytype) void {
    var buf: [1024]u8 = undefined;
    const out = std.fmt.bufPrint(&buf, fmt ++ "\n", args) catch return;
    writeStdout(out);
}

fn parseOptions() Options {
    var options = Options{};
    var iter = std.process.Args.Iterator.init(g_args);
    defer iter.deinit();
    _ = iter.skip();

    while (iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            options.json = true;
        } else if (std.mem.eql(u8, arg, "--quiet")) {
            options.quiet = true;
        } else if (std.mem.eql(u8, arg, "--iterations")) {
            const val = iter.next() orelse {
                writeStdout("Missing value for --iterations\n");
                writeStdout(usage);
                std.process.exit(1);
            };
            options.iterations = std.fmt.parseInt(u32, val, 10) catch {
                writeStdout("Invalid value for --iterations\n");
                writeStdout(usage);
                std.process.exit(1);
            };
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            writeStdout(usage);
            std.process.exit(0);
        }
    }

    if (options.iterations == 0) options.iterations = 1;
    return options;
}

const Result = struct {
    name: []const u8,
    iterations: u32,
    ns_per_iter: u64,
    bytes_alloc: u64,
    codegen_bytes: u64,
    ir_nodes: u64,
    bytecode_len: u64,
    success: bool,
    error_name: ?[]const u8 = null,
};

fn runFixture(allocator: std.mem.Allocator, fx: Fixture, iterations: u32) Result {
    const fail = struct {
        fn mk(fixture: Fixture, iters: u32, bytes: u64, cg_bytes: u64, ir_nodes: u64, bc_len: u64, err_name: []const u8) Result {
            return .{
                .name = fixture.name,
                .iterations = iters,
                .ns_per_iter = 0,
                .bytes_alloc = bytes,
                .codegen_bytes = cg_bytes,
                .ir_nodes = ir_nodes,
                .bytecode_len = bc_len,
                .success = false,
                .error_name = err_name,
            };
        }
    }.mk;

    // One warm compile to surface parse errors before timing.
    const warm = compileOnce(allocator, fx.source) catch |err| {
        return fail(fx, iterations, 0, 0, 0, 0, @errorName(err));
    };

    const start = compat.Instant.now() catch {
        return fail(fx, iterations, warm.bytes, warm.codegen_bytes, warm.ir_nodes, warm.bytecode_len, "TimerUnavailable");
    };

    var total_bytes: u64 = 0;
    var total_cg_bytes: u64 = 0;
    var i: u32 = 0;
    while (i < iterations) : (i += 1) {
        const s = compileOnce(allocator, fx.source) catch |err| {
            return fail(fx, iterations, total_bytes, total_cg_bytes, warm.ir_nodes, warm.bytecode_len, @errorName(err));
        };
        total_bytes += s.bytes;
        total_cg_bytes += s.codegen_bytes;
    }

    const end = compat.Instant.now() catch {
        return fail(fx, iterations, total_bytes / iterations, total_cg_bytes / iterations, warm.ir_nodes, warm.bytecode_len, "TimerUnavailable");
    };
    const elapsed_ns = end.since(start);

    return .{
        .name = fx.name,
        .iterations = iterations,
        .ns_per_iter = elapsed_ns / iterations,
        .bytes_alloc = total_bytes / iterations,
        .codegen_bytes = total_cg_bytes / iterations,
        .ir_nodes = warm.ir_nodes,
        .bytecode_len = warm.bytecode_len,
        .success = true,
    };
}

fn emitJson(results: []const Result) void {
    writeStdout("{\n  \"schema_version\": 1,\n  \"benchmarks\": [\n");
    for (results, 0..) |r, i| {
        var line: [640]u8 = undefined;
        const written = if (r.success)
            std.fmt.bufPrint(
                &line,
                "    {{\"name\":\"{s}\",\"iterations\":{},\"ns_per_iter\":{},\"bytes_alloc\":{},\"codegen_bytes\":{},\"ir_nodes\":{},\"bytecode_len\":{},\"success\":true}}",
                .{ r.name, r.iterations, r.ns_per_iter, r.bytes_alloc, r.codegen_bytes, r.ir_nodes, r.bytecode_len },
            ) catch continue
        else
            std.fmt.bufPrint(
                &line,
                "    {{\"name\":\"{s}\",\"iterations\":{},\"ns_per_iter\":{},\"bytes_alloc\":{},\"codegen_bytes\":{},\"ir_nodes\":{},\"bytecode_len\":{},\"success\":false,\"error_name\":\"{s}\"}}",
                .{ r.name, r.iterations, r.ns_per_iter, r.bytes_alloc, r.codegen_bytes, r.ir_nodes, r.bytecode_len, r.error_name orelse "unknown" },
            ) catch continue;
        writeStdout(written);
        writeStdout(if (i + 1 < results.len) ",\n" else "\n");
    }
    writeStdout("  ]\n}\n");
}

fn emitHuman(results: []const Result) void {
    writeStdout("\n=== zigts compile-time microbench ===\n\n");
    printFmt("{s:<24} {s:>12} {s:>14} {s:>12} {s:>10} {s:>8}", .{ "fixture", "ns/compile", "bytes/compile", "cg_bytes", "ir_nodes", "bc_len" });
    writeStdout("------------------------------------------------------------------------------------\n");
    for (results) |r| {
        if (r.success) {
            printFmt("{s:<24} {d:>12} {d:>14} {d:>12} {d:>10} {d:>8}", .{
                r.name, r.ns_per_iter, r.bytes_alloc, r.codegen_bytes, r.ir_nodes, r.bytecode_len,
            });
        } else {
            printFmt("{s:<24} FAILED: {s}", .{ r.name, r.error_name orelse "unknown" });
        }
    }
    writeStdout("\n");
}

var g_args: std.process.Args = undefined;

pub fn main(init: std.process.Init.Minimal) !void {
    g_args = init.args;
    const options = parseOptions();
    // libc malloc/free for proper per-compile freeing without GPA tracking
    // overhead. Matches packages/runtime/src/benchmark.zig.
    const allocator = std.heap.c_allocator;

    var results: [fixtures.len]Result = undefined;
    for (fixtures, 0..) |fx, i| {
        results[i] = runFixture(allocator, fx, options.iterations);
    }

    if (options.json) {
        emitJson(&results);
    } else if (!options.quiet) {
        emitHuman(&results);
    }
}

// -- Tests -------------------------------------------------------------------

test "compile-bench runs one iteration per fixture under 2s" {
    // Parser currently leaks nested-function bytecode duplicates when
    // driven outside the runtime (zruntime's Context owns that memory).
    // An arena isolates the test from that pre-existing behavior; every
    // allocation is released at the end of the test regardless.
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const start = try compat.Instant.now();
    for (fixtures) |fx| {
        const r = runFixture(allocator, fx, 1);
        try std.testing.expect(r.success);
        try std.testing.expect(r.ir_nodes > 0);
    }
    const end = try compat.Instant.now();
    const elapsed_ns = end.since(start);
    // 2s wall-clock ceiling. Exists to catch catastrophic regressions
    // (e.g. an accidental O(n^2) path), not as a perf gate.
    try std.testing.expect(elapsed_ns < 2 * std.time.ns_per_s);
}
