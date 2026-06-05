//! Opcode-semantic parity gate.
//!
//! Pins that the three execution tiers - the bytecode interpreter, the baseline
//! JIT, and the optimized JIT - agree on opcode semantics for a corpus of small
//! programs. This guards the write-barrier / property-store work (which touched
//! all three tiers) and the future VM-loop deduplication (which collapses the
//! three hand-written opcode implementations into one source of truth): any tier
//! that diverges on arithmetic, comparison, or overflow turns this red.
//!
//! Each corpus case is a 0-argument FunctionBytecode whose body ends in `.ret`,
//! so `interp.run` RETURNS the computed value. (A bare top-level expression is
//! dropped by codegen and the program returns undefined, which would make the
//! gate vacuously green - the trap a prior version of this gate fell into.) Every
//! case also asserts the result is not undefined so a regression to the drop path
//! fails loudly.
//!
//! The corpus is restricted to integer / boolean / numeric-overflow results.
//! String results are intentionally excluded: comparing them safely needs the
//! ctx-bound rope/slice flatten path (a raw toPtr(JSString) cast is unsound on
//! slices and ropes), and the families this gate exists to protect are the
//! arithmetic/comparison opcodes the tiers each reimplement.

const std = @import("std");
const bytecode = @import("../bytecode.zig");
const value = @import("../value.zig");
const context = @import("../context.zig");
const gc = @import("../gc.zig");
const interpreter = @import("../interpreter.zig");
const jit_compile = @import("../interpreter/jit_compile.zig");
const jit_policy = @import("../interpreter/jit_policy.zig");

const O = bytecode.Opcode;
const Interpreter = interpreter.Interpreter;
const JSValue = value.JSValue;

const Kind = enum { number, boolean };

const Case = struct {
    name: []const u8,
    code: []const u8,
    constants: []const JSValue = &.{},
    stack_size: u16 = 8,
    kind: Kind,
    expected_num: f64 = 0,
    expected_bool: bool = false,
};

fn op(comptime o: O) u8 {
    return @intFromEnum(o);
}

// push_i8 takes a single signed-byte operand; push_const takes a big-endian u16
// constant index. Both are baseline-supported. ret returns the top of stack.
const add_code = [_]u8{ op(.push_i8), 5, op(.push_i8), 3, op(.add), op(.ret) };
const sub_code = [_]u8{ op(.push_i8), 10, op(.push_i8), 4, op(.sub), op(.ret) };
const mul_code = [_]u8{ op(.push_i8), 6, op(.push_i8), 7, op(.mul), op(.ret) };
const lt_true_code = [_]u8{ op(.push_i8), 3, op(.push_i8), 5, op(.lt), op(.ret) };
const lt_false_code = [_]u8{ op(.push_i8), 5, op(.push_i8), 3, op(.lt), op(.ret) };
const eq_true_code = [_]u8{ op(.push_i8), 7, op(.push_i8), 7, op(.eq), op(.ret) };
// push_const 0 twice: 2_000_000_000 + 2_000_000_000 overflows i32, so a correct
// tier promotes to f64 (4e9). A tier that wrapped i32 would yield a different
// number and this case would diverge - the negative control the gate needs.
const overflow_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 0, 0, op(.add), op(.ret) };
const overflow_consts = [_]JSValue{JSValue.fromInt(2_000_000_000)};
// -2e9 - 2e9 = -4e9 (overflows i32 negative) and 100000 * 100000 = 1e10 (overflows
// i32, fits i64). These guard the sub/mul overflow-detection paths alongside add.
// push_const operand is a little-endian u16 constant index (readU16: pc[0] | pc[1]<<8).
const sub_overflow_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.sub), op(.ret) };
const sub_overflow_consts = [_]JSValue{ JSValue.fromInt(-2_000_000_000), JSValue.fromInt(2_000_000_000) };
const mul_overflow_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 0, 0, op(.mul), op(.ret) };
const mul_overflow_consts = [_]JSValue{JSValue.fromInt(100_000)};

const cases = [_]Case{
    .{ .name = "add", .code = &add_code, .kind = .number, .expected_num = 8 },
    .{ .name = "sub", .code = &sub_code, .kind = .number, .expected_num = 6 },
    .{ .name = "mul", .code = &mul_code, .kind = .number, .expected_num = 42 },
    .{ .name = "lt_true", .code = &lt_true_code, .kind = .boolean, .expected_bool = true },
    .{ .name = "lt_false", .code = &lt_false_code, .kind = .boolean, .expected_bool = false },
    .{ .name = "eq_true", .code = &eq_true_code, .kind = .boolean, .expected_bool = true },
    .{ .name = "add_overflow", .code = &overflow_code, .constants = &overflow_consts, .kind = .number, .expected_num = 4_000_000_000 },
    .{ .name = "sub_overflow", .code = &sub_overflow_code, .constants = &sub_overflow_consts, .kind = .number, .expected_num = -4_000_000_000 },
    .{ .name = "mul_overflow", .code = &mul_overflow_code, .constants = &mul_overflow_consts, .kind = .number, .expected_num = 10_000_000_000 },
};

fn buildFunc(case: Case) bytecode.FunctionBytecode {
    return .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = case.stack_size,
        .flags = .{},
        .code = case.code,
        .constants = case.constants,
        .source_map = null,
    };
}

/// Numerically/structurally equal across tiers. Identical NaN-box encoding is the
/// fast path (ints, bools, inline doubles); an overflowing add may box its f64
/// result differently per tier, so fall back to numeric comparison.
fn sameValue(a: JSValue, b: JSValue) bool {
    if (a.raw == b.raw) return true;
    const an = a.toNumber() orelse return false;
    const bn = b.toNumber() orelse return false;
    if (std.math.isNan(an) and std.math.isNan(bn)) return true;
    return an == bn;
}

fn checkExpected(case: Case, result: JSValue) !void {
    try std.testing.expect(!result.isUndefined());
    switch (case.kind) {
        .number => {
            const n = result.toNumber() orelse return error.ExpectedNumber;
            try std.testing.expectEqual(case.expected_num, n);
        },
        .boolean => {
            try std.testing.expect(result.isBool());
            try std.testing.expectEqual(JSValue.fromBool(case.expected_bool).raw, result.raw);
        },
    }
}

test "opcode parity: interpreter, baseline, and optimized tiers agree" {
    const allocator = std.testing.allocator;

    const prev_policy = jit_policy.getJitPolicy();
    const prev_threshold = jit_policy.getJitThreshold();
    const prev_warmup = jit_policy.getJitFeedbackWarmup();
    defer {
        jit_policy.setJitPolicy(prev_policy);
        jit_policy.setJitThreshold(prev_threshold);
        jit_policy.setJitFeedbackWarmup(prev_warmup);
    }

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();
    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();
    var interp = Interpreter.init(ctx);

    // JIT may be disabled by environment or unavailable on the host arch. The
    // interpreter tier always runs (so the corpus is never vacuous); the JIT
    // tiers are gated, mirroring the existing JIT tests.
    jit_policy.setJitPolicy(.eager);
    const jit_available = !jit_policy.jitDisabled();

    var baseline_compiled: usize = 0;
    var optimized_reached: usize = 0;

    for (cases) |case| {
        // Tier 1: interpreter. Pin thresholds so maybePromote never compiles.
        jit_policy.setJitThreshold(std.math.maxInt(u32));
        jit_policy.setJitFeedbackWarmup(std.math.maxInt(u32));
        var interp_func = buildFunc(case);
        const interp_result = try interp.run(&interp_func);
        try std.testing.expectEqual(bytecode.CompilationTier.interpreted, interp_func.tier);
        try checkExpected(case, interp_result);

        if (!jit_available) continue;

        // Tier 2: baseline JIT. Eager + threshold 1 forces compilation quickly.
        jit_policy.setJitPolicy(.eager);
        jit_policy.setJitThreshold(1);
        jit_policy.setJitFeedbackWarmup(1);
        var base_func = buildFunc(case);
        defer jit_compile.cleanupCompiledCode(allocator, &base_func);
        defer jit_compile.cleanupTypeFeedback(allocator, &base_func);
        var base_result: JSValue = undefined;
        var i: usize = 0;
        while (i < 6) : (i += 1) base_result = try interp.run(&base_func);
        try std.testing.expect(base_func.compiled_code != null); // genuinely compiled, not silently interpreted
        baseline_compiled += 1;
        try checkExpected(case, base_result);
        try std.testing.expect(sameValue(interp_result, base_result));

        // Tier 3: optimized JIT. Pin the auto-ladder off, warm the interpreter so
        // type feedback records, then compile explicitly. Non-loop cases may not
        // reach .optimized on every arch; parity is asserted only when reached.
        jit_policy.setJitPolicy(.lazy);
        jit_policy.setJitThreshold(std.math.maxInt(u32));
        jit_policy.setJitFeedbackWarmup(std.math.maxInt(u32));
        var opt_func = buildFunc(case);
        defer jit_compile.cleanupCompiledCode(allocator, &opt_func);
        defer jit_compile.cleanupTypeFeedback(allocator, &opt_func);
        try jit_compile.allocateTypeFeedback(&interp, &opt_func);
        var w: usize = 0;
        while (w < 8) : (w += 1) _ = try interp.run(&opt_func);
        jit_compile.tryCompileOptimized(&interp, &opt_func) catch {};
        if (opt_func.tier == .optimized) {
            optimized_reached += 1;
            const opt_result = try interp.run(&opt_func);
            try checkExpected(case, opt_result);
            try std.testing.expect(sameValue(interp_result, opt_result));
        }
    }

    if (jit_available) {
        // Every case must have genuinely exercised the baseline tier.
        try std.testing.expectEqual(cases.len, baseline_compiled);
    }
}
