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
//! String results are intentionally excluded from RAW comparison: comparing
//! them safely needs the ctx-bound rope/slice flatten path (a raw
//! toPtr(JSString) cast is unsound on slices and ropes). String-PRODUCING
//! opcodes are still covered by a dedicated gate below that reduces every
//! string result to a boolean inside the VM (strict_eq against an expected
//! constant), so no tier's string output is ever inspected via raw casts here.

const std = @import("std");
const bytecode = @import("../bytecode.zig");
const value = @import("../value.zig");
const context = @import("../context.zig");
const gc = @import("../gc.zig");
const interpreter = @import("../interpreter.zig");
const object = @import("../object.zig");
const arena_mod = @import("../arena.zig");
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
    expect_nan: bool = false,
    expect_neg_zero: bool = false,
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

// --- ECMAScript numeric-conformance cases (regression guards) ---
// ToInt32 of a float >= 2^63 must NOT panic (the naive @intFromFloat(@trunc) was
// illegal behavior); `1e30 & 1` reduces modulo 2^32 first and is even -> 0.
const to_int32_big_code = [_]u8{ op(.push_const), 0, 0, op(.push_i8), 1, op(.bit_and), op(.ret) };
const to_int32_big_consts = [_]JSValue{JSValue.fromFloat(1e30)};
// Float modulo and modulo-by-zero: `1.5 % 2.5 == 1.5`; `5 % 0 == NaN` (not a
// thrown DivisionByZero that aborts the handler).
const mod_float_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.mod), op(.ret) };
const mod_float_consts = [_]JSValue{ JSValue.fromFloat(1.5), JSValue.fromFloat(2.5) };
const mod_zero_code = [_]u8{ op(.push_i8), 5, op(.push_i8), 0, op(.mod), op(.ret) };
// A NaN relational operand yields false (unordered), not a thrown TypeError.
const nan_lt_code = [_]u8{ op(.push_const), 0, 0, op(.push_i8), 5, op(.lt), op(.ret) };
const nan_lt_consts = [_]JSValue{JSValue.fromFloat(std.math.nan(f64))};
// `(-1) >>> 0` is the unsigned 32-bit value 4294967295, boxed as a float because
// it exceeds maxInt(i32) - not a negative int. push_i8 0xFF is the signed byte -1.
const ushr_neg_code = [_]u8{ op(.push_i8), 0xFF, op(.push_i8), 0, op(.ushr), op(.ret) };

// --- Negative-zero cases ---
// IEEE -0.0 NaN-boxes as the sign-bit-only raw double (0x8000_0000_0000_0000,
// prefix 0x8000 < 0xFFFC), so it survives push_const unmolested on every tier.
// `1 / -0 == -Infinity` is the observable that distinguishes -0 from +0 (===
// cannot, per spec); `1 / -Infinity` PRODUCES -0 arithmetically rather than
// just round-tripping a constant. checkExpected asserts the sign bit via
// expect_neg_zero (expectEqual passes for -0 vs +0, so a plain expected_num
// of 0 would be vacuous), and sameValue compares zero signs across tiers.
const neg_zero_const_code = [_]u8{ op(.push_const), 0, 0, op(.ret) };
const neg_zero_consts = [_]JSValue{JSValue.fromFloat(-0.0)};
const div_neg_zero_code = [_]u8{ op(.push_i8), 1, op(.push_const), 0, 0, op(.div), op(.ret) };
const div_neg_inf_code = [_]u8{ op(.push_i8), 1, op(.push_const), 0, 0, op(.div), op(.ret) };
const div_neg_inf_consts = [_]JSValue{JSValue.fromFloat(-std.math.inf(f64))};
// -1.0 * 0 multiplies through the float slow path (mixed float/int operands)
// and must yield -0, not +0, on every tier.
const mul_neg_zero_code = [_]u8{ op(.push_const), 0, 0, op(.push_i8), 0, op(.mul), op(.ret) };
const mul_neg_zero_consts = [_]JSValue{JSValue.fromFloat(-1.0)};
// `-0.0 === 0.0` is true per ECMAScript (=== treats the zeros as equal); both
// operands are raw doubles so this exercises the float equality lane, not the
// raw-bits fast path.
const neg_zero_strict_eq_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.strict_eq), op(.ret) };
const neg_zero_strict_eq_consts = [_]JSValue{ JSValue.fromFloat(-0.0), JSValue.fromFloat(0.0) };
// --- Unary minus across the int and float lanes ---
// The baseline's emitNeg once guarded its fast path by LSB alone (a
// shift-tagged-SMI leftover that never matched this engine's NaN-boxing):
// every value with an even low bit - even ints, INT_MIN, floats like 2.5,
// and +0.0 - was sar/neg/shl'd on the full raw word into a garbage double
// (or, for +0.0, into sign-stripped +0). These cases pin the prefix-correct
// behavior: int payload negation on the fast path (int 0 stays int 0, like
// negValue), with floats and INT_MIN routed to the jitNeg helper, which
// preserves -0 via the float lane and promotes -INT_MIN to f64.
const neg_float_zero_code = [_]u8{ op(.push_const), 0, 0, op(.neg), op(.ret) };
const neg_float_zero_consts = [_]JSValue{JSValue.fromFloat(0.0)};
const neg_even_int_code = [_]u8{ op(.push_i8), 4, op(.neg), op(.ret) };
const neg_odd_int_code = [_]u8{ op(.push_i8), 3, op(.neg), op(.ret) };
// push_i8 0xFC is the signed byte -4; -(-4) covers negative (sign-extended) payloads.
const neg_negative_int_code = [_]u8{ op(.push_i8), 0xFC, op(.neg), op(.ret) };
const neg_int_zero_code = [_]u8{ op(.push_i8), 0, op(.neg), op(.ret) };
const neg_int_min_code = [_]u8{ op(.push_const), 0, 0, op(.neg), op(.ret) };
const neg_int_min_consts = [_]JSValue{JSValue.fromInt(std.math.minInt(i32))};
const neg_float_code = [_]u8{ op(.push_const), 0, 0, op(.neg), op(.ret) };
const neg_float_consts = [_]JSValue{JSValue.fromFloat(2.5)};

// --- inc / dec ---
// Same broken-LSB history as neg: emitIncDec once sar/op/shl'd the full raw
// word, so inc on an even int landed on payload bit 1 (4 -> 6), dec on
// INT_MIN and inc on a float produced garbage doubles, and only odd-LSB
// values escaped to the (correct) helper. No codegen site emits inc/dec
// today (the language bans ++/--), but the opcodes are interpreter- and
// JIT-implemented, so the corpus pins them at the bytecode level.
const inc_even_int_code = [_]u8{ op(.push_i8), 4, op(.inc), op(.ret) };
const inc_odd_int_code = [_]u8{ op(.push_i8), 3, op(.inc), op(.ret) };
const dec_even_int_code = [_]u8{ op(.push_i8), 4, op(.dec), op(.ret) };
const inc_int_max_code = [_]u8{ op(.push_const), 0, 0, op(.inc), op(.ret) };
const inc_int_max_consts = [_]JSValue{JSValue.fromInt(std.math.maxInt(i32))};
const dec_int_min_code = [_]u8{ op(.push_const), 0, 0, op(.dec), op(.ret) };
const dec_int_min_consts = [_]JSValue{JSValue.fromInt(std.math.minInt(i32))};
const inc_float_code = [_]u8{ op(.push_const), 0, 0, op(.inc), op(.ret) };
const inc_float_consts = [_]JSValue{JSValue.fromFloat(2.5)};

// --- NaN comparison cases ---
// nan_lt above covers NaN-on-the-left vs an int operand (helper-call lane,
// since the int's tag prefix bails the emitted fast path). These extend the
// family: NaN on the right, the remaining relational operators, and float
// operands chosen so the baseline's emitted raw-double compare (fcmp/ucomisd
// unordered handling) runs instead of the helper - 0.3's bit pattern has LSB
// 1, which steers the emitted code off the integer guard onto the float lane.
const lt_nan_rhs_code = [_]u8{ op(.push_i8), 5, op(.push_const), 0, 0, op(.lt), op(.ret) };
const nan_gt_code = [_]u8{ op(.push_const), 0, 0, op(.push_i8), 5, op(.gt), op(.ret) };
const nan_float_consts = [_]JSValue{ JSValue.fromFloat(std.math.nan(f64)), JSValue.fromFloat(0.3) };
const nan_lt_float_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.lt), op(.ret) };
const nan_eq_float_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.eq), op(.ret) };
const float_gte_nan_code = [_]u8{ op(.push_const), 1, 0, op(.push_const), 0, 0, op(.gte), op(.ret) };
// Identical-bit NaN equality: all tiers now correctly return false for
// `NaN === NaN` (IEEE 754 / JS spec). The interpreter's strictEquals/looseEquals
// fast paths and the baseline JIT raw-compare fast path all guard against NaN.
const nan_strict_eq_self_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 0, 0, op(.strict_eq), op(.ret) };
const nan_eq_self_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 0, 0, op(.eq), op(.ret) };
// Different-bit NaNs (canonical vs sign-flipped) dodge the raw fast path and
// land on the real NaN handling: false on every tier, ES-correct.
const nan_diff_consts = [_]JSValue{ JSValue.fromFloat(std.math.nan(f64)), JSValue.fromFloat(-std.math.nan(f64)) };
const nan_strict_eq_diff_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.strict_eq), op(.ret) };
const nan_eq_diff_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.eq), op(.ret) };
// NaN comparison feeding a branch. `NaN < 5` is false, so if_true falls
// through and the program returns 7; a tier that read the comparison as true
// returns 9 instead. Branch offsets are i16 little-endian relative to the pc
// after the operand bytes (if_true at 6, operands at 7..8, next pc 9, taken
// target 9 + 3 = 12).
const nan_branch_code = [_]u8{
    op(.push_const), 0, 0, // NaN
    op(.push_i8),    5,
    op(.lt), // false
    op(.if_true),
    3,
    0,
    op(.push_i8),
    7,
    op(.ret),
    op(.push_i8),
    9,
    op(.ret),
};
// Taken-branch polarity: `NaN != 5` is true (the one comparison NaN satisfies),
// so if_true jumps and the program returns 9.
const nan_neq_branch_code = [_]u8{
    op(.push_const), 0, 0, // NaN
    op(.push_i8),    5,
    op(.neq), // true
    op(.if_true),
    3,
    0,
    op(.push_i8),
    7,
    op(.ret),
    op(.push_i8),
    9,
    op(.ret),
};

// --- Property-access cases (write then read back the same slot) ---
// new_object; dup; push_i8 42; put_field .length; get_field .length; ret.
// put_field pops [val, obj] (val on top) and pushes nothing, so the dup keeps a
// second obj for get_field; reading back must yield the stored 42 on every tier.
// Atom.length = 4 -> little-endian operand bytes 4,0. All four property opcodes
// are baseline-emitted (none hit the UnsupportedOpcode else arm), so the
// baseline_compiled invariant holds; loopless bodies skip the optimized tier
// exactly like the arithmetic corpus. This is the positive control for the
// hidden-class store path the write-barrier work touched on every tier.
const get_put_field_code = [_]u8{
    op(.new_object), // [obj]
    op(.dup), // [obj, obj]
    op(.push_i8), 42, // [obj, obj, 42]
    op(.put_field), 4, 0, // obj.length = 42; [obj]
    op(.get_field), 4, 0, // [obj.length]
    op(.ret), // returns 42
};
// Same semantics through the inline-cache opcodes. The +u16 cache_idx (0,0)
// indexes Interpreter.pic_cache[512]; the first run misses then self-populates,
// so the read-back parity also pins miss-then-hit IC agreement across tiers.
const get_put_field_ic_code = [_]u8{
    op(.new_object), // [obj]
    op(.dup), // [obj, obj]
    op(.push_i8), 42, // [obj, obj, 42]
    op(.put_field_ic), 4, 0, 0, 0, // obj.length = 42; atom=4 cache=0; [obj]
    op(.get_field_ic), 4, 0, 0, 0, // [obj.length]; atom=4 cache=0
    op(.ret), // returns 42
};

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
    .{ .name = "to_int32_big", .code = &to_int32_big_code, .constants = &to_int32_big_consts, .kind = .number, .expected_num = 0 },
    .{ .name = "mod_float", .code = &mod_float_code, .constants = &mod_float_consts, .kind = .number, .expected_num = 1.5 },
    .{ .name = "mod_zero", .code = &mod_zero_code, .kind = .number, .expect_nan = true },
    .{ .name = "nan_lt", .code = &nan_lt_code, .constants = &nan_lt_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "ushr_neg", .code = &ushr_neg_code, .kind = .number, .expected_num = 4294967295 },
    .{ .name = "neg_zero_const", .code = &neg_zero_const_code, .constants = &neg_zero_consts, .kind = .number, .expect_neg_zero = true },
    .{ .name = "div_one_by_neg_zero", .code = &div_neg_zero_code, .constants = &neg_zero_consts, .kind = .number, .expected_num = -std.math.inf(f64) },
    .{ .name = "div_one_by_neg_inf", .code = &div_neg_inf_code, .constants = &div_neg_inf_consts, .kind = .number, .expect_neg_zero = true },
    .{ .name = "mul_neg_zero", .code = &mul_neg_zero_code, .constants = &mul_neg_zero_consts, .kind = .number, .expect_neg_zero = true },
    .{ .name = "neg_zero_strict_eq_zero", .code = &neg_zero_strict_eq_code, .constants = &neg_zero_strict_eq_consts, .kind = .boolean, .expected_bool = true },
    .{ .name = "neg_even_int", .code = &neg_even_int_code, .kind = .number, .expected_num = -4 },
    .{ .name = "neg_odd_int", .code = &neg_odd_int_code, .kind = .number, .expected_num = -3 },
    .{ .name = "neg_negative_int", .code = &neg_negative_int_code, .kind = .number, .expected_num = 4 },
    .{ .name = "neg_int_zero", .code = &neg_int_zero_code, .kind = .number, .expected_num = 0 },
    .{ .name = "neg_int_min", .code = &neg_int_min_code, .constants = &neg_int_min_consts, .kind = .number, .expected_num = 2147483648 },
    .{ .name = "neg_float", .code = &neg_float_code, .constants = &neg_float_consts, .kind = .number, .expected_num = -2.5 },
    .{ .name = "neg_float_zero", .code = &neg_float_zero_code, .constants = &neg_float_zero_consts, .kind = .number, .expect_neg_zero = true },
    .{ .name = "inc_even_int", .code = &inc_even_int_code, .kind = .number, .expected_num = 5 },
    .{ .name = "inc_odd_int", .code = &inc_odd_int_code, .kind = .number, .expected_num = 4 },
    .{ .name = "dec_even_int", .code = &dec_even_int_code, .kind = .number, .expected_num = 3 },
    .{ .name = "inc_int_max", .code = &inc_int_max_code, .constants = &inc_int_max_consts, .kind = .number, .expected_num = 2147483648 },
    .{ .name = "dec_int_min", .code = &dec_int_min_code, .constants = &dec_int_min_consts, .kind = .number, .expected_num = -2147483649 },
    .{ .name = "inc_float", .code = &inc_float_code, .constants = &inc_float_consts, .kind = .number, .expected_num = 3.5 },
    .{ .name = "lt_nan_rhs", .code = &lt_nan_rhs_code, .constants = &nan_lt_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "nan_gt", .code = &nan_gt_code, .constants = &nan_lt_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "nan_lt_float", .code = &nan_lt_float_code, .constants = &nan_float_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "nan_eq_float", .code = &nan_eq_float_code, .constants = &nan_float_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "float_gte_nan", .code = &float_gte_nan_code, .constants = &nan_float_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "nan_strict_eq_self", .code = &nan_strict_eq_self_code, .constants = &nan_lt_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "nan_eq_self", .code = &nan_eq_self_code, .constants = &nan_lt_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "nan_strict_eq_diff", .code = &nan_strict_eq_diff_code, .constants = &nan_diff_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "nan_eq_diff", .code = &nan_eq_diff_code, .constants = &nan_diff_consts, .kind = .boolean, .expected_bool = false },
    .{ .name = "nan_branch_not_taken", .code = &nan_branch_code, .constants = &nan_lt_consts, .kind = .number, .expected_num = 7 },
    .{ .name = "nan_neq_branch_taken", .code = &nan_neq_branch_code, .constants = &nan_lt_consts, .kind = .number, .expected_num = 9 },
    .{ .name = "get_put_field", .code = &get_put_field_code, .kind = .number, .expected_num = 42 },
    .{ .name = "get_put_field_ic", .code = &get_put_field_ic_code, .kind = .number, .expected_num = 42 },
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
/// result differently per tier, so fall back to numeric comparison. Zeros compare
/// by sign bit (SameValue, not ==): a tier returning +0 where another returns -0
/// is a real divergence that `an == bn` would wave through.
fn sameValue(a: JSValue, b: JSValue) bool {
    if (a.raw == b.raw) return true;
    const an = a.toNumber() orelse return false;
    const bn = b.toNumber() orelse return false;
    if (std.math.isNan(an) and std.math.isNan(bn)) return true;
    if (an == 0 and bn == 0) return std.math.signbit(an) == std.math.signbit(bn);
    return an == bn;
}

fn checkExpected(case: Case, result: JSValue) !void {
    try std.testing.expect(!result.isUndefined());
    switch (case.kind) {
        .number => {
            const n = result.toNumber() orelse return error.ExpectedNumber;
            if (case.expect_nan) {
                try std.testing.expect(std.math.isNan(n));
            } else if (case.expect_neg_zero) {
                // expectEqual cannot see the sign of zero (-0.0 == 0.0).
                try std.testing.expectEqual(@as(f64, 0), n);
                try std.testing.expect(std.math.signbit(n));
            } else {
                try std.testing.expectEqual(case.expected_num, n);
            }
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

    // The property cases run `new_object`, which in non-hybrid mode allocates
    // GC-managed objects from the raw allocator that this corpus never collects.
    // A request-scoped arena (the production allocation path) reclaims every such
    // object at deinit, so `testing.allocator` still flags genuine leaks in the
    // JIT/feedback machinery. Inline NaN-boxed floats from the arithmetic cases
    // need no heap, so the arena only catches the object allocations.
    var req_arena = try arena_mod.Arena.init(allocator, .{ .size = 8192 });
    defer req_arena.deinit();
    var hybrid = arena_mod.HybridAllocator{ .persistent = allocator, .arena = &req_arena };
    ctx.setHybridAllocator(&hybrid);

    var interp = Interpreter.init(ctx);

    // JIT may be disabled by environment or unavailable on the host arch. The
    // interpreter tier always runs (so the corpus is never vacuous); the JIT
    // tiers are gated, mirroring the existing JIT tests.
    jit_policy.setJitPolicy(.eager);
    const jit_available = !jit_policy.jitDisabled();

    var baseline_compiled: usize = 0;
    var optimized_reached: usize = 0;

    for (cases) |case| {
        errdefer std.debug.print("opcode parity: failing case '{s}'\n", .{case.name});
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

// `call` needs a callee FUNCTION value that only exists once `ctx`/the allocator
// do (createBytecodeFunction stores a heap pointer), so it cannot be a comptime
// `Case` literal like the corpus above. This second gate builds the callee at
// runtime and runs the same three-tier protocol, asserting the call returns 42
// identically on interpreter, baseline JIT, and (when reached) optimized JIT.
test "opcode parity: call returns identical value across tiers" {
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

    jit_policy.setJitPolicy(.eager);
    const jit_available = !jit_policy.jitDisabled();

    // Callee body: push_i8 42; ret. Heap-allocated because createBytecodeFunction
    // stores a pointer and destroyFull frees both the code and the FunctionBytecode.
    // The callee runs via doCall's interpreter path (its own body is never JIT'd),
    // so any opcodes are fair game here - only the OUTER body is tier-forced.
    const callee_code = try allocator.alloc(u8, 3);
    callee_code[0] = op(.push_i8);
    callee_code[1] = 42;
    callee_code[2] = op(.ret);
    const callee_func = try allocator.create(bytecode.FunctionBytecode);
    callee_func.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 4,
        .flags = .{},
        .code = callee_code,
        .constants = &.{},
        .source_map = null,
    };
    const callee_obj = try object.JSObject.createBytecodeFunction(allocator, ctx.root_class_idx, callee_func, .length);
    defer callee_obj.destroyFull(allocator); // frees callee_code + the FunctionBytecode

    // Outer body: push_const 0 (the callee object); call argc=0; ret. push_const,
    // call, and ret all baseline- AND optimized-compile (call routes through the
    // extern jitCall -> doCall, the same dispatch the interpreter uses). MUST NOT
    // use make_function/make_closure - those hit the UnsupportedOpcode else arm
    // and would leave the outer function interpreted, so the callee lives in
    // constants[0] instead (mirroring the existing call_ic parity test).
    const call_consts = [_]JSValue{callee_obj.toValue()};
    const call_code = [_]u8{ op(.push_const), 0, 0, op(.call), 0, op(.ret) };
    const call_case = Case{ .name = "call", .code = &call_code, .constants = &call_consts, .kind = .number, .expected_num = 42 };

    // Tier 1: interpreter. Pin thresholds so maybePromote never compiles.
    jit_policy.setJitThreshold(std.math.maxInt(u32));
    jit_policy.setJitFeedbackWarmup(std.math.maxInt(u32));
    var interp_func = buildFunc(call_case);
    const interp_result = try interp.run(&interp_func);
    try std.testing.expectEqual(bytecode.CompilationTier.interpreted, interp_func.tier);
    try checkExpected(call_case, interp_result);

    if (!jit_available) return;

    // Tier 2: baseline JIT. Eager + threshold 1 forces compilation quickly.
    jit_policy.setJitPolicy(.eager);
    jit_policy.setJitThreshold(1);
    jit_policy.setJitFeedbackWarmup(1);
    var base_func = buildFunc(call_case);
    defer jit_compile.cleanupCompiledCode(allocator, &base_func);
    defer jit_compile.cleanupTypeFeedback(allocator, &base_func);
    var base_result: JSValue = undefined;
    var i: usize = 0;
    while (i < 6) : (i += 1) base_result = try interp.run(&base_func);
    try std.testing.expect(base_func.compiled_code != null); // genuinely compiled, not silently interpreted
    try checkExpected(call_case, base_result);
    try std.testing.expect(sameValue(interp_result, base_result));

    // Tier 3: optimized JIT. Loopless, so it won't reach .optimized on most archs;
    // parity is asserted only when it does, exactly like the corpus above.
    jit_policy.setJitPolicy(.lazy);
    jit_policy.setJitThreshold(std.math.maxInt(u32));
    jit_policy.setJitFeedbackWarmup(std.math.maxInt(u32));
    var opt_func = buildFunc(call_case);
    defer jit_compile.cleanupCompiledCode(allocator, &opt_func);
    defer jit_compile.cleanupTypeFeedback(allocator, &opt_func);
    try jit_compile.allocateTypeFeedback(&interp, &opt_func);
    var w: usize = 0;
    while (w < 8) : (w += 1) _ = try interp.run(&opt_func);
    jit_compile.tryCompileOptimized(&interp, &opt_func) catch {};
    if (opt_func.tier == .optimized) {
        const opt_result = try interp.run(&opt_func);
        try checkExpected(call_case, opt_result);
        try std.testing.expect(sameValue(interp_result, opt_result));
    }
}

// String-valued results. The header's exclusion holds for RAW comparison: this
// harness must never toPtr/flatten a tier's string output. The dodge is doing
// the comparison INSIDE the VM - concatenate, then strict_eq against an
// expected constant - so every case still reduces to a boolean and sameValue
// stays number/bool-only. That observable is also the only meaningful one: the
// interpreter's add builds a rope while jitAdd builds a flat string, so the
// tiers legitimately return different representations of the same content and
// only in-VM content equality (which strictEquals defines across flat/rope/
// slice) can compare them. String constants need ctx (arena-backed
// allocation), so like `call` these cannot be comptime Case literals.
test "opcode parity: string-producing opcodes agree across tiers via in-VM comparison" {
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

    // Arena-backed hybrid allocation (the production path): every string this
    // corpus creates - the constants below and the concat results inside the
    // VM - is reclaimed at arena deinit, so testing.allocator still flags
    // genuine leaks in the JIT/feedback machinery.
    var req_arena = try arena_mod.Arena.init(allocator, .{ .size = 8192 });
    defer req_arena.deinit();
    var hybrid = arena_mod.HybridAllocator{ .persistent = allocator, .arena = &req_arena };
    ctx.setHybridAllocator(&hybrid);

    var interp = Interpreter.init(ctx);

    jit_policy.setJitPolicy(.eager);
    const jit_available = !jit_policy.jitDisabled();

    const s_foo = try ctx.createString("foo");
    const s_bar = try ctx.createString("bar");
    const s_foobar = try ctx.createString("foobar");
    const s_foobaz = try ctx.createString("foobaz");
    const s_foo_dup = try ctx.createString("foo");
    const s_v = try ctx.createString("v");
    const s_v5 = try ctx.createString("v5");

    // add on two strings concatenates; strict_eq then compares content.
    const concat_eq_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.add), op(.push_const), 2, 0, op(.strict_eq), op(.ret) };
    const concat_eq_consts = [_]JSValue{ s_foo, s_bar, s_foobar };
    // Negative control: the same concat against the wrong constant must read
    // false, guarding against a tier (or strict_eq itself) degenerating to
    // always-true.
    const concat_wrong_consts = [_]JSValue{ s_foo, s_bar, s_foobaz };
    // Content equality across two DISTINCT heap strings: pointer inequality
    // with byte equality, so the raw-bits fast path cannot answer this one.
    const content_eq_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.strict_eq), op(.ret) };
    const content_eq_consts = [_]JSValue{ s_foo, s_foo_dup };
    // Number-to-string coercion inside concat ("v" + 5 -> "v5") pins the
    // int-toString lane (small-int cache) across tiers.
    const num_concat_code = [_]u8{ op(.push_const), 0, 0, op(.push_i8), 5, op(.add), op(.push_const), 1, 0, op(.strict_eq), op(.ret) };
    const num_concat_consts = [_]JSValue{ s_v, s_v5 };
    // concat_n is the dedicated N-way concatenation opcode (template literals);
    // +u8 count operand.
    const concat_n_code = [_]u8{ op(.push_const), 0, 0, op(.push_const), 1, 0, op(.concat_n), 2, op(.push_const), 2, 0, op(.strict_eq), op(.ret) };

    const string_cases = [_]Case{
        .{ .name = "str_concat_eq", .code = &concat_eq_code, .constants = &concat_eq_consts, .kind = .boolean, .expected_bool = true },
        .{ .name = "str_concat_wrong", .code = &concat_eq_code, .constants = &concat_wrong_consts, .kind = .boolean, .expected_bool = false },
        .{ .name = "str_content_eq", .code = &content_eq_code, .constants = &content_eq_consts, .kind = .boolean, .expected_bool = true },
        .{ .name = "str_num_concat", .code = &num_concat_code, .constants = &num_concat_consts, .kind = .boolean, .expected_bool = true },
        .{ .name = "str_concat_n", .code = &concat_n_code, .constants = &concat_eq_consts, .kind = .boolean, .expected_bool = true },
    };

    var baseline_compiled: usize = 0;

    for (string_cases) |case| {
        errdefer std.debug.print("opcode parity: failing case '{s}'\n", .{case.name});
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

        // Tier 3: optimized JIT. Loopless bodies may not reach .optimized;
        // parity is asserted only when reached, exactly like the corpus above.
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
            const opt_result = try interp.run(&opt_func);
            try checkExpected(case, opt_result);
            try std.testing.expect(sameValue(interp_result, opt_result));
        }
    }

    if (jit_available) {
        // Every case must have genuinely exercised the baseline tier.
        try std.testing.expectEqual(string_cases.len, baseline_compiled);
    }
}

// Failure-path parity. The two fault representations DIFFER BY DESIGN: the
// interpreter returns an `error` from run(); the JIT does not poll ctx.exception
// in straight-line code, so it returns the `exception_val` sentinel (with
// ctx.exception set) as an ordinary JSValue and keeps going. A raw value compare
// across tiers is therefore structurally impossible - sameValue(exception_val, _)
// is false and the interpreter side is an `error`, not a value. The meaningful,
// tier-stable comparison is the fault VERDICT ("did this fault?"). Each case is
// the fault op immediately followed by `ret`, so the JIT's first post-fault value
// is exactly the sentinel and `isException() or hasException()` is a reliable
// normalizer; clear ctx.exception between runs so a sticky sentinel can't poison
// the next case's verdict.
const FaultCase = struct {
    name: []const u8,
    code: []const u8,
    constants: []const JSValue = &.{},
    expect_err: Interpreter.InterpreterError,
};

// add of two undefineds: addValuesSlow returns error.TypeError (interpreter) /
// jitAdd's toNumber() orelse jitThrow() returns the sentinel (JIT).
const fault_type_error_code = [_]u8{ op(.push_undefined), op(.push_undefined), op(.add), op(.ret) };
// call on a non-callable (the integer 1): doCall's !isCallable() returns
// error.NotCallable (interpreter) / jitCall's catch returns the sentinel (JIT).
const fault_not_callable_code = [_]u8{ op(.push_i8), 1, op(.call), 0, op(.ret) };

const fault_cases = [_]FaultCase{
    .{ .name = "add_type_error", .code = &fault_type_error_code, .expect_err = error.TypeError },
    .{ .name = "call_not_callable", .code = &fault_not_callable_code, .expect_err = error.NotCallable },
};

fn faultFunc(case: FaultCase) bytecode.FunctionBytecode {
    return .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 8,
        .flags = .{},
        .code = case.code,
        .constants = case.constants,
        .source_map = null,
    };
}

/// A JIT run faulted iff it returned the sentinel or left ctx.exception set.
fn jitFaulted(interp: *Interpreter, result: JSValue) bool {
    return result.isException() or interp.ctx.hasException();
}

test "opcode parity: failure paths fault identically across tiers" {
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

    jit_policy.setJitPolicy(.eager);
    const jit_available = !jit_policy.jitDisabled();

    for (fault_cases) |case| {
        // Tier 1: interpreter. The run() must return the pinned error tag.
        jit_policy.setJitPolicy(.eager);
        jit_policy.setJitThreshold(std.math.maxInt(u32));
        jit_policy.setJitFeedbackWarmup(std.math.maxInt(u32));
        interp.ctx.clearException();
        var interp_func = faultFunc(case);
        try std.testing.expectError(case.expect_err, interp.run(&interp_func));

        if (!jit_available) continue;

        // Tier 2: baseline JIT. Warm to force compilation, then assert the run
        // faults (returns the sentinel and/or sets ctx.exception). The
        // interpreter faulted, so baseline must too: faulted == faulted.
        jit_policy.setJitPolicy(.eager);
        jit_policy.setJitThreshold(1);
        jit_policy.setJitFeedbackWarmup(1);
        var base_func = faultFunc(case);
        defer jit_compile.cleanupCompiledCode(allocator, &base_func);
        defer jit_compile.cleanupTypeFeedback(allocator, &base_func);
        var base_faulted = false;
        var i: usize = 0;
        while (i < 6) : (i += 1) {
            interp.ctx.clearException();
            const r = interp.run(&base_func) catch {
                // Pre-compile warm runs fall back to the interpreter and may
                // return the error directly; that is still a fault.
                base_faulted = true;
                continue;
            };
            base_faulted = jitFaulted(&interp, r);
        }
        try std.testing.expect(base_func.compiled_code != null); // genuinely compiled
        try std.testing.expect(base_faulted); // interpreter faulted; baseline must agree
        interp.ctx.clearException();
    }
}

// Post-fault continuation. The body faults at `add` (undefined + undefined ->
// TypeError) and is FOLLOWED by `push_i8 99; ret`. The interpreter aborts at the
// fault: run() returns the error and the trailing opcodes never execute. A tier
// that keeps running past the fault pushes 99 over the sentinel and `ret` returns
// 99, so run() yields an ORDINARY value (not even the sentinel) for a program that
// faulted. The tier-stable contract is "a fault makes run() return an error"; a
// raw `cc.execute` boundary that never polls ctx.exception breaks it by handing
// back a post-fault value instead. (`add` of undefineds is a guaranteed sentinel
// path: undefined operands never earn .smi feedback, so the baseline emits the
// general emitBinaryOp -> jitAdd helper call, not a deopt-to-interpreter.)
const fault_then_continue_code = [_]u8{
    op(.push_undefined), op(.push_undefined), op(.add), // faults: TypeError
    op(.push_i8), 99, // executes only if the tier ran past the fault
    op(.ret),
};

/// run() that returned a Zig error faulted; run() that returned ANY value did not
/// (even the exception_val sentinel counts as "did not return an error").
fn runReturnedError(interp: *Interpreter, func: *bytecode.FunctionBytecode) bool {
    if (interp.run(func)) |_| {
        return false;
    } else |_| {
        return true;
    }
}

test "opcode parity: a fault makes run() return an error on every tier, not a post-fault value" {
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

    jit_policy.setJitPolicy(.eager);
    const jit_available = !jit_policy.jitDisabled();

    const case = FaultCase{ .name = "fault_then_continue", .code = &fault_then_continue_code, .expect_err = error.TypeError };

    // Tier 1: interpreter aborts at the fault; run() returns the error.
    jit_policy.setJitThreshold(std.math.maxInt(u32));
    jit_policy.setJitFeedbackWarmup(std.math.maxInt(u32));
    interp.ctx.clearException();
    var interp_func = faultFunc(case);
    try std.testing.expectError(error.TypeError, interp.run(&interp_func));

    if (!jit_available) return;

    // Tier 2: baseline JIT. Warm to force compilation, then the compiled run must
    // ALSO surface the fault as a run() error - not return 99 from the opcode that
    // executed past the fault point.
    jit_policy.setJitPolicy(.eager);
    jit_policy.setJitThreshold(1);
    jit_policy.setJitFeedbackWarmup(1);
    var base_func = faultFunc(case);
    defer jit_compile.cleanupCompiledCode(allocator, &base_func);
    defer jit_compile.cleanupTypeFeedback(allocator, &base_func);
    var i: usize = 0;
    while (i < 6) : (i += 1) {
        interp.ctx.clearException();
        _ = interp.run(&base_func) catch {};
    }
    try std.testing.expect(base_func.compiled_code != null); // genuinely compiled
    interp.ctx.clearException();
    try std.testing.expect(runReturnedError(&interp, &base_func));
    interp.ctx.clearException();

    // Tier 3: optimized JIT, when reached. Same boundary contract.
    jit_policy.setJitPolicy(.lazy);
    jit_policy.setJitThreshold(std.math.maxInt(u32));
    jit_policy.setJitFeedbackWarmup(std.math.maxInt(u32));
    var opt_func = faultFunc(case);
    defer jit_compile.cleanupCompiledCode(allocator, &opt_func);
    defer jit_compile.cleanupTypeFeedback(allocator, &opt_func);
    try jit_compile.allocateTypeFeedback(&interp, &opt_func);
    var w: usize = 0;
    while (w < 8) : (w += 1) {
        interp.ctx.clearException();
        _ = interp.run(&opt_func) catch {};
    }
    jit_compile.tryCompileOptimized(&interp, &opt_func) catch {};
    if (opt_func.tier == .optimized) {
        interp.ctx.clearException();
        try std.testing.expect(runReturnedError(&interp, &opt_func));
    }
    interp.ctx.clearException();
}

// A JIT fault must consume its own exception side channel. run() is the top frame
// (no pushState/popState to restore ctx.exception), so if the boundary leaves the
// sentinel set after converting it to a Zig error, the NEXT compiled run() on the
// same Context sees the stale exception via hasException() and spuriously errors a
// clean function. Run a faulting compiled function (leaving ctx.exception exactly
// as the boundary left it - NO manual clear), then a clean compiled function: the
// clean one must return its value, not inherit the prior fault.
const clean_ret_42_code = [_]u8{ op(.push_i8), 42, op(.ret) };

test "opcode parity: a JIT fault does not poison the next run() on the same context" {
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

    jit_policy.setJitPolicy(.eager);
    if (jit_policy.jitDisabled()) return;

    jit_policy.setJitThreshold(1);
    jit_policy.setJitFeedbackWarmup(1);

    var fault_func = faultFunc(.{ .name = "fault", .code = &fault_then_continue_code, .expect_err = error.TypeError });
    defer jit_compile.cleanupCompiledCode(allocator, &fault_func);
    defer jit_compile.cleanupTypeFeedback(allocator, &fault_func);
    const clean_case = Case{ .name = "clean", .code = &clean_ret_42_code, .kind = .number, .expected_num = 42 };
    var clean_func = buildFunc(clean_case);
    defer jit_compile.cleanupCompiledCode(allocator, &clean_func);
    defer jit_compile.cleanupTypeFeedback(allocator, &clean_func);

    // Warm both to baseline (clear the fault between warm runs so warmup proceeds).
    var i: usize = 0;
    while (i < 6) : (i += 1) {
        interp.ctx.clearException();
        _ = interp.run(&fault_func) catch {};
        interp.ctx.clearException();
        _ = interp.run(&clean_func) catch {};
    }
    try std.testing.expect(fault_func.compiled_code != null);
    try std.testing.expect(clean_func.compiled_code != null);

    // Clean slate, then fault and DO NOT clear afterward.
    interp.ctx.clearException();
    try std.testing.expect(runReturnedError(&interp, &fault_func));

    // The boundary must have consumed the exception: a clean compiled run now
    // returns 42, not a spurious error inherited from the fault above.
    const result = try interp.run(&clean_func);
    try checkExpected(clean_case, result);
}

// Optimized-tier coverage note. After the executeCompiled extraction there is ONE
// compiled-execution boundary shared by `run`, `callBytecodeFunction`, AND both the
// baseline and optimized tiers - the tier changes only how the BODY compiles, never
// how a fault is reconciled (the optimized body faults through the same
// Context.jitAdd / jitCall helpers as baseline) - so the baseline fault assertions
// above are the guarantee for the optimized tier too. The "a fault makes run()
// return an error on every tier" test above already asserts the optimized
// fault->error path opportunistically (its `if (opt_func.tier == .optimized)`
// block), though a loopless body never reaches .optimized on the dev host. A
// dedicated forced-.optimized fault test is omitted because it needs a hot SMI loop
// and the only post-loop faulting opcodes either are unsupported at the optimized
// tier (push_undefined) or bail the optimized compiler (an always-faulting `call` ->
// error.OutOfMemory on that synthetic shape, which real codegen never emits).

// Post-fault SIDE EFFECTS in the innermost compiled frame. The boundary
// (executeCompiled) converts a pending exception into a run() error, so the
// return-value contract holds even when compiled code runs past a fault - but
// any STORE the tail opcodes perform is an observable divergence: the
// interpreter aborts at the fault and never executes it. The body calls a
// non-callable (faults NotCallable via the jitCall sentinel), then stores 99
// to a global. On every tier the global must stay undefined after a faulting
// run: compiled call sites must bail to the fault exit on the sentinel
// instead of executing the tail opcodes.
test "opcode parity: compiled code performs no side effects past a faulted call" {
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

    jit_policy.setJitPolicy(.eager);
    if (jit_policy.jitDisabled()) return;

    const leak_atom = try ctx.atoms.intern("__parity_fault_leak");
    const atom_idx: u16 = @intCast(@intFromEnum(leak_atom));

    const code = [_]u8{
        op(.push_i8), 1, // non-callable callee
        op(.call), 0, // faults: NotCallable
        op(.drop), // tail opcodes the interpreter never reaches
        op(.push_i8),
        99,
        op(.put_global),
        @intCast(atom_idx & 0xFF),
        @intCast(atom_idx >> 8),
        op(.ret_undefined),
    };

    // Interpreter: aborts at the fault; the store never runs.
    jit_policy.setJitThreshold(std.math.maxInt(u32));
    jit_policy.setJitFeedbackWarmup(std.math.maxInt(u32));
    interp.ctx.clearException();
    var interp_func = faultFunc(.{ .name = "fault_store", .code = &code, .expect_err = error.NotCallable });
    try std.testing.expectError(error.NotCallable, interp.run(&interp_func));
    try std.testing.expect(ctx.getGlobal(leak_atom) == null);

    // Baseline: warm to compiled, reset the global, then a compiled faulting
    // run must leave the global unset - the tail store must not execute.
    jit_policy.setJitThreshold(1);
    jit_policy.setJitFeedbackWarmup(1);
    var base_func = faultFunc(.{ .name = "fault_store", .code = &code, .expect_err = error.NotCallable });
    defer jit_compile.cleanupCompiledCode(allocator, &base_func);
    defer jit_compile.cleanupTypeFeedback(allocator, &base_func);
    var i: usize = 0;
    while (i < 6) : (i += 1) {
        interp.ctx.clearException();
        _ = interp.run(&base_func) catch {};
    }
    try std.testing.expect(base_func.compiled_code != null); // genuinely compiled
    try ctx.setGlobal(leak_atom, JSValue.undefined_val);
    interp.ctx.clearException();
    _ = interp.run(&base_func) catch {};
    interp.ctx.clearException();
    const leaked = ctx.getGlobal(leak_atom) orelse JSValue.undefined_val;
    try std.testing.expect(leaked.isUndefined());
}

// Post-fault tail CALLS in the innermost compiled frame. Store helpers refuse
// writes while a fault is pending (Context.jitPutGlobal, jitPutFieldIC), but a
// post-fault tail CALL would execute an entire callee - interpreted, with all
// of its side effects unguarded - that the interpreter tier never reaches. The
// jitCall/jitCallBytecode/jitCallBytecodeFast entry guards refuse the call on a
// pending fault. Body: fault at `add` (undefined + undefined, a guaranteed
// sentinel path), then call a callee that stores 77 to a global. The global
// must stay undefined on every tier.
test "opcode parity: compiled code does not invoke callees past a fault" {
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

    jit_policy.setJitPolicy(.eager);
    if (jit_policy.jitDisabled()) return;

    const leak_atom = try ctx.atoms.intern("__parity_fault_callee_leak");
    const atom_idx: u16 = @intCast(@intFromEnum(leak_atom));

    // Callee: call_spread (interpreter no-op pushing undefined, but REJECTED by
    // the baseline compiler) pins the callee to the interpreter tier even at
    // threshold 1 - a compiled callee would route its store through the already
    // guarded Context.jitPutGlobal and mask the call-entry hole. Then: drop;
    // push_i8 77; put_global <atom>; ret_undefined. The interpreted put_global
    // has no pending-fault guard - exactly the side effect the call-entry
    // guards must prevent. Heap-allocated because createBytecodeFunction stores
    // a pointer and destroyFull frees both the code and the FunctionBytecode.
    const callee_code = try allocator.alloc(u8, 8);
    callee_code[0] = op(.call_spread);
    callee_code[1] = op(.drop);
    callee_code[2] = op(.push_i8);
    callee_code[3] = 77;
    callee_code[4] = op(.put_global);
    callee_code[5] = @intCast(atom_idx & 0xFF);
    callee_code[6] = @intCast(atom_idx >> 8);
    callee_code[7] = op(.ret_undefined);
    const callee_func = try allocator.create(bytecode.FunctionBytecode);
    callee_func.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 4,
        .flags = .{},
        .code = callee_code,
        .constants = &.{},
        .source_map = null,
    };
    const callee_obj = try object.JSObject.createBytecodeFunction(allocator, ctx.root_class_idx, callee_func, .length);
    defer callee_obj.destroyFull(allocator); // frees callee_code + the FunctionBytecode

    const consts = [_]JSValue{callee_obj.toValue()};
    const code = [_]u8{
        op(.push_undefined), op(.push_undefined), op(.add), // faults: TypeError
        op(.drop), // tail opcodes the interpreter never reaches
        op(.push_const),
        0,
        0,
        op(.call),
        0,
        op(.drop),
        op(.ret_undefined),
    };

    // Interpreter: aborts at the fault; the callee never runs.
    jit_policy.setJitThreshold(std.math.maxInt(u32));
    jit_policy.setJitFeedbackWarmup(std.math.maxInt(u32));
    interp.ctx.clearException();
    var interp_func = faultFunc(.{
        .name = "fault_callee",
        .code = &code,
        .constants = &consts,
        .expect_err = error.TypeError,
    });
    try std.testing.expectError(error.TypeError, interp.run(&interp_func));
    try std.testing.expect(ctx.getGlobal(leak_atom) == null);

    // Baseline: warm to compiled, reset the global, then a compiled faulting
    // run must not invoke the callee - the global stays undefined.
    jit_policy.setJitThreshold(1);
    jit_policy.setJitFeedbackWarmup(1);
    var base_func = faultFunc(.{
        .name = "fault_callee",
        .code = &code,
        .constants = &consts,
        .expect_err = error.TypeError,
    });
    defer jit_compile.cleanupCompiledCode(allocator, &base_func);
    defer jit_compile.cleanupTypeFeedback(allocator, &base_func);
    var i: usize = 0;
    while (i < 6) : (i += 1) {
        interp.ctx.clearException();
        _ = interp.run(&base_func) catch {};
    }
    try std.testing.expect(base_func.compiled_code != null); // genuinely compiled
    try ctx.setGlobal(leak_atom, JSValue.undefined_val);
    interp.ctx.clearException();
    _ = interp.run(&base_func) catch {};
    interp.ctx.clearException();
    const leaked = ctx.getGlobal(leak_atom) orelse JSValue.undefined_val;
    try std.testing.expect(leaked.isUndefined());
}
