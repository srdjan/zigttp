//! Bytecode Verifier
//!
//! Validates bytecode before execution to reject invalid or malformed bytecode.
//! Performs five checks:
//! 1. Opcode validity: every byte at an instruction boundary is a recognized opcode
//! 2. Operand bounds: jump targets are within the bytecode range and land on
//!    instruction boundaries
//! 3. Constant pool indices: push_const, make_function, etc. reference valid entries
//! 4. Stack discipline: stack height never goes negative and stays within limits
//! 5. Structural constraints: local indices within local_count, upvalue indices within
//!    upvalue_count, function call arity sanity

const std = @import("std");
const bytecode = @import("bytecode.zig");
const value = @import("value.zig");
const ic = @import("interpreter/ic.zig");

const Opcode = bytecode.Opcode;
const FunctionBytecode = bytecode.FunctionBytecode;

pub const VerifyError = error{
    InvalidOpcode,
    TruncatedOperand,
    JumpOutOfBounds,
    JumpNotOnBoundary,
    ConstantIndexOutOfBounds,
    StackUnderflow,
    StackOverflow,
    LocalIndexOutOfBounds,
    UpvalueIndexOutOfBounds,
    CacheIndexOutOfBounds,
    EmptyBytecode,
};

pub const VerifyResult = struct {
    valid: bool,
    err: ?VerifyError = null,
    offset: usize = 0, // bytecode offset where the error was found
    message: []const u8 = "",
};

/// Maximum stack depth we consider sane
const MAX_STACK_DEPTH: i32 = 4096;
const UNREACHED_STACK_HEIGHT: i32 = std.math.minInt(i32);

const StackEffect = struct {
    pop: i32,
    push: i32,
    // Minimum stack height the op requires before it executes. Equals `pop`
    // for ordinary opcodes, but is higher for opcodes that PEEK slots they do
    // not pop (dup/dup2/for_of_next), so the underflow check catches an
    // OOB stack read that `pop` alone would miss.
    min_height: i32,
};

/// Verify a FunctionBytecode is well-formed. Returns a VerifyResult indicating
/// whether the bytecode is valid and, if not, where the first error occurs.
pub fn verify(func: *const FunctionBytecode) VerifyResult {
    const code = func.code;
    if (code.len == 0) {
        return .{ .valid = false, .err = VerifyError.EmptyBytecode, .message = "bytecode is empty" };
    }

    const constants_len: u16 = @intCast(@min(func.constants.len, std.math.maxInt(u16)));

    // Phase 1+2: scan all instructions and validate opcodes/operand sizes.
    // Jump boundary checks later rescan instruction sizes instead of using a
    // fixed-size side table, so large bytecode gets the same validation as
    // small bytecode.

    var pc: usize = 0;
    while (pc < code.len) {
        const raw_op = code[pc];

        // Check opcode validity
        const op: Opcode = @enumFromInt(raw_op);
        const info = bytecode.getOpcodeInfo(op);

        // Unknown opcode (caught by the _ arm returning "unknown" name)
        if (std.mem.eql(u8, info.name, "unknown")) {
            return .{
                .valid = false,
                .err = VerifyError.InvalidOpcode,
                .offset = pc,
                .message = "unrecognized opcode",
            };
        }

        // Check operand bytes are available
        if (pc + info.size > code.len) {
            return .{
                .valid = false,
                .err = VerifyError.TruncatedOperand,
                .offset = pc,
                .message = "instruction extends past end of bytecode",
            };
        }

        // Phase 3: validate constant pool indices for relevant opcodes
        switch (op) {
            .push_const, .make_function, .make_async => {
                const idx = readU16(code, pc + 1);
                if (idx >= constants_len) {
                    return .{
                        .valid = false,
                        .err = VerifyError.ConstantIndexOutOfBounds,
                        .offset = pc,
                        .message = "constant pool index out of bounds",
                    };
                }
            },
            .push_const_call => {
                const idx = readU16(code, pc + 1);
                if (idx >= constants_len) {
                    return .{
                        .valid = false,
                        .err = VerifyError.ConstantIndexOutOfBounds,
                        .offset = pc,
                        .message = "constant pool index out of bounds in push_const_call",
                    };
                }
            },
            .make_closure => {
                const func_idx = readU16(code, pc + 1);
                if (func_idx >= constants_len) {
                    return .{
                        .valid = false,
                        .err = VerifyError.ConstantIndexOutOfBounds,
                        .offset = pc,
                        .message = "function index out of bounds in make_closure",
                    };
                }
                const upvalue_count = code[pc + 3];
                const child_value = func.constants[func_idx];
                if (!child_value.isExternPtr()) {
                    return .{
                        .valid = false,
                        .err = VerifyError.ConstantIndexOutOfBounds,
                        .offset = pc,
                        .message = "make_closure constant is not function bytecode",
                    };
                }
                const child = child_value.toExternPtr(FunctionBytecode);
                if (child.header.magic != bytecode.MAGIC) {
                    return .{
                        .valid = false,
                        .err = VerifyError.ConstantIndexOutOfBounds,
                        .offset = pc,
                        .message = "make_closure constant is not function bytecode",
                    };
                }
                if (upvalue_count != child.upvalue_count or upvalue_count != child.upvalue_info.len) {
                    return .{
                        .valid = false,
                        .err = VerifyError.UpvalueIndexOutOfBounds,
                        .offset = pc,
                        .message = "make_closure upvalue count does not match child metadata",
                    };
                }
                for (child.upvalue_info) |upvalue| {
                    if (upvalue.is_local and upvalue.index >= func.local_count) {
                        return .{
                            .valid = false,
                            .err = VerifyError.LocalIndexOutOfBounds,
                            .offset = pc,
                            .message = "closure local capture index out of bounds",
                        };
                    }
                    if (!upvalue.is_local and upvalue.index >= func.upvalue_count) {
                        return .{
                            .valid = false,
                            .err = VerifyError.UpvalueIndexOutOfBounds,
                            .offset = pc,
                            .message = "closure upvalue capture index out of bounds",
                        };
                    }
                }
            },
            // Inline-cache index validation. get_field_ic/put_field_ic index
            // Interpreter.pic_cache[cache_idx] directly (and the JIT intrinsics
            // do too), so an out-of-range index is a hard OOB read/write. The
            // compiler never emits one, but corrupted/crafted bytecode could.
            // Layout: opcode + u16 atom + u16 cache_idx, so cache_idx is at pc+3.
            .get_field_ic, .put_field_ic => {
                const cache_idx = readU16(code, pc + 3);
                if (cache_idx >= ic.IC_CACHE_SIZE) {
                    return .{
                        .valid = false,
                        .err = VerifyError.CacheIndexOutOfBounds,
                        .offset = pc,
                        .message = "inline-cache index out of bounds",
                    };
                }
            },
            // Local variable index validation (Phase 5)
            .get_loc, .put_loc => {
                const local_idx = code[pc + 1];
                if (local_idx >= func.local_count) {
                    return .{
                        .valid = false,
                        .err = VerifyError.LocalIndexOutOfBounds,
                        .offset = pc,
                        .message = "local variable index out of bounds",
                    };
                }
            },
            .get_loc_add => {
                const local_idx = code[pc + 1];
                if (local_idx >= func.local_count) {
                    return .{
                        .valid = false,
                        .err = VerifyError.LocalIndexOutOfBounds,
                        .offset = pc,
                        .message = "local variable index out of bounds in get_loc_add",
                    };
                }
            },
            .get_loc_get_loc_add => {
                const local_idx1 = code[pc + 1];
                const local_idx2 = code[pc + 2];
                if (local_idx1 >= func.local_count or local_idx2 >= func.local_count) {
                    return .{
                        .valid = false,
                        .err = VerifyError.LocalIndexOutOfBounds,
                        .offset = pc,
                        .message = "local variable index out of bounds in get_loc_get_loc_add",
                    };
                }
            },
            .for_of_next_put_loc => {
                const local_idx = code[pc + 1];
                if (local_idx >= func.local_count) {
                    return .{
                        .valid = false,
                        .err = VerifyError.LocalIndexOutOfBounds,
                        .offset = pc,
                        .message = "local variable index out of bounds in for_of_next_put_loc",
                    };
                }
            },
            // Upvalue index validation
            .get_upvalue, .put_upvalue => {
                const upval_idx = code[pc + 1];
                if (upval_idx >= func.upvalue_count) {
                    return .{
                        .valid = false,
                        .err = VerifyError.UpvalueIndexOutOfBounds,
                        .offset = pc,
                        .message = "upvalue index out of bounds",
                    };
                }
            },
            .close_upvalue => {
                const local_idx = code[pc + 1];
                if (local_idx >= func.local_count) {
                    return .{
                        .valid = false,
                        .err = VerifyError.LocalIndexOutOfBounds,
                        .offset = pc,
                        .message = "local index out of bounds in close_upvalue",
                    };
                }
            },
            .set_slot => {
                // slot_idx validation would need hidden class info, skip
            },
            else => {},
        }

        pc += info.size;
    }

    // Phase 2 continued: validate jump targets
    pc = 0;
    while (pc < code.len) {
        const op: Opcode = @enumFromInt(code[pc]);
        const info = bytecode.getOpcodeInfo(op);

        switch (op) {
            .goto, .if_true, .if_false, .if_false_goto, .drop_goto => {
                const offset = readI16(code, pc + 1);
                const target = @as(i64, @intCast(pc)) + @as(i64, info.size) + @as(i64, offset);
                if (target < 0 or target >= code.len) {
                    return .{
                        .valid = false,
                        .err = VerifyError.JumpOutOfBounds,
                        .offset = pc,
                        .message = "jump target out of bytecode range",
                    };
                }
                const target_usize: usize = @intCast(target);
                if (!isInstructionBoundary(code, target_usize)) {
                    return .{
                        .valid = false,
                        .err = VerifyError.JumpNotOnBoundary,
                        .offset = pc,
                        .message = "jump target is not on an instruction boundary",
                    };
                }
            },
            .loop => {
                const offset = readI16(code, pc + 1);
                const target = @as(i64, @intCast(pc)) + @as(i64, info.size) - @as(i64, offset);
                if (target < 0 or target >= code.len) {
                    return .{
                        .valid = false,
                        .err = VerifyError.JumpOutOfBounds,
                        .offset = pc,
                        .message = "loop target out of bytecode range",
                    };
                }
                const target_usize: usize = @intCast(target);
                if (!isInstructionBoundary(code, target_usize)) {
                    return .{
                        .valid = false,
                        .err = VerifyError.JumpNotOnBoundary,
                        .offset = pc,
                        .message = "loop target is not on an instruction boundary",
                    };
                }
            },
            .for_of_next => {
                const offset = readI16(code, pc + 1);
                const target = @as(i64, @intCast(pc)) + @as(i64, info.size) + @as(i64, offset);
                if (target < 0 or target >= code.len) {
                    return .{
                        .valid = false,
                        .err = VerifyError.JumpOutOfBounds,
                        .offset = pc,
                        .message = "for_of_next end offset out of range",
                    };
                }
                if (!isInstructionBoundary(code, @intCast(target))) {
                    return .{
                        .valid = false,
                        .err = VerifyError.JumpNotOnBoundary,
                        .offset = pc,
                        .message = "for_of_next end target is not on an instruction boundary",
                    };
                }
            },
            .for_of_next_put_loc => {
                const offset = readI16(code, pc + 2);
                const target = @as(i64, @intCast(pc)) + @as(i64, info.size) + @as(i64, offset);
                if (target < 0 or target >= code.len) {
                    return .{
                        .valid = false,
                        .err = VerifyError.JumpOutOfBounds,
                        .offset = pc,
                        .message = "for_of_next_put_loc end offset out of range",
                    };
                }
                if (!isInstructionBoundary(code, @intCast(target))) {
                    return .{
                        .valid = false,
                        .err = VerifyError.JumpNotOnBoundary,
                        .offset = pc,
                        .message = "for_of_next_put_loc end target is not on an instruction boundary",
                    };
                }
            },
            else => {},
        }

        pc += info.size;
    }

    // Phase 4: stack discipline verification over the reachable control-flow graph.
    const allocator = std.heap.smp_allocator;
    const stack_heights = allocator.alloc(i32, code.len) catch {
        return .{
            .valid = false,
            .err = VerifyError.StackOverflow,
            .message = "stack verifier metadata allocation failed",
        };
    };
    defer allocator.free(stack_heights);
    @memset(stack_heights, UNREACHED_STACK_HEIGHT);

    const worklist = allocator.alloc(usize, code.len) catch {
        return .{
            .valid = false,
            .err = VerifyError.StackOverflow,
            .message = "stack verifier worklist allocation failed",
        };
    };
    defer allocator.free(worklist);

    const queued = allocator.alloc(bool, code.len) catch {
        return .{
            .valid = false,
            .err = VerifyError.StackOverflow,
            .message = "stack verifier queue metadata allocation failed",
        };
    };
    defer allocator.free(queued);
    @memset(queued, false);

    stack_heights[0] = 0;
    worklist[0] = 0;
    queued[0] = true;
    var worklist_len: usize = 1;

    while (worklist_len > 0) {
        worklist_len -= 1;
        pc = worklist[worklist_len];
        queued[pc] = false;
        const stack_height = stack_heights[pc];
        const op: Opcode = @enumFromInt(code[pc]);
        const info = bytecode.getOpcodeInfo(op);

        const effect = stackEffect(code, pc, op, info);
        if (stack_height < effect.min_height) {
            return .{
                .valid = false,
                .err = VerifyError.StackUnderflow,
                .offset = pc,
                .message = "stack underflow",
            };
        }

        const after_pop = stack_height - effect.pop;
        const after_op = after_pop + effect.push;

        if (after_op > MAX_STACK_DEPTH) {
            return .{
                .valid = false,
                .err = VerifyError.StackOverflow,
                .offset = pc,
                .message = "stack height exceeds maximum",
            };
        }

        const next_pc = pc + info.size;
        const branch_target = switch (op) {
            .goto, .if_true, .if_false, .if_false_goto, .drop_goto, .for_of_next => @as(usize, @intCast(forwardTarget(pc, info.size, readI16(code, pc + 1)))),
            .loop => @as(usize, @intCast(loopTarget(pc, info.size, readI16(code, pc + 1)))),
            .for_of_next_put_loc => @as(usize, @intCast(forwardTarget(pc, info.size, readI16(code, pc + 2)))),
            else => null,
        };

        switch (op) {
            .halt, .ret, .ret_undefined => {},
            .goto, .loop, .drop_goto => {
                if (propagateStackHeight(code, stack_heights, queued, worklist, &worklist_len, branch_target.?, after_op, pc)) |failure| return failure;
            },
            .if_true, .if_false, .if_false_goto => {
                if (propagateStackHeight(code, stack_heights, queued, worklist, &worklist_len, next_pc, after_op, pc)) |failure| return failure;
                if (propagateStackHeight(code, stack_heights, queued, worklist, &worklist_len, branch_target.?, after_op, pc)) |failure| return failure;
            },
            .for_of_next => {
                if (propagateStackHeight(code, stack_heights, queued, worklist, &worklist_len, next_pc, after_op, pc)) |failure| return failure;
                if (propagateStackHeight(code, stack_heights, queued, worklist, &worklist_len, branch_target.?, after_pop, pc)) |failure| return failure;
            },
            .for_of_next_put_loc => {
                if (propagateStackHeight(code, stack_heights, queued, worklist, &worklist_len, next_pc, after_op, pc)) |failure| return failure;
                if (propagateStackHeight(code, stack_heights, queued, worklist, &worklist_len, branch_target.?, after_op, pc)) |failure| return failure;
            },
            else => {
                if (propagateStackHeight(code, stack_heights, queued, worklist, &worklist_len, next_pc, after_op, pc)) |failure| return failure;
            },
        }
    }

    return .{ .valid = true };
}

fn stackEffect(code: []const u8, pc: usize, op: Opcode, info: bytecode.OpcodeInfo) StackEffect {
    const pop: i32 = switch (op) {
        // call/tail_call: pop argc args + the callee = argc + 1.
        .call, .tail_call => @as(i32, code[pc + 1]) + 1,
        // call_method additionally pops the `this` receiver beneath the callee,
        // so it consumes argc args + callee + this = argc + 2 (interpreter
        // doCall(argc, is_method=true)). Modelling it as argc + 1 under-requires
        // the stack by one and accepts bytecode that underflows on the this pop.
        .call_method => @as(i32, code[pc + 1]) + 2,
        // call_ic: pop argc + 1
        .call_ic => @as(i32, code[pc + 1]) + 1,
        // concat_n: pop N values
        .concat_n => @as(i32, code[pc + 1]),
        // push_const_call fuses `push_const` of the final argument with `call`.
        // The opcode contributes one argument itself, so the pre-op stack only
        // contains the callee plus argc-1 already-pushed arguments.
        .push_const_call => @as(i32, code[pc + 3]),
        // get_field_call: pop argc (operand at pc+3)
        .get_field_call => @as(i32, code[pc + 3]) + 2,
        else => @as(i32, info.n_pop),
    };

    // Opcodes that PEEK slots without popping them carry n_pop == 0 yet still
    // read the operand stack, so the underflow check must require their read
    // floor, not the pop count. Without this a `dup` at height 0 or a
    // `for_of_next` at height < 2 passes verification and OOB-reads the stack
    // at runtime (Context.peek/peekAt are assert-only, elided in ReleaseFast).
    const read_floor: i32 = switch (op) {
        .dup => 1, // reads stack[sp-1]
        .dup2, .for_of_next, .for_of_next_put_loc => 2, // read stack[sp-1] and stack[sp-2]
        else => 0,
    };

    return .{
        .pop = pop,
        .push = @as(i32, info.n_push),
        .min_height = @max(pop, read_floor),
    };
}

fn forwardTarget(pc: usize, instruction_size: usize, offset: i16) i64 {
    return @as(i64, @intCast(pc)) + @as(i64, @intCast(instruction_size)) + @as(i64, offset);
}

fn loopTarget(pc: usize, instruction_size: usize, offset: i16) i64 {
    return @as(i64, @intCast(pc)) + @as(i64, @intCast(instruction_size)) - @as(i64, offset);
}

fn propagateStackHeight(
    code: []const u8,
    stack_heights: []i32,
    queued: []bool,
    worklist: []usize,
    worklist_len: *usize,
    target: usize,
    stack_height: i32,
    source_pc: usize,
) ?VerifyResult {
    if (target >= code.len) {
        return .{
            .valid = false,
            .err = VerifyError.JumpOutOfBounds,
            .offset = source_pc,
            .message = "control flow leaves bytecode",
        };
    }
    if (stack_height > MAX_STACK_DEPTH) {
        return .{
            .valid = false,
            .err = VerifyError.StackOverflow,
            .offset = source_pc,
            .message = "stack height exceeds maximum",
        };
    }

    const previous = stack_heights[target];
    if (previous == UNREACHED_STACK_HEIGHT or stack_height < previous) {
        stack_heights[target] = stack_height;
        if (!queued[target]) {
            worklist[worklist_len.*] = target;
            worklist_len.* += 1;
            queued[target] = true;
        }
    }

    return null;
}

fn isInstructionBoundary(code: []const u8, target: usize) bool {
    var pc: usize = 0;
    while (pc < code.len) {
        if (pc == target) return true;
        const info = bytecode.getOpcodeInfo(@enumFromInt(code[pc]));
        if (std.mem.eql(u8, info.name, "unknown") or pc + info.size > code.len) return false;
        pc += info.size;
    }
    return false;
}

// Helpers for reading operands

fn readU16(code: []const u8, offset: usize) u16 {
    if (offset + 2 > code.len) return 0;
    return @as(u16, code[offset]) | (@as(u16, code[offset + 1]) << 8);
}

fn readI16(code: []const u8, offset: usize) i16 {
    return @bitCast(readU16(code, offset));
}

// ============================================================================
// Tests
// ============================================================================

test "verify: empty bytecode rejected" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{},
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.EmptyBytecode, result.err.?);
}

test "verify: simple ret_undefined passes" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{@intFromEnum(Opcode.ret_undefined)},
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(result.valid);
}

test "verify: push_true + ret passes" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_true),
            @intFromEnum(Opcode.ret),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(result.valid);
}

test "verify: truncated operand rejected" {
    // push_const needs 3 bytes total (opcode + u16), but we only have 2
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_const),
            0x00, // missing second byte of u16
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.TruncatedOperand, result.err.?);
}

test "verify: constant index out of bounds rejected" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_const),
            0x05,                     0x00, // index 5, but constants is empty
            @intFromEnum(Opcode.ret),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.ConstantIndexOutOfBounds, result.err.?);
}

test "verify: make_closure rejects declared upvalues beyond child metadata" {
    const child_upvalues = [_]bytecode.UpvalueInfo{
        .{ .is_local = true, .index = 0 },
    };
    var child = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .upvalue_count = 1,
        .upvalue_info = &child_upvalues,
        .code = &.{@intFromEnum(Opcode.ret_undefined)},
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const constants = [_]value.JSValue{value.JSValue.fromExternPtr(&child)};
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 1,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.make_closure),
            0x00, 0x00, // child function constant index 0
            0x02, // declares 2 upvalues, child has 1 descriptor
            @intFromEnum(Opcode.drop),
            @intFromEnum(Opcode.ret_undefined),
        },
        .constants = &constants,
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.UpvalueIndexOutOfBounds, result.err.?);
}

test "verify: make_closure rejects child local descriptor outside parent locals" {
    const child_upvalues = [_]bytecode.UpvalueInfo{
        .{ .is_local = true, .index = 1 },
    };
    var child = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .upvalue_count = 1,
        .upvalue_info = &child_upvalues,
        .code = &.{@intFromEnum(Opcode.ret_undefined)},
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const constants = [_]value.JSValue{value.JSValue.fromExternPtr(&child)};
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 1,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.make_closure),
            0x00,                               0x00, // child function constant index 0
            0x01,                               @intFromEnum(Opcode.drop),
            @intFromEnum(Opcode.ret_undefined),
        },
        .constants = &constants,
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.LocalIndexOutOfBounds, result.err.?);
}

test "verify: make_closure rejects child upvalue descriptor outside parent upvalues" {
    const child_upvalues = [_]bytecode.UpvalueInfo{
        .{ .is_local = false, .index = 1 },
    };
    var child = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .upvalue_count = 1,
        .upvalue_info = &child_upvalues,
        .code = &.{@intFromEnum(Opcode.ret_undefined)},
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const constants = [_]value.JSValue{value.JSValue.fromExternPtr(&child)};
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .upvalue_count = 1,
        .code = &.{
            @intFromEnum(Opcode.make_closure),
            0x00,                               0x00, // child function constant index 0
            0x01,                               @intFromEnum(Opcode.drop),
            @intFromEnum(Opcode.ret_undefined),
        },
        .constants = &constants,
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.UpvalueIndexOutOfBounds, result.err.?);
}

test "verify: stack underflow rejected" {
    // ret pops 1, but stack is empty
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.ret), // pops value, but nothing was pushed
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.StackUnderflow, result.err.?);
}

test "verify: taken conditional branch underflow rejected" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_true), // offset 0
            @intFromEnum(Opcode.if_true), // offset 1
            0x01, 0x00, // target = offset 5
            @intFromEnum(Opcode.push_0), // offset 4, skipped by taken branch
            @intFromEnum(Opcode.ret), // offset 5
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.StackUnderflow, result.err.?);
}

test "verify: balanced forward conditional branch passes" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_true), // offset 0
            @intFromEnum(Opcode.if_true), // offset 1
            0x04, 0x00, // true path target = offset 8
            @intFromEnum(Opcode.push_0), // offset 4, false path value
            @intFromEnum(Opcode.goto), // offset 5
            0x01, 0x00, // target = offset 9
            @intFromEnum(Opcode.push_1), // offset 8, true path value
            @intFromEnum(Opcode.ret), // offset 9
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(result.valid);
}

test "verify: local index out of bounds rejected" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 2, // only locals 0 and 1
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.get_loc),
            5, // local index 5 is out of bounds
            @intFromEnum(Opcode.ret),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.LocalIndexOutOfBounds, result.err.?);
}

test "verify: inline-cache index out of bounds rejected" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.get_field_ic),
            0, 0, // atom index 0
            0x58,                               0x02, // cache_idx = 600, >= IC_CACHE_SIZE (512)
            @intFromEnum(Opcode.ret_undefined),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.CacheIndexOutOfBounds, result.err.?);
}

test "verify: valid local access passes" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 4,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.get_loc_0), // local 0, ok with local_count=4
            @intFromEnum(Opcode.ret),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(result.valid);
}

test "verify: jump within bounds passes" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_true), // offset 0
            @intFromEnum(Opcode.if_true), // offset 1
            0x01, 0x00, // jump +1 byte (past ret_undefined to second ret_undefined)
            @intFromEnum(Opcode.ret_undefined), // offset 4
            @intFromEnum(Opcode.ret_undefined), // offset 5 (jump target)
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(result.valid);
}

test "verify: jump to end of bytecode is rejected" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.goto),
            0x00, 0x00, // target = code.len
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.JumpOutOfBounds, result.err.?);
}

test "verify: conditional jump to end of bytecode is rejected" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_false),
            @intFromEnum(Opcode.if_false),
            0x00, 0x00, // target = code.len
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.JumpOutOfBounds, result.err.?);
}

test "verify: drop_goto jump to end of bytecode is rejected" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_0),
            @intFromEnum(Opcode.drop_goto),
            0x00, 0x00, // target = code.len
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.JumpOutOfBounds, result.err.?);
}

test "verify: jump target inside an operand is rejected" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.goto),
            0xff,                               0xff, // target = 2, inside this instruction
            @intFromEnum(Opcode.ret_undefined),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.JumpNotOnBoundary, result.err.?);
}

test "verify: large bytecode receives full jump validation" {
    const allocator = std.testing.allocator;
    const len = 65_537;
    const code = try allocator.alloc(u8, len);
    defer allocator.free(code);
    @memset(code, @intFromEnum(Opcode.nop));
    code[len - 3] = @intFromEnum(Opcode.goto);
    code[len - 2] = 0x00;
    code[len - 1] = 0x00;

    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.JumpOutOfBounds, result.err.?);
}

test "verify: push_const_call accounts for fused argument" {
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 1,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.get_loc_0),
            @intFromEnum(Opcode.push_const_call),
            0x00, 0x00, // const index 0
            0x01, // argc = 1
            @intFromEnum(Opcode.drop),
            @intFromEnum(Opcode.ret_undefined),
        },
        .constants = &.{value.JSValue.fromInt(1)},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(result.valid);
}

test "verify: call_method underflow on the this receiver rejected" {
    // call_method pops argc args + callee + the `this` receiver = argc + 2.
    // Two pushes leave height 2; a call_method with argc=1 needs 3, so the
    // `this` pop underflows. The verifier must require argc + 2, not argc + 1.
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_true), // offset 0 -> height 1
            @intFromEnum(Opcode.push_true), // offset 1 -> height 2
            @intFromEnum(Opcode.call_method), // offset 2, pops argc+2 = 3
            0x01, // argc = 1
            @intFromEnum(Opcode.ret),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.StackUnderflow, result.err.?);
}

test "verify: dup on an empty stack rejected" {
    // dup peeks stack[sp-1] without popping (n_pop == 0), so it requires a
    // non-empty stack. The verifier must enforce that read floor.
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.dup), // reads stack[sp-1] at height 0
            @intFromEnum(Opcode.ret),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.StackUnderflow, result.err.?);
}

test "verify: dup2 below its read floor rejected" {
    // dup2 peeks stack[sp-1] and stack[sp-2]; it needs height >= 2.
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_true), // height 1
            @intFromEnum(Opcode.dup2), // reads stack[sp-1] and stack[sp-2] at height 1
            @intFromEnum(Opcode.ret),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(!result.valid);
    try std.testing.expectEqual(VerifyError.StackUnderflow, result.err.?);
}

test "verify: valid dup passes" {
    // Read floor must not reject a dup with a value on the stack.
    const func = FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 256,
        .flags = .{},
        .code = &.{
            @intFromEnum(Opcode.push_true),
            @intFromEnum(Opcode.dup),
            @intFromEnum(Opcode.drop),
            @intFromEnum(Opcode.ret),
        },
        .constants = &.{},
        .source_map = null,
        .line_table = null,
    };
    const result = verify(&func);
    try std.testing.expect(result.valid);
}
