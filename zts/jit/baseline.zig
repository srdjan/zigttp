//! Baseline JIT Compiler
//!
//! Translates bytecode to native machine code without optimization.
//! Focuses on dispatch elimination: removes the interpreter loop overhead.
//!
//! Supports multiple architectures via comptime selection:
//! - x86-64: System V AMD64 ABI
//! - ARM64: Apple ARM64 ABI

const std = @import("std");
const builtin = @import("builtin");
const x86 = @import("x86.zig");
const arm64 = @import("arm64.zig");
const alloc = @import("alloc.zig");
const bytecode = @import("../bytecode.zig");
const value_mod = @import("../value.zig");
const context_mod = @import("../context.zig");

const CodeAllocator = alloc.CodeAllocator;
const CompiledCode = alloc.CompiledCode;
const Opcode = bytecode.Opcode;
const Context = context_mod.Context;

extern fn jitCall(ctx: *Context, argc: u8, is_method: u8) value_mod.JSValue;

// Context field offsets for JIT code generation
// These are computed at compile time using @offsetOf
const CTX_STACK_PTR_OFF: i32 = @intCast(@offsetOf(Context, "stack"));
const CTX_SP_OFF: i32 = @intCast(@offsetOf(Context, "sp"));
const CTX_FP_OFF: i32 = @intCast(@offsetOf(Context, "fp"));

// Architecture-specific types selected at compile time
pub const Arch = builtin.cpu.arch;
pub const is_x86_64 = Arch == .x86_64;
pub const is_aarch64 = Arch == .aarch64;

pub const Emitter = if (is_x86_64) x86.X86Emitter else if (is_aarch64) arm64.Arm64Emitter else @compileError("Unsupported architecture for JIT");
pub const Register = if (is_x86_64) x86.Register else if (is_aarch64) arm64.Register else void;
pub const Regs = if (is_x86_64) x86.Regs else if (is_aarch64) arm64.Regs else void;

/// Error types for compilation
pub const CompileError = error{
    UnsupportedOpcode,
    CodeTooLarge,
    AllocationFailed,
    OutOfMemory,
    OffsetTooLarge,
    UnalignedBranch,
};

/// Compiled function entry point signature
/// Takes context pointer, returns JS value (u64)
pub const CompiledFn = *const fn (ctx: *anyopaque) callconv(.c) u64;

/// Baseline compiler state
pub const BaselineCompiler = struct {
    emitter: Emitter,
    code_alloc: *CodeAllocator,
    func: *const bytecode.FunctionBytecode,

    /// Label offsets for forward jumps (bytecode offset -> native offset)
    labels: std.AutoHashMapUnmanaged(u32, u32),
    /// Pending forward jump patches (native offset -> bytecode target)
    pending_jumps: std.ArrayListUnmanaged(PendingJump),
    allocator: std.mem.Allocator,
    next_local_label: u32,

    const PendingJump = struct {
        native_offset: u32, // Offset in emitted code where the rel32/imm is
        bytecode_target: u32, // Target bytecode offset
        is_conditional: bool, // For ARM64: conditional vs unconditional branch
    };

    pub fn init(allocator: std.mem.Allocator, code_alloc: *CodeAllocator, func: *const bytecode.FunctionBytecode) BaselineCompiler {
        return .{
            .emitter = Emitter.init(allocator),
            .code_alloc = code_alloc,
            .func = func,
            .labels = .{},
            .pending_jumps = .{},
            .allocator = allocator,
            .next_local_label = 0,
        };
    }

    pub fn deinit(self: *BaselineCompiler) void {
        self.emitter.deinit();
        self.labels.deinit(self.allocator);
        self.pending_jumps.deinit(self.allocator);
    }

    /// Compile the function to native code
    pub fn compile(self: *BaselineCompiler) CompileError!CompiledCode {
        // Emit prologue
        try self.emitPrologue();

        // Compile each bytecode instruction
        var pc: u32 = 0;
        const code = self.func.code;

        while (pc < code.len) {
            // Record label for this bytecode offset
            self.labels.put(self.allocator, pc, @intCast(self.emitter.buffer.items.len)) catch return CompileError.OutOfMemory;

            const op: Opcode = @enumFromInt(code[pc]);
            pc += 1;

            pc = try self.compileOpcode(op, pc, code);
        }

        // Emit epilogue if we haven't returned yet
        try self.emitEpilogue();

        // Patch forward jumps
        try self.patchJumps();

        // Allocate executable memory and copy code
        const native_code = self.code_alloc.alloc(self.emitter.buffer.items.len) catch return CompileError.AllocationFailed;
        @memcpy(native_code, self.emitter.buffer.items);

        // Make executable
        self.code_alloc.makeAllExecutable() catch return CompileError.AllocationFailed;

        return CompiledCode.fromSlice(native_code);
    }

    fn emitPrologue(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            // x86-64 prologue: save callee-saved registers
            self.emitter.pushReg(.rbp) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rbp, .rsp) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.rbx) catch return CompileError.OutOfMemory;
            // Save context pointer (rdi) to rbx for later use
            self.emitter.movRegReg(.rbx, .rdi) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // ARM64 prologue: save frame pointer and link register
            self.emitter.stpPreIndex(.x29, .x30, .sp, -16) catch return CompileError.OutOfMemory;
            // mov x29, sp (via add immediate)
            self.emitter.addRegImm12(.x29, .sp, 0) catch return CompileError.OutOfMemory;
            // Save callee-saved registers we use
            self.emitter.stpPreIndex(.x19, .x20, .sp, -16) catch return CompileError.OutOfMemory;
            // Save context pointer (x0) to x19 for later use
            self.emitter.movRegReg(.x19, .x0) catch return CompileError.OutOfMemory;
        }
    }

    fn emitEpilogue(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            // x86-64 epilogue: restore callee-saved registers
            self.emitter.popReg(.rbx) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.rbp) catch return CompileError.OutOfMemory;
            self.emitter.ret() catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // ARM64 epilogue: restore registers and return
            self.emitter.ldpPostIndex(.x19, .x20, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ldpPostIndex(.x29, .x30, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ret() catch return CompileError.OutOfMemory;
        }
    }

    fn compileOpcode(self: *BaselineCompiler, op: Opcode, pc: u32, code: []const u8) CompileError!u32 {
        var new_pc = pc;

        switch (op) {
            .nop => {
                // No operation - emit nothing
            },

            .push_const => {
                const idx = readU16(code, new_pc);
                new_pc += 2;
                const val = self.func.constants[idx];
                try self.emitPushImm64(@bitCast(val));
            },

            .push_0 => {
                const val = value_mod.JSValue.fromInt(0);
                try self.emitPushImm64(@bitCast(val));
            },

            .push_1 => {
                const val = value_mod.JSValue.fromInt(1);
                try self.emitPushImm64(@bitCast(val));
            },

            .push_2 => {
                const val = value_mod.JSValue.fromInt(2);
                try self.emitPushImm64(@bitCast(val));
            },

            .push_3 => {
                const val = value_mod.JSValue.fromInt(3);
                try self.emitPushImm64(@bitCast(val));
            },

            .push_i8 => {
                const imm: i8 = @bitCast(code[new_pc]);
                new_pc += 1;
                const val = value_mod.JSValue.fromInt(@as(i32, imm));
                try self.emitPushImm64(@bitCast(val));
            },

            .push_null => {
                try self.emitPushImm64(@bitCast(value_mod.JSValue.null_val));
            },

            .push_undefined => {
                try self.emitPushImm64(@bitCast(value_mod.JSValue.undefined_val));
            },

            .push_true => {
                try self.emitPushImm64(@bitCast(value_mod.JSValue.true_val));
            },

            .push_false => {
                try self.emitPushImm64(@bitCast(value_mod.JSValue.false_val));
            },

            .dup => {
                try self.emitDup();
            },

            .dup2 => {
                try self.emitDup2();
            },

            .drop => {
                try self.emitDrop();
            },

            .swap => {
                try self.emitSwap();
            },

            .rot3 => {
                try self.emitRot3();
            },

            // Local variable access
            .get_loc => {
                const idx = code[new_pc];
                new_pc += 1;
                try self.emitGetLocal(idx);
            },

            .get_loc_0 => try self.emitGetLocal(0),
            .get_loc_1 => try self.emitGetLocal(1),
            .get_loc_2 => try self.emitGetLocal(2),
            .get_loc_3 => try self.emitGetLocal(3),

            .put_loc => {
                const idx = code[new_pc];
                new_pc += 1;
                try self.emitPutLocal(idx);
            },

            .put_loc_0 => try self.emitPutLocal(0),
            .put_loc_1 => try self.emitPutLocal(1),
            .put_loc_2 => try self.emitPutLocal(2),
            .put_loc_3 => try self.emitPutLocal(3),

            .add => {
                try self.emitBinaryOp(.add);
            },

            .sub => {
                try self.emitBinaryOp(.sub);
            },

            .mul => {
                try self.emitBinaryOp(.mul);
            },

            .div => {
                try self.emitDiv();
            },

            .mod => {
                try self.emitMod();
            },

            .pow => {
                try self.emitPow();
            },

            .neg => {
                try self.emitNeg();
            },

            .inc => {
                try self.emitIncDec(true);
            },

            .dec => {
                try self.emitIncDec(false);
            },

            .ret => {
                try self.emitPop(.ret);
                try self.emitEpilogue();
            },

            .ret_undefined => {
                try self.emitMovImm64(.ret, @bitCast(value_mod.JSValue.undefined_val));
                try self.emitEpilogue();
            },

            .get_loc_add => {
                const idx = code[new_pc];
                new_pc += 1;
                try self.emitGetLocal(idx);
                try self.emitSwap();
                try self.emitBinaryOp(.add);
            },

            .get_loc_get_loc_add => {
                const idx1 = code[new_pc];
                const idx2 = code[new_pc + 1];
                new_pc += 2;
                try self.emitGetLocal(idx1);
                try self.emitGetLocal(idx2);
                try self.emitBinaryOp(.add);
            },

            .if_false_goto => {
                const offset: i16 = @bitCast(readU16(code, new_pc));
                new_pc += 2;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitConditionalJump(target, false);
            },

            .push_const_call => {
                const idx = readU16(code, new_pc);
                const argc: u8 = code[new_pc + 2];
                new_pc += 3;
                const val = self.func.constants[idx];
                try self.emitPushImm64(@bitCast(val));
                try self.emitCall(argc, false);
            },

            .add_mod => {
                const divisor_idx = readU16(code, new_pc);
                new_pc += 2;
                try self.emitBinaryOp(.add);
                const divisor = self.func.constants[divisor_idx];
                try self.emitPushImm64(@bitCast(divisor));
                try self.emitMod();
            },

            .sub_mod => {
                const divisor_idx = readU16(code, new_pc);
                new_pc += 2;
                try self.emitBinaryOp(.sub);
                const divisor = self.func.constants[divisor_idx];
                try self.emitPushImm64(@bitCast(divisor));
                try self.emitMod();
            },

            .mul_mod => {
                const divisor_idx = readU16(code, new_pc);
                new_pc += 2;
                try self.emitBinaryOp(.mul);
                const divisor = self.func.constants[divisor_idx];
                try self.emitPushImm64(@bitCast(divisor));
                try self.emitMod();
            },

            .mod_const => {
                const divisor_idx = readU16(code, new_pc);
                new_pc += 2;
                const divisor = self.func.constants[divisor_idx];
                try self.emitPushImm64(@bitCast(divisor));
                try self.emitMod();
            },

            .mod_const_i8 => {
                const divisor: i8 = @bitCast(code[new_pc]);
                new_pc += 1;
                try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(@as(i32, divisor))));
                try self.emitMod();
            },

            .add_const_i8 => {
                const constant: i8 = @bitCast(code[new_pc]);
                new_pc += 1;
                try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(@as(i32, constant))));
                try self.emitBinaryOp(.add);
            },

            .sub_const_i8 => {
                const constant: i8 = @bitCast(code[new_pc]);
                new_pc += 1;
                try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(@as(i32, constant))));
                try self.emitBinaryOp(.sub);
            },

            .mul_const_i8 => {
                const constant: i8 = @bitCast(code[new_pc]);
                new_pc += 1;
                try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(@as(i32, constant))));
                try self.emitBinaryOp(.mul);
            },

            .lt_const_i8 => {
                const constant: i8 = @bitCast(code[new_pc]);
                new_pc += 1;
                try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(@as(i32, constant))));
                try self.emitComparison(.lt);
            },

            .le_const_i8 => {
                const constant: i8 = @bitCast(code[new_pc]);
                new_pc += 1;
                try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(@as(i32, constant))));
                try self.emitComparison(.lte);
            },

            .shr_1 => {
                try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(1)));
                try self.emitShift(.shr);
            },

            .mul_2 => {
                try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(2)));
                try self.emitBinaryOp(.mul);
            },

            .for_of_next => {
                const offset: i16 = @bitCast(readU16(code, new_pc));
                new_pc += 2;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitForOfNext(target);
            },

            .for_of_next_put_loc => {
                const local_idx = code[new_pc];
                const offset: i16 = @bitCast(readU16(code, new_pc + 1));
                new_pc += 3;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitForOfNextPutLoc(local_idx, target);
            },

            .new_object => {
                try self.emitNewObject();
            },

            .new_array => {
                const length = readU16(code, new_pc);
                new_pc += 2;
                try self.emitNewArray(length);
            },

            .get_field => {
                const atom_idx = readU16(code, new_pc);
                new_pc += 2;
                try self.emitGetField(atom_idx);
            },

            .get_field_ic => {
                const atom_idx = readU16(code, new_pc);
                // Skip cache_idx (u16)
                new_pc += 4;
                try self.emitGetField(atom_idx);
            },

            .put_field => {
                const atom_idx = readU16(code, new_pc);
                new_pc += 2;
                try self.emitPutField(atom_idx, false);
            },

            .put_field_keep => {
                const atom_idx = readU16(code, new_pc);
                new_pc += 2;
                try self.emitPutField(atom_idx, true);
            },

            .put_field_ic => {
                const atom_idx = readU16(code, new_pc);
                // Skip cache_idx (u16)
                new_pc += 4;
                try self.emitPutField(atom_idx, false);
            },

            .get_elem => {
                try self.emitGetElem();
            },

            .put_elem => {
                try self.emitPutElem();
            },

            .get_global => {
                const atom_idx = readU16(code, new_pc);
                new_pc += 2;
                try self.emitGetGlobal(atom_idx);
            },

            .put_global => {
                const atom_idx = readU16(code, new_pc);
                new_pc += 2;
                try self.emitPutGlobal(atom_idx);
            },

            .get_field_call => {
                const atom_idx = readU16(code, new_pc);
                const argc: u8 = code[new_pc + 2];
                new_pc += 3;
                try self.emitGetField(atom_idx);
                try self.emitCall(argc, true);
            },

            // Function calls
            .call => {
                const argc = code[new_pc];
                new_pc += 1;
                try self.emitCall(argc, false);
            },

            .call_method => {
                const argc = code[new_pc];
                new_pc += 1;
                try self.emitCall(argc, true);
            },

            .tail_call => {
                const argc = code[new_pc];
                new_pc += 1;
                try self.emitCall(argc, false);
            },

            .goto => {
                const offset: i16 = @bitCast(readU16(code, new_pc));
                new_pc += 2;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitJump(target, false);
            },

            .if_true => {
                const offset: i16 = @bitCast(readU16(code, new_pc));
                new_pc += 2;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitConditionalJump(target, true);
            },

            .if_false => {
                const offset: i16 = @bitCast(readU16(code, new_pc));
                new_pc += 2;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitConditionalJump(target, false);
            },

            // Comparison opcodes (integer fast path)
            .lt => try self.emitComparison(.lt),
            .lte => try self.emitComparison(.lte),
            .gt => try self.emitComparison(.gt),
            .gte => try self.emitComparison(.gte),

            // Equality opcodes
            .eq => try self.emitComparison(.eq),
            .neq => try self.emitComparison(.neq),
            .strict_eq => try self.emitStrictEquality(false),
            .strict_neq => try self.emitStrictEquality(true),

            // Loop back-edge
            .loop => {
                const offset: i16 = @bitCast(readU16(code, new_pc));
                new_pc += 2;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitJump(target, false);
            },

            // Bitwise operations
            .bit_and => try self.emitBitwiseOp(.@"and"),
            .bit_or => try self.emitBitwiseOp(.@"or"),
            .bit_xor => try self.emitBitwiseOp(.xor),
            .bit_not => try self.emitBitwiseNot(),
            .shl => try self.emitShift(.shl),
            .shr => try self.emitShift(.shr),
            .ushr => try self.emitShift(.ushr),

            // Type operators
            .typeof => try self.emitTypeOf(),

            // Logical NOT
            .not => try self.emitLogicalNot(),

            else => {
                return CompileError.UnsupportedOpcode;
            },
        }

        return new_pc;
    }

    // ========================================
    // Architecture-agnostic helpers
    // ========================================

    const BinaryOp = enum { add, sub, mul };

    const RegAlias = enum { ret, tmp0, tmp1 };

    fn getReg(alias: RegAlias) Register {
        if (is_x86_64) {
            return switch (alias) {
                .ret => .rax,
                .tmp0 => .rcx,
                .tmp1 => .rdx,
            };
        } else if (is_aarch64) {
            return switch (alias) {
                .ret => .x0,
                .tmp0 => .x9,
                .tmp1 => .x10,
            };
        }
    }

    fn getCtxReg() Register {
        if (is_x86_64) {
            return .rbx;
        } else if (is_aarch64) {
            return .x19;
        }
    }

    fn emitLoadSp(self: *BaselineCompiler, dst: Register) CompileError!void {
        const ctx = getCtxReg();
        if (is_x86_64) {
            self.emitter.movRegMem(dst, ctx, CTX_SP_OFF) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.ldrImm(dst, ctx, @intCast(@as(u32, @bitCast(CTX_SP_OFF)))) catch return CompileError.OutOfMemory;
        }
    }

    fn emitStoreSp(self: *BaselineCompiler, src: Register) CompileError!void {
        const ctx = getCtxReg();
        if (is_x86_64) {
            self.emitter.movMemReg(ctx, CTX_SP_OFF, src) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.strImm(src, ctx, @intCast(@as(u32, @bitCast(CTX_SP_OFF)))) catch return CompileError.OutOfMemory;
        }
    }

    fn emitLoadStackPtr(self: *BaselineCompiler, dst: Register) CompileError!void {
        const ctx = getCtxReg();
        if (is_x86_64) {
            self.emitter.movRegMem(dst, ctx, CTX_STACK_PTR_OFF) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.ldrImm(dst, ctx, @intCast(@as(u32, @bitCast(CTX_STACK_PTR_OFF)))) catch return CompileError.OutOfMemory;
        }
    }

    fn newLocalLabel(self: *BaselineCompiler) u32 {
        const id = 0x8000_0000 | self.next_local_label;
        self.next_local_label += 1;
        return id;
    }

    fn markLabel(self: *BaselineCompiler, id: u32) CompileError!void {
        self.labels.put(self.allocator, id, @intCast(self.emitter.buffer.items.len)) catch return CompileError.OutOfMemory;
    }

    fn emitJmpToLabel(self: *BaselineCompiler, target: u32) CompileError!void {
        if (self.labels.get(target)) |native_offset| {
            const current = @as(i32, @intCast(self.emitter.buffer.items.len));
            if (is_x86_64) {
                const rel = @as(i32, @intCast(native_offset)) - current - 5;
                self.emitter.jmp(rel) catch return CompileError.OutOfMemory;
            } else if (is_aarch64) {
                const rel = @as(i32, @intCast(native_offset)) - current;
                self.emitter.b(rel) catch return CompileError.OutOfMemory;
            }
        } else {
            const patch_offset: u32 = @intCast(self.emitter.buffer.items.len);
            if (is_x86_64) {
                self.emitter.jmp(0) catch return CompileError.OutOfMemory;
                self.pending_jumps.append(self.allocator, .{
                    .native_offset = patch_offset + 1,
                    .bytecode_target = target,
                    .is_conditional = false,
                }) catch return CompileError.OutOfMemory;
            } else if (is_aarch64) {
                self.emitter.b(0) catch return CompileError.OutOfMemory;
                self.pending_jumps.append(self.allocator, .{
                    .native_offset = patch_offset,
                    .bytecode_target = target,
                    .is_conditional = false,
                }) catch return CompileError.OutOfMemory;
            }
        }
    }

    fn emitJccToLabel(self: *BaselineCompiler, cond: x86.X86Emitter.Condition, target: u32) CompileError!void {
        if (!is_x86_64) return;
        if (self.labels.get(target)) |native_offset| {
            const current = @as(i32, @intCast(self.emitter.buffer.items.len));
            const rel = @as(i32, @intCast(native_offset)) - current - 6;
            self.emitter.jcc(cond, rel) catch return CompileError.OutOfMemory;
        } else {
            const patch_offset: u32 = @intCast(self.emitter.buffer.items.len + 2);
            self.emitter.jcc(cond, 0) catch return CompileError.OutOfMemory;
            self.pending_jumps.append(self.allocator, .{
                .native_offset = patch_offset,
                .bytecode_target = target,
                .is_conditional = true,
            }) catch return CompileError.OutOfMemory;
        }
    }

    fn emitBcondToLabel(self: *BaselineCompiler, cond: arm64.Condition, target: u32) CompileError!void {
        if (!is_aarch64) return;
        if (self.labels.get(target)) |native_offset| {
            const current = @as(i32, @intCast(self.emitter.buffer.items.len));
            const rel = @as(i32, @intCast(native_offset)) - current;
            self.emitter.bcond(cond, rel) catch return CompileError.OutOfMemory;
        } else {
            const patch_offset: u32 = @intCast(self.emitter.buffer.items.len);
            self.emitter.bcond(cond, 0) catch return CompileError.OutOfMemory;
            self.pending_jumps.append(self.allocator, .{
                .native_offset = patch_offset,
                .bytecode_target = target,
                .is_conditional = true,
            }) catch return CompileError.OutOfMemory;
        }
    }

    fn emitPushReg(self: *BaselineCompiler, val_reg: Register) CompileError!void {
        if (is_x86_64) {
            // r8 = sp, r9 = stack_ptr, r10 = addr
            try self.emitLoadSp(.r8);
            try self.emitLoadStackPtr(.r9);
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r10, 0, val_reg) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(.r8, 1) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.r8);
        } else if (is_aarch64) {
            // x12 = sp, x13 = stack_ptr, x14 = addr
            try self.emitLoadSp(.x12);
            try self.emitLoadStackPtr(.x13);
            self.emitter.lslRegImm(.x14, .x12, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x14, .x13, .x14) catch return CompileError.OutOfMemory;
            self.emitter.strImm(val_reg, .x14, 0) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm12(.x12, .x12, 1) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.x12);
        }
    }

    fn emitPopReg(self: *BaselineCompiler, dst: Register) CompileError!void {
        if (is_x86_64) {
            // r8 = sp, r9 = stack_ptr, r10 = addr
            try self.emitLoadSp(.r8);
            self.emitter.subRegImm32(.r8, 1) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.r8);
            try self.emitLoadStackPtr(.r9);
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(dst, .r10, 0) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // x12 = sp, x13 = stack_ptr, x14 = addr
            try self.emitLoadSp(.x12);
            self.emitter.subRegImm12(.x12, .x12, 1) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.x12);
            try self.emitLoadStackPtr(.x13);
            self.emitter.lslRegImm(.x14, .x12, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x14, .x13, .x14) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(dst, .x14, 0) catch return CompileError.OutOfMemory;
        }
    }

    fn emitMovImm64(self: *BaselineCompiler, reg: RegAlias, imm: u64) CompileError!void {
        const r = getReg(reg);
        self.emitter.movRegImm64(r, imm) catch return CompileError.OutOfMemory;
    }

    fn emitPushImm64(self: *BaselineCompiler, imm: u64) CompileError!void {
        if (is_x86_64) {
            self.emitter.movRegImm64(.rax, imm) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegImm64(.x9, imm) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
        }
    }

    fn emitPop(self: *BaselineCompiler, dst: RegAlias) CompileError!void {
        const r = getReg(dst);
        try self.emitPopReg(r);
    }

    fn emitDup(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            // r8 = sp, r9 = stack_ptr, r10 = addr (top), r11 = value
            try self.emitLoadSp(.r8);
            try self.emitLoadStackPtr(.r9);
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.r11, .r10, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r10, 0, .r11) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(.r8, 1) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.r8);
        } else if (is_aarch64) {
            // x9 = sp, x10 = stack_ptr, x11 = addr, x12 = value
            try self.emitLoadSp(.x9);
            try self.emitLoadStackPtr(.x10);
            self.emitter.subRegImm12(.x11, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x11, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x12, .x11, 0) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x11, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x12, .x11, 0) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm12(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.x9);
        }
    }

    fn emitDrop(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            try self.emitLoadSp(.r8);
            self.emitter.subRegImm32(.r8, 1) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.r8);
        } else if (is_aarch64) {
            try self.emitLoadSp(.x9);
            self.emitter.subRegImm12(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.x9);
        }
    }

    fn emitSwap(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            try self.emitLoadSp(.r8);
            try self.emitLoadStackPtr(.r9);

            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory; // addr_top

            self.emitter.movRegReg(.r11, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r11, 2) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r11, .r9) catch return CompileError.OutOfMemory; // addr_second

            self.emitter.movRegMem(.rax, .r10, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rcx, .r11, 0) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r10, 0, .rcx) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r11, 0, .rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            try self.emitLoadSp(.x9);
            try self.emitLoadStackPtr(.x10);

            self.emitter.subRegImm12(.x11, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x11, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory; // addr_top

            self.emitter.subRegImm12(.x12, .x9, 2) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x12, .x12, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x12, .x10, .x12) catch return CompileError.OutOfMemory; // addr_second

            self.emitter.ldrImm(.x13, .x11, 0) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x14, .x12, 0) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x14, .x11, 0) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x13, .x12, 0) catch return CompileError.OutOfMemory;
        }
    }

    fn emitDup2(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            try self.emitLoadSp(.r8);
            try self.emitLoadStackPtr(.r9);

            // Load second (a)
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 2) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rax, .r10, 0) catch return CompileError.OutOfMemory;

            // Load top (b)
            self.emitter.movRegReg(.r11, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r11, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rcx, .r11, 0) catch return CompileError.OutOfMemory;

            // Store a at sp
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r10, 0, .rax) catch return CompileError.OutOfMemory;

            // Store b at sp+1
            self.emitter.movRegReg(.r11, .r8) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r11, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r11, 0, .rcx) catch return CompileError.OutOfMemory;

            // sp += 2
            self.emitter.addRegImm32(.r8, 2) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.r8);
        } else if (is_aarch64) {
            try self.emitLoadSp(.x9);
            try self.emitLoadStackPtr(.x10);

            self.emitter.subRegImm12(.x11, .x9, 2) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x11, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x12, .x11, 0) catch return CompileError.OutOfMemory; // a

            self.emitter.subRegImm12(.x11, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x11, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x13, .x11, 0) catch return CompileError.OutOfMemory; // b

            self.emitter.lslRegImm(.x11, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x12, .x11, 0) catch return CompileError.OutOfMemory;

            self.emitter.addRegImm12(.x14, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x14, .x14, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x14, .x10, .x14) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x13, .x14, 0) catch return CompileError.OutOfMemory;

            self.emitter.addRegImm12(.x9, .x9, 2) catch return CompileError.OutOfMemory;
            try self.emitStoreSp(.x9);
        }
    }

    fn emitRot3(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            try self.emitLoadSp(.r8);
            try self.emitLoadStackPtr(.r9);

            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rax, .r10, 0) catch return CompileError.OutOfMemory; // a

            self.emitter.movRegReg(.r11, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r11, 2) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r11, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rcx, .r11, 0) catch return CompileError.OutOfMemory; // b

            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rdx, .r10, 0) catch return CompileError.OutOfMemory; // c

            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r10, 0, .rcx) catch return CompileError.OutOfMemory; // b -> a

            self.emitter.movRegReg(.r11, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r11, 2) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r11, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r11, 0, .rdx) catch return CompileError.OutOfMemory; // c -> b

            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.r10, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r10, 0, .rax) catch return CompileError.OutOfMemory; // a -> c
        } else if (is_aarch64) {
            try self.emitLoadSp(.x9);
            try self.emitLoadStackPtr(.x10);

            self.emitter.subRegImm12(.x11, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x11, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x13, .x11, 0) catch return CompileError.OutOfMemory; // a

            self.emitter.subRegImm12(.x12, .x9, 2) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x12, .x12, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x12, .x10, .x12) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x14, .x12, 0) catch return CompileError.OutOfMemory; // b

            self.emitter.subRegImm12(.x15, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x15, .x15, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x15, .x10, .x15) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x15, .x15, 0) catch return CompileError.OutOfMemory; // c (reuse x15)

            self.emitter.subRegImm12(.x11, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x11, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x14, .x11, 0) catch return CompileError.OutOfMemory; // b -> a

            self.emitter.subRegImm12(.x12, .x9, 2) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x12, .x12, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x12, .x10, .x12) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x15, .x12, 0) catch return CompileError.OutOfMemory; // c -> b

            self.emitter.subRegImm12(.x11, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x11, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x13, .x11, 0) catch return CompileError.OutOfMemory; // a -> c
        }
    }

    fn emitBinaryOp(self: *BaselineCompiler, op: BinaryOp) CompileError!void {
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop right -> rcx, left -> rax
            try self.emitPopReg(.rcx);
            try self.emitPopReg(.rax);
            // Preserve boxed values for slow path
            self.emitter.movRegReg(.r8, .rax) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r9, .rcx) catch return CompileError.OutOfMemory;

            // Tag guard: both ints (LSB == 0)
            self.emitter.movRegReg(.rdx, .rax) catch return CompileError.OutOfMemory;
            self.emitter.orRegReg(.rdx, .rcx) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.rdx, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.rdx, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Unbox
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;

            switch (op) {
                .add => {
                    self.emitter.addRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.o, slow);
                },
                .sub => {
                    self.emitter.subRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.o, slow);
                },
                .mul => {
                    self.emitter.imulRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.o, slow);
                },
            }

            // Rebox
            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            // Slow path
            try self.markLabel(slow);
            const fn_ptr: u64 = switch (op) {
                .add => @intFromPtr(&Context.jitAdd),
                .sub => @intFromPtr(&Context.jitSub),
                .mul => @intFromPtr(&Context.jitMul),
            };
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdx, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop right -> x10, left -> x9
            try self.emitPopReg(.x10);
            try self.emitPopReg(.x9);
            // Preserve boxed values for slow path
            self.emitter.movRegReg(.x12, .x9) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x13, .x10) catch return CompileError.OutOfMemory;

            if (op == .mul) {
                try self.emitJmpToLabel(slow);
            }

            // Tag guard: both ints
            self.emitter.orrRegReg(.x11, .x9, .x10) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm(.x11, .x11, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Unbox
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;

            switch (op) {
                .add => {
                    self.emitter.addsRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.vs, slow);
                },
                .sub => {
                    self.emitter.subsRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.vs, slow);
                },
                .mul => {
                    self.emitter.mulRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.vs, slow);
                },
            }

            // Rebox
            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
            try self.emitJmpToLabel(done);

            // Slow path
            try self.markLabel(slow);
            const fn_ptr: u64 = switch (op) {
                .add => @intFromPtr(&Context.jitAdd),
                .sub => @intFromPtr(&Context.jitSub),
                .mul => @intFromPtr(&Context.jitMul),
            };
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x2, .x13) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x12, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x12) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    fn emitNeg(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            try self.emitPopReg(.rax);
            self.emitter.movRegReg(.r8, .rax) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rcx, .rax) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.rcx, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.rcx, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Unbox, negate, rebox
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.negReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.o, slow);
            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            const fn_ptr = @intFromPtr(&Context.jitNeg);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            try self.emitPopReg(.x9);
            self.emitter.movRegReg(.x12, .x9) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm(.x11, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.negsReg(.x9, .x9) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.vs, slow);
            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            const fn_ptr = @intFromPtr(&Context.jitNeg);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x12, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x12) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    fn emitCall(self: *BaselineCompiler, argc: u8, is_method: bool) CompileError!void {
        const fn_ptr = @intFromPtr(&jitCall);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rsi, @intCast(argc)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rdx, if (is_method) 1 else 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, argc) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, if (is_method) 1 else 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    fn emitDiv(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitDiv);
        if (is_x86_64) {
            try self.emitPopReg(.rdx); // b
            try self.emitPopReg(.rsi); // a
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // b
            try self.emitPopReg(.x1); // a
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    fn emitMod(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitMod);
        if (is_x86_64) {
            try self.emitPopReg(.rdx); // b
            try self.emitPopReg(.rsi); // a
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // b
            try self.emitPopReg(.x1); // a
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    fn emitPow(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitPow);
        if (is_x86_64) {
            try self.emitPopReg(.rdx); // b
            try self.emitPopReg(.rsi); // a
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // b
            try self.emitPopReg(.x1); // a
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    fn emitIncDec(self: *BaselineCompiler, is_inc: bool) CompileError!void {
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            try self.emitPopReg(.rax);
            self.emitter.movRegReg(.r8, .rax) catch return CompileError.OutOfMemory;

            self.emitter.movRegReg(.rcx, .rax) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.rcx, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.rcx, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            if (is_inc) {
                self.emitter.addRegImm32(.rax, 1) catch return CompileError.OutOfMemory;
            } else {
                self.emitter.subRegImm32(.rax, 1) catch return CompileError.OutOfMemory;
            }
            try self.emitJccToLabel(.o, slow);

            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            const fn_ptr = if (is_inc) @intFromPtr(&Context.jitInc) else @intFromPtr(&Context.jitDec);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            try self.emitPopReg(.x9);
            self.emitter.movRegReg(.x12, .x9) catch return CompileError.OutOfMemory;

            self.emitter.andRegImm(.x11, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x10, 1) catch return CompileError.OutOfMemory;
            if (is_inc) {
                self.emitter.addsRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
            } else {
                self.emitter.subsRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
            }
            try self.emitBcondToLabel(.vs, slow);

            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            const fn_ptr = if (is_inc) @intFromPtr(&Context.jitInc) else @intFromPtr(&Context.jitDec);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x14, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x14) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    /// Emit code to get a local variable and push it onto the JS operand stack
    /// Local variables are stored at ctx->stack[ctx->fp + idx]
    fn emitGetLocal(self: *BaselineCompiler, idx: u8) CompileError!void {
        if (is_x86_64) {
            // rbx = ctx pointer (saved in prologue)
            // 1. Load ctx->fp into rax
            self.emitter.movRegMem(.rax, .rbx, CTX_FP_OFF) catch return CompileError.OutOfMemory;
            // 2. Add local index to get total offset
            if (idx > 0) {
                self.emitter.addRegImm32(.rax, idx) catch return CompileError.OutOfMemory;
            }
            // 3. Load ctx->stack.ptr into rcx
            self.emitter.movRegMem(.rcx, .rbx, CTX_STACK_PTR_OFF) catch return CompileError.OutOfMemory;
            // 4. Load value from stack[fp + idx] into rax
            // Address = rcx + rax * 8 (but we need scaled addressing)
            // Use: rax = rcx + rax * 8, then load [rax]
            self.emitter.shlRegImm(.rax, 3) catch return CompileError.OutOfMemory; // rax *= 8
            self.emitter.addRegReg(.rax, .rcx) catch return CompileError.OutOfMemory; // rax = stack_ptr + offset*8
            self.emitter.movRegMem(.rax, .rax, 0) catch return CompileError.OutOfMemory; // rax = [rax]
            // 5. Push onto operand stack
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            // x19 = ctx pointer (saved in prologue)
            // 1. Load ctx->fp into x9
            self.emitter.ldrImm(.x9, .x19, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
            // 2. Add local index
            if (idx > 0) {
                self.emitter.addRegImm12(.x9, .x9, idx) catch return CompileError.OutOfMemory;
            }
            // 3. Load ctx->stack.ptr into x10
            self.emitter.ldrImm(.x10, .x19, @intCast(@as(u32, @bitCast(CTX_STACK_PTR_OFF)))) catch return CompileError.OutOfMemory;
            // 4. Load value from stack[fp + idx]
            // Address = x10 + x9 * 8
            self.emitter.addRegReg(.x9, .x10, .x9) catch return CompileError.OutOfMemory; // Can't do scaled add directly
            // Need: x9 = x10 + x9 * 8. Use shift then add.
            // Actually, let's recalculate: x9 = fp + idx, need x10 + x9 * 8
            // Redo: shift x9 by 3, then add x10
            self.emitter.ldrImm(.x9, .x19, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
            if (idx > 0) {
                self.emitter.addRegImm12(.x9, .x9, idx) catch return CompileError.OutOfMemory;
            }
            // x9 = x9 << 3 (multiply by 8)
            // ARM64 doesn't have shift by immediate in the emitter, use add with shifted register
            // For now, use multiple adds as a workaround or load with scaled offset
            // Actually, ldrImm supports scaled offset. Let's use: x10 + x9 * 8
            // We need ldr x11, [x10, x9, lsl #3] but we don't have that instruction
            // Workaround: manually compute address
            self.emitter.addRegReg(.x11, .x9, .x9) catch return CompileError.OutOfMemory; // x11 = x9 * 2
            self.emitter.addRegReg(.x11, .x11, .x11) catch return CompileError.OutOfMemory; // x11 = x9 * 4
            self.emitter.addRegReg(.x11, .x11, .x11) catch return CompileError.OutOfMemory; // x11 = x9 * 8
            self.emitter.addRegReg(.x11, .x10, .x11) catch return CompileError.OutOfMemory; // x11 = stack_ptr + offset*8
            self.emitter.ldrImm(.x9, .x11, 0) catch return CompileError.OutOfMemory; // x9 = [x11]
            // 5. Push onto operand stack
            try self.emitPushReg(.x9);
        }
    }

    /// Emit code to pop a value from the JS operand stack and store it in a local variable
    /// Local variables are stored at ctx->stack[ctx->fp + idx]
    fn emitPutLocal(self: *BaselineCompiler, idx: u8) CompileError!void {
        if (is_x86_64) {
            // Pop value from operand stack into rax
            try self.emitPopReg(.rax);
            // rbx = ctx pointer
            // 1. Load ctx->fp into rcx
            self.emitter.movRegMem(.rcx, .rbx, CTX_FP_OFF) catch return CompileError.OutOfMemory;
            // 2. Add local index
            if (idx > 0) {
                self.emitter.addRegImm32(.rcx, idx) catch return CompileError.OutOfMemory;
            }
            // 3. Load ctx->stack.ptr into rdx
            self.emitter.movRegMem(.rdx, .rbx, CTX_STACK_PTR_OFF) catch return CompileError.OutOfMemory;
            // 4. Compute address: stack_ptr + (fp + idx) * 8
            self.emitter.shlRegImm(.rcx, 3) catch return CompileError.OutOfMemory; // rcx *= 8
            self.emitter.addRegReg(.rcx, .rdx) catch return CompileError.OutOfMemory; // rcx = address
            // 5. Store value
            self.emitter.movMemReg(.rcx, 0, .rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Pop value from operand stack into x9
            try self.emitPopReg(.x9);
            // x19 = ctx pointer
            // 1. Load ctx->fp into x10
            self.emitter.ldrImm(.x10, .x19, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
            // 2. Add local index
            if (idx > 0) {
                self.emitter.addRegImm12(.x10, .x10, idx) catch return CompileError.OutOfMemory;
            }
            // 3. Load ctx->stack.ptr into x11
            self.emitter.ldrImm(.x11, .x19, @intCast(@as(u32, @bitCast(CTX_STACK_PTR_OFF)))) catch return CompileError.OutOfMemory;
            // 4. Compute address: stack_ptr + (fp + idx) * 8
            self.emitter.addRegReg(.x12, .x10, .x10) catch return CompileError.OutOfMemory; // x12 = x10 * 2
            self.emitter.addRegReg(.x12, .x12, .x12) catch return CompileError.OutOfMemory; // x12 = x10 * 4
            self.emitter.addRegReg(.x12, .x12, .x12) catch return CompileError.OutOfMemory; // x12 = x10 * 8
            self.emitter.addRegReg(.x12, .x11, .x12) catch return CompileError.OutOfMemory; // x12 = address
            // 5. Store value
            self.emitter.strImm(.x9, .x12, 0) catch return CompileError.OutOfMemory;
        }
    }

    /// Emit code to get an object property by atom index via helper
    fn emitGetField(self: *BaselineCompiler, atom_idx: u16) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitGetField);
        if (is_x86_64) {
            try self.emitPopReg(.rsi); // obj
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rdx, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x1); // obj
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    /// Emit code to set an object property by atom index via helper
    /// If keep is true, pushes the assigned value onto the operand stack.
    fn emitPutField(self: *BaselineCompiler, atom_idx: u16, keep: bool) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitPutField);
        if (is_x86_64) {
            try self.emitPopReg(.rcx); // val
            try self.emitPopReg(.rsi); // obj
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rdx, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            if (keep) {
                try self.emitPushReg(.rax);
            }
        } else if (is_aarch64) {
            try self.emitPopReg(.x3); // val
            try self.emitPopReg(.x1); // obj
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            if (keep) {
                try self.emitPushReg(.x0);
            }
        }
    }

    /// Emit code to get an element by index via helper
    fn emitGetElem(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitGetElem);
        if (is_x86_64) {
            try self.emitPopReg(.rdx); // idx
            try self.emitPopReg(.rsi); // obj
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // idx
            try self.emitPopReg(.x1); // obj
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    /// Emit code to get a global by atom index via helper
    fn emitGetGlobal(self: *BaselineCompiler, atom_idx: u16) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitGetGlobal);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rsi, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    /// Emit code to set a global by atom index via helper
    fn emitPutGlobal(self: *BaselineCompiler, atom_idx: u16) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitPutGlobal);
        if (is_x86_64) {
            try self.emitPopReg(.rdx); // val
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rsi, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // val
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
        }
    }

    /// Emit code to create a new object via helper
    fn emitNewObject(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitNewObject);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    /// Emit code to create a new array via helper
    fn emitNewArray(self: *BaselineCompiler, length: u16) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitNewArray);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rsi, length) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, length) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    /// Emit code to set an element by index via helper
    fn emitPutElem(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitPutElem);
        if (is_x86_64) {
            try self.emitPopReg(.rcx); // val
            try self.emitPopReg(.rdx); // idx
            try self.emitPopReg(.rsi); // obj
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            try self.emitPopReg(.x3); // val
            try self.emitPopReg(.x2); // idx
            try self.emitPopReg(.x1); // obj
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
        }
    }

    fn emitForOfNext(self: *BaselineCompiler, target: u32) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitForOfNext);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            self.emitter.testRegReg(.rax, .rax) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, target);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x0, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, target);
        }
    }

    fn emitForOfNextPutLoc(self: *BaselineCompiler, local_idx: u8, target: u32) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitForOfNextPutLoc);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rsi, local_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            self.emitter.testRegReg(.rax, .rax) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, target);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, local_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x0, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, target);
        }
    }

    const ComparisonOp = enum { lt, lte, gt, gte, eq, neq };

    /// Emit comparison with integer fast path + slow-path helpers.
    /// Result is pushed as JSValue.true_val or JSValue.false_val (or exception on slow path).
    fn emitComparison(self: *BaselineCompiler, op: ComparisonOp) CompileError!void {
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();
            const is_eq = op == .eq or op == .neq;
            const fast_equal = if (is_eq) self.newLocalLabel() else 0;

            // Pop right operand into rcx, left into rax
            try self.emitPopReg(.rcx); // right
            try self.emitPopReg(.rax); // left
            // Preserve boxed values for slow path
            self.emitter.movRegReg(.r8, .rax) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r9, .rcx) catch return CompileError.OutOfMemory;

            if (is_eq) {
                // Fast path: raw equality covers same-pointer/same-int/specials
                self.emitter.cmpRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.e, fast_equal);
            }

            // Tag guard: both ints (LSB == 0)
            self.emitter.movRegReg(.rdx, .rax) catch return CompileError.OutOfMemory;
            self.emitter.orRegReg(.rdx, .rcx) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.rdx, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.rdx, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Unbox and compare
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.rax, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rdx, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            const cond: x86.X86Emitter.Condition = switch (op) {
                .lt => .l,
                .lte => .le,
                .gt => .g,
                .gte => .ge,
                .eq => .e,
                .neq => .ne,
            };
            self.emitter.cmovcc(cond, .rax, .rdx) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            if (is_eq) {
                try self.markLabel(fast_equal);
                const imm = if (op == .eq) value_mod.JSValue.true_val else value_mod.JSValue.false_val;
                try self.emitPushImm64(@bitCast(imm));
                try self.emitJmpToLabel(done);
            }

            // Slow path
            try self.markLabel(slow);
            const fn_ptr: u64 = switch (op) {
                .lt => @intFromPtr(&Context.jitCompareLt),
                .lte => @intFromPtr(&Context.jitCompareLte),
                .gt => @intFromPtr(&Context.jitCompareGt),
                .gte => @intFromPtr(&Context.jitCompareGte),
                .eq, .neq => @intFromPtr(&Context.jitLooseEquals),
            };
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdx, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            if (op == .neq) {
                // Invert boolean result: (true -> false, false -> true)
                self.emitter.movRegReg(.rdx, .rax) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rcx, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rax, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.rdx, .rcx) catch return CompileError.OutOfMemory;
                self.emitter.cmovcc(.ne, .rax, .rcx) catch return CompileError.OutOfMemory;
            }
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();
            const is_eq = op == .eq or op == .neq;
            const fast_equal = if (is_eq) self.newLocalLabel() else 0;

            // Pop right operand into x10, left into x9
            try self.emitPopReg(.x10); // right
            try self.emitPopReg(.x9); // left
            // Preserve boxed values for slow path
            self.emitter.movRegReg(.x12, .x9) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x13, .x10) catch return CompileError.OutOfMemory;

            if (is_eq) {
                // Fast path: raw equality covers same-pointer/same-int/specials
                self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.eq, fast_equal);
            }

            // Tag guard: both ints
            self.emitter.orrRegReg(.x11, .x9, .x10) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm(.x11, .x11, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Unbox and compare
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x12, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            const cond: arm64.Condition = switch (op) {
                .lt => .lt,
                .lte => .le,
                .gt => .gt,
                .gte => .ge,
                .eq => .eq,
                .neq => .ne,
            };
            self.emitter.csel(.x9, .x12, .x11, cond) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
            try self.emitJmpToLabel(done);

            if (is_eq) {
                try self.markLabel(fast_equal);
                const imm = if (op == .eq) value_mod.JSValue.true_val else value_mod.JSValue.false_val;
                try self.emitPushImm64(@bitCast(imm));
                try self.emitJmpToLabel(done);
            }

            // Slow path
            try self.markLabel(slow);
            const fn_ptr: u64 = switch (op) {
                .lt => @intFromPtr(&Context.jitCompareLt),
                .lte => @intFromPtr(&Context.jitCompareLte),
                .gt => @intFromPtr(&Context.jitCompareGt),
                .gte => @intFromPtr(&Context.jitCompareGte),
                .eq, .neq => @intFromPtr(&Context.jitLooseEquals),
            };
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x2, .x13) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x14, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x14) catch return CompileError.OutOfMemory;
            if (op == .neq) {
                // Invert boolean result: (true -> false, false -> true)
                self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x12, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x0, .x11) catch return CompileError.OutOfMemory;
                self.emitter.csel(.x0, .x12, .x11, .eq) catch return CompileError.OutOfMemory;
            }
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    /// Emit strict equality comparison
    /// For strict equality, use raw compare fast path and helper for floats/strings.
    fn emitStrictEquality(self: *BaselineCompiler, negate: bool) CompileError!void {
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();
            const fast_equal = self.newLocalLabel();

            // Pop right operand into rcx, left into rax
            try self.emitPopReg(.rcx); // right
            try self.emitPopReg(.rax); // left
            // Preserve boxed values for slow path
            self.emitter.movRegReg(.r8, .rax) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r9, .rcx) catch return CompileError.OutOfMemory;

            // Fast path: raw equality
            self.emitter.cmpRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, fast_equal);

            // Slow path: strictEquals helper (handles floats/strings)
            try self.emitJmpToLabel(slow);

            try self.markLabel(fast_equal);
            const imm = if (negate) value_mod.JSValue.false_val else value_mod.JSValue.true_val;
            try self.emitPushImm64(@bitCast(imm));
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            const fn_ptr: u64 = @intFromPtr(&Context.jitStrictEquals);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdx, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            if (negate) {
                self.emitter.movRegReg(.rdx, .rax) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rcx, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rax, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.rdx, .rcx) catch return CompileError.OutOfMemory;
                self.emitter.cmovcc(.ne, .rax, .rcx) catch return CompileError.OutOfMemory;
            }
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();
            const fast_equal = self.newLocalLabel();

            // Pop right operand into x10, left into x9
            try self.emitPopReg(.x10); // right
            try self.emitPopReg(.x9); // left
            // Preserve boxed values for slow path
            self.emitter.movRegReg(.x12, .x9) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x13, .x10) catch return CompileError.OutOfMemory;

            // Fast path: raw equality
            self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, fast_equal);

            try self.emitJmpToLabel(slow);

            try self.markLabel(fast_equal);
            const imm = if (negate) value_mod.JSValue.false_val else value_mod.JSValue.true_val;
            try self.emitPushImm64(@bitCast(imm));
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            const fn_ptr: u64 = @intFromPtr(&Context.jitStrictEquals);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x2, .x13) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x14, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x14) catch return CompileError.OutOfMemory;
            if (negate) {
                self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x12, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x0, .x11) catch return CompileError.OutOfMemory;
                self.emitter.csel(.x0, .x12, .x11, .eq) catch return CompileError.OutOfMemory;
            }
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    const BitwiseOp = enum { @"and", @"or", xor };

    /// Emit bitwise operation (AND, OR, XOR)
    /// For NaN-boxed integers: extract int32, apply op, re-encode
    fn emitBitwiseOp(self: *BaselineCompiler, op: BitwiseOp) CompileError!void {
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop right operand into rcx, left into rax
            try self.emitPopReg(.rcx);
            try self.emitPopReg(.rax);
            // Preserve boxed values for slow path
            self.emitter.movRegReg(.r8, .rax) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r9, .rcx) catch return CompileError.OutOfMemory;

            // Tag guard: both ints (LSB == 0)
            self.emitter.movRegReg(.rdx, .rax) catch return CompileError.OutOfMemory;
            self.emitter.orRegReg(.rdx, .rcx) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.rdx, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.rdx, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Extract integers (shift right by 1)
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;

            // Apply bitwise operation
            switch (op) {
                .@"and" => self.emitter.andRegReg(.rax, .rcx) catch return CompileError.OutOfMemory,
                .@"or" => self.emitter.orRegReg(.rax, .rcx) catch return CompileError.OutOfMemory,
                .xor => self.emitter.xorRegReg(.rax, .rcx) catch return CompileError.OutOfMemory,
            }

            // Canonicalize to 32-bit
            self.emitter.movRegReg32(.rax, .rax) catch return CompileError.OutOfMemory;

            // Re-encode as JSValue (shift left by 1)
            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            // Slow path
            try self.markLabel(slow);
            const fn_ptr: u64 = switch (op) {
                .@"and" => @intFromPtr(&Context.jitBitAnd),
                .@"or" => @intFromPtr(&Context.jitBitOr),
                .xor => @intFromPtr(&Context.jitBitXor),
            };
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdx, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop right operand into x10, left into x9
            try self.emitPopReg(.x10);
            try self.emitPopReg(.x9);
            // Preserve boxed values for slow path
            self.emitter.movRegReg(.x12, .x9) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x13, .x10) catch return CompileError.OutOfMemory;

            // Tag guard: both ints
            self.emitter.orrRegReg(.x11, .x9, .x10) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm(.x11, .x11, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract integers (shift right by 1)
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;

            // Apply bitwise operation
            switch (op) {
                .@"and" => self.emitter.andRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
                .@"or" => self.emitter.orrRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
                .xor => self.emitter.eorRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
            }

            // Canonicalize to 32-bit
            self.emitter.lslRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
            self.emitter.lsrRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;

            // Re-encode as JSValue (shift left by 1)
            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
            try self.emitJmpToLabel(done);

            // Slow path
            try self.markLabel(slow);
            const fn_ptr: u64 = switch (op) {
                .@"and" => @intFromPtr(&Context.jitBitAnd),
                .@"or" => @intFromPtr(&Context.jitBitOr),
                .xor => @intFromPtr(&Context.jitBitXor),
            };
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x2, .x13) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x14, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x14) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    /// Emit bitwise NOT
    fn emitBitwiseNot(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop operand into rax
            try self.emitPopReg(.rax);
            self.emitter.movRegReg(.r8, .rax) catch return CompileError.OutOfMemory;

            // Tag guard: int
            self.emitter.movRegReg(.rcx, .rax) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.rcx, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.rcx, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Extract integer (shift right by 1)
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;

            // Apply NOT
            self.emitter.notReg(.rax) catch return CompileError.OutOfMemory;

            // Canonicalize to 32-bit
            self.emitter.movRegReg32(.rax, .rax) catch return CompileError.OutOfMemory;

            // Re-encode as JSValue (shift left by 1)
            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            const fn_ptr = @intFromPtr(&Context.jitBitNot);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop operand into x9
            try self.emitPopReg(.x9);
            self.emitter.movRegReg(.x12, .x9) catch return CompileError.OutOfMemory;

            // Tag guard: int
            self.emitter.andRegImm(.x11, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract integer (shift right by 1)
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;

            // Apply NOT
            self.emitter.mvnReg(.x9, .x9) catch return CompileError.OutOfMemory;

            // Canonicalize to 32-bit
            self.emitter.lslRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
            self.emitter.lsrRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;

            // Re-encode as JSValue (shift left by 1)
            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            const fn_ptr = @intFromPtr(&Context.jitBitNot);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x14, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x14) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    const ShiftOp = enum { shl, shr, ushr };

    /// Emit shift operation (SHL, SHR, USHR)
    fn emitShift(self: *BaselineCompiler, op: ShiftOp) CompileError!void {
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop shift amount into rcx, value into rax
            try self.emitPopReg(.rcx);
            try self.emitPopReg(.rax);
            self.emitter.movRegReg(.r8, .rax) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r9, .rcx) catch return CompileError.OutOfMemory;

            // Tag guard: both ints
            self.emitter.movRegReg(.rdx, .rax) catch return CompileError.OutOfMemory;
            self.emitter.orRegReg(.rdx, .rcx) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.rdx, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.rdx, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Extract integers (shift right by 1)
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;

            // Mask shift amount to 5 bits (0x1F) - JS spec
            self.emitter.andRegImm32(.rcx, 0x1F) catch return CompileError.OutOfMemory;

            // Apply shift (rcx holds the count)
            switch (op) {
                .shl => self.emitter.shlRegCl(.rax) catch return CompileError.OutOfMemory,
                .shr => {
                    self.emitter.movsxdRegReg(.rax, .rax) catch return CompileError.OutOfMemory;
                    self.emitter.sarRegCl(.rax) catch return CompileError.OutOfMemory;
                },
                .ushr => {
                    self.emitter.movRegReg32(.rax, .rax) catch return CompileError.OutOfMemory;
                    self.emitter.shrRegCl(.rax) catch return CompileError.OutOfMemory;
                },
            }

            // Canonicalize to 32-bit
            self.emitter.movRegReg32(.rax, .rax) catch return CompileError.OutOfMemory;

            // Re-encode as JSValue (shift left by 1)
            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            // Slow path
            try self.markLabel(slow);
            const fn_ptr: u64 = switch (op) {
                .shl => @intFromPtr(&Context.jitShiftShl),
                .shr => @intFromPtr(&Context.jitShiftShr),
                .ushr => @intFromPtr(&Context.jitShiftUShr),
            };
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdx, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop shift amount into x10, value into x9
            try self.emitPopReg(.x10);
            try self.emitPopReg(.x9);
            self.emitter.movRegReg(.x12, .x9) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x13, .x10) catch return CompileError.OutOfMemory;

            // Tag guard: both ints
            self.emitter.orrRegReg(.x11, .x9, .x10) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm(.x11, .x11, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract integers (shift right by 1)
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;

            // Mask shift amount to 5 bits (0x1F) - JS spec
            self.emitter.andRegImm(.x10, .x10, 0x1F) catch return CompileError.OutOfMemory;

            // Apply shift
            switch (op) {
                .shl => self.emitter.lslRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
                .shr => {
                    self.emitter.lslRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                    self.emitter.asrRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                    self.emitter.asrRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                },
                .ushr => {
                    self.emitter.lslRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                    self.emitter.lsrRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                    self.emitter.lsrRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                },
            }

            // Canonicalize to 32-bit
            self.emitter.lslRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
            self.emitter.lsrRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;

            // Re-encode as JSValue (shift left by 1)
            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
            try self.emitJmpToLabel(done);

            // Slow path
            try self.markLabel(slow);
            const fn_ptr: u64 = switch (op) {
                .shl => @intFromPtr(&Context.jitShiftShl),
                .shr => @intFromPtr(&Context.jitShiftShr),
                .ushr => @intFromPtr(&Context.jitShiftUShr),
            };
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x2, .x13) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x14, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x14) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    fn emitTypeOf(self: *BaselineCompiler) CompileError!void {
        // typeof pops a value and pushes a string describing its type
        // We call Context.jitTypeOf(ctx, value) which returns a JSValue string

        const fn_ptr = @intFromPtr(&Context.jitTypeOf);

        if (is_x86_64) {
            // Pop value into rsi (second argument)
            try self.emitPopReg(.rsi);
            // Move context pointer from rbx to rdi (first argument)
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            // Load function address into rax and call it
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            // Push result (rax contains JSValue)
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            // Pop value into x1 (second argument)
            try self.emitPopReg(.x1);
            // Move context pointer from x19 to x0 (first argument)
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            // Load function address and call it
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            // Push result (x0 contains JSValue)
            try self.emitPushReg(.x0);
        }
    }

    fn emitLogicalNot(self: *BaselineCompiler) CompileError!void {
        // Logical NOT: convert value to boolean and negate.
        // Use Context.jitToBoolean for correctness across all value types.

        const fn_ptr = @intFromPtr(&Context.jitToBoolean);

        if (is_x86_64) {
            // Pop value into rsi (second argument)
            try self.emitPopReg(.rsi);
            // Move context pointer from rbx to rdi (first argument)
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            // Call helper -> bool in rax
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;

            // Convert bool to JSValue (!bool)
            self.emitter.movRegReg(.rcx, .rax) catch return CompileError.OutOfMemory; // rcx = bool
            self.emitter.movRegImm64(.rax, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rdx, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;
            self.emitter.testRegReg(.rcx, .rcx) catch return CompileError.OutOfMemory;
            self.emitter.cmovcc(.e, .rax, .rdx) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            // Pop value into x1 (second argument)
            try self.emitPopReg(.x1);
            // Move context pointer from x19 to x0 (first argument)
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            // Call helper -> bool in x0
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;

            // Convert bool to JSValue (!bool)
            self.emitter.movRegReg(.x9, .x0) catch return CompileError.OutOfMemory; // x9 = bool
            self.emitter.movRegImm64(.x10, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
            self.emitter.csel(.x0, .x11, .x10, .eq) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    fn emitJump(self: *BaselineCompiler, target: u32, is_conditional: bool) CompileError!void {
        if (self.labels.get(target)) |native_offset| {
            // Backward jump - emit directly
            const current = @as(i32, @intCast(self.emitter.buffer.items.len));
            if (is_x86_64) {
                const rel = @as(i32, @intCast(native_offset)) - current - 5;
                self.emitter.jmp(rel) catch return CompileError.OutOfMemory;
            } else if (is_aarch64) {
                const rel = @as(i32, @intCast(native_offset)) - current;
                self.emitter.b(rel) catch return CompileError.OutOfMemory;
            }
        } else {
            // Forward jump - record for patching
            const patch_offset: u32 = @intCast(self.emitter.buffer.items.len);
            if (is_x86_64) {
                self.emitter.jmp(0) catch return CompileError.OutOfMemory;
                self.pending_jumps.append(self.allocator, .{
                    .native_offset = patch_offset + 1, // Skip opcode byte
                    .bytecode_target = target,
                    .is_conditional = is_conditional,
                }) catch return CompileError.OutOfMemory;
            } else if (is_aarch64) {
                self.emitter.b(0) catch return CompileError.OutOfMemory;
                self.pending_jumps.append(self.allocator, .{
                    .native_offset = patch_offset,
                    .bytecode_target = target,
                    .is_conditional = is_conditional,
                }) catch return CompileError.OutOfMemory;
            }
        }
    }

    fn emitConditionalJump(self: *BaselineCompiler, target: u32, jump_if_true: bool) CompileError!void {
        // Pop condition and convert to boolean
        const fn_ptr = @intFromPtr(&Context.jitToBoolean);
        if (is_x86_64) {
            try self.emitPopReg(.rsi);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;
            self.emitter.testRegReg(.rax, .rax) catch return CompileError.OutOfMemory;

            const cond: x86.X86Emitter.Condition = if (jump_if_true) .ne else .e;

            if (self.labels.get(target)) |native_offset| {
                const current = @as(i32, @intCast(self.emitter.buffer.items.len));
                const rel = @as(i32, @intCast(native_offset)) - current - 6;
                self.emitter.jcc(cond, rel) catch return CompileError.OutOfMemory;
            } else {
                const patch_offset: u32 = @intCast(self.emitter.buffer.items.len + 2);
                self.emitter.jcc(cond, 0) catch return CompileError.OutOfMemory;
                self.pending_jumps.append(self.allocator, .{
                    .native_offset = patch_offset,
                    .bytecode_target = target,
                    .is_conditional = true,
                }) catch return CompileError.OutOfMemory;
            }
        } else if (is_aarch64) {
            try self.emitPopReg(.x1);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x0, 0) catch return CompileError.OutOfMemory;

            const cond: arm64.Condition = if (jump_if_true) .ne else .eq;

            if (self.labels.get(target)) |native_offset| {
                const current = @as(i32, @intCast(self.emitter.buffer.items.len));
                const rel = @as(i32, @intCast(native_offset)) - current;
                self.emitter.bcond(cond, rel) catch return CompileError.OutOfMemory;
            } else {
                const patch_offset: u32 = @intCast(self.emitter.buffer.items.len);
                self.emitter.bcond(cond, 0) catch return CompileError.OutOfMemory;
                self.pending_jumps.append(self.allocator, .{
                    .native_offset = patch_offset,
                    .bytecode_target = target,
                    .is_conditional = true,
                }) catch return CompileError.OutOfMemory;
            }
        }
    }

    fn patchJumps(self: *BaselineCompiler) CompileError!void {
        for (self.pending_jumps.items) |jump| {
            const target_native = self.labels.get(jump.bytecode_target) orelse return CompileError.AllocationFailed;

            if (is_x86_64) {
                const rel = @as(i32, @intCast(target_native)) - @as(i32, @intCast(jump.native_offset)) - 4;
                const bytes: [4]u8 = @bitCast(rel);
                self.emitter.buffer.items[jump.native_offset] = bytes[0];
                self.emitter.buffer.items[jump.native_offset + 1] = bytes[1];
                self.emitter.buffer.items[jump.native_offset + 2] = bytes[2];
                self.emitter.buffer.items[jump.native_offset + 3] = bytes[3];
            } else if (is_aarch64) {
                const rel = @as(i32, @intCast(target_native)) - @as(i32, @intCast(jump.native_offset));
                const existing: u32 = @bitCast(self.emitter.buffer.items[jump.native_offset..][0..4].*);

                if (jump.is_conditional) {
                    // B.cond: patch imm19
                    const imm19: i19 = @intCast(@divExact(rel, 4));
                    const patched = (existing & 0xFF00001F) | (@as(u32, @as(u19, @bitCast(imm19))) << 5);
                    const bytes: [4]u8 = @bitCast(patched);
                    @memcpy(self.emitter.buffer.items[jump.native_offset..][0..4], &bytes);
                } else {
                    // B: patch imm26
                    const imm26: i26 = @intCast(@divExact(rel, 4));
                    const patched = (existing & 0xFC000000) | @as(u32, @as(u26, @bitCast(imm26)));
                    const bytes: [4]u8 = @bitCast(patched);
                    @memcpy(self.emitter.buffer.items[jump.native_offset..][0..4], &bytes);
                }
            }
        }
    }
};

fn readU16(code: []const u8, offset: u32) u16 {
    return @as(u16, code[offset]) | (@as(u16, code[offset + 1]) << 8);
}

/// Compile a function to native code
pub fn compileFunction(allocator: std.mem.Allocator, code_alloc: *CodeAllocator, func: *const bytecode.FunctionBytecode) CompileError!CompiledCode {
    var compiler = BaselineCompiler.init(allocator, code_alloc, func);
    defer compiler.deinit();
    return compiler.compile();
}

// ============================================================================
// Tests
// ============================================================================

test "baseline: compile simple return" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile arithmetic" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.add),
        @intFromEnum(Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile div/mod/pow/inc/dec" {
    const testing = std.testing;

    const binary_ops = [_]Opcode{ .div, .mod, .pow };
    for (binary_ops) |op| {
        const code = [_]u8{
            @intFromEnum(Opcode.push_2),
            @intFromEnum(Opcode.push_1),
            @intFromEnum(op),
            @intFromEnum(Opcode.ret),
        };

        var func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = 0,
            .stack_size = 2,
            .flags = .{},
            .code = &code,
            .constants = &.{},
            .source_map = null,
        };

        var code_alloc = CodeAllocator.init(testing.allocator);
        defer code_alloc.deinit();

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
        try testing.expect(compiled.code.len > 0);
    }

    const unary_ops = [_]Opcode{ .inc, .dec };
    for (unary_ops) |op| {
        const code = [_]u8{
            @intFromEnum(Opcode.push_1),
            @intFromEnum(op),
            @intFromEnum(Opcode.ret),
        };

        var func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = 0,
            .stack_size = 1,
            .flags = .{},
            .code = &code,
            .constants = &.{},
            .source_map = null,
        };

        var code_alloc = CodeAllocator.init(testing.allocator);
        defer code_alloc.deinit();

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
        try testing.expect(compiled.code.len > 0);
    }
}

test "baseline: compile call opcodes" {
    const testing = std.testing;

    const ops = [_]Opcode{ .call, .call_method, .tail_call };
    for (ops) |op| {
        const code = [_]u8{
            @intFromEnum(Opcode.push_undefined),
            @intFromEnum(op),
            0x00,
            @intFromEnum(Opcode.ret),
        };

        var func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = 0,
            .stack_size = 1,
            .flags = .{},
            .code = &code,
            .constants = &.{},
            .source_map = null,
        };

        var code_alloc = CodeAllocator.init(testing.allocator);
        defer code_alloc.deinit();

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
        try testing.expect(compiled.code.len > 0);
    }
}

test "baseline: compile property access opcodes" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.get_field), 0x01, 0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_field), 0x02, 0x00,
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_field_keep), 0x03, 0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.get_field_ic), 0x04, 0x00, 0x00, 0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_field_ic), 0x05, 0x00, 0x00, 0x00,
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.push_0),
        @intFromEnum(Opcode.get_elem),
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.push_0),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_elem),
        @intFromEnum(Opcode.ret_undefined),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 4,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile get_field_call opcode" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.dup),
        @intFromEnum(Opcode.get_field_call), 0x01, 0x00, 0x00,
        @intFromEnum(Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile globals and object creation" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.new_object),
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.new_array), 0x02, 0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.get_global), 0x01, 0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_global), 0x02, 0x00,
        @intFromEnum(Opcode.ret_undefined),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile superinstructions and const ops" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_loc_0),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.put_loc_1),

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.get_loc_add), 0x00,

        @intFromEnum(Opcode.get_loc_get_loc_add), 0x00, 0x01,

        @intFromEnum(Opcode.push_false),
        @intFromEnum(Opcode.if_false_goto), 0x00, 0x00,

        @intFromEnum(Opcode.push_const_call), 0x00, 0x00, 0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.add_mod), 0x01, 0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.sub_mod), 0x01, 0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.mul_mod), 0x01, 0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.mod_const), 0x01, 0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.mod_const_i8), 0x03,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.add_const_i8), 0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.sub_const_i8), 0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.mul_const_i8), 0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.lt_const_i8), 0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.le_const_i8), 0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.shr_1),

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.mul_2),

        @intFromEnum(Opcode.for_of_next), 0x00, 0x00,
        @intFromEnum(Opcode.for_of_next_put_loc), 0x00, 0x00, 0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.swap),
        @intFromEnum(Opcode.dup2),
        @intFromEnum(Opcode.rot3),
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.drop),

        @intFromEnum(Opcode.ret_undefined),
    };

    const constants = [_]value_mod.JSValue{
        value_mod.JSValue.undefined_val,
        value_mod.JSValue.fromInt(7),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 2,
        .stack_size = 6,
        .flags = .{},
        .code = &code,
        .constants = &constants,
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: unsupported opcode returns error" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.call_spread),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const result = compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expectError(CompileError.UnsupportedOpcode, result);
}

test "baseline: verify generated code size" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.ret_undefined),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 0,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);

    // Should have prologue + body + epilogue
    try testing.expect(compiled.code.len > 10);

    // ARM64 uses 4-byte instructions, x86-64 is variable length
    if (is_aarch64) {
        try testing.expect(@mod(compiled.code.len, 4) == 0);
    }
}

test "baseline: compile local variable access" {
    const testing = std.testing;

    // Bytecode for: let x = 1; let y = 2; return x + y;
    // push_1, put_loc_0, push_2, put_loc_1, get_loc_0, get_loc_1, add, ret
    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_loc_0),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.put_loc_1),
        @intFromEnum(Opcode.get_loc_0),
        @intFromEnum(Opcode.get_loc_1),
        @intFromEnum(Opcode.add),
        @intFromEnum(Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 2,
        .stack_size = 4,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile get_loc with index" {
    const testing = std.testing;

    // Bytecode using get_loc with explicit index
    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_loc),
        5, // local index 5
        @intFromEnum(Opcode.get_loc),
        5, // local index 5
        @intFromEnum(Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 6,
        .stack_size = 8,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile comparison opcodes" {
    const testing = std.testing;

    // Test all comparison opcodes: push 1, push 2, compare, ret
    const comparisons = [_]Opcode{ .lt, .lte, .gt, .gte, .eq, .neq, .strict_eq, .strict_neq };

    for (comparisons) |cmp_op| {
        const code = [_]u8{
            @intFromEnum(Opcode.push_1),
            @intFromEnum(Opcode.push_2),
            @intFromEnum(cmp_op),
            @intFromEnum(Opcode.ret),
        };

        var func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = 0,
            .stack_size = 2,
            .flags = .{},
            .code = &code,
            .constants = &.{},
            .source_map = null,
        };

        var code_alloc = CodeAllocator.init(testing.allocator);
        defer code_alloc.deinit();

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
        try testing.expect(compiled.code.len > 0);
    }
}

test "baseline: compile loop opcode" {
    const testing = std.testing;

    // Simple loop: push 1, loop back to start (loop offset -3)
    // Bytecode: push_1, loop -3
    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.loop),
        0xFD, // -3 as i16 little endian
        0xFF,
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile comparison with conditional jump" {
    const testing = std.testing;

    // Simple if: push 1, push 2, lt, if_true +1, push_0, ret
    // After if_true reads its 2-byte offset, pc is at 6. Jump target = 6 + 1 = 7 (the ret)
    // This tests comparisons work correctly with conditionals
    const code = [_]u8{
        @intFromEnum(Opcode.push_1), // 0
        @intFromEnum(Opcode.push_2), // 1
        @intFromEnum(Opcode.lt), // 2
        @intFromEnum(Opcode.if_true), // 3
        0x01, // +1 offset (little endian) // 4
        0x00, // 5
        @intFromEnum(Opcode.push_0), // 6
        @intFromEnum(Opcode.ret), // 7
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile bitwise operations" {
    const testing = std.testing;

    // Test all bitwise binary operations: push 1, push 2, op, ret
    const bitwise_ops = [_]Opcode{ .bit_and, .bit_or, .bit_xor };

    for (bitwise_ops) |bit_op| {
        const code = [_]u8{
            @intFromEnum(Opcode.push_1),
            @intFromEnum(Opcode.push_2),
            @intFromEnum(bit_op),
            @intFromEnum(Opcode.ret),
        };

        var func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = 0,
            .stack_size = 2,
            .flags = .{},
            .code = &code,
            .constants = &.{},
            .source_map = null,
        };

        var code_alloc = CodeAllocator.init(testing.allocator);
        defer code_alloc.deinit();

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
        try testing.expect(compiled.code.len > 0);
    }
}

test "baseline: compile bitwise NOT" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.bit_not),
        @intFromEnum(Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile shift operations" {
    const testing = std.testing;

    // Test all shift operations: push value, push shift amount, op, ret
    const shift_ops = [_]Opcode{ .shl, .shr, .ushr };

    for (shift_ops) |shift_op| {
        const code = [_]u8{
            @intFromEnum(Opcode.push_i8),
            16, // value = 16
            @intFromEnum(Opcode.push_2), // shift by 2
            @intFromEnum(shift_op),
            @intFromEnum(Opcode.ret),
        };

        var func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = 0,
            .stack_size = 2,
            .flags = .{},
            .code = &code,
            .constants = &.{},
            .source_map = null,
        };

        var code_alloc = CodeAllocator.init(testing.allocator);
        defer code_alloc.deinit();

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
        try testing.expect(compiled.code.len > 0);
    }
}

test "baseline: compile typeof operation" {
    const testing = std.testing;

    // typeof integer: push_1, typeof, ret
    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.typeof),
        @intFromEnum(Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile typeof with different values" {
    const testing = std.testing;

    // Test typeof with different value types
    const test_cases = [_]Opcode{
        .push_null, // typeof null -> "object"
        .push_undefined, // typeof undefined -> "undefined"
        .push_true, // typeof true -> "boolean"
        .push_false, // typeof false -> "boolean"
        .push_0, // typeof 0 -> "number"
    };

    for (test_cases) |push_op| {
        const code = [_]u8{
            @intFromEnum(push_op),
            @intFromEnum(Opcode.typeof),
            @intFromEnum(Opcode.ret),
        };

        var func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = 0,
            .stack_size = 1,
            .flags = .{},
            .code = &code,
            .constants = &.{},
            .source_map = null,
        };

        var code_alloc = CodeAllocator.init(testing.allocator);
        defer code_alloc.deinit();

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
        try testing.expect(compiled.code.len > 0);
    }
}

test "baseline: compile logical NOT operation" {
    const testing = std.testing;

    // !true -> false: push_true, not, ret
    const code = [_]u8{
        @intFromEnum(Opcode.push_true),
        @intFromEnum(Opcode.not),
        @intFromEnum(Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var code_alloc = CodeAllocator.init(testing.allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile logical NOT with falsy values" {
    const testing = std.testing;

    // Test !falsy values should produce true
    const falsy_ops = [_]Opcode{
        .push_false, // !false -> true
        .push_null, // !null -> true
        .push_undefined, // !undefined -> true
        .push_0, // !0 -> true
    };

    for (falsy_ops) |push_op| {
        const code = [_]u8{
            @intFromEnum(push_op),
            @intFromEnum(Opcode.not),
            @intFromEnum(Opcode.ret),
        };

        var func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = 0,
            .stack_size = 1,
            .flags = .{},
            .code = &code,
            .constants = &.{},
            .source_map = null,
        };

        var code_alloc = CodeAllocator.init(testing.allocator);
        defer code_alloc.deinit();

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func);
        try testing.expect(compiled.code.len > 0);
    }
}
