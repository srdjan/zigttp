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
            self.emitter.stpPreIndex(.x29, .x30, .x29, -16) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x29, .x29) catch return CompileError.OutOfMemory; // Set frame pointer
            // Save callee-saved registers we use
            self.emitter.stpPreIndex(.x19, .x20, .x29, -16) catch return CompileError.OutOfMemory;
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
            self.emitter.ldpPostIndex(.x19, .x20, .x29, 16) catch return CompileError.OutOfMemory;
            self.emitter.ldpPostIndex(.x29, .x30, .x29, 16) catch return CompileError.OutOfMemory;
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

            .drop => {
                try self.emitDrop();
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

            .neg => {
                try self.emitNeg();
            },

            .ret => {
                try self.emitPop(.ret);
                try self.emitEpilogue();
            },

            .ret_undefined => {
                try self.emitMovImm64(.ret, @bitCast(value_mod.JSValue.undefined_val));
                try self.emitEpilogue();
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

    fn emitMovImm64(self: *BaselineCompiler, reg: RegAlias, imm: u64) CompileError!void {
        const r = getReg(reg);
        self.emitter.movRegImm64(r, imm) catch return CompileError.OutOfMemory;
    }

    fn emitPushImm64(self: *BaselineCompiler, imm: u64) CompileError!void {
        if (is_x86_64) {
            self.emitter.movRegImm64(.rax, imm) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.movRegImm64(.x9, imm) catch return CompileError.OutOfMemory;
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
        }
    }

    fn emitPop(self: *BaselineCompiler, dst: RegAlias) CompileError!void {
        const r = getReg(dst);
        if (is_x86_64) {
            self.emitter.popReg(r) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.ldrPostIndex(r, .x29, 8) catch return CompileError.OutOfMemory;
        }
    }

    fn emitDup(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            self.emitter.movRegMem(.rax, .rsp, 0) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.ldrImm(.x9, .x29, 0) catch return CompileError.OutOfMemory;
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
        }
    }

    fn emitDrop(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            self.emitter.addRegImm32(.rsp, 8) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.addRegImm12(.x29, .x29, 8) catch return CompileError.OutOfMemory;
        }
    }

    fn emitBinaryOp(self: *BaselineCompiler, op: BinaryOp) CompileError!void {
        if (is_x86_64) {
            self.emitter.popReg(.rcx) catch return CompileError.OutOfMemory; // right
            self.emitter.popReg(.rax) catch return CompileError.OutOfMemory; // left
            switch (op) {
                .add => self.emitter.addRegReg(.rax, .rcx) catch return CompileError.OutOfMemory,
                .sub => self.emitter.subRegReg(.rax, .rcx) catch return CompileError.OutOfMemory,
                .mul => self.emitter.imulRegReg(.rax, .rcx) catch return CompileError.OutOfMemory,
            }
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.ldrPostIndex(.x10, .x29, 8) catch return CompileError.OutOfMemory; // right
            self.emitter.ldrPostIndex(.x9, .x29, 8) catch return CompileError.OutOfMemory; // left
            switch (op) {
                .add => self.emitter.addRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
                .sub => self.emitter.subRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
                .mul => self.emitter.mulRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
            }
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
        }
    }

    fn emitNeg(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            self.emitter.popReg(.rax) catch return CompileError.OutOfMemory;
            self.emitter.negReg(.rax) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.ldrPostIndex(.x9, .x29, 8) catch return CompileError.OutOfMemory;
            self.emitter.negReg(.x9, .x9) catch return CompileError.OutOfMemory;
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
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
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
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
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
        }
    }

    /// Emit code to pop a value from the JS operand stack and store it in a local variable
    /// Local variables are stored at ctx->stack[ctx->fp + idx]
    fn emitPutLocal(self: *BaselineCompiler, idx: u8) CompileError!void {
        if (is_x86_64) {
            // Pop value from operand stack into rax
            self.emitter.popReg(.rax) catch return CompileError.OutOfMemory;
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
            self.emitter.ldrPostIndex(.x9, .x29, 8) catch return CompileError.OutOfMemory;
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

    const ComparisonOp = enum { lt, lte, gt, gte, eq, neq };

    /// Emit integer comparison with fast path
    /// Compares two values on the operand stack (both must be integers)
    /// Result is pushed as JSValue.true_val or JSValue.false_val
    fn emitComparison(self: *BaselineCompiler, op: ComparisonOp) CompileError!void {
        if (is_x86_64) {
            // Pop right operand into rcx, left into rax
            self.emitter.popReg(.rcx) catch return CompileError.OutOfMemory; // right
            self.emitter.popReg(.rax) catch return CompileError.OutOfMemory; // left

            // For integer values: raw >> 1 gives the signed integer
            // Shift both by 1 to get actual integer values
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;

            // Compare
            self.emitter.cmpRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.rax, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rdx, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            // Conditionally select based on comparison result
            const cond: x86.X86Emitter.Condition = switch (op) {
                .lt => .l,
                .lte => .le,
                .gt => .g,
                .gte => .ge,
                .eq => .e,
                .neq => .ne,
            };
            self.emitter.cmovcc(cond, .rax, .rdx) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Pop right operand into x10, left into x9
            self.emitter.ldrPostIndex(.x10, .x29, 8) catch return CompileError.OutOfMemory; // right
            self.emitter.ldrPostIndex(.x9, .x29, 8) catch return CompileError.OutOfMemory; // left

            // Shift both by 1 to get actual integer values
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;

            // Compare
            self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x12, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            // Conditionally select based on comparison result
            const cond: arm64.Condition = switch (op) {
                .lt => .lt,
                .lte => .le,
                .gt => .gt,
                .gte => .ge,
                .eq => .eq,
                .neq => .ne,
            };
            self.emitter.csel(.x9, .x12, .x11, cond) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
        }
    }

    /// Emit strict equality comparison
    /// For strict equality, we can compare raw values directly (same type and value)
    fn emitStrictEquality(self: *BaselineCompiler, negate: bool) CompileError!void {
        if (is_x86_64) {
            // Pop right operand into rcx, left into rax
            self.emitter.popReg(.rcx) catch return CompileError.OutOfMemory; // right
            self.emitter.popReg(.rax) catch return CompileError.OutOfMemory; // left

            // Compare raw values
            self.emitter.cmpRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.rax, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rdx, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            // Conditionally select: for strict_eq use .e, for strict_neq use .ne
            const cond: x86.X86Emitter.Condition = if (negate) .ne else .e;
            self.emitter.cmovcc(cond, .rax, .rdx) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Pop right operand into x10, left into x9
            self.emitter.ldrPostIndex(.x10, .x29, 8) catch return CompileError.OutOfMemory; // right
            self.emitter.ldrPostIndex(.x9, .x29, 8) catch return CompileError.OutOfMemory; // left

            // Compare raw values
            self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x12, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            // Conditionally select: for strict_eq use .eq, for strict_neq use .ne
            const cond: arm64.Condition = if (negate) .ne else .eq;
            self.emitter.csel(.x9, .x12, .x11, cond) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
        }
    }

    const BitwiseOp = enum { @"and", @"or", xor };

    /// Emit bitwise operation (AND, OR, XOR)
    /// For NaN-boxed integers: extract int32, apply op, re-encode
    fn emitBitwiseOp(self: *BaselineCompiler, op: BitwiseOp) CompileError!void {
        if (is_x86_64) {
            // Pop right operand into rcx, left into rax
            self.emitter.popReg(.rcx) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.rax) catch return CompileError.OutOfMemory;

            // Extract integers (shift right by 1)
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;

            // Apply bitwise operation
            switch (op) {
                .@"and" => self.emitter.andRegReg(.rax, .rcx) catch return CompileError.OutOfMemory,
                .@"or" => self.emitter.orRegReg(.rax, .rcx) catch return CompileError.OutOfMemory,
                .xor => self.emitter.xorRegReg(.rax, .rcx) catch return CompileError.OutOfMemory,
            }

            // Re-encode as JSValue (shift left by 1)
            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Pop right operand into x10, left into x9
            self.emitter.ldrPostIndex(.x10, .x29, 8) catch return CompileError.OutOfMemory;
            self.emitter.ldrPostIndex(.x9, .x29, 8) catch return CompileError.OutOfMemory;

            // Extract integers (shift right by 1)
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;

            // Apply bitwise operation
            switch (op) {
                .@"and" => self.emitter.andRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
                .@"or" => self.emitter.orrRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
                .xor => self.emitter.eorRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
            }

            // Re-encode as JSValue (shift left by 1)
            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
        }
    }

    /// Emit bitwise NOT
    fn emitBitwiseNot(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            // Pop operand into rax
            self.emitter.popReg(.rax) catch return CompileError.OutOfMemory;

            // Extract integer (shift right by 1)
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;

            // Apply NOT
            self.emitter.notReg(.rax) catch return CompileError.OutOfMemory;

            // Re-encode as JSValue (shift left by 1)
            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Pop operand into x9
            self.emitter.ldrPostIndex(.x9, .x29, 8) catch return CompileError.OutOfMemory;

            // Extract integer (shift right by 1)
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;

            // Apply NOT
            self.emitter.mvnReg(.x9, .x9) catch return CompileError.OutOfMemory;

            // Re-encode as JSValue (shift left by 1)
            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
        }
    }

    const ShiftOp = enum { shl, shr, ushr };

    /// Emit shift operation (SHL, SHR, USHR)
    fn emitShift(self: *BaselineCompiler, op: ShiftOp) CompileError!void {
        if (is_x86_64) {
            // Pop shift amount into rcx, value into rax
            self.emitter.popReg(.rcx) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.rax) catch return CompileError.OutOfMemory;

            // Extract integers (shift right by 1)
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;

            // Mask shift amount to 5 bits (0x1F) - JS spec
            self.emitter.andRegImm32(.rcx, 0x1F) catch return CompileError.OutOfMemory;

            // Apply shift (rcx holds the count)
            switch (op) {
                .shl => self.emitter.shlRegCl(.rax) catch return CompileError.OutOfMemory,
                .shr => self.emitter.sarRegCl(.rax) catch return CompileError.OutOfMemory,
                .ushr => self.emitter.shrRegCl(.rax) catch return CompileError.OutOfMemory,
            }

            // Re-encode as JSValue (shift left by 1)
            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.pushReg(.rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Pop shift amount into x10, value into x9
            self.emitter.ldrPostIndex(.x10, .x29, 8) catch return CompileError.OutOfMemory;
            self.emitter.ldrPostIndex(.x9, .x29, 8) catch return CompileError.OutOfMemory;

            // Extract integers (shift right by 1)
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;

            // Mask shift amount to 5 bits (0x1F) - JS spec
            self.emitter.andRegImm(.x10, .x10, 0x1F) catch return CompileError.OutOfMemory;

            // Apply shift
            switch (op) {
                .shl => self.emitter.lslRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
                .shr => self.emitter.asrRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
                .ushr => self.emitter.lsrRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory,
            }

            // Re-encode as JSValue (shift left by 1)
            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;

            // Push result
            self.emitter.strPreIndex(.x9, .x29, -8) catch return CompileError.OutOfMemory;
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
        // Pop condition and compare with false
        if (is_x86_64) {
            self.emitter.popReg(.rax) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rcx, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;

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
            self.emitter.ldrPostIndex(.x9, .x29, 8) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x10, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;

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

test "baseline: unsupported opcode returns error" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.call),
        0,
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
