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

const CodeAllocator = alloc.CodeAllocator;
const CompiledCode = alloc.CompiledCode;
const Opcode = bytecode.Opcode;

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
        } else if (is_aarch64) {
            // ARM64 prologue: save frame pointer and link register
            self.emitter.stpPreIndex(.x29, .x30, .x29, -16) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x29, .x29) catch return CompileError.OutOfMemory; // Set frame pointer
            // Save callee-saved registers we use
            self.emitter.stpPreIndex(.x19, .x20, .x29, -16) catch return CompileError.OutOfMemory;
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
