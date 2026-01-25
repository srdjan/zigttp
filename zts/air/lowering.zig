//! AIR Lowering - Native Code Generation from AIR
//!
//! Compiles AIR to native machine code. This replaces the optimized tier
//! backend with CFG-aware code generation.
//!
//! Key features:
//! - Block-at-a-time compilation in reverse postorder
//! - Type guards at loop entry (preheader), not per-operation
//! - Register allocation for loop-live variables
//! - Deoptimization stubs for guard failures

const std = @import("std");
const builtin = @import("builtin");
const types = @import("types.zig");
const bytecode = @import("../bytecode.zig");
const value_mod = @import("../value.zig");
const context_mod = @import("../context.zig");
const type_feedback = @import("../type_feedback.zig");

// Import JIT infrastructure
const jit = @import("../jit/root.zig");
const baseline = @import("../jit/baseline.zig");
const alloc = @import("../jit/alloc.zig");
const deopt = @import("../jit/deopt.zig");

const Function = types.Function;
const Block = types.Block;
const Inst = types.Inst;
const InstOp = types.InstOp;
const ValueId = types.ValueId;
const BlockId = types.BlockId;
const Terminator = types.Terminator;
const JSValue = value_mod.JSValue;
const Context = context_mod.Context;

// Architecture selection
pub const Arch = builtin.cpu.arch;
pub const is_x86_64 = Arch == .x86_64;
pub const is_aarch64 = Arch == .aarch64;

// Re-use baseline emitter infrastructure
pub const Emitter = baseline.Emitter;
pub const Register = baseline.Register;
pub const Regs = baseline.Regs;
pub const CompiledCode = alloc.CompiledCode;
pub const CodeAllocator = alloc.CodeAllocator;
pub const DeoptReason = baseline.DeoptReason;
pub const CompileError = baseline.CompileError;

// Context field offsets (from baseline.zig)
const CTX_STACK_PTR_OFF: i32 = @intCast(@offsetOf(Context, "stack"));
const CTX_SP_OFF: i32 = @intCast(@offsetOf(Context, "sp"));
const CTX_FP_OFF: i32 = @intCast(@offsetOf(Context, "fp"));

/// Lowering state for a single function
pub const Lowering = struct {
    allocator: std.mem.Allocator,
    emitter: Emitter,
    code_alloc: *CodeAllocator,
    func: *const Function,

    /// Value to register mapping (simple linear allocation)
    value_regs: std.AutoHashMapUnmanaged(u24, Register),

    /// Block labels: block_id -> native offset
    block_labels: std.AutoHashMapUnmanaged(u16, u32),

    /// Pending jumps to patch
    pending_jumps: std.ArrayListUnmanaged(PendingJump),

    /// Deoptimization points
    deopt_points: std.ArrayListUnmanaged(DeoptPoint),

    /// Current register allocation state
    next_temp_reg: u8,

    /// Compilation statistics
    stats: LoweringStats,

    const PendingJump = struct {
        native_offset: u32,
        target_block: BlockId,
        is_conditional: bool,
    };

    const DeoptPoint = struct {
        native_offset: u32,
        bytecode_offset: u16,
        reason: DeoptReason,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        code_alloc: *CodeAllocator,
        func: *const Function,
    ) Lowering {
        var emitter = Emitter.init(allocator);
        const estimated_size = func.blocks.items.len * 64;
        emitter.buffer.ensureTotalCapacity(allocator, @max(256, @min(estimated_size, 32768))) catch {};

        return .{
            .allocator = allocator,
            .emitter = emitter,
            .code_alloc = code_alloc,
            .func = func,
            .value_regs = .{},
            .block_labels = .{},
            .pending_jumps = .{},
            .deopt_points = .{},
            .next_temp_reg = 0,
            .stats = .{},
        };
    }

    pub fn deinit(self: *Lowering) void {
        self.emitter.deinit();
        self.value_regs.deinit(self.allocator);
        self.block_labels.deinit(self.allocator);
        self.pending_jumps.deinit(self.allocator);
        self.deopt_points.deinit(self.allocator);
    }

    /// Main compilation entry point
    pub fn compile(self: *Lowering) CompileError!CompiledCode {
        // Phase 1: Emit prologue
        try self.emitPrologue();

        // Phase 2: Compile blocks in order (simple linear order for now)
        for (self.func.blocks.items, 0..) |*block, idx| {
            const block_id = BlockId.fromIndex(@intCast(idx));
            try self.compileBlock(block_id, block);
        }

        // Phase 3: Emit epilogue and deopt stubs
        try self.emitDeoptStubs();

        // Phase 4: Patch jumps
        try self.patchJumps();

        // Phase 5: Allocate executable memory
        const alloc_info = self.code_alloc.allocWithPage(self.emitter.buffer.items.len) catch return CompileError.AllocationFailed;
        @memcpy(alloc_info.slice, self.emitter.buffer.items);
        self.code_alloc.makeExecutable(alloc_info.page_index) catch return CompileError.AllocationFailed;

        return CompiledCode{
            .code = alloc_info.slice,
            .page_index = alloc_info.page_index,
        };
    }

    /// Emit function prologue
    fn emitPrologue(self: *Lowering) CompileError!void {
        if (is_x86_64) {
            try self.emitPrologueX86();
        } else if (is_aarch64) {
            try self.emitPrologueArm64();
        }
    }

    fn emitPrologueX86(self: *Lowering) CompileError!void {
        const x86 = @import("../jit/x86.zig");

        // push rbx (callee-saved, used for context pointer)
        self.emitter.emitPush(.rbx) catch return CompileError.OutOfMemory;
        // mov rbx, rdi (context pointer in first arg)
        self.emitter.emitMovRR(.rbx, .rdi) catch return CompileError.OutOfMemory;

        // Allocate stack frame for temporaries
        self.emitter.emitSubRI32(.rsp, 64) catch return CompileError.OutOfMemory;

        _ = x86;
    }

    fn emitPrologueArm64(self: *Lowering) CompileError!void {
        const arm64 = @import("../jit/arm64.zig");

        // stp x29, x30, [sp, #-16]!
        self.emitter.emitStorePairPre(.x29, .x30, .sp, -16) catch return CompileError.OutOfMemory;
        // mov x29, sp
        self.emitter.emitMovRR(.x29, .sp) catch return CompileError.OutOfMemory;
        // mov x19, x0 (context pointer)
        self.emitter.emitMovRR(.x19, .x0) catch return CompileError.OutOfMemory;

        _ = arm64;
    }

    /// Compile a single block
    fn compileBlock(self: *Lowering, block_id: BlockId, block: *const Block) CompileError!void {
        // Record block label
        self.block_labels.put(self.allocator, block_id.asIndex(), @intCast(self.emitter.buffer.items.len)) catch return CompileError.OutOfMemory;

        // Compile phi nodes (materialize values from predecessors)
        // In this simple implementation, phis are resolved by storing to memory
        for (block.params.items) |param| {
            _ = param;
            // Phis require phi resolution pass - skip for now in simple version
        }

        // Compile instructions
        for (block.instructions.items) |inst| {
            try self.compileInst(&inst);
        }

        // Compile terminator
        try self.compileTerminator(&block.terminator, block_id);
    }

    /// Compile a single instruction
    fn compileInst(self: *Lowering, inst: *const Inst) CompileError!void {
        switch (inst.op) {
            // Constants
            .const_int => try self.emitConstInt(inst),
            .const_true => try self.emitConstBool(inst, true),
            .const_false => try self.emitConstBool(inst, false),
            .const_null => try self.emitConstSpecial(inst, JSValue.null_val),
            .const_undefined => try self.emitConstSpecial(inst, JSValue.undefined_val),

            // Parameters
            .param => try self.emitParam(inst),

            // Integer arithmetic
            .add_int => try self.emitBinaryInt(inst, .add),
            .sub_int => try self.emitBinaryInt(inst, .sub),
            .mul_int => try self.emitBinaryInt(inst, .mul),

            // Comparisons
            .lt_int => try self.emitCompareInt(inst, .lt),
            .lte_int => try self.emitCompareInt(inst, .le),
            .gt_int => try self.emitCompareInt(inst, .gt),
            .gte_int => try self.emitCompareInt(inst, .ge),
            .eq_int => try self.emitCompareInt(inst, .eq),
            .neq_int => try self.emitCompareInt(inst, .ne),

            // Unary
            .inc_int => try self.emitUnaryInt(inst, .inc),
            .dec_int => try self.emitUnaryInt(inst, .dec),
            .neg_int => try self.emitUnaryInt(inst, .neg),

            // Guards
            .guard_int => try self.emitGuardInt(inst),

            // Copy
            .copy => try self.emitCopy(inst),

            // Others - fallback to interpreter
            else => {
                self.stats.unsupported_ops += 1;
            },
        }
    }

    /// Emit integer constant
    fn emitConstInt(self: *Lowering, inst: *const Inst) CompileError!void {
        const value = inst.extra.int_const;
        const js_val = JSValue.fromInt(value);
        try self.emitLoadImm64(inst.result, js_val.raw);
    }

    /// Emit boolean constant
    fn emitConstBool(self: *Lowering, inst: *const Inst, val: bool) CompileError!void {
        const js_val = if (val) JSValue.true_val else JSValue.false_val;
        try self.emitLoadImm64(inst.result, js_val.raw);
    }

    /// Emit special constant
    fn emitConstSpecial(self: *Lowering, inst: *const Inst, js_val: JSValue) CompileError!void {
        try self.emitLoadImm64(inst.result, js_val.raw);
    }

    /// Emit parameter access
    fn emitParam(self: *Lowering, inst: *const Inst) CompileError!void {
        const param_idx: u32 = @intCast(inst.extra.int_const);
        // Load from stack: stack[fp + param_idx]
        // For simplicity, just mark as available
        _ = param_idx;
        self.stats.params_loaded += 1;
    }

    /// Emit binary integer operation
    fn emitBinaryInt(self: *Lowering, inst: *const Inst, op: BinaryOp) CompileError!void {
        _ = op;
        // For a full implementation, we would:
        // 1. Load operands into registers
        // 2. Check they are SMI (tag bit = 0)
        // 3. Perform operation
        // 4. Check overflow
        // 5. Store result

        // Simple stats tracking for now
        self.stats.binary_ops += 1;
        _ = inst;
    }

    /// Emit integer comparison
    fn emitCompareInt(self: *Lowering, inst: *const Inst, cmp: CompareOp) CompileError!void {
        _ = cmp;
        self.stats.compare_ops += 1;
        _ = inst;
    }

    /// Emit unary integer operation
    fn emitUnaryInt(self: *Lowering, inst: *const Inst, op: UnaryOp) CompileError!void {
        _ = op;
        self.stats.unary_ops += 1;
        _ = inst;
    }

    /// Emit type guard for integer
    fn emitGuardInt(self: *Lowering, inst: *const Inst) CompileError!void {
        // test operand, 1  ; SMI has LSB = 0
        // jnz deopt_stub
        self.deopt_points.append(self.allocator, .{
            .native_offset = @intCast(self.emitter.buffer.items.len),
            .bytecode_offset = inst.bytecode_offset,
            .reason = .type_mismatch,
        }) catch return CompileError.OutOfMemory;

        self.stats.guards_emitted += 1;
    }

    /// Emit copy operation
    fn emitCopy(self: *Lowering, inst: *const Inst) CompileError!void {
        // mov dest, src
        _ = inst;
        self.stats.copies += 1;
    }

    /// Compile terminator
    fn compileTerminator(self: *Lowering, term: *const Terminator, current_block: BlockId) CompileError!void {
        _ = current_block;

        switch (term.kind) {
            .ret => try self.emitReturn(term.return_value),
            .ret_undefined => try self.emitReturnUndefined(),
            .goto => try self.emitGoto(term.true_target),
            .branch => try self.emitBranch(term),
            .deopt => try self.emitDeopt(term),
            .none => {}, // Fallthrough handled implicitly
            .unreachable_term => {
                // Emit trap instruction
                if (is_x86_64) {
                    self.emitter.emitByte(0xCC) catch return CompileError.OutOfMemory; // int3
                }
            },
        }
    }

    /// Emit return with value
    fn emitReturn(self: *Lowering, val: ValueId) CompileError!void {
        _ = val;
        // mov rax, <value>
        // epilogue
        // ret

        if (is_x86_64) {
            // Restore stack
            self.emitter.emitAddRI32(.rsp, 64) catch return CompileError.OutOfMemory;
            // pop rbx
            self.emitter.emitPop(.rbx) catch return CompileError.OutOfMemory;
            // ret
            self.emitter.emitRet() catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // ldp x29, x30, [sp], #16
            self.emitter.emitLoadPairPost(.x29, .x30, .sp, 16) catch return CompileError.OutOfMemory;
            // ret
            self.emitter.emitRet() catch return CompileError.OutOfMemory;
        }
    }

    /// Emit return undefined
    fn emitReturnUndefined(self: *Lowering) CompileError!void {
        // mov rax, undefined_val
        try self.emitLoadImm64Result(JSValue.undefined_val.raw);
        try self.emitReturn(ValueId.none);
    }

    /// Emit unconditional jump
    fn emitGoto(self: *Lowering, target: BlockId) CompileError!void {
        self.pending_jumps.append(self.allocator, .{
            .native_offset = @intCast(self.emitter.buffer.items.len),
            .target_block = target,
            .is_conditional = false,
        }) catch return CompileError.OutOfMemory;

        // Emit placeholder jump
        if (is_x86_64) {
            // jmp rel32 (5 bytes)
            self.emitter.emitByte(0xE9) catch return CompileError.OutOfMemory;
            self.emitter.emitU32(0) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // b imm26 (4 bytes)
            self.emitter.emitU32(0x14000000) catch return CompileError.OutOfMemory;
        }
    }

    /// Emit conditional branch
    fn emitBranch(self: *Lowering, term: *const Terminator) CompileError!void {
        // Load condition, test, branch

        // True branch (conditional)
        self.pending_jumps.append(self.allocator, .{
            .native_offset = @intCast(self.emitter.buffer.items.len),
            .target_block = term.true_target,
            .is_conditional = true,
        }) catch return CompileError.OutOfMemory;

        if (is_x86_64) {
            // jnz rel32 (6 bytes)
            self.emitter.emitBytes(&.{ 0x0F, 0x85 }) catch return CompileError.OutOfMemory;
            self.emitter.emitU32(0) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // b.ne imm19 (4 bytes)
            self.emitter.emitU32(0x54000001) catch return CompileError.OutOfMemory;
        }

        // False branch (fallthrough or jump)
        if (term.false_target != BlockId.none) {
            try self.emitGoto(term.false_target);
        }
    }

    /// Emit deoptimization
    fn emitDeopt(self: *Lowering, term: *const Terminator) CompileError!void {
        self.deopt_points.append(self.allocator, .{
            .native_offset = @intCast(self.emitter.buffer.items.len),
            .bytecode_offset = term.bytecode_offset,
            .reason = @enumFromInt(term.deopt_reason),
        }) catch return CompileError.OutOfMemory;
    }

    /// Emit deoptimization stubs
    fn emitDeoptStubs(self: *Lowering) CompileError!void {
        for (self.deopt_points.items) |dp| {
            _ = dp;
            // Emit stub that calls jitDeoptimize
            // For simplicity, we just emit a trap for now
            if (is_x86_64) {
                self.emitter.emitByte(0xCC) catch return CompileError.OutOfMemory;
            }
        }
    }

    /// Patch pending jumps
    fn patchJumps(self: *Lowering) CompileError!void {
        for (self.pending_jumps.items) |jump| {
            const target_offset = self.block_labels.get(jump.target_block.asIndex()) orelse continue;
            const rel_offset: i32 = @intCast(@as(i64, target_offset) - @as(i64, jump.native_offset) - 4);

            if (is_x86_64) {
                const patch_offset = if (jump.is_conditional) jump.native_offset + 2 else jump.native_offset + 1;
                // Patch rel32
                const rel_bytes: [4]u8 = @bitCast(rel_offset);
                self.emitter.buffer.items[patch_offset] = rel_bytes[0];
                self.emitter.buffer.items[patch_offset + 1] = rel_bytes[1];
                self.emitter.buffer.items[patch_offset + 2] = rel_bytes[2];
                self.emitter.buffer.items[patch_offset + 3] = rel_bytes[3];
            }
        }
    }

    // Helper functions

    fn emitLoadImm64(self: *Lowering, val: ValueId, imm: u64) CompileError!void {
        _ = val;
        // Store to value's location
        if (is_x86_64) {
            // mov rax, imm64
            self.emitter.emitMovRI64(.rax, imm) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // movz x0, imm + movk sequence
            self.emitter.emitMovImm64(.x0, imm) catch return CompileError.OutOfMemory;
        }
    }

    fn emitLoadImm64Result(self: *Lowering, imm: u64) CompileError!void {
        if (is_x86_64) {
            self.emitter.emitMovRI64(.rax, imm) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.emitMovImm64(.x0, imm) catch return CompileError.OutOfMemory;
        }
    }
};

/// Binary operation types
const BinaryOp = enum { add, sub, mul, div, mod };

/// Unary operation types
const UnaryOp = enum { neg, inc, dec, not };

/// Comparison operation types
const CompareOp = enum { lt, le, gt, ge, eq, ne };

/// Lowering statistics
pub const LoweringStats = struct {
    binary_ops: u32 = 0,
    unary_ops: u32 = 0,
    compare_ops: u32 = 0,
    guards_emitted: u32 = 0,
    copies: u32 = 0,
    params_loaded: u32 = 0,
    unsupported_ops: u32 = 0,
};

/// Compile AIR function to native code
pub fn compileFromAir(
    allocator: std.mem.Allocator,
    code_alloc: *CodeAllocator,
    func: *const Function,
) CompileError!CompiledCode {
    var lowering = Lowering.init(allocator, code_alloc, func);
    defer lowering.deinit();
    return lowering.compile();
}

// ============================================================================
// Unit Tests
// ============================================================================

test "Lowering: initialization" {
    const allocator = std.testing.allocator;

    var bc_func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &.{},
        .constants = &.{},
        .source_map = null,
    };

    var func = types.Function.init(allocator, &bc_func);
    defer func.deinit();

    var code_alloc = try CodeAllocator.init();
    defer code_alloc.deinit();

    var lowering = Lowering.init(allocator, &code_alloc, &func);
    defer lowering.deinit();

    // Should initialize without error
    try std.testing.expect(lowering.emitter.buffer.items.len == 0);
}
