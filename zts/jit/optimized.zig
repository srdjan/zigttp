//! Optimized JIT Tier Compiler
//!
//! Generates type-specialized native code for hot loops detected by the baseline tier.
//! Uses loop-level type guards instead of per-operation guards to eliminate redundant checks.
//!
//! Key optimizations:
//! - Single type check at loop entry instead of per-operation checks
//! - Unboxed integer arithmetic in loop body (SMI tag removed at entry, restored at exit)
//! - Register allocation for loop-live variables (avoids memory traffic)
//! - Overflow-only guards (no type checks inside loop)
//!
//! Deoptimization:
//! - Entry type mismatch: Fall back to baseline JIT
//! - Overflow during computation: Rebox values and resume in interpreter

const std = @import("std");
const builtin = @import("builtin");
const x86 = @import("x86.zig");
const arm64 = @import("arm64.zig");
const alloc = @import("alloc.zig");
const deopt = @import("deopt.zig");
const baseline = @import("baseline.zig");
const bytecode = @import("../bytecode.zig");
const value_mod = @import("../value.zig");
const context_mod = @import("../context.zig");
const object = @import("../object.zig");
const type_feedback = @import("../type_feedback.zig");

const CodeAllocator = alloc.CodeAllocator;
const CompiledCode = alloc.CompiledCode;
const Opcode = bytecode.Opcode;
const Context = context_mod.Context;

// Re-use baseline definitions
pub const Arch = baseline.Arch;
pub const is_x86_64 = baseline.is_x86_64;
pub const is_aarch64 = baseline.is_aarch64;
pub const Emitter = baseline.Emitter;
pub const Register = baseline.Register;
pub const Regs = baseline.Regs;
pub const CompileError = baseline.CompileError;
pub const DeoptReason = baseline.DeoptReason;
pub const CompiledFn = baseline.CompiledFn;

const jitDeoptimize = deopt.jitDeoptimize;

/// Maximum number of loops we can optimize in a single function
pub const MAX_OPTIMIZED_LOOPS: usize = 8;

/// Maximum number of locals that can be register-allocated for a loop
pub const MAX_LOOP_REG_LOCALS: usize = if (is_x86_64) 4 else 6;

/// x86-64 registers available for loop variable allocation
/// These are callee-saved registers not used by baseline JIT:
/// rbx = context pointer, r12-r15 = available for loop locals
const X86_LOOP_REGS = [_]x86.Register{ .r12, .r13, .r14, .r15 };

/// ARM64 registers available for loop variable allocation
const ARM64_LOOP_REGS = [_]arm64.Register{ .x23, .x24, .x25, .x26, .x27, .x28 };

/// Loop optimization state
pub const OptimizedLoop = struct {
    /// Loop info from type feedback analysis
    info: type_feedback.LoopInfo,
    /// Native code offset of loop entry (after type checks)
    entry_offset: u32,
    /// Native code offset of deopt stub for this loop
    deopt_stub_offset: u32,
    /// Register assignments for loop-local variables
    /// Index is local_idx, value is register index (0-3 for x86, 0-5 for ARM64)
    local_to_reg: [16]?u8,
    /// Number of locals allocated to registers
    reg_local_count: u8,
    /// Whether this loop was successfully optimized
    is_optimized: bool,

    pub fn init(info: type_feedback.LoopInfo) OptimizedLoop {
        return .{
            .info = info,
            .entry_offset = 0,
            .deopt_stub_offset = 0,
            .local_to_reg = .{null} ** 16,
            .reg_local_count = 0,
            .is_optimized = false,
        };
    }

    /// Get the register assigned to a local variable in this loop
    pub fn getLocalReg(self: *const OptimizedLoop, local_idx: u8) ?Register {
        if (local_idx >= 16) return null;
        const reg_idx = self.local_to_reg[local_idx] orelse return null;

        if (is_x86_64) {
            if (reg_idx < X86_LOOP_REGS.len) {
                return X86_LOOP_REGS[reg_idx];
            }
        } else if (is_aarch64) {
            if (reg_idx < ARM64_LOOP_REGS.len) {
                return ARM64_LOOP_REGS[reg_idx];
            }
        }
        return null;
    }

    /// Assign registers to the most frequently accessed locals in this loop
    pub fn assignRegisters(self: *OptimizedLoop) void {
        // Count accesses per local from the bitmask
        const max_regs: u8 = if (is_x86_64) @intCast(X86_LOOP_REGS.len) else @intCast(ARM64_LOOP_REGS.len);
        var assigned: u8 = 0;

        // Simple greedy assignment: assign registers to accessed locals in order
        var local_idx: u8 = 0;
        while (local_idx < 16 and assigned < max_regs) : (local_idx += 1) {
            if (self.info.isLocalAccessed(local_idx)) {
                self.local_to_reg[local_idx] = assigned;
                assigned += 1;
            }
        }
        self.reg_local_count = assigned;
    }
};

/// Optimized tier compiler state
pub const OptimizedCompiler = struct {
    /// Underlying emitter (shared with baseline)
    emitter: Emitter,
    /// Code allocator
    code_alloc: *CodeAllocator,
    /// Function being compiled
    func: *const bytecode.FunctionBytecode,
    /// Memory allocator
    allocator: std.mem.Allocator,

    /// Detected loops suitable for optimization
    loops: [MAX_OPTIMIZED_LOOPS]OptimizedLoop,
    /// Number of detected loops
    loop_count: u8,
    /// Currently compiling loop index (or null if not in a loop)
    current_loop_idx: ?u8,

    /// Type feedback data
    tf: ?*type_feedback.TypeFeedback,
    feedback_site_map: ?[]u16,

    /// Label management
    labels: std.AutoHashMapUnmanaged(u32, u32),
    pending_jumps: std.ArrayListUnmanaged(PendingJump),
    jump_targets: std.AutoHashMapUnmanaged(u32, void),
    next_local_label: u32,

    /// Deoptimization points
    deopt_points: std.ArrayListUnmanaged(DeoptPoint),

    const PendingJump = struct {
        native_offset: u32,
        bytecode_target: u32,
        is_conditional: bool,
    };

    const DeoptPoint = struct {
        native_offset: u32,
        bytecode_offset: u32,
        reason: DeoptReason,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        code_alloc: *CodeAllocator,
        func: *const bytecode.FunctionBytecode,
    ) OptimizedCompiler {
        var emitter = Emitter.init(allocator);
        const estimated_size = func.code.len * 8; // Optimized code may be larger
        emitter.buffer.ensureTotalCapacity(allocator, @max(512, @min(estimated_size, 65536))) catch {};

        return .{
            .emitter = emitter,
            .code_alloc = code_alloc,
            .func = func,
            .allocator = allocator,
            .loops = undefined,
            .loop_count = 0,
            .current_loop_idx = null,
            .tf = func.type_feedback_ptr,
            .feedback_site_map = func.feedback_site_map,
            .labels = .{},
            .pending_jumps = .{},
            .jump_targets = .{},
            .next_local_label = 0,
            .deopt_points = .{},
        };
    }

    pub fn deinit(self: *OptimizedCompiler) void {
        self.emitter.deinit();
        self.labels.deinit(self.allocator);
        self.pending_jumps.deinit(self.allocator);
        self.jump_targets.deinit(self.allocator);
        self.deopt_points.deinit(self.allocator);
    }

    /// Detect loops in the bytecode and analyze their type characteristics
    fn detectLoops(self: *OptimizedCompiler) CompileError!void {
        const code = self.func.code;
        var pc: u32 = 0;

        while (pc < code.len and self.loop_count < MAX_OPTIMIZED_LOOPS) {
            const op: Opcode = @enumFromInt(code[pc]);
            const op_offset = pc;
            pc += 1;

            switch (op) {
                .loop, .goto => {
                    // Loop/goto opcode: check if backward jump (loop back-edge)
                    if (pc + 2 <= code.len) {
                        const offset: i16 = @bitCast(readU16(code, pc));
                        pc += 2;

                        // Only consider backward jumps as loop candidates
                        if (offset < 0) {
                            // Calculate loop header offset (target of backward jump)
                            const back_edge_offset = op_offset;
                            const header_offset: u32 = @intCast(@as(i32, @intCast(pc)) + offset);

                            // Create loop info and analyze it
                            var loop_info = type_feedback.LoopInfo.init(header_offset, back_edge_offset);
                            self.analyzeLoop(&loop_info, header_offset, back_edge_offset);

                            // Check if loop is suitable for optimization
                            if (self.isLoopOptimizable(&loop_info)) {
                                self.loops[self.loop_count] = OptimizedLoop.init(loop_info);
                                self.loops[self.loop_count].assignRegisters();
                                self.loop_count += 1;
                            }
                        }
                    } else {
                        pc += 2;
                    }
                },
                // Skip other opcodes
                .if_true, .if_false, .if_false_goto => pc += 2,
                .for_of_next => pc += 2,
                .for_of_next_put_loc => pc += 3,
                .push_const => pc += 2,
                .push_i8, .get_loc, .put_loc => pc += 1,
                .push_i16 => pc += 2,
                .new_array, .get_field, .put_field, .put_field_keep, .get_global, .put_global => pc += 2,
                .get_field_ic, .put_field_ic => pc += 4,
                .call, .call_method, .tail_call => pc += 1,
                .get_loc_add => pc += 1,
                .get_loc_get_loc_add => pc += 2,
                .add_mod, .sub_mod, .mul_mod, .mod_const => pc += 2,
                .mod_const_i8, .add_const_i8, .sub_const_i8, .mul_const_i8, .lt_const_i8, .le_const_i8 => pc += 1,
                .get_upvalue, .put_upvalue, .close_upvalue => pc += 1,
                .get_field_call => pc += 3,
                .push_const_call => pc += 3,
                .make_closure => pc += 4,
                else => {},
            }
        }
    }

    /// Analyze a loop to collect binary operation sites and accessed locals
    fn analyzeLoop(
        self: *OptimizedCompiler,
        loop_info: *type_feedback.LoopInfo,
        header: u32,
        back_edge: u32,
    ) void {
        const code = self.func.code;
        const site_map = self.feedback_site_map orelse return;
        var pc = header;

        while (pc <= back_edge and pc < code.len) {
            const op: Opcode = @enumFromInt(code[pc]);
            const op_offset = pc;
            pc += 1;

            switch (op) {
                // Binary operations that can be type-specialized
                .add, .sub, .mul => {
                    if (op_offset < site_map.len) {
                        const site_idx = site_map[op_offset];
                        if (site_idx != 0xFFFF) {
                            loop_info.addBinaryOpSite(site_idx);
                        }
                    }
                },
                // Local variable access
                .get_loc_0, .put_loc_0 => loop_info.markLocalAccessed(0),
                .get_loc_1, .put_loc_1 => loop_info.markLocalAccessed(1),
                .get_loc_2, .put_loc_2 => loop_info.markLocalAccessed(2),
                .get_loc_3, .put_loc_3 => loop_info.markLocalAccessed(3),
                .get_loc, .put_loc => {
                    if (pc < code.len) {
                        loop_info.markLocalAccessed(code[pc]);
                        pc += 1;
                    }
                },
                .get_loc_add => {
                    if (pc < code.len) {
                        loop_info.markLocalAccessed(code[pc]);
                        pc += 1;
                    }
                    // Also record as binary op
                    if (op_offset < site_map.len) {
                        const site_idx = site_map[op_offset];
                        if (site_idx != 0xFFFF) {
                            loop_info.addBinaryOpSite(site_idx);
                        }
                    }
                },
                .get_loc_get_loc_add => {
                    if (pc + 1 < code.len) {
                        loop_info.markLocalAccessed(code[pc]);
                        loop_info.markLocalAccessed(code[pc + 1]);
                        pc += 2;
                    }
                    // Also record as binary op
                    if (op_offset < site_map.len) {
                        const site_idx = site_map[op_offset];
                        if (site_idx != 0xFFFF) {
                            loop_info.addBinaryOpSite(site_idx);
                        }
                    }
                },
                // Operations that prevent optimization (side effects)
                .call, .call_method, .tail_call => {
                    loop_info.has_side_effects = true;
                    pc += 1;
                },
                .get_field, .put_field, .get_field_ic, .put_field_ic => {
                    loop_info.has_side_effects = true;
                    pc += switch (op) {
                        .get_field, .put_field => 2,
                        .get_field_ic, .put_field_ic => 4,
                        else => 0,
                    };
                },
                .make_closure => {
                    loop_info.has_side_effects = true;
                    pc += 4;
                },
                // Skip other opcodes
                .loop, .goto, .if_true, .if_false => pc += 2,
                .if_false_goto, .for_of_next => pc += 2,
                .for_of_next_put_loc => pc += 3,
                .push_const => pc += 2,
                .push_i8 => pc += 1,
                .push_i16 => pc += 2,
                .new_array, .put_field_keep, .get_global, .put_global => pc += 2,
                .add_mod, .sub_mod, .mul_mod, .mod_const => pc += 2,
                .mod_const_i8, .add_const_i8, .sub_const_i8, .mul_const_i8, .lt_const_i8, .le_const_i8 => pc += 1,
                .get_upvalue, .put_upvalue, .close_upvalue => pc += 1,
                .get_field_call => pc += 3,
                .push_const_call => pc += 3,
                else => {},
            }
        }
    }

    /// Check if a loop is suitable for optimized compilation
    fn isLoopOptimizable(self: *OptimizedCompiler, loop_info: *type_feedback.LoopInfo) bool {
        // Must have type feedback
        const tf = self.tf orelse return false;
        const site_map = self.feedback_site_map orelse return false;

        // Analyze types using type feedback
        if (!type_feedback.analyzeLoopTypes(tf, site_map, loop_info)) {
            return false;
        }

        return type_feedback.isLoopOptimizable(loop_info);
    }

    /// Find jump targets in the bytecode
    fn findJumpTargets(self: *OptimizedCompiler) CompileError!void {
        const code = self.func.code;
        var pc: u32 = 0;

        while (pc < code.len) {
            const op: Opcode = @enumFromInt(code[pc]);
            pc += 1;

            switch (op) {
                .goto, .loop, .if_true, .if_false, .if_false_goto => {
                    if (pc + 2 <= code.len) {
                        const offset: i16 = @bitCast(readU16(code, pc));
                        const target: u32 = @intCast(@as(i32, @intCast(pc + 2)) + offset);
                        self.jump_targets.put(self.allocator, target, {}) catch return CompileError.OutOfMemory;
                    }
                    pc += 2;
                },
                .for_of_next => {
                    if (pc + 2 <= code.len) {
                        const offset: i16 = @bitCast(readU16(code, pc));
                        const target: u32 = @intCast(@as(i32, @intCast(pc + 2)) + offset);
                        self.jump_targets.put(self.allocator, target, {}) catch return CompileError.OutOfMemory;
                    }
                    pc += 2;
                },
                .for_of_next_put_loc => {
                    if (pc + 3 <= code.len) {
                        const offset: i16 = @bitCast(readU16(code, pc + 1));
                        const target: u32 = @intCast(@as(i32, @intCast(pc + 3)) + offset);
                        self.jump_targets.put(self.allocator, target, {}) catch return CompileError.OutOfMemory;
                    }
                    pc += 3;
                },
                .push_const => pc += 2,
                .push_i8, .get_loc, .put_loc => pc += 1,
                .push_i16 => pc += 2,
                .new_array, .get_field, .put_field, .put_field_keep, .get_global, .put_global => pc += 2,
                .get_field_ic, .put_field_ic => pc += 4,
                .call, .call_method, .tail_call => pc += 1,
                .get_loc_add => pc += 1,
                .get_loc_get_loc_add => pc += 2,
                .add_mod, .sub_mod, .mul_mod, .mod_const => pc += 2,
                .mod_const_i8, .add_const_i8, .sub_const_i8, .mul_const_i8, .lt_const_i8, .le_const_i8 => pc += 1,
                .get_upvalue, .put_upvalue, .close_upvalue => pc += 1,
                .get_field_call => pc += 3,
                .push_const_call => pc += 3,
                .make_closure => pc += 4,
                else => {},
            }
        }
    }

    /// Main compilation entry point
    pub fn compile(self: *OptimizedCompiler) CompileError!CompiledCode {
        // Phase 1: Detect and analyze loops
        try self.detectLoops();

        // If no loops are optimizable, fall back to baseline
        if (self.loop_count == 0) {
            return CompileError.UnsupportedOpcode;
        }

        // Phase 2: Find jump targets
        try self.findJumpTargets();

        // Phase 3: Emit prologue
        try self.emitPrologue();

        // Phase 4: Compile bytecode with optimized loop handling
        var pc: u32 = 0;
        const code = self.func.code;

        while (pc < code.len) {
            // Record label if this is a jump target
            if (self.jump_targets.contains(pc)) {
                self.labels.put(self.allocator, pc, @intCast(self.emitter.buffer.items.len)) catch return CompileError.OutOfMemory;
            }

            // Check if we're entering an optimized loop
            const loop_idx = self.getLoopAtHeader(pc);
            if (loop_idx) |idx| {
                // Emit loop entry type checks
                try self.emitLoopPrologue(idx);
                self.current_loop_idx = idx;
            }

            // Check if we're at a loop back-edge
            if (self.current_loop_idx) |idx| {
                if (pc == self.loops[idx].info.back_edge_bc_offset) {
                    // Emit loop epilogue (rebox values)
                    try self.emitLoopEpilogue(idx);
                    self.current_loop_idx = null;
                }
            }

            const op: Opcode = @enumFromInt(code[pc]);
            pc += 1;

            // Compile the opcode
            pc = try self.compileOpcode(op, pc, code);
        }

        // Phase 5: Emit epilogue
        try self.emitEpilogue();

        // Phase 6: Emit deopt stubs
        try self.emitDeoptStubs();

        // Phase 7: Patch jumps
        try self.patchJumps();

        // Phase 8: Allocate executable memory
        const alloc_info = self.code_alloc.allocWithPage(self.emitter.buffer.items.len) catch return CompileError.AllocationFailed;
        @memcpy(alloc_info.slice, self.emitter.buffer.items);
        self.code_alloc.makeExecutable(alloc_info.page_index) catch return CompileError.AllocationFailed;

        return CompiledCode.fromSlice(alloc_info.slice);
    }

    /// Check if a bytecode offset is the header of an optimized loop
    fn getLoopAtHeader(self: *OptimizedCompiler, bc_offset: u32) ?u8 {
        for (0..self.loop_count) |i| {
            if (self.loops[i].info.header_bc_offset == bc_offset) {
                return @intCast(i);
            }
        }
        return null;
    }

    /// Emit function prologue (save registers, setup context)
    fn emitPrologue(self: *OptimizedCompiler) CompileError!void {
        if (is_x86_64) {
            // Save frame pointer
            self.emitter.pushReg(.rbp) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rbp, .rsp) catch return CompileError.OutOfMemory;

            // Save callee-saved registers we'll use for loop variables
            self.emitter.pushReg(.rbx) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.r12) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.r13) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.r14) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.r15) catch return CompileError.OutOfMemory;

            // Save context pointer
            self.emitter.movRegReg(.rbx, .rdi) catch return CompileError.OutOfMemory;

            // Initialize stack pointer cache
            try self.emitInitStackCache();
        } else if (is_aarch64) {
            // Save frame pointer and link register
            self.emitter.stpPreIndex(.x29, .x30, .sp, -16) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm12(.x29, .sp, 0) catch return CompileError.OutOfMemory;

            // Save callee-saved registers
            self.emitter.stpPreIndex(.x19, .x20, .sp, -16) catch return CompileError.OutOfMemory;
            self.emitter.stpPreIndex(.x21, .x22, .sp, -16) catch return CompileError.OutOfMemory;
            self.emitter.stpPreIndex(.x23, .x24, .sp, -16) catch return CompileError.OutOfMemory;
            self.emitter.stpPreIndex(.x25, .x26, .sp, -16) catch return CompileError.OutOfMemory;
            self.emitter.stpPreIndex(.x27, .x28, .sp, -16) catch return CompileError.OutOfMemory;

            // x19 = context pointer
            self.emitter.movRegReg(.x19, .x0) catch return CompileError.OutOfMemory;

            try self.emitInitStackCache();
        }
    }

    /// Emit function epilogue (restore registers, return)
    fn emitEpilogue(self: *OptimizedCompiler) CompileError!void {
        if (is_x86_64) {
            // Return value is in rax
            self.emitter.popReg(.r15) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.r14) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.r13) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.r12) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.rbx) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.rbp) catch return CompileError.OutOfMemory;
            self.emitter.ret() catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Return value is in x0
            self.emitter.ldpPostIndex(.x27, .x28, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ldpPostIndex(.x25, .x26, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ldpPostIndex(.x23, .x24, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ldpPostIndex(.x21, .x22, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ldpPostIndex(.x19, .x20, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ldpPostIndex(.x29, .x30, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ret() catch return CompileError.OutOfMemory;
        }
    }

    /// Emit loop prologue: type checks for all accessed locals
    fn emitLoopPrologue(self: *OptimizedCompiler, loop_idx: u8) CompileError!void {
        const loop = &self.loops[loop_idx];
        const deopt_label = self.newLocalLabel();

        // Record the deopt label for this loop
        loop.deopt_stub_offset = deopt_label;
        loop.entry_offset = @intCast(self.emitter.buffer.items.len);

        if (is_x86_64) {
            // Check all accessed locals are SMI
            var local_idx: u8 = 0;
            while (local_idx < 16) : (local_idx += 1) {
                if (loop.info.isLocalAccessed(local_idx)) {
                    // Load local from stack
                    const offset = @as(i32, @intCast(local_idx)) * 8;
                    const fp_reg = getFramePointerReg();
                    self.emitter.movRegMem(.rax, fp_reg, offset) catch return CompileError.OutOfMemory;

                    // Check SMI tag (LSB == 0)
                    self.emitter.testRegImm32(.rax, 1) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.ne, deopt_label);

                    // Unbox and store in assigned register if allocated
                    if (loop.getLocalReg(local_idx)) |reg| {
                        self.emitter.movRegReg(reg, .rax) catch return CompileError.OutOfMemory;
                        self.emitter.sarRegImm(reg, 1) catch return CompileError.OutOfMemory;
                    }
                }
            }
        } else if (is_aarch64) {
            var local_idx: u8 = 0;
            while (local_idx < 16) : (local_idx += 1) {
                if (loop.info.isLocalAccessed(local_idx)) {
                    const offset: i12 = @intCast(@as(i32, @intCast(local_idx)) * 8);
                    const fp_reg = getFramePointerReg();
                    self.emitter.ldrImm(.x9, fp_reg, offset) catch return CompileError.OutOfMemory;

                    // Check SMI tag
                    self.emitter.andRegImm(.x10, .x9, 1) catch return CompileError.OutOfMemory;
                    self.emitter.cmpRegImm12(.x10, 0) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.ne, deopt_label);

                    // Unbox and store in assigned register
                    if (loop.getLocalReg(local_idx)) |reg| {
                        self.emitter.movRegReg(reg, .x9) catch return CompileError.OutOfMemory;
                        self.emitter.asrRegImm(reg, reg, 1) catch return CompileError.OutOfMemory;
                    }
                }
            }
        }

        loop.is_optimized = true;
    }

    /// Emit loop epilogue: rebox values and store back to stack
    fn emitLoopEpilogue(self: *OptimizedCompiler, loop_idx: u8) CompileError!void {
        const loop = &self.loops[loop_idx];

        if (!loop.is_optimized) return;

        if (is_x86_64) {
            var local_idx: u8 = 0;
            while (local_idx < 16) : (local_idx += 1) {
                if (loop.getLocalReg(local_idx)) |reg| {
                    // Rebox: shift left by 1
                    self.emitter.shlRegImm(reg, 1) catch return CompileError.OutOfMemory;

                    // Store back to stack
                    const offset = @as(i32, @intCast(local_idx)) * 8;
                    const fp_reg = getFramePointerReg();
                    self.emitter.movMemReg(fp_reg, offset, reg) catch return CompileError.OutOfMemory;
                }
            }
        } else if (is_aarch64) {
            var local_idx: u8 = 0;
            while (local_idx < 16) : (local_idx += 1) {
                if (loop.getLocalReg(local_idx)) |reg| {
                    // Rebox
                    self.emitter.lslRegImm(reg, reg, 1) catch return CompileError.OutOfMemory;

                    // Store back
                    const offset: i12 = @intCast(@as(i32, @intCast(local_idx)) * 8);
                    const fp_reg = getFramePointerReg();
                    self.emitter.strImm(reg, fp_reg, offset) catch return CompileError.OutOfMemory;
                }
            }
        }
    }

    /// Emit deoptimization stubs for all loops
    fn emitDeoptStubs(self: *OptimizedCompiler) CompileError!void {
        for (0..self.loop_count) |i| {
            const loop = &self.loops[i];
            if (loop.is_optimized) {
                try self.markLabel(loop.deopt_stub_offset);
                try self.emitDeoptExit(loop.info.header_bc_offset, .type_mismatch);
            }
        }
    }

    /// Emit deopt exit: call jitDeoptimize and return
    fn emitDeoptExit(self: *OptimizedCompiler, bytecode_offset: u32, reason: DeoptReason) CompileError!void {
        const fn_ptr = @intFromPtr(&jitDeoptimize);

        if (is_x86_64) {
            // Call jitDeoptimize(ctx, bytecode_offset, reason)
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rsi, @as(u64, bytecode_offset)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rdx, @as(u64, @intFromEnum(reason))) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;

            // Return the deopt result
            try self.emitEpilogue();
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, @as(u64, bytecode_offset)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, @as(u64, @intFromEnum(reason))) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;

            try self.emitEpilogue();
        }
    }

    /// Compile a single opcode - delegates to baseline for non-loop code,
    /// uses optimized paths for arithmetic inside loops
    fn compileOpcode(self: *OptimizedCompiler, op: Opcode, pc: u32, code: []const u8) CompileError!u32 {
        var new_pc = pc;

        // If we're inside an optimized loop and this is a binary op, use unguarded path
        if (self.current_loop_idx) |loop_idx| {
            const loop = &self.loops[loop_idx];
            if (loop.is_optimized) {
                switch (op) {
                    .add => {
                        try self.emitUnguardedIntBinaryOp(.add, loop_idx);
                        return new_pc;
                    },
                    .sub => {
                        try self.emitUnguardedIntBinaryOp(.sub, loop_idx);
                        return new_pc;
                    },
                    .mul => {
                        try self.emitUnguardedIntBinaryOp(.mul, loop_idx);
                        return new_pc;
                    },
                    else => {},
                }
            }
        }

        // For other opcodes, use baseline compilation
        // This is simplified - a full implementation would handle all opcodes
        switch (op) {
            .push_const => {
                const idx = readU16(code, new_pc);
                new_pc += 2;
                const val = self.func.constants[idx];
                try self.emitPushImm64(@bitCast(val));
            },
            .push_0 => try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(0))),
            .push_1 => try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(1))),
            .push_2 => try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(2))),
            .push_3 => try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(3))),
            .push_i8 => {
                const val: i8 = @bitCast(code[new_pc]);
                new_pc += 1;
                try self.emitPushImm64(@bitCast(value_mod.JSValue.fromInt(@as(i32, val))));
            },
            .get_loc_0 => try self.emitGetLocal(0),
            .get_loc_1 => try self.emitGetLocal(1),
            .get_loc_2 => try self.emitGetLocal(2),
            .get_loc_3 => try self.emitGetLocal(3),
            .get_loc => {
                const idx = code[new_pc];
                new_pc += 1;
                try self.emitGetLocal(idx);
            },
            .put_loc_0 => try self.emitPutLocal(0),
            .put_loc_1 => try self.emitPutLocal(1),
            .put_loc_2 => try self.emitPutLocal(2),
            .put_loc_3 => try self.emitPutLocal(3),
            .put_loc => {
                const idx = code[new_pc];
                new_pc += 1;
                try self.emitPutLocal(idx);
            },
            .loop => {
                const offset: i16 = @bitCast(readU16(code, new_pc));
                new_pc += 2;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitJump(target);
            },
            .goto => {
                const offset: i16 = @bitCast(readU16(code, new_pc));
                new_pc += 2;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitJump(target);
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
            .lt => try self.emitComparison(.lt),
            .lte => try self.emitComparison(.lte),
            .gt => try self.emitComparison(.gt),
            .gte => try self.emitComparison(.gte),
            .ret => try self.emitReturn(),
            .mod => try self.emitMod(),
            .inc => try self.emitInc(),
            .dec => try self.emitDec(),
            // Fallback for unsupported opcodes
            else => return CompileError.UnsupportedOpcode,
        }

        return new_pc;
    }

    /// Emit unguarded integer binary operation (no type checks, only overflow)
    fn emitUnguardedIntBinaryOp(self: *OptimizedCompiler, op: BinaryOp, loop_idx: u8) CompileError!void {
        const loop = &self.loops[loop_idx];
        const overflow_deopt = loop.deopt_stub_offset;

        if (is_x86_64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Pop operands (already unboxed since we're in optimized loop)
            // Actually, operands on stack are still boxed - we need to unbox them
            self.emitter.movRegReg(.r10, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rcx, .r11, 0) catch return CompileError.OutOfMemory; // b
            self.emitter.subRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rax, .r11, 0) catch return CompileError.OutOfMemory; // a

            // Unbox (we assume they are SMI - type was checked at loop entry)
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;

            // Perform operation with overflow check only
            switch (op) {
                .add => {
                    self.emitter.addRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.o, overflow_deopt);
                },
                .sub => {
                    self.emitter.subRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.o, overflow_deopt);
                },
                .mul => {
                    self.emitter.imulRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.o, overflow_deopt);
                },
            }

            // Rebox result
            self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;

            // Store result
            self.emitter.movRegReg(.r10, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 2) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r11, 0, .rax) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(sp, 1) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Load operands
            self.emitter.movRegReg(.x11, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(.x11, .x11, 1) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x12, stack_ptr, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x10, .x12, 0) catch return CompileError.OutOfMemory; // b
            self.emitter.subRegImm12(.x11, .x11, 1) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x12, stack_ptr, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x9, .x12, 0) catch return CompileError.OutOfMemory; // a

            // Unbox
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;

            // Perform operation
            switch (op) {
                .add => {
                    self.emitter.addsRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.vs, overflow_deopt);
                },
                .sub => {
                    self.emitter.subsRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.vs, overflow_deopt);
                },
                .mul => {
                    self.emitter.mulRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                    // Check i31 range
                    self.emitter.lslRegImm(.x11, .x9, 33) catch return CompileError.OutOfMemory;
                    self.emitter.asrRegImm(.x11, .x11, 33) catch return CompileError.OutOfMemory;
                    self.emitter.cmpRegReg(.x9, .x11) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.ne, overflow_deopt);
                },
            }

            // Rebox
            self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;

            // Store result
            self.emitter.movRegReg(.x14, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(.x14, .x14, 2) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x15, stack_ptr, .x14, 3) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x9, .x15, 0) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(sp, sp, 1) catch return CompileError.OutOfMemory;
        }
    }

    const BinaryOp = enum { add, sub, mul };

    // Helper functions - simplified versions of baseline implementations

    fn newLocalLabel(self: *OptimizedCompiler) u32 {
        const label = self.next_local_label;
        self.next_local_label += 1;
        return label | 0x80000000; // Mark as local label
    }

    fn markLabel(self: *OptimizedCompiler, label: u32) CompileError!void {
        self.labels.put(self.allocator, label, @intCast(self.emitter.buffer.items.len)) catch return CompileError.OutOfMemory;
    }

    fn emitInitStackCache(self: *OptimizedCompiler) CompileError!void {
        // Simplified - would load stack pointer and sp from context
        if (is_x86_64) {
            const CTX_SP_OFF: i32 = @intCast(@offsetOf(Context, "sp"));
            const CTX_STACK_PTR_OFF: i32 = @intCast(@offsetOf(Context, "stack"));
            self.emitter.movRegMem(getSpCacheReg(), .rbx, CTX_SP_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(getStackPtrCacheReg(), .rbx, CTX_STACK_PTR_OFF) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            const CTX_SP_OFF: i12 = @intCast(@offsetOf(Context, "sp"));
            const CTX_STACK_PTR_OFF: i12 = @intCast(@offsetOf(Context, "stack"));
            self.emitter.ldrImm(getSpCacheReg(), .x19, CTX_SP_OFF) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(getStackPtrCacheReg(), .x19, CTX_STACK_PTR_OFF) catch return CompileError.OutOfMemory;
        }
    }

    fn emitPushImm64(self: *OptimizedCompiler, val: u64) CompileError!void {
        if (is_x86_64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.movRegImm64(.rax, val) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, sp, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r11, 0, .rax) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(sp, 1) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.movRegImm64(.x9, val) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x10, stack_ptr, sp, 3) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x9, .x10, 0) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm12(sp, sp, 1) catch return CompileError.OutOfMemory;
        }
    }

    fn emitGetLocal(self: *OptimizedCompiler, idx: u8) CompileError!void {
        // Check if local is in a register in current loop
        if (self.current_loop_idx) |loop_idx| {
            const loop = &self.loops[loop_idx];
            if (loop.getLocalReg(idx)) |reg| {
                // Local is in register - push boxed value
                if (is_x86_64) {
                    self.emitter.movRegReg(.rax, reg) catch return CompileError.OutOfMemory;
                    self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory; // Rebox
                    try self.emitPushReg(.rax);
                } else if (is_aarch64) {
                    self.emitter.movRegReg(.x9, reg) catch return CompileError.OutOfMemory;
                    self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
                    try self.emitPushReg(.x9);
                }
                return;
            }
        }

        // Load from stack
        if (is_x86_64) {
            const fp_reg = getFramePointerReg();
            const offset = @as(i32, @intCast(idx)) * 8;
            self.emitter.movRegMem(.rax, fp_reg, offset) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            const fp_reg = getFramePointerReg();
            const offset: i12 = @intCast(@as(i32, @intCast(idx)) * 8);
            self.emitter.ldrImm(.x9, fp_reg, offset) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
        }
    }

    fn emitPutLocal(self: *OptimizedCompiler, idx: u8) CompileError!void {
        // Check if local is in a register in current loop
        if (self.current_loop_idx) |loop_idx| {
            const loop = &self.loops[loop_idx];
            if (loop.getLocalReg(idx)) |reg| {
                // Pop value and unbox into register
                if (is_x86_64) {
                    try self.emitPopReg(.rax);
                    self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
                    self.emitter.movRegReg(reg, .rax) catch return CompileError.OutOfMemory;
                } else if (is_aarch64) {
                    try self.emitPopReg(.x9);
                    self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
                    self.emitter.movRegReg(reg, .x9) catch return CompileError.OutOfMemory;
                }
                return;
            }
        }

        // Store to stack
        if (is_x86_64) {
            try self.emitPopReg(.rax);
            const fp_reg = getFramePointerReg();
            const offset = @as(i32, @intCast(idx)) * 8;
            self.emitter.movMemReg(fp_reg, offset, .rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            try self.emitPopReg(.x9);
            const fp_reg = getFramePointerReg();
            const offset: i12 = @intCast(@as(i32, @intCast(idx)) * 8);
            self.emitter.strImm(.x9, fp_reg, offset) catch return CompileError.OutOfMemory;
        }
    }

    fn emitPushReg(self: *OptimizedCompiler, reg: Register) CompileError!void {
        if (is_x86_64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.leaRegMem(.r11, stack_ptr, sp, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r11, 0, reg) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(sp, 1) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.addRegRegShift(.x10, stack_ptr, sp, 3) catch return CompileError.OutOfMemory;
            self.emitter.strImm(reg, .x10, 0) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm12(sp, sp, 1) catch return CompileError.OutOfMemory;
        }
    }

    fn emitPopReg(self: *OptimizedCompiler, reg: Register) CompileError!void {
        if (is_x86_64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.subRegImm32(sp, 1) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, sp, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(reg, .r11, 0) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.subRegImm12(sp, sp, 1) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x10, stack_ptr, sp, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(reg, .x10, 0) catch return CompileError.OutOfMemory;
        }
    }

    fn emitJump(self: *OptimizedCompiler, target: u32) CompileError!void {
        if (self.labels.get(target)) |offset| {
            // Backward jump - target is known
            if (is_x86_64) {
                const current = @as(i32, @intCast(self.emitter.buffer.items.len + 5));
                const rel = @as(i32, @intCast(offset)) - current;
                self.emitter.jmpRel32(rel) catch return CompileError.OutOfMemory;
            } else if (is_aarch64) {
                const current: i32 = @intCast(self.emitter.buffer.items.len);
                const off: i32 = @intCast(offset);
                const rel = @divExact(off - current, 4);
                self.emitter.b(rel) catch return CompileError.OutOfMemory;
            }
        } else {
            // Forward jump - record for patching
            try self.pending_jumps.append(self.allocator, .{
                .native_offset = @intCast(self.emitter.buffer.items.len),
                .bytecode_target = target,
                .is_conditional = false,
            });
            if (is_x86_64) {
                self.emitter.jmpRel32(0) catch return CompileError.OutOfMemory;
            } else if (is_aarch64) {
                self.emitter.b(0) catch return CompileError.OutOfMemory;
            }
        }
    }

    fn emitConditionalJump(self: *OptimizedCompiler, target: u32, jump_if_true: bool) CompileError!void {
        // Pop condition
        if (is_x86_64) {
            try self.emitPopReg(.rax);
            // Check if truthy (non-zero, non-false, non-null, non-undefined)
            self.emitter.cmpRegImm64(.rax, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            const cond: x86.Condition = if (jump_if_true) .ne else .e;
            try self.emitJccToTarget(cond, target);
        } else if (is_aarch64) {
            try self.emitPopReg(.x9);
            self.emitter.movRegImm64(.x10, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;
            const cond: arm64.Condition = if (jump_if_true) .ne else .eq;
            try self.emitBcondToTarget(cond, target);
        }
    }

    fn emitJccToLabel(self: *OptimizedCompiler, cond: x86.Condition, label: u32) CompileError!void {
        try self.pending_jumps.append(self.allocator, .{
            .native_offset = @intCast(self.emitter.buffer.items.len),
            .bytecode_target = label,
            .is_conditional = true,
        });
        self.emitter.jccRel32(cond, 0) catch return CompileError.OutOfMemory;
    }

    fn emitBcondToLabel(self: *OptimizedCompiler, cond: arm64.Condition, label: u32) CompileError!void {
        try self.pending_jumps.append(self.allocator, .{
            .native_offset = @intCast(self.emitter.buffer.items.len),
            .bytecode_target = label,
            .is_conditional = true,
        });
        self.emitter.bcond(cond, 0) catch return CompileError.OutOfMemory;
    }

    fn emitJccToTarget(self: *OptimizedCompiler, cond: x86.Condition, target: u32) CompileError!void {
        if (self.labels.get(target)) |offset| {
            const current = @as(i32, @intCast(self.emitter.buffer.items.len + 6));
            const rel = @as(i32, @intCast(offset)) - current;
            self.emitter.jccRel32(cond, rel) catch return CompileError.OutOfMemory;
        } else {
            try self.emitJccToLabel(cond, target);
        }
    }

    fn emitBcondToTarget(self: *OptimizedCompiler, cond: arm64.Condition, target: u32) CompileError!void {
        if (self.labels.get(target)) |offset| {
            const current: i32 = @intCast(self.emitter.buffer.items.len);
            const off: i32 = @intCast(offset);
            const rel = @divExact(off - current, 4);
            self.emitter.bcond(cond, @intCast(rel)) catch return CompileError.OutOfMemory;
        } else {
            try self.emitBcondToLabel(cond, target);
        }
    }

    fn emitComparison(self: *OptimizedCompiler, cmp: enum { lt, lte, gt, gte }) CompileError!void {
        if (is_x86_64) {
            try self.emitPopReg(.rcx); // b
            try self.emitPopReg(.rax); // a

            // Unbox (assume SMI)
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;

            self.emitter.cmpRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;

            const true_val: u64 = @bitCast(value_mod.JSValue.true_val);
            const false_val: u64 = @bitCast(value_mod.JSValue.false_val);

            self.emitter.movRegImm64(.rax, true_val) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rcx, false_val) catch return CompileError.OutOfMemory;

            const cond: x86.Condition = switch (cmp) {
                .lt => .l,
                .lte => .le,
                .gt => .g,
                .gte => .ge,
            };
            self.emitter.cmovcc(cond, .rax, .rcx) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x10); // b
            try self.emitPopReg(.x9); // a

            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;

            self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;

            const true_val: u64 = @bitCast(value_mod.JSValue.true_val);
            const false_val: u64 = @bitCast(value_mod.JSValue.false_val);

            self.emitter.movRegImm64(.x11, true_val) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x12, false_val) catch return CompileError.OutOfMemory;

            const cond: arm64.Condition = switch (cmp) {
                .lt => .lt,
                .lte => .le,
                .gt => .gt,
                .gte => .ge,
            };
            self.emitter.csel(.x9, .x11, .x12, cond) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
        }
    }

    fn emitReturn(self: *OptimizedCompiler) CompileError!void {
        if (is_x86_64) {
            try self.emitPopReg(.rax);
            try self.emitEpilogue();
        } else if (is_aarch64) {
            try self.emitPopReg(.x0);
            try self.emitEpilogue();
        }
    }

    fn emitMod(self: *OptimizedCompiler) CompileError!void {
        // Simplified modulo - calls helper for the operation
        // This is less optimized but simpler and works on all platforms
        const fn_ptr = @intFromPtr(&Context.jitMod);

        if (is_x86_64) {
            try self.emitPopReg(.rdx); // divisor
            try self.emitPopReg(.rsi); // dividend

            // Call jitMod(ctx, dividend, divisor)
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.callReg(.rax) catch return CompileError.OutOfMemory;

            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // divisor
            try self.emitPopReg(.x1); // dividend

            // Call jitMod(ctx, dividend, divisor)
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            self.emitter.blr(.x9) catch return CompileError.OutOfMemory;

            try self.emitPushReg(.x0);
        }
    }

    fn emitInc(self: *OptimizedCompiler) CompileError!void {
        if (is_x86_64) {
            try self.emitPopReg(.rax);
            self.emitter.addRegImm32(.rax, 2) catch return CompileError.OutOfMemory; // +1 in SMI encoding = +2 in raw
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x9);
            self.emitter.addRegImm12(.x9, .x9, 2) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
        }
    }

    fn emitDec(self: *OptimizedCompiler) CompileError!void {
        if (is_x86_64) {
            try self.emitPopReg(.rax);
            self.emitter.subRegImm32(.rax, 2) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x9);
            self.emitter.subRegImm12(.x9, .x9, 2) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
        }
    }

    fn patchJumps(self: *OptimizedCompiler) CompileError!void {
        for (self.pending_jumps.items) |jump| {
            const target_offset = self.labels.get(jump.bytecode_target) orelse continue;

            if (is_x86_64) {
                const jump_end = jump.native_offset + (if (jump.is_conditional) @as(u32, 6) else @as(u32, 5));
                const rel = @as(i32, @intCast(target_offset)) - @as(i32, @intCast(jump_end));
                const patch_offset = jump.native_offset + (if (jump.is_conditional) @as(u32, 2) else @as(u32, 1));
                const rel_bytes: [4]u8 = @bitCast(rel);
                @memcpy(self.emitter.buffer.items[patch_offset..][0..4], &rel_bytes);
            } else if (is_aarch64) {
                const current: i32 = @intCast(jump.native_offset);
                const target: i32 = @intCast(target_offset);
                const rel = @divExact(target - current, 4);

                if (jump.is_conditional) {
                    // Patch b.cond instruction (19-bit immediate)
                    const existing = std.mem.readInt(u32, self.emitter.buffer.items[jump.native_offset..][0..4], .little);
                    const imm19: u32 = @bitCast(@as(i32, rel) & 0x7FFFF);
                    const patched = (existing & 0xFF00001F) | (imm19 << 5);
                    std.mem.writeInt(u32, self.emitter.buffer.items[jump.native_offset..][0..4], patched, .little);
                } else {
                    // Patch b instruction (26-bit immediate)
                    const existing = std.mem.readInt(u32, self.emitter.buffer.items[jump.native_offset..][0..4], .little);
                    const imm26: u32 = @bitCast(@as(i32, rel) & 0x3FFFFFF);
                    const patched = (existing & 0xFC000000) | imm26;
                    std.mem.writeInt(u32, self.emitter.buffer.items[jump.native_offset..][0..4], patched, .little);
                }
            }
        }
    }

    fn emitJmpToLabel(self: *OptimizedCompiler, label: u32) CompileError!void {
        try self.pending_jumps.append(self.allocator, .{
            .native_offset = @intCast(self.emitter.buffer.items.len),
            .bytecode_target = label,
            .is_conditional = false,
        });
        if (is_x86_64) {
            self.emitter.jmpRel32(0) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.b(0) catch return CompileError.OutOfMemory;
        }
    }
};

// Register helpers
fn getSpCacheReg() Register {
    if (is_x86_64) return .r13;
    if (is_aarch64) return .x21;
    unreachable;
}

fn getStackPtrCacheReg() Register {
    if (is_x86_64) return .r14;
    if (is_aarch64) return .x20;
    unreachable;
}

fn getFramePointerReg() Register {
    if (is_x86_64) return .r15;
    if (is_aarch64) return .x22;
    unreachable;
}

fn readU16(code: []const u8, offset: u32) u16 {
    return std.mem.readInt(u16, code[offset..][0..2], .little);
}

/// Try to compile a function using the optimized tier.
/// Returns the compiled code on success, or an error if the function
/// is not suitable for optimization.
pub fn compileOptimized(
    allocator: std.mem.Allocator,
    code_alloc: *CodeAllocator,
    func: *const bytecode.FunctionBytecode,
) CompileError!CompiledCode {
    var compiler = OptimizedCompiler.init(allocator, code_alloc, func);
    defer compiler.deinit();
    return compiler.compile();
}

// ============================================================================
// Unit Tests
// ============================================================================

test "OptimizedLoop register assignment" {
    var loop_info = type_feedback.LoopInfo.init(0, 100);
    loop_info.markLocalAccessed(0);
    loop_info.markLocalAccessed(2);
    loop_info.markLocalAccessed(5);

    var loop = OptimizedLoop.init(loop_info);
    loop.assignRegisters();

    try std.testing.expectEqual(@as(u8, 3), loop.reg_local_count);
    try std.testing.expect(loop.local_to_reg[0] != null);
    try std.testing.expect(loop.local_to_reg[2] != null);
    try std.testing.expect(loop.local_to_reg[5] != null);
    try std.testing.expect(loop.local_to_reg[1] == null);
}

test "LoopInfo accessed locals" {
    var info = type_feedback.LoopInfo.init(10, 50);

    info.markLocalAccessed(0);
    info.markLocalAccessed(3);
    info.markLocalAccessed(7);

    try std.testing.expect(info.isLocalAccessed(0));
    try std.testing.expect(!info.isLocalAccessed(1));
    try std.testing.expect(!info.isLocalAccessed(2));
    try std.testing.expect(info.isLocalAccessed(3));
    try std.testing.expect(info.isLocalAccessed(7));
    try std.testing.expectEqual(@as(u8, 3), info.accessedLocalCount());
}
