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
const deopt = @import("deopt.zig");
const bytecode = @import("../bytecode.zig");
const value_mod = @import("../value.zig");
const context_mod = @import("../context.zig");
const object = @import("../object.zig");
const interpreter_mod = @import("../interpreter.zig");
const type_feedback = @import("../type_feedback.zig");
const arena_mod = @import("../arena.zig");
const builtins = @import("../builtins.zig");

const CodeAllocator = alloc.CodeAllocator;
const CompiledCode = alloc.CompiledCode;
const Opcode = bytecode.Opcode;
const Context = context_mod.Context;
const JSObject = object.JSObject;
const Interpreter = interpreter_mod.Interpreter;
const PolymorphicInlineCache = interpreter_mod.PolymorphicInlineCache;
const PICEntry = interpreter_mod.PICEntry;

extern fn jitCall(ctx: *Context, argc: u8, is_method: u8) value_mod.JSValue;
extern fn jitCallBytecode(ctx: *Context, func_bc: *const bytecode.FunctionBytecode, argc: u8, is_method: u8) value_mod.JSValue;
extern fn jitCallBytecodeFast(ctx: *Context, func_bc: *const bytecode.FunctionBytecode, argc: u8, is_method: u8) value_mod.JSValue;
extern fn jitMathFloor(ctx: *Context, arg: value_mod.JSValue) value_mod.JSValue;
extern fn jitMathCeil(ctx: *Context, arg: value_mod.JSValue) value_mod.JSValue;
extern fn jitMathRound(ctx: *Context, arg: value_mod.JSValue) value_mod.JSValue;
extern fn jitMathAbs(ctx: *Context, arg: value_mod.JSValue) value_mod.JSValue;
extern fn jitMathMin2(ctx: *Context, arg1: value_mod.JSValue, arg2: value_mod.JSValue) value_mod.JSValue;
extern fn jitMathMax2(ctx: *Context, arg1: value_mod.JSValue, arg2: value_mod.JSValue) value_mod.JSValue;
extern fn jitGetFieldIC(ctx: *Context, obj: value_mod.JSValue, atom_idx: u16, cache_idx: u16) value_mod.JSValue;
extern fn jitPutFieldIC(ctx: *Context, obj: value_mod.JSValue, atom_idx: u16, val: value_mod.JSValue, cache_idx: u16) value_mod.JSValue;
extern fn jitConcatN(ctx: *Context, count: u8) value_mod.JSValue;
extern fn jitForOfNext(ctx: *Context) u64;
extern fn jitForOfNextPutLoc(ctx: *Context, local_idx: u8) u64;
const jitDeoptimize = deopt.jitDeoptimize;

// Context field offsets for JIT code generation
// These are computed at compile time using @offsetOf
const CTX_STACK_PTR_OFF: i32 = @intCast(@offsetOf(Context, "stack"));
const CTX_STACK_LEN_OFF: i32 = @intCast(@offsetOf(Context, "stack") + @sizeOf(*value_mod.JSValue));
const CTX_SP_OFF: i32 = @intCast(@offsetOf(Context, "sp"));
const CTX_FP_OFF: i32 = @intCast(@offsetOf(Context, "fp"));
const CTX_JIT_INTERP_OFF: i32 = @intCast(@offsetOf(Context, "jit_interpreter"));
const CTX_HYBRID_OFF: i32 = @intCast(@offsetOf(Context, "hybrid"));

// Arena offsets for inline bump allocation
const HYBRID_ARENA_OFF: i32 = @intCast(@offsetOf(arena_mod.HybridAllocator, "arena"));
const ARENA_PTR_OFF: i32 = @intCast(@offsetOf(arena_mod.Arena, "ptr"));
const ARENA_LIMIT_OFF: i32 = @intCast(@offsetOf(arena_mod.Arena, "limit"));
const JSOBJECT_SIZE: u32 = @sizeOf(JSObject);
const JSOBJECT_SIZE_ALIGNED: u32 = (JSOBJECT_SIZE + 7) & ~@as(u32, 7); // 8-byte aligned

// Pointer extraction mask: clears TAG_PREFIX (upper 16 bits) and low 3 tag bits
// Used to extract actual memory address from JSValue pointer encoding
// PAYLOAD_MASK & ~7 = 0x0000_FFFF_FFFF_FFF8
const PTR_EXTRACT_MASK: u64 = 0x0000_FFFF_FFFF_FFF8;

// JSObject field offsets for inline cache fast path
const OBJ_HIDDEN_CLASS_OFF: i32 = @intCast(@offsetOf(JSObject, "hidden_class_idx"));
const OBJ_INLINE_SLOTS_OFF: i32 = @intCast(@offsetOf(JSObject, "inline_slots"));
const OBJ_FUNC_DATA_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.FUNC_DATA)) * 8;
const OBJ_FUNC_IS_BYTECODE_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.FUNC_IS_BYTECODE)) * 8;
const OBJ_FUNC_GUARD_ID_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.FUNC_GUARD_ID)) * 8;
const OBJ_CLASS_ID_OFF: i32 = @intCast(@offsetOf(JSObject, "class_id"));
// Range iterator inline slot offsets (each slot is 8 bytes = sizeof(JSValue))
const OBJ_RANGE_START_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.RANGE_START)) * 8;
const OBJ_RANGE_STEP_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.RANGE_STEP)) * 8;
const OBJ_RANGE_LENGTH_OFF: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, @intCast(object.JSObject.Slots.RANGE_LENGTH)) * 8;
const NATIVE_ARGCOUNT_OFF: i32 = @intCast(@offsetOf(object.NativeFunctionData, "arg_count"));

// Interpreter field offsets for inline cache fast path
const INTERP_PIC_CACHE_OFF: i32 = @intCast(@offsetOf(Interpreter, "pic_cache"));

// PIC structure sizes and offsets
const PIC_SIZE: i32 = @intCast(@sizeOf(PolymorphicInlineCache));
const PIC_ENTRY_SIZE: i32 = @intCast(@sizeOf(PICEntry));
// PICEntry layout: hidden_class_idx (u32) + slot_offset (u16)
const PIC_ENTRY_HIDDEN_CLASS_OFF: i32 = 0;
const PIC_ENTRY_SLOT_OFF: i32 = @intCast(@sizeOf(object.HiddenClassIndex));
// Number of PIC entries to check in generated code (balance code size vs hit rate)
const PIC_CHECK_COUNT: i32 = 4;

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

/// Deoptimization reason for type guards
pub const DeoptReason = enum(u8) {
    type_mismatch,
    hidden_class_mismatch,
    overflow,
    callee_changed,
};

/// Deoptimization point recorded during compilation
pub const DeoptPoint = struct {
    native_offset: u32, // Where to jump for deopt
    bytecode_offset: u32, // Where to resume in interpreter
    reason: DeoptReason,
};

// ========================================================================
// Register Allocation for Hot Locals (ARM64 only)
// ========================================================================

/// Registers available for local variable allocation on ARM64.
/// These are callee-saved registers not used by the baseline JIT:
/// x19 = ctx pointer, x20 = JS stack pointer, x21-x22 = reserved
/// x23-x28 = available for hot locals (6 registers)
pub const LocalAllocRegs = if (is_aarch64) [_]arm64.Register{
    .x23,
    .x24,
    .x25,
    .x26,
    .x27,
    .x28,
} else [_]void{};

pub const MAX_REG_LOCALS: u8 = if (is_aarch64) 6 else 0;

/// Allocation info for a single local variable
pub const LocalAlloc = struct {
    register: ?arm64.Register, // null = in memory (slow path)
    access_count: u16, // Number of get_loc/put_loc accesses
    helper_written: bool, // true if written by helper (can't allocate)
};

/// Register allocation state for a compiled function
pub const RegAllocState = struct {
    /// Allocation info for each local (indexed by local variable index)
    local_allocs: []LocalAlloc,
    /// Maps register index (0-5 for x23-x28) to local variable index
    reg_to_local: [MAX_REG_LOCALS]?u8,
    /// Number of locals actually allocated to registers
    reg_local_count: u8,

    /// Codegen tracking: which allocated locals have been loaded into their register
    /// Reset to false at jump targets (conservative)
    loaded: [MAX_REG_LOCALS]bool,
    /// Codegen tracking: which allocated locals have been modified (need spill before helpers/exit)
    dirty: [MAX_REG_LOCALS]bool,

    pub fn init(allocator: std.mem.Allocator, local_count: u16) !RegAllocState {
        const allocs = try allocator.alloc(LocalAlloc, local_count);
        for (allocs) |*a| {
            a.* = .{ .register = null, .access_count = 0, .helper_written = false };
        }
        return .{
            .local_allocs = allocs,
            .reg_to_local = .{null} ** MAX_REG_LOCALS,
            .reg_local_count = 0,
            .loaded = .{false} ** MAX_REG_LOCALS,
            .dirty = .{false} ** MAX_REG_LOCALS,
        };
    }

    pub fn deinit(self: *RegAllocState, allocator: std.mem.Allocator) void {
        allocator.free(self.local_allocs);
    }
};

/// Baseline compiler state
pub const BaselineCompiler = struct {
    emitter: Emitter,
    code_alloc: *CodeAllocator,
    func: *const bytecode.FunctionBytecode,

    /// Label offsets for forward jumps (bytecode offset -> native offset)
    labels: std.AutoHashMapUnmanaged(u32, u32),
    /// Pending forward jump patches (native offset -> bytecode target)
    pending_jumps: std.ArrayListUnmanaged(PendingJump),
    /// Jump targets: bytecode offsets that are targets of jump instructions
    /// Only these need labels recorded during compilation (reduces hash ops by ~80%)
    jump_targets: std.AutoHashMapUnmanaged(u32, void),
    allocator: std.mem.Allocator,
    next_local_label: u32,

    /// Type feedback for speculative optimization
    tf: ?*type_feedback.TypeFeedback,
    feedback_site_map: ?[]u16,

    /// Hidden class pool for monomorphic property access optimization
    hidden_class_pool: ?*object.HiddenClassPool,

    /// Deoptimization points for type guard failures
    deopt_points: std.ArrayListUnmanaged(DeoptPoint),
    /// Offset where deopt stubs begin in native code
    deopt_stubs_offset: u32,

    /// Inlining state
    inline_depth: u32,
    total_inlined_size: u32,
    inline_exit_label: ?u32,
    inline_is_method: bool,
    stack_overflow_label: ?u32,
    suppress_stack_checks: bool,

    /// Register allocation state for hot locals (ARM64 only)
    reg_alloc: ?RegAllocState,

    /// Virtual stack top: tracks when the last push came from a register-allocated local.
    /// This allows fusing get_local + get_field_ic by skipping the redundant push/pop
    /// and using the callee-saved register directly for property access.
    /// Set by emitGetLocal when the local is register-allocated.
    /// Consumed (and cleared) by emitMonomorphicGetField/PutField.
    /// Cleared by any instruction that modifies the operand stack in unexpected ways.
    pending_reg_push: ?Register,

    const PendingJump = struct {
        native_offset: u32, // Offset in emitted code where the rel32/imm is
        bytecode_target: u32, // Target bytecode offset
        is_conditional: bool, // For ARM64: conditional vs unconditional branch
    };

    pub fn init(allocator: std.mem.Allocator, code_alloc: *CodeAllocator, func: *const bytecode.FunctionBytecode, hidden_class_pool: ?*object.HiddenClassPool) BaselineCompiler {
        // Pre-allocate emitter buffer based on bytecode size heuristic
        // Native code is typically 4-8x bytecode size; use 6x as conservative estimate
        // This eliminates 3-5 reallocs during compilation for typical functions
        var emitter = Emitter.init(allocator);
        const estimated_size = func.code.len * 6;
        const initial_capacity = @max(256, @min(estimated_size, 32768));
        emitter.buffer.ensureTotalCapacity(allocator, initial_capacity) catch {};

        return .{
            .emitter = emitter,
            .code_alloc = code_alloc,
            .func = func,
            .labels = .{},
            .pending_jumps = .{},
            .jump_targets = .{},
            .allocator = allocator,
            .next_local_label = 0,
            // Type feedback from interpreter (may be null)
            .tf = func.type_feedback_ptr,
            .feedback_site_map = func.feedback_site_map,
            .hidden_class_pool = hidden_class_pool,
            .deopt_points = .{},
            .deopt_stubs_offset = 0,
            .inline_depth = 0,
            .total_inlined_size = 0,
            .inline_exit_label = null,
            .inline_is_method = false,
            .stack_overflow_label = null,
            .suppress_stack_checks = false,
            .reg_alloc = null,
            .pending_reg_push = null,
        };
    }

    pub fn deinit(self: *BaselineCompiler) void {
        self.emitter.deinit();
        self.labels.deinit(self.allocator);
        self.pending_jumps.deinit(self.allocator);
        self.jump_targets.deinit(self.allocator);
        self.deopt_points.deinit(self.allocator);
        if (self.reg_alloc) |*ra| {
            ra.deinit(self.allocator);
        }
    }

    // ========================================================================
    // Type Feedback Queries
    // ========================================================================

    /// Get type feedback site for a binary operation at given bytecode offset
    fn getBinaryOpFeedback(self: *BaselineCompiler, bytecode_offset: u32) ?struct { left: *type_feedback.TypeFeedbackSite, right: *type_feedback.TypeFeedbackSite } {
        const tf = self.tf orelse return null;
        const site_map = self.feedback_site_map orelse return null;

        if (bytecode_offset >= site_map.len) return null;
        const site_idx = site_map[bytecode_offset];
        if (site_idx == 0xFFFF) return null;

        if (site_idx + 1 >= tf.sites.len) return null;
        return .{
            .left = &tf.sites[site_idx],
            .right = &tf.sites[site_idx + 1],
        };
    }

    /// Check if a binary op should use specialized integer path
    fn shouldSpecializeIntBinaryOp(self: *BaselineCompiler, bytecode_offset: u32) bool {
        const fb = self.getBinaryOpFeedback(bytecode_offset) orelse return false;

        // Both operands must be monomorphic SMI
        return fb.left.isMonomorphic() and
            fb.left.dominantType() == .smi and
            fb.right.isMonomorphic() and
            fb.right.dominantType() == .smi;
    }

    /// Get type feedback site for property access at given bytecode offset
    fn getPropertyFeedback(self: *BaselineCompiler, bytecode_offset: u32) ?*type_feedback.TypeFeedbackSite {
        const tf = self.tf orelse return null;
        const site_map = self.feedback_site_map orelse return null;

        if (bytecode_offset >= site_map.len) return null;
        const site_idx = site_map[bytecode_offset];
        if (site_idx == 0xFFFF) return null;

        if (site_idx >= tf.sites.len) return null;
        return &tf.sites[site_idx];
    }

    /// Check if property access should use monomorphic specialization
    /// Returns the hidden class index if monomorphic with object type
    fn getMonomorphicHiddenClass(self: *BaselineCompiler, bytecode_offset: u32) ?object.HiddenClassIndex {
        const fb = self.getPropertyFeedback(bytecode_offset) orelse return null;

        // Must be monomorphic with object or array type
        if (!fb.isMonomorphic()) return null;
        const dominant = fb.dominantType() orelse return null;
        if (dominant != .object and dominant != .array) return null;

        // Get the recorded hidden class
        return fb.getMonomorphicHiddenClass();
    }

    /// Get monomorphic property access info at compile time
    /// Returns (hidden_class_index, slot_offset) if monomorphic with known slot
    fn getMonomorphicPropertyInfo(self: *BaselineCompiler, atom_idx: u16, bytecode_offset: u32) ?struct { hc_idx: object.HiddenClassIndex, slot: u16 } {
        // Need both type feedback and hidden class pool
        const hc_idx = self.getMonomorphicHiddenClass(bytecode_offset) orelse return null;
        const pool = self.hidden_class_pool orelse return null;

        // Look up slot offset for this property in the hidden class
        const atom: object.Atom = @enumFromInt(atom_idx);
        const slot = pool.findProperty(hc_idx, atom) orelse return null;

        // Only optimize inline slots (< 8) for simplicity
        if (slot >= JSObject.INLINE_SLOT_COUNT) return null;

        return .{ .hc_idx = hc_idx, .slot = slot };
    }

    /// Get call site feedback for a call instruction at given bytecode offset
    fn getCallSiteFeedback(self: *BaselineCompiler, bytecode_offset: u32) ?*type_feedback.CallSiteFeedback {
        const tf = self.tf orelse return null;
        const site_map = self.feedback_site_map orelse return null;

        if (bytecode_offset >= site_map.len) return null;
        const site_idx = site_map[bytecode_offset];

        // Call sites have high bit set (0x8000)
        if ((site_idx & 0x8000) == 0) return null;
        const call_idx = site_idx & 0x7FFF;

        if (call_idx >= tf.call_sites.len) return null;
        return &tf.call_sites[call_idx];
    }

    /// Get monomorphic call target for fast path, or null if not suitable
    fn getMonomorphicCallTarget(self: *BaselineCompiler, bytecode_offset: u32) ?*const bytecode.FunctionBytecode {
        const cs = self.getCallSiteFeedback(bytecode_offset) orelse return null;
        if (!cs.isMonomorphic()) return null;
        const callee = cs.getMonomorphicCallee() orelse return null;
        // Use lower threshold for hot callees (already proven to be frequently executed)
        const min_calls = if (callee.execution_count > type_feedback.InliningPolicy.HOT_CALLEE_THRESHOLD)
            type_feedback.InliningPolicy.MIN_CALL_COUNT_HOT
        else
            type_feedback.InliningPolicy.MIN_CALL_COUNT;
        if (cs.total_calls < min_calls) return null;
        if (callee.flags.is_generator or callee.flags.is_async) return null;
        return callee;
    }

    /// Check if a call site should be inlined and return the callee
    fn getInlineCandidate(self: *BaselineCompiler, bytecode_offset: u32) ?*const bytecode.FunctionBytecode {
        const cs = self.getCallSiteFeedback(bytecode_offset) orelse return null;

        // Must be monomorphic
        const callee = cs.getMonomorphicCallee() orelse return null;

        // Check inlining policy
        const decision = type_feedback.InliningPolicy.shouldInline(
            self.func,
            callee,
            cs,
            self.inline_depth,
            self.total_inlined_size,
        );

        if (decision != .yes) return null;
        return callee;
    }

    // ========================================================================
    // Register Allocation for Hot Locals
    // ========================================================================

    /// Pre-scan bytecode to count local variable accesses.
    /// Returns true if register allocation should be used for this function.
    fn scanLocalAccesses(self: *BaselineCompiler) CompileError!bool {
        // Only ARM64 supports register allocation for now
        if (!is_aarch64) return false;

        const local_count = self.func.local_count;
        if (local_count == 0) return false;

        // Initialize register allocation state
        self.reg_alloc = RegAllocState.init(self.allocator, local_count) catch return CompileError.OutOfMemory;
        var ra = &self.reg_alloc.?;

        // Single pass over bytecode to count accesses and detect problematic patterns
        const code = self.func.code;
        var pc: u32 = 0;
        var has_closure = false;
        var has_loop = false;

        while (pc < code.len) {
            const op: Opcode = @enumFromInt(code[pc]);
            pc += 1;

            switch (op) {
                // Direct local access opcodes
                .get_loc_0, .put_loc_0 => {
                    ra.local_allocs[0].access_count +|= 1;
                },
                .get_loc_1, .put_loc_1 => {
                    if (local_count > 1) ra.local_allocs[1].access_count +|= 1;
                },
                .get_loc_2, .put_loc_2 => {
                    if (local_count > 2) ra.local_allocs[2].access_count +|= 1;
                },
                .get_loc_3, .put_loc_3 => {
                    if (local_count > 3) ra.local_allocs[3].access_count +|= 1;
                },
                .get_loc, .put_loc => {
                    const idx = code[pc];
                    pc += 1;
                    if (idx < local_count) {
                        ra.local_allocs[idx].access_count +|= 1;
                    }
                },
                .get_loc_add => {
                    const idx = code[pc];
                    pc += 1;
                    if (idx < local_count) {
                        ra.local_allocs[idx].access_count +|= 1;
                    }
                },
                .get_loc_get_loc_add => {
                    const idx1 = code[pc];
                    const idx2 = code[pc + 1];
                    pc += 2;
                    if (idx1 < local_count) ra.local_allocs[idx1].access_count +|= 1;
                    if (idx2 < local_count) ra.local_allocs[idx2].access_count +|= 1;
                },
                // Closure creation - locals might be captured
                .make_closure => {
                    has_closure = true;
                    pc += 4;
                },
                // Loop detection (backward jump and for-of loops)
                .loop => {
                    has_loop = true;
                    pc += 2;
                },
                .for_of_next => {
                    has_loop = true;
                    pc += 2;
                },
                // for_of_next_put_loc writes via helper - can't allocate that local
                .for_of_next_put_loc => {
                    has_loop = true;
                    const idx = code[pc];
                    if (idx < local_count) {
                        ra.local_allocs[idx].helper_written = true;
                    }
                    pc += 3;
                },
                // Skip other opcodes by their operand size
                .push_const, .push_const_call => pc += if (op == .push_const) 2 else 3,
                .push_i8 => pc += 1,
                .push_i16 => pc += 2,
                .new_array, .get_field, .put_field, .put_field_keep, .get_global, .put_global => pc += 2,
                .get_field_ic, .put_field_ic => pc += 4,
                .call, .call_method, .tail_call => pc += 1,
                .add_mod, .sub_mod, .mul_mod, .mod_const => pc += 2,
                .mod_const_i8, .add_const_i8, .sub_const_i8, .mul_const_i8, .lt_const_i8, .le_const_i8 => pc += 1,
                .get_upvalue, .put_upvalue, .close_upvalue => pc += 1,
                .get_field_call => pc += 3,
                .goto, .if_true, .if_false, .if_false_goto => pc += 2,
                else => {},
            }
        }

        // If function creates closures, disable register allocation
        if (has_closure) {
            ra.deinit(self.allocator);
            self.reg_alloc = null;
            return false;
        }

        // Only enable for functions with loops (where the benefit is significant)
        if (!has_loop) {
            ra.deinit(self.allocator);
            self.reg_alloc = null;
            return false;
        }

        return true;
    }

    /// Minimum access count for a local to be worth allocating to a register.
    /// With lazy loading and deferred stores, overhead is low, so we can be aggressive.
    /// A local accessed just twice in a loop body will be accessed many times at runtime.
    const MIN_ACCESS_COUNT_FOR_REG: u16 = 2;

    /// Assign registers to the most frequently accessed locals.
    /// Only allocates if a local has at least MIN_ACCESS_COUNT_FOR_REG accesses.
    fn assignRegistersToLocals(self: *BaselineCompiler) void {
        var ra = &(self.reg_alloc orelse return);
        const local_count = self.func.local_count;
        if (local_count == 0) return;

        // Find top N locals by access count using simple selection
        // (N = MAX_REG_LOCALS = 6 for ARM64)
        var assigned: u8 = 0;

        while (assigned < MAX_REG_LOCALS) {
            var best_idx: ?u8 = null;
            var best_count: u16 = 0;

            // Find the local with highest access count that isn't already assigned
            // Skip locals that are written by helpers (can't be safely allocated)
            for (0..local_count) |i| {
                const idx: u8 = @intCast(i);
                const local_alloc = &ra.local_allocs[idx];
                if (local_alloc.register == null and !local_alloc.helper_written and local_alloc.access_count > best_count) {
                    best_count = local_alloc.access_count;
                    best_idx = idx;
                }
            }

            // Stop if no more locals with sufficient accesses
            // A local needs at least MIN_ACCESS_COUNT_FOR_REG accesses to justify register overhead
            if (best_idx == null or best_count < MIN_ACCESS_COUNT_FOR_REG) break;

            // Assign register to this local
            const reg = LocalAllocRegs[assigned];
            ra.local_allocs[best_idx.?].register = reg;
            ra.reg_to_local[assigned] = best_idx;
            assigned += 1;
        }

        ra.reg_local_count = assigned;

        // If no locals qualified for register allocation, clean up
        if (assigned == 0) {
            ra.deinit(self.allocator);
            self.reg_alloc = null;
        }
    }

    // NaN-boxing integer tag: TAG_PREFIX | TYPE_INT
    // TAG_PREFIX = 0xFFFC_0000_0000_0000, TYPE_INT = 1 << 44 = 0x0000_1000_0000_0000
    // Result: 0xFFFC_1000_0000_0000
    const INT_TAG: u64 = 0xFFFC_1000_0000_0000;
    // Mask for checking integer type: upper 20 bits (TAG + TYPE)
    const INT_TYPE_MASK: u64 = 0xFFFF_F000_0000_0000;

    /// Emit code to extract pointer address from JSValue in a register
    /// Input: src_reg contains JSValue with pointer encoding
    /// Output: dst_reg contains raw pointer address (TAG_PREFIX and low 3 bits cleared)
    ///
    /// On ARM64, x22 holds the pre-loaded PTR_EXTRACT_MASK, making this a single AND.
    fn emitExtractPtr(self: *BaselineCompiler, dst_reg: Register, src_reg: Register) CompileError!void {
        if (is_x86_64) {
            // Load PTR_EXTRACT_MASK into scratch, AND with src, store to dst
            // Use r11 as scratch when dst is r10, otherwise use r10
            const scratch: Register = if (dst_reg == .r10) .r11 else .r10;
            if (dst_reg != src_reg) {
                self.emitter.movRegReg(dst_reg, src_reg) catch return CompileError.OutOfMemory;
            }
            self.emitter.movRegImm64(scratch, PTR_EXTRACT_MASK) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(dst_reg, scratch) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // x22 holds PTR_EXTRACT_MASK (loaded in prologue), so just AND
            self.emitter.andRegReg(dst_reg, src_reg, .x22) catch return CompileError.OutOfMemory;
        }
    }

    /// Emit specialized integer binary operation when type feedback shows both operands are integers.
    /// Guards integer type and deoptimizes on mismatch. On overflow, falls back to slow path helper.
    fn emitSpecializedIntBinaryOp(self: *BaselineCompiler, op: BinaryOp) CompileError!void {
        if (is_x86_64) {
            const overflow_slow = self.newLocalLabel();
            const try_float = self.newLocalLabel();
            const call_helper = self.newLocalLabel();
            const store_result = self.newLocalLabel();
            const done = self.newLocalLabel();

            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Load operands from stack without modifying sp
            self.emitter.movRegReg(.r10, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rcx, .r11, 0) catch return CompileError.OutOfMemory; // b
            self.emitter.subRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rax, .r11, 0) catch return CompileError.OutOfMemory; // a

            // Save boxed values for overflow slow path
            self.emitter.movRegReg(.r8, .rax) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r9, .rcx) catch return CompileError.OutOfMemory;

            // Guard NaN-boxed integer type: (value & INT_TYPE_MASK) == INT_TAG
            self.emitter.movRegImm64(.r10, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r11, .rax) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.r11, .r10) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.r10, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.r11, .r10) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, try_float);
            // Check b
            self.emitter.movRegImm64(.r10, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r11, .rcx) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.r11, .r10) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.r10, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.r11, .r10) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, try_float);

            // Unbox: sign-extend lower 32 bits (NaN-boxing stores i32 in lower 32 bits)
            // Register encoding for movsxd src is identical for rax/eax (same reg number)
            self.emitter.movsxdRegReg(.rax, .rax) catch return CompileError.OutOfMemory;
            self.emitter.movsxdRegReg(.rcx, .rcx) catch return CompileError.OutOfMemory;

            // Perform operation with overflow check
            switch (op) {
                .add => {
                    self.emitter.addRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.o, overflow_slow);
                },
                .sub => {
                    self.emitter.subRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.o, overflow_slow);
                },
                .mul => {
                    self.emitter.imulRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
                    try self.emitJccToLabel(.o, overflow_slow);
                },
            }

            // Rebox: OR with INT_TAG (result in lower 32 bits, just add the tag)
            self.emitter.movRegImm64(.r10, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.orRegReg(.rax, .r10) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(store_result);

            // Overflow slow path: call helper (integer overflow produces float)
            try self.markLabel(overflow_slow);
            try self.emitJmpToLabel(call_helper);

            // Try inline float path when SMI guard fails
            try self.markLabel(try_float);
            // Check if both operands are inline floats (tag == 5)
            // r8 = a (original), r9 = b (original)
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper);
            self.emitter.movRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper);

            // Both are inline floats - extract f32 bits and convert to f64
            // Extract a's f32 bits from upper 32 bits
            self.emitter.movRegReg(.rax, .r8) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
            self.emitter.movdXmmReg32(.xmm0, .rax) catch return CompileError.OutOfMemory;
            self.emitter.cvtss2sd(.xmm0, .xmm0) catch return CompileError.OutOfMemory;

            // Extract b's f32 bits from upper 32 bits
            self.emitter.movRegReg(.rax, .r9) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
            self.emitter.movdXmmReg32(.xmm1, .rax) catch return CompileError.OutOfMemory;
            self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory;

            // Perform FPU operation
            switch (op) {
                .add => self.emitter.addsd(.xmm0, .xmm1) catch return CompileError.OutOfMemory,
                .sub => self.emitter.subsd(.xmm0, .xmm1) catch return CompileError.OutOfMemory,
                .mul => self.emitter.mulsd(.xmm0, .xmm1) catch return CompileError.OutOfMemory,
            }

            // Try to convert result back to inline float (check if fits in f32)
            self.emitter.cvtsd2ss(.xmm1, .xmm0) catch return CompileError.OutOfMemory; // f64 -> f32
            self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory; // f32 -> f64
            self.emitter.ucomisd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;
            // If not equal (precision loss) or unordered (NaN), call helper
            try self.emitJccToLabel(.ne, call_helper);
            try self.emitJccToLabel(.p, call_helper); // parity flag set for NaN

            // Create inline float result (tag 5)
            // xmm1 already has the f32 value (lower 32 bits after cvtsd2ss)
            self.emitter.movdReg32Xmm(.rax, .xmm1) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.rax, 32) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(.rax, 5) catch return CompileError.OutOfMemory; // Add tag 5
            try self.emitJmpToLabel(store_result);

            // Call helper for complex cases (boxed floats, mixed types, string concat for add)
            try self.markLabel(call_helper);
            const fn_ptr: u64 = switch (op) {
                .add => @intFromPtr(&Context.jitAdd),
                .sub => @intFromPtr(&Context.jitSub),
                .mul => @intFromPtr(&Context.jitMul),
            };
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdx, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);

            // Store result to stack[sp-2] and decrement sp
            try self.markLabel(store_result);
            self.emitter.movRegReg(.r10, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 2) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r11, 0, .rax) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(sp, 1) catch return CompileError.OutOfMemory;

            try self.markLabel(done);
        } else if (is_aarch64) {
            const overflow_slow = self.newLocalLabel();
            const try_float = self.newLocalLabel();
            const call_helper = self.newLocalLabel();
            const store_result = self.newLocalLabel();
            const done = self.newLocalLabel();

            const sp_reg = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Load operands from stack without modifying sp
            self.emitter.movRegReg(.x11, sp_reg) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(.x11, .x11, 1) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x12, stack_ptr, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x10, .x12, 0) catch return CompileError.OutOfMemory; // b
            self.emitter.subRegImm12(.x11, .x11, 1) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x12, stack_ptr, .x11, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x9, .x12, 0) catch return CompileError.OutOfMemory; // a

            // Save boxed values for slow path
            // Use callee-saved x21, x22 if available, or stack
            // For now, use caller-saved x12, x13 and rely on helper restoring them
            self.emitter.movRegReg(.x12, .x9) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x13, .x10) catch return CompileError.OutOfMemory;

            // Guard NaN-boxed integer type: (value & INT_TYPE_MASK) == INT_TAG
            self.emitter.movRegImm64(.x14, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x15, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.x11, .x9, .x14) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x11, .x15) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, try_float);
            // Check b
            self.emitter.andRegReg(.x11, .x10, .x14) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x11, .x15) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, try_float);

            // Unbox: sign-extend lower 32 bits (NaN-boxing stores i32 in lower 32 bits)
            self.emitter.sxtwRegReg(.x9, .x9) catch return CompileError.OutOfMemory;
            self.emitter.sxtwRegReg(.x10, .x10) catch return CompileError.OutOfMemory;

            // Perform operation with overflow check
            switch (op) {
                .add => {
                    self.emitter.addsRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.vs, overflow_slow);
                },
                .sub => {
                    self.emitter.subsRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.vs, overflow_slow);
                },
                .mul => {
                    self.emitter.mulRegReg(.x9, .x9, .x10) catch return CompileError.OutOfMemory;
                    // Check i32 range: sign-extend and compare
                    self.emitter.sxtwRegReg(.x11, .x9) catch return CompileError.OutOfMemory;
                    self.emitter.cmpRegReg(.x9, .x11) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.ne, overflow_slow);
                },
            }

            // Rebox: OR with INT_TAG
            self.emitter.movRegImm64(.x11, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.orrRegReg(.x9, .x9, .x11) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x0, .x9) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(store_result);

            // Overflow slow path: call helper
            try self.markLabel(overflow_slow);
            try self.emitJmpToLabel(call_helper);

            // Try inline float path when SMI guard fails
            try self.markLabel(try_float);
            // Check if both operands are inline floats (tag == 5)
            // x12 = a (original), x13 = b (original)
            self.emitter.andRegImm(.x14, .x12, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x14, 5) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);
            self.emitter.andRegImm(.x14, .x13, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x14, 5) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);

            // Both are inline floats - extract f32 bits and convert to f64
            // Extract a's f32 bits from upper 32 bits
            self.emitter.lsrRegImm(.x9, .x12, 32) catch return CompileError.OutOfMemory;
            self.emitter.fmovSingleFromGpr32(.x0, .x9) catch return CompileError.OutOfMemory; // d0 = f32 bits
            self.emitter.fcvtDoubleFromSingle(.x0, .x0) catch return CompileError.OutOfMemory; // d0 = f64

            // Extract b's f32 bits from upper 32 bits
            self.emitter.lsrRegImm(.x10, .x13, 32) catch return CompileError.OutOfMemory;
            self.emitter.fmovSingleFromGpr32(.x1, .x10) catch return CompileError.OutOfMemory; // d1 = f32 bits
            self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory; // d1 = f64

            // Perform FPU operation
            switch (op) {
                .add => self.emitter.faddDouble(.x0, .x0, .x1) catch return CompileError.OutOfMemory,
                .sub => self.emitter.fsubDouble(.x0, .x0, .x1) catch return CompileError.OutOfMemory,
                .mul => self.emitter.fmulDouble(.x0, .x0, .x1) catch return CompileError.OutOfMemory,
            }

            // Try to convert result back to inline float (check if fits in f32)
            self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory; // s1 = f32
            self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory; // d1 = f64 roundtrip
            self.emitter.fcmpDouble(.x0, .x1) catch return CompileError.OutOfMemory;
            // If not equal (precision loss) or unordered (NaN), call helper
            try self.emitBcondToLabel(.ne, call_helper);
            try self.emitBcondToLabel(.vs, call_helper); // unordered (NaN)

            // Create inline float result (tag 5)
            // Get f32 bits from s1
            self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory;
            self.emitter.fmovGpr32FromSingle(.x9, .x1) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x0, .x9, 32) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm12(.x0, .x0, 5) catch return CompileError.OutOfMemory; // Add tag 5
            try self.emitJmpToLabel(store_result);

            // Call helper for complex cases
            try self.markLabel(call_helper);
            const fn_ptr: u64 = switch (op) {
                .add => @intFromPtr(&Context.jitAdd),
                .sub => @intFromPtr(&Context.jitSub),
                .mul => @intFromPtr(&Context.jitMul),
            };
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x2, .x13) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x14, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x14);

            // Store result to stack[sp-2] and decrement sp
            try self.markLabel(store_result);
            self.emitter.movRegReg(.x14, sp_reg) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(.x14, .x14, 2) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x15, stack_ptr, .x14, 3) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x0, .x15, 0) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(sp_reg, sp_reg, 1) catch return CompileError.OutOfMemory;

            try self.markLabel(done);
        }
    }

    /// Emit monomorphic get field when type feedback shows consistent hidden class.
    /// Hardcodes the hidden class check and slot offset, skipping PIC lookup entirely.
    fn emitMonomorphicGetField(self: *BaselineCompiler, hc_idx: object.HiddenClassIndex, slot: u16, atom_idx: u16, cache_idx: u16, pending_obj_reg: ?Register) CompileError!void {
        const fn_ptr = @intFromPtr(&jitGetFieldIC);

        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop object value into r8
            try self.emitPopReg(.r8);

            // Quick pointer check: (raw & 0x7) == 1
            self.emitter.movRegReg(.r9, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r9, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r9, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.r9, .r8);

            // Check MemTag.object in header
            self.emitter.movRegMem32(.r10, .r9, 0) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 0xF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Check hidden class matches expected (hardcoded from type feedback)
            self.emitter.movRegMem32(.r10, .r9, OBJ_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, @bitCast(@intFromEnum(hc_idx))) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Fast path: load directly from inline slot (hardcoded offset)
            const slot_offset: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, slot) * 8;
            self.emitter.movRegMem(.rax, .r9, slot_offset) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            // Slow path: call helper
            try self.markLabel(slow);
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rdx, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rcx, cache_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Fused get_local + get_field_ic optimization:
            // If pending_obj_reg is set, the previous get_local already pushed the object
            // from a callee-saved register. Instead of popping it back, undo the push
            // (decrement sp) and use the register directly. This saves 6 instructions
            // (3 for push + 3 for pop) per property access.
            const obj_reg: Register = if (pending_obj_reg) |reg| blk: {
                // Undo the push from get_local: just decrement sp
                const sp = getSpCacheReg();
                self.emitter.subRegImm12(sp, sp, 1) catch return CompileError.OutOfMemory;
                break :blk reg;
            } else blk: {
                // Normal path: pop object value from operand stack
                try self.emitPopReg(.x12);
                break :blk .x12;
            };

            // Quick pointer check: (raw & 0x7) == 1
            // This confirms we have a tagged object pointer
            self.emitter.andRegImm(.x9, obj_reg, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x9, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.x9, obj_reg);

            // Skip MemTag check - pointer tag already confirms object pointer
            // Hidden class check will catch any type mismatches

            // Check hidden class matches expected (hardcoded from type feedback)
            self.emitter.ldrImmW(.x10, .x9, OBJ_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x11, @intFromEnum(hc_idx)) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x11) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Fast path: load directly from inline slot (hardcoded offset)
            const slot_offset: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, slot) * 8;
            self.emitter.ldrImm(.x0, .x9, slot_offset) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
            try self.emitJmpToLabel(done);

            // Slow path: call helper
            // For fused path, re-push the register value so the helper can pop it
            try self.markLabel(slow);
            if (pending_obj_reg) |reg| {
                // The object was never on the operand stack (we undid the push).
                // The helper expects the object as an argument, not on the stack.
                self.emitter.movRegReg(.x1, reg) catch return CompileError.OutOfMemory;
            } else {
                self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            }
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x3, cache_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    /// Emit monomorphic put field when type feedback shows consistent hidden class.
    /// Hardcodes the hidden class check and slot offset, skipping PIC lookup entirely.
    fn emitMonomorphicPutField(self: *BaselineCompiler, hc_idx: object.HiddenClassIndex, slot: u16, atom_idx: u16, cache_idx: u16) CompileError!void {
        const fn_ptr = @intFromPtr(&jitPutFieldIC);

        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop value into r8, object into r9
            try self.emitPopReg(.r8);
            try self.emitPopReg(.r9);

            // Quick pointer check: (raw & 0x7) == 1
            self.emitter.movRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.r10, .r9);

            // Check MemTag.object in header
            self.emitter.movRegMem32(.r11, .r10, 0) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r11, 0xF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Check hidden class matches expected
            self.emitter.movRegMem32(.r11, .r10, OBJ_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r11, @bitCast(@intFromEnum(hc_idx))) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Fast path: store directly to inline slot
            const slot_offset: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, slot) * 8;
            self.emitter.movMemReg(.r10, slot_offset, .r8) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(done);

            // Slow path: call helper
            try self.markLabel(slow);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rdx, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rcx, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.r8, cache_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop value into x12, object into x13
            try self.emitPopReg(.x12);
            try self.emitPopReg(.x13);

            // Quick pointer check: (raw & 0x7) == 1
            // This confirms we have a tagged object pointer
            self.emitter.andRegImm(.x9, .x13, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x9, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.x9, .x13);

            // Skip MemTag check - pointer tag already confirms object pointer
            // Hidden class check will catch any type mismatches

            // Check hidden class matches expected
            self.emitter.ldrImmW(.x10, .x9, OBJ_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x11, @intFromEnum(hc_idx)) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x11) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Fast path: store directly to inline slot
            const slot_offset: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, slot) * 8;
            self.emitter.strImm(.x12, .x9, slot_offset) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(done);

            // Slow path: call helper
            try self.markLabel(slow);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x13) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x3, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x4, cache_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);

            try self.markLabel(done);
        }
    }

    /// Pre-scan bytecode to find all jump targets
    /// This allows us to only record labels for bytecode offsets that are actually jumped to,
    /// reducing hash map operations by ~80% for typical functions
    fn findJumpTargets(self: *BaselineCompiler) CompileError!void {
        const code = self.func.code;
        var pc: u32 = 0;

        while (pc < code.len) {
            const op: Opcode = @enumFromInt(code[pc]);
            pc += 1;

            // Check for jump instructions and record their targets
            switch (op) {
                .goto, .loop, .if_true, .if_false, .if_false_goto => {
                    // These have i16 offset immediately after opcode
                    if (pc + 2 <= code.len) {
                        const offset: i16 = @bitCast(readU16(code, pc));
                        const target: u32 = @intCast(@as(i32, @intCast(pc + 2)) + offset);
                        self.jump_targets.put(self.allocator, target, {}) catch return CompileError.OutOfMemory;
                    }
                    pc += 2;
                },
                .for_of_next => {
                    // i16 offset
                    if (pc + 2 <= code.len) {
                        const offset: i16 = @bitCast(readU16(code, pc));
                        const target: u32 = @intCast(@as(i32, @intCast(pc + 2)) + offset);
                        self.jump_targets.put(self.allocator, target, {}) catch return CompileError.OutOfMemory;
                    }
                    pc += 2;
                },
                .for_of_next_put_loc => {
                    // u8 local + i16 offset
                    if (pc + 3 <= code.len) {
                        const offset: i16 = @bitCast(readU16(code, pc + 1));
                        const target: u32 = @intCast(@as(i32, @intCast(pc + 3)) + offset);
                        self.jump_targets.put(self.allocator, target, {}) catch return CompileError.OutOfMemory;
                    }
                    pc += 3;
                },
                // Skip other opcodes by their operand size
                .push_const, .push_const_call => pc += if (op == .push_const) 2 else 3,
                .push_i8, .get_loc, .put_loc => pc += 1,
                .push_i16 => pc += 2,
                .new_array, .get_field, .put_field, .put_field_keep, .get_global, .put_global => pc += 2,
                .get_field_ic, .put_field_ic => pc += 4,
                .call, .call_method, .tail_call => pc += 1,
                .get_loc_add, .get_loc_get_loc_add => pc += if (op == .get_loc_add) 1 else 2,
                .add_mod, .sub_mod, .mul_mod, .mod_const => pc += 2,
                .mod_const_i8, .add_const_i8, .sub_const_i8, .mul_const_i8, .lt_const_i8, .le_const_i8 => pc += 1,
                .get_upvalue, .put_upvalue, .close_upvalue => pc += 1,
                .get_field_call => pc += 3,
                .make_closure => pc += 4, // +u16 func_idx +u8 upvalue_count
                else => {}, // Opcodes with no operands
            }
        }
    }

    /// Compile the function to native code
    pub fn compile(self: *BaselineCompiler) CompileError!CompiledCode {
        // Pre-scan to find jump targets (lazy label optimization)
        try self.findJumpTargets();

        // Pre-scan for register allocation (ARM64 only)
        // Counts local variable accesses and assigns hot locals to registers
        if (try self.scanLocalAccesses()) {
            self.assignRegistersToLocals();
        }

        // Emit prologue
        try self.emitPrologue();

        // Compile each bytecode instruction
        var pc: u32 = 0;
        const code = self.func.code;

        while (pc < code.len) {
            // Only record labels for jump targets (reduces hash ops by ~80%)
            if (self.jump_targets.contains(pc)) {
                self.labels.put(self.allocator, pc, @intCast(self.emitter.buffer.items.len)) catch return CompileError.OutOfMemory;
                // NOTE: We don't reset loaded state at jump targets.
                // Register-allocated locals use callee-saved regs (x23-x28) which are
                // preserved across helper calls. Locals that helpers might modify are
                // excluded via helper_written flag. So register values stay valid.
            }

            const op: Opcode = @enumFromInt(code[pc]);
            pc += 1;

            pc = try self.compileOpcode(op, pc, code);
        }

        if (self.stack_overflow_label) |label| {
            const done_label = self.newLocalLabel();
            try self.emitJmpToLabel(done_label);
            try self.markLabel(label);
            try self.emitStackOverflowExit();
            try self.markLabel(done_label);
        }

        // Emit epilogue if we haven't returned yet
        try self.emitEpilogue();

        // Patch forward jumps
        try self.patchJumps();

        // Allocate executable memory and copy code
        const alloc_info = self.code_alloc.allocWithPage(self.emitter.buffer.items.len) catch return CompileError.AllocationFailed;
        @memcpy(alloc_info.slice, self.emitter.buffer.items);

        // Make only the used page executable
        self.code_alloc.makeExecutable(alloc_info.page_index) catch return CompileError.AllocationFailed;

        return CompiledCode.fromSlice(alloc_info.slice);
    }

    fn emitPrologue(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            // x86-64 prologue: save callee-saved registers
            self.emitter.pushReg(.rbp) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rbp, .rsp) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.rbx) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.r12) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.r13) catch return CompileError.OutOfMemory;
            self.emitter.pushReg(.r14) catch return CompileError.OutOfMemory;
            // Save context pointer (rdi) to rbx for later use
            self.emitter.movRegReg(.rbx, .rdi) catch return CompileError.OutOfMemory;
            try self.emitInitStackCache();
        } else if (is_aarch64) {
            // ARM64 prologue: save frame pointer and link register
            self.emitter.stpPreIndex(.x29, .x30, .sp, -16) catch return CompileError.OutOfMemory;
            // mov x29, sp (via add immediate)
            self.emitter.addRegImm12(.x29, .sp, 0) catch return CompileError.OutOfMemory;
            // Save callee-saved registers we use
            self.emitter.stpPreIndex(.x19, .x20, .sp, -16) catch return CompileError.OutOfMemory;
            self.emitter.stpPreIndex(.x21, .x22, .sp, -16) catch return CompileError.OutOfMemory;

            // Save additional callee-saved registers for local allocation (x23-x28)
            if (self.reg_alloc) |ra| {
                if (ra.reg_local_count > 0) {
                    // Save x23-x24 if using 1-2 locals
                    self.emitter.stpPreIndex(.x23, .x24, .sp, -16) catch return CompileError.OutOfMemory;
                }
                if (ra.reg_local_count > 2) {
                    // Save x25-x26 if using 3-4 locals
                    self.emitter.stpPreIndex(.x25, .x26, .sp, -16) catch return CompileError.OutOfMemory;
                }
                if (ra.reg_local_count > 4) {
                    // Save x27-x28 if using 5-6 locals
                    self.emitter.stpPreIndex(.x27, .x28, .sp, -16) catch return CompileError.OutOfMemory;
                }
            }

            // Save context pointer (x0) to x19 for later use
            self.emitter.movRegReg(.x19, .x0) catch return CompileError.OutOfMemory;
            try self.emitInitStackCache();

            // Load PTR_EXTRACT_MASK into x22 for fast pointer extraction
            // This turns every emitExtractPtr from 4-5 instructions to just 1 AND
            self.emitter.movRegImm64(.x22, PTR_EXTRACT_MASK) catch return CompileError.OutOfMemory;

            // NOTE: With lazy loading, we DON'T load locals here.
            // They will be loaded on first access in emitGetLocal.
            // This avoids loading locals that might not be used on all code paths.
        }
    }

    fn emitEpilogue(self: *BaselineCompiler) CompileError!void {
        if (is_x86_64) {
            // x86-64 epilogue: restore callee-saved registers
            try self.emitFlushStackCache();
            self.emitter.popReg(.r14) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.r13) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.r12) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.rbx) catch return CompileError.OutOfMemory;
            self.emitter.popReg(.rbp) catch return CompileError.OutOfMemory;
            self.emitter.ret() catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // ARM64 epilogue: store register locals back to memory, restore registers and return

            // Store DIRTY allocated locals back from registers to memory
            // Only locals that were written (put_loc emitted) need to be stored back
            if (self.reg_alloc) |ra| {
                // Check if any locals are dirty (written to)
                var has_dirty = false;
                for (0..ra.reg_local_count) |i| {
                    if (ra.dirty[i]) {
                        has_dirty = true;
                        break;
                    }
                }

                if (has_dirty) {
                    // Load ctx->fp into x9
                    self.emitter.ldrImm(.x9, .x19, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
                    // Load ctx->stack.ptr into x10
                    self.emitter.ldrImm(.x10, .x19, @intCast(@as(u32, @bitCast(CTX_STACK_PTR_OFF)))) catch return CompileError.OutOfMemory;

                    // Store only dirty locals back to memory
                    for (0..ra.reg_local_count) |i| {
                        if (!ra.dirty[i]) continue;

                        const local_idx = ra.reg_to_local[i] orelse continue;
                        const reg = ra.local_allocs[local_idx].register orelse continue;

                        // Compute address: x11 = x10 + (x9 + local_idx) * 8
                        if (local_idx > 0) {
                            self.emitter.addRegImm12(.x11, .x9, local_idx) catch return CompileError.OutOfMemory;
                            self.emitter.addRegRegShift(.x11, .x10, .x11, 3) catch return CompileError.OutOfMemory;
                        } else {
                            // local_idx == 0, just x11 = x10 + x9 * 8
                            self.emitter.addRegRegShift(.x11, .x10, .x9, 3) catch return CompileError.OutOfMemory;
                        }
                        // Store value from allocated register
                        self.emitter.strImm(reg, .x11, 0) catch return CompileError.OutOfMemory;
                    }
                }
            }

            try self.emitFlushStackCache();

            // Restore additional callee-saved registers (reverse order of prologue)
            if (self.reg_alloc) |ra| {
                if (ra.reg_local_count > 4) {
                    // Restore x27-x28 if we saved them
                    self.emitter.ldpPostIndex(.x27, .x28, .sp, 16) catch return CompileError.OutOfMemory;
                }
                if (ra.reg_local_count > 2) {
                    // Restore x25-x26 if we saved them
                    self.emitter.ldpPostIndex(.x25, .x26, .sp, 16) catch return CompileError.OutOfMemory;
                }
                if (ra.reg_local_count > 0) {
                    // Restore x23-x24 if we saved them
                    self.emitter.ldpPostIndex(.x23, .x24, .sp, 16) catch return CompileError.OutOfMemory;
                }
            }

            self.emitter.ldpPostIndex(.x21, .x22, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ldpPostIndex(.x19, .x20, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ldpPostIndex(.x29, .x30, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.ret() catch return CompileError.OutOfMemory;
        }
    }

    /// Get the register index for a local variable, or null if not allocated.
    fn getRegIndexForLocal(self: *BaselineCompiler, local_idx: u8) ?usize {
        const ra = self.reg_alloc orelse return null;
        for (0..ra.reg_local_count) |i| {
            if (ra.reg_to_local[i] == local_idx) return i;
        }
        return null;
    }

    fn compileOpcode(self: *BaselineCompiler, op: Opcode, pc: u32, code: []const u8) CompileError!u32 {
        var new_pc = pc;

        // Capture and clear the pending register push from a previous get_local.
        // Only get_field_ic/put_field_ic with monomorphic info will use this to
        // skip redundant push/pop and access the object register directly.
        const pending_obj_reg = self.pending_reg_push;
        self.pending_reg_push = null;

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
                // Type-specialized path when both operands are known SMI
                const opcode_offset = pc - 1;
                if (self.shouldSpecializeIntBinaryOp(opcode_offset)) {
                    try self.emitSpecializedIntBinaryOp(.add);
                } else {
                    try self.emitBinaryOp(.add);
                }
            },

            .sub => {
                // Type-specialized path when both operands are known SMI
                const opcode_offset = pc - 1;
                if (self.shouldSpecializeIntBinaryOp(opcode_offset)) {
                    try self.emitSpecializedIntBinaryOp(.sub);
                } else {
                    try self.emitBinaryOp(.sub);
                }
            },

            .mul => {
                // Type-specialized path when both operands are known SMI
                const opcode_offset = pc - 1;
                if (self.shouldSpecializeIntBinaryOp(opcode_offset)) {
                    try self.emitSpecializedIntBinaryOp(.mul);
                } else {
                    try self.emitBinaryOp(.mul);
                }
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

            .concat_n => {
                const count = code[new_pc];
                new_pc += 1;
                try self.emitConcatN(count);
            },

            // Math builtins - compile-time specialized
            .math_floor => try self.emitMathFloor(),
            .math_ceil => try self.emitMathCeil(),
            .math_round => try self.emitMathRound(),
            .math_abs => try self.emitMathAbs(),
            .math_min2 => try self.emitMathMin2(),
            .math_max2 => try self.emitMathMax2(),

            .ret => {
                if (self.inline_exit_label) |label| {
                    try self.emitJmpToLabel(label);
                } else {
                    try self.emitPop(.ret);
                    try self.emitEpilogue();
                }
            },

            .ret_undefined => {
                if (self.inline_exit_label) |label| {
                    try self.emitPushImm64(@bitCast(value_mod.JSValue.undefined_val));
                    try self.emitJmpToLabel(label);
                } else {
                    try self.emitMovImm64(.ret, @bitCast(value_mod.JSValue.undefined_val));
                    try self.emitEpilogue();
                }
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
                const opcode_offset = pc - 1;
                const val = self.func.constants[idx];
                try self.emitPushImm64(@bitCast(val));
                try self.emitCallWithFeedback(argc, false, opcode_offset, false);
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

            .new_object_literal => {
                const shape_idx = readU16(code, new_pc);
                new_pc += 2;
                const prop_count = code[new_pc];
                new_pc += 1;
                try self.emitNewObjectLiteral(shape_idx, prop_count);
            },

            .set_slot => {
                const slot_idx = code[new_pc];
                new_pc += 1;
                try self.emitSetSlot(slot_idx);
            },

            .get_field => {
                const atom_idx = readU16(code, new_pc);
                new_pc += 2;
                try self.emitGetField(atom_idx);
            },

            .get_field_ic => {
                const atom_idx = readU16(code, new_pc);
                const cache_idx = readU16(code, new_pc + 2);
                new_pc += 4;
                const opcode_offset = pc - 1;
                if (self.getMonomorphicPropertyInfo(atom_idx, opcode_offset)) |info| {
                    try self.emitMonomorphicGetField(info.hc_idx, info.slot, atom_idx, cache_idx, pending_obj_reg);
                } else {
                    try self.emitGetFieldIC(atom_idx, cache_idx);
                }
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
                const cache_idx = readU16(code, new_pc + 2);
                new_pc += 4;
                const opcode_offset = pc - 1;
                if (self.getMonomorphicPropertyInfo(atom_idx, opcode_offset)) |info| {
                    try self.emitMonomorphicPutField(info.hc_idx, info.slot, atom_idx, cache_idx);
                } else {
                    try self.emitPutFieldIC(atom_idx, cache_idx);
                }
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
                if (!try self.emitMathBuiltinFastPath(argc, true)) {
                    try self.emitCall(argc, true);
                }
            },

            // Function calls
            .call => {
                const argc = code[new_pc];
                new_pc += 1;
                const opcode_offset = pc - 1;
                try self.emitCallWithFeedback(argc, false, opcode_offset, true);
            },

            .call_method => {
                const argc = code[new_pc];
                new_pc += 1;
                const opcode_offset = pc - 1;
                try self.emitCallWithFeedback(argc, true, opcode_offset, true);
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

            // Loop back-edge jump
            .loop => {
                const offset: i16 = @bitCast(readU16(code, new_pc));
                new_pc += 2;
                const target: u32 = @intCast(@as(i32, @intCast(new_pc)) + offset);
                try self.emitJump(target);
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

    fn getSpCacheReg() Register {
        if (is_x86_64) {
            return .r12;
        } else if (is_aarch64) {
            return .x20;
        }
    }

    fn getStackPtrCacheReg() Register {
        if (is_x86_64) {
            return .r13;
        } else if (is_aarch64) {
            return .x21;
        }
    }

    fn getInlineFpSaveReg() Register {
        if (is_x86_64) {
            return .r14;
        } else if (is_aarch64) {
            return .x22;
        }
    }

    fn emitInitStackCache(self: *BaselineCompiler) CompileError!void {
        const ctx = getCtxReg();
        const sp = getSpCacheReg();
        const stack_ptr = getStackPtrCacheReg();
        if (is_x86_64) {
            self.emitter.movRegMem(sp, ctx, CTX_SP_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(stack_ptr, ctx, CTX_STACK_PTR_OFF) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.ldrImm(sp, ctx, @intCast(@as(u32, @bitCast(CTX_SP_OFF)))) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(stack_ptr, ctx, @intCast(@as(u32, @bitCast(CTX_STACK_PTR_OFF)))) catch return CompileError.OutOfMemory;
        }
    }

    fn emitFlushStackCache(self: *BaselineCompiler) CompileError!void {
        const ctx = getCtxReg();
        const sp = getSpCacheReg();
        if (is_x86_64) {
            self.emitter.movMemReg(ctx, CTX_SP_OFF, sp) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.strImm(sp, ctx, @intCast(@as(u32, @bitCast(CTX_SP_OFF)))) catch return CompileError.OutOfMemory;
        }
    }

    fn emitReloadStackCache(self: *BaselineCompiler) CompileError!void {
        try self.emitInitStackCache();
    }

    fn emitLoadSp(self: *BaselineCompiler, dst: Register) CompileError!void {
        const sp = getSpCacheReg();
        if (is_x86_64) {
            if (dst != sp) {
                self.emitter.movRegReg(dst, sp) catch return CompileError.OutOfMemory;
            }
        } else if (is_aarch64) {
            if (dst != sp) {
                self.emitter.movRegReg(dst, sp) catch return CompileError.OutOfMemory;
            }
        }
    }

    fn emitStoreSp(self: *BaselineCompiler, src: Register) CompileError!void {
        const sp = getSpCacheReg();
        if (is_x86_64) {
            if (src != sp) {
                self.emitter.movRegReg(sp, src) catch return CompileError.OutOfMemory;
            }
        } else if (is_aarch64) {
            if (src != sp) {
                self.emitter.movRegReg(sp, src) catch return CompileError.OutOfMemory;
            }
        }
    }

    fn emitLoadStackPtr(self: *BaselineCompiler, dst: Register) CompileError!void {
        const stack_ptr = getStackPtrCacheReg();
        if (is_x86_64) {
            if (dst != stack_ptr) {
                self.emitter.movRegReg(dst, stack_ptr) catch return CompileError.OutOfMemory;
            }
        } else if (is_aarch64) {
            if (dst != stack_ptr) {
                self.emitter.movRegReg(dst, stack_ptr) catch return CompileError.OutOfMemory;
            }
        }
    }

    fn emitCallHelperReg(self: *BaselineCompiler, reg: Register) CompileError!void {
        try self.emitFlushStackCache();
        // NOTE: We don't spill before helper calls because:
        // 1. Locals written by helpers are excluded from register allocation (helper_written)
        // 2. Helpers don't read other locals via ctx->stack (they use params/operand stack)
        // 3. We use callee-saved registers so helper calls preserve our values
        if (is_x86_64) {
            self.emitter.callReg(reg) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            self.emitter.blr(reg) catch return CompileError.OutOfMemory;
        }
        try self.emitReloadStackCache();
    }

    /// Emit a deoptimization exit that resumes in the interpreter and returns.
    fn emitDeoptExit(self: *BaselineCompiler, bytecode_offset: u32, reason: DeoptReason) CompileError!void {
        const fn_ptr: u64 = @intFromPtr(&jitDeoptimize);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rsi, @intCast(bytecode_offset)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rdx, @intFromEnum(reason)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitEpilogue();
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, bytecode_offset) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, @intFromEnum(reason)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitEpilogue();
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

    fn appendPendingJump(self: *BaselineCompiler, native_offset: u32, target: u32, is_conditional: bool) CompileError!void {
        self.pending_jumps.append(self.allocator, .{
            .native_offset = native_offset,
            .bytecode_target = target,
            .is_conditional = is_conditional,
        }) catch return CompileError.OutOfMemory;
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
                try self.appendPendingJump(patch_offset + 1, target, false);
            } else if (is_aarch64) {
                self.emitter.b(0) catch return CompileError.OutOfMemory;
                try self.appendPendingJump(patch_offset, target, false);
            }
        }
    }

    fn emitJccToLabel(self: *BaselineCompiler, cond: x86.Condition, target: u32) CompileError!void {
        if (!is_x86_64) return;
        if (self.labels.get(target)) |native_offset| {
            const current = @as(i32, @intCast(self.emitter.buffer.items.len));
            const rel = @as(i32, @intCast(native_offset)) - current - 6;
            self.emitter.jcc(cond, rel) catch return CompileError.OutOfMemory;
        } else {
            const patch_offset: u32 = @intCast(self.emitter.buffer.items.len + 2);
            self.emitter.jcc(cond, 0) catch return CompileError.OutOfMemory;
            try self.appendPendingJump(patch_offset, target, true);
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
            try self.appendPendingJump(patch_offset, target, true);
        }
    }

    fn getStackOverflowLabel(self: *BaselineCompiler) u32 {
        if (self.stack_overflow_label == null) {
            self.stack_overflow_label = self.newLocalLabel();
        }
        return self.stack_overflow_label.?;
    }

    fn emitStackCheck(self: *BaselineCompiler, tmp: Register) CompileError!void {
        const ctx = getCtxReg();
        const sp = getSpCacheReg();
        const overflow_label = self.getStackOverflowLabel();
        if (is_x86_64) {
            self.emitter.movRegMem(tmp, ctx, CTX_STACK_LEN_OFF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(sp, tmp) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ae, overflow_label);
        } else if (is_aarch64) {
            self.emitter.ldrImm(tmp, ctx, @intCast(@as(u32, @bitCast(CTX_STACK_LEN_OFF)))) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(sp, tmp) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.hs, overflow_label);
        }
    }

    fn emitInlineStackCheck(self: *BaselineCompiler, extra: u16) CompileError!void {
        if (extra == 0) return;
        const ctx = getCtxReg();
        const sp = getSpCacheReg();
        const tmp0 = getReg(.tmp0);
        const tmp1 = getReg(.tmp1);
        const overflow_label = self.getStackOverflowLabel();

        if (is_x86_64) {
            self.emitter.movRegMem(tmp0, ctx, CTX_STACK_LEN_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(tmp1, sp) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(tmp1, extra) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(tmp1, tmp0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ae, overflow_label);
        } else if (is_aarch64) {
            self.emitter.ldrImm(tmp0, ctx, @intCast(@as(u32, @bitCast(CTX_STACK_LEN_OFF)))) catch return CompileError.OutOfMemory;
            if (extra <= 4095) {
                self.emitter.addRegImm12(tmp1, sp, @intCast(extra)) catch return CompileError.OutOfMemory;
            } else {
                self.emitter.movRegImm64(tmp1, extra) catch return CompileError.OutOfMemory;
                self.emitter.addRegRegShift(tmp1, sp, tmp1, 0) catch return CompileError.OutOfMemory;
            }
            self.emitter.cmpRegReg(tmp1, tmp0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.hs, overflow_label);
        }
    }

    fn emitStackOverflowExit(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitStackOverflow);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitEpilogue();
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitEpilogue();
        }
    }

    fn emitPushReg(self: *BaselineCompiler, val_reg: Register) CompileError!void {
        if (!self.suppress_stack_checks) {
            const tmp0 = getReg(.tmp0);
            const tmp1 = getReg(.tmp1);
            const tmp = if (val_reg == tmp0) tmp1 else tmp0;
            try self.emitStackCheck(tmp);
        }
        if (is_x86_64) {
            // r12 = cached sp, r13 = cached stack_ptr, r10 = addr
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.leaRegMem(.r10, stack_ptr, sp, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r10, 0, val_reg) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(sp, 1) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // x20 = cached sp, x21 = cached stack_ptr, x14 = addr
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.addRegRegShift(.x14, stack_ptr, sp, 3) catch return CompileError.OutOfMemory;
            self.emitter.strImm(val_reg, .x14, 0) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm12(sp, sp, 1) catch return CompileError.OutOfMemory;
        }
    }

    fn emitPopReg(self: *BaselineCompiler, dst: Register) CompileError!void {
        if (is_x86_64) {
            // r12 = cached sp, r13 = cached stack_ptr, r10 = addr
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.subRegImm32(sp, 1) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r10, stack_ptr, sp, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(dst, .r10, 0) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // x20 = cached sp, x21 = cached stack_ptr, x14 = addr
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.subRegImm12(sp, sp, 1) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x14, stack_ptr, sp, 3) catch return CompileError.OutOfMemory;
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
            const sp = getSpCacheReg();
            self.emitter.subRegImm32(sp, 1) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            const sp = getSpCacheReg();
            self.emitter.subRegImm12(sp, sp, 1) catch return CompileError.OutOfMemory;
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

            // Tag guard: both NaN-boxed integers
            // Check: (value & INT_TYPE_MASK) == INT_TAG for both
            self.emitter.movRegImm64(.r10, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.r11, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdx, .rax) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.rdx, .r10) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.rdx, .r11) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);
            self.emitter.movRegReg(.rdx, .rcx) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.rdx, .r10) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.rdx, .r11) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Unbox: sign-extend lower 32 bits
            self.emitter.movsxdRegReg(.rax, .rax) catch return CompileError.OutOfMemory;
            self.emitter.movsxdRegReg(.rcx, .rcx) catch return CompileError.OutOfMemory;

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

            // Rebox: OR with INT_TAG
            self.emitter.movRegImm64(.r10, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.orRegReg(.rax, .r10) catch return CompileError.OutOfMemory;
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
            try self.emitCallHelperReg(.rax);
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

            // Tag guard: both NaN-boxed integers
            self.emitter.movRegImm64(.x14, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x15, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.x11, .x9, .x14) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x11, .x15) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);
            self.emitter.andRegReg(.x11, .x10, .x14) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x11, .x15) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Unbox: sign-extend lower 32 bits
            self.emitter.sxtwRegReg(.x9, .x9) catch return CompileError.OutOfMemory;
            self.emitter.sxtwRegReg(.x10, .x10) catch return CompileError.OutOfMemory;

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
                    // Check i32 range
                    self.emitter.sxtwRegReg(.x11, .x9) catch return CompileError.OutOfMemory;
                    self.emitter.cmpRegReg(.x9, .x11) catch return CompileError.OutOfMemory;
                    try self.emitBcondToLabel(.ne, slow);
                },
            }

            // Rebox: OR with INT_TAG
            self.emitter.movRegImm64(.x11, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.orrRegReg(.x9, .x9, .x11) catch return CompileError.OutOfMemory;
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
            try self.emitCallHelperReg(.x12);
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
            try self.emitCallHelperReg(.rax);
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
            try self.emitCallHelperReg(.x12);
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
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, argc) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, if (is_method) 1 else 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);
        }
    }

    fn emitCallWithFeedback(self: *BaselineCompiler, argc: u8, is_method: bool, opcode_offset: u32, allow_math_fast_path: bool) CompileError!void {
        if (self.getInlineCandidate(opcode_offset)) |callee| {
            return self.emitInlinedCall(callee, argc, is_method, opcode_offset);
        }
        if (self.getMonomorphicCallTarget(opcode_offset)) |callee| {
            return self.emitMonomorphicCall(callee, argc, is_method);
        }
        if (allow_math_fast_path and try self.emitMathBuiltinFastPath(argc, is_method)) {
            return;
        }
        try self.emitCall(argc, is_method);
    }

    fn emitMathBuiltinFastPath(self: *BaselineCompiler, argc: u8, is_method: bool) CompileError!bool {
        if (argc == 0 or argc > 2) return false;

        const slow = self.newLocalLabel();
        const done = self.newLocalLabel();

        const abs_id: u8 = @intFromEnum(object.BuiltinId.math_abs);
        const floor_id: u8 = @intFromEnum(object.BuiltinId.math_floor);
        const ceil_id: u8 = @intFromEnum(object.BuiltinId.math_ceil);
        const round_id: u8 = @intFromEnum(object.BuiltinId.math_round);
        const min_id: u8 = @intFromEnum(object.BuiltinId.math_min);
        const max_id: u8 = @intFromEnum(object.BuiltinId.math_max);

        if (is_x86_64) {
            const int_min_i32: i32 = std.math.minInt(i32);
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Load function value from stack without modifying sp
            self.emitter.movRegReg(.r10, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, @intCast(@as(i32, argc) + 1)) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.r8, .r11, 0) catch return CompileError.OutOfMemory;

            // Pointer tag check: (raw & 0x7) == 1
            self.emitter.movRegReg(.r9, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r9, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r9, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.r9, .r8);

            // Check MemTag.object in header
            self.emitter.movRegMem32(.r11, .r9, 0) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r11, 0xF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Ensure FUNC_IS_BYTECODE is undefined (native function)
            self.emitter.movRegMem(.r11, .r9, OBJ_FUNC_IS_BYTECODE_OFF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r11, @intCast(@as(u32, @truncate(value_mod.JSValue.undefined_val.raw)))) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Load native function data pointer and verify extern pointer tag
            self.emitter.movRegMem(.r11, .r9, OBJ_FUNC_DATA_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.r10, .r11) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Extract extern pointer and load builtin_id (u8) from arg_count/builtin_id word
            try self.emitExtractPtr(.r11, .r11);
            self.emitter.movzxRegMem16(.r10, .r11, NATIVE_ARGCOUNT_OFF) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm32(.r10, 8) catch return CompileError.OutOfMemory;

            if (argc == 1) {
                const abs_label = self.newLocalLabel();
                const floor_label = self.newLocalLabel();
                const ceil_label = self.newLocalLabel();
                const round_label = self.newLocalLabel();

                self.emitter.cmpRegImm32(.r10, abs_id) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.e, abs_label);
                self.emitter.cmpRegImm32(.r10, floor_id) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.e, floor_label);
                self.emitter.cmpRegImm32(.r10, ceil_id) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.e, ceil_label);
                self.emitter.cmpRegImm32(.r10, round_id) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.e, round_label);
                try self.emitJmpToLabel(slow);

                try self.markLabel(abs_label);
                const abs_helper = self.newLocalLabel();
                const abs_pos = self.newLocalLabel();
                try self.emitPopReg(.rsi);
                try self.emitPopReg(.r11);
                if (is_method) try self.emitPopReg(.r11);
                // Fast path for int args
                self.emitter.movRegReg(.r11, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 0) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, abs_helper);

                self.emitter.movRegReg(.r10, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.r10, 1) catch return CompileError.OutOfMemory;
                self.emitter.movsxdRegReg(.r10, .r10) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r10, int_min_i32) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.e, abs_helper);
                self.emitter.cmpRegImm32(.r10, 0) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ge, abs_pos);
                self.emitter.negReg(.r10) catch return CompileError.OutOfMemory;
                try self.markLabel(abs_pos);
                self.emitter.shlRegImm(.r10, 1) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.rax, .r10) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                try self.markLabel(abs_helper);
                self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rax, @intFromPtr(&jitMathAbs)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.rax);
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                try self.markLabel(floor_label);
                const floor_helper = self.newLocalLabel();
                const floor_inline = self.newLocalLabel();
                try self.emitPopReg(.rsi);
                try self.emitPopReg(.r11);
                if (is_method) try self.emitPopReg(.r11);
                // Check if integer (tag bit 0 == 0)
                self.emitter.movRegReg(.r11, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 0) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, floor_inline);
                try self.emitPushReg(.rsi);
                try self.emitJmpToLabel(done);

                // Check for inline float (tag == 5)
                try self.markLabel(floor_inline);
                self.emitter.movRegReg(.r11, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 5) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, floor_helper);
                // Inline float path: extract f32 bits, convert, round, return int
                self.emitter.movRegReg(.rax, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
                // movd xmm0, eax (load f32 bits)
                self.emitter.movdXmmReg32(.xmm0, .rax) catch return CompileError.OutOfMemory;
                // cvtss2sd xmm0, xmm0 (single to double)
                self.emitter.cvtss2sd(.xmm0, .xmm0) catch return CompileError.OutOfMemory;
                // roundsd xmm0, xmm0, floor
                self.emitter.roundsd(.xmm0, .xmm0, .floor) catch return CompileError.OutOfMemory;
                // cvttsd2si rax, xmm0 (convert to i64)
                self.emitter.cvttsd2siRegXmm(.rax, .xmm0) catch return CompileError.OutOfMemory;
                // Check if fits in i32: compare with INT32_MAX
                self.emitter.movRegImm64(.r11, 0x7FFFFFFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.rax, .r11) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.g, floor_helper);
                // Check INT32_MIN
                self.emitter.movRegImm64(.r11, @bitCast(@as(i64, -0x80000000))) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.rax, .r11) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.l, floor_helper);
                // Create integer JSValue: shl rax, 1 (tag 0)
                self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                try self.markLabel(floor_helper);
                self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rax, @intFromPtr(&jitMathFloor)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.rax);
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                try self.markLabel(ceil_label);
                const ceil_helper = self.newLocalLabel();
                const ceil_inline = self.newLocalLabel();
                try self.emitPopReg(.rsi);
                try self.emitPopReg(.r11);
                if (is_method) try self.emitPopReg(.r11);
                // Check if integer (tag bit 0 == 0)
                self.emitter.movRegReg(.r11, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 0) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, ceil_inline);
                try self.emitPushReg(.rsi);
                try self.emitJmpToLabel(done);

                // Check for inline float (tag == 5)
                try self.markLabel(ceil_inline);
                self.emitter.movRegReg(.r11, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 5) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, ceil_helper);
                // Inline float path: extract f32 bits, convert, round, return int
                self.emitter.movRegReg(.rax, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
                // movd xmm0, eax (load f32 bits)
                self.emitter.movdXmmReg32(.xmm0, .rax) catch return CompileError.OutOfMemory;
                // cvtss2sd xmm0, xmm0 (single to double)
                self.emitter.cvtss2sd(.xmm0, .xmm0) catch return CompileError.OutOfMemory;
                // roundsd xmm0, xmm0, ceil
                self.emitter.roundsd(.xmm0, .xmm0, .ceil) catch return CompileError.OutOfMemory;
                // cvttsd2si rax, xmm0 (convert to i64)
                self.emitter.cvttsd2siRegXmm(.rax, .xmm0) catch return CompileError.OutOfMemory;
                // Check if fits in i32: compare with INT32_MAX
                self.emitter.movRegImm64(.r11, 0x7FFFFFFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.rax, .r11) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.g, ceil_helper);
                // Check INT32_MIN
                self.emitter.movRegImm64(.r11, @bitCast(@as(i64, -0x80000000))) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.rax, .r11) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.l, ceil_helper);
                // Create integer JSValue: shl rax, 1 (tag 0)
                self.emitter.shlRegImm(.rax, 1) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                try self.markLabel(ceil_helper);
                self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rax, @intFromPtr(&jitMathCeil)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.rax);
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                try self.markLabel(round_label);
                const round_helper = self.newLocalLabel();
                try self.emitPopReg(.rsi);
                try self.emitPopReg(.r11);
                if (is_method) try self.emitPopReg(.r11);
                self.emitter.movRegReg(.r11, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 0) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, round_helper);
                try self.emitPushReg(.rsi);
                try self.emitJmpToLabel(done);

                try self.markLabel(round_helper);
                self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rax, @intFromPtr(&jitMathRound)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.rax);
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);
            } else {
                const min_label = self.newLocalLabel();
                const max_label = self.newLocalLabel();

                self.emitter.cmpRegImm32(.r10, min_id) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.e, min_label);
                self.emitter.cmpRegImm32(.r10, max_id) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.e, max_label);
                try self.emitJmpToLabel(slow);

                try self.markLabel(min_label);
                const min_try_float = self.newLocalLabel();
                const min_helper = self.newLocalLabel();
                try self.emitPopReg(.rdx); // arg1
                try self.emitPopReg(.rsi); // arg0
                try self.emitPopReg(.r11); // func
                if (is_method) try self.emitPopReg(.r11); // this
                // Save original values for float path
                self.emitter.movRegReg(.r8, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.r9, .rdx) catch return CompileError.OutOfMemory;
                // Check if both are integers (SMI)
                self.emitter.movRegReg(.r11, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 0) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, min_try_float);
                self.emitter.movRegReg(.r11, .rdx) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 0) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, min_try_float);

                // Integer fast path
                self.emitter.movRegReg(.r10, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.r10, 1) catch return CompileError.OutOfMemory;
                self.emitter.movsxdRegReg(.r10, .r10) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.r11, .rdx) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.movsxdRegReg(.r11, .r11) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.r10, .r11) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.rax, .rdx) catch return CompileError.OutOfMemory;
                self.emitter.cmovcc(.le, .rax, .rsi) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                // Try inline float path
                try self.markLabel(min_try_float);
                // Check if both are inline floats (tag == 5)
                self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, min_helper);
                self.emitter.movRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, min_helper);

                // Both are inline floats - extract and compare
                self.emitter.movRegReg(.rax, .r8) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
                self.emitter.movdXmmReg32(.xmm0, .rax) catch return CompileError.OutOfMemory;
                self.emitter.cvtss2sd(.xmm0, .xmm0) catch return CompileError.OutOfMemory;

                self.emitter.movRegReg(.rax, .r9) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
                self.emitter.movdXmmReg32(.xmm1, .rax) catch return CompileError.OutOfMemory;
                self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory;

                // Use minsd instruction
                self.emitter.minsd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;

                // Try to create inline float result
                self.emitter.cvtsd2ss(.xmm1, .xmm0) catch return CompileError.OutOfMemory;
                self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory;
                self.emitter.ucomisd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, min_helper);
                try self.emitJccToLabel(.p, min_helper); // NaN

                // Create inline float result
                self.emitter.movdReg32Xmm(.rax, .xmm1) catch return CompileError.OutOfMemory;
                self.emitter.shlRegImm(.rax, 32) catch return CompileError.OutOfMemory;
                self.emitter.addRegImm32(.rax, 5) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                try self.markLabel(min_helper);
                self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.rdx, .r9) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rax, @intFromPtr(&jitMathMin2)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.rax);
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                try self.markLabel(max_label);
                const max_try_float = self.newLocalLabel();
                const max_helper = self.newLocalLabel();
                try self.emitPopReg(.rdx); // arg1
                try self.emitPopReg(.rsi); // arg0
                try self.emitPopReg(.r11); // func
                if (is_method) try self.emitPopReg(.r11); // this
                // Save original values for float path
                self.emitter.movRegReg(.r8, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.r9, .rdx) catch return CompileError.OutOfMemory;
                // Check if both are integers (SMI)
                self.emitter.movRegReg(.r11, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 0) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, max_try_float);
                self.emitter.movRegReg(.r11, .rdx) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r11, 0) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, max_try_float);

                // Integer fast path
                self.emitter.movRegReg(.r10, .rsi) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.r10, 1) catch return CompileError.OutOfMemory;
                self.emitter.movsxdRegReg(.r10, .r10) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.r11, .rdx) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.r11, 1) catch return CompileError.OutOfMemory;
                self.emitter.movsxdRegReg(.r11, .r11) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.r10, .r11) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.rax, .rdx) catch return CompileError.OutOfMemory;
                self.emitter.cmovcc(.ge, .rax, .rsi) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                // Try inline float path
                try self.markLabel(max_try_float);
                // Check if both are inline floats (tag == 5)
                self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, max_helper);
                self.emitter.movRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
                self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, max_helper);

                // Both are inline floats - extract and compare
                self.emitter.movRegReg(.rax, .r8) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
                self.emitter.movdXmmReg32(.xmm0, .rax) catch return CompileError.OutOfMemory;
                self.emitter.cvtss2sd(.xmm0, .xmm0) catch return CompileError.OutOfMemory;

                self.emitter.movRegReg(.rax, .r9) catch return CompileError.OutOfMemory;
                self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
                self.emitter.movdXmmReg32(.xmm1, .rax) catch return CompileError.OutOfMemory;
                self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory;

                // Use maxsd instruction
                self.emitter.maxsd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;

                // Try to create inline float result
                self.emitter.cvtsd2ss(.xmm1, .xmm0) catch return CompileError.OutOfMemory;
                self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory;
                self.emitter.ucomisd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;
                try self.emitJccToLabel(.ne, max_helper);
                try self.emitJccToLabel(.p, max_helper); // NaN

                // Create inline float result
                self.emitter.movdReg32Xmm(.rax, .xmm1) catch return CompileError.OutOfMemory;
                self.emitter.shlRegImm(.rax, 32) catch return CompileError.OutOfMemory;
                self.emitter.addRegImm32(.rax, 5) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);

                try self.markLabel(max_helper);
                self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.rdx, .r9) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.rax, @intFromPtr(&jitMathMax2)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.rax);
                try self.emitPushReg(.rax);
                try self.emitJmpToLabel(done);
            }

            try self.markLabel(slow);
            try self.emitCall(argc, is_method);
            try self.markLabel(done);
        } else if (is_aarch64) {
            const int_min_i64: u64 = @bitCast(@as(i64, std.math.minInt(i32)));
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Load function value from stack without modifying sp
            self.emitter.movRegReg(.x9, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(.x9, .x9, @intCast(@as(u32, argc) + 1)) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x10, stack_ptr, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x12, .x10, 0) catch return CompileError.OutOfMemory;

            // Pointer tag check: (raw & 0x7) == 1
            self.emitter.andRegImm(.x11, .x12, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.x11, .x12);

            // Check MemTag.object in header
            self.emitter.ldrImmW(.x10, .x11, 0) catch return CompileError.OutOfMemory;
            self.emitter.lsrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm(.x10, .x10, 0xF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x10, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Ensure FUNC_IS_BYTECODE is undefined (native function)
            self.emitter.ldrImm(.x10, .x11, OBJ_FUNC_IS_BYTECODE_OFF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x10, @intCast(@as(u12, @truncate(value_mod.JSValue.undefined_val.raw)))) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Load native function data pointer and verify extern pointer tag
            self.emitter.ldrImm(.x10, .x11, OBJ_FUNC_DATA_OFF) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm(.x9, .x10, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x9, 7) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract extern pointer and load builtin_id (u8) from arg_count/builtin_id word
            try self.emitExtractPtr(.x10, .x10);
            self.emitter.ldrImmW(.x9, .x10, NATIVE_ARGCOUNT_OFF) catch return CompileError.OutOfMemory;
            self.emitter.lsrRegImm(.x9, .x9, 8) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x9, .x9, 56) catch return CompileError.OutOfMemory;
            self.emitter.lsrRegImm(.x9, .x9, 56) catch return CompileError.OutOfMemory;

            if (argc == 1) {
                const abs_label = self.newLocalLabel();
                const floor_label = self.newLocalLabel();
                const ceil_label = self.newLocalLabel();
                const round_label = self.newLocalLabel();

                self.emitter.cmpRegImm12(.x9, abs_id) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.eq, abs_label);
                self.emitter.cmpRegImm12(.x9, floor_id) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.eq, floor_label);
                self.emitter.cmpRegImm12(.x9, ceil_id) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.eq, ceil_label);
                self.emitter.cmpRegImm12(.x9, round_id) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.eq, round_label);
                try self.emitJmpToLabel(slow);

                try self.markLabel(abs_label);
                const abs_helper = self.newLocalLabel();
                const abs_pos = self.newLocalLabel();
                try self.emitPopReg(.x1);
                try self.emitPopReg(.x11);
                if (is_method) try self.emitPopReg(.x11);
                self.emitter.andRegImm(.x9, .x1, 0x1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, abs_helper);

                self.emitter.lsrRegImm(.x9, .x1, 1) catch return CompileError.OutOfMemory;
                self.emitter.lslRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                self.emitter.asrRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x10, int_min_i64) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.eq, abs_helper);
                self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ge, abs_pos);
                self.emitter.negReg(.x9, .x9) catch return CompileError.OutOfMemory;
                try self.markLabel(abs_pos);
                self.emitter.lslRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.x9);
                try self.emitJmpToLabel(done);

                try self.markLabel(abs_helper);
                self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x12, @intFromPtr(&jitMathAbs)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.x12);
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);

                try self.markLabel(floor_label);
                const floor_helper = self.newLocalLabel();
                try self.emitPopReg(.x1);
                try self.emitPopReg(.x11);
                if (is_method) try self.emitPopReg(.x11);
                self.emitter.andRegImm(.x9, .x1, 0x1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, floor_helper);
                try self.emitPushReg(.x1);
                try self.emitJmpToLabel(done);

                try self.markLabel(floor_helper);
                self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x12, @intFromPtr(&jitMathFloor)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.x12);
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);

                try self.markLabel(ceil_label);
                const ceil_helper = self.newLocalLabel();
                try self.emitPopReg(.x1);
                try self.emitPopReg(.x11);
                if (is_method) try self.emitPopReg(.x11);
                self.emitter.andRegImm(.x9, .x1, 0x1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, ceil_helper);
                try self.emitPushReg(.x1);
                try self.emitJmpToLabel(done);

                try self.markLabel(ceil_helper);
                self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x12, @intFromPtr(&jitMathCeil)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.x12);
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);

                try self.markLabel(round_label);
                const round_helper = self.newLocalLabel();
                try self.emitPopReg(.x1);
                try self.emitPopReg(.x11);
                if (is_method) try self.emitPopReg(.x11);
                self.emitter.andRegImm(.x9, .x1, 0x1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, round_helper);
                try self.emitPushReg(.x1);
                try self.emitJmpToLabel(done);

                try self.markLabel(round_helper);
                self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x12, @intFromPtr(&jitMathRound)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.x12);
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);
            } else {
                const min_label = self.newLocalLabel();
                const max_label = self.newLocalLabel();

                self.emitter.cmpRegImm12(.x9, min_id) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.eq, min_label);
                self.emitter.cmpRegImm12(.x9, max_id) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.eq, max_label);
                try self.emitJmpToLabel(slow);

                try self.markLabel(min_label);
                const min_try_float = self.newLocalLabel();
                const min_helper = self.newLocalLabel();
                try self.emitPopReg(.x2); // arg1
                try self.emitPopReg(.x1); // arg0
                try self.emitPopReg(.x11); // func
                if (is_method) try self.emitPopReg(.x11); // this
                // Save original values for float path
                self.emitter.movRegReg(.x12, .x1) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.x13, .x2) catch return CompileError.OutOfMemory;
                // Check if both are integers (SMI)
                self.emitter.andRegImm(.x9, .x1, 0x1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, min_try_float);
                self.emitter.andRegImm(.x9, .x2, 0x1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, min_try_float);

                // Integer fast path
                self.emitter.lsrRegImm(.x9, .x1, 1) catch return CompileError.OutOfMemory;
                self.emitter.lslRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                self.emitter.asrRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                self.emitter.lsrRegImm(.x10, .x2, 1) catch return CompileError.OutOfMemory;
                self.emitter.lslRegImm(.x10, .x10, 32) catch return CompileError.OutOfMemory;
                self.emitter.asrRegImm(.x10, .x10, 32) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;
                self.emitter.csel(.x0, .x1, .x2, .le) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);

                // Try inline float path
                try self.markLabel(min_try_float);
                // Check if both are inline floats (tag == 5)
                self.emitter.andRegImm(.x9, .x12, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 5) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, min_helper);
                self.emitter.andRegImm(.x9, .x13, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 5) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, min_helper);

                // Both are inline floats - extract and use fmin
                self.emitter.lsrRegImm(.x9, .x12, 32) catch return CompileError.OutOfMemory;
                self.emitter.fmovSingleFromGpr32(.x0, .x9) catch return CompileError.OutOfMemory;
                self.emitter.fcvtDoubleFromSingle(.x0, .x0) catch return CompileError.OutOfMemory;

                self.emitter.lsrRegImm(.x10, .x13, 32) catch return CompileError.OutOfMemory;
                self.emitter.fmovSingleFromGpr32(.x1, .x10) catch return CompileError.OutOfMemory;
                self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory;

                // Use fminDouble instruction
                self.emitter.fminDouble(.x0, .x0, .x1) catch return CompileError.OutOfMemory;

                // Try to create inline float result
                self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory;
                self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory;
                self.emitter.fcmpDouble(.x0, .x1) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, min_helper);
                try self.emitBcondToLabel(.vs, min_helper); // NaN

                // Create inline float result
                self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory;
                self.emitter.fmovGpr32FromSingle(.x9, .x1) catch return CompileError.OutOfMemory;
                self.emitter.lslRegImm(.x0, .x9, 32) catch return CompileError.OutOfMemory;
                self.emitter.addRegImm12(.x0, .x0, 5) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);

                try self.markLabel(min_helper);
                self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.x2, .x13) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x14, @intFromPtr(&jitMathMin2)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.x14);
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);

                try self.markLabel(max_label);
                const max_try_float = self.newLocalLabel();
                const max_helper = self.newLocalLabel();
                try self.emitPopReg(.x2); // arg1
                try self.emitPopReg(.x1); // arg0
                try self.emitPopReg(.x11); // func
                if (is_method) try self.emitPopReg(.x11); // this
                // Save original values for float path
                self.emitter.movRegReg(.x12, .x1) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.x13, .x2) catch return CompileError.OutOfMemory;
                // Check if both are integers (SMI)
                self.emitter.andRegImm(.x9, .x1, 0x1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, max_try_float);
                self.emitter.andRegImm(.x9, .x2, 0x1) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, max_try_float);

                // Integer fast path
                self.emitter.lsrRegImm(.x9, .x1, 1) catch return CompileError.OutOfMemory;
                self.emitter.lslRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                self.emitter.asrRegImm(.x9, .x9, 32) catch return CompileError.OutOfMemory;
                self.emitter.lsrRegImm(.x10, .x2, 1) catch return CompileError.OutOfMemory;
                self.emitter.lslRegImm(.x10, .x10, 32) catch return CompileError.OutOfMemory;
                self.emitter.asrRegImm(.x10, .x10, 32) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;
                self.emitter.csel(.x0, .x1, .x2, .ge) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);

                // Try inline float path
                try self.markLabel(max_try_float);
                // Check if both are inline floats (tag == 5)
                self.emitter.andRegImm(.x9, .x12, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 5) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, max_helper);
                self.emitter.andRegImm(.x9, .x13, 7) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegImm12(.x9, 5) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, max_helper);

                // Both are inline floats - extract and use fmax
                self.emitter.lsrRegImm(.x9, .x12, 32) catch return CompileError.OutOfMemory;
                self.emitter.fmovSingleFromGpr32(.x0, .x9) catch return CompileError.OutOfMemory;
                self.emitter.fcvtDoubleFromSingle(.x0, .x0) catch return CompileError.OutOfMemory;

                self.emitter.lsrRegImm(.x10, .x13, 32) catch return CompileError.OutOfMemory;
                self.emitter.fmovSingleFromGpr32(.x1, .x10) catch return CompileError.OutOfMemory;
                self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory;

                // Use fmaxDouble instruction
                self.emitter.fmaxDouble(.x0, .x0, .x1) catch return CompileError.OutOfMemory;

                // Try to create inline float result
                self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory;
                self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory;
                self.emitter.fcmpDouble(.x0, .x1) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, max_helper);
                try self.emitBcondToLabel(.vs, max_helper); // NaN

                // Create inline float result
                self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory;
                self.emitter.fmovGpr32FromSingle(.x9, .x1) catch return CompileError.OutOfMemory;
                self.emitter.lslRegImm(.x0, .x9, 32) catch return CompileError.OutOfMemory;
                self.emitter.addRegImm12(.x0, .x0, 5) catch return CompileError.OutOfMemory;
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);

                try self.markLabel(max_helper);
                self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
                self.emitter.movRegReg(.x2, .x13) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x14, @intFromPtr(&jitMathMax2)) catch return CompileError.OutOfMemory;
                try self.emitCallHelperReg(.x14);
                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);
            }

            try self.markLabel(slow);
            try self.emitCall(argc, is_method);
            try self.markLabel(done);
        }

        return true;
    }

    /// Emit monomorphic call fast path with callee guard.
    /// Falls back to generic call helper on mismatch.
    /// Uses streamlined guard_id comparison: single 64-bit compare instead of 5 field checks.
    fn emitMonomorphicCall(self: *BaselineCompiler, callee: *const bytecode.FunctionBytecode, argc: u8, is_method: bool) CompileError!void {
        // Use fast helper that skips redundant validation since guards already checked
        const fn_ptr = @intFromPtr(&jitCallBytecodeFast);
        const expected_ptr: u64 = @intFromPtr(callee);

        // Ensure callee has a guard_id assigned
        const callee_mut = @constCast(callee);
        callee_mut.ensureGuardId();
        const expected_guard_id: u64 = callee_mut.guard_id;

        const slow = self.newLocalLabel();
        const done = self.newLocalLabel();

        if (is_x86_64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Load function value from stack without modifying sp
            self.emitter.movRegReg(.r10, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, @intCast(@as(i32, argc) + 1)) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.r8, .r11, 0) catch return CompileError.OutOfMemory;

            // Fast guard: pointer tag check + single guard_id comparison
            // Pointer tag check: (raw & 0x7) == 1 (needed for safe memory access)
            self.emitter.movRegReg(.r9, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r9, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r9, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.r9, .r8);

            // Load guard_id from FUNC_GUARD_ID slot and compare
            // Single 64-bit comparison replaces 5 sequential checks
            self.emitter.movRegMem(.r11, .r9, OBJ_FUNC_GUARD_ID_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.r10, expected_guard_id) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.r11, .r10) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Fast path: call jitCallBytecodeFast (guards verified, skip redundant checks)
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rsi, expected_ptr) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rdx, @intCast(argc)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rcx, if (is_method) 1 else 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            // Slow path: generic call
            try self.markLabel(slow);
            try self.emitCall(argc, is_method);
            try self.markLabel(done);
        } else if (is_aarch64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Load function value from stack without modifying sp
            self.emitter.movRegReg(.x9, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(.x9, .x9, @intCast(@as(u32, argc) + 1)) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x10, stack_ptr, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x12, .x10, 0) catch return CompileError.OutOfMemory;

            // Fast guard: pointer tag check + single guard_id comparison
            // Pointer tag check: (raw & 0x7) == 1 (needed for safe memory access)
            self.emitter.andRegImm(.x11, .x12, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.x11, .x12);

            // Load guard_id from FUNC_GUARD_ID slot and compare
            // Single 64-bit comparison replaces 5 sequential checks
            self.emitter.ldrImm(.x10, .x11, OBJ_FUNC_GUARD_ID_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x13, expected_guard_id) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x13) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Fast path: call jitCallBytecodeFast (guards verified, skip redundant checks)
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, expected_ptr) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, argc) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x3, if (is_method) 1 else 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);
            try self.emitJmpToLabel(done);

            // Slow path
            try self.markLabel(slow);
            try self.emitCall(argc, is_method);
            try self.markLabel(done);
        }
    }

    fn canInlineOpcode(op: Opcode) bool {
        return switch (op) {
            // Exclude complex/unsupported operations from inlining
            .call_spread,
            .array_spread,
            .await_val,
            .make_async,
            .import_module,
            .import_name,
            .export_name,
            .import_default,
            .export_default,
            .get_upvalue,
            .put_upvalue,
            .close_upvalue,
            .make_closure,
            .call_ic,
            => false,
            else => true,
        };
    }

    fn canInlineFunction(callee: *const bytecode.FunctionBytecode) bool {
        // Reject closures/generators/async
        if (callee.upvalue_count > 0) return false;
        if (callee.flags.is_generator or callee.flags.is_async) return false;

        var pc: usize = 0;
        while (pc < callee.code.len) {
            const op: Opcode = @enumFromInt(callee.code[pc]);
            if (!canInlineOpcode(op)) return false;
            pc += 1 + @as(usize, @intCast(bytecode.getOpcodeInfo(op).size - 1));
        }
        return true;
    }

    fn emitInlinePrologue(self: *BaselineCompiler, callee: *const bytecode.FunctionBytecode, argc: u8) CompileError!void {
        const ctx = getCtxReg();
        const sp = getSpCacheReg();
        const saved_fp = getInlineFpSaveReg();

        if (is_x86_64) {
            // Save caller fp into r14
            self.emitter.movRegMem(saved_fp, ctx, CTX_FP_OFF) catch return CompileError.OutOfMemory;
            // new fp = sp - argc
            self.emitter.movRegReg(.r10, sp) catch return CompileError.OutOfMemory;
            if (argc > 0) {
                self.emitter.subRegImm32(.r10, argc) catch return CompileError.OutOfMemory;
            }
            self.emitter.movMemReg(ctx, CTX_FP_OFF, .r10) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Save caller FP to machine stack (x22 is now used for PTR_EXTRACT_MASK)
            self.emitter.ldrImm(.x9, ctx, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
            self.emitter.strPreIndex(.x9, .sp, -16) catch return CompileError.OutOfMemory;
            // new fp = sp - argc
            self.emitter.movRegReg(.x9, sp) catch return CompileError.OutOfMemory;
            if (argc > 0) {
                self.emitter.subRegImm12(.x9, .x9, argc) catch return CompileError.OutOfMemory;
            }
            self.emitter.strImm(.x9, ctx, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
        }

        // Initialize missing locals with undefined (args already on stack)
        var idx: u16 = argc;
        while (idx < callee.local_count) : (idx += 1) {
            try self.emitPushImm64(@bitCast(value_mod.JSValue.undefined_val));
        }
    }

    fn emitInlineCleanup(self: *BaselineCompiler) CompileError!void {
        const ctx = getCtxReg();
        const sp = getSpCacheReg();
        const stack_ptr = getStackPtrCacheReg();
        const saved_fp = getInlineFpSaveReg();
        const result_offset: i32 = if (self.inline_is_method) 2 else 1;

        if (is_x86_64) {
            // Load result from top of stack into rax
            self.emitter.movRegReg(.r10, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rax, .r11, 0) catch return CompileError.OutOfMemory;

            // Load callee fp
            self.emitter.movRegMem(.r10, ctx, CTX_FP_OFF) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, result_offset) catch return CompileError.OutOfMemory;

            // Store result into slot (func or obj)
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.r11, 0, .rax) catch return CompileError.OutOfMemory;

            // sp = result_index + 1
            self.emitter.addRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(sp, .r10) catch return CompileError.OutOfMemory;

            // Restore caller fp
            self.emitter.movMemReg(ctx, CTX_FP_OFF, saved_fp) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Load result from top of stack into x9
            self.emitter.movRegReg(.x10, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(.x10, .x10, 1) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x11, stack_ptr, .x10, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x9, .x11, 0) catch return CompileError.OutOfMemory;

            // Load callee fp
            self.emitter.ldrImm(.x10, ctx, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(.x10, .x10, @intCast(result_offset)) catch return CompileError.OutOfMemory;

            // Store result into slot (func or obj)
            self.emitter.addRegRegShift(.x11, stack_ptr, .x10, 3) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x9, .x11, 0) catch return CompileError.OutOfMemory;

            // sp = result_index + 1
            self.emitter.addRegImm12(.x10, .x10, 1) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(sp, .x10) catch return CompileError.OutOfMemory;

            // Restore caller fp from machine stack
            self.emitter.ldrPostIndex(.x9, .sp, 16) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x9, ctx, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
        }
    }

    fn emitInlinedCall(self: *BaselineCompiler, callee: *const bytecode.FunctionBytecode, argc: u8, is_method: bool, bytecode_offset: u32) CompileError!void {
        if (argc > callee.local_count) {
            try self.emitMonomorphicCall(callee, argc, is_method);
            return;
        }
        if (!canInlineFunction(callee)) {
            try self.emitMonomorphicCall(callee, argc, is_method);
            return;
        }

        const deopt_label = self.newLocalLabel();
        const done_label = self.newLocalLabel();

        // Streamlined guard: verify callee via guard_id (single 64-bit comparison)
        const callee_mut = @constCast(callee);
        callee_mut.ensureGuardId();
        const expected_guard_id: u64 = callee_mut.guard_id;

        if (is_x86_64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Load function value from stack
            self.emitter.movRegReg(.r10, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm32(.r10, @intCast(@as(i32, argc) + 1)) catch return CompileError.OutOfMemory;
            self.emitter.leaRegMem(.r11, stack_ptr, .r10, 3, 0) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.r8, .r11, 0) catch return CompileError.OutOfMemory;

            // Pointer tag check: (raw & 0x7) == 1 (needed for safe memory access)
            self.emitter.movRegReg(.r9, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r9, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r9, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, deopt_label);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.r9, .r8);

            // Load guard_id and compare - single check replaces 5 sequential checks
            self.emitter.movRegMem(.r11, .r9, OBJ_FUNC_GUARD_ID_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.r10, expected_guard_id) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.r11, .r10) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, deopt_label);
        } else if (is_aarch64) {
            const sp = getSpCacheReg();
            const stack_ptr = getStackPtrCacheReg();

            // Load function value from stack
            self.emitter.movRegReg(.x9, sp) catch return CompileError.OutOfMemory;
            self.emitter.subRegImm12(.x9, .x9, @intCast(@as(u32, argc) + 1)) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x10, stack_ptr, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x12, .x10, 0) catch return CompileError.OutOfMemory;

            // Pointer tag check: (raw & 0x7) == 1 (needed for safe memory access)
            self.emitter.andRegImm(.x11, .x12, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, deopt_label);

            // Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.x11, .x12);

            // Load guard_id and compare - single check replaces 5 sequential checks
            self.emitter.ldrImm(.x10, .x11, OBJ_FUNC_GUARD_ID_OFF) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x13, expected_guard_id) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x13) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, deopt_label);
        }

        // Reserve space for the inlined frame to avoid per-push checks.
        // Skip if already within an inline (stack was already reserved by parent inline).
        if (!self.suppress_stack_checks) {
            try self.emitInlineStackCheck(callee.stack_size);
        }

        // Inline prologue (sets ctx->fp, initializes locals)
        try self.emitInlinePrologue(callee, argc);

        // Save caller compiler state and switch to callee
        const saved_func = self.func;
        const saved_tf = self.tf;
        const saved_feedback = self.feedback_site_map;
        const saved_labels = self.labels;
        const saved_pending = self.pending_jumps;
        const saved_jump_targets = self.jump_targets;
        const saved_inline_exit = self.inline_exit_label;
        const saved_inline_is_method = self.inline_is_method;
        const saved_inline_depth = self.inline_depth;
        const saved_total_inlined = self.total_inlined_size;
        const saved_suppress_checks = self.suppress_stack_checks;

        self.func = callee;
        self.tf = callee.type_feedback_ptr;
        self.feedback_site_map = callee.feedback_site_map;
        self.labels = .{};
        self.pending_jumps = .{};
        self.jump_targets = .{};
        self.inline_exit_label = self.newLocalLabel();
        self.inline_is_method = is_method;
        self.inline_depth = saved_inline_depth + 1;
        self.total_inlined_size = saved_total_inlined + @as(u32, @intCast(callee.code.len));
        self.suppress_stack_checks = true;

        // Compile callee bytecode inline (no prologue/epilogue)
        try self.findJumpTargets();
        var pc: u32 = 0;
        const code = callee.code;
        while (pc < code.len) {
            if (self.jump_targets.contains(pc)) {
                self.labels.put(self.allocator, pc, @intCast(self.emitter.buffer.items.len)) catch return CompileError.OutOfMemory;
            }
            const op: Opcode = @enumFromInt(code[pc]);
            pc += 1;
            pc = try self.compileOpcode(op, pc, code);
        }

        // Inline exit: cleanup and return to caller
        try self.markLabel(self.inline_exit_label.?);
        try self.emitInlineCleanup();

        // Patch inline jumps and restore compiler state
        try self.patchJumps();
        self.labels.deinit(self.allocator);
        self.pending_jumps.deinit(self.allocator);
        self.jump_targets.deinit(self.allocator);

        self.func = saved_func;
        self.tf = saved_tf;
        self.feedback_site_map = saved_feedback;
        self.labels = saved_labels;
        self.pending_jumps = saved_pending;
        self.jump_targets = saved_jump_targets;
        self.inline_exit_label = saved_inline_exit;
        self.inline_is_method = saved_inline_is_method;
        self.inline_depth = saved_inline_depth;
        self.suppress_stack_checks = saved_suppress_checks;
        self.total_inlined_size = saved_total_inlined;

        // Skip deopt path on success
        try self.emitJmpToLabel(done_label);

        // Deopt on callee mismatch
        try self.markLabel(deopt_label);
        try self.emitDeoptExit(bytecode_offset, .callee_changed);

        try self.markLabel(done_label);
    }

    fn emitDiv(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitDiv);
        if (is_x86_64) {
            const try_float = self.newLocalLabel();
            const call_helper = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop operands
            try self.emitPopReg(.r9); // b
            try self.emitPopReg(.r8); // a

            // Check if a is inline float (tag == 5)
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, try_float);

            // Check if a is SMI (tag == 0, LSB == 0)
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper);

            // a is SMI, check if b is SMI or inline float
            self.emitter.movRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, try_float);
            self.emitter.movRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper);

            // Both are SMI - convert to f64 and divide
            self.emitter.movRegReg(.rax, .r8) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory; // Unbox a
            self.emitter.cvtsi2sdXmmReg(.xmm0, .rax) catch return CompileError.OutOfMemory;

            self.emitter.movRegReg(.rax, .r9) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory; // Unbox b
            self.emitter.cvtsi2sdXmmReg(.xmm1, .rax) catch return CompileError.OutOfMemory;

            self.emitter.divsd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;

            // Try to create inline float result
            self.emitter.cvtsd2ss(.xmm1, .xmm0) catch return CompileError.OutOfMemory;
            self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory;
            self.emitter.ucomisd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper);
            try self.emitJccToLabel(.p, call_helper);

            // Create inline float
            self.emitter.movdReg32Xmm(.rax, .xmm1) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.rax, 32) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(.rax, 5) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            // Try inline float path
            try self.markLabel(try_float);
            // Check b's type
            self.emitter.movRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper); // b must be inline float for fast path

            // Check if a is inline float or SMI
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
            const a_is_smi = self.newLocalLabel();
            try self.emitJccToLabel(.ne, a_is_smi);

            // a is inline float - extract and convert
            self.emitter.movRegReg(.rax, .r8) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
            self.emitter.movdXmmReg32(.xmm0, .rax) catch return CompileError.OutOfMemory;
            self.emitter.cvtss2sd(.xmm0, .xmm0) catch return CompileError.OutOfMemory;
            const convert_b = self.newLocalLabel();
            try self.emitJmpToLabel(convert_b);

            // a is SMI - convert to f64
            try self.markLabel(a_is_smi);
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper);
            self.emitter.movRegReg(.rax, .r8) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.cvtsi2sdXmmReg(.xmm0, .rax) catch return CompileError.OutOfMemory;

            // Convert b (inline float) to f64
            try self.markLabel(convert_b);
            self.emitter.movRegReg(.rax, .r9) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
            self.emitter.movdXmmReg32(.xmm1, .rax) catch return CompileError.OutOfMemory;
            self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory;

            // Perform division
            self.emitter.divsd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;

            // Try to create inline float result
            self.emitter.cvtsd2ss(.xmm1, .xmm0) catch return CompileError.OutOfMemory;
            self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory;
            self.emitter.ucomisd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper);
            try self.emitJccToLabel(.p, call_helper);

            // Create inline float
            self.emitter.movdReg32Xmm(.rax, .xmm1) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.rax, 32) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm32(.rax, 5) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            // Call helper for complex cases
            try self.markLabel(call_helper);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdx, .r9) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const try_float = self.newLocalLabel();
            const call_helper = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Pop operands
            try self.emitPopReg(.x13); // b
            try self.emitPopReg(.x12); // a

            // Check if a is inline float (tag == 5)
            self.emitter.andRegImm(.x14, .x12, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x14, 5) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, try_float);

            // Check if a is SMI (LSB == 0)
            self.emitter.andRegImm(.x14, .x12, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x14, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);

            // a is SMI, check b
            self.emitter.andRegImm(.x14, .x13, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x14, 5) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, try_float);
            self.emitter.andRegImm(.x14, .x13, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x14, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);

            // Both are SMI - convert to f64 and divide
            self.emitter.asrRegImm(.x9, .x12, 1) catch return CompileError.OutOfMemory;
            self.emitter.scvtfDoubleFromGpr(.x0, .x9) catch return CompileError.OutOfMemory; // d0 = a
            self.emitter.asrRegImm(.x10, .x13, 1) catch return CompileError.OutOfMemory;
            self.emitter.scvtfDoubleFromGpr(.x1, .x10) catch return CompileError.OutOfMemory; // d1 = b
            self.emitter.fdivDouble(.x0, .x0, .x1) catch return CompileError.OutOfMemory;

            // Try to create inline float result
            self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory;
            self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory;
            self.emitter.fcmpDouble(.x0, .x1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);
            try self.emitBcondToLabel(.vs, call_helper);

            // Create inline float
            self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory;
            self.emitter.fmovGpr32FromSingle(.x9, .x1) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x0, .x9, 32) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm12(.x0, .x0, 5) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
            try self.emitJmpToLabel(done);

            // Try inline float path (at least one operand is inline float)
            try self.markLabel(try_float);
            // For simplicity, require both to be either SMI or inline float
            // Check if both are inline floats
            self.emitter.andRegImm(.x14, .x12, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x14, 5) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);
            self.emitter.andRegImm(.x14, .x13, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x14, 5) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);

            // Both are inline floats
            self.emitter.lsrRegImm(.x9, .x12, 32) catch return CompileError.OutOfMemory;
            self.emitter.fmovSingleFromGpr32(.x0, .x9) catch return CompileError.OutOfMemory;
            self.emitter.fcvtDoubleFromSingle(.x0, .x0) catch return CompileError.OutOfMemory;

            self.emitter.lsrRegImm(.x10, .x13, 32) catch return CompileError.OutOfMemory;
            self.emitter.fmovSingleFromGpr32(.x1, .x10) catch return CompileError.OutOfMemory;
            self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory;

            self.emitter.fdivDouble(.x0, .x0, .x1) catch return CompileError.OutOfMemory;

            // Try to create inline float result
            self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory;
            self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory;
            self.emitter.fcmpDouble(.x0, .x1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);
            try self.emitBcondToLabel(.vs, call_helper);

            // Create inline float
            self.emitter.fcvtSingleFromDouble(.x1, .x0) catch return CompileError.OutOfMemory;
            self.emitter.fmovGpr32FromSingle(.x9, .x1) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x0, .x9, 32) catch return CompileError.OutOfMemory;
            self.emitter.addRegImm12(.x0, .x0, 5) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
            try self.emitJmpToLabel(done);

            // Call helper
            try self.markLabel(call_helper);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x2, .x13) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    fn emitMod(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitMod);
        if (is_x86_64) {
            try self.emitPopReg(.rdx); // b
            try self.emitPopReg(.rsi); // a
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // b
            try self.emitPopReg(.x1); // a
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
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
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // b
            try self.emitPopReg(.x1); // a
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
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
            try self.emitCallHelperReg(.rax);
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
            try self.emitCallHelperReg(.x14);
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    /// Emit code for concat_n: concatenate N values from stack into a single string.
    /// This is a simple helper call since string concatenation is complex and
    /// the performance gain comes from single allocation in the helper, not from inlining.
    fn emitConcatN(self: *BaselineCompiler, count: u8) CompileError!void {
        const fn_ptr = @intFromPtr(&jitConcatN);

        if (is_x86_64) {
            // Call jitConcatN(ctx, count)
            // The helper will pop count values from the stack and push the result
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory; // ctx
            self.emitter.movRegImm64(.rsi, count) catch return CompileError.OutOfMemory; // count
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            // Call jitConcatN(ctx, count)
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory; // ctx
            self.emitter.movRegImm64(.x1, count) catch return CompileError.OutOfMemory; // count
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);
        }
    }

    /// Emit code for math_floor: pop arg, push floor(arg)
    /// ARM64: inline integer fast path + inline FRINTM for raw doubles
    /// x86-64: inline integer fast path, helper call for doubles
    fn emitMathFloor(self: *BaselineCompiler) CompileError!void {
        try self.emitMathRoundingOp(&jitMathFloor);
    }

    /// Shared implementation for floor/ceil/round: integer fast path + helper call for non-integers
    fn emitMathRoundingOp(self: *BaselineCompiler, helper: *const anyopaque) CompileError!void {
        const done = self.newLocalLabel();
        const slow = self.newLocalLabel();

        if (is_x86_64) {
            try self.emitPopReg(.rsi); // arg
            // Correct integer check: (raw & INT_TYPE_MASK) == INT_TAG
            self.emitter.movRegImm64(.r10, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.r10, .rsi) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.r11, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.r10, .r11) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);
            // Integer: floor/ceil/round(int) = int
            try self.emitPushReg(.rsi);
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory; // ctx
            // rsi already has arg
            self.emitter.movRegImm64(.rax, @intFromPtr(helper)) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            try self.emitPopReg(.x1); // arg

            // Fast integer check using upper 16 bits + type bits:
            // Tagged values have (raw >> 48) == 0xFFFC
            // Integers additionally have (raw >> 44) & 7 == 1
            // Check tagged first (raw >> 48)
            self.emitter.lsrRegImm(.x10, .x1, 48) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x11, 0xFFFC) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x11) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow); // not tagged -> raw double, use helper
            // Tagged: check if integer type (bits 44-46 == 1)
            self.emitter.ubfx(.x10, .x1, 44, 3) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x10, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow); // tagged non-integer -> helper
            // Integer: floor/ceil/round(int) = int, push unchanged
            try self.emitPushReg(.x1);
            try self.emitJmpToLabel(done);

            // All non-integer values: call helper (handles both raw doubles and specials)
            try self.markLabel(slow);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory; // ctx
            self.emitter.movRegImm64(.x9, @intFromPtr(helper)) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    /// Emit code for math_ceil: pop arg, push ceil(arg)
    /// ARM64: inline integer fast path + inline FRINTP for raw doubles
    fn emitMathCeil(self: *BaselineCompiler) CompileError!void {
        try self.emitMathRoundingOp(&jitMathCeil);
    }

    /// Emit code for math_round: pop arg, push round(arg)
    /// ARM64: inline integer fast path + inline FRINTA for raw doubles
    fn emitMathRound(self: *BaselineCompiler) CompileError!void {
        try self.emitMathRoundingOp(&jitMathRound);
    }

    /// Emit code for math_abs: pop arg, push abs(arg)
    /// ARM64: inline for both integers (negate if negative, check INT_MIN)
    /// and raw doubles (FABS instruction)
    fn emitMathAbs(self: *BaselineCompiler) CompileError!void {
        const done = self.newLocalLabel();
        const slow = self.newLocalLabel();

        if (is_x86_64) {
            try self.emitPopReg(.rsi);
            // Correct integer check: (raw & INT_TYPE_MASK) == INT_TAG
            self.emitter.movRegImm64(.r10, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.r10, .rsi) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.r11, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.r10, .r11) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Extract i32 from lower 32 bits of JSValue
            self.emitter.movRegReg(.r10, .rsi) catch return CompileError.OutOfMemory;
            // r10 lower 32 bits = the i32 value (as u32 bitcast)
            // Sign-extend to 64 bits for comparison
            self.emitter.movsxdRegReg(.r10, .r10) catch return CompileError.OutOfMemory;

            // Check for INT_MIN (abs overflow)
            const int_min_i32: i32 = std.math.minInt(i32);
            self.emitter.cmpRegImm32(.r10, int_min_i32) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, slow);

            // Already non-negative?
            const pos = self.newLocalLabel();
            self.emitter.cmpRegImm32(.r10, 0) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ge, pos);
            self.emitter.negReg(.r10) catch return CompileError.OutOfMemory;

            try self.markLabel(pos);
            // Re-encode as integer JSValue: INT_TAG | (u32)result
            // r10 has the absolute value, mask to lower 32 bits
            self.emitter.movRegImm64(.r11, 0xFFFFFFFF) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.r10, .r11) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.r11, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.orRegReg(.r10, .r11) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.r10);
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, @intFromPtr(&jitMathAbs)) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const pos = self.newLocalLabel();

            try self.emitPopReg(.x1); // arg

            // Fast integer check: tagged + TYPE_INT
            self.emitter.lsrRegImm(.x10, .x1, 48) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x11, 0xFFFC) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x11) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow); // not tagged -> helper
            self.emitter.ubfx(.x10, .x1, 44, 3) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x10, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow); // tagged non-integer -> helper

            // Integer abs: extract i32, negate if negative, re-encode
            // Extract i32: lower 32 bits, sign-extend
            self.emitter.lslRegImm(.x10, .x1, 32) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 32) catch return CompileError.OutOfMemory;
            // Check for INT_MIN (abs(-2147483648) overflows i32)
            const int_min_u64: u64 = @bitCast(@as(i64, std.math.minInt(i32)));
            self.emitter.movRegImm64(.x11, int_min_u64) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x11) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, slow);
            // If non-negative, skip negate
            self.emitter.cmpRegImm12(.x10, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ge, pos);
            self.emitter.negReg(.x10, .x10) catch return CompileError.OutOfMemory;

            try self.markLabel(pos);
            // Re-encode as JSValue int: INT_TAG | (u32)abs_val
            self.emitter.ubfx(.x10, .x10, 0, 32) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x11, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.orrRegReg(.x10, .x10, .x11) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x10);
            try self.emitJmpToLabel(done);

            // Non-integer (raw doubles and specials): call helper
            try self.markLabel(slow);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory; // ctx
            self.emitter.movRegImm64(.x9, @intFromPtr(&jitMathAbs)) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    /// Emit code for math_min2: pop 2 args, push min(a, b)
    /// ARM64: inline for both integers (CSEL) and raw doubles (FMIN)
    fn emitMathMin2(self: *BaselineCompiler) CompileError!void {
        try self.emitMathMinMax(&jitMathMin2, .min);
    }

    /// Emit code for math_max2: pop 2 args, push max(a, b)
    /// ARM64: inline for both integers (CSEL) and raw doubles (FMAX)
    fn emitMathMax2(self: *BaselineCompiler) CompileError!void {
        try self.emitMathMinMax(&jitMathMax2, .max);
    }

    const MinMaxKind = enum { min, max };

    /// Shared implementation for min/max with correct NaN-boxing integer checks
    fn emitMathMinMax(self: *BaselineCompiler, helper: *const anyopaque, kind: MinMaxKind) CompileError!void {
        const done = self.newLocalLabel();
        const slow = self.newLocalLabel();

        if (is_x86_64) {
            try self.emitPopReg(.rdx); // b
            try self.emitPopReg(.rsi); // a
            // Check if both are integers: (raw & INT_TYPE_MASK) == INT_TAG
            self.emitter.movRegImm64(.r10, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.r11, INT_TAG) catch return CompileError.OutOfMemory;
            // Check a
            self.emitter.movRegReg(.rax, .rsi) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.rax, .r10) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.rax, .r11) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);
            // Check b
            self.emitter.movRegReg(.rax, .rdx) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.rax, .r10) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.rax, .r11) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Both integers: extract i32 from lower 32 bits and sign-extend
            self.emitter.movsxdRegReg(.r10, .rsi) catch return CompileError.OutOfMemory;
            self.emitter.movsxdRegReg(.r11, .rdx) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.r10, .r11) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rax, .rdx) catch return CompileError.OutOfMemory;
            const cond: x86.Condition = if (kind == .min) .le else .ge;
            self.emitter.cmovcc(cond, .rax, .rsi) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            try self.markLabel(slow);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, @intFromPtr(helper)) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const int_path = self.newLocalLabel();

            try self.emitPopReg(.x2); // b
            try self.emitPopReg(.x1); // a

            // Fast integer check for a: (raw >> 48) == 0xFFFC && ubfx(raw, 44, 3) == 1
            self.emitter.lsrRegImm(.x10, .x1, 48) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x11, 0xFFFC) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x11) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow); // a not tagged -> slow
            self.emitter.ubfx(.x10, .x1, 44, 3) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x10, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow); // a not int -> slow
            // Fast integer check for b
            self.emitter.lsrRegImm(.x10, .x2, 48) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x11) catch return CompileError.OutOfMemory; // x11 still 0xFFFC
            try self.emitBcondToLabel(.ne, slow); // b not tagged -> slow
            self.emitter.ubfx(.x10, .x2, 44, 3) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x10, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, int_path); // both ints -> int path
            try self.emitJmpToLabel(slow); // b tagged non-int -> slow

            // --- Integer path ---
            try self.markLabel(int_path);
            // Extract i32 values and sign-extend for signed comparison
            // Lower 32 bits of JSValue contain the i32 payload (bitcast as u32)
            self.emitter.lslRegImm(.x10, .x1, 32) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 32) catch return CompileError.OutOfMemory;
            self.emitter.lslRegImm(.x11, .x2, 32) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x11, .x11, 32) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x10, .x11) catch return CompileError.OutOfMemory;
            // Select the original JSValue (not the extracted int)
            const cond: arm64.Condition = if (kind == .min) .le else .ge;
            self.emitter.csel(.x0, .x1, .x2, cond) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
            try self.emitJmpToLabel(done);

            // --- Slow path ---
            try self.markLabel(slow);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory; // ctx
            self.emitter.movRegImm64(.x9, @intFromPtr(helper)) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);

            try self.markLabel(done);
        }
    }

    /// Emit code to get a local variable and push it onto the JS operand stack
    /// Local variables are stored at ctx->stack[ctx->fp + idx]
    /// With lazy loading: first access loads from memory, subsequent accesses use register.
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
            self.emitter.shlRegImm(.rax, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;
            self.emitter.movRegMem(.rax, .rax, 0) catch return CompileError.OutOfMemory;
            // 5. Push onto operand stack
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            // Check if local is allocated to a register
            if (self.reg_alloc) |*ra| {
                if (self.getRegIndexForLocal(idx)) |reg_idx| {
                    const reg = ra.local_allocs[idx].register.?;

                    // Lazy loading: if not yet loaded, load from memory first
                    if (!ra.loaded[reg_idx]) {
                        // Load from memory into the allocated register
                        self.emitter.ldrImm(.x9, .x19, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
                        self.emitter.ldrImm(.x10, .x19, @intCast(@as(u32, @bitCast(CTX_STACK_PTR_OFF)))) catch return CompileError.OutOfMemory;
                        if (idx > 0) {
                            self.emitter.addRegImm12(.x11, .x9, idx) catch return CompileError.OutOfMemory;
                            self.emitter.addRegRegShift(.x11, .x10, .x11, 3) catch return CompileError.OutOfMemory;
                        } else {
                            self.emitter.addRegRegShift(.x11, .x10, .x9, 3) catch return CompileError.OutOfMemory;
                        }
                        self.emitter.ldrImm(reg, .x11, 0) catch return CompileError.OutOfMemory;
                        ra.loaded[reg_idx] = true;
                    }

                    // Record that we're pushing from a callee-saved register.
                    // If the next instruction is get_field_ic (monomorphic), it can
                    // undo this push and use the register directly, saving 6 instructions.
                    self.pending_reg_push = reg;

                    // Now push the register value
                    return self.emitPushReg(reg);
                }
            }

            // Slow path: load from memory (not allocated to register)
            // Use stack_ptr_cache (x21) instead of reloading from ctx
            const stack_ptr = getStackPtrCacheReg();
            self.emitter.ldrImm(.x9, .x19, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
            if (idx > 0) {
                self.emitter.addRegImm12(.x9, .x9, idx) catch return CompileError.OutOfMemory;
            }
            self.emitter.addRegRegShift(.x11, stack_ptr, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x9, .x11, 0) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
        }
    }

    /// Emit code to pop a value from the JS operand stack and store it in a local variable.
    /// With deferred stores: value stays in register, only written to memory before helpers/exit.
    fn emitPutLocal(self: *BaselineCompiler, idx: u8) CompileError!void {
        if (is_x86_64) {
            // Pop value from operand stack into rax
            try self.emitPopReg(.rax);
            // rbx = ctx pointer
            self.emitter.movRegMem(.rcx, .rbx, CTX_FP_OFF) catch return CompileError.OutOfMemory;
            if (idx > 0) {
                self.emitter.addRegImm32(.rcx, idx) catch return CompileError.OutOfMemory;
            }
            self.emitter.movRegMem(.rdx, .rbx, CTX_STACK_PTR_OFF) catch return CompileError.OutOfMemory;
            self.emitter.shlRegImm(.rcx, 3) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.rcx, .rdx) catch return CompileError.OutOfMemory;
            self.emitter.movMemReg(.rcx, 0, .rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Check if local is allocated to a register
            if (self.reg_alloc) |*ra| {
                if (self.getRegIndexForLocal(idx)) |reg_idx| {
                    const reg = ra.local_allocs[idx].register.?;

                    // Pop directly into the allocated register (no memory store)
                    try self.emitPopReg(reg);

                    // Mark as loaded (we now have the value) and dirty (needs spill before helpers)
                    ra.loaded[reg_idx] = true;
                    ra.dirty[reg_idx] = true;
                    return;
                }
            }

            // Slow path: store to memory (not allocated to register)
            // Use stack_ptr_cache (x21) instead of reloading from ctx
            const stack_ptr = getStackPtrCacheReg();
            try self.emitPopReg(.x9);
            self.emitter.ldrImm(.x10, .x19, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
            if (idx > 0) {
                self.emitter.addRegImm12(.x10, .x10, idx) catch return CompileError.OutOfMemory;
            }
            self.emitter.addRegRegShift(.x12, stack_ptr, .x10, 3) catch return CompileError.OutOfMemory;
            // Store value
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
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x1); // obj
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);
        }
    }

    /// Emit code to get an object property via PIC helper (get_field_ic)
    /// With polymorphic inline cache fast path: checks up to 4 cached shapes,
    /// loads directly from inline slots on hit, falling back to helper on miss.
    fn emitGetFieldIC(self: *BaselineCompiler, atom_idx: u16, cache_idx: u16) CompileError!void {
        const fn_ptr = @intFromPtr(&jitGetFieldIC);
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();
            const use_slot = self.newLocalLabel();

            // Pop object value into r8 (preserved for slow path)
            try self.emitPopReg(.r8);

            // Step 1: Check if pointer (NaN-boxing: (raw & 0x7) == 1)
            self.emitter.movRegReg(.r9, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r9, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r9, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Step 2: Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.r9, .r8);
            // r9 = object pointer

            // Step 3: Check MemTag.object in header
            // Header is first u32: object tag = ((header >> 1) & 0xF) == 1
            self.emitter.movRegMem32(.r10, .r9, 0) catch return CompileError.OutOfMemory; // load header u32
            self.emitter.shrRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 0xF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 1) catch return CompileError.OutOfMemory; // MemTag.object = 1
            try self.emitJccToLabel(.ne, slow);

            // Step 4: Load obj->hidden_class_idx
            self.emitter.movRegMem32(.r10, .r9, OBJ_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
            // r10 = hidden_class_idx (u32, zero-extended to 64-bit)

            // Step 5: Load interpreter pointer from ctx->jit_interpreter
            self.emitter.movRegMem(.r11, .rbx, CTX_JIT_INTERP_OFF) catch return CompileError.OutOfMemory;

            // Step 6: Check if interpreter is set (null check)
            self.emitter.testRegReg(.r11, .r11) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, slow);

            // Step 7: Polymorphic check - test up to PIC_CHECK_COUNT entries
            // Base offset of pic_cache[cache_idx].entries array
            const pic_base = INTERP_PIC_CACHE_OFF + @as(i32, cache_idx) * PIC_SIZE;

            // Check entry 0
            const entry0_hc = pic_base + 0 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF;
            self.emitter.movRegMem32(.rcx, .r11, entry0_hc) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg32(.r10, .rcx) catch return CompileError.OutOfMemory;
            const check1 = self.newLocalLabel();
            try self.emitJccToLabel(.ne, check1);
            // Entry 0 matched - load slot_offset
            self.emitter.movzxRegMem16(.rcx, .r11, pic_base + 0 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(use_slot);

            // Check entry 1
            try self.markLabel(check1);
            const entry1_hc = pic_base + 1 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF;
            self.emitter.movRegMem32(.rcx, .r11, entry1_hc) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg32(.r10, .rcx) catch return CompileError.OutOfMemory;
            const check2 = self.newLocalLabel();
            try self.emitJccToLabel(.ne, check2);
            // Entry 1 matched - load slot_offset
            self.emitter.movzxRegMem16(.rcx, .r11, pic_base + 1 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(use_slot);

            // Check entry 2
            try self.markLabel(check2);
            const entry2_hc = pic_base + 2 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF;
            self.emitter.movRegMem32(.rcx, .r11, entry2_hc) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg32(.r10, .rcx) catch return CompileError.OutOfMemory;
            const check3 = self.newLocalLabel();
            try self.emitJccToLabel(.ne, check3);
            // Entry 2 matched - load slot_offset
            self.emitter.movzxRegMem16(.rcx, .r11, pic_base + 2 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(use_slot);

            // Check entry 3
            try self.markLabel(check3);
            const entry3_hc = pic_base + 3 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF;
            self.emitter.movRegMem32(.rcx, .r11, entry3_hc) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg32(.r10, .rcx) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);
            // Entry 3 matched - load slot_offset
            self.emitter.movzxRegMem16(.rcx, .r11, pic_base + 3 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
            // Fall through to use_slot

            // Step 8: Use slot_offset (in rcx) to load from inline_slots
            try self.markLabel(use_slot);
            // Check if slot < INLINE_SLOT_COUNT (8)
            self.emitter.cmpRegImm32(.rcx, JSObject.INLINE_SLOT_COUNT) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ae, slow); // slot >= 8, use slow path

            // Step 9: Load from inline_slots: obj + inline_slots_offset + slot * 8
            self.emitter.shlRegImm(.rcx, 3) catch return CompileError.OutOfMemory; // slot * 8
            self.emitter.addRegImm32(.rcx, OBJ_INLINE_SLOTS_OFF) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.rcx, .r9) catch return CompileError.OutOfMemory; // obj + offset
            self.emitter.movRegMem(.rax, .rcx, 0) catch return CompileError.OutOfMemory; // load value

            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            // Slow path: call helper
            try self.markLabel(slow);
            // r8 still holds the object value
            self.emitter.movRegReg(.rsi, .r8) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rdx, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rcx, cache_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();
            const use_slot = self.newLocalLabel();

            // Pop object value into x12 (preserved for slow path)
            try self.emitPopReg(.x12);

            // Step 1: Check if pointer (NaN-boxing: (raw & 0x7) == 1)
            // ARM64 andRegImm expects a low-bit mask, so use 0x7 for tag bits.
            self.emitter.andRegImm(.x9, .x12, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x9, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Step 2: Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.x9, .x12);
            // x9 = object pointer

            // Step 3: Check MemTag.object in header
            // Header is first u32: object tag = ((header >> 1) & 0xF) == 1
            self.emitter.ldrImmW(.x10, .x9, 0) catch return CompileError.OutOfMemory; // load header u32
            self.emitter.lsrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;
            // ARM64 andRegImm expects a low-bit mask, so use 0xF for MemTag bits.
            self.emitter.andRegImm(.x10, .x10, 0xF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x10, 1) catch return CompileError.OutOfMemory; // MemTag.object = 1
            try self.emitBcondToLabel(.ne, slow);

            // Step 4: Load obj->hidden_class_idx
            self.emitter.ldrImmW(.x10, .x9, OBJ_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
            // x10 = hidden_class_idx

            // Step 5: Load interpreter pointer from ctx->jit_interpreter
            self.emitter.ldrImm(.x11, .x19, CTX_JIT_INTERP_OFF) catch return CompileError.OutOfMemory;

            // Step 6: Check if interpreter is set (null check)
            self.emitter.cmpRegImm12(.x11, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, slow);

            // Step 7: Polymorphic check - test up to 4 entries
            const pic_base = INTERP_PIC_CACHE_OFF + @as(i32, cache_idx) * PIC_SIZE;
            // Check if base offset fits in 12 bits
            if (pic_base >= 0 and pic_base <= 4095) {
                // x13 = base address of PIC entries for this cache slot
                self.emitter.addRegImm12(.x13, .x11, @intCast(pic_base)) catch return CompileError.OutOfMemory;

                // Check entry 0
                self.emitter.ldrImmW(.x14, .x13, 0 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x10, .x14) catch return CompileError.OutOfMemory;
                const check1 = self.newLocalLabel();
                try self.emitBcondToLabel(.ne, check1);
                // Entry 0 matched - load slot_offset
                self.emitter.ldrhImm(.x14, .x13, 0 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
                try self.emitJmpToLabel(use_slot);

                // Check entry 1
                try self.markLabel(check1);
                self.emitter.ldrImmW(.x14, .x13, 1 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x10, .x14) catch return CompileError.OutOfMemory;
                const check2 = self.newLocalLabel();
                try self.emitBcondToLabel(.ne, check2);
                // Entry 1 matched - load slot_offset
                self.emitter.ldrhImm(.x14, .x13, 1 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
                try self.emitJmpToLabel(use_slot);

                // Check entry 2
                try self.markLabel(check2);
                self.emitter.ldrImmW(.x14, .x13, 2 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x10, .x14) catch return CompileError.OutOfMemory;
                const check3 = self.newLocalLabel();
                try self.emitBcondToLabel(.ne, check3);
                // Entry 2 matched - load slot_offset
                self.emitter.ldrhImm(.x14, .x13, 2 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
                try self.emitJmpToLabel(use_slot);

                // Check entry 3
                try self.markLabel(check3);
                self.emitter.ldrImmW(.x14, .x13, 3 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x10, .x14) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, slow);
                // Entry 3 matched - load slot_offset
                self.emitter.ldrhImm(.x14, .x13, 3 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
                // Fall through to use_slot

                // Step 8: Use slot_offset (in x14) to load from inline_slots
                try self.markLabel(use_slot);
                // Check if slot < INLINE_SLOT_COUNT (8)
                self.emitter.cmpRegImm12(.x14, JSObject.INLINE_SLOT_COUNT) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.hs, slow); // slot >= 8, use slow path

                // Step 9: Load from inline_slots: obj + inline_slots_offset + slot * 8
                self.emitter.lslRegImm(.x14, .x14, 3) catch return CompileError.OutOfMemory; // slot * 8
                self.emitter.addRegImm12(.x14, .x14, @intCast(@as(u32, @bitCast(OBJ_INLINE_SLOTS_OFF)))) catch return CompileError.OutOfMemory;
                self.emitter.addRegReg(.x14, .x9, .x14) catch return CompileError.OutOfMemory; // obj + offset
                self.emitter.ldrImm(.x0, .x14, 0) catch return CompileError.OutOfMemory; // load value

                try self.emitPushReg(.x0);
                try self.emitJmpToLabel(done);
            }
            // else: offset too large, fall through to slow path

            // Slow path: call helper
            try self.markLabel(slow);
            // x12 still holds the object value
            self.emitter.movRegReg(.x1, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x3, cache_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);

            try self.markLabel(done);
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
            try self.emitCallHelperReg(.rax);
            if (keep) {
                try self.emitPushReg(.rax);
            }
        } else if (is_aarch64) {
            try self.emitPopReg(.x3); // val
            try self.emitPopReg(.x1); // obj
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            if (keep) {
                try self.emitPushReg(.x0);
            }
        }
    }

    /// Emit code to set an object property via PIC helper (put_field_ic)
    /// With inline monomorphic cache fast path: checks hidden class match and
    /// stores directly to inline slots, falling back to helper on cache miss.
    fn emitPutFieldIC(self: *BaselineCompiler, atom_idx: u16, cache_idx: u16) CompileError!void {
        const fn_ptr = @intFromPtr(&jitPutFieldIC);
        if (is_x86_64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();
            const use_slot = self.newLocalLabel();

            // Pop value into r8, object into r9 (preserved for slow path)
            try self.emitPopReg(.r8); // val
            try self.emitPopReg(.r9); // obj

            // Step 1: Check if object is pointer (NaN-boxing: (raw & 0x7) == 1)
            self.emitter.movRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 1) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);

            // Step 2: Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.r10, .r9);
            // r10 = object pointer

            // Step 3: Check MemTag.object in header
            self.emitter.movRegMem32(.r11, .r10, 0) catch return CompileError.OutOfMemory; // load header u32
            self.emitter.shrRegImm32(.r11, 1) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r11, 0xF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r11, 1) catch return CompileError.OutOfMemory; // MemTag.object = 1
            try self.emitJccToLabel(.ne, slow);

            // Step 4: Load obj->hidden_class_idx
            self.emitter.movRegMem32(.r11, .r10, OBJ_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
            // r11 = hidden_class_idx

            // Step 5: Load interpreter pointer from ctx->jit_interpreter
            self.emitter.movRegMem(.rax, .rbx, CTX_JIT_INTERP_OFF) catch return CompileError.OutOfMemory;

            // Step 6: Check if interpreter is set (null check)
            self.emitter.testRegReg(.rax, .rax) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, slow);

            // Step 7: Polymorphic check - test up to 4 entries
            const pic_base = INTERP_PIC_CACHE_OFF + @as(i32, cache_idx) * PIC_SIZE;

            // Check entry 0
            const entry0_hc = pic_base + 0 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF;
            self.emitter.movRegMem32(.rcx, .rax, entry0_hc) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg32(.r11, .rcx) catch return CompileError.OutOfMemory;
            const check1 = self.newLocalLabel();
            try self.emitJccToLabel(.ne, check1);
            // Entry 0 matched - load slot_offset
            self.emitter.movzxRegMem16(.rcx, .rax, pic_base + 0 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(use_slot);

            // Check entry 1
            try self.markLabel(check1);
            const entry1_hc = pic_base + 1 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF;
            self.emitter.movRegMem32(.rcx, .rax, entry1_hc) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg32(.r11, .rcx) catch return CompileError.OutOfMemory;
            const check2 = self.newLocalLabel();
            try self.emitJccToLabel(.ne, check2);
            // Entry 1 matched - load slot_offset
            self.emitter.movzxRegMem16(.rcx, .rax, pic_base + 1 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(use_slot);

            // Check entry 2
            try self.markLabel(check2);
            const entry2_hc = pic_base + 2 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF;
            self.emitter.movRegMem32(.rcx, .rax, entry2_hc) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg32(.r11, .rcx) catch return CompileError.OutOfMemory;
            const check3 = self.newLocalLabel();
            try self.emitJccToLabel(.ne, check3);
            // Entry 2 matched - load slot_offset
            self.emitter.movzxRegMem16(.rcx, .rax, pic_base + 2 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
            try self.emitJmpToLabel(use_slot);

            // Check entry 3
            try self.markLabel(check3);
            const entry3_hc = pic_base + 3 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF;
            self.emitter.movRegMem32(.rcx, .rax, entry3_hc) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg32(.r11, .rcx) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, slow);
            // Entry 3 matched - load slot_offset
            self.emitter.movzxRegMem16(.rcx, .rax, pic_base + 3 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
            // Fall through to use_slot

            // Step 8: Use slot_offset (in rcx) to store to inline_slots
            try self.markLabel(use_slot);
            // Check if slot < INLINE_SLOT_COUNT (8)
            self.emitter.cmpRegImm32(.rcx, JSObject.INLINE_SLOT_COUNT) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ae, slow); // slot >= 8, use slow path

            // Step 9: Store to inline_slots: obj + inline_slots_offset + slot * 8
            self.emitter.shlRegImm(.rcx, 3) catch return CompileError.OutOfMemory; // slot * 8
            self.emitter.addRegImm32(.rcx, OBJ_INLINE_SLOTS_OFF) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.rcx, .r10) catch return CompileError.OutOfMemory; // obj + offset
            self.emitter.movMemReg(.rcx, 0, .r8) catch return CompileError.OutOfMemory; // store value

            try self.emitJmpToLabel(done);

            // Slow path: call helper
            try self.markLabel(slow);
            // jitPutFieldIC(ctx, obj, atom_idx, val, cache_idx)
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory; // ctx
            self.emitter.movRegReg(.rsi, .r9) catch return CompileError.OutOfMemory; // obj
            self.emitter.movRegImm32(.rdx, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.rcx, .r8) catch return CompileError.OutOfMemory; // val
            self.emitter.movRegImm32(.r8, cache_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);

            try self.markLabel(done);
        } else if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();
            const use_slot = self.newLocalLabel();

            // Pop value into x12, object into x13 (preserved for slow path)
            try self.emitPopReg(.x12); // val
            try self.emitPopReg(.x13); // obj

            // Step 1: Check if object is pointer (NaN-boxing: (raw & 0x7) == 1)
            // ARM64 andRegImm expects a low-bit mask, so use 0x7 for tag bits.
            self.emitter.andRegImm(.x9, .x13, 0x7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x9, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Step 2: Extract object pointer (clear TAG_PREFIX and low 3 bits)
            try self.emitExtractPtr(.x9, .x13);
            // x9 = object pointer

            // Step 3: Check MemTag.object in header
            self.emitter.ldrImmW(.x10, .x9, 0) catch return CompileError.OutOfMemory; // load header u32
            self.emitter.lsrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;
            // ARM64 andRegImm expects a low-bit mask, so use 0xF for MemTag bits.
            self.emitter.andRegImm(.x10, .x10, 0xF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x10, 1) catch return CompileError.OutOfMemory; // MemTag.object = 1
            try self.emitBcondToLabel(.ne, slow);

            // Step 4: Load obj->hidden_class_idx
            self.emitter.ldrImmW(.x10, .x9, OBJ_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
            // x10 = hidden_class_idx

            // Step 5: Load interpreter pointer from ctx->jit_interpreter
            self.emitter.ldrImm(.x11, .x19, CTX_JIT_INTERP_OFF) catch return CompileError.OutOfMemory;

            // Step 6: Check if interpreter is set (null check)
            self.emitter.cmpRegImm12(.x11, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, slow);

            // Step 7: Polymorphic check - test up to 4 entries
            const pic_base = INTERP_PIC_CACHE_OFF + @as(i32, cache_idx) * PIC_SIZE;
            // Check if base offset fits in 12 bits
            if (pic_base >= 0 and pic_base <= 4095) {
                // x14 = base address of PIC entries for this cache slot
                self.emitter.addRegImm12(.x14, .x11, @intCast(pic_base)) catch return CompileError.OutOfMemory;

                // Check entry 0
                self.emitter.ldrImmW(.x15, .x14, 0 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x10, .x15) catch return CompileError.OutOfMemory;
                const check1 = self.newLocalLabel();
                try self.emitBcondToLabel(.ne, check1);
                // Entry 0 matched - load slot_offset
                self.emitter.ldrhImm(.x15, .x14, 0 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
                try self.emitJmpToLabel(use_slot);

                // Check entry 1
                try self.markLabel(check1);
                self.emitter.ldrImmW(.x15, .x14, 1 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x10, .x15) catch return CompileError.OutOfMemory;
                const check2 = self.newLocalLabel();
                try self.emitBcondToLabel(.ne, check2);
                // Entry 1 matched - load slot_offset
                self.emitter.ldrhImm(.x15, .x14, 1 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
                try self.emitJmpToLabel(use_slot);

                // Check entry 2
                try self.markLabel(check2);
                self.emitter.ldrImmW(.x15, .x14, 2 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x10, .x15) catch return CompileError.OutOfMemory;
                const check3 = self.newLocalLabel();
                try self.emitBcondToLabel(.ne, check3);
                // Entry 2 matched - load slot_offset
                self.emitter.ldrhImm(.x15, .x14, 2 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
                try self.emitJmpToLabel(use_slot);

                // Check entry 3
                try self.markLabel(check3);
                self.emitter.ldrImmW(.x15, .x14, 3 * PIC_ENTRY_SIZE + PIC_ENTRY_HIDDEN_CLASS_OFF) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x10, .x15) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.ne, slow);
                // Entry 3 matched - load slot_offset
                self.emitter.ldrhImm(.x15, .x14, 3 * PIC_ENTRY_SIZE + PIC_ENTRY_SLOT_OFF) catch return CompileError.OutOfMemory;
                // Fall through to use_slot

                // Step 8: Use slot_offset (in x15) to store to inline_slots
                try self.markLabel(use_slot);
                // Check if slot < INLINE_SLOT_COUNT (8)
                self.emitter.cmpRegImm12(.x15, JSObject.INLINE_SLOT_COUNT) catch return CompileError.OutOfMemory;
                try self.emitBcondToLabel(.hs, slow); // slot >= 8, use slow path

                // Step 9: Store to inline_slots: obj + inline_slots_offset + slot * 8
                self.emitter.lslRegImm(.x15, .x15, 3) catch return CompileError.OutOfMemory; // slot * 8
                self.emitter.addRegImm12(.x15, .x15, @intCast(@as(u32, @bitCast(OBJ_INLINE_SLOTS_OFF)))) catch return CompileError.OutOfMemory;
                self.emitter.addRegReg(.x15, .x9, .x15) catch return CompileError.OutOfMemory; // obj + offset
                self.emitter.strImm(.x12, .x15, 0) catch return CompileError.OutOfMemory; // store value

                try self.emitJmpToLabel(done);
            }
            // else: offset too large, fall through to slow path

            // Slow path: call helper
            try self.markLabel(slow);
            // jitPutFieldIC(ctx, obj, atom_idx, val, cache_idx)
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory; // ctx
            self.emitter.movRegReg(.x1, .x13) catch return CompileError.OutOfMemory; // obj
            self.emitter.movRegImm64(.x2, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegReg(.x3, .x12) catch return CompileError.OutOfMemory; // val
            self.emitter.movRegImm64(.x4, cache_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);

            try self.markLabel(done);
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
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // idx
            try self.emitPopReg(.x1); // obj
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
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
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
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
            try self.emitCallHelperReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x2); // val
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, atom_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
        }
    }

    /// Emit code to create a new object via helper
    fn emitNewObject(self: *BaselineCompiler) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitNewObject);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
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
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, length) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);
        }
    }

    /// Emit code to create object with pre-compiled literal shape via helper
    /// Uses fast path that only initializes needed slots when prop_count is known
    fn emitNewObjectLiteral(self: *BaselineCompiler, shape_idx: u16, prop_count: u8) CompileError!void {
        const fn_ptr = @intFromPtr(&Context.jitNewObjectLiteralFast);
        if (is_x86_64) {
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rsi, shape_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rdx, prop_count) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, shape_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x2, prop_count) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            try self.emitPushReg(.x0);
        }
    }

    /// Emit code for direct slot write (used for pre-compiled object literals)
    /// Stack: [..., obj, val] -> [...]
    fn emitSetSlot(self: *BaselineCompiler, slot_idx: u8) CompileError!void {
        // Calculate the offset into inline_slots array
        const slot_offset: i32 = OBJ_INLINE_SLOTS_OFF + @as(i32, slot_idx) * 8;

        if (is_x86_64) {
            // Pop value into scratch register
            try self.emitPopReg(.rax);
            // Pop object pointer into another register
            try self.emitPopReg(.rcx);
            // Extract pointer: AND with PTR_EXTRACT_MASK to clear TAG_PREFIX and low 3 bits
            try self.emitExtractPtr(.rcx, .rcx);
            // Store value to obj->inline_slots[slot_idx]
            self.emitter.movMemReg(.rcx, slot_offset, .rax) catch return CompileError.OutOfMemory;
        } else if (is_aarch64) {
            // Pop value into x0
            try self.emitPopReg(.x0);
            // Pop object into x1
            try self.emitPopReg(.x1);
            // Extract pointer: AND with PTR_EXTRACT_MASK
            try self.emitExtractPtr(.x1, .x1);
            // Store value to obj->inline_slots[slot_idx]
            self.emitter.strImm(.x0, .x1, slot_offset) catch return CompileError.OutOfMemory;
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
            try self.emitCallHelperReg(.rax);
        } else if (is_aarch64) {
            try self.emitPopReg(.x3); // val
            try self.emitPopReg(.x2); // idx
            try self.emitPopReg(.x1); // obj
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
        }
    }

    fn emitForOfNext(self: *BaselineCompiler, target: u32) CompileError!void {
        // Inline range iterator fast path, falling back to helper for arrays/other iterators
        //
        // Stack layout: [iter_val @ sp-2, idx_val @ sp-1]
        // Range iterator: check class_id == 23 (range_iterator), idx < length
        // Compute element = start + idx * step, push element, update idx to idx+1

        const sp = getSpCacheReg();
        const stack_ptr = getStackPtrCacheReg();

        if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Load iter_val from stack[sp-2]
            // x9 = &stack[(sp-2)] = stack_ptr + (sp-2)*8
            self.emitter.subRegImm12(.x9, sp, 2) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x9, stack_ptr, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x10, .x9, 0) catch return CompileError.OutOfMemory; // x10 = iter_val

            // Load idx_val from stack[sp-1]
            self.emitter.ldrImm(.x11, .x9, 8) catch return CompileError.OutOfMemory; // x11 = idx_val

            // Check iter_val is a pointer: (raw & 7) == 1
            self.emitter.andRegImm(.x12, .x10, 0b111) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x12, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract object pointer from iter_val
            self.emitter.andRegReg(.x10, .x10, .x22) catch return CompileError.OutOfMemory; // x10 = obj ptr

            // Check memtag == object (1): load header u32, (header >> 1) & 0xF == 1
            self.emitter.ldrImmW(.x12, .x10, 0) catch return CompileError.OutOfMemory; // x12 = header word
            // (header >> 1) & 0xF: shift right 1, mask low 4 bits
            self.emitter.lsrRegImm(.x12, .x12, 1) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm(.x12, .x12, 0b1111) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x12, 1) catch return CompileError.OutOfMemory; // MemTag.object == 1
            try self.emitBcondToLabel(.ne, slow);

            // Check class_id == range_iterator (23)
            self.emitter.ldrbImm(.x12, .x10, OBJ_CLASS_ID_OFF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x12, 23) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Check idx_val is an integer: (raw & INT_TYPE_MASK) == INT_TAG
            self.emitter.movRegImm64(.x12, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.x13, .x11, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x12, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x13, .x12) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract idx as u32 (lower 32 bits of NaN-boxed int)
            self.emitter.ubfx(.x13, .x11, 0, 32) catch return CompileError.OutOfMemory; // x13 = idx

            // Load RANGE_LENGTH from inline_slots[3] and extract u32
            self.emitter.ldrImm(.x14, .x10, OBJ_RANGE_LENGTH_OFF) catch return CompileError.OutOfMemory;
            self.emitter.ubfx(.x14, .x14, 0, 32) catch return CompileError.OutOfMemory; // x14 = length

            // Compare idx < length (unsigned)
            self.emitter.cmpRegReg(.x13, .x14) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.hs, target); // idx >= length means done

            // Load start and step, extract u32
            self.emitter.ldrImm(.x14, .x10, OBJ_RANGE_START_OFF) catch return CompileError.OutOfMemory;
            self.emitter.ubfx(.x14, .x14, 0, 32) catch return CompileError.OutOfMemory; // x14 = start
            self.emitter.ldrImm(.x15, .x10, OBJ_RANGE_STEP_OFF) catch return CompileError.OutOfMemory;
            self.emitter.ubfx(.x15, .x15, 0, 32) catch return CompileError.OutOfMemory; // x15 = step

            // Compute element = start + idx * step
            self.emitter.mulRegReg(.x15, .x13, .x15) catch return CompileError.OutOfMemory; // x15 = idx * step
            self.emitter.addRegReg(.x14, .x14, .x15) catch return CompileError.OutOfMemory; // x14 = element (u32)
            // Truncate to 32 bits to handle signed wraparound correctly
            self.emitter.ubfx(.x14, .x14, 0, 32) catch return CompileError.OutOfMemory;

            // Encode as JSValue int: INT_TAG | (u32)val
            self.emitter.movRegImm64(.x15, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x14, .x14, .x15) catch return CompileError.OutOfMemory; // x14 = JSValue int

            // Push element to stack[sp] and increment sp
            self.emitter.addRegRegShift(.x15, stack_ptr, sp, 3) catch return CompileError.OutOfMemory; // &stack[sp]
            self.emitter.strImm(.x14, .x15, 0) catch return CompileError.OutOfMemory; // stack[sp] = element
            self.emitter.addRegImm12(sp, sp, 1) catch return CompileError.OutOfMemory; // sp++

            // Update idx on stack: stack[sp-2] = JSValue.fromInt(idx + 1)
            // sp was incremented, so sp-2 is the old sp-1 position
            self.emitter.addRegImm12(.x13, .x13, 1) catch return CompileError.OutOfMemory; // idx + 1
            self.emitter.movRegImm64(.x15, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x13, .x13, .x15) catch return CompileError.OutOfMemory; // encode as JSValue int
            // x9 still points to stack[sp-2] (before sp increment), +8 = stack[sp-1] = idx slot
            self.emitter.strImm(.x13, .x9, 8) catch return CompileError.OutOfMemory;

            // Continue loop (don't jump to target)
            try self.emitJmpToLabel(done);

            // Slow path: call helper for arrays and other iterators
            try self.markLabel(slow);
            const fn_ptr = @intFromPtr(&jitForOfNext);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            self.emitter.cmpRegImm12(.x0, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, target);

            try self.markLabel(done);
        } else if (is_x86_64) {
            // x86-64: fall back to helper call (range iterator optimization is ARM64-focused)
            const fn_ptr = @intFromPtr(&jitForOfNext);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            self.emitter.testRegReg(.rax, .rax) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, target);
        }
    }

    fn emitForOfNextPutLoc(self: *BaselineCompiler, local_idx: u8, target: u32) CompileError!void {
        // Inline range iterator fast path for for-of with local variable
        //
        // Stack layout: [iter_val @ sp-2, idx_val @ sp-1]
        // Range iterator: check class_id == 23 (range_iterator), idx < length
        // Compute element = start + idx * step, store to local, update idx to idx+1

        const sp = getSpCacheReg();
        const stack_ptr = getStackPtrCacheReg();
        const ctx = getCtxReg();

        if (is_aarch64) {
            const slow = self.newLocalLabel();
            const done = self.newLocalLabel();

            // Load iter_val from stack[sp-2] and idx_val from stack[sp-1]
            self.emitter.subRegImm12(.x9, sp, 2) catch return CompileError.OutOfMemory;
            self.emitter.addRegRegShift(.x9, stack_ptr, .x9, 3) catch return CompileError.OutOfMemory;
            self.emitter.ldrImm(.x10, .x9, 0) catch return CompileError.OutOfMemory; // x10 = iter_val
            self.emitter.ldrImm(.x11, .x9, 8) catch return CompileError.OutOfMemory; // x11 = idx_val

            // Check iter_val is a pointer: (raw & 7) == 1
            self.emitter.andRegImm(.x12, .x10, 0b111) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x12, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract object pointer
            self.emitter.andRegReg(.x10, .x10, .x22) catch return CompileError.OutOfMemory; // x10 = obj ptr

            // Check memtag == object (1)
            self.emitter.ldrImmW(.x12, .x10, 0) catch return CompileError.OutOfMemory; // header word
            self.emitter.lsrRegImm(.x12, .x12, 1) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm(.x12, .x12, 0b1111) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x12, 1) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Check class_id == range_iterator (23)
            self.emitter.ldrbImm(.x12, .x10, OBJ_CLASS_ID_OFF) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x12, 23) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Check idx_val is an integer
            self.emitter.movRegImm64(.x12, INT_TYPE_MASK) catch return CompileError.OutOfMemory;
            self.emitter.andRegReg(.x13, .x11, .x12) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x12, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x13, .x12) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, slow);

            // Extract idx as u32 and range_length as u32
            self.emitter.ubfx(.x13, .x11, 0, 32) catch return CompileError.OutOfMemory; // x13 = idx
            self.emitter.ldrImm(.x14, .x10, OBJ_RANGE_LENGTH_OFF) catch return CompileError.OutOfMemory;
            self.emitter.ubfx(.x14, .x14, 0, 32) catch return CompileError.OutOfMemory; // x14 = length

            // Compare idx < length (unsigned)
            self.emitter.cmpRegReg(.x13, .x14) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.hs, target); // idx >= length means done

            // Load start and step, extract u32
            self.emitter.ldrImm(.x14, .x10, OBJ_RANGE_START_OFF) catch return CompileError.OutOfMemory;
            self.emitter.ubfx(.x14, .x14, 0, 32) catch return CompileError.OutOfMemory; // x14 = start
            self.emitter.ldrImm(.x15, .x10, OBJ_RANGE_STEP_OFF) catch return CompileError.OutOfMemory;
            self.emitter.ubfx(.x15, .x15, 0, 32) catch return CompileError.OutOfMemory; // x15 = step

            // Compute element = start + idx * step
            self.emitter.mulRegReg(.x15, .x13, .x15) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x14, .x14, .x15) catch return CompileError.OutOfMemory; // x14 = element
            // Truncate to 32 bits to handle signed wraparound correctly
            self.emitter.ubfx(.x14, .x14, 0, 32) catch return CompileError.OutOfMemory;

            // Encode as JSValue int: INT_TAG | (u32)val
            self.emitter.movRegImm64(.x15, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x14, .x14, .x15) catch return CompileError.OutOfMemory; // x14 = JSValue

            // Store element to local: stack[fp + local_idx]
            self.emitter.ldrImm(.x15, ctx, @intCast(@as(u32, @bitCast(CTX_FP_OFF)))) catch return CompileError.OutOfMemory;
            if (local_idx > 0) {
                self.emitter.addRegImm12(.x15, .x15, local_idx) catch return CompileError.OutOfMemory;
            }
            self.emitter.addRegRegShift(.x15, stack_ptr, .x15, 3) catch return CompileError.OutOfMemory;
            self.emitter.strImm(.x14, .x15, 0) catch return CompileError.OutOfMemory;

            // Update idx on stack: stack[sp-1] = JSValue.fromInt(idx + 1)
            self.emitter.addRegImm12(.x13, .x13, 1) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x15, INT_TAG) catch return CompileError.OutOfMemory;
            self.emitter.addRegReg(.x13, .x13, .x15) catch return CompileError.OutOfMemory;
            // x9 still points to stack[sp-2], so sp-1 is x9+8
            self.emitter.strImm(.x13, .x9, 8) catch return CompileError.OutOfMemory;

            // Continue loop
            try self.emitJmpToLabel(done);

            // Slow path: call helper for arrays and other iterators
            try self.markLabel(slow);
            const fn_ptr = @intFromPtr(&jitForOfNextPutLoc);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x1, local_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            self.emitter.cmpRegImm12(.x0, 0) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.eq, target);

            try self.markLabel(done);
        } else if (is_x86_64) {
            // x86-64: fall back to helper call
            const fn_ptr = @intFromPtr(&jitForOfNextPutLoc);
            self.emitter.movRegReg(.rdi, .rbx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm32(.rsi, local_idx) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rax, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.rax);
            self.emitter.testRegReg(.rax, .rax) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.e, target);
        }
    }

    const ComparisonOp = enum { lt, lte, gt, gte, eq, neq };

    /// Emit comparison with integer fast path, inline float path, and slow-path helpers.
    /// Result is pushed as JSValue.true_val or JSValue.false_val (or exception on slow path).
    fn emitComparison(self: *BaselineCompiler, op: ComparisonOp) CompileError!void {
        if (is_x86_64) {
            const try_float = self.newLocalLabel();
            const call_helper = self.newLocalLabel();
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
            try self.emitJccToLabel(.ne, try_float);

            // Unbox and compare
            self.emitter.sarRegImm(.rax, 1) catch return CompileError.OutOfMemory;
            self.emitter.sarRegImm(.rcx, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.rax, .rcx) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.rax, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rdx, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            const cond: x86.Condition = switch (op) {
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

            // Try inline float path when SMI guard fails
            try self.markLabel(try_float);
            // Check if both operands are inline floats (tag == 5)
            // r8 = left (original), r9 = right (original)
            self.emitter.movRegReg(.r10, .r8) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper);
            self.emitter.movRegReg(.r10, .r9) catch return CompileError.OutOfMemory;
            self.emitter.andRegImm32(.r10, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm32(.r10, 5) catch return CompileError.OutOfMemory;
            try self.emitJccToLabel(.ne, call_helper);

            // Both are inline floats - extract f32 bits and convert to f64
            // Extract left's f32 bits from upper 32 bits
            self.emitter.movRegReg(.rax, .r8) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
            self.emitter.movdXmmReg32(.xmm0, .rax) catch return CompileError.OutOfMemory;
            self.emitter.cvtss2sd(.xmm0, .xmm0) catch return CompileError.OutOfMemory;

            // Extract right's f32 bits from upper 32 bits
            self.emitter.movRegReg(.rax, .r9) catch return CompileError.OutOfMemory;
            self.emitter.shrRegImm(.rax, 32) catch return CompileError.OutOfMemory;
            self.emitter.movdXmmReg32(.xmm1, .rax) catch return CompileError.OutOfMemory;
            self.emitter.cvtss2sd(.xmm1, .xmm1) catch return CompileError.OutOfMemory;

            // Compare floats using ucomisd (sets ZF, PF, CF flags)
            // ucomisd sets: ZF=1, PF=1, CF=1 if unordered (NaN)
            //               ZF=1, PF=0, CF=0 if equal
            //               ZF=0, PF=0, CF=1 if less than
            //               ZF=0, PF=0, CF=0 if greater than
            self.emitter.ucomisd(.xmm0, .xmm1) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.rax, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.rdx, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            // For NaN comparisons: JS semantics say NaN < x, NaN > x, NaN <= x, NaN >= x are all false
            // NaN == x is false, NaN != x is true
            // ucomisd sets parity flag (PF) when unordered (NaN involved)
            switch (op) {
                .lt => {
                    // true if CF=1 and PF=0 (less than, not unordered)
                    // Use 'b' (below) which checks CF=1, then verify not unordered
                    const unordered = self.newLocalLabel();
                    try self.emitJccToLabel(.p, unordered); // Jump if parity (NaN)
                    self.emitter.cmovcc(.b, .rax, .rdx) catch return CompileError.OutOfMemory;
                    try self.markLabel(unordered);
                },
                .lte => {
                    // true if (CF=1 or ZF=1) and PF=0 (less or equal, not unordered)
                    const unordered = self.newLocalLabel();
                    try self.emitJccToLabel(.p, unordered);
                    self.emitter.cmovcc(.be, .rax, .rdx) catch return CompileError.OutOfMemory;
                    try self.markLabel(unordered);
                },
                .gt => {
                    // true if CF=0 and ZF=0 and PF=0 (greater than, not unordered)
                    const unordered = self.newLocalLabel();
                    try self.emitJccToLabel(.p, unordered);
                    self.emitter.cmovcc(.a, .rax, .rdx) catch return CompileError.OutOfMemory;
                    try self.markLabel(unordered);
                },
                .gte => {
                    // true if CF=0 and PF=0 (greater or equal, not unordered)
                    const unordered = self.newLocalLabel();
                    try self.emitJccToLabel(.p, unordered);
                    self.emitter.cmovcc(.ae, .rax, .rdx) catch return CompileError.OutOfMemory;
                    try self.markLabel(unordered);
                },
                .eq => {
                    // true if ZF=1 and PF=0 (equal, not unordered)
                    const unordered = self.newLocalLabel();
                    try self.emitJccToLabel(.p, unordered);
                    self.emitter.cmovcc(.e, .rax, .rdx) catch return CompileError.OutOfMemory;
                    try self.markLabel(unordered);
                },
                .neq => {
                    // true if ZF=0 or PF=1 (not equal, or unordered)
                    // For neq: if unordered (NaN), result is true
                    self.emitter.cmovcc(.p, .rax, .rdx) catch return CompileError.OutOfMemory; // true if NaN
                    self.emitter.cmovcc(.ne, .rax, .rdx) catch return CompileError.OutOfMemory; // true if not equal
                },
            }
            try self.emitPushReg(.rax);
            try self.emitJmpToLabel(done);

            // Call helper for complex cases (boxed floats, mixed types, strings)
            try self.markLabel(call_helper);
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
            try self.emitCallHelperReg(.rax);
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
            const try_float = self.newLocalLabel();
            const call_helper = self.newLocalLabel();
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
            try self.emitBcondToLabel(.ne, try_float);

            // Unbox and compare
            self.emitter.asrRegImm(.x9, .x9, 1) catch return CompileError.OutOfMemory;
            self.emitter.asrRegImm(.x10, .x10, 1) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegReg(.x9, .x10) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x14, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            const cond: arm64.Condition = switch (op) {
                .lt => .lt,
                .lte => .le,
                .gt => .gt,
                .gte => .ge,
                .eq => .eq,
                .neq => .ne,
            };
            self.emitter.csel(.x9, .x14, .x11, cond) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x9);
            try self.emitJmpToLabel(done);

            if (is_eq) {
                try self.markLabel(fast_equal);
                const imm = if (op == .eq) value_mod.JSValue.true_val else value_mod.JSValue.false_val;
                try self.emitPushImm64(@bitCast(imm));
                try self.emitJmpToLabel(done);
            }

            // Try inline float path when SMI guard fails
            try self.markLabel(try_float);
            // Check if both operands are inline floats (tag == 5)
            // x12 = left (original), x13 = right (original)
            self.emitter.andRegImm(.x11, .x12, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 5) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);
            self.emitter.andRegImm(.x11, .x13, 7) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x11, 5) catch return CompileError.OutOfMemory;
            try self.emitBcondToLabel(.ne, call_helper);

            // Both are inline floats - extract f32 bits and convert to f64
            // Extract left's f32 bits from upper 32 bits
            self.emitter.lsrRegImm(.x9, .x12, 32) catch return CompileError.OutOfMemory;
            self.emitter.fmovSingleFromGpr32(.x0, .x9) catch return CompileError.OutOfMemory;
            self.emitter.fcvtDoubleFromSingle(.x0, .x0) catch return CompileError.OutOfMemory;

            // Extract right's f32 bits from upper 32 bits
            self.emitter.lsrRegImm(.x10, .x13, 32) catch return CompileError.OutOfMemory;
            self.emitter.fmovSingleFromGpr32(.x1, .x10) catch return CompileError.OutOfMemory;
            self.emitter.fcvtDoubleFromSingle(.x1, .x1) catch return CompileError.OutOfMemory;

            // Compare floats using fcmp (sets NZCV flags)
            // For NaN: V flag is set (unordered)
            self.emitter.fcmpDouble(.x0, .x1) catch return CompileError.OutOfMemory;

            // Load true and false values
            self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x14, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;

            // For NaN comparisons: JS semantics - all comparisons with NaN are false except !=
            // fcmp sets V flag when unordered (NaN involved)
            // ARM64 conditions for ordered comparisons:
            // mi = less than (ordered)
            // ls = less than or equal (ordered) - note: need to check not-unordered
            // gt = greater than (ordered)
            // ge = greater than or equal (ordered)
            // eq = equal (ordered)
            // ne = not equal - but this includes unordered!
            switch (op) {
                .lt => {
                    // mi: N=1 and V=0 (less than, ordered)
                    self.emitter.csel(.x9, .x14, .x11, .mi) catch return CompileError.OutOfMemory;
                },
                .lte => {
                    // ls: C=0 or Z=1, but we need ordered, so use le which is Z=1 or (N=V)
                    // Actually for floats: ls means "less or same" but includes unordered
                    // Use: le condition which is Z=1 OR N!=V, but for fcmp this includes unordered
                    // Correct: check V=0 (ordered) first, then check ls
                    const unordered = self.newLocalLabel();
                    try self.emitBcondToLabel(.vs, unordered); // V=1 means unordered
                    self.emitter.csel(.x9, .x14, .x11, .ls) catch return CompileError.OutOfMemory;
                    try self.emitJmpToLabel(done);
                    try self.markLabel(unordered);
                    self.emitter.movRegReg(.x9, .x11) catch return CompileError.OutOfMemory; // false for NaN
                },
                .gt => {
                    // gt: Z=0 and N=V (greater than, ordered)
                    self.emitter.csel(.x9, .x14, .x11, .gt) catch return CompileError.OutOfMemory;
                },
                .gte => {
                    // ge: N=V (greater or equal, but includes unordered when both NaN)
                    const unordered = self.newLocalLabel();
                    try self.emitBcondToLabel(.vs, unordered);
                    self.emitter.csel(.x9, .x14, .x11, .ge) catch return CompileError.OutOfMemory;
                    try self.emitJmpToLabel(done);
                    try self.markLabel(unordered);
                    self.emitter.movRegReg(.x9, .x11) catch return CompileError.OutOfMemory;
                },
                .eq => {
                    // eq: Z=1 and V=0 (equal, ordered)
                    const unordered = self.newLocalLabel();
                    try self.emitBcondToLabel(.vs, unordered);
                    self.emitter.csel(.x9, .x14, .x11, .eq) catch return CompileError.OutOfMemory;
                    try self.emitJmpToLabel(done);
                    try self.markLabel(unordered);
                    self.emitter.movRegReg(.x9, .x11) catch return CompileError.OutOfMemory;
                },
                .neq => {
                    // ne: Z=0, but for NaN we want true
                    // If V=1 (unordered/NaN), result is true
                    // If V=0 and Z=0, result is true
                    // If V=0 and Z=1, result is false
                    self.emitter.csel(.x9, .x14, .x11, .vs) catch return CompileError.OutOfMemory; // true if NaN
                    self.emitter.csel(.x9, .x14, .x9, .ne) catch return CompileError.OutOfMemory; // true if not equal
                },
            }
            try self.emitPushReg(.x9);
            try self.emitJmpToLabel(done);

            // Call helper for complex cases
            try self.markLabel(call_helper);
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
            self.emitter.movRegImm64(.x15, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x15);
            if (op == .neq) {
                // Invert boolean result: (true -> false, false -> true)
                self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;
                self.emitter.movRegImm64(.x14, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
                self.emitter.cmpRegReg(.x0, .x11) catch return CompileError.OutOfMemory;
                self.emitter.csel(.x0, .x14, .x11, .eq) catch return CompileError.OutOfMemory;
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
            try self.emitCallHelperReg(.rax);
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
            try self.emitCallHelperReg(.x14);
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
            try self.emitCallHelperReg(.rax);
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
            try self.emitCallHelperReg(.x14);
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
            try self.emitCallHelperReg(.rax);
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
            try self.emitCallHelperReg(.x14);
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
            try self.emitCallHelperReg(.rax);
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
            try self.emitCallHelperReg(.x14);
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
            try self.emitCallHelperReg(.rax);
            // Push result (rax contains JSValue)
            try self.emitPushReg(.rax);
        } else if (is_aarch64) {
            // Pop value into x1 (second argument)
            try self.emitPopReg(.x1);
            // Move context pointer from x19 to x0 (first argument)
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            // Load function address and call it
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
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
            try self.emitCallHelperReg(.rax);

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
            try self.emitCallHelperReg(.x9);

            // Convert bool to JSValue (!bool)
            self.emitter.movRegReg(.x9, .x0) catch return CompileError.OutOfMemory; // x9 = bool
            self.emitter.movRegImm64(.x10, @bitCast(value_mod.JSValue.false_val)) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x11, @bitCast(value_mod.JSValue.true_val)) catch return CompileError.OutOfMemory;
            self.emitter.cmpRegImm12(.x9, 0) catch return CompileError.OutOfMemory;
            self.emitter.csel(.x0, .x11, .x10, .eq) catch return CompileError.OutOfMemory;
            try self.emitPushReg(.x0);
        }
    }

    fn emitJump(self: *BaselineCompiler, target: u32) CompileError!void {
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
                try self.appendPendingJump(patch_offset + 1, target, false); // Skip opcode byte
            } else if (is_aarch64) {
                self.emitter.b(0) catch return CompileError.OutOfMemory;
                try self.appendPendingJump(patch_offset, target, false);
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
            try self.emitCallHelperReg(.rax);
            self.emitter.testRegReg(.rax, .rax) catch return CompileError.OutOfMemory;

            const cond: x86.Condition = if (jump_if_true) .ne else .e;

            if (self.labels.get(target)) |native_offset| {
                const current = @as(i32, @intCast(self.emitter.buffer.items.len));
                const rel = @as(i32, @intCast(native_offset)) - current - 6;
                self.emitter.jcc(cond, rel) catch return CompileError.OutOfMemory;
            } else {
                const patch_offset: u32 = @intCast(self.emitter.buffer.items.len + 2);
                self.emitter.jcc(cond, 0) catch return CompileError.OutOfMemory;
                try self.appendPendingJump(patch_offset, target, true);
            }
        } else if (is_aarch64) {
            try self.emitPopReg(.x1);
            self.emitter.movRegReg(.x0, .x19) catch return CompileError.OutOfMemory;
            self.emitter.movRegImm64(.x9, fn_ptr) catch return CompileError.OutOfMemory;
            try self.emitCallHelperReg(.x9);
            self.emitter.cmpRegImm12(.x0, 0) catch return CompileError.OutOfMemory;

            const cond: arm64.Condition = if (jump_if_true) .ne else .eq;

            if (self.labels.get(target)) |native_offset| {
                const current = @as(i32, @intCast(self.emitter.buffer.items.len));
                const rel = @as(i32, @intCast(native_offset)) - current;
                self.emitter.bcond(cond, rel) catch return CompileError.OutOfMemory;
            } else {
                const patch_offset: u32 = @intCast(self.emitter.buffer.items.len);
                self.emitter.bcond(cond, 0) catch return CompileError.OutOfMemory;
                try self.appendPendingJump(patch_offset, target, true);
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
pub fn compileFunction(allocator: std.mem.Allocator, code_alloc: *CodeAllocator, func: *const bytecode.FunctionBytecode, hidden_class_pool: ?*object.HiddenClassPool) CompileError!CompiledCode {
    var compiler = BaselineCompiler.init(allocator, code_alloc, func, hidden_class_pool);
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: inline call stack check label patching" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const callee_code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.ret),
    };

    var callee = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &callee_code,
        .constants = &.{},
        .source_map = null,
    };

    const caller_code = [_]u8{
        @intFromEnum(Opcode.push_const),
        0,
        0,
        @intFromEnum(Opcode.push_i8),
        1,
        @intFromEnum(Opcode.call),
        1,
        @intFromEnum(Opcode.ret),
    };

    const consts = [_]value_mod.JSValue{
        value_mod.JSValue.undefined_val,
    };

    const tf = try type_feedback.TypeFeedback.init(allocator, 0, 1);
    defer tf.deinit();

    const site_map = try allocator.alloc(u16, caller_code.len);
    defer allocator.free(site_map);
    @memset(site_map, 0xFFFF);

    // Call opcode at offset 5.
    site_map[5] = 0x8000;
    for (0..type_feedback.InliningPolicy.MIN_CALL_COUNT) |_| {
        tf.call_sites[0].recordCallee(&callee);
    }

    var caller = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 4,
        .flags = .{},
        .code = &caller_code,
        .constants = &consts,
        .source_map = null,
        .type_feedback_ptr = tf,
        .feedback_site_map = site_map,
    };

    var code_alloc = CodeAllocator.init(allocator);
    defer code_alloc.deinit();

    const compiled = try compileFunction(allocator, &code_alloc, &caller, null);
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

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
        try testing.expect(compiled.code.len > 0);
    }
}

test "baseline: compile property access opcodes" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.get_field),
        0x01,
        0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_field),
        0x02,
        0x00,
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_field_keep),
        0x03,
        0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.get_field_ic),
        0x04,
        0x00,
        0x00,
        0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_field_ic),
        0x05,
        0x00,
        0x00,
        0x00,
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile get_field_call opcode" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.push_null),
        @intFromEnum(Opcode.dup),
        @intFromEnum(Opcode.get_field_call),
        0x01,
        0x00,
        0x00,
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
    try testing.expect(compiled.code.len > 0);
}

test "baseline: compile globals and object creation" {
    const testing = std.testing;

    const code = [_]u8{
        @intFromEnum(Opcode.new_object),
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.new_array),
        0x02,
        0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.get_global),
        0x01,
        0x00,
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_global),
        0x02,
        0x00,
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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
        @intFromEnum(Opcode.get_loc_add),
        0x00,

        @intFromEnum(Opcode.get_loc_get_loc_add),
        0x00,
        0x01,

        @intFromEnum(Opcode.push_false),
        @intFromEnum(Opcode.if_false_goto),
        0x00,
        0x00,

        @intFromEnum(Opcode.push_const_call),
        0x00,
        0x00,
        0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.add_mod),
        0x01,
        0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.sub_mod),
        0x01,
        0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.mul_mod),
        0x01,
        0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.mod_const),
        0x01,
        0x00,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.mod_const_i8),
        0x03,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.add_const_i8),
        0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.sub_const_i8),
        0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.mul_const_i8),
        0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.lt_const_i8),
        0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.le_const_i8),
        0x02,

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.shr_1),

        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.mul_2),

        @intFromEnum(Opcode.for_of_next),
        0x00,
        0x00,
        @intFromEnum(Opcode.for_of_next_put_loc),
        0x00,
        0x00,
        0x00,

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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

    const result = compileFunction(testing.allocator, &code_alloc, &func, null);
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);

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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

    const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
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

        const compiled = try compileFunction(testing.allocator, &code_alloc, &func, null);
        try testing.expect(compiled.code.len > 0);
    }
}
