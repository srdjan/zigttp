//! Bytecode interpreter with computed goto dispatch
//!
//! Threaded code execution with tail calls for opcode handlers.
//! Implements all opcodes from bytecode.zig.

const std = @import("std");
const compat = @import("compat.zig");
const value = @import("value.zig");
const bytecode = @import("bytecode.zig");
const context = @import("context.zig");
const heap = @import("heap.zig");
const object = @import("object.zig");
const string = @import("string.zig");
const jit = @import("jit/root.zig");
const type_feedback = @import("type_feedback.zig");
const builtins = @import("builtins/root.zig");
const perf = @import("interpreter/perf.zig");
const jit_policy = @import("interpreter/jit_policy.zig");
const ic = @import("interpreter/ic.zig");
const jit_compile = @import("interpreter/jit_compile.zig");
const arith = @import("interpreter/arith.zig");
const trace = @import("interpreter/trace.zig");
const cmp = @import("interpreter/cmp.zig");
const frame = @import("interpreter/frame.zig");

const empty_code: [0]u8 = .{};
const tier_count = perf.tier_count;

pub const PerfStats = perf.PerfStats;
pub const enable_opcode_histogram = perf.enable_opcode_histogram;
const countNonZeroHistogramEntries = perf.countNonZeroHistogramEntries;

pub const JitPolicy = jit_policy.JitPolicy;
pub const getJitPolicy = jit_policy.getJitPolicy;
pub const getJitThreshold = jit_policy.getJitThreshold;
pub const getJitFeedbackWarmup = jit_policy.getJitFeedbackWarmup;
pub const setJitPolicy = jit_policy.setJitPolicy;
pub const setJitThreshold = jit_policy.setJitThreshold;
pub const setJitFeedbackWarmup = jit_policy.setJitFeedbackWarmup;
pub const disableJitForTests = jit_policy.disableJitForTests;
pub const isTieringDeoptSuppressEnabled = jit_policy.isTieringDeoptSuppressEnabled;
pub const resetTieringDeoptSuppressCache = jit_policy.resetTieringDeoptSuppressCache;
const jitDisabled = jit_policy.jitDisabled;

pub const PICEntry = ic.PICEntry;
pub const PIC_ENTRIES = ic.PIC_ENTRIES;
pub const pic_entries_tracks_feedback = ic.pic_entries_tracks_feedback;
pub const effective_pic_cap = ic.effective_pic_cap;
pub const PolymorphicInlineCache = ic.PolymorphicInlineCache;
pub const IC_CACHE_SIZE = ic.IC_CACHE_SIZE;
pub const getPicMegaRecoveryWindow = ic.getPicMegaRecoveryWindow;
pub const isPicRecoveryDisabled = ic.isPicRecoveryDisabled;

pub threadlocal var current_interpreter: ?*Interpreter = null;

/// Interpreter state
pub const Interpreter = struct {
    pub const MAX_STATE_DEPTH = 1024;
    pub const SavedState = struct {
        pc: [*]const u8,
        code_end: [*]const u8,
        constants: []const value.JSValue,
        current_func: ?*const bytecode.FunctionBytecode,
        sp: usize,
        fp: usize,
        call_depth: usize,
        catch_depth: usize,
        exception: value.JSValue,
    };

    ctx: *context.Context,
    pc: [*]const u8, // Program counter
    code_end: [*]const u8,
    constants: []const value.JSValue, // Constant pool (direct JSValue array)
    current_func: ?*const bytecode.FunctionBytecode,
    current_closure: ?*object.ClosureData, // Current closure (if executing closure)
    open_upvalues: ?*object.Upvalue, // Linked list of open upvalues
    state_stack: [MAX_STATE_DEPTH]SavedState,
    state_depth: usize,
    /// Polymorphic inline cache for property access optimization
    /// Indexed by cache_idx from get_field_ic/put_field_ic instructions
    /// Each entry can cache up to 4 (hidden_class, slot_offset) pairs
    pic_cache: [IC_CACHE_SIZE]PolymorphicInlineCache,

    /// Distance from current pc to the opcode that initiated the call.
    /// Default 2 for .call/.call_method (opcode + argc).
    /// Set to 4 for .push_const_call (opcode + u16 + argc) and .get_field_call.
    call_opcode_offset: usize = 2,

    // JIT profiling counters (Phase 11)
    backedge_count: u32 = 0, // Back-edge counter for hot loop detection
    pic_hits: u32 = 0, // PIC cache hits (type feedback)
    pic_misses: u32 = 0, // PIC cache misses (type feedback)
    deopt_count: u32 = 0, // JIT deoptimizations observed through this interpreter
    mega_recoveries: u32 = 0, // PIC sites that recovered from megamorphic to monomorphic
    tier_promotions: [tier_count]u32 = [_]u32{0} ** tier_count,
    promotion_attempted: u32 = 0, // Phase 6: every profileFunctionEntry tier transition attempt
    promotion_succeeded: u32 = 0, // Phase 6: attempts that resulted in a tier bump
    promotion_rejected_deopt_storm: u32 = 0, // Phase 6: optimized_candidate promotions blocked by deopt storm
    opcode_histogram: [256]u32 = [_]u32{0} ** 256,
    last_op: bytecode.Opcode = .nop,

    pub fn init(ctx: *context.Context) Interpreter {
        return .{
            .ctx = ctx,
            .pc = @ptrCast(&empty_code),
            .code_end = @ptrCast(&empty_code),
            .constants = &.{},
            .current_func = null,
            .current_closure = null,
            .open_upvalues = null,
            .state_stack = undefined,
            .state_depth = 0,
            .pic_cache = [_]PolymorphicInlineCache{.{}} ** IC_CACHE_SIZE,
            .backedge_count = 0,
            .pic_hits = 0,
            .pic_misses = 0,
            .deopt_count = 0,
            .mega_recoveries = 0,
            .tier_promotions = [_]u32{0} ** tier_count,
            .promotion_attempted = 0,
            .promotion_succeeded = 0,
            .promotion_rejected_deopt_storm = 0,
            .opcode_histogram = [_]u32{0} ** 256,
            .last_op = .nop,
        };
    }

    /// Profile function entry: increment execution count, check for JIT threshold
    /// Returns true if function should be promoted to JIT candidate
    inline fn profileFunctionEntry(self: *Interpreter, func: *bytecode.FunctionBytecode) bool {
        // Use non-atomic increment for single-threaded interpreter
        func.execution_count +%= 1;
        if (jitDisabled()) return false;
        if (func.execution_count == getJitThreshold() and func.tier == .interpreted) {
            self.promotion_attempted +%= 1;
            jit_compile.setTier(self, func, .baseline_candidate);
            self.promotion_succeeded +%= 1;
            return true;
        }
        // Promote baseline to optimized_candidate after OPTIMIZED_THRESHOLD calls
        if (func.execution_count == bytecode.OPTIMIZED_THRESHOLD and func.tier == .baseline) {
            self.promotion_attempted +%= 1;
            if (isTieringDeoptSuppressEnabled() and
                type_feedback.InliningPolicy.shouldSuppressOnDeoptStorm(
                    func.deopt_count,
                    func.execution_count,
                    func.last_deopt_exec_count,
                ))
            {
                self.promotion_rejected_deopt_storm +%= 1;
                return false;
            }
            jit_compile.setTier(self, func, .optimized_candidate);
            self.promotion_succeeded +%= 1;
            return true;
        }
        return false;
    }

    /// Profile back-edge: increment counter, return true if loop is hot
    inline fn profileBackedge(self: *Interpreter) bool {
        self.backedge_count +%= 1;
        return self.backedge_count >= bytecode.LOOP_THRESHOLD;
    }

    /// Reset profiling counters (for testing or new function)
    pub fn resetProfilingCounters(self: *Interpreter) void {
        self.backedge_count = 0;
        self.pic_hits = 0;
        self.pic_misses = 0;
        self.deopt_count = 0;
        self.mega_recoveries = 0;
        self.tier_promotions = [_]u32{0} ** tier_count;
        self.promotion_attempted = 0;
        self.promotion_succeeded = 0;
        self.promotion_rejected_deopt_storm = 0;
        self.opcode_histogram = [_]u32{0} ** 256;
    }

    /// Update a PIC and observe whether it performed a megamorphic recovery,
    /// bumping PerfStats.mega_recoveries when it does.
    pub inline fn updatePic(
        self: *Interpreter,
        pic: *PolymorphicInlineCache,
        hidden_class_idx: object.HiddenClassIndex,
        slot_offset: u16,
    ) void {
        _ = pic.update(hidden_class_idx, slot_offset);
        if (pic.just_recovered) {
            pic.just_recovered = false;
            self.mega_recoveries +%= 1;
        }
    }

    pub fn snapshotPerfStats(self: *const Interpreter) PerfStats {
        return .{
            .backedge_count = self.backedge_count,
            .pic_hits = self.pic_hits,
            .pic_misses = self.pic_misses,
            .deopt_count = self.deopt_count,
            .mega_recoveries = self.mega_recoveries,
            .tier_promotions = self.tier_promotions,
            .promotion_attempted = self.promotion_attempted,
            .promotion_succeeded = self.promotion_succeeded,
            .promotion_rejected_deopt_storm = self.promotion_rejected_deopt_storm,
            .opcode_histogram_enabled = enable_opcode_histogram,
            .opcode_histogram_nonzero = if (enable_opcode_histogram)
                countNonZeroHistogramEntries(self.opcode_histogram[0..])
            else
                0,
            .opcode_histogram = self.opcode_histogram,
        };
    }

    pub fn recordDeopt(self: *Interpreter) void {
        self.deopt_count +%= 1;
    }

    /// Offset the program counter by a signed value
    /// Consolidates the verbose type-casting pattern used throughout dispatch
    inline fn offsetPc(self: *Interpreter, offset: i16) void {
        self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc))) + offset)));
    }

    /// Run bytecode function
    pub fn run(self: *Interpreter, func: *const bytecode.FunctionBytecode) InterpreterError!value.JSValue {
        // Cast away const for profiling/JIT - safe because we only modify profiling fields
        const func_mut = @constCast(func);

        // Profile function entry and potentially trigger JIT compilation
        const is_candidate = self.profileFunctionEntry(func_mut);
        if (is_candidate) {
            // Allocate type feedback for future optimization
            jit_compile.allocateTypeFeedback(self, func_mut) catch {};

            // If no feedback sites exist, compile immediately
            if (func_mut.type_feedback_ptr == null and func_mut.feedback_site_map == null) {
                jit_compile.tryCompileBaseline(self, func_mut) catch {
                    // Compilation failed - continue with interpreter
                };
            }
        }

        // Compile after feedback warmup if eligible
        if (!jitDisabled() and func_mut.tier == .baseline_candidate and func_mut.type_feedback_ptr != null) {
            const warmup_target = getJitThreshold() + getJitFeedbackWarmup();
            if (func_mut.execution_count >= warmup_target) {
                jit_compile.tryCompileBaseline(self, func_mut) catch {
                    // Compilation failed - continue with interpreter
                };
            }
        }

        // Hot loop promotion: compile after type feedback warmup for functions with hot loops
        // Functions can become baseline_candidate via execution_count OR hot loop detection
        if (!jitDisabled() and func_mut.tier == .baseline_candidate and
            func_mut.backedge_count >= bytecode.LOOP_JIT_THRESHOLD)
        {
            // Allocate type feedback if not present
            if (func_mut.type_feedback_ptr == null) {
                jit_compile.allocateTypeFeedback(self, func_mut) catch {};
            }
            // Wait for feedback warmup before compiling (need call site feedback for inlining)
            // Use a shorter warmup than normal since hot loops execute frequently
            const hot_loop_warmup: u32 = 5;
            if (func_mut.type_feedback_ptr != null and func_mut.execution_count >= hot_loop_warmup) {
                jit_compile.tryCompileBaseline(self, func_mut) catch {};
            }
        }

        // Optimized tier promotion: compile when promoted from baseline
        if (!jitDisabled() and func_mut.tier == .optimized_candidate) {
            jit_compile.tryCompileOptimized(self, func_mut) catch {
                // Compilation failed - stay at baseline
                jit_compile.setTier(self, func_mut, .baseline);
            };
        }

        // Allocate space for locals
        const local_count = func.local_count;
        try self.ctx.ensureStack(local_count);
        for (0..local_count) |_| {
            try self.ctx.push(value.JSValue.undefined_val);
        }

        // Check if function is JIT-compiled and execute via JIT
        if (!jitDisabled() and (func.tier == .baseline or func.tier == .optimized)) {
            if (func.compiled_code) |cc_opaque| {
                const cc: *jit.CompiledCode = @ptrCast(@alignCast(cc_opaque));
                const prev_func = self.current_func;
                const prev_constants = self.constants;
                const prev_code_end = self.code_end;
                const prev_pc = self.pc;
                self.current_func = func;
                self.constants = func.constants;
                self.code_end = func.code.ptr + func.code.len;
                self.pc = func.code.ptr;
                defer {
                    self.current_func = prev_func;
                    self.constants = prev_constants;
                    self.code_end = prev_code_end;
                    self.pc = prev_pc;
                }
                const prev_interp = current_interpreter;
                current_interpreter = self;
                defer current_interpreter = prev_interp;
                // Set interpreter pointer in context for IC fast path
                self.ctx.jit_interpreter = @ptrCast(self);
                defer self.ctx.jit_interpreter = null;
                const result_raw = cc.execute(self.ctx);
                return value.JSValue{ .raw = result_raw };
            }
        }

        // Fall back to interpreter
        self.pc = func.code.ptr;
        self.code_end = func.code.ptr + func.code.len;
        self.constants = func.constants;
        self.current_func = func;

        return self.dispatch() catch |err| {
            if (err == error.TypeError) trace.traceLastOp(self, "run");
            return err;
        };
    }

    /// Execute a bytecode function with given arguments and return the result.
    ///
    /// This function handles the full lifecycle of a JavaScript function call:
    /// 1. Saves current interpreter state (pc, code_end, constants)
    /// 2. Pushes a new call frame onto the context's call stack
    /// 3. Sets up local variables from arguments (undefined for missing args)
    /// 4. Executes the function's bytecode via dispatch()
    /// 5. Restores state and returns the result value
    ///
    /// Closures: If func_val is a closure, the caller should have already set up
    /// upvalue access. The function uses the context's scope chain for upvalues.
    ///
    /// Errors: Returns InterpreterError on stack overflow, type errors, or
    /// unhandled exceptions. The call frame is always cleaned up via errdefer.
    pub fn callBytecodeFunction(
        self: *Interpreter,
        func_val: value.JSValue,
        func_bc: *const bytecode.FunctionBytecode,
        this_val: value.JSValue,
        args: []const value.JSValue,
    ) InterpreterError!value.JSValue {
        trace.traceCall(self, "bc enter", @intCast(args.len), false);
        defer trace.traceCall(self, "bc exit", @intCast(args.len), false);
        // Cast away const for profiling/JIT - safe because we only modify profiling fields
        const func_bc_mut = @constCast(func_bc);

        // Profile function entry and potentially trigger JIT compilation
        const is_candidate = self.profileFunctionEntry(func_bc_mut);
        if (is_candidate) {
            // Allocate type feedback for future optimization
            jit_compile.allocateTypeFeedback(self, func_bc_mut) catch {};

            // If no feedback sites exist, compile immediately
            if (func_bc_mut.type_feedback_ptr == null and func_bc_mut.feedback_site_map == null) {
                jit_compile.tryCompileBaseline(self, func_bc_mut) catch {
                    // Compilation failed (other than UnsupportedOpcode) - continue with interpreter
                };
            }
        }

        // Compile after feedback warmup if eligible
        if (!jitDisabled() and func_bc_mut.tier == .baseline_candidate and func_bc_mut.type_feedback_ptr != null) {
            const warmup_target = getJitThreshold() + getJitFeedbackWarmup();
            if (func_bc_mut.execution_count >= warmup_target) {
                jit_compile.tryCompileBaseline(self, func_bc_mut) catch {
                    // Compilation failed (other than UnsupportedOpcode) - continue with interpreter
                };
            }
        }

        // Hot loop path: functions promoted via hot loop need type feedback warmup
        // Type feedback allocation now happens post-execution to avoid corrupting
        // register state. See allocateTypeFeedbackDeferred() calls below.

        // After warmup with type feedback, compile to baseline
        if (!jitDisabled() and func_bc_mut.tier == .baseline_candidate and func_bc_mut.execution_count < getJitThreshold()) {
            if (func_bc_mut.type_feedback_ptr) |tf| {
                const baseline_warmup: u32 = 50;
                if (func_bc_mut.execution_count >= baseline_warmup or tf.totalHits() > 100) {
                    jit_compile.tryCompileBaseline(self, func_bc_mut) catch {};
                }
            }
        }

        // Optimized tier promotion: compile when promoted from baseline by hot loop
        if (!jitDisabled() and func_bc_mut.tier == .optimized_candidate) {
            jit_compile.tryCompileOptimized(self, func_bc_mut) catch {
                // Compilation failed - stay at baseline
                jit_compile.setTier(self, func_bc_mut, .baseline);
            };
        }

        try frame.pushState(self);
        defer frame.popState(self);

        // Push call frame
        try self.ctx.pushFrame(func_val, this_val, @intFromPtr(self.pc));
        errdefer {
            frame.closeUpvaluesAbove(self, 0);
            _ = self.ctx.popFrame();
        }

        // Set up new function's locals with arguments
        const local_count = func_bc.local_count;
        try self.ctx.ensureStack(local_count);

        var local_idx: usize = 0;
        while (local_idx < local_count) : (local_idx += 1) {
            if (local_idx < args.len) {
                try self.ctx.push(args[local_idx]);
            } else {
                try self.ctx.push(value.JSValue.undefined_val);
            }
        }

        // Check if function is JIT-compiled and execute via JIT
        if (!jitDisabled() and (func_bc.tier == .baseline or func_bc.tier == .optimized)) {
            if (func_bc.compiled_code) |cc_opaque| {
                const cc: *jit.CompiledCode = @ptrCast(@alignCast(cc_opaque));
                const prev_func = self.current_func;
                const prev_constants = self.constants;
                const prev_code_end = self.code_end;
                const prev_pc = self.pc;
                self.current_func = func_bc;
                self.constants = func_bc.constants;
                self.code_end = func_bc.code.ptr + func_bc.code.len;
                self.pc = func_bc.code.ptr;
                defer {
                    self.current_func = prev_func;
                    self.constants = prev_constants;
                    self.code_end = prev_code_end;
                    self.pc = prev_pc;
                }
                const prev_interp = current_interpreter;
                current_interpreter = self;
                defer current_interpreter = prev_interp;
                // Set interpreter pointer in context for IC fast path
                self.ctx.jit_interpreter = @ptrCast(self);
                defer self.ctx.jit_interpreter = null;
                const result_raw = cc.execute(self.ctx);
                const result = value.JSValue{ .raw = result_raw };

                frame.closeUpvaluesAbove(self, 0);
                _ = self.ctx.popFrame();

                // Allocate type feedback after function completes (safe boundary)
                if (!jitDisabled() and func_bc_mut.tier == .baseline_candidate) {
                    jit_compile.allocateTypeFeedbackDeferred(self, func_bc_mut) catch {};
                }

                return result;
            }
        }

        // Fall back to interpreter
        self.pc = func_bc.code.ptr;
        self.code_end = func_bc.code.ptr + func_bc.code.len;
        self.constants = func_bc.constants;
        self.current_func = func_bc;

        const result = self.dispatch() catch |err| {
            if (err == error.TypeError) trace.traceLastOp(self, "dispatch");
            return err;
        };

        frame.closeUpvaluesAbove(self, 0);
        _ = self.ctx.popFrame();

        // Allocate type feedback after function completes (safe boundary)
        if (!jitDisabled() and func_bc_mut.tier == .baseline_candidate) {
            jit_compile.allocateTypeFeedbackDeferred(self, func_bc_mut) catch {};
        }

        return result;
    }

    /// Error set for interpreter operations
    pub const InterpreterError = error{
        StackOverflow,
        CallStackOverflow,
        TypeError,
        TooManyArguments,
        InvalidConstant,
        NotCallable,
        NativeFunctionError,
        UnimplementedOpcode,
        IntegerOverflow,
        DivisionByZero,
        OutOfMemory,
        NoRootClass,
        ArenaObjectEscape, // Arena object stored into persistent object
        NoHiddenClassPool,
        SoundModeViolation, // Non-boolean value in boolean context
        DurableSuspended,
    };

    /// Main bytecode dispatch loop - executes opcodes until halt, return, or error.
    ///
    /// This is the core interpreter loop that executes JavaScript bytecode. It uses
    /// a switch-based dispatch (Zig optimizes this to computed goto on supported platforms).
    ///
    /// Execution model:
    /// - Fetches opcode at pc, increments pc, then executes via switch
    /// - Stack-based: operands popped from stack, results pushed
    /// - Immediate values: small constants encoded inline after opcode
    /// - Constants: large values referenced by index into constants array
    ///
    /// Control flow:
    /// - ret: Returns top of stack value, exits dispatch loop
    /// - halt: Returns top of stack or undefined, exits dispatch loop
    /// - goto/goto_if_false: Absolute jump by modifying pc
    /// - call/call_method: Recursive dispatch via callBytecodeFunction
    ///
    /// Public because native callbacks may need to call back into JS.
    pub fn dispatch(self: *Interpreter) InterpreterError!value.JSValue {
        @setEvalBranchQuota(10000);
        return sw: switch (@as(bytecode.Opcode, @enumFromInt(self.pc[0]))) {
            // ========================================
            // Stack Operations
            // ========================================
            .nop => {
                self.advanceOp();
                continue :sw @enumFromInt(self.pc[0]);
            },
            .halt => {
                self.advanceOp();
                break :sw if (self.ctx.sp > 0) self.ctx.pop() else value.JSValue.undefined_val;
            },
            .push_0 => {
                self.advanceOp();
                try self.ctx.push(value.JSValue.fromInt(0));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_1 => {
                self.advanceOp();
                try self.ctx.push(value.JSValue.fromInt(1));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_2 => {
                self.advanceOp();
                try self.ctx.push(value.JSValue.fromInt(2));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_3 => {
                self.advanceOp();
                try self.ctx.push(value.JSValue.fromInt(3));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_null => {
                self.advanceOp();
                try self.ctx.push(value.JSValue.null_val);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_undefined => {
                self.advanceOp();
                try self.ctx.push(value.JSValue.undefined_val);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_true => {
                self.advanceOp();
                try self.ctx.push(value.JSValue.true_val);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_false => {
                self.advanceOp();
                try self.ctx.push(value.JSValue.false_val);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_i8 => {
                self.advanceOp();
                const val: i8 = @bitCast(self.pc[0]);
                self.pc += 1;
                try self.ctx.push(value.JSValue.fromInt(val));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_i16 => {
                self.advanceOp();
                const val = readI16(self.pc);
                self.pc += 2;
                try self.ctx.push(value.JSValue.fromInt(val));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_const => {
                self.advanceOp();
                const idx = readU16(self.pc);
                self.pc += 2;
                try self.ctx.push(try self.getConstant(idx));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .dup => {
                self.advanceOp();
                const top = self.ctx.peek();
                try self.ctx.push(top);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .drop => {
                self.advanceOp();
                _ = self.ctx.pop();
                continue :sw @enumFromInt(self.pc[0]);
            },
            .swap => {
                self.advanceOp();
                self.ctx.swap2();
                continue :sw @enumFromInt(self.pc[0]);
            },
            .rot3 => {
                self.advanceOp();
                self.ctx.rot3();
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_length => {
                self.advanceOp();
                // Optimized - modify stack in place
                const sp = self.ctx.sp;
                const obj_val = self.ctx.stack[sp - 1];
                if (obj_val.isObject()) {
                    const obj = object.JSObject.fromValue(obj_val);
                    if (obj.class_id == .array) {
                        self.ctx.stack[sp - 1] = obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH];
                    } else if (obj.class_id == .range_iterator) {
                        self.ctx.stack[sp - 1] = obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH];
                    } else {
                        // Fallback to property lookup
                        const pool = self.ctx.hidden_class_pool orelse {
                            self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                            continue :sw @enumFromInt(self.pc[0]);
                        };
                        if (obj.getProperty(pool, .length)) |len| {
                            self.ctx.stack[sp - 1] = len;
                        } else {
                            self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                        }
                    }
                } else if (obj_val.isAnyString()) {
                    self.ctx.stack[sp - 1] = cmp.getAnyStringLength(obj_val);
                } else {
                    self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .dup2 => {
                self.advanceOp();
                // Duplicate top 2 stack values: [a, b] -> [a, b, a, b]
                const b = self.ctx.peekAt(0);
                const a = self.ctx.peekAt(1);
                try self.ctx.push(a);
                try self.ctx.push(b);
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Local Variables
            // ========================================
            .get_loc => {
                self.advanceOp();
                const idx = self.pc[0];
                self.pc += 1;
                try self.ctx.push(self.ctx.getLocal(idx));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_loc => {
                self.advanceOp();
                const idx = self.pc[0];
                self.pc += 1;
                const val = self.ctx.pop();
                self.ctx.setLocal(idx, val);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_loc_0 => {
                self.advanceOp();
                try self.ctx.push(self.ctx.getLocal(0));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_loc_1 => {
                self.advanceOp();
                try self.ctx.push(self.ctx.getLocal(1));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_loc_2 => {
                self.advanceOp();
                try self.ctx.push(self.ctx.getLocal(2));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_loc_3 => {
                self.advanceOp();
                try self.ctx.push(self.ctx.getLocal(3));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_loc_0 => {
                self.advanceOp();
                self.ctx.setLocal(0, self.ctx.pop());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_loc_1 => {
                self.advanceOp();
                self.ctx.setLocal(1, self.ctx.pop());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_loc_2 => {
                self.advanceOp();
                self.ctx.setLocal(2, self.ctx.pop());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_loc_3 => {
                self.advanceOp();
                self.ctx.setLocal(3, self.ctx.pop());
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Arithmetic
            // ========================================
            .add => {
                self.advanceOp();
                // Direct stack manipulation avoids push bounds check
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                // Record type feedback for JIT optimization
                jit_compile.recordBinaryOpFeedback(self, a, b);
                // Inline integer fast path
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    const ai = a.getInt();
                    const bi = b.getInt();
                    const sum, const overflow = @addWithOverflow(ai, bi);
                    if (overflow == 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(sum);
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    // Integer overflow - convert to float
                    self.ctx.stack[sp - 2] = try self.allocFloat(@as(f64, @floatFromInt(ai)) + @as(f64, @floatFromInt(bi)));
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                } else {
                    // Slow path for strings/floats
                    @branchHint(.cold);
                    self.ctx.sp = sp - 2;
                    self.ctx.pushUnchecked(try arith.addValuesSlow(self, a, b));
                    continue :sw @enumFromInt(self.pc[0]);
                }
            },
            .sub => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                jit_compile.recordBinaryOpFeedback(self, a, b);
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    const ai = a.getInt();
                    const bi = b.getInt();
                    const diff, const overflow = @subWithOverflow(ai, bi);
                    if (overflow == 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(diff);
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    // Integer overflow - convert to float
                    self.ctx.stack[sp - 2] = try self.allocFloat(@as(f64, @floatFromInt(ai)) - @as(f64, @floatFromInt(bi)));
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                } else {
                    @branchHint(.cold);
                    self.ctx.sp = sp - 2;
                    self.ctx.pushUnchecked(try arith.subValuesSlow(self, a, b));
                    continue :sw @enumFromInt(self.pc[0]);
                }
            },
            .mul => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                jit_compile.recordBinaryOpFeedback(self, a, b);
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    const ai = a.getInt();
                    const bi = b.getInt();
                    const product, const overflow = @mulWithOverflow(ai, bi);
                    if (overflow == 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(product);
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    // Integer overflow - convert to float
                    self.ctx.stack[sp - 2] = try self.allocFloat(@as(f64, @floatFromInt(ai)) * @as(f64, @floatFromInt(bi)));
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                } else {
                    @branchHint(.cold);
                    self.ctx.sp = sp - 2;
                    self.ctx.pushUnchecked(try arith.mulValuesSlow(self, a, b));
                    continue :sw @enumFromInt(self.pc[0]);
                }
            },
            .div => {
                self.advanceOp();
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                jit_compile.recordBinaryOpFeedback(self, a, b);
                self.ctx.pushUnchecked(try arith.divValues(self, a, b));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .mod => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                jit_compile.recordBinaryOpFeedback(self, a, b);
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    const bv = b.getInt();
                    if (bv != 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(@rem(a.getInt(), bv));
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                }
                return error.DivisionByZero;
            },
            .pow => {
                self.advanceOp();
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                try self.ctx.push(try arith.powValues(self, a, b));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .neg => {
                self.advanceOp();
                const a = self.ctx.pop();
                try self.ctx.push(try arith.negValue(self, a));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .inc => {
                self.advanceOp();
                // Optimize for common integer case - modify stack in place
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];
                if (a.isInt()) {
                    @branchHint(.likely);
                    const ai = a.getInt();
                    const sum, const overflow = @addWithOverflow(ai, 1);
                    if (overflow == 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(sum);
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    // Overflow - convert to float
                    self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(ai)) + 1.0);
                    continue :sw @enumFromInt(self.pc[0]);
                } else if (a.isFloat64()) {
                    self.ctx.stack[sp - 1] = try self.allocFloat(a.getFloat64() + 1.0);
                    continue :sw @enumFromInt(self.pc[0]);
                } else {
                    return error.TypeError;
                }
            },
            .dec => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];
                if (a.isInt()) {
                    const ai = a.getInt();
                    const diff, const overflow = @subWithOverflow(ai, 1);
                    if (overflow == 0) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(diff);
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(ai)) - 1.0);
                    continue :sw @enumFromInt(self.pc[0]);
                } else if (a.isFloat64()) {
                    self.ctx.stack[sp - 1] = try self.allocFloat(a.getFloat64() - 1.0);
                    continue :sw @enumFromInt(self.pc[0]);
                } else {
                    return error.TypeError;
                }
            },
            .concat_n => {
                self.advanceOp();
                const count = self.pc[0];
                self.pc += 1;
                const result = try arith.concatNValues(self, count);
                try self.ctx.push(result);
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Math Builtins
            // ========================================
            .math_floor => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const arg = self.ctx.stack[sp - 1];
                if (arg.isInt()) {
                    // floor(int) = int
                } else if (arg.isFloat64()) {
                    const n = arg.getFloat64();
                    const floored = @floor(n);
                    if (floored >= -2147483648 and floored <= 2147483647) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(@intFromFloat(floored));
                    } else {
                        self.ctx.stack[sp - 1] = try self.allocFloat(floored);
                    }
                } else {
                    self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .math_ceil => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const arg = self.ctx.stack[sp - 1];
                if (arg.isInt()) {
                    // ceil(int) = int
                } else if (arg.isFloat64()) {
                    const n = arg.getFloat64();
                    const ceiled = @ceil(n);
                    if (ceiled >= -2147483648 and ceiled <= 2147483647) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(@intFromFloat(ceiled));
                    } else {
                        self.ctx.stack[sp - 1] = try self.allocFloat(ceiled);
                    }
                } else {
                    self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .math_round => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const arg = self.ctx.stack[sp - 1];
                if (arg.isInt()) {
                    // round(int) = int
                } else if (arg.isFloat64()) {
                    const n = arg.getFloat64();
                    const rounded = @round(n);
                    if (rounded >= -2147483648 and rounded <= 2147483647) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(@intFromFloat(rounded));
                    } else {
                        self.ctx.stack[sp - 1] = try self.allocFloat(rounded);
                    }
                } else {
                    self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .math_abs => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const arg = self.ctx.stack[sp - 1];
                if (arg.isInt()) {
                    const v = arg.getInt();
                    if (v == std.math.minInt(i32)) {
                        const result = try self.allocFloat(@as(f64, 2147483648.0));
                        self.ctx.stack[sp - 1] = result;
                    } else if (v < 0) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(-v);
                    }
                } else if (arg.isFloat64()) {
                    const n = arg.getFloat64();
                    const absed = @abs(n);
                    if (absed >= 0 and absed <= 2147483647) {
                        const truncated = @floor(absed);
                        if (truncated == absed) {
                            self.ctx.stack[sp - 1] = value.JSValue.fromInt(@intFromFloat(absed));
                        } else {
                            self.ctx.stack[sp - 1] = try self.allocFloat(absed);
                        }
                    } else {
                        self.ctx.stack[sp - 1] = try self.allocFloat(absed);
                    }
                } else {
                    self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .math_min2 => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    const av = a.getInt();
                    const bv = b.getInt();
                    self.ctx.stack[sp - 2] = value.JSValue.fromInt(@min(av, bv));
                } else {
                    const an = a.toNumber() orelse std.math.nan(f64);
                    const bn = b.toNumber() orelse std.math.nan(f64);
                    if (std.math.isNan(an) or std.math.isNan(bn)) {
                        self.ctx.stack[sp - 2] = try self.allocFloat(std.math.nan(f64));
                    } else {
                        self.ctx.stack[sp - 2] = try self.allocFloat(@min(an, bn));
                    }
                }
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .math_max2 => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    const av = a.getInt();
                    const bv = b.getInt();
                    self.ctx.stack[sp - 2] = value.JSValue.fromInt(@max(av, bv));
                } else {
                    const an = a.toNumber() orelse std.math.nan(f64);
                    const bn = b.toNumber() orelse std.math.nan(f64);
                    if (std.math.isNan(an) or std.math.isNan(bn)) {
                        self.ctx.stack[sp - 2] = try self.allocFloat(std.math.nan(f64));
                    } else {
                        self.ctx.stack[sp - 2] = try self.allocFloat(@max(an, bn));
                    }
                }
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Comparison / Logical
            // ========================================
            .lt => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() < b.getInt());
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                } else {
                    @branchHint(.cold);
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(try cmp.compareValues(a, b) == .lt);
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
            },
            .lte => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() <= b.getInt());
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                const order = try cmp.compareValues(a, b);
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(order == .lt or order == .eq);
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .gt => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() > b.getInt());
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(try cmp.compareValues(a, b) == .gt);
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .gte => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() >= b.getInt());
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                const order = try cmp.compareValues(a, b);
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(order == .gt or order == .eq);
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .eq => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(cmp.looseEquals(a, b));
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .neq => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(!cmp.looseEquals(a, b));
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .strict_eq => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.strictEquals(b));
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .strict_neq => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(!a.strictEquals(b));
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .not => {
                self.advanceOp();
                const a = self.ctx.pop();
                const cond_bool = a.toConditionBool() orelse {
                    self.ctx.exception = try self.createBoolError(a);
                    break :sw value.JSValue.undefined_val;
                };
                try self.ctx.push(value.JSValue.fromBool(!cond_bool));
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Bitwise Operations
            // ========================================
            .bit_and => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) & toInt32(b));
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .bit_or => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) | toInt32(b));
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .bit_xor => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) ^ toInt32(b));
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .bit_not => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];
                self.ctx.stack[sp - 1] = value.JSValue.fromInt(~toInt32(a));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .shl => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                const shift: u5 = @intCast(@as(u32, @bitCast(toInt32(b))) & 0x1F);
                self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) << shift);
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .shr => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                const shift: u5 = @intCast(@as(u32, @bitCast(toInt32(b))) & 0x1F);
                self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) >> shift);
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .ushr => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                const shift: u5 = @intCast(@as(u32, @bitCast(toInt32(b))) & 0x1F);
                const ua: u32 = @bitCast(toInt32(a));
                self.ctx.stack[sp - 2] = value.JSValue.fromInt(@bitCast(ua >> shift));
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Control Flow
            // ========================================
            .goto => {
                self.advanceOp();
                const offset = readI16(self.pc);
                self.pc += 2;
                if (offset < 0) {
                    if (false) {
                        if (self.current_func) |func| {
                            const func_mut = @constCast(func);
                            if (func_mut.tier == .interpreted) {
                                func_mut.tier = .baseline_candidate;
                            }
                            func_mut.backedge_count = self.backedge_count;
                        }
                        self.backedge_count = 0;
                    }
                }
                self.offsetPc(offset);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .loop => {
                self.advanceOp();
                const offset = readI16(self.pc);
                self.pc += 2;
                if (false) {
                    if (self.current_func) |func| {
                        const func_mut = @constCast(func);
                        if (func_mut.tier == .interpreted) {
                            func_mut.tier = .baseline_candidate;
                        }
                        func_mut.backedge_count = self.backedge_count;
                    }
                    self.backedge_count = 0;
                }
                self.offsetPc(-offset);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .if_true => {
                self.advanceOp();
                const cond = self.ctx.pop();
                const cond_bool = cond.toConditionBool() orelse {
                    self.ctx.exception = try self.createBoolError(cond);
                    break :sw value.JSValue.undefined_val;
                };
                const offset = readI16(self.pc);
                self.pc += 2;
                if (cond_bool) {
                    self.offsetPc(offset);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .if_false => {
                self.advanceOp();
                const cond = self.ctx.pop();
                const cond_bool = cond.toConditionBool() orelse {
                    self.ctx.exception = try self.createBoolError(cond);
                    break :sw value.JSValue.undefined_val;
                };
                const offset = readI16(self.pc);
                self.pc += 2;
                if (!cond_bool) {
                    self.offsetPc(offset);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .ret => {
                self.advanceOp();
                break :sw self.ctx.pop();
            },
            .ret_undefined => {
                self.advanceOp();
                break :sw value.JSValue.undefined_val;
            },

            // ========================================
            // Object Operations
            // ========================================
            .new_object => {
                self.advanceOp();
                const obj = try self.createObject();
                try self.ctx.push(obj.toValue());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .new_array => {
                self.advanceOp();
                const length = readU16(self.pc);
                self.pc += 2;
                const obj = try self.createArray();
                obj.setArrayLength(@intCast(length));
                try self.ctx.push(obj.toValue());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .new_object_literal => {
                self.advanceOp();
                const shape_idx = readU16(self.pc);
                self.pc += 2;
                const prop_count = self.pc[0];
                self.pc += 1;
                _ = prop_count;

                const class_idx = self.ctx.getLiteralShape(shape_idx) orelse {
                    const obj = try self.createObject();
                    try self.ctx.push(obj.toValue());
                    continue :sw @enumFromInt(self.pc[0]);
                };

                const obj = try self.ctx.createObjectWithClass(class_idx, null);
                try self.ctx.push(obj.toValue());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .set_slot => {
                self.advanceOp();
                const slot_idx: u16 = self.pc[0];
                self.pc += 1;
                const val = self.ctx.pop();
                const obj_val = self.ctx.pop();

                if (obj_val.isObject()) {
                    const obj = object.JSObject.fromValue(obj_val);
                    obj.setSlot(slot_idx, val);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_field => {
                self.advanceOp();
                const atom_idx = readU16(self.pc);
                self.pc += 2;
                const atom: object.Atom = @enumFromInt(atom_idx);
                const obj_val = self.ctx.pop();

                if (obj_val.isObject()) {
                    const obj = object.JSObject.fromValue(obj_val);
                    const pool = self.ctx.hidden_class_pool orelse {
                        try self.ctx.push(value.JSValue.undefined_val);
                        continue :sw @enumFromInt(self.pc[0]);
                    };
                    if (obj.getProperty(pool, atom)) |prop_val| {
                        try self.ctx.push(prop_val);
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                } else if (obj_val.isAnyString()) {
                    if (atom == .length) {
                        try self.ctx.push(cmp.getAnyStringLength(obj_val));
                    } else if (self.ctx.string_prototype) |proto| {
                        const pool = self.ctx.hidden_class_pool orelse {
                            try self.ctx.push(value.JSValue.undefined_val);
                            continue :sw @enumFromInt(self.pc[0]);
                        };
                        if (proto.getProperty(pool, atom)) |prop_val| {
                            try self.ctx.push(prop_val);
                        } else {
                            try self.ctx.push(value.JSValue.undefined_val);
                        }
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                } else {
                    try self.ctx.push(value.JSValue.undefined_val);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_field => {
                self.advanceOp();
                const atom_idx = readU16(self.pc);
                self.pc += 2;
                const atom: object.Atom = @enumFromInt(atom_idx);
                const val = self.ctx.pop();
                const obj_val = self.ctx.pop();

                if (obj_val.isObject()) {
                    const obj = object.JSObject.fromValue(obj_val);
                    try self.ctx.setPropertyChecked(obj, atom, val);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_field_keep => {
                self.advanceOp();
                const atom_idx = readU16(self.pc);
                self.pc += 2;
                const atom: object.Atom = @enumFromInt(atom_idx);
                const val = self.ctx.pop();
                const obj_val = self.ctx.pop();

                if (obj_val.isObject()) {
                    const obj = object.JSObject.fromValue(obj_val);
                    try self.ctx.setPropertyChecked(obj, atom, val);
                }
                try self.ctx.push(val);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_field_ic => {
                self.advanceOp();
                const atom_idx = readU16(self.pc);
                const cache_idx = readU16(self.pc + 2);
                self.pc += 4;
                const atom: object.Atom = @enumFromInt(atom_idx);
                const obj_val = self.ctx.pop();
                jit_compile.recordPropertyFeedback(self, obj_val);

                if (obj_val.isObject()) {
                    const obj = object.JSObject.fromValue(obj_val);
                    const pic = &self.pic_cache[cache_idx];

                    if (pic.lookup(obj.hidden_class_idx)) |slot_offset| {
                        self.pic_hits +%= 1;
                        try self.ctx.push(obj.getSlot(slot_offset));
                        continue :sw @enumFromInt(self.pc[0]);
                    }

                    self.pic_misses +%= 1;
                    const pool = self.ctx.hidden_class_pool orelse {
                        try self.ctx.push(value.JSValue.undefined_val);
                        continue :sw @enumFromInt(self.pc[0]);
                    };
                    if (pool.findProperty(obj.hidden_class_idx, atom)) |slot_offset| {
                        self.updatePic(pic, obj.hidden_class_idx, slot_offset);
                        try self.ctx.push(obj.getSlot(slot_offset));
                    } else if (obj.getProperty(pool, atom)) |prop_val| {
                        try self.ctx.push(prop_val);
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                } else if (obj_val.isAnyString()) {
                    if (atom == .length) {
                        try self.ctx.push(cmp.getAnyStringLength(obj_val));
                    } else if (self.ctx.string_prototype) |proto| {
                        const pool = self.ctx.hidden_class_pool orelse {
                            try self.ctx.push(value.JSValue.undefined_val);
                            continue :sw @enumFromInt(self.pc[0]);
                        };
                        if (proto.getProperty(pool, atom)) |prop_val| {
                            try self.ctx.push(prop_val);
                        } else {
                            try self.ctx.push(value.JSValue.undefined_val);
                        }
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                } else {
                    try self.ctx.push(value.JSValue.undefined_val);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_field_ic => {
                self.advanceOp();
                const atom_idx = readU16(self.pc);
                const cache_idx = readU16(self.pc + 2);
                self.pc += 4;
                const atom: object.Atom = @enumFromInt(atom_idx);
                const val = self.ctx.pop();
                const obj_val = self.ctx.pop();
                jit_compile.recordPropertyFeedback(self, obj_val);

                if (obj_val.isObject()) {
                    const obj = object.JSObject.fromValue(obj_val);
                    const pic = &self.pic_cache[cache_idx];
                    if (self.ctx.enforce_arena_escape and self.ctx.hybrid != null and !obj.flags.is_arena and self.ctx.isEphemeralValue(val)) {
                        return error.ArenaObjectEscape;
                    }

                    if (pic.lookup(obj.hidden_class_idx)) |slot_offset| {
                        self.pic_hits +%= 1;
                        obj.setSlot(slot_offset, val);
                        continue :sw @enumFromInt(self.pc[0]);
                    }

                    self.pic_misses +%= 1;
                    const pool = self.ctx.hidden_class_pool orelse {
                        try self.ctx.setPropertyChecked(obj, atom, val);
                        continue :sw @enumFromInt(self.pc[0]);
                    };
                    if (pool.findProperty(obj.hidden_class_idx, atom)) |slot_offset| {
                        self.updatePic(pic, obj.hidden_class_idx, slot_offset);
                        obj.setSlot(slot_offset, val);
                    } else {
                        try self.ctx.setPropertyChecked(obj, atom, val);
                    }
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_elem => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const index_val = self.ctx.stack[sp - 1];
                const obj_val = self.ctx.stack[sp - 2];

                if (obj_val.isObject() and index_val.isInt()) {
                    const obj = object.JSObject.fromValue(obj_val);
                    const idx = index_val.getInt();
                    if (idx >= 0) {
                        const idx_u: u32 = @intCast(idx);
                        if (obj.class_id == .array) {
                            const len = @as(u32, @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt()));
                            if (idx_u < len) {
                                self.ctx.stack[sp - 2] = obj.getIndexUnchecked(idx_u);
                            } else {
                                self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                            }
                            self.ctx.sp = sp - 1;
                            continue :sw @enumFromInt(self.pc[0]);
                        } else if (obj.class_id == .range_iterator) {
                            const len = @as(u32, @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt()));
                            if (idx_u < len) {
                                const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                                const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                                self.ctx.stack[sp - 2] = value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step);
                            } else {
                                self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                            }
                            self.ctx.sp = sp - 1;
                            continue :sw @enumFromInt(self.pc[0]);
                        } else {
                            var idx_buf: [32]u8 = undefined;
                            const idx_slice = std.fmt.bufPrint(&idx_buf, "{d}", .{idx}) catch {
                                self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                                self.ctx.sp = sp - 1;
                                continue :sw @enumFromInt(self.pc[0]);
                            };
                            const atom = self.ctx.atoms.intern(idx_slice) catch {
                                self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                                self.ctx.sp = sp - 1;
                                continue :sw @enumFromInt(self.pc[0]);
                            };
                            const pool = self.ctx.hidden_class_pool orelse {
                                self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                                self.ctx.sp = sp - 1;
                                continue :sw @enumFromInt(self.pc[0]);
                            };
                            self.ctx.stack[sp - 2] = obj.getProperty(pool, atom) orelse value.JSValue.undefined_val;
                            self.ctx.sp = sp - 1;
                            continue :sw @enumFromInt(self.pc[0]);
                        }
                    }
                }
                self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_elem => {
                self.advanceOp();
                const val = self.ctx.pop();
                const index_val = self.ctx.pop();
                const obj_val = self.ctx.pop();

                if (obj_val.isObject() and index_val.isInt()) {
                    const obj = object.JSObject.fromValue(obj_val);
                    const idx = index_val.getInt();
                    if (idx >= 0) {
                        if (obj.class_id == .array) {
                            try self.ctx.setIndexChecked(obj, @intCast(idx), val);
                        } else {
                            var idx_buf: [32]u8 = undefined;
                            const idx_slice = std.fmt.bufPrint(&idx_buf, "{d}", .{idx}) catch {
                                continue :sw @enumFromInt(self.pc[0]);
                            };
                            const atom = self.ctx.atoms.intern(idx_slice) catch {
                                continue :sw @enumFromInt(self.pc[0]);
                            };
                            try self.ctx.setPropertyChecked(obj, atom, val);
                        }
                    }
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_global => {
                self.advanceOp();
                const atom_idx = readU16(self.pc);
                self.pc += 2;
                const atom: object.Atom = @enumFromInt(atom_idx);
                if (self.ctx.getGlobal(atom)) |val| {
                    try self.ctx.push(val);
                } else {
                    try self.ctx.push(value.JSValue.undefined_val);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_global => {
                self.advanceOp();
                const atom_idx = readU16(self.pc);
                self.pc += 2;
                const atom: object.Atom = @enumFromInt(atom_idx);
                const val = self.ctx.pop();
                try self.ctx.setGlobal(atom, val);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .define_global => {
                self.advanceOp();
                const atom_idx = readU16(self.pc);
                self.pc += 2;
                const atom: object.Atom = @enumFromInt(atom_idx);
                const val = self.ctx.pop();
                try self.ctx.defineGlobal(atom, val);
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Function / Closure Creation
            // ========================================
            .make_function => {
                self.advanceOp();
                const const_idx = readU16(self.pc);
                self.pc += 2;
                const bc_val = try self.getConstant(const_idx);
                if (!bc_val.isExternPtr()) return error.TypeError;
                const bc_ptr = bc_val.toExternPtr(bytecode.FunctionBytecode);
                const root_class_idx = self.ctx.root_class_idx;
                const func_obj = try object.JSObject.createBytecodeFunction(
                    self.ctx.allocator,
                    root_class_idx,
                    bc_ptr,
                    @enumFromInt(bc_ptr.name_atom),
                );
                try self.ctx.bytecode_functions.append(self.ctx.allocator, func_obj);
                try self.ctx.push(func_obj.toValue());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .make_async => {
                self.advanceOp();
                const const_idx = readU16(self.pc);
                self.pc += 2;
                const bc_val = try self.getConstant(const_idx);
                if (!bc_val.isExternPtr()) return error.TypeError;
                const bc_ptr = bc_val.toExternPtr(bytecode.FunctionBytecode);
                const root_class_idx = self.ctx.root_class_idx;
                const func_obj = try object.JSObject.createBytecodeFunction(
                    self.ctx.allocator,
                    root_class_idx,
                    bc_ptr,
                    @enumFromInt(bc_ptr.name_atom),
                );
                func_obj.flags.is_async = true;
                try self.ctx.bytecode_functions.append(self.ctx.allocator, func_obj);
                try self.ctx.push(func_obj.toValue());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .make_closure => {
                self.advanceOp();
                const const_idx = readU16(self.pc);
                self.pc += 2;
                const upvalue_count: u8 = self.pc[0];
                self.pc += 1;

                const bc_val = try self.getConstant(const_idx);
                if (!bc_val.isExternPtr()) return error.TypeError;
                const bc_ptr = bc_val.toExternPtr(bytecode.FunctionBytecode);

                const upvalues = try self.ctx.allocator.alloc(*object.Upvalue, upvalue_count);
                errdefer self.ctx.allocator.free(upvalues);

                for (0..upvalue_count) |i| {
                    const info = bc_ptr.upvalue_info[i];
                    if (info.is_local) {
                        upvalues[i] = try frame.captureUpvalue(self, info.index);
                    } else {
                        if (self.current_closure) |closure| {
                            upvalues[i] = closure.upvalues[info.index];
                        } else {
                            const uv = try self.ctx.gc_state.acquireUpvalue();
                            uv.* = .{
                                .location = .{ .closed = value.JSValue.undefined_val },
                                .next = null,
                            };
                            upvalues[i] = uv;
                        }
                    }
                }

                const root_class_idx = self.ctx.root_class_idx;
                const closure_obj = try object.JSObject.createClosure(
                    self.ctx.allocator,
                    root_class_idx,
                    bc_ptr,
                    @enumFromInt(bc_ptr.name_atom),
                    upvalues,
                );
                try self.ctx.bytecode_functions.append(self.ctx.allocator, closure_obj);
                try self.ctx.push(closure_obj.toValue());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_upvalue => {
                self.advanceOp();
                const idx = self.pc[0];
                self.pc += 1;
                if (self.current_closure) |closure| {
                    if (idx < closure.upvalues.len) {
                        const uv = closure.upvalues[idx];
                        try self.ctx.push(uv.get());
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                } else {
                    try self.ctx.push(value.JSValue.undefined_val);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .put_upvalue => {
                self.advanceOp();
                const idx = self.pc[0];
                self.pc += 1;
                const val = self.ctx.pop();
                if (self.current_closure) |closure| {
                    if (idx < closure.upvalues.len) {
                        closure.upvalues[idx].set(val);
                    }
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .close_upvalue => {
                self.advanceOp();
                const local_idx = self.pc[0];
                self.pc += 1;
                frame.closeUpvaluesAbove(self, local_idx);
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Await / typeof / spread
            // ========================================
            .await_val => {
                self.advanceOp();
                const awaited = self.ctx.peek();
                _ = awaited;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .typeof => {
                self.advanceOp();
                const a = self.ctx.pop();
                const type_str = a.typeOf();
                const js_str = self.createString(type_str) catch {
                    try self.ctx.push(value.JSValue.undefined_val);
                    continue :sw @enumFromInt(self.pc[0]);
                };
                try self.ctx.push(value.JSValue.fromPtr(js_str));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .to_number => {
                self.advanceOp();
                const a = self.ctx.pop();
                if (a.isInt()) {
                    try self.ctx.push(a);
                } else if (a.toNumber()) |n| {
                    try self.ctx.push(value.JSValue.fromFloat(n));
                } else {
                    try self.ctx.push(value.JSValue.nan_val);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .array_spread => {
                // Stack: [target_array, current_index, source_array]
                self.advanceOp();
                const source_val = self.ctx.pop();
                const idx_val = self.ctx.pop();
                const target_val = self.ctx.peek();

                if (target_val.isObject() and source_val.isObject() and idx_val.isInt()) {
                    const target = object.JSObject.fromValue(target_val);
                    const source = object.JSObject.fromValue(source_val);
                    var idx: usize = @intCast(idx_val.getInt());
                    const pool = self.ctx.hidden_class_pool orelse {
                        try self.ctx.push(idx_val);
                        continue :sw @enumFromInt(self.pc[0]);
                    };

                    if (source.getProperty(pool, .length)) |len_val| {
                        if (len_val.isInt()) {
                            const src_len: usize = @intCast(len_val.getInt());
                            for (0..src_len) |i| {
                                const elem = source.getSlot(@intCast(i));
                                if (self.ctx.enforce_arena_escape and self.ctx.hybrid != null and !target.flags.is_arena and self.ctx.isEphemeralValue(elem)) {
                                    return error.ArenaObjectEscape;
                                }
                                target.setSlot(@intCast(idx), elem);
                                idx += 1;
                            }
                            try self.ctx.push(value.JSValue.fromInt(@intCast(idx)));
                            continue :sw @enumFromInt(self.pc[0]);
                        }
                    }
                }
                try self.ctx.push(idx_val);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .call_spread => {
                self.advanceOp();
                try self.ctx.push(value.JSValue.undefined_val);
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Function Calls
            // ========================================
            .call => {
                self.advanceOp();
                const argc: u8 = self.pc[0];
                self.pc += 1;
                try self.doCall(argc, false);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .call_method => {
                self.advanceOp();
                const argc: u8 = self.pc[0];
                self.pc += 1;

                // Native builtin fast path for hot String methods. Bypasses
                // doCall's generic prologue (trace defers, guard check, arg
                // collection loop) when the stack resolves to a known hot
                // native with a matching arity. Keeps identical semantics -
                // the builtin fns themselves handle type coercion/arg
                // defaults - so a mismatch or exception falls through to
                // the generic path by virtue of the switch's else branch.
                const sp0 = self.ctx.sp;
                if (sp0 >= @as(u32, argc) + 2) {
                    const func_val = self.ctx.stack[sp0 - argc - 1];
                    if (func_val.isObject()) {
                        const func_obj = object.JSObject.fromValue(func_val);
                        if (func_obj.getNativeFunctionData()) |native_data| {
                            const this_val = self.ctx.stack[sp0 - argc - 2];
                            const args_ptr: [*]const value.JSValue = @ptrCast(&self.ctx.stack[sp0 - argc]);
                            const fast_result: ?value.JSValue = switch (native_data.builtin_id) {
                                .string_index_of => builtins.stringIndexOf(self.ctx, this_val, args_ptr[0..argc]),
                                .string_slice => builtins.stringSlice(self.ctx, this_val, args_ptr[0..argc]),
                                else => null,
                            };
                            if (fast_result) |r| {
                                if (!self.ctx.hasException()) {
                                    self.ctx.sp = sp0 - argc - 2;
                                    self.ctx.stack[self.ctx.sp] = r;
                                    self.ctx.sp += 1;
                                    continue :sw @enumFromInt(self.pc[0]);
                                }
                                return error.NativeFunctionError;
                            }
                        }
                    }
                }

                try self.doCall(argc, true);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .tail_call => {
                self.advanceOp();
                const argc: u8 = self.pc[0];
                self.pc += 1;
                try self.doCall(argc, false);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .push_const_call => {
                self.advanceOp();
                const const_idx = readU16(self.pc);
                const argc: u8 = self.pc[2];
                self.pc += 3;
                try self.ctx.push(try self.getConstant(const_idx));
                self.call_opcode_offset = 4;
                try self.doCall(argc, false);
                self.call_opcode_offset = 2;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_field_call => {
                self.advanceOp();
                const atom_idx = readU16(self.pc);
                const argc: u8 = self.pc[2];
                self.pc += 3;
                self.call_opcode_offset = 4;
                defer self.call_opcode_offset = 2;
                const atom: object.Atom = @enumFromInt(atom_idx);

                const obj = self.ctx.pop();
                if (obj.isObject()) {
                    const js_obj = object.JSObject.fromValue(obj);
                    const pool = self.ctx.hidden_class_pool orelse {
                        try self.ctx.push(value.JSValue.undefined_val);
                        try self.doCall(argc, true);
                        continue :sw @enumFromInt(self.pc[0]);
                    };
                    if (js_obj.getProperty(pool, atom)) |method| {
                        try self.ctx.push(method);
                        try self.doCall(argc, true);
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                } else if (obj.isAnyString()) {
                    if (self.ctx.string_prototype) |proto| {
                        const pool = self.ctx.hidden_class_pool orelse {
                            try self.ctx.push(value.JSValue.undefined_val);
                            try self.doCall(argc, true);
                            continue :sw @enumFromInt(self.pc[0]);
                        };
                        if (proto.getProperty(pool, atom)) |method| {
                            try self.ctx.push(method);
                            try self.doCall(argc, true);
                            continue :sw @enumFromInt(self.pc[0]);
                        }
                    }
                }
                try self.ctx.push(value.JSValue.undefined_val);
                try self.doCall(argc, true);
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Iterator Operations
            // ========================================
            .for_of_next => {
                // Stack: [iterable, index] -> [iterable, index+1, element] or jump to end
                self.advanceOp();
                const end_offset = readI16(self.pc);
                self.pc += 2;
                const sp = self.ctx.sp;
                const idx_val = self.ctx.stack[sp - 1];
                const iter_val = self.ctx.stack[sp - 2];

                if (iter_val.isObject() and idx_val.isInt()) {
                    @branchHint(.likely);
                    const obj = object.JSObject.fromValue(iter_val);
                    const idx = idx_val.getInt();
                    if (idx >= 0) {
                        @branchHint(.likely);
                        const idx_u: u32 = @intCast(idx);
                        if (obj.class_id == .array) {
                            const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt());
                            if (idx_u < len) {
                                @branchHint(.likely);
                                if (false) {
                                    if (self.current_func) |func| {
                                        const func_mut = @constCast(func);
                                        if (func_mut.tier == .interpreted) {
                                            func_mut.tier = .baseline_candidate;
                                        }
                                        func_mut.backedge_count = self.backedge_count;
                                    }
                                    self.backedge_count = 0;
                                }
                                try self.ctx.push(obj.getIndexUnchecked(idx_u));
                                self.ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                                continue :sw @enumFromInt(self.pc[0]);
                            }
                        } else if (obj.class_id == .range_iterator) {
                            const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt());
                            if (idx_u < len) {
                                @branchHint(.likely);
                                if (false) {
                                    if (self.current_func) |func| {
                                        const func_mut = @constCast(func);
                                        if (func_mut.tier == .interpreted) {
                                            func_mut.tier = .baseline_candidate;
                                        }
                                        func_mut.backedge_count = self.backedge_count;
                                    }
                                    self.backedge_count = 0;
                                }
                                const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                                const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                                try self.ctx.push(value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step));
                                self.ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                                continue :sw @enumFromInt(self.pc[0]);
                            }
                        }
                    }
                }
                // Loop done - jump to cleanup
                self.offsetPc(end_offset);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .for_of_next_put_loc => {
                // Fused for_of_next + put_loc: stores element directly to local
                // Stack: [iterable, index] -> [iterable, index+1] (no element pushed)
                self.advanceOp();
                const local_idx = self.pc[0];
                self.pc += 1;
                const end_offset = readI16(self.pc);
                self.pc += 2;
                const sp = self.ctx.sp;
                const idx_val = self.ctx.stack[sp - 1];
                const iter_val = self.ctx.stack[sp - 2];

                if (iter_val.isObject() and idx_val.isInt()) {
                    @branchHint(.likely);
                    const obj = object.JSObject.fromValue(iter_val);
                    const idx = idx_val.getInt();
                    if (idx >= 0) {
                        @branchHint(.likely);
                        const idx_u: u32 = @intCast(idx);
                        if (obj.class_id == .array) {
                            const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt());
                            if (idx_u < len) {
                                @branchHint(.likely);
                                if (false) {
                                    if (self.current_func) |func| {
                                        const func_mut = @constCast(func);
                                        if (func_mut.tier == .interpreted) {
                                            func_mut.tier = .baseline_candidate;
                                        }
                                        func_mut.backedge_count = self.backedge_count;
                                    }
                                    self.backedge_count = 0;
                                }
                                self.ctx.setLocal(local_idx, obj.getIndexUnchecked(idx_u));
                                self.ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                                continue :sw @enumFromInt(self.pc[0]);
                            }
                        } else if (obj.class_id == .range_iterator) {
                            const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt());
                            if (idx_u < len) {
                                @branchHint(.likely);
                                if (false) {
                                    if (self.current_func) |func| {
                                        const func_mut = @constCast(func);
                                        if (func_mut.tier == .interpreted) {
                                            func_mut.tier = .baseline_candidate;
                                        }
                                        func_mut.backedge_count = self.backedge_count;
                                    }
                                    self.backedge_count = 0;
                                }
                                const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                                const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                                self.ctx.setLocal(local_idx, value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step));
                                self.ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                                continue :sw @enumFromInt(self.pc[0]);
                            }
                        }
                    }
                }
                // Loop done - jump to cleanup
                self.offsetPc(end_offset);
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Module Operations
            // ========================================
            .import_module => {
                self.advanceOp();
                const module_idx = readU16(self.pc);
                self.pc += 2;
                const module_name_val = try self.getConstant(module_idx);
                _ = module_name_val;
                const namespace = try self.ctx.createObject(null);
                try self.ctx.push(namespace.toValue());
                continue :sw @enumFromInt(self.pc[0]);
            },
            .import_name => {
                self.advanceOp();
                const name_idx = readU16(self.pc);
                self.pc += 2;
                const name_val = try self.getConstant(name_idx);
                _ = name_val;
                const namespace_val = self.ctx.pop();
                if (namespace_val.isObject()) {
                    const namespace = object.JSObject.fromValue(namespace_val);
                    if (namespace.getSlot(0).isUndefined()) {
                        try self.ctx.push(value.JSValue.undefined_val);
                    } else {
                        try self.ctx.push(namespace.getSlot(0));
                    }
                } else {
                    try self.ctx.push(value.JSValue.undefined_val);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .import_default => {
                self.advanceOp();
                const namespace_val = self.ctx.pop();
                if (namespace_val.isObject()) {
                    const namespace = object.JSObject.fromValue(namespace_val);
                    _ = namespace;
                    try self.ctx.push(value.JSValue.undefined_val);
                } else {
                    try self.ctx.push(value.JSValue.undefined_val);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .export_name => {
                self.advanceOp();
                const name_idx = readU16(self.pc);
                self.pc += 2;
                _ = name_idx;
                _ = self.ctx.pop();
                continue :sw @enumFromInt(self.pc[0]);
            },
            .export_default => {
                self.advanceOp();
                _ = self.ctx.pop();
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Superinstructions (fused hot paths)
            // ========================================
            .get_loc_add => {
                self.advanceOp();
                const idx = self.pc[0];
                self.pc += 1;
                const b = self.ctx.getLocal(idx);
                const a = self.ctx.pop();
                try self.ctx.push(try self.addValues(a, b));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .get_loc_get_loc_add => {
                self.advanceOp();
                const idx1 = self.pc[0];
                const idx2 = self.pc[1];
                self.pc += 2;
                const a = self.ctx.getLocal(idx1);
                const b = self.ctx.getLocal(idx2);
                try self.ctx.push(try self.addValues(a, b));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .if_false_goto => {
                self.advanceOp();
                const cond = self.ctx.pop();
                const cond_bool = cond.toConditionBool() orelse {
                    self.ctx.exception = try self.createBoolError(cond);
                    return error.TypeError;
                };
                const offset = readI16(self.pc);
                self.pc += 2;
                if (!cond_bool) {
                    self.offsetPc(offset);
                }
                continue :sw @enumFromInt(self.pc[0]);
            },
            .drop_goto => {
                self.advanceOp();
                _ = self.ctx.pop();
                const offset = readI16(self.pc);
                self.pc += 2;
                self.offsetPc(offset);
                continue :sw @enumFromInt(self.pc[0]);
            },

            // Fused arithmetic-modulo
            .add_mod => {
                self.advanceOp();
                const divisor_idx = readU16(self.pc);
                self.pc += 2;
                const divisor_val = try self.getConstant(divisor_idx);
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];

                if (a.isInt() and b.isInt() and divisor_val.isInt()) {
                    const ai: i64 = a.getInt();
                    const bi: i64 = b.getInt();
                    const div: i64 = divisor_val.getInt();
                    if (div != 0) {
                        const sum = ai + bi;
                        const result: i32 = @intCast(@mod(sum, div));
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(result);
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                }
                // Fallback to normal path
                self.ctx.sp = sp - 2;
                const add_result = try self.addValues(a, b);
                self.ctx.pushUnchecked(try modValues(add_result, divisor_val));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .sub_mod => {
                self.advanceOp();
                const divisor_idx = readU16(self.pc);
                self.pc += 2;
                const divisor_val = try self.getConstant(divisor_idx);
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];

                if (a.isInt() and b.isInt() and divisor_val.isInt()) {
                    const ai: i64 = a.getInt();
                    const bi: i64 = b.getInt();
                    const div: i64 = divisor_val.getInt();
                    if (div != 0) {
                        const diff = ai - bi;
                        const result: i32 = @intCast(@mod(diff, div));
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(result);
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                }
                self.ctx.sp = sp - 2;
                const sub_result = try self.subValues(a, b);
                self.ctx.pushUnchecked(try modValues(sub_result, divisor_val));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .mul_mod => {
                self.advanceOp();
                const divisor_idx = readU16(self.pc);
                self.pc += 2;
                const divisor_val = try self.getConstant(divisor_idx);
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];

                if (a.isInt() and b.isInt() and divisor_val.isInt()) {
                    const ai: i64 = a.getInt();
                    const bi: i64 = b.getInt();
                    const div: i64 = divisor_val.getInt();
                    if (div != 0) {
                        const product = ai * bi;
                        const result: i32 = @intCast(@mod(product, div));
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(result);
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                }
                self.ctx.sp = sp - 2;
                const mul_result = try self.mulValues(a, b);
                self.ctx.pushUnchecked(try modValues(mul_result, divisor_val));
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Specialized Constant Opcodes
            // ========================================
            .shr_1 => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];
                if (a.isInt()) {
                    self.ctx.stack[sp - 1] = value.JSValue.fromInt(a.getInt() >> 1);
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.stack[sp - 1] = value.JSValue.fromInt(toInt32(a) >> 1);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .mul_2 => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];
                if (a.isInt()) {
                    const ai = a.getInt();
                    const shifted, const overflow = @shlWithOverflow(ai, 1);
                    if (overflow == 0) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(shifted);
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(ai)) * 2.0);
                    continue :sw @enumFromInt(self.pc[0]);
                }
                if (a.isFloat64()) {
                    self.ctx.stack[sp - 1] = try self.allocFloat(a.getFloat64() * 2.0);
                    continue :sw @enumFromInt(self.pc[0]);
                }
                return error.TypeError;
            },
            .mod_const => {
                self.advanceOp();
                const divisor_idx = readU16(self.pc);
                self.pc += 2;
                const divisor_val = try self.getConstant(divisor_idx);
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];

                if (a.isInt() and divisor_val.isInt()) {
                    @branchHint(.likely);
                    const div = divisor_val.getInt();
                    if (div != 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(@rem(a.getInt(), div));
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                }
                self.ctx.stack[sp - 1] = try modValues(a, divisor_val);
                continue :sw @enumFromInt(self.pc[0]);
            },
            .mod_const_i8 => {
                self.advanceOp();
                const divisor: i8 = @bitCast(self.pc[0]);
                self.pc += 1;
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];

                if (a.isInt() and divisor != 0) {
                    @branchHint(.likely);
                    self.ctx.stack[sp - 1] = value.JSValue.fromInt(@rem(a.getInt(), divisor));
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.stack[sp - 1] = try modValues(a, value.JSValue.fromInt(divisor));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .add_const_i8 => {
                self.advanceOp();
                const constant: i8 = @bitCast(self.pc[0]);
                self.pc += 1;
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];

                if (a.isInt()) {
                    @branchHint(.likely);
                    const sum, const overflow = @addWithOverflow(a.getInt(), constant);
                    if (overflow == 0) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(sum);
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) + @as(f64, @floatFromInt(constant)));
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.stack[sp - 1] = try self.addValues(a, value.JSValue.fromInt(constant));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .sub_const_i8 => {
                self.advanceOp();
                const constant: i8 = @bitCast(self.pc[0]);
                self.pc += 1;
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];

                if (a.isInt()) {
                    @branchHint(.likely);
                    const diff, const overflow = @subWithOverflow(a.getInt(), constant);
                    if (overflow == 0) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(diff);
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) - @as(f64, @floatFromInt(constant)));
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.stack[sp - 1] = try self.subValues(a, value.JSValue.fromInt(constant));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .mul_const_i8 => {
                self.advanceOp();
                const constant: i8 = @bitCast(self.pc[0]);
                self.pc += 1;
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];

                if (a.isInt()) {
                    @branchHint(.likely);
                    const ai: i64 = a.getInt();
                    const result = ai * constant;
                    if (result >= std.math.minInt(i32) and result <= std.math.maxInt(i32)) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(@intCast(result));
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(result)));
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.stack[sp - 1] = try self.mulValues(a, value.JSValue.fromInt(constant));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .lt_const_i8 => {
                self.advanceOp();
                const constant: i8 = @bitCast(self.pc[0]);
                self.pc += 1;
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];

                if (a.isInt()) {
                    @branchHint(.likely);
                    self.ctx.stack[sp - 1] = if (a.getInt() < constant) value.JSValue.true_val else value.JSValue.false_val;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                const num = a.toNumber() orelse {
                    self.ctx.stack[sp - 1] = value.JSValue.false_val;
                    continue :sw @enumFromInt(self.pc[0]);
                };
                self.ctx.stack[sp - 1] = if (num < @as(f64, @floatFromInt(constant))) value.JSValue.true_val else value.JSValue.false_val;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .le_const_i8 => {
                self.advanceOp();
                const constant: i8 = @bitCast(self.pc[0]);
                self.pc += 1;
                const sp = self.ctx.sp;
                const a = self.ctx.stack[sp - 1];

                if (a.isInt()) {
                    @branchHint(.likely);
                    self.ctx.stack[sp - 1] = if (a.getInt() <= constant) value.JSValue.true_val else value.JSValue.false_val;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                const num = a.toNumber() orelse {
                    self.ctx.stack[sp - 1] = value.JSValue.false_val;
                    continue :sw @enumFromInt(self.pc[0]);
                };
                self.ctx.stack[sp - 1] = if (num <= @as(f64, @floatFromInt(constant))) value.JSValue.true_val else value.JSValue.false_val;
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Inline Cache (call_ic)
            // ========================================
            .call_ic => {
                self.advanceOp();
                const argc: u8 = self.pc[0];
                // cache_idx (u16) is reserved for a future direct-index fast path;
                // feedback currently flows through feedback_site_map keyed on bc offset.
                self.pc += 3;
                self.call_opcode_offset = 4;
                try self.doCall(argc, false);
                self.call_opcode_offset = 2;
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Type-specialized arithmetic
            // ========================================
            .add_num => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    const ai = a.getInt();
                    const bi = b.getInt();
                    const sum, const overflow = @addWithOverflow(ai, bi);
                    if (overflow == 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(sum);
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    self.ctx.stack[sp - 2] = try self.allocFloat(@as(f64, @floatFromInt(ai)) + @as(f64, @floatFromInt(bi)));
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                // Numeric-only slow path (no string dispatch)
                self.ctx.sp = sp - 2;
                self.ctx.pushUnchecked(try arith.addNumericOnly(self, a, b));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .sub_num => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    const ai = a.getInt();
                    const bi = b.getInt();
                    const diff, const overflow = @subWithOverflow(ai, bi);
                    if (overflow == 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(diff);
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    self.ctx.stack[sp - 2] = try self.allocFloat(@as(f64, @floatFromInt(ai)) - @as(f64, @floatFromInt(bi)));
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.sp = sp - 2;
                self.ctx.pushUnchecked(try arith.subValuesSlow(self, a, b));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .mul_num => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    const ai = a.getInt();
                    const bi = b.getInt();
                    const product, const overflow = @mulWithOverflow(ai, bi);
                    if (overflow == 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 2] = value.JSValue.fromInt(product);
                        self.ctx.sp = sp - 1;
                        continue :sw @enumFromInt(self.pc[0]);
                    }
                    self.ctx.stack[sp - 2] = try self.allocFloat(@as(f64, @floatFromInt(ai)) * @as(f64, @floatFromInt(bi)));
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.sp = sp - 2;
                self.ctx.pushUnchecked(try arith.mulValuesSlow(self, a, b));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .div_num => {
                self.advanceOp();
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                self.ctx.pushUnchecked(try arith.divValues(self, a, b));
                continue :sw @enumFromInt(self.pc[0]);
            },
            .lt_num => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() < b.getInt());
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(try cmp.compareValues(a, b) == .lt);
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .gt_num => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() > b.getInt());
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(try cmp.compareValues(a, b) == .gt);
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .lte_num => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() <= b.getInt());
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                const order = try cmp.compareValues(a, b);
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(order == .lt or order == .eq);
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .gte_num => {
                self.advanceOp();
                const sp = self.ctx.sp;
                const b = self.ctx.stack[sp - 1];
                const a = self.ctx.stack[sp - 2];
                if (a.isInt() and b.isInt()) {
                    @branchHint(.likely);
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() >= b.getInt());
                    self.ctx.sp = sp - 1;
                    continue :sw @enumFromInt(self.pc[0]);
                }
                const order = try cmp.compareValues(a, b);
                self.ctx.stack[sp - 2] = value.JSValue.fromBool(order == .gt or order == .eq);
                self.ctx.sp = sp - 1;
                continue :sw @enumFromInt(self.pc[0]);
            },
            .concat_2 => {
                self.advanceOp();
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                self.ctx.pushUnchecked(try arith.concatToString(self, a, b));
                continue :sw @enumFromInt(self.pc[0]);
            },

            // ========================================
            // Unimplemented / Reserved
            // ========================================
            _ => {
                std.log.warn("Unimplemented opcode: {}", .{@as(bytecode.Opcode, @enumFromInt(self.pc[0]))});
                return error.UnimplementedOpcode;
            },
        };
    }

    /// Advance program counter past opcode byte and track last opcode for diagnostics.
    inline fn advanceOp(self: *Interpreter) void {
        self.last_op = @enumFromInt(self.pc[0]);
        if (comptime enable_opcode_histogram) {
            self.opcode_histogram[@intFromEnum(self.last_op)] +%= 1;
        }
        self.pc += 1;
    }

    // ========================================================================
    // Value Operations (with heap allocation for floats)
    // ========================================================================

    /// Add two values - optimized for the common integer case
    inline fn addValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Integer fast path FIRST - most common case in arithmetic benchmarks
        // Checking integers before strings saves a branch in the hot path
        if (a.isInt() and b.isInt()) {
            const sum, const overflow = @addWithOverflow(a.getInt(), b.getInt());
            if (overflow == 0) {
                return value.JSValue.fromInt(sum);
            }
            // Overflow - convert to float
            return try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) + @as(f64, @floatFromInt(b.getInt())));
        }
        // Slow path: strings or floats
        return arith.addValuesSlow(self, a, b);
    }

    /// Slow path for addition - handles strings and floats
    /// Subtract two values - optimized for integer fast path
    inline fn subValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Integer fast path
        if (a.isInt() and b.isInt()) {
            const diff, const overflow = @subWithOverflow(a.getInt(), b.getInt());
            if (overflow == 0) {
                return value.JSValue.fromInt(diff);
            }
            // Overflow - convert to float
            return try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) - @as(f64, @floatFromInt(b.getInt())));
        }
        // Slow path
        return arith.subValuesSlow(self, a, b);
    }

    /// Multiply two values - optimized for integer fast path
    inline fn mulValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Integer fast path
        if (a.isInt() and b.isInt()) {
            const product, const overflow = @mulWithOverflow(a.getInt(), b.getInt());
            if (overflow == 0) {
                return value.JSValue.fromInt(product);
            }
            // Overflow - convert to float
            return try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) * @as(f64, @floatFromInt(b.getInt())));
        }
        // Slow path
        return arith.mulValuesSlow(self, a, b);
    }

    pub inline fn allocFloat(self: *Interpreter, v: f64) !value.JSValue {
        // NaN-boxing: ALL f64 values are stored inline - no heap allocation!
        // This eliminates the 41.6x performance gap in mathOps benchmark.
        _ = self;
        return value.JSValue.fromFloat(v);
    }

    fn createObject(self: *Interpreter) !*object.JSObject {
        const root_class_idx = self.ctx.root_class_idx;
        // Use arena for ephemeral object allocation if hybrid mode enabled
        if (self.ctx.hybrid) |h| {
            return object.JSObject.createWithArena(h.arena, root_class_idx, null, self.ctx.hidden_class_pool) orelse
                return error.OutOfMemory;
        }
        return try object.JSObject.create(self.ctx.allocator, root_class_idx, null, self.ctx.hidden_class_pool);
    }

    fn createArray(self: *Interpreter) !*object.JSObject {
        const root_class_idx = self.ctx.root_class_idx;
        // Use arena for ephemeral array allocation if hybrid mode enabled
        if (self.ctx.hybrid) |h| {
            const obj = object.JSObject.createArrayWithArena(h.arena, root_class_idx) orelse
                return error.OutOfMemory;
            obj.prototype = self.ctx.array_prototype;
            return obj;
        }
        const obj = try object.JSObject.createArray(self.ctx.allocator, root_class_idx);
        obj.prototype = self.ctx.array_prototype;
        return obj;
    }

    /// Create a string using hybrid allocator if available
    /// Ephemeral strings use arena allocation, persistent strings use standard allocator
    pub inline fn createString(self: *Interpreter, s: []const u8) !*string.JSString {
        if (self.ctx.hybrid) |h| {
            return string.createStringWithArena(h.arena, s) orelse
                return error.OutOfMemory;
        }
        return try string.createString(self.ctx.allocator, s);
    }

    fn createBoolError(self: *Interpreter, val: value.JSValue) !value.JSValue {
        const type_name = val.typeOf();
        const prefix = "condition rejected: ";
        const suffix = " has no falsy state";
        // Build error message
        var buf: [80]u8 = undefined;
        const total = @min(prefix.len + type_name.len + suffix.len, buf.len);
        @memcpy(buf[0..prefix.len], prefix);
        const type_copy_len = @min(type_name.len, total - prefix.len);
        @memcpy(buf[prefix.len..][0..type_copy_len], type_name[0..type_copy_len]);
        const suffix_len = @min(suffix.len, total - prefix.len - type_copy_len);
        @memcpy(buf[prefix.len + type_copy_len ..][0..suffix_len], suffix[0..suffix_len]);
        const msg = buf[0 .. prefix.len + type_copy_len + suffix_len];
        const js_str = blk: {
            if (self.ctx.hybrid) |h| {
                break :blk string.createStringWithArena(h.arena, msg) orelse return error.OutOfMemory;
            }
            break :blk try string.createString(self.ctx.allocator, msg);
        };
        return value.JSValue.fromPtr(js_str);
    }

    fn getConstant(self: *Interpreter, idx: u16) !value.JSValue {
        if (idx >= self.constants.len) return error.InvalidConstant;
        return self.constants[idx];
    }

    /// Maximum arguments for stack-based allocation (security limit)
    pub const MAX_STACK_ARGS = 256;

    /// Perform function call, dispatching to native functions, bytecode functions,
    /// generators, or async functions as appropriate.
    ///
    /// Stack layout on entry:
    ///   Regular call: [func, arg0, arg1, ..., argN-1] (argc values after func)
    ///   Method call:  [obj, func, arg0, arg1, ..., argN-1] (obj is 'this')
    ///
    /// Call dispatch order:
    /// 1. Pop arguments from stack into local array
    /// 2. Pop function value
    /// 3. For method calls, pop 'this' value
    /// 4. Check if value is callable (has function data)
    /// 5. Dispatch based on function type:
    ///    - Native: Call C/Zig function directly, push result
    ///    - Generator: Create generator object, initialize locals, push iterator
    ///    - Async: Execute synchronously, wrap result in Promise-like object
    ///    - Bytecode: Call via callBytecodeFunction, push result
    ///
    /// Security: Limits argc to MAX_STACK_ARGS (256) to prevent stack buffer overflow.
    pub fn doCall(self: *Interpreter, argc: u8, is_method: bool) InterpreterError!void {
        // Stack layout for regular call: [func, arg0, arg1, ..., argN-1]
        // Stack layout for method call: [obj, func, arg0, arg1, ..., argN-1]

        // CRITICAL: Bounds check to prevent buffer overflow (security fix)
        if (argc > MAX_STACK_ARGS) {
            return error.TooManyArguments;
        }
        trace.traceCall(self, "enter", argc, is_method);
        defer trace.traceCall(self, "exit", argc, is_method);
        const guard = trace.callGuardDepth();
        if (guard != 0 and self.ctx.call_depth >= guard) {
            trace.traceCall(self, "guard", argc, is_method);
            return error.CallStackOverflow;
        }

        // Collect arguments (in reverse order from stack)
        var args: [MAX_STACK_ARGS]value.JSValue = undefined;
        var i: usize = argc;
        while (i > 0) {
            i -= 1;
            args[i] = self.ctx.pop();
        }

        // Pop function
        const func_val = self.ctx.pop();

        // Pop 'this' for method calls
        const this_val = if (is_method) self.ctx.pop() else value.JSValue.undefined_val;

        // Check if callable
        if (!func_val.isCallable()) {
            if (trace.callTraceEnabled()) {
                std.debug.print(
                    "[call] not-callable type={s} func={} this={} depth={} sp={} fp={}\n",
                    .{ func_val.typeOf(), func_val, this_val, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
                );
                if (func_val.isObject()) {
                    const obj = object.JSObject.fromValue(func_val);
                    std.debug.print(
                        "[call] not-callable object class={} callable={} generator={} async={}\n",
                        .{
                            @intFromEnum(obj.class_id),
                            @intFromBool(obj.flags.is_callable),
                            @intFromBool(obj.flags.is_generator),
                            @intFromBool(obj.flags.is_async),
                        },
                    );
                }
                if (self.current_func) |cur| {
                    const pc_off = @as(usize, @intCast(@intFromPtr(self.pc) - @intFromPtr(cur.code.ptr)));
                    std.debug.print("[call] not-callable pc_off={} func_locals={}\n", .{ pc_off, cur.local_count });
                }
            }
            return error.NotCallable;
        }

        // Get the function object
        const func_obj = object.JSObject.fromValue(func_val);

        // Check for native function
        if (func_obj.getNativeFunctionData()) |native_data| {
            // Fast dispatch for hot builtins - bypass wrapper overhead
            const result: value.JSValue = switch (native_data.builtin_id) {
                .json_parse => builtins.jsonParse(self.ctx, this_val, args[0..argc]),
                .json_stringify => builtins.jsonStringify(self.ctx, this_val, args[0..argc]),
                .string_index_of => builtins.stringIndexOf(self.ctx, this_val, args[0..argc]),
                .string_slice => builtins.stringSlice(self.ctx, this_val, args[0..argc]),
                // Math methods - direct calls without error handling wrapper
                .math_floor => builtins.mathFloor(self.ctx, this_val, args[0..argc]),
                .math_ceil => builtins.mathCeil(self.ctx, this_val, args[0..argc]),
                .math_round => builtins.mathRound(self.ctx, this_val, args[0..argc]),
                .math_abs => builtins.mathAbs(self.ctx, this_val, args[0..argc]),
                .math_min => builtins.mathMin(self.ctx, this_val, args[0..argc]),
                .math_max => builtins.mathMax(self.ctx, this_val, args[0..argc]),
                // Number parsing
                .parse_int => builtins.numberParseInt(self.ctx, this_val, args[0..argc]),
                .parse_float => builtins.numberParseFloat(self.ctx, this_val, args[0..argc]),
                .none => blk: {
                    // Generic path for non-hot builtins
                    break :blk native_data.func(self.ctx, this_val, args[0..argc]) catch |err| {
                        if (err == error.DurableSuspended) return error.DurableSuspended;
                        const func_name = if (native_data.name.toPredefinedName()) |name| name else "<native>";
                        std.log.err("Native function '{s}' error: {}", .{ func_name, err });
                        self.ctx.throwException(value.JSValue.exception_val);
                        return error.NativeFunctionError;
                    };
                },
            };
            // Check for exception from direct builtin calls
            if (self.ctx.hasException()) {
                return error.NativeFunctionError;
            }
            try self.ctx.push(result);
            return;
        }

        // Closure or bytecode function
        const closure_data = func_obj.getClosureData();
        const func_bc_opt = if (closure_data) |cd|
            cd.bytecode
        else if (func_obj.getBytecodeFunctionData()) |bc_data|
            bc_data.bytecode
        else
            null;

        // Record call site feedback for inlining decisions
        jit_compile.recordCallSiteFeedback(self, func_bc_opt);

        if (func_bc_opt) |func_bc| {
            // Set current closure context for this call (null for non-closures)
            const prev_closure = self.current_closure;
            self.current_closure = closure_data;
            defer self.current_closure = prev_closure;

            // Check if this is a generator function (using cached flag)
            if (func_obj.flags.is_generator) {
                // Create a generator object instead of executing
                const root_class_idx = self.ctx.root_class_idx;
                const gen_obj = if (self.ctx.hybrid) |h|
                    (object.JSObject.createGeneratorWithArena(
                        h.arena,
                        root_class_idx,
                        func_bc,
                        self.ctx.generator_prototype,
                    ) orelse return error.OutOfMemory)
                else
                    (object.JSObject.createGenerator(
                        self.ctx.allocator,
                        root_class_idx,
                        func_bc,
                        self.ctx.generator_prototype,
                    ) catch return error.OutOfMemory);

                // Initialize generator locals with arguments
                const gen_data = gen_obj.getGeneratorData().?;
                var local_idx: usize = 0;
                while (local_idx < gen_data.locals.len) : (local_idx += 1) {
                    if (local_idx < argc) {
                        gen_data.locals[local_idx] = args[local_idx];
                    }
                }

                try self.ctx.push(gen_obj.toValue());
                return;
            }

            // Check if this is an async function (using cached flag)
            if (func_obj.flags.is_async) {
                // Execute async function synchronously and wrap result in Promise-like object
                // For full async support, we'd need an event loop and proper Promise integration
                // This simplified version executes synchronously and returns a resolved Promise
                const result = try self.callBytecodeFunction(func_val, func_bc, this_val, args[0..argc]);

                // Create a resolved Promise-like object {then: fn, value: result}
                const promise_obj = self.ctx.createObject(null) catch {
                    return error.OutOfMemory;
                };

                // Store the resolved value
                const value_atom = self.ctx.atoms.intern("value") catch return error.OutOfMemory;
                self.ctx.setPropertyChecked(promise_obj, value_atom, result) catch |err| {
                    return switch (err) {
                        error.ArenaObjectEscape => error.ArenaObjectEscape,
                        else => error.OutOfMemory,
                    };
                };

                // Add a 'then' method (simplified - just calls callback with value)
                // For a full implementation, we'd create a proper Promise with thenable support
                const then_atom = self.ctx.atoms.intern("then") catch return error.OutOfMemory;
                self.ctx.setPropertyChecked(promise_obj, then_atom, value.JSValue.true_val) catch |err| {
                    return switch (err) {
                        error.ArenaObjectEscape => error.ArenaObjectEscape,
                        else => error.OutOfMemory,
                    };
                };

                try self.ctx.push(promise_obj.toValue());
                return;
            }

            const result = try self.callBytecodeFunction(func_val, func_bc, this_val, args[0..argc]);
            try self.ctx.push(result);
            return;
        }

        // Unknown function type
        try self.ctx.push(value.JSValue.undefined_val);
    }
};

// ============================================================================
// Helper Functions (standalone, used by interpreter)
// ============================================================================

/// Modulo two values
/// Modulo operation - optimized for integer fast path, with float fallback
inline fn modValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const bv = b.getInt();
        if (bv == 0) return error.DivisionByZero;
        return value.JSValue.fromInt(@rem(a.getInt(), bv));
    }
    // Float fallback: JS % works on all numeric types
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    if (bn == 0.0) return value.JSValue.nan_val;
    return value.JSValue.fromFloat(@rem(an, bn));
}

/// Compare two values
/// Convert value to Int32 (for bitwise operations, per ECMAScript ToInt32)
inline fn toInt32(v: value.JSValue) i32 {
    if (v.isInt()) return v.getInt();
    if (v.isFloat64()) {
        const f = v.getFloat64();
        if (std.math.isNan(f) or std.math.isInf(f) or f == 0) return 0;
        // ToInt32 truncation
        const int_val: i64 = @intFromFloat(@trunc(f));
        return @truncate(int_val);
    }
    return 0;
}

/// Read i16 from bytecode
fn readI16(pc: [*]const u8) i16 {
    return @bitCast(@as(u16, pc[0]) | (@as(u16, pc[1]) << 8));
}

/// Read u16 from bytecode
fn readU16(pc: [*]const u8) u16 {
    return @as(u16, pc[0]) | (@as(u16, pc[1]) << 8);
}

test "Interpreter basic arithmetic" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Test code: push 3, push 4, add, ret
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_3),
        @intFromEnum(bytecode.Opcode.push_i8),
        4,
        @intFromEnum(bytecode.Opcode.add),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 7), result.getInt());
}

test "Interpreter local variables" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: local0 = 10; local1 = 20; return local0 + local1
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),   10,
        @intFromEnum(bytecode.Opcode.put_loc_0), @intFromEnum(bytecode.Opcode.push_i8),
        20,                                      @intFromEnum(bytecode.Opcode.put_loc_1),
        @intFromEnum(bytecode.Opcode.get_loc_0), @intFromEnum(bytecode.Opcode.get_loc_1),
        @intFromEnum(bytecode.Opcode.add),       @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 2,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 30), result.getInt());
}

test "Interpreter bitwise operations" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 0xFF & 0x0F = 0x0F = 15
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 0xFF,
        @intFromEnum(bytecode.Opcode.push_i8), 0x0F,
        @intFromEnum(bytecode.Opcode.bit_and), @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 0x0F), result.getInt());
}

test "Interpreter shift operations" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 1 << 4 = 16
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_1),
        @intFromEnum(bytecode.Opcode.push_i8),
        4,
        @intFromEnum(bytecode.Opcode.shl),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 16), result.getInt());
}

test "Interpreter comparison" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 5 < 10 = true
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 5,
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.lt),      @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isTrue());
}

test "Interpreter conditional jump" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: if (true) { return 42 } else { return 0 }
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_true),
        @intFromEnum(bytecode.Opcode.if_false), 3,                                 0, // jump +3 if false
        @intFromEnum(bytecode.Opcode.push_i8),  42,                                @intFromEnum(bytecode.Opcode.ret),
        @intFromEnum(bytecode.Opcode.push_0),   @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 42), result.getInt());
}

test "Interpreter superinstruction get_loc_get_loc_add" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: local0 = 7; local1 = 8; return local0 + local1 (using superinstruction)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),             7,
        @intFromEnum(bytecode.Opcode.put_loc_0),           @intFromEnum(bytecode.Opcode.push_i8),
        8,                                                 @intFromEnum(bytecode.Opcode.put_loc_1),
        @intFromEnum(bytecode.Opcode.get_loc_get_loc_add), 0,
        1,                                                 @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 2,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 15), result.getInt());
}

test "Interpreter division produces float" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 7 / 2 = 3.5
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 7,
        @intFromEnum(bytecode.Opcode.push_2),  @intFromEnum(bytecode.Opcode.div),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isFloat64());
    try std.testing.expectEqual(@as(f64, 3.5), result.getFloat64());
}

test "Interpreter modulo" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 17 % 5 = 2
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 17,
        @intFromEnum(bytecode.Opcode.push_i8), 5,
        @intFromEnum(bytecode.Opcode.mod),     @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 2), result.getInt());
}

test "End-to-end: parse and execute JS" {
    // Use arena to avoid memory leak detection issues with function bytecode
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test: simple expression parsing and execution
    // Note: Expression statements drop their values, so we just verify execution completes
    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    var p = parser_mod.Parser.init(allocator, "function f() { return 1 + 2; } f()", &strings, null);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    // Expression statements drop their values, so result is undefined
    // Full integration testing is done in zruntime tests
    const result = try interp.run(&func);
    try std.testing.expect(result.isUndefined());
}

test "Hybrid: reject arena escape to global" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");
    const arena_mod = @import("arena.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var req_arena = try arena_mod.Arena.init(allocator, .{ .size = 4096 });
    defer req_arena.deinit();
    var hybrid = arena_mod.HybridAllocator{
        .persistent = allocator,
        .arena = &req_arena,
    };
    ctx.setHybridAllocator(&hybrid);

    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    const source =
        \\let x = { a: 1 };
    ;

    var p = parser_mod.Parser.init(allocator, source, &strings, &ctx.atoms);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    try std.testing.expectError(error.ArenaObjectEscape, interp.run(&func));
}

test "End-to-end: closure captures local" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    const source =
        \\function make() { let x = 2; return () => x + 3; }
        \\let f = make();
        \\let result = f();
    ;

    var p = parser_mod.Parser.init(allocator, source, &strings, &ctx.atoms);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    _ = try interp.run(&func);

    const result_atom = try ctx.atoms.intern("result");
    const result_opt = ctx.getGlobal(result_atom);
    try std.testing.expect(result_opt != null);
    const result_val = result_opt.?;
    try std.testing.expect(result_val.isInt());
    try std.testing.expectEqual(@as(i32, 5), result_val.getInt());
}

test "End-to-end: JSX parse, compile, and execute" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");
    // builtins imported at module level

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();
    try builtins.initBuiltins(ctx);

    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    const source =
        \\let result = renderToString(<div><span>Hi</span></div>);
        \\let link = renderToString(<a href="/api/health">GET /api/health</a>);
    ;

    var p = parser_mod.Parser.init(allocator, source, &strings, &ctx.atoms);
    defer p.deinit();
    p.enableJsx();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    // Materialize object literal shapes (including JSX props shapes)
    const shapes = p.getShapes();
    if (shapes.len > 0) {
        try ctx.materializeShapes(shapes);
    }

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    _ = try interp.run(&func);

    const result_atom = try ctx.atoms.intern("result");
    const link_atom = try ctx.atoms.intern("link");

    const result_opt = ctx.getGlobal(result_atom);
    const link_opt = ctx.getGlobal(link_atom);
    try std.testing.expect(result_opt != null);
    try std.testing.expect(link_opt != null);

    const result_val = result_opt.?;
    const link_val = link_opt.?;

    try std.testing.expect(result_val.isString());
    try std.testing.expect(link_val.isString());

    const result_str = result_val.toPtr(string.JSString).data();
    const link_str = link_val.toPtr(string.JSString).data();

    try std.testing.expectEqualStrings("<div><span>Hi</span></div>", result_str);
    try std.testing.expectEqualStrings(
        "<a href=\"/api/health\">GET /api/health</a>",
        link_str,
    );
}

test "Interpreter property access" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Test: new_object, put_field (length = 42), get_field, ret
    // Atom.length = 4
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.new_object),
        @intFromEnum(bytecode.Opcode.dup), // Duplicate obj for get_field
        @intFromEnum(bytecode.Opcode.push_i8),
        42,
        @intFromEnum(bytecode.Opcode.put_field),
        4,
        0, // Atom.length = 4 (little-endian u16)
        @intFromEnum(bytecode.Opcode.get_field),
        4,
        0, // Atom.length = 4 (little-endian u16)
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 42), result.getInt());
}

test "Interpreter global access" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Set a global value
    try ctx.setGlobal(.length, value.JSValue.fromInt(100));

    var interp = Interpreter.init(ctx);

    // Test: get_global(length), ret
    // Atom.length = 4
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        4,
        0, // Atom.length = 4 (little-endian u16)
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 100), result.getInt());
}

test "Interpreter native function call" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Define a native function that returns 42
    const testFn = struct {
        fn call(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            return value.JSValue.fromInt(42);
        }
    }.call;

    // Register as global "abs" (Atom.abs = 95)
    try ctx.registerGlobalFunction(.abs, testFn, 0);

    var interp = Interpreter.init(ctx);

    // Test: get_global(abs), call(0), ret
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        95,
        0, // Atom.abs = 95 (little-endian u16)
        @intFromEnum(bytecode.Opcode.call),
        0, // 0 arguments
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 42), result.getInt());
}

test "Interpreter native function with arguments" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Define a native function that adds two numbers
    const addFn = struct {
        fn call(_: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            if (args.len < 2) return value.JSValue.undefined_val;
            const a = args[0].getInt();
            const b = args[1].getInt();
            return value.JSValue.fromInt(a + b);
        }
    }.call;

    // Register as global "max" (Atom.max = 100)
    try ctx.registerGlobalFunction(.max, addFn, 2);

    var interp = Interpreter.init(ctx);

    // Test: get_global(max), push 10, push 20, call(2), ret
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        100,
        0, // Atom.max = 100 (little-endian u16)
        @intFromEnum(bytecode.Opcode.push_i8),
        10,
        @intFromEnum(bytecode.Opcode.push_i8),
        20,
        @intFromEnum(bytecode.Opcode.call),
        2, // 2 arguments
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 30), result.getInt());
}

test "Interpreter bytecode function call" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create a simple function: function add(a, b) { return a + b; }
    // Bytecode: get_loc_0, get_loc_1, add, ret
    // Must heap-allocate since destroyFull will free bc.code
    const inner_code = try allocator.alloc(u8, 4);
    inner_code[0] = @intFromEnum(bytecode.Opcode.get_loc_0); // a
    inner_code[1] = @intFromEnum(bytecode.Opcode.get_loc_1); // b
    inner_code[2] = @intFromEnum(bytecode.Opcode.add);
    inner_code[3] = @intFromEnum(bytecode.Opcode.ret);

    // Must heap-allocate the FunctionBytecode since createBytecodeFunction stores a pointer
    const inner_func = try allocator.create(bytecode.FunctionBytecode);
    inner_func.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 2,
        .local_count = 2, // a, b
        .stack_size = 16,
        .flags = .{},
        .code = inner_code,
        .constants = &.{},
        .source_map = null,
    };

    // Create function object
    const root_class_idx = ctx.root_class_idx;
    const func_obj = try object.JSObject.createBytecodeFunction(allocator, root_class_idx, inner_func, .length);
    defer func_obj.destroyFull(allocator);

    // Register as global "add" (Atom.abs = 95)
    try ctx.setGlobal(.abs, func_obj.toValue());
    defer ctx.setGlobal(.abs, value.JSValue.undefined_val) catch {};

    var interp = Interpreter.init(ctx);

    // Test: get_global(abs), push 7, push 8, call(2), ret
    // This code is stack-allocated but only used for interp.run, not stored
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        95,
        0, // Atom.abs = 95 (little-endian u16)
        @intFromEnum(bytecode.Opcode.push_i8),
        7,
        @intFromEnum(bytecode.Opcode.push_i8),
        8,
        @intFromEnum(bytecode.Opcode.call),
        2, // 2 arguments
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 15), result.getInt()); // 7 + 8 = 15
}

test "JIT: inlined call deopts on callee change" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    const prev_policy = getJitPolicy();
    const prev_threshold = getJitThreshold();
    const prev_warmup = getJitFeedbackWarmup();
    defer {
        setJitPolicy(prev_policy);
        setJitThreshold(prev_threshold);
        setJitFeedbackWarmup(prev_warmup);
    }

    setJitPolicy(.eager);
    setJitThreshold(1);
    setJitFeedbackWarmup(type_feedback.InliningPolicy.MIN_CALL_COUNT);

    if (jitDisabled()) return;

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Callee add(a, b) { return a + b; }
    const add_code = try allocator.alloc(u8, 4);
    add_code[0] = @intFromEnum(bytecode.Opcode.get_loc_0);
    add_code[1] = @intFromEnum(bytecode.Opcode.get_loc_1);
    add_code[2] = @intFromEnum(bytecode.Opcode.add);
    add_code[3] = @intFromEnum(bytecode.Opcode.ret);

    const add_func = try allocator.create(bytecode.FunctionBytecode);
    add_func.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 2,
        .local_count = 2,
        .stack_size = 16,
        .flags = .{},
        .code = add_code,
        .constants = &.{},
        .source_map = null,
    };

    const add_obj_ptr = try object.JSObject.createBytecodeFunction(allocator, ctx.root_class_idx, add_func, .length);
    // destroyFull handles bytecode cleanup including TypeFeedback, no separate cleanupTypeFeedback needed
    defer add_obj_ptr.destroyFull(allocator);

    // Alternate callee sub(a, b) { return a - b; }
    const sub_code = try allocator.alloc(u8, 4);
    sub_code[0] = @intFromEnum(bytecode.Opcode.get_loc_0);
    sub_code[1] = @intFromEnum(bytecode.Opcode.get_loc_1);
    sub_code[2] = @intFromEnum(bytecode.Opcode.sub);
    sub_code[3] = @intFromEnum(bytecode.Opcode.ret);

    const sub_func = try allocator.create(bytecode.FunctionBytecode);
    sub_func.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 2,
        .local_count = 2,
        .stack_size = 16,
        .flags = .{},
        .code = sub_code,
        .constants = &.{},
        .source_map = null,
    };
    const sub_obj_ptr = try object.JSObject.createBytecodeFunction(allocator, ctx.root_class_idx, sub_func, .length);
    // destroyFull handles bytecode cleanup including TypeFeedback, no separate cleanupTypeFeedback needed
    defer sub_obj_ptr.destroyFull(allocator);

    const holder = try ctx.createObject(null);
    defer holder.destroy(allocator);
    try ctx.setPropertyChecked(holder, .abs, add_obj_ptr.toValue());

    const holder_const = [_]value.JSValue{holder.toValue()};

    // Caller: return holder.abs(7, 8);
    const caller_code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0, // constant 0
        @intFromEnum(bytecode.Opcode.get_field),
        95,
        0, // Atom.abs
        @intFromEnum(bytecode.Opcode.push_i8),
        7,
        @intFromEnum(bytecode.Opcode.push_i8),
        8,
        @intFromEnum(bytecode.Opcode.call),
        2,
        @intFromEnum(bytecode.Opcode.ret),
    };

    var caller_func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &caller_code,
        .constants = &holder_const,
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &caller_func);
    defer jit_compile.cleanupCompiledCode(allocator, &caller_func);

    const warmup_calls = type_feedback.InliningPolicy.MIN_CALL_COUNT + 1;
    var i: u32 = 0;
    while (i < warmup_calls) : (i += 1) {
        const res = try interp.run(&caller_func);
        if (i == 0) {
            try std.testing.expectEqual(@as(i32, 15), res.getInt());
        }
    }

    try std.testing.expect(caller_func.compiled_code != null);
    try std.testing.expectEqual(bytecode.CompilationTier.baseline, caller_func.tier);

    // Callee changed -> should trigger inline guard deopt.
    jit.deopt.resetDeoptStats();
    try ctx.setPropertyChecked(holder, .abs, sub_obj_ptr.toValue());
    const result = try interp.run(&caller_func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, -1), result.getInt());

    const stats = jit.deopt.getDeoptStats();
    try std.testing.expectEqual(@as(u64, 1), stats.callee_changed_count);
}

test "call_ic: operational parity with .call and feedback is recorded" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    const prev_policy = getJitPolicy();
    const prev_threshold = getJitThreshold();
    const prev_warmup = getJitFeedbackWarmup();
    defer {
        setJitPolicy(prev_policy);
        setJitThreshold(prev_threshold);
        setJitFeedbackWarmup(prev_warmup);
    }

    // Keep the test on the interpreter path so call_ic feedback is observable
    // via the feedback_site_map without racing against JIT promotion.
    setJitPolicy(.disabled);

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Callee: return 42;
    const callee_code = try allocator.alloc(u8, 4);
    callee_code[0] = @intFromEnum(bytecode.Opcode.push_i8);
    callee_code[1] = 42;
    callee_code[2] = @intFromEnum(bytecode.Opcode.ret);
    callee_code[3] = @intFromEnum(bytecode.Opcode.nop);

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
    const callee_obj_ptr = try object.JSObject.createBytecodeFunction(allocator, ctx.root_class_idx, callee_func, .length);
    defer callee_obj_ptr.destroyFull(allocator);

    const callee_const = [_]value.JSValue{callee_obj_ptr.toValue()};

    // Caller: push_const callee; call_ic argc=0 cache_idx=0; ret
    const caller_code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_const), 0, 0,
        @intFromEnum(bytecode.Opcode.call_ic),    0, 0, 0,
        @intFromEnum(bytecode.Opcode.ret),
    };

    var caller_func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &caller_code,
        .constants = &callee_const,
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &caller_func);
    defer jit_compile.cleanupCompiledCode(allocator, &caller_func);

    // Force feedback allocation so call_ic recording has a slot to write.
    try jit_compile.allocateTypeFeedback(&interp, &caller_func);

    const res = try interp.run(&caller_func);
    try std.testing.expectEqual(@as(i32, 42), res.getInt());

    const tf = caller_func.type_feedback_ptr orelse return error.TestFailed;
    try std.testing.expectEqual(@as(usize, 1), tf.call_sites.len);
    try std.testing.expect(tf.call_sites[0].isMonomorphic());
    try std.testing.expect(tf.call_sites[0].getMonomorphicCallee() == callee_func);
    try std.testing.expectEqual(@as(u32, 1), tf.call_sites[0].total_calls);
}

test "JIT: deopt on type mismatch in specialized add" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    const prev_policy = getJitPolicy();
    const prev_threshold = getJitThreshold();
    const prev_warmup = getJitFeedbackWarmup();
    defer {
        setJitPolicy(prev_policy);
        setJitThreshold(prev_threshold);
        setJitFeedbackWarmup(prev_warmup);
    }

    setJitPolicy(.eager);
    setJitThreshold(1);
    setJitFeedbackWarmup(1);

    if (jitDisabled()) return;

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Callee add(a, b) { return a + b; }
    const add_code = try allocator.alloc(u8, 4);
    add_code[0] = @intFromEnum(bytecode.Opcode.get_loc_0);
    add_code[1] = @intFromEnum(bytecode.Opcode.get_loc_1);
    add_code[2] = @intFromEnum(bytecode.Opcode.add);
    add_code[3] = @intFromEnum(bytecode.Opcode.ret);

    const add_func = try allocator.create(bytecode.FunctionBytecode);
    add_func.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 2,
        .local_count = 2,
        .stack_size = 16,
        .flags = .{},
        .code = add_code,
        .constants = &.{},
        .source_map = null,
    };

    const add_obj = try object.JSObject.createBytecodeFunction(allocator, ctx.root_class_idx, add_func, .length);
    defer add_obj.destroyFull(allocator);
    defer jit_compile.cleanupTypeFeedback(allocator, add_func);

    const func_val = add_obj.toValue();
    const this_val = value.JSValue.undefined_val;
    const int_args = [_]value.JSValue{
        value.JSValue.fromInt(40),
        value.JSValue.fromInt(2),
    };

    _ = try interp.callBytecodeFunction(func_val, add_func, this_val, int_args[0..]);
    _ = try interp.callBytecodeFunction(func_val, add_func, this_val, int_args[0..]);

    try std.testing.expect(add_func.compiled_code != null);

    const str1 = try string.createString(allocator, "a");
    const str2 = try string.createString(allocator, "b");
    defer {
        string.freeString(allocator, str1);
        string.freeString(allocator, str2);
    }
    const str_args = [_]value.JSValue{
        value.JSValue.fromPtr(str1),
        value.JSValue.fromPtr(str2),
    };

    jit.deopt.resetDeoptStats();
    const result = try interp.callBytecodeFunction(func_val, add_func, this_val, str_args[0..]);
    try std.testing.expect(result.isString());
    const result_str = result.toPtr(string.JSString);
    try std.testing.expectEqualStrings("ab", result_str.data());
    string.freeString(allocator, result_str);

    // JIT add handles string concatenation via helper function without deoptimization
    // (type mismatch deopts were removed for add operations - see baseline.zig comment)
    const stats = jit.deopt.getDeoptStats();
    try std.testing.expectEqual(@as(u64, 0), stats.type_mismatch_count);
}

test "JIT: Math int fast paths" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    const prev_policy = getJitPolicy();
    const prev_threshold = getJitThreshold();
    const prev_warmup = getJitFeedbackWarmup();
    defer {
        setJitPolicy(prev_policy);
        setJitThreshold(prev_threshold);
        setJitFeedbackWarmup(prev_warmup);
    }

    setJitPolicy(.eager);
    setJitThreshold(1);
    setJitFeedbackWarmup(1);

    if (jitDisabled()) return;

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();
    try builtins.initBuiltins(ctx);

    var interp = Interpreter.init(ctx);

    const math_atom: u16 = @intFromEnum(object.Atom.Math);
    const abs_atom: u16 = @intFromEnum(object.Atom.abs);
    const floor_atom: u16 = @intFromEnum(object.Atom.floor);
    const ceil_atom: u16 = @intFromEnum(object.Atom.ceil);
    const round_atom: u16 = @intFromEnum(object.Atom.round);
    const min_atom: u16 = @intFromEnum(object.Atom.min);
    const max_atom: u16 = @intFromEnum(object.Atom.max);

    const Runner = struct {
        const Expect = union(enum) { int: i32, float: f64 };
        fn run(runner: *Interpreter, func: *bytecode.FunctionBytecode, expect: Expect) !void {
            var result: value.JSValue = value.JSValue.undefined_val;
            var i: u32 = 0;
            while (i < 3) : (i += 1) {
                result = try runner.run(func);
            }

            switch (expect) {
                .int => |v| {
                    try std.testing.expect(result.isInt());
                    try std.testing.expectEqual(v, result.getInt());
                },
                .float => |v| {
                    try std.testing.expect(result.isFloat64());
                    try std.testing.expectEqual(v, result.getFloat64());
                },
            }

            try std.testing.expect(func.compiled_code != null);
            try std.testing.expectEqual(bytecode.CompilationTier.baseline, func.tier);
        }
    };

    // Math.abs(INT_MIN) -> 2147483648 (must be float)
    const abs_consts = [_]value.JSValue{value.JSValue.fromInt(std.math.minInt(i32))};
    const code_abs = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(abs_atom & 0xFF),
        @intCast(abs_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0,
        @intFromEnum(bytecode.Opcode.call),
        1,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_abs = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_abs,
        .constants = &abs_consts,
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_abs);
    defer jit_compile.cleanupCompiledCode(allocator, &func_abs);
    try Runner.run(&interp, &func_abs, .{ .float = 2147483648.0 });

    // Math.floor(5) -> 5
    const code_floor = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(floor_atom & 0xFF),
        @intCast(floor_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_i8),
        5,
        @intFromEnum(bytecode.Opcode.call),
        1,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_floor = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_floor,
        .constants = &.{},
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_floor);
    defer jit_compile.cleanupCompiledCode(allocator, &func_floor);
    try Runner.run(&interp, &func_floor, .{ .int = 5 });

    // Math.ceil(5) -> 5
    const code_ceil = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(ceil_atom & 0xFF),
        @intCast(ceil_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_i8),
        5,
        @intFromEnum(bytecode.Opcode.call),
        1,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_ceil = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_ceil,
        .constants = &.{},
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_ceil);
    defer jit_compile.cleanupCompiledCode(allocator, &func_ceil);
    try Runner.run(&interp, &func_ceil, .{ .int = 5 });

    // Math.round(5) -> 5
    const code_round = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(round_atom & 0xFF),
        @intCast(round_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_i8),
        5,
        @intFromEnum(bytecode.Opcode.call),
        1,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_round = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_round,
        .constants = &.{},
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_round);
    defer jit_compile.cleanupCompiledCode(allocator, &func_round);
    try Runner.run(&interp, &func_round, .{ .int = 5 });

    // Math.min(7, -3) -> -3
    const code_min = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(min_atom & 0xFF),
        @intCast(min_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_i8),
        7,
        @intFromEnum(bytecode.Opcode.push_i8),
        @as(u8, @bitCast(@as(i8, -3))),
        @intFromEnum(bytecode.Opcode.call),
        2,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_min = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_min,
        .constants = &.{},
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_min);
    defer jit_compile.cleanupCompiledCode(allocator, &func_min);
    try Runner.run(&interp, &func_min, .{ .int = -3 });

    // Math.max(7, -3) -> 7
    const code_max = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(max_atom & 0xFF),
        @intCast(max_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_i8),
        7,
        @intFromEnum(bytecode.Opcode.push_i8),
        @as(u8, @bitCast(@as(i8, -3))),
        @intFromEnum(bytecode.Opcode.call),
        2,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_max = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_max,
        .constants = &.{},
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_max);
    defer jit_compile.cleanupCompiledCode(allocator, &func_max);
    try Runner.run(&interp, &func_max, .{ .int = 7 });

    // Mixed int/float cases (force helper path)
    // With NaN-boxing, floats are stored inline - no allocation needed
    const absf_val = value.JSValue.fromFloat(-2.5);
    const floorf_val = value.JSValue.fromFloat(5.5);
    const ceilf_val = value.JSValue.fromFloat(-3.2);
    const roundf_val = value.JSValue.fromFloat(2.6);
    const minf_val = value.JSValue.fromFloat(3.5);
    const maxf_val = value.JSValue.fromFloat(4.75);

    // Math.abs(-2.5) -> 2.5
    const absf_consts = [_]value.JSValue{absf_val};
    const code_absf = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(abs_atom & 0xFF),
        @intCast(abs_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0,
        @intFromEnum(bytecode.Opcode.call),
        1,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_absf = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_absf,
        .constants = &absf_consts,
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_absf);
    defer jit_compile.cleanupCompiledCode(allocator, &func_absf);
    try Runner.run(&interp, &func_absf, .{ .float = 2.5 });

    // Math.floor(5.5) -> 5
    const floorf_consts = [_]value.JSValue{floorf_val};
    const code_floorf = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(floor_atom & 0xFF),
        @intCast(floor_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0,
        @intFromEnum(bytecode.Opcode.call),
        1,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_floorf = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_floorf,
        .constants = &floorf_consts,
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_floorf);
    defer jit_compile.cleanupCompiledCode(allocator, &func_floorf);
    try Runner.run(&interp, &func_floorf, .{ .int = 5 });

    // Math.ceil(-3.2) -> -3
    const ceilf_consts = [_]value.JSValue{ceilf_val};
    const code_ceilf = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(ceil_atom & 0xFF),
        @intCast(ceil_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0,
        @intFromEnum(bytecode.Opcode.call),
        1,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_ceilf = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_ceilf,
        .constants = &ceilf_consts,
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_ceilf);
    defer jit_compile.cleanupCompiledCode(allocator, &func_ceilf);
    try Runner.run(&interp, &func_ceilf, .{ .int = -3 });

    // Math.round(2.6) -> 3
    const roundf_consts = [_]value.JSValue{roundf_val};
    const code_roundf = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(round_atom & 0xFF),
        @intCast(round_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0,
        @intFromEnum(bytecode.Opcode.call),
        1,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_roundf = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_roundf,
        .constants = &roundf_consts,
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_roundf);
    defer jit_compile.cleanupCompiledCode(allocator, &func_roundf);
    try Runner.run(&interp, &func_roundf, .{ .int = 3 });

    // Math.min(7, 3.5) -> 3.5
    const minf_consts = [_]value.JSValue{minf_val};
    const code_minf = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(min_atom & 0xFF),
        @intCast(min_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_i8),
        7,
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0,
        @intFromEnum(bytecode.Opcode.call),
        2,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_minf = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_minf,
        .constants = &minf_consts,
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_minf);
    defer jit_compile.cleanupCompiledCode(allocator, &func_minf);
    try Runner.run(&interp, &func_minf, .{ .float = 3.5 });

    // Math.max(-2, 4.75) -> 4.75
    const maxf_consts = [_]value.JSValue{maxf_val};
    const code_maxf = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        @intCast(math_atom & 0xFF),
        @intCast(math_atom >> 8),
        @intFromEnum(bytecode.Opcode.get_field),
        @intCast(max_atom & 0xFF),
        @intCast(max_atom >> 8),
        @intFromEnum(bytecode.Opcode.push_i8),
        @as(u8, @bitCast(@as(i8, -2))),
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0,
        @intFromEnum(bytecode.Opcode.call),
        2,
        @intFromEnum(bytecode.Opcode.ret),
    };
    var func_maxf = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code_maxf,
        .constants = &maxf_consts,
        .source_map = null,
    };
    defer jit_compile.cleanupTypeFeedback(allocator, &func_maxf);
    defer jit_compile.cleanupCompiledCode(allocator, &func_maxf);
    try Runner.run(&interp, &func_maxf, .{ .float = 4.75 });
}

test "End-to-end: function declaration" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test: nested function declaration and call
    // Note: Expression statements drop values, so we verify execution completes
    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    const code_str = "function outer() { function add(a, b) { return a + b; } return add(3, 4); } outer()";
    var p = parser_mod.Parser.init(allocator, code_str, &strings, null);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    // Expression statements drop values, so result is undefined
    const result = try interp.run(&func);
    try std.testing.expect(result.isUndefined());
}

test "Interpreter string concatenation" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Create two string constants
    const str1 = try string.createString(allocator, "hello");
    const str2 = try string.createString(allocator, " world");

    // Test: push "hello", push " world", add (concat), ret
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0, // constant 0
        @intFromEnum(bytecode.Opcode.push_const),
        1,
        0, // constant 1
        @intFromEnum(bytecode.Opcode.add),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const constants = [_]value.JSValue{
        value.JSValue.fromPtr(str1),
        value.JSValue.fromPtr(str2),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &constants,
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isString());
    const result_str = result.toPtr(string.JSString);
    try std.testing.expectEqualStrings("hello world", result_str.data());

    // Cleanup
    string.freeString(allocator, result_str);
    string.freeString(allocator, str1);
    string.freeString(allocator, str2);
}

test "Interpreter typeof" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Test: typeof 42 = "number"
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),
        42,
        @intFromEnum(bytecode.Opcode.typeof),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isString());
    const result_str = result.toPtr(string.JSString);
    try std.testing.expectEqualStrings("number", result_str.data());

    // Cleanup
    string.freeString(allocator, result_str);
}

test "End-to-end: default parameters" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test: function with default parameter
    // Note: Expression statements drop values, so we verify execution completes
    var strings = string_mod.StringTable.init(allocator);

    const code_str = "function outer() { function greet(name = 'World') { return name; } return greet(); } outer()";
    var p = parser_mod.Parser.init(allocator, code_str, &strings, null);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    // Expression statements drop values, so result is undefined
    const result = try interp.run(&func);
    try std.testing.expect(result.isUndefined());
}

test "Interpreter unary negation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 42, negate it: -42
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 42,
        @intFromEnum(bytecode.Opcode.neg),     @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, -42), result.getInt());
}

test "Interpreter increment and decrement" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 10, increment: 11
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.inc),     @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 11), result.getInt());
}

test "Interpreter logical not" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // !true = false
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_true),
        @intFromEnum(bytecode.Opcode.not),
        @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.false_val, result);
}

test "Interpreter bitwise not" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // ~0 = -1 (all bits flipped)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_0),
        @intFromEnum(bytecode.Opcode.bit_not),
        @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, -1), result.getInt());
}

test "Interpreter power operator" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // 2 ** 10 = 1024
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_2),
        @intFromEnum(bytecode.Opcode.push_i8),
        10,
        @intFromEnum(bytecode.Opcode.pow),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    // pow always returns a float
    try std.testing.expect(result.isFloat64());
    try std.testing.expectEqual(@as(f64, 1024.0), result.getFloat64());
}

test "Interpreter new_object" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create empty object
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.new_object),
        @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isObject());
}

test "Interpreter strict equality edge cases" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // null === null should be true
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_null),
        @intFromEnum(bytecode.Opcode.push_null),
        @intFromEnum(bytecode.Opcode.strict_eq),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.true_val, result);
}

test "Interpreter local variable get and put" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Store 42 in local 0, then return it
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),   42,
        @intFromEnum(bytecode.Opcode.put_loc_0), @intFromEnum(bytecode.Opcode.get_loc_0),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 1,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 42), result.getInt());
}

test "Interpreter multiple locals" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Store 10 in local 0, 20 in local 1, add them
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),   10,
        @intFromEnum(bytecode.Opcode.put_loc_0), @intFromEnum(bytecode.Opcode.push_i8),
        20,                                      @intFromEnum(bytecode.Opcode.put_loc_1),
        @intFromEnum(bytecode.Opcode.get_loc_0), @intFromEnum(bytecode.Opcode.get_loc_1),
        @intFromEnum(bytecode.Opcode.add),       @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 30), result.getInt());
}

test "Interpreter undefined equals undefined" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // undefined === undefined should be true
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_undefined),
        @intFromEnum(bytecode.Opcode.push_undefined),
        @intFromEnum(bytecode.Opcode.strict_eq),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.true_val, result);
}

test "Interpreter null vs undefined strict not equal" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // null === undefined should be false
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_null),
        @intFromEnum(bytecode.Opcode.push_undefined),
        @intFromEnum(bytecode.Opcode.strict_eq),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.false_val, result);
}

test "Interpreter typeof operations" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // typeof 42 should return "number"
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 42,
        @intFromEnum(bytecode.Opcode.typeof),  @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("number", result.toPtr(string.JSString).data());
}

test "Interpreter modulo operation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // 10 % 3 = 1
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.push_3),  @intFromEnum(bytecode.Opcode.mod),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 1), result.getInt());
}

test "Interpreter inc dec roundtrip" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // 5 -> inc -> dec -> should be 5
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 5,
        @intFromEnum(bytecode.Opcode.inc),     @intFromEnum(bytecode.Opcode.dec),
        @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 5), result.getInt());
}

test "Interpreter negation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // -42 should be -42
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 42,
        @intFromEnum(bytecode.Opcode.neg),     @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, -42), result.getInt());
}

test "Interpreter dup operation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 7, dup, add -> 14
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 7,
        @intFromEnum(bytecode.Opcode.dup),     @intFromEnum(bytecode.Opcode.add),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 14), result.getInt());
}

test "Interpreter drop operation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 5, push 10, drop, ret -> should return 5
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 5,
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.drop),    @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 5), result.getInt());
}

test "Interpreter swap operation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 10, push 3, swap, sub -> 10 - 3 = 7
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.push_3),  @intFromEnum(bytecode.Opcode.swap),
        @intFromEnum(bytecode.Opcode.sub),     @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    // After swap: [3, 10], sub: 3 - 10 = -7
    try std.testing.expectEqual(@as(i32, -7), result.getInt());
}

test "Interpreter not operator inverts false" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // !false = true
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_false),
        @intFromEnum(bytecode.Opcode.not),
        @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.true_val, result);
}

test "Interpreter new_array" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create empty array (new_array takes u16 length)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.new_array), 0, 0, // u16 length = 0
        @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isObject());
}

test "Interpreter ret_undefined" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.ret_undefined),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isUndefined());
}

test "Interpreter push_i16" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 1000 (requires i16)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i16),
        @as(u8, @truncate(1000 & 0xFF)), // low byte
        @as(u8, @truncate((1000 >> 8) & 0xFF)), // high byte
        @intFromEnum(bytecode.Opcode.ret),
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

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 1000), result.getInt());
}

test "PolymorphicInlineCache unit tests" {
    // Test PIC lookup and update
    var pic = PolymorphicInlineCache{};

    // Create distinct hidden class indices
    const class1: object.HiddenClassIndex = @enumFromInt(1);
    const class2: object.HiddenClassIndex = @enumFromInt(2);
    const class3: object.HiddenClassIndex = @enumFromInt(3);
    const class4: object.HiddenClassIndex = @enumFromInt(4);
    const class5: object.HiddenClassIndex = @enumFromInt(5);

    // Initially empty
    try std.testing.expectEqual(@as(?u16, null), pic.lookup(class1));
    try std.testing.expectEqual(@as(u8, 0), pic.count);
    try std.testing.expect(!pic.megamorphic);

    // Add first entry
    try std.testing.expect(pic.update(class1, 10));
    try std.testing.expectEqual(@as(u8, 1), pic.count);
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1));

    // Add second entry
    try std.testing.expect(pic.update(class2, 20));
    try std.testing.expectEqual(@as(u8, 2), pic.count);
    try std.testing.expectEqual(@as(?u16, 20), pic.lookup(class2));

    // First entry still works
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1));

    // Fill cache up to effective cap (MAX_POLYMORPHIC_SHAPES)
    try std.testing.expect(pic.update(class3, 30));
    try std.testing.expect(pic.update(class4, 40));
    try std.testing.expectEqual(@as(u8, effective_pic_cap), pic.count);
    try std.testing.expect(!pic.megamorphic);

    // All entries work
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1));
    try std.testing.expectEqual(@as(?u16, 20), pic.lookup(class2));
    try std.testing.expectEqual(@as(?u16, 30), pic.lookup(class3));
    try std.testing.expectEqual(@as(?u16, 40), pic.lookup(class4));

    // One-past-cap entry triggers megamorphic
    try std.testing.expect(!pic.update(class5, 50));
    try std.testing.expect(pic.megamorphic);
    try std.testing.expectEqual(@as(?u16, null), pic.lookup(class5));

    // Existing entries still work (megamorphic doesn't clear cache)
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1));

    // Update existing entry in megamorphic state - returns false because we
    // don't mutate the cache while megamorphic; the original slot remains valid.
    try std.testing.expect(!pic.update(class1, 100));
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1)); // unchanged

    // Reset test
    pic.reset();
    try std.testing.expectEqual(@as(u8, 0), pic.count);
    try std.testing.expect(!pic.megamorphic);
    try std.testing.expectEqual(@as(?u16, null), pic.lookup(class1));
}

test "PolymorphicInlineCache: megamorphic recovery after stable shape window" {
    var pic = PolymorphicInlineCache{};
    const window = getPicMegaRecoveryWindow();

    // Saturate beyond cap to force megamorphic.
    var class_idx: u16 = 1;
    while (class_idx <= effective_pic_cap + 1) : (class_idx += 1) {
        const cls: object.HiddenClassIndex = @enumFromInt(class_idx);
        _ = pic.update(cls, class_idx * 10);
    }
    try std.testing.expect(pic.megamorphic);

    const dominant: object.HiddenClassIndex = @enumFromInt(42);
    var observations: u16 = 0;
    while (observations < window - 1) : (observations += 1) {
        try std.testing.expect(!pic.update(dominant, 777));
    }
    try std.testing.expect(pic.megamorphic);
    try std.testing.expect(!pic.just_recovered);

    // One more observation crosses the threshold and performs recovery.
    try std.testing.expect(pic.update(dominant, 777));
    try std.testing.expect(!pic.megamorphic);
    try std.testing.expect(pic.just_recovered);
    try std.testing.expectEqual(@as(u8, 1), pic.count);
    try std.testing.expectEqual(@as(?u16, 777), pic.lookup(dominant));

    // A second shape interrupting recovery counting should not recover.
    pic.reset();
    class_idx = 1;
    while (class_idx <= effective_pic_cap + 1) : (class_idx += 1) {
        const cls: object.HiddenClassIndex = @enumFromInt(class_idx);
        _ = pic.update(cls, class_idx * 10);
    }
    try std.testing.expect(pic.megamorphic);

    const shape_a: object.HiddenClassIndex = @enumFromInt(100);
    const shape_b: object.HiddenClassIndex = @enumFromInt(101);
    var i: u16 = 0;
    while (i < window * 2) : (i += 1) {
        const cls = if (i % 2 == 0) shape_a else shape_b;
        try std.testing.expect(!pic.update(cls, 1));
    }
    try std.testing.expect(pic.megamorphic);
    try std.testing.expect(!pic.just_recovered);
}

test "End-to-end: polymorphic property access" {
    // Test that PIC handles multiple object shapes correctly
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    // Test: access .x on objects with different shapes (polymorphic site)
    // Each object has a different set of properties, so different hidden classes
    const source =
        \\function getX(obj) { return obj.x; }
        \\let a = { x: 1 };
        \\let b = { x: 10, y: 2 };
        \\let c = { x: 100, y: 3, z: 4 };
        \\let d = { w: 0, x: 1000 };
        \\let result = getX(a) + getX(b) + getX(c) + getX(d);
    ;

    var p = parser_mod.Parser.init(allocator, source, &strings, &ctx.atoms);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    // Materialize object literal shapes before execution
    const shapes = p.getShapes();
    if (shapes.len > 0) {
        try ctx.materializeShapes(shapes);
    }

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    _ = try interp.run(&func);

    // Get result from global
    const result_atom = try ctx.atoms.intern("result");
    const result_opt = ctx.getGlobal(result_atom);
    try std.testing.expect(result_opt != null);

    const result_val = result_opt.?;
    try std.testing.expect(result_val.isInt());
    // 1 + 10 + 100 + 1000 = 1111
    try std.testing.expectEqual(@as(i32, 1111), result_val.getInt());
}

test "JIT profiling: execution counting" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test that function execution counting works correctly
    var func = bytecode.FunctionBytecode{
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

    // Initially at tier interpreted with zero count
    try std.testing.expectEqual(bytecode.CompilationTier.interpreted, func.tier);
    try std.testing.expectEqual(@as(u32, 0), func.execution_count);

    var interp = Interpreter.init(ctx);

    // Simulate function calls up to threshold - 1
    var i: u32 = 0;
    while (i < bytecode.JIT_THRESHOLD - 1) : (i += 1) {
        const promoted = interp.profileFunctionEntry(&func);
        try std.testing.expect(!promoted);
        try std.testing.expectEqual(bytecode.CompilationTier.interpreted, func.tier);
    }
    try std.testing.expectEqual(bytecode.JIT_THRESHOLD - 1, func.execution_count);

    // One more call should hit threshold and promote to baseline_candidate
    const promoted = interp.profileFunctionEntry(&func);
    try std.testing.expect(promoted);
    try std.testing.expectEqual(bytecode.CompilationTier.baseline_candidate, func.tier);
    try std.testing.expectEqual(bytecode.JIT_THRESHOLD, func.execution_count);
    try std.testing.expectEqual(@as(u32, 1), interp.tier_promotions[@intFromEnum(bytecode.CompilationTier.baseline_candidate)]);

    // Subsequent calls don't re-promote
    const promoted2 = interp.profileFunctionEntry(&func);
    try std.testing.expect(!promoted2);
    try std.testing.expectEqual(bytecode.CompilationTier.baseline_candidate, func.tier);
}

test "tiering: deopt storm suppresses optimized promotion when enabled" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Force the feature on for this test. Restore default after.
    jit_policy.setTieringDeoptSuppressForTests(true);
    defer jit_policy.setTieringDeoptSuppressForTests(null);

    var func = bytecode.FunctionBytecode{
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

    // Simulate a function that already reached .baseline and has just deopted
    // DEOPT_SUPPRESS_COUNT times, with the most recent deopt at the current
    // exec count.
    func.tier = bytecode.CompilationTier.baseline;
    func.execution_count = bytecode.OPTIMIZED_THRESHOLD - 1;
    func.deopt_count = type_feedback.InliningPolicy.DEOPT_SUPPRESS_COUNT;
    func.last_deopt_exec_count = func.execution_count;

    var interp = Interpreter.init(ctx);

    // Next profile bumps exec_count to OPTIMIZED_THRESHOLD and should reject
    // promotion because of the recent deopt storm.
    const promoted = interp.profileFunctionEntry(&func);
    try std.testing.expect(!promoted);
    try std.testing.expectEqual(bytecode.CompilationTier.baseline, func.tier);
    try std.testing.expectEqual(@as(u32, 1), interp.promotion_attempted);
    try std.testing.expectEqual(@as(u32, 0), interp.promotion_succeeded);
    try std.testing.expectEqual(@as(u32, 1), interp.promotion_rejected_deopt_storm);
}

test "tiering: deopt storm does not suppress when feature is disabled" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Feature explicitly off.
    jit_policy.setTieringDeoptSuppressForTests(false);
    defer jit_policy.setTieringDeoptSuppressForTests(null);

    var func = bytecode.FunctionBytecode{
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

    func.tier = bytecode.CompilationTier.baseline;
    func.execution_count = bytecode.OPTIMIZED_THRESHOLD - 1;
    func.deopt_count = type_feedback.InliningPolicy.DEOPT_SUPPRESS_COUNT + 5;
    func.last_deopt_exec_count = func.execution_count;

    var interp = Interpreter.init(ctx);

    const promoted = interp.profileFunctionEntry(&func);
    try std.testing.expect(promoted);
    try std.testing.expectEqual(bytecode.CompilationTier.optimized_candidate, func.tier);
    try std.testing.expectEqual(@as(u32, 1), interp.promotion_attempted);
    try std.testing.expectEqual(@as(u32, 1), interp.promotion_succeeded);
    try std.testing.expectEqual(@as(u32, 0), interp.promotion_rejected_deopt_storm);
}

test "tiering: shouldSuppressOnDeoptStorm boundary conditions" {
    const Policy = type_feedback.InliningPolicy;

    // Below threshold: never suppress.
    try std.testing.expect(!Policy.shouldSuppressOnDeoptStorm(
        Policy.DEOPT_SUPPRESS_COUNT - 1,
        5_000,
        5_000,
    ));

    // At threshold and recent: suppress.
    try std.testing.expect(Policy.shouldSuppressOnDeoptStorm(
        Policy.DEOPT_SUPPRESS_COUNT,
        1_500,
        1_500,
    ));

    // At threshold but the last deopt is outside the cooldown: do not suppress.
    try std.testing.expect(!Policy.shouldSuppressOnDeoptStorm(
        Policy.DEOPT_SUPPRESS_COUNT + 5,
        Policy.DEOPT_SUPPRESS_COOLDOWN + 10,
        0,
    ));
}

test "JIT profiling: back-edge counting" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Initially zero
    try std.testing.expectEqual(@as(u32, 0), interp.backedge_count);

    // Simulate back-edges up to threshold - 1
    var i: u32 = 0;
    while (i < bytecode.LOOP_THRESHOLD - 1) : (i += 1) {
        const hot = interp.profileBackedge();
        try std.testing.expect(!hot);
    }

    // One more should hit threshold
    const hot = interp.profileBackedge();
    try std.testing.expect(hot);
    try std.testing.expectEqual(bytecode.LOOP_THRESHOLD, interp.backedge_count);

    // Reset and verify
    interp.resetProfilingCounters();
    try std.testing.expectEqual(@as(u32, 0), interp.backedge_count);
}

test "JIT profiling: PIC hit/miss counting" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Initially zero
    try std.testing.expectEqual(@as(u32, 0), interp.pic_hits);
    try std.testing.expectEqual(@as(u32, 0), interp.pic_misses);

    // Simulate some hits and misses
    interp.pic_hits = 100;
    interp.pic_misses = 10;

    // Calculate hit rate
    const total = interp.pic_hits + interp.pic_misses;
    const hit_rate = @as(f32, @floatFromInt(interp.pic_hits)) / @as(f32, @floatFromInt(total));
    try std.testing.expect(hit_rate > 0.9); // 90%+ hit rate

    // Reset and verify
    interp.resetProfilingCounters();
    try std.testing.expectEqual(@as(u32, 0), interp.pic_hits);
    try std.testing.expectEqual(@as(u32, 0), interp.pic_misses);
}

test "perf snapshot: empty program yields zeroed counters" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);
    const snapshot = interp.snapshotPerfStats();

    try std.testing.expectEqual(@as(u32, 0), snapshot.backedge_count);
    try std.testing.expectEqual(@as(u32, 0), snapshot.pic_hits);
    try std.testing.expectEqual(@as(u32, 0), snapshot.pic_misses);
    try std.testing.expectEqual(@as(u32, 0), snapshot.deopt_count);
    try std.testing.expectEqual(@as(u32, 0), snapshot.opcode_histogram_nonzero);
    for (snapshot.tier_promotions) |count| {
        try std.testing.expectEqual(@as(u32, 0), count);
    }
}

test "JIT integration: tryCompileBaseline triggers on threshold" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Create a simple bytecode function that can be JIT-compiled
    // push_1 + ret is supported by the baseline compiler
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_1),
        @intFromEnum(bytecode.Opcode.ret),
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
        .execution_count = 0,
        .tier = .interpreted,
        .compiled_code = null,
    };

    // Initially interpreted
    try std.testing.expectEqual(bytecode.CompilationTier.interpreted, func.tier);
    try std.testing.expect(func.compiled_code == null);

    // Simulate calls up to threshold - 1
    func.execution_count = bytecode.JIT_THRESHOLD - 1;

    // Next call should trigger JIT compilation
    const is_candidate = interp.profileFunctionEntry(&func);
    try std.testing.expect(is_candidate);
    try std.testing.expectEqual(bytecode.CompilationTier.baseline_candidate, func.tier);

    // Try to compile
    try jit_compile.tryCompileBaseline(&interp, &func);

    // Should now be compiled
    try std.testing.expectEqual(bytecode.CompilationTier.baseline, func.tier);
    try std.testing.expect(func.compiled_code != null);

    // Clean up the compiled code
    if (func.compiled_code) |cc_opaque| {
        const cc: *jit.CompiledCode = @ptrCast(@alignCast(cc_opaque));
        allocator.destroy(cc);
    }
}

test "JIT integration: unsupported opcodes stay interpreted" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Create a bytecode function with unsupported opcode (call_spread)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.call_spread),
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
        .execution_count = bytecode.JIT_THRESHOLD - 1,
        .tier = .baseline_candidate,
        .compiled_code = null,
    };

    // Try to compile - should fail with UnsupportedOpcode and revert to interpreted
    try jit_compile.tryCompileBaseline(&interp, &func);

    // Should stay interpreted (not baseline)
    try std.testing.expectEqual(bytecode.CompilationTier.interpreted, func.tier);
    try std.testing.expect(func.compiled_code == null);
}

