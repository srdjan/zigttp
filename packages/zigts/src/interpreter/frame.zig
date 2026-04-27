//! Frame-state and upvalue lifecycle helpers.
//!
//! `pushState`/`popState` save and restore the per-frame interpreter state
//! (pc, code_end, constants, current_func, plus context counters) across a
//! nested call. `captureUpvalue`/`closeUpvaluesAbove` manage the linked
//! list of open upvalues that lexically references locals on the value
//! stack.
//!
//! Lived as methods on `Interpreter` until the Slice F split. Moved here
//! as free functions taking `*Interpreter` so frame management is no
//! longer tangled with the dispatch-heavy interpreter.zig. The nested
//! `MAX_STATE_DEPTH` and `SavedState` types are now `pub` on Interpreter
//! to allow this access.

const value = @import("../value.zig");
const bytecode = @import("../bytecode.zig");
const object = @import("../object.zig");
const jit = @import("../jit/root.zig");
const jit_compile = @import("jit_compile.zig");
const jit_policy = @import("jit_policy.zig");
const trace = @import("trace.zig");
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;
const InterpreterError = Interpreter.InterpreterError;

pub fn pushState(self: *Interpreter) InterpreterError!void {
    if (self.state_depth >= Interpreter.MAX_STATE_DEPTH) {
        return error.CallStackOverflow;
    }
    self.state_stack[self.state_depth] = .{
        .pc = self.pc,
        .code_end = self.code_end,
        .constants = self.constants,
        .current_func = self.current_func,
        .sp = self.ctx.sp,
        .fp = self.ctx.fp,
        .call_depth = self.ctx.call_depth,
        .catch_depth = self.ctx.catch_depth,
        .exception = self.ctx.exception,
    };
    self.state_depth += 1;
}

pub fn popState(self: *Interpreter) void {
    @import("std").debug.assert(self.state_depth > 0);
    self.state_depth -= 1;
    const state = self.state_stack[self.state_depth];
    self.pc = state.pc;
    self.code_end = state.code_end;
    self.constants = state.constants;
    self.current_func = state.current_func;
    self.ctx.sp = state.sp;
    self.ctx.fp = state.fp;
    self.ctx.call_depth = state.call_depth;
    self.ctx.catch_depth = state.catch_depth;
    self.ctx.exception = state.exception;
}

/// Capture a local variable as an upvalue.
/// Reuses an existing open upvalue when one already points at the slot.
pub fn captureUpvalue(self: *Interpreter, local_idx: u8) !*object.Upvalue {
    const local_ptr = self.ctx.getLocalPtr(local_idx);

    // Search for existing open upvalue pointing to this slot.
    var prev: ?*object.Upvalue = null;
    var current = self.open_upvalues;
    while (current) |uv| {
        switch (uv.location) {
            .open => |ptr| {
                if (ptr == local_ptr) {
                    return uv;
                }
                // Upvalues are ordered by slot address (higher addresses first);
                // once we pass the target slot, insert before the current node.
                if (@intFromPtr(ptr) < @intFromPtr(local_ptr)) {
                    break;
                }
            },
            .closed => {},
        }
        prev = uv;
        current = uv.next;
    }

    const new_uv = try self.ctx.gc_state.acquireUpvalue();
    new_uv.* = object.Upvalue.init(local_ptr);

    if (prev) |p| {
        new_uv.next = p.next;
        p.next = new_uv;
    } else {
        new_uv.next = self.open_upvalues;
        self.open_upvalues = new_uv;
    }

    return new_uv;
}

/// Close all open upvalues that reference slots at or above the given index.
pub fn closeUpvaluesAbove(self: *Interpreter, local_idx: u8) void {
    const threshold = self.ctx.getLocalPtr(local_idx);

    while (self.open_upvalues) |uv| {
        switch (uv.location) {
            .open => |ptr| {
                if (@intFromPtr(ptr) < @intFromPtr(threshold)) {
                    break;
                }
                uv.close();
                self.open_upvalues = uv.next;
            },
            .closed => {
                self.open_upvalues = uv.next;
            },
        }
    }
}

/// Execute a bytecode function with given arguments and return the result.
///
/// Lifecycle: profile entry (and possibly trigger JIT compile), save the
/// caller's interpreter state, push a call frame on the context stack,
/// initialize locals from `args` (undefined for missing positions),
/// dispatch via JIT or interpreter, then pop the frame and restore.
///
/// Closures: callers must set up upvalue access on `func_val` before
/// arrival; this function relies on the context's scope chain.
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
        jit_compile.allocateTypeFeedback(self, func_bc_mut) catch {};

        // If no feedback sites exist, compile immediately
        if (func_bc_mut.type_feedback_ptr == null and func_bc_mut.feedback_site_map == null) {
            jit_compile.tryCompileBaseline(self, func_bc_mut) catch {};
        }
    }

    // Compile after feedback warmup if eligible
    if (!jit_policy.jitDisabled() and func_bc_mut.tier == .baseline_candidate and func_bc_mut.type_feedback_ptr != null) {
        const warmup_target = jit_policy.getJitThreshold() + jit_policy.getJitFeedbackWarmup();
        if (func_bc_mut.execution_count >= warmup_target) {
            jit_compile.tryCompileBaseline(self, func_bc_mut) catch {};
        }
    }

    // After warmup with type feedback, compile to baseline.
    // (Hot-loop promotion uses post-execution feedback allocation -- see allocateTypeFeedbackDeferred below.)
    if (!jit_policy.jitDisabled() and func_bc_mut.tier == .baseline_candidate and func_bc_mut.execution_count < jit_policy.getJitThreshold()) {
        if (func_bc_mut.type_feedback_ptr) |tf| {
            const baseline_warmup: u32 = 50;
            if (func_bc_mut.execution_count >= baseline_warmup or tf.totalHits() > 100) {
                jit_compile.tryCompileBaseline(self, func_bc_mut) catch {};
            }
        }
    }

    // Optimized tier promotion: compile when promoted from baseline by hot loop
    if (!jit_policy.jitDisabled() and func_bc_mut.tier == .optimized_candidate) {
        jit_compile.tryCompileOptimized(self, func_bc_mut) catch {
            jit_compile.setTier(self, func_bc_mut, .baseline);
        };
    }

    try pushState(self);
    defer popState(self);

    try self.ctx.pushFrame(func_val, this_val, @intFromPtr(self.pc));
    errdefer {
        closeUpvaluesAbove(self, 0);
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
    if (!jit_policy.jitDisabled() and (func_bc.tier == .baseline or func_bc.tier == .optimized)) {
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
            const prev_interp = interpreter.current_interpreter;
            interpreter.current_interpreter = self;
            defer interpreter.current_interpreter = prev_interp;
            // Set interpreter pointer in context for IC fast path
            self.ctx.jit_interpreter = @ptrCast(self);
            defer self.ctx.jit_interpreter = null;
            const result_raw = cc.execute(self.ctx);
            const result = value.JSValue{ .raw = result_raw };

            closeUpvaluesAbove(self, 0);
            _ = self.ctx.popFrame();

            // Allocate type feedback after function completes (safe boundary)
            if (!jit_policy.jitDisabled() and func_bc_mut.tier == .baseline_candidate) {
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

    closeUpvaluesAbove(self, 0);
    _ = self.ctx.popFrame();

    // Allocate type feedback after function completes (safe boundary)
    if (!jit_policy.jitDisabled() and func_bc_mut.tier == .baseline_candidate) {
        jit_compile.allocateTypeFeedbackDeferred(self, func_bc_mut) catch {};
    }

    return result;
}
