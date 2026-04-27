//! Frame-state and upvalue lifecycle helpers, plus the bytecode call entry.

const std = @import("std");
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
    std.debug.assert(self.state_depth > 0);
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

pub fn callBytecodeFunction(
    self: *Interpreter,
    func_val: value.JSValue,
    func_bc: *const bytecode.FunctionBytecode,
    this_val: value.JSValue,
    args: []const value.JSValue,
) InterpreterError!value.JSValue {
    trace.traceCall(self, "bc enter", @intCast(args.len), false);
    defer trace.traceCall(self, "bc exit", @intCast(args.len), false);
    // const_cast safe: only profiling fields are mutated below.
    const func_bc_mut = @constCast(func_bc);

    jit_compile.maybePromote(self, func_bc_mut, .nested);

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

/// Top-level execution entry. Unlike `callBytecodeFunction`, this is the
/// outermost frame: no pushState/popState, no errdefer-popFrame, locals
/// seeded with `undefined` rather than caller-supplied args, JIT-compiled
/// path returns directly without re-entering upvalue cleanup.
pub fn run(self: *Interpreter, func: *const bytecode.FunctionBytecode) InterpreterError!value.JSValue {
    // const_cast safe: only profiling fields are mutated below.
    const func_mut = @constCast(func);

    jit_compile.maybePromote(self, func_mut, .entry);

    // Allocate space for locals
    const local_count = func.local_count;
    try self.ctx.ensureStack(local_count);
    for (0..local_count) |_| {
        try self.ctx.push(value.JSValue.undefined_val);
    }

    // Check if function is JIT-compiled and execute via JIT
    if (!jit_policy.jitDisabled() and (func.tier == .baseline or func.tier == .optimized)) {
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
            const prev_interp = interpreter.current_interpreter;
            interpreter.current_interpreter = self;
            defer interpreter.current_interpreter = prev_interp;
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
