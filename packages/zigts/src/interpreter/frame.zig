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

const object = @import("../object.zig");
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
