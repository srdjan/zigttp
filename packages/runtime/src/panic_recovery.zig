//! Per-thread setjmp/longjmp recovery for handler panics.
//!
//! Armed only around handler execution in runtime_pool.zig; panics anywhere
//! else (startup, allocation, compile paths) fall through to
//! std.debug.defaultPanic and remain fatal.
//!
//! KNOWN LIMITATIONS:
//! - Recovery is effective in Debug and ReleaseSafe builds. In ReleaseFast,
//!   safety checks compile out and become UB rather than catchable panics.
//!   Explicit @panic() and std.debug.panic() sites are caught in all modes.
//! - Native stack overflow and SIGSEGV are signals, not panics - out of scope.
//! - If pool.trace_mutex is held when a panic fires inside the trace recorder,
//!   subsequent trace writes will deadlock. Window is microseconds; acceptable.
//! - If rt.deinit() itself panics during quarantine, the current recovery
//!   frame is already popped, so the panic unwinds to the enclosing frame (a
//!   nested in-process dispatch) or aborts via defaultPanic when none remains
//!   (fail-closed).
//!
//! NESTING: frames form a per-thread stack. An in-process sub-handler dispatch
//! (zigttp:workflow.call) arms its own frame on top of the orchestrator's, so a
//! sub-handler panic longjmps to the sub frame and surfaces as
//! error.HandlerPanicked to the orchestrator, leaving the orchestrator's outer
//! frame armed. Without the stack, nested arming would assert/abort.

const std = @import("std");
const builtin = @import("builtin");

pub const c = @cImport(@cInclude("setjmp.h"));

const linux = struct {
    extern fn _setjmp(e: *c.jmp_buf) c_int;
    extern fn _longjmp(e: *c.jmp_buf, v: c_int) noreturn;
};

// OS-agnostic setjmp/longjmp entry points.
// On macOS setjmp/longjmp are real libc functions; on glibc they are macros
// that @cImport may not expose as callable symbols. `setjmpFn` must be a
// direct alias, not a Zig wrapper: longjmp must resume in callHandlerGuarded's
// frame, not in a helper frame that returned before the panic.
pub const setjmpFn = if (builtin.os.tag == .linux) linux._setjmp else c.setjmp;

pub fn longjmpFn(env: *c.jmp_buf, val: c_int) noreturn {
    if (comptime builtin.os.tag == .linux) {
        linux._longjmp(env, val);
    } else {
        c.longjmp(env, val);
    }
}

pub const Frame = struct {
    jb: c.jmp_buf,
    msg_buf: [256]u8 = undefined,
    msg_len: usize = 0,
    /// Enclosing frame, if this one was armed inside another guarded handler
    /// (nested in-process dispatch). Restored on disarm / panic-pop.
    prev: ?*Frame = null,

    pub fn message(self: *const Frame) []const u8 {
        return self.msg_buf[0..self.msg_len];
    }
};

threadlocal var active_frame: ?*Frame = null;

/// Arm a recovery frame for the current thread, pushing it onto the per-thread
/// stack. Nested arming is supported: an orchestrator handler can dispatch a
/// co-located sub-handler in-process (zigttp:workflow.call), and the sub-call
/// arms its own frame on top of the orchestrator's.
pub fn arm(f: *Frame) void {
    f.msg_len = 0;
    f.prev = active_frame;
    active_frame = f;
}

/// Disarm the current recovery frame (normal-completion path), restoring the
/// enclosing frame if any. The panic path pops inside `handlePanic` instead,
/// because longjmp skips this `defer`.
pub fn disarm() void {
    if (active_frame) |f| active_frame = f.prev;
}

/// Root panic handler body. Wire into binary roots via:
///   pub const panic = std.debug.FullPanic(panic_recovery.handlePanic);
pub fn handlePanic(msg: []const u8, first_trace_addr: ?usize) noreturn {
    if (active_frame) |f| {
        // Pop to the enclosing frame first: a second panic during recovery
        // (e.g. in rt.deinit) unwinds to that frame, or escalates to
        // defaultPanic when the stack is empty (fail-closed).
        active_frame = f.prev;
        const n = @min(msg.len, f.msg_buf.len);
        @memcpy(f.msg_buf[0..n], msg[0..n]);
        f.msg_len = n;
        longjmpFn(&f.jb, 1);
    }
    std.debug.defaultPanic(msg, first_trace_addr);
}
