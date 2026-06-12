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
//! - If rt.deinit() itself panics during quarantine, the recovery frame is
//!   already disarmed so the process aborts via defaultPanic (fail-closed).

const std = @import("std");
const builtin = @import("builtin");

pub const c = @cImport(@cInclude("setjmp.h"));

// OS-agnostic setjmp/longjmp wrappers.
// On macOS setjmp/longjmp are real libc functions; on glibc they are macros
// that @cImport may not expose as callable symbols. Use _setjmp/_longjmp
// (no signal mask) on Linux to avoid the macro issue.
pub fn setjmpFn(env: *c.jmp_buf) c_int {
    return if (comptime builtin.os.tag == .linux)
        blk: {
            const S = struct {
                extern fn _setjmp(e: *c.jmp_buf) c_int;
            };
            break :blk S._setjmp(env);
        }
    else
        c.setjmp(env);
}

pub fn longjmpFn(env: *c.jmp_buf, val: c_int) noreturn {
    if (comptime builtin.os.tag == .linux) {
        const S = struct {
            extern fn _longjmp(e: *c.jmp_buf, v: c_int) noreturn;
        };
        S._longjmp(env, val);
    } else {
        c.longjmp(env, val);
    }
}

pub const Frame = struct {
    jb: c.jmp_buf,
    msg_buf: [256]u8 = undefined,
    msg_len: usize = 0,

    pub fn message(self: *const Frame) []const u8 {
        return self.msg_buf[0..self.msg_len];
    }
};

threadlocal var active_frame: ?*Frame = null;

/// Arm the recovery frame for the current thread. Must not be called when a
/// frame is already armed (no nested arming).
pub fn arm(f: *Frame) void {
    std.debug.assert(active_frame == null); // no nested arming
    f.msg_len = 0;
    active_frame = f;
}

/// Disarm the recovery frame for the current thread.
pub fn disarm() void {
    active_frame = null;
}

/// Root panic handler body. Wire into binary roots via:
///   pub const panic = std.debug.FullPanic(panic_recovery.handlePanic);
pub fn handlePanic(msg: []const u8, first_trace_addr: ?usize) noreturn {
    if (active_frame) |f| {
        // Disarm first: a second panic during recovery (e.g. in rt.deinit)
        // will escalate to defaultPanic and abort the process (fail-closed).
        active_frame = null;
        const n = @min(msg.len, f.msg_buf.len);
        @memcpy(f.msg_buf[0..n], msg[0..n]);
        f.msg_len = n;
        longjmpFn(&f.jb, 1);
    }
    std.debug.defaultPanic(msg, first_trace_addr);
}
