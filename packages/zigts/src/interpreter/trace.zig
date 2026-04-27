//! Diagnostic call/type tracing for the interpreter.
//!
//! Env vars: `ZTS_TRACE_CALLS` (master switch), `ZTS_TRACE_CALLS_LIMIT`
//! (per-thread call-event budget), `ZTS_CALL_GUARD` (max call depth before
//! guard logging), `ZTS_TRACE_BC_FULL` (dump full bytecode window instead
//! of a +/-12 op slice). `call_trace_count` is threadlocal so per-worker
//! budgets don't bleed across pooled handler runs.

const std = @import("std");
const value = @import("../value.zig");
const bytecode = @import("../bytecode.zig");
const env_cache = @import("env_cache.zig");
const interpreter = @import("../interpreter.zig");
const Interpreter = interpreter.Interpreter;

var call_trace_cache: ?bool = null;
var call_trace_limit_cache: ?usize = null;
var call_guard_cache: ?usize = null;
var trace_bc_full_cache: ?bool = null;
threadlocal var call_trace_count: usize = 0;

pub inline fn callTraceEnabled() bool {
    return env_cache.cachedBoolPresent("ZTS_TRACE_CALLS", &call_trace_cache);
}

pub fn callTraceLimit() usize {
    return env_cache.cachedUsizeNonzero("ZTS_TRACE_CALLS_LIMIT", &call_trace_limit_cache, 200);
}

pub fn callGuardDepth() usize {
    return env_cache.cachedUsize("ZTS_CALL_GUARD", &call_guard_cache, 0);
}

pub fn traceCall(self: *Interpreter, label: []const u8, argc: u8, is_method: bool) void {
    if (!callTraceEnabled()) return;
    const limit = callTraceLimit();
    if (call_trace_count >= limit) return;
    call_trace_count += 1;
    std.debug.print(
        "[call] {s} depth={} sp={} fp={} argc={} method={}\n",
        .{ label, self.ctx.call_depth, self.ctx.sp, self.ctx.fp, argc, @intFromBool(is_method) },
    );
}

pub fn traceTypeError(self: *Interpreter, label: []const u8, a: value.JSValue, b: value.JSValue) void {
    if (!callTraceEnabled()) return;
    std.debug.print(
        "[typeerror] {s} a_type={s} a={} b_type={s} b={} depth={} sp={} fp={}\n",
        .{ label, a.typeOf(), a, b.typeOf(), b, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
    );
    if (self.current_func) |cur| {
        const pc_off = @as(usize, @intCast(@intFromPtr(self.pc) - @intFromPtr(cur.code.ptr)));
        std.debug.print("[typeerror] pc_off={} last_op={s}\n", .{ pc_off, @tagName(self.last_op) });
        const op_off = if (pc_off > 0) pc_off - 1 else 0;
        traceBytecodeWindow(self, op_off);
    }
}

pub fn traceLastOp(self: *Interpreter, label: []const u8) void {
    if (!callTraceEnabled()) return;
    if (self.current_func) |cur| {
        const pc_off = @as(usize, @intCast(@intFromPtr(self.pc) - @intFromPtr(cur.code.ptr)));
        std.debug.print(
            "[typeerror] {s} op={s} pc_off={} depth={} sp={} fp={}\n",
            .{ label, @tagName(self.last_op), pc_off, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
        );
        const op_off = if (pc_off > 0) pc_off - 1 else 0;
        traceBytecodeWindow(self, op_off);
    } else {
        std.debug.print(
            "[typeerror] {s} op={s} depth={} sp={} fp={}\n",
            .{ label, @tagName(self.last_op), self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
        );
    }
}

pub fn traceBytecodeWindow(self: *Interpreter, center_off: usize) void {
    if (!callTraceEnabled()) return;
    const cur = self.current_func orelse return;
    const code = cur.code;
    if (code.len == 0) return;
    const full = env_cache.cachedBoolPresent("ZTS_TRACE_BC_FULL", &trace_bc_full_cache);
    const window: usize = 12;
    const start = if (full) 0 else if (center_off > window) center_off - window else 0;
    const end = if (full) code.len else @min(code.len, center_off + window);
    var pos: usize = start;
    while (pos < end) {
        const op: bytecode.Opcode = @enumFromInt(code[pos]);
        const info = bytecode.getOpcodeInfo(op);
        std.debug.print("[bytecode] +{} {s}\n", .{ pos, @tagName(op) });
        if (info.size == 0) break;
        pos += info.size;
    }
}
