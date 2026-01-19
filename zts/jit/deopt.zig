//! Deoptimization Support for JIT
//!
//! Handles transitions from JIT-compiled code back to the interpreter when
//! speculative assumptions (type guards) are violated at runtime.
//!
//! Deoptimization involves:
//! 1. Saving the JIT execution state (bytecode offset, stack state)
//! 2. Restoring interpreter state to continue from the deopt point
//! 3. Marking the function for potential recompilation with updated feedback
//!
//! This module provides the runtime support functions called by JIT guard failures.

const std = @import("std");
const bytecode = @import("../bytecode.zig");
const value = @import("../value.zig");
const context = @import("../context.zig");
const interpreter_mod = @import("../interpreter.zig");
const baseline = @import("baseline.zig");

/// Deoptimization state passed from JIT to interpreter
pub const DeoptState = struct {
    /// Bytecode offset where deopt occurred (resume point for interpreter)
    bytecode_offset: u32,
    /// Reason for deoptimization (for profiling/debugging)
    reason: baseline.DeoptReason,
    /// Function that was being executed
    func: *bytecode.FunctionBytecode,
};

/// Statistics for deoptimization events (debugging/profiling)
pub const DeoptStats = struct {
    total_deopts: u64 = 0,
    type_mismatch_count: u64 = 0,
    hidden_class_mismatch_count: u64 = 0,
    overflow_count: u64 = 0,
    callee_changed_count: u64 = 0,

    pub fn record(self: *DeoptStats, reason: baseline.DeoptReason) void {
        self.total_deopts += 1;
        switch (reason) {
            .type_mismatch => self.type_mismatch_count += 1,
            .hidden_class_mismatch => self.hidden_class_mismatch_count += 1,
            .overflow => self.overflow_count += 1,
            .callee_changed => self.callee_changed_count += 1,
        }
    }
};

/// Thread-local deopt statistics
threadlocal var deopt_stats: DeoptStats = .{};

/// Get deopt statistics for the current thread
pub fn getDeoptStats() *DeoptStats {
    return &deopt_stats;
}

/// Reset deopt statistics
pub fn resetDeoptStats() void {
    deopt_stats = .{};
}

/// Called from JIT code when a type guard fails.
///
/// This function handles the transition from JIT execution back to interpreted mode.
/// It restores the interpreter state and continues execution from the deopt point.
///
/// Arguments:
///   - ctx: The execution context
///   - bytecode_offset: Where in the bytecode to resume
///   - reason: Why deoptimization occurred
///
/// Returns: The result of continuing execution in the interpreter, or an error sentinel.
///
/// Note: This implementation resumes execution in the interpreter at the
/// provided bytecode offset and returns the final result. It relies on JIT
/// code calling this before mutating the operand stack for the current op.
pub export fn jitDeoptimize(ctx: *context.Context, bytecode_offset: u32, reason: u32) u64 {
    // Record stats
    const deopt_reason: baseline.DeoptReason = @enumFromInt(@as(u8, @truncate(reason)));
    deopt_stats.record(deopt_reason);

    // Get the interpreter from context
    const interp_ptr = ctx.jit_interpreter orelse {
        // No interpreter available - return undefined as fallback
        return value.JSValue.undefined_val.raw;
    };
    const interp: *interpreter_mod.Interpreter = @ptrCast(@alignCast(interp_ptr));

    // Get current function
    const func = interp.current_func orelse {
        return value.JSValue.undefined_val.raw;
    };
    std.log.debug("jit deopt: reason={} offset={d} tier={}", .{ deopt_reason, bytecode_offset, func.tier });

    // Mark function for recompilation with updated type feedback
    // The next time this function is called, it will be re-JITted with
    // the new feedback which may produce different (more generic) code
    if (func.tier == .baseline) {
        // Demote to candidate so it gets recompiled on next hot call
        @constCast(func).tier = .baseline_candidate;
    }

    // Restore interpreter state and continue from the deopt point
    interp.pc = func.code.ptr + bytecode_offset;
    interp.code_end = func.code.ptr + func.code.len;
    interp.constants = func.constants;
    interp.current_func = func;

    const result = interp.dispatch() catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val.raw;
    };

    return result.raw;
}

/// Check if a function should be recompiled after deoptimization.
///
/// Heuristic: recompile if the function has deoptimized fewer than N times
/// and has been called enough times to warrant recompilation.
pub fn shouldRecompile(func: *const bytecode.FunctionBytecode) bool {
    // Don't recompile if still interpreted
    if (func.tier == .interpreted) return false;

    // Don't recompile if already at highest tier
    if (func.tier == .optimized) return false;

    // Require enough executions to justify recompilation
    if (func.execution_count < bytecode.JIT_THRESHOLD * 2) return false;

    // Could add deopt count tracking per-function to limit recompilation attempts
    return true;
}

// ============================================================================
// Unit Tests
// ============================================================================

test "DeoptStats recording" {
    var stats = DeoptStats{};

    stats.record(.type_mismatch);
    stats.record(.overflow);
    stats.record(.type_mismatch);

    try std.testing.expectEqual(@as(u64, 3), stats.total_deopts);
    try std.testing.expectEqual(@as(u64, 2), stats.type_mismatch_count);
    try std.testing.expectEqual(@as(u64, 1), stats.overflow_count);
    try std.testing.expectEqual(@as(u64, 0), stats.hidden_class_mismatch_count);
}

test "DeoptStats reset" {
    var stats = DeoptStats{};
    stats.record(.overflow);
    stats.total_deopts = 100;

    stats = .{};

    try std.testing.expectEqual(@as(u64, 0), stats.total_deopts);
}
