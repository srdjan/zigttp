//! JIT compilation policy: env-driven knobs (ZTS_JIT_POLICY, ZTS_JIT_THRESHOLD,
//! ZTS_JIT_FEEDBACK_WARMUP, ZTS_DISABLE_JIT, ZTS_TIERING_DEOPT_SUPPRESS) plus
//! programmatic overrides used by tests.

const std = @import("std");
const bytecode = @import("../bytecode.zig");
const env_cache = @import("env_cache.zig");

/// JIT compilation policy for FaaS-aware optimization.
pub const JitPolicy = enum {
    /// Never JIT compile - pure interpreter mode (fastest cold start)
    disabled,
    /// Default: JIT after threshold (balanced)
    lazy,
    /// Lower threshold for faster warmup (more aggressive)
    eager,
};

var jit_policy_cache: ?JitPolicy = null;
var jit_threshold_cache: ?u32 = null;
var jit_feedback_warmup_cache: ?u32 = null;
var jit_disabled_cache: ?bool = null;
var tiering_deopt_suppress_cache: ?bool = null;

/// Get current JIT policy (cached, reads ZTS_JIT_POLICY env on first call)
pub fn getJitPolicy() JitPolicy {
    if (jit_policy_cache) |cached| return cached;
    if (std.c.getenv("ZTS_JIT_POLICY")) |policy_ptr| {
        const policy_str = std.mem.sliceTo(policy_ptr, 0);
        const policy = std.meta.stringToEnum(JitPolicy, policy_str) orelse .lazy;
        jit_policy_cache = policy;
        return policy;
    }
    jit_policy_cache = .lazy;
    return .lazy;
}

/// Get current JIT threshold (cached, reads ZTS_JIT_THRESHOLD env on first call)
pub fn getJitThreshold() u32 {
    if (jit_threshold_cache) |cached| return cached;
    if (std.c.getenv("ZTS_JIT_THRESHOLD")) |threshold_ptr| {
        const threshold_str = std.mem.sliceTo(threshold_ptr, 0);
        const threshold = std.fmt.parseInt(u32, threshold_str, 10) catch bytecode.JIT_THRESHOLD;
        jit_threshold_cache = threshold;
        return threshold;
    }
    const policy = getJitPolicy();
    const threshold = switch (policy) {
        .disabled => std.math.maxInt(u32),
        .lazy => bytecode.JIT_THRESHOLD,
        .eager => bytecode.JIT_THRESHOLD / 4,
    };
    jit_threshold_cache = threshold;
    return threshold;
}

/// Get warmup calls for type feedback collection before JIT compilation.
/// Can be overridden by ZTS_JIT_FEEDBACK_WARMUP.
pub fn getJitFeedbackWarmup() u32 {
    if (jit_feedback_warmup_cache) |cached| return cached;
    if (std.c.getenv("ZTS_JIT_FEEDBACK_WARMUP")) |warmup_ptr| {
        const warmup_str = std.mem.sliceTo(warmup_ptr, 0);
        const warmup = std.fmt.parseInt(u32, warmup_str, 10) catch 50;
        jit_feedback_warmup_cache = warmup;
        return warmup;
    }
    const policy = getJitPolicy();
    const warmup: u32 = switch (policy) {
        .disabled => @intCast(std.math.maxInt(u32)),
        .lazy => 50,
        .eager => 10,
    };
    jit_feedback_warmup_cache = warmup;
    return warmup;
}

/// Set JIT policy programmatically (overrides env var)
pub fn setJitPolicy(policy: JitPolicy) void {
    jit_policy_cache = policy;
    jit_threshold_cache = null;
    jit_feedback_warmup_cache = null;
}

/// Set JIT threshold programmatically (overrides env var and policy default)
pub fn setJitThreshold(threshold: u32) void {
    jit_threshold_cache = threshold;
}

/// Set feedback warmup count programmatically (overrides env var and policy default).
pub fn setJitFeedbackWarmup(warmup: u32) void {
    jit_feedback_warmup_cache = warmup;
}

/// Force-disable JIT in the current process (used by tests that exercise
/// multithreaded runtime behavior without JIT stability guarantees yet).
pub fn disableJitForTests() void {
    jit_disabled_cache = true;
    jit_policy_cache = .disabled;
}

pub fn jitDisabled() bool {
    if (getJitPolicy() == .disabled) return true;
    return env_cache.cachedBoolPresent("ZTS_DISABLE_JIT", &jit_disabled_cache);
}

/// When ZTS_TIERING_DEOPT_SUPPRESS is set, functions that recently deopted
/// repeatedly are denied promotion to optimized_candidate for a cooldown
/// window. Defaults to off until field data confirms the heuristic.
pub fn isTieringDeoptSuppressEnabled() bool {
    return env_cache.cachedBoolPresent("ZTS_TIERING_DEOPT_SUPPRESS", &tiering_deopt_suppress_cache);
}

/// Reset the cached tiering suppression env read; tests use this to toggle
/// behavior without restarting the process.
pub fn resetTieringDeoptSuppressCache() void {
    tiering_deopt_suppress_cache = null;
}

/// Test-only override of the cached tiering suppression value. Pass null to
/// clear so the next call re-reads the env var.
pub fn setTieringDeoptSuppressForTests(value: ?bool) void {
    tiering_deopt_suppress_cache = value;
}
