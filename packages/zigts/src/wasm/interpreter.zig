//! Wasm policy interpreter - Phase 2 skeleton.
//!
//! Spec sections 6 (Wasm component interface / WIT ABI) and 10 (performance:
//! never instantiate per request, strict timeout, fail-closed). The real
//! implementation replaces the @panic stubs with a minimal Wasm 1.0 decoder +
//! executor that calls the policy-check export through the WIT ABI.

const std = @import("std");
const policy = @import("../policy.zig");

pub const WasmInterpreter = struct {
    allocator: std.mem.Allocator,
    module_bytes: []const u8,

    /// Load and instantiate a Wasm module from `module_bytes`. The caller owns
    /// the returned pointer and must call `deinit`. `module_bytes` must remain
    /// valid for the lifetime of the interpreter.
    pub fn init(allocator: std.mem.Allocator, module_bytes: []const u8) !*WasmInterpreter {
        _ = allocator;
        _ = module_bytes;
        @panic("phase 2: not implemented");
    }

    pub fn deinit(self: *WasmInterpreter) void {
        _ = self;
        @panic("phase 2: not implemented");
    }

    /// Invoke the `policy-check` export via the WIT ABI (spec section 6).
    /// Strict timeout and fail-closed trap handling are enforced per spec
    /// section 10. Returns `.{ .deny = ... }` on any trap or timeout.
    pub fn callPolicyCheck(self: *WasmInterpreter, input: policy.PolicyInput) !policy.PolicyResult {
        _ = self;
        _ = input;
        @panic("phase 2: not implemented");
    }
};
