//! Wasm policy interpreter - Phase 2 skeleton.
//!
//! Spec sections 6 (Wasm component interface / WIT ABI) and 10 (performance:
//! never instantiate per request, strict timeout, fail-closed). The real
//! implementation replaces the @panic stubs with a minimal Wasm 1.0 decoder +
//! executor that calls the policy-check export through the WIT ABI. Until then
//! every entry point fails closed with no reachable success path; the module
//! is imported so it compiles, not because any call can succeed.

const std = @import("std");
const policy = @import("../policy.zig");

pub const WasmInterpreter = struct {
    allocator: std.mem.Allocator,
    module_bytes: []const u8,

    pub const Error = error{WasmPolicyUnavailable};

    /// Load and instantiate a Wasm module from `module_bytes`. The caller owns
    /// the returned pointer and must call `deinit`. `module_bytes` must remain
    /// valid for the lifetime of the interpreter.
    pub fn init(allocator: std.mem.Allocator, module_bytes: []const u8) Error!*WasmInterpreter {
        _ = allocator;
        _ = module_bytes;
        return error.WasmPolicyUnavailable;
    }

    pub fn deinit(self: *WasmInterpreter) void {
        _ = self;
    }

    /// Invoke the `policy-check` export via the WIT ABI (spec section 6).
    /// Strict timeout and fail-closed trap handling are enforced per spec
    /// section 10. Returns `.{ .deny = ... }` on any trap or timeout.
    pub fn callPolicyCheck(self: *WasmInterpreter, input: policy.PolicyInput) Error!policy.PolicyResult {
        _ = self;
        _ = input;
        return error.WasmPolicyUnavailable;
    }
};
