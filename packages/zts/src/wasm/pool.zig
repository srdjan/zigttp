//! Pooled Wasm interpreter instances - Phase 2 skeleton.
//!
//! Spec section 10: "Never instantiate per request." This wraps the zts
//! LockFreePool to hold pre-instantiated WasmInterpreters so the hot path
//! only acquires from the pool rather than paying Wasm instantiation cost.
//! Skeleton: every entry currently returns error.WasmPolicyUnavailable.

const std = @import("std");
const WasmInterpreter = @import("interpreter.zig").WasmInterpreter;

pub const WasmPool = struct {
    allocator: std.mem.Allocator,

    pub const Error = error{WasmPolicyUnavailable};

    pub fn init(allocator: std.mem.Allocator, module_bytes: []const u8, pool_size: usize) Error!WasmPool {
        _ = allocator;
        _ = module_bytes;
        _ = pool_size;
        return error.WasmPolicyUnavailable;
    }

    pub fn deinit(self: *WasmPool) void {
        _ = self;
    }

    /// Borrow a pre-instantiated interpreter. Caller must return it via
    /// `release` to avoid starving concurrent requests.
    pub fn acquire(self: *WasmPool) Error!*WasmInterpreter {
        _ = self;
        return error.WasmPolicyUnavailable;
    }

    pub fn release(self: *WasmPool, interp: *WasmInterpreter) void {
        _ = self;
        _ = interp;
    }
};
