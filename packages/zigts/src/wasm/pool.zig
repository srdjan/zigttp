//! Pooled Wasm interpreter instances - Phase 2 skeleton.
//!
//! Spec section 10: "Never instantiate per request." This wraps the zigts
//! LockFreePool to hold pre-instantiated WasmInterpreters so the hot path
//! only acquires from the pool rather than paying Wasm instantiation cost.

const std = @import("std");
const WasmInterpreter = @import("interpreter.zig").WasmInterpreter;

pub const WasmPool = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, module_bytes: []const u8, pool_size: usize) !WasmPool {
        _ = allocator;
        _ = module_bytes;
        _ = pool_size;
        @panic("phase 2: not implemented");
    }

    pub fn deinit(self: *WasmPool) void {
        _ = self;
        @panic("phase 2: not implemented");
    }

    /// Borrow a pre-instantiated interpreter. Caller must return it via
    /// `release` to avoid starving concurrent requests.
    pub fn acquire(self: *WasmPool) !*WasmInterpreter {
        _ = self;
        @panic("phase 2: not implemented");
    }

    pub fn release(self: *WasmPool, interp: *WasmInterpreter) void {
        _ = self;
        _ = interp;
        @panic("phase 2: not implemented");
    }
};
