//! JIT Compilation Module
//!
//! Provides just-in-time compilation for frequently executed JavaScript code.
//! Phase 11 of the zts performance enhancement plan.
//!
//! Components:
//! - x86.zig: x86-64 instruction emitter
//! - arm64.zig: ARM64/AArch64 instruction emitter
//! - alloc.zig: Executable memory allocator
//! - baseline.zig: Baseline compiler (bytecode -> native, no optimization)

pub const x86 = @import("x86.zig");
pub const arm64 = @import("arm64.zig");
pub const alloc = @import("alloc.zig");
pub const baseline = @import("baseline.zig");

// Re-export common types (architecture-specific via baseline)
pub const CodeAllocator = alloc.CodeAllocator;
pub const CodePage = alloc.CodePage;
pub const CompiledCode = alloc.CompiledCode;
pub const BaselineCompiler = baseline.BaselineCompiler;
pub const compileFunction = baseline.compileFunction;

// Architecture detection
pub const is_x86_64 = baseline.is_x86_64;
pub const is_aarch64 = baseline.is_aarch64;
pub const Emitter = baseline.Emitter;
pub const Register = baseline.Register;

test {
    _ = x86;
    _ = arm64;
    _ = alloc;
    _ = baseline;
}
