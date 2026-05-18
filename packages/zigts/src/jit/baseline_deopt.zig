//! Shared deopt types and the baseline-JIT signature ABI. Kept separate
//! from baseline.zig so deopt.zig can reference these without inducing
//! a cycle through the full compiler module.

pub const CompileError = error{
    UnsupportedOpcode,
    CodeTooLarge,
    AllocationFailed,
    OutOfMemory,
    OffsetTooLarge,
    UnalignedBranch,
};

/// Compiled function entry point signature: takes context pointer, returns
/// a JS value packed into u64.
pub const CompiledFn = *const fn (ctx: *anyopaque) callconv(.c) u64;

/// Reason a type-guard fails and the interpreter must resume.
pub const DeoptReason = enum(u8) {
    type_mismatch,
    hidden_class_mismatch,
    overflow,
    callee_changed,
    bool_expected,
};

/// Native -> bytecode resume record emitted during compilation.
pub const DeoptPoint = struct {
    native_offset: u32,
    bytecode_offset: u32,
    reason: DeoptReason,
};
