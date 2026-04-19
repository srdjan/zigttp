//! zigttp:compose - Handler composition primitives
//!
//! Exports:
//!   guard(fn) -> fn (identity, compile-time marker)
//!     Marks a function as a guard for pipe-based handler composition.
//!     The parser recognizes guard() calls in pipe chains and desugars
//!     the entire chain into a single flat function at compile time.
//!
//!   pipe(f1, f2, ..., fN) -> result (compile-time desugaring)
//!     Chains functions sequentially: calls f1() with no args, passes
//!     result to f2, then to f3, etc. Desugared at parse time into
//!     nested calls: fN(...(f2(f1()))...).
//!
//! Both are compile-time constructs. The parser replaces all usage with
//! desugared code. Native functions exist only for import validation.

const value = @import("../../value.zig");
const resolver = @import("../internal/resolver.zig");
const mb = @import("../../module_binding.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:compose",
    .name = "compose",
    .comptime_only = true,
    .exports = &.{
        .{ .name = "guard", .func = guardNative, .arg_count = 1, .effect = .none, .returns = .string, .param_types = &.{.string}, .traceable = false },
        .{ .name = "pipe", .func = pipeNative, .arg_count = 0, .effect = .none, .returns = .unknown, .param_types = &.{}, .traceable = false },
    },
};

pub const exports = binding.toModuleExports();

/// guard(fn) - compile-time marker for handler composition.
/// At runtime this is an identity function (returns its argument).
/// The parser desugars guard() pipe chains before this is ever called.
fn guardNative(_: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
    if (args.len == 0) return value.JSValue.undefined_val;
    return args[0];
}

/// pipe(f1, f2, ..., fN) - compile-time function chaining.
/// The parser desugars pipe() calls into nested function calls.
/// This fallback returns undefined and should never be reached.
fn pipeNative(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
    return value.JSValue.undefined_val;
}
