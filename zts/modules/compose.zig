//! zigttp:compose - Handler composition primitives
//!
//! Exports:
//!   guard(fn) -> fn (identity, compile-time marker)
//!     Marks a function as a guard for pipe-based handler composition.
//!     The parser recognizes guard() calls in pipe chains and desugars
//!     the entire chain into a single flat function at compile time.
//!
//! guard() is a compile-time construct. It should never be called at
//! runtime because the parser replaces guard pipe chains with desugared
//! code. The native function exists only for import validation and as
//! a fallback identity if used outside a pipe chain.

const value = @import("../value.zig");
const resolver = @import("resolver.zig");
const mb = @import("../module_binding.zig");

pub const binding = mb.ModuleBinding{
    .specifier = "zigttp:compose",
    .name = "compose",
    .comptime_only = true,
    .exports = &.{
        .{ .name = "guard", .func = guardNative, .arg_count = 1,
           .effect = .none, .returns = .string, .param_types = &.{.string},
           .traceable = false },
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
