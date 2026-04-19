//! zigttp:compose - handler composition primitives.
//!
//! Exports:
//!   guard(fn) -> fn  (identity at runtime; parser-desugared at compile time)
//!   pipe(f1, ..., fN) -> result  (parser-desugared into nested calls)
//!
//! Both are compile-time constructs. The parser replaces all usage; the
//! native fns exist only so `import` resolves.

const sdk = @import("zigttp-sdk");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:compose",
    .name = "compose",
    .comptime_only = true,
    .exports = &.{
        .{
            .name = "guard",
            .module_func = guardImpl,
            .arg_count = 1,
            .effect = .none,
            .returns = .string,
            .param_types = &.{.string},
            .traceable = false,
        },
        .{
            .name = "pipe",
            .module_func = pipeImpl,
            .arg_count = 0,
            .effect = .none,
            .returns = .unknown,
            .param_types = &.{},
            .traceable = false,
        },
    },
};

fn guardImpl(_: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    return args[0];
}

fn pipeImpl(_: *sdk.ModuleHandle, _: sdk.JSValue, _: []const sdk.JSValue) anyerror!sdk.JSValue {
    return sdk.JSValue.undefined_val;
}
