const sdk = @import("zigttp-sdk");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp-ext:math",
    .name = "ext_math",
    .exports = &.{
        .{
            .name = "double",
            .module_func = doubleNative,
            .arg_count = 1,
            .effect = .read,
            .returns = .number,
            .param_types = &.{.number},
            .traceable = true,
        },
        .{
            .name = "isEven",
            .module_func = isEvenNative,
            .arg_count = 1,
            .effect = .read,
            .returns = .boolean,
            .param_types = &.{.number},
            .traceable = true,
        },
    },
};

comptime {
    sdk.validateBindings(&.{binding});
}

fn doubleNative(_: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    const n = if (args.len > 0) args[0].toInt() orelse 0 else 0;
    return sdk.JSValue.fromInt(n * 2);
}

fn isEvenNative(_: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    const n = if (args.len > 0) args[0].toInt() orelse 0 else 0;
    return sdk.JSValue.fromBool(@mod(n, 2) == 0);
}
