//! zigttp:env - Environment variable access

const std = @import("std");
const sdk = @import("zigttp-sdk");
const util = @import("../internal/util.zig");

pub const binding = sdk.ModuleBinding{
    .specifier = "zigttp:env",
    .name = "env",
    .required_capabilities = &.{ .env, .policy_check },
    .contract_section = "env",
    .sandboxable = true,
    .exports = &.{
        .{
            .name = "env",
            .module_func = envImpl,
            .arg_count = 1,
            .returns = .optional_string,
            .param_types = &.{.string},
            .failure_severity = .expected,
            .contract_extractions = &.{.{ .category = .env }},
            .return_labels = .{ .secret = true },
            .laws = &.{.pure},
        },
    },
};

fn envImpl(handle: *sdk.ModuleHandle, _: sdk.JSValue, args: []const sdk.JSValue) anyerror!sdk.JSValue {
    if (args.len == 0) return sdk.JSValue.undefined_val;
    const name = sdk.extractString(args[0]) orelse return sdk.JSValue.undefined_val;

    if (!sdk.allowsEnv(handle, name)) {
        return util.throwCapabilityPolicyError(handle, "env access", name);
    }

    const raw = sdk.readEnv(handle, name) orelse return sdk.JSValue.undefined_val;
    return sdk.createString(handle, raw) catch sdk.JSValue.undefined_val;
}
