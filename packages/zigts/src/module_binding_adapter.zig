const std = @import("std");
const sdk = @import("zigttp-sdk");
const internal = @import("module_binding.zig");
const object = @import("object.zig");
const value = @import("value.zig");

pub const ModuleBinding = internal.ModuleBinding;

comptime {
    if (@sizeOf(sdk.JSValue) != @sizeOf(value.JSValue))
        @compileError("zigttp-sdk.JSValue must match runtime JSValue size");
    if (@bitSizeOf(sdk.JSValue) != @bitSizeOf(value.JSValue))
        @compileError("zigttp-sdk.JSValue must match runtime JSValue bit size");
    // Enum ordinal alignment: last variant of each enum must match.
    if (@intFromEnum(sdk.EffectClass.none) != @intFromEnum(internal.EffectClass.none))
        @compileError("sdk.EffectClass ordinals diverge from internal");
    if (@intFromEnum(sdk.ReturnKind.result) != @intFromEnum(internal.ReturnKind.result))
        @compileError("sdk.ReturnKind ordinals diverge from internal");
    if (@intFromEnum(sdk.FailureSeverity.none) != @intFromEnum(internal.FailureSeverity.none))
        @compileError("sdk.FailureSeverity ordinals diverge from internal");
    if (@intFromEnum(sdk.ModuleCapability.policy_check) != @intFromEnum(internal.ModuleCapability.policy_check))
        @compileError("sdk.ModuleCapability ordinals diverge from internal");
    if (@intFromEnum(sdk.ModuleCapability.websocket) != @intFromEnum(internal.ModuleCapability.websocket))
        @compileError("sdk.ModuleCapability.websocket ordinal diverges from internal");
    if (@intFromEnum(sdk.ContractCategory.fetch_host) != @intFromEnum(internal.ContractCategory.fetch_host))
        @compileError("sdk.ContractCategory ordinals diverge from internal");
    if (@intFromEnum(sdk.LawKind.absorbing) != @intFromEnum(internal.LawKind.absorbing))
        @compileError("sdk.LawKind ordinals diverge from internal");
}

pub fn adaptModuleBinding(comptime binding: sdk.ModuleBinding) internal.ModuleBinding {
    const builtin_prefix = std.mem.startsWith(u8, binding.specifier, "zigttp:");
    const extension_prefix = std.mem.startsWith(u8, binding.specifier, "zigttp-ext:");
    if (!builtin_prefix and !extension_prefix) {
        @compileError("adapted module specifier must start with 'zigttp:' or 'zigttp-ext:': " ++ binding.specifier);
    }

    const exports = comptime adaptFunctionBindings(binding.specifier, binding.required_capabilities, binding.exports);
    const required_capabilities = comptime adaptModuleCapabilities(binding.required_capabilities);
    return .{
        .specifier = binding.specifier,
        .name = binding.name,
        .exports = &exports,
        .required_capabilities = &required_capabilities,
        .stateful = binding.stateful,
        .state_init = binding.state_init,
        .state_deinit = binding.state_deinit,
        .contract_section = binding.contract_section,
        .sandboxable = binding.sandboxable,
        .comptime_only = binding.comptime_only,
        .self_managed_io = binding.self_managed_io,
    };
}

fn adaptFunctionBindings(
    comptime specifier: []const u8,
    comptime required_capabilities: []const sdk.ModuleCapability,
    comptime exports: []const sdk.FunctionBinding,
) [exports.len]internal.FunctionBinding {
    var out: [exports.len]internal.FunctionBinding = undefined;
    for (exports, 0..) |binding, i| {
        out[i] = adaptFunctionBinding(specifier, required_capabilities, binding);
    }
    return out;
}

fn adaptModuleCapabilities(comptime capabilities: []const sdk.ModuleCapability) [capabilities.len]internal.ModuleCapability {
    var out: [capabilities.len]internal.ModuleCapability = undefined;
    for (capabilities, 0..) |capability, i| {
        out[i] = @enumFromInt(@intFromEnum(capability));
    }
    return out;
}

fn adaptFunctionBinding(
    comptime specifier: []const u8,
    comptime required_capabilities: []const sdk.ModuleCapability,
    comptime binding: sdk.FunctionBinding,
) internal.FunctionBinding {
    const param_types = comptime adaptReturnKinds(binding.param_types);
    const contract_extractions = comptime adaptContractExtractions(binding.contract_extractions);
    const laws = comptime adaptLaws(binding.laws);
    const internal_required_capabilities = comptime adaptModuleCapabilities(required_capabilities);
    return .{
        .name = binding.name,
        .func = wrapToNativeFn(binding.module_func, specifier, &internal_required_capabilities),
        .arg_count = binding.arg_count,
        .effect = @enumFromInt(@intFromEnum(binding.effect)),
        .returns = @enumFromInt(@intFromEnum(binding.returns)),
        .param_types = &param_types,
        .traceable = binding.traceable,
        .contract_extractions = &contract_extractions,
        .contract_flags = .{
            .sets_scope_used = binding.contract_flags.sets_scope_used,
            .sets_durable_used = binding.contract_flags.sets_durable_used,
            .sets_durable_timers = binding.contract_flags.sets_durable_timers,
            .sets_bearer_auth = binding.contract_flags.sets_bearer_auth,
            .sets_jwt_auth = binding.contract_flags.sets_jwt_auth,
        },
        .return_labels = @bitCast(binding.return_labels),
        .failure_severity = @enumFromInt(@intFromEnum(binding.failure_severity)),
        .laws = &laws,
    };
}

fn adaptLaws(comptime laws: []const sdk.Law) [laws.len]internal.Law {
    var out: [laws.len]internal.Law = undefined;
    for (laws, 0..) |law, i| {
        out[i] = switch (law) {
            .pure => .pure,
            .idempotent_call => .idempotent_call,
            .inverse_of => |target| .{ .inverse_of = target },
            .absorbing => |pattern| .{ .absorbing = .{
                .arg_position = pattern.arg_position,
                .argument_shape = @enumFromInt(@intFromEnum(pattern.argument_shape)),
                .residue = @enumFromInt(@intFromEnum(pattern.residue)),
            } },
        };
    }
    return out;
}

fn adaptReturnKinds(comptime kinds: []const sdk.ReturnKind) [kinds.len]internal.ReturnKind {
    var out: [kinds.len]internal.ReturnKind = undefined;
    for (kinds, 0..) |kind, i| {
        out[i] = @enumFromInt(@intFromEnum(kind));
    }
    return out;
}

fn adaptContractExtractions(comptime extractions: []const sdk.ContractExtraction) [extractions.len]internal.ContractExtraction {
    var out: [extractions.len]internal.ContractExtraction = undefined;
    for (extractions, 0..) |extraction, i| {
        out[i] = .{
            .arg_position = extraction.arg_position,
            .category = @enumFromInt(@intFromEnum(extraction.category)),
            .transform = if (extraction.transform) |transform|
                @enumFromInt(@intFromEnum(transform))
            else
                null,
            .flag_only = extraction.flag_only,
        };
    }
    return out;
}

/// Produce a NativeFn directly from a sdk.ModuleFn, avoiding double-wrapping.
/// Both JSValue types are bit-identical packed structs, so args are aliased
/// via @ptrCast with zero per-element copy.
fn wrapToNativeFn(
    comptime module_fn: sdk.ModuleFn,
    comptime specifier: []const u8,
    comptime required_capabilities: []const internal.ModuleCapability,
) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            const prev = internal.pushActiveModuleContext(specifier, required_capabilities);
            defer internal.popActiveModuleContext(prev);

            const sdk_args: []const sdk.JSValue = @ptrCast(args);
            const result = try module_fn(
                @ptrCast(ctx_ptr),
                @bitCast(this),
                sdk_args,
            );
            return @bitCast(result);
        }
    }.call;
}

test "adaptModuleBinding preserves required capabilities" {
    const binding = sdk.ModuleBinding{
        .specifier = "zigttp-ext:test",
        .name = "test",
        .required_capabilities = &.{ .clock, .runtime_callback },
        .exports = &.{
            .{
                .name = "noop",
                .module_func = struct {
                    fn f(_: *sdk.ModuleHandle, _: sdk.JSValue, _: []const sdk.JSValue) anyerror!sdk.JSValue {
                        return sdk.JSValue.undefined_val;
                    }
                }.f,
                .arg_count = 0,
            },
        },
    };

    const adapted = comptime adaptModuleBinding(binding);
    try std.testing.expectEqual(@as(usize, 2), adapted.required_capabilities.len);
    try std.testing.expectEqual(internal.ModuleCapability.clock, adapted.required_capabilities[0]);
    try std.testing.expectEqual(internal.ModuleCapability.runtime_callback, adapted.required_capabilities[1]);
}
