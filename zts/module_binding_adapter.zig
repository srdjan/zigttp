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
}

pub fn adaptModuleBinding(comptime binding: sdk.ModuleBinding) internal.ModuleBinding {
    if (!std.mem.startsWith(u8, binding.specifier, "zigttp-ext:")) {
        @compileError("extension binding specifier must start with 'zigttp-ext:': " ++ binding.specifier);
    }

    const exports = comptime adaptFunctionBindings(binding.exports);
    return .{
        .specifier = binding.specifier,
        .name = binding.name,
        .exports = &exports,
        .stateful = binding.stateful,
        .state_init = binding.state_init,
        .state_deinit = binding.state_deinit,
        .contract_section = binding.contract_section,
        .sandboxable = binding.sandboxable,
        .comptime_only = binding.comptime_only,
    };
}

fn adaptFunctionBindings(comptime exports: []const sdk.FunctionBinding) [exports.len]internal.FunctionBinding {
    var out: [exports.len]internal.FunctionBinding = undefined;
    for (exports, 0..) |binding, i| {
        out[i] = adaptFunctionBinding(binding);
    }
    return out;
}

fn adaptFunctionBinding(comptime binding: sdk.FunctionBinding) internal.FunctionBinding {
    const param_types = comptime adaptReturnKinds(binding.param_types);
    const contract_extractions = comptime adaptContractExtractions(binding.contract_extractions);
    return .{
        .name = binding.name,
        .func = wrapToNativeFn(binding.module_func),
        .arg_count = binding.arg_count,
        .effect = @enumFromInt(@intFromEnum(binding.effect)),
        .returns = @enumFromInt(@intFromEnum(binding.returns)),
        .param_types = &param_types,
        .traceable = binding.traceable,
        .contract_extractions = &contract_extractions,
        .contract_flags = .{
            .sets_durable_used = binding.contract_flags.sets_durable_used,
            .sets_durable_timers = binding.contract_flags.sets_durable_timers,
            .sets_bearer_auth = binding.contract_flags.sets_bearer_auth,
            .sets_jwt_auth = binding.contract_flags.sets_jwt_auth,
        },
        .return_labels = @bitCast(binding.return_labels),
        .failure_severity = @enumFromInt(@intFromEnum(binding.failure_severity)),
    };
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
fn wrapToNativeFn(comptime module_fn: sdk.ModuleFn) object.NativeFn {
    return struct {
        fn call(ctx_ptr: *anyopaque, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
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
