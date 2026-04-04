const std = @import("std");
const sdk = @import("zigttp-sdk");
const internal = @import("module_binding.zig");
const value = @import("value.zig");

pub const ModuleBinding = internal.ModuleBinding;

comptime {
    if (@sizeOf(sdk.JSValue) != @sizeOf(value.JSValue)) {
        @compileError("zigttp-sdk.JSValue must match runtime JSValue size");
    }
    if (@bitSizeOf(sdk.JSValue) != @bitSizeOf(value.JSValue)) {
        @compileError("zigttp-sdk.JSValue must match runtime JSValue bit size");
    }
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
        .module_func = wrapModuleFn(binding.module_func, binding.arg_count),
        .arg_count = binding.arg_count,
        .effect = mapEffectClass(binding.effect),
        .returns = mapReturnKind(binding.returns),
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
        .failure_severity = mapFailureSeverity(binding.failure_severity),
    };
}

fn adaptReturnKinds(comptime kinds: []const sdk.ReturnKind) [kinds.len]internal.ReturnKind {
    var out: [kinds.len]internal.ReturnKind = undefined;
    for (kinds, 0..) |kind, i| {
        out[i] = mapReturnKind(kind);
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

fn mapEffectClass(effect: sdk.EffectClass) internal.EffectClass {
    return @enumFromInt(@intFromEnum(effect));
}

fn mapReturnKind(kind: sdk.ReturnKind) internal.ReturnKind {
    return @enumFromInt(@intFromEnum(kind));
}

fn mapFailureSeverity(severity: sdk.FailureSeverity) internal.FailureSeverity {
    return @enumFromInt(@intFromEnum(severity));
}

fn wrapModuleFn(comptime module_fn: sdk.ModuleFn, comptime arg_count: u8) internal.ModuleFn {
    return struct {
        fn call(handle: *internal.ModuleHandle, this: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            std.debug.assert(args.len <= arg_count);

            var converted_args: [arg_count]sdk.JSValue = undefined;
            for (args, 0..) |arg, i| {
                converted_args[i] = toSdkValue(arg);
            }

            const result = try module_fn(
                @ptrCast(handle),
                toSdkValue(this),
                converted_args[0..args.len],
            );
            return toInternalValue(result);
        }
    }.call;
}

fn toSdkValue(v: value.JSValue) sdk.JSValue {
    return .{ .raw = v.raw };
}

fn toInternalValue(v: sdk.JSValue) value.JSValue {
    return .{ .raw = v.raw };
}
