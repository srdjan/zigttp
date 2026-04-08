const std = @import("std");

pub const ModuleHandle = opaque {};

/// Public ABI-facing JS value representation for packaged virtual modules.
/// The runtime adapts this to its internal NaN-boxed value type by raw bit cast.
pub const JSValue = packed struct(u64) {
    raw: u64,

    const INT_PREFIX: u64 = 0xFFFD_0000_0000_0000;
    const SPECIAL_PREFIX: u64 = 0xFFFE_0000_0000_0000;
    const PREFIX_MASK: u64 = 0xFFFF_0000_0000_0000;
    const MIN_TAG_PREFIX: u64 = 0xFFFC;
    const CANONICAL_NAN_BITS: u64 = 0x7FF8_0000_0000_0000;

    pub const undefined_val: JSValue = .{ .raw = SPECIAL_PREFIX | 1 };
    pub const true_val: JSValue = .{ .raw = SPECIAL_PREFIX | 2 };
    pub const false_val: JSValue = .{ .raw = SPECIAL_PREFIX | 3 };

    pub inline fn isInt(self: JSValue) bool {
        return (self.raw & PREFIX_MASK) == INT_PREFIX;
    }

    pub inline fn fromInt(v: i32) JSValue {
        const bits: u32 = @bitCast(v);
        return .{ .raw = INT_PREFIX | @as(u64, bits) };
    }

    pub inline fn getInt(self: JSValue) i32 {
        std.debug.assert(self.isInt());
        return @bitCast(@as(u32, @truncate(self.raw)));
    }

    pub inline fn toInt(self: JSValue) ?i32 {
        return if (self.isInt()) self.getInt() else null;
    }

    pub inline fn isBool(self: JSValue) bool {
        return self.raw == true_val.raw or self.raw == false_val.raw;
    }

    pub inline fn fromBool(v: bool) JSValue {
        return if (v) true_val else false_val;
    }

    pub inline fn getBool(self: JSValue) bool {
        std.debug.assert(self.isBool());
        return self.raw == true_val.raw;
    }

    pub inline fn isRawDouble(self: JSValue) bool {
        return (self.raw >> 48) < MIN_TAG_PREFIX;
    }

    pub inline fn fromFloat(v: f64) JSValue {
        const bits: u64 = @bitCast(v);
        if ((bits >> 48) >= MIN_TAG_PREFIX) {
            return .{ .raw = CANONICAL_NAN_BITS };
        }
        return .{ .raw = bits };
    }

    pub inline fn toFloat(self: JSValue) ?f64 {
        return if (self.isRawDouble()) @bitCast(self.raw) else null;
    }
};

pub const ModuleFn = *const fn (
    handle: *ModuleHandle,
    this: JSValue,
    args: []const JSValue,
) anyerror!JSValue;

pub const EffectClass = enum {
    read,
    write,
    none,
};

pub const ReturnKind = enum {
    boolean,
    number,
    string,
    object,
    undefined,
    unknown,
    optional_string,
    optional_object,
    result,
};

pub const FailureSeverity = enum {
    critical,
    expected,
    upstream,
    none,
};

pub const ModuleCapability = enum {
    env,
    clock,
    random,
    crypto,
    stderr,
    runtime_callback,
    sqlite,
    filesystem,
    network,
    policy_check,
};

pub const ModuleCapabilityError = error{
    MissingModuleCapability,
    ClockUnavailable,
    StderrWriteFailed,
};

extern fn zigttpSdkHasCapability(handle: *ModuleHandle, capability_tag: u8) bool;
extern fn zigttpSdkNowMs(handle: *ModuleHandle, out_ms: *i64) bool;
extern fn zigttpSdkFillRandom(handle: *ModuleHandle, buf_ptr: [*]u8, len: usize) void;
extern fn zigttpSdkWriteStderr(handle: *ModuleHandle, buf_ptr: [*]const u8, len: usize) bool;

pub fn hasCapability(handle: *ModuleHandle, capability: ModuleCapability) bool {
    return zigttpSdkHasCapability(handle, @intFromEnum(capability));
}

pub fn requireCapability(handle: *ModuleHandle, capability: ModuleCapability) ModuleCapabilityError!void {
    if (hasCapability(handle, capability)) return;
    return error.MissingModuleCapability;
}

pub fn nowMs(handle: *ModuleHandle) ModuleCapabilityError!i64 {
    try requireCapability(handle, .clock);
    var out_ms: i64 = 0;
    if (!zigttpSdkNowMs(handle, &out_ms)) return error.ClockUnavailable;
    return out_ms;
}

pub fn fillRandom(handle: *ModuleHandle, buf: []u8) ModuleCapabilityError!void {
    try requireCapability(handle, .random);
    if (buf.len == 0) return;
    zigttpSdkFillRandom(handle, buf.ptr, buf.len);
}

pub fn writeStderr(handle: *ModuleHandle, buf: []const u8) ModuleCapabilityError!void {
    try requireCapability(handle, .stderr);
    if (buf.len == 0) return;
    if (!zigttpSdkWriteStderr(handle, buf.ptr, buf.len)) return error.StderrWriteFailed;
}

pub const DataLabel = enum(u3) {
    secret,
    credential,
    user_input,
    config,
    internal,
    external,
    validated,
};

pub const LabelSet = packed struct(u8) {
    secret: bool = false,
    credential: bool = false,
    user_input: bool = false,
    config: bool = false,
    internal: bool = false,
    external: bool = false,
    validated: bool = false,
    _pad: u1 = 0,

    pub const empty: LabelSet = .{};
};

pub const ContractCategory = enum {
    env,
    cache_namespace,
    sql_registration,
    durable_key,
    durable_step,
    durable_signal,
    durable_producer_key,
    schema_compile,
    request_schema,
    route_pattern,
};

pub const ContractTransform = enum {
    extract_host,
    identity,
};

pub const ContractExtraction = struct {
    arg_position: u8 = 0,
    category: ContractCategory,
    transform: ?ContractTransform = null,
    flag_only: bool = false,
};

pub const ContractFlags = struct {
    sets_durable_used: bool = false,
    sets_durable_timers: bool = false,
    sets_bearer_auth: bool = false,
    sets_jwt_auth: bool = false,
};

pub const FunctionBinding = struct {
    name: []const u8,
    module_func: ModuleFn,
    arg_count: u8,
    effect: EffectClass = .read,
    returns: ReturnKind = .unknown,
    param_types: []const ReturnKind = &.{},
    traceable: bool = true,
    contract_extractions: []const ContractExtraction = &.{},
    contract_flags: ContractFlags = .{},
    return_labels: LabelSet = .{},
    failure_severity: FailureSeverity = .none,
};

pub const ModuleBinding = struct {
    specifier: []const u8,
    name: []const u8,
    exports: []const FunctionBinding,
    required_capabilities: []const ModuleCapability = &.{},
    stateful: bool = false,
    state_init: ?*const fn (*anyopaque, std.mem.Allocator) anyerror!void = null,
    state_deinit: ?*const fn (*anyopaque, std.mem.Allocator) void = null,
    contract_section: ?[]const u8 = null,
    sandboxable: bool = false,
    comptime_only: bool = false,
};

pub fn validateBindings(comptime bindings: []const ModuleBinding) void {
    @setEvalBranchQuota(5000);

    for (bindings) |binding| {
        if (!std.mem.startsWith(u8, binding.specifier, "zigttp-ext:")) {
            @compileError("package module specifier must start with 'zigttp-ext:': " ++ binding.specifier);
        }

        if (binding.state_init != null and binding.state_deinit == null) {
            @compileError("module has state_init but missing state_deinit: " ++ binding.specifier);
        }
        if (binding.state_init == null and binding.state_deinit != null) {
            @compileError("module has state_deinit but missing state_init: " ++ binding.specifier);
        }
        if (findDuplicateRequiredCapability(binding.required_capabilities)) |capability| {
            @compileError("duplicate required capability '" ++ @tagName(capability) ++ "' in " ++ binding.specifier);
        }
    }

    for (bindings, 0..) |a, i| {
        for (bindings[i + 1 ..]) |b| {
            if (std.mem.eql(u8, a.specifier, b.specifier)) {
                @compileError("duplicate module specifier: " ++ a.specifier);
            }
        }
        for (a.exports, 0..) |af, afi| {
            for (a.exports[afi + 1 ..]) |af2| {
                if (std.mem.eql(u8, af.name, af2.name)) {
                    @compileError("duplicate function name within " ++ a.specifier ++ ": " ++ af.name);
                }
            }
            for (bindings[i + 1 ..]) |b| {
                for (b.exports) |bf| {
                    if (std.mem.eql(u8, af.name, bf.name)) {
                        @compileError("duplicate function name '" ++ af.name ++ "' in " ++ a.specifier ++ " and " ++ b.specifier);
                    }
                }
            }
        }
    }
}

fn findDuplicateRequiredCapability(comptime capabilities: []const ModuleCapability) ?ModuleCapability {
    for (capabilities, 0..) |capability, i| {
        for (capabilities[i + 1 ..]) |other| {
            if (capability == other) return capability;
        }
    }
    return null;
}
