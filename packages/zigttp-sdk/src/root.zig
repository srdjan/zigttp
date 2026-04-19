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

    pub const null_val: JSValue = .{ .raw = SPECIAL_PREFIX | 0 };
    pub const undefined_val: JSValue = .{ .raw = SPECIAL_PREFIX | 1 };
    pub const true_val: JSValue = .{ .raw = SPECIAL_PREFIX | 2 };
    pub const false_val: JSValue = .{ .raw = SPECIAL_PREFIX | 3 };

    pub inline fn isNull(self: JSValue) bool {
        return self.raw == null_val.raw;
    }

    pub inline fn isUndefined(self: JSValue) bool {
        return self.raw == undefined_val.raw;
    }

    pub inline fn isTrue(self: JSValue) bool {
        return self.raw == true_val.raw;
    }

    pub inline fn isNullish(self: JSValue) bool {
        return self.isNull() or self.isUndefined();
    }

    pub inline fn isNumber(self: JSValue) bool {
        return self.isInt() or self.isRawDouble();
    }

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
    websocket,
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

pub const RuntimeError = error{
    OutOfMemory,
    RuntimeFailure,
};

// JSValue is packed struct(u64); zigts's value.JSValue is bit-identical
// and verified by module_binding_adapter.zig. The bridge exports below
// can therefore pass JSValue directly across the extern boundary.
extern fn zigttpSdkExtractString(val: JSValue, out_ptr: *[*]const u8, out_len: *usize) bool;
extern fn zigttpSdkCreateString(handle: *ModuleHandle, ptr: [*]const u8, len: usize, out: *JSValue) bool;
extern fn zigttpSdkCreateObject(handle: *ModuleHandle, out: *JSValue) bool;
extern fn zigttpSdkObjectSet(handle: *ModuleHandle, obj: JSValue, key_ptr: [*]const u8, key_len: usize, val: JSValue) bool;
extern fn zigttpSdkObjectGet(handle: *ModuleHandle, obj: JSValue, key_ptr: [*]const u8, key_len: usize, out: *JSValue) bool;
extern fn zigttpSdkThrowError(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize, msg_ptr: [*]const u8, msg_len: usize) JSValue;
extern fn zigttpSdkResultOk(handle: *ModuleHandle, payload: JSValue, out: *JSValue) bool;
extern fn zigttpSdkResultErr(handle: *ModuleHandle, msg_ptr: [*]const u8, msg_len: usize, out: *JSValue) bool;
extern fn zigttpSdkResultErrValue(handle: *ModuleHandle, payload: JSValue, out: *JSValue) bool;
extern fn zigttpSdkResultErrs(handle: *ModuleHandle, payload: JSValue, out: *JSValue) bool;
extern fn zigttpSdkGetAllocator(handle: *ModuleHandle) *const std.mem.Allocator;
extern fn zigttpSdkSha256(data_ptr: [*]const u8, data_len: usize, out: [*]u8) bool;
extern fn zigttpSdkHmacSha256(data_ptr: [*]const u8, data_len: usize, key_ptr: [*]const u8, key_len: usize, out: [*]u8) bool;
extern fn zigttpSdkParseJson(handle: *ModuleHandle, json_ptr: [*]const u8, json_len: usize, out: *JSValue) bool;

pub const StateDeinitFn = *const fn (*anyopaque) callconv(.c) void;
extern fn zigttpSdkGetModuleState(handle: *ModuleHandle, slot: usize) ?*anyopaque;
extern fn zigttpSdkSetModuleState(handle: *ModuleHandle, slot: usize, ptr: *anyopaque, deinit_fn: StateDeinitFn) bool;

extern fn zigttpSdkIsString(val: JSValue) bool;
extern fn zigttpSdkIsObject(val: JSValue) bool;
extern fn zigttpSdkIsArray(val: JSValue) bool;
extern fn zigttpSdkArrayLength(val: JSValue, out: *u32) bool;
extern fn zigttpSdkArrayGet(handle: *ModuleHandle, arr: JSValue, index: u32, out: *JSValue) bool;
extern fn zigttpSdkArraySet(handle: *ModuleHandle, arr: JSValue, index: u32, val: JSValue) bool;
extern fn zigttpSdkCreateArray(handle: *ModuleHandle, out: *JSValue) bool;
extern fn zigttpSdkStringify(handle: *ModuleHandle, val: JSValue, out: *JSValue) bool;

/// Extract a borrowed string slice from a JSValue. Handles flat, slice,
/// and leaf rope strings. Returns null for non-string values or
/// non-flattened concat ropes. Slice is valid for the current call.
pub fn extractString(val: JSValue) ?[]const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    if (!zigttpSdkExtractString(val, &ptr, &len)) return null;
    return ptr[0..len];
}

/// Extract an i32 from a JSValue, handling both int and whole-number float
/// representations.
pub fn extractInt(val: JSValue) ?i32 {
    if (val.toInt()) |i| return i;
    if (val.toFloat()) |f| {
        const i: i32 = @intFromFloat(f);
        if (@as(f64, @floatFromInt(i)) == f) return i;
    }
    return null;
}

/// Extract an f64 from a JSValue, widening int values.
pub fn extractFloat(val: JSValue) ?f64 {
    if (val.toInt()) |i| return @floatFromInt(i);
    return val.toFloat();
}

/// Allocate a new JS string owned by the runtime GC.
pub fn createString(handle: *ModuleHandle, data: []const u8) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkCreateString(handle, data.ptr, data.len, &out)) return error.OutOfMemory;
    return out;
}

/// Allocate a new empty JS object owned by the runtime GC.
pub fn createObject(handle: *ModuleHandle) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkCreateObject(handle, &out)) return error.OutOfMemory;
    return out;
}

/// Set a property on a JS object. Key is a UTF-8 string; the runtime
/// interns it into an atom.
pub fn objectSet(handle: *ModuleHandle, obj: JSValue, key: []const u8, val: JSValue) RuntimeError!void {
    if (!zigttpSdkObjectSet(handle, obj, key.ptr, key.len, val)) return error.RuntimeFailure;
}

/// Get a property from a JS object. Returns null if the property is
/// absent or the target is not an object.
pub fn objectGet(handle: *ModuleHandle, obj: JSValue, key: []const u8) ?JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkObjectGet(handle, obj, key.ptr, key.len, &out)) return null;
    return out;
}

/// Raise a JS exception. The returned JSValue is the exception sentinel;
/// return it from your module function.
pub fn throwError(handle: *ModuleHandle, name: []const u8, message: []const u8) JSValue {
    return zigttpSdkThrowError(handle, name.ptr, name.len, message.ptr, message.len);
}

/// Build `{ ok: true, value: payload }`.
pub fn resultOk(handle: *ModuleHandle, payload: JSValue) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkResultOk(handle, payload, &out)) return error.OutOfMemory;
    return out;
}

/// Build `{ ok: false, error: message }`.
pub fn resultErr(handle: *ModuleHandle, message: []const u8) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkResultErr(handle, message.ptr, message.len, &out)) return error.OutOfMemory;
    return out;
}

/// Build `{ ok: false, error: payload }` with a JSValue payload.
pub fn resultErrValue(handle: *ModuleHandle, payload: JSValue) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkResultErrValue(handle, payload, &out)) return error.OutOfMemory;
    return out;
}

/// Build `{ ok: false, errors: payload }` with an array payload.
pub fn resultErrs(handle: *ModuleHandle, payload: JSValue) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkResultErrs(handle, payload, &out)) return error.OutOfMemory;
    return out;
}

/// Borrow the runtime's general-purpose allocator. Valid for the module
/// call's lifetime.
pub fn getAllocator(handle: *ModuleHandle) std.mem.Allocator {
    return zigttpSdkGetAllocator(handle).*;
}

pub const Sha256Digest = [32]u8;
pub const HmacSha256Mac = [32]u8;

/// Compute SHA-256. Capability enforcement lives inside the bridge;
/// callers must declare `.crypto` in `required_capabilities`.
pub fn sha256(handle: *ModuleHandle, data: []const u8, out: *Sha256Digest) ModuleCapabilityError!void {
    _ = handle;
    if (!zigttpSdkSha256(data.ptr, data.len, out)) return error.MissingModuleCapability;
}

/// Compute HMAC-SHA256. Capability enforcement lives inside the bridge.
pub fn hmacSha256(
    handle: *ModuleHandle,
    data: []const u8,
    key: []const u8,
    out: *HmacSha256Mac,
) ModuleCapabilityError!void {
    _ = handle;
    if (!zigttpSdkHmacSha256(data.ptr, data.len, key.ptr, key.len, out)) return error.MissingModuleCapability;
}

/// Parse a JSON string into a JSValue owned by the runtime GC.
pub fn parseJson(handle: *ModuleHandle, json: []const u8) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkParseJson(handle, json.ptr, json.len, &out)) return error.RuntimeFailure;
    return out;
}

/// Get typed module state from a slot. Returns null if the slot has not
/// been initialized.
pub fn getModuleState(handle: *ModuleHandle, comptime T: type, slot: usize) ?*T {
    const ptr = zigttpSdkGetModuleState(handle, slot) orelse return null;
    return @ptrCast(@alignCast(ptr));
}

/// Install module state into a slot with a cleanup callback. The callback
/// receives the state pointer on context teardown; modules typically store
/// their own allocator in the state struct and free through it.
pub fn setModuleState(
    handle: *ModuleHandle,
    slot: usize,
    ptr: *anyopaque,
    deinit_fn: StateDeinitFn,
) RuntimeError!void {
    if (!zigttpSdkSetModuleState(handle, slot, ptr, deinit_fn)) return error.OutOfMemory;
}

pub fn isString(val: JSValue) bool {
    return zigttpSdkIsString(val);
}

pub fn isObject(val: JSValue) bool {
    return zigttpSdkIsObject(val);
}

pub fn isArray(val: JSValue) bool {
    return zigttpSdkIsArray(val);
}

pub fn arrayLength(val: JSValue) ?u32 {
    var len: u32 = 0;
    if (!zigttpSdkArrayLength(val, &len)) return null;
    return len;
}

pub fn arrayGet(handle: *ModuleHandle, arr: JSValue, index: u32) ?JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkArrayGet(handle, arr, index, &out)) return null;
    return out;
}

pub fn arraySet(handle: *ModuleHandle, arr: JSValue, index: u32, val: JSValue) RuntimeError!void {
    if (!zigttpSdkArraySet(handle, arr, index, val)) return error.RuntimeFailure;
}

pub fn createArray(handle: *ModuleHandle) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkCreateArray(handle, &out)) return error.OutOfMemory;
    return out;
}

/// Serialize a JSValue to a JSON string.
pub fn stringify(handle: *ModuleHandle, val: JSValue) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkStringify(handle, val, &out)) return error.RuntimeFailure;
    return out;
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
    scope_name,
    durable_key,
    durable_step,
    durable_signal,
    durable_producer_key,
    schema_compile,
    request_schema,
    route_pattern,
    cookie_name,
    cors_origin,
    rate_limit_key,
    service_call,
    fetch_host,
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
    sets_scope_used: bool = false,
    sets_durable_used: bool = false,
    sets_durable_timers: bool = false,
    sets_bearer_auth: bool = false,
    sets_jwt_auth: bool = false,
};

pub const LawKind = enum {
    pure,
    idempotent_call,
    inverse_of,
    absorbing,
};

pub const AbsorbingPattern = struct {
    arg_position: u8 = 0,
    argument_shape: ArgumentShape,
    residue: Residue,

    pub const ArgumentShape = enum {
        empty_string_literal,
        undefined_literal,
    };

    pub const Residue = enum {
        result_err,
        returns_undefined,
        returns_false,
    };
};

pub const Law = union(LawKind) {
    pure: void,
    idempotent_call: void,
    inverse_of: []const u8,
    absorbing: AbsorbingPattern,
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
    laws: []const Law = &.{},
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
    self_managed_io: bool = false,
};

pub fn validateBindings(comptime bindings: []const ModuleBinding) void {
    @setEvalBranchQuota(5000);

    for (bindings) |binding| {
        const builtin_prefix = std.mem.startsWith(u8, binding.specifier, "zigttp:");
        const extension_prefix = std.mem.startsWith(u8, binding.specifier, "zigttp-ext:");
        if (!builtin_prefix and !extension_prefix) {
            @compileError("module specifier must start with 'zigttp:' or 'zigttp-ext:': " ++ binding.specifier);
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
