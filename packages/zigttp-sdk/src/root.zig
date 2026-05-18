const std = @import("std");

pub const handles = @import("handle.zig");
pub const value = @import("value.zig");
pub const binding = @import("binding.zig");
pub const capability = @import("capability.zig");
pub const string = @import("string.zig");
pub const object = @import("object.zig");
pub const array = @import("array.zig");
pub const result = @import("result.zig");
pub const sqlite = @import("sqlite.zig");

pub const ModuleHandle = handles.ModuleHandle;
pub const RuntimeError = handles.RuntimeError;
pub const JSValue = value.JSValue;

pub const ModuleFn = binding.ModuleFn;
pub const EffectClass = binding.EffectClass;
pub const ReturnKind = binding.ReturnKind;
pub const FailureSeverity = binding.FailureSeverity;
pub const ModuleCapability = binding.ModuleCapability;
pub const ModuleCapabilityError = binding.ModuleCapabilityError;
pub const DataLabel = binding.DataLabel;
pub const LabelSet = binding.LabelSet;
pub const ContractCategory = binding.ContractCategory;
pub const ContractTransform = binding.ContractTransform;
pub const ContractExtraction = binding.ContractExtraction;
pub const ContractFlags = binding.ContractFlags;
pub const LawKind = binding.LawKind;
pub const AbsorbingPattern = binding.AbsorbingPattern;
pub const Law = binding.Law;
pub const FunctionBinding = binding.FunctionBinding;
pub const ModuleBinding = binding.ModuleBinding;
pub const validateBindings = binding.validateBindings;

pub const hasCapability = capability.hasCapability;
pub const requireCapability = capability.requireCapability;
pub const nowMs = capability.nowMs;
pub const fillRandom = capability.fillRandom;
pub const writeStderr = capability.writeStderr;

pub const extractString = string.extractString;
pub const createString = string.createString;
pub const isString = string.isString;

pub const createObject = object.createObject;
pub const objectSet = object.objectSet;
pub const objectGet = object.objectGet;
pub const objectKeys = object.objectKeys;
pub const isObject = object.isObject;

pub const isArray = array.isArray;
pub const arrayLength = array.arrayLength;
pub const arrayGet = array.arrayGet;
pub const arraySet = array.arraySet;
pub const createArray = array.createArray;
pub const arrayPush = array.arrayPush;

pub const throwError = result.throwError;
pub const resultOk = result.resultOk;
pub const resultErr = result.resultErr;
pub const resultErrValue = result.resultErrValue;
pub const resultErrs = result.resultErrs;

pub const SqliteDb = sqlite.SqliteDb;
pub const SqliteStmt = sqlite.SqliteStmt;
pub const sqlite_row = sqlite.sqlite_row;
pub const sqlite_done = sqlite.sqlite_done;
pub const sqlite_integer = sqlite.sqlite_integer;
pub const sqlite_float = sqlite.sqlite_float;
pub const sqlite_text = sqlite.sqlite_text;
pub const sqlite_blob = sqlite.sqlite_blob;
pub const sqlite_null = sqlite.sqlite_null;
pub const SqliteError = sqlite.SqliteError;
pub const allowsSqlQuery = sqlite.allowsSqlQuery;
pub const allowsSqlWrite = sqlite.allowsSqlWrite;
pub const sqliteOpen = sqlite.sqliteOpen;
pub const sqliteClose = sqlite.sqliteClose;
pub const sqliteChanges = sqlite.sqliteChanges;
pub const sqliteLastInsertRowId = sqlite.sqliteLastInsertRowId;
pub const sqliteErrmsg = sqlite.sqliteErrmsg;
pub const sqlitePrepare = sqlite.sqlitePrepare;
pub const sqliteFinalize = sqlite.sqliteFinalize;
pub const sqliteStep = sqlite.sqliteStep;
pub const sqliteReadonly = sqlite.sqliteReadonly;
pub const sqliteStmtErrmsg = sqlite.sqliteStmtErrmsg;
pub const sqliteParamCount = sqlite.sqliteParamCount;
pub const sqliteParamName = sqlite.sqliteParamName;
pub const sqliteBindNull = sqlite.sqliteBindNull;
pub const sqliteBindInt64 = sqlite.sqliteBindInt64;
pub const sqliteBindDouble = sqlite.sqliteBindDouble;
pub const sqliteBindText = sqlite.sqliteBindText;
pub const sqliteColumnCount = sqlite.sqliteColumnCount;
pub const sqliteColumnName = sqlite.sqliteColumnName;
pub const sqliteColumnType = sqlite.sqliteColumnType;
pub const sqliteColumnInt64 = sqlite.sqliteColumnInt64;
pub const sqliteColumnDouble = sqlite.sqliteColumnDouble;
pub const sqliteColumnText = sqlite.sqliteColumnText;

pub const StateDeinitFn = *const fn (*anyopaque) callconv(.c) void;

// JSValue is packed struct(u64); zigts's value.JSValue is bit-identical
// and verified by module_binding_adapter.zig. The bridge exports below
// can therefore pass JSValue directly across the extern boundary.
extern fn zigttpSdkGetAllocator(handle: *ModuleHandle) *const std.mem.Allocator;
extern fn zigttpSdkSha256(data_ptr: [*]const u8, data_len: usize, out: [*]u8) bool;
extern fn zigttpSdkHmacSha256(data_ptr: [*]const u8, data_len: usize, key_ptr: [*]const u8, key_len: usize, out: [*]u8) bool;
extern fn zigttpSdkParseJson(handle: *ModuleHandle, json_ptr: [*]const u8, json_len: usize, out: *JSValue) bool;
extern fn zigttpSdkStringify(handle: *ModuleHandle, val: JSValue, out: *JSValue) bool;
extern fn zigttpSdkGetModuleState(handle: *ModuleHandle, slot: usize) ?*anyopaque;
extern fn zigttpSdkSetModuleState(handle: *ModuleHandle, slot: usize, ptr: *anyopaque, deinit_fn: StateDeinitFn) bool;
extern fn zigttpSdkIsCallable(val: JSValue) bool;
extern fn zigttpSdkReadFile(
    handle: *ModuleHandle,
    path_ptr: [*]const u8,
    path_len: usize,
    max_size: usize,
    out_ptr: *[*]u8,
    out_len: *usize,
) bool;
extern fn zigttpSdkReadEnv(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize, out_ptr: *[*]const u8, out_len: *usize) bool;
extern fn zigttpSdkAllowsEnv(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize) bool;
extern fn zigttpSdkAllowsCacheNamespace(handle: *ModuleHandle, ns_ptr: [*]const u8, ns_len: usize) bool;

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

/// Box a Zig f64 as a JS number, folding safe-integer values into the
/// int32 tagged representation. Matches the runtime's allocFloat hot path.
pub fn numberFromF64(v: f64) JSValue {
    if (!std.math.isNan(v) and !std.math.isInf(v) and @floor(v) == v and v >= -2147483648 and v <= 2147483647) {
        return JSValue.fromInt(@intFromFloat(v));
    }
    return JSValue.fromFloat(v);
}

/// Borrow the runtime's general-purpose allocator. Valid for the module
/// call's lifetime.
pub fn getAllocator(handle: *ModuleHandle) std.mem.Allocator {
    return zigttpSdkGetAllocator(handle).*;
}

pub const Sha256Digest = [32]u8;
pub const HmacSha256Mac = [32]u8;

/// Compute SHA-256. Requires `.crypto` to be declared on the module
/// binding; both the SDK wrapper and the bridge check, matching the
/// self-checking pattern used by `sqliteOpen`.
pub fn sha256(handle: *ModuleHandle, data: []const u8, out: *Sha256Digest) ModuleCapabilityError!void {
    try requireCapability(handle, .crypto);
    if (!zigttpSdkSha256(data.ptr, data.len, out)) return error.MissingModuleCapability;
}

/// Compute HMAC-SHA256. Requires `.crypto` (same enforcement as `sha256`).
pub fn hmacSha256(
    handle: *ModuleHandle,
    data: []const u8,
    key: []const u8,
    out: *HmacSha256Mac,
) ModuleCapabilityError!void {
    try requireCapability(handle, .crypto);
    if (!zigttpSdkHmacSha256(data.ptr, data.len, key.ptr, key.len, out)) return error.MissingModuleCapability;
}

/// Parse a JSON string into a JSValue owned by the runtime GC.
pub fn parseJson(handle: *ModuleHandle, json: []const u8) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkParseJson(handle, json.ptr, json.len, &out)) return error.RuntimeFailure;
    return out;
}

/// Serialize a JSValue to a JSON string.
pub fn stringify(handle: *ModuleHandle, val: JSValue) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkStringify(handle, val, &out)) return error.RuntimeFailure;
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

pub fn isCallable(val: JSValue) bool {
    return zigttpSdkIsCallable(val);
}

/// Read a file through the capability-gated filesystem path. The buffer
/// is allocated with the runtime allocator (`getAllocator(handle)`);
/// callers free it via `getAllocator(handle).free(buf)`.
pub fn readFile(handle: *ModuleHandle, path: []const u8, max_size: usize) ![]u8 {
    try requireCapability(handle, .filesystem);
    var ptr: [*]u8 = undefined;
    var len: usize = 0;
    if (!zigttpSdkReadFile(handle, path.ptr, path.len, max_size, &ptr, &len)) return error.FileReadFailed;
    return ptr[0..len];
}

/// Read an environment variable through the capability-policy gate.
/// Returns null when unset or when policy denies. The slice is valid for
/// the current call.
pub fn readEnv(handle: *ModuleHandle, name: []const u8) ?[]const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    if (!zigttpSdkReadEnv(handle, name.ptr, name.len, &ptr, &len)) return null;
    return ptr[0..len];
}

/// Ask the capability policy whether `env(name)` is permitted.
pub fn allowsEnv(handle: *ModuleHandle, name: []const u8) bool {
    return zigttpSdkAllowsEnv(handle, name.ptr, name.len);
}

/// Ask the capability policy whether `cache(namespace)` is permitted.
pub fn allowsCacheNamespace(handle: *ModuleHandle, ns: []const u8) bool {
    return zigttpSdkAllowsCacheNamespace(handle, ns.ptr, ns.len);
}
