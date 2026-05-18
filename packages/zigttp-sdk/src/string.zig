const handles = @import("handle.zig");
const value = @import("value.zig");

pub const ModuleHandle = handles.ModuleHandle;
pub const RuntimeError = handles.RuntimeError;
pub const JSValue = value.JSValue;

extern fn zigttpSdkExtractString(val: JSValue, out_ptr: *[*]const u8, out_len: *usize) bool;
extern fn zigttpSdkCreateString(handle: *ModuleHandle, ptr: [*]const u8, len: usize, out: *JSValue) bool;
extern fn zigttpSdkIsString(val: JSValue) bool;

/// Extract a borrowed string slice from a JSValue. Handles flat, slice,
/// and leaf rope strings. Returns null for non-string values or
/// non-flattened concat ropes. Slice is valid for the current call.
pub fn extractString(val: JSValue) ?[]const u8 {
    var ptr: [*]const u8 = undefined;
    var len: usize = 0;
    if (!zigttpSdkExtractString(val, &ptr, &len)) return null;
    return ptr[0..len];
}

/// Allocate a new JS string owned by the runtime GC.
pub fn createString(handle: *ModuleHandle, data: []const u8) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zigttpSdkCreateString(handle, data.ptr, data.len, &out)) return error.OutOfMemory;
    return out;
}

pub fn isString(val: JSValue) bool {
    return zigttpSdkIsString(val);
}
