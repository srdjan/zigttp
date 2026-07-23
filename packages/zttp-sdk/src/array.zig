const handles = @import("handle.zig");
const value = @import("value.zig");

pub const ModuleHandle = handles.ModuleHandle;
pub const RuntimeError = handles.RuntimeError;
pub const JSValue = value.JSValue;

extern fn zttpSdkIsArray(val: JSValue) bool;
extern fn zttpSdkArrayLength(val: JSValue, out: *u32) bool;
extern fn zttpSdkArrayGet(handle: *ModuleHandle, arr: JSValue, index: u32, out: *JSValue) bool;
extern fn zttpSdkArraySet(handle: *ModuleHandle, arr: JSValue, index: u32, val: JSValue) bool;
extern fn zttpSdkCreateArray(handle: *ModuleHandle, out: *JSValue) bool;
extern fn zttpSdkArrayPush(handle: *ModuleHandle, arr: JSValue, val: JSValue) bool;

pub fn isArray(val: JSValue) bool {
    return zttpSdkIsArray(val);
}

pub fn arrayLength(val: JSValue) ?u32 {
    var len: u32 = 0;
    if (!zttpSdkArrayLength(val, &len)) return null;
    return len;
}

pub fn arrayGet(handle: *ModuleHandle, arr: JSValue, index: u32) ?JSValue {
    var out: JSValue = undefined;
    if (!zttpSdkArrayGet(handle, arr, index, &out)) return null;
    return out;
}

pub fn arraySet(handle: *ModuleHandle, arr: JSValue, index: u32, val: JSValue) RuntimeError!void {
    if (!zttpSdkArraySet(handle, arr, index, val)) return error.RuntimeFailure;
}

pub fn createArray(handle: *ModuleHandle) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zttpSdkCreateArray(handle, &out)) return error.OutOfMemory;
    return out;
}

/// Append a value to the end of a JS array.
pub fn arrayPush(handle: *ModuleHandle, arr: JSValue, val: JSValue) RuntimeError!void {
    if (!zttpSdkArrayPush(handle, arr, val)) return error.RuntimeFailure;
}
