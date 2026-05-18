const handles = @import("handle.zig");
const value = @import("value.zig");

pub const ModuleHandle = handles.ModuleHandle;
pub const RuntimeError = handles.RuntimeError;
pub const JSValue = value.JSValue;

extern fn zigttpSdkIsArray(val: JSValue) bool;
extern fn zigttpSdkArrayLength(val: JSValue, out: *u32) bool;
extern fn zigttpSdkArrayGet(handle: *ModuleHandle, arr: JSValue, index: u32, out: *JSValue) bool;
extern fn zigttpSdkArraySet(handle: *ModuleHandle, arr: JSValue, index: u32, val: JSValue) bool;
extern fn zigttpSdkCreateArray(handle: *ModuleHandle, out: *JSValue) bool;
extern fn zigttpSdkArrayPush(handle: *ModuleHandle, arr: JSValue, val: JSValue) bool;

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

/// Append a value to the end of a JS array.
pub fn arrayPush(handle: *ModuleHandle, arr: JSValue, val: JSValue) RuntimeError!void {
    if (!zigttpSdkArrayPush(handle, arr, val)) return error.RuntimeFailure;
}
