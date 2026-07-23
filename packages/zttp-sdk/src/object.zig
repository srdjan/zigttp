const handles = @import("handle.zig");
const value = @import("value.zig");

pub const ModuleHandle = handles.ModuleHandle;
pub const RuntimeError = handles.RuntimeError;
pub const JSValue = value.JSValue;

extern fn zttpSdkCreateObject(handle: *ModuleHandle, out: *JSValue) bool;
extern fn zttpSdkObjectSet(handle: *ModuleHandle, obj: JSValue, key_ptr: [*]const u8, key_len: usize, val: JSValue) bool;
extern fn zttpSdkObjectGet(handle: *ModuleHandle, obj: JSValue, key_ptr: [*]const u8, key_len: usize, out: *JSValue) bool;
extern fn zttpSdkObjectKeys(handle: *ModuleHandle, obj: JSValue, out: *JSValue) bool;
extern fn zttpSdkIsObject(val: JSValue) bool;

/// Allocate a new empty JS object owned by the runtime GC.
pub fn createObject(handle: *ModuleHandle) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zttpSdkCreateObject(handle, &out)) return error.OutOfMemory;
    return out;
}

/// Set a property on a JS object. Key is a UTF-8 string; the runtime
/// interns it into an atom.
pub fn objectSet(handle: *ModuleHandle, obj: JSValue, key: []const u8, val: JSValue) RuntimeError!void {
    if (!zttpSdkObjectSet(handle, obj, key.ptr, key.len, val)) return error.RuntimeFailure;
}

/// Get a property from a JS object. Returns null if the property is
/// absent or the target is not an object.
pub fn objectGet(handle: *ModuleHandle, obj: JSValue, key: []const u8) ?JSValue {
    var out: JSValue = undefined;
    if (!zttpSdkObjectGet(handle, obj, key.ptr, key.len, &out)) return null;
    return out;
}

/// Return the own enumerable property keys of an object as a JS array of
/// strings. Iterate via `arrayLength` + `arrayGet`.
pub fn objectKeys(handle: *ModuleHandle, obj: JSValue) RuntimeError!JSValue {
    var out: JSValue = undefined;
    if (!zttpSdkObjectKeys(handle, obj, &out)) return error.RuntimeFailure;
    return out;
}

pub fn isObject(val: JSValue) bool {
    return zttpSdkIsObject(val);
}
