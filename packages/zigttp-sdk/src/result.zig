const handles = @import("handle.zig");
const value = @import("value.zig");

pub const ModuleHandle = handles.ModuleHandle;
pub const RuntimeError = handles.RuntimeError;
pub const JSValue = value.JSValue;

extern fn zigttpSdkThrowError(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize, msg_ptr: [*]const u8, msg_len: usize) JSValue;
extern fn zigttpSdkResultOk(handle: *ModuleHandle, payload: JSValue, out: *JSValue) bool;
extern fn zigttpSdkResultErr(handle: *ModuleHandle, msg_ptr: [*]const u8, msg_len: usize, out: *JSValue) bool;
extern fn zigttpSdkResultErrValue(handle: *ModuleHandle, payload: JSValue, out: *JSValue) bool;
extern fn zigttpSdkResultErrs(handle: *ModuleHandle, payload: JSValue, out: *JSValue) bool;

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
