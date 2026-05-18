const handles = @import("handle.zig");
const value = @import("value.zig");

const ModuleHandle = handles.ModuleHandle;
const RuntimeError = handles.RuntimeError;
const JSValue = value.JSValue;

extern fn zigttpSdkParseJson(handle: *ModuleHandle, json_ptr: [*]const u8, json_len: usize, out: *JSValue) bool;
extern fn zigttpSdkStringify(handle: *ModuleHandle, val: JSValue, out: *JSValue) bool;

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
