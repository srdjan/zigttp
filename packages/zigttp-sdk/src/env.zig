const handles = @import("handle.zig");

const ModuleHandle = handles.ModuleHandle;

extern fn zigttpSdkReadEnv(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize, out_ptr: *[*]const u8, out_len: *usize) bool;
extern fn zigttpSdkAllowsEnv(handle: *ModuleHandle, name_ptr: [*]const u8, name_len: usize) bool;

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
