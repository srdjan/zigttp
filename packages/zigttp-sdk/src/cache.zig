const handles = @import("handle.zig");

const ModuleHandle = handles.ModuleHandle;

extern fn zigttpSdkAllowsCacheNamespace(handle: *ModuleHandle, ns_ptr: [*]const u8, ns_len: usize) bool;

/// Ask the capability policy whether `cache(namespace)` is permitted.
pub fn allowsCacheNamespace(handle: *ModuleHandle, ns: []const u8) bool {
    return zigttpSdkAllowsCacheNamespace(handle, ns.ptr, ns.len);
}
