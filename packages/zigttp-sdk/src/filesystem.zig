const handles = @import("handle.zig");
const capability = @import("capability.zig");

const ModuleHandle = handles.ModuleHandle;

extern fn zigttpSdkReadFile(
    handle: *ModuleHandle,
    path_ptr: [*]const u8,
    path_len: usize,
    max_size: usize,
    out_ptr: *[*]u8,
    out_len: *usize,
) bool;

/// Read a file through the capability-gated filesystem path. The runtime also
/// requires the canonical path to be preallowed by its SDK filesystem
/// allowlist. The buffer is allocated with the runtime allocator
/// (`getAllocator(handle)`); callers free it via `getAllocator(handle).free(buf)`.
pub fn readFile(handle: *ModuleHandle, path: []const u8, max_size: usize) ![]u8 {
    try capability.requireCapability(handle, .filesystem);
    var ptr: [*]u8 = undefined;
    var len: usize = 0;
    if (!zigttpSdkReadFile(handle, path.ptr, path.len, max_size, &ptr, &len)) return error.FileReadFailed;
    return ptr[0..len];
}
