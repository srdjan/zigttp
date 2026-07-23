const handles = @import("handle.zig");
const binding = @import("binding.zig");

pub const ModuleHandle = handles.ModuleHandle;
pub const ModuleCapability = binding.ModuleCapability;
pub const ModuleCapabilityError = binding.ModuleCapabilityError;

extern fn zttpSdkHasCapability(handle: *ModuleHandle, capability_tag: u8) bool;
extern fn zttpSdkNowMs(handle: *ModuleHandle, out_ms: *i64) bool;
extern fn zttpSdkFillRandom(handle: *ModuleHandle, buf_ptr: [*]u8, len: usize) bool;
extern fn zttpSdkWriteStderr(handle: *ModuleHandle, buf_ptr: [*]const u8, len: usize) bool;

pub fn hasCapability(handle: *ModuleHandle, capability: ModuleCapability) bool {
    return zttpSdkHasCapability(handle, @intFromEnum(capability));
}

pub fn requireCapability(handle: *ModuleHandle, capability: ModuleCapability) ModuleCapabilityError!void {
    if (hasCapability(handle, capability)) return;
    return error.MissingModuleCapability;
}

pub fn nowMs(handle: *ModuleHandle) ModuleCapabilityError!i64 {
    try requireCapability(handle, .clock);
    var out_ms: i64 = 0;
    if (!zttpSdkNowMs(handle, &out_ms)) return error.ClockUnavailable;
    return out_ms;
}

pub fn fillRandom(handle: *ModuleHandle, buf: []u8) ModuleCapabilityError!void {
    try requireCapability(handle, .random);
    if (buf.len == 0) return;
    // The bridge zeroes the buffer and returns false when it cannot fill it
    // (OS entropy unavailable). Surface that as an error rather than let the
    // caller emit the all-zero buffer as a "random" value (a predictable token).
    if (!zttpSdkFillRandom(handle, buf.ptr, buf.len)) return error.RandomUnavailable;
}

pub fn writeStderr(handle: *ModuleHandle, buf: []const u8) ModuleCapabilityError!void {
    try requireCapability(handle, .stderr);
    if (buf.len == 0) return;
    if (!zttpSdkWriteStderr(handle, buf.ptr, buf.len)) return error.StderrWriteFailed;
}
