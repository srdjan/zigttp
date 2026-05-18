const handles = @import("handle.zig");
const binding = @import("binding.zig");
const capability = @import("capability.zig");

const ModuleHandle = handles.ModuleHandle;
const ModuleCapabilityError = binding.ModuleCapabilityError;

pub const Sha256Digest = [32]u8;
pub const HmacSha256Mac = [32]u8;

extern fn zigttpSdkSha256(data_ptr: [*]const u8, data_len: usize, out: [*]u8) bool;
extern fn zigttpSdkHmacSha256(data_ptr: [*]const u8, data_len: usize, key_ptr: [*]const u8, key_len: usize, out: [*]u8) bool;

pub fn sha256(handle: *ModuleHandle, data: []const u8, out: *Sha256Digest) ModuleCapabilityError!void {
    try capability.requireCapability(handle, .crypto);
    if (!zigttpSdkSha256(data.ptr, data.len, out)) return error.MissingModuleCapability;
}

pub fn hmacSha256(
    handle: *ModuleHandle,
    data: []const u8,
    key: []const u8,
    out: *HmacSha256Mac,
) ModuleCapabilityError!void {
    try capability.requireCapability(handle, .crypto);
    if (!zigttpSdkHmacSha256(data.ptr, data.len, key.ptr, key.len, out)) return error.MissingModuleCapability;
}
