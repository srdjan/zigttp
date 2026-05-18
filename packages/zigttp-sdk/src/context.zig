const std = @import("std");
const handles = @import("handle.zig");

const ModuleHandle = handles.ModuleHandle;
const RuntimeError = handles.RuntimeError;

pub const StateDeinitFn = *const fn (*anyopaque) callconv(.c) void;

extern fn zigttpSdkGetAllocator(handle: *ModuleHandle) *const std.mem.Allocator;
extern fn zigttpSdkGetModuleState(handle: *ModuleHandle, slot: usize) ?*anyopaque;
extern fn zigttpSdkSetModuleState(handle: *ModuleHandle, slot: usize, ptr: *anyopaque, deinit_fn: StateDeinitFn) bool;

/// Borrow the runtime's general-purpose allocator. Valid for the module
/// call's lifetime.
pub fn getAllocator(handle: *ModuleHandle) std.mem.Allocator {
    return zigttpSdkGetAllocator(handle).*;
}

/// Get typed module state from a slot. Returns null if the slot has not
/// been initialized.
pub fn getModuleState(handle: *ModuleHandle, comptime T: type, slot: usize) ?*T {
    const ptr = zigttpSdkGetModuleState(handle, slot) orelse return null;
    return @ptrCast(@alignCast(ptr));
}

/// Install module state into a slot with a cleanup callback. The callback
/// receives the state pointer on context teardown; modules typically store
/// their own allocator in the state struct and free through it.
pub fn setModuleState(
    handle: *ModuleHandle,
    slot: usize,
    ptr: *anyopaque,
    deinit_fn: StateDeinitFn,
) RuntimeError!void {
    if (!zigttpSdkSetModuleState(handle, slot, ptr, deinit_fn)) return error.OutOfMemory;
}
