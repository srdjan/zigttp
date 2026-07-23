const std = @import("std");
const handles = @import("handle.zig");

const ModuleHandle = handles.ModuleHandle;
const RuntimeError = handles.RuntimeError;

pub const StateDeinitFn = *const fn (*anyopaque) callconv(.c) void;

extern fn zttpSdkGetAllocator(handle: *ModuleHandle) *const std.mem.Allocator;
extern fn zttpSdkGetModuleState(handle: *ModuleHandle, slot: usize) ?*anyopaque;
extern fn zttpSdkSetModuleState(handle: *ModuleHandle, slot: usize, ptr: *anyopaque, deinit_fn: StateDeinitFn) bool;

/// Borrow the runtime's general-purpose allocator. Valid for the module
/// call's lifetime.
pub fn getAllocator(handle: *ModuleHandle) std.mem.Allocator {
    return zttpSdkGetAllocator(handle).*;
}

/// Get typed module state from a slot owned by the current active module.
/// Returns null if the slot is uninitialized or not owned by that module.
pub fn getModuleState(handle: *ModuleHandle, comptime T: type, slot: usize) ?*T {
    const ptr = zttpSdkGetModuleState(handle, slot) orelse return null;
    return @ptrCast(@alignCast(ptr));
}

/// Install module state into a slot owned by the current active module with a
/// cleanup callback. The callback receives the state pointer on context
/// teardown; modules typically store their own allocator in the state struct
/// and free through it.
pub fn setModuleState(
    handle: *ModuleHandle,
    slot: usize,
    ptr: *anyopaque,
    deinit_fn: StateDeinitFn,
) RuntimeError!void {
    if (!zttpSdkSetModuleState(handle, slot, ptr, deinit_fn)) return error.OutOfMemory;
}
