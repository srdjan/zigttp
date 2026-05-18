const value = @import("value.zig");

const JSValue = value.JSValue;

extern fn zigttpSdkIsCallable(val: JSValue) bool;

pub fn isCallable(val: JSValue) bool {
    return zigttpSdkIsCallable(val);
}
