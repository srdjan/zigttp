const value = @import("value.zig");

const JSValue = value.JSValue;

extern fn zttpSdkIsCallable(val: JSValue) bool;

pub fn isCallable(val: JSValue) bool {
    return zttpSdkIsCallable(val);
}
