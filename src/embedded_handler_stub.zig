//! Stub embedded handler - used when building without -Dhandler
//! This allows tests and development builds to compile without a precompiled handler.

const zq = @import("zts");

pub const has_aot = false;
pub const bytecode: []const u8 = &.{};
pub const dep_count: u16 = 0;
pub const dep_bytecodes = [_][]const u8{};

pub fn aotHandler(_: *zq.Context, _: []const zq.JSValue) anyerror!zq.JSValue {
    return error.AotBail;
}
