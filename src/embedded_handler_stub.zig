//! Stub embedded handler - used when building without -Dhandler
//! This allows tests and development builds to compile without a precompiled handler.

pub const bytecode: []const u8 = &.{};
