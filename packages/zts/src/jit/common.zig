//! Primitives shared by the baseline and optimized JIT tiers.
//!
//! The two tiers deliberately keep their own specialized emit methods (the
//! optimized tier elides stack checks, specializes by type, and allocates
//! registers differently), so this module is intentionally small: it holds only
//! the genuinely tier-independent helpers. Previously `readU16` was duplicated
//! across both files with two different (but equivalent) implementations.

const std = @import("std");

/// Decode a little-endian u16 from the bytecode stream at `offset`.
pub fn readU16(code: []const u8, offset: u32) u16 {
    return std.mem.readInt(u16, code[offset..][0..2], .little);
}
