//! Wasm artifact loader - Phase 2 skeleton.
//!
//! Returns the raw bytes of a Wasm policy artifact from disk. Later phases
//! add WIT-component validation, checksum verification (spec section 14 -
//! sign/checksum step), and version pinning.

const std = @import("std");

/// Read raw Wasm bytes from `path`. Caller owns the returned slice and must
/// free it with `allocator.free`. Later phases layer WIT validation on top.
pub fn loadWasmArtifact(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    _ = allocator;
    _ = path;
    @panic("phase 2: not implemented");
}
