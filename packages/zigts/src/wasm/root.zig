//! Phase 2 Wasm policy runtime - skeleton re-exports.

pub const interpreter = @import("interpreter.zig");
pub const pool = @import("pool.zig");
pub const loader = @import("loader.zig");

pub const WasmInterpreter = interpreter.WasmInterpreter;
pub const WasmPool = pool.WasmPool;
pub const loadWasmArtifact = loader.loadWasmArtifact;
