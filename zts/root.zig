//! zts - Zig TypeScript compiler
//!
//! A performance-oriented JavaScript engine featuring:
//! - Generational GC with bump allocation
//! - SIMD-accelerated string operations
//! - Hidden classes and inline caching
//! - Lock-free runtime pooling
//!
//! ## Quick Start
//!
//! ```zig
//! const zts = @import("zts");
//!
//! // Create a runtime pool
//! var pool = try zts.LockFreePool.init(allocator, .{});
//! defer pool.deinit();
//!
//! // Acquire a runtime
//! const runtime = try pool.acquire();
//! defer pool.release(runtime);
//!
//! // Execute JavaScript
//! const result = runtime.ctx.eval("1 + 2");
//! ```

const std = @import("std");

// Core modules
pub const value = @import("value.zig");
pub const heap = @import("heap.zig");
pub const gc = @import("gc.zig");
pub const string = @import("string.zig");
pub const object = @import("object.zig");
pub const context = @import("context.zig");
pub const bytecode = @import("bytecode.zig");
pub const interpreter = @import("interpreter.zig");
pub const builtins = @import("builtins.zig");
// New two-pass parser with proper function compilation
pub const parser = @import("parser/root.zig");
// Note: Legacy single-pass parser removed; use parser/root.zig
pub const pool = @import("pool.zig");
pub const http = @import("http.zig");
pub const stripper = @import("stripper.zig");
pub const comptime_eval = @import("comptime.zig");
pub const intern_pool = @import("intern_pool.zig");
pub const bytecode_cache = @import("bytecode_cache.zig");
pub const bytecode_opt = @import("bytecode_opt.zig");
pub const arena = @import("arena.zig");
pub const handler_analyzer = @import("handler_analyzer.zig");
pub const modules = @import("modules/root.zig");
// Optional/experimental modules (not wired into runtime by default)
// These are kept in zts/ for future development but not exported publicly.
// To use: @import("zts/perf.zig") directly, or uncomment below.
// pub const perf = @import("perf.zig");
// pub const compiler = @import("compiler.zig");
pub const jit = @import("jit/root.zig");

// Re-export main types for convenience
pub const JSValue = value.JSValue;
pub const Context = context.Context;
pub const GC = gc.GC;
pub const GCConfig = gc.GCConfig;
pub const LockFreePool = pool.LockFreePool;
pub const Runtime = pool.LockFreePool.Runtime;
pub const Interpreter = interpreter.Interpreter;
pub const Opcode = bytecode.Opcode;
pub const FunctionBytecode = bytecode.FunctionBytecode;
pub const FunctionBytecodeCompact = bytecode.FunctionBytecodeCompact;
pub const Atom = object.Atom;
pub const HiddenClass = object.HiddenClass;
pub const HiddenClassIndex = object.HiddenClassIndex;
pub const HiddenClassPool = object.HiddenClassPool;
pub const JSObject = object.JSObject;
pub const NativeFn = object.NativeFn;
pub const InlineCache = object.InlineCache;
pub const JSString = string.JSString;
pub const StringTable = string.StringTable;
pub const createString = string.createString;
pub const Parser = parser.Parser;
pub const StripResult = stripper.StripResult;
pub const StripOptions = stripper.StripOptions;
pub const ComptimeEnv = stripper.ComptimeEnv;
pub const strip = stripper.strip;
pub const ComptimeEvaluator = comptime_eval.ComptimeEvaluator;
pub const ComptimeValue = comptime_eval.ComptimeValue;
pub const emitLiteral = comptime_eval.emitLiteral;
pub const InternPool = intern_pool.InternPool;
pub const InternPoolIndex = intern_pool.Index;
pub const BytecodeCache = bytecode_cache.BytecodeCache;
// Perf types (uncomment if needed - perf.zig is experimental)
// pub const PerfReport = perf.PerfReport;
// pub const CompileStats = perf.CompileStats;
// pub const RuntimeStats = perf.RuntimeStats;
// pub const GCStats = perf.GCStats;
// pub const OpcodeProfile = perf.OpcodeProfile;
// pub const Timer = perf.Timer;
pub const BytecodeOptimizer = bytecode_opt.BytecodeOptimizer;
pub const optimizeBytecode = bytecode_opt.optimizeBytecode;
pub const OptStats = bytecode_opt.OptStats;
pub const HandlerAnalyzer = handler_analyzer.HandlerAnalyzer;
pub const PatternDispatchTable = bytecode.PatternDispatchTable;
pub const HandlerPattern = bytecode.HandlerPattern;
pub const HandlerFlags = bytecode.HandlerFlags;

/// Version information
pub const version = struct {
    pub const major = 0;
    pub const minor = 1;
    pub const patch = 0;
    pub const string = "0.1.0";
};

/// Create a new standalone context (not pooled)
pub fn createContext(allocator: std.mem.Allocator, gc_config: GCConfig) !*Context {
    const gc_state = try allocator.create(GC);
    errdefer allocator.destroy(gc_state);

    gc_state.* = try GC.init(allocator, gc_config);
    errdefer gc_state.deinit();

    // Initialize heap for size-class allocation and wire up to GC
    const heap_state = try allocator.create(heap.Heap);
    errdefer allocator.destroy(heap_state);
    heap_state.* = heap.Heap.init(allocator, .{});
    gc_state.setHeap(heap_state);

    return try Context.init(allocator, gc_state, .{});
}

/// Destroy a standalone context
pub fn destroyContext(ctx: *Context) void {
    const allocator = ctx.allocator;
    const gc_state = ctx.gc_state;
    const heap_state = gc_state.heap_ptr;
    ctx.deinit();
    gc_state.deinit();
    if (heap_state) |h| {
        h.deinit();
        allocator.destroy(h);
    }
    allocator.destroy(gc_state);
}

// Run all module tests
test {
    std.testing.refAllDecls(@This());
}

test "version" {
    try std.testing.expectEqualStrings("0.1.0", version.string);
}

test "create and destroy context" {
    var test_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer test_arena.deinit();

    const allocator = test_arena.allocator();
    const ctx = try createContext(allocator, .{ .nursery_size = 4096 });
    defer destroyContext(ctx);

    try std.testing.expect(ctx.sp == 0);
}
