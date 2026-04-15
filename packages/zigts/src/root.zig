//! zigts - Zig TypeScript compiler
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
//! const zigts = @import("zigts");
//!
//! // Create a runtime pool
//! var pool = try zigts.LockFreePool.init(allocator, .{});
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
pub const builtins = @import("builtins/root.zig");
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
pub const handler_verifier = @import("handler_verifier.zig");
pub const handler_contract = @import("handler_contract.zig");
pub const handler_policy = @import("handler_policy.zig");
pub const bool_checker = @import("bool_checker.zig");
pub const flow_checker = @import("flow_checker.zig");
pub const path_generator = @import("path_generator.zig");
pub const behavior_canonical = @import("behavior_canonical.zig");
pub const fault_coverage = @import("fault_coverage.zig");
pub const property_diagnostics = @import("property_diagnostics.zig");
pub const route_match = @import("route_match.zig");
pub const type_map = @import("type_map.zig");
pub const type_pool = @import("type_pool.zig");
pub const type_env = @import("type_env.zig");
pub const service_types = @import("service_types.zig");
pub const type_checker = @import("type_checker.zig");
pub const bytecode_verifier = @import("bytecode_verifier.zig");
pub const trace = @import("trace.zig");
pub const file_io = @import("file_io.zig");
pub const module_slots = @import("module_slots.zig");
pub const contract_diff = @import("contract_diff.zig");
pub const system_linker = @import("system_linker.zig");
pub const rule_error = @import("rule_error.zig");
pub const rule_registry = @import("rule_registry.zig");
pub const module_binding = @import("module_binding.zig");
pub const builtin_modules = @import("builtin_modules.zig");
pub const security_events = @import("security_events.zig");
pub const sqlite = @import("sqlite.zig");
pub const sql_analysis = @import("sql_analysis.zig");
pub const modules = @import("modules/root.zig");
pub const compat = @import("compat.zig");
// Optional/experimental modules (not wired into runtime by default)
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
pub const BytecodeOptimizer = bytecode_opt.BytecodeOptimizer;
pub const optimizeBytecode = bytecode_opt.optimizeBytecode;
pub const OptStats = bytecode_opt.OptStats;
pub const HandlerAnalyzer = handler_analyzer.HandlerAnalyzer;
pub const HandlerVerifier = handler_verifier.HandlerVerifier;
pub const BoolChecker = bool_checker.BoolChecker;
pub const FlowChecker = flow_checker.FlowChecker;
pub const PathGenerator = path_generator.PathGenerator;
pub const TypeMap = type_map.TypeMap;
pub const TypeMapEntry = type_map.TypeMapEntry;
pub const TypeMapKind = type_map.TypeMapKind;
pub const TypePool = type_pool.TypePool;
pub const TypeIndex = type_pool.TypeIndex;
pub const null_type_idx = type_pool.null_type_idx;
pub const parseTypeExpr = type_pool.parseTypeExpr;
pub const TypeEnv = type_env.TypeEnv;
pub const TypeChecker = type_checker.TypeChecker;
pub const BytecodeVerifier = bytecode_verifier;
pub const ContractBuilder = handler_contract.ContractBuilder;
pub const HandlerContract = handler_contract.HandlerContract;
pub const writeContractJson = handler_contract.writeContractJson;
pub const HandlerPolicy = handler_policy.HandlerPolicy;
pub const RuntimePolicy = handler_policy.RuntimePolicy;
pub const PatternDispatchTable = bytecode.PatternDispatchTable;
pub const HandlerPattern = bytecode.HandlerPattern;
pub const HandlerFlags = bytecode.HandlerFlags;
pub const TraceRecorder = trace.TraceRecorder;
pub const TRACE_STATE_SLOT = trace.TRACE_STATE_SLOT;

/// Version information
pub const version = struct {
    pub const major = 0;
    pub const minor = 16;
    pub const patch = 0;
    pub const string = "0.16.0";
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
    try std.testing.expectEqualStrings("0.16.0", version.string);
}

test "create and destroy context" {
    var test_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer test_arena.deinit();

    const allocator = test_arena.allocator();
    const ctx = try createContext(allocator, .{ .nursery_size = 4096 });
    defer destroyContext(ctx);

    try std.testing.expect(ctx.sp == 0);
}
