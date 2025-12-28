//! ZQuickJS - High-performance Zig port of MicroQuickJS
//!
//! A performance-oriented JavaScript engine featuring:
//! - Generational GC with bump allocation
//! - SIMD-accelerated string operations
//! - Hidden classes and inline caching
//! - Optional baseline JIT compiler
//! - Lock-free runtime pooling
//!
//! ## Quick Start
//!
//! ```zig
//! const zq = @import("zquickjs");
//!
//! // Create a runtime pool
//! var pool = try zq.LockFreePool.init(allocator, .{});
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
pub const parser = @import("parser.zig");
pub const pool = @import("pool.zig");
pub const c_abi = @import("c_abi.zig");
pub const http = @import("http.zig");

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
pub const Atom = object.Atom;
pub const HiddenClass = object.HiddenClass;
pub const JSObject = object.JSObject;
pub const NativeFn = object.NativeFn;
pub const InlineCache = object.InlineCache;
pub const JSString = string.JSString;
pub const StringTable = string.StringTable;
pub const createString = string.createString;
pub const Parser = parser.Parser;

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

    return try Context.init(allocator, gc_state, .{});
}

/// Destroy a standalone context
pub fn destroyContext(ctx: *Context) void {
    const allocator = ctx.allocator;
    const gc_state = ctx.gc_state;
    ctx.deinit();
    gc_state.deinit();
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    const ctx = try createContext(allocator, .{ .nursery_size = 4096 });
    defer destroyContext(ctx);

    try std.testing.expect(ctx.sp == 0);
}
