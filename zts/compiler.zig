//! Parallel Compilation Driver
//!
//! Phase 15 of zts performance plan. Provides parallel compilation for
//! multi-file projects, using shared thread-safe InternPool for constant
//! deduplication across compilation units.
//!
//! Design:
//! - Per-thread arenas for parse/codegen (no cross-thread memory sharing)
//! - Thread-safe InternPool for constant deduplication
//! - Deterministic output regardless of thread scheduling
//! - Graceful error handling with per-file error attribution

const std = @import("std");
const parser = @import("parser/root.zig");
const bytecode = @import("bytecode.zig");
const value = @import("value.zig");
const string = @import("string.zig");
const context = @import("context.zig");
const intern_pool = @import("intern_pool.zig");

/// A single compilation unit (one source file)
pub const CompileUnit = struct {
    source: []const u8,
    filename: []const u8,
};

/// Result of compiling a single unit
pub const CompileResult = struct {
    /// Compiled bytecode (null if compilation failed)
    bytecode: ?*bytecode.FunctionBytecode,
    /// Source filename for error attribution
    filename: []const u8,
    /// Parse/compile errors (empty if successful)
    errors: []CompileError,
    /// Memory arena owning the bytecode and errors
    arena: ?*std.heap.ArenaAllocator,

    /// Error details
    pub const CompileError = struct {
        message: []const u8,
        line: u32,
        column: u32,
    };

    pub fn deinit(self: *CompileResult) void {
        if (self.arena) |arena| {
            arena.deinit();
            // Note: arena itself is allocated by Compiler.allocator
        }
    }

    pub fn isSuccess(self: *const CompileResult) bool {
        return self.bytecode != null and self.errors.len == 0;
    }
};

/// Parallel compilation driver using raw threads
pub const Compiler = struct {
    /// Backing allocator
    allocator: std.mem.Allocator,
    /// Thread-safe intern pool for shared constant storage
    shared_intern_pool: ?*intern_pool.ThreadSafeInternPool,
    /// Compilation results (protected by mutex)
    results: std.ArrayListUnmanaged(CompileResult),
    results_mutex: std.Thread.Mutex,
    /// Maximum thread count for parallel compilation
    max_threads: u32,

    /// Initialize compiler with given allocator and thread count
    /// If thread_count is 0 or null, uses number of CPU cores
    pub fn init(allocator: std.mem.Allocator, thread_count_opt: ?u32) Compiler {
        const thread_count = thread_count_opt orelse @as(u32, @intCast(std.Thread.getCpuCount() catch 4));

        return .{
            .allocator = allocator,
            .shared_intern_pool = null,
            .results = .{},
            .results_mutex = .{},
            .max_threads = thread_count,
        };
    }

    pub fn deinit(self: *Compiler) void {
        // Free all result arenas
        for (self.results.items) |*result| {
            if (result.arena) |arena| {
                arena.deinit();
                self.allocator.destroy(arena);
            }
        }
        self.results.deinit(self.allocator);
    }

    /// Set shared intern pool for constant deduplication
    pub fn setInternPool(self: *Compiler, pool: *intern_pool.ThreadSafeInternPool) void {
        self.shared_intern_pool = pool;
    }

    /// Compile multiple source files in parallel using raw threads
    /// Returns results in the same order as input units
    pub fn compileParallel(self: *Compiler, units: []const CompileUnit) ![]CompileResult {
        // Pre-allocate result slots
        self.results.clearRetainingCapacity();
        try self.results.resize(self.allocator, units.len);
        for (self.results.items) |*r| {
            r.* = .{
                .bytecode = null,
                .filename = "",
                .errors = &.{},
                .arena = null,
            };
        }

        // Determine thread count
        const thread_count = @min(self.max_threads, @as(u32, @intCast(units.len)));

        if (thread_count <= 1 or units.len <= 1) {
            // Sequential compilation
            for (units, 0..) |unit, i| {
                self.results.items[i] = self.compileOneInternal(unit);
            }
            return self.results.items;
        }

        // Parallel compilation with raw threads
        const threads = try self.allocator.alloc(std.Thread, thread_count);
        defer self.allocator.free(threads);

        // Work distribution: each thread processes a range of units
        const units_per_thread = (units.len + thread_count - 1) / thread_count;

        for (threads, 0..) |*t, thread_idx| {
            const start = thread_idx * units_per_thread;
            const end = @min(start + units_per_thread, units.len);

            if (start >= units.len) {
                // More threads than units
                t.* = std.Thread.spawn(.{}, noopWorker, .{}) catch {
                    // If spawn fails, continue with fewer threads
                    continue;
                };
            } else {
                t.* = std.Thread.spawn(.{}, workerThread, .{
                    self,
                    units[start..end],
                    start,
                }) catch {
                    // Fallback: compile this range sequentially
                    for (units[start..end], start..) |unit, i| {
                        self.results.items[i] = self.compileOneInternal(unit);
                    }
                    continue;
                };
            }
        }

        // Wait for all threads
        for (threads) |t| {
            t.join();
        }

        return self.results.items;
    }

    fn noopWorker() void {}

    fn workerThread(self: *Compiler, units: []const CompileUnit, start_idx: usize) void {
        for (units, 0..) |unit, i| {
            const result = self.compileOneInternal(unit);

            self.results_mutex.lock();
            defer self.results_mutex.unlock();
            self.results.items[start_idx + i] = result;
        }
    }

    /// Compile a single unit (public API)
    pub fn compileOne(self: *Compiler, unit: CompileUnit) CompileResult {
        return self.compileOneInternal(unit);
    }

    /// Internal compile function
    fn compileOneInternal(self: *Compiler, unit: CompileUnit) CompileResult {
        var result: CompileResult = .{
            .bytecode = null,
            .filename = unit.filename,
            .errors = &.{},
            .arena = null,
        };

        // Create per-unit arena
        const arena = self.allocator.create(std.heap.ArenaAllocator) catch {
            return result;
        };
        arena.* = std.heap.ArenaAllocator.init(self.allocator);
        result.arena = arena;
        const alloc = arena.allocator();

        // Create string table for this compilation
        var strings = string.StringTable.init(alloc);

        // Parse source
        var p = parser.Parser.init(alloc, unit.source, &strings, null);
        defer p.deinit();

        const code = p.parse() catch |err| {
            // Compilation error
            const errors = alloc.alloc(CompileResult.CompileError, 1) catch {
                return result;
            };
            errors[0] = .{
                .message = @errorName(err),
                .line = 0,
                .column = 0,
            };
            result.errors = errors;
            return result;
        };

        // Create bytecode struct
        const func = alloc.create(bytecode.FunctionBytecode) catch {
            return result;
        };
        func.* = .{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = @intCast(p.max_local_count),
            .stack_size = @intCast(p.max_local_count + 16),
            .flags = .{},
            .upvalue_count = 0,
            .upvalue_info = &.{},
            .code = code,
            .constants = p.constants.items,
            .source_map = null,
        };

        result.bytecode = func;
        return result;
    }
};

// ============================================================================
// Simple Single-Threaded Compile API
// ============================================================================

/// Compile source code to bytecode (simple API for single files)
pub fn compile(
    allocator: std.mem.Allocator,
    source: []const u8,
) !*bytecode.FunctionBytecode {
    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var p = parser.Parser.init(allocator, source, &strings, null);
    defer p.deinit();

    const code = try p.parse();

    // Copy code and constants to output allocator (parser memory is freed on deinit)
    const code_copy = try allocator.dupe(u8, code);
    errdefer allocator.free(code_copy);

    const constants_copy = try allocator.dupe(value.JSValue, p.constants.items);
    errdefer allocator.free(constants_copy);

    const func = try allocator.create(bytecode.FunctionBytecode);
    func.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = @intCast(p.max_local_count),
        .stack_size = @intCast(p.max_local_count + 16),
        .flags = .{},
        .upvalue_count = 0,
        .upvalue_info = &.{},
        .code = code_copy,
        .constants = constants_copy,
        .source_map = null,
    };

    return func;
}

/// Compile with options
pub const CompileOptions = struct {
    jsx_enabled: bool = false,
    strict_mode: bool = true,
    filename: []const u8 = "<eval>",
};

pub fn compileWithOptions(
    allocator: std.mem.Allocator,
    source: []const u8,
    options: CompileOptions,
) !*bytecode.FunctionBytecode {
    _ = options; // TODO: Pass to parser
    return compile(allocator, source);
}

// ============================================================================
// Tests
// ============================================================================

test "Compiler single unit" {
    const allocator = std.testing.allocator;

    var compiler = Compiler.init(allocator, 1);
    defer compiler.deinit();

    // Use 'let' instead of 'var' - zts is strict mode only
    var result = compiler.compileOne(.{
        .source = "let x = 1 + 2;",
        .filename = "test.js",
    });
    defer {
        // Clean up the arena manually since compileOne returns ownership to caller
        if (result.arena) |arena| {
            arena.deinit();
            allocator.destroy(arena);
        }
    }

    try std.testing.expect(result.isSuccess());
    try std.testing.expect(result.bytecode != null);
}

test "Compiler parallel compilation" {
    const allocator = std.testing.allocator;

    var compiler = Compiler.init(allocator, 2);
    defer compiler.deinit();

    // Use 'let' instead of 'var' - zts is strict mode only
    const units = [_]CompileUnit{
        .{ .source = "let a = 1;", .filename = "a.js" },
        .{ .source = "let b = 2;", .filename = "b.js" },
        .{ .source = "let c = 3;", .filename = "c.js" },
    };

    const results = try compiler.compileParallel(&units);

    // All should succeed
    for (results) |r| {
        try std.testing.expect(r.isSuccess());
    }

    // Results should be in order
    try std.testing.expectEqualStrings("a.js", results[0].filename);
    try std.testing.expectEqualStrings("b.js", results[1].filename);
    try std.testing.expectEqualStrings("c.js", results[2].filename);
}

test "Compiler error handling" {
    const allocator = std.testing.allocator;

    var compiler = Compiler.init(allocator, 1);
    defer compiler.deinit();

    var result = compiler.compileOne(.{
        .source = "let x = {{{", // Invalid syntax
        .filename = "bad.js",
    });
    defer {
        // Clean up the arena manually
        if (result.arena) |arena| {
            arena.deinit();
            allocator.destroy(arena);
        }
    }

    try std.testing.expect(!result.isSuccess());
    try std.testing.expect(result.errors.len > 0);
}

test "simple compile API" {
    const allocator = std.testing.allocator;

    // Use 'let' instead of 'var' - zts is strict mode only
    const func = try compile(allocator, "let x = 42;");
    defer {
        allocator.free(func.code);
        allocator.free(func.constants);
        allocator.destroy(func);
    }

    try std.testing.expect(func.code.len > 0);
}
