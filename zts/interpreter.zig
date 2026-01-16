//! Bytecode interpreter with computed goto dispatch
//!
//! Threaded code execution with tail calls for opcode handlers.
//! Implements all opcodes from bytecode.zig.

const std = @import("std");
const value = @import("value.zig");
const bytecode = @import("bytecode.zig");
const context = @import("context.zig");
const heap = @import("heap.zig");
const object = @import("object.zig");
const string = @import("string.zig");
const jit = @import("jit/root.zig");

const empty_code: [0]u8 = .{};

pub threadlocal var current_interpreter: ?*Interpreter = null;

var jit_disabled_cache: ?bool = null;
var call_trace_cache: ?bool = null;
var call_trace_limit_cache: usize = 0;
var call_trace_limit_cached = false;
var call_guard_cache: usize = 0;
var call_guard_cached = false;
var call_trace_count: usize = 0;

// ============================================================================
// JIT Policy Configuration
// ============================================================================

/// JIT compilation policy for FaaS-aware optimization.
pub const JitPolicy = enum {
    /// Never JIT compile - pure interpreter mode (fastest cold start)
    disabled,
    /// Default: JIT after threshold (balanced)
    lazy,
    /// Lower threshold for faster warmup (more aggressive)
    eager,
};

/// Current JIT policy (defaults to lazy, overridable via env or API)
var jit_policy_cache: ?JitPolicy = null;

/// Current JIT threshold (defaults to bytecode.JIT_THRESHOLD, overridable via env or API)
var jit_threshold_cache: ?u32 = null;

/// Get current JIT policy (cached, reads ZTS_JIT_POLICY env on first call)
pub fn getJitPolicy() JitPolicy {
    if (jit_policy_cache) |cached| return cached;
    if (std.c.getenv("ZTS_JIT_POLICY")) |policy_ptr| {
        const policy_str = std.mem.sliceTo(policy_ptr, 0);
        const policy = std.meta.stringToEnum(JitPolicy, policy_str) orelse .lazy;
        jit_policy_cache = policy;
        return policy;
    }
    jit_policy_cache = .lazy;
    return .lazy;
}

/// Get current JIT threshold (cached, reads ZTS_JIT_THRESHOLD env on first call)
pub fn getJitThreshold() u32 {
    if (jit_threshold_cache) |cached| return cached;
    if (std.c.getenv("ZTS_JIT_THRESHOLD")) |threshold_ptr| {
        const threshold_str = std.mem.sliceTo(threshold_ptr, 0);
        const threshold = std.fmt.parseInt(u32, threshold_str, 10) catch bytecode.JIT_THRESHOLD;
        jit_threshold_cache = threshold;
        return threshold;
    }
    // Eager policy uses lower threshold
    const policy = getJitPolicy();
    const threshold = switch (policy) {
        .disabled => std.math.maxInt(u32), // Never reached
        .lazy => bytecode.JIT_THRESHOLD,
        .eager => bytecode.JIT_THRESHOLD / 4, // 25 calls instead of 100
    };
    jit_threshold_cache = threshold;
    return threshold;
}

/// Set JIT policy programmatically (overrides env var)
pub fn setJitPolicy(policy: JitPolicy) void {
    jit_policy_cache = policy;
    jit_threshold_cache = null; // Reset threshold to recalculate based on policy
}

/// Set JIT threshold programmatically (overrides env var and policy default)
pub fn setJitThreshold(threshold: u32) void {
    jit_threshold_cache = threshold;
}

/// Force-disable JIT in the current process (used by tests that exercise
/// multithreaded runtime behavior without JIT stability guarantees yet).
pub fn disableJitForTests() void {
    jit_disabled_cache = true;
    jit_policy_cache = .disabled;
}

fn jitDisabled() bool {
    // Check policy first
    if (getJitPolicy() == .disabled) return true;
    // Then check legacy env var
    if (jit_disabled_cache) |cached| return cached;
    const disabled = std.c.getenv("ZTS_DISABLE_JIT") != null;
    jit_disabled_cache = disabled;
    return disabled;
}

fn callTraceEnabled() bool {
    if (call_trace_cache) |cached| return cached;
    const enabled = std.c.getenv("ZTS_TRACE_CALLS") != null;
    call_trace_cache = enabled;
    return enabled;
}

fn callTraceLimit() usize {
    if (call_trace_limit_cached) return call_trace_limit_cache;
    const default_limit: usize = 200;
    if (std.c.getenv("ZTS_TRACE_CALLS_LIMIT")) |raw_ptr| {
        const raw = std.mem.sliceTo(raw_ptr, 0);
        const parsed = std.fmt.parseUnsigned(usize, raw, 10) catch default_limit;
        call_trace_limit_cache = if (parsed == 0) default_limit else parsed;
    } else {
        call_trace_limit_cache = default_limit;
    }
    call_trace_limit_cached = true;
    return call_trace_limit_cache;
}

fn callGuardDepth() usize {
    if (call_guard_cached) return call_guard_cache;
    if (std.c.getenv("ZTS_CALL_GUARD")) |raw_ptr| {
        const raw = std.mem.sliceTo(raw_ptr, 0);
        const parsed = std.fmt.parseUnsigned(usize, raw, 10) catch 0;
        call_guard_cache = parsed;
    } else {
        call_guard_cache = 0;
    }
    call_guard_cached = true;
    return call_guard_cache;
}

fn traceCall(self: *Interpreter, label: []const u8, argc: u8, is_method: bool) void {
    if (!callTraceEnabled()) return;
    const limit = callTraceLimit();
    if (call_trace_count >= limit) return;
    call_trace_count += 1;
    std.debug.print(
        "[call] {s} depth={} sp={} fp={} argc={} method={}\n",
        .{ label, self.ctx.call_depth, self.ctx.sp, self.ctx.fp, argc, @intFromBool(is_method) },
    );
}

fn traceTypeError(self: *Interpreter, label: []const u8, a: value.JSValue, b: value.JSValue) void {
    if (!callTraceEnabled()) return;
    std.debug.print(
        "[typeerror] {s} a_type={s} a={} b_type={s} b={} depth={} sp={} fp={}\n",
        .{ label, a.typeOf(), a, b.typeOf(), b, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
    );
    if (self.current_func) |cur| {
        const pc_off = @as(usize, @intCast(@intFromPtr(self.pc) - @intFromPtr(cur.code.ptr)));
        std.debug.print("[typeerror] pc_off={} last_op={s}\n", .{ pc_off, @tagName(self.last_op) });
        const op_off = if (pc_off > 0) pc_off - 1 else 0;
        traceBytecodeWindow(self, op_off);
    }
}

fn traceLastOp(self: *Interpreter, label: []const u8) void {
    if (!callTraceEnabled()) return;
    if (self.current_func) |cur| {
        const pc_off = @as(usize, @intCast(@intFromPtr(self.pc) - @intFromPtr(cur.code.ptr)));
        std.debug.print(
            "[typeerror] {s} op={s} pc_off={} depth={} sp={} fp={}\n",
            .{ label, @tagName(self.last_op), pc_off, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
        );
        const op_off = if (pc_off > 0) pc_off - 1 else 0;
        traceBytecodeWindow(self, op_off);
    } else {
        std.debug.print(
            "[typeerror] {s} op={s} depth={} sp={} fp={}\n",
            .{ label, @tagName(self.last_op), self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
        );
    }
}

fn traceBytecodeWindow(self: *Interpreter, center_off: usize) void {
    if (!callTraceEnabled()) return;
    const cur = self.current_func orelse return;
    const code = cur.code;
    if (code.len == 0) return;
    const full = std.c.getenv("ZTS_TRACE_BC_FULL") != null;
    const window: usize = 12;
    const start = if (full) 0 else if (center_off > window) center_off - window else 0;
    const end = if (full) code.len else @min(code.len, center_off + window);
    var pos: usize = start;
    while (pos < end) {
        const op: bytecode.Opcode = @enumFromInt(code[pos]);
        const info = bytecode.getOpcodeInfo(op);
        std.debug.print("[bytecode] +{} {s}\n", .{ pos, @tagName(op) });
        if (info.size == 0) break;
        pos += info.size;
    }
}

/// Single entry in a polymorphic inline cache
/// Stores hidden class index and slot offset for one observed shape
const PICEntry = struct {
    hidden_class_idx: object.HiddenClassIndex,
    slot_offset: u16,
};

/// Number of entries in a polymorphic inline cache
/// 4 entries provides good coverage for common polymorphic patterns
/// while keeping memory overhead reasonable (40 bytes per IC site)
pub const PIC_ENTRIES = 4;

/// Polymorphic Inline Cache for property access optimization
/// Caches up to 4 (hidden_class, slot_offset) pairs per access site
/// Falls back to megamorphic mode when more than 4 shapes are observed
pub const PolymorphicInlineCache = struct {
    /// Cached entries (only first `count` are valid)
    entries: [PIC_ENTRIES]PICEntry = undefined,
    /// Number of valid entries (0-4)
    count: u8 = 0,
    /// Megamorphic flag: true when > PIC_ENTRIES shapes observed
    /// When megamorphic, skip caching entirely (too polymorphic to benefit)
    megamorphic: bool = false,

    /// Lookup hidden class in cache, return slot offset if found
    pub inline fn lookup(self: *const PolymorphicInlineCache, hidden_class_idx: object.HiddenClassIndex) ?u16 {
        // Linear search through valid entries
        for (self.entries[0..self.count]) |entry| {
            if (entry.hidden_class_idx == hidden_class_idx) {
                return entry.slot_offset;
            }
        }
        return null;
    }

    /// Update cache with new (hidden_class_idx, slot_offset) pair
    /// Returns true if entry was added, false if megamorphic
    pub inline fn update(self: *PolymorphicInlineCache, hidden_class_idx: object.HiddenClassIndex, slot_offset: u16) bool {
        if (self.megamorphic) return false;

        // Check if already cached (update existing entry)
        for (self.entries[0..self.count]) |*entry| {
            if (entry.hidden_class_idx == hidden_class_idx) {
                entry.slot_offset = slot_offset;
                return true;
            }
        }

        // Add new entry if space available
        if (self.count < PIC_ENTRIES) {
            self.entries[self.count] = .{
                .hidden_class_idx = hidden_class_idx,
                .slot_offset = slot_offset,
            };
            self.count += 1;
            return true;
        }

        // Cache full with new shape: transition to megamorphic
        self.megamorphic = true;
        return false;
    }

    /// Reset cache to initial state (for debugging/testing)
    pub fn reset(self: *PolymorphicInlineCache) void {
        self.count = 0;
        self.megamorphic = false;
    }
};

/// Legacy monomorphic inline cache entry (kept for reference)
/// Caches the hidden class pointer and slot offset for fast subsequent lookups
pub const InlineCacheEntry = struct {
    /// Cached hidden class pointer (null = cache miss/uninitialized)
    hidden_class: ?*object.HiddenClass = null,
    /// Cached slot offset for direct property access
    slot_offset: u16 = 0,
};

/// Maximum number of inline cache slots per function
/// Each get_field_ic/put_field_ic instruction references a cache index
pub const IC_CACHE_SIZE = 256;

/// Interpreter state
pub const Interpreter = struct {
    const MAX_STATE_DEPTH = 1024;
    const SavedState = struct {
        pc: [*]const u8,
        code_end: [*]const u8,
        constants: []const value.JSValue,
        current_func: ?*const bytecode.FunctionBytecode,
        sp: usize,
        fp: usize,
        call_depth: usize,
        catch_depth: usize,
        exception: value.JSValue,
    };

    ctx: *context.Context,
    pc: [*]const u8, // Program counter
    code_end: [*]const u8,
    constants: []const value.JSValue, // Constant pool (direct JSValue array)
    current_func: ?*const bytecode.FunctionBytecode,
    current_closure: ?*object.ClosureData, // Current closure (if executing closure)
    open_upvalues: ?*object.Upvalue, // Linked list of open upvalues
    state_stack: [MAX_STATE_DEPTH]SavedState,
    state_depth: usize,
    /// Polymorphic inline cache for property access optimization
    /// Indexed by cache_idx from get_field_ic/put_field_ic instructions
    /// Each entry can cache up to 4 (hidden_class, slot_offset) pairs
    pic_cache: [IC_CACHE_SIZE]PolymorphicInlineCache,

    // JIT profiling counters (Phase 11)
    backedge_count: u32 = 0, // Back-edge counter for hot loop detection
    pic_hits: u32 = 0, // PIC cache hits (type feedback)
    pic_misses: u32 = 0, // PIC cache misses (type feedback)
    last_op: bytecode.Opcode = .nop,

    pub fn init(ctx: *context.Context) Interpreter {
        return .{
            .ctx = ctx,
            .pc = @ptrCast(&empty_code),
            .code_end = @ptrCast(&empty_code),
            .constants = &.{},
            .current_func = null,
            .current_closure = null,
            .open_upvalues = null,
            .state_stack = undefined,
            .state_depth = 0,
            .pic_cache = [_]PolymorphicInlineCache{.{}} ** IC_CACHE_SIZE,
            .backedge_count = 0,
            .pic_hits = 0,
            .pic_misses = 0,
            .last_op = .nop,
        };
    }

    /// Profile function entry: increment execution count, check for JIT threshold
    /// Returns true if function should be promoted to JIT candidate
    inline fn profileFunctionEntry(func: *bytecode.FunctionBytecode) bool {
        // Use non-atomic increment for single-threaded interpreter
        func.execution_count +%= 1;
        if (jitDisabled()) return false;
        if (func.execution_count == getJitThreshold() and func.tier == .interpreted) {
            func.tier = .baseline_candidate;
            return true;
        }
        return false;
    }

    /// Profile back-edge: increment counter, return true if loop is hot
    inline fn profileBackedge(self: *Interpreter) bool {
        self.backedge_count +%= 1;
        return self.backedge_count >= bytecode.LOOP_THRESHOLD;
    }

    /// Reset profiling counters (for testing or new function)
    pub fn resetProfilingCounters(self: *Interpreter) void {
        self.backedge_count = 0;
        self.pic_hits = 0;
        self.pic_misses = 0;
    }

    /// Try to compile a function using the baseline JIT compiler.
    /// On success, stores compiled code in func.compiled_code and sets tier to .baseline.
    /// On UnsupportedOpcode, marks the function as interpreted (won't retry).
    /// Other errors are propagated.
    fn tryCompileBaseline(self: *Interpreter, func: *bytecode.FunctionBytecode) !void {
        // Get or create the JIT code allocator
        const code_alloc = try self.ctx.getOrCreateCodeAllocator();

        var timer: ?std.time.Timer = null;
        if (context.enable_jit_metrics) {
            timer = std.time.Timer.start() catch null;
        }

        // Try to compile
        const compiled = jit.compileFunction(self.ctx.allocator, code_alloc, func) catch |err| {
            switch (err) {
                jit.CompileError.UnsupportedOpcode => {
                    // Function uses opcodes we can't compile yet - stay interpreted
                    func.tier = .interpreted;
                    return;
                },
                else => return err,
            }
        };

        if (timer) |*t| {
            self.ctx.recordJitCompile(t.read(), compiled.code.len);
        }

        // Allocate CompiledCode struct on heap and store it
        const compiled_ptr = try self.ctx.allocator.create(jit.CompiledCode);
        compiled_ptr.* = compiled;
        func.compiled_code = compiled_ptr;
        func.tier = .baseline;
    }

    /// Offset the program counter by a signed value
    /// Consolidates the verbose type-casting pattern used throughout dispatch
    inline fn offsetPc(self: *Interpreter, offset: i16) void {
        self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc))) + offset)));
    }

    /// Run bytecode function
    pub fn run(self: *Interpreter, func: *const bytecode.FunctionBytecode) InterpreterError!value.JSValue {
        // Cast away const for profiling/JIT - safe because we only modify profiling fields
        const func_mut = @constCast(func);

        // Profile function entry and potentially trigger JIT compilation
        const is_candidate = profileFunctionEntry(func_mut);
        if (is_candidate) {
            self.tryCompileBaseline(func_mut) catch {
                // Compilation failed - continue with interpreter
            };
        }

        // Allocate space for locals
        const local_count = func.local_count;
        try self.ctx.ensureStack(local_count);
        for (0..local_count) |_| {
            try self.ctx.push(value.JSValue.undefined_val);
        }

        // Check if function is JIT-compiled and execute via JIT
        if (!jitDisabled() and func.tier == .baseline) {
            if (func.compiled_code) |cc_opaque| {
                const cc: *jit.CompiledCode = @ptrCast(@alignCast(cc_opaque));
                const prev_interp = current_interpreter;
                current_interpreter = self;
                defer current_interpreter = prev_interp;
                const result_raw = cc.execute(self.ctx);
                return value.JSValue{ .raw = result_raw };
            }
        }

        // Fall back to interpreter
        self.pc = func.code.ptr;
        self.code_end = func.code.ptr + func.code.len;
        self.constants = func.constants;
        self.current_func = func;

        return self.dispatch() catch |err| {
            if (err == error.TypeError) traceLastOp(self, "run");
            return err;
        };
    }

    fn pushState(self: *Interpreter) InterpreterError!void {
        if (self.state_depth >= MAX_STATE_DEPTH) {
            return error.CallStackOverflow;
        }
        self.state_stack[self.state_depth] = .{
            .pc = self.pc,
            .code_end = self.code_end,
            .constants = self.constants,
            .current_func = self.current_func,
            .sp = self.ctx.sp,
            .fp = self.ctx.fp,
            .call_depth = self.ctx.call_depth,
            .catch_depth = self.ctx.catch_depth,
            .exception = self.ctx.exception,
        };
        self.state_depth += 1;
    }

    fn popState(self: *Interpreter) void {
        std.debug.assert(self.state_depth > 0);
        self.state_depth -= 1;
        const state = self.state_stack[self.state_depth];
        self.pc = state.pc;
        self.code_end = state.code_end;
        self.constants = state.constants;
        self.current_func = state.current_func;
        self.ctx.sp = state.sp;
        self.ctx.fp = state.fp;
        self.ctx.call_depth = state.call_depth;
        self.ctx.catch_depth = state.catch_depth;
        self.ctx.exception = state.exception;
    }

    /// Capture a local variable as an upvalue
    /// Reuses existing open upvalue if one exists, otherwise creates new one
    fn captureUpvalue(self: *Interpreter, local_idx: u8) !*object.Upvalue {
        // Get pointer to the local slot
        const local_ptr = self.ctx.getLocalPtr(local_idx);

        // Search for existing open upvalue pointing to this slot
        var prev: ?*object.Upvalue = null;
        var current = self.open_upvalues;
        while (current) |uv| {
            switch (uv.location) {
                .open => |ptr| {
                    if (ptr == local_ptr) {
                        // Found existing upvalue for this slot
                        return uv;
                    }
                    // Upvalues are ordered by slot address (higher addresses first)
                    // If we've passed the slot, we need to insert here
                    if (@intFromPtr(ptr) < @intFromPtr(local_ptr)) {
                        break;
                    }
                },
                .closed => {},
            }
            prev = uv;
            current = uv.next;
        }

        // Create new open upvalue from pool
        const new_uv = try self.ctx.gc_state.acquireUpvalue();
        new_uv.* = object.Upvalue.init(local_ptr);

        // Insert into linked list
        if (prev) |p| {
            new_uv.next = p.next;
            p.next = new_uv;
        } else {
            new_uv.next = self.open_upvalues;
            self.open_upvalues = new_uv;
        }

        return new_uv;
    }

    /// Close all open upvalues that reference slots at or above the given index
    fn closeUpvaluesAbove(self: *Interpreter, local_idx: u8) void {
        const threshold = self.ctx.getLocalPtr(local_idx);

        while (self.open_upvalues) |uv| {
            switch (uv.location) {
                .open => |ptr| {
                    if (@intFromPtr(ptr) < @intFromPtr(threshold)) {
                        // This upvalue is below the threshold, stop
                        break;
                    }
                    // Close this upvalue
                    uv.close();
                    self.open_upvalues = uv.next;
                },
                .closed => {
                    // Already closed, remove from list
                    self.open_upvalues = uv.next;
                },
            }
        }
    }

    /// Execute a bytecode function with given arguments and return the result.
    ///
    /// This function handles the full lifecycle of a JavaScript function call:
    /// 1. Saves current interpreter state (pc, code_end, constants)
    /// 2. Pushes a new call frame onto the context's call stack
    /// 3. Sets up local variables from arguments (undefined for missing args)
    /// 4. Executes the function's bytecode via dispatch()
    /// 5. Restores state and returns the result value
    ///
    /// Closures: If func_val is a closure, the caller should have already set up
    /// upvalue access. The function uses the context's scope chain for upvalues.
    ///
    /// Errors: Returns InterpreterError on stack overflow, type errors, or
    /// unhandled exceptions. The call frame is always cleaned up via errdefer.
    pub fn callBytecodeFunction(
        self: *Interpreter,
        func_val: value.JSValue,
        func_bc: *const bytecode.FunctionBytecode,
        this_val: value.JSValue,
        args: []const value.JSValue,
    ) InterpreterError!value.JSValue {
        traceCall(self, "bc enter", @intCast(args.len), false);
        defer traceCall(self, "bc exit", @intCast(args.len), false);
        // Cast away const for profiling/JIT - safe because we only modify profiling fields
        const func_bc_mut = @constCast(func_bc);

        // Profile function entry and potentially trigger JIT compilation
        const is_candidate = profileFunctionEntry(func_bc_mut);
        if (is_candidate) {
            // Function hit JIT threshold - try to compile
            self.tryCompileBaseline(func_bc_mut) catch {
                // Compilation failed (other than UnsupportedOpcode) - continue with interpreter
            };
        }

        try self.pushState();
        defer self.popState();

        // Push call frame
        try self.ctx.pushFrame(func_val, this_val, @intFromPtr(self.pc));
        errdefer {
            self.closeUpvaluesAbove(0);
            _ = self.ctx.popFrame();
        }

        // Set up new function's locals with arguments
        const local_count = func_bc.local_count;
        try self.ctx.ensureStack(local_count);

        var local_idx: usize = 0;
        while (local_idx < local_count) : (local_idx += 1) {
            if (local_idx < args.len) {
                try self.ctx.push(args[local_idx]);
            } else {
                try self.ctx.push(value.JSValue.undefined_val);
            }
        }

        // Check if function is JIT-compiled and execute via JIT
        if (!jitDisabled() and func_bc.tier == .baseline) {
            if (func_bc.compiled_code) |cc_opaque| {
                const cc: *jit.CompiledCode = @ptrCast(@alignCast(cc_opaque));
                const prev_interp = current_interpreter;
                current_interpreter = self;
                defer current_interpreter = prev_interp;
                const result_raw = cc.execute(self.ctx);
                const result = value.JSValue{ .raw = result_raw };

                self.closeUpvaluesAbove(0);
                _ = self.ctx.popFrame();
                return result;
            }
        }

        // Fall back to interpreter
        self.pc = func_bc.code.ptr;
        self.code_end = func_bc.code.ptr + func_bc.code.len;
        self.constants = func_bc.constants;
        self.current_func = func_bc;

        const result = self.dispatch() catch |err| {
            if (err == error.TypeError) traceLastOp(self, "dispatch");
            return err;
        };

        self.closeUpvaluesAbove(0);
        _ = self.ctx.popFrame();
        return result;
    }

    /// Error set for interpreter operations
    pub const InterpreterError = error{
        StackOverflow,
        CallStackOverflow,
        TypeError,
        TooManyArguments,
        InvalidConstant,
        NotCallable,
        NativeFunctionError,
        UnimplementedOpcode,
        IntegerOverflow,
        DivisionByZero,
        OutOfMemory,
        NoTransition,
        NoRootClass,
        ArenaObjectEscape, // Arena object stored into persistent object
        NoHiddenClassPool,
    };

    /// Main bytecode dispatch loop - executes opcodes until halt, return, or error.
    ///
    /// This is the core interpreter loop that executes JavaScript bytecode. It uses
    /// a switch-based dispatch (Zig optimizes this to computed goto on supported platforms).
    ///
    /// Execution model:
    /// - Fetches opcode at pc, increments pc, then executes via switch
    /// - Stack-based: operands popped from stack, results pushed
    /// - Immediate values: small constants encoded inline after opcode
    /// - Constants: large values referenced by index into constants array
    ///
    /// Control flow:
    /// - ret: Returns top of stack value, exits dispatch loop
    /// - halt: Returns top of stack or undefined, exits dispatch loop
    /// - goto/goto_if_false: Absolute jump by modifying pc
    /// - call/call_method: Recursive dispatch via callBytecodeFunction
    ///
    /// Public because native callbacks may need to call back into JS.
    pub fn dispatch(self: *Interpreter) InterpreterError!value.JSValue {
        @setEvalBranchQuota(10000);
        dispatch: while (@intFromPtr(self.pc) < @intFromPtr(self.code_end)) {
            const op: bytecode.Opcode = @enumFromInt(self.pc[0]);
            self.pc += 1;
            self.last_op = op;

            switch (op) {
                // ========================================
                // Stack Operations
                // ========================================
                .nop => {},

                .halt => {
                    // End of script - return last value on stack or undefined
                    if (self.ctx.sp > 0) {
                        return self.ctx.pop();
                    }
                    return value.JSValue.undefined_val;
                },

                .push_0 => try self.ctx.push(value.JSValue.fromInt(0)),
                .push_1 => try self.ctx.push(value.JSValue.fromInt(1)),
                .push_2 => try self.ctx.push(value.JSValue.fromInt(2)),
                .push_3 => try self.ctx.push(value.JSValue.fromInt(3)),
                .push_null => try self.ctx.push(value.JSValue.null_val),
                .push_undefined => try self.ctx.push(value.JSValue.undefined_val),
                .push_true => try self.ctx.push(value.JSValue.true_val),
                .push_false => try self.ctx.push(value.JSValue.false_val),

                .push_i8 => {
                    const val: i8 = @bitCast(self.pc[0]);
                    self.pc += 1;
                    try self.ctx.push(value.JSValue.fromInt(val));
                },

                .push_i16 => {
                    const val = readI16(self.pc);
                    self.pc += 2;
                    try self.ctx.push(value.JSValue.fromInt(val));
                },

                .push_const => {
                    const idx = readU16(self.pc);
                    self.pc += 2;
                    try self.ctx.push(try self.getConstant(idx));
                },

                .dup => {
                    const top = self.ctx.peek();
                    try self.ctx.push(top);
                },

                .drop => _ = self.ctx.pop(),

                .swap => {
                    self.ctx.swap2();
                },

                .rot3 => {
                    self.ctx.rot3();
                },

                .get_length => {
                    // Optimized - modify stack in place
                    const sp = self.ctx.sp;
                    const obj_val = self.ctx.stack[sp - 1];
                    if (obj_val.isObject()) {
                        const obj = object.JSObject.fromValue(obj_val);
                        if (obj.class_id == .array) {
                            self.ctx.stack[sp - 1] = obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH];
                            continue :dispatch;
                        } else if (obj.class_id == .range_iterator) {
                            self.ctx.stack[sp - 1] = obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH];
                            continue :dispatch;
                        } else {
                            // Fallback to property lookup
                            const pool = self.ctx.hidden_class_pool orelse {
                                self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                                continue :dispatch;
                            };
                            if (obj.getProperty(pool, .length)) |len| {
                                self.ctx.stack[sp - 1] = len;
                            } else {
                                self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                            }
                            continue :dispatch;
                        }
                    } else if (obj_val.isString()) {
                        const str = obj_val.toPtr(string.JSString);
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(@intCast(str.len));
                    } else {
                        self.ctx.stack[sp - 1] = value.JSValue.undefined_val;
                    }
                },

                .dup2 => {
                    // Duplicate top 2 stack values: [a, b] -> [a, b, a, b]
                    const b = self.ctx.peekAt(0);
                    const a = self.ctx.peekAt(1);
                    try self.ctx.push(a);
                    try self.ctx.push(b);
                },

                // ========================================
                // Local Variables
                // ========================================
                .get_loc => {
                    const idx = self.pc[0];
                    self.pc += 1;
                    try self.ctx.push(self.ctx.getLocal(idx));
                },

                .put_loc => {
                    const idx = self.pc[0];
                    self.pc += 1;
                    const val = self.ctx.pop();
                    self.ctx.setLocal(idx, val);
                },

                .get_loc_0 => try self.ctx.push(self.ctx.getLocal(0)),
                .get_loc_1 => try self.ctx.push(self.ctx.getLocal(1)),
                .get_loc_2 => try self.ctx.push(self.ctx.getLocal(2)),
                .get_loc_3 => try self.ctx.push(self.ctx.getLocal(3)),

                .put_loc_0 => self.ctx.setLocal(0, self.ctx.pop()),
                .put_loc_1 => self.ctx.setLocal(1, self.ctx.pop()),
                .put_loc_2 => self.ctx.setLocal(2, self.ctx.pop()),
                .put_loc_3 => self.ctx.setLocal(3, self.ctx.pop()),

                // ========================================
                // Arithmetic - optimized direct stack access
                // ========================================
                .add => {
                    // Direct stack manipulation avoids push bounds check
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    // Inline integer fast path
                    if (a.isInt() and b.isInt()) {
                        @branchHint(.likely);
                        const ai = a.getInt();
                        const bi = b.getInt();
                        const result = @addWithOverflow(ai, bi);
                        if (result[1] == 0) {
                            @branchHint(.likely);
                            self.ctx.stack[sp - 2] = value.JSValue.fromInt(result[0]);
                            self.ctx.sp = sp - 1;
                            continue :dispatch;
                        }
                        // Integer overflow - convert to float
                        self.ctx.stack[sp - 2] = try self.allocFloat(@as(f64, @floatFromInt(ai)) + @as(f64, @floatFromInt(bi)));
                        self.ctx.sp = sp - 1;
                        continue :dispatch;
                    } else {
                        // Slow path for strings/floats
                        @branchHint(.cold);
                        self.ctx.sp = sp - 2;
                        self.ctx.pushUnchecked(try self.addValuesSlow(a, b));
                    }
                },

                .sub => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    if (a.isInt() and b.isInt()) {
                        @branchHint(.likely);
                        const ai = a.getInt();
                        const bi = b.getInt();
                        const result = @subWithOverflow(ai, bi);
                        if (result[1] == 0) {
                            @branchHint(.likely);
                            self.ctx.stack[sp - 2] = value.JSValue.fromInt(result[0]);
                            self.ctx.sp = sp - 1;
                            continue :dispatch;
                        }
                        // Integer overflow - convert to float
                        self.ctx.stack[sp - 2] = try self.allocFloat(@as(f64, @floatFromInt(ai)) - @as(f64, @floatFromInt(bi)));
                        self.ctx.sp = sp - 1;
                        continue :dispatch;
                    } else {
                        @branchHint(.cold);
                        self.ctx.sp = sp - 2;
                        self.ctx.pushUnchecked(try self.subValuesSlow(a, b));
                    }
                },

                .mul => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    if (a.isInt() and b.isInt()) {
                        @branchHint(.likely);
                        const ai = a.getInt();
                        const bi = b.getInt();
                        const result = @mulWithOverflow(ai, bi);
                        if (result[1] == 0) {
                            @branchHint(.likely);
                            self.ctx.stack[sp - 2] = value.JSValue.fromInt(result[0]);
                            self.ctx.sp = sp - 1;
                            continue :dispatch;
                        }
                        // Integer overflow - convert to float
                        self.ctx.stack[sp - 2] = try self.allocFloat(@as(f64, @floatFromInt(ai)) * @as(f64, @floatFromInt(bi)));
                        self.ctx.sp = sp - 1;
                        continue :dispatch;
                    } else {
                        @branchHint(.cold);
                        self.ctx.sp = sp - 2;
                        self.ctx.pushUnchecked(try self.mulValuesSlow(a, b));
                    }
                },

                .div => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    self.ctx.pushUnchecked(try self.divValues(a, b));
                },

                .mod => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    if (a.isInt() and b.isInt()) {
                        @branchHint(.likely);
                        const bv = b.getInt();
                        if (bv != 0) {
                            @branchHint(.likely);
                            self.ctx.stack[sp - 2] = value.JSValue.fromInt(@rem(a.getInt(), bv));
                            self.ctx.sp = sp - 1;
                            continue :dispatch;
                        }
                    }
                    return error.DivisionByZero;
                },

                .pow => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(try self.powValues(a, b));
                },

                .neg => {
                    const a = self.ctx.pop();
                    try self.ctx.push(try self.negValue(a));
                },

                .inc => {
                    // Optimize for common integer case - modify stack in place
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];
                    if (a.isInt()) {
                        @branchHint(.likely);
                        const ai = a.getInt();
                        const result = @addWithOverflow(ai, 1);
                        if (result[1] == 0) {
                            @branchHint(.likely);
                            self.ctx.stack[sp - 1] = value.JSValue.fromInt(result[0]);
                            continue :dispatch;
                        }
                        // Overflow - convert to float
                        self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(ai)) + 1.0);
                        continue :dispatch;
                    } else if (a.isFloat64()) {
                        self.ctx.stack[sp - 1] = try self.allocFloat(a.getFloat64() + 1.0);
                    } else {
                        return error.TypeError;
                    }
                },

                .dec => {
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];
                    if (a.isInt()) {
                        const ai = a.getInt();
                        const result = @subWithOverflow(ai, 1);
                        if (result[1] == 0) {
                            self.ctx.stack[sp - 1] = value.JSValue.fromInt(result[0]);
                            continue :dispatch;
                        }
                        self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(ai)) - 1.0);
                        continue :dispatch;
                    } else if (a.isFloat64()) {
                        self.ctx.stack[sp - 1] = try self.allocFloat(a.getFloat64() - 1.0);
                    } else {
                        return error.TypeError;
                    }
                },

                // ========================================
                // Bitwise Operations - optimized direct stack access
                // ========================================
                .bit_and => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) & toInt32(b));
                    self.ctx.sp = sp - 1;
                },

                .bit_or => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) | toInt32(b));
                    self.ctx.sp = sp - 1;
                },

                .bit_xor => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) ^ toInt32(b));
                    self.ctx.sp = sp - 1;
                },

                .bit_not => {
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];
                    self.ctx.stack[sp - 1] = value.JSValue.fromInt(~toInt32(a));
                },

                .shl => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    const shift: u5 = @intCast(@as(u32, @bitCast(toInt32(b))) & 0x1F);
                    self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) << shift);
                    self.ctx.sp = sp - 1;
                },

                .shr => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    const shift: u5 = @intCast(@as(u32, @bitCast(toInt32(b))) & 0x1F);
                    self.ctx.stack[sp - 2] = value.JSValue.fromInt(toInt32(a) >> shift);
                    self.ctx.sp = sp - 1;
                },

                .ushr => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    const shift: u5 = @intCast(@as(u32, @bitCast(toInt32(b))) & 0x1F);
                    const ua: u32 = @bitCast(toInt32(a));
                    self.ctx.stack[sp - 2] = value.JSValue.fromInt(@bitCast(ua >> shift));
                    self.ctx.sp = sp - 1;
                },

                // ========================================
                // Comparison - optimized with inline integer fast paths
                // ========================================
                .lt => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    // Inline integer fast path
                    if (a.isInt() and b.isInt()) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() < b.getInt());
                        self.ctx.sp = sp - 1;
                        continue :dispatch;
                    } else {
                        @branchHint(.cold);
                        self.ctx.stack[sp - 2] = value.JSValue.fromBool(try compareValues(a, b) == .lt);
                        self.ctx.sp = sp - 1;
                    }
                },

                .lte => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    if (a.isInt() and b.isInt()) {
                        self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() <= b.getInt());
                        self.ctx.sp = sp - 1;
                        continue :dispatch;
                    }
                    const cmp = try compareValues(a, b);
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(cmp == .lt or cmp == .eq);
                    self.ctx.sp = sp - 1;
                },

                .gt => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    if (a.isInt() and b.isInt()) {
                        self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() > b.getInt());
                        self.ctx.sp = sp - 1;
                        continue :dispatch;
                    }
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(try compareValues(a, b) == .gt);
                    self.ctx.sp = sp - 1;
                },

                .gte => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    if (a.isInt() and b.isInt()) {
                        self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.getInt() >= b.getInt());
                        self.ctx.sp = sp - 1;
                        continue :dispatch;
                    }
                    const cmp = try compareValues(a, b);
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(cmp == .gt or cmp == .eq);
                    self.ctx.sp = sp - 1;
                },

                .eq => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(looseEquals(a, b));
                    self.ctx.sp = sp - 1;
                },

                .neq => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(!looseEquals(a, b));
                    self.ctx.sp = sp - 1;
                },

                .strict_eq => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(a.strictEquals(b));
                    self.ctx.sp = sp - 1;
                },

                .strict_neq => {
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];
                    self.ctx.stack[sp - 2] = value.JSValue.fromBool(!a.strictEquals(b));
                    self.ctx.sp = sp - 1;
                },

                // ========================================
                // Logical
                // ========================================
                .not => {
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(!a.toBoolean()));
                },

                // ========================================
                // Control Flow
                // ========================================
                .goto => {
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    self.offsetPc(offset);
                },
                .loop => {
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    // Profile back-edge for hot loop detection (Phase 11)
                    _ = self.profileBackedge();
                    self.offsetPc(-offset);
                },

                .if_true => {
                    const cond = self.ctx.pop();
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    if (cond.toBoolean()) {
                        self.offsetPc(offset);
                    }
                },

                .if_false => {
                    const cond = self.ctx.pop();
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    if (!cond.toBoolean()) {
                        self.offsetPc(offset);
                    }
                },

                .ret => return self.ctx.pop(),

                .ret_undefined => return value.JSValue.undefined_val,

                // ========================================
                // Object Operations
                // ========================================

                .new_object => {
                    const obj = try self.createObject();
                    try self.ctx.push(obj.toValue());
                },

                .new_array => {
                    const length = readU16(self.pc);
                    self.pc += 2;
                    // Create array object with Array.prototype
                    const obj = try self.createArray();
                    obj.setArrayLength(@intCast(length));
                    try self.ctx.push(obj.toValue());
                },

                .get_field => {
                    const atom_idx = readU16(self.pc);
                    self.pc += 2;
                    const atom: object.Atom = @enumFromInt(atom_idx);
                    const obj_val = self.ctx.pop();

                    if (obj_val.isObject()) {
                        const obj = object.JSObject.fromValue(obj_val);
                        const pool = self.ctx.hidden_class_pool orelse {
                            try self.ctx.push(value.JSValue.undefined_val);
                            continue :dispatch;
                        };
                        if (obj.getProperty(pool, atom)) |prop_val| {
                            try self.ctx.push(prop_val);
                        } else {
                            try self.ctx.push(value.JSValue.undefined_val);
                        }
                    } else if (obj_val.isString()) {
                        // Primitive string property access
                        if (atom == .length) {
                            const str = obj_val.toPtr(string.JSString);
                            try self.ctx.push(value.JSValue.fromInt(@intCast(str.len)));
                        } else if (self.ctx.string_prototype) |proto| {
                            // Look up method on String.prototype
                            const pool = self.ctx.hidden_class_pool orelse {
                                try self.ctx.push(value.JSValue.undefined_val);
                                continue :dispatch;
                            };
                            if (proto.getProperty(pool, atom)) |prop_val| {
                                try self.ctx.push(prop_val);
                            } else {
                                try self.ctx.push(value.JSValue.undefined_val);
                            }
                        } else {
                            try self.ctx.push(value.JSValue.undefined_val);
                        }
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                },

                .put_field => {
                    const atom_idx = readU16(self.pc);
                    self.pc += 2;
                    const atom: object.Atom = @enumFromInt(atom_idx);
                    const val = self.ctx.pop();
                    const obj_val = self.ctx.pop();

                    if (obj_val.isObject()) {
                        const obj = object.JSObject.fromValue(obj_val);
                        try self.ctx.setPropertyChecked(obj, atom, val);
                    }
                    // Non-object assignment silently fails in non-strict mode
                },

                .put_field_keep => {
                    // Same as put_field but keeps the value on the stack
                    const atom_idx = readU16(self.pc);
                    self.pc += 2;
                    const atom: object.Atom = @enumFromInt(atom_idx);
                    const val = self.ctx.pop();
                    const obj_val = self.ctx.pop();

                    if (obj_val.isObject()) {
                        const obj = object.JSObject.fromValue(obj_val);
                        try self.ctx.setPropertyChecked(obj, atom, val);
                    }
                    // Push value back as assignment expression result
                    try self.ctx.push(val);
                },

                // Polymorphic inline cache opcodes for optimized property access
                // Format: +u16 atom_idx +u16 cache_idx
                // Caches up to 4 (hidden_class, slot_offset) pairs per site
                .get_field_ic => {
                    const atom_idx = readU16(self.pc);
                    const cache_idx = readU16(self.pc + 2);
                    self.pc += 4;
                    const atom: object.Atom = @enumFromInt(atom_idx);
                    const obj_val = self.ctx.pop();

                    if (obj_val.isObject()) {
                        const obj = object.JSObject.fromValue(obj_val);
                        const pic = &self.pic_cache[cache_idx];

                        // PIC lookup: check all cached entries
                        if (pic.lookup(obj.hidden_class_idx)) |slot_offset| {
                            self.pic_hits +%= 1; // Profile PIC hit (Phase 11)
                            try self.ctx.push(obj.getSlot(slot_offset));
                            continue :dispatch;
                        }

                        // Cache miss: full lookup and update PIC
                        self.pic_misses +%= 1; // Profile PIC miss (Phase 11)
                        const pool = self.ctx.hidden_class_pool orelse {
                            try self.ctx.push(value.JSValue.undefined_val);
                            continue :dispatch;
                        };
                        if (pool.findProperty(obj.hidden_class_idx, atom)) |slot_offset| {
                            _ = pic.update(obj.hidden_class_idx, slot_offset);
                            try self.ctx.push(obj.getSlot(slot_offset));
                        } else if (obj.getProperty(pool, atom)) |prop_val| {
                            // Property found in prototype chain (don't cache)
                            try self.ctx.push(prop_val);
                        } else {
                            try self.ctx.push(value.JSValue.undefined_val);
                        }
                    } else if (obj_val.isString()) {
                        // Primitive string property access (same as get_field)
                        if (atom == .length) {
                            const str = obj_val.toPtr(string.JSString);
                            try self.ctx.push(value.JSValue.fromInt(@intCast(str.len)));
                        } else if (self.ctx.string_prototype) |proto| {
                            const pool = self.ctx.hidden_class_pool orelse {
                                try self.ctx.push(value.JSValue.undefined_val);
                                continue :dispatch;
                            };
                            if (proto.getProperty(pool, atom)) |prop_val| {
                                try self.ctx.push(prop_val);
                            } else {
                                try self.ctx.push(value.JSValue.undefined_val);
                            }
                        } else {
                            try self.ctx.push(value.JSValue.undefined_val);
                        }
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                },

                .put_field_ic => {
                    const atom_idx = readU16(self.pc);
                    const cache_idx = readU16(self.pc + 2);
                    self.pc += 4;
                    const atom: object.Atom = @enumFromInt(atom_idx);
                    const val = self.ctx.pop();
                    const obj_val = self.ctx.pop();

                    if (obj_val.isObject()) {
                        const obj = object.JSObject.fromValue(obj_val);
                        const pic = &self.pic_cache[cache_idx];
                        if (self.ctx.enforce_arena_escape and self.ctx.hybrid != null and !obj.flags.is_arena and self.ctx.isEphemeralValue(val)) {
                            return error.ArenaObjectEscape;
                        }

                        // PIC lookup: check all cached entries
                        if (pic.lookup(obj.hidden_class_idx)) |slot_offset| {
                            self.pic_hits +%= 1; // Profile PIC hit (Phase 11)
                            obj.setSlot(slot_offset, val);
                            continue :dispatch;
                        }

                        // Cache miss: check if property exists
                        self.pic_misses +%= 1; // Profile PIC miss (Phase 11)
                        const pool = self.ctx.hidden_class_pool orelse {
                            // No pool, use full setProperty path
                            try self.ctx.setPropertyChecked(obj, atom, val);
                            continue :dispatch;
                        };
                        if (pool.findProperty(obj.hidden_class_idx, atom)) |slot_offset| {
                            // Property exists, update PIC and set value
                            _ = pic.update(obj.hidden_class_idx, slot_offset);
                            obj.setSlot(slot_offset, val);
                        } else {
                            // Property doesn't exist, use full setProperty (may transition)
                            // Don't cache transitions as hidden class will change
                            try self.ctx.setPropertyChecked(obj, atom, val);
                        }
                    }
                    // Non-object assignment silently fails in non-strict mode
                },

                .get_elem => {
                    // Optimized with direct stack access
                    const sp = self.ctx.sp;
                    const index_val = self.ctx.stack[sp - 1];
                    const obj_val = self.ctx.stack[sp - 2];

                    if (obj_val.isObject() and index_val.isInt()) {
                        const obj = object.JSObject.fromValue(obj_val);
                        const idx = index_val.getInt();
                        if (idx >= 0) {
                            const idx_u: u32 = @intCast(idx);
                            if (obj.class_id == .array) {
                                // Fast path: check bounds once, then unchecked access
                                const len = @as(u32, @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt()));
                                if (idx_u < len) {
                                    self.ctx.stack[sp - 2] = obj.getIndexUnchecked(idx_u);
                                } else {
                                    self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                                }
                                self.ctx.sp = sp - 1;
                                continue :dispatch;
                            } else if (obj.class_id == .range_iterator) {
                                // Inline range iterator computation
                                const len = @as(u32, @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt()));
                                if (idx_u < len) {
                                    const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                                    const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                                    self.ctx.stack[sp - 2] = value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step);
                                } else {
                                    self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                                }
                                self.ctx.sp = sp - 1;
                                continue :dispatch;
                            } else {
                                var idx_buf: [32]u8 = undefined;
                                const idx_slice = std.fmt.bufPrint(&idx_buf, "{d}", .{idx}) catch {
                                    self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                                    self.ctx.sp = sp - 1;
                                    continue :dispatch;
                                };
                                const atom = self.ctx.atoms.intern(idx_slice) catch {
                                    self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                                    self.ctx.sp = sp - 1;
                                    continue :dispatch;
                                };
                                const pool = self.ctx.hidden_class_pool orelse {
                                    self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                                    self.ctx.sp = sp - 1;
                                    continue :dispatch;
                                };
                                self.ctx.stack[sp - 2] = obj.getProperty(pool, atom) orelse value.JSValue.undefined_val;
                                self.ctx.sp = sp - 1;
                                continue :dispatch;
                            }
                        }
                    }
                    self.ctx.stack[sp - 2] = value.JSValue.undefined_val;
                    self.ctx.sp = sp - 1;
                },

                .put_elem => {
                    const val = self.ctx.pop();
                    const index_val = self.ctx.pop();
                    const obj_val = self.ctx.pop();

                    if (obj_val.isObject() and index_val.isInt()) {
                        const obj = object.JSObject.fromValue(obj_val);
                        const idx = index_val.getInt();
                        if (idx >= 0) {
                            if (obj.class_id == .array) {
                                try self.ctx.setIndexChecked(obj, @intCast(idx), val);
                            } else {
                                var idx_buf: [32]u8 = undefined;
                                const idx_slice = std.fmt.bufPrint(&idx_buf, "{d}", .{idx}) catch continue :dispatch;
                                const atom = self.ctx.atoms.intern(idx_slice) catch continue :dispatch;
                                try self.ctx.setPropertyChecked(obj, atom, val);
                            }
                        }
                    }
                },

                .get_global => {
                    const atom_idx = readU16(self.pc);
                    self.pc += 2;
                    const atom: object.Atom = @enumFromInt(atom_idx);
                    if (self.ctx.getGlobal(atom)) |val| {
                        try self.ctx.push(val);
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                },

                .put_global => {
                    const atom_idx = readU16(self.pc);
                    self.pc += 2;
                    const atom: object.Atom = @enumFromInt(atom_idx);
                    const val = self.ctx.pop();
                    try self.ctx.setGlobal(atom, val);
                },

                .define_global => {
                    const atom_idx = readU16(self.pc);
                    self.pc += 2;
                    const atom: object.Atom = @enumFromInt(atom_idx);
                    const val = self.ctx.pop();
                    try self.ctx.defineGlobal(atom, val);
                },

                .make_function => {
                    const const_idx = readU16(self.pc);
                    self.pc += 2;
                    // Get bytecode pointer from constant pool
                    const bc_val = try self.getConstant(const_idx);
                    if (!bc_val.isPtr()) return error.TypeError;
                    const bc_ptr = bc_val.toPtr(bytecode.FunctionBytecode);
                    // Create function object
                    const root_class_idx = self.ctx.root_class_idx;
                    const func_obj = try object.JSObject.createBytecodeFunction(
                        self.ctx.allocator,
                        root_class_idx,
                        bc_ptr,
                        @enumFromInt(bc_ptr.name_atom),
                    );
                    try self.ctx.push(func_obj.toValue());
                },

                .make_async => {
                    const const_idx = readU16(self.pc);
                    self.pc += 2;
                    // Get bytecode pointer from constant pool
                    const bc_val = try self.getConstant(const_idx);
                    if (!bc_val.isPtr()) return error.TypeError;
                    const bc_ptr = bc_val.toPtr(bytecode.FunctionBytecode);
                    // Create async function object (marked as async via slot 3)
                    const root_class_idx = self.ctx.root_class_idx;
                    const func_obj = try object.JSObject.createBytecodeFunction(
                        self.ctx.allocator,
                        root_class_idx,
                        bc_ptr,
                        @enumFromInt(bc_ptr.name_atom),
                    );
                    // Mark as async function using flag (more efficient than inline slot check)
                    func_obj.flags.is_async = true;
                    try self.ctx.push(func_obj.toValue());
                },

                .await_val => {
                    // For synchronous execution, await just returns the value directly
                    // A full implementation would integrate with an event loop and Promises
                    // For now, if the value is a Promise-like object with .then(), we don't support it
                    // Just pass the value through (works for non-Promise values)
                    const awaited = self.ctx.peek();
                    _ = awaited;
                    // Value stays on stack unchanged for sync execution
                },

                // Module operations
                .import_module => {
                    const module_idx = readU16(self.pc);
                    self.pc += 2;
                    // Get module name from constant pool
                    const module_name_val = try self.getConstant(module_idx);
                    _ = module_name_val;
                    // Module loading requires a module registry/loader which would be
                    // set up in the context. For now, push an empty namespace object.
                    const namespace = try self.ctx.createObject(null);
                    try self.ctx.push(namespace.toValue());
                },

                .import_name => {
                    const name_idx = readU16(self.pc);
                    self.pc += 2;
                    // Get the name from constant pool
                    const name_val = try self.getConstant(name_idx);
                    _ = name_val;
                    // Pop module namespace and get named export
                    const namespace_val = self.ctx.pop();
                    if (namespace_val.isObject()) {
                        const namespace = object.JSObject.fromValue(namespace_val);
                        // Try to get the property by name
                        // For now, just push undefined as we don't have real module loading
                        if (namespace.getSlot(0).isUndefined()) {
                            try self.ctx.push(value.JSValue.undefined_val);
                        } else {
                            try self.ctx.push(namespace.getSlot(0));
                        }
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                },

                .import_default => {
                    // Pop module namespace and get default export
                    const namespace_val = self.ctx.pop();
                    if (namespace_val.isObject()) {
                        const namespace = object.JSObject.fromValue(namespace_val);
                        // Default export is typically stored with "default" key
                        // For now, just return undefined
                        _ = namespace;
                        try self.ctx.push(value.JSValue.undefined_val);
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                },

                .export_name => {
                    const name_idx = readU16(self.pc);
                    self.pc += 2;
                    _ = name_idx;
                    // Pop the value to export - in a real implementation this would
                    // register the export in the module's export table
                    _ = self.ctx.pop();
                },

                .export_default => {
                    // Pop the default value to export - in a real implementation
                    // this would register it as the default export
                    _ = self.ctx.pop();
                },

                .array_spread => {
                    // Stack: [target_array, current_index, source_array]
                    const source_val = self.ctx.pop();
                    const idx_val = self.ctx.pop();
                    const target_val = self.ctx.peek();

                    if (target_val.isObject() and source_val.isObject() and idx_val.isInt()) {
                        const target = object.JSObject.fromValue(target_val);
                        const source = object.JSObject.fromValue(source_val);
                        var idx: usize = @intCast(idx_val.getInt());
                        const pool = self.ctx.hidden_class_pool orelse {
                            try self.ctx.push(idx_val);
                            continue :dispatch;
                        };

                        // Get source array length
                        if (source.getProperty(pool, .length)) |len_val| {
                            if (len_val.isInt()) {
                                const src_len: usize = @intCast(len_val.getInt());
                                // Copy each element from source to target
                                for (0..src_len) |i| {
                                    const elem = source.getSlot(@intCast(i));
                                    if (self.ctx.enforce_arena_escape and self.ctx.hybrid != null and !target.flags.is_arena and self.ctx.isEphemeralValue(elem)) {
                                        return error.ArenaObjectEscape;
                                    }
                                    target.setSlot(@intCast(idx), elem);
                                    idx += 1;
                                }
                                // Push new index for subsequent elements
                                try self.ctx.push(value.JSValue.fromInt(@intCast(idx)));
                                continue;
                            }
                        }
                    }
                    // If spread failed, just push the original index back
                    try self.ctx.push(idx_val);
                },

                .call_spread => {
                    // For now, call_spread is not fully implemented
                    // Would need to collect spread arguments into a single args array
                    try self.ctx.push(value.JSValue.undefined_val);
                },

                .typeof => {
                    const a = self.ctx.pop();
                    const type_str = a.typeOf();
                    // Create JS string for typeof result
                    const js_str = self.createString(type_str) catch {
                        try self.ctx.push(value.JSValue.undefined_val);
                        continue;
                    };
                    try self.ctx.push(value.JSValue.fromPtr(js_str));
                },

                .instanceof => {
                    const ctor = self.ctx.pop();
                    const obj = self.ctx.pop();

                    // obj instanceof Constructor checks if Constructor.prototype
                    // is in the prototype chain of obj
                    if (!ctor.isObject()) {
                        try self.ctx.push(value.JSValue.false_val);
                        continue;
                    }

                    if (!obj.isObject()) {
                        try self.ctx.push(value.JSValue.false_val);
                        continue;
                    }

                    const ctor_obj = object.JSObject.fromValue(ctor);
                    const target_obj = object.JSObject.fromValue(obj);
                    const pool = self.ctx.hidden_class_pool orelse {
                        try self.ctx.push(value.JSValue.false_val);
                        continue :dispatch;
                    };

                    // Get Constructor.prototype
                    if (ctor_obj.getProperty(pool, .prototype)) |proto_val| {
                        if (proto_val.isObject()) {
                            const proto = object.JSObject.fromValue(proto_val);
                            // Walk the prototype chain of obj
                            var current: ?*object.JSObject = target_obj.prototype;
                            while (current) |p| {
                                if (p == proto) {
                                    try self.ctx.push(value.JSValue.true_val);
                                    continue :dispatch;
                                }
                                current = p.prototype;
                            }
                        }
                    }
                    try self.ctx.push(value.JSValue.false_val);
                },

                // ========================================
                // Function Calls
                // ========================================
                .call => {
                    const argc: u8 = self.pc[0];
                    self.pc += 1;
                    try self.doCall(argc, false);
                },

                .call_method => {
                    const argc: u8 = self.pc[0];
                    self.pc += 1;
                    try self.doCall(argc, true);
                },

                .tail_call => {
                    const argc: u8 = self.pc[0];
                    self.pc += 1;
                    // TODO: Implement proper tail call optimization
                    try self.doCall(argc, false);
                },

                // ========================================
                // Superinstructions (fused hot paths)
                // ========================================
                .get_loc_add => {
                    const idx = self.pc[0];
                    self.pc += 1;
                    const a = self.ctx.getLocal(idx);
                    const b = self.ctx.pop();
                    try self.ctx.push(try self.addValues(a, b));
                },

                .get_loc_get_loc_add => {
                    const idx1 = self.pc[0];
                    const idx2 = self.pc[1];
                    self.pc += 2;
                    const a = self.ctx.getLocal(idx1);
                    const b = self.ctx.getLocal(idx2);
                    try self.ctx.push(try self.addValues(a, b));
                },

                .if_false_goto => {
                    const cond = self.ctx.pop();
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    if (!cond.toBoolean()) {
                        self.offsetPc(offset);
                    }
                },

                // Fused push_const + call: load constant and call in one dispatch
                .push_const_call => {
                    const const_idx = readU16(self.pc);
                    const argc: u8 = self.pc[2];
                    self.pc += 3;
                    // Push the constant (function to call)
                    try self.ctx.push(try self.getConstant(const_idx));
                    // Call it
                    try self.doCall(argc, false);
                },

                // Fused get_field + call_method: load method and call in one dispatch
                .get_field_call => {
                    const atom_idx = readU16(self.pc);
                    const argc: u8 = self.pc[2];
                    self.pc += 3;
                    const atom: object.Atom = @enumFromInt(atom_idx);

                    // Stack: [obj, obj] (dup from codegen)
                    // Need to: get method from top obj (pop), then call_method with original obj as 'this'
                    const obj = self.ctx.pop();
                    if (obj.isObject()) {
                        const js_obj = object.JSObject.fromValue(obj);
                        const pool = self.ctx.hidden_class_pool orelse {
                            try self.ctx.push(value.JSValue.undefined_val);
                            try self.doCall(argc, true);
                            continue :dispatch;
                        };
                        if (js_obj.getProperty(pool, atom)) |method| {
                            // Push method on stack (obj is still there)
                            try self.ctx.push(method);
                            // Call as method (will use obj as 'this')
                            try self.doCall(argc, true);
                            continue :dispatch;
                        }
                    }
                    // Fallback: property not found, push undefined and call
                    try self.ctx.push(value.JSValue.undefined_val);
                    try self.doCall(argc, true);
                },

                // Fused arithmetic-modulo: (a op b) % divisor
                // Uses i64 intermediate to avoid overflow, then modulo brings result back to i32 range
                .add_mod => {
                    const divisor_idx = readU16(self.pc);
                    self.pc += 2;
                    const divisor_val = try self.getConstant(divisor_idx);
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];

                    if (a.isInt() and b.isInt() and divisor_val.isInt()) {
                        const ai: i64 = a.getInt();
                        const bi: i64 = b.getInt();
                        const div: i64 = divisor_val.getInt();
                        if (div != 0) {
                            const sum = ai + bi;
                            // JavaScript % can return negative for negative numerators
                            // Use @mod for always-positive result (matches common usage)
                            const result: i32 = @intCast(@mod(sum, div));
                            self.ctx.stack[sp - 2] = value.JSValue.fromInt(result);
                            self.ctx.sp = sp - 1;
                            continue :dispatch;
                        }
                    }
                    // Fallback to normal path
                    self.ctx.sp = sp - 2;
                    const add_result = try self.addValues(a, b);
                    self.ctx.pushUnchecked(try modValues(add_result, divisor_val));
                },

                .sub_mod => {
                    const divisor_idx = readU16(self.pc);
                    self.pc += 2;
                    const divisor_val = try self.getConstant(divisor_idx);
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];

                    if (a.isInt() and b.isInt() and divisor_val.isInt()) {
                        const ai: i64 = a.getInt();
                        const bi: i64 = b.getInt();
                        const div: i64 = divisor_val.getInt();
                        if (div != 0) {
                            const diff = ai - bi;
                            const result: i32 = @intCast(@mod(diff, div));
                            self.ctx.stack[sp - 2] = value.JSValue.fromInt(result);
                            self.ctx.sp = sp - 1;
                            continue :dispatch;
                        }
                    }
                    self.ctx.sp = sp - 2;
                    const sub_result = try self.subValues(a, b);
                    self.ctx.pushUnchecked(try modValues(sub_result, divisor_val));
                },

                .mul_mod => {
                    const divisor_idx = readU16(self.pc);
                    self.pc += 2;
                    const divisor_val = try self.getConstant(divisor_idx);
                    const sp = self.ctx.sp;
                    const b = self.ctx.stack[sp - 1];
                    const a = self.ctx.stack[sp - 2];

                    if (a.isInt() and b.isInt() and divisor_val.isInt()) {
                        const ai: i64 = a.getInt();
                        const bi: i64 = b.getInt();
                        const div: i64 = divisor_val.getInt();
                        if (div != 0) {
                            const product = ai * bi;
                            const result: i32 = @intCast(@mod(product, div));
                            self.ctx.stack[sp - 2] = value.JSValue.fromInt(result);
                            self.ctx.sp = sp - 1;
                            continue :dispatch;
                        }
                    }
                    self.ctx.sp = sp - 2;
                    const mul_result = try self.mulValues(a, b);
                    self.ctx.pushUnchecked(try modValues(mul_result, divisor_val));
                },

                // ========================================
                // Loop Optimization Superinstructions
                // ========================================
                .for_of_next => {
                    // Combined bounds check + element fetch + index increment
                    // Stack: [iterable, index] -> [iterable, index+1] with element pushed
                    // Or jumps to end_offset if iteration complete
                    const end_offset = readI16(self.pc);
                    self.pc += 2;
                    const sp = self.ctx.sp;
                    const idx_val = self.ctx.stack[sp - 1];
                    const iter_val = self.ctx.stack[sp - 2];

                    if (iter_val.isObject() and idx_val.isInt()) {
                        @branchHint(.likely);
                        const obj = object.JSObject.fromValue(iter_val);
                        const idx = idx_val.getInt();
                        if (idx >= 0) {
                            @branchHint(.likely);
                            const idx_u: u32 = @intCast(idx);
                            // Array fast path
                            if (obj.class_id == .array) {
                                const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt());
                                if (idx_u < len) {
                                    @branchHint(.likely);
                                    // Push element, increment index in-place
                                    try self.ctx.push(obj.getIndexUnchecked(idx_u));
                                    self.ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                                    continue :dispatch;
                                }
                            }
                            // Range iterator fast path
                            else if (obj.class_id == .range_iterator) {
                                const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt());
                                if (idx_u < len) {
                                    @branchHint(.likely);
                                    const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                                    const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                                    try self.ctx.push(value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step));
                                    self.ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                                    continue :dispatch;
                                }
                            }
                        }
                    }
                    // Loop done - jump to cleanup
                    self.offsetPc(end_offset);
                },

                .for_of_next_put_loc => {
                    // Fused: for_of_next + put_loc (stores element directly to local)
                    // Stack: [iterable, index] -> [iterable, index+1] (no element pushed)
                    const local_idx = self.pc[0];
                    self.pc += 1;
                    const end_offset = readI16(self.pc);
                    self.pc += 2;
                    const sp = self.ctx.sp;
                    const idx_val = self.ctx.stack[sp - 1];
                    const iter_val = self.ctx.stack[sp - 2];

                    if (iter_val.isObject() and idx_val.isInt()) {
                        @branchHint(.likely);
                        const obj = object.JSObject.fromValue(iter_val);
                        const idx = idx_val.getInt();
                        if (idx >= 0) {
                            @branchHint(.likely);
                            const idx_u: u32 = @intCast(idx);
                            // Array fast path
                            if (obj.class_id == .array) {
                                const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt());
                                if (idx_u < len) {
                                    @branchHint(.likely);
                                    // Store element directly to local, increment index in-place
                                    self.ctx.setLocal(local_idx, obj.getIndexUnchecked(idx_u));
                                    self.ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                                    continue :dispatch;
                                }
                            }
                            // Range iterator fast path
                            else if (obj.class_id == .range_iterator) {
                                const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt());
                                if (idx_u < len) {
                                    @branchHint(.likely);
                                    const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                                    const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                                    self.ctx.setLocal(local_idx, value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step));
                                    self.ctx.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                                    continue :dispatch;
                                }
                            }
                        }
                    }
                    // Loop done - jump to cleanup
                    self.offsetPc(end_offset);
                },

                // ========================================
                // Specialized Constant Opcodes
                // ========================================
                .shr_1 => {
                    // Optimized shift right by 1 (common pattern x >> 1)
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];
                    if (a.isInt()) {
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(a.getInt() >> 1);
                        continue :dispatch;
                    }
                    // Fallback for non-integer
                    self.ctx.stack[sp - 1] = value.JSValue.fromInt(toInt32(a) >> 1);
                },

                .mul_2 => {
                    // Optimized multiply by 2 (common pattern x * 2)
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];
                    if (a.isInt()) {
                        const ai = a.getInt();
                        // Use shift left with overflow check (multiply by 2 = shift left 1)
                        const result = @shlWithOverflow(ai, 1);
                        if (result[1] == 0) {
                            self.ctx.stack[sp - 1] = value.JSValue.fromInt(result[0]);
                            continue :dispatch;
                        }
                        // Overflow - convert to float
                        self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(ai)) * 2.0);
                        continue :dispatch;
                    }
                    // Float path
                    if (a.isFloat64()) {
                        self.ctx.stack[sp - 1] = try self.allocFloat(a.getFloat64() * 2.0);
                        continue :dispatch;
                    }
                    return error.TypeError;
                },

                .mod_const => {
                    // Optimized modulo by constant (common pattern x % constant)
                    const divisor_idx = readU16(self.pc);
                    self.pc += 2;
                    const divisor_val = try self.getConstant(divisor_idx);
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];

                    if (a.isInt() and divisor_val.isInt()) {
                        @branchHint(.likely);
                        const div = divisor_val.getInt();
                        if (div != 0) {
                            @branchHint(.likely);
                            self.ctx.stack[sp - 1] = value.JSValue.fromInt(@rem(a.getInt(), div));
                            continue :dispatch;
                        }
                    }
                    // Fallback to helper (handles division by zero error)
                    self.ctx.stack[sp - 1] = try modValues(a, divisor_val);
                    continue :dispatch;
                },

                .mod_const_i8 => {
                    // Modulo by inline i8 constant (no constant pool lookup)
                    const divisor: i8 = @bitCast(self.pc[0]);
                    self.pc += 1;
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];

                    if (a.isInt() and divisor != 0) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 1] = value.JSValue.fromInt(@rem(a.getInt(), divisor));
                        continue :dispatch;
                    }
                    // Fallback
                    self.ctx.stack[sp - 1] = try modValues(a, value.JSValue.fromInt(divisor));
                },

                .add_const_i8 => {
                    // Add inline i8 constant
                    const constant: i8 = @bitCast(self.pc[0]);
                    self.pc += 1;
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];

                    if (a.isInt()) {
                        @branchHint(.likely);
                        const result = @addWithOverflow(a.getInt(), constant);
                        if (result[1] == 0) {
                            self.ctx.stack[sp - 1] = value.JSValue.fromInt(result[0]);
                            continue :dispatch;
                        }
                        // Overflow - convert to float
                        self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) + @as(f64, @floatFromInt(constant)));
                        continue :dispatch;
                    }
                    // Fallback
                    self.ctx.stack[sp - 1] = try self.addValues(a, value.JSValue.fromInt(constant));
                },

                .sub_const_i8 => {
                    // Subtract inline i8 constant
                    const constant: i8 = @bitCast(self.pc[0]);
                    self.pc += 1;
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];

                    if (a.isInt()) {
                        @branchHint(.likely);
                        const result = @subWithOverflow(a.getInt(), constant);
                        if (result[1] == 0) {
                            self.ctx.stack[sp - 1] = value.JSValue.fromInt(result[0]);
                            continue :dispatch;
                        }
                        // Overflow - convert to float
                        self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) - @as(f64, @floatFromInt(constant)));
                        continue :dispatch;
                    }
                    // Fallback
                    self.ctx.stack[sp - 1] = try self.subValues(a, value.JSValue.fromInt(constant));
                },

                .mul_const_i8 => {
                    // Multiply by inline i8 constant
                    const constant: i8 = @bitCast(self.pc[0]);
                    self.pc += 1;
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];

                    if (a.isInt()) {
                        @branchHint(.likely);
                        const ai: i64 = a.getInt();
                        const result = ai * constant;
                        if (result >= std.math.minInt(i32) and result <= std.math.maxInt(i32)) {
                            self.ctx.stack[sp - 1] = value.JSValue.fromInt(@intCast(result));
                            continue :dispatch;
                        }
                        // Overflow - convert to float
                        self.ctx.stack[sp - 1] = try self.allocFloat(@as(f64, @floatFromInt(result)));
                        continue :dispatch;
                    }
                    // Fallback
                    self.ctx.stack[sp - 1] = try self.mulValues(a, value.JSValue.fromInt(constant));
                },

                .lt_const_i8 => {
                    // Less than inline i8 constant (common in loop conditions)
                    const constant: i8 = @bitCast(self.pc[0]);
                    self.pc += 1;
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];

                    if (a.isInt()) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 1] = if (a.getInt() < constant) value.JSValue.true_val else value.JSValue.false_val;
                        continue :dispatch;
                    }
                    // Fallback for floats (null from toNumber = NaN, comparisons with NaN are false)
                    const num = a.toNumber() orelse {
                        self.ctx.stack[sp - 1] = value.JSValue.false_val;
                        continue :dispatch;
                    };
                    self.ctx.stack[sp - 1] = if (num < @as(f64, @floatFromInt(constant))) value.JSValue.true_val else value.JSValue.false_val;
                },

                .le_const_i8 => {
                    // Less than or equal to inline i8 constant
                    const constant: i8 = @bitCast(self.pc[0]);
                    self.pc += 1;
                    const sp = self.ctx.sp;
                    const a = self.ctx.stack[sp - 1];

                    if (a.isInt()) {
                        @branchHint(.likely);
                        self.ctx.stack[sp - 1] = if (a.getInt() <= constant) value.JSValue.true_val else value.JSValue.false_val;
                        continue :dispatch;
                    }
                    // Fallback for floats (null from toNumber = NaN, comparisons with NaN are false)
                    const num = a.toNumber() orelse {
                        self.ctx.stack[sp - 1] = value.JSValue.false_val;
                        continue :dispatch;
                    };
                    self.ctx.stack[sp - 1] = if (num <= @as(f64, @floatFromInt(constant))) value.JSValue.true_val else value.JSValue.false_val;
                },

                // ========================================
                // Closure Operations
                // ========================================
                .get_upvalue => {
                    const idx = self.pc[0];
                    self.pc += 1;
                    // Get the upvalue from the current closure
                    if (self.current_closure) |closure| {
                        if (idx < closure.upvalues.len) {
                            const uv = closure.upvalues[idx];
                            try self.ctx.push(uv.get());
                        } else {
                            try self.ctx.push(value.JSValue.undefined_val);
                        }
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
                },

                .put_upvalue => {
                    const idx = self.pc[0];
                    self.pc += 1;
                    const val = self.ctx.pop();
                    // Set the upvalue in the current closure
                    if (self.current_closure) |closure| {
                        if (idx < closure.upvalues.len) {
                            closure.upvalues[idx].set(val);
                        }
                    }
                },

                .close_upvalue => {
                    const local_idx = self.pc[0];
                    self.pc += 1;
                    // Close any open upvalues pointing to this local slot
                    self.closeUpvaluesAbove(local_idx);
                },

                .make_closure => {
                    const const_idx = readU16(self.pc);
                    self.pc += 2;
                    const upvalue_count: u8 = self.pc[0];
                    self.pc += 1;

                    // Get bytecode pointer from constant pool
                    const bc_val = try self.getConstant(const_idx);
                    if (!bc_val.isPtr()) return error.TypeError;
                    const bc_ptr = bc_val.toPtr(bytecode.FunctionBytecode);

                    // Allocate upvalue array
                    const upvalues = try self.ctx.allocator.alloc(*object.Upvalue, upvalue_count);
                    errdefer self.ctx.allocator.free(upvalues);

                    // Capture upvalues based on bytecode info
                    for (0..upvalue_count) |i| {
                        const info = bc_ptr.upvalue_info[i];
                        if (info.is_local) {
                            // Capture from current function's locals
                            upvalues[i] = try self.captureUpvalue(info.index);
                        } else {
                            // Capture from current closure's upvalues
                            if (self.current_closure) |closure| {
                                upvalues[i] = closure.upvalues[info.index];
                            } else {
                                // Create a new closed upvalue with undefined from pool
                                const uv = try self.ctx.gc_state.acquireUpvalue();
                                uv.* = .{
                                    .location = .{ .closed = value.JSValue.undefined_val },
                                    .next = null,
                                };
                                upvalues[i] = uv;
                            }
                        }
                    }

                    // Create closure object
                    const root_class_idx = self.ctx.root_class_idx;
                    const closure_obj = try object.JSObject.createClosure(
                        self.ctx.allocator,
                        root_class_idx,
                        bc_ptr,
                        @enumFromInt(bc_ptr.name_atom),
                        upvalues,
                    );
                    try self.ctx.push(closure_obj.toValue());
                },

                else => {
                    std.log.warn("Unimplemented opcode: {}", .{op});
                    return error.UnimplementedOpcode;
                },
            }
        }

        return value.JSValue.undefined_val;
    }

    // ========================================================================
    // Value Operations (with heap allocation for floats)
    // ========================================================================

    /// Add two values - optimized for the common integer case
    inline fn addValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Integer fast path FIRST - most common case in arithmetic benchmarks
        // Checking integers before strings saves a branch in the hot path
        if (a.isInt() and b.isInt()) {
            const result = @addWithOverflow(a.getInt(), b.getInt());
            if (result[1] == 0) {
                return value.JSValue.fromInt(result[0]);
            }
            // Overflow - convert to float
            return try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) + @as(f64, @floatFromInt(b.getInt())));
        }
        // Slow path: strings or floats
        return self.addValuesSlow(a, b);
    }

    /// Slow path for addition - handles strings and floats
    fn addValuesSlow(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        @branchHint(.cold);
        // String concatenation - if either operand is a string
        if (a.isString() or b.isString()) {
            return try self.concatToString(a, b);
        }
        // Float path
        const an = a.toNumber() orelse {
            traceTypeError(self, "add(a)", a, b);
            return error.TypeError;
        };
        const bn = b.toNumber() orelse {
            traceTypeError(self, "add(b)", a, b);
            return error.TypeError;
        };
        return try self.allocFloat(an + bn);
    }

    /// Subtract two values - optimized for integer fast path
    inline fn subValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Integer fast path
        if (a.isInt() and b.isInt()) {
            const result = @subWithOverflow(a.getInt(), b.getInt());
            if (result[1] == 0) {
                return value.JSValue.fromInt(result[0]);
            }
            // Overflow - convert to float
            return try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) - @as(f64, @floatFromInt(b.getInt())));
        }
        // Slow path
        return self.subValuesSlow(a, b);
    }

    fn subValuesSlow(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        @branchHint(.cold);
        const an = a.toNumber() orelse {
            traceTypeError(self, "sub(a)", a, b);
            return error.TypeError;
        };
        const bn = b.toNumber() orelse {
            traceTypeError(self, "sub(b)", a, b);
            return error.TypeError;
        };
        return try self.allocFloat(an - bn);
    }

    /// Multiply two values - optimized for integer fast path
    inline fn mulValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Integer fast path
        if (a.isInt() and b.isInt()) {
            const result = @mulWithOverflow(a.getInt(), b.getInt());
            if (result[1] == 0) {
                return value.JSValue.fromInt(result[0]);
            }
            // Overflow - convert to float
            return try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) * @as(f64, @floatFromInt(b.getInt())));
        }
        // Slow path
        return self.mulValuesSlow(a, b);
    }

    fn mulValuesSlow(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        @branchHint(.cold);
        const an = a.toNumber() orelse {
            traceTypeError(self, "mul(a)", a, b);
            return error.TypeError;
        };
        const bn = b.toNumber() orelse {
            traceTypeError(self, "mul(b)", a, b);
            return error.TypeError;
        };
        return try self.allocFloat(an * bn);
    }

    /// Convert value to string and concatenate
    fn concatToString(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Convert a to string
        const str_a = try self.valueToString(a);
        // Only free temp strings in non-hybrid mode (arena handles cleanup in hybrid mode)
        defer if (self.ctx.hybrid == null and !a.isString()) string.freeString(self.ctx.allocator, str_a);

        // Convert b to string
        const str_b = try self.valueToString(b);
        defer if (self.ctx.hybrid == null and !b.isString()) string.freeString(self.ctx.allocator, str_b);

        // Use arena when hybrid mode is enabled
        if (self.ctx.hybrid) |h| {
            const result = string.concatStringsWithArena(h.arena, str_a, str_b) orelse
                return error.OutOfMemory;
            return value.JSValue.fromPtr(result);
        }

        // Concatenate
        const result = try string.concatStrings(self.ctx.allocator, str_a, str_b);
        return value.JSValue.fromPtr(result);
    }

    /// Convert a JSValue to a JSString
    fn valueToString(self: *Interpreter, val: value.JSValue) !*string.JSString {
        if (val.isString()) {
            return val.toPtr(string.JSString);
        }
        if (val.isInt()) {
            var buf: [32]u8 = undefined;
            const slice = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return try self.createString("0");
            return try self.createString(slice);
        }
        if (val.isNull()) {
            return try self.createString("null");
        }
        if (val.isUndefined()) {
            return try self.createString("undefined");
        }
        if (val.isTrue()) {
            return try self.createString("true");
        }
        if (val.isFalse()) {
            return try self.createString("false");
        }
        if (val.isObject()) {
            return try self.createString("[object Object]");
        }
        // Float
        if (val.toNumber()) |n| {
            var buf: [64]u8 = undefined;
            const slice = std.fmt.bufPrint(&buf, "{d}", .{n}) catch return try self.createString("NaN");
            return try self.createString(slice);
        }
        return try self.createString("undefined");
    }

    fn divValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Division always produces float in JS
        const an = a.toNumber() orelse {
            traceTypeError(self, "div(a)", a, b);
            return error.TypeError;
        };
        const bn = b.toNumber() orelse {
            traceTypeError(self, "div(b)", a, b);
            return error.TypeError;
        };
        return try self.allocFloat(an / bn);
    }

    fn powValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        const an = a.toNumber() orelse {
            traceTypeError(self, "pow(a)", a, b);
            return error.TypeError;
        };
        const bn = b.toNumber() orelse {
            traceTypeError(self, "pow(b)", a, b);
            return error.TypeError;
        };
        return try self.allocFloat(std.math.pow(f64, an, bn));
    }

    fn negValue(self: *Interpreter, a: value.JSValue) !value.JSValue {
        if (a.isInt()) {
            const v = a.getInt();
            if (v == std.math.minInt(i32)) {
                // Negating minInt overflows
                return try self.allocFloat(-@as(f64, @floatFromInt(v)));
            }
            return value.JSValue.fromInt(-v);
        }
        if (a.isFloat64()) {
            return try self.allocFloat(-a.getFloat64());
        }
        return error.TypeError;
    }

    fn allocFloat(self: *Interpreter, v: f64) !value.JSValue {
        // Use arena for ephemeral float allocation if hybrid mode enabled
        if (self.ctx.hybrid) |h| {
            // Use createAligned to respect Float64Box alignment requirements
            const box = h.arena.createAligned(value.JSValue.Float64Box) orelse
                return error.OutOfMemory;
            box.* = .{
                .header = heap.MemBlockHeader.init(.float64, @sizeOf(value.JSValue.Float64Box)),
                ._pad = 0,
                .value = v,
            };
            return value.JSValue.fromPtr(box);
        }
        // Fallback to GC-managed allocation
        const box = try self.ctx.gc_state.allocFloat(v);
        return value.JSValue.fromPtr(box);
    }

    fn createObject(self: *Interpreter) !*object.JSObject {
        const root_class_idx = self.ctx.root_class_idx;
        // Use arena for ephemeral object allocation if hybrid mode enabled
        if (self.ctx.hybrid) |h| {
            return object.JSObject.createWithArena(h.arena, root_class_idx, null) orelse
                return error.OutOfMemory;
        }
        return try object.JSObject.create(self.ctx.allocator, root_class_idx, null);
    }

    fn createArray(self: *Interpreter) !*object.JSObject {
        const root_class_idx = self.ctx.root_class_idx;
        // Use arena for ephemeral array allocation if hybrid mode enabled
        if (self.ctx.hybrid) |h| {
            const obj = object.JSObject.createArrayWithArena(h.arena, root_class_idx) orelse
                return error.OutOfMemory;
            obj.prototype = self.ctx.array_prototype;
            return obj;
        }
        const obj = try object.JSObject.createArray(self.ctx.allocator, root_class_idx);
        obj.prototype = self.ctx.array_prototype;
        return obj;
    }

    /// Create a string using hybrid allocator if available
    /// Ephemeral strings use arena allocation, persistent strings use standard allocator
    fn createString(self: *Interpreter, s: []const u8) !*string.JSString {
        // Use arena for ephemeral string allocation if hybrid mode enabled
        if (self.ctx.hybrid) |h| {
            return string.createStringWithArena(h.arena, s) orelse
                return error.OutOfMemory;
        }
        return try string.createString(self.ctx.allocator, s);
    }

    fn getConstant(self: *Interpreter, idx: u16) !value.JSValue {
        if (idx >= self.constants.len) return error.InvalidConstant;
        return self.constants[idx];
    }

    /// Maximum arguments for stack-based allocation (security limit)
    const MAX_STACK_ARGS = 256;

    /// Perform function call, dispatching to native functions, bytecode functions,
    /// generators, or async functions as appropriate.
    ///
    /// Stack layout on entry:
    ///   Regular call: [func, arg0, arg1, ..., argN-1] (argc values after func)
    ///   Method call:  [obj, func, arg0, arg1, ..., argN-1] (obj is 'this')
    ///
    /// Call dispatch order:
    /// 1. Pop arguments from stack into local array
    /// 2. Pop function value
    /// 3. For method calls, pop 'this' value
    /// 4. Check if value is callable (has function data)
    /// 5. Dispatch based on function type:
    ///    - Native: Call C/Zig function directly, push result
    ///    - Generator: Create generator object, initialize locals, push iterator
    ///    - Async: Execute synchronously, wrap result in Promise-like object
    ///    - Bytecode: Call via callBytecodeFunction, push result
    ///
    /// Security: Limits argc to MAX_STACK_ARGS (256) to prevent stack buffer overflow.
    fn doCall(self: *Interpreter, argc: u8, is_method: bool) InterpreterError!void {
        // Stack layout for regular call: [func, arg0, arg1, ..., argN-1]
        // Stack layout for method call: [obj, func, arg0, arg1, ..., argN-1]

        // CRITICAL: Bounds check to prevent buffer overflow (security fix)
        if (argc > MAX_STACK_ARGS) {
            return error.TooManyArguments;
        }
        traceCall(self, "enter", argc, is_method);
        defer traceCall(self, "exit", argc, is_method);
        const guard = callGuardDepth();
        if (guard != 0 and self.ctx.call_depth >= guard) {
            traceCall(self, "guard", argc, is_method);
            return error.CallStackOverflow;
        }

        // Collect arguments (in reverse order from stack)
        var args: [MAX_STACK_ARGS]value.JSValue = undefined;
        var i: usize = argc;
        while (i > 0) {
            i -= 1;
            args[i] = self.ctx.pop();
        }

        // Pop function
        const func_val = self.ctx.pop();

        // Pop 'this' for method calls
        const this_val = if (is_method) self.ctx.pop() else value.JSValue.undefined_val;

        // Check if callable
        if (!func_val.isCallable()) {
            if (callTraceEnabled()) {
                std.debug.print(
                    "[call] not-callable type={s} func={} this={} depth={} sp={} fp={}\n",
                    .{ func_val.typeOf(), func_val, this_val, self.ctx.call_depth, self.ctx.sp, self.ctx.fp },
                );
                if (func_val.isObject()) {
                    const obj = object.JSObject.fromValue(func_val);
                    std.debug.print(
                        "[call] not-callable object class={} callable={} generator={} async={}\n",
                        .{
                            @intFromEnum(obj.class_id),
                            @intFromBool(obj.flags.is_callable),
                            @intFromBool(obj.flags.is_generator),
                            @intFromBool(obj.flags.is_async),
                        },
                    );
                }
                if (self.current_func) |cur| {
                    const pc_off = @as(usize, @intCast(@intFromPtr(self.pc) - @intFromPtr(cur.code.ptr)));
                    std.debug.print("[call] not-callable pc_off={} func_locals={}\n", .{ pc_off, cur.local_count });
                }
            }
            return error.NotCallable;
        }

        // Get the function object
        const func_obj = object.JSObject.fromValue(func_val);

        // Check for native function
        if (func_obj.getNativeFunctionData()) |native_data| {
            // Call native function
            const result = native_data.func(self.ctx, this_val, args[0..argc]) catch |err| {
                // Create error message with function name for better debugging
                const func_name = if (native_data.name.toPredefinedName()) |name| name else "<native>";
                std.log.err("Native function '{s}' error: {}", .{ func_name, err });
                // Set exception on context for JS-level error handling
                self.ctx.throwException(value.JSValue.exception_val);
                return error.NativeFunctionError;
            };
            try self.ctx.push(result);
            return;
        }

        // Closure or bytecode function
        const closure_data = func_obj.getClosureData();
        const func_bc_opt = if (closure_data) |cd|
            cd.bytecode
        else if (func_obj.getBytecodeFunctionData()) |bc_data|
            bc_data.bytecode
        else
            null;

        if (func_bc_opt) |func_bc| {
            // Set current closure context for this call (null for non-closures)
            const prev_closure = self.current_closure;
            self.current_closure = closure_data;
            defer self.current_closure = prev_closure;

            // Check if this is a generator function (using cached flag)
            if (func_obj.flags.is_generator) {
                // Create a generator object instead of executing
                const root_class_idx = self.ctx.root_class_idx;
                const gen_obj = if (self.ctx.hybrid) |h|
                    (object.JSObject.createGeneratorWithArena(
                        h.arena,
                        root_class_idx,
                        func_bc,
                        self.ctx.generator_prototype,
                    ) orelse return error.OutOfMemory)
                else
                    (object.JSObject.createGenerator(
                        self.ctx.allocator,
                        root_class_idx,
                        func_bc,
                        self.ctx.generator_prototype,
                    ) catch return error.OutOfMemory);

                // Initialize generator locals with arguments
                const gen_data = gen_obj.getGeneratorData().?;
                var local_idx: usize = 0;
                while (local_idx < gen_data.locals.len) : (local_idx += 1) {
                    if (local_idx < argc) {
                        gen_data.locals[local_idx] = args[local_idx];
                    }
                }

                try self.ctx.push(gen_obj.toValue());
                return;
            }

            // Check if this is an async function (using cached flag)
            if (func_obj.flags.is_async) {
                // Execute async function synchronously and wrap result in Promise-like object
                // For full async support, we'd need an event loop and proper Promise integration
                // This simplified version executes synchronously and returns a resolved Promise
                const result = try self.callBytecodeFunction(func_val, func_bc, this_val, args[0..argc]);

                // Create a resolved Promise-like object {then: fn, value: result}
                const promise_obj = self.ctx.createObject(null) catch {
                    return error.OutOfMemory;
                };

                // Store the resolved value
                const value_atom = self.ctx.atoms.intern("value") catch return error.OutOfMemory;
                self.ctx.setPropertyChecked(promise_obj, value_atom, result) catch |err| {
                    return switch (err) {
                        error.ArenaObjectEscape => error.ArenaObjectEscape,
                        else => error.OutOfMemory,
                    };
                };

                // Add a 'then' method (simplified - just calls callback with value)
                // For a full implementation, we'd create a proper Promise with thenable support
                const then_atom = self.ctx.atoms.intern("then") catch return error.OutOfMemory;
                self.ctx.setPropertyChecked(promise_obj, then_atom, value.JSValue.true_val) catch |err| {
                    return switch (err) {
                        error.ArenaObjectEscape => error.ArenaObjectEscape,
                        else => error.OutOfMemory,
                    };
                };

                try self.ctx.push(promise_obj.toValue());
                return;
            }

            const result = try self.callBytecodeFunction(func_val, func_bc, this_val, args[0..argc]);
            try self.ctx.push(result);
            return;
        }

        // Unknown function type
        try self.ctx.push(value.JSValue.undefined_val);
    }

    /// Dispatch a single opcode (returns result if halt/ret, error otherwise to continue)
    fn dispatchOne(self: *Interpreter) InterpreterError!value.JSValue {
        const op: bytecode.Opcode = @enumFromInt(self.pc[0]);
        self.pc += 1;
        self.last_op = op;

        switch (op) {
            .nop => return error.NoTransition,
            .halt => {
                if (self.ctx.sp > 0) return self.ctx.pop();
                return value.JSValue.undefined_val;
            },
            .push_0 => {
                try self.ctx.push(value.JSValue.fromInt(0));
                return error.NoTransition;
            },
            .push_1 => {
                try self.ctx.push(value.JSValue.fromInt(1));
                return error.NoTransition;
            },
            .push_2 => {
                try self.ctx.push(value.JSValue.fromInt(2));
                return error.NoTransition;
            },
            .push_3 => {
                try self.ctx.push(value.JSValue.fromInt(3));
                return error.NoTransition;
            },
            .push_null => {
                try self.ctx.push(value.JSValue.null_val);
                return error.NoTransition;
            },
            .push_undefined => {
                try self.ctx.push(value.JSValue.undefined_val);
                return error.NoTransition;
            },
            .push_true => {
                try self.ctx.push(value.JSValue.true_val);
                return error.NoTransition;
            },
            .push_false => {
                try self.ctx.push(value.JSValue.false_val);
                return error.NoTransition;
            },
            .push_i8 => {
                const val: i8 = @bitCast(self.pc[0]);
                self.pc += 1;
                try self.ctx.push(value.JSValue.fromInt(val));
                return error.NoTransition;
            },
            .push_const => {
                const idx = readU16(self.pc);
                self.pc += 2;
                try self.ctx.push(try self.getConstant(idx));
                return error.NoTransition;
            },
            .get_loc => {
                const idx = self.pc[0];
                self.pc += 1;
                try self.ctx.push(self.ctx.getLocal(idx));
                return error.NoTransition;
            },
            .put_loc => {
                const idx = self.pc[0];
                self.pc += 1;
                self.ctx.setLocal(idx, self.ctx.pop());
                return error.NoTransition;
            },
            .get_loc_0 => {
                try self.ctx.push(self.ctx.getLocal(0));
                return error.NoTransition;
            },
            .get_loc_1 => {
                try self.ctx.push(self.ctx.getLocal(1));
                return error.NoTransition;
            },
            .get_loc_2 => {
                try self.ctx.push(self.ctx.getLocal(2));
                return error.NoTransition;
            },
            .get_loc_3 => {
                try self.ctx.push(self.ctx.getLocal(3));
                return error.NoTransition;
            },
            .put_loc_0 => {
                self.ctx.setLocal(0, self.ctx.pop());
                return error.NoTransition;
            },
            .put_loc_1 => {
                self.ctx.setLocal(1, self.ctx.pop());
                return error.NoTransition;
            },
            .put_loc_2 => {
                self.ctx.setLocal(2, self.ctx.pop());
                return error.NoTransition;
            },
            .put_loc_3 => {
                self.ctx.setLocal(3, self.ctx.pop());
                return error.NoTransition;
            },
            .add => {
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                try self.ctx.push(try self.addValues(a, b));
                return error.NoTransition;
            },
            .sub => {
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                try self.ctx.push(try self.subValues(a, b));
                return error.NoTransition;
            },
            .lt => {
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                const order = try compareValues(a, b);
                try self.ctx.push(if (order == .lt) value.JSValue.true_val else value.JSValue.false_val);
                return error.NoTransition;
            },
            .lte => {
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                const order = try compareValues(a, b);
                try self.ctx.push(if (order != .gt) value.JSValue.true_val else value.JSValue.false_val);
                return error.NoTransition;
            },
            .gt => {
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                const order = try compareValues(a, b);
                try self.ctx.push(if (order == .gt) value.JSValue.true_val else value.JSValue.false_val);
                return error.NoTransition;
            },
            .gte => {
                const b = self.ctx.pop();
                const a = self.ctx.pop();
                const order = try compareValues(a, b);
                try self.ctx.push(if (order != .lt) value.JSValue.true_val else value.JSValue.false_val);
                return error.NoTransition;
            },
            .if_true => {
                const cond = self.ctx.pop();
                const offset = readI16(self.pc);
                self.pc += 2;
                if (cond.toBoolean()) {
                    self.offsetPc(offset);
                }
                return error.NoTransition;
            },
            .if_false => {
                const cond = self.ctx.pop();
                const offset = readI16(self.pc);
                self.pc += 2;
                if (!cond.toBoolean()) {
                    self.offsetPc(offset);
                }
                return error.NoTransition;
            },
            .goto => {
                const offset = readI16(self.pc);
                self.pc += 2;
                self.offsetPc(offset);
                return error.NoTransition;
            },
            .loop => {
                const offset = readI16(self.pc);
                self.pc += 2;
                self.offsetPc(-offset);
                return error.NoTransition;
            },
            .inc => {
                const val = self.ctx.pop();
                if (val.isInt()) {
                    try self.ctx.push(value.JSValue.fromInt(val.getInt() + 1));
                } else {
                    try self.ctx.push(value.JSValue.undefined_val);
                }
                return error.NoTransition;
            },
            .dec => {
                const val = self.ctx.pop();
                if (val.isInt()) {
                    try self.ctx.push(value.JSValue.fromInt(val.getInt() - 1));
                } else {
                    try self.ctx.push(value.JSValue.undefined_val);
                }
                return error.NoTransition;
            },
            .dup => {
                try self.ctx.push(self.ctx.peek());
                return error.NoTransition;
            },
            .drop => {
                _ = self.ctx.pop();
                return error.NoTransition;
            },
            .ret => return self.ctx.pop(),
            .ret_undefined => return value.JSValue.undefined_val,
            else => return error.UnimplementedOpcode,
        }
    }
};

/// JIT helper: perform a call/call_method from compiled code.
/// Pops arguments and function from the context stack using interpreter logic,
/// then returns the result as a JSValue.
pub export fn jitCall(ctx: *context.Context, argc: u8, is_method: u8) value.JSValue {
    const interp = current_interpreter orelse {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    interp.doCall(argc, is_method != 0) catch {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    return ctx.pop();
}

/// JIT helper: get_field_ic using the interpreter's PIC cache.
pub export fn jitGetFieldIC(ctx: *context.Context, obj_val: value.JSValue, atom_idx: u16, cache_idx: u16) value.JSValue {
    const interp = current_interpreter orelse {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    const atom: object.Atom = @enumFromInt(atom_idx);
    if (obj_val.isObject()) {
        const obj = object.JSObject.fromValue(obj_val);
        const pic = &interp.pic_cache[cache_idx];

        if (pic.lookup(obj.hidden_class_idx)) |slot_offset| {
            interp.pic_hits +%= 1;
            return obj.getSlot(slot_offset);
        }

        interp.pic_misses +%= 1;
        const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
        if (pool.findProperty(obj.hidden_class_idx, atom)) |slot_offset| {
            _ = pic.update(obj.hidden_class_idx, slot_offset);
            return obj.getSlot(slot_offset);
        }
        if (obj.getProperty(pool, atom)) |prop_val| {
            return prop_val;
        }
        return value.JSValue.undefined_val;
    } else if (obj_val.isString()) {
        if (atom == .length) {
            const str = obj_val.toPtr(string.JSString);
            return value.JSValue.fromInt(@intCast(str.len));
        }
        if (ctx.string_prototype) |proto| {
            const pool = ctx.hidden_class_pool orelse return value.JSValue.undefined_val;
            return proto.getProperty(pool, atom) orelse value.JSValue.undefined_val;
        }
        return value.JSValue.undefined_val;
    }

    return value.JSValue.undefined_val;
}

/// JIT helper: put_field_ic using the interpreter's PIC cache.
pub export fn jitPutFieldIC(ctx: *context.Context, obj_val: value.JSValue, atom_idx: u16, val: value.JSValue, cache_idx: u16) value.JSValue {
    const interp = current_interpreter orelse {
        ctx.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    };

    const atom: object.Atom = @enumFromInt(atom_idx);
    if (obj_val.isObject()) {
        const obj = object.JSObject.fromValue(obj_val);
        const pic = &interp.pic_cache[cache_idx];

        if (ctx.enforce_arena_escape and ctx.hybrid != null and !obj.flags.is_arena and ctx.isEphemeralValue(val)) {
            ctx.throwException(value.JSValue.exception_val);
            return value.JSValue.exception_val;
        }

        if (pic.lookup(obj.hidden_class_idx)) |slot_offset| {
            interp.pic_hits +%= 1;
            obj.setSlot(slot_offset, val);
            return val;
        }

        interp.pic_misses +%= 1;
        const pool = ctx.hidden_class_pool orelse {
            ctx.setPropertyChecked(obj, atom, val) catch {
                ctx.throwException(value.JSValue.exception_val);
                return value.JSValue.exception_val;
            };
            return val;
        };

        if (pool.findProperty(obj.hidden_class_idx, atom)) |slot_offset| {
            _ = pic.update(obj.hidden_class_idx, slot_offset);
            obj.setSlot(slot_offset, val);
            return val;
        }

        ctx.setPropertyChecked(obj, atom, val) catch {
            ctx.throwException(value.JSValue.exception_val);
            return value.JSValue.exception_val;
        };
    }

    return val;
}

// ============================================================================
// Helper Functions (standalone, used by interpreter)
// ============================================================================

/// Modulo two values
/// Modulo operation - optimized for integer fast path
inline fn modValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const bv = b.getInt();
        if (bv == 0) return error.DivisionByZero;
        return value.JSValue.fromInt(@rem(a.getInt(), bv));
    }
    return error.TypeError;
}

/// Compare two values
fn compareValues(a: value.JSValue, b: value.JSValue) !std.math.Order {
    // Integer fast path
    if (a.isInt() and b.isInt()) {
        return std.math.order(a.getInt(), b.getInt());
    }
    // Float comparison
    const an = a.toNumber() orelse return error.TypeError;
    const bn = b.toNumber() orelse return error.TypeError;
    // NaN comparisons are always false
    if (std.math.isNan(an) or std.math.isNan(bn)) {
        return error.TypeError;
    }
    return std.math.order(an, bn);
}

/// Convert value to Int32 (for bitwise operations, per ECMAScript ToInt32)
inline fn toInt32(v: value.JSValue) i32 {
    if (v.isInt()) return v.getInt();
    if (v.isFloat64()) {
        const f = v.getFloat64();
        if (std.math.isNan(f) or std.math.isInf(f) or f == 0) return 0;
        // ToInt32 truncation
        const int_val: i64 = @intFromFloat(@trunc(f));
        return @truncate(int_val);
    }
    return 0;
}

/// Loose equality (==)
fn looseEquals(a: value.JSValue, b: value.JSValue) bool {
    // Same type - strict equals
    if (a.raw == b.raw) return true;

    // null == undefined
    if ((a.isNull() and b.isUndefined()) or (a.isUndefined() and b.isNull())) return true;

    // Number comparison
    if (a.isNumber() and b.isNumber()) {
        const an = a.toNumber() orelse return false;
        const bn = b.toNumber() orelse return false;
        if (std.math.isNan(an) or std.math.isNan(bn)) return false;
        return an == bn;
    }

    // TODO: String coercion, object coercion
    return false;
}

/// Read i16 from bytecode
fn readI16(pc: [*]const u8) i16 {
    return @bitCast(@as(u16, pc[0]) | (@as(u16, pc[1]) << 8));
}

/// Read u16 from bytecode
fn readU16(pc: [*]const u8) u16 {
    return @as(u16, pc[0]) | (@as(u16, pc[1]) << 8);
}

test "Interpreter basic arithmetic" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Test code: push 3, push 4, add, ret
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_3),
        @intFromEnum(bytecode.Opcode.push_i8),
        4,
        @intFromEnum(bytecode.Opcode.add),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 7), result.getInt());
}

test "Interpreter local variables" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: local0 = 10; local1 = 20; return local0 + local1
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),   10,
        @intFromEnum(bytecode.Opcode.put_loc_0), @intFromEnum(bytecode.Opcode.push_i8),
        20,                                      @intFromEnum(bytecode.Opcode.put_loc_1),
        @intFromEnum(bytecode.Opcode.get_loc_0), @intFromEnum(bytecode.Opcode.get_loc_1),
        @intFromEnum(bytecode.Opcode.add),       @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 2,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 30), result.getInt());
}

test "Interpreter bitwise operations" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 0xFF & 0x0F = 0x0F = 15
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 0xFF,
        @intFromEnum(bytecode.Opcode.push_i8), 0x0F,
        @intFromEnum(bytecode.Opcode.bit_and), @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 0x0F), result.getInt());
}

test "Interpreter shift operations" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 1 << 4 = 16
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_1),
        @intFromEnum(bytecode.Opcode.push_i8),
        4,
        @intFromEnum(bytecode.Opcode.shl),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 16), result.getInt());
}

test "Interpreter comparison" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 5 < 10 = true
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 5,
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.lt),      @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isTrue());
}

test "Interpreter conditional jump" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: if (true) { return 42 } else { return 0 }
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_true),
        @intFromEnum(bytecode.Opcode.if_false), 3,                                 0, // jump +3 if false
        @intFromEnum(bytecode.Opcode.push_i8),  42,                                @intFromEnum(bytecode.Opcode.ret),
        @intFromEnum(bytecode.Opcode.push_0),   @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 42), result.getInt());
}

test "Interpreter superinstruction get_loc_get_loc_add" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: local0 = 7; local1 = 8; return local0 + local1 (using superinstruction)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),             7,
        @intFromEnum(bytecode.Opcode.put_loc_0),           @intFromEnum(bytecode.Opcode.push_i8),
        8,                                                 @intFromEnum(bytecode.Opcode.put_loc_1),
        @intFromEnum(bytecode.Opcode.get_loc_get_loc_add), 0,
        1,                                                 @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 2,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 15), result.getInt());
}

test "Interpreter division produces float" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 7 / 2 = 3.5
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 7,
        @intFromEnum(bytecode.Opcode.push_2),  @intFromEnum(bytecode.Opcode.div),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isFloat64());
    try std.testing.expectEqual(@as(f64, 3.5), result.getFloat64());
}

test "Interpreter modulo" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Code: 17 % 5 = 2
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 17,
        @intFromEnum(bytecode.Opcode.push_i8), 5,
        @intFromEnum(bytecode.Opcode.mod),     @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 2), result.getInt());
}

test "End-to-end: parse and execute JS" {
    // Use arena to avoid memory leak detection issues with function bytecode
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test: simple expression parsing and execution
    // Note: Expression statements drop their values, so we just verify execution completes
    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    var p = parser_mod.Parser.init(allocator, "function f() { return 1 + 2; } f()", &strings, null);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    // Expression statements drop their values, so result is undefined
    // Full integration testing is done in zruntime tests
    const result = try interp.run(&func);
    try std.testing.expect(result.isUndefined());
}

test "Hybrid: reject arena escape to global" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");
    const arena_mod = @import("arena.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var req_arena = try arena_mod.Arena.init(allocator, .{ .size = 4096 });
    defer req_arena.deinit();
    var hybrid = arena_mod.HybridAllocator{
        .persistent = allocator,
        .arena = &req_arena,
    };
    ctx.setHybridAllocator(&hybrid);

    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    const source =
        \\let x = { a: 1 };
    ;

    var p = parser_mod.Parser.init(allocator, source, &strings, &ctx.atoms);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    try std.testing.expectError(error.ArenaObjectEscape, interp.run(&func));
}

test "End-to-end: closure captures local" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    const source =
        \\function make() { let x = 2; return () => x + 3; }
        \\let f = make();
        \\let result = f();
    ;

    var p = parser_mod.Parser.init(allocator, source, &strings, &ctx.atoms);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    _ = try interp.run(&func);

    const result_atom = try ctx.atoms.intern("result");
    const result_opt = ctx.getGlobal(result_atom);
    try std.testing.expect(result_opt != null);
    const result_val = result_opt.?;
    try std.testing.expect(result_val.isInt());
    try std.testing.expectEqual(@as(i32, 5), result_val.getInt());
}

test "End-to-end: JSX parse, compile, and execute" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");
    const builtins = @import("builtins.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();
    try builtins.initBuiltins(ctx);

    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    const source =
        \\let result = renderToString(<div><span>Hi</span></div>);
        \\let link = renderToString(<a href="/api/health">GET /api/health</a>);
    ;

    var p = parser_mod.Parser.init(allocator, source, &strings, &ctx.atoms);
    defer p.deinit();
    p.enableJsx();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    _ = try interp.run(&func);

    const result_atom = try ctx.atoms.intern("result");
    const link_atom = try ctx.atoms.intern("link");

    const result_opt = ctx.getGlobal(result_atom);
    const link_opt = ctx.getGlobal(link_atom);
    try std.testing.expect(result_opt != null);
    try std.testing.expect(link_opt != null);

    const result_val = result_opt.?;
    const link_val = link_opt.?;

    try std.testing.expect(result_val.isString());
    try std.testing.expect(link_val.isString());

    const result_str = result_val.toPtr(string.JSString).data();
    const link_str = link_val.toPtr(string.JSString).data();

    try std.testing.expectEqualStrings("<div><span>Hi</span></div>", result_str);
    try std.testing.expectEqualStrings(
        "<a href=\"/api/health\">GET /api/health</a>",
        link_str,
    );
}

test "Interpreter property access" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Test: new_object, put_field (length = 42), get_field, ret
    // Atom.length = 4
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.new_object),
        @intFromEnum(bytecode.Opcode.dup), // Duplicate obj for get_field
        @intFromEnum(bytecode.Opcode.push_i8),
        42,
        @intFromEnum(bytecode.Opcode.put_field),
        4,
        0, // Atom.length = 4 (little-endian u16)
        @intFromEnum(bytecode.Opcode.get_field),
        4,
        0, // Atom.length = 4 (little-endian u16)
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 42), result.getInt());
}

test "Interpreter global access" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Set a global value
    try ctx.setGlobal(.length, value.JSValue.fromInt(100));

    var interp = Interpreter.init(ctx);

    // Test: get_global(length), ret
    // Atom.length = 4
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        4,
        0, // Atom.length = 4 (little-endian u16)
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 100), result.getInt());
}

test "Interpreter native function call" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Define a native function that returns 42
    const testFn = struct {
        fn call(_: *anyopaque, _: value.JSValue, _: []const value.JSValue) anyerror!value.JSValue {
            return value.JSValue.fromInt(42);
        }
    }.call;

    // Register as global "abs" (Atom.abs = 95)
    try ctx.registerGlobalFunction(.abs, testFn, 0);

    var interp = Interpreter.init(ctx);

    // Test: get_global(abs), call(0), ret
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        95,
        0, // Atom.abs = 95 (little-endian u16)
        @intFromEnum(bytecode.Opcode.call),
        0, // 0 arguments
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 42), result.getInt());
}

test "Interpreter native function with arguments" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Define a native function that adds two numbers
    const addFn = struct {
        fn call(_: *anyopaque, _: value.JSValue, args: []const value.JSValue) anyerror!value.JSValue {
            if (args.len < 2) return value.JSValue.undefined_val;
            const a = args[0].getInt();
            const b = args[1].getInt();
            return value.JSValue.fromInt(a + b);
        }
    }.call;

    // Register as global "max" (Atom.max = 100)
    try ctx.registerGlobalFunction(.max, addFn, 2);

    var interp = Interpreter.init(ctx);

    // Test: get_global(max), push 10, push 20, call(2), ret
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        100,
        0, // Atom.max = 100 (little-endian u16)
        @intFromEnum(bytecode.Opcode.push_i8),
        10,
        @intFromEnum(bytecode.Opcode.push_i8),
        20,
        @intFromEnum(bytecode.Opcode.call),
        2, // 2 arguments
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 30), result.getInt());
}

test "Interpreter bytecode function call" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create a simple function: function add(a, b) { return a + b; }
    // Bytecode: get_loc_0, get_loc_1, add, ret
    // Must heap-allocate since destroyFull will free bc.code
    const inner_code = try allocator.alloc(u8, 4);
    inner_code[0] = @intFromEnum(bytecode.Opcode.get_loc_0); // a
    inner_code[1] = @intFromEnum(bytecode.Opcode.get_loc_1); // b
    inner_code[2] = @intFromEnum(bytecode.Opcode.add);
    inner_code[3] = @intFromEnum(bytecode.Opcode.ret);

    // Must heap-allocate the FunctionBytecode since createBytecodeFunction stores a pointer
    const inner_func = try allocator.create(bytecode.FunctionBytecode);
    inner_func.* = .{
        .header = .{},
        .name_atom = 0,
        .arg_count = 2,
        .local_count = 2, // a, b
        .stack_size = 16,
        .flags = .{},
        .code = inner_code,
        .constants = &.{},
        .source_map = null,
    };

    // Create function object
    const root_class_idx = ctx.root_class_idx;
    const func_obj = try object.JSObject.createBytecodeFunction(allocator, root_class_idx, inner_func, .length);

    // Register as global "add" (Atom.abs = 95)
    // The function will be cleaned up by ctx.deinit() -> destroyBuiltin -> destroyFull
    try ctx.setGlobal(.abs, func_obj.toValue());

    var interp = Interpreter.init(ctx);

    // Test: get_global(abs), push 7, push 8, call(2), ret
    // This code is stack-allocated but only used for interp.run, not stored
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.get_global),
        95,
        0, // Atom.abs = 95 (little-endian u16)
        @intFromEnum(bytecode.Opcode.push_i8),
        7,
        @intFromEnum(bytecode.Opcode.push_i8),
        8,
        @intFromEnum(bytecode.Opcode.call),
        2, // 2 arguments
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 15), result.getInt()); // 7 + 8 = 15
}

test "End-to-end: function declaration" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test: nested function declaration and call
    // Note: Expression statements drop values, so we verify execution completes
    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    const code_str = "function outer() { function add(a, b) { return a + b; } return add(3, 4); } outer()";
    var p = parser_mod.Parser.init(allocator, code_str, &strings, null);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    // Expression statements drop values, so result is undefined
    const result = try interp.run(&func);
    try std.testing.expect(result.isUndefined());
}

test "Interpreter string concatenation" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Create two string constants
    const str1 = try string.createString(allocator, "hello");
    const str2 = try string.createString(allocator, " world");

    // Test: push "hello", push " world", add (concat), ret
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_const),
        0,
        0, // constant 0
        @intFromEnum(bytecode.Opcode.push_const),
        1,
        0, // constant 1
        @intFromEnum(bytecode.Opcode.add),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const constants = [_]value.JSValue{
        value.JSValue.fromPtr(str1),
        value.JSValue.fromPtr(str2),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &constants,
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isString());
    const result_str = result.toPtr(string.JSString);
    try std.testing.expectEqualStrings("hello world", result_str.data());

    // Cleanup
    string.freeString(allocator, result_str);
    string.freeString(allocator, str1);
    string.freeString(allocator, str2);
}

test "Interpreter typeof" {
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Test: typeof 42 = "number"
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),
        42,
        @intFromEnum(bytecode.Opcode.typeof),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 16,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    const result = try interp.run(&func);
    try std.testing.expect(result.isString());
    const result_str = result.toPtr(string.JSString);
    try std.testing.expectEqualStrings("number", result_str.data());

    // Cleanup
    string.freeString(allocator, result_str);
}

test "End-to-end: default parameters" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test: function with default parameter
    // Note: Expression statements drop values, so we verify execution completes
    var strings = string_mod.StringTable.init(allocator);

    const code_str = "function outer() { function greet(name = 'World') { return name; } return greet(); } outer()";
    var p = parser_mod.Parser.init(allocator, code_str, &strings, null);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    // Expression statements drop values, so result is undefined
    const result = try interp.run(&func);
    try std.testing.expect(result.isUndefined());
}

test "Interpreter unary negation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 42, negate it: -42
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 42,
        @intFromEnum(bytecode.Opcode.neg),     @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, -42), result.getInt());
}

test "Interpreter increment and decrement" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 10, increment: 11
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.inc),     @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 11), result.getInt());
}

test "Interpreter logical not" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // !true = false
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_true),
        @intFromEnum(bytecode.Opcode.not),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.false_val, result);
}

test "Interpreter bitwise not" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // ~0 = -1 (all bits flipped)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_0),
        @intFromEnum(bytecode.Opcode.bit_not),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, -1), result.getInt());
}

test "Interpreter power operator" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // 2 ** 10 = 1024
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_2),
        @intFromEnum(bytecode.Opcode.push_i8),
        10,
        @intFromEnum(bytecode.Opcode.pow),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    // pow always returns a float
    try std.testing.expect(result.isFloat64());
    try std.testing.expectEqual(@as(f64, 1024.0), result.getFloat64());
}

test "Interpreter new_object" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create empty object
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.new_object),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isObject());
}

test "Interpreter strict equality edge cases" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // null === null should be true
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_null),
        @intFromEnum(bytecode.Opcode.push_null),
        @intFromEnum(bytecode.Opcode.strict_eq),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.true_val, result);
}

test "Interpreter local variable get and put" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Store 42 in local 0, then return it
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),   42,
        @intFromEnum(bytecode.Opcode.put_loc_0), @intFromEnum(bytecode.Opcode.get_loc_0),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 1,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 42), result.getInt());
}

test "Interpreter multiple locals" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Store 10 in local 0, 20 in local 1, add them
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8),   10,
        @intFromEnum(bytecode.Opcode.put_loc_0), @intFromEnum(bytecode.Opcode.push_i8),
        20,                                      @intFromEnum(bytecode.Opcode.put_loc_1),
        @intFromEnum(bytecode.Opcode.get_loc_0), @intFromEnum(bytecode.Opcode.get_loc_1),
        @intFromEnum(bytecode.Opcode.add),       @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 2,
        .stack_size = 4,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 30), result.getInt());
}

test "Interpreter undefined equals undefined" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // undefined === undefined should be true
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_undefined),
        @intFromEnum(bytecode.Opcode.push_undefined),
        @intFromEnum(bytecode.Opcode.strict_eq),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.true_val, result);
}

test "Interpreter null vs undefined strict not equal" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // null === undefined should be false
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_null),
        @intFromEnum(bytecode.Opcode.push_undefined),
        @intFromEnum(bytecode.Opcode.strict_eq),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.false_val, result);
}

test "Interpreter typeof operations" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // typeof 42 should return "number"
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 42,
        @intFromEnum(bytecode.Opcode.typeof),  @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isString());
    try std.testing.expectEqualStrings("number", result.toPtr(string.JSString).data());
}

test "Interpreter modulo operation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // 10 % 3 = 1
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.push_3),  @intFromEnum(bytecode.Opcode.mod),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 1), result.getInt());
}

test "Interpreter inc dec roundtrip" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // 5 -> inc -> dec -> should be 5
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 5,
        @intFromEnum(bytecode.Opcode.inc),     @intFromEnum(bytecode.Opcode.dec),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 5), result.getInt());
}

test "Interpreter negation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // -42 should be -42
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 42,
        @intFromEnum(bytecode.Opcode.neg),     @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, -42), result.getInt());
}

test "Interpreter dup operation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 7, dup, add -> 14
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 7,
        @intFromEnum(bytecode.Opcode.dup),     @intFromEnum(bytecode.Opcode.add),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 14), result.getInt());
}

test "Interpreter drop operation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 5, push 10, drop, ret -> should return 5
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 5,
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.drop),    @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 5), result.getInt());
}

test "Interpreter swap operation" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 10, push 3, swap, sub -> 10 - 3 = 7
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.push_3),  @intFromEnum(bytecode.Opcode.swap),
        @intFromEnum(bytecode.Opcode.sub),     @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 3,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    // After swap: [3, 10], sub: 3 - 10 = -7
    try std.testing.expectEqual(@as(i32, -7), result.getInt());
}

test "Interpreter not operator inverts false" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // !false = true
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_false),
        @intFromEnum(bytecode.Opcode.not),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(value.JSValue.true_val, result);
}

test "Interpreter new_array" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create empty array (new_array takes u16 length)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.new_array), 0, 0, // u16 length = 0
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isObject());
}

test "Interpreter ret_undefined" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.ret_undefined),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expect(result.isUndefined());
}

test "Interpreter push_i16" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push 1000 (requires i16)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_i16),
        @as(u8, @truncate(1000 & 0xFF)), // low byte
        @as(u8, @truncate((1000 >> 8) & 0xFF)), // high byte
        @intFromEnum(bytecode.Opcode.ret),
    };

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 2,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    const result = try interp.run(&func);
    try std.testing.expectEqual(@as(i32, 1000), result.getInt());
}

test "PolymorphicInlineCache unit tests" {
    // Test PIC lookup and update
    var pic = PolymorphicInlineCache{};

    // Create distinct hidden class indices
    const class1: object.HiddenClassIndex = @enumFromInt(1);
    const class2: object.HiddenClassIndex = @enumFromInt(2);
    const class3: object.HiddenClassIndex = @enumFromInt(3);
    const class4: object.HiddenClassIndex = @enumFromInt(4);
    const class5: object.HiddenClassIndex = @enumFromInt(5);

    // Initially empty
    try std.testing.expectEqual(@as(?u16, null), pic.lookup(class1));
    try std.testing.expectEqual(@as(u8, 0), pic.count);
    try std.testing.expect(!pic.megamorphic);

    // Add first entry
    try std.testing.expect(pic.update(class1, 10));
    try std.testing.expectEqual(@as(u8, 1), pic.count);
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1));

    // Add second entry
    try std.testing.expect(pic.update(class2, 20));
    try std.testing.expectEqual(@as(u8, 2), pic.count);
    try std.testing.expectEqual(@as(?u16, 20), pic.lookup(class2));

    // First entry still works
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1));

    // Add third and fourth entries
    try std.testing.expect(pic.update(class3, 30));
    try std.testing.expect(pic.update(class4, 40));
    try std.testing.expectEqual(@as(u8, 4), pic.count);
    try std.testing.expect(!pic.megamorphic);

    // All four entries work
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1));
    try std.testing.expectEqual(@as(?u16, 20), pic.lookup(class2));
    try std.testing.expectEqual(@as(?u16, 30), pic.lookup(class3));
    try std.testing.expectEqual(@as(?u16, 40), pic.lookup(class4));

    // Fifth entry triggers megamorphic
    try std.testing.expect(!pic.update(class5, 50));
    try std.testing.expect(pic.megamorphic);
    try std.testing.expectEqual(@as(?u16, null), pic.lookup(class5));

    // Existing entries still work (megamorphic doesn't clear cache)
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1));

    // Update existing entry in megamorphic state - no change
    try std.testing.expect(!pic.update(class1, 100));
    try std.testing.expectEqual(@as(?u16, 10), pic.lookup(class1)); // unchanged

    // Reset test
    pic.reset();
    try std.testing.expectEqual(@as(u8, 0), pic.count);
    try std.testing.expect(!pic.megamorphic);
    try std.testing.expectEqual(@as(?u16, null), pic.lookup(class1));
}

test "End-to-end: polymorphic property access" {
    // Test that PIC handles multiple object shapes correctly
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");
    const heap_mod = @import("heap.zig");
    const parser_mod = @import("parser/root.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var heap_state = heap_mod.Heap.init(allocator, .{});
    defer heap_state.deinit();
    gc_state.setHeap(&heap_state);

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    // Test: access .x on objects with different shapes (polymorphic site)
    // Each object has a different set of properties, so different hidden classes
    const source =
        \\function getX(obj) { return obj.x; }
        \\let a = { x: 1 };
        \\let b = { x: 10, y: 2 };
        \\let c = { x: 100, y: 3, z: 4 };
        \\let d = { w: 0, x: 1000 };
        \\let result = getX(a) + getX(b) + getX(c) + getX(d);
    ;

    var p = parser_mod.Parser.init(allocator, source, &strings, &ctx.atoms);
    defer p.deinit();

    const code = try p.parse();
    try std.testing.expect(code.len > 0);

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = code,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = Interpreter.init(ctx);
    _ = try interp.run(&func);

    // Get result from global
    const result_atom = try ctx.atoms.intern("result");
    const result_opt = ctx.getGlobal(result_atom);
    try std.testing.expect(result_opt != null);

    const result_val = result_opt.?;
    try std.testing.expect(result_val.isInt());
    // 1 + 10 + 100 + 1000 = 1111
    try std.testing.expectEqual(@as(i32, 1111), result_val.getInt());
}

test "JIT profiling: execution counting" {
    // Test that function execution counting works correctly
    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &.{},
        .constants = &.{},
        .source_map = null,
    };

    // Initially at tier interpreted with zero count
    try std.testing.expectEqual(bytecode.CompilationTier.interpreted, func.tier);
    try std.testing.expectEqual(@as(u32, 0), func.execution_count);

    // Simulate function calls up to threshold - 1
    var i: u32 = 0;
    while (i < bytecode.JIT_THRESHOLD - 1) : (i += 1) {
        const promoted = Interpreter.profileFunctionEntry(&func);
        try std.testing.expect(!promoted);
        try std.testing.expectEqual(bytecode.CompilationTier.interpreted, func.tier);
    }
    try std.testing.expectEqual(bytecode.JIT_THRESHOLD - 1, func.execution_count);

    // One more call should hit threshold and promote to baseline_candidate
    const promoted = Interpreter.profileFunctionEntry(&func);
    try std.testing.expect(promoted);
    try std.testing.expectEqual(bytecode.CompilationTier.baseline_candidate, func.tier);
    try std.testing.expectEqual(bytecode.JIT_THRESHOLD, func.execution_count);

    // Subsequent calls don't re-promote
    const promoted2 = Interpreter.profileFunctionEntry(&func);
    try std.testing.expect(!promoted2);
    try std.testing.expectEqual(bytecode.CompilationTier.baseline_candidate, func.tier);
}

test "JIT profiling: back-edge counting" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Initially zero
    try std.testing.expectEqual(@as(u32, 0), interp.backedge_count);

    // Simulate back-edges up to threshold - 1
    var i: u32 = 0;
    while (i < bytecode.LOOP_THRESHOLD - 1) : (i += 1) {
        const hot = interp.profileBackedge();
        try std.testing.expect(!hot);
    }

    // One more should hit threshold
    const hot = interp.profileBackedge();
    try std.testing.expect(hot);
    try std.testing.expectEqual(bytecode.LOOP_THRESHOLD, interp.backedge_count);

    // Reset and verify
    interp.resetProfilingCounters();
    try std.testing.expectEqual(@as(u32, 0), interp.backedge_count);
}

test "JIT profiling: PIC hit/miss counting" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Initially zero
    try std.testing.expectEqual(@as(u32, 0), interp.pic_hits);
    try std.testing.expectEqual(@as(u32, 0), interp.pic_misses);

    // Simulate some hits and misses
    interp.pic_hits = 100;
    interp.pic_misses = 10;

    // Calculate hit rate
    const total = interp.pic_hits + interp.pic_misses;
    const hit_rate = @as(f32, @floatFromInt(interp.pic_hits)) / @as(f32, @floatFromInt(total));
    try std.testing.expect(hit_rate > 0.9); // 90%+ hit rate

    // Reset and verify
    interp.resetProfilingCounters();
    try std.testing.expectEqual(@as(u32, 0), interp.pic_hits);
    try std.testing.expectEqual(@as(u32, 0), interp.pic_misses);
}

test "JIT integration: tryCompileBaseline triggers on threshold" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Create a simple bytecode function that can be JIT-compiled
    // push_1 + ret is supported by the baseline compiler
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.push_1),
        @intFromEnum(bytecode.Opcode.ret),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
        .execution_count = 0,
        .tier = .interpreted,
        .compiled_code = null,
    };

    // Initially interpreted
    try std.testing.expectEqual(bytecode.CompilationTier.interpreted, func.tier);
    try std.testing.expect(func.compiled_code == null);

    // Simulate calls up to threshold - 1
    func.execution_count = bytecode.JIT_THRESHOLD - 1;

    // Next call should trigger JIT compilation
    const is_candidate = Interpreter.profileFunctionEntry(&func);
    try std.testing.expect(is_candidate);
    try std.testing.expectEqual(bytecode.CompilationTier.baseline_candidate, func.tier);

    // Try to compile
    try interp.tryCompileBaseline(&func);

    // Should now be compiled
    try std.testing.expectEqual(bytecode.CompilationTier.baseline, func.tier);
    try std.testing.expect(func.compiled_code != null);

    // Clean up the compiled code
    if (func.compiled_code) |cc_opaque| {
        const cc: *jit.CompiledCode = @ptrCast(@alignCast(cc_opaque));
        allocator.destroy(cc);
    }
}

test "JIT integration: unsupported opcodes stay interpreted" {
    const allocator = std.testing.allocator;
    const gc = @import("gc.zig");

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var interp = Interpreter.init(ctx);

    // Create a bytecode function with unsupported opcode (call_spread)
    const code = [_]u8{
        @intFromEnum(bytecode.Opcode.call_spread),
    };

    var func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 0,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
        .execution_count = bytecode.JIT_THRESHOLD - 1,
        .tier = .baseline_candidate,
        .compiled_code = null,
    };

    // Try to compile - should fail with UnsupportedOpcode and revert to interpreted
    try interp.tryCompileBaseline(&func);

    // Should stay interpreted (not baseline)
    try std.testing.expectEqual(bytecode.CompilationTier.interpreted, func.tier);
    try std.testing.expect(func.compiled_code == null);
}
