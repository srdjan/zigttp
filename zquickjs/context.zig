//! JavaScript execution context
//!
//! Thread-local context with stack, atoms, and global state.

const std = @import("std");
const value = @import("value.zig");
const gc = @import("gc.zig");
const object = @import("object.zig");

/// Context configuration
pub const ContextConfig = struct {
    stack_size: usize = 1024 * 1024, // 1MB value stack
    call_stack_size: usize = 1024, // Max call depth
    init_globals: bool = true, // Initialize global object
};

/// Call frame on the call stack
pub const CallFrame = struct {
    return_pc: usize,
    return_sp: usize,
    return_fp: usize,
    func: value.JSValue,
    this: value.JSValue,
};

/// Exception handler entry
pub const CatchHandler = struct {
    catch_pc: usize, // PC to jump to on exception
    sp: usize, // Stack pointer to restore
    fp: usize, // Frame pointer at entry
};

/// JavaScript execution context
pub const Context = struct {
    /// Allocator for context-owned memory
    allocator: std.mem.Allocator,
    /// Garbage collector
    gc_state: *gc.GC,
    /// Value stack
    stack: []value.JSValue,
    /// Stack pointer (grows up)
    sp: usize,
    /// Frame pointer
    fp: usize,
    /// Call stack
    call_stack: []CallFrame,
    /// Call stack depth
    call_depth: usize,
    /// Global object
    global: value.JSValue,
    /// Global object (as JSObject pointer for fast access)
    global_obj: ?*object.JSObject,
    /// Root hidden class for new objects
    root_class: ?*object.HiddenClass,
    /// Built-in prototypes
    array_prototype: ?*object.JSObject,
    string_prototype: ?*object.JSObject,
    object_prototype: ?*object.JSObject,
    function_prototype: ?*object.JSObject,
    /// Atom table for dynamic atoms
    atoms: AtomTable,
    /// Exception value (if any)
    exception: value.JSValue,
    /// Catch handler stack
    catch_stack: [32]CatchHandler,
    catch_depth: usize,
    /// Configuration
    config: ContextConfig,

    pub fn init(allocator: std.mem.Allocator, gc_state: *gc.GC, config: ContextConfig) !*Context {
        const ctx = try allocator.create(Context);
        errdefer allocator.destroy(ctx);

        const stack = try allocator.alloc(value.JSValue, config.stack_size / @sizeOf(value.JSValue));
        errdefer allocator.free(stack);

        const call_stack = try allocator.alloc(CallFrame, config.call_stack_size);
        errdefer allocator.free(call_stack);

        // Create root hidden class for all objects
        const root_class = try object.HiddenClass.init(allocator);
        errdefer root_class.deinit(allocator);

        // Create global object
        const global_obj = try object.JSObject.create(allocator, root_class, null);
        errdefer global_obj.destroy(allocator);

        ctx.* = .{
            .allocator = allocator,
            .gc_state = gc_state,
            .stack = stack,
            .sp = 0,
            .fp = 0,
            .call_stack = call_stack,
            .call_depth = 0,
            .global = global_obj.toValue(),
            .global_obj = global_obj,
            .root_class = root_class,
            .array_prototype = null,
            .string_prototype = null,
            .object_prototype = null,
            .function_prototype = null,
            .atoms = AtomTable.init(allocator),
            .exception = value.JSValue.undefined_val,
            .catch_stack = undefined,
            .catch_depth = 0,
            .config = config,
        };

        return ctx;
    }

    pub fn deinit(self: *Context) void {
        // Note: global_obj and root_class are managed by GC in a full implementation
        // For now, we don't free them to avoid double-free with hidden class transitions
        self.atoms.deinit();
        self.allocator.free(self.call_stack);
        self.allocator.free(self.stack);
        self.allocator.destroy(self);
    }

    // ========================================================================
    // Global Object Access
    // ========================================================================

    /// Get global property by atom
    pub fn getGlobal(self: *Context, atom: object.Atom) ?value.JSValue {
        if (self.global_obj) |g| {
            return g.getProperty(atom);
        }
        return null;
    }

    /// Set global property by atom
    pub fn setGlobal(self: *Context, atom: object.Atom, val: value.JSValue) !void {
        if (self.global_obj) |g| {
            try g.setProperty(self.allocator, atom, val);
        }
    }

    /// Define global variable (same as set for now)
    pub fn defineGlobal(self: *Context, atom: object.Atom, val: value.JSValue) !void {
        try self.setGlobal(atom, val);
    }

    /// Register a native function on the global object
    pub fn registerGlobalFunction(self: *Context, name: object.Atom, func: object.NativeFn, arg_count: u8) !void {
        const root_class = self.root_class orelse return error.NoRootClass;
        const func_obj = try object.JSObject.createNativeFunction(self.allocator, root_class, func, name, arg_count);
        try self.setGlobal(name, func_obj.toValue());
    }

    // ========================================================================
    // Stack Operations
    // ========================================================================

    /// Push value onto stack
    pub inline fn push(self: *Context, val: value.JSValue) !void {
        if (self.sp >= self.stack.len) {
            return error.StackOverflow;
        }
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    /// Pop value from stack
    pub inline fn pop(self: *Context) value.JSValue {
        std.debug.assert(self.sp > 0);
        self.sp -= 1;
        return self.stack[self.sp];
    }

    /// Peek at stack top
    pub inline fn peek(self: *Context) value.JSValue {
        std.debug.assert(self.sp > 0);
        return self.stack[self.sp - 1];
    }

    /// Peek at stack offset from top
    pub inline fn peekAt(self: *Context, offset: usize) value.JSValue {
        std.debug.assert(self.sp > offset);
        return self.stack[self.sp - 1 - offset];
    }

    /// Push call frame
    pub fn pushFrame(self: *Context, func: value.JSValue, this: value.JSValue, return_pc: usize) !void {
        if (self.call_depth >= self.call_stack.len) {
            return error.CallStackOverflow;
        }
        self.call_stack[self.call_depth] = .{
            .return_pc = return_pc,
            .return_sp = self.sp,
            .return_fp = self.fp,
            .func = func,
            .this = this,
        };
        self.call_depth += 1;
        self.fp = self.sp;
    }

    /// Pop call frame
    pub fn popFrame(self: *Context) ?CallFrame {
        if (self.call_depth == 0) return null;
        self.call_depth -= 1;
        const frame = self.call_stack[self.call_depth];
        self.fp = frame.return_fp;
        return frame;
    }

    /// Set exception
    pub fn throwException(self: *Context, exception: value.JSValue) void {
        self.exception = exception;
    }

    /// Clear exception
    pub fn clearException(self: *Context) void {
        self.exception = value.JSValue.undefined_val;
    }

    /// Check if exception is pending
    pub fn hasException(self: *Context) bool {
        return self.exception.isException() or
            (!self.exception.isUndefined() and !self.exception.isNull());
    }

    // ========================================================================
    // Exception Handler Stack
    // ========================================================================

    /// Push a catch handler
    pub fn pushCatch(self: *Context, catch_pc: usize) !void {
        if (self.catch_depth >= self.catch_stack.len) {
            return error.CallStackOverflow;
        }
        self.catch_stack[self.catch_depth] = .{
            .catch_pc = catch_pc,
            .sp = self.sp,
            .fp = self.fp,
        };
        self.catch_depth += 1;
    }

    /// Pop a catch handler (normal exit from try block)
    pub fn popCatch(self: *Context) void {
        if (self.catch_depth > 0) {
            self.catch_depth -= 1;
        }
    }

    /// Get current catch handler (for exception dispatch)
    pub fn getCatchHandler(self: *Context) ?CatchHandler {
        if (self.catch_depth == 0) return null;
        return self.catch_stack[self.catch_depth - 1];
    }

    // ========================================================================
    // Local Variable Access
    // ========================================================================

    /// Get local variable by index (relative to frame pointer)
    pub inline fn getLocal(self: *Context, idx: usize) value.JSValue {
        return self.stack[self.fp + idx];
    }

    /// Set local variable by index
    pub inline fn setLocal(self: *Context, idx: usize, val: value.JSValue) void {
        self.stack[self.fp + idx] = val;
    }

    /// Get argument by index (before frame pointer)
    pub inline fn getArg(self: *Context, idx: usize, arg_count: usize) value.JSValue {
        // Arguments are pushed before the frame, in reverse order
        if (idx >= arg_count) return value.JSValue.undefined_val;
        const frame_base = if (self.call_depth > 0) self.call_stack[self.call_depth - 1].return_sp else 0;
        return self.stack[frame_base + idx];
    }

    /// Get 'this' value for current frame
    pub inline fn getThis(self: *Context) value.JSValue {
        if (self.call_depth == 0) return self.global;
        return self.call_stack[self.call_depth - 1].this;
    }

    /// Ensure stack has at least n slots
    pub inline fn ensureStack(self: *Context, n: usize) !void {
        if (self.sp + n > self.stack.len) {
            return error.StackOverflow;
        }
    }

    /// Get current stack depth from frame pointer
    pub inline fn stackDepth(self: *Context) usize {
        return self.sp - self.fp;
    }

    /// Drop n values from stack
    pub inline fn dropN(self: *Context, n: usize) void {
        std.debug.assert(self.sp >= n);
        self.sp -= n;
    }
};

/// Dynamic atom table
pub const AtomTable = struct {
    strings: std.StringHashMap(object.Atom),
    next_id: u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AtomTable {
        return .{
            .strings = std.StringHashMap(object.Atom).init(allocator),
            .next_id = object.Atom.FIRST_DYNAMIC,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *AtomTable) void {
        // Free all interned string keys
        var it = self.strings.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.strings.deinit();
    }

    /// Intern a string and get its atom
    pub fn intern(self: *AtomTable, s: []const u8) !object.Atom {
        if (self.strings.get(s)) |existing| {
            return existing;
        }

        const atom: object.Atom = @enumFromInt(self.next_id);
        self.next_id += 1;

        // Copy string for storage
        const key = try self.allocator.dupe(u8, s);
        try self.strings.put(key, atom);

        return atom;
    }
};

test "Context stack operations" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    try ctx.push(value.JSValue.fromInt(42));
    try ctx.push(value.JSValue.true_val);

    try std.testing.expectEqual(value.JSValue.true_val, ctx.pop());
    try std.testing.expectEqual(@as(i32, 42), ctx.pop().getInt());
}

test "AtomTable interning" {
    const allocator = std.testing.allocator;

    var atoms = AtomTable.init(allocator);
    defer atoms.deinit();

    const atom1 = try atoms.intern("hello");
    const atom2 = try atoms.intern("hello");
    const atom3 = try atoms.intern("world");

    try std.testing.expectEqual(atom1, atom2);
    try std.testing.expect(atom1 != atom3);
}
