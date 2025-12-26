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
};

/// Call frame on the call stack
pub const CallFrame = struct {
    return_pc: usize,
    return_sp: usize,
    return_fp: usize,
    func: value.JSValue,
    this: value.JSValue,
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
    /// Atom table for dynamic atoms
    atoms: AtomTable,
    /// Exception value (if any)
    exception: value.JSValue,
    /// Configuration
    config: ContextConfig,

    pub fn init(allocator: std.mem.Allocator, gc_state: *gc.GC, config: ContextConfig) !*Context {
        const ctx = try allocator.create(Context);
        errdefer allocator.destroy(ctx);

        const stack = try allocator.alloc(value.JSValue, config.stack_size / @sizeOf(value.JSValue));
        errdefer allocator.free(stack);

        const call_stack = try allocator.alloc(CallFrame, config.call_stack_size);
        errdefer allocator.free(call_stack);

        ctx.* = .{
            .allocator = allocator,
            .gc_state = gc_state,
            .stack = stack,
            .sp = 0,
            .fp = 0,
            .call_stack = call_stack,
            .call_depth = 0,
            .global = value.JSValue.undefined_val,
            .atoms = AtomTable.init(allocator),
            .exception = value.JSValue.undefined_val,
            .config = config,
        };

        return ctx;
    }

    pub fn deinit(self: *Context) void {
        self.atoms.deinit();
        self.allocator.free(self.call_stack);
        self.allocator.free(self.stack);
        self.allocator.destroy(self);
    }

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
