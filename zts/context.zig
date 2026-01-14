//! JavaScript execution context
//!
//! Thread-local context with stack, atoms, and global state.

const std = @import("std");
const value = @import("value.zig");
const gc = @import("gc.zig");
const heap = @import("heap.zig");
const object = @import("object.zig");
const arena_mod = @import("arena.zig");
const string = @import("string.zig");
const jit = @import("jit/root.zig");

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
    /// Index-based hidden class pool (Phase 1 migration)
    hidden_class_pool: ?*object.HiddenClassPool,
    /// Root class index in the pool
    root_class_idx: object.HiddenClassIndex,
    /// Built-in prototypes
    array_prototype: ?*object.JSObject,
    string_prototype: ?*object.JSObject,
    object_prototype: ?*object.JSObject,
    function_prototype: ?*object.JSObject,
    generator_prototype: ?*object.JSObject,
    result_prototype: ?*object.JSObject,
    /// Atom table for dynamic atoms
    atoms: AtomTable,
    /// Exception value (if any)
    exception: value.JSValue,
    /// Catch handler stack
    catch_stack: [32]CatchHandler,
    catch_depth: usize,
    /// Configuration
    config: ContextConfig,
    /// Optional hybrid allocator for request-scoped allocation
    /// When set, ephemeral allocations use arena, persistent use standard allocator
    hybrid: ?*arena_mod.HybridAllocator,
    /// Reusable buffer for JSON serialization to reduce allocations
    json_writer: std.Io.Writer.Allocating,
    /// JIT code allocator for compiled functions (Phase 11)
    /// Lazily initialized when first JIT compilation is requested
    code_allocator: ?*jit.CodeAllocator,

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

        // Create index-based hidden class pool (Phase 1 migration)
        const hidden_class_pool = try object.HiddenClassPool.init(allocator);
        errdefer hidden_class_pool.deinit();

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
            .hidden_class_pool = hidden_class_pool,
            .root_class_idx = hidden_class_pool.getEmptyClass(),
            .array_prototype = null,
            .string_prototype = null,
            .object_prototype = null,
            .function_prototype = null,
            .generator_prototype = null,
            .result_prototype = null,
            .atoms = AtomTable.init(allocator),
            .exception = value.JSValue.undefined_val,
            .catch_stack = undefined,
            .catch_depth = 0,
            .config = config,
            .hybrid = null,
            .json_writer = std.Io.Writer.Allocating.init(allocator),
            .code_allocator = null,
        };

        return ctx;
    }

    /// Set hybrid allocator (called by Runtime when using hybrid allocation)
    /// Also enables hybrid_mode on GC to disable collection
    pub fn setHybridAllocator(self: *Context, hybrid: *arena_mod.HybridAllocator) void {
        self.hybrid = hybrid;
        // Disable GC when hybrid allocator is active - arena handles ephemeral cleanup
        self.gc_state.hybrid_mode = true;
    }

    // ========================================================================
    // Hybrid Allocation Helpers
    // ========================================================================

    /// Allocate ephemeral memory (dies at request end)
    /// Uses arena if hybrid allocator set, otherwise falls back to standard allocator
    pub fn allocEphemeral(self: *Context, size: usize) !*anyopaque {
        if (self.hybrid) |h| {
            return h.alloc(.ephemeral, size) orelse return error.OutOfMemory;
        }
        // Fallback to GC-managed allocation
        return self.gc_state.allocWithGC(size);
    }

    /// Allocate ephemeral typed object
    pub fn createEphemeral(self: *Context, comptime T: type) !*T {
        if (self.hybrid) |h| {
            return h.create(.ephemeral, T) orelse return error.OutOfMemory;
        }
        return self.allocator.create(T);
    }

    /// Allocate persistent memory (lives forever)
    /// Always uses standard allocator
    pub fn allocPersistent(self: *Context, size: usize) !*anyopaque {
        if (self.hybrid) |h| {
            return h.alloc(.persistent, size) orelse return error.OutOfMemory;
        }
        const mem = try self.allocator.alignedAlloc(u8, .@"8", size);
        return @ptrCast(mem.ptr);
    }

    /// Allocate persistent typed object
    pub fn createPersistent(self: *Context, comptime T: type) !*T {
        return self.allocator.create(T);
    }

    /// Create a JS object, using arena when hybrid mode is enabled
    pub fn createObject(self: *Context, prototype: ?*object.JSObject) !*object.JSObject {
        const root_class = self.root_class orelse return error.NoRootClass;
        if (self.hybrid) |h| {
            return object.JSObject.createWithArena(h.arena, root_class, prototype) orelse
                return error.OutOfMemory;
        }
        return try object.JSObject.create(self.allocator, root_class, prototype);
    }

    /// Create a JS array, using arena when hybrid mode is enabled
    pub fn createArray(self: *Context) !*object.JSObject {
        const root_class = self.root_class orelse return error.NoRootClass;
        if (self.hybrid) |h| {
            return object.JSObject.createArrayWithArena(h.arena, root_class) orelse
                return error.OutOfMemory;
        }
        return try object.JSObject.createArray(self.allocator, root_class);
    }

    /// Create a JS string pointer, using arena when hybrid mode is enabled
    pub fn createStringPtr(self: *Context, s: []const u8) !*string.JSString {
        if (self.hybrid) |h| {
            return string.createStringWithArena(h.arena, s) orelse return error.OutOfMemory;
        }
        return try string.createString(self.allocator, s);
    }

    /// Create a JS string value, using arena when hybrid mode is enabled
    pub fn createString(self: *Context, s: []const u8) !value.JSValue {
        const str = try self.createStringPtr(s);
        return value.JSValue.fromPtr(str);
    }

    /// Extract a string slice from a JSValue if it is a string
    pub fn getString(self: *const Context, val: value.JSValue) ?[]const u8 {
        _ = self;
        if (val.isString()) {
            return val.toPtr(string.JSString).data();
        }
        return null;
    }

    /// Check if hybrid allocation is enabled
    pub fn isHybridEnabled(self: *const Context) bool {
        return self.hybrid != null;
    }

    /// Check if a JSValue points to arena-allocated memory
    pub fn isEphemeralValue(self: *const Context, val: value.JSValue) bool {
        if (!val.isPtr()) return false;
        if (self.hybrid) |h| {
            const ptr: *anyopaque = @ptrCast(val.toPtr(u8));
            return h.arena.contains(ptr);
        }
        return false;
    }

    /// Set property with arena escape protection (reject-on-escape)
    pub fn setPropertyChecked(self: *Context, obj: *object.JSObject, name: object.Atom, val: value.JSValue) !void {
        if (self.hybrid != null and !obj.flags.is_arena and self.isEphemeralValue(val)) {
            self.throwException(value.JSValue.exception_val);
            return error.ArenaObjectEscape;
        }
        obj.setProperty(self.allocator, name, val) catch |err| {
            self.throwException(value.JSValue.exception_val);
            return err;
        };
    }

    /// Set array index with arena escape protection (reject-on-escape)
    pub fn setIndexChecked(self: *Context, obj: *object.JSObject, index: u32, val: value.JSValue) !void {
        if (self.hybrid != null and !obj.flags.is_arena and self.isEphemeralValue(val)) {
            self.throwException(value.JSValue.exception_val);
            return error.ArenaObjectEscape;
        }
        obj.setIndex(self.allocator, index, val) catch |err| {
            self.throwException(value.JSValue.exception_val);
            return err;
        };
    }

    pub fn deinit(self: *Context) void {
        if (self.array_prototype) |proto| proto.destroy(self.allocator);
        if (self.string_prototype) |proto| proto.destroy(self.allocator);
        if (self.object_prototype) |proto| proto.destroy(self.allocator);
        if (self.function_prototype) |proto| proto.destroy(self.allocator);
        if (self.generator_prototype) |proto| proto.destroy(self.allocator);
        if (self.result_prototype) |proto| proto.destroy(self.allocator);
        if (self.global_obj) |g| g.destroy(self.allocator);
        if (self.root_class) |root| root.deinitRecursive(self.allocator);
        if (self.hidden_class_pool) |pool| pool.deinit();

        // Clean up JIT code allocator
        if (self.code_allocator) |ca| {
            ca.deinit();
            self.allocator.destroy(ca);
        }

        self.atoms.deinit();
        self.json_writer.deinit();
        self.allocator.free(self.call_stack);
        self.allocator.free(self.stack);
        self.allocator.destroy(self);
    }

    // ========================================================================
    // JIT Support
    // ========================================================================

    /// JIT helper: get typeof result as JSValue string
    /// Called from JIT-compiled code via function pointer
    /// Returns pre-created string for the type name
    pub fn jitTypeOf(self: *Context, val: value.JSValue) callconv(.c) value.JSValue {
        const type_str = val.typeOf();
        return self.createString(type_str) catch value.JSValue.undefined_val;
    }

    /// JIT helper: convert value to boolean without allocation
    /// Returns a native bool for branch decisions
    pub fn jitToBoolean(_: *Context, val: value.JSValue) callconv(.c) bool {
        return val.toBoolean();
    }

    /// JIT helper: get property by atom index
    pub fn jitGetField(self: *Context, obj_val: value.JSValue, atom_idx: u16) callconv(.c) value.JSValue {
        const atom: object.Atom = @enumFromInt(atom_idx);
        if (obj_val.isObject()) {
            const obj = object.JSObject.fromValue(obj_val);
            return obj.getProperty(atom) orelse value.JSValue.undefined_val;
        }
        if (obj_val.isString()) {
            if (atom == .length) {
                const str = obj_val.toPtr(string.JSString);
                return value.JSValue.fromInt(@intCast(str.len));
            }
            if (self.string_prototype) |proto| {
                return proto.getProperty(atom) orelse value.JSValue.undefined_val;
            }
        }
        return value.JSValue.undefined_val;
    }

    /// JIT helper: get global by atom index
    pub fn jitGetGlobal(self: *Context, atom_idx: u16) callconv(.c) value.JSValue {
        const atom: object.Atom = @enumFromInt(atom_idx);
        return self.getGlobal(atom) orelse value.JSValue.undefined_val;
    }

    /// JIT helper: set global by atom index
    /// Returns the assigned value (or exception_val on error).
    pub fn jitPutGlobal(self: *Context, atom_idx: u16, val: value.JSValue) callconv(.c) value.JSValue {
        const atom: object.Atom = @enumFromInt(atom_idx);
        self.setGlobal(atom, val) catch return self.jitThrow();
        return val;
    }

    /// JIT helper: create a new object
    pub fn jitNewObject(self: *Context) callconv(.c) value.JSValue {
        const obj = self.createObject(null) catch return self.jitThrow();
        return obj.toValue();
    }

    /// JIT helper: create a new array with length
    pub fn jitNewArray(self: *Context, length: u16) callconv(.c) value.JSValue {
        const root_class = self.root_class orelse return self.jitThrow();
        const obj = if (self.hybrid) |h|
            object.JSObject.createArrayWithArena(h.arena, root_class) orelse return self.jitThrow()
        else
            object.JSObject.createArray(self.allocator, root_class) catch return self.jitThrow();
        obj.prototype = self.array_prototype;
        obj.setArrayLength(@intCast(length));
        return obj.toValue();
    }

    /// JIT helper: set property by atom index
    /// Returns the assigned value (or exception_val on error).
    pub fn jitPutField(self: *Context, obj_val: value.JSValue, atom_idx: u16, val: value.JSValue) callconv(.c) value.JSValue {
        if (obj_val.isObject()) {
            const obj = object.JSObject.fromValue(obj_val);
            self.setPropertyChecked(obj, @enumFromInt(atom_idx), val) catch return self.jitThrow();
        }
        // Non-object assignment silently fails in non-strict mode
        return val;
    }

    /// JIT helper: get element by index
    pub fn jitGetElem(self: *Context, obj_val: value.JSValue, index_val: value.JSValue) callconv(.c) value.JSValue {
        if (obj_val.isObject() and index_val.isInt()) {
            const obj = object.JSObject.fromValue(obj_val);
            const idx = index_val.getInt();
            if (idx >= 0) {
                const idx_u: u32 = @intCast(idx);
                if (obj.class_id == .array) {
                    const len = @as(u32, @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt()));
                    if (idx_u < len) {
                        return obj.getIndexUnchecked(idx_u);
                    }
                    return value.JSValue.undefined_val;
                } else if (obj.class_id == .range_iterator) {
                    const len = @as(u32, @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt()));
                    if (idx_u < len) {
                        const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                        const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                        return value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step);
                    }
                    return value.JSValue.undefined_val;
                } else {
                    var idx_buf: [32]u8 = undefined;
                    const idx_slice = std.fmt.bufPrint(&idx_buf, "{d}", .{idx}) catch return value.JSValue.undefined_val;
                    const atom = self.atoms.intern(idx_slice) catch return value.JSValue.undefined_val;
                    return obj.getProperty(atom) orelse value.JSValue.undefined_val;
                }
            }
        }
        return value.JSValue.undefined_val;
    }

    /// JIT helper: set element by index
    /// Returns the assigned value (or exception_val on error).
    pub fn jitPutElem(self: *Context, obj_val: value.JSValue, index_val: value.JSValue, val: value.JSValue) callconv(.c) value.JSValue {
        if (obj_val.isObject() and index_val.isInt()) {
            const obj = object.JSObject.fromValue(obj_val);
            const idx = index_val.getInt();
            if (idx >= 0) {
                if (obj.class_id == .array) {
                    self.setIndexChecked(obj, @intCast(idx), val) catch return self.jitThrow();
                } else {
                    var idx_buf: [32]u8 = undefined;
                    const idx_slice = std.fmt.bufPrint(&idx_buf, "{d}", .{idx}) catch return val;
                    const atom = self.atoms.intern(idx_slice) catch return val;
                    self.setPropertyChecked(obj, atom, val) catch return self.jitThrow();
                }
            }
        }
        return val;
    }

    /// JIT helper: for_of_next superinstruction
    /// Returns true if iteration should continue, false if loop ends.
    pub fn jitForOfNext(self: *Context) callconv(.c) bool {
        if (self.sp < 2) {
            _ = self.jitThrow();
            return false;
        }
        const sp = self.sp;
        const idx_val = self.stack[sp - 1];
        const iter_val = self.stack[sp - 2];

        if (iter_val.isObject() and idx_val.isInt()) {
            const obj = object.JSObject.fromValue(iter_val);
            const idx = idx_val.getInt();
            if (idx >= 0) {
                const idx_u: u32 = @intCast(idx);
                if (obj.class_id == .array) {
                    const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt());
                    if (idx_u < len) {
                        self.push(obj.getIndexUnchecked(idx_u)) catch {
                            _ = self.jitThrow();
                            return false;
                        };
                        self.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                        return true;
                    }
                } else if (obj.class_id == .range_iterator) {
                    const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt());
                    if (idx_u < len) {
                        const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                        const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                        self.push(value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step)) catch {
                            _ = self.jitThrow();
                            return false;
                        };
                        self.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /// JIT helper: for_of_next_put_loc superinstruction
    /// Returns true if iteration should continue, false if loop ends.
    pub fn jitForOfNextPutLoc(self: *Context, local_idx: u8) callconv(.c) bool {
        if (self.sp < 2) {
            _ = self.jitThrow();
            return false;
        }
        const sp = self.sp;
        const idx_val = self.stack[sp - 1];
        const iter_val = self.stack[sp - 2];

        if (iter_val.isObject() and idx_val.isInt()) {
            const obj = object.JSObject.fromValue(iter_val);
            const idx = idx_val.getInt();
            if (idx >= 0) {
                const idx_u: u32 = @intCast(idx);
                if (obj.class_id == .array) {
                    const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.ARRAY_LENGTH].getInt());
                    if (idx_u < len) {
                        self.setLocal(local_idx, obj.getIndexUnchecked(idx_u));
                        self.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                        return true;
                    }
                } else if (obj.class_id == .range_iterator) {
                    const len: u32 = @intCast(obj.inline_slots[object.JSObject.Slots.RANGE_LENGTH].getInt());
                    if (idx_u < len) {
                        const start = obj.inline_slots[object.JSObject.Slots.RANGE_START].getInt();
                        const step = obj.inline_slots[object.JSObject.Slots.RANGE_STEP].getInt();
                        self.setLocal(local_idx, value.JSValue.fromInt(start + @as(i32, @intCast(idx_u)) * step));
                        self.stack[sp - 1] = value.JSValue.fromInt(idx + 1);
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /// JIT helper: add two values with full JS semantics.
    /// On error, sets exception and returns exception_val.
    pub fn jitAdd(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        if (a.isInt() and b.isInt()) {
            const result = @addWithOverflow(a.getInt(), b.getInt());
            if (result[1] == 0) {
                return value.JSValue.fromInt(result[0]);
            }
            return self.jitAllocFloat(@as(f64, @floatFromInt(a.getInt())) + @as(f64, @floatFromInt(b.getInt())));
        }
        if (a.isString() or b.isString()) {
            const str_a = self.jitValueToString(a) catch return self.jitThrow();
            const str_b = self.jitValueToString(b) catch return self.jitThrow();
            // Use arena when available
            if (self.hybrid) |h| {
                const result = string.concatStringsWithArena(h.arena, str_a, str_b) orelse return self.jitThrow();
                return value.JSValue.fromPtr(result);
            }
            // Non-hybrid: clean up temp strings and use raw allocator
            defer if (!a.isString()) string.freeString(self.allocator, str_a);
            defer if (!b.isString()) string.freeString(self.allocator, str_b);
            const result = string.concatStrings(self.allocator, str_a, str_b) catch return self.jitThrow();
            return value.JSValue.fromPtr(result);
        }
        const an = a.toNumber() orelse return self.jitThrow();
        const bn = b.toNumber() orelse return self.jitThrow();
        return self.jitAllocFloat(an + bn);
    }

    /// JIT helper: subtract two values.
    pub fn jitSub(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        if (a.isInt() and b.isInt()) {
            const result = @subWithOverflow(a.getInt(), b.getInt());
            if (result[1] == 0) {
                return value.JSValue.fromInt(result[0]);
            }
            return self.jitAllocFloat(@as(f64, @floatFromInt(a.getInt())) - @as(f64, @floatFromInt(b.getInt())));
        }
        const an = a.toNumber() orelse return self.jitThrow();
        const bn = b.toNumber() orelse return self.jitThrow();
        return self.jitAllocFloat(an - bn);
    }

    /// JIT helper: multiply two values.
    pub fn jitMul(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        if (a.isInt() and b.isInt()) {
            const result = @mulWithOverflow(a.getInt(), b.getInt());
            if (result[1] == 0) {
                return value.JSValue.fromInt(result[0]);
            }
            return self.jitAllocFloat(@as(f64, @floatFromInt(a.getInt())) * @as(f64, @floatFromInt(b.getInt())));
        }
        const an = a.toNumber() orelse return self.jitThrow();
        const bn = b.toNumber() orelse return self.jitThrow();
        return self.jitAllocFloat(an * bn);
    }

    /// JIT helper: negate a value.
    pub fn jitNeg(self: *Context, a: value.JSValue) callconv(.c) value.JSValue {
        if (a.isInt()) {
            const v = a.getInt();
            if (v == std.math.minInt(i32)) {
                return self.jitAllocFloat(-@as(f64, @floatFromInt(v)));
            }
            return value.JSValue.fromInt(-v);
        }
        if (a.isFloat64()) {
            return self.jitAllocFloat(-a.getFloat64());
        }
        return self.jitThrow();
    }

    /// JIT helper: divide two values (always produces float)
    pub fn jitDiv(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        const an = a.toNumber() orelse return self.jitThrow();
        const bn = b.toNumber() orelse return self.jitThrow();
        return self.jitAllocFloat(an / bn);
    }

    /// JIT helper: modulo two values (integer-only fast path)
    pub fn jitMod(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        if (a.isInt() and b.isInt()) {
            const bv = b.getInt();
            if (bv == 0) return self.jitThrow();
            return value.JSValue.fromInt(@rem(a.getInt(), bv));
        }
        return self.jitThrow();
    }

    /// JIT helper: exponentiation
    pub fn jitPow(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        const an = a.toNumber() orelse return self.jitThrow();
        const bn = b.toNumber() orelse return self.jitThrow();
        return self.jitAllocFloat(std.math.pow(f64, an, bn));
    }

    /// JIT helper: increment
    pub fn jitInc(self: *Context, a: value.JSValue) callconv(.c) value.JSValue {
        if (a.isInt()) {
            const result = @addWithOverflow(a.getInt(), 1);
            if (result[1] == 0) {
                return value.JSValue.fromInt(result[0]);
            }
            return self.jitAllocFloat(@as(f64, @floatFromInt(a.getInt())) + 1.0);
        }
        if (a.isFloat64()) {
            return self.jitAllocFloat(a.getFloat64() + 1.0);
        }
        return self.jitThrow();
    }

    /// JIT helper: decrement
    pub fn jitDec(self: *Context, a: value.JSValue) callconv(.c) value.JSValue {
        if (a.isInt()) {
            const result = @subWithOverflow(a.getInt(), 1);
            if (result[1] == 0) {
                return value.JSValue.fromInt(result[0]);
            }
            return self.jitAllocFloat(@as(f64, @floatFromInt(a.getInt())) - 1.0);
        }
        if (a.isFloat64()) {
            return self.jitAllocFloat(a.getFloat64() - 1.0);
        }
        return self.jitThrow();
    }

    fn jitToInt32(val: value.JSValue) i32 {
        if (val.isInt()) return val.getInt();
        if (val.isFloat64()) {
            const f = val.getFloat64();
            if (std.math.isNan(f) or std.math.isInf(f) or f == 0) return 0;
            const int_val: i64 = @intFromFloat(@trunc(f));
            return @truncate(int_val);
        }
        return 0;
    }

    /// JIT helpers: bitwise operations
    pub fn jitBitAnd(_: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return value.JSValue.fromInt(jitToInt32(a) & jitToInt32(b));
    }

    pub fn jitBitOr(_: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return value.JSValue.fromInt(jitToInt32(a) | jitToInt32(b));
    }

    pub fn jitBitXor(_: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return value.JSValue.fromInt(jitToInt32(a) ^ jitToInt32(b));
    }

    pub fn jitBitNot(_: *Context, a: value.JSValue) callconv(.c) value.JSValue {
        return value.JSValue.fromInt(~jitToInt32(a));
    }

    /// JIT helpers: shift operations
    pub fn jitShiftShl(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return self.jitShift(a, b, .shl);
    }

    pub fn jitShiftShr(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return self.jitShift(a, b, .shr);
    }

    pub fn jitShiftUShr(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return self.jitShift(a, b, .ushr);
    }

    fn jitShift(_: *Context, a: value.JSValue, b: value.JSValue, op: enum { shl, shr, ushr }) value.JSValue {
        const shift: u5 = @intCast(@as(u32, @bitCast(jitToInt32(b))) & 0x1F);
        const ai = jitToInt32(a);
        const result: i32 = switch (op) {
            .shl => ai << shift,
            .shr => ai >> shift,
            .ushr => @bitCast(@as(u32, @bitCast(ai)) >> shift),
        };
        return value.JSValue.fromInt(result);
    }

    /// JIT helper: strict equality (===)
    pub fn jitStrictEquals(_: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return value.JSValue.fromBool(a.strictEquals(b));
    }

    /// JIT helper: loose equality (==)
    pub fn jitLooseEquals(_: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        if (a.raw == b.raw) return value.JSValue.true_val;
        if ((a.isNull() and b.isUndefined()) or (a.isUndefined() and b.isNull())) {
            return value.JSValue.true_val;
        }
        if (a.isNumber() and b.isNumber()) {
            const an = a.toNumber() orelse return value.JSValue.false_val;
            const bn = b.toNumber() orelse return value.JSValue.false_val;
            if (std.math.isNan(an) or std.math.isNan(bn)) return value.JSValue.false_val;
            return value.JSValue.fromBool(an == bn);
        }
        return value.JSValue.false_val;
    }

    /// JIT helpers: relational comparisons
    pub fn jitCompareLt(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return self.jitCompare(a, b, .lt);
    }

    pub fn jitCompareLte(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return self.jitCompare(a, b, .lte);
    }

    pub fn jitCompareGt(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return self.jitCompare(a, b, .gt);
    }

    pub fn jitCompareGte(self: *Context, a: value.JSValue, b: value.JSValue) callconv(.c) value.JSValue {
        return self.jitCompare(a, b, .gte);
    }

    fn jitCompare(self: *Context, a: value.JSValue, b: value.JSValue, op: enum { lt, lte, gt, gte }) value.JSValue {
        if (a.isInt() and b.isInt()) {
            const ai = a.getInt();
            const bi = b.getInt();
            const result = switch (op) {
                .lt => ai < bi,
                .lte => ai <= bi,
                .gt => ai > bi,
                .gte => ai >= bi,
            };
            return value.JSValue.fromBool(result);
        }
        const an = a.toNumber() orelse return self.jitThrow();
        const bn = b.toNumber() orelse return self.jitThrow();
        if (std.math.isNan(an) or std.math.isNan(bn)) return self.jitThrow();
        const result = switch (op) {
            .lt => an < bn,
            .lte => an <= bn,
            .gt => an > bn,
            .gte => an >= bn,
        };
        return value.JSValue.fromBool(result);
    }

    fn jitAllocFloat(self: *Context, v: f64) value.JSValue {
        if (self.hybrid) |h| {
            const box = h.arena.createAligned(value.JSValue.Float64Box) orelse return self.jitThrow();
            box.* = .{
                .header = heap.MemBlockHeader.init(.float64, @sizeOf(value.JSValue.Float64Box)),
                ._pad = 0,
                .value = v,
            };
            return value.JSValue.fromPtr(box);
        }
        const box = self.gc_state.allocFloat(v) catch return self.jitThrow();
        return value.JSValue.fromPtr(box);
    }

    fn jitValueToString(self: *Context, val: value.JSValue) !*string.JSString {
        if (val.isString()) {
            return val.toPtr(string.JSString);
        }
        if (val.isInt()) {
            var buf: [32]u8 = undefined;
            const slice = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return self.createStringPtr("0");
            return try self.createStringPtr(slice);
        }
        if (val.isNull()) {
            return try self.createStringPtr("null");
        }
        if (val.isUndefined()) {
            return try self.createStringPtr("undefined");
        }
        if (val.isTrue()) {
            return try self.createStringPtr("true");
        }
        if (val.isFalse()) {
            return try self.createStringPtr("false");
        }
        if (val.isObject()) {
            return try self.createStringPtr("[object Object]");
        }
        if (val.toNumber()) |n| {
            var buf: [64]u8 = undefined;
            const slice = std.fmt.bufPrint(&buf, "{d}", .{n}) catch return self.createStringPtr("NaN");
            return try self.createStringPtr(slice);
        }
        return try self.createStringPtr("undefined");
    }

    fn jitThrow(self: *Context) value.JSValue {
        self.throwException(value.JSValue.exception_val);
        return value.JSValue.exception_val;
    }

    /// Get or create the JIT code allocator (lazy initialization)
    pub fn getOrCreateCodeAllocator(self: *Context) !*jit.CodeAllocator {
        if (self.code_allocator) |ca| {
            return ca;
        }
        const ca = try self.allocator.create(jit.CodeAllocator);
        ca.* = jit.CodeAllocator.init(self.allocator);
        self.code_allocator = ca;
        return ca;
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
            try self.setPropertyChecked(g, atom, val);
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
    // Index-Based Hidden Class Operations (Phase 1)
    // ========================================================================

    /// Get a hidden class with an additional property
    /// Returns the transitioned class index (cached if already exists)
    pub fn getClassWithProperty(self: *Context, from: object.HiddenClassIndex, name: object.Atom) !object.HiddenClassIndex {
        const pool = self.hidden_class_pool orelse return error.NoHiddenClassPool;
        return pool.addProperty(from, name);
    }

    /// Look up property offset in a hidden class
    /// Returns null if property not found
    pub fn getPropertyOffset(self: *const Context, class_idx: object.HiddenClassIndex, name: object.Atom) ?u16 {
        const pool = self.hidden_class_pool orelse return null;
        return pool.findProperty(class_idx, name);
    }

    /// Get property count for a hidden class
    pub fn getClassPropertyCount(self: *const Context, class_idx: object.HiddenClassIndex) u16 {
        const pool = self.hidden_class_pool orelse return 0;
        return pool.getPropertyCount(class_idx);
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

    /// Push without bounds check - caller must ensure stack space
    pub inline fn pushUnchecked(self: *Context, val: value.JSValue) void {
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

    /// Swap top two stack values in place (no pop/push)
    pub inline fn swap2(self: *Context) void {
        std.debug.assert(self.sp >= 2);
        const a = self.stack[self.sp - 1];
        self.stack[self.sp - 1] = self.stack[self.sp - 2];
        self.stack[self.sp - 2] = a;
    }

    /// Rotate top 3 stack values: [a,b,c] -> [b,c,a] (no pop/push)
    pub inline fn rot3(self: *Context) void {
        std.debug.assert(self.sp >= 3);
        const c = self.stack[self.sp - 1];
        const b = self.stack[self.sp - 2];
        const a = self.stack[self.sp - 3];
        self.stack[self.sp - 3] = b;
        self.stack[self.sp - 2] = c;
        self.stack[self.sp - 1] = a;
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

    /// Get pointer to local variable slot (for upvalue capture)
    pub inline fn getLocalPtr(self: *Context, idx: usize) *value.JSValue {
        return &self.stack[self.fp + idx];
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

/// Dynamic atom table with O(1) reverse lookup
pub const AtomTable = struct {
    strings: std.StringHashMap(object.Atom),
    reverse: std.AutoHashMap(object.Atom, []const u8), // O(1) reverse lookup
    next_id: u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AtomTable {
        return .{
            .strings = std.StringHashMap(object.Atom).init(allocator),
            .reverse = std.AutoHashMap(object.Atom, []const u8).init(allocator),
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
        self.reverse.deinit();
    }

    /// Intern a string and get its atom
    pub fn intern(self: *AtomTable, s: []const u8) !object.Atom {
        if (object.lookupPredefinedAtom(s)) |predef| {
            return predef;
        }
        if (self.strings.get(s)) |existing| {
            return existing;
        }

        const atom: object.Atom = @enumFromInt(self.next_id);
        self.next_id += 1;

        // Copy string for storage
        const key = try self.allocator.dupe(u8, s);
        try self.strings.put(key, atom);
        try self.reverse.put(atom, key); // Populate reverse map

        return atom;
    }

    /// Prune unused atoms during major GC
    /// Takes a set of atoms that are still in use (referenced by live objects)
    pub fn pruneUnused(self: *AtomTable, used_atoms: *const std.AutoHashMap(object.Atom, void)) void {
        // Build list of keys to remove (can't remove during iteration)
        var to_remove = std.ArrayList([]const u8).init(self.allocator);
        var atoms_to_remove = std.ArrayList(object.Atom).init(self.allocator);
        defer to_remove.deinit();
        defer atoms_to_remove.deinit();

        var it = self.strings.iterator();
        while (it.next()) |entry| {
            const atom = entry.value_ptr.*;
            // Keep predefined atoms (they're always in use)
            if (atom.isPredefined()) continue;

            // Check if this dynamic atom is still referenced
            if (!used_atoms.contains(atom)) {
                to_remove.append(entry.key_ptr.*) catch continue;
                atoms_to_remove.append(atom) catch continue;
            }
        }

        // Remove unreferenced atoms from both maps
        for (to_remove.items, atoms_to_remove.items) |key, atom| {
            _ = self.strings.remove(key);
            _ = self.reverse.remove(atom);
            self.allocator.free(key);
        }
    }

    /// Reset atom table to initial state (for request isolation)
    pub fn reset(self: *AtomTable) void {
        // Free all interned string keys
        var it = self.strings.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.strings.clearRetainingCapacity();
        self.reverse.clearRetainingCapacity();
        self.next_id = object.Atom.FIRST_DYNAMIC;
    }

    /// Get current atom count (for monitoring)
    pub fn count(self: *AtomTable) usize {
        return self.strings.count();
    }

    /// Get string name for an atom - O(1) using reverse lookup map
    pub fn getName(self: *AtomTable, atom: object.Atom) ?[]const u8 {
        // Check predefined atoms first (already O(1) via switch)
        if (atom.isPredefined()) {
            return atom.toPredefinedName();
        }
        // O(1) lookup for dynamic atoms
        return self.reverse.get(atom);
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

test "Context pushFrame and popFrame" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push a frame
    try ctx.pushFrame(value.JSValue.undefined_val, value.JSValue.undefined_val, 0);
    try std.testing.expectEqual(@as(usize, 1), ctx.call_depth);

    // Pop the frame
    const frame = ctx.popFrame();
    try std.testing.expect(frame != null);
    try std.testing.expectEqual(@as(usize, 0), ctx.call_depth);
}

test "Context exception handling" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Initially no exception
    try std.testing.expect(!ctx.hasException());

    // Throw exception
    ctx.throwException(value.JSValue.fromInt(42));
    try std.testing.expect(ctx.hasException());

    // Clear exception
    ctx.clearException();
    try std.testing.expect(!ctx.hasException());
}

test "Context catch handler" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // No handler initially
    try std.testing.expect(ctx.getCatchHandler() == null);

    // Push a catch handler
    try ctx.pushCatch(100);
    const handler = ctx.getCatchHandler();
    try std.testing.expect(handler != null);
    try std.testing.expectEqual(@as(usize, 100), handler.?.catch_pc);

    // Pop the handler
    ctx.popCatch();
    try std.testing.expect(ctx.getCatchHandler() == null);
}

test "Context global variables" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    const atom = try ctx.atoms.intern("myVar");

    // Initially undefined
    try std.testing.expect(ctx.getGlobal(atom) == null);

    // Set global
    try ctx.setGlobal(atom, value.JSValue.fromInt(123));

    // Get global
    const val = ctx.getGlobal(atom);
    try std.testing.expect(val != null);
    try std.testing.expectEqual(@as(i32, 123), val.?.getInt());
}

test "Hybrid rejects arena value to global" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    var req_arena = try arena_mod.Arena.init(allocator, .{ .size = 4096 });
    defer req_arena.deinit();
    var hybrid = arena_mod.HybridAllocator{
        .persistent = allocator,
        .arena = &req_arena,
    };
    ctx.setHybridAllocator(&hybrid);

    const arena_obj = try ctx.createObject(null);
    const atom = try ctx.atoms.intern("ephemeral");

    try std.testing.expectError(error.ArenaObjectEscape, ctx.setGlobal(atom, arena_obj.toValue()));
    try std.testing.expect(ctx.hasException());
}

test "AtomTable getName" {
    const allocator = std.testing.allocator;

    var atoms = AtomTable.init(allocator);
    defer atoms.deinit();

    const atom = try atoms.intern("testName");
    const name = atoms.getName(atom);

    try std.testing.expect(name != null);
    try std.testing.expectEqualStrings("testName", name.?);
}

test "AtomTable count" {
    const allocator = std.testing.allocator;

    var atoms = AtomTable.init(allocator);
    defer atoms.deinit();

    try std.testing.expectEqual(@as(usize, 0), atoms.count());

    _ = try atoms.intern("first");
    try std.testing.expectEqual(@as(usize, 1), atoms.count());

    _ = try atoms.intern("second");
    try std.testing.expectEqual(@as(usize, 2), atoms.count());

    // Interning same string shouldn't increase count
    _ = try atoms.intern("first");
    try std.testing.expectEqual(@as(usize, 2), atoms.count());
}

test "AtomTable reset" {
    const allocator = std.testing.allocator;

    var atoms = AtomTable.init(allocator);
    defer atoms.deinit();

    _ = try atoms.intern("one");
    _ = try atoms.intern("two");
    try std.testing.expectEqual(@as(usize, 2), atoms.count());

    atoms.reset();
    try std.testing.expectEqual(@as(usize, 0), atoms.count());
}

test "Context nested catch handlers" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Push two nested handlers
    try ctx.pushCatch(100);
    try ctx.pushCatch(200);

    // Should get most recent handler
    const handler1 = ctx.getCatchHandler();
    try std.testing.expect(handler1 != null);
    try std.testing.expectEqual(@as(usize, 200), handler1.?.catch_pc);

    // Pop and get outer handler
    ctx.popCatch();
    const handler2 = ctx.getCatchHandler();
    try std.testing.expect(handler2 != null);
    try std.testing.expectEqual(@as(usize, 100), handler2.?.catch_pc);

    ctx.popCatch();
    try std.testing.expect(ctx.getCatchHandler() == null);
}

test "Context popFrame on empty returns null" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // No frames pushed
    try std.testing.expect(ctx.popFrame() == null);
}

test "Context index-based hidden class pool" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Pool should be initialized
    try std.testing.expect(ctx.hidden_class_pool != null);

    // Root class index should be the empty class
    try std.testing.expectEqual(object.HiddenClassIndex.empty, ctx.root_class_idx);
    try std.testing.expectEqual(@as(u16, 0), ctx.getClassPropertyCount(ctx.root_class_idx));
}

test "Context getClassWithProperty transitions" {
    const allocator = std.testing.allocator;

    var gc_state = try gc.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Add property 'x' to empty class
    const class_x = try ctx.getClassWithProperty(ctx.root_class_idx, .length);
    try std.testing.expectEqual(@as(u16, 1), ctx.getClassPropertyCount(class_x));

    // Property offset should be 0
    const offset = ctx.getPropertyOffset(class_x, .length);
    try std.testing.expect(offset != null);
    try std.testing.expectEqual(@as(u16, 0), offset.?);

    // Add another property 'y'
    const class_xy = try ctx.getClassWithProperty(class_x, .name);
    try std.testing.expectEqual(@as(u16, 2), ctx.getClassPropertyCount(class_xy));

    // First property should still be at offset 0
    try std.testing.expectEqual(@as(u16, 0), ctx.getPropertyOffset(class_xy, .length).?);
    // Second property at offset 1
    try std.testing.expectEqual(@as(u16, 1), ctx.getPropertyOffset(class_xy, .name).?);
}
