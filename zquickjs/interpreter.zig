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

const empty_code: [0]u8 = .{};

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
    state_stack: [MAX_STATE_DEPTH]SavedState,
    state_depth: usize,

    pub fn init(ctx: *context.Context) Interpreter {
        return .{
            .ctx = ctx,
            .pc = @ptrCast(&empty_code),
            .code_end = @ptrCast(&empty_code),
            .constants = &.{},
            .current_func = null,
            .state_stack = undefined,
            .state_depth = 0,
        };
    }

    /// Offset the program counter by a signed value
    /// Consolidates the verbose type-casting pattern used throughout dispatch
    inline fn offsetPc(self: *Interpreter, offset: i16) void {
        self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc))) + offset)));
    }

    /// Run bytecode function
    pub fn run(self: *Interpreter, func: *const bytecode.FunctionBytecode) InterpreterError!value.JSValue {
        self.pc = func.code.ptr;
        self.code_end = func.code.ptr + func.code.len;
        self.constants = func.constants;
        self.current_func = func;

        // Allocate space for locals
        const local_count = func.local_count;
        try self.ctx.ensureStack(local_count);
        for (0..local_count) |_| {
            try self.ctx.push(value.JSValue.undefined_val);
        }

        return self.dispatch();
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

    pub fn callBytecodeFunction(
        self: *Interpreter,
        func_val: value.JSValue,
        func_bc: *const bytecode.FunctionBytecode,
        this_val: value.JSValue,
        args: []const value.JSValue,
    ) InterpreterError!value.JSValue {
        try self.pushState();
        defer self.popState();

        // Push call frame
        try self.ctx.pushFrame(func_val, this_val, @intFromPtr(self.pc));
        errdefer _ = self.ctx.popFrame();

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

        // Execute the function bytecode
        self.pc = func_bc.code.ptr;
        self.code_end = func_bc.code.ptr + func_bc.code.len;
        self.constants = func_bc.constants;
        self.current_func = func_bc;

        const result = self.dispatch() catch |err| {
            _ = self.ctx.popFrame();
            return err;
        };

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
    };

    /// Main dispatch loop (public for use by native function callbacks that need to call JS functions)
    pub fn dispatch(self: *Interpreter) InterpreterError!value.JSValue {
        dispatch: while (@intFromPtr(self.pc) < @intFromPtr(self.code_end)) {
            const op: bytecode.Opcode = @enumFromInt(self.pc[0]);
            self.pc += 1;

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
                    const a = self.ctx.pop();
                    const b = self.ctx.pop();
                    try self.ctx.push(a);
                    try self.ctx.push(b);
                },

                .rot3 => {
                    const c = self.ctx.pop();
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(b);
                    try self.ctx.push(c);
                    try self.ctx.push(a);
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
                // Arithmetic
                // ========================================
                .add => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(try self.addValues(a, b));
                },

                .sub => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(try subValues(a, b));
                },

                .mul => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(try mulValues(a, b));
                },

                .div => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(try self.divValues(a, b));
                },

                .mod => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(try modValues(a, b));
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
                    const a = self.ctx.pop();
                    if (a.isInt()) {
                        const result = @addWithOverflow(a.getInt(), 1);
                        if (result[1] == 0) {
                            try self.ctx.push(value.JSValue.fromInt(result[0]));
                        } else {
                            try self.ctx.push(try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) + 1.0));
                        }
                    } else if (a.isFloat64()) {
                        try self.ctx.push(try self.allocFloat(a.getFloat64() + 1.0));
                    } else {
                        return error.TypeError;
                    }
                },

                .dec => {
                    const a = self.ctx.pop();
                    if (a.isInt()) {
                        const result = @subWithOverflow(a.getInt(), 1);
                        if (result[1] == 0) {
                            try self.ctx.push(value.JSValue.fromInt(result[0]));
                        } else {
                            try self.ctx.push(try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) - 1.0));
                        }
                    } else if (a.isFloat64()) {
                        try self.ctx.push(try self.allocFloat(a.getFloat64() - 1.0));
                    } else {
                        return error.TypeError;
                    }
                },

                // ========================================
                // Bitwise Operations
                // ========================================
                .bit_and => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromInt(toInt32(a) & toInt32(b)));
                },

                .bit_or => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromInt(toInt32(a) | toInt32(b)));
                },

                .bit_xor => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromInt(toInt32(a) ^ toInt32(b)));
                },

                .bit_not => {
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromInt(~toInt32(a)));
                },

                .shl => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    const shift: u5 = @intCast(@as(u32, @bitCast(toInt32(b))) & 0x1F);
                    try self.ctx.push(value.JSValue.fromInt(toInt32(a) << shift));
                },

                .shr => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    const shift: u5 = @intCast(@as(u32, @bitCast(toInt32(b))) & 0x1F);
                    try self.ctx.push(value.JSValue.fromInt(toInt32(a) >> shift));
                },

                .ushr => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    const shift: u5 = @intCast(@as(u32, @bitCast(toInt32(b))) & 0x1F);
                    const ua: u32 = @bitCast(toInt32(a));
                    try self.ctx.push(value.JSValue.fromInt(@bitCast(ua >> shift)));
                },

                // ========================================
                // Comparison
                // ========================================
                .lt => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(try compareValues(a, b) == .lt));
                },

                .lte => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    const cmp = try compareValues(a, b);
                    try self.ctx.push(value.JSValue.fromBool(cmp == .lt or cmp == .eq));
                },

                .gt => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(try compareValues(a, b) == .gt));
                },

                .gte => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    const cmp = try compareValues(a, b);
                    try self.ctx.push(value.JSValue.fromBool(cmp == .gt or cmp == .eq));
                },

                .eq => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(looseEquals(a, b)));
                },

                .neq => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(!looseEquals(a, b)));
                },

                .strict_eq => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(a.strictEquals(b)));
                },

                .strict_neq => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(!a.strictEquals(b)));
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

                .throw => {
                    const exception_val = self.ctx.pop();
                    self.ctx.throwException(exception_val);

                    // Check for catch handler
                    if (self.ctx.getCatchHandler()) |handler| {
                        // Restore stack state
                        self.ctx.sp = handler.sp;
                        self.ctx.fp = handler.fp;
                        // Pop the catch handler
                        self.ctx.popCatch();
                        // Jump to catch block
                        self.pc = @ptrFromInt(handler.catch_pc);
                        continue :dispatch;
                    }

                    // No catch handler - propagate exception
                    return value.JSValue.exception_val;
                },

                // ========================================
                // Object Operations
                // ========================================
                .push_this => try self.ctx.push(self.ctx.getThis()),

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
                        if (obj.getProperty(atom)) |prop_val| {
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
                            if (proto.getProperty(atom)) |prop_val| {
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
                        try obj.setProperty(self.ctx.allocator, atom, val);
                    }
                    // Non-object assignment silently fails in non-strict mode
                },

                .get_elem => {
                    const index_val = self.ctx.pop();
                    const obj_val = self.ctx.pop();

                    if (obj_val.isObject() and index_val.isInt()) {
                        const obj = object.JSObject.fromValue(obj_val);
                        const idx = index_val.getInt();
                        if (idx >= 0) {
                            if (obj.class_id == .array) {
                                const idx_u: u32 = @intCast(idx);
                                if (obj.getIndex(idx_u)) |val| {
                                    try self.ctx.push(val);
                                } else {
                                    try self.ctx.push(value.JSValue.undefined_val);
                                }
                            } else {
                                var idx_buf: [32]u8 = undefined;
                                const idx_slice = std.fmt.bufPrint(&idx_buf, "{d}", .{idx}) catch {
                                    try self.ctx.push(value.JSValue.undefined_val);
                                    continue :dispatch;
                                };
                                const atom = self.ctx.atoms.intern(idx_slice) catch {
                                    try self.ctx.push(value.JSValue.undefined_val);
                                    continue :dispatch;
                                };
                                const val = obj.getProperty(atom) orelse value.JSValue.undefined_val;
                                try self.ctx.push(val);
                            }
                        } else {
                            try self.ctx.push(value.JSValue.undefined_val);
                        }
                    } else {
                        try self.ctx.push(value.JSValue.undefined_val);
                    }
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
                                try obj.setIndex(self.ctx.allocator, @intCast(idx), val);
                            } else {
                                var idx_buf: [32]u8 = undefined;
                                const idx_slice = std.fmt.bufPrint(&idx_buf, "{d}", .{idx}) catch continue :dispatch;
                                const atom = self.ctx.atoms.intern(idx_slice) catch continue :dispatch;
                                try obj.setProperty(self.ctx.allocator, atom, val);
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
                    const root_class = self.ctx.root_class orelse return error.NoRootClass;
                    const func_obj = try object.JSObject.createBytecodeFunction(
                        self.ctx.allocator,
                        root_class,
                        bc_ptr,
                        @enumFromInt(bc_ptr.name_atom),
                    );
                    try self.ctx.push(func_obj.toValue());
                },

                .make_generator => {
                    const const_idx = readU16(self.pc);
                    self.pc += 2;
                    // Get bytecode pointer from constant pool
                    const bc_val = try self.getConstant(const_idx);
                    if (!bc_val.isPtr()) return error.TypeError;
                    const bc_ptr = bc_val.toPtr(bytecode.FunctionBytecode);
                    // Create generator function object (marked as generator via slot 2)
                    const root_class = self.ctx.root_class orelse return error.NoRootClass;
                    const func_obj = try object.JSObject.createBytecodeFunction(
                        self.ctx.allocator,
                        root_class,
                        bc_ptr,
                        @enumFromInt(bc_ptr.name_atom),
                    );
                    // Mark as generator function using slot 2
                    func_obj.inline_slots[2] = value.JSValue.true_val;
                    try self.ctx.push(func_obj.toValue());
                },

                .yield_val => {
                    // This is handled specially in runGenerator
                    // If we reach it during normal execution, it's an error
                    return error.UnimplementedOpcode;
                },

                .yield_star => {
                    // Delegate to another iterator - complex, placeholder for now
                    return error.UnimplementedOpcode;
                },

                .make_async => {
                    const const_idx = readU16(self.pc);
                    self.pc += 2;
                    // Get bytecode pointer from constant pool
                    const bc_val = try self.getConstant(const_idx);
                    if (!bc_val.isPtr()) return error.TypeError;
                    const bc_ptr = bc_val.toPtr(bytecode.FunctionBytecode);
                    // Create async function object (marked as async via slot 3)
                    const root_class = self.ctx.root_class orelse return error.NoRootClass;
                    const func_obj = try object.JSObject.createBytecodeFunction(
                        self.ctx.allocator,
                        root_class,
                        bc_ptr,
                        @enumFromInt(bc_ptr.name_atom),
                    );
                    // Mark as async function using slot 3
                    func_obj.inline_slots[3] = value.JSValue.true_val;
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
                    const root_class = self.ctx.root_class orelse return error.NoRootClass;
                    const namespace = try object.JSObject.create(self.ctx.allocator, root_class, null);
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

                        // Get source array length
                        if (source.getProperty(.length)) |len_val| {
                            if (len_val.isInt()) {
                                const src_len: usize = @intCast(len_val.getInt());
                                // Copy each element from source to target
                                for (0..src_len) |i| {
                                    const elem = source.getSlot(@intCast(i));
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
                    const js_str = string.createString(self.ctx.allocator, type_str) catch {
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

                    // Get Constructor.prototype
                    if (ctor_obj.getProperty(.prototype)) |proto_val| {
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
                // Exception Handling
                // ========================================
                .push_catch => {
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    // Calculate absolute address of catch block
                    const catch_pc = @intFromPtr(self.pc) + @as(usize, @intCast(@as(i32, offset)));
                    try self.ctx.pushCatch(catch_pc);
                },

                .pop_catch => {
                    self.ctx.popCatch();
                },

                .get_exception => {
                    // Push the exception value onto the stack
                    try self.ctx.push(self.ctx.exception);
                    // Clear the exception
                    self.ctx.clearException();
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

                .call_constructor => {
                    const argc: u8 = self.pc[0];
                    self.pc += 1;
                    try self.doConstruct(argc);
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

    fn addValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // String concatenation - if either operand is a string
        if (a.isString() or b.isString()) {
            return try self.concatToString(a, b);
        }
        // Integer fast path
        if (a.isInt() and b.isInt()) {
            const result = @addWithOverflow(a.getInt(), b.getInt());
            if (result[1] == 0) {
                return value.JSValue.fromInt(result[0]);
            }
            // Overflow - convert to float
            return try self.allocFloat(@as(f64, @floatFromInt(a.getInt())) + @as(f64, @floatFromInt(b.getInt())));
        }
        // Float path
        const an = a.toNumber() orelse return error.TypeError;
        const bn = b.toNumber() orelse return error.TypeError;
        return try self.allocFloat(an + bn);
    }

    /// Convert value to string and concatenate
    fn concatToString(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Convert a to string
        const str_a = try self.valueToString(a);
        defer if (!a.isString()) string.freeString(self.ctx.allocator, str_a);

        // Convert b to string
        const str_b = try self.valueToString(b);
        defer if (!b.isString()) string.freeString(self.ctx.allocator, str_b);

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
            const slice = std.fmt.bufPrint(&buf, "{d}", .{val.getInt()}) catch return try string.createString(self.ctx.allocator, "0");
            return try string.createString(self.ctx.allocator, slice);
        }
        if (val.isNull()) {
            return try string.createString(self.ctx.allocator, "null");
        }
        if (val.isUndefined()) {
            return try string.createString(self.ctx.allocator, "undefined");
        }
        if (val.isTrue()) {
            return try string.createString(self.ctx.allocator, "true");
        }
        if (val.isFalse()) {
            return try string.createString(self.ctx.allocator, "false");
        }
        if (val.isObject()) {
            return try string.createString(self.ctx.allocator, "[object Object]");
        }
        // Float
        if (val.toNumber()) |n| {
            var buf: [64]u8 = undefined;
            const slice = std.fmt.bufPrint(&buf, "{d}", .{n}) catch return try string.createString(self.ctx.allocator, "NaN");
            return try string.createString(self.ctx.allocator, slice);
        }
        return try string.createString(self.ctx.allocator, "undefined");
    }

    fn divValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        // Division always produces float in JS
        const an = a.toNumber() orelse return error.TypeError;
        const bn = b.toNumber() orelse return error.TypeError;
        return try self.allocFloat(an / bn);
    }

    fn powValues(self: *Interpreter, a: value.JSValue, b: value.JSValue) !value.JSValue {
        const an = a.toNumber() orelse return error.TypeError;
        const bn = b.toNumber() orelse return error.TypeError;
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
        const box = try self.ctx.gc_state.allocFloat(v);
        return value.JSValue.fromPtr(box);
    }

    fn createObject(self: *Interpreter) !*object.JSObject {
        const root_class = self.ctx.root_class orelse return error.NoRootClass;
        return try object.JSObject.create(self.ctx.allocator, root_class, null);
    }

    fn createArray(self: *Interpreter) !*object.JSObject {
        const root_class = self.ctx.root_class orelse return error.NoRootClass;
        const obj = try object.JSObject.createArray(self.ctx.allocator, root_class);
        obj.prototype = self.ctx.array_prototype;
        return obj;
    }

    fn getConstant(self: *Interpreter, idx: u16) !value.JSValue {
        if (idx >= self.constants.len) return error.InvalidConstant;
        return self.constants[idx];
    }

    /// Maximum arguments for stack-based allocation (security limit)
    const MAX_STACK_ARGS = 256;

    /// Perform function call
    fn doCall(self: *Interpreter, argc: u8, is_method: bool) InterpreterError!void {
        // Stack layout for regular call: [func, arg0, arg1, ..., argN-1]
        // Stack layout for method call: [obj, func, arg0, arg1, ..., argN-1]

        // CRITICAL: Bounds check to prevent buffer overflow (security fix)
        if (argc > MAX_STACK_ARGS) {
            return error.TooManyArguments;
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

        // Check for bytecode function
        if (func_obj.getBytecodeFunctionData()) |bc_data| {
            const func_bc = bc_data.bytecode;

            // Check if this is a generator function (slot[2] is true)
            if (func_obj.inline_slots[2].isTrue()) {
                // Create a generator object instead of executing
                const root_class = self.ctx.root_class orelse return error.NoRootClass;
                const gen_obj = object.JSObject.createGenerator(
                    self.ctx.allocator,
                    root_class,
                    func_bc,
                    self.ctx.generator_prototype,
                ) catch return error.OutOfMemory;

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

            // Check if this is an async function (slot[3] is true)
            if (func_obj.inline_slots[3].isTrue()) {
                // Execute async function synchronously and wrap result in Promise-like object
                // For full async support, we'd need an event loop and proper Promise integration
                // This simplified version executes synchronously and returns a resolved Promise
                const result = try self.callBytecodeFunction(func_val, func_bc, this_val, args[0..argc]);

                // Create a resolved Promise-like object {then: fn, value: result}
                const root_class = self.ctx.root_class orelse return error.NoRootClass;
                const promise_obj = object.JSObject.create(
                    self.ctx.allocator,
                    root_class,
                    null,
                ) catch return error.OutOfMemory;

                // Store the resolved value
                const value_atom = self.ctx.atoms.intern("value") catch return error.OutOfMemory;
                promise_obj.setProperty(self.ctx.allocator, value_atom, result) catch return error.OutOfMemory;

                // Add a 'then' method (simplified - just calls callback with value)
                // For a full implementation, we'd create a proper Promise with thenable support
                const then_atom = self.ctx.atoms.intern("then") catch return error.OutOfMemory;
                promise_obj.setProperty(self.ctx.allocator, then_atom, value.JSValue.true_val) catch return error.OutOfMemory;

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

    /// Perform constructor call (new operator)
    fn doConstruct(self: *Interpreter, argc: u8) InterpreterError!void {
        // Stack layout: [constructor, arg0, arg1, ..., argN-1]

        // Bounds check (same as doCall)
        if (argc > MAX_STACK_ARGS) {
            return error.TooManyArguments;
        }

        // Collect arguments (in reverse order from stack)
        var args: [MAX_STACK_ARGS]value.JSValue = undefined;
        var i: usize = argc;
        while (i > 0) {
            i -= 1;
            args[i] = self.ctx.pop();
        }

        // Pop constructor function
        const ctor_val = self.ctx.pop();

        // Check if callable
        if (!ctor_val.isCallable()) {
            return error.NotCallable;
        }

        // Get the constructor object
        const ctor_obj = object.JSObject.fromValue(ctor_val);

        // Create a new object with the constructor's prototype (if available)
        // Look for a "prototype" property on the constructor
        const root_class = self.ctx.root_class orelse return error.NoRootClass;
        var proto: ?*object.JSObject = null;

        // Try to get constructor.prototype
        const proto_atom = object.Atom.prototype;
        if (ctor_obj.getProperty(proto_atom)) |proto_val| {
            if (proto_val.isObject()) {
                proto = object.JSObject.fromValue(proto_val);
            }
        }

        // Create the new object
        const new_obj = try object.JSObject.create(self.ctx.allocator, root_class, proto);
        const this_val = new_obj.toValue();

        // Check for native function constructor
        if (ctor_obj.getNativeFunctionData()) |native_data| {
            // Call native constructor
            const result = native_data.func(self.ctx, this_val, args[0..argc]) catch |err| {
                // Create error message with constructor name for better debugging
                const ctor_name = if (native_data.name.toPredefinedName()) |name| name else "<constructor>";
                std.log.err("Native constructor '{s}' error: {}", .{ ctor_name, err });
                // Set exception on context for JS-level error handling
                self.ctx.throwException(value.JSValue.exception_val);
                return error.NativeFunctionError;
            };

            // If constructor returns an object, use that; otherwise use 'this'
            if (result.isObject()) {
                try self.ctx.push(result);
            } else {
                try self.ctx.push(this_val);
            }
            return;
        }

        // Check for bytecode function constructor
        if (ctor_obj.getBytecodeFunctionData()) |bc_data| {
            const func_bc = bc_data.bytecode;
            const result = try self.callBytecodeFunction(ctor_val, func_bc, this_val, args[0..argc]);

            // If constructor returns an object, use that; otherwise use 'this'
            if (result.isObject()) {
                try self.ctx.push(result);
            } else {
                try self.ctx.push(this_val);
            }
            return;
        }

        // Unknown constructor type - just return the new object
        try self.ctx.push(this_val);
    }

    /// Generator result for next() calls
    pub const GeneratorResult = struct {
        value: value.JSValue,
        done: bool,
    };

    /// Run generator until next yield or return
    pub fn runGenerator(self: *Interpreter, gen_obj: *object.JSObject) InterpreterError!GeneratorResult {
        const gen_data = gen_obj.getGeneratorData() orelse return error.TypeError;

        // Check if generator is already completed
        if (gen_data.state == .completed) {
            return .{ .value = value.JSValue.undefined_val, .done = true };
        }

        // Mark as executing
        gen_data.state = .executing;

        const func_bc = gen_data.bytecode;

        // Set up interpreter state
        self.pc = func_bc.code.ptr + gen_data.pc_offset;
        self.code_end = func_bc.code.ptr + func_bc.code.len;
        self.constants = func_bc.constants;
        self.current_func = func_bc;

        // Restore locals to stack
        try self.ctx.ensureStack(gen_data.locals.len + gen_data.stack_len);
        for (gen_data.locals) |local| {
            try self.ctx.push(local);
        }
        // Restore saved stack values
        for (0..gen_data.stack_len) |i| {
            try self.ctx.push(gen_data.stack[i]);
        }

        // Execute until yield or return
        while (@intFromPtr(self.pc) < @intFromPtr(self.code_end)) {
            const op: bytecode.Opcode = @enumFromInt(self.pc[0]);
            self.pc += 1;

            switch (op) {
                .yield_val => {
                    // Pop the yielded value
                    const yielded = self.ctx.pop();

                    // Save current state
                    gen_data.pc_offset = @intCast(@intFromPtr(self.pc) - @intFromPtr(func_bc.code.ptr));
                    gen_data.state = .suspended_yield;

                    // Save locals
                    for (0..gen_data.locals.len) |i| {
                        gen_data.locals[i] = self.ctx.getLocal(@intCast(i));
                    }

                    // Save remaining stack
                    gen_data.stack_len = 0;
                    while (self.ctx.sp > gen_data.locals.len) {
                        if (gen_data.stack_len >= gen_data.stack.len) break;
                        gen_data.stack[gen_data.stack_len] = self.ctx.pop();
                        gen_data.stack_len += 1;
                    }

                    // Clean up stack
                    self.ctx.sp = 0;

                    return .{ .value = yielded, .done = false };
                },
                .ret, .ret_undefined => {
                    const result = if (op == .ret) self.ctx.pop() else value.JSValue.undefined_val;
                    gen_data.state = .completed;
                    self.ctx.sp = 0;
                    return .{ .value = result, .done = true };
                },
                else => {
                    // Handle all other opcodes using main dispatch
                    // This is a simplified approach - we inline critical opcodes
                    self.pc -= 1; // Back up to re-read opcode
                    const result = self.dispatchOne();
                    if (result) |_| {
                        // Should not happen in generator
                        gen_data.state = .completed;
                        self.ctx.sp = 0;
                        return .{ .value = value.JSValue.undefined_val, .done = true };
                    } else |_| {
                        // Continue with next opcode
                    }
                },
            }
        }

        // Reached end of bytecode without explicit return
        gen_data.state = .completed;
        self.ctx.sp = 0;
        return .{ .value = value.JSValue.undefined_val, .done = true };
    }

    /// Dispatch a single opcode (returns result if halt/ret, error otherwise to continue)
    fn dispatchOne(self: *Interpreter) InterpreterError!value.JSValue {
        const op: bytecode.Opcode = @enumFromInt(self.pc[0]);
        self.pc += 1;

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
                try self.ctx.push(try subValues(a, b));
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

// ============================================================================
// Helper Functions (standalone, used by interpreter)
// ============================================================================

/// Subtract two values (integer fast path)
fn subValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const result = @subWithOverflow(a.getInt(), b.getInt());
        if (result[1] == 0) {
            return value.JSValue.fromInt(result[0]);
        }
        return error.IntegerOverflow;
    }
    // Float fallback - need to use interpreter's allocFloat for heap allocation
    // For standalone function, just return error (use interpreter method for floats)
    return error.TypeError;
}

/// Multiply two values (integer fast path)
fn mulValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const result = @mulWithOverflow(a.getInt(), b.getInt());
        if (result[1] == 0) {
            return value.JSValue.fromInt(result[0]);
        }
        return error.IntegerOverflow;
    }
    return error.TypeError;
}

/// Modulo two values
fn modValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
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
fn toInt32(v: value.JSValue) i32 {
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
    const allocator = std.testing.allocator;
    const gc_mod = @import("gc.zig");
    const parser_mod = @import("parser.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 4096 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test: 1 + 2
    {
        var strings = string_mod.StringTable.init(allocator);
        defer strings.deinit();

        var p = parser_mod.Parser.init(allocator, "1 + 2", &strings, null);
        defer p.deinit();

        const code = try p.parse();

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
        const result = try interp.run(&func);
        try std.testing.expect(result.isInt());
        try std.testing.expectEqual(@as(i32, 3), result.getInt());
    }

    // Reset stack for next test
    ctx.sp = 0;

    // Test: var x = 10; x * 2
    {
        var strings = string_mod.StringTable.init(allocator);
        defer strings.deinit();

        var p = parser_mod.Parser.init(allocator, "var x = 10; x * 2", &strings, null);
        defer p.deinit();

        const code = try p.parse();

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
        const result = try interp.run(&func);
        try std.testing.expect(result.isInt());
        try std.testing.expectEqual(@as(i32, 20), result.getInt());
    }
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const gc_mod = @import("gc.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Create a simple function: function add(a, b) { return a + b; }
    // Bytecode: get_loc_0, get_loc_1, add, ret
    const inner_code = [_]u8{
        @intFromEnum(bytecode.Opcode.get_loc_0), // a
        @intFromEnum(bytecode.Opcode.get_loc_1), // b
        @intFromEnum(bytecode.Opcode.add),
        @intFromEnum(bytecode.Opcode.ret),
    };

    const inner_func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 2,
        .local_count = 2, // a, b
        .stack_size = 16,
        .flags = .{},
        .code = &inner_code,
        .constants = &.{},
        .source_map = null,
    };

    // Create function object
    const root_class = ctx.root_class.?;
    const func_obj = try object.JSObject.createBytecodeFunction(allocator, root_class, &inner_func, .length);

    // Register as global "add" (Atom.abs = 95)
    try ctx.setGlobal(.abs, func_obj.toValue());

    var interp = Interpreter.init(ctx);

    // Test: get_global(abs), push 7, push 8, call(2), ret
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
    const parser_mod = @import("parser.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test: function add(a, b) { return a + b; } add(3, 4)
    var strings = string_mod.StringTable.init(allocator);
    defer strings.deinit();

    var p = parser_mod.Parser.init(allocator, "function add(a, b) { return a + b; } add(3, 4)", &strings, null);
    defer p.deinit();

    const code = try p.parse();

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
    const result = try interp.run(&func);
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 7), result.getInt()); // 3 + 4 = 7
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
    const parser_mod = @import("parser.zig");
    const string_mod = @import("string.zig");

    var gc_state = try gc_mod.GC.init(allocator, .{ .nursery_size = 8192 });
    defer gc_state.deinit();

    var ctx = try context.Context.init(allocator, &gc_state, .{});
    defer ctx.deinit();

    // Test: function with default parameter
    var strings = string_mod.StringTable.init(allocator);

    var p = parser_mod.Parser.init(allocator, "function greet(name = 'World') { return name; } greet()", &strings, null);
    defer p.deinit();

    const code = try p.parse();

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
    const result = try interp.run(&func);
    try std.testing.expect(result.isString());
    const result_str = result.toPtr(string.JSString);
    try std.testing.expectEqualStrings("World", result_str.data());

    // Cleanup
    string.freeString(allocator, result_str);
}
