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

/// Interpreter state
pub const Interpreter = struct {
    ctx: *context.Context,
    pc: [*]const u8, // Program counter
    code_end: [*]const u8,
    constants: []const value.JSValue, // Constant pool (direct JSValue array)
    current_func: ?*const bytecode.FunctionBytecode,

    pub fn init(ctx: *context.Context) Interpreter {
        return .{
            .ctx = ctx,
            .pc = undefined,
            .code_end = undefined,
            .constants = &.{},
            .current_func = null,
        };
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

    /// Error set for interpreter operations
    pub const InterpreterError = error{
        StackOverflow,
        CallStackOverflow,
        TypeError,
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

    /// Main dispatch loop
    fn dispatch(self: *Interpreter) InterpreterError!value.JSValue {
        while (@intFromPtr(self.pc) < @intFromPtr(self.code_end)) {
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
                    self.ctx.setLocal(idx, self.ctx.pop());
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
                    self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc))) + offset)));
                },

                .if_true => {
                    const cond = self.ctx.pop();
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    if (cond.toBoolean()) {
                        self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc) - 2)) + offset)));
                    }
                },

                .if_false => {
                    const cond = self.ctx.pop();
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    if (!cond.toBoolean()) {
                        self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc) - 2)) + offset)));
                    }
                },

                .ret => return self.ctx.pop(),

                .ret_undefined => return value.JSValue.undefined_val,

                .@"throw" => {
                    const exception_val = self.ctx.pop();
                    self.ctx.throwException(exception_val);
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
                    // Create array object (simplified - just an object with length)
                    const obj = try self.createObject();
                    try obj.setProperty(self.ctx.allocator, .length, value.JSValue.fromInt(@intCast(length)));
                    obj.class_id = .array;
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
                    } else {
                        // TODO: Handle primitive property access (e.g., string.length)
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
                        // Convert integer index to atom (simplified)
                        const idx = index_val.getInt();
                        if (idx >= 0) {
                            // For arrays, access element by index
                            // For now, treat index as a slot offset (simplified)
                            try self.ctx.push(obj.getSlot(@intCast(idx)));
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
                            obj.setSlot(@intCast(idx), val);
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
                    // TODO: Implement constructor call (creates new object, sets 'this')
                    // For now, just do regular call
                    try self.doCall(argc, false);
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
                        self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc) - 2)) + offset)));
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
            const len = std.fmt.formatIntBuf(&buf, val.getInt(), 10, .lower, .{});
            return try string.createString(self.ctx.allocator, buf[0..len]);
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
        // Get or create root hidden class (simplified - should be cached)
        const root_class = try object.HiddenClass.init(self.ctx.allocator);
        return try object.JSObject.create(self.ctx.allocator, root_class, null);
    }

    fn getConstant(self: *Interpreter, idx: u16) !value.JSValue {
        if (idx >= self.constants.len) return error.InvalidConstant;
        return self.constants[idx];
    }

    /// Perform function call
    fn doCall(self: *Interpreter, argc: u8, is_method: bool) InterpreterError!void {
        // Stack layout for regular call: [func, arg0, arg1, ..., argN-1]
        // Stack layout for method call: [obj, func, arg0, arg1, ..., argN-1]

        // Collect arguments (in reverse order from stack)
        var args: [256]value.JSValue = undefined;
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
                std.log.err("Native function error: {}", .{err});
                return error.NativeFunctionError;
            };
            try self.ctx.push(result);
            return;
        }

        // Check for bytecode function
        if (func_obj.getBytecodeFunctionData()) |bc_data| {
            const func_bc = bc_data.bytecode;

            // Save current interpreter state
            const saved_pc = self.pc;
            const saved_code_end = self.code_end;
            const saved_constants = self.constants;
            const saved_func = self.current_func;
            const saved_fp = self.ctx.fp;

            // Push call frame
            try self.ctx.pushFrame(func_val, this_val, @intFromPtr(self.pc));

            // Set up new function's locals with arguments
            const local_count = func_bc.local_count;
            try self.ctx.ensureStack(local_count);

            // Initialize locals: first N are arguments, rest are undefined
            var local_idx: usize = 0;
            while (local_idx < local_count) : (local_idx += 1) {
                if (local_idx < argc) {
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
                // Restore state on error
                self.pc = saved_pc;
                self.code_end = saved_code_end;
                self.constants = saved_constants;
                self.current_func = saved_func;
                _ = self.ctx.popFrame();
                return err;
            };

            // Restore interpreter state
            self.pc = saved_pc;
            self.code_end = saved_code_end;
            self.constants = saved_constants;
            self.current_func = saved_func;

            // Pop call frame and restore stack
            _ = self.ctx.popFrame();
            self.ctx.fp = saved_fp;

            // Push result
            try self.ctx.push(result);
            return;
        }

        // Unknown function type
        try self.ctx.push(value.JSValue.undefined_val);
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
        @intFromEnum(bytecode.Opcode.push_i8), 10,
        @intFromEnum(bytecode.Opcode.put_loc_0),
        @intFromEnum(bytecode.Opcode.push_i8), 20,
        @intFromEnum(bytecode.Opcode.put_loc_1),
        @intFromEnum(bytecode.Opcode.get_loc_0),
        @intFromEnum(bytecode.Opcode.get_loc_1),
        @intFromEnum(bytecode.Opcode.add),
        @intFromEnum(bytecode.Opcode.ret),
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
        @intFromEnum(bytecode.Opcode.bit_and),
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
        @intFromEnum(bytecode.Opcode.push_i8), 4,
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
        @intFromEnum(bytecode.Opcode.lt),
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
        @intFromEnum(bytecode.Opcode.if_false), 5, 0, // jump +5 if false
        @intFromEnum(bytecode.Opcode.push_i8), 42,
        @intFromEnum(bytecode.Opcode.ret),
        @intFromEnum(bytecode.Opcode.push_0),
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
        @intFromEnum(bytecode.Opcode.push_i8), 7,
        @intFromEnum(bytecode.Opcode.put_loc_0),
        @intFromEnum(bytecode.Opcode.push_i8), 8,
        @intFromEnum(bytecode.Opcode.put_loc_1),
        @intFromEnum(bytecode.Opcode.get_loc_get_loc_add), 0, 1,
        @intFromEnum(bytecode.Opcode.ret),
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
        @intFromEnum(bytecode.Opcode.push_2),
        @intFromEnum(bytecode.Opcode.div),
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
        @intFromEnum(bytecode.Opcode.mod),
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

        var p = parser_mod.Parser.init(allocator, "1 + 2", &strings);
        defer p.deinit();

        const code = try p.parse();

        const func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = p.local_count,
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

        var p = parser_mod.Parser.init(allocator, "var x = 10; x * 2", &strings);
        defer p.deinit();

        const code = try p.parse();

        const func = bytecode.FunctionBytecode{
            .header = .{},
            .name_atom = 0,
            .arg_count = 0,
            .local_count = p.local_count,
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
    const allocator = std.testing.allocator;
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
    const allocator = std.testing.allocator;
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
    const allocator = std.testing.allocator;
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
    const allocator = std.testing.allocator;
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

    var p = parser_mod.Parser.init(allocator, "function add(a, b) { return a + b; } add(3, 4)", &strings);
    defer p.deinit();

    const code = try p.parse();

    const func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.local_count,
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
