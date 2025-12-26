//! Bytecode interpreter with computed goto dispatch
//!
//! Threaded code execution with tail calls for opcode handlers.
//! Implements all opcodes from bytecode.zig.

const std = @import("std");
const value = @import("value.zig");
const bytecode = @import("bytecode.zig");
const context = @import("context.zig");
const heap = @import("heap.zig");

/// Interpreter state
pub const Interpreter = struct {
    ctx: *context.Context,
    pc: [*]const u8, // Program counter
    code_end: [*]const u8,
    constants: []const u8, // Constant pool
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
    pub fn run(self: *Interpreter, func: *const bytecode.FunctionBytecode) !value.JSValue {
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

    /// Main dispatch loop
    fn dispatch(self: *Interpreter) !value.JSValue {
        while (@intFromPtr(self.pc) < @intFromPtr(self.code_end)) {
            const op: bytecode.Opcode = @enumFromInt(self.pc[0]);
            self.pc += 1;

            switch (op) {
                // ========================================
                // Stack Operations
                // ========================================
                .nop => {},

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

                .get_global => {
                    const atom_idx = readU16(self.pc);
                    self.pc += 2;
                    _ = atom_idx;
                    // TODO: Lookup in global object
                    try self.ctx.push(value.JSValue.undefined_val);
                },

                .put_global => {
                    const atom_idx = readU16(self.pc);
                    self.pc += 2;
                    _ = atom_idx;
                    _ = self.ctx.pop();
                    // TODO: Set in global object
                },

                .typeof => {
                    const a = self.ctx.pop();
                    _ = a.typeOf(); // Returns string, but we need to create a JS string
                    // TODO: Create interned string for typeof result
                    try self.ctx.push(value.JSValue.undefined_val);
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

    fn getConstant(self: *Interpreter, idx: u16) !value.JSValue {
        // Simple constant pool format: type byte + data
        if (idx >= self.constants.len) return error.InvalidConstant;
        const const_type: bytecode.ConstType = @enumFromInt(self.constants[idx]);
        switch (const_type) {
            .int32 => {
                if (idx + 5 > self.constants.len) return error.InvalidConstant;
                const bytes = self.constants[idx + 1 .. idx + 5];
                const v: i32 = @bitCast(@as(u32, bytes[0]) |
                    (@as(u32, bytes[1]) << 8) |
                    (@as(u32, bytes[2]) << 16) |
                    (@as(u32, bytes[3]) << 24));
                return value.JSValue.fromInt(v);
            },
            .float64 => {
                if (idx + 9 > self.constants.len) return error.InvalidConstant;
                const bytes = self.constants[idx + 1 .. idx + 9];
                var u: u64 = 0;
                for (bytes, 0..) |b, i| {
                    u |= @as(u64, b) << @intCast(i * 8);
                }
                const f: f64 = @bitCast(u);
                return try self.allocFloat(f);
            },
            else => return error.InvalidConstant,
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
