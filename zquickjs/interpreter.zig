//! Bytecode interpreter with computed goto dispatch
//!
//! Threaded code execution with tail calls for opcode handlers.

const std = @import("std");
const value = @import("value.zig");
const bytecode = @import("bytecode.zig");
const context = @import("context.zig");

/// Interpreter state
pub const Interpreter = struct {
    ctx: *context.Context,
    pc: [*]const u8, // Program counter
    code_end: [*]const u8,

    pub fn init(ctx: *context.Context) Interpreter {
        return .{
            .ctx = ctx,
            .pc = undefined,
            .code_end = undefined,
        };
    }

    /// Run bytecode function
    pub fn run(self: *Interpreter, func: *const bytecode.FunctionBytecode) !value.JSValue {
        self.pc = func.code.ptr;
        self.code_end = func.code.ptr + func.code.len;

        return self.dispatch();
    }

    /// Main dispatch loop
    fn dispatch(self: *Interpreter) !value.JSValue {
        while (@intFromPtr(self.pc) < @intFromPtr(self.code_end)) {
            const op: bytecode.Opcode = @enumFromInt(self.pc[0]);
            self.pc += 1;

            switch (op) {
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

                .add => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(try addValues(a, b));
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

                .neg => {
                    const a = self.ctx.pop();
                    if (a.isInt()) {
                        try self.ctx.push(value.JSValue.fromInt(-a.getInt()));
                    } else {
                        return error.TypeError;
                    }
                },

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

                .eq, .strict_eq => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(a.raw == b.raw));
                },

                .neq, .strict_neq => {
                    const b = self.ctx.pop();
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(a.raw != b.raw));
                },

                .not => {
                    const a = self.ctx.pop();
                    try self.ctx.push(value.JSValue.fromBool(!isTruthy(a)));
                },

                .goto => {
                    const offset = readI16(self.pc);
                    self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc))) + offset)));
                },

                .if_true => {
                    const cond = self.ctx.pop();
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    if (isTruthy(cond)) {
                        self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc) - 2)) + offset)));
                    }
                },

                .if_false => {
                    const cond = self.ctx.pop();
                    const offset = readI16(self.pc);
                    self.pc += 2;
                    if (!isTruthy(cond)) {
                        self.pc = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(@intFromPtr(self.pc) - 2)) + offset)));
                    }
                },

                .ret => return self.ctx.pop(),

                .ret_undefined => return value.JSValue.undefined_val,

                else => {
                    std.log.warn("Unimplemented opcode: {}", .{op});
                    return error.UnimplementedOpcode;
                },
            }
        }

        return value.JSValue.undefined_val;
    }
};

/// Add two values
fn addValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        // Fast path: integer addition
        const result = @addWithOverflow(a.getInt(), b.getInt());
        if (result[1] == 0) {
            return value.JSValue.fromInt(result[0]);
        }
        // Overflow - need to convert to float (not implemented yet)
        return error.IntegerOverflow;
    }
    // TODO: Handle other types (float, string concat)
    return error.TypeError;
}

/// Subtract two values
fn subValues(a: value.JSValue, b: value.JSValue) !value.JSValue {
    if (a.isInt() and b.isInt()) {
        const result = @subWithOverflow(a.getInt(), b.getInt());
        if (result[1] == 0) {
            return value.JSValue.fromInt(result[0]);
        }
        return error.IntegerOverflow;
    }
    return error.TypeError;
}

/// Multiply two values
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

/// Compare two values
fn compareValues(a: value.JSValue, b: value.JSValue) !std.math.Order {
    if (a.isInt() and b.isInt()) {
        return std.math.order(a.getInt(), b.getInt());
    }
    return error.TypeError;
}

/// Check if value is truthy
fn isTruthy(v: value.JSValue) bool {
    if (v.isNull() or v.isUndefined() or v.isFalse()) return false;
    if (v.isTrue()) return true;
    if (v.isInt()) return v.getInt() != 0;
    // TODO: Handle other types
    return true;
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
