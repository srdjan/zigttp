//! AIR Interpreter - Reference Execution Engine
//!
//! Validates AIR semantics by executing AIR instructions directly.
//! Used for differential testing against the bytecode interpreter.
//!
//! This interpreter is intentionally simple and unoptimized - it exists
//! purely for correctness validation, not performance.

const std = @import("std");
const types = @import("types.zig");
const value_mod = @import("../value.zig");

const Function = types.Function;
const Block = types.Block;
const Inst = types.Inst;
const InstOp = types.InstOp;
const ValueId = types.ValueId;
const BlockId = types.BlockId;
const Terminator = types.Terminator;
const JSValue = value_mod.JSValue;

/// Interpreter execution errors
pub const InterpError = error{
    InvalidBlockId,
    InvalidValueId,
    UndefinedValue,
    TypeMismatch,
    Overflow,
    DivisionByZero,
    Deoptimize,
    Unreachable,
    OutOfMemory,
};

/// AIR interpreter state
pub const AirInterpreter = struct {
    allocator: std.mem.Allocator,
    func: *const Function,

    /// Value registers: ValueId.index -> JSValue
    registers: std.AutoHashMapUnmanaged(u24, JSValue),

    /// Current block being executed
    current_block: BlockId,

    /// Previous block (for phi resolution)
    prev_block: BlockId,

    /// Arguments passed to the function
    args: []const JSValue,

    pub fn init(allocator: std.mem.Allocator, func: *const Function) AirInterpreter {
        return .{
            .allocator = allocator,
            .func = func,
            .registers = .{},
            .current_block = BlockId.entry,
            .prev_block = BlockId.none,
            .args = &.{},
        };
    }

    pub fn deinit(self: *AirInterpreter) void {
        self.registers.deinit(self.allocator);
    }

    /// Execute the function with given arguments
    pub fn run(self: *AirInterpreter, args: []const JSValue) InterpError!JSValue {
        self.args = args;
        self.current_block = BlockId.entry;
        self.prev_block = BlockId.none;
        self.registers.clearRetainingCapacity();

        // Store arguments as parameter values
        for (args, 0..) |arg, i| {
            self.setReg(ValueId.param(@intCast(i)), arg) catch return InterpError.OutOfMemory;
        }

        // Execute until we hit a return
        while (true) {
            const block = self.func.getBlockConst(self.current_block) orelse return InterpError.InvalidBlockId;

            // Execute phi assignments from previous block
            for (block.params.items) |param| {
                const incoming = param.phi.getValueFor(self.prev_block);
                if (incoming) |val| {
                    const resolved = try self.getReg(val);
                    self.setReg(param.value, resolved) catch return InterpError.OutOfMemory;
                }
            }

            // Execute instructions
            for (block.instructions.items) |inst| {
                try self.executeInst(inst);
            }

            // Handle terminator
            switch (block.terminator.kind) {
                .ret => {
                    return try self.getReg(block.terminator.return_value);
                },
                .ret_undefined => {
                    return JSValue.undefined_val;
                },
                .goto => {
                    self.prev_block = self.current_block;
                    self.current_block = block.terminator.true_target;
                },
                .branch => {
                    const cond = try self.getReg(block.terminator.condition);
                    const taken = self.toBoolean(cond);
                    self.prev_block = self.current_block;
                    self.current_block = if (taken)
                        block.terminator.true_target
                    else
                        block.terminator.false_target;
                },
                .deopt => {
                    return InterpError.Deoptimize;
                },
                .unreachable_term => {
                    return InterpError.Unreachable;
                },
                .none => {
                    // Fallthrough to next block (implicit)
                    self.prev_block = self.current_block;
                    const next_idx = self.current_block.asIndex() + 1;
                    if (next_idx < self.func.blocks.items.len) {
                        self.current_block = BlockId.fromIndex(next_idx);
                    } else {
                        return JSValue.undefined_val;
                    }
                },
            }
        }
    }

    /// Execute a single instruction
    fn executeInst(self: *AirInterpreter, inst: Inst) InterpError!void {
        const result: JSValue = switch (inst.op) {
            // Constants
            .const_int => JSValue.fromInt(inst.extra.int_const),
            .const_true => JSValue.true_val,
            .const_false => JSValue.false_val,
            .const_null => JSValue.null_val,
            .const_undefined => JSValue.undefined_val,

            // Parameters
            .param => blk: {
                const idx: u32 = @intCast(inst.extra.int_const);
                if (idx < self.args.len) {
                    break :blk self.args[idx];
                }
                break :blk JSValue.undefined_val;
            },

            // Integer arithmetic
            .add_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                const a = lhs.getInt();
                const b = rhs.getInt();
                const res = @addWithOverflow(a, b);
                if (res[1] != 0) return InterpError.Overflow;
                break :blk JSValue.fromInt(res[0]);
            },

            .sub_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                const a = lhs.getInt();
                const b = rhs.getInt();
                const res = @subWithOverflow(a, b);
                if (res[1] != 0) return InterpError.Overflow;
                break :blk JSValue.fromInt(res[0]);
            },

            .mul_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                const a = lhs.getInt();
                const b = rhs.getInt();
                const res = @mulWithOverflow(a, b);
                if (res[1] != 0) return InterpError.Overflow;
                break :blk JSValue.fromInt(res[0]);
            },

            .div_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                const a = lhs.getInt();
                const b = rhs.getInt();
                if (b == 0) return InterpError.DivisionByZero;
                break :blk JSValue.fromInt(@divTrunc(a, b));
            },

            .mod_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                const a = lhs.getInt();
                const b = rhs.getInt();
                if (b == 0) return InterpError.DivisionByZero;
                break :blk JSValue.fromInt(@mod(a, b));
            },

            .neg_int => blk: {
                const operand = try self.getReg(inst.operands[0]);
                if (!operand.isInt()) return InterpError.TypeMismatch;
                const a = operand.getInt();
                const res = @subWithOverflow(@as(i32, 0), a);
                if (res[1] != 0) return InterpError.Overflow;
                break :blk JSValue.fromInt(res[0]);
            },

            .inc_int => blk: {
                const operand = try self.getReg(inst.operands[0]);
                if (!operand.isInt()) return InterpError.TypeMismatch;
                const a = operand.getInt();
                const res = @addWithOverflow(a, @as(i32, 1));
                if (res[1] != 0) return InterpError.Overflow;
                break :blk JSValue.fromInt(res[0]);
            },

            .dec_int => blk: {
                const operand = try self.getReg(inst.operands[0]);
                if (!operand.isInt()) return InterpError.TypeMismatch;
                const a = operand.getInt();
                const res = @subWithOverflow(a, @as(i32, 1));
                if (res[1] != 0) return InterpError.Overflow;
                break :blk JSValue.fromInt(res[0]);
            },

            // Comparisons
            .lt_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromBool(lhs.getInt() < rhs.getInt());
            },

            .lte_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromBool(lhs.getInt() <= rhs.getInt());
            },

            .gt_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromBool(lhs.getInt() > rhs.getInt());
            },

            .gte_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromBool(lhs.getInt() >= rhs.getInt());
            },

            .eq_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromBool(lhs.getInt() == rhs.getInt());
            },

            .neq_int => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromBool(lhs.getInt() != rhs.getInt());
            },

            // Bitwise operations
            .bit_and => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromInt(lhs.getInt() & rhs.getInt());
            },

            .bit_or => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromInt(lhs.getInt() | rhs.getInt());
            },

            .bit_xor => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromInt(lhs.getInt() ^ rhs.getInt());
            },

            .bit_not => blk: {
                const operand = try self.getReg(inst.operands[0]);
                if (!operand.isInt()) return InterpError.TypeMismatch;
                break :blk JSValue.fromInt(~operand.getInt());
            },

            .shl => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                const shift: u5 = @intCast(@mod(rhs.getInt(), 32));
                break :blk JSValue.fromInt(lhs.getInt() << shift);
            },

            .shr => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                const shift: u5 = @intCast(@mod(rhs.getInt(), 32));
                break :blk JSValue.fromInt(lhs.getInt() >> shift);
            },

            .ushr => blk: {
                const lhs = try self.getReg(inst.operands[0]);
                const rhs = try self.getReg(inst.operands[1]);
                if (!lhs.isInt() or !rhs.isInt()) return InterpError.TypeMismatch;
                const unsigned_lhs: u32 = @bitCast(lhs.getInt());
                const shift: u5 = @intCast(@mod(rhs.getInt(), 32));
                const result: i32 = @bitCast(unsigned_lhs >> shift);
                break :blk JSValue.fromInt(result);
            },

            // Logical
            .not => blk: {
                const operand = try self.getReg(inst.operands[0]);
                break :blk JSValue.fromBool(!self.toBoolean(operand));
            },

            // Guards
            .guard_int => blk: {
                const operand = try self.getReg(inst.operands[0]);
                if (!operand.isInt()) return InterpError.Deoptimize;
                break :blk operand;
            },

            // Copy
            .copy => try self.getReg(inst.operands[0]),

            // Phi is handled at block entry
            .phi => JSValue.undefined_val,

            // Load/store local (should be resolved during SSA construction)
            .load_local, .store_local => JSValue.undefined_val,

            // Generic operations (fallback)
            .add_generic, .sub_generic, .mul_generic => JSValue.undefined_val,

            .const_float => JSValue.undefined_val, // Not supported in simple interpreter
        };

        if (!inst.result.isNone()) {
            self.setReg(inst.result, result) catch return InterpError.OutOfMemory;
        }
    }

    /// Get value from register
    fn getReg(self: *AirInterpreter, val: ValueId) InterpError!JSValue {
        if (val.isNone()) return JSValue.undefined_val;

        return switch (val.kind) {
            .local => self.registers.get(val.index) orelse InterpError.UndefinedValue,
            .param => blk: {
                if (val.index < self.args.len) {
                    break :blk self.args[val.index];
                }
                break :blk JSValue.undefined_val;
            },
            .constant => JSValue.undefined_val, // TODO: look up in constant pool
            .undefined => JSValue.undefined_val,
        };
    }

    /// Set value in register
    fn setReg(self: *AirInterpreter, val: ValueId, value: JSValue) !void {
        if (val.kind == .local) {
            try self.registers.put(self.allocator, val.index, value);
        }
    }

    /// Convert value to boolean (JavaScript truthiness)
    fn toBoolean(self: *AirInterpreter, val: JSValue) bool {
        _ = self;
        if (val.isNull() or val.isUndefined()) return false;
        if (val.isBool()) return val.getBool();
        if (val.isInt()) return val.getInt() != 0;
        // Strings, objects are always truthy (simplified)
        return true;
    }
};

/// Execute AIR function and return result
pub fn execute(
    allocator: std.mem.Allocator,
    func: *const Function,
    args: []const JSValue,
) InterpError!JSValue {
    var interp = AirInterpreter.init(allocator, func);
    defer interp.deinit();
    return interp.run(args);
}

// ============================================================================
// Unit Tests
// ============================================================================

const bytecode = @import("../bytecode.zig");
const builder = @import("builder.zig");

test "AirInterpreter: constant return" {
    const allocator = std.testing.allocator;
    const Opcode = bytecode.Opcode;

    // push_1, ret -> returns 1
    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.ret),
    };

    var bc_func = bytecode.FunctionBytecode{
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

    var func = try builder.buildFromBytecode(allocator, &bc_func);
    defer func.deinit();

    const result = try execute(allocator, &func, &.{});
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 1), result.getInt());
}

test "AirInterpreter: arithmetic" {
    const allocator = std.testing.allocator;
    const Opcode = bytecode.Opcode;

    // push_2, push_3, add, ret -> returns 5
    const code = [_]u8{
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.push_3),
        @intFromEnum(Opcode.add),
        @intFromEnum(Opcode.ret),
    };

    var bc_func = bytecode.FunctionBytecode{
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

    var func = try builder.buildFromBytecode(allocator, &bc_func);
    defer func.deinit();

    const result = try execute(allocator, &func, &.{});
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 5), result.getInt());
}

test "AirInterpreter: comparison and branch" {
    const allocator = std.testing.allocator;
    const Opcode = bytecode.Opcode;

    // if (true) { return 1; } else { return 2; }
    const code = [_]u8{
        @intFromEnum(Opcode.push_true), // 0
        @intFromEnum(Opcode.if_false), 0x02, 0x00, // 1-3: -> offset 6
        @intFromEnum(Opcode.push_1), // 4
        @intFromEnum(Opcode.ret), // 5
        @intFromEnum(Opcode.push_2), // 6
        @intFromEnum(Opcode.ret), // 7
    };

    var bc_func = bytecode.FunctionBytecode{
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

    var func = try builder.buildFromBytecode(allocator, &bc_func);
    defer func.deinit();

    const result = try execute(allocator, &func, &.{});
    try std.testing.expect(result.isInt());
    // true branch should return 1
    try std.testing.expectEqual(@as(i32, 1), result.getInt());
}

test "AirInterpreter: local variables" {
    const allocator = std.testing.allocator;
    const Opcode = bytecode.Opcode;

    // x = 10; return x;
    const code = [_]u8{
        @intFromEnum(Opcode.push_i8), 10, // 0-1
        @intFromEnum(Opcode.put_loc_0), // 2
        @intFromEnum(Opcode.get_loc_0), // 3
        @intFromEnum(Opcode.ret), // 4
    };

    var bc_func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = 1,
        .stack_size = 1,
        .flags = .{},
        .code = &code,
        .constants = &.{},
        .source_map = null,
    };

    var func = try builder.buildFromBytecode(allocator, &bc_func);
    defer func.deinit();

    const result = try execute(allocator, &func, &.{});
    try std.testing.expect(result.isInt());
    try std.testing.expectEqual(@as(i32, 10), result.getInt());
}
