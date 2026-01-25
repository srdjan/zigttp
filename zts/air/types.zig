//! AIR Type Definitions
//!
//! Core data structures for the Abstract Intermediate Representation (AIR).
//! AIR is a CFG-based SSA form used for the optimized JIT tier.

const std = @import("std");
const cfg = @import("cfg.zig");
const bytecode = @import("../bytecode.zig");
const type_feedback = @import("../type_feedback.zig");

// Re-export BlockId from cfg
pub const BlockId = cfg.BlockId;

/// Value identifier in SSA form
/// Encodes both the index and the kind of value in a packed 32-bit representation
pub const ValueId = packed struct(u32) {
    index: u24,
    kind: ValueKind,

    pub const ValueKind = enum(u8) {
        local = 0, // SSA value (instruction result)
        param = 1, // Function parameter
        constant = 2, // Constant pool index
        undefined = 3, // Undefined sentinel
    };

    pub const none: ValueId = .{ .index = 0xFFFFFF, .kind = .undefined };

    pub fn isNone(self: ValueId) bool {
        return self.index == 0xFFFFFF and self.kind == .undefined;
    }

    pub fn local(idx: u24) ValueId {
        return .{ .index = idx, .kind = .local };
    }

    pub fn param(idx: u24) ValueId {
        return .{ .index = idx, .kind = .param };
    }

    pub fn constant(idx: u24) ValueId {
        return .{ .index = idx, .kind = .constant };
    }

    pub fn format(self: ValueId, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        if (self.isNone()) {
            try writer.writeAll("_");
        } else switch (self.kind) {
            .local => try writer.print("%{d}", .{self.index}),
            .param => try writer.print("$arg{d}", .{self.index}),
            .constant => try writer.print("#{d}", .{self.index}),
            .undefined => try writer.writeAll("undef"),
        }
    }
};

/// AIR instruction opcodes
pub const InstOp = enum(u8) {
    // Parameters and constants
    param, // Function parameter
    const_int, // Integer constant
    const_float, // Float constant
    const_true, // Boolean true
    const_false, // Boolean false
    const_null, // Null value
    const_undefined, // Undefined value

    // Phi node (SSA merge)
    phi,

    // Integer arithmetic (SMI-specialized)
    add_int,
    sub_int,
    mul_int,
    div_int,
    mod_int,
    neg_int,
    inc_int,
    dec_int,

    // Comparison (int-specialized)
    lt_int,
    lte_int,
    gt_int,
    gte_int,
    eq_int,
    neq_int,

    // Bitwise operations
    bit_and,
    bit_or,
    bit_xor,
    bit_not,
    shl,
    shr,
    ushr,

    // Type guards
    guard_int, // Deopt if not SMI

    // Logical
    not,

    // Local variable operations (pre-SSA, converted during construction)
    load_local,
    store_local,

    // Copy (for phi resolution)
    copy,

    // Generic operations (non-specialized)
    add_generic,
    sub_generic,
    mul_generic,

    pub fn isTerminator(self: InstOp) bool {
        return false; // Instructions are never terminators - use Terminator struct
    }

    pub fn hasSideEffects(self: InstOp) bool {
        return switch (self) {
            .store_local, .guard_int => true,
            else => false,
        };
    }
};

/// Extra data for instructions that need additional info
pub const InstExtra = union {
    none: void,
    int_const: i32,
    float_const: f64,
    local_idx: u8,
    phi_data: PhiData,
};

/// Phi node data - maps predecessors to their incoming values
pub const PhiData = struct {
    /// Predecessor block IDs
    preds: [4]BlockId,
    /// Incoming values from each predecessor
    values: [4]ValueId,
    /// Number of valid entries
    count: u8,

    pub const empty: PhiData = .{
        .preds = .{BlockId.none} ** 4,
        .values = .{ValueId.none} ** 4,
        .count = 0,
    };

    pub fn addIncoming(self: *PhiData, pred: BlockId, value: ValueId) void {
        if (self.count < 4) {
            self.preds[self.count] = pred;
            self.values[self.count] = value;
            self.count += 1;
        }
    }

    pub fn getValueFor(self: *const PhiData, pred: BlockId) ?ValueId {
        for (self.preds[0..self.count], self.values[0..self.count]) |p, v| {
            if (p == pred) return v;
        }
        return null;
    }
};

/// AIR instruction
pub const Inst = struct {
    op: InstOp,
    result: ValueId,
    operands: [3]ValueId,
    bytecode_offset: u16, // For deopt mapping
    extra: InstExtra,

    pub fn init(op: InstOp, result: ValueId) Inst {
        return .{
            .op = op,
            .result = result,
            .operands = .{ValueId.none} ** 3,
            .bytecode_offset = 0,
            .extra = .{ .none = {} },
        };
    }

    pub fn withOperand(self: Inst, idx: usize, val: ValueId) Inst {
        var copy = self;
        copy.operands[idx] = val;
        return copy;
    }

    pub fn withBytecodeOffset(self: Inst, offset: u16) Inst {
        var copy = self;
        copy.bytecode_offset = offset;
        return copy;
    }

    pub fn withExtra(self: Inst, extra: InstExtra) Inst {
        var copy = self;
        copy.extra = extra;
        return copy;
    }

    /// Create a binary operation instruction
    pub fn binary(op: InstOp, result: ValueId, lhs: ValueId, rhs: ValueId) Inst {
        return Inst.init(op, result)
            .withOperand(0, lhs)
            .withOperand(1, rhs);
    }

    /// Create a unary operation instruction
    pub fn unary(op: InstOp, result: ValueId, operand: ValueId) Inst {
        return Inst.init(op, result)
            .withOperand(0, operand);
    }

    /// Create a constant integer instruction
    pub fn constInt(result: ValueId, value: i32) Inst {
        return Inst.init(.const_int, result)
            .withExtra(.{ .int_const = value });
    }

    /// Create a phi instruction
    pub fn phi(result: ValueId, data: PhiData) Inst {
        return Inst.init(.phi, result)
            .withExtra(.{ .phi_data = data });
    }
};

/// Block parameter (phi node at block entry)
pub const BlockParam = struct {
    value: ValueId,
    phi: PhiData,
};

/// Block terminator
pub const Terminator = struct {
    kind: Kind,
    condition: ValueId,
    true_target: BlockId,
    false_target: BlockId,
    return_value: ValueId,
    deopt_reason: u8,
    bytecode_offset: u16,

    pub const Kind = enum(u8) {
        none,
        goto, // Unconditional jump
        branch, // Conditional branch
        ret, // Return with value
        ret_undefined, // Return undefined
        deopt, // Deoptimization exit
        unreachable_term,
    };

    pub const default: Terminator = .{
        .kind = .none,
        .condition = ValueId.none,
        .true_target = BlockId.none,
        .false_target = BlockId.none,
        .return_value = ValueId.none,
        .deopt_reason = 0,
        .bytecode_offset = 0,
    };

    pub fn goto(target: BlockId) Terminator {
        var t = default;
        t.kind = .goto;
        t.true_target = target;
        return t;
    }

    pub fn branch(cond: ValueId, true_target: BlockId, false_target: BlockId) Terminator {
        return .{
            .kind = .branch,
            .condition = cond,
            .true_target = true_target,
            .false_target = false_target,
            .return_value = ValueId.none,
            .deopt_reason = 0,
            .bytecode_offset = 0,
        };
    }

    pub fn ret(value: ValueId) Terminator {
        var t = default;
        t.kind = .ret;
        t.return_value = value;
        return t;
    }

    pub fn retUndefined() Terminator {
        var t = default;
        t.kind = .ret_undefined;
        return t;
    }

    pub fn deopt(reason: u8, bc_offset: u16) Terminator {
        return .{
            .kind = .deopt,
            .condition = ValueId.none,
            .true_target = BlockId.none,
            .false_target = BlockId.none,
            .return_value = ValueId.none,
            .deopt_reason = reason,
            .bytecode_offset = bc_offset,
        };
    }
};

/// Basic block in AIR
pub const Block = struct {
    id: BlockId,
    predecessors: std.ArrayListUnmanaged(BlockId),
    instructions: std.ArrayListUnmanaged(Inst),
    params: std.ArrayListUnmanaged(BlockParam), // Phi nodes
    terminator: Terminator,
    bc_start: u32,
    bc_end: u32,
    is_loop_header: bool,
    is_sealed: bool, // All predecessors known

    pub fn init(allocator: std.mem.Allocator, id: BlockId) Block {
        _ = allocator;
        return .{
            .id = id,
            .predecessors = .{},
            .instructions = .{},
            .params = .{},
            .terminator = Terminator.default,
            .bc_start = 0,
            .bc_end = 0,
            .is_loop_header = false,
            .is_sealed = false,
        };
    }

    pub fn deinit(self: *Block, allocator: std.mem.Allocator) void {
        self.predecessors.deinit(allocator);
        self.instructions.deinit(allocator);
        self.params.deinit(allocator);
    }

    pub fn addInstruction(self: *Block, allocator: std.mem.Allocator, inst: Inst) !void {
        try self.instructions.append(allocator, inst);
    }

    pub fn addPredecessor(self: *Block, allocator: std.mem.Allocator, pred: BlockId) !void {
        // Check for duplicates
        for (self.predecessors.items) |p| {
            if (p == pred) return;
        }
        try self.predecessors.append(allocator, pred);
    }

    pub fn seal(self: *Block) void {
        self.is_sealed = true;
    }
};

/// AIR function representation
pub const Function = struct {
    allocator: std.mem.Allocator,
    blocks: std.ArrayListUnmanaged(Block),
    next_value_id: u24,
    bytecode_func: *const bytecode.FunctionBytecode,
    type_feedback_ptr: ?*type_feedback.TypeFeedback,

    pub fn init(
        allocator: std.mem.Allocator,
        bc_func: *const bytecode.FunctionBytecode,
    ) Function {
        return .{
            .allocator = allocator,
            .blocks = .{},
            .next_value_id = 0,
            .bytecode_func = bc_func,
            .type_feedback_ptr = bc_func.type_feedback_ptr,
        };
    }

    pub fn deinit(self: *Function) void {
        for (self.blocks.items) |*block| {
            block.deinit(self.allocator);
        }
        self.blocks.deinit(self.allocator);
    }

    /// Allocate a new SSA value ID
    pub fn newValue(self: *Function) ValueId {
        const id = self.next_value_id;
        self.next_value_id += 1;
        return ValueId.local(id);
    }

    /// Add a new block
    pub fn addBlock(self: *Function) !BlockId {
        const id = BlockId.fromIndex(@intCast(self.blocks.items.len));
        var block = Block.init(self.allocator, id);
        try self.blocks.append(self.allocator, block);
        return id;
    }

    /// Get a block by ID
    pub fn getBlock(self: *Function, id: BlockId) ?*Block {
        const idx = id.asIndex();
        if (idx < self.blocks.items.len) {
            return &self.blocks.items[idx];
        }
        return null;
    }

    /// Get a block by ID (const version)
    pub fn getBlockConst(self: *const Function, id: BlockId) ?*const Block {
        const idx = id.asIndex();
        if (idx < self.blocks.items.len) {
            return &self.blocks.items[idx];
        }
        return null;
    }

    /// Get entry block
    pub fn entryBlock(self: *Function) ?*Block {
        return self.getBlock(BlockId.entry);
    }
};

// ============================================================================
// Unit Tests
// ============================================================================

test "ValueId encoding" {
    const local_val = ValueId.local(42);
    try std.testing.expectEqual(@as(u24, 42), local_val.index);
    try std.testing.expectEqual(ValueId.ValueKind.local, local_val.kind);
    try std.testing.expect(!local_val.isNone());

    const param_val = ValueId.param(0);
    try std.testing.expectEqual(ValueId.ValueKind.param, param_val.kind);

    const const_val = ValueId.constant(100);
    try std.testing.expectEqual(ValueId.ValueKind.constant, const_val.kind);

    try std.testing.expect(ValueId.none.isNone());
}

test "Inst creation" {
    const result = ValueId.local(0);
    const lhs = ValueId.local(1);
    const rhs = ValueId.local(2);

    const inst = Inst.binary(.add_int, result, lhs, rhs);
    try std.testing.expectEqual(InstOp.add_int, inst.op);
    try std.testing.expectEqual(result, inst.result);
    try std.testing.expectEqual(lhs, inst.operands[0]);
    try std.testing.expectEqual(rhs, inst.operands[1]);
}

test "Inst constInt" {
    const result = ValueId.local(5);
    const inst = Inst.constInt(result, 42);

    try std.testing.expectEqual(InstOp.const_int, inst.op);
    try std.testing.expectEqual(@as(i32, 42), inst.extra.int_const);
}

test "PhiData" {
    var phi_data = PhiData.empty;

    phi_data.addIncoming(BlockId.fromIndex(1), ValueId.local(10));
    phi_data.addIncoming(BlockId.fromIndex(2), ValueId.local(20));

    try std.testing.expectEqual(@as(u8, 2), phi_data.count);
    try std.testing.expectEqual(ValueId.local(10), phi_data.getValueFor(BlockId.fromIndex(1)).?);
    try std.testing.expectEqual(ValueId.local(20), phi_data.getValueFor(BlockId.fromIndex(2)).?);
    try std.testing.expect(phi_data.getValueFor(BlockId.fromIndex(3)) == null);
}

test "Terminator creation" {
    const goto_term = Terminator.goto(BlockId.fromIndex(5));
    try std.testing.expectEqual(Terminator.Kind.goto, goto_term.kind);
    try std.testing.expectEqual(BlockId.fromIndex(5), goto_term.true_target);

    const branch_term = Terminator.branch(
        ValueId.local(0),
        BlockId.fromIndex(1),
        BlockId.fromIndex(2),
    );
    try std.testing.expectEqual(Terminator.Kind.branch, branch_term.kind);
    try std.testing.expectEqual(ValueId.local(0), branch_term.condition);

    const ret_term = Terminator.ret(ValueId.local(10));
    try std.testing.expectEqual(Terminator.Kind.ret, ret_term.kind);
    try std.testing.expectEqual(ValueId.local(10), ret_term.return_value);
}

test "Function and Block" {
    const allocator = std.testing.allocator;

    // Create a dummy bytecode function
    var bc_func = bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 2,
        .local_count = 3,
        .stack_size = 8,
        .flags = .{},
        .code = &.{},
        .constants = &.{},
        .source_map = null,
    };

    var func = Function.init(allocator, &bc_func);
    defer func.deinit();

    // Add blocks
    const bb0 = try func.addBlock();
    const bb1 = try func.addBlock();

    try std.testing.expectEqual(BlockId.entry, bb0);
    try std.testing.expectEqual(BlockId.fromIndex(1), bb1);

    // Get block and add instruction
    const block = func.getBlock(bb0).?;
    const val = func.newValue();
    try block.addInstruction(allocator, Inst.constInt(val, 100));

    try std.testing.expectEqual(@as(usize, 1), block.instructions.items.len);
}
