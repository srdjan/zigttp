//! AIR Builder - SSA Construction from Bytecode
//!
//! Implements stack-phi SSA construction algorithm to convert
//! stack-based bytecode into CFG-based SSA form.
//!
//! Algorithm overview:
//! 1. Build CFG from bytecode using cfg.zig
//! 2. Translate each block, maintaining abstract stack
//! 3. At join points, insert phi nodes for differing definitions
//! 4. Use "sealed block" approach: defer phi operands until all predecessors known

const std = @import("std");
const cfg = @import("cfg.zig");
const types = @import("types.zig");
const bytecode = @import("../bytecode.zig");

const BlockId = types.BlockId;
const ValueId = types.ValueId;
const Inst = types.Inst;
const InstOp = types.InstOp;
const Block = types.Block;
const Function = types.Function;
const Terminator = types.Terminator;
const PhiData = types.PhiData;
const BlockParam = types.BlockParam;
const Opcode = bytecode.Opcode;

/// Error types for AIR building
pub const BuildError = error{
    OutOfMemory,
    InvalidBytecode,
    UnsupportedOpcode,
    StackUnderflow,
    StackOverflow,
    InvalidBlockId,
};

/// AIR Builder - converts bytecode to SSA form
pub const Builder = struct {
    allocator: std.mem.Allocator,
    func: Function,
    cfg_builder: cfg.CfgBuilder,

    /// Abstract operand stack during translation
    stack: std.ArrayListUnmanaged(ValueId),

    /// Local variable definitions per block: block_id -> (local_idx -> value_id)
    local_defs: std.AutoHashMapUnmanaged(LocalDefKey, ValueId),

    /// Incomplete phis: block_id -> list of (local_idx, phi_value_id)
    incomplete_phis: std.AutoHashMapUnmanaged(BlockId, std.ArrayListUnmanaged(IncompletePhi)),

    /// Current block being translated
    current_block: BlockId,

    const LocalDefKey = struct {
        block_id: u16,
        local_idx: u8,
    };

    const IncompletePhi = struct {
        local_idx: u8,
        phi_value: ValueId,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        bc_func: *const bytecode.FunctionBytecode,
    ) !Builder {
        var cfg_builder = try cfg.CfgBuilder.init(allocator, bc_func.code);
        errdefer cfg_builder.deinit();

        return .{
            .allocator = allocator,
            .func = Function.init(allocator, bc_func),
            .cfg_builder = cfg_builder,
            .stack = .{},
            .local_defs = .{},
            .incomplete_phis = .{},
            .current_block = BlockId.entry,
        };
    }

    pub fn deinit(self: *Builder) void {
        self.stack.deinit(self.allocator);
        self.local_defs.deinit(self.allocator);

        var it = self.incomplete_phis.valueIterator();
        while (it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.incomplete_phis.deinit(self.allocator);

        self.cfg_builder.deinit();
        self.func.deinit();
    }

    /// Build AIR from bytecode
    pub fn build(self: *Builder) BuildError!void {
        // Phase 1: Build CFG
        self.cfg_builder.build() catch return BuildError.InvalidBytecode;

        // Phase 2: Create AIR blocks corresponding to CFG blocks
        for (0..self.cfg_builder.blockCount()) |_| {
            _ = self.func.addBlock() catch return BuildError.OutOfMemory;
        }

        // Copy block metadata from CFG
        for (self.cfg_builder.blocks.items, 0..) |bounds, idx| {
            const block = self.func.getBlock(BlockId.fromIndex(@intCast(idx))) orelse continue;
            block.bc_start = bounds.start;
            block.bc_end = bounds.end;
            block.is_loop_header = bounds.is_loop_header;
        }

        // Phase 3: Setup predecessors
        for (self.cfg_builder.edges.items) |edge| {
            const target_block = self.func.getBlock(edge.target) orelse continue;
            target_block.addPredecessor(self.allocator, edge.source) catch return BuildError.OutOfMemory;
        }

        // Phase 4: Generate function parameters in entry block
        try self.generateParams();

        // Phase 5: Translate each block
        for (0..self.cfg_builder.blockCount()) |idx| {
            const block_id = BlockId.fromIndex(@intCast(idx));
            try self.translateBlock(block_id);
        }

        // Phase 6: Seal all blocks and resolve incomplete phis
        for (0..self.cfg_builder.blockCount()) |idx| {
            const block_id = BlockId.fromIndex(@intCast(idx));
            try self.sealBlock(block_id);
        }
    }

    /// Generate parameter instructions in entry block
    fn generateParams(self: *Builder) BuildError!void {
        const entry = self.func.getBlock(BlockId.entry) orelse return;
        const param_count = self.func.bytecode_func.arg_count;

        for (0..param_count) |i| {
            const val = self.func.newValue();
            const inst = Inst.init(.param, val)
                .withExtra(.{ .int_const = @intCast(i) });
            entry.addInstruction(self.allocator, inst) catch return BuildError.OutOfMemory;

            // Parameters are defined as locals 0..param_count-1
            self.writeLocal(BlockId.entry, @intCast(i), val) catch return BuildError.OutOfMemory;
        }
    }

    /// Translate a single basic block
    fn translateBlock(self: *Builder, block_id: BlockId) BuildError!void {
        self.current_block = block_id;
        self.stack.clearRetainingCapacity();

        const block = self.func.getBlock(block_id) orelse return BuildError.InvalidBlockId;
        const code = self.func.bytecode_func.code;

        var pc = block.bc_start;
        while (pc < block.bc_end and pc < code.len) {
            const op: Opcode = @enumFromInt(code[pc]);
            const bc_offset: u16 = @intCast(pc);
            pc += 1;

            pc = try self.translateOpcode(op, pc, bc_offset);
        }
    }

    /// Translate a single opcode
    fn translateOpcode(self: *Builder, op: Opcode, pc: u32, bc_offset: u16) BuildError!u32 {
        const code = self.func.bytecode_func.code;
        var next_pc = pc;

        switch (op) {
            // Constants
            .push_0 => try self.emitConstInt(0, bc_offset),
            .push_1 => try self.emitConstInt(1, bc_offset),
            .push_2 => try self.emitConstInt(2, bc_offset),
            .push_3 => try self.emitConstInt(3, bc_offset),

            .push_i8 => {
                const val: i8 = @bitCast(code[next_pc]);
                next_pc += 1;
                try self.emitConstInt(val, bc_offset);
            },

            .push_i16 => {
                const val: i16 = @bitCast(readU16(code, next_pc));
                next_pc += 2;
                try self.emitConstInt(val, bc_offset);
            },

            .push_true => try self.emitConst(.const_true, bc_offset),
            .push_false => try self.emitConst(.const_false, bc_offset),
            .push_null => try self.emitConst(.const_null, bc_offset),
            .push_undefined => try self.emitConst(.const_undefined, bc_offset),

            // Local variable access
            .get_loc_0 => try self.emitLoadLocal(0, bc_offset),
            .get_loc_1 => try self.emitLoadLocal(1, bc_offset),
            .get_loc_2 => try self.emitLoadLocal(2, bc_offset),
            .get_loc_3 => try self.emitLoadLocal(3, bc_offset),
            .get_loc => {
                const local_idx = code[next_pc];
                next_pc += 1;
                try self.emitLoadLocal(local_idx, bc_offset);
            },

            .put_loc_0 => try self.emitStoreLocal(0, bc_offset),
            .put_loc_1 => try self.emitStoreLocal(1, bc_offset),
            .put_loc_2 => try self.emitStoreLocal(2, bc_offset),
            .put_loc_3 => try self.emitStoreLocal(3, bc_offset),
            .put_loc => {
                const local_idx = code[next_pc];
                next_pc += 1;
                try self.emitStoreLocal(local_idx, bc_offset);
            },

            // Arithmetic (specialized to int for now)
            .add => try self.emitBinary(.add_int, bc_offset),
            .sub => try self.emitBinary(.sub_int, bc_offset),
            .mul => try self.emitBinary(.mul_int, bc_offset),
            .div => try self.emitBinary(.div_int, bc_offset),
            .mod => try self.emitBinary(.mod_int, bc_offset),
            .neg => try self.emitUnary(.neg_int, bc_offset),
            .inc => try self.emitUnary(.inc_int, bc_offset),
            .dec => try self.emitUnary(.dec_int, bc_offset),

            // Comparison
            .lt => try self.emitBinary(.lt_int, bc_offset),
            .lte => try self.emitBinary(.lte_int, bc_offset),
            .gt => try self.emitBinary(.gt_int, bc_offset),
            .gte => try self.emitBinary(.gte_int, bc_offset),
            .eq, .strict_eq => try self.emitBinary(.eq_int, bc_offset),
            .neq, .strict_neq => try self.emitBinary(.neq_int, bc_offset),

            // Bitwise
            .bit_and => try self.emitBinary(.bit_and, bc_offset),
            .bit_or => try self.emitBinary(.bit_or, bc_offset),
            .bit_xor => try self.emitBinary(.bit_xor, bc_offset),
            .bit_not => try self.emitUnary(.bit_not, bc_offset),
            .shl => try self.emitBinary(.shl, bc_offset),
            .shr => try self.emitBinary(.shr, bc_offset),
            .ushr => try self.emitBinary(.ushr, bc_offset),

            // Logical
            .not => try self.emitUnary(.not, bc_offset),

            // Stack operations
            .dup => {
                const top = try self.stackPeek();
                try self.stackPush(top);
            },
            .drop => {
                _ = try self.stackPop();
            },
            .swap => {
                const a = try self.stackPop();
                const b = try self.stackPop();
                try self.stackPush(a);
                try self.stackPush(b);
            },

            // Control flow - handled by terminator generation
            .goto => {
                const offset: i16 = @bitCast(readU16(code, next_pc));
                next_pc += 2;
                const target = computeTarget(bc_offset, 3, offset);
                const target_block = self.cfg_builder.getBlockAt(target) orelse return BuildError.InvalidBytecode;

                const block = self.func.getBlock(self.current_block) orelse return BuildError.InvalidBlockId;
                block.terminator = Terminator.goto(target_block);
            },

            .loop => {
                const offset: i16 = @bitCast(readU16(code, next_pc));
                next_pc += 2;
                const target = computeTarget(bc_offset, 3, offset);
                const target_block = self.cfg_builder.getBlockAt(target) orelse return BuildError.InvalidBytecode;

                const block = self.func.getBlock(self.current_block) orelse return BuildError.InvalidBlockId;
                block.terminator = Terminator.goto(target_block);
            },

            .if_true => {
                const offset: i16 = @bitCast(readU16(code, next_pc));
                next_pc += 2;
                const target = computeTarget(bc_offset, 3, offset);
                const fallthrough = bc_offset + 3;

                const cond = try self.stackPop();
                const true_block = self.cfg_builder.getBlockAt(target) orelse return BuildError.InvalidBytecode;
                const false_block = self.cfg_builder.getBlockAt(fallthrough) orelse return BuildError.InvalidBytecode;

                const block = self.func.getBlock(self.current_block) orelse return BuildError.InvalidBlockId;
                block.terminator = Terminator.branch(cond, true_block, false_block);
            },

            .if_false, .if_false_goto => {
                const offset: i16 = @bitCast(readU16(code, next_pc));
                next_pc += 2;
                const target = computeTarget(bc_offset, 3, offset);
                const fallthrough = bc_offset + 3;

                const cond = try self.stackPop();
                const false_block = self.cfg_builder.getBlockAt(target) orelse return BuildError.InvalidBytecode;
                const true_block = self.cfg_builder.getBlockAt(fallthrough) orelse return BuildError.InvalidBytecode;

                const block = self.func.getBlock(self.current_block) orelse return BuildError.InvalidBlockId;
                block.terminator = Terminator.branch(cond, true_block, false_block);
            },

            .ret => {
                const val = try self.stackPop();
                const block = self.func.getBlock(self.current_block) orelse return BuildError.InvalidBlockId;
                block.terminator = Terminator.ret(val);
            },

            .ret_undefined => {
                const block = self.func.getBlock(self.current_block) orelse return BuildError.InvalidBlockId;
                block.terminator = Terminator.retUndefined();
            },

            // Superinstructions
            .get_loc_add => {
                const local_idx = code[next_pc];
                next_pc += 1;
                try self.emitLoadLocal(local_idx, bc_offset);
                try self.emitBinary(.add_int, bc_offset);
            },

            .get_loc_get_loc_add => {
                const local1 = code[next_pc];
                const local2 = code[next_pc + 1];
                next_pc += 2;
                try self.emitLoadLocal(local1, bc_offset);
                try self.emitLoadLocal(local2, bc_offset);
                try self.emitBinary(.add_int, bc_offset);
            },

            // Skip unsupported opcodes for v0
            .nop, .halt => {},

            else => {
                // Skip opcode operands based on size
                const info = bytecode.getOpcodeInfo(op);
                next_pc = (bc_offset + 1) + (info.size - 1);
                // For unsupported opcodes, we'll return error later when we need the result
                // For now, push undefined to keep stack balanced if needed
                if (info.n_push > 0) {
                    for (0..info.n_push) |_| {
                        try self.emitConst(.const_undefined, bc_offset);
                    }
                }
            },
        }

        return next_pc;
    }

    // Emit helpers

    fn emitConstInt(self: *Builder, value: i32, bc_offset: u16) BuildError!void {
        const result = self.func.newValue();
        const inst = Inst.constInt(result, value).withBytecodeOffset(bc_offset);
        try self.addInst(inst);
        try self.stackPush(result);
    }

    fn emitConst(self: *Builder, op: InstOp, bc_offset: u16) BuildError!void {
        const result = self.func.newValue();
        const inst = Inst.init(op, result).withBytecodeOffset(bc_offset);
        try self.addInst(inst);
        try self.stackPush(result);
    }

    fn emitBinary(self: *Builder, op: InstOp, bc_offset: u16) BuildError!void {
        const rhs = try self.stackPop();
        const lhs = try self.stackPop();
        const result = self.func.newValue();
        const inst = Inst.binary(op, result, lhs, rhs).withBytecodeOffset(bc_offset);
        try self.addInst(inst);
        try self.stackPush(result);
    }

    fn emitUnary(self: *Builder, op: InstOp, bc_offset: u16) BuildError!void {
        const operand = try self.stackPop();
        const result = self.func.newValue();
        const inst = Inst.unary(op, result, operand).withBytecodeOffset(bc_offset);
        try self.addInst(inst);
        try self.stackPush(result);
    }

    fn emitLoadLocal(self: *Builder, local_idx: u8, bc_offset: u16) BuildError!void {
        const val = try self.readLocal(self.current_block, local_idx);
        _ = bc_offset;
        try self.stackPush(val);
    }

    fn emitStoreLocal(self: *Builder, local_idx: u8, bc_offset: u16) BuildError!void {
        const val = try self.stackPop();
        _ = bc_offset;
        self.writeLocal(self.current_block, local_idx, val) catch return BuildError.OutOfMemory;
    }

    fn addInst(self: *Builder, inst: Inst) BuildError!void {
        const block = self.func.getBlock(self.current_block) orelse return BuildError.InvalidBlockId;
        block.addInstruction(self.allocator, inst) catch return BuildError.OutOfMemory;
    }

    // Stack operations

    fn stackPush(self: *Builder, val: ValueId) BuildError!void {
        self.stack.append(self.allocator, val) catch return BuildError.OutOfMemory;
    }

    fn stackPop(self: *Builder) BuildError!ValueId {
        return self.stack.popOrNull() orelse BuildError.StackUnderflow;
    }

    fn stackPeek(self: *Builder) BuildError!ValueId {
        if (self.stack.items.len == 0) return BuildError.StackUnderflow;
        return self.stack.items[self.stack.items.len - 1];
    }

    // SSA variable access

    fn writeLocal(self: *Builder, block_id: BlockId, local_idx: u8, val: ValueId) !void {
        const key = LocalDefKey{
            .block_id = block_id.asIndex(),
            .local_idx = local_idx,
        };
        try self.local_defs.put(self.allocator, key, val);
    }

    fn readLocal(self: *Builder, block_id: BlockId, local_idx: u8) BuildError!ValueId {
        const key = LocalDefKey{
            .block_id = block_id.asIndex(),
            .local_idx = local_idx,
        };

        // Check if defined in current block
        if (self.local_defs.get(key)) |val| {
            return val;
        }

        // If block is sealed and has single predecessor, recurse
        const block = self.func.getBlock(block_id) orelse return BuildError.InvalidBlockId;

        if (block.is_sealed) {
            if (block.predecessors.items.len == 0) {
                // No definition - return undefined
                return ValueId.none;
            } else if (block.predecessors.items.len == 1) {
                // Single predecessor - recurse
                return self.readLocal(block.predecessors.items[0], local_idx);
            } else {
                // Multiple predecessors - need phi
                return try self.createPhi(block_id, local_idx);
            }
        } else {
            // Block not sealed - create incomplete phi
            return try self.createIncompletePhi(block_id, local_idx);
        }
    }

    fn createPhi(self: *Builder, block_id: BlockId, local_idx: u8) BuildError!ValueId {
        const block = self.func.getBlock(block_id) orelse return BuildError.InvalidBlockId;
        const phi_val = self.func.newValue();

        var phi_data = PhiData.empty;
        for (block.predecessors.items) |pred| {
            const incoming = try self.readLocal(pred, local_idx);
            phi_data.addIncoming(pred, incoming);
        }

        // Add phi to block params
        block.params.append(self.allocator, .{
            .value = phi_val,
            .phi = phi_data,
        }) catch return BuildError.OutOfMemory;

        // Cache the definition
        self.writeLocal(block_id, local_idx, phi_val) catch return BuildError.OutOfMemory;

        return phi_val;
    }

    fn createIncompletePhi(self: *Builder, block_id: BlockId, local_idx: u8) BuildError!ValueId {
        const phi_val = self.func.newValue();

        // Record incomplete phi
        var list = self.incomplete_phis.get(block_id) orelse std.ArrayListUnmanaged(IncompletePhi){};
        list.append(self.allocator, .{
            .local_idx = local_idx,
            .phi_value = phi_val,
        }) catch return BuildError.OutOfMemory;
        self.incomplete_phis.put(self.allocator, block_id, list) catch return BuildError.OutOfMemory;

        // Cache the definition
        self.writeLocal(block_id, local_idx, phi_val) catch return BuildError.OutOfMemory;

        return phi_val;
    }

    fn sealBlock(self: *Builder, block_id: BlockId) BuildError!void {
        const block = self.func.getBlock(block_id) orelse return BuildError.InvalidBlockId;
        if (block.is_sealed) return;

        // Resolve incomplete phis
        if (self.incomplete_phis.get(block_id)) |list| {
            for (list.items) |incomplete| {
                var phi_data = PhiData.empty;
                for (block.predecessors.items) |pred| {
                    const incoming = try self.readLocal(pred, incomplete.local_idx);
                    phi_data.addIncoming(pred, incoming);
                }

                // Add phi to block params
                block.params.append(self.allocator, .{
                    .value = incomplete.phi_value,
                    .phi = phi_data,
                }) catch return BuildError.OutOfMemory;
            }
        }

        block.seal();
    }

    /// Get the built function (transfers ownership)
    pub fn finalize(self: *Builder) Function {
        const result = self.func;
        self.func = Function.init(self.allocator, self.func.bytecode_func);
        return result;
    }
};

// Helper functions

fn readU16(code: []const u8, offset: u32) u16 {
    if (offset + 2 <= code.len) {
        return @as(u16, code[offset]) | (@as(u16, code[offset + 1]) << 8);
    }
    return 0;
}

fn computeTarget(op_offset: u16, op_size: u8, offset: i16) u32 {
    const base: i32 = @as(i32, op_offset) + op_size;
    const target: i32 = base + offset;
    return if (target >= 0) @intCast(target) else 0;
}

/// Build AIR from bytecode function
pub fn buildFromBytecode(
    allocator: std.mem.Allocator,
    bc_func: *const bytecode.FunctionBytecode,
) !Function {
    var builder = try Builder.init(allocator, bc_func);
    defer builder.deinit();

    try builder.build();
    return builder.finalize();
}

// ============================================================================
// Unit Tests
// ============================================================================

test "Builder: simple constant" {
    const allocator = std.testing.allocator;

    // push_1, ret
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

    var func = try buildFromBytecode(allocator, &bc_func);
    defer func.deinit();

    // Should have one block
    try std.testing.expectEqual(@as(usize, 1), func.blocks.items.len);

    const block = func.getBlock(BlockId.entry).?;
    // Should have const_int instruction
    try std.testing.expect(block.instructions.items.len >= 1);
    try std.testing.expectEqual(InstOp.const_int, block.instructions.items[0].op);

    // Should have ret terminator
    try std.testing.expectEqual(Terminator.Kind.ret, block.terminator.kind);
}

test "Builder: local variable" {
    const allocator = std.testing.allocator;

    // push_1, put_loc_0, get_loc_0, ret
    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.put_loc_0),
        @intFromEnum(Opcode.get_loc_0),
        @intFromEnum(Opcode.ret),
    };

    var bc_func = bytecode.FunctionBytecode{
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

    var func = try buildFromBytecode(allocator, &bc_func);
    defer func.deinit();

    // The get_loc_0 should reuse the value from put_loc_0 (SSA)
    // So the ret should return the same value as was stored
    const block = func.getBlock(BlockId.entry).?;
    try std.testing.expectEqual(Terminator.Kind.ret, block.terminator.kind);
}

test "Builder: arithmetic" {
    const allocator = std.testing.allocator;

    // push_1, push_2, add, ret
    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
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

    var func = try buildFromBytecode(allocator, &bc_func);
    defer func.deinit();

    const block = func.getBlock(BlockId.entry).?;

    // Should have: const_int 1, const_int 2, add_int
    var add_count: usize = 0;
    for (block.instructions.items) |inst| {
        if (inst.op == .add_int) add_count += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), add_count);
}

test "Builder: branch" {
    const allocator = std.testing.allocator;

    // push_true, if_false +2, push_1, ret, push_2, ret
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

    var func = try buildFromBytecode(allocator, &bc_func);
    defer func.deinit();

    // Should have 3 blocks
    try std.testing.expectEqual(@as(usize, 3), func.blocks.items.len);

    // Entry block should have branch terminator
    const entry = func.getBlock(BlockId.entry).?;
    try std.testing.expectEqual(Terminator.Kind.branch, entry.terminator.kind);
}
