//! Control Flow Graph (CFG) Lens for Bytecode
//!
//! Builds a CFG view over linear bytecode by identifying basic block
//! leaders and computing edges from control flow instructions.
//!
//! This is a "lens" - it doesn't copy bytecode, just provides a structured
//! view for SSA construction.

const std = @import("std");
const bytecode = @import("../bytecode.zig");
const Opcode = bytecode.Opcode;

/// Block identifier - index into the blocks array
pub const BlockId = enum(u16) {
    entry = 0,
    _,

    pub const none: BlockId = @enumFromInt(0xFFFF);

    pub fn asIndex(self: BlockId) u16 {
        return @intFromEnum(self);
    }

    pub fn fromIndex(idx: u16) BlockId {
        return @enumFromInt(idx);
    }
};

/// Edge in the CFG
pub const CfgEdge = struct {
    source: BlockId,
    target: BlockId,
    is_fallthrough: bool,
};

/// Basic block boundaries in bytecode
pub const BlockBounds = struct {
    start: u32,
    end: u32, // Exclusive - first byte of next block or end of bytecode
    is_loop_header: bool,
};

/// CFG builder - constructs CFG view from bytecode
pub const CfgBuilder = struct {
    allocator: std.mem.Allocator,
    bytecode_ptr: []const u8,

    /// Dense array: is_leader[offset] = true if block starts at offset
    is_leader: []bool,
    /// Dense array: is_loop_target[offset] = true if this is a loop back-edge target
    is_loop_target: []bool,

    /// Maps bytecode offset to block ID (only valid for leaders)
    offset_to_block: std.AutoHashMapUnmanaged(u32, BlockId),
    /// Block boundaries
    blocks: std.ArrayListUnmanaged(BlockBounds),
    /// CFG edges
    edges: std.ArrayListUnmanaged(CfgEdge),

    pub fn init(allocator: std.mem.Allocator, code: []const u8) !CfgBuilder {
        const is_leader = try allocator.alloc(bool, code.len);
        @memset(is_leader, false);

        const is_loop_target = try allocator.alloc(bool, code.len);
        @memset(is_loop_target, false);

        return .{
            .allocator = allocator,
            .bytecode_ptr = code,
            .is_leader = is_leader,
            .is_loop_target = is_loop_target,
            .offset_to_block = .{},
            .blocks = .{},
            .edges = .{},
        };
    }

    pub fn deinit(self: *CfgBuilder) void {
        self.allocator.free(self.is_leader);
        self.allocator.free(self.is_loop_target);
        self.offset_to_block.deinit(self.allocator);
        self.blocks.deinit(self.allocator);
        self.edges.deinit(self.allocator);
    }

    /// Build the CFG from bytecode
    pub fn build(self: *CfgBuilder) !void {
        try self.findLeaders();
        try self.assignBlockIds();
        try self.computeEdges();
    }

    /// Phase 1: Find all leader positions (block start points)
    ///
    /// Leader rules:
    /// 1. Offset 0 is always a leader (entry)
    /// 2. All jump targets are leaders
    /// 3. Instruction after any terminator is a leader
    pub fn findLeaders(self: *CfgBuilder) !void {
        const code = self.bytecode_ptr;
        if (code.len == 0) return;

        // Rule 1: Entry is always a leader
        self.is_leader[0] = true;

        var pc: u32 = 0;
        while (pc < code.len) {
            const op: Opcode = @enumFromInt(code[pc]);
            const info = bytecode.getOpcodeInfo(op);
            const op_offset = pc;
            const next_pc = pc + info.size;

            switch (op) {
                // Unconditional jumps - single successor
                .goto => {
                    const offset = self.readI16(pc + 1);
                    const target = self.computeTarget(op_offset, info.size, offset);
                    if (target) |t| {
                        if (t < code.len) self.is_leader[t] = true;
                    }
                    // Next instruction is a leader (dead code or jump target for something else)
                    if (next_pc < code.len) self.is_leader[next_pc] = true;
                },

                // Loop back-edge - single successor
                .loop => {
                    const offset = self.readI16(pc + 1);
                    const target = self.computeTarget(op_offset, info.size, offset);
                    if (target) |t| {
                        if (t < code.len) {
                            self.is_leader[t] = true;
                            self.is_loop_target[t] = true;
                        }
                    }
                    // Next instruction is a leader
                    if (next_pc < code.len) self.is_leader[next_pc] = true;
                },

                // Conditional branches - two successors
                .if_true, .if_false, .if_false_goto => {
                    const offset = self.readI16(pc + 1);
                    const target = self.computeTarget(op_offset, info.size, offset);
                    if (target) |t| {
                        if (t < code.len) self.is_leader[t] = true;
                    }
                    // Fallthrough is also a leader
                    if (next_pc < code.len) self.is_leader[next_pc] = true;
                },

                // For-of iteration - two successors
                .for_of_next => {
                    const offset = self.readI16(pc + 1);
                    const target = self.computeTarget(op_offset, info.size, offset);
                    if (target) |t| {
                        if (t < code.len) self.is_leader[t] = true;
                    }
                    if (next_pc < code.len) self.is_leader[next_pc] = true;
                },

                .for_of_next_put_loc => {
                    // Offset is at pc+2 (after local_idx byte)
                    const offset = self.readI16(pc + 2);
                    const target = self.computeTarget(op_offset, info.size, offset);
                    if (target) |t| {
                        if (t < code.len) self.is_leader[t] = true;
                    }
                    if (next_pc < code.len) self.is_leader[next_pc] = true;
                },

                // Terminators with no successors in this function
                .ret, .ret_undefined, .halt => {
                    // Next instruction is a leader (dead code or jump target)
                    if (next_pc < code.len) self.is_leader[next_pc] = true;
                },

                // Tail call - exits function
                .tail_call => {
                    if (next_pc < code.len) self.is_leader[next_pc] = true;
                },

                else => {},
            }

            pc = next_pc;
        }
    }

    /// Phase 2: Assign block IDs to leaders
    pub fn assignBlockIds(self: *CfgBuilder) !void {
        const code = self.bytecode_ptr;
        var block_id: u16 = 0;
        var block_start: u32 = 0;

        for (self.is_leader, 0..) |is_lead, offset| {
            if (is_lead) {
                // Close previous block if any
                if (block_id > 0 or offset > 0) {
                    if (block_start < offset) {
                        // Update end of previous block
                        if (self.blocks.items.len > 0) {
                            self.blocks.items[self.blocks.items.len - 1].end = @intCast(offset);
                        }
                    }
                }

                // Start new block
                const bid = BlockId.fromIndex(block_id);
                try self.offset_to_block.put(self.allocator, @intCast(offset), bid);
                try self.blocks.append(self.allocator, .{
                    .start = @intCast(offset),
                    .end = @intCast(code.len), // Will be updated when next block starts
                    .is_loop_header = self.is_loop_target[offset],
                });
                block_start = @intCast(offset);
                block_id += 1;
            }
        }
    }

    /// Phase 3: Compute edges between blocks
    pub fn computeEdges(self: *CfgBuilder) !void {
        const code = self.bytecode_ptr;

        for (self.blocks.items, 0..) |block, idx| {
            const source_id = BlockId.fromIndex(@intCast(idx));

            // Find the terminator instruction in this block
            var pc = block.start;
            var last_op: Opcode = .nop;
            var last_op_offset: u32 = pc;

            while (pc < block.end) {
                const op: Opcode = @enumFromInt(code[pc]);
                last_op = op;
                last_op_offset = pc;
                pc += bytecode.getOpcodeInfo(op).size;
            }

            // Add edges based on terminator type
            const info = bytecode.getOpcodeInfo(last_op);
            switch (last_op) {
                .goto => {
                    const offset = self.readI16(last_op_offset + 1);
                    if (self.computeTarget(last_op_offset, info.size, offset)) |target| {
                        if (self.offset_to_block.get(target)) |target_id| {
                            try self.edges.append(self.allocator, .{
                                .source = source_id,
                                .target = target_id,
                                .is_fallthrough = false,
                            });
                        }
                    }
                },

                .loop => {
                    const offset = self.readI16(last_op_offset + 1);
                    if (self.computeTarget(last_op_offset, info.size, offset)) |target| {
                        if (self.offset_to_block.get(target)) |target_id| {
                            try self.edges.append(self.allocator, .{
                                .source = source_id,
                                .target = target_id,
                                .is_fallthrough = false,
                            });
                        }
                    }
                },

                .if_true, .if_false, .if_false_goto => {
                    const offset = self.readI16(last_op_offset + 1);
                    // Branch target
                    if (self.computeTarget(last_op_offset, info.size, offset)) |target| {
                        if (self.offset_to_block.get(target)) |target_id| {
                            try self.edges.append(self.allocator, .{
                                .source = source_id,
                                .target = target_id,
                                .is_fallthrough = false,
                            });
                        }
                    }
                    // Fallthrough
                    const ft = last_op_offset + info.size;
                    if (self.offset_to_block.get(ft)) |ft_id| {
                        try self.edges.append(self.allocator, .{
                            .source = source_id,
                            .target = ft_id,
                            .is_fallthrough = true,
                        });
                    }
                },

                .for_of_next => {
                    const offset = self.readI16(last_op_offset + 1);
                    if (self.computeTarget(last_op_offset, info.size, offset)) |target| {
                        if (self.offset_to_block.get(target)) |target_id| {
                            try self.edges.append(self.allocator, .{
                                .source = source_id,
                                .target = target_id,
                                .is_fallthrough = false,
                            });
                        }
                    }
                    const ft = last_op_offset + info.size;
                    if (self.offset_to_block.get(ft)) |ft_id| {
                        try self.edges.append(self.allocator, .{
                            .source = source_id,
                            .target = ft_id,
                            .is_fallthrough = true,
                        });
                    }
                },

                .for_of_next_put_loc => {
                    const offset = self.readI16(last_op_offset + 2);
                    if (self.computeTarget(last_op_offset, info.size, offset)) |target| {
                        if (self.offset_to_block.get(target)) |target_id| {
                            try self.edges.append(self.allocator, .{
                                .source = source_id,
                                .target = target_id,
                                .is_fallthrough = false,
                            });
                        }
                    }
                    const ft = last_op_offset + info.size;
                    if (self.offset_to_block.get(ft)) |ft_id| {
                        try self.edges.append(self.allocator, .{
                            .source = source_id,
                            .target = ft_id,
                            .is_fallthrough = true,
                        });
                    }
                },

                .ret, .ret_undefined, .halt, .tail_call => {
                    // No successors - function exit
                },

                else => {
                    // Non-terminator at end of block means fallthrough
                    const ft = last_op_offset + info.size;
                    if (ft < code.len) {
                        if (self.offset_to_block.get(ft)) |ft_id| {
                            try self.edges.append(self.allocator, .{
                                .source = source_id,
                                .target = ft_id,
                                .is_fallthrough = true,
                            });
                        }
                    }
                },
            }
        }
    }

    /// Get the block ID at a bytecode offset (must be a leader)
    pub fn getBlockAt(self: *const CfgBuilder, offset: u32) ?BlockId {
        return self.offset_to_block.get(offset);
    }

    /// Get block bounds by ID
    pub fn getBlockBounds(self: *const CfgBuilder, id: BlockId) ?BlockBounds {
        const idx = id.asIndex();
        if (idx < self.blocks.items.len) {
            return self.blocks.items[idx];
        }
        return null;
    }

    /// Get predecessors of a block
    pub fn getPredecessors(self: *const CfgBuilder, allocator: std.mem.Allocator, block_id: BlockId) ![]BlockId {
        var preds = std.ArrayListUnmanaged(BlockId){};
        for (self.edges.items) |edge| {
            if (edge.target == block_id) {
                try preds.append(allocator, edge.source);
            }
        }
        return preds.toOwnedSlice(allocator);
    }

    /// Get successors of a block
    pub fn getSuccessors(self: *const CfgBuilder, allocator: std.mem.Allocator, block_id: BlockId) ![]BlockId {
        var succs = std.ArrayListUnmanaged(BlockId){};
        for (self.edges.items) |edge| {
            if (edge.source == block_id) {
                try succs.append(allocator, edge.target);
            }
        }
        return succs.toOwnedSlice(allocator);
    }

    /// Check if a block is a loop header
    pub fn isLoopHeader(self: *const CfgBuilder, block_id: BlockId) bool {
        if (self.getBlockBounds(block_id)) |bounds| {
            return bounds.is_loop_header;
        }
        return false;
    }

    /// Get total block count
    pub fn blockCount(self: *const CfgBuilder) usize {
        return self.blocks.items.len;
    }

    // Helper: read i16 from bytecode
    fn readI16(self: *const CfgBuilder, offset: u32) i16 {
        const code = self.bytecode_ptr;
        if (offset + 2 <= code.len) {
            return @bitCast(@as(u16, code[offset]) | (@as(u16, code[offset + 1]) << 8));
        }
        return 0;
    }

    // Helper: compute absolute target from relative offset
    fn computeTarget(self: *const CfgBuilder, op_offset: u32, op_size: u8, offset: i16) ?u32 {
        _ = self;
        const base: i32 = @intCast(op_offset + op_size);
        const target: i32 = base + offset;
        if (target >= 0) {
            return @intCast(target);
        }
        return null;
    }
};

// ============================================================================
// Unit Tests
// ============================================================================

test "CfgBuilder: simple linear code" {
    const allocator = std.testing.allocator;

    // push_1, push_2, add, ret
    const code = [_]u8{
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.push_2),
        @intFromEnum(Opcode.add),
        @intFromEnum(Opcode.ret),
    };

    var cfg = try CfgBuilder.init(allocator, &code);
    defer cfg.deinit();

    try cfg.build();

    // Should have exactly one block (entry)
    try std.testing.expectEqual(@as(usize, 1), cfg.blockCount());

    // Entry block should span entire code
    const bounds = cfg.getBlockBounds(BlockId.entry).?;
    try std.testing.expectEqual(@as(u32, 0), bounds.start);
    try std.testing.expectEqual(@as(u32, 4), bounds.end);
    try std.testing.expect(!bounds.is_loop_header);
}

test "CfgBuilder: simple if-else" {
    const allocator = std.testing.allocator;

    // 0: push_true
    // 1-3: if_false +3 (skip to offset 7 = 4 + 3)
    // 4: push_1
    // 5-7: goto +1 (skip to offset 9 = 8 + 1)
    // 8: push_2
    // 9: ret
    const code = [_]u8{
        @intFromEnum(Opcode.push_true), // 0
        @intFromEnum(Opcode.if_false), 0x03, 0x00, // 1-3: if_false +3
        @intFromEnum(Opcode.push_1), // 4
        @intFromEnum(Opcode.goto), 0x01, 0x00, // 5-7: goto +1
        @intFromEnum(Opcode.push_2), // 8
        @intFromEnum(Opcode.ret), // 9
    };

    var cfg = try CfgBuilder.init(allocator, &code);
    defer cfg.deinit();

    try cfg.build();

    // Should have 4 blocks:
    // bb0: 0-3 (entry, ends with if_false)
    // bb1: 4-7 (true branch, ends with goto)
    // bb2: 8 (false branch / merge point)
    // bb3: 9 (return)
    try std.testing.expectEqual(@as(usize, 4), cfg.blockCount());

    // Check that offset 7 (if_false target) is a leader
    try std.testing.expect(cfg.is_leader[7]);

    // Check that offset 8 (goto fallthrough) is a leader
    try std.testing.expect(cfg.is_leader[8]);
}

test "CfgBuilder: simple loop" {
    const allocator = std.testing.allocator;

    // 0: push_0         ; i = 0
    // 1: put_loc_0
    // 2: get_loc_0      ; loop header
    // 3: push_i8 10
    // 5: lt
    // 6-8: if_false +5  ; exit loop (to offset 12 = 9 + 3)
    // 9: inc           ; i++
    // 10-12: loop -8     ; back to offset 2 (13 + (-11) = 2)
    // Actually let's recalculate:
    // loop at offset 10, size 3, offset -11: target = 10 + 3 + (-11) = 2
    const code = [_]u8{
        @intFromEnum(Opcode.push_0), // 0
        @intFromEnum(Opcode.put_loc_0), // 1
        @intFromEnum(Opcode.get_loc_0), // 2 (loop header)
        @intFromEnum(Opcode.push_i8), 10, // 3-4
        @intFromEnum(Opcode.lt), // 5
        @intFromEnum(Opcode.if_false), 0x03, 0x00, // 6-8: if_false +3 -> offset 12
        @intFromEnum(Opcode.inc), // 9
        @intFromEnum(Opcode.loop), @bitCast(@as(i16, -11) & 0xFF), @bitCast((@as(i16, -11) >> 8) & 0xFF), // 10-12: loop to offset 2
        @intFromEnum(Opcode.ret), // 13
    };

    var cfg = try CfgBuilder.init(allocator, &code);
    defer cfg.deinit();

    try cfg.build();

    // Check that offset 2 is a loop header
    try std.testing.expect(cfg.is_leader[2]);
    try std.testing.expect(cfg.is_loop_target[2]);

    // Get block at offset 2 and verify it's a loop header
    const loop_block = cfg.getBlockAt(2).?;
    try std.testing.expect(cfg.isLoopHeader(loop_block));
}

test "CfgBuilder: edge computation" {
    const allocator = std.testing.allocator;

    // 0: push_true
    // 1-3: if_false +2 (to offset 6 = 4 + 2, but wait: 1+3+2=6)
    // Actually: if_false at offset 1, size 3, offset +2: target = 1 + 3 + 2 = 6
    // 4: push_1
    // 5: ret           ; end of true branch
    // 6: push_2        ; false branch
    // 7: ret
    const code = [_]u8{
        @intFromEnum(Opcode.push_true), // 0
        @intFromEnum(Opcode.if_false), 0x02, 0x00, // 1-3: if_false +2 -> offset 6
        @intFromEnum(Opcode.push_1), // 4
        @intFromEnum(Opcode.ret), // 5
        @intFromEnum(Opcode.push_2), // 6
        @intFromEnum(Opcode.ret), // 7
    };

    var cfg = try CfgBuilder.init(allocator, &code);
    defer cfg.deinit();

    try cfg.build();

    // Should have 3 blocks: bb0 (0-3), bb1 (4-5), bb2 (6-7)
    try std.testing.expectEqual(@as(usize, 3), cfg.blockCount());

    // bb0 should have 2 edges: to bb1 (fallthrough) and bb2 (branch)
    const succs = try cfg.getSuccessors(allocator, BlockId.entry);
    defer allocator.free(succs);
    try std.testing.expectEqual(@as(usize, 2), succs.len);

    // bb1 and bb2 should have no successors (both end with ret)
    const bb1_succs = try cfg.getSuccessors(allocator, BlockId.fromIndex(1));
    defer allocator.free(bb1_succs);
    try std.testing.expectEqual(@as(usize, 0), bb1_succs.len);

    const bb2_succs = try cfg.getSuccessors(allocator, BlockId.fromIndex(2));
    defer allocator.free(bb2_succs);
    try std.testing.expectEqual(@as(usize, 0), bb2_succs.len);
}

test "CfgBuilder: predecessors" {
    const allocator = std.testing.allocator;

    // Same code as edge computation test
    const code = [_]u8{
        @intFromEnum(Opcode.push_true), // 0
        @intFromEnum(Opcode.if_false), 0x02, 0x00, // 1-3
        @intFromEnum(Opcode.push_1), // 4
        @intFromEnum(Opcode.ret), // 5
        @intFromEnum(Opcode.push_2), // 6
        @intFromEnum(Opcode.ret), // 7
    };

    var cfg = try CfgBuilder.init(allocator, &code);
    defer cfg.deinit();

    try cfg.build();

    // bb0 should have no predecessors (entry)
    const bb0_preds = try cfg.getPredecessors(allocator, BlockId.entry);
    defer allocator.free(bb0_preds);
    try std.testing.expectEqual(@as(usize, 0), bb0_preds.len);

    // bb1 and bb2 should both have bb0 as predecessor
    const bb1_preds = try cfg.getPredecessors(allocator, BlockId.fromIndex(1));
    defer allocator.free(bb1_preds);
    try std.testing.expectEqual(@as(usize, 1), bb1_preds.len);
    try std.testing.expectEqual(BlockId.entry, bb1_preds[0]);

    const bb2_preds = try cfg.getPredecessors(allocator, BlockId.fromIndex(2));
    defer allocator.free(bb2_preds);
    try std.testing.expectEqual(@as(usize, 1), bb2_preds.len);
    try std.testing.expectEqual(BlockId.entry, bb2_preds[0]);
}
