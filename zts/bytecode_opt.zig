//! Bytecode Peephole Optimizer
//!
//! Post-emit optimization pass that fuses instruction sequences into superinstructions.
//! Runs after codegen and before execution to reduce dispatch overhead.
//!
//! Fusion patterns:
//!   get_loc N + add              -> get_loc_add N           (saves 1 dispatch)
//!   get_loc N + get_loc M + add  -> get_loc_get_loc_add N M (saves 2 dispatches)
//!   push_const X + call N        -> push_const_call X N     (saves 1 dispatch)
//!   get_field X + call_method N  -> get_field_call X N      (saves 1 dispatch)
//!
//! Safety: Never fuses across basic block boundaries (jump targets).

const std = @import("std");
const bytecode = @import("bytecode.zig");
const Opcode = bytecode.Opcode;

/// Statistics from optimization pass
pub const OptStats = struct {
    /// Number of get_loc + add fusions
    get_loc_add_count: u32 = 0,
    /// Number of get_loc + get_loc + add fusions
    get_loc_get_loc_add_count: u32 = 0,
    /// Number of push_const + call fusions
    push_const_call_count: u32 = 0,
    /// Number of get_field + call fusions
    get_field_call_count: u32 = 0,
    /// Total bytes saved
    bytes_saved: u32 = 0,
    /// Total dispatches saved
    dispatches_saved: u32 = 0,

    pub fn totalFusions(self: OptStats) u32 {
        return self.get_loc_add_count +
            self.get_loc_get_loc_add_count +
            self.push_const_call_count +
            self.get_field_call_count;
    }
};

/// Peephole optimizer for bytecode
pub const BytecodeOptimizer = struct {
    allocator: std.mem.Allocator,
    /// Jump targets - positions that are targets of jumps (basic block starts)
    jump_targets: std.AutoHashMapUnmanaged(u32, void),

    pub fn init(allocator: std.mem.Allocator) BytecodeOptimizer {
        return .{
            .allocator = allocator,
            .jump_targets = .{},
        };
    }

    pub fn deinit(self: *BytecodeOptimizer) void {
        self.jump_targets.deinit(self.allocator);
    }

    /// Optimize bytecode in-place, returns statistics
    pub fn optimize(self: *BytecodeOptimizer, code: []u8) !OptStats {
        var stats = OptStats{};

        // First pass: find all jump targets to avoid fusing across basic blocks
        try self.findJumpTargets(code);

        // Second pass: apply peephole optimizations
        var pos: u32 = 0;
        while (pos < code.len) {
            const fused = self.tryFuseAt(code, pos, &stats);
            if (fused > 0) {
                // Fusion happened, advance past the fused instruction
                pos += fused;
            } else {
                // No fusion, advance past current instruction
                const op: Opcode = @enumFromInt(code[pos]);
                pos += bytecode.getOpcodeInfo(op).size;
            }
        }

        return stats;
    }

    /// Find all positions that are targets of jump instructions
    fn findJumpTargets(self: *BytecodeOptimizer, code: []const u8) !void {
        self.jump_targets.clearRetainingCapacity();

        var pos: u32 = 0;
        while (pos < code.len) {
            const op: Opcode = @enumFromInt(code[pos]);
            const info = bytecode.getOpcodeInfo(op);

            // Check if this is a jump instruction
            switch (op) {
                .goto, .if_true, .if_false, .loop, .for_of_next, .for_of_next_put_loc => {
                    // Read the i16 offset
                    const offset = self.readI16(code, pos + 1);
                    // Calculate absolute target position
                    const target_pos: i32 = @as(i32, @intCast(pos)) + info.size + offset;
                    if (target_pos >= 0 and target_pos < @as(i32, @intCast(code.len))) {
                        try self.jump_targets.put(self.allocator, @intCast(target_pos), {});
                    }
                },
                else => {},
            }

            pos += info.size;
        }
    }

    /// Check if position is a jump target (start of basic block)
    fn isJumpTarget(self: *const BytecodeOptimizer, pos: u32) bool {
        return self.jump_targets.contains(pos);
    }

    /// Try to fuse instructions starting at pos
    /// Returns the size of the fused instruction if fusion happened, 0 otherwise
    fn tryFuseAt(self: *BytecodeOptimizer, code: []u8, pos: u32, stats: *OptStats) u32 {
        if (pos >= code.len) return 0;

        // Don't fuse if current instruction is a jump target
        if (self.isJumpTarget(pos)) return 0;

        const op1: Opcode = @enumFromInt(code[pos]);
        const info1 = bytecode.getOpcodeInfo(op1);
        const next_pos = pos + info1.size;

        if (next_pos >= code.len) return 0;

        // Don't fuse if next instruction is a jump target
        if (self.isJumpTarget(next_pos)) return 0;

        const op2: Opcode = @enumFromInt(code[next_pos]);
        const info2 = bytecode.getOpcodeInfo(op2);

        // Pattern: get_loc N + add -> get_loc_add N
        // Original: get_loc(2) + add(1) = 3 bytes
        // Fused: get_loc_add(2) = 2 bytes, NOP 1 byte
        if (op1 == .get_loc and op2 == .add) {
            const local_idx = code[pos + 1];
            code[pos] = @intFromEnum(Opcode.get_loc_add);
            code[pos + 1] = local_idx;
            // NOP out the add instruction (position 2)
            code[pos + 2] = @intFromEnum(Opcode.nop);
            stats.get_loc_add_count += 1;
            stats.dispatches_saved += 1;
            return 2; // Size of get_loc_add
        }

        // Pattern: get_loc_0/1/2/3 + add -> get_loc_add 0/1/2/3
        if ((op1 == .get_loc_0 or op1 == .get_loc_1 or op1 == .get_loc_2 or op1 == .get_loc_3) and op2 == .add) {
            const local_idx: u8 = switch (op1) {
                .get_loc_0 => 0,
                .get_loc_1 => 1,
                .get_loc_2 => 2,
                .get_loc_3 => 3,
                else => unreachable,
            };
            // Replace get_loc_X (1 byte) with get_loc_add (2 bytes)
            // This expands - need to handle differently
            // For now, only fuse when there's a following nop we can absorb
            // Skip this optimization for now since it changes instruction size
            _ = local_idx;
        }

        // Pattern: get_loc N + get_loc M + add -> get_loc_get_loc_add N M
        // Original: get_loc(2) + get_loc(2) + add(1) = 5 bytes
        // Fused: get_loc_get_loc_add(3) = 3 bytes, NOP 2 bytes
        if (op1 == .get_loc and op2 == .get_loc) {
            const third_pos = next_pos + info2.size;
            if (third_pos < code.len and !self.isJumpTarget(third_pos)) {
                const op3: Opcode = @enumFromInt(code[third_pos]);
                if (op3 == .add) {
                    const local_n = code[pos + 1];
                    const local_m = code[next_pos + 1];
                    code[pos] = @intFromEnum(Opcode.get_loc_get_loc_add);
                    code[pos + 1] = local_n;
                    code[pos + 2] = local_m;
                    // NOP out remaining bytes (positions 3 and 4)
                    code[pos + 3] = @intFromEnum(Opcode.nop);
                    code[pos + 4] = @intFromEnum(Opcode.nop);
                    stats.get_loc_get_loc_add_count += 1;
                    stats.dispatches_saved += 2;
                    stats.bytes_saved += 2;
                    return 3; // Size of get_loc_get_loc_add
                }
            }
        }

        // Pattern: push_const X + call N -> push_const_call X N
        // Original: push_const(3) + call(2) = 5 bytes
        // Fused: push_const_call(4) = 4 bytes, NOP 1 byte
        if (op1 == .push_const and op2 == .call) {
            const const_idx = self.readU16(code, pos + 1);
            const argc = code[next_pos + 1];
            code[pos] = @intFromEnum(Opcode.push_const_call);
            self.writeU16(code, pos + 1, const_idx);
            code[pos + 3] = argc;
            // NOP out remaining byte (position 4)
            code[pos + 4] = @intFromEnum(Opcode.nop);
            stats.push_const_call_count += 1;
            stats.dispatches_saved += 1;
            stats.bytes_saved += 1;
            return 4; // Size of push_const_call
        }

        // Pattern: get_field X + call_method N -> get_field_call X N
        // Original: get_field(3) + call_method(2) = 5 bytes
        // Fused: get_field_call(4) = 4 bytes, NOP 1 byte
        if (op1 == .get_field and op2 == .call_method) {
            const atom_idx = self.readU16(code, pos + 1);
            const argc = code[next_pos + 1];
            code[pos] = @intFromEnum(Opcode.get_field_call);
            self.writeU16(code, pos + 1, atom_idx);
            code[pos + 3] = argc;
            // NOP out remaining byte (position 4)
            code[pos + 4] = @intFromEnum(Opcode.nop);
            stats.get_field_call_count += 1;
            stats.dispatches_saved += 1;
            stats.bytes_saved += 1;
            return 4; // Size of get_field_call
        }

        // Pattern: get_field_ic X Y + call_method N -> get_field_call X N
        // Original: get_field_ic(5) + call_method(2) = 7 bytes
        // Fused: get_field_call(4) = 4 bytes, NOP 3 bytes
        // Note: Drops the IC index, but method dispatch typically doesn't benefit from IC anyway
        if (op1 == .get_field_ic and op2 == .call_method) {
            const atom_idx = self.readU16(code, pos + 1);
            const argc = code[next_pos + 1];
            code[pos] = @intFromEnum(Opcode.get_field_call);
            self.writeU16(code, pos + 1, atom_idx);
            code[pos + 3] = argc;
            // NOP out remaining bytes (positions 4, 5, 6)
            code[pos + 4] = @intFromEnum(Opcode.nop);
            code[pos + 5] = @intFromEnum(Opcode.nop);
            code[pos + 6] = @intFromEnum(Opcode.nop);
            stats.get_field_call_count += 1;
            stats.dispatches_saved += 1;
            stats.bytes_saved += 3;
            return 4;
        }

        return 0;
    }

    fn readU16(self: *const BytecodeOptimizer, code: []const u8, pos: u32) u16 {
        _ = self;
        return @as(u16, code[pos]) | (@as(u16, code[pos + 1]) << 8);
    }

    fn readI16(self: *const BytecodeOptimizer, code: []const u8, pos: u32) i16 {
        return @bitCast(self.readU16(code, pos));
    }

    fn writeU16(self: *const BytecodeOptimizer, code: []u8, pos: u32, val: u16) void {
        _ = self;
        code[pos] = @truncate(val);
        code[pos + 1] = @truncate(val >> 8);
    }

    /// Remove NOPs from bytecode (compaction pass)
    /// Returns new length after compaction
    /// Also updates jump offsets to account for removed NOPs
    pub fn compact(self: *BytecodeOptimizer, code: []u8) !usize {
        // Build offset mapping: old position -> new position
        var offset_map = std.AutoHashMapUnmanaged(u32, u32){};
        defer offset_map.deinit(self.allocator);

        // First pass: calculate new positions
        var read_pos: u32 = 0;
        var write_pos: u32 = 0;
        while (read_pos < code.len) {
            try offset_map.put(self.allocator, read_pos, write_pos);
            const op: Opcode = @enumFromInt(code[read_pos]);
            const size = bytecode.getOpcodeInfo(op).size;
            if (op != .nop) {
                write_pos += size;
            }
            read_pos += size;
        }
        // Map end position too
        try offset_map.put(self.allocator, read_pos, write_pos);

        // Second pass: update jump offsets
        read_pos = 0;
        while (read_pos < code.len) {
            const op: Opcode = @enumFromInt(code[read_pos]);
            const info = bytecode.getOpcodeInfo(op);

            switch (op) {
                .goto, .if_true, .if_false, .loop, .for_of_next => {
                    const old_offset = self.readI16(code, read_pos + 1);
                    const old_target: i32 = @as(i32, @intCast(read_pos)) + info.size + old_offset;

                    if (old_target >= 0) {
                        const new_source = offset_map.get(read_pos) orelse read_pos;
                        const new_target = offset_map.get(@intCast(old_target)) orelse @as(u32, @intCast(old_target));
                        const new_offset: i16 = @intCast(@as(i32, @intCast(new_target)) - @as(i32, @intCast(new_source)) - info.size);
                        self.writeU16(code, read_pos + 1, @bitCast(new_offset));
                    }
                },
                .for_of_next_put_loc => {
                    // Offset is at position +2 (after local_idx byte)
                    const old_offset = self.readI16(code, read_pos + 2);
                    const old_target: i32 = @as(i32, @intCast(read_pos)) + info.size + old_offset;

                    if (old_target >= 0) {
                        const new_source = offset_map.get(read_pos) orelse read_pos;
                        const new_target = offset_map.get(@intCast(old_target)) orelse @as(u32, @intCast(old_target));
                        const new_offset: i16 = @intCast(@as(i32, @intCast(new_target)) - @as(i32, @intCast(new_source)) - info.size);
                        self.writeU16(code, read_pos + 2, @bitCast(new_offset));
                    }
                },
                else => {},
            }

            read_pos += info.size;
        }

        // Third pass: compact by removing NOPs
        read_pos = 0;
        write_pos = 0;
        while (read_pos < code.len) {
            const op: Opcode = @enumFromInt(code[read_pos]);
            const size = bytecode.getOpcodeInfo(op).size;
            if (op != .nop) {
                if (write_pos != read_pos) {
                    // Move instruction
                    var i: u32 = 0;
                    while (i < size) : (i += 1) {
                        code[write_pos + i] = code[read_pos + i];
                    }
                }
                write_pos += size;
            }
            read_pos += size;
        }

        return write_pos;
    }
};

/// Convenience function: optimize and compact bytecode
/// Returns new length
pub fn optimizeBytecode(allocator: std.mem.Allocator, code: []u8) !struct { len: usize, stats: OptStats } {
    var optimizer = BytecodeOptimizer.init(allocator);
    defer optimizer.deinit();

    const stats = try optimizer.optimize(code);
    const new_len = try optimizer.compact(code);

    return .{ .len = new_len, .stats = stats };
}

// ============================================================================
// Tests
// ============================================================================

test "BytecodeOptimizer: get_loc + add fusion" {
    const allocator = std.testing.allocator;

    // get_loc 5, add -> get_loc_add 5
    var code = [_]u8{
        @intFromEnum(Opcode.get_loc), 5, // get_loc 5
        @intFromEnum(Opcode.add), // add
        @intFromEnum(Opcode.ret), // ret
    };

    var optimizer = BytecodeOptimizer.init(allocator);
    defer optimizer.deinit();

    const stats = try optimizer.optimize(&code);

    try std.testing.expectEqual(@as(u32, 1), stats.get_loc_add_count);
    try std.testing.expectEqual(@as(u32, 1), stats.dispatches_saved);

    // Verify bytecode was modified
    try std.testing.expectEqual(@intFromEnum(Opcode.get_loc_add), code[0]);
    try std.testing.expectEqual(@as(u8, 5), code[1]);
    try std.testing.expectEqual(@intFromEnum(Opcode.nop), code[2]);
}

test "BytecodeOptimizer: get_loc + get_loc + add fusion" {
    const allocator = std.testing.allocator;

    // get_loc 2, get_loc 3, add -> get_loc_get_loc_add 2 3
    var code = [_]u8{
        @intFromEnum(Opcode.get_loc), 2, // get_loc 2
        @intFromEnum(Opcode.get_loc), 3, // get_loc 3
        @intFromEnum(Opcode.add), // add
        @intFromEnum(Opcode.ret), // ret
    };

    var optimizer = BytecodeOptimizer.init(allocator);
    defer optimizer.deinit();

    const stats = try optimizer.optimize(&code);

    try std.testing.expectEqual(@as(u32, 1), stats.get_loc_get_loc_add_count);
    try std.testing.expectEqual(@as(u32, 2), stats.dispatches_saved);

    // Verify bytecode
    try std.testing.expectEqual(@intFromEnum(Opcode.get_loc_get_loc_add), code[0]);
    try std.testing.expectEqual(@as(u8, 2), code[1]);
    try std.testing.expectEqual(@as(u8, 3), code[2]);
}

test "BytecodeOptimizer: push_const + call fusion" {
    const allocator = std.testing.allocator;

    // push_const 0x0102, call 3 -> push_const_call 0x0102 3
    var code = [_]u8{
        @intFromEnum(Opcode.push_const), 0x02, 0x01, // push_const 258 (little-endian)
        @intFromEnum(Opcode.call), 3, // call with 3 args
        @intFromEnum(Opcode.ret), // ret
    };

    var optimizer = BytecodeOptimizer.init(allocator);
    defer optimizer.deinit();

    const stats = try optimizer.optimize(&code);

    try std.testing.expectEqual(@as(u32, 1), stats.push_const_call_count);

    // Verify bytecode
    try std.testing.expectEqual(@intFromEnum(Opcode.push_const_call), code[0]);
    try std.testing.expectEqual(@as(u8, 0x02), code[1]); // Low byte of const idx
    try std.testing.expectEqual(@as(u8, 0x01), code[2]); // High byte of const idx
    try std.testing.expectEqual(@as(u8, 3), code[3]); // argc
}

test "BytecodeOptimizer: no fusion across jump target" {
    const allocator = std.testing.allocator;

    // Jump targets should prevent fusion
    // goto +0 (to the get_loc), get_loc 5, add
    var code = [_]u8{
        @intFromEnum(Opcode.goto), 0x00, 0x00, // goto +0 (jump to next instruction)
        @intFromEnum(Opcode.get_loc), 5, // get_loc 5 - this is a jump target
        @intFromEnum(Opcode.add), // add
        @intFromEnum(Opcode.ret),
    };

    var optimizer = BytecodeOptimizer.init(allocator);
    defer optimizer.deinit();

    const stats = try optimizer.optimize(&code);

    // No fusion should happen because get_loc is a jump target
    try std.testing.expectEqual(@as(u32, 0), stats.get_loc_add_count);

    // Verify bytecode unchanged (except for our pattern detection, but get_loc should stay)
    try std.testing.expectEqual(@intFromEnum(Opcode.get_loc), code[3]);
}

test "BytecodeOptimizer: compact removes NOPs" {
    const allocator = std.testing.allocator;

    var code = [_]u8{
        @intFromEnum(Opcode.push_0), // push_0
        @intFromEnum(Opcode.nop), // nop (to be removed)
        @intFromEnum(Opcode.nop), // nop (to be removed)
        @intFromEnum(Opcode.push_1), // push_1
        @intFromEnum(Opcode.ret), // ret
    };

    var optimizer = BytecodeOptimizer.init(allocator);
    defer optimizer.deinit();

    const new_len = try optimizer.compact(&code);

    try std.testing.expectEqual(@as(usize, 3), new_len);
    try std.testing.expectEqual(@intFromEnum(Opcode.push_0), code[0]);
    try std.testing.expectEqual(@intFromEnum(Opcode.push_1), code[1]);
    try std.testing.expectEqual(@intFromEnum(Opcode.ret), code[2]);
}

test "BytecodeOptimizer: compact updates jump offsets" {
    const allocator = std.testing.allocator;

    // if_false +3 (skip over nop, nop, push_1 to ret)
    // nop, nop, push_1, ret
    // After compaction: if_false +1 (skip push_1 to ret)
    var code = [_]u8{
        @intFromEnum(Opcode.push_true),
        @intFromEnum(Opcode.if_false), 0x03, 0x00, // if_false, offset +3
        @intFromEnum(Opcode.nop),
        @intFromEnum(Opcode.nop),
        @intFromEnum(Opcode.push_1),
        @intFromEnum(Opcode.ret),
    };

    var optimizer = BytecodeOptimizer.init(allocator);
    defer optimizer.deinit();

    const new_len = try optimizer.compact(&code);

    try std.testing.expectEqual(@as(usize, 6), new_len);
    // After compaction: push_true, if_false +1, push_1, ret
    try std.testing.expectEqual(@intFromEnum(Opcode.push_true), code[0]);
    try std.testing.expectEqual(@intFromEnum(Opcode.if_false), code[1]);
    // Offset should be updated: originally +3 (over nop, nop, push_1)
    // Now should be +1 (over push_1)
    const new_offset: i16 = @bitCast(@as(u16, code[2]) | (@as(u16, code[3]) << 8));
    try std.testing.expectEqual(@as(i16, 1), new_offset);
}

test "BytecodeOptimizer: full optimize and compact" {
    const allocator = std.testing.allocator;

    // get_loc 1, get_loc 2, add, ret
    // Should become: get_loc_get_loc_add 1 2, ret (with NOPs compacted)
    var code = [_]u8{
        @intFromEnum(Opcode.get_loc), 1,
        @intFromEnum(Opcode.get_loc), 2,
        @intFromEnum(Opcode.add),
        @intFromEnum(Opcode.ret),
    };

    const result = try optimizeBytecode(allocator, &code);

    try std.testing.expectEqual(@as(u32, 1), result.stats.get_loc_get_loc_add_count);
    // After compaction: get_loc_get_loc_add 1 2 (3 bytes) + ret (1 byte) = 4 bytes
    try std.testing.expectEqual(@as(usize, 4), result.len);
    try std.testing.expectEqual(@intFromEnum(Opcode.get_loc_get_loc_add), code[0]);
    try std.testing.expectEqual(@as(u8, 1), code[1]);
    try std.testing.expectEqual(@as(u8, 2), code[2]);
    try std.testing.expectEqual(@intFromEnum(Opcode.ret), code[3]);
}
