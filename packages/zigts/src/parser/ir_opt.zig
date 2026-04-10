//! IR Optimization Pass
//!
//! Performs constant folding and simplification on the IR before codegen.
//! Runs between parsing and bytecode generation.
//!
//! Optimizations:
//!   - Boolean constant folding: !true -> false, !false -> true
//!   - Float arithmetic: 1.5 + 2.5 -> 4.0
//!   - String concatenation: "hello" + "world" -> "helloworld"
//!   - Short-circuit simplification: true && x -> x, false || x -> x
//!
//! Design: Single-pass O(n) traversal for cold-start friendliness.
//! Uses replacement map instead of rewriting IR (append-only structure).

const std = @import("std");
const ir = @import("ir.zig");

const NodeTag = ir.NodeTag;
const NodeIndex = ir.NodeIndex;
const null_node = ir.null_node;
const BinaryOp = ir.BinaryOp;
const UnaryOp = ir.UnaryOp;
const IRStore = ir.IRStore;
const ConstantPool = ir.ConstantPool;
const SourceLocation = ir.SourceLocation;
const DataPayload = ir.DataPayload;

/// Statistics from IR optimization pass
pub const IROptStats = struct {
    bool_folds: u32 = 0,
    float_folds: u32 = 0,
    string_concats: u32 = 0,
    short_circuit_simplifications: u32 = 0,

    pub fn totalOptimizations(self: IROptStats) u32 {
        return self.bool_folds + self.float_folds +
            self.string_concats + self.short_circuit_simplifications;
    }
};

/// IR Optimizer
pub const IROptimizer = struct {
    ir_store: *IRStore,
    constants: *ConstantPool,
    allocator: std.mem.Allocator,
    stats: IROptStats,

    // Replacement map: original node index -> replacement node index
    // If an entry exists, use the replacement when visiting that node
    replacements: std.AutoHashMapUnmanaged(NodeIndex, NodeIndex),

    pub fn init(
        allocator: std.mem.Allocator,
        ir_store: *IRStore,
        constants: *ConstantPool,
    ) IROptimizer {
        return .{
            .ir_store = ir_store,
            .constants = constants,
            .allocator = allocator,
            .stats = .{},
            .replacements = .{},
        };
    }

    pub fn deinit(self: *IROptimizer) void {
        self.replacements.deinit(self.allocator);
    }

    /// Optimize the IR tree starting from root
    /// Returns statistics about optimizations performed
    pub fn optimize(self: *IROptimizer, root: NodeIndex) !IROptStats {
        if (root == null_node) return self.stats;

        // Single pass: visit each node and potentially replace with constant
        _ = try self.visitNode(root);

        return self.stats;
    }

    /// Get the replacement for a node, or the node itself if no replacement
    pub fn getReplacement(self: *const IROptimizer, idx: NodeIndex) NodeIndex {
        return self.replacements.get(idx) orelse idx;
    }

    /// Error type for optimization operations
    pub const OptError = std.mem.Allocator.Error;

    /// Visit a node and return the optimized result (may be same or different node)
    fn visitNode(self: *IROptimizer, idx: NodeIndex) OptError!NodeIndex {
        if (idx == null_node) return null_node;

        // Check if already have a replacement
        if (self.replacements.get(idx)) |replacement| {
            return replacement;
        }

        const tag = self.ir_store.getTag(idx);

        return switch (tag) {
            .unary_op => try self.optimizeUnary(idx),
            .binary_op => try self.optimizeBinary(idx),
            else => idx, // No optimization for other node types
        };
    }

    /// Optimize unary expressions
    fn optimizeUnary(self: *IROptimizer, idx: NodeIndex) OptError!NodeIndex {
        const data = self.ir_store.getData(idx);
        const op: UnaryOp = @enumFromInt(@as(u4, @truncate(data.a)));
        const operand_idx: NodeIndex = data.b;

        // First optimize the operand
        const opt_operand = try self.visitNode(operand_idx);
        const operand_tag = self.ir_store.getTag(opt_operand);

        // Boolean NOT folding: !true -> false, !false -> true
        if (op == .not and operand_tag == .lit_bool) {
            const bool_val = self.ir_store.getData(opt_operand).a != 0;
            const loc = self.ir_store.getLoc(idx);
            const result = try self.ir_store.addNode(.lit_bool, loc, .{ .a = if (bool_val) 0 else 1, .b = 0 });
            try self.replacements.put(self.allocator, idx, result);
            self.stats.bool_folds += 1;
            return result;
        }

        // Double NOT folding on literal: !!true -> true, !!false -> false
        if (op == .not and operand_tag == .unary_op) {
            const inner_data = self.ir_store.getData(opt_operand);
            const inner_op: UnaryOp = @enumFromInt(@as(u4, @truncate(inner_data.a)));
            if (inner_op == .not) {
                const inner_operand: NodeIndex = inner_data.b;
                const inner_tag = self.ir_store.getTag(inner_operand);
                if (inner_tag == .lit_bool) {
                    // !!bool_literal -> bool_literal
                    try self.replacements.put(self.allocator, idx, inner_operand);
                    self.stats.bool_folds += 1;
                    return inner_operand;
                }
            }
        }

        // Negation of float constant: -1.5 -> -1.5 (as new constant)
        if (op == .neg and operand_tag == .lit_float) {
            const float_idx: u16 = @truncate(self.ir_store.getData(opt_operand).a);
            if (self.constants.getFloat(float_idx)) |f| {
                const neg_idx = try self.constants.addFloat(-f);
                const loc = self.ir_store.getLoc(idx);
                const result = try self.ir_store.addLitFloat(loc, neg_idx);
                try self.replacements.put(self.allocator, idx, result);
                self.stats.float_folds += 1;
                return result;
            }
        }

        // Negation of int constant: -5 -> -5 (as new constant)
        if (op == .neg and operand_tag == .lit_int) {
            const int_val = self.ir_store.getData(opt_operand).toInt();
            const loc = self.ir_store.getLoc(idx);
            const result = try self.ir_store.addLitInt(loc, -int_val);
            try self.replacements.put(self.allocator, idx, result);
            self.stats.float_folds += 1; // Count with float folds for simplicity
            return result;
        }

        return idx;
    }

    /// Optimize binary expressions
    fn optimizeBinary(self: *IROptimizer, idx: NodeIndex) OptError!NodeIndex {
        const data = self.ir_store.getData(idx);
        const binary = data.toBinary();
        const op = binary.op;

        // First optimize operands
        const opt_left = try self.visitNode(binary.left);
        const opt_right = try self.visitNode(binary.right);

        const left_tag = self.ir_store.getTag(opt_left);
        const right_tag = self.ir_store.getTag(opt_right);
        const loc = self.ir_store.getLoc(idx);

        // Integer arithmetic: lit_int op lit_int -> lit_int
        if (left_tag == .lit_int and right_tag == .lit_int) {
            const left_val = self.ir_store.getData(opt_left).toInt();
            const right_val = self.ir_store.getData(opt_right).toInt();

            const result_val: ?i32 = switch (op) {
                .add => if (@addWithOverflow(left_val, right_val)[1] == 0) left_val + right_val else null,
                .sub => if (@subWithOverflow(left_val, right_val)[1] == 0) left_val - right_val else null,
                .mul => if (@mulWithOverflow(left_val, right_val)[1] == 0) left_val * right_val else null,
                .div => if (right_val != 0) @divTrunc(left_val, right_val) else null,
                .mod => if (right_val != 0) @mod(left_val, right_val) else null,
                .bit_and => left_val & right_val,
                .bit_or => left_val | right_val,
                .bit_xor => left_val ^ right_val,
                .shl => blk: {
                    const shift: u5 = @intCast(@as(u32, @bitCast(right_val)) & 0x1F);
                    break :blk @shlWithOverflow(left_val, shift)[0];
                },
                .shr => blk: {
                    const shift: u5 = @intCast(@as(u32, @bitCast(right_val)) & 0x1F);
                    break :blk left_val >> shift;
                },
                else => null,
            };

            if (result_val) |val| {
                const result = try self.ir_store.addLitInt(loc, val);
                try self.replacements.put(self.allocator, idx, result);
                self.stats.float_folds += 1;
                return result;
            }
        }

        // Float arithmetic: lit_float op lit_float -> lit_float
        if (left_tag == .lit_float and right_tag == .lit_float) {
            const left_idx: u16 = @truncate(self.ir_store.getData(opt_left).a);
            const right_idx: u16 = @truncate(self.ir_store.getData(opt_right).a);

            if (self.constants.getFloat(left_idx)) |left_val| {
                if (self.constants.getFloat(right_idx)) |right_val| {
                    const result_val: ?f64 = switch (op) {
                        .add => left_val + right_val,
                        .sub => left_val - right_val,
                        .mul => left_val * right_val,
                        .div => if (right_val != 0) left_val / right_val else null,
                        .pow => std.math.pow(f64, left_val, right_val),
                        else => null,
                    };

                    if (result_val) |val| {
                        const result_idx = try self.constants.addFloat(val);
                        const result = try self.ir_store.addLitFloat(loc, result_idx);
                        try self.replacements.put(self.allocator, idx, result);
                        self.stats.float_folds += 1;
                        return result;
                    }
                }
            }
        }

        // String concatenation: lit_string + lit_string -> lit_string
        if (op == .add and left_tag == .lit_string and right_tag == .lit_string) {
            const left_idx: u16 = @truncate(self.ir_store.getData(opt_left).a);
            const right_idx: u16 = @truncate(self.ir_store.getData(opt_right).a);

            if (self.constants.getString(left_idx)) |left_str| {
                if (self.constants.getString(right_idx)) |right_str| {
                    // Concatenate strings - ConstantPool stores slices without copying,
                    // so the concatenated string is owned by the allocator and lives
                    // for the duration of compilation (same as other constant strings)
                    const concat = try std.mem.concat(self.allocator, u8, &.{ left_str, right_str });
                    // Note: Do NOT free concat - ConstantPool.addString stores the slice directly

                    const result_idx = try self.constants.addString(concat);
                    const result = try self.ir_store.addLitString(loc, result_idx);
                    try self.replacements.put(self.allocator, idx, result);
                    self.stats.string_concats += 1;
                    return result;
                }
            }
        }

        // Short-circuit simplification for &&
        if (op == .and_op and left_tag == .lit_bool) {
            const left_val = self.ir_store.getData(opt_left).a != 0;
            if (left_val) {
                // true && x -> x
                try self.replacements.put(self.allocator, idx, opt_right);
                self.stats.short_circuit_simplifications += 1;
                return opt_right;
            } else {
                // false && x -> false
                try self.replacements.put(self.allocator, idx, opt_left);
                self.stats.short_circuit_simplifications += 1;
                return opt_left;
            }
        }

        // Short-circuit simplification for ||
        if (op == .or_op and left_tag == .lit_bool) {
            const left_val = self.ir_store.getData(opt_left).a != 0;
            if (!left_val) {
                // false || x -> x
                try self.replacements.put(self.allocator, idx, opt_right);
                self.stats.short_circuit_simplifications += 1;
                return opt_right;
            } else {
                // true || x -> true
                try self.replacements.put(self.allocator, idx, opt_left);
                self.stats.short_circuit_simplifications += 1;
                return opt_left;
            }
        }

        // Comparison folding for integers
        if (left_tag == .lit_int and right_tag == .lit_int) {
            const left_val = self.ir_store.getData(opt_left).toInt();
            const right_val = self.ir_store.getData(opt_right).toInt();

            const result_val: ?bool = switch (op) {
                .eq, .strict_eq => left_val == right_val,
                .neq, .strict_neq => left_val != right_val,
                .lt => left_val < right_val,
                .lte => left_val <= right_val,
                .gt => left_val > right_val,
                .gte => left_val >= right_val,
                else => null,
            };

            if (result_val) |val| {
                const result = try self.ir_store.addNode(.lit_bool, loc, .{ .a = if (val) 1 else 0, .b = 0 });
                try self.replacements.put(self.allocator, idx, result);
                self.stats.bool_folds += 1;
                return result;
            }
        }

        return idx;
    }
};

// ============================================================================
// Convenience function
// ============================================================================

/// Optimize IR, returns statistics
pub fn optimizeIR(
    allocator: std.mem.Allocator,
    ir_store: *IRStore,
    constants: *ConstantPool,
    root: NodeIndex,
) !IROptStats {
    var optimizer = IROptimizer.init(allocator, ir_store, constants);
    defer optimizer.deinit();
    return optimizer.optimize(root);
}

// ============================================================================
// Tests
// ============================================================================

test "IROptimizer: boolean NOT folding" {
    const allocator = std.testing.allocator;
    var store = IRStore.init(allocator);
    defer store.deinit();
    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create !true -> should become false
    const true_node = try store.addNode(.lit_bool, loc, .{ .a = 1, .b = 0 });
    const not_node = try store.addUnary(loc, .not, true_node);

    var optimizer = IROptimizer.init(allocator, &store, &constants);
    defer optimizer.deinit();

    const result = try optimizer.visitNode(not_node);

    try std.testing.expectEqual(@as(u32, 1), optimizer.stats.bool_folds);
    // Result should be a lit_bool with value false
    try std.testing.expectEqual(NodeTag.lit_bool, store.getTag(result));
    try std.testing.expectEqual(@as(u32, 0), store.getData(result).a);
}

test "IROptimizer: boolean NOT false -> true" {
    const allocator = std.testing.allocator;
    var store = IRStore.init(allocator);
    defer store.deinit();
    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create !false -> should become true
    const false_node = try store.addNode(.lit_bool, loc, .{ .a = 0, .b = 0 });
    const not_node = try store.addUnary(loc, .not, false_node);

    var optimizer = IROptimizer.init(allocator, &store, &constants);
    defer optimizer.deinit();

    const result = try optimizer.visitNode(not_node);

    try std.testing.expectEqual(@as(u32, 1), optimizer.stats.bool_folds);
    try std.testing.expectEqual(NodeTag.lit_bool, store.getTag(result));
    try std.testing.expectEqual(@as(u32, 1), store.getData(result).a);
}

test "IROptimizer: integer arithmetic folding" {
    const allocator = std.testing.allocator;
    var store = IRStore.init(allocator);
    defer store.deinit();
    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create 10 + 32 -> should become 42
    const left_node = try store.addLitInt(loc, 10);
    const right_node = try store.addLitInt(loc, 32);
    const add_node = try store.addBinary(loc, .add, left_node, right_node);

    var optimizer = IROptimizer.init(allocator, &store, &constants);
    defer optimizer.deinit();

    const result = try optimizer.visitNode(add_node);

    try std.testing.expectEqual(@as(u32, 1), optimizer.stats.float_folds);
    try std.testing.expectEqual(NodeTag.lit_int, store.getTag(result));
    try std.testing.expectEqual(@as(i32, 42), store.getData(result).toInt());
}

test "IROptimizer: float arithmetic folding" {
    const allocator = std.testing.allocator;
    var store = IRStore.init(allocator);
    defer store.deinit();
    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create 1.5 + 2.5 -> should become 4.0
    const left_idx = try constants.addFloat(1.5);
    const right_idx = try constants.addFloat(2.5);
    const left_node = try store.addLitFloat(loc, left_idx);
    const right_node = try store.addLitFloat(loc, right_idx);
    const add_node = try store.addBinary(loc, .add, left_node, right_node);

    var optimizer = IROptimizer.init(allocator, &store, &constants);
    defer optimizer.deinit();

    const result = try optimizer.visitNode(add_node);

    try std.testing.expectEqual(@as(u32, 1), optimizer.stats.float_folds);
    try std.testing.expectEqual(NodeTag.lit_float, store.getTag(result));
    // Result should be 4.0
    const result_idx: u16 = @truncate(store.getData(result).a);
    try std.testing.expectEqual(@as(f64, 4.0), constants.getFloat(result_idx).?);
}

test "IROptimizer: string concatenation" {
    // Use arena allocator since concatenated strings are stored in ConstantPool
    // without being copied (the pool stores slices directly)
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var store = IRStore.init(allocator);
    var constants = ConstantPool.init(allocator);

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create "hello" + "world" -> should become "helloworld"
    const left_idx = try constants.addString("hello");
    const right_idx = try constants.addString("world");
    const left_node = try store.addLitString(loc, left_idx);
    const right_node = try store.addLitString(loc, right_idx);
    const add_node = try store.addBinary(loc, .add, left_node, right_node);

    var optimizer = IROptimizer.init(allocator, &store, &constants);

    const result = try optimizer.visitNode(add_node);

    try std.testing.expectEqual(@as(u32, 1), optimizer.stats.string_concats);
    try std.testing.expectEqual(NodeTag.lit_string, store.getTag(result));
    // Result should be "helloworld"
    const result_idx: u16 = @truncate(store.getData(result).a);
    try std.testing.expectEqualStrings("helloworld", constants.getString(result_idx).?);
}

test "IROptimizer: short-circuit && with true" {
    const allocator = std.testing.allocator;
    var store = IRStore.init(allocator);
    defer store.deinit();
    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create true && 42 -> should become 42
    const true_node = try store.addNode(.lit_bool, loc, .{ .a = 1, .b = 0 });
    const int_node = try store.addLitInt(loc, 42);
    const and_node = try store.addBinary(loc, .and_op, true_node, int_node);

    var optimizer = IROptimizer.init(allocator, &store, &constants);
    defer optimizer.deinit();

    const result = try optimizer.visitNode(and_node);

    // true && x should simplify to x
    try std.testing.expectEqual(int_node, result);
    try std.testing.expectEqual(@as(u32, 1), optimizer.stats.short_circuit_simplifications);
}

test "IROptimizer: short-circuit && with false" {
    const allocator = std.testing.allocator;
    var store = IRStore.init(allocator);
    defer store.deinit();
    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create false && 42 -> should become false
    const false_node = try store.addNode(.lit_bool, loc, .{ .a = 0, .b = 0 });
    const int_node = try store.addLitInt(loc, 42);
    const and_node = try store.addBinary(loc, .and_op, false_node, int_node);

    var optimizer = IROptimizer.init(allocator, &store, &constants);
    defer optimizer.deinit();

    const result = try optimizer.visitNode(and_node);

    // false && x should simplify to false
    try std.testing.expectEqual(false_node, result);
    try std.testing.expectEqual(@as(u32, 1), optimizer.stats.short_circuit_simplifications);
}

test "IROptimizer: short-circuit || with false" {
    const allocator = std.testing.allocator;
    var store = IRStore.init(allocator);
    defer store.deinit();
    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create false || 42 -> should become 42
    const false_node = try store.addNode(.lit_bool, loc, .{ .a = 0, .b = 0 });
    const int_node = try store.addLitInt(loc, 42);
    const or_node = try store.addBinary(loc, .or_op, false_node, int_node);

    var optimizer = IROptimizer.init(allocator, &store, &constants);
    defer optimizer.deinit();

    const result = try optimizer.visitNode(or_node);

    // false || x should simplify to x
    try std.testing.expectEqual(int_node, result);
    try std.testing.expectEqual(@as(u32, 1), optimizer.stats.short_circuit_simplifications);
}

test "IROptimizer: comparison folding" {
    const allocator = std.testing.allocator;
    var store = IRStore.init(allocator);
    defer store.deinit();
    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create 5 < 10 -> should become true
    const left_node = try store.addLitInt(loc, 5);
    const right_node = try store.addLitInt(loc, 10);
    const lt_node = try store.addBinary(loc, .lt, left_node, right_node);

    var optimizer = IROptimizer.init(allocator, &store, &constants);
    defer optimizer.deinit();

    const result = try optimizer.visitNode(lt_node);

    try std.testing.expectEqual(@as(u32, 1), optimizer.stats.bool_folds);
    try std.testing.expectEqual(NodeTag.lit_bool, store.getTag(result));
    try std.testing.expectEqual(@as(u32, 1), store.getData(result).a); // true
}

test "IROptimizer: convenience function" {
    const allocator = std.testing.allocator;
    var store = IRStore.init(allocator);
    defer store.deinit();
    var constants = ConstantPool.init(allocator);
    defer constants.deinit();

    const loc = SourceLocation{ .line = 1, .column = 1, .offset = 0 };

    // Create !false
    const false_node = try store.addNode(.lit_bool, loc, .{ .a = 0, .b = 0 });
    const not_node = try store.addUnary(loc, .not, false_node);

    const stats = try optimizeIR(allocator, &store, &constants, not_node);

    try std.testing.expectEqual(@as(u32, 1), stats.bool_folds);
    try std.testing.expectEqual(@as(u32, 1), stats.totalOptimizations());
}
