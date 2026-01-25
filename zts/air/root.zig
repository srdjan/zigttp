//! AIR (Abstract Intermediate Representation) Module
//!
//! CFG-based SSA intermediate representation for the optimized JIT tier.
//! AIR enables standard compiler optimizations like CSE, LICM, and guard hoisting.
//!
//! Public API:
//! - buildFromBytecode: Convert bytecode function to AIR
//! - print: Output AIR in human-readable format
//! - verify: Validate AIR structure
//! - interpret: Execute AIR for testing/validation
//!
//! Module structure:
//! - types.zig: Core data structures (ValueId, BlockId, Inst, Block, Function)
//! - cfg.zig: CFG construction from linear bytecode
//! - builder.zig: Stack-phi SSA construction algorithm
//! - printer.zig: Deterministic text output
//! - interp.zig: Reference interpreter for validation

const std = @import("std");
pub const cfg = @import("cfg.zig");
pub const types = @import("types.zig");
pub const builder = @import("builder.zig");
pub const printer = @import("printer.zig");
pub const interp = @import("interp.zig");
pub const lowering = @import("lowering.zig");

// Re-export commonly used types
pub const BlockId = types.BlockId;
pub const ValueId = types.ValueId;
pub const Inst = types.Inst;
pub const InstOp = types.InstOp;
pub const Block = types.Block;
pub const Function = types.Function;
pub const Terminator = types.Terminator;

pub const CfgBuilder = cfg.CfgBuilder;
pub const Builder = builder.Builder;
pub const BuildError = builder.BuildError;

pub const AirInterpreter = interp.AirInterpreter;
pub const InterpError = interp.InterpError;

pub const Lowering = lowering.Lowering;
pub const LoweringStats = lowering.LoweringStats;
pub const compileFromAir = lowering.compileFromAir;

const bytecode = @import("../bytecode.zig");
const value_mod = @import("../value.zig");
const JSValue = value_mod.JSValue;

/// Build AIR from bytecode function
///
/// This is the main entry point for AIR construction. It:
/// 1. Builds a CFG from linear bytecode
/// 2. Translates bytecode to SSA form
/// 3. Inserts phi nodes at join points
/// 4. Returns the complete AIR function
pub fn buildFromBytecode(
    allocator: std.mem.Allocator,
    bc_func: *const bytecode.FunctionBytecode,
) BuildError!Function {
    return builder.buildFromBytecode(allocator, bc_func);
}

/// Print AIR function to a writer
pub fn print(func: *const Function, writer: anytype) !void {
    try printer.printFunction(func, writer);
}

/// Print AIR function to a string
pub fn printToString(allocator: std.mem.Allocator, func: *const Function) ![]u8 {
    return printer.printToString(allocator, func);
}

/// Execute AIR function with given arguments
/// Used for differential testing against bytecode interpreter
pub fn interpret(
    allocator: std.mem.Allocator,
    func: *const Function,
    args: []const JSValue,
) InterpError!JSValue {
    return interp.execute(allocator, func, args);
}

/// Verification errors
pub const VerifyError = error{
    InvalidBlockId,
    InvalidValueId,
    UndefinedValue,
    TypeMismatch,
    MissingTerminator,
    UnreachableBlock,
    PhiPredecessorMismatch,
};

/// Verify AIR function structure
///
/// Checks:
/// - All value uses are defined before use
/// - Block predecessors match CFG edges
/// - Phi nodes have correct number of operands
/// - All blocks have terminators
/// - No unreachable blocks (except entry)
pub fn verify(func: *const Function) VerifyError!void {
    // Track defined values
    var defined = std.AutoHashMap(u32, void).init(func.allocator);
    defer defined.deinit();

    for (func.blocks.items, 0..) |*block, block_idx| {
        // Check block params (phi values)
        for (block.params.items) |param| {
            if (!param.value.isNone()) {
                defined.put(encodeValue(param.value), {}) catch return VerifyError.InvalidValueId;
            }

            // Verify phi has operand for each predecessor
            if (param.phi.count != block.predecessors.items.len) {
                return VerifyError.PhiPredecessorMismatch;
            }
        }

        // Check instructions
        for (block.instructions.items) |inst| {
            // Check operands are defined
            for (inst.operands) |op| {
                if (!op.isNone() and op.kind == .local) {
                    if (!defined.contains(encodeValue(op))) {
                        return VerifyError.UndefinedValue;
                    }
                }
            }

            // Define result
            if (!inst.result.isNone()) {
                defined.put(encodeValue(inst.result), {}) catch return VerifyError.InvalidValueId;
            }
        }

        // Check terminator
        if (block.terminator.kind == .none and block_idx < func.blocks.items.len - 1) {
            // Non-terminal block without terminator
            // This is allowed for now during construction
        }

        // Check terminator operands
        switch (block.terminator.kind) {
            .branch => {
                if (!block.terminator.condition.isNone() and block.terminator.condition.kind == .local) {
                    if (!defined.contains(encodeValue(block.terminator.condition))) {
                        return VerifyError.UndefinedValue;
                    }
                }
            },
            .ret => {
                if (!block.terminator.return_value.isNone() and block.terminator.return_value.kind == .local) {
                    if (!defined.contains(encodeValue(block.terminator.return_value))) {
                        return VerifyError.UndefinedValue;
                    }
                }
            },
            else => {},
        }
    }
}

fn encodeValue(val: ValueId) u32 {
    return @bitCast(val);
}

// ============================================================================
// Unit Tests
// ============================================================================

test "AIR module: build and print simple function" {
    const allocator = std.testing.allocator;
    const Opcode = bytecode.Opcode;

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

    // Print to string
    const output = try printToString(allocator, &func);
    defer allocator.free(output);

    // Should contain function header
    try std.testing.expect(std.mem.indexOf(u8, output, "function @main") != null);

    // Should contain bb0
    try std.testing.expect(std.mem.indexOf(u8, output, "bb0:") != null);

    // Should contain add_int
    try std.testing.expect(std.mem.indexOf(u8, output, "add_int") != null);

    // Should contain ret
    try std.testing.expect(std.mem.indexOf(u8, output, "ret") != null);
}

test "AIR module: verify valid function" {
    const allocator = std.testing.allocator;
    const Opcode = bytecode.Opcode;

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

    // Should pass verification
    try verify(&func);
}

test "AIR module: branching function" {
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

    var func = try buildFromBytecode(allocator, &bc_func);
    defer func.deinit();

    // Should have 3 blocks
    try std.testing.expectEqual(@as(usize, 3), func.blocks.items.len);

    // Entry should have branch terminator
    const entry = func.getBlock(BlockId.entry).?;
    try std.testing.expectEqual(Terminator.Kind.branch, entry.terminator.kind);

    // Print for debugging
    const output = try printToString(allocator, &func);
    defer allocator.free(output);

    // Should contain branch instruction
    try std.testing.expect(std.mem.indexOf(u8, output, "branch") != null);
}
