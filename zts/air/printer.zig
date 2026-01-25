//! AIR Printer - Deterministic Text Output
//!
//! Prints AIR functions in a human-readable text format suitable for
//! golden tests and debugging.
//!
//! Format:
//! ```
//! function @name {
//!   bb0:
//!     %0 = param 0 ; bc@0
//!     %1 = const_int 0 ; bc@2
//!   bb1:                          ; loop header
//!     %2 = phi [bb0: %1], [bb2: %5]
//!     %3 = lt_int %2, %0 ; bc@5
//!     branch %3, bb2, bb3 ; bc@7
//!   bb2:
//!     %4 = const_int 1 ; bc@10
//!     %5 = add_int %2, %4 ; bc@12
//!     goto bb1 ; bc@13
//!   bb3:
//!     ret %2 ; bc@16
//! }
//! ```

const std = @import("std");
const types = @import("types.zig");

const Function = types.Function;
const Block = types.Block;
const Inst = types.Inst;
const InstOp = types.InstOp;
const ValueId = types.ValueId;
const BlockId = types.BlockId;
const Terminator = types.Terminator;
const PhiData = types.PhiData;

/// Print an AIR function to a writer
pub fn printFunction(func: *const Function, writer: anytype) !void {
    try writer.writeAll("function @main {\n");

    for (func.blocks.items, 0..) |*block, idx| {
        try printBlock(block, @intCast(idx), writer);
    }

    try writer.writeAll("}\n");
}

/// Print a single block
fn printBlock(block: *const Block, idx: u16, writer: anytype) !void {
    // Block header
    try writer.print("  bb{d}:", .{idx});
    if (block.is_loop_header) {
        try writer.writeAll("  ; loop header");
    }
    try writer.writeAll("\n");

    // Phi nodes (block params)
    for (block.params.items) |param| {
        try writer.writeAll("    ");
        try printValue(param.value, writer);
        try writer.writeAll(" = phi ");
        try printPhiData(&param.phi, writer);
        try writer.writeAll("\n");
    }

    // Instructions
    for (block.instructions.items) |inst| {
        try printInst(&inst, writer);
    }

    // Terminator
    try printTerminator(&block.terminator, writer);
}

/// Print a phi node's incoming values
fn printPhiData(phi: *const PhiData, writer: anytype) !void {
    var first = true;
    for (phi.preds[0..phi.count], phi.values[0..phi.count]) |pred, val| {
        if (!first) try writer.writeAll(", ");
        try writer.print("[bb{d}: ", .{pred.asIndex()});
        try printValue(val, writer);
        try writer.writeAll("]");
        first = false;
    }
}

/// Print a single instruction
fn printInst(inst: *const Inst, writer: anytype) !void {
    try writer.writeAll("    ");

    // Result
    if (!inst.result.isNone()) {
        try printValue(inst.result, writer);
        try writer.writeAll(" = ");
    }

    // Opcode
    try writer.writeAll(@tagName(inst.op));

    // Operands
    switch (inst.op) {
        .const_int => {
            try writer.print(" {d}", .{inst.extra.int_const});
        },
        .const_float => {
            try writer.print(" {d}", .{inst.extra.float_const});
        },
        .param => {
            try writer.print(" {d}", .{inst.extra.int_const});
        },
        .load_local, .store_local => {
            try writer.print(" loc{d}", .{inst.extra.local_idx});
        },
        .phi => {
            try writer.writeAll(" ");
            try printPhiData(&inst.extra.phi_data, writer);
        },
        .add_int, .sub_int, .mul_int, .div_int, .mod_int,
        .lt_int, .lte_int, .gt_int, .gte_int, .eq_int, .neq_int,
        .bit_and, .bit_or, .bit_xor, .shl, .shr, .ushr,
        .add_generic, .sub_generic, .mul_generic,
        => {
            try writer.writeAll(" ");
            try printValue(inst.operands[0], writer);
            try writer.writeAll(", ");
            try printValue(inst.operands[1], writer);
        },
        .neg_int, .inc_int, .dec_int, .bit_not, .not,
        .guard_int, .copy,
        => {
            try writer.writeAll(" ");
            try printValue(inst.operands[0], writer);
        },
        .const_true, .const_false, .const_null, .const_undefined => {},
        else => {
            // Print all non-none operands
            var first = true;
            for (inst.operands) |op| {
                if (!op.isNone()) {
                    if (first) {
                        try writer.writeAll(" ");
                        first = false;
                    } else {
                        try writer.writeAll(", ");
                    }
                    try printValue(op, writer);
                }
            }
        },
    }

    // Bytecode offset comment
    if (inst.bytecode_offset > 0) {
        try writer.print(" ; bc@{d}", .{inst.bytecode_offset});
    }

    try writer.writeAll("\n");
}

/// Print a terminator
fn printTerminator(term: *const Terminator, writer: anytype) !void {
    switch (term.kind) {
        .none => {},
        .goto => {
            try writer.print("    goto bb{d}", .{term.true_target.asIndex()});
        },
        .branch => {
            try writer.writeAll("    branch ");
            try printValue(term.condition, writer);
            try writer.print(", bb{d}, bb{d}", .{
                term.true_target.asIndex(),
                term.false_target.asIndex(),
            });
        },
        .ret => {
            try writer.writeAll("    ret ");
            try printValue(term.return_value, writer);
        },
        .ret_undefined => {
            try writer.writeAll("    ret_undefined");
        },
        .deopt => {
            try writer.print("    deopt reason={d} bc@{d}", .{
                term.deopt_reason,
                term.bytecode_offset,
            });
        },
        .unreachable_term => {
            try writer.writeAll("    unreachable");
        },
    }

    if (term.kind != .none) {
        if (term.bytecode_offset > 0) {
            try writer.print(" ; bc@{d}", .{term.bytecode_offset});
        }
        try writer.writeAll("\n");
    }
}

/// Print a value ID
fn printValue(val: ValueId, writer: anytype) !void {
    if (val.isNone()) {
        try writer.writeAll("_");
    } else switch (val.kind) {
        .local => try writer.print("%{d}", .{val.index}),
        .param => try writer.print("$arg{d}", .{val.index}),
        .constant => try writer.print("#{d}", .{val.index}),
        .undefined => try writer.writeAll("undef"),
    }
}

/// Print to a string (for testing)
pub fn printToString(allocator: std.mem.Allocator, func: *const Function) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    errdefer list.deinit();
    try printFunction(func, list.writer());
    return list.toOwnedSlice();
}

// ============================================================================
// Unit Tests
// ============================================================================

test "Printer: value formatting" {
    var buf: [64]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const writer = fbs.writer();

    try printValue(ValueId.local(42), writer);
    try std.testing.expectEqualStrings("%42", fbs.getWritten());

    fbs.reset();
    try printValue(ValueId.param(0), writer);
    try std.testing.expectEqualStrings("$arg0", fbs.getWritten());

    fbs.reset();
    try printValue(ValueId.constant(10), writer);
    try std.testing.expectEqualStrings("#10", fbs.getWritten());

    fbs.reset();
    try printValue(ValueId.none, writer);
    try std.testing.expectEqualStrings("_", fbs.getWritten());
}

test "Printer: instruction formatting" {
    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const writer = fbs.writer();

    const inst = Inst.binary(.add_int, ValueId.local(2), ValueId.local(0), ValueId.local(1))
        .withBytecodeOffset(10);

    try printInst(&inst, writer);
    const output = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, output, "%2 = add_int %0, %1") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "bc@10") != null);
}

test "Printer: terminator formatting" {
    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const writer = fbs.writer();

    const term = Terminator.branch(
        ValueId.local(5),
        BlockId.fromIndex(1),
        BlockId.fromIndex(2),
    );

    try printTerminator(&term, writer);
    const output = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, output, "branch %5, bb1, bb2") != null);
}
