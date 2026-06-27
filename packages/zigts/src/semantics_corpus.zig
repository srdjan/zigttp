//! semantics_corpus.zig - mechanism 4: the differential corpus.
//!
//! Mechanisms 1-3 (semantics_check.zig) prove that the registry is internally
//! consistent: exec(DECLARED lower) == DECLARED denote. They never look at the
//! real compiler. This file closes that gap. For each value rule it compiles a
//! tiny source snippet through the REAL zigts front end (parser + codegen),
//! symbolically executes the bytecode the compiler actually emitted, and checks
//! the recovered denotation equals the registry's declared denotation. If
//! codegen.zig ever lowered `+` to `sub`, mechanism 3 would still pass (the
//! declared lowering is unchanged) and mechanism 4 would fail.
//!
//! How the recovery works (measured, not assumed): a top-level expression
//! statement compiles to `<operand pushes> <operator> drop ret_undefined`, with
//! `const` bindings becoming `put_global`/`get_global`. The expression's value is
//! always consumed by exactly one `drop`. Symbolically executing the stream and
//! capturing what that `drop` discards yields the expression's denotation, with
//! every leaf operand (literal push, global/local read) canonicalized to a single
//! operand sentinel - so the check is about the OPERATOR STRUCTURE the compiler
//! emits, not operand identity.
//!
//! Scope honesty:
//!   - Covers add, sub, mul, lt (the binops with an emittable source operator),
//!     neg, not, identifier, the two literal kinds, and the ternary branch shape.
//!   - `BinKind.eq` has NO corpus case: the restricted language forbids `==`, and
//!     `===` lowers to `strict_eq`, not the bare `eq` opcode the registry maps to.
//!     `eq` therefore stays covered by mechanism 3 only. (`strict_eq` itself is
//!     not yet in the symbolic registry - a known, recorded gap.)
//!   - `call` is the oracle boundary and is not differentially tested.
//!
//! This module decodes bytecode only; it does not run the interpreter, so it
//! needs no GC/Context and stays cheap.

const std = @import("std");
const compiler = @import("compiler.zig");
const bytecode = @import("bytecode.zig");
const semantics = @import("semantics.zig");

const Term = semantics.Term;
const Opcode = semantics.Opcode;
const NodeTag = semantics.NodeTag;
const BinKind = semantics.BinKind;
const UnKind = semantics.UnKind;

// ---------------------------------------------------------------------------
// Corpus: one snippet per differentially-testable value rule.
// ---------------------------------------------------------------------------

const Shape = enum { straight, branch };

const Case = struct {
    tag: NodeTag,
    binop: ?BinKind = null,
    unop: ?UnKind = null,
    source: []const u8,
    shape: Shape = .straight,
    required_opcode: ?Opcode = null,
};

pub const corpus = [_]Case{
    .{ .tag = .lit_int, .source = "7", .required_opcode = .push_i8 },
    .{ .tag = .lit_bool, .source = "true", .required_opcode = .push_true },
    .{ .tag = .identifier, .source = "const a = 5; a" },
    .{ .tag = .binary_op, .binop = .add, .source = "const a = 5; const b = 3; a + b" },
    .{ .tag = .binary_op, .binop = .sub, .source = "const a = 5; const b = 3; a - b" },
    .{ .tag = .binary_op, .binop = .mul, .source = "const a = 5; const b = 3; a * b" },
    .{ .tag = .binary_op, .binop = .lt, .source = "const a = 5; const b = 3; a < b" },
    .{ .tag = .unary_op, .unop = .neg, .source = "const a = 5; -a" },
    .{ .tag = .unary_op, .unop = .not, .source = "const a = true; !a" },
    .{ .tag = .ternary, .shape = .branch, .source = "const c = true; const t = 7; const e = 9; c ? t : e" },
};

// ---------------------------------------------------------------------------
// Symbolic recovery of a denotation from real bytecode.
// ---------------------------------------------------------------------------

/// The single operand sentinel: every leaf value the compiler pushes (a literal,
/// a global read, a local read) collapses to this. The differential is about the
/// operator the compiler applies, not which storage the operand came from.
const operand: Term = .imm;

const Recovered = struct {
    /// What the most recent `drop` discarded. Only meaningful when `drop_count`
    /// is exactly 1 (the single-expression-statement invariant the corpus relies
    /// on); the caller checks that and fails loud otherwise.
    dropped: ?[]const Term,
    /// How many `drop`s the stream issued. A clean single-expression snippet
    /// drops exactly once; anything else means the snippet did not lower the way
    /// the corpus assumes, so the recovered denotation cannot be trusted.
    drop_count: usize,
    /// Whether the stream used the branch control opcodes (if_false / goto).
    has_if_false: bool,
    has_goto: bool,
    /// Recovered branch operands for a basic ternary expression. The final value
    /// is `cond then else select`, matching the registry's denotation.
    branch_cond: ?[]const Term,
    branch_then: ?[]const Term,
    branch_value: ?[]const Term,
    /// First opcode the recoverer did not know how to model (fail loud).
    unhandled: ?Opcode,
};

fn containsOpcode(code: []const u8, wanted: Opcode) bool {
    var pc: usize = 0;
    while (pc < code.len) {
        const op: Opcode = @enumFromInt(code[pc]);
        if (op == wanted) return true;
        const info = bytecode.getOpcodeInfo(op);
        if (info.size == 0) break;
        pc += info.size;
    }
    return false;
}

fn isOperandPush(op: Opcode) bool {
    return switch (op) {
        .push_const, .push_0, .push_1, .push_2, .push_3, .push_i8, .push_i16, .push_null, .push_undefined, .push_true, .push_false, .get_global, .get_loc, .get_loc_0, .get_loc_1, .get_loc_2, .get_loc_3 => true,
        else => false,
    };
}

fn isConsumer(op: Opcode) bool {
    return switch (op) {
        .put_global, .put_loc, .put_loc_0, .put_loc_1, .put_loc_2, .put_loc_3 => true,
        else => false,
    };
}

fn isIgnorable(op: Opcode) bool {
    return switch (op) {
        .ret_undefined, .ret, .nop => true,
        else => false,
    };
}

/// Symbolically execute a real bytecode stream, capturing the term sequence
/// discarded by the (single) expression-statement `drop`. Operands are generic.
fn recover(scratch: std.mem.Allocator, code: []const u8) !Recovered {
    var result: Recovered = .{
        .dropped = null,
        .drop_count = 0,
        .has_if_false = false,
        .has_goto = false,
        .branch_cond = null,
        .branch_then = null,
        .branch_value = null,
        .unhandled = null,
    };
    var stack: std.ArrayList([]const Term) = .empty;

    var pc: usize = 0;
    while (pc < code.len) {
        const op: Opcode = @enumFromInt(code[pc]);
        const info = bytecode.getOpcodeInfo(op);
        if (info.size == 0) break;

        if (op == .if_false) {
            if (stack.items.len == 0) {
                result.unhandled = op;
                return result;
            }
            result.branch_cond = stack.items[stack.items.len - 1];
            stack.items.len -= 1;
            result.has_if_false = true;
        } else if (op == .goto) {
            if (stack.items.len == 0) {
                result.unhandled = op;
                return result;
            }
            result.branch_then = stack.items[stack.items.len - 1];
            stack.items.len -= 1;
            result.has_goto = true;
        } else if (op == .drop) {
            if (stack.items.len == 0) {
                result.unhandled = op;
                return result;
            }
            const dropped = stack.items[stack.items.len - 1];
            result.dropped = dropped;
            result.drop_count += 1;
            stack.items.len -= 1;
            if (result.has_if_false and result.has_goto) {
                if (result.branch_cond) |cond| {
                    if (result.branch_then) |then_value| {
                        result.branch_value = try std.mem.concat(scratch, Term, &.{ cond, then_value, dropped, &.{.select} });
                    }
                }
            }
        } else if (isOperandPush(op)) {
            try stack.append(scratch, try scratch.dupe(Term, &.{operand}));
        } else if (isConsumer(op)) {
            if (stack.items.len == 0) {
                result.unhandled = op;
                return result;
            }
            stack.items.len -= 1;
        } else if (semantics.opTransition(op)) |tr| {
            switch (tr) {
                .binop => |k| {
                    if (stack.items.len < 2) {
                        result.unhandled = op;
                        return result;
                    }
                    const b = stack.items[stack.items.len - 1];
                    const a = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;
                    try stack.append(scratch, try std.mem.concat(scratch, Term, &.{ a, b, &.{.{ .binop = k }} }));
                },
                .unop => |k| {
                    if (stack.items.len < 1) {
                        result.unhandled = op;
                        return result;
                    }
                    const a = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    try stack.append(scratch, try std.mem.concat(scratch, Term, &.{ a, &.{.{ .unop = k }} }));
                },
                .control => {},
            }
        } else if (isIgnorable(op)) {
            // no stack effect we model
        } else {
            result.unhandled = op;
            return result;
        }
        pc += info.size;
    }
    return result;
}

// ---------------------------------------------------------------------------
// Expected denotation, derived from the registry rule (canonicalized).
// ---------------------------------------------------------------------------

/// The registry's declared denotation for a case, with leaf operands canonical-
/// ized to the operand sentinel and the parametric operator instantiated. Derived
/// from semantics.node_rules so the corpus stays tied to the registry, not a copy.
fn expectedDenote(scratch: std.mem.Allocator, c: Case) ![]const Term {
    const rule = blk: {
        for (semantics.node_rules) |r| {
            if (r.tag == c.tag) break :blk r;
        }
        unreachable; // every corpus case names a real rule
    };
    const out = try scratch.alloc(Term, rule.denote.len);
    for (rule.denote, 0..) |t, i| {
        out[i] = switch (t) {
            .imm, .child, .local, .call_result => operand,
            .binop_self => Term{ .binop = c.binop.? },
            .unop_self => Term{ .unop = c.unop.? },
            else => t, // .binop / .unop / .select pass through
        };
    }
    return out;
}

// ---------------------------------------------------------------------------
// Result + driver.
// ---------------------------------------------------------------------------

pub const Divergence = struct {
    where: []const u8,
    message: []const u8,
};

pub const CorpusResult = struct {
    arena: std.heap.ArenaAllocator,
    cases_total: usize = 0,
    cases_passed: usize = 0,
    failures: std.ArrayList(Divergence) = .empty,

    pub fn ok(self: *const CorpusResult) bool {
        return self.failures.items.len == 0;
    }
    pub fn deinit(self: *CorpusResult) void {
        self.arena.deinit();
    }
    fn fail(self: *CorpusResult, where: []const u8, message: []const u8) !void {
        const a = self.arena.allocator();
        try self.failures.append(a, .{ .where = try a.dupe(u8, where), .message = try a.dupe(u8, message) });
    }
};

fn termsToString(alloc: std.mem.Allocator, terms: []const Term) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    for (terms, 0..) |t, idx| {
        if (idx != 0) try buf.append(alloc, ' ');
        switch (t) {
            .imm => try buf.appendSlice(alloc, "opnd"),
            .binop => |k| try buf.appendSlice(alloc, @tagName(k)),
            .unop => |k| try buf.appendSlice(alloc, try std.fmt.allocPrint(alloc, "{s}u", .{@tagName(k)})),
            .select => try buf.appendSlice(alloc, "?:"),
            else => try buf.appendSlice(alloc, "?"),
        }
    }
    return buf.items;
}

/// Compile every corpus snippet through the real front end and check the
/// recovered denotation against the registry. `backing` owns transient compile
/// output; the returned result owns its own arena for the failure list.
pub fn runCorpus(backing: std.mem.Allocator) !CorpusResult {
    var result = CorpusResult{ .arena = std.heap.ArenaAllocator.init(backing) };
    errdefer result.deinit();
    const ra = result.arena.allocator();

    for (corpus) |c| {
        result.cases_total += 1;
        const where = @tagName(c.tag);

        // Each compile gets a fresh scratch arena so transient bytecode and the
        // symbolic stacks are reclaimed before the next case.
        var scratch_arena = std.heap.ArenaAllocator.init(backing);
        defer scratch_arena.deinit();
        const scratch = scratch_arena.allocator();

        const func = compiler.compile(scratch, c.source) catch |e| {
            try result.fail(where, try std.fmt.allocPrint(ra, "compile failed: {s}", .{@errorName(e)}));
            continue;
        };
        if (c.required_opcode) |required| {
            if (!containsOpcode(func.code, required)) {
                try result.fail(where, try std.fmt.allocPrint(ra, "real codegen did not emit required opcode: {s}", .{@tagName(required)}));
                continue;
            }
        }

        const rec = recover(scratch, func.code) catch {
            try result.fail(where, "recovery ran out of memory");
            continue;
        };
        if (rec.unhandled) |op| {
            try result.fail(where, try std.fmt.allocPrint(ra, "real codegen used an unmodeled opcode: {s}", .{@tagName(op)}));
            continue;
        }

        // The recovery trusts exactly one expression-statement `drop` to carry the
        // denotation. Enforce that invariant rather than assume it: a snippet that
        // lowers to zero or several drops is not one the corpus can read, so fail
        // loud instead of silently checking the last drop.
        if (rec.drop_count != 1) {
            try result.fail(where, try std.fmt.allocPrint(ra, "expected exactly one dropped expression value, saw {d}", .{rec.drop_count}));
            continue;
        }

        switch (c.shape) {
            .straight => {
                // drop_count == 1 (checked above) guarantees `dropped` is set.
                const got = rec.dropped.?;
                const want = try expectedDenote(scratch, c);
                if (!semantics.termsEql(got, want)) {
                    const w = try termsToString(ra, want);
                    const g = try termsToString(ra, got);
                    try result.fail(where, try std.fmt.allocPrint(ra, "registry denote=[{s}] but real codegen=>[{s}]", .{ w, g }));
                    continue;
                }
            },
            .branch => {
                // The declared ternary lowering wires the branch with if_false +
                // goto; require the real codegen to use the same control shape.
                if (!rec.has_if_false or !rec.has_goto) {
                    try result.fail(where, "real codegen did not lower the conditional to an if_false/goto branch");
                    continue;
                }
                const got = rec.branch_value orelse {
                    try result.fail(where, "real codegen branch did not recover to a selected expression value");
                    continue;
                };
                const want = try expectedDenote(scratch, c);
                if (!semantics.termsEql(got, want)) {
                    const w = try termsToString(ra, want);
                    const g = try termsToString(ra, got);
                    try result.fail(where, try std.fmt.allocPrint(ra, "registry denote=[{s}] but real branch codegen=>[{s}]", .{ w, g }));
                    continue;
                }
            },
        }
        result.cases_passed += 1;
    }

    return result;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "differential corpus: registry denotations match real codegen" {
    var result = try runCorpus(std.testing.allocator);
    defer result.deinit();

    for (result.failures.items) |f| {
        std.debug.print("corpus divergence at {s}: {s}\n", .{ f.where, f.message });
    }
    try std.testing.expect(result.ok());
    try std.testing.expectEqual(corpus.len, result.cases_passed);
    try std.testing.expectEqual(corpus.len, result.cases_total);
}

test "recover reads the dropped expression denotation from real bytecode" {
    var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch.deinit();

    const func = try compiler.compile(scratch.allocator(), "const a = 5; const b = 3; a + b");
    const rec = try recover(scratch.allocator(), func.code);
    try std.testing.expect(rec.unhandled == null);
    // A single-expression snippet drops exactly once - the invariant the corpus
    // enforces before trusting the recovered denotation.
    try std.testing.expectEqual(@as(usize, 1), rec.drop_count);
    const got = rec.dropped.?;
    // operand operand add
    try std.testing.expect(semantics.termsEql(got, &.{ operand, operand, .{ .binop = .add } }));
}

test "recover counts every drop so the single-expression invariant is enforceable" {
    var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch.deinit();

    // push_3; drop; push_3; drop; ret_undefined -> two dropped values, which the
    // corpus guard rejects rather than silently checking only the last.
    const code = [_]u8{
        @intFromEnum(Opcode.push_3),        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.push_3),        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.ret_undefined),
    };
    const rec = try recover(scratch.allocator(), &code);
    try std.testing.expect(rec.unhandled == null);
    try std.testing.expectEqual(@as(usize, 2), rec.drop_count);
}

test "recover reconstructs ternary branch denotation" {
    var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch.deinit();

    const func = try compiler.compile(scratch.allocator(), "const c = true; const t = 7; const e = 9; c ? t : e");
    const rec = try recover(scratch.allocator(), func.code);
    try std.testing.expect(rec.unhandled == null);
    try std.testing.expect(rec.has_if_false);
    try std.testing.expect(rec.has_goto);
    try std.testing.expect(rec.branch_value != null);
    try std.testing.expect(semantics.termsEql(rec.branch_value.?, &.{ operand, operand, operand, .select }));
}

test "literal corpus cases require exact literal opcodes" {
    var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch.deinit();

    const true_func = try compiler.compile(scratch.allocator(), "true");
    try std.testing.expect(containsOpcode(true_func.code, .push_true));
    try std.testing.expect(!containsOpcode(true_func.code, .push_false));

    const int_func = try compiler.compile(scratch.allocator(), "7");
    try std.testing.expect(containsOpcode(int_func.code, .push_i8));

    const wrong_true = [_]u8{
        @intFromEnum(Opcode.push_false),
        @intFromEnum(Opcode.drop),
        @intFromEnum(Opcode.ret_undefined),
    };
    try std.testing.expect(!containsOpcode(&wrong_true, .push_true));
}

test "a corpus case whose expected operator is wrong is caught" {
    var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch.deinit();
    const s = scratch.allocator();

    // Real codegen for `a - b` recovers `opnd opnd sub`; assert it does NOT match
    // an add expectation - i.e. the differential would catch a sub-for-add bug.
    const func = try compiler.compile(s, "const a = 5; const b = 3; a - b");
    const rec = try recover(s, func.code);
    const got = rec.dropped.?;
    try std.testing.expect(!semantics.termsEql(got, &.{ operand, operand, .{ .binop = .add } }));
    try std.testing.expect(semantics.termsEql(got, &.{ operand, operand, .{ .binop = .sub } }));
}
