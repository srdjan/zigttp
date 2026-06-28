//! semantics_smt.zig - the Phase 2 value-model encoder (northstar mechanism 5).
//!
//! Phase 1's mechanism 3 proves exec(lower) == denote by *structural* RPN
//! equality (semantics.termsEql). That is sound but incomplete: it only accepts a
//! lowering that is syntactically identical to the denotation. It cannot admit a
//! genuinely-equivalent-but-different refinement (a strength-reduced `x*2 -> x+x`,
//! a fused multiply-add, a type-specialized fast path) - exactly the refinements
//! the northstar's Alive2-style check is meant to certify.
//!
//! This module is the PURE half of that check: it encodes an equivalence
//! obligation `lhs == rhs` over the slice's value model into an SMT-LIB2 query
//! whose negation is asked of a solver (`unsat` => equivalent, `sat` =>
//! counterexample). It performs no I/O - the solver is invoked from the native
//! CLI layer (packages/tools/src/smt_solver.zig) and injected into the driver, so
//! `std.process.Child` never enters the freestanding/wasm analyzer build.
//!
//! Value model (this slice): the language is typed, so the encoding is
//! type-directed over native SMT sorts (Int, Bool) rather than a tagged value
//! datatype. A tagged datatype produces spurious counterexamples on algebraic
//! laws - if a free leaf may be a bool, `getInt` of it is unconstrained junk and
//! `neg(neg(x)) == x` comes back `sat`. Native sorts with light type inference
//! over the slice's two-element type lattice (int/bool) avoids that. The tagged /
//! poison datatype is the northstar's heavier end-state, for when the heap,
//! object identity, and NaN-boxing enter the model.
//!
//! SOUNDNESS BOUNDARY: numbers are encoded as the unbounded mathematical
//! integers ℤ (SMT-LIB `Int`), which is an ABSTRACTION of the engine's actual
//! number type (i32 promoting to IEEE-754 f64 on overflow). ℤ has no rounding,
//! overflow, NaN, or signed zero, so this encoding is sound only for laws whose
//! truth is independent of those effects. The registry (`semantics.algebraic_laws`)
//! is responsible for asserting only such laws - addition associativity is the
//! canonical one that is NOT (it holds over ℤ but not over f64); see the
//! SOUNDNESS BOUNDARY note there. A faithful f64 encoding is northstar work.

const std = @import("std");
const semantics = @import("semantics.zig");

const Term = semantics.Term;
const BinKind = semantics.BinKind;
const UnKind = semantics.UnKind;

/// The solver's answer to "is the negated equivalence satisfiable?".
pub const Verdict = enum {
    /// unsat: no counterexample exists -> the two sides are equivalent.
    equivalent,
    /// sat: a counterexample exists -> the sides differ on some input.
    counterexample,
    /// the check did not produce a decision, with no evidence the solver is
    /// broken: a timeout (z3 prints `unknown`), or an environmental failure to run
    /// z3 at all this attempt (spawn/IO error, or z3 gone since the availability
    /// check). Genuinely undecided / not-run, not a z3 evaluation failure.
    unknown,
    /// z3 RAN but could not evaluate the query - it errored or produced no
    /// recognizable verdict (z3 `(error ...)`, an unsupported/missing theory, or
    /// garbage output). Distinct from `unknown`: the present solver cannot do the
    /// math, so the audit fails closed rather than treating it as a benign timeout.
    solver_error,
};

/// Parse a solver's stdout into a verdict. An `(error ...)` anywhere in the output
/// is a solver error regardless of any verdict z3 prints afterwards. Otherwise the
/// first `sat`/`unsat`/`unknown` token is the check-sat result; benign preamble (a
/// z3 startup banner, or `success` lines under :print-success) is skipped. No
/// verdict at all is a solver error - z3 produced nothing usable.
pub fn parseVerdict(stdout: []const u8) Verdict {
    // A reported error wins over any verdict z3 may print after it.
    if (std.mem.indexOf(u8, stdout, "(error") != null) return .solver_error;
    var it = std.mem.tokenizeAny(u8, stdout, " \t\r\n");
    while (it.next()) |tok| {
        if (std.mem.eql(u8, tok, "unsat")) return .equivalent;
        if (std.mem.eql(u8, tok, "sat")) return .counterexample;
        if (std.mem.eql(u8, tok, "unknown")) return .unknown;
        // skip non-verdict tokens (banner, `success`) and keep scanning.
    }
    return .solver_error;
}

pub const EncodeError = error{
    /// A parametric placeholder reached the encoder un-instantiated.
    Uninstantiated,
    /// The RPN does not reduce to exactly one value (under/overflow of operands).
    Malformed,
    /// A node is used at two incompatible types, or the two sides of the
    /// obligation have different root sorts (a genuinely ill-typed obligation).
    TypeMismatch,
    OutOfMemory,
};

const Ty = enum {
    int,
    boolean,
    unknown,

    fn sortName(self: Ty) []const u8 {
        return switch (self) {
            .int, .unknown => "Int", // unresolved leaves default to Int.
            .boolean => "Bool",
        };
    }
};

const Leaf = struct {
    name: []const u8,
    ty: Ty,
};

const Entry = struct {
    expr: []const u8,
    ty: Ty,
    /// index into `leaves` if this entry is itself a bare leaf, else null.
    leaf: ?usize,
};

/// Builder over one obligation. Both sides share the leaf environment so a leaf
/// appearing in both (e.g. `c0`) is one SMT constant unified across the sides.
const Builder = struct {
    arena: std.mem.Allocator,
    leaves: std.ArrayList(Leaf) = .empty,

    fn leafIndex(self: *Builder, name: []const u8) !usize {
        for (self.leaves.items, 0..) |l, i| {
            if (std.mem.eql(u8, l.name, name)) return i;
        }
        try self.leaves.append(self.arena, .{ .name = name, .ty = .unknown });
        return self.leaves.items.len - 1;
    }

    fn entryTy(self: *Builder, e: Entry) Ty {
        if (e.leaf) |li| return self.leaves.items[li].ty;
        return e.ty;
    }

    /// Impose a concrete type on an operand, resolving (or checking) a leaf.
    fn require(self: *Builder, e: Entry, ty: Ty) !void {
        if (e.leaf) |li| {
            const cur = self.leaves.items[li].ty;
            if (cur == .unknown) {
                self.leaves.items[li].ty = ty;
            } else if (cur != ty) {
                return error.TypeMismatch;
            }
        } else if (e.ty != ty) {
            return error.TypeMismatch;
        }
    }

    /// Unify two operands to a common type (defaulting to Int when both are
    /// free), resolving both leaves. Used by `eq` and `select`'s branches.
    fn unify(self: *Builder, a: Entry, b: Entry) !Ty {
        const ta = self.entryTy(a);
        const tb = self.entryTy(b);
        if (ta != .unknown and tb != .unknown and ta != tb) return error.TypeMismatch;
        var target: Ty = if (ta != .unknown) ta else tb;
        if (target == .unknown) target = .int;
        try self.require(a, target);
        try self.require(b, target);
        return target;
    }

    fn pushLeaf(self: *Builder, stack: *std.ArrayList(Entry), name: []const u8) !void {
        const li = try self.leafIndex(name);
        try stack.append(self.arena, .{ .expr = name, .ty = self.leaves.items[li].ty, .leaf = li });
    }

    fn pushExpr(self: *Builder, stack: *std.ArrayList(Entry), expr: []const u8, ty: Ty) !void {
        try stack.append(self.arena, .{ .expr = expr, .ty = ty, .leaf = null });
    }

    fn pop(stack: *std.ArrayList(Entry)) !Entry {
        if (stack.items.len == 0) return error.Malformed;
        return stack.pop().?;
    }

    fn buildBinop(self: *Builder, stack: *std.ArrayList(Entry), k: BinKind) !void {
        const b = try pop(stack);
        const a = try pop(stack);
        switch (k) {
            .add, .sub, .mul => {
                try self.require(a, .int);
                try self.require(b, .int);
                const sym: []const u8 = switch (k) {
                    .add => "+",
                    .sub => "-",
                    .mul => "*",
                    else => unreachable,
                };
                try self.pushExpr(
                    stack,
                    try std.fmt.allocPrint(self.arena, "({s} {s} {s})", .{ sym, a.expr, b.expr }),
                    .int,
                );
            },
            .lt => {
                try self.require(a, .int);
                try self.require(b, .int);
                try self.pushExpr(
                    stack,
                    try std.fmt.allocPrint(self.arena, "(< {s} {s})", .{ a.expr, b.expr }),
                    .boolean,
                );
            },
            .eq => {
                _ = try self.unify(a, b);
                try self.pushExpr(
                    stack,
                    try std.fmt.allocPrint(self.arena, "(= {s} {s})", .{ a.expr, b.expr }),
                    .boolean,
                );
            },
        }
    }

    fn buildUnop(self: *Builder, stack: *std.ArrayList(Entry), k: UnKind) !void {
        const a = try pop(stack);
        switch (k) {
            .neg => {
                try self.require(a, .int);
                try self.pushExpr(stack, try std.fmt.allocPrint(self.arena, "(- {s})", .{a.expr}), .int);
            },
            .not => {
                try self.require(a, .boolean);
                try self.pushExpr(stack, try std.fmt.allocPrint(self.arena, "(not {s})", .{a.expr}), .boolean);
            },
        }
    }

    fn buildSelect(self: *Builder, stack: *std.ArrayList(Entry)) !void {
        const e = try pop(stack);
        const th = try pop(stack);
        const c = try pop(stack);
        try self.require(c, .boolean);
        const ty = try self.unify(th, e);
        try self.pushExpr(
            stack,
            try std.fmt.allocPrint(self.arena, "(ite {s} {s} {s})", .{ c.expr, th.expr, e.expr }),
            ty,
        );
    }

    /// Reduce an RPN denotation to a single SMT expression, inferring types.
    fn build(self: *Builder, terms: []const Term) !Entry {
        var stack: std.ArrayList(Entry) = .empty;
        for (terms) |t| {
            switch (t) {
                .imm => try self.pushLeaf(&stack, "imm"),
                .child => |i| try self.pushLeaf(&stack, try std.fmt.allocPrint(self.arena, "c{d}", .{i})),
                .local => |i| try self.pushLeaf(&stack, try std.fmt.allocPrint(self.arena, "L{d}", .{i})),
                .call_result => |i| try self.pushLeaf(&stack, try std.fmt.allocPrint(self.arena, "k{d}", .{i})),
                .binop => |k| try self.buildBinop(&stack, k),
                .unop => |k| try self.buildUnop(&stack, k),
                .select => try self.buildSelect(&stack),
                .binop_self, .unop_self => return error.Uninstantiated,
            }
        }
        if (stack.items.len != 1) return error.Malformed;
        return stack.items[0];
    }
};

/// Encode an equivalence obligation `lhs == rhs` into an SMT-LIB2 query. The
/// query asserts the *negation*: a solver answering `unsat` proves equivalence,
/// `sat` yields a counterexample. Leaves shared by the two sides become one
/// shared constant. When `want_model` is set, a trailing `(get-model)` is
/// emitted (only valid to consume after a `sat`).
pub fn encodeEquivalence(
    caller: std.mem.Allocator,
    lhs: []const Term,
    rhs: []const Term,
    want_model: bool,
) EncodeError![]u8 {
    var arena_state = std.heap.ArenaAllocator.init(caller);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var b = Builder{ .arena = arena };
    const l = try b.build(lhs);
    const r = try b.build(rhs);

    // Default any leaf never touched by a typing operator (e.g. a bare-leaf
    // denotation like `identifier` => [local 0]) to Int. After this, no entry's
    // type can be .unknown - leaf entries read a now-defaulted leaf, and every
    // non-leaf entry was built with a concrete .ty - so the two root sorts
    // compare directly. A mismatch means a genuinely ill-typed obligation.
    for (b.leaves.items) |*leaf| {
        if (leaf.ty == .unknown) leaf.ty = .int;
    }
    if (b.entryTy(l) != b.entryTy(r)) return error.TypeMismatch;

    var out: std.ArrayList(u8) = .empty;
    // A :timeout makes the `unknown` verdict reachable. Without it a hard or
    // undecidable obligation (the structurally-different equivalence mechanism 5
    // exists to admit) would run z3 unbounded and block std.process.run forever
    // instead of degrading to the non-fatal "unproven" path (see Verdict.unknown).
    // 15s mirrors the exclusion audit's ceiling.
    try out.appendSlice(arena, "(set-option :timeout 15000)\n(set-logic ALL)\n");
    for (b.leaves.items) |leaf| {
        try out.appendSlice(arena, try std.fmt.allocPrint(
            arena,
            "(declare-const {s} {s})\n",
            .{ leaf.name, leaf.ty.sortName() },
        ));
    }
    try out.appendSlice(arena, try std.fmt.allocPrint(
        arena,
        "(assert (not (= {s} {s})))\n(check-sat)\n",
        .{ l.expr, r.expr },
    ));
    if (want_model) try out.appendSlice(arena, "(get-model)\n");

    return caller.dupe(u8, out.items);
}

// ---------------------------------------------------------------------------
// Tests (pure; no solver needed).
// ---------------------------------------------------------------------------

test "parseVerdict reads the check-sat line" {
    try std.testing.expectEqual(Verdict.equivalent, parseVerdict("unsat\n"));
    try std.testing.expectEqual(Verdict.counterexample, parseVerdict("sat\n(\n (define-fun x () Int 4))\n"));
    try std.testing.expectEqual(Verdict.unknown, parseVerdict("unknown\n"));
    // a z3 error or empty/garbage output is a solver_error, NOT a benign timeout.
    try std.testing.expectEqual(Verdict.solver_error, parseVerdict("(error \"boom\")\n"));
    try std.testing.expectEqual(Verdict.solver_error, parseVerdict("   \n"));
    try std.testing.expectEqual(Verdict.solver_error, parseVerdict("garbage\n"));
}

test "parseVerdict skips benign preamble but an (error ...) wins" {
    // a startup banner or :print-success `success` lines precede the verdict.
    try std.testing.expectEqual(Verdict.equivalent, parseVerdict("Z3 4.13.0\nunsat\n"));
    try std.testing.expectEqual(Verdict.equivalent, parseVerdict("success\nsuccess\nunsat\n"));
    try std.testing.expectEqual(Verdict.counterexample, parseVerdict("success\nsat\n((x 5))\n"));
    // an error is fatal even when z3 prints `unknown` after it.
    try std.testing.expectEqual(Verdict.solver_error, parseVerdict("(error \"unsupported\")\nunknown\n"));
}

test "encode a binary_op denotation (int operands)" {
    const a = std.testing.allocator;
    // c0 c1 add
    const terms = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .add } };
    const q = try encodeEquivalence(a, &terms, &terms, false);
    defer a.free(q);
    try std.testing.expect(std.mem.indexOf(u8, q, "(declare-const c0 Int)") != null);
    try std.testing.expect(std.mem.indexOf(u8, q, "(declare-const c1 Int)") != null);
    try std.testing.expect(std.mem.indexOf(u8, q, "(+ c0 c1)") != null);
    try std.testing.expect(std.mem.indexOf(u8, q, "(check-sat)") != null);
    // a :timeout backstop must be present so the `unknown` verdict is reachable.
    try std.testing.expect(std.mem.indexOf(u8, q, ":timeout") != null);
    // self-equivalence query must not request a model.
    try std.testing.expect(std.mem.indexOf(u8, q, "get-model") == null);
}

test "type inference: not forces a Bool leaf, neg forces an Int leaf" {
    const a = std.testing.allocator;
    const not_terms = [_]Term{ .{ .child = 0 }, .{ .unop = .not } };
    const qn = try encodeEquivalence(a, &not_terms, &not_terms, false);
    defer a.free(qn);
    try std.testing.expect(std.mem.indexOf(u8, qn, "(declare-const c0 Bool)") != null);
    try std.testing.expect(std.mem.indexOf(u8, qn, "(not c0)") != null);

    const neg_terms = [_]Term{ .{ .child = 0 }, .{ .unop = .neg } };
    const qg = try encodeEquivalence(a, &neg_terms, &neg_terms, false);
    defer a.free(qg);
    try std.testing.expect(std.mem.indexOf(u8, qg, "(declare-const c0 Int)") != null);
}

test "lt produces a Bool root; ternary selects with a Bool condition" {
    const a = std.testing.allocator;
    // c0 c1 lt  -> (< c0 c1), bool root
    const lt_terms = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .lt } };
    const ql = try encodeEquivalence(a, &lt_terms, &lt_terms, false);
    defer a.free(ql);
    try std.testing.expect(std.mem.indexOf(u8, ql, "(< c0 c1)") != null);

    // c0 c1 c2 select -> cond c0 must be Bool, branches default Int.
    const sel = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .child = 2 }, .select };
    const qs = try encodeEquivalence(a, &sel, &sel, false);
    defer a.free(qs);
    try std.testing.expect(std.mem.indexOf(u8, qs, "(declare-const c0 Bool)") != null);
    try std.testing.expect(std.mem.indexOf(u8, qs, "(ite c0 c1 c2)") != null);
}

test "want_model appends get-model" {
    const a = std.testing.allocator;
    const terms = [_]Term{ .{ .child = 0 }, .{ .unop = .neg } };
    const q = try encodeEquivalence(a, &terms, &terms, true);
    defer a.free(q);
    try std.testing.expect(std.mem.indexOf(u8, q, "(get-model)") != null);
}

test "a non-structural law shares one leaf across both sides" {
    const a = std.testing.allocator;
    // neg(neg(c0)) vs c0  -- different RPN, one shared leaf c0.
    const lhs = [_]Term{ .{ .child = 0 }, .{ .unop = .neg }, .{ .unop = .neg } };
    const rhs = [_]Term{.{ .child = 0 }};
    const q = try encodeEquivalence(a, &lhs, &rhs, false);
    defer a.free(q);
    // exactly one declaration of c0.
    const first = std.mem.indexOf(u8, q, "(declare-const c0").?;
    try std.testing.expect(std.mem.indexOf(u8, q[first + 1 ..], "(declare-const c0") == null);
    try std.testing.expect(std.mem.indexOf(u8, q, "(- (- c0))") != null);
}

test "an un-instantiated parametric placeholder is rejected" {
    const a = std.testing.allocator;
    const terms = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .binop_self };
    try std.testing.expectError(error.Uninstantiated, encodeEquivalence(a, &terms, &terms, false));
}

test "a malformed RPN (leftover operands) is rejected" {
    const a = std.testing.allocator;
    const terms = [_]Term{ .{ .child = 0 }, .{ .child = 1 } }; // two values, no combiner
    try std.testing.expectError(error.Malformed, encodeEquivalence(a, &terms, &terms, false));
}

test "mismatched root sorts are rejected as ill-typed" {
    const a = std.testing.allocator;
    const lhs = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .lt } }; // Bool
    const rhs = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .add } }; // Int
    try std.testing.expectError(error.TypeMismatch, encodeEquivalence(a, &lhs, &rhs, false));
}
