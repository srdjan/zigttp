//! semantics.zig - the fourth registry: what each IR `NodeTag` means and each
//! bytecode `Opcode` computes.
//!
//! zts already pins three declarative registries (features, modules, rules),
//! each reducible to a stable hash. The meaning of the language has no such home
//! today; it lives only implicitly in `parser/codegen.zig` (lowering) and
//! `interpreter.zig` (execution). This file is the L2 on-ramp to a full semantics
//! registry: every node rule carries its denotation and lowering AS DATA here, so
//! the registry is the single source of truth and `semanticsHash` covers all of
//! it; `semantics_check.zig` is a pure interpreter of this data.
//!
//! Scope (this slice): denotation proofs for the pure, value-producing core,
//! statements marked structural-only, `call` modeled as an opaque oracle. Only
//! part of the language is specified; the drift gate pins the size of the IR and
//! bytecode alphabets (see the caveat there about non-exhaustive `Opcode`).
//!
//! The northstar (docs/zts-formal-spec-northstar.html) replaces the hand-rolled
//! symbolic executor with an SMT refinement check; this data model migrates there
//! unchanged.

const std = @import("std");
const bytecode = @import("bytecode.zig");
const ir = @import("parser/ir.zig");

pub const Opcode = bytecode.Opcode;
pub const NodeTag = ir.NodeTag;

/// Binary operators that appear in the slice's denotations.
pub const BinKind = enum { add, sub, mul, lt, eq };

/// Unary operators that appear in the slice's denotations.
pub const UnKind = enum { not, neg };

/// A symbolic term in reverse-polish (postfix) form. A denotation is a slice of
/// `Term`; symbolic execution of a lowering produces another slice; the two are
/// compared token-for-token. `child`, `local`, `call_result`, and `imm` are
/// opaque symbols standing for sub-results the checker does not look inside.
/// `binop_self`/`unop_self` are placeholders for "this node's operator", used by
/// parametric rules and substituted to a concrete `binop`/`unop` per instance
/// before any execution or comparison.
pub const Term = union(enum) {
    /// The node's own literal immediate (a lit_int's value, a lit_bool's value).
    imm,
    /// The denotation of the i-th subexpression (opaque).
    child: u8,
    /// locals[i].
    local: u8,
    /// The opaque result of the i-th call site (the FFI / oracle boundary).
    call_result: u8,
    /// Combine the top two terms with a binary operator.
    binop: BinKind,
    /// Combine the top term with a unary operator.
    unop: UnKind,
    /// Combine the top three terms (cond, then, else) into a selection.
    select,
    /// Parametric placeholder: this node's binary operator (substituted per instance).
    binop_self,
    /// Parametric placeholder: this node's unary operator (substituted per instance).
    unop_self,

    pub fn eql(a: Term, b: Term) bool {
        if (@as(std.meta.Tag(Term), a) != @as(std.meta.Tag(Term), b)) return false;
        return switch (a) {
            .imm, .select, .binop_self, .unop_self => true,
            .child => |i| i == b.child,
            .local => |i| i == b.local,
            .call_result => |i| i == b.call_result,
            .binop => |k| k == b.binop,
            .unop => |k| k == b.unop,
        };
    }
};

/// Compare two RPN denotations for structural equality.
pub fn termsEql(a: []const Term, b: []const Term) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (!x.eql(y)) return false;
    }
    return true;
}

/// One step of a straight-line lowering template. The symbolic executor runs
/// these against a stack of Term sequences to reconstruct the node's denotation.
pub const Step = union(enum) {
    /// push_0..3 / push_i8 / push_i16 / push_const: leave the node's immediate.
    push_imm,
    /// the opcodes that evaluate child i and leave its value on the stack.
    eval_child: u8,
    /// get_loc i.
    push_local: u8,
    /// a call that yields call_result(i) on top of the stack.
    call_site: u8,
    /// a stack opcode whose transition is declared in `op_rules`.
    op: Opcode,
    /// Parametric placeholder: the opcode implementing this node's operator
    /// (substituted to a concrete `op` per instance before execution).
    op_self,
};

/// How a node lowers. `branch` models cond ? then : else, whose three regions are
/// wired with control opcodes; the wiring's stack discipline is checked via the
/// arity table (mechanism 2), the value via symbolic execution (mechanism 3).
pub const Lowering = union(enum) {
    straight: []const Step,
    branch: struct {
        cond: []const Step,
        then: []const Step,
        else_: []const Step,
        wiring: []const Opcode,
    },
};

/// How a node's meaning is established.
pub const ProofKind = enum {
    /// A value-producing node: `denote` and `lower` are present and proven equal.
    value,
    /// A statement / non-value node: coverage + (no value) for this slice.
    structural,
};

/// Operator-parameterization of a value rule. `binop`/`unop` rules carry
/// `binop_self`/`unop_self`/`op_self` placeholders the checker instantiates over
/// every BinKind/UnKind via `binOpcode`/`unOpcode`.
pub const Parametric = enum { none, binop, unop };

pub const NodeRule = struct {
    tag: NodeTag,
    proof: ProofKind,
    parametric: Parametric = .none,
    denote: []const Term = &.{},
    lower: ?Lowering = null,
};

/// The opcode that implements a binary operator (the op_map the parametric proof
/// is generic over; hashed into semanticsHash so the mapping is pinned too).
pub fn binOpcode(k: BinKind) Opcode {
    return switch (k) {
        .add => .add,
        .sub => .sub,
        .mul => .mul,
        .lt => .lt,
        .eq => .eq,
    };
}

pub fn unOpcode(k: UnKind) Opcode {
    return switch (k) {
        .not => .not,
        .neg => .neg,
    };
}

/// The symbolic transition of an opcode over the Term stack.
pub const Transition = union(enum) {
    /// pop 2 (a, b), push a ++ b ++ [binop k].
    binop: BinKind,
    /// pop 1 (a), push a ++ [unop k].
    unop: UnKind,
    /// control flow (if_false / goto / ret): no value pushed.
    control,
};

pub const OpRule = struct {
    op: Opcode,
    t: Transition,
};

// The symbolic transition registry: the arithmetic / comparison / logical opcodes
// that appear as `op` steps in lowerings. Stack (push_*), local (get_loc ->
// push_local Step), control-flow, object, module, and optimized opcodes are not
// in the symbolic registry for this slice and are modeled structurally.
pub const op_rules = [_]OpRule{
    .{ .op = .add, .t = .{ .binop = .add } },
    .{ .op = .sub, .t = .{ .binop = .sub } },
    .{ .op = .mul, .t = .{ .binop = .mul } },
    .{ .op = .lt, .t = .{ .binop = .lt } },
    .{ .op = .eq, .t = .{ .binop = .eq } },
    .{ .op = .not, .t = .{ .unop = .not } },
    .{ .op = .neg, .t = .{ .unop = .neg } },
};

/// Look up an opcode's symbolic transition, if specified.
pub fn opTransition(op: Opcode) ?Transition {
    for (op_rules) |r| {
        if (r.op == op) return r.t;
    }
    return null;
}

// ---------------------------------------------------------------------------
// The slice registry. Every value rule carries denote + lower AS DATA, so
// semanticsHash covers all of it and the checker is a pure interpreter.
// ---------------------------------------------------------------------------

pub const node_rules = [_]NodeRule{
    // Literals: lower by pushing their own immediate, which is what they denote.
    .{ .tag = .lit_int, .proof = .value, .denote = &.{.imm}, .lower = .{ .straight = &.{.push_imm} } },
    .{ .tag = .lit_bool, .proof = .value, .denote = &.{.imm}, .lower = .{ .straight = &.{.push_imm} } },
    // Identifier (local read): get_loc, denotes locals[0].
    .{ .tag = .identifier, .proof = .value, .denote = &.{.{ .local = 0 }}, .lower = .{ .straight = &.{.{ .push_local = 0 }} } },
    // binary_op: generic over its operator. denote = c0 c1 <op>; lower = <c0> <c1> <op>.
    .{ .tag = .binary_op, .proof = .value, .parametric = .binop, .denote = &.{ .{ .child = 0 }, .{ .child = 1 }, .binop_self }, .lower = .{ .straight = &.{ .{ .eval_child = 0 }, .{ .eval_child = 1 }, .op_self } } },
    // unary_op: generic over its operator.
    .{ .tag = .unary_op, .proof = .value, .parametric = .unop, .denote = &.{ .{ .child = 0 }, .unop_self }, .lower = .{ .straight = &.{ .{ .eval_child = 0 }, .op_self } } },
    // ternary: cond ? then : else, denoting select(c, t, e).
    .{ .tag = .ternary, .proof = .value, .denote = &.{ .{ .child = 0 }, .{ .child = 1 }, .{ .child = 2 }, .select }, .lower = .{ .branch = .{
        .cond = &.{.{ .eval_child = 0 }},
        .then = &.{.{ .eval_child = 1 }},
        .else_ = &.{.{ .eval_child = 2 }},
        .wiring = &.{ .if_false, .goto },
    } } },
    // call: the oracle boundary - denotes an opaque call_result, lowering pushes it.
    .{ .tag = .call, .proof = .value, .denote = &.{.{ .call_result = 0 }}, .lower = .{ .straight = &.{.{ .call_site = 0 }} } },
    // Statements: covered, no value denotation in this slice.
    .{ .tag = .if_stmt, .proof = .structural },
    .{ .tag = .return_stmt, .proof = .structural },
    .{ .tag = .block, .proof = .structural },
};

/// A fused/superinstruction opcode and the base opcode sequence it must be
/// equivalent to. `fused_effect` and `base` are authored as two independent
/// fields; the checker proves they leave the same symbolic stack. For a faithful
/// concatenation superinstruction they coincide - the check exists to catch one
/// that does not (see the negative test in semantics_check.zig).
pub const Refinement = struct {
    fused: Opcode,
    fused_effect: []const Step,
    base: []const Step,
};

pub const refinements = [_]Refinement{
    .{
        .fused = .get_loc_add,
        .fused_effect = &.{ .{ .push_local = 0 }, .{ .op = .add } },
        .base = &.{ .{ .push_local = 0 }, .{ .op = .add } },
    },
};

/// Algebraic laws the language's value semantics must satisfy: equivalences
/// between *structurally different* denotations. Mechanism 3 (structural RPN
/// equality) cannot admit these - both sides have different `Term` sequences -
/// but the SMT check (mechanism 5, `semantics_smt.zig`) certifies them over the
/// value model. They are spec content (claims about the language), so they are
/// hashed into `semanticsHash` and rendered into the spec artifact. A law that
/// did NOT hold would surface as an SMT counterexample at `spec-check` time.
///
/// SOUNDNESS BOUNDARY - read before adding a law. The SMT encoder abstracts the
/// engine's number type (i32 that promotes to IEEE-754 f64 on overflow; see
/// interpreter.zig `addValues`) as the unbounded mathematical integers Z. That
/// abstraction is sound ONLY for a law whose truth does not depend on JS
/// coercion, string concatenation, truthiness, float rounding, integer overflow,
/// NaN, or signed zero. A law valid over Z but not over the engine's value
/// behavior would be falsely "proven" and folded into the signed receipt.
///
/// Associativity of addition is the canonical numeric boundary: it holds over Z
/// but FAILS on the engine, because a `child` can be a large float (e.g. a
/// mul-overflow result past 2^53) and f64 addition is not associative there.
/// Likewise, generic `+` can concatenate strings and `!` returns a boolean via
/// JS truthiness, so `a + b == b + a` and `!!x == x` are not slice-wide laws.
/// Faithful f64 and heap/string encoding is northstar work; until then, a law
/// must hold under the engine's value model, not merely under Z.
pub const Law = struct {
    name: []const u8,
    lhs: []const Term,
    rhs: []const Term,
};

// No unconditioned algebraic law survives the engine's polymorphic, coercing
// value model: every tempting equivalence is in `excluded_laws` below (machine-
// refuted). Asserting laws that hold only for a restricted operand type needs the
// reachability / type-precondition layer (the deferred faithful-model work), so
// this table is intentionally empty for now. Mechanism 5 still proves every value
// node's denote == exec(lower) and the refinements; the algebraic-law slot is
// where guarded laws will land once preconditions exist.
pub const algebraic_laws = [_]Law{};

/// Excluded laws: equivalences that are tempting but FALSE under the engine's
/// faithful value model, declared here so the exclusion is machine-checked rather
/// than only asserted in the SOUNDNESS BOUNDARY comment above. The audit
/// (`semantics_audit.zig`, via spec-check) encodes each over a faithful
/// tagged-value model and REQUIRES the solver to find a counterexample - if one
/// of these ever came back as actually holding, the build fails (the exclusion
/// was wrong, or the value model drifted). They reuse the `Law` shape; the audit
/// interprets them as non-laws.
pub const excluded_laws = [_]Law{
    // associativity of +  - holds over ℤ, fails on f64 rounding past 2^53.
    .{
        .name = "add_associative",
        .lhs = &.{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .add }, .{ .child = 2 }, .{ .binop = .add } },
        .rhs = &.{ .{ .child = 0 }, .{ .child = 1 }, .{ .child = 2 }, .{ .binop = .add }, .{ .binop = .add } },
    },
    // commutativity of +  - generic `+` concatenates strings: "a"+"b" != "b"+"a".
    .{
        .name = "add_commutative",
        .lhs = &.{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .add } },
        .rhs = &.{ .{ .child = 1 }, .{ .child = 0 }, .{ .binop = .add } },
    },
    // involution of !  - `!` coerces via truthiness, so `!!x` is a bool, not x.
    .{
        .name = "not_involution",
        .lhs = &.{ .{ .child = 0 }, .{ .unop = .not }, .{ .unop = .not } },
        .rhs = &.{.{ .child = 0 }},
    },
    // involution of unary -  - `-` coerces to number, so `-(-"5") === "5"` is
    // false (number 5 !== string "5"). Was previously asserted over the ℤ tier;
    // now machine-refuted here, removing the receipt over-claim.
    .{
        .name = "neg_involution",
        .lhs = &.{ .{ .child = 0 }, .{ .unop = .neg }, .{ .unop = .neg } },
        .rhs = &.{.{ .child = 0 }},
    },
};

/// Conformance diagnostic codes (ZTS75x). Kept as local registry data rather
/// than in rule_registry.zig: registering them there would change policyHash and
/// break the pinned policy hash. The 75x block is distinct from the 70x codes
/// already used elsewhere (e.g. the missing-sql-schema ZTS700).
pub const SpecCode = enum {
    uncovered_node,
    uncovered_opcode,
    unbalanced_lowering,
    lowering_divergence,
    refinement_divergence,
    smt_counterexample,
    smt_unencodable,
    excluded_law_holds,
    audit_solver_error,

    pub fn code(self: SpecCode) []const u8 {
        return switch (self) {
            .uncovered_node => "ZTS750",
            .uncovered_opcode => "ZTS751",
            .unbalanced_lowering => "ZTS752",
            .lowering_divergence => "ZTS753",
            .refinement_divergence => "ZTS754",
            .smt_counterexample => "ZTS755",
            .smt_unencodable => "ZTS756",
            .excluded_law_holds => "ZTS757",
            .audit_solver_error => "ZTS758",
        };
    }
};

// ---------------------------------------------------------------------------
// Drift gate.
// ---------------------------------------------------------------------------

pub fn isSpecifiedNode(tag: NodeTag) bool {
    for (node_rules) |r| {
        if (r.tag == tag) return true;
    }
    return false;
}

pub fn isSpecifiedOpcode(op: Opcode) bool {
    for (op_rules) |r| {
        if (r.op == op) return true;
    }
    return false;
}

// The pinned size of each alphabet. The comptime gate fails the build when these
// no longer match the enums - the moment a NAMED NodeTag or Opcode is added or
// removed - so the author must specify the new member or consciously re-pin.
//
// Caveat (honest): `Opcode` is a NON-EXHAUSTIVE enum (bytecode.zig `_,`), so
// `fields.len` counts only named members. An opcode wired through the reserved
// numeric range without a named member would NOT trip this gate. In practice
// opcodes are always added as named members, but the guarantee is "named
// alphabet", not "every byte value". Rename/reorder at equal size is caught by
// the receipt's irTableHash/opcodeTableHash at check time. A per-member
// rule-or-pending list plus SMT-checked coverage is the northstar's version.
pub const expected_nodes = 81;
pub const expected_opcodes = 132;

comptime {
    const n = @typeInfo(NodeTag).@"enum".fields.len;
    if (n != expected_nodes) {
        @compileError(std.fmt.comptimePrint(
            "semantics drift: NodeTag has {d} named members, registry pinned to {d}. " ++
                "An IR node was added or removed - give it a rule in semantics.zig (or decide it is structural), then set expected_nodes = {d}.",
            .{ n, expected_nodes, n },
        ));
    }
    const m = @typeInfo(Opcode).@"enum".fields.len;
    if (m != expected_opcodes) {
        @compileError(std.fmt.comptimePrint(
            "semantics drift: Opcode has {d} named members, registry pinned to {d}. " ++
                "A bytecode opcode was added or removed - give it a transition in semantics.zig op_rules (or acknowledge it), then set expected_opcodes = {d}.",
            .{ m, expected_opcodes, m },
        ));
    }
}

// ---------------------------------------------------------------------------
// Hashes. `semanticsHash` mirrors rule_registry.policyHash: a stable SHA-256 over
// the canonicalized registry, covering every rule's denotation AND lowering, the
// operator->opcode maps, and the refinements - so a change to any declared
// meaning moves the hash. The table hashes pin the alphabets the registry is
// written against.
// ---------------------------------------------------------------------------

fn hashTerms(hasher: *std.crypto.hash.sha2.Sha256, terms: []const Term) void {
    for (terms) |t| {
        const tag_byte: u8 = @intCast(@intFromEnum(@as(std.meta.Tag(Term), t)));
        hasher.update(&[_]u8{tag_byte});
        switch (t) {
            .imm, .select, .binop_self, .unop_self => {},
            .child => |i| hasher.update(&[_]u8{i}),
            .local => |i| hasher.update(&[_]u8{i}),
            .call_result => |i| hasher.update(&[_]u8{i}),
            .binop => |k| hasher.update(&[_]u8{@intCast(@intFromEnum(k))}),
            .unop => |k| hasher.update(&[_]u8{@intCast(@intFromEnum(k))}),
        }
    }
}

fn hashU64(hasher: *std.crypto.hash.sha2.Sha256, value: u64) void {
    var bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &bytes, value, .little);
    hasher.update(&bytes);
}

fn hashSteps(hasher: *std.crypto.hash.sha2.Sha256, steps: []const Step) void {
    for (steps) |s| {
        const tag_byte: u8 = @intCast(@intFromEnum(@as(std.meta.Tag(Step), s)));
        hasher.update(&[_]u8{tag_byte});
        switch (s) {
            .push_imm, .op_self => {},
            .eval_child => |i| hasher.update(&[_]u8{i}),
            .push_local => |i| hasher.update(&[_]u8{i}),
            .call_site => |i| hasher.update(&[_]u8{i}),
            .op => |o| hasher.update(@tagName(o)),
        }
    }
}

fn hashTransition(hasher: *std.crypto.hash.sha2.Sha256, t: Transition) void {
    const tag_byte: u8 = @intCast(@intFromEnum(@as(std.meta.Tag(Transition), t)));
    hasher.update(&[_]u8{tag_byte});
    switch (t) {
        .binop => |k| hashU64(hasher, @intCast(@intFromEnum(k))),
        .unop => |k| hashU64(hasher, @intCast(@intFromEnum(k))),
        .control => {},
    }
}

fn hashLowering(hasher: *std.crypto.hash.sha2.Sha256, lower: Lowering) void {
    switch (lower) {
        .straight => |steps| {
            hasher.update("s");
            hashSteps(hasher, steps);
        },
        .branch => |b| {
            hasher.update("b");
            hashSteps(hasher, b.cond);
            hashSteps(hasher, b.then);
            hashSteps(hasher, b.else_);
            for (b.wiring) |w| hasher.update(@tagName(w));
        },
    }
}

var cached_semantics_hash: ?[64]u8 = null;

pub fn semanticsHash() [64]u8 {
    if (cached_semantics_hash) |h| return h;
    const h = computeSemanticsHash();
    cached_semantics_hash = h;
    return h;
}

fn computeSemanticsHash() [64]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    for (node_rules) |r| {
        hasher.update(@tagName(r.tag));
        hasher.update("\x00");
        hasher.update(@tagName(r.proof));
        hasher.update("\x00");
        hasher.update(@tagName(r.parametric));
        hasher.update("\x00");
        hashTerms(&hasher, r.denote);
        if (r.lower) |l| hashLowering(&hasher, l);
        hasher.update("\x01");
    }
    for (op_rules) |r| {
        hasher.update(@tagName(r.op));
        hasher.update("\x00");
        hashTransition(&hasher, r.t);
        hasher.update("\x01");
    }
    // operator -> opcode maps (so changing binOpcode/unOpcode moves the hash).
    inline for (std.meta.tags(BinKind)) |k| {
        hasher.update(@tagName(k));
        hasher.update(@tagName(binOpcode(k)));
    }
    inline for (std.meta.tags(UnKind)) |k| {
        hasher.update(@tagName(k));
        hasher.update(@tagName(unOpcode(k)));
    }
    for (refinements) |rf| {
        hasher.update(@tagName(rf.fused));
        hasher.update("\x00");
        hashSteps(&hasher, rf.fused_effect);
        hasher.update("\x00");
        hashSteps(&hasher, rf.base);
        hasher.update("\x01");
    }
    // algebraic laws (the SMT-certified non-structural equivalences) are spec
    // content too, so a change to any law moves the hash.
    for (algebraic_laws) |law| {
        hasher.update(law.name);
        hasher.update("\x00");
        hashTerms(&hasher, law.lhs);
        hasher.update("\x00");
        hashTerms(&hasher, law.rhs);
        hasher.update("\x01");
    }
    // excluded laws (machine-refuted non-laws) are spec content too: the claim
    // "these are NOT laws" is part of the soundness boundary.
    hasher.update("excluded\x00");
    for (excluded_laws) |law| {
        hasher.update(law.name);
        hasher.update("\x00");
        hashTerms(&hasher, law.lhs);
        hasher.update("\x00");
        hashTerms(&hasher, law.rhs);
        hasher.update("\x01");
    }
    const digest = hasher.finalResult();
    return std.fmt.bytesToHex(digest, .lower);
}

/// SHA-256 over the IR alphabet (every named `NodeTag` member, in order).
pub fn irTableHash() [64]u8 {
    return enumTableHash(NodeTag);
}

/// SHA-256 over the bytecode alphabet (every named `Opcode` member and value).
pub fn opcodeTableHash() [64]u8 {
    return enumTableHash(Opcode);
}

fn enumTableHash(comptime E: type) [64]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    inline for (@typeInfo(E).@"enum".fields) |f| {
        hasher.update(f.name);
        hasher.update("\x00");
        hashU64(&hasher, @intCast(f.value));
        hasher.update("\x00");
    }
    const digest = hasher.finalResult();
    return std.fmt.bytesToHex(digest, .lower);
}

fn transitionHashForTest(t: Transition) [64]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashTransition(&hasher, t);
    const digest = hasher.finalResult();
    return std.fmt.bytesToHex(digest, .lower);
}

/// Coverage counts for reporting.
pub const Coverage = struct {
    nodes_total: usize,
    nodes_specified: usize,
    opcodes_total: usize,
    opcodes_specified: usize,
};

pub fn coverage() Coverage {
    return .{
        .nodes_total = @typeInfo(NodeTag).@"enum".fields.len,
        .nodes_specified = node_rules.len,
        .opcodes_total = @typeInfo(Opcode).@"enum".fields.len,
        .opcodes_specified = op_rules.len,
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "drift gate is pinned to the real enums" {
    try std.testing.expectEqual(expected_nodes, @typeInfo(NodeTag).@"enum".fields.len);
    try std.testing.expectEqual(expected_opcodes, @typeInfo(Opcode).@"enum".fields.len);

    const c = coverage();
    try std.testing.expect(c.nodes_specified > 0);
    try std.testing.expect(c.nodes_specified <= c.nodes_total);
    try std.testing.expect(c.opcodes_specified > 0);
    try std.testing.expect(c.opcodes_specified <= c.opcodes_total);
}

test "hashes are deterministic and well-formed" {
    const a = semanticsHash();
    const b = semanticsHash();
    try std.testing.expectEqualSlices(u8, &a, &b);
    try std.testing.expectEqual(@as(usize, 64), a.len);
    try std.testing.expect(!std.mem.eql(u8, &irTableHash(), &opcodeTableHash()));
}

test "semantics hash input distinguishes transition payloads" {
    const add = transitionHashForTest(.{ .binop = .add });
    const sub = transitionHashForTest(.{ .binop = .sub });
    const not = transitionHashForTest(.{ .unop = .not });

    try std.testing.expect(!std.mem.eql(u8, &add, &sub));
    try std.testing.expect(!std.mem.eql(u8, &add, &not));
}

fn hasLawForTest(name: []const u8) bool {
    for (algebraic_laws) |law| {
        if (std.mem.eql(u8, law.name, name)) return true;
    }
    return false;
}

fn hasExcludedForTest(name: []const u8) bool {
    for (excluded_laws) |law| {
        if (std.mem.eql(u8, law.name, name)) return true;
    }
    return false;
}

test "JS coercive identities are excluded, not asserted" {
    // None of these hold under the engine's polymorphic, coercing value model, so
    // none may be an asserted algebraic law; each must instead be an excluded law
    // the faithful audit refutes. neg_involution moved here from the law table
    // once the audit existed (string coercion: -(-"5") !== "5").
    for ([_][]const u8{ "add_commutative", "not_involution", "neg_involution", "add_associative" }) |name| {
        try std.testing.expect(!hasLawForTest(name));
        try std.testing.expect(hasExcludedForTest(name));
    }
    // The law table is intentionally empty until the reachability layer lands.
    try std.testing.expectEqual(@as(usize, 0), algebraic_laws.len);
}

test "table hashes include enum discriminants" {
    const A = enum(u8) {
        first = 1,
        second = 2,
    };
    const B = enum(u8) {
        first = 2,
        second = 1,
    };

    const a = enumTableHash(A);
    const b = enumTableHash(B);
    try std.testing.expect(!std.mem.eql(u8, &a, &b));
}

test "every value rule carries denotation and lowering" {
    // The structural fix: meaning lives in the registry, not the checker - so the
    // hash covers it. Assert no value rule is a bare marker.
    for (node_rules) |r| {
        switch (r.proof) {
            .value => {
                try std.testing.expect(r.denote.len > 0);
                try std.testing.expect(r.lower != null);
            },
            .structural => {
                try std.testing.expect(r.denote.len == 0);
                try std.testing.expect(r.lower == null);
            },
        }
    }
}

test "term structural equality" {
    try std.testing.expect(Term.eql(.imm, .imm));
    try std.testing.expect(Term.eql(.{ .child = 1 }, .{ .child = 1 }));
    try std.testing.expect(!Term.eql(.{ .child = 1 }, .{ .child = 2 }));
    try std.testing.expect(!Term.eql(.imm, .select));
    try std.testing.expect(termsEql(&.{.{ .child = 0 }}, &.{.{ .child = 0 }}));
    try std.testing.expect(!termsEql(&.{.{ .child = 0 }}, &.{.{ .child = 1 }}));
}
