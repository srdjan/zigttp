//! semantics_check.zig - the L2 binding mechanisms over the semantics registry.
//! A pure interpreter of `semantics.node_rules` / `op_rules` / `refinements`:
//! the meaning lives in the registry (and is covered by semanticsHash); this file
//! only checks it.
//!
//! Four mechanisms, weakest-but-total to strongest-but-scoped:
//!   1. Table coverage   - the IR/bytecode alphabet size is pinned at comptime
//!                         (the drift gate in semantics.zig; reported here).
//!   2. Stack-effect      - every value lowering nets exactly one stack value,
//!                         using the existing `bytecode.getOpcodeInfo` arity table,
//!                         failing loud on any opcode outside the symbolic registry.
//!   3. Symbolic lowering - exec(lower(node)) must equal denote(node), proven by
//!                         structural equality of RPN denotations. Includes the
//!                         refinement check (a fused opcode equals its base sequence).
//!   4. Differential      - the registry's denotations run against the REAL
//!                         compiler on a corpus (semantics_corpus.zig). Closes
//!                         the gap mechanism 3 cannot see: that the declared
//!                         lowering matches what codegen actually emits.
//!
//! The signed `kind=semantics` receipt (sign/verify round-trip proven here with an
//! ephemeral key) is emitted under the persistent attest identity by the runtime
//! probe `semantics_probe_lib.zig`, injected like the workflow/link receipts; the
//! keyless `zigts` binary prints the unsigned hash only.

const std = @import("std");
const semantics = @import("semantics.zig");
const semantics_smt = @import("semantics_smt.zig");
const bytecode = @import("bytecode.zig");

const Term = semantics.Term;
const Step = semantics.Step;
const Lowering = semantics.Lowering;
const Opcode = semantics.Opcode;
const Transition = semantics.Transition;
const BinKind = semantics.BinKind;
const UnKind = semantics.UnKind;
const Ed25519 = std.crypto.sign.Ed25519;

// ---------------------------------------------------------------------------
// Symbolic executor: run a lowering against a stack of RPN denotations.
// ---------------------------------------------------------------------------

const SymStack = struct {
    items: std.ArrayList([]const Term) = .empty,
    alloc: std.mem.Allocator,

    fn push(self: *SymStack, seq: []const Term) !void {
        try self.items.append(self.alloc, seq);
    }
    fn pop(self: *SymStack) ?[]const Term {
        if (self.items.items.len == 0) return null;
        const v = self.items.items[self.items.items.len - 1];
        self.items.items.len -= 1;
        return v;
    }
};

/// Run a straight-line step sequence; returns the single resulting denotation,
/// or null if the lowering is malformed (unknown opcode, wrong arity, leftover
/// `op_self` placeholder, or it does not leave exactly one value on the stack).
fn execStraight(alloc: std.mem.Allocator, steps: []const Step) !?[]const Term {
    var stack = SymStack{ .alloc = alloc };
    for (steps) |s| {
        switch (s) {
            .push_imm => try stack.push(try alloc.dupe(Term, &.{.imm})),
            .eval_child => |i| try stack.push(try alloc.dupe(Term, &.{.{ .child = i }})),
            .push_local => |i| try stack.push(try alloc.dupe(Term, &.{.{ .local = i }})),
            .call_site => |i| try stack.push(try alloc.dupe(Term, &.{.{ .call_result = i }})),
            .op_self => return null, // must be substituted before execution
            .op => |o| {
                const tr = semantics.opTransition(o) orelse return null;
                switch (tr) {
                    .binop => |k| {
                        const b = stack.pop() orelse return null;
                        const a = stack.pop() orelse return null;
                        try stack.push(try std.mem.concat(alloc, Term, &.{ a, b, &.{.{ .binop = k }} }));
                    },
                    .unop => |k| {
                        const a = stack.pop() orelse return null;
                        try stack.push(try std.mem.concat(alloc, Term, &.{ a, &.{.{ .unop = k }} }));
                    },
                    .control => {},
                }
            },
        }
    }
    if (stack.items.items.len != 1) return null;
    return stack.items.items[0];
}

/// Net stack delta of a straight-line lowering, using the existing arity table.
/// Returns null (fail loud) for any `op` step outside the symbolic registry or a
/// leftover `op_self`, so a malformed lowering cannot be silently treated as a
/// no-op. A value lowering must net +1.
fn stackDelta(steps: []const Step) ?i32 {
    var delta: i32 = 0;
    for (steps) |s| {
        switch (s) {
            .push_imm, .eval_child, .push_local, .call_site => delta += 1,
            .op_self => return null,
            .op => |o| {
                if (semantics.opTransition(o) == null) return null;
                const info = bytecode.getOpcodeInfo(o);
                delta += @as(i32, info.n_push) - @as(i32, info.n_pop);
            },
        }
    }
    return delta;
}

// ---------------------------------------------------------------------------
// Parametric instantiation: substitute the per-node operator placeholders.
// ---------------------------------------------------------------------------

fn instantiateTerms(scratch: std.mem.Allocator, terms: []const Term, binop: ?BinKind, unop: ?UnKind) ![]const Term {
    const out = try scratch.alloc(Term, terms.len);
    for (terms, 0..) |t, i| {
        out[i] = switch (t) {
            .binop_self => Term{ .binop = binop.? },
            .unop_self => Term{ .unop = unop.? },
            else => t,
        };
    }
    return out;
}

fn instantiateSteps(scratch: std.mem.Allocator, steps: []const Step, op: Opcode) ![]const Step {
    const out = try scratch.alloc(Step, steps.len);
    for (steps, 0..) |s, i| {
        out[i] = switch (s) {
            .op_self => Step{ .op = op },
            else => s,
        };
    }
    return out;
}

// ---------------------------------------------------------------------------
// Result types
// ---------------------------------------------------------------------------

pub const Counterexample = struct {
    code: semantics.SpecCode,
    where: []const u8,
    message: []const u8,
};

pub const CheckResult = struct {
    arena: std.heap.ArenaAllocator,
    nodes_proven: usize = 0,
    nodes_structural: usize = 0,
    binop_instances: usize = 0,
    unop_instances: usize = 0,
    refinements_proven: usize = 0,
    stack_effect_checked: usize = 0,
    failures: std.ArrayList(Counterexample) = .empty,

    pub fn ok(self: *const CheckResult) bool {
        return self.failures.items.len == 0;
    }
    pub fn deinit(self: *CheckResult) void {
        self.arena.deinit();
    }
    fn fail(self: *CheckResult, code: semantics.SpecCode, where: []const u8, message: []const u8) !void {
        const a = self.arena.allocator();
        try self.failures.append(a, .{
            .code = code,
            .where = try a.dupe(u8, where),
            .message = try a.dupe(u8, message),
        });
    }
};

fn termsToString(alloc: std.mem.Allocator, terms: []const Term) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    for (terms, 0..) |t, idx| {
        if (idx != 0) try buf.append(alloc, ' ');
        switch (t) {
            .imm => try buf.appendSlice(alloc, "imm"),
            .child => |i| try buf.appendSlice(alloc, try std.fmt.allocPrint(alloc, "c{d}", .{i})),
            .local => |i| try buf.appendSlice(alloc, try std.fmt.allocPrint(alloc, "L{d}", .{i})),
            .call_result => |i| try buf.appendSlice(alloc, try std.fmt.allocPrint(alloc, "call{d}", .{i})),
            .binop => |k| try buf.appendSlice(alloc, @tagName(k)),
            .unop => |k| try buf.appendSlice(alloc, try std.fmt.allocPrint(alloc, "{s}u", .{@tagName(k)})),
            .select => try buf.appendSlice(alloc, "?:"),
            .binop_self => try buf.appendSlice(alloc, "<binop>"),
            .unop_self => try buf.appendSlice(alloc, "<unop>"),
        }
    }
    return buf.items;
}

// ---------------------------------------------------------------------------
// Mechanism 2 + 3 per value rule.
// ---------------------------------------------------------------------------

/// Prove a single, fully-instantiated value rule: stack-effect (+1) and symbolic
/// equality of exec(lower) with denote.
fn proveValueRule(
    result: *CheckResult,
    scratch: std.mem.Allocator,
    where: []const u8,
    denote: []const Term,
    lower: Lowering,
) !void {
    switch (lower) {
        .straight => |steps| {
            result.stack_effect_checked += 1;
            const delta = stackDelta(steps) orelse {
                try result.fail(.unbalanced_lowering, where, "lowering uses an opcode with no symbolic transition");
                return;
            };
            if (delta != 1) {
                const msg = try std.fmt.allocPrint(scratch, "lowering nets {d} stack values, expected 1", .{delta});
                try result.fail(.unbalanced_lowering, where, msg);
                return;
            }
            const got = (try execStraight(scratch, steps)) orelse {
                try result.fail(.lowering_divergence, where, "lowering does not produce a single well-formed value");
                return;
            };
            if (!semantics.termsEql(got, denote)) {
                const e = try termsToString(scratch, denote);
                const g = try termsToString(scratch, got);
                const msg = try std.fmt.allocPrint(scratch, "denote=[{s}] but lower=>[{s}]", .{ e, g });
                try result.fail(.lowering_divergence, where, msg);
                return;
            }
        },
        .branch => |b| {
            // Each region must net +1, and the wiring must consume the condition
            // exactly: cond(+1) + wiring(net) == 0. This models if_false's pop,
            // not just that wiring opcodes push nothing.
            const cd = stackDelta(b.cond) orelse return result.fail(.unbalanced_lowering, where, "cond region uses an unspecified opcode");
            const td = stackDelta(b.then) orelse return result.fail(.unbalanced_lowering, where, "then region uses an unspecified opcode");
            const ed = stackDelta(b.else_) orelse return result.fail(.unbalanced_lowering, where, "else region uses an unspecified opcode");
            result.stack_effect_checked += 3;
            if (cd != 1 or td != 1 or ed != 1) {
                try result.fail(.unbalanced_lowering, where, "a branch region does not net one value");
                return;
            }
            var wiring_delta: i32 = 0;
            for (b.wiring) |w| {
                const info = bytecode.getOpcodeInfo(w);
                if (info.n_push != 0) {
                    try result.fail(.unbalanced_lowering, where, "branch wiring opcode pushes a value");
                    return;
                }
                wiring_delta += @as(i32, info.n_push) - @as(i32, info.n_pop);
            }
            if (cd + wiring_delta != 0) {
                try result.fail(.unbalanced_lowering, where, "branch wiring does not consume the condition");
                return;
            }
            const c = (try execStraight(scratch, b.cond)) orelse return result.fail(.lowering_divergence, where, "cond region malformed");
            const t = (try execStraight(scratch, b.then)) orelse return result.fail(.lowering_divergence, where, "then region malformed");
            const e = (try execStraight(scratch, b.else_)) orelse return result.fail(.lowering_divergence, where, "else region malformed");
            const got = try std.mem.concat(scratch, Term, &.{ c, t, e, &.{.select} });
            if (!semantics.termsEql(got, denote)) {
                try result.fail(.lowering_divergence, where, "branch denotation mismatch");
                return;
            }
        },
    }
    result.nodes_proven += 1;
}

/// Refinement: a fused opcode's effect must equal its base step sequence, both
/// run from the same symbolic initial stack. Models `get_loc_add == get_loc; add`.
pub fn proveRefinement(scratch: std.mem.Allocator, fused_effect: []const Step, base: []const Step) !bool {
    // Seed an initial operand (`child 0`) already on the stack, since a fused op
    // applies to a value produced earlier.
    const seed = [_]Step{.{ .eval_child = 0 }};
    const fused_full = try std.mem.concat(scratch, Step, &.{ &seed, fused_effect });
    const base_full = try std.mem.concat(scratch, Step, &.{ &seed, base });
    const f = (try execStraight(scratch, fused_full)) orelse return false;
    const b = (try execStraight(scratch, base_full)) orelse return false;
    return semantics.termsEql(f, b);
}

// ---------------------------------------------------------------------------
// Mechanism 5: SMT equivalence (the northstar's Alive2-style refinement check).
//
// Mechanism 3 proves exec(lower) == denote by *structural* RPN equality, which
// can only admit a lowering syntactically identical to the denotation. Mechanism
// 5 lifts the SAME registry data to a semantic check: it asks an SMT solver
// whether the two sides agree on ALL inputs, so it admits structurally-different
// but semantically-equal obligations - the refinements and algebraic laws that
// mechanism 3 cannot. The pure encoder lives in semantics_smt.zig; the solver is
// injected (the native CLI spawns z3) so std.process.Child never enters this
// (wasm-reachable) module. When no solver is injected the check is skipped and
// mechanism 3's structural guarantee stands - so spec-check still works with no
// z3 (CI, the wasm build, these tests).
//
// Outcome policy (each obligation maps to exactly one):
//   - `equivalent`     -> proved.
//   - `counterexample` -> FATAL: the law/refinement is false in the value model
//                         (ZTS755). This is the only positive evidence of a bug.
//   - `unknown`        -> UNPROVEN, non-fatal. The solver could not decide or
//                         could not run (missing/broken mid-run, undecidable
//                         fragment, unwritable scratch dir). That is not evidence
//                         the law is false - it is the same situation as "no z3
//                         installed", so it is reported but does not fail the
//                         build. The receipt records proved < total honestly.
//   - encode error     -> FATAL: an ill-typed / malformed obligation is a spec
//                         authoring bug, distinct from a counterexample (ZTS756).
// Only `counterexample` and encode errors are failures; unknown is unproven.
// ---------------------------------------------------------------------------

pub const SmtObligation = struct {
    where: []const u8,
    lhs: []const Term,
    rhs: []const Term,
};

const ObligationError = error{ MalformedObligation, OutOfMemory };

/// Symbolically execute a lowering (straight or branch) to its denotation, the
/// same reconstruction proveValueRule does for mechanism 3. Returns null on a
/// malformed lowering. Shared by the SMT obligation collector.
fn execLowering(scratch: std.mem.Allocator, lower: Lowering) !?[]const Term {
    switch (lower) {
        .straight => |steps| return execStraight(scratch, steps),
        .branch => |b| {
            const c = (try execStraight(scratch, b.cond)) orelse return null;
            const t = (try execStraight(scratch, b.then)) orelse return null;
            const e = (try execStraight(scratch, b.else_)) orelse return null;
            return try std.mem.concat(scratch, Term, &.{ c, t, e, &.{.select} });
        },
    }
}

/// Every equivalence obligation mechanism 5 discharges: each value node's
/// `denote == exec(lower)` (parametric rules instantiated over every operator),
/// each refinement (exec(fused) vs exec(base)), and each declared algebraic law.
/// The per-node obligations are structurally identical today, so SMT proves them
/// trivially - but expressing the full value-rule set in the semantic check makes
/// mechanism 5 a superset of mechanism 3 and catches the day a lowering stops
/// being a syntactic transliteration of its denotation.
pub fn collectSmtObligations(arena: std.mem.Allocator) ObligationError![]SmtObligation {
    var list: std.ArrayList(SmtObligation) = .empty;

    // Per-node value rules: denote vs exec(lower).
    for (semantics.node_rules) |rule| {
        if (rule.proof != .value) continue;
        switch (rule.parametric) {
            .none => {
                const got = (try execLowering(arena, rule.lower.?)) orelse return error.MalformedObligation;
                try list.append(arena, .{ .where = @tagName(rule.tag), .lhs = rule.denote, .rhs = got });
            },
            .binop => {
                const steps = rule.lower.?.straight;
                inline for (std.meta.tags(BinKind)) |k| {
                    const d = try instantiateTerms(arena, rule.denote, k, null);
                    const l = Lowering{ .straight = try instantiateSteps(arena, steps, semantics.binOpcode(k)) };
                    const got = (try execLowering(arena, l)) orelse return error.MalformedObligation;
                    const where = try std.fmt.allocPrint(arena, "{s}[{s}]", .{ @tagName(rule.tag), @tagName(k) });
                    try list.append(arena, .{ .where = where, .lhs = d, .rhs = got });
                }
            },
            .unop => {
                const steps = rule.lower.?.straight;
                inline for (std.meta.tags(UnKind)) |k| {
                    const d = try instantiateTerms(arena, rule.denote, null, k);
                    const l = Lowering{ .straight = try instantiateSteps(arena, steps, semantics.unOpcode(k)) };
                    const got = (try execLowering(arena, l)) orelse return error.MalformedObligation;
                    const where = try std.fmt.allocPrint(arena, "{s}[{s}]", .{ @tagName(rule.tag), @tagName(k) });
                    try list.append(arena, .{ .where = where, .lhs = d, .rhs = got });
                }
            },
        }
    }

    // Refinements: a fused opcode equals its base sequence (translation validation).
    const seed = [_]Step{.{ .eval_child = 0 }};
    for (semantics.refinements) |rf| {
        const fused_full = try std.mem.concat(arena, Step, &.{ &seed, rf.fused_effect });
        const base_full = try std.mem.concat(arena, Step, &.{ &seed, rf.base });
        const f = (try execStraight(arena, fused_full)) orelse return error.MalformedObligation;
        const b = (try execStraight(arena, base_full)) orelse return error.MalformedObligation;
        try list.append(arena, .{ .where = @tagName(rf.fused), .lhs = f, .rhs = b });
    }

    // Algebraic laws: structurally-different equivalences (mechanism 3 cannot see).
    for (semantics.algebraic_laws) |law| {
        try list.append(arena, .{ .where = law.name, .lhs = law.lhs, .rhs = law.rhs });
    }
    return list.items;
}

/// A solver call: encode-then-decide. Injected by the native CLI; returns a
/// total verdict (the wrapper maps any solver/spawn/IO error to `.unknown`).
pub const SolveFn = *const fn (query: []const u8, alloc: std.mem.Allocator) semantics_smt.Verdict;

pub const SmtResult = struct {
    arena: std.heap.ArenaAllocator,
    available: bool = false,
    proved: usize = 0,
    /// Obligations the solver could neither prove nor refute (undecided, or the
    /// solver could not run). Non-fatal: reported, not a failure.
    unproven: usize = 0,
    total: usize = 0,
    failures: std.ArrayList(Counterexample) = .empty,

    /// A failure is a genuine counterexample (ZTS755) or an unencodable
    /// obligation (ZTS756). `unproven` is NOT a failure.
    pub fn ok(self: *const SmtResult) bool {
        return self.failures.items.len == 0;
    }
    pub fn deinit(self: *SmtResult) void {
        self.arena.deinit();
    }
};

/// Run mechanism 5 over every obligation. `solve == null` => skipped (no z3).
pub fn runSmt(allocator: std.mem.Allocator, solve: ?SolveFn) !SmtResult {
    var result = SmtResult{ .arena = std.heap.ArenaAllocator.init(allocator) };
    errdefer result.deinit();

    var scratch_arena = std.heap.ArenaAllocator.init(allocator);
    defer scratch_arena.deinit();
    const scratch = scratch_arena.allocator();

    const obligations = try collectSmtObligations(scratch);
    result.total = obligations.len;

    const solver = solve orelse return result; // available stays false: skipped.
    result.available = true;

    try proveSmtObligations(&result, scratch, obligations, solver);
    return result;
}

fn proveSmtObligations(
    result: *SmtResult,
    scratch: std.mem.Allocator,
    obligations: []const SmtObligation,
    solver: SolveFn,
) !void {
    const a = result.arena.allocator();
    for (obligations) |ob| {
        const query = semantics_smt.encodeEquivalence(scratch, ob.lhs, ob.rhs, false) catch {
            // An ill-typed / malformed obligation is a spec-authoring bug, not a
            // value-equivalence counterexample. Fail loud under its own code.
            try result.failures.append(a, .{
                .code = .smt_unencodable,
                .where = try a.dupe(u8, ob.where),
                .message = try a.dupe(u8, "obligation could not be encoded for SMT (ill-typed or malformed law)"),
            });
            continue;
        };
        switch (solver(query, scratch)) {
            .equivalent => result.proved += 1,
            .counterexample => try result.failures.append(a, .{
                .code = .smt_counterexample,
                .where = try a.dupe(u8, ob.where),
                .message = try a.dupe(u8, "SMT found a counterexample: the two sides are not equivalent"),
            }),
            // Could not decide or could not run (undecidable fragment, or the
            // solver was unavailable / errored mid-run). Same situation as no z3:
            // unproven, reported, not a failure.
            .unknown => result.unproven += 1,
        }
    }
}

// ---------------------------------------------------------------------------
// Driver: a pure interpreter of the registry.
// ---------------------------------------------------------------------------

pub fn runCheck(allocator: std.mem.Allocator) !CheckResult {
    var result = CheckResult{ .arena = std.heap.ArenaAllocator.init(allocator) };
    errdefer result.deinit();

    var scratch_arena = std.heap.ArenaAllocator.init(allocator);
    defer scratch_arena.deinit();
    const scratch = scratch_arena.allocator();

    for (semantics.node_rules) |rule| {
        const where = @tagName(rule.tag);
        switch (rule.proof) {
            .structural => result.nodes_structural += 1,
            .value => switch (rule.parametric) {
                .none => try proveValueRule(&result, scratch, where, rule.denote, rule.lower.?),
                .binop => {
                    const steps = rule.lower.?.straight; // parametric rules are straight in this slice
                    inline for (std.meta.tags(BinKind)) |k| {
                        const d = try instantiateTerms(scratch, rule.denote, k, null);
                        const l = Lowering{ .straight = try instantiateSteps(scratch, steps, semantics.binOpcode(k)) };
                        const before = result.nodes_proven;
                        try proveValueRule(&result, scratch, where, d, l);
                        if (result.nodes_proven > before) result.binop_instances += 1;
                    }
                },
                .unop => {
                    const steps = rule.lower.?.straight;
                    inline for (std.meta.tags(UnKind)) |k| {
                        const d = try instantiateTerms(scratch, rule.denote, null, k);
                        const l = Lowering{ .straight = try instantiateSteps(scratch, steps, semantics.unOpcode(k)) };
                        const before = result.nodes_proven;
                        try proveValueRule(&result, scratch, where, d, l);
                        if (result.nodes_proven > before) result.unop_instances += 1;
                    }
                },
            },
        }
    }

    // Refinements: each fused opcode must equal its declared base sequence.
    for (semantics.refinements) |rf| {
        if (try proveRefinement(scratch, rf.fused_effect, rf.base)) {
            result.refinements_proven += 1;
        } else {
            try result.fail(.refinement_divergence, @tagName(rf.fused), "fused opcode does not equal its base sequence");
        }
    }

    return result;
}

// ---------------------------------------------------------------------------
// Receipt: kind=semantics, signed (Ed25519 JWS). Round-trip proven in tests.
// ---------------------------------------------------------------------------

pub const Receipt = struct {
    semantics_hash: [64]u8,
    ir_table_hash: [64]u8,
    opcode_table_hash: [64]u8,
    nodes_proven: u32,
    nodes_total: u32,
    opcodes_specified: u32,
    opcodes_total: u32,
    binop_instances: u32,
    unop_instances: u32,
    refinements_proven: u32,
    // Mechanism 4 (differential corpus vs the real compiler). The counts are
    // supplied by the caller, which owns both the check and the corpus results -
    // this module stays free of the engine-dependent corpus.
    differential_passed: u32,
    differential_total: u32,
    // Mechanism 5 (SMT equivalence). smt_available is 0 when no solver was
    // injected (the obligations were skipped); the proof rests on mechanisms 1-4.
    smt_available: u32,
    smt_proved: u32,
    smt_total: u32,
    failures: u32,
};

/// Mechanism-5 summary the caller threads into the receipt (the CLI owns the
/// injected solver, so this module does not build it).
pub const SmtSummary = struct {
    available: bool,
    proved: u32,
    total: u32,
};

pub const DifferentialSummary = struct {
    passed: u32,
    total: u32,
};

pub const ReceiptInput = struct {
    differential: DifferentialSummary,
    smt: SmtSummary,
};

pub fn buildReceiptFromInput(result: *const CheckResult, input: ReceiptInput) Receipt {
    const cov = semantics.coverage();
    return .{
        .semantics_hash = semantics.semanticsHash(),
        .ir_table_hash = semantics.irTableHash(),
        .opcode_table_hash = semantics.opcodeTableHash(),
        .nodes_proven = @intCast(result.nodes_proven),
        .nodes_total = @intCast(cov.nodes_total),
        .opcodes_specified = @intCast(cov.opcodes_specified),
        .opcodes_total = @intCast(cov.opcodes_total),
        .binop_instances = @intCast(result.binop_instances),
        .unop_instances = @intCast(result.unop_instances),
        .refinements_proven = @intCast(result.refinements_proven),
        .differential_passed = input.differential.passed,
        .differential_total = input.differential.total,
        .smt_available = @intFromBool(input.smt.available),
        .smt_proved = input.smt.proved,
        .smt_total = input.smt.total,
        .failures = @intCast(result.failures.items.len),
    };
}

pub fn buildReceipt(
    result: *const CheckResult,
    differential_passed: u32,
    differential_total: u32,
    smt: SmtSummary,
) Receipt {
    return buildReceiptFromInput(result, .{
        .differential = .{ .passed = differential_passed, .total = differential_total },
        .smt = smt,
    });
}

fn payloadString(allocator: std.mem.Allocator, r: Receipt) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "zigttp-semantics-v3\n{s}\n{s}\n{s}\n{d}\n{d}\n{d}\n{d}\n{d}\n{d}\n{d}\n{d}\n{d}\n{d}\n{d}\n{d}\n{d}",
        .{
            r.semantics_hash,     r.ir_table_hash,       r.opcode_table_hash,
            r.nodes_proven,       r.nodes_total,         r.opcodes_specified,
            r.opcodes_total,      r.binop_instances,     r.unop_instances,
            r.refinements_proven, r.differential_passed, r.differential_total,
            r.smt_available,      r.smt_proved,          r.smt_total,
            r.failures,
        },
    );
}

const jws_header = "{\"alg\":\"EdDSA\",\"typ\":\"zigttp-semantics+jws\"}";
const payload_version = "zigttp-semantics-v3";

const b64 = std.base64.url_safe_no_pad;

fn b64encode(allocator: std.mem.Allocator, bytes: []const u8) ![]u8 {
    const out = try allocator.alloc(u8, b64.Encoder.calcSize(bytes.len));
    _ = b64.Encoder.encode(out, bytes);
    return out;
}

fn b64decode(allocator: std.mem.Allocator, bytes: []const u8) ![]u8 {
    const out = try allocator.alloc(u8, try b64.Decoder.calcSizeForSlice(bytes));
    errdefer allocator.free(out);
    try b64.Decoder.decode(out, bytes);
    return out;
}

fn isLowerHex64(bytes: []const u8) bool {
    if (bytes.len != 64) return false;
    for (bytes) |c| {
        if (!((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f'))) return false;
    }
    return true;
}

fn isDecimal(bytes: []const u8) bool {
    if (bytes.len == 0) return false;
    for (bytes) |c| {
        if (c < '0' or c > '9') return false;
    }
    return true;
}

fn isReceiptPayload(bytes: []const u8) bool {
    var it = std.mem.splitScalar(u8, bytes, '\n');
    const version = it.next() orelse return false;
    if (!std.mem.eql(u8, version, payload_version)) return false;

    for (0..3) |_| {
        const hash = it.next() orelse return false;
        if (!isLowerHex64(hash)) return false;
    }
    for (0..13) |_| {
        const n = it.next() orelse return false;
        if (!isDecimal(n)) return false;
    }
    return it.next() == null;
}

/// Sign a receipt into a compact Ed25519 JWS (header.payload.signature).
pub fn sign(allocator: std.mem.Allocator, r: Receipt, key_pair: Ed25519.KeyPair) ![]u8 {
    const payload = try payloadString(allocator, r);
    defer allocator.free(payload);

    const h64 = try b64encode(allocator, jws_header);
    defer allocator.free(h64);
    const p64 = try b64encode(allocator, payload);
    defer allocator.free(p64);

    const signing_input = try std.mem.concat(allocator, u8, &.{ h64, ".", p64 });
    defer allocator.free(signing_input);

    const sig = try key_pair.sign(signing_input, null);
    const sig_bytes = sig.toBytes();
    const s64 = try b64encode(allocator, &sig_bytes);
    defer allocator.free(s64);

    return std.mem.concat(allocator, u8, &.{ signing_input, ".", s64 });
}

/// Verify a compact JWS against a public key. Returns true iff the signature is
/// valid over the header.payload prefix. Fails closed on any malformed input -
/// in particular the signature segment must decode to exactly an Ed25519
/// signature, gated BEFORE writing into the fixed buffer (untrusted input).
pub fn verify(allocator: std.mem.Allocator, compact: []const u8, public_key: Ed25519.PublicKey) !bool {
    var it = std.mem.splitScalar(u8, compact, '.');
    const h64 = it.next() orelse return false;
    const p64 = it.next() orelse return false;
    const s64 = it.next() orelse return false;
    if (it.next() != null) return false;

    const header = b64decode(allocator, h64) catch return false;
    defer allocator.free(header);
    if (!std.mem.eql(u8, header, jws_header)) return false;

    const payload = b64decode(allocator, p64) catch return false;
    defer allocator.free(payload);
    if (!isReceiptPayload(payload)) return false;

    const decoded_len = b64.Decoder.calcSizeForSlice(s64) catch return false;
    if (decoded_len != Ed25519.Signature.encoded_length) return false;

    const signing_input = try std.mem.concat(allocator, u8, &.{ h64, ".", p64 });
    defer allocator.free(signing_input);

    var sig_bytes: [Ed25519.Signature.encoded_length]u8 = undefined;
    b64.Decoder.decode(&sig_bytes, s64) catch return false;
    const sig = Ed25519.Signature.fromBytes(sig_bytes);
    sig.verify(signing_input, public_key) catch return false;
    return true;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "registry passes all slice mechanisms" {
    var result = try runCheck(std.testing.allocator);
    defer result.deinit();

    try std.testing.expect(result.ok());
    try std.testing.expectEqual(@as(usize, 0), result.failures.items.len);
    // lit_int, lit_bool, identifier, ternary, call = 5 fixed value proofs
    // + 5 binop instances + 2 unop instances = 12 nodes_proven
    try std.testing.expectEqual(@as(usize, 5), result.binop_instances);
    try std.testing.expectEqual(@as(usize, 2), result.unop_instances);
    try std.testing.expectEqual(@as(usize, 12), result.nodes_proven);
    try std.testing.expectEqual(@as(usize, 1), result.refinements_proven);
    try std.testing.expectEqual(@as(usize, 3), result.nodes_structural); // if_stmt, return_stmt, block
}

test "a wrong lowering is caught as divergence" {
    var result = CheckResult{ .arena = std.heap.ArenaAllocator.init(std.testing.allocator) };
    defer result.deinit();
    var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch.deinit();

    // claim binary_op(add) denotes c0+c1, but lower it with `sub` - must diverge.
    const denote = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .add } };
    const wrong = Lowering{ .straight = &.{ .{ .eval_child = 0 }, .{ .eval_child = 1 }, .{ .op = .sub } } };
    try proveValueRule(&result, scratch.allocator(), "binary_op", &denote, wrong);

    try std.testing.expect(!result.ok());
    try std.testing.expectEqual(semantics.SpecCode.lowering_divergence, result.failures.items[0].code);
}

test "an unbalanced lowering is caught" {
    var result = CheckResult{ .arena = std.heap.ArenaAllocator.init(std.testing.allocator) };
    defer result.deinit();
    var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch.deinit();

    // two children, no combining op: nets +2, not +1.
    const denote = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .add } };
    const unbal = Lowering{ .straight = &.{ .{ .eval_child = 0 }, .{ .eval_child = 1 } } };
    try proveValueRule(&result, scratch.allocator(), "binary_op", &denote, unbal);

    try std.testing.expect(!result.ok());
    try std.testing.expectEqual(semantics.SpecCode.unbalanced_lowering, result.failures.items[0].code);
}

test "stackDelta fails loud on an opcode outside the symbolic registry" {
    // get_field is a real opcode but not in op_rules -> must be rejected, not 0/0.
    const steps = [_]Step{ .{ .eval_child = 0 }, .{ .op = .get_field } };
    try std.testing.expect(stackDelta(&steps) == null);
}

test "refinement catches a divergent fused opcode" {
    var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch.deinit();

    // get_loc_add must equal get_loc; add
    const good_fused = [_]Step{ .{ .push_local = 0 }, .{ .op = .add } };
    const base = [_]Step{ .{ .push_local = 0 }, .{ .op = .add } };
    try std.testing.expect(try proveRefinement(scratch.allocator(), &good_fused, &base));

    // a fused op that subtracts instead must be rejected
    const bad_fused = [_]Step{ .{ .push_local = 0 }, .{ .op = .sub } };
    try std.testing.expect(!try proveRefinement(scratch.allocator(), &bad_fused, &base));
}

test "receipt sign and verify round-trip" {
    const allocator = std.testing.allocator;
    var result = try runCheck(allocator);
    defer result.deinit();

    const receipt = buildReceipt(&result, 10, 10, .{ .available = true, .proved = 5, .total = 5 });

    const seed = [_]u8{7} ** 32;
    const kp = try Ed25519.KeyPair.generateDeterministic(seed);

    const compact = try sign(allocator, receipt, kp);
    defer allocator.free(compact);

    try std.testing.expect(try verify(allocator, compact, kp.public_key));

    // tamper: flip a byte in the payload region -> verification must fail
    const tampered = try allocator.dupe(u8, compact);
    defer allocator.free(tampered);
    const dot = std.mem.indexOfScalar(u8, tampered, '.').? + 1;
    tampered[dot] = if (tampered[dot] == 'A') 'B' else 'A';
    try std.testing.expect(!try verify(allocator, tampered, kp.public_key));
}

fn signRawCompactForTest(
    allocator: std.mem.Allocator,
    header: []const u8,
    payload: []const u8,
    key_pair: Ed25519.KeyPair,
) ![]u8 {
    const h64 = try b64encode(allocator, header);
    defer allocator.free(h64);
    const p64 = try b64encode(allocator, payload);
    defer allocator.free(p64);

    const signing_input = try std.mem.concat(allocator, u8, &.{ h64, ".", p64 });
    defer allocator.free(signing_input);

    const sig = try key_pair.sign(signing_input, null);
    const sig_bytes = sig.toBytes();
    const s64 = try b64encode(allocator, &sig_bytes);
    defer allocator.free(s64);

    return std.mem.concat(allocator, u8, &.{ signing_input, ".", s64 });
}

test "receipt verify rejects a valid signature with the wrong JWS subtype" {
    const allocator = std.testing.allocator;
    var result = try runCheck(allocator);
    defer result.deinit();
    const receipt = buildReceipt(&result, 10, 10, .{ .available = true, .proved = 5, .total = 5 });

    const seed = [_]u8{7} ** 32;
    const kp = try Ed25519.KeyPair.generateDeterministic(seed);
    const payload = try payloadString(allocator, receipt);
    defer allocator.free(payload);

    const compact = try signRawCompactForTest(
        allocator,
        "{\"alg\":\"EdDSA\",\"typ\":\"zigttp-attest+jws\"}",
        payload,
        kp,
    );
    defer allocator.free(compact);

    try std.testing.expect(!try verify(allocator, compact, kp.public_key));
}

test "receipt verify rejects a valid signature with a non-semantics payload" {
    const allocator = std.testing.allocator;
    const seed = [_]u8{7} ** 32;
    const kp = try Ed25519.KeyPair.generateDeterministic(seed);

    const compact = try signRawCompactForTest(allocator, jws_header, "zigttp-build-v1\nok", kp);
    defer allocator.free(compact);

    try std.testing.expect(!try verify(allocator, compact, kp.public_key));
}

fn fakeAllEquivalent(_: []const u8, _: std.mem.Allocator) semantics_smt.Verdict {
    return .equivalent;
}

fn fakeAllCounterexample(_: []const u8, _: std.mem.Allocator) semantics_smt.Verdict {
    return .counterexample;
}

fn fakeAllUnknown(_: []const u8, _: std.mem.Allocator) semantics_smt.Verdict {
    return .unknown;
}

test "collectSmtObligations covers every value rule, refinement, and law" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const obs = try collectSmtObligations(arena.allocator());

    // Expected per-node obligations: 1 per non-parametric value rule, one per
    // operator for the parametric ones.
    const n_bin = @typeInfo(semantics.BinKind).@"enum".fields.len;
    const n_un = @typeInfo(semantics.UnKind).@"enum".fields.len;
    var per_node: usize = 0;
    for (semantics.node_rules) |rule| {
        if (rule.proof != .value) continue;
        per_node += switch (rule.parametric) {
            .none => 1,
            .binop => n_bin,
            .unop => n_un,
        };
    }
    try std.testing.expectEqual(
        per_node + semantics.refinements.len + semantics.algebraic_laws.len,
        obs.len,
    );
    // each obligation has both sides populated.
    for (obs) |ob| {
        try std.testing.expect(ob.lhs.len > 0);
        try std.testing.expect(ob.rhs.len > 0);
    }
}

test "runSmt skips cleanly when no solver is injected" {
    var r = try runSmt(std.testing.allocator, null);
    defer r.deinit();
    try std.testing.expect(!r.available);
    try std.testing.expect(r.ok()); // skipped is not a failure
    try std.testing.expect(r.total > 0);
    try std.testing.expectEqual(@as(usize, 0), r.proved);
}

test "runSmt with an all-equivalent solver proves every obligation" {
    var r = try runSmt(std.testing.allocator, fakeAllEquivalent);
    defer r.deinit();
    try std.testing.expect(r.available);
    try std.testing.expect(r.ok());
    try std.testing.expectEqual(r.total, r.proved);
}

test "runSmt records a counterexample as a loud failure" {
    var r = try runSmt(std.testing.allocator, fakeAllCounterexample);
    defer r.deinit();
    try std.testing.expect(r.available);
    try std.testing.expect(!r.ok());
    try std.testing.expectEqual(r.total, r.failures.items.len);
    try std.testing.expectEqual(@as(usize, 0), r.unproven);
    try std.testing.expectEqual(semantics.SpecCode.smt_counterexample, r.failures.items[0].code);
}

test "runSmt treats solver unknown as unproven, not a failure" {
    var r = try runSmt(std.testing.allocator, fakeAllUnknown);
    defer r.deinit();
    try std.testing.expect(r.available);
    try std.testing.expect(r.ok()); // unknown is non-fatal
    try std.testing.expectEqual(r.total, r.unproven);
    try std.testing.expectEqual(@as(usize, 0), r.proved);
    try std.testing.expectEqual(@as(usize, 0), r.failures.items.len);
}

test "runSmt records unencodable obligations as spec authoring failures" {
    var r = SmtResult{ .arena = std.heap.ArenaAllocator.init(std.testing.allocator) };
    defer r.deinit();
    r.available = true;
    r.total = 1;

    var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer scratch.deinit();

    const lhs = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .lt } }; // Bool root
    const rhs = [_]Term{ .{ .child = 0 }, .{ .child = 1 }, .{ .binop = .add } }; // Int root
    const obligations = [_]SmtObligation{.{ .where = "bad_law", .lhs = &lhs, .rhs = &rhs }};

    try proveSmtObligations(&r, scratch.allocator(), &obligations, fakeAllEquivalent);

    try std.testing.expect(!r.ok());
    try std.testing.expectEqual(@as(usize, 0), r.proved);
    try std.testing.expectEqual(@as(usize, 0), r.unproven);
    try std.testing.expectEqual(@as(usize, 1), r.failures.items.len);
    try std.testing.expectEqual(semantics.SpecCode.smt_unencodable, r.failures.items[0].code);
    try std.testing.expectEqualStrings("bad_law", r.failures.items[0].where);
}

test "verify rejects an over-long signature segment without overflow" {
    const allocator = std.testing.allocator;
    const seed = [_]u8{9} ** 32;
    const kp = try Ed25519.KeyPair.generateDeterministic(seed);
    // header.payload.<200 base64url chars> - decodes to 150 bytes, far over 64.
    const over_long = "aGVhZGVy.cGF5bG9hZA." ++ ("A" ** 200);
    try std.testing.expect(!try verify(allocator, over_long, kp.public_key));
}
