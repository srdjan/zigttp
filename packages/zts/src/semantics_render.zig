//! semantics_render.zig - the readable view of the semantics registry.
//!
//! Pillar 1 of the northstar ("one source, many generated views"), at L2 scope:
//! `semantics.zig` is the single source of truth, and this renderer projects it
//! into a TypeScript-shaped spec that reads like code. The artifact carries the
//! three registry hashes, so a reader (and `spec-render --check` in CI) can see
//! it is tied to this exact build. Nothing here is authoritative - it is a view -
//! but because it is generated from the same data `semanticsHash` covers, it
//! cannot silently drift from the meaning the checker proves.
//!
//! The committed artifact lives at docs/spec/semantics.spec.ts; regenerate with
//! `zts spec-render --out docs/spec/semantics.spec.ts`.

const std = @import("std");
const semantics = @import("semantics.zig");

const Term = semantics.Term;
const Step = semantics.Step;
const BinKind = semantics.BinKind;
const UnKind = semantics.UnKind;

fn binSym(k: BinKind) []const u8 {
    return switch (k) {
        .add => "+",
        .sub => "-",
        .mul => "*",
        .lt => "<",
        .eq => "===",
    };
}

fn unSym(k: UnKind) []const u8 {
    return switch (k) {
        .not => "!",
        .neg => "-",
    };
}

/// Render an RPN denotation as an infix TypeScript expression.
fn renderDenote(alloc: std.mem.Allocator, terms: []const Term) ![]u8 {
    var stack: std.ArrayList([]const u8) = .empty;
    defer stack.deinit(alloc);
    for (terms) |t| {
        switch (t) {
            .imm => try stack.append(alloc, "imm"),
            .child => |i| try stack.append(alloc, try std.fmt.allocPrint(alloc, "c{d}", .{i})),
            .local => |i| try stack.append(alloc, try std.fmt.allocPrint(alloc, "locals[{d}]", .{i})),
            .call_result => |i| try stack.append(alloc, try std.fmt.allocPrint(alloc, "call{d}()", .{i})),
            .binop => |k| {
                const b = stack.pop() orelse "?";
                const a = stack.pop() orelse "?";
                try stack.append(alloc, try std.fmt.allocPrint(alloc, "({s} {s} {s})", .{ a, binSym(k), b }));
            },
            .unop => |k| {
                const a = stack.pop() orelse "?";
                try stack.append(alloc, try std.fmt.allocPrint(alloc, "({s}{s})", .{ unSym(k), a }));
            },
            .select => {
                const e = stack.pop() orelse "?";
                const th = stack.pop() orelse "?";
                const c = stack.pop() orelse "?";
                try stack.append(alloc, try std.fmt.allocPrint(alloc, "({s} ? {s} : {s})", .{ c, th, e }));
            },
            .binop_self => {
                const b = stack.pop() orelse "?";
                const a = stack.pop() orelse "?";
                try stack.append(alloc, try std.fmt.allocPrint(alloc, "({s} <op> {s})", .{ a, b }));
            },
            .unop_self => {
                const a = stack.pop() orelse "?";
                try stack.append(alloc, try std.fmt.allocPrint(alloc, "(<op>{s})", .{a}));
            },
        }
    }
    if (stack.items.len == 1) return alloc.dupe(u8, stack.items[0]);
    return alloc.dupe(u8, "<malformed>");
}

fn renderSteps(alloc: std.mem.Allocator, steps: []const Step) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    for (steps, 0..) |s, i| {
        if (i != 0) try buf.appendSlice(alloc, "; ");
        switch (s) {
            .push_imm => try buf.appendSlice(alloc, "imm"),
            .eval_child => |c| try buf.appendSlice(alloc, try std.fmt.allocPrint(alloc, "eval(c{d})", .{c})),
            .push_local => |c| try buf.appendSlice(alloc, try std.fmt.allocPrint(alloc, "get_loc[{d}]", .{c})),
            .call_site => |c| try buf.appendSlice(alloc, try std.fmt.allocPrint(alloc, "call{d}()", .{c})),
            .op => |o| try buf.appendSlice(alloc, @tagName(o)),
            .op_self => try buf.appendSlice(alloc, "<op>"),
        }
    }
    return buf.items;
}

/// Render the whole registry as a TypeScript-shaped spec. All intermediate
/// rendering allocations live in an internal arena; only the returned slice is
/// owned by the caller.
pub fn renderSpecTs(caller: std.mem.Allocator) ![]u8 {
    var arena = std.heap.ArenaAllocator.init(caller);
    defer arena.deinit();
    const allocator = arena.allocator();

    var out: std.ArrayList(u8) = .empty;
    const w = &out;

    const sh = semantics.semanticsHash();
    const ir = semantics.irTableHash();
    const op = semantics.opcodeTableHash();
    const cov = semantics.coverage();

    try fmt(allocator, w, "// zts semantics spec - GENERATED from packages/zts/src/semantics.zig.\n", .{});
    try fmt(allocator, w, "// Do not edit by hand; run `zts spec-render` to regenerate.\n", .{});
    try fmt(allocator, w, "//\n", .{});
    try fmt(allocator, w, "// semanticsHash:   {s}\n", .{sh});
    try fmt(allocator, w, "// irTableHash:     {s}\n", .{ir});
    try fmt(allocator, w, "// opcodeTableHash: {s}\n", .{op});
    try fmt(allocator, w, "//\n", .{});
    try fmt(allocator, w, "// Coverage: {d}/{d} IR nodes, {d}/{d} bytecode opcodes specified.\n", .{ cov.nodes_specified, cov.nodes_total, cov.opcodes_specified, cov.opcodes_total });
    try fmt(allocator, w, "// `denote` is what the node computes; `lower` is the bytecode it compiles to.\n", .{});
    try fmt(allocator, w, "// A value node's lower, symbolically executed, equals its denote (spec-check\n", .{});
    try fmt(allocator, w, "// mechanism 3), and the real compiler agrees on a corpus (mechanism 4).\n\n", .{});

    // Operator -> opcode maps.
    try fmt(allocator, w, "// operator -> opcode\n", .{});
    try fmt(allocator, w, "export const binOpcode = {{", .{});
    inline for (std.meta.tags(BinKind), 0..) |k, i| {
        if (i != 0) try fmt(allocator, w, ",", .{});
        try fmt(allocator, w, " {s}: \"{s}\"", .{ @tagName(k), @tagName(semantics.binOpcode(k)) });
    }
    try fmt(allocator, w, " }};\n", .{});
    try fmt(allocator, w, "export const unOpcode = {{", .{});
    inline for (std.meta.tags(UnKind), 0..) |k, i| {
        if (i != 0) try fmt(allocator, w, ",", .{});
        try fmt(allocator, w, " {s}: \"{s}\"", .{ @tagName(k), @tagName(semantics.unOpcode(k)) });
    }
    try fmt(allocator, w, " }};\n\n", .{});

    // Node rules.
    for (semantics.node_rules) |r| {
        switch (r.proof) {
            .structural => {
                try fmt(allocator, w, "// {s}: statement / non-value (structural-only in this slice)\n", .{@tagName(r.tag)});
                try fmt(allocator, w, "export const {s} = {{ proof: \"structural\" }};\n\n", .{@tagName(r.tag)});
            },
            .value => {
                const denote = try renderDenote(allocator, r.denote);
                try fmt(allocator, w, "// {s}: value node", .{@tagName(r.tag)});
                if (r.parametric != .none) {
                    try fmt(allocator, w, " (generic over its {s}; <op> ranges over the map above)", .{@tagName(r.parametric)});
                }
                try fmt(allocator, w, "\n", .{});
                try fmt(allocator, w, "export const {s} = {{\n", .{@tagName(r.tag)});
                try fmt(allocator, w, "  proof: \"value\",\n", .{});
                if (r.parametric != .none) {
                    try fmt(allocator, w, "  parametric: \"{s}\",\n", .{@tagName(r.parametric)});
                }
                try fmt(allocator, w, "  denote: ({s}) => {s},\n", .{ denoteParams(r), denote });
                switch (r.lower.?) {
                    .straight => |steps| {
                        const ls = try renderSteps(allocator, steps);
                        try fmt(allocator, w, "  lower: \"{s}\",\n", .{ls});
                    },
                    .branch => |b| {
                        const c = try renderSteps(allocator, b.cond);
                        const th = try renderSteps(allocator, b.then);
                        const e = try renderSteps(allocator, b.else_);
                        try fmt(allocator, w, "  lower: {{ cond: \"{s}\", then: \"{s}\", else: \"{s}\", wiring: [", .{ c, th, e });
                        for (b.wiring, 0..) |wop, i| {
                            if (i != 0) try fmt(allocator, w, ", ", .{});
                            try fmt(allocator, w, "\"{s}\"", .{@tagName(wop)});
                        }
                        try fmt(allocator, w, "] }},\n", .{});
                    },
                }
                try fmt(allocator, w, "}};\n\n", .{});
            },
        }
    }

    // Refinements.
    try fmt(allocator, w, "// fused-opcode refinements (each must equal its base sequence)\n", .{});
    for (semantics.refinements) |rf| {
        const fe = try renderSteps(allocator, rf.fused_effect);
        const base = try renderSteps(allocator, rf.base);
        try fmt(allocator, w, "// {s}:  ({s})  ==  ({s})\n", .{ @tagName(rf.fused), fe, base });
    }

    // Algebraic laws: structurally-different equivalences the SMT check certifies
    // (spec-check mechanism 5) but structural equality (mechanism 3) cannot.
    try fmt(allocator, w, "\n// algebraic laws - SMT-certified value-model equivalences (mechanism 5)\n", .{});
    for (semantics.algebraic_laws) |law| {
        const lhs = try renderDenote(allocator, law.lhs);
        const rhs = try renderDenote(allocator, law.rhs);
        try fmt(allocator, w, "// {s}:  {s}  ==  {s}\n", .{ law.name, lhs, rhs });
    }

    // Excluded laws: machine-REFUTED non-laws (the faithful-model exclusion audit).
    try fmt(allocator, w, "\n// excluded laws - REFUTED under the faithful value model (false on the engine)\n", .{});
    for (semantics.excluded_laws) |law| {
        const lhs = try renderDenote(allocator, law.lhs);
        const rhs = try renderDenote(allocator, law.rhs);
        try fmt(allocator, w, "// {s}:  {s}  !=  {s}\n", .{ law.name, lhs, rhs });
    }

    return caller.dupe(u8, out.items);
}

/// The arrow-function parameter list for a value node's denote, so the rendered
/// TS is well-formed (the params named by the denotation's leaf operands).
fn denoteParams(r: semantics.NodeRule) []const u8 {
    return switch (r.tag) {
        .lit_int, .lit_bool => "imm",
        .identifier => "locals",
        .binary_op => "c0, c1",
        .unary_op => "c0",
        .ternary => "c0, c1, c2",
        .call => "",
        else => "",
    };
}

fn fmt(allocator: std.mem.Allocator, buf: *std.ArrayList(u8), comptime f: []const u8, args: anytype) !void {
    const s = try std.fmt.allocPrint(allocator, f, args);
    defer allocator.free(s);
    try buf.appendSlice(allocator, s);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "rendered spec is tied to the registry: carries the hash and every node" {
    const allocator = std.testing.allocator;
    const ts = try renderSpecTs(allocator);
    defer allocator.free(ts);

    const sh = semantics.semanticsHash();
    try std.testing.expect(std.mem.indexOf(u8, ts, &sh) != null);

    // Every specified node tag appears as an exported const.
    for (semantics.node_rules) |r| {
        const decl = try std.fmt.allocPrint(allocator, "export const {s} =", .{@tagName(r.tag)});
        defer allocator.free(decl);
        try std.testing.expect(std.mem.indexOf(u8, ts, decl) != null);
    }
}

test "render is deterministic" {
    const allocator = std.testing.allocator;
    const a = try renderSpecTs(allocator);
    defer allocator.free(a);
    const b = try renderSpecTs(allocator);
    defer allocator.free(b);
    try std.testing.expectEqualStrings(a, b);
}

test "denotations render as readable infix" {
    const allocator = std.testing.allocator;
    const ts = try renderSpecTs(allocator);
    defer allocator.free(ts);
    // binary_op renders its generic infix form; ternary renders the conditional.
    try std.testing.expect(std.mem.indexOf(u8, ts, "(c0 <op> c1)") != null);
    try std.testing.expect(std.mem.indexOf(u8, ts, "(c0 ? c1 : c2)") != null);
}

test "rendered spec lists the algebraic and excluded laws" {
    const allocator = std.testing.allocator;
    const ts = try renderSpecTs(allocator);
    defer allocator.free(ts);
    // every declared (and excluded) law name appears in the readable spec.
    for (semantics.algebraic_laws) |law| {
        try std.testing.expect(std.mem.indexOf(u8, ts, law.name) != null);
    }
    for (semantics.excluded_laws) |law| {
        try std.testing.expect(std.mem.indexOf(u8, ts, law.name) != null);
    }
    try std.testing.expect(std.mem.indexOf(u8, ts, "excluded laws") != null);
}
