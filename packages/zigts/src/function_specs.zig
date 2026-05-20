//! Proof-capsule discharge for user-defined helper functions.
//!
//! Walks every named function the effect analyzer collected, reads its
//! `Proof<T, S>` capsule annotation, and discharges the declared properties
//! against the facts effect inference and path-return analysis proved.
//!
//! The reserved `handler` entry point is skipped: its return type carries
//! `Spec<...>`, discharged separately by the contract builder. Discharging it
//! here too would double-report and mis-flag handler-only flow specs.

const std = @import("std");
const ir = @import("parser/ir.zig");
const effect_inference = @import("effect_inference.zig");
const handler_verifier = @import("handler_verifier.zig");
const type_env_mod = @import("type_env.zig");
const type_pool_mod = @import("type_pool.zig");
const spec_discharge = @import("spec_discharge.zig");
const contract_types = @import("contract_types.zig");
const json_utils = @import("json_utils.zig");

const IrView = ir.IrView;
const EffectAnalyzer = effect_inference.Analyzer;
const TypeEnv = type_env_mod.TypeEnv;
const SpecDiagnostic = contract_types.SpecDiagnostic;
const CapsuleFacts = spec_discharge.CapsuleFacts;

/// The reserved handler entry point - discharged via `Spec<...>` elsewhere.
const handler_fn_name = "handler";

/// One helper's proof capsule: the declared properties, the facts the
/// compiler proved, and the per-function discharge diagnostics.
pub const Capsule = struct {
    /// Owned copy of the function name.
    name: []const u8,
    /// 1-based source line of the function declaration (0 when unknown).
    line: u32,
    /// Owned, de-duplicated capsule property names declared on the return
    /// type. Empty when the helper carries no `Proof<...>` annotation.
    declared: std.ArrayList([]const u8) = .empty,
    /// Facts proved about this function by effect inference + return analysis.
    proven: CapsuleFacts,
    /// ZTS500 / ZTS502 for this function's own declared capsule.
    diagnostics: std.ArrayList(SpecDiagnostic) = .empty,

    /// True when the function declared a capsule and every property held.
    pub fn discharged(self: *const Capsule) bool {
        return self.declared.items.len > 0 and self.diagnostics.items.len == 0;
    }

    pub fn deinit(self: *Capsule, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.declared.items) |s| allocator.free(s);
        self.declared.deinit(allocator);
        for (self.diagnostics.items) |*d| @constCast(d).deinit(allocator);
        self.diagnostics.deinit(allocator);
    }
};

/// One capsule per user-defined named function (excluding `handler`).
pub const CapsuleTable = struct {
    capsules: std.ArrayList(Capsule) = .empty,

    pub fn deinit(self: *CapsuleTable, allocator: std.mem.Allocator) void {
        for (self.capsules.items) |*c| c.deinit(allocator);
        self.capsules.deinit(allocator);
    }

    /// Look up a capsule by function name. Helper names are expected unique
    /// within a module; the first match wins.
    pub fn byName(self: *const CapsuleTable, name: []const u8) ?*const Capsule {
        for (self.capsules.items) |*c| {
            if (std.mem.eql(u8, c.name, name)) return c;
        }
        return null;
    }
};

/// Discharge every user-defined helper's `Proof<...>` capsule. `analyzer`
/// must already have run `analyze`. `env` may be null (no type info): then
/// every capsule is property-free and only the `proven` facts are recorded.
pub fn discharge(
    allocator: std.mem.Allocator,
    analyzer: *const EffectAnalyzer,
    env: ?*const TypeEnv,
    ir_view: IrView,
) !CapsuleTable {
    var table: CapsuleTable = .{};
    errdefer table.deinit(allocator);

    for (analyzer.all()) |fe| {
        if (std.mem.eql(u8, fe.name, handler_fn_name)) continue;

        const line: u32 = if (ir_view.getLoc(fe.decl_node)) |loc| loc.line else 0;

        const facts: CapsuleFacts = .{
            .total = handler_verifier.functionAlwaysReturns(ir_view, fe.body_node),
            .pure = fe.row.pure,
            .read_only = fe.row.readOnly(),
            .deterministic = fe.row.deterministic,
            .recursive = fe.row.recursive,
        };

        var declared: std.ArrayList([]const u8) = .empty;
        errdefer {
            for (declared.items) |s| allocator.free(s);
            declared.deinit(allocator);
        }
        try collectDeclared(allocator, env, line, &declared);

        var diagnostics = try spec_discharge.dischargeCapsule(allocator, declared.items, facts);
        errdefer {
            for (diagnostics.items) |*d| @constCast(d).deinit(allocator);
            diagnostics.deinit(allocator);
        }

        const owned_name = try allocator.dupe(u8, fe.name);
        errdefer allocator.free(owned_name);

        try table.capsules.append(allocator, .{
            .name = owned_name,
            .line = line,
            .declared = declared,
            .proven = facts,
            .diagnostics = diagnostics,
        });
    }

    return table;
}

/// Read the declared capsule property names from a function's return-type
/// annotation. Owned, de-duplicated copies are appended to `out`.
fn collectDeclared(
    allocator: std.mem.Allocator,
    env: ?*const TypeEnv,
    line: u32,
    out: *std.ArrayList([]const u8),
) !void {
    const e = env orelse return;
    if (line == 0) return;
    const sig = e.getFnSigByLoc(line) orelse return;
    if (sig.return_type == type_pool_mod.null_type_idx) return;

    var raw: std.ArrayListUnmanaged([]const u8) = .empty;
    defer raw.deinit(allocator);
    e.extractSpecMembers(sig.return_type, &raw);

    for (raw.items) |name| {
        if (json_utils.containsString(out.items, name)) continue;
        const dup = try allocator.dupe(u8, name);
        errdefer allocator.free(dup);
        try out.append(allocator, dup);
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const context = @import("context.zig");
const JsParser = @import("parser/root.zig").JsParser;

test "discharge records proven facts and skips the handler" {
    const allocator = std.testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    const source =
        \\function clean(s) { return s; }
        \\function nowMs() { return Date.now(); }
        \\function handler(req) { return clean(req); }
    ;
    var parser = JsParser.init(allocator, source);
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = EffectAnalyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    var table = try discharge(allocator, &analyzer, null, view);
    defer table.deinit(allocator);

    // The handler is discharged via Spec<...> elsewhere; capsule discharge
    // skips it. Helpers each get a capsule entry.
    try std.testing.expect(table.byName("handler") == null);

    const clean = table.byName("clean") orelse return error.MissingCapsule;
    try std.testing.expect(clean.proven.pure);
    try std.testing.expect(clean.proven.total);
    try std.testing.expect(clean.proven.deterministic);
    try std.testing.expect(clean.proven.read_only);

    const now = table.byName("nowMs") orelse return error.MissingCapsule;
    try std.testing.expect(!now.proven.deterministic);
    try std.testing.expect(!now.proven.pure);
    try std.testing.expect(now.proven.total);

    // No declared capsules means no discharge diagnostics.
    try std.testing.expectEqual(@as(usize, 0), clean.diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), now.diagnostics.items.len);
}

test "discharge records non-total facts for a helper that may not return" {
    const allocator = std.testing.allocator;
    var atoms = context.AtomTable.init(allocator);
    defer atoms.deinit();
    // The `if` has no else, so one path falls through without returning.
    const source = "function maybe(n) { if (n) { return n; } }";
    var parser = JsParser.init(allocator, source);
    parser.setAtomTable(&atoms);
    defer parser.deinit();
    const root = try parser.parse();
    const view = IrView.fromIRStore(&parser.nodes, &parser.constants);
    var analyzer = EffectAnalyzer.init(allocator, view, &atoms);
    defer analyzer.deinit();
    try analyzer.analyze(root);

    var table = try discharge(allocator, &analyzer, null, view);
    defer table.deinit(allocator);

    const maybe = table.byName("maybe") orelse return error.MissingCapsule;
    try std.testing.expect(!maybe.proven.total);
}
