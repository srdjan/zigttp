//! Shared projection from `zts.HandlerContract.spec_diagnostics` into the
//! `review.SpecState` shape used by the proof HUD, the studio JSON, the
//! deploy review card, and the proof ledger. Consumed by both
//! `live_reload.zig` (per-recompile) and `deploy.zig` (per-deploy review).

const std = @import("std");
const zts = @import("zts");
const review = @import("review.zig");

/// Build a transient SpecState for a single declared spec name. Strings
/// borrow from the contract (zero-allocation); `ReviewFacts.fromProvenFacts`
/// dupes them when persisting.
pub fn specStateFor(contract: *const zts.HandlerContract, name: []const u8) review.SpecState {
    for (contract.spec_diagnostics.items) |d| {
        // Author-declared Specs carry one diagnostic per spec_name. The
        // implicit-default profile collapses its undischarged properties into a
        // single not_discharged whose spec_name is the joined "a, b, c" set, so
        // match `name` against any token in that set too - otherwise the HUD,
        // studio, deploy card, and ledger would all report the collapsed
        // properties as discharged.
        const matches = std.mem.eql(u8, d.spec_name, name) or
            (d.implicit_default and d.kind == .not_discharged and joinedSetContains(d.spec_name, name));
        if (!matches) continue;
        return .{
            .name = name,
            .discharged = false,
            .diagnostic_code = d.kind.code(),
            .diagnostic_message = specDiagnosticMessage(d),
            .source_line = if (d.cause) |c| c.line else null,
            .source_column = if (d.cause) |c| c.column else null,
            .source_snippet = if (d.cause) |c| c.snippet else null,
        };
    }
    return .{ .name = name, .discharged = true };
}

/// True if `name` is one of the `", "`-joined tokens in `joined` (the
/// collapsed implicit-default spec_name, e.g. "fault_covered, pure").
fn joinedSetContains(joined: []const u8, name: []const u8) bool {
    var it = std.mem.tokenizeSequence(u8, joined, ", ");
    while (it.next()) |token| {
        if (std.mem.eql(u8, token, name)) return true;
    }
    return false;
}

test "joinedSetContains matches a token in the collapsed spec_name set" {
    try std.testing.expect(joinedSetContains("fault_covered, idempotent, pure", "idempotent"));
    try std.testing.expect(joinedSetContains("fault_covered, idempotent, pure", "fault_covered"));
    try std.testing.expect(joinedSetContains("fault_covered, idempotent, pure", "pure"));
    try std.testing.expect(!joinedSetContains("fault_covered, idempotent, pure", "purely"));
    try std.testing.expect(!joinedSetContains("fault_covered, idempotent, pure", "retry_safe"));
    try std.testing.expect(joinedSetContains("pure", "pure"));
}

fn specDiagnosticMessage(d: zts.SpecDiagnostic) []const u8 {
    return switch (d.kind) {
        .not_discharged => d.suggestion orelse "property not discharged",
        .incompatible_with_import => d.incompatible_module orelse "spec contradicts a virtual-module import",
        .unknown_name => "spec name is not in the v1 set",
        .missing_capsule => d.suggestion orelse "helper has no Proof<...> capsule for a demanded property",
        .effect_undeclared => d.suggestion orelse "capability reached outside the declared Effects<...> ceiling",
        .effect_unknown_capability => d.suggestion orelse "Effects<...> names an unknown capability",
        .effect_over_declared => d.suggestion orelse "declared capability is never reached",
        .budget_exceeded => d.suggestion orelse "capability reached outside the handler's Effects<...> budget",
        .helper_budget_exceeded => d.suggestion orelse "helper reaches a capability outside the handler's budget",
        .missing_effects_capsule => d.suggestion orelse "exported helper has no Effects<...> capsule",
        .missing_proof_capsule_export => d.suggestion orelse "exported helper has no Proof<...> capsule",
        .workflow_call_in_step => d.suggestion orelse "workflow.call/saga/fanout/follow used inside step() silently loses durability",
        .saga_step_missing_compensate => d.suggestion orelse "a non-last saga step has no compensate",
    };
}
