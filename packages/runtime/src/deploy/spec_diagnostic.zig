//! Shared projection from `zigts.HandlerContract.spec_diagnostics` into the
//! `review.SpecState` shape used by the proof HUD, the studio JSON, the
//! deploy review card, and the proof ledger. Consumed by both
//! `live_reload.zig` (per-recompile) and `deploy.zig` (per-deploy review).

const std = @import("std");
const zigts = @import("zigts");
const review = @import("review.zig");

/// Build a transient SpecState for a single declared spec name. Strings
/// borrow from the contract (zero-allocation); `ReviewFacts.fromProvenFacts`
/// dupes them when persisting.
pub fn specStateFor(contract: *const zigts.HandlerContract, name: []const u8) review.SpecState {
    for (contract.spec_diagnostics.items) |d| {
        if (!std.mem.eql(u8, d.spec_name, name)) continue;
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

fn specDiagnosticMessage(d: zigts.SpecDiagnostic) []const u8 {
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
    };
}
