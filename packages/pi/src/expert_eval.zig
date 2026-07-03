//! Offline deterministic evals for the Pi expert routing surface.
//!
//! These are not model-quality benchmarks. They pin the host-owned part of the
//! "low round-trip" contract: common ZigTS coding asks must enter the first
//! model request with a useful compiler-native workflow route and no provider
//! call is needed to test that guarantee.

const std = @import("std");
const expert_workflow = @import("expert_workflow.zig");

pub const EvalCase = struct {
    name: []const u8,
    prompt: []const u8,
    expected_kind: expert_workflow.TaskKind,
    expected_confidence: expert_workflow.Confidence,
    note_must_contain: []const u8,
};

pub const cases = [_]EvalCase{
    .{
        .name = "route-add",
        .prompt = "Add a GET /users route",
        .expected_kind = .route_add,
        .expected_confidence = .high,
        .note_must_contain = "pi_forge_route",
    },
    .{
        .name = "jwt-auth",
        .prompt = "Protect this handler with bearer JWT auth",
        .expected_kind = .auth_jwt,
        .expected_confidence = .high,
        .note_must_contain = "no credential leakage",
    },
    .{
        .name = "sql-feature",
        .prompt = "Add a sqlite query for users",
        .expected_kind = .sql_feature,
        .expected_confidence = .high,
        .note_must_contain = "named parameters",
    },
    .{
        .name = "proof-goal",
        .prompt = "Prove this endpoint is injection_safe",
        .expected_kind = .spec_goal,
        .expected_confidence = .high,
        .note_must_contain = "pi_goal_candidate",
    },
    .{
        .name = "workflow-authoring",
        .prompt = "Create a durable workflow handler that dispatches a greet child handler with workflow.call",
        .expected_kind = .workflow_authoring,
        .expected_confidence = .high,
        .note_must_contain = "ZTS509",
    },
    .{
        .name = "violation-fix",
        .prompt = "Fix the ZTS300 compiler error",
        .expected_kind = .violation_fix,
        .expected_confidence = .high,
        .note_must_contain = "pi_repair_plan",
    },
    .{
        .name = "env-secret",
        .prompt = "Add the STRIPE_SECRET env var",
        .expected_kind = .env_feature,
        .expected_confidence = .medium,
        .note_must_contain = "no secret leakage",
    },
    .{
        .name = "test-generation",
        .prompt = "Write test case for the successful path",
        .expected_kind = .test_generation,
        .expected_confidence = .medium,
        .note_must_contain = "workspace_gen_tests",
    },
};

pub const EvalSummary = struct {
    total: usize,
    routed: usize,
    high_confidence: usize,
};

pub fn summarize() EvalSummary {
    var routed: usize = 0;
    var high: usize = 0;
    for (cases) |case| {
        const hint = expert_workflow.classify(case.prompt);
        if (hint.kind != .unknown) routed += 1;
        if (hint.confidence == .high) high += 1;
    }
    return .{ .total = cases.len, .routed = routed, .high_confidence = high };
}

const testing = std.testing;

test "static expert eval routes common ZigTS coding asks" {
    for (cases) |case| {
        const hint = expert_workflow.classify(case.prompt);
        try testing.expectEqual(case.expected_kind, hint.kind);
        try testing.expectEqual(case.expected_confidence, hint.confidence);
        const note = (try expert_workflow.renderSystemNote(testing.allocator, hint)) orelse return error.MissingWorkflowNote;
        defer testing.allocator.free(note);
        try testing.expect(std.mem.indexOf(u8, note, case.note_must_contain) != null);
    }
}

test "static expert eval summary is fully routed without model calls" {
    const summary = summarize();
    try testing.expectEqual(cases.len, summary.total);
    try testing.expectEqual(cases.len, summary.routed);
    try testing.expect(summary.high_confidence >= 4);
}
