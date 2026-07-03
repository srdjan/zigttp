//! Deterministic task routing for `zigttp expert`.
//!
//! This module does not decide whether an edit is safe. It only gives the
//! model a compiler-native route before the first model round-trip so common
//! tasks start with the right tools instead of spending attempts discovering
//! the process.

const std = @import("std");

pub const TaskKind = enum {
    unknown,
    route_add,
    handler_scaffold,
    violation_fix,
    spec_goal,
    workflow_authoring,
    sql_feature,
    auth_jwt,
    env_feature,
    test_generation,
    review_explain,
};

pub const Confidence = enum {
    low,
    medium,
    high,
};

pub const WorkflowHint = struct {
    kind: TaskKind = .unknown,
    confidence: Confidence = .low,

    pub fn injectsSystemNote(self: WorkflowHint) bool {
        return self.kind != .unknown and self.confidence != .low;
    }
};

pub fn taskKindName(kind: TaskKind) []const u8 {
    return @tagName(kind);
}

pub fn confidenceName(confidence: Confidence) []const u8 {
    return @tagName(confidence);
}

pub fn classify(user_text: []const u8) WorkflowHint {
    if (containsAny(user_text, &.{ "jwt", "bearer", "authorization" }) and
        containsAny(user_text, &.{ "auth", "verify", "token", "protect" }))
    {
        return .{ .kind = .auth_jwt, .confidence = .high };
    }

    if (containsAny(user_text, &.{ "zts", "violation", "diagnostic", "compiler error" }) and
        containsAny(user_text, &.{ "fix", "repair", "clean", "resolve" }))
    {
        return .{ .kind = .violation_fix, .confidence = .high };
    }

    if (isWorkflowAuthoring(user_text)) {
        return .{ .kind = .workflow_authoring, .confidence = .high };
    }

    if (containsAny(user_text, &.{ "sql", "sqlite", "database", "query", "select ", "insert ", "update ", "delete " })) {
        return .{ .kind = .sql_feature, .confidence = .high };
    }

    if (containsAny(user_text, &.{ "spec<", "proof", "prove", "property goal", "injection_safe", "no_secret_leakage", "no_credential_leakage", "deterministic", "idempotent", "retry_safe", "retry safe", "fault_covered", "fault covered", "durable workflow", "zigttp:durable", "at-least-once", "safe to cache" })) {
        return .{ .kind = .spec_goal, .confidence = .high };
    }

    if (containsAny(user_text, &.{ "add route", "new route", "create route", "route table", "router", "get /", "post /", "put /", "patch /", "delete /" }) or
        (containsFold(user_text, "route") and containsAny(user_text, &.{ "add", "new", "create" })))
    {
        return .{ .kind = .route_add, .confidence = .high };
    }

    if (containsAny(user_text, &.{ "scaffold", "new handler", "create handler", "minimal handler" })) {
        return .{ .kind = .handler_scaffold, .confidence = .high };
    }

    if (containsAny(user_text, &.{ "env var", "environment variable", "zigttp:env", "secret", "api key" })) {
        return .{ .kind = .env_feature, .confidence = .medium };
    }

    if (containsAny(user_text, &.{ "write test", "add test", "jsonl test", "test case" })) {
        return .{ .kind = .test_generation, .confidence = .medium };
    }

    if (containsAny(user_text, &.{ "explain", "review", "what does", "how does" })) {
        return .{ .kind = .review_explain, .confidence = .medium };
    }

    return .{};
}

pub fn renderSystemNote(
    allocator: std.mem.Allocator,
    hint: WorkflowHint,
) !?[]u8 {
    if (!hint.injectsSystemNote()) return null;

    const route = workflowRoute(hint.kind);
    return try std.fmt.allocPrint(
        allocator,
        "[expert workflow] kind={s} confidence={s}\n{s}\n",
        .{ taskKindName(hint.kind), confidenceName(hint.confidence), route },
    );
}

fn workflowRoute(kind: TaskKind) []const u8 {
    return switch (kind) {
        .route_add =>
        \\Use the compiler-native Route Forge path before manual code. First read the target file or list files if the target is unknown, capture `zigts_expert_verify_paths`, call `pi_forge_route`, and apply only an approved candidate through `pi_apply_feature_plan`.
        ,
        .handler_scaffold =>
        \\Create the smallest canonical handler that satisfies the request. Read nearby handlers first, use live module/rule tools for imports and syntax, then let the compiler veto verify the complete scaffold before apply.
        ,
        .violation_fix =>
        \\Use compiler diagnostics as the source of truth. Read the target, run `zigts_expert_verify_paths`, call `pi_repair_plan`, dry-run supported plans with `pi_apply_repair_plan` or `pi_goal_candidate`, and edit manually only for unsupported repair intents.
        ,
        .spec_goal =>
        \\Drive proof work through the proof tools. Start with `pi_specs_status`, `pi_witnesses`, and `pi_repair_plan`; inspect `proof.proofTrace.durable_workflow_*` when durable retry/idempotency is involved, use `pi_goal_candidate` or `pi_apply_repair_plan` for supported repairs, then `pi_goal_check` to confirm the requested goals.
        ,
        .workflow_authoring =>
        \\Author workflow code from the shipped ZigTS grammar. Call `zigts_expert_modules` for `zigttp:durable`/`zigttp:workflow`, use `req.headers.get("idempotency-key")` for durable run keys, keep `workflow.call`/`fanout`/`follow` at durable depth 0 inside `run()` and never inside `step()` (ZTS509), check ZTS510 before saga drafts, dry-run with `zigts_expert_edit_simulate`, then inspect `proofTrace.durable_workflow_*` after the veto/proof card.
        ,
        .sql_feature =>
        \\Check SQL support before drafting. Read the handler and `zigttp.json`, verify the configured sqlite schema exists, use named parameters supported by the analyzer, and do not retry unchanged if the veto reports missing SQL schema configuration.
        ,
        .auth_jwt =>
        \\Use the auth module path deliberately. Check `zigts_expert_modules` for `zigttp:auth`, keep bearer tokens and claims out of responses/logs, avoid fallback secrets, and verify no credential leakage after the edit.
        ,
        .env_feature =>
        \\Treat environment values as toxic. Check `zigttp:env` with live module tools, avoid fallback secrets, redact in output, and verify no secret leakage before applying the edit.
        ,
        .test_generation =>
        \\Use the existing test shape. Read adjacent tests and handler proofs first, then use `workspace_gen_tests` when a proven handler path exists; keep tests aligned with compiler-proven behavior.
        ,
        .review_explain =>
        \\Do not edit unless the user asks for a change. Use live rule/module tools for ZigTS facts and cite compiler diagnostics or proof output rather than relying on memory.
        ,
        .unknown => "",
    };
}

fn isWorkflowAuthoring(user_text: []const u8) bool {
    const has_workflow_surface = containsAny(user_text, &.{
        "zigttp:workflow",
        "workflow.call",
        "workflow call",
        "workflow queue",
        "--workflow-queue",
        "queued child",
        "child dispatch",
        "child handler",
        "orchestrator",
        "orchestrate",
        "durable handler",
        "durable workflow",
        "zigttp:durable",
        "run()",
        "step()",
        "waitsignal",
        "signalat",
        "saga",
        "fanout",
        "follow",
    });
    if (!has_workflow_surface) return false;

    const authoring = containsAny(user_text, &.{
        "add",
        "build",
        "create",
        "dispatch",
        "generate",
        "handler",
        "implement",
        "orchestrate",
        "orchestrator",
        "scaffold",
        "use ",
        "using",
        "write",
    });
    if (!authoring) return false;

    const explicit_write = containsAny(user_text, &.{ "add", "build", "create", "generate", "implement", "scaffold", "write" });
    const proof_intent = containsAny(user_text, &.{ "proof", "prove", "property goal", "deterministic", "idempotent", "retry_safe", "retry safe", "fault_covered", "fault covered" });
    if (proof_intent and !explicit_write) return false;

    const explanation_only = containsAny(user_text, &.{ "explain", "review", "what does", "how does" });
    return !explanation_only or explicit_write;
}

fn containsAny(haystack: []const u8, needles: []const []const u8) bool {
    for (needles) |needle| {
        if (containsFold(haystack, needle)) return true;
    }
    return false;
}

fn containsFold(haystack: []const u8, needle: []const u8) bool {
    if (needle.len == 0) return true;
    if (needle.len > haystack.len) return false;
    var i: usize = 0;
    while (i + needle.len <= haystack.len) : (i += 1) {
        var j: usize = 0;
        while (j < needle.len) : (j += 1) {
            if (asciiLower(haystack[i + j]) != asciiLower(needle[j])) break;
        }
        if (j == needle.len) return true;
    }
    return false;
}

fn asciiLower(ch: u8) u8 {
    if (ch >= 'A' and ch <= 'Z') return ch + ('a' - 'A');
    return ch;
}

const testing = std.testing;

test "classify route additions before generic handler work" {
    const hint = classify("Add a GET /users route to this handler");
    try testing.expectEqual(TaskKind.route_add, hint.kind);
    try testing.expectEqual(Confidence.high, hint.confidence);
}

test "classify proof goals" {
    const hint = classify("prove this endpoint is injection_safe");
    try testing.expectEqual(TaskKind.spec_goal, hint.kind);
    try testing.expect(hint.injectsSystemNote());
}

test "classify durable workflow proof goals" {
    const hint = classify("make this durable workflow retry safe");
    try testing.expectEqual(TaskKind.spec_goal, hint.kind);
    const note = (try renderSystemNote(testing.allocator, hint)) orelse return error.TestExpected;
    defer testing.allocator.free(note);
    try testing.expect(std.mem.indexOf(u8, note, "proof.proofTrace.durable_workflow_*") != null);
}

test "classify workflow handler proof requests as proof goals" {
    const hint = classify("prove this durable workflow handler is deterministic");
    try testing.expectEqual(TaskKind.spec_goal, hint.kind);
}

test "classify workflow authoring requests separately from proof goals" {
    const hint = classify("Create a durable workflow handler that calls a greet child handler via workflow.call");
    try testing.expectEqual(TaskKind.workflow_authoring, hint.kind);
    try testing.expectEqual(Confidence.high, hint.confidence);
    const note = (try renderSystemNote(testing.allocator, hint)) orelse return error.TestExpected;
    defer testing.allocator.free(note);
    try testing.expect(std.mem.indexOf(u8, note, "workflow.call") != null);
    try testing.expect(std.mem.indexOf(u8, note, "ZTS509") != null);
    try testing.expect(std.mem.indexOf(u8, note, "proofTrace.durable_workflow_*") != null);
}

test "classify workflow diagnostic repairs before authoring prompts" {
    const hint = classify("Fix the ZTS510 error in this saga handler");
    try testing.expectEqual(TaskKind.violation_fix, hint.kind);
    try testing.expectEqual(Confidence.high, hint.confidence);
    const note = (try renderSystemNote(testing.allocator, hint)) orelse return error.TestExpected;
    defer testing.allocator.free(note);
    try testing.expect(std.mem.indexOf(u8, note, "pi_repair_plan") != null);
}

test "explain workflow prompts remain review tasks" {
    const hint = classify("explain how workflow.call works");
    try testing.expectEqual(TaskKind.review_explain, hint.kind);
}

test "classify JWT auth before env secrets" {
    const hint = classify("add bearer JWT auth using JWT_SECRET");
    try testing.expectEqual(TaskKind.auth_jwt, hint.kind);
}

test "unknown prompt does not inject a note" {
    const hint = classify("make it nicer");
    try testing.expectEqual(TaskKind.unknown, hint.kind);
    try testing.expect(!hint.injectsSystemNote());
}

test "renderSystemNote names required compiler-native tool path" {
    const note = (try renderSystemNote(testing.allocator, classify("fix ZTS300 violation"))) orelse return error.TestExpected;
    defer testing.allocator.free(note);
    try testing.expect(std.mem.indexOf(u8, note, "pi_repair_plan") != null);
    try testing.expect(std.mem.indexOf(u8, note, "kind=violation_fix") != null);
}
