//! Deterministic task routing for `zttp expert`.
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

// Proof-property vocabulary shared by the spec_goal route and the
// isWorkflowAuthoring proof-intent guard, so every property that pulls a prompt
// toward spec_goal also keeps it out of workflow_authoring. A property listed in
// only one place is the "proof_intent subset" misroute: e.g. "make this durable
// workflow handler injection_safe" flips to authoring while the retry_safe
// variant stays spec_goal. Keep this in sync with the spec_goal branch below.
const proof_property_needles = [_][]const u8{
    "spec<",
    "proof",
    "prove",
    "property goal",
    "injection_safe",
    "no_secret_leakage",
    "no_credential_leakage",
    "deterministic",
    "idempotent",
    "retry_safe",
    "retry safe",
    "fault_covered",
    "fault covered",
    "at-least-once",
    "safe to cache",
};

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

    if (containsAny(user_text, &proof_property_needles) or
        containsAny(user_text, &.{ "durable workflow", "zttp:durable" }))
    {
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

    if (containsAny(user_text, &.{ "env var", "environment variable", "zttp:env", "secret", "api key" })) {
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
        \\Use the compiler-native Route Forge path before manual code. First read the target file or list files if the target is unknown, capture `zts_expert_verify_paths`, and call `pi_forge_route`. Submit its returned `proposed_content` as one `apply_edit` call so the host veto and approval gate own the write.
        ,
        .handler_scaffold =>
        \\Create the smallest canonical handler that satisfies the request. Read nearby handlers first, use live module/rule tools for imports and syntax, then let the compiler veto verify the complete scaffold before apply.
        ,
        .violation_fix =>
        \\Use compiler diagnostics as the source of truth. Read the target, run `zts_expert_verify_paths`, call `pi_repair_plan`, dry-run supported plans with `pi_apply_repair_plan` or `pi_goal_candidate`, and edit manually only for unsupported repair intents.
        ,
        .spec_goal =>
        \\Drive proof work through the proof tools. Start with `pi_specs_status`, `pi_witnesses`, and `pi_repair_plan`; inspect `proof.proofTrace.durable_workflow_*` when durable retry/idempotency is involved, use `pi_goal_candidate` or `pi_apply_repair_plan` for supported repairs, then `pi_goal_check` to confirm the requested goals.
        ,
        .workflow_authoring =>
        \\Author workflow code from the shipped ZigTS grammar. Call `zts_expert_modules` for `zttp:durable`/`zttp:workflow`, use `req.headers.get("idempotency-key")` for durable run keys, keep `workflow.call`/`fanout`/`follow` at step depth 0 inside `run()` and never inside `step()` (ZTS509), check ZTS510 before saga drafts, dry-run with `zts_expert_edit_simulate`, confirm the child handler resolves with `zts_expert_system_proof` (a single-file veto cannot see a dangling child), then inspect `proof.proofTrace.durable_workflow_*` after the veto/proof card.
        ,
        .sql_feature =>
        \\Check SQL support before drafting. Read the handler and `zttp.json`, verify the configured sqlite schema exists, use named parameters supported by the analyzer, and do not retry unchanged if the veto reports missing SQL schema configuration.
        ,
        .auth_jwt =>
        \\Use the auth module path deliberately. Check `zts_expert_modules` for `zttp:auth`, keep bearer tokens and claims out of responses/logs, avoid fallback secrets, and verify no credential leakage after the edit.
        ,
        .env_feature =>
        \\Treat environment values as toxic. Check `zttp:env` with live module tools, avoid fallback secrets, redact in output, and verify no secret leakage before applying the edit.
        ,
        .test_generation =>
        \\Use the existing test shape. Read adjacent tests and handler proofs first, draft JSONL from compiler-proven behavior, then submit the complete test file through one `apply_edit` call.
        ,
        .review_explain =>
        \\Do not edit unless the user asks for a change. Use live rule/module tools for ZigTS facts and cite compiler diagnostics or proof output rather than relying on memory.
        ,
        .unknown => "",
    };
}

fn isWorkflowAuthoring(user_text: []const u8) bool {
    // Surface: distinctive workflow spellings only. Bare English words are the
    // trap - "follow" is anchored to its call/module forms so "following the
    // pattern" / "as follows" do not collide via substring. "saga"/"fanout" stay
    // bare because they rarely occur outside workflow prose.
    const has_workflow_surface = containsAny(user_text, &.{
        "zttp:workflow",
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
        "zttp:durable",
        "run()",
        "step()",
        "waitsignal",
        "signalat",
        "saga",
        "fanout",
        "workflow.follow",
        "follow(",
    });
    if (!has_workflow_surface) return false;

    // Authoring verbs are matched as whole words so "address"/"because" cannot
    // trip "add"/"use". "orchestrate"/"dispatch" are deliberately kept (they are
    // authoring verbs and their noun forms "orchestrator"/"dispatches" fail the
    // word boundary), but the pure noun "handler" is dropped so read-only
    // questions ("why is my durable handler slow?") do not read as authoring.
    const authoring = containsWordAny(user_text, &.{
        "add",
        "build",
        "create",
        "dispatch",
        "generate",
        "implement",
        "orchestrate",
        "scaffold",
        "use",
        "using",
        "write",
    });
    if (!authoring) return false;

    // Defer to test_generation when the prompt authors a test that merely names
    // a workflow primitive ("jsonl test case for the fanout handler").
    if (containsAny(user_text, &.{ "write test", "add test", "jsonl test", "test case" })) return false;

    const explicit_write = containsWordAny(user_text, &.{ "add", "build", "create", "generate", "implement", "scaffold", "write" });

    // Proof intent uses the SAME property vocabulary as the spec_goal route, so
    // "make this durable workflow injection_safe" bails to spec_goal just like
    // the retry_safe variant. A prompt that carries an explicit write verb stays
    // in authoring even when it names a Spec/proof ("add retry_safe to this
    // workflow's Spec") - the user asked to write code, not to run proof tools.
    const proof_intent = containsAny(user_text, &proof_property_needles);
    if (proof_intent and !explicit_write) return false;

    const explanation_only = containsAny(user_text, &.{
        "explain",
        "review",
        "what does",
        "what is",
        "how does",
        "how do i",
        "why does",
        "why is",
        "show me",
        "can i",
        "should i",
        "when does",
    });
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

fn containsWordAny(haystack: []const u8, needles: []const []const u8) bool {
    for (needles) |needle| {
        if (containsWordFold(haystack, needle)) return true;
    }
    return false;
}

// Case-insensitive whole-word match: the needle must be bounded by a non-word
// char (or a string edge) on both sides, so short verbs like "add"/"use" do not
// substring-match "address"/"because".
fn containsWordFold(haystack: []const u8, needle: []const u8) bool {
    if (needle.len == 0) return true;
    if (needle.len > haystack.len) return false;
    var i: usize = 0;
    while (i + needle.len <= haystack.len) : (i += 1) {
        var j: usize = 0;
        while (j < needle.len) : (j += 1) {
            if (asciiLower(haystack[i + j]) != asciiLower(needle[j])) break;
        }
        if (j != needle.len) continue;
        const before_ok = (i == 0) or !isWordChar(haystack[i - 1]);
        const after = i + needle.len;
        const after_ok = (after == haystack.len) or !isWordChar(haystack[after]);
        if (before_ok and after_ok) return true;
    }
    return false;
}

fn isWordChar(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or (ch >= '0' and ch <= '9');
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

test "workflow authoring does not steal route or sql or test prompts" {
    // A bare "follow" substring no longer hijacks these into workflow_authoring.
    try testing.expectEqual(TaskKind.route_add, classify("Add a GET /users route following the existing pattern").kind);
    try testing.expectEqual(TaskKind.sql_feature, classify("Add a sqlite query that follows the schema in schema.sql").kind);
    try testing.expectEqual(TaskKind.test_generation, classify("Add a jsonl test case for the saga handler").kind);
    try testing.expect(classify("Add a follow-up test case for the error path").kind != .workflow_authoring);
}

test "read-only workflow questions are not workflow authoring" {
    // Noun "handler" and word-bounded verbs keep questions out of authoring.
    try testing.expect(classify("Show me how workflow.call dispatches a child handler").kind != .workflow_authoring);
    try testing.expect(classify("Why does my durable handler hang at waitSignal?").kind != .workflow_authoring);
    try testing.expect(classify("The orchestrator doesn't address the timeout case").kind != .workflow_authoring);
}

test "flow-safety proof requests on workflows route to spec goal" {
    // proof_intent shares spec_goal's property vocabulary, so injection_safe and
    // no_secret_leakage bail to spec_goal just like retry_safe already did.
    try testing.expectEqual(TaskKind.spec_goal, classify("Make this durable workflow handler injection_safe").kind);
    try testing.expectEqual(TaskKind.spec_goal, classify("Prove no_secret_leakage for this durable workflow").kind);
}

test "explicit-write spec asks stay in workflow authoring" {
    // A write verb keeps authoring even when a proof property is named: the user
    // asked to write code, not to run the proof tools. Proof-only phrasings
    // without a write verb still route to spec_goal (see the test above).
    try testing.expectEqual(TaskKind.workflow_authoring, classify("Add retry_safe to this durable workflow's Spec").kind);
    try testing.expectEqual(TaskKind.workflow_authoring, classify("Add a Spec annotation proving this saga handler is deterministic").kind);
}

test "explanation gate keeps use-phrased workflow review out of authoring" {
    // "use" is an authoring word but not an explicit write verb, so this reaches
    // and is saved by the explanation_only gate; deleting the gate would let it
    // fall through to workflow_authoring.
    try testing.expectEqual(TaskKind.review_explain, classify("review how i use workflow.call here").kind);
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
