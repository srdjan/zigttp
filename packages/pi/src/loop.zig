//! Expert loop driver. Pumps transcript state, Anthropic replies, compiler
//! veto results, and structured tool batches through `turn.TurnMachine`.

const std = @import("std");
const turn = @import("turn.zig");
const veto = @import("veto.zig");
const transcript_mod = @import("transcript.zig");
const registry_mod = @import("registry/registry.zig");
const ui_payload_mod = @import("ui_payload.zig");
const proof_enrichment = @import("proof_enrichment.zig");
const zigts = @import("zigts");
const file_io = zigts.file_io;
const apply_edit = @import("providers/anthropic/apply_edit.zig");
const tools_common = @import("tools/common.zig");
const json_writer = @import("providers/anthropic/json_writer.zig");
const session_events = @import("session/events.zig");
const expert_workflow = @import("expert_workflow.zig");
const auto_repair = @import("auto_repair.zig");
const pi_goal_candidate = @import("tools/pi_goal_candidate.zig");

const PostApplyReport = struct {
    ok: bool,
    summary: ?[]u8 = null,
};

const RepairLinks = struct {
    repair_plan_ids: [][]u8 = &.{},

    fn deinit(self: *RepairLinks, allocator: std.mem.Allocator) void {
        for (self.repair_plan_ids) |item| allocator.free(item);
        allocator.free(self.repair_plan_ids);
        self.* = .{};
    }
};

pub const ModelCallResult = struct {
    reply: turn.AssistantReply,
    usage: turn.Usage = .{},
};

pub const ModelClient = struct {
    context: *anyopaque,
    request_fn: *const fn (
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!ModelCallResult,

    pub fn request(
        self: ModelClient,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) !ModelCallResult {
        return self.request_fn(self.context, arena, transcript, extra_user_text);
    }
};

/// What the user sees before approving a verified edit. Carries enough to make
/// an informed decision: the target file, the pre- and post-edit bytes (so a
/// surface can show a diff or change size), and the proven properties the edit
/// established. Built only on the interactive `.ask` path; auto policies ignore
/// it. `after` is the exact bytes that will be written if approved.
pub const ApprovalPreview = struct {
    file: []const u8,
    before: ?[]const u8 = null,
    after: []const u8 = "",
    properties: ?ui_payload_mod.PropertiesSnapshot = null,
    /// Canonical-normalization rewrites applied during salvage. Non-empty when
    /// the veto auto-fixed canonical violations before approval. Borrows from
    /// the veto report; does not outlive the run_veto arm's arena.
    rewrite_trace: []const []u8 = &.{},
};

pub const ApprovalFn = union(enum) {
    bare: *const fn (preview: ApprovalPreview) anyerror!bool,
    contextual: struct {
        context: *anyopaque,
        func: *const fn (context: *anyopaque, preview: ApprovalPreview) anyerror!bool,
    },

    pub fn fromFn(func: *const fn (preview: ApprovalPreview) anyerror!bool) ApprovalFn {
        return .{ .bare = func };
    }

    pub fn withContext(
        context: *anyopaque,
        func: *const fn (context: *anyopaque, preview: ApprovalPreview) anyerror!bool,
    ) ApprovalFn {
        return .{ .contextual = .{ .context = context, .func = func } };
    }

    pub fn call(self: ApprovalFn, preview: ApprovalPreview) anyerror!bool {
        return switch (self) {
            .bare => |func| func(preview),
            .contextual => |wrapped| wrapped.func(wrapped.context, preview),
        };
    }
};

pub const ApprovalPolicy = enum { ask, auto_approve, auto_reject };

pub fn autoApprove(preview: ApprovalPreview) anyerror!bool {
    _ = preview;
    return true;
}

pub fn autoReject(preview: ApprovalPreview) anyerror!bool {
    _ = preview;
    return false;
}

/// Appended to the bare "agent budget exhausted before a final answer" line so a
/// turn that runs out of model round-trips ends with a concrete next step rather
/// than a dead end.
pub const budget_exhausted_next_step =
    "Next: re-run the request, narrow it to one change at a time, or run " ++
    "`zigttp check <handler>` to see the remaining diagnostics directly.";

/// Appended to the veto-exhaustion diagnostic box so a turn that burns every
/// verification attempt ends with a recoverable next step rather than a silent
/// dead end. Mirrors `budget_exhausted_next_step`.
pub const veto_exhausted_next_step =
    "Next: narrow the ask to one change, fix it directly " ++
    "(`zigttp check <handler>` shows the full diagnostic), or rephrase the goal. " ++
    "The attempts are saved in your session ledger (/ledger).";

/// One-line, actionable remediation for a model-backend error, or null for
/// errors that are not provider failures (local I/O, parse, etc.). The wire
/// layer maps HTTP status and in-stream errors to these typed errors; the
/// interactive and `--print` catch sites render this hint so the user sees what
/// to do instead of a bare CamelCase error name.
pub fn providerErrorRemediation(err: anyerror) ?[]const u8 {
    return switch (err) {
        error.AuthFailed => "Authentication failed. Check your key with `zigttp auth status`, or re-run `zigttp auth claude`.",
        error.InsufficientCredit => "The provider rejected the request for insufficient credit. Check your account credit balance.",
        error.RateLimited => "Rate limited by the provider. Wait a moment and try again.",
        error.ModelNotFound => "The configured model was not found. Switch with `/model <id>` or check the model name.",
        error.ProviderOverloaded => "The provider is overloaded. Try again shortly.",
        error.ProviderServerError => "The provider returned a server error. Try again shortly.",
        error.ApiError => "The provider returned an error mid-response (details logged above).",
        error.PromptTooLong => "The conversation is too large for the model's context window. Run `/compact` to shrink it, then retry.",
        error.RequestTimedOut => "The request timed out with no response. Check your network and try again.",
        // SSE/stream decode failures from `providers/openai/sse_parser.zig`. A
        // mangled or partial stream (often a proxy) otherwise prints a bare
        // CamelCase name; collapse them all into one actionable line.
        error.MalformedSse,
        error.MissingType,
        error.UnknownEventType,
        error.UnexpectedJsonShape,
        => "The provider response could not be parsed (possibly a proxy or partial stream). Try again.",
        else => null,
    };
}

/// Write a failed turn's error name to stderr, followed by one-line remediation
/// when it is a known provider error. Shared by the interactive REPL and
/// `--print` so both surface the same actionable message instead of a bare
/// error name (or, for `--print`, a Zig error-return trace).
pub fn writeTurnErrorToStderr(err: anyerror) void {
    var buf: [512]u8 = undefined;
    const text = if (providerErrorRemediation(err)) |hint|
        std.fmt.bufPrint(&buf, "error: {s}\n{s}\n", .{ @errorName(err), hint }) catch "error\n"
    else
        std.fmt.bufPrint(&buf, "error: {s}\n", .{@errorName(err)}) catch "error\n";
    _ = std.c.write(std.c.STDERR_FILENO, text.ptr, text.len);
}

pub fn resolveApprovalFn(policy: ApprovalPolicy, ask_fn: ?ApprovalFn) ApprovalFn {
    return switch (policy) {
        .auto_approve => ApprovalFn.fromFn(autoApprove),
        .auto_reject => ApprovalFn.fromFn(autoReject),
        .ask => ask_fn orelse ApprovalFn.fromFn(autoReject),
    };
}

pub const TurnResult = struct {
    final_state: turn.TurnState,
    attempt: u8,
    usage: turn.Usage = .{},
    end_reason: session_events.TurnEndReason = .approved,
    /// Model round-trips consumed by this turn. Surfaced so the session layer
    /// can accumulate "round-trips to first green proof" without re-parsing the
    /// events log.
    roundtrips: u8 = 0,
    /// True when this turn applied a compiler-verified edit. A plain-text turn
    /// (e.g. a clarifying question) ends `.approved` without applying one, so
    /// metrics key "handler advanced" on this, not on `end_reason`.
    applied_edit: bool = false,
    /// Proof guarantees discharged / tracked on the applied edit, counted at the
    /// veto (see PropertiesSnapshot.guaranteeCounts). Both 0 when no edit applied.
    proven_guarantees: u32 = 0,
    tracked_guarantees: u32 = 0,
    /// Host-classified workflow for this turn. The hint is advisory; compiler
    /// veto remains the authority on whether edits can land.
    workflow_kind: expert_workflow.TaskKind = .unknown,
    workflow_confidence: expert_workflow.Confidence = .low,
    workflow_hint_injected: bool = false,
    /// True when the first model draft passed the edit-simulate veto. This is
    /// a quality signal for "expert first draft" effectiveness, not an apply
    /// authorization.
    first_draft_veto_pass: bool = false,
    veto_retry_count: u32 = 0,
    tool_call_count: u32 = 0,
    /// True when this turn applied a compiler-authored repair candidate with no
    /// model round-trip (the model's draft failed veto, the deterministic lane
    /// produced a fix that passed the full veto, and it landed through the
    /// approval gate). The headline "model-free apply" signal.
    compiler_authored_apply: bool = false,
};

pub const RunOptions = struct {
    max_attempts: u8 = 3,
    workspace_root: []const u8 = ".",
    approval_fn: ?ApprovalFn = null,
    max_model_roundtrips_per_turn: u8 = 18,
    max_tool_calls_per_turn: usize = 16,
    max_tool_batch_size: usize = 8,
    replay_mode: bool = false,
    /// Per-turn wall-time limit in milliseconds. When elapsed at the start of
    /// a model roundtrip, the turn is cut short the same way a roundtrip-budget
    /// exhaustion is: the model sees a "budget exhausted" prompt and the turn
    /// returns in .awaiting_user state. 0 disables the limit.
    turn_timeout_ms: u64 = 60_000,
};

/// Verification attempts granted to interactive and `--print` turns. Higher
/// than the conservative library default (3): a non-trivial edit often needs
/// several retry rounds because the first diagnostic surfaces only one of
/// multiple issues, and three attempts ends the turn "failed" with nothing
/// applied. Sourced here so the REPL `/settings` display cannot drift from it.
pub const interactive_max_attempts: u8 = 5;

/// Upper bound on repairs the in-process auto-repair lane chains onto one
/// failed draft before producing a candidate. Matches pi_goal_candidate's
/// max_repairs cap; a draft needing more distinct fixes than this falls back to
/// an enriched model retry.
const max_auto_repairs: usize = 8;

const PreparedEdit = struct {
    edit: turn.Edit,
    resolved_path: []const u8,
};

fn callModel(
    allocator: std.mem.Allocator,
    arena: std.mem.Allocator,
    client: ModelClient,
    transcript: *transcript_mod.Transcript,
    extra_prompt: ?[]const u8,
) !ModelCallResult {
    const result = try client.request(arena, transcript, extra_prompt);
    if (result.reply.preamble) |text| {
        if (text.len > 0) try transcript.append(allocator, .{ .model_text = text });
    }
    return result;
}

pub fn runTurnWith(
    allocator: std.mem.Allocator,
    client: ModelClient,
    registry: *const registry_mod.Registry,
    transcript: *transcript_mod.Transcript,
    user_text: []const u8,
    options: RunOptions,
) !TurnResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const ta = arena.allocator();

    try transcript.append(allocator, .{ .user_text = user_text });
    const workflow_hint = expert_workflow.classify(user_text);
    var workflow_hint_injected = false;
    if (try expert_workflow.renderSystemNote(ta, workflow_hint)) |note| {
        try transcript.append(allocator, .{ .system_note = note });
        workflow_hint_injected = true;
    }

    var machine: turn.TurnMachine = .{ .max_attempts = options.max_attempts };
    var next_event: turn.TurnEvent = .{ .user_submitted = user_text };
    var model_roundtrips: u8 = 0;
    var tool_calls_used: usize = 0;
    var turn_usage: turn.Usage = .{};
    // Set when we stop the turn for running out of model round-trips. The
    // `.prompt_user` arm reads it to append a concrete next step to the bare
    // "budget exhausted" line so the user is not left at a dead end.
    var hit_roundtrip_budget = false;
    // Set when the per-turn tool-call budget is exceeded; causes end_turn/none
    // to log budget_tool_calls instead of approved.
    var hit_tool_budget = false;
    // Set when the per-turn wall-time budget is exhausted. The `.prompt_user`
    // arm reads it to show a timeout-specific message instead of the generic
    // round-trip exhaustion text.
    var hit_timeout_budget = false;
    const turn_start_ms: i64 = nowMonotonicMs();
    // Accumulated diagnostics across failed verification attempts. The retry
    // prompt feeds the whole history (not just the latest envelope) so the model
    // does not re-introduce a violation it already fixed on an earlier attempt.
    var diag_history: []const u8 = "";
    // Compiler-authored repair guidance for the most recent failed draft, built
    // by the in-process repair lane on a veto failure and consumed by the next
    // `.retry_draft` so the model gets the exact fix instead of re-deriving it.
    // Arena-owned; null when the lane produced no actionable plans.
    var auto_repair_block: ?[]const u8 = null;
    // Project SQL schema for the veto, discovered lazily on the first edit
    // attempt and reused for every retry in the turn (the discovery walks the
    // filesystem for zigttp.json; once per turn is enough). Arena-owned.
    var sql_schema_resolved = false;
    var sql_schema_path: ?[]u8 = null;
    // How many times a SQL veto has failed in this turn. When it reaches 2, an
    // extra escalation hint is injected before the next model retry so the model
    // gets diagnostic guidance without burning another attempt blind.
    var sql_veto_fail_count: u32 = 0;
    // Set when a SQL escalation hint should be prepended to the next retry prompt.
    var inject_sql_escalation = false;
    // Set when this turn applies a compiler-verified edit; carries the proof
    // guarantee counts of the applied bytes back to the session layer for metrics.
    var applied_edit = false;
    var applied_proven: u32 = 0;
    var applied_tracked: u32 = 0;
    var first_draft_veto_pass = false;
    var veto_retry_count: u32 = 0;
    // Set when this turn lands a compiler-authored repair candidate model-free
    // (Phase B). Carried into the TurnResult so the session layer can report
    // "% of edits that became model-free".
    var compiler_authored_apply = false;

    while (true) {
        const action = machine.transition(next_event);
        switch (action) {
            .request_model => {
                if (model_roundtrips >= options.max_model_roundtrips_per_turn) {
                    hit_roundtrip_budget = true;
                    next_event = .budget_exhausted;
                    continue;
                }
                if (options.turn_timeout_ms > 0 and
                    nowMonotonicMs() - turn_start_ms >= @as(i64, @intCast(options.turn_timeout_ms)))
                {
                    hit_timeout_budget = true;
                    next_event = .budget_exhausted;
                    continue;
                }
                model_roundtrips += 1;
                const result = try callModel(allocator, ta, client, transcript, null);
                turn_usage.add(result.usage);
                next_event = .{ .model_replied = result.reply };
            },
            .retry_draft => |payload| {
                if (model_roundtrips >= options.max_model_roundtrips_per_turn) {
                    hit_roundtrip_budget = true;
                    next_event = .budget_exhausted;
                    continue;
                }
                if (options.turn_timeout_ms > 0 and
                    nowMonotonicMs() - turn_start_ms >= @as(i64, @intCast(options.turn_timeout_ms)))
                {
                    hit_timeout_budget = true;
                    next_event = .budget_exhausted;
                    continue;
                }
                veto_retry_count += 1;
                model_roundtrips += 1;
                diag_history = try std.fmt.allocPrint(
                    ta,
                    "{s}--- attempt {d} diagnostic ---\n{s}\n\n",
                    .{ diag_history, payload.attempt, payload.diagnostic },
                );
                const sql_hint: []const u8 = if (inject_sql_escalation) blk: {
                    inject_sql_escalation = false;
                    break :blk "\n\nYou have failed the SQL check twice. Common causes: " ++
                        "(a) the query references a table or column not in the schema, " ++
                        "(b) you used a non-supported SQL statement (only SELECT/INSERT/UPDATE/DELETE with named parameters). " ++
                        "Use the zigts_expert_describe_rule tool to look up ZTS3xx SQL rules, " ++
                        "or ask the user to verify the schema path in zigttp.json.";
                } else "";
                // Compiler-authored repair guidance for the draft that just
                // failed, if the lane produced any. Consumed once: it pertains to
                // the latest draft, so it is cleared after this prompt is built.
                const repair_block: []const u8 = auto_repair_block orelse "";
                auto_repair_block = null;
                const prompt = try std.fmt.allocPrint(
                    ta,
                    "Your previous edit failed compiler verification (attempt {d}/{d}). " ++
                        "Apply the compiler-authored repairs below verbatim, fix all flagged " ++
                        "violations, and do NOT re-introduce one you already fixed in an earlier " ++
                        "attempt. Emit a new, complete edit.\n\n{s}{s}{s}",
                    .{ payload.attempt, payload.max_attempts, repair_block, diag_history, sql_hint },
                );
                const result = try callModel(allocator, ta, client, transcript, prompt);
                turn_usage.add(result.usage);
                next_event = .{ .model_replied = result.reply };
            },
            .run_veto => |edit| {
                const prepared = prepareEdit(ta, options.workspace_root, edit) catch |err| switch (err) {
                    // A path that resolves outside the workspace root is the
                    // model's mistake, not a fatal agent error. Surface it as a
                    // failed edit so the turn machine re-prompts the model to
                    // fix the path, instead of propagating out of the turn and
                    // crashing the whole agent. Other errors (OOM, unreadable
                    // `before` file) remain fatal.
                    error.PathOutsideWorkspace => {
                        const msg = try std.fmt.allocPrint(
                            ta,
                            "edit rejected: the file path `{s}` resolves outside the workspace " ++
                                "root. Use a path relative to the workspace (for example " ++
                                "`src/handler.ts`), not an absolute path or one that escapes the " ++
                                "project directory.",
                            .{edit.file},
                        );
                        try transcript.append(allocator, .{ .diagnostic_box = .{ .llm_text = msg } });
                        next_event = .{ .edit_verified = .{ .ok = false, .llm_text = msg } };
                        continue;
                    },
                    else => return err,
                };
                if (!sql_schema_resolved) {
                    sql_schema_resolved = true;
                    sql_schema_path = veto.discoverSqlSchemaPath(ta);
                }
                const veto_result = try veto.runVetoWithSchema(ta, .{
                    .file = prepared.edit.file,
                    .content = prepared.edit.content,
                    .before = prepared.edit.before,
                }, sql_schema_path);
                // The outcome handed to the state machine. The model-free
                // repair path (Phase B) overrides it to a pass after it lands a
                // candidate, so the failed draft still drives the turn to done.
                var edit_event: turn.EditOutcome = veto_result.outcome;
                if (veto_result.outcome.ok and !options.replay_mode) {
                    if (machine.attempt == 1) first_draft_veto_pass = true;
                    const st = try applyVerifiedEdit(allocator, ta, registry, transcript, options, prepared, veto_result.report, null);
                    if (st.denied) {
                        return finishTurn(&machine, turn_usage, .approval_denied, model_roundtrips, applied_edit, applied_proven, applied_tracked, workflow_hint, workflow_hint_injected, first_draft_veto_pass, veto_retry_count, tool_calls_used, compiler_authored_apply);
                    }
                    applied_edit = st.applied;
                    applied_proven = st.proven;
                    applied_tracked = st.tracked;
                }
                if (!veto_result.outcome.ok and veto_result.sql_failure) {
                    sql_veto_fail_count += 1;
                    if (sql_veto_fail_count >= 2) inject_sql_escalation = true;
                }
                // Failed draft: run the deterministic repair lane in-process on
                // the un-written draft. If it produces a candidate that ALSO
                // passes the full veto against the ORIGINAL pre-edit handler (the
                // authority - the lane only verifies repairs against the draft),
                // apply it model-free through the same approval gate and receipt
                // (Phase B). Otherwise feed the exact repair templates + smallest
                // witness into the next retry (Phase A). Skipped for SQL failures
                // (own escalation), zero-new-violation failures, and replay.
                if (!veto_result.outcome.ok and !options.replay_mode and
                    auto_repair.shouldRunLane(veto_result.sql_failure, veto_result.report.new))
                {
                    if (pi_goal_candidate.candidateFromSource(ta, prepared.edit.content, prepared.edit.file, &.{}, max_auto_repairs)) |cand_val| {
                        var cand = cand_val;
                        defer cand.deinit(ta);
                        var phase_b_applied = false;
                        if (cand.verified()) {
                            if (cand.proposed_content) |proposed| {
                                const synth: PreparedEdit = .{
                                    .edit = .{ .file = prepared.edit.file, .content = proposed, .before = prepared.edit.before },
                                    .resolved_path = prepared.resolved_path,
                                };
                                const reveto = try veto.runVetoWithSchema(ta, .{
                                    .file = synth.edit.file,
                                    .content = synth.edit.content,
                                    .before = synth.edit.before,
                                }, sql_schema_path);
                                if (reveto.outcome.ok) {
                                    const st = try applyVerifiedEdit(allocator, ta, registry, transcript, options, synth, reveto.report, cand.plan_ids);
                                    if (st.denied) {
                                        return finishTurn(&machine, turn_usage, .approval_denied, model_roundtrips, applied_edit, applied_proven, applied_tracked, workflow_hint, workflow_hint_injected, first_draft_veto_pass, veto_retry_count, tool_calls_used, compiler_authored_apply);
                                    }
                                    applied_edit = st.applied;
                                    applied_proven = st.proven;
                                    applied_tracked = st.tracked;
                                    compiler_authored_apply = true;
                                    edit_event = .{ .ok = true, .llm_text = try ta.dupe(u8, "compiler-authored repair applied") };
                                    phase_b_applied = true;
                                }
                            }
                        }
                        if (!phase_b_applied) {
                            if (auto_repair.buildRetryBlock(ta, cand.plans_json) catch null) |block| {
                                auto_repair_block = block;
                            }
                        }
                    } else |_| {}
                }
                next_event = .{ .edit_verified = edit_event };
            },
            .invoke_tool_batch => |calls| {
                try transcript.append(allocator, .{ .assistant_tool_use = calls });

                const mixed_apply_edit = containsApplyEdit(calls) and calls.len > 1;
                const over_budget = calls.len > options.max_tool_batch_size or
                    tool_calls_used + calls.len > options.max_tool_calls_per_turn;

                if (mixed_apply_edit or over_budget) {
                    if (over_budget) hit_tool_budget = true;
                    for (calls) |call| {
                        const message = if (mixed_apply_edit)
                            "apply_edit was grouped with other tool calls. It must be issued alone in a single response so the compiler veto can run cleanly. Re-issue just the apply_edit call without any other tools."
                        else
                            "tool-call budget exceeded for this turn";
                        try transcript.append(allocator, .{ .tool_result = .{
                            .tool_use_id = call.id,
                            .tool_name = call.name,
                            .ok = false,
                            .llm_text = message,
                            .ui_payload = null,
                        } });
                    }
                    next_event = .tool_batch_completed;
                    continue;
                }

                tool_calls_used += calls.len;
                for (calls) |call| {
                    var result = try invokeToolRecovering(ta, registry, call);
                    defer result.deinit(ta);
                    try transcript.append(allocator, .{ .tool_result = .{
                        .tool_use_id = call.id,
                        .tool_name = call.name,
                        .ok = result.ok,
                        .llm_text = result.llm_text,
                        .ui_payload = result.ui_payload,
                    } });
                }
                next_event = .tool_batch_completed;
            },
            .render => |msg| {
                // A diagnostic_box in the render arm always means veto exhaustion
                // (the turn machine emits it only after the final failed attempt).
                // Append a concrete next step so the user is not left at a dead end,
                // mirroring the budget-exhausted path. ui_payload is preserved so the
                // structured diagnostic surface is unchanged.
                const entry: turn.Message = switch (msg) {
                    .diagnostic_box => |box| .{ .diagnostic_box = .{
                        .llm_text = try std.fmt.allocPrint(ta, "{s}\n\n{s}", .{ box.llm_text, veto_exhausted_next_step }),
                        .ui_payload = box.ui_payload,
                    } },
                    else => msg,
                };
                try transcript.append(allocator, entry);
                const end_reason: session_events.TurnEndReason = switch (msg) {
                    .diagnostic_box => .veto_exhausted,
                    else => .approved,
                };
                return finishTurn(
                    &machine,
                    turn_usage,
                    end_reason,
                    model_roundtrips,
                    applied_edit,
                    applied_proven,
                    applied_tracked,
                    workflow_hint,
                    workflow_hint_injected,
                    first_draft_veto_pass,
                    veto_retry_count,
                    tool_calls_used,
                    compiler_authored_apply,
                );
            },
            .prompt_user => |question| {
                const text = if (hit_timeout_budget)
                    try std.fmt.allocPrint(
                        ta,
                        "{s}\nTurn time limit reached ({d}s). Run /compact or narrow the task and retry.",
                        .{ question, options.turn_timeout_ms / 1000 },
                    )
                else if (hit_roundtrip_budget)
                    try std.fmt.allocPrint(ta, "{s}\n{s}", .{ question, budget_exhausted_next_step })
                else
                    question;
                try transcript.append(allocator, .{ .diagnostic_box = .{ .llm_text = text } });
                const budget_reason: session_events.TurnEndReason = if (hit_timeout_budget) .budget_timeout else .budget_roundtrips;
                return finishTurn(
                    &machine,
                    turn_usage,
                    budget_reason,
                    model_roundtrips,
                    applied_edit,
                    applied_proven,
                    applied_tracked,
                    workflow_hint,
                    workflow_hint_injected,
                    first_draft_veto_pass,
                    veto_retry_count,
                    tool_calls_used,
                    compiler_authored_apply,
                );
            },
            .end_turn => return finishTurn(
                &machine,
                turn_usage,
                if (hit_tool_budget) .budget_tool_calls else .approved,
                model_roundtrips,
                applied_edit,
                applied_proven,
                applied_tracked,
                workflow_hint,
                workflow_hint_injected,
                first_draft_veto_pass,
                veto_retry_count,
                tool_calls_used,
                compiler_authored_apply,
            ),
            .none => return finishTurn(
                &machine,
                turn_usage,
                if (hit_tool_budget) .budget_tool_calls else .approved,
                model_roundtrips,
                applied_edit,
                applied_proven,
                applied_tracked,
                workflow_hint,
                workflow_hint_injected,
                first_draft_veto_pass,
                veto_retry_count,
                tool_calls_used,
                compiler_authored_apply,
            ),
        }
    }
}

fn finishTurn(
    machine: *const turn.TurnMachine,
    usage: turn.Usage,
    reason: session_events.TurnEndReason,
    model_roundtrips: u8,
    applied_edit: bool,
    applied_proven: u32,
    applied_tracked: u32,
    workflow_hint: expert_workflow.WorkflowHint,
    workflow_hint_injected: bool,
    first_draft_veto_pass: bool,
    veto_retry_count: u32,
    tool_calls_used: usize,
    compiler_authored_apply: bool,
) TurnResult {
    return .{
        .final_state = machine.state,
        .attempt = machine.attempt,
        .usage = usage,
        .end_reason = reason,
        .roundtrips = model_roundtrips,
        .applied_edit = applied_edit,
        .proven_guarantees = applied_proven,
        .tracked_guarantees = applied_tracked,
        .workflow_kind = workflow_hint.kind,
        .workflow_confidence = workflow_hint.confidence,
        .workflow_hint_injected = workflow_hint_injected,
        .first_draft_veto_pass = first_draft_veto_pass,
        .veto_retry_count = veto_retry_count,
        .tool_call_count = @intCast(@min(tool_calls_used, std.math.maxInt(u32))),
        .compiler_authored_apply = compiler_authored_apply,
    };
}

/// Monotonic time in milliseconds. Delegates to zigts.compat.monotonicNowNs
/// which reads CLOCK_MONOTONIC directly and is safe for interval checks.
fn nowMonotonicMs() i64 {
    return @intCast((zigts.compat.monotonicNowNs() catch 0) / 1_000_000);
}

fn invokeToolRecovering(
    allocator: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    call: turn.ToolCall,
) !registry_mod.ToolResult {
    return registry.invokeJson(allocator, call.name, call.args_json) catch |err| switch (err) {
        registry_mod.RegistryError.ToolNotFound => registry_mod.ToolResult.errFmt(
            allocator,
            "unknown tool: {s}",
            .{call.name},
        ),
        error.InvalidToolArgsJson => registry_mod.ToolResult.errFmt(
            allocator,
            "{s}: invalid structured tool arguments",
            .{call.name},
        ),
        else => registry_mod.ToolResult.errFmt(
            allocator,
            "{s}: {s}",
            .{ call.name, @errorName(err) },
        ),
    };
}

fn containsApplyEdit(calls: []const turn.ToolCall) bool {
    for (calls) |call| {
        if (std.mem.eql(u8, call.name, apply_edit.tool_name)) return true;
    }
    return false;
}

fn prepareEdit(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    edit: turn.Edit,
) !PreparedEdit {
    const target_path = try tools_common.resolveInsideWorkspace(allocator, workspace_root, edit.file);
    errdefer allocator.free(target_path);

    const before = edit.before orelse blk: {
        // 16 MiB matches the workspace tools' cap so a normal large handler
        // still provides before-context. FileTooBig (beyond that) is recoverable
        // like FileNotFound: treat it as a fresh edit rather than propagating the
        // error out of runTurnWith, which would crash the whole turn (exit(1) in
        // --print mode) instead of letting the veto re-prompt.
        const current = file_io.readFile(allocator, target_path, 16 * 1024 * 1024) catch |err| switch (err) {
            error.FileNotFound, error.FileTooBig => break :blk null,
            else => return err,
        };
        break :blk current;
    };

    return .{
        .edit = .{
            .file = edit.file,
            .content = edit.content,
            .before = before,
        },
        .resolved_path = target_path,
    };
}

fn applyPreparedEdit(
    allocator: std.mem.Allocator,
    prepared: PreparedEdit,
    content: []const u8,
) !void {
    const parent = std.fs.path.dirname(prepared.resolved_path);
    if (parent) |dir_path| {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), dir_path);
    }
    try file_io.writeFile(allocator, prepared.resolved_path, content);
}

const ApplyState = struct {
    applied: bool = false,
    proven: u32 = 0,
    tracked: u32 = 0,
    /// True when the approval policy rejected the verified edit. The caller
    /// ends the turn with .approval_denied; nothing was written.
    denied: bool = false,
};

/// Apply an already-veto-passed edit: surface the approval preview, write the
/// (normalized) bytes, run the post-apply regression checks, and append the
/// verified-patch receipt. Shared by the model-green path and the model-free
/// compiler-repair path (Phase B) so both go through the exact same approval
/// gate and equivalence receipt. `repair_plan_ids_override` is non-null only
/// for the compiler-repair path, where the plan ids come from the in-process
/// candidate rather than a transcript tool_result.
fn applyVerifiedEdit(
    allocator: std.mem.Allocator,
    ta: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    transcript: *transcript_mod.Transcript,
    options: RunOptions,
    prepared: PreparedEdit,
    report: veto.VetoReport,
    repair_plan_ids_override: ?[]const []const u8,
) !ApplyState {
    // The bytes that will actually be written (post-normalization). Bound here
    // so the approval preview shows exactly what lands.
    const applied_content = report.normalized_content orelse prepared.edit.content;

    // Log canonical normalization to stderr in auto-approve mode so the user
    // knows what changed even when there is no prompt.
    const is_auto_approve: bool = if (options.approval_fn) |fn_| switch (fn_) {
        .bare => |f| f == &autoApprove,
        else => false,
    } else false;
    if (is_auto_approve and report.rewrite_trace.len > 0) {
        var buf: [512]u8 = undefined;
        var fw = std.Io.Writer.fixed(&buf);
        fw.writeAll("note: canonical normalization applied ") catch {};
        fw.print("{d}", .{report.rewrite_trace.len}) catch {};
        fw.writeAll(" rewrite(s): ") catch {};
        for (report.rewrite_trace, 0..) |rname, ri| {
            if (ri > 0) fw.writeAll(", ") catch {};
            fw.writeAll(rname) catch {};
        }
        fw.writeByte('\n') catch {};
        const out = fw.buffered();
        _ = std.c.write(std.c.STDERR_FILENO, out.ptr, out.len);
    }

    if (options.approval_fn) |approve| {
        const preview: ApprovalPreview = .{
            .file = prepared.edit.file,
            .before = prepared.edit.before,
            .after = applied_content,
            .properties = report.after_properties,
            .rewrite_trace = report.rewrite_trace,
        };
        if (!try approve.call(preview)) {
            try transcript.append(allocator, .{ .tool_result = .{
                .tool_use_id = "approval",
                .tool_name = "apply_edit",
                .ok = false,
                .llm_text = "edit verified but not applied by user approval policy",
                .ui_payload = null,
            } });
            return .{ .denied = true };
        }
    }

    // Normalize-on-apply: `applied_content` is the veto's Canonical Normal Form
    // reduction, fed to BOTH the disk write and the verified-patch entry so the
    // file on disk, the equivalence receipt (after=applied), and the transcript
    // attest the same bytes.
    try applyPreparedEdit(ta, prepared, applied_content);
    const post_apply = try postApplyCheck(allocator, ta, registry, transcript, prepared);
    defer if (post_apply.summary) |s| allocator.free(s);
    try appendVerifiedPatchEntry(
        allocator,
        transcript,
        options.workspace_root,
        prepared,
        applied_content,
        report,
        post_apply,
        repair_plan_ids_override,
    );
    var st: ApplyState = .{ .applied = true };
    if (report.after_properties) |snap| {
        const counts = snap.guaranteeCounts();
        st.proven = counts.proven;
        st.tracked = counts.tracked;
    }
    return st;
}

/// One post-apply tool check. The tool name + args + diagnostic prefix +
/// summary text all vary per check; the surrounding "lookup, invoke,
/// transcribe on failure, update report" ceremony is identical. Bundling
/// here so the two checks in `postApplyCheck` stay one-call-site each
/// instead of two ~25-line nested blocks.
const PostCheckSpec = struct {
    tool_name: []const u8,
    args_json: []const u8,
    note_prefix: []const u8,
    summary: []const u8,
    /// `true` for the review-patch check, which is meant to override an
    /// earlier `verify_paths regressed` summary because the review's
    /// finding is more specific. `false` for verify_paths, which only
    /// sets the summary if nothing has set it yet.
    overwrite_summary: bool,
};

fn runPostApplyTool(
    allocator: std.mem.Allocator,
    arena: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    transcript: *transcript_mod.Transcript,
    report: *PostApplyReport,
    spec: PostCheckSpec,
) !void {
    if (registry.findByName(spec.tool_name) == null) return;

    // Note: invokeJson errors are propagated up to postApplyCheck which
    // catches them and returns the report-so-far — preserving the
    // original `catch return report` short-circuit semantics.
    var result = try registry.invokeJson(arena, spec.tool_name, spec.args_json);
    defer result.deinit(arena);
    if (result.ok) return;

    const note = try std.fmt.allocPrint(allocator, "{s}{s}", .{ spec.note_prefix, result.llm_text });
    defer allocator.free(note);
    try transcript.append(allocator, .{ .diagnostic_box = .{
        .llm_text = note,
        .ui_payload = if (result.ui_payload) |payload|
            try payload.clone(allocator)
        else
            null,
    } });

    report.ok = false;
    if (spec.overwrite_summary) {
        // Null the field BEFORE the free, then re-assign. If `dupe` later
        // OOMs the field is null rather than dangling — otherwise the
        // caller's `runPostApplyTool(...) catch return report` would
        // swallow the error and the outer call site's
        // `defer if (post_apply.summary) |s| allocator.free(s);` would
        // free the already-freed pointer. Guards against the regression
        // verified in the prior code review (CONFIRMED at loop.zig:393).
        if (report.summary) |s| {
            report.summary = null;
            allocator.free(s);
        }
        report.summary = try allocator.dupe(u8, spec.summary);
    } else if (report.summary == null) {
        report.summary = try allocator.dupe(u8, spec.summary);
    }
}

fn postApplyCheck(
    allocator: std.mem.Allocator,
    arena: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    transcript: *transcript_mod.Transcript,
    prepared: PreparedEdit,
) !PostApplyReport {
    var report: PostApplyReport = .{ .ok = true, .summary = null };
    errdefer if (report.summary) |s| allocator.free(s);

    // Build the args JSON with the escaping writer rather than raw {s}
    // interpolation: edit.file is model-controlled, so a quote/backslash/control
    // char would otherwise produce malformed JSON and silently disable this
    // post-apply regression gate.
    const verify_paths_args = blk: {
        var buf: std.ArrayList(u8) = .empty;
        var aw: std.Io.Writer.Allocating = .fromArrayList(arena, &buf);
        const w = &aw.writer;
        try w.writeAll("{\"paths\":[");
        try json_writer.writeString(w, prepared.edit.file);
        try w.writeAll("]}");
        buf = aw.toArrayList();
        break :blk buf.items;
    };
    runPostApplyTool(allocator, arena, registry, transcript, &report, .{
        .tool_name = "zigts_expert_verify_paths",
        .args_json = verify_paths_args,
        .note_prefix = "post-apply regression: verify_paths found violations\n",
        .summary = "verify_paths regressed",
        .overwrite_summary = false,
    }) catch return report;

    if (prepared.edit.before != null) {
        const review_args = blk: {
            var buf: std.ArrayList(u8) = .empty;
            var aw: std.Io.Writer.Allocating = .fromArrayList(arena, &buf);
            const w = &aw.writer;
            try w.writeAll("{\"file\":");
            try json_writer.writeString(w, prepared.edit.file);
            try w.writeAll(",\"diff_only\":true}");
            buf = aw.toArrayList();
            break :blk buf.items;
        };
        runPostApplyTool(allocator, arena, registry, transcript, &report, .{
            .tool_name = "zigts_expert_review_patch",
            .args_json = review_args,
            .note_prefix = "post-apply diff review: new violations found\n",
            .summary = "review_patch flagged new violations",
            .overwrite_summary = true,
        }) catch return report;
    }

    return report;
}

fn appendVerifiedPatchEntry(
    allocator: std.mem.Allocator,
    transcript: *transcript_mod.Transcript,
    workspace_root: []const u8,
    prepared: PreparedEdit,
    applied_content: []const u8,
    report: veto.VetoReport,
    post_apply: PostApplyReport,
    repair_plan_ids_override: ?[]const []const u8,
) !void {
    const workspace_root_abs = try std.fs.path.resolve(allocator, &.{workspace_root});
    defer allocator.free(workspace_root_abs);
    // Repair-plan provenance: the model-free compiler-repair path passes its
    // candidate's plan ids directly (there is no pi_apply_repair_plan
    // tool_result in the transcript to scan). The model path passes null and we
    // recover the links from the transcript, keying on the model's *proposed*
    // content (what the candidate tool emitted), not the post-normalize bytes —
    // a candidate the model echoed verbatim should still link even if normalize
    // then canonicalized it on the way to disk.
    var owned_links: ?RepairLinks = null;
    defer if (owned_links) |*l| l.deinit(allocator);
    const repair_plan_ids: []const []const u8 = if (repair_plan_ids_override) |ids| ids else blk: {
        owned_links = try collectRecentRepairLinks(
            allocator,
            transcript,
            workspace_root_abs,
            prepared.edit.file,
            prepared.edit.content,
        );
        break :blk owned_links.?.repair_plan_ids;
    };

    // `after` (the equivalence-receipt after-image), the disk write, and the
    // transcript all attest `applied_content` — the canonicalized bytes.
    const payload: ui_payload_mod.UiPayload = .{ .verified_patch = try proof_enrichment.buildVerifiedPatchPayload(
        allocator,
        .{
            .workspace_root = workspace_root_abs,
            .file = prepared.edit.file,
            .before = prepared.edit.before,
            .after = applied_content,
            .policy_hash = report.policy_hash,
            .applied_at_unix_ms = tools_common.nowUnixMs(),
            .post_apply_ok = post_apply.ok,
            .post_apply_summary = post_apply.summary,
            .transcript = transcript,
            .repair_plan_ids = repair_plan_ids,
            .emit_perf_receipt = true,
            .is_canonical = report.is_canonical,
            .rewrite_trace = report.rewrite_trace,
        },
    ) };
    errdefer {
        var owned = payload;
        owned.deinit(allocator);
    }

    const patch = payload.verified_patch;
    const summary_line = try std.fmt.allocPrint(
        allocator,
        "verified: {s} ({s}, {d} total, {d} new, {d} preexisting)",
        .{
            patch.file,
            if (patch.prove) |prove| prove.classification else "unclassified",
            patch.stats.total,
            patch.stats.new,
            patch.stats.preexisting orelse 0,
        },
    );
    errdefer allocator.free(summary_line);

    try transcript.entries.append(allocator, .{ .verified_patch = .{
        .llm_text = summary_line,
        .ui_payload = payload,
    } });
}

fn collectRecentRepairLinks(
    allocator: std.mem.Allocator,
    transcript: *const transcript_mod.Transcript,
    workspace_root_abs: []const u8,
    file: []const u8,
    content: []const u8,
) !RepairLinks {
    const file_abs = try std.fs.path.resolve(allocator, &.{ workspace_root_abs, file });
    defer allocator.free(file_abs);
    var i = transcript.len();
    while (i > 0) {
        i -= 1;
        switch (transcript.at(i).*) {
            .tool_result => |result| {
                if (std.mem.eql(u8, result.tool_name, "pi_apply_repair_plan")) {
                    if (try repairLinksFromCandidate(allocator, result.ui_payload, workspace_root_abs, file_abs, content)) |links| return links;
                }
            },
            else => {},
        }
    }
    return .{};
}

fn repairLinksFromCandidate(
    allocator: std.mem.Allocator,
    payload: ?ui_payload_mod.UiPayload,
    workspace_root_abs: []const u8,
    file_abs: []const u8,
    content: []const u8,
) !?RepairLinks {
    const value = payload orelse return null;
    return switch (value) {
        .repair_candidate => |candidate| blk: {
            const candidate_abs = try std.fs.path.resolve(allocator, &.{ workspace_root_abs, candidate.path });
            defer allocator.free(candidate_abs);
            if (!std.mem.eql(u8, candidate_abs, file_abs)) break :blk null;
            if (!std.mem.eql(u8, candidate.proposed_content, content)) break :blk null;
            const repair_plan_ids = try allocator.alloc([]u8, 1);
            errdefer allocator.free(repair_plan_ids);
            repair_plan_ids[0] = try allocator.dupe(u8, candidate.plan_id);
            break :blk .{ .repair_plan_ids = repair_plan_ids };
        },
        else => null,
    };
}

const testing = std.testing;
const Tag = transcript_mod.Tag;

const CannedClient = struct {
    reply: turn.AssistantReply,
    saw_workflow_note: bool = false,
    request_count: usize = 0,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!ModelCallResult {
        const self: *CannedClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = extra_user_text;
        self.request_count += 1;
        for (transcript.entries.items) |entry| {
            switch (entry) {
                .system_note => |body| {
                    if (std.mem.indexOf(u8, body, "[expert workflow]") != null) {
                        self.saw_workflow_note = true;
                    }
                },
                else => {},
            }
        }
        return .{ .reply = self.reply };
    }

    pub fn asClient(self: *CannedClient) ModelClient {
        return .{ .context = self, .request_fn = requestFn };
    }
};

const SequenceClient = struct {
    replies: []const turn.AssistantReply,
    index: usize = 0,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!ModelCallResult {
        const self: *SequenceClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
        if (self.index >= self.replies.len) return error.TestSequenceExhausted;
        const reply = self.replies[self.index];
        self.index += 1;
        return .{ .reply = reply };
    }

    pub fn asClient(self: *SequenceClient) ModelClient {
        return .{ .context = self, .request_fn = requestFn };
    }
};

fn stubExecute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    _ = args;
    return .{ .ok = true, .llm_text = try allocator.dupe(u8, "{\"stub\":\"ok\"}\n") };
}

fn stubDecodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    return registry_mod.helpers.decodeNoArgs(allocator, args_json);
}

const stub_tool: registry_mod.ToolDef = .{
    .name = "stub",
    .label = "stub",
    .description = "Test stub",
    .input_schema = "{\"type\":\"object\",\"properties\":{},\"required\":[]}",
    .decode_json = stubDecodeJson,
    .execute = stubExecute,
};

// `std.testing.tmpDir` creates `.zig-cache/tmp/<sub_path>/`, but `tmp.sub_path`
// is only the 16-char random component. Resolving it directly against CWD
// (the repo root) would create stray `<repo>/<sub_path>/` folders that
// `tmp.cleanup()` never deletes. Compose the full relative path so writes
// land inside the real tmp dir.
fn tmpWorkspacePath(allocator: std.mem.Allocator, tmp: *const std.testing.TmpDir) ![]u8 {
    return std.fmt.allocPrint(allocator, ".zig-cache/tmp/{s}", .{tmp.sub_path});
}

const bad_handler =
    "function handler(req: Request): Response & Spec<\"deterministic\"> { var x = 1; return Response.json({x}); }";
const clean_handler =
    "function handler(req: Request): Response & Spec<\"deterministic\"> { return Response.json({ok: true}); }";

// Accesses result.value without checking result.ok: a HandlerVerifier error
// (ZTS303 unchecked_result_value) that the repair lane can author a fix for.
const unchecked_result_handler =
    "import { validateJson } from \"zigttp:validate\";\n" ++
    "function handler(req: Request): Response & Spec<\"deterministic\"> {\n" ++
    "  const result = validateJson(\"item\", req.body);\n" ++
    "  const data = result.value;\n" ++
    "  return Response.json({ data });\n" ++
    "}\n";

// A scripted client that flags whether any retry prompt carried the
// compiler-authored repair block, so the auto-repair wiring can be asserted
// end-to-end without reaching into the loop's internal prompt construction.
const RetryCaptureClient = struct {
    replies: []const turn.AssistantReply,
    index: usize = 0,
    saw_repair_block: bool = false,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!ModelCallResult {
        const self: *RetryCaptureClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        if (extra_user_text) |t| {
            if (std.mem.indexOf(u8, t, "COMPILER-AUTHORED FIX") != null) self.saw_repair_block = true;
        }
        if (self.index >= self.replies.len) return error.TestSequenceExhausted;
        const reply = self.replies[self.index];
        self.index += 1;
        return .{ .reply = reply };
    }

    pub fn asClient(self: *RetryCaptureClient) ModelClient {
        return .{ .context = self, .request_fn = requestFn };
    }
};

test "veto failure triggers model-free compiler-authored apply" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/src/handler.ts", .{workspace_root});
    defer testing.allocator.free(written_path);

    var client: RetryCaptureClient = .{ .replies = &.{
        .{ .response = .{ .edit = .{ .file = "src/handler.ts", .content = unchecked_result_handler, .before = null } } },
        .{ .response = .{ .edit = .{ .file = "src/handler.ts", .content = clean_handler, .before = null } } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(testing.allocator, client.asClient(), &registry, &tr, "fix the handler", .{
        .workspace_root = workspace_root,
        .max_attempts = 2,
        .approval_fn = ApprovalFn.fromFn(autoApprove),
        .turn_timeout_ms = 0,
    });

    // The first draft (unchecked result) failed veto; the deterministic lane
    // authored a fix that passed the binding re-veto and landed model-free -
    // no retry, no second model call.
    try testing.expect(result.applied_edit);
    try testing.expect(result.compiler_authored_apply);
    try testing.expectEqual(@as(u32, 0), result.veto_retry_count);
    try testing.expect(!client.saw_repair_block);
    try testing.expectEqual(@as(usize, 1), client.index); // second reply never requested

    // The compiler-authored guard is what actually landed on disk.
    const written = try file_io.readFile(testing.allocator, written_path, 1 << 20);
    defer testing.allocator.free(written);
    try testing.expect(std.mem.indexOf(u8, written, "if (!result.ok)") != null);
}

test "text reply path injects workflow note before model text" {
    var canned: CannedClient = .{ .reply = .{
        .response = .{ .final_text = "here is the plan" },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(testing.allocator, canned.asClient(), &registry, &tr, "add a GET route", .{});
    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(expert_workflow.TaskKind.route_add, result.workflow_kind);
    try testing.expect(result.workflow_hint_injected);
    try testing.expect(canned.saw_workflow_note);
    try testing.expectEqual(@as(usize, 1), canned.request_count);
    switch (tr.at(0).*) {
        .user_text => |body| try testing.expectEqualStrings("add a GET route", body),
        else => return error.TestFailed,
    }
    switch (tr.at(1).*) {
        .system_note => |body| try testing.expect(std.mem.indexOf(u8, body, "pi_forge_route") != null),
        else => return error.TestFailed,
    }
    switch (tr.at(2).*) {
        .model_text => |body| try testing.expectEqualStrings("here is the plan", body),
        else => return error.TestFailed,
    }
}

test "clean edit path: veto passes and writes file" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/src/handler.ts", .{workspace_root});
    defer testing.allocator.free(written_path);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "src/handler.ts",
            .content = clean_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add an ok response",
        .{ .workspace_root = workspace_root },
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    switch (tr.at(tr.len() - 1).*) {
        .proof_card => |body| try testing.expect(std.mem.indexOf(u8, body.llm_text, "\"total\":0") != null),
        else => return error.TestFailed,
    }
    const written = try file_io.readFile(testing.allocator, written_path, 1024 * 1024);
    defer testing.allocator.free(written);
    try testing.expectEqualStrings(clean_handler, written);
}

test "broken edit path: veto fails with diagnostic box" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "src/handler.ts",
            .content = bad_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add a bad handler",
        .{ .workspace_root = workspace_root, .max_attempts = 1 },
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(session_events.TurnEndReason.veto_exhausted, result.end_reason);
    switch (tr.at(tr.len() - 1).*) {
        .diagnostic_box => |body| {
            // The recurring diagnostic is still present...
            try testing.expect(std.mem.indexOf(u8, body.llm_text, "\"ZTS001\"") != null);
            // ...and the box now ends with a concrete next step rather than a
            // silent dead end (Set C: veto-exhaustion off-ramp).
            try testing.expect(std.mem.indexOf(u8, body.llm_text, veto_exhausted_next_step) != null);
        },
        else => return error.TestFailed,
    }
}

test "tool batch path: invoke_tool_batch -> tool_result -> final model text" {
    const replies = [_]turn.AssistantReply{
        .{
            .preamble = "I'll inspect first.",
            .response = .{ .tool_calls = &[_]turn.ToolCall{
                .{ .id = "toolu_stub", .name = "stub", .args_json = "{}" },
            } },
        },
        .{
            .response = .{ .final_text = "inspection complete" },
        },
    };
    var seq: SequenceClient = .{ .replies = &replies };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);
    try registry.register(testing.allocator, stub_tool);

    const result = try runTurnWith(testing.allocator, seq.asClient(), &registry, &tr, "run the stub", .{});
    try testing.expectEqual(turn.TurnState.done, result.final_state);
    switch (tr.at(1).*) {
        .model_text => |body| try testing.expectEqualStrings("I'll inspect first.", body),
        else => return error.TestFailed,
    }
    try testing.expectEqual(Tag.assistant_tool_use, @as(Tag, tr.at(2).*));
}

test "retry: one bad draft then one good draft lands a proof card" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/handler.ts", .{workspace_root});
    defer testing.allocator.free(written_path);

    const replies = [_]turn.AssistantReply{
        .{ .response = .{ .edit = .{ .file = "handler.ts", .content = bad_handler, .before = null } } },
        .{ .response = .{ .edit = .{ .file = "handler.ts", .content = clean_handler, .before = null } } },
    };
    var seq: SequenceClient = .{ .replies = &replies };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(
        testing.allocator,
        seq.asClient(),
        &registry,
        &tr,
        "add a GET route",
        .{ .workspace_root = workspace_root },
    );

    try testing.expectEqual(@as(u8, 2), result.attempt);
    try testing.expectEqual(@as(u32, 1), result.veto_retry_count);
    try testing.expect(!result.first_draft_veto_pass);
    switch (tr.at(tr.len() - 1).*) {
        .proof_card => {},
        else => return error.TestFailed,
    }
    const written = try file_io.readFile(testing.allocator, written_path, 1024 * 1024);
    defer testing.allocator.free(written);
    try testing.expectEqualStrings(clean_handler, written);
}

test "approval callback can block an otherwise verified edit from being written" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/handler.ts", .{workspace_root});
    defer testing.allocator.free(written_path);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "handler.ts",
            .content = clean_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    _ = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add a GET route",
        .{
            .workspace_root = workspace_root,
            .approval_fn = ApprovalFn.fromFn(autoReject),
        },
    );

    switch (tr.at(tr.len() - 1).*) {
        .tool_result => |result| try testing.expect(std.mem.indexOf(u8, result.llm_text, "not applied") != null),
        else => return error.TestFailed,
    }
    try testing.expect(!file_io.fileExists(testing.allocator, written_path));
}

test "edit path outside the workspace is surfaced as a recoverable diagnostic, not a crash" {
    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "../outside-handler.ts",
            .content = clean_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    // An out-of-workspace path is the model's mistake, not a fatal agent
    // error: the turn must complete (re-prompting the model) instead of
    // propagating the error and crashing the whole agent.
    const result = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "escape the workspace",
        .{},
    );
    _ = result;

    var saw_path_diag = false;
    for (tr.entries.items) |*entry| {
        switch (entry.*) {
            .diagnostic_box => |box| {
                if (std.mem.indexOf(u8, box.llm_text, "outside the workspace") != null) {
                    saw_path_diag = true;
                }
            },
            else => {},
        }
    }
    try testing.expect(saw_path_diag);
}

test "resolveInsideWorkspace converts a relative root to absolute" {
    const abs = try tools_common.resolveInsideWorkspace(testing.allocator, ".", "build.zig");
    defer testing.allocator.free(abs);
    try testing.expect(std.fs.path.isAbsolute(abs));
    try testing.expect(std.mem.endsWith(u8, abs, "build.zig"));
}

test "prepareEdit accepts an in-tree relative path under the default '.' root" {
    // Regression guard: std.fs.path.resolve is lexical, so a relative
    // workspace_root (".") used to leave every relative edit path rejected as
    // "outside the workspace" - which meant the agent could never apply an
    // edit in --print mode. The root must be anchored at the real cwd.
    const prepared = try prepareEdit(testing.allocator, ".", .{
        .file = "src/handler.ts",
        .content = "x",
        .before = null,
    });
    defer testing.allocator.free(prepared.resolved_path);
    defer if (prepared.edit.before) |b| testing.allocator.free(b);
    try testing.expect(std.fs.path.isAbsolute(prepared.resolved_path));
    try testing.expect(std.mem.endsWith(u8, prepared.resolved_path, "src/handler.ts"));
}

test "autoApprove returns true for any preview" {
    try testing.expect(try autoApprove(.{ .file = "any/file.zig", .after = "x" }));
}

test "autoReject returns false for any preview" {
    try testing.expect(!try autoReject(.{ .file = "any/file.zig", .after = "x" }));
}

test "ApprovalPolicy enum is exported" {
    try testing.expect(@sizeOf(ApprovalPolicy) > 0);
}

test "replay_mode skips filesystem writes for a verified edit" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/src/handler.ts", .{workspace_root});
    defer testing.allocator.free(written_path);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "src/handler.ts",
            .content = clean_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add an ok response",
        .{ .workspace_root = workspace_root, .replay_mode = true },
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    switch (tr.at(tr.len() - 1).*) {
        .proof_card => {},
        else => return error.TestFailed,
    }
    try testing.expect(!file_io.fileExists(testing.allocator, written_path));
}

test "replay_mode skips the approval callback" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/handler.ts", .{workspace_root});
    defer testing.allocator.free(written_path);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "handler.ts",
            .content = clean_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    _ = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add a GET route",
        .{
            .workspace_root = workspace_root,
            .approval_fn = ApprovalFn.fromFn(autoReject),
            .replay_mode = true,
        },
    );

    switch (tr.at(tr.len() - 1).*) {
        .proof_card => {},
        else => return error.TestFailed,
    }
    try testing.expect(!file_io.fileExists(testing.allocator, written_path));
}

test "replay_mode off preserves existing write behavior" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/src/handler.ts", .{workspace_root});
    defer testing.allocator.free(written_path);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "src/handler.ts",
            .content = clean_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    _ = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add an ok response",
        .{ .workspace_root = workspace_root, .replay_mode = false },
    );

    switch (tr.at(tr.len() - 1).*) {
        .proof_card => {},
        else => return error.TestFailed,
    }
    const written = try file_io.readFile(testing.allocator, written_path, 1024 * 1024);
    defer testing.allocator.free(written);
    try testing.expectEqualStrings(clean_handler, written);
}

test "verified edit path appends a verified_patch entry before the proof card" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "handler.ts",
            .content = clean_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "write the handler",
        .{ .workspace_root = workspace_root },
    );
    try testing.expectEqual(turn.TurnState.done, result.final_state);

    // proof_card remains the final entry so the turn state machine contract is
    // unchanged; verified_patch is the entry immediately before it.
    switch (tr.at(tr.len() - 1).*) {
        .proof_card => {},
        else => return error.TestFailed,
    }
    try testing.expect(tr.len() >= 2);
    switch (tr.at(tr.len() - 2).*) {
        .verified_patch => |message| {
            try testing.expect(message.ui_payload != null);
            switch (message.ui_payload.?) {
                .verified_patch => |payload| {
                    try testing.expectEqualStrings("handler.ts", payload.file);
                    try testing.expectEqual(@as(usize, 64), payload.policy_hash.len);
                    try testing.expect(payload.before == null);
                    try testing.expectEqualStrings(clean_handler, payload.after);
                    try testing.expect(payload.post_apply_ok);
                    try testing.expect(payload.post_apply_summary == null);
                    try testing.expectEqual(@as(u32, 0), payload.stats.new);
                },
                else => return error.TestFailed,
            }
        },
        else => return error.TestFailed,
    }
}

// A legal first-draft a model would naturally emit (an arrow-form handler)
// that passes the veto on the first attempt. The applied bytes are normalized
// on the way to disk; the file content, the attested `after`, and the
// `is_canonical` flag must all agree. Because every canonical-band rule is a
// hard `check` error that would itself fail the veto, an edit that reaches the
// green arm is already canonical, so normalize is a behavior-preserving no-op
// and the attested bytes equal the model's draft byte-for-byte.
const arrow_handler =
    "const handler = (req: Request): Response & Spec<\"deterministic\"> => Response.json({ok: true});";

test "non-canonical-but-legal first draft lands in one attempt; disk == attested bytes and is_canonical" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/handler.ts", .{workspace_root});
    defer testing.allocator.free(written_path);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "handler.ts",
            .content = arrow_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add an ok response",
        .{ .workspace_root = workspace_root },
    );

    // One attempt: the draft passed the veto without any retry_draft round.
    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(@as(u8, 1), result.attempt);

    // The verified_patch entry sits directly before the final proof card.
    switch (tr.at(tr.len() - 1).*) {
        .proof_card => {},
        else => return error.TestFailed,
    }
    const attested = switch (tr.at(tr.len() - 2).*) {
        .verified_patch => |message| blk: {
            switch (message.ui_payload.?) {
                .verified_patch => |payload| {
                    try testing.expect(payload.is_canonical);
                    break :blk payload.after;
                },
                else => return error.TestFailed,
            }
        },
        else => return error.TestFailed,
    };

    // Disk bytes == attested `after` bytes == the model's draft (normalize was
    // a behavior-preserving no-op for this veto-passing edit).
    const written = try file_io.readFile(testing.allocator, written_path, 1024 * 1024);
    defer testing.allocator.free(written);
    try testing.expectEqualStrings(attested, written);
    try testing.expectEqualStrings(arrow_handler, written);
}

test "verified patch does not infer links from broad repair plan result" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "handler.ts",
            .content = clean_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const calls = [_]turn.ToolCall{
        .{ .id = "toolu_repair", .name = "pi_repair_plan", .args_json = "{\"path\":\"handler.ts\"}" },
    };
    try tr.append(testing.allocator, .{ .assistant_tool_use = &calls });
    try tr.append(testing.allocator, .{ .tool_result = .{
        .tool_use_id = "toolu_repair",
        .tool_name = "pi_repair_plan",
        .ok = false,
        .llm_text =
        \\{"ok":false,"plans":[{"id":"rp_001","closes":["wit_001"]},{"id":"rp_002","closes":["wit_002"]}]}
        ,
        .ui_payload = null,
    } });

    _ = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "write the handler",
        .{ .workspace_root = workspace_root },
    );

    switch (tr.at(tr.len() - 2).*) {
        .verified_patch => |message| switch (message.ui_payload.?) {
            .verified_patch => |payload| {
                try testing.expectEqual(@as(usize, 0), payload.repair_plan_ids.len);
                try testing.expectEqual(@as(usize, 0), payload.closed_witness_ids.len);
            },
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}

test "verified patch records matching repair candidate plan link" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "handler.ts",
            .content = clean_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    var candidate_payload: ui_payload_mod.UiPayload = .{ .repair_candidate = try ui_payload_mod.RepairCandidatePayload.init(
        testing.allocator,
        "handler.ts",
        "rp_candidate",
        "insert_guard_before_line",
        clean_handler,
        true,
        "0 total, 0 new, 0 preexisting",
        .{ .total = 0, .new = 0, .preexisting = 0 },
    ) };
    defer candidate_payload.deinit(testing.allocator);
    try tr.append(testing.allocator, .{ .tool_result = .{
        .tool_use_id = "toolu_apply_repair",
        .tool_name = "pi_apply_repair_plan",
        .ok = true,
        .llm_text = "{\"ok\":true,\"applied\":false}",
        .ui_payload = candidate_payload,
    } });

    _ = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "write the candidate",
        .{ .workspace_root = workspace_root },
    );

    switch (tr.at(tr.len() - 2).*) {
        .verified_patch => |message| switch (message.ui_payload.?) {
            .verified_patch => |payload| {
                try testing.expectEqual(@as(usize, 1), payload.repair_plan_ids.len);
                try testing.expectEqualStrings("rp_candidate", payload.repair_plan_ids[0]);
            },
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}

test "failed veto does not append a verified_patch entry" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const workspace_root = try tmpWorkspacePath(testing.allocator, &tmp);
    defer testing.allocator.free(workspace_root);

    var canned: CannedClient = .{ .reply = .{
        .response = .{ .edit = .{
            .file = "handler.ts",
            .content = bad_handler,
            .before = null,
        } },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    _ = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add a bad handler",
        .{ .workspace_root = workspace_root, .max_attempts = 1 },
    );

    for (tr.entries.items) |*entry| {
        switch (entry.*) {
            .verified_patch => return error.TestFailed,
            else => {},
        }
    }
}

test "providerErrorRemediation: SSE parse errors map to a non-empty actionable hint" {
    // EXP-12: a proxy-mangled stream previously printed a bare CamelCase name.
    const sse_errors = [_]anyerror{
        error.MalformedSse,
        error.MissingType,
        error.UnknownEventType,
        error.UnexpectedJsonShape,
    };
    for (sse_errors) |err| {
        const hint = providerErrorRemediation(err) orelse return error.TestFailed;
        try testing.expect(hint.len > 0);
        try testing.expect(std.mem.indexOf(u8, hint, "could not be parsed") != null);
    }
}

test "budget-exhausted prompt carries a concrete next step" {
    // EXP-9: the bare "budget exhausted" line must end with an actionable step.
    try testing.expect(budget_exhausted_next_step.len > 0);
    try testing.expect(std.mem.indexOf(u8, budget_exhausted_next_step, "zigttp check") != null);
}
