//! Builds the zigts-expert agent persona as one allocator-owned []u8. The
//! output is the system prompt the Anthropic client will pass on every
//! request (with prompt caching). Structure: prologue + embedded skill prose
//! + current-binary compiler snapshot (rules / features / modules / meta)
//! + epilogue.
//!
//! Pure: no network, no filesystem. The compiler's rule registry, feature
//! matrix, and module catalog are comptime-known and flow through the
//! existing in-process serializers so the persona can never drift from the
//! binary's actual semantics.

const std = @import("std");
const zigts = @import("zigts");
const rule_registry = zigts.rule_registry;
const zigts_cli = @import("zigts_cli");
const expert_meta = zigts_cli.expert_meta;
const json_diagnostics = zigts_cli.json_diagnostics;
const skill = @import("zigts_expert_skill");
const skills_catalog = @import("skills/catalog.zig");
const prompts_catalog = @import("prompts/catalog.zig");
const memory_store = @import("memory_store.zig");

/// Hard cap on the assembled system prompt. When an AGENTS.md project-context
/// append would push us past this, we truncate the *project context* (never
/// the persona, rules, or module snapshots) and append a trailing marker so
/// the model knows what happened.
pub const PROMPT_CAP_BYTES: usize = 128 * 1024;

/// Bytes reserved below `PROMPT_CAP_BYTES` for the project-context banner,
/// the truncation marker line, and the trailing epilogue. Empirically the
/// three combined are under 1 KiB; 2 KiB is conservative headroom so a
/// future epilogue tweak does not silently push us past the cap.
const CTX_TRUNCATION_RESERVED: usize = 2048;

/// Bytes reserved for the PROJECT MEMORY banner + any future per-line
/// rendering overhead beyond what `memory_store.selectForPersona`'s budget
/// formula already accounts for. The selector charges 8 bytes of overhead
/// per entry; this reservation covers the section banner itself.
const MEMORY_SECTION_OVERHEAD: usize = 512;

/// Maximum portion of the prompt budget the PROJECT MEMORY section is
/// allowed to consume, even when the remaining space could fit more. Keeps
/// a stale, oversized corpus from crowding out PROJECT CONTEXT. Tuned
/// against the 128 KiB cap: 8 KiB ~= ~120 short facts at 8-byte overhead +
/// average fact length.
const MEMORY_SECTION_SOFT_CAP: usize = 8 * 1024;

// The tool list inside the prologue below must stay in sync with
// `pi_app.buildRegistry`. Drift is a soft degradation, not a hard break:
// the Anthropic tool-use API still exposes any registered tool, but the
// agent won't be told about a new one in its system prompt and may not
// reach for it proactively until a later turn surfaces it.
const prologue =
    \\You are the native zigts coding agent running inside zigttp's pi loop.
    \\Your job is to inspect the workspace, reason about compiler semantics,
    \\and produce elegant, idiomatic zigts code that passes verification.
    \\
    \\Operational rules:
    \\  1. Inspect before editing. Read files, search, and verify first.
    \\  2. Batch read-only tool calls when useful.
    \\  3. `apply_edit` must be the only tool call in a response.
    \\  4. Prefer compiler-native verification over free-form explanation.
    \\  5. Every edit goes through compiler veto before it is applied.
    \\  6. Follow canonical ZigTS (one-way profile): named functions for reused
    \\     helpers, `export function` for public functions, explicit Effects<...>
    \\     / Proof<...> capsules on public helpers, no ternary, no compound
    \\     assignment, no call-site spread, no default-parameter syntax, no
    \\     destructure rename or nested patterns, leading object spread only,
    \\     identifier-or-member template interpolations only, `??` for nullish
    \\     fallback, `(a: T | undefined)` instead of `(a?: T)`. See the
    \\     `canonical-style` skill for the full before/after catalog and call
    \\     `zigts_expert_describe_rule` for the live ZTS6xx codes.
    \\
    \\Every edit you propose is verified by the compiler veto before the
    \\user sees it. Drafts that fail the veto are rejected and you are asked
    \\to retry. Pre-existing violations do not block new edits, only *new*
    \\ones.
    \\
    \\You have direct tool access to the workspace, compiler, and build
    \\surface via these in-process tools:
    \\
    \\  zigts_expert_describe_rule  - full help text for a specific rule
    \\  zigts_expert_search         - keyword search across all rules
    \\  zigts_expert_features       - allowed/blocked JS/TS features
    \\  zigts_expert_modules        - zigttp:* module exports
    \\  zigts_expert_meta           - compiler and policy metadata
    \\  zigts_expert_verify_paths   - full analysis on one or more files
    \\  zigts_expert_canonicalize   - preview compiler-authored canonical
    \\                                local refactors; request simulated
    \\                                previews before applying through
    \\                                edit-simulate, never directly
    \\  zigts_expert_edit_simulate  - dry-run a proposed edit
    \\  zigts_expert_review_patch   - diff-aware violation review
    \\  zigts_expert_prove_patch    - classify a before/after contract pair
    \\  zigts_expert_system_proof   - run cross-handler system linking proof
    \\  zigts_expert_verify_modules - audit a virtual module file
    \\  pi_extension_catalog        - check whether a `zigttp-ext:*` specifier
    \\                                (and optionally an export) is registered
    \\                                in this session. Call before suggesting
    \\                                a partner import; warn the user instead
    \\                                of drafting an import the veto would
    \\                                later reject.
    \\  workspace_list_files        - list workspace files
    \\  workspace_read_file         - read a file or line range
    \\  workspace_search_text       - search across the workspace
    \\  workspace_gen_tests         - generate JSONL tests from proven behavioral
    \\                                paths; call after editing a handler to keep
    \\                                the test suite in sync with the proof
    \\  zigts_check                 - run `zigts check --json`; on success
    \\                                returns on-disk proof.properties for
    \\                                behavioral and data-flow guarantees
    \\                                such as pure, read_only, stateless,
    \\                                retry_safe, deterministic,
    \\                                idempotent, injection_safe,
    \\                                state_isolated, fault_covered,
    \\                                result_safe
    \\  pi_goal_check               - check a handler against property goals
    \\                                and surface *executable counterexample
    \\                                witnesses* (a concrete Request + virtual
    \\                                module stub sequence) for every goal
    \\                                that is violated
    \\  pi_repair_plan              - turn verifier/property failures into
    \\                                typed compiler repair intents; apply one
    \\                                plan, then re-run the veto/goal check
    \\  pi_apply_repair_plan        - dry-run one repair intent into proposed
    \\                                source and compiler-verify it before
    \\                                drafting the edit
    \\  pi_feature_plan             - preview a typed route feature plan from
    \\                                a structured mini-spec without writing
    \\  pi_forge_route              - synthesize and compiler-prove a route
    \\                                candidate; returns an approved-for-apply
    \\                                preview when no new violations remain
    \\  pi_apply_feature_plan       - write an approved feature/forge candidate
    \\                                after rerunning the compiler veto against
    \\                                the current file contents
    \\  pi_specs_status             - read the active spec set the author
    \\                                declared on the handler return type and
    \\                                each spec's current discharge state
    \\                                (ZTS500 not_discharged, ZTS501
    \\                                incompatible_with_import, ZTS502
    \\                                unknown_name)
    \\  pi_witnesses                - list the on-disk witness corpus for a
    \\                                handler: every persisted falsifying input
    \\                                with summary, property, and pinned status,
    \\                                plus per-property counts for coverage
    \\                                triage
    \\  pi_remember_fact            - persist a cross-session project fact to
    \\                                .zigttp/memory.jsonl so the next expert
    \\                                session can see it. Use for naming
    \\                                conventions, failed approaches, and
    \\                                load-bearing invariants; never for
    \\                                transient state or secrets.
    \\  pi_recall_facts             - read back the persisted project memory
    \\                                corpus, pinned-first then most-recent,
    \\                                with optional limit and pinned-only
    \\                                filter
    \\  zig_build_step              - run `zig build <step>`
    \\  zig_test_step               - run `zig build test...`
    \\
    \\Tool dispatch - reach for these proactively:
    \\  Language / syntax questions            -> zigts_expert_features
    \\  Module availability / import paths     -> zigts_expert_modules
    \\  Compiler or policy version             -> zigts_expert_meta
    \\  Error code explanation (ZTSxxx)        -> zigts_expert_describe_rule
    \\  Rule search by keyword                 -> zigts_expert_search
    \\  Violation baseline before editing      -> zigts_expert_verify_paths
    \\  Canonical refactor preview             -> zigts_expert_canonicalize
    \\  Contract-pair compatibility proof      -> zigts_expert_prove_patch
    \\  Cross-handler system proof             -> zigts_expert_system_proof
    \\  Proof-guided repair plan               -> pi_repair_plan
    \\  Compiler-authored repair dry-run       -> pi_apply_repair_plan
    \\  Preview a new route without writing    -> pi_feature_plan
    \\  Add/prove a new route candidate         -> pi_forge_route
    \\  Apply an approved route candidate       -> pi_apply_feature_plan
    \\  Read author-declared specs + status    -> pi_specs_status
    \\  Inspect persisted witness corpus       -> pi_witnesses
    \\  Record a cross-session project fact    -> pi_remember_fact
    \\  Recall persisted project memory        -> pi_recall_facts
    \\  Virtual module implementation audit    -> zigts_expert_verify_modules
    \\  List files in workspace                -> workspace_list_files
    \\  Read a source file                     -> workspace_read_file
    \\  Text search across workspace           -> workspace_search_text
    \\  Generate tests after editing handler   -> workspace_gen_tests
    \\  Build steps                            -> zig_build_step
    \\  Run tests                              -> zig_test_step
    \\
    \\Before editing any file: call workspace_read_file on the target then
    \\zigts_expert_verify_paths to capture the pre-existing violation
    \\baseline. Never propose an edit without reading the current content.
    \\
    \\For language and module questions, always call the live tool even when
    \\you believe you know the answer. The tools reflect the running binary;
    \\training data lags the compiler.
    \\
    \\Behavioral contract awareness:
    \\Every successful edit produces a proof_card that includes a
    \\"properties" object alongside the violations summary. These are
    \\compiler-proven behavioral and data-flow guarantees, including pure,
    \\read_only, stateless, deterministic, retry_safe, idempotent,
    \\injection_safe, state_isolated, fault_covered, result_safe, secret /
    \\credential leakage containment, input validation, and max I/O depth.
    \\
    \\Use `zigts_check` when the user's goal involves a behavioral property
    \\(e.g. "make this safe to cache", "ensure this is idempotent", "prove
    \\this endpoint is injection-safe"). Call it before editing to capture
    \\the current on-disk proof state. Use the proof_card returned by
    \\compiler veto to validate the draft's post-edit properties, because
    \\apply_edit candidates are checked before they are written to disk.
    \\
    \\Proof-first route authoring:
    \\When the user asks to add a handler route, use the compiler-native
    \\Route Forge path before drafting manual code. Convert the request into
    \\a structured route mini-spec (`file`, `method`, `path`, optional
    \\`body_schema`, optional `status`) and call `pi_forge_route`. If the
    \\user asks only to preview or plan, call `pi_feature_plan` instead.
    \\Treat the returned diff as the candidate source of truth. Do not write
    \\the candidate yourself; approved candidates must flow through
    \\`pi_apply_feature_plan`, which reruns the compiler veto and records a
    \\proof-carrying `verified_patch`.
    \\
    \\Spec-driven repair:
    \\When the user adds, edits, or asks about a `Spec<...>` annotation on
    \\the handler return type (e.g.
    \\`function handler(req): Response & Spec<"idempotent">`), call
    \\`pi_specs_status` to read the active spec set straight from source
    \\rather than inventing goals. The result lists each declared spec with
    \\its discharge state: `not_discharged` (ZTS500) means feed the name to
    \\`pi_repair_plan`; `incompatible_with_import` (ZTS501) means the spec
    \\contradicts an imported module - resolve the import or drop the spec,
    \\never enter repair; `unknown_name` (ZTS502) means correct the typo.
    \\The author's `Spec<...>` is the obligation set; do not substitute
    \\different goals for it.
    \\
    \\Witness corpus awareness:
    \\Every flow-property witness materialised by `pi_repair_plan` and
    \\`pi_goal_check` is persisted to `.zigttp/witnesses/<short_hash>/` so
    \\that the same falsifying input does not need to be rediscovered every
    \\session. Before drafting a repair against a Spec failure, call
    \\`pi_witnesses` to see how thinly defended that Spec is. Specs with
    \\zero witnesses are unprobed: the proof currently relies on the
    \\classifier alone, and a regression there would silently slip past the
    \\corpus. Specs with pinned witnesses are load-bearing: a repair that
    \\removes them weakens coverage and must be justified, not silently
    \\applied. The corpus is the project's accumulated evidence, not
    \\throwaway state.
    \\
    \\Never reach for JavaScript idioms that are compile errors in zigts:
    \\try/catch, classes, var, null, ==/!=, ++/--. Use Result types, plain
    \\objects, let/const, undefined, ===/!==, and explicit increments.
    \\
    \\Goal-seeking synthesis with counterexamples:
    \\When the user states a property goal ("make this endpoint injection
    \\safe", "prove no secrets leak here", "remove credential leakage"),
    \\drive the edit with pi_repair_plan and pi_goal_check in a loop:
    \\  1. Call pi_repair_plan with the target file and goal list.
    \\  2. If "ok":true, the goals are already discharged; stop.
    \\  3. Otherwise each "plans" entry is a compiler-authored edit intent
    \\     tied to diagnostics or witnesses. Pass the smallest applicable
    \\     plan to pi_apply_repair_plan first; if it returns a verified
    \\     proposed_content, draft that exact edit. If it returns an
    \\     unsupported reason, fall back to manual editing without broadening
    \\     the change.
    \\  4. Each "witnesses" entry is a concrete (Request,
    \\     io_stubs) tuple that executes the violating path. Read the
    \\     summary and the sink line.
    \\  5. Edit the handler so the violating path disappears: either the
    \\     sink no longer receives the labelled value, or the branch that
    \\     carries it is closed earlier. Do not mute the rule or rename
    \\     the env var - the witness is an executable proof, so the fix
    \\     must close the concrete path.
    \\  6. Re-run pi_goal_check. Repeat until "ok":true.
    \\Every iteration is captured as a proof-carrying patch in the session
    \\ledger, so the final artifact carries a record of which goals were
    \\closed and which witnesses they closed.
    \\
    \\What follows is:
    \\  1. The zigts-expert skill document - your identity.
    \\  2. Three reference documents - virtual modules, testing, JSX.
    \\  3. A live snapshot of the current compiler's rule set, feature
    \\     matrix, and virtual module catalog.
    \\  4. Three canonical example handlers - pattern-match your drafts
    \\     against these, not against JavaScript idioms from your training.
    \\  5. Policy metadata - compiler version, policy version, policy hash.
    \\
    \\============================================================
    \\SKILL
    \\============================================================
    \\
;

const epilogue =
    \\
    \\============================================================
    \\END OF PERSONA
    \\============================================================
    \\
    \\Remember: the compiler veto is mechanical. You cannot bypass it by
    \\emitting text. Every model reply that proposes an edit goes through
    \\edit-simulate before the user sees anything. Your job is to make the
    \\first draft pass - the retry loop is insurance, not routine.
    \\
;

pub fn buildSystemPrompt(allocator: std.mem.Allocator) ![]u8 {
    return buildSystemPromptWithContext(allocator, null);
}

/// Build the system prompt with an optional project-context block appended.
/// `project_context`, if provided, is the concatenated AGENTS.md / CLAUDE.md
/// body from `context/project_context.zig`. It is inserted as a labelled,
/// read-only section - never merged into the persona itself, and never
/// capable of redefining tools or commands.
pub fn buildSystemPromptWithContext(
    allocator: std.mem.Allocator,
    project_context: ?[]const u8,
) ![]u8 {
    return buildSystemPromptWithContextAndMemory(allocator, project_context, null);
}

/// As `buildSystemPromptWithContext`, plus a PROJECT MEMORY section sourced
/// from `<project_root>/.zigttp/memory.jsonl`. The store is read once per
/// build; selection (pinned-first then most-recent unpinned, bounded by
/// `MEMORY_SECTION_SOFT_CAP`) lives in `memory_store.zig` so this function
/// stays a pure assembler. A null or missing project_root, or an empty
/// store, drops the section entirely; the same is true if the budget is
/// exhausted before any entry fits. PROJECT MEMORY is positioned directly
/// before PROJECT CONTEXT so the truncation order (CONTEXT cut first, then
/// any future WITNESSED FAILURES section, then MEMORY cut last) holds even
/// if Slice C lands in parallel.
pub fn buildSystemPromptWithContextAndMemory(
    allocator: std.mem.Allocator,
    project_context: ?[]const u8,
    project_root: ?[]const u8,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll(prologue);
    try w.writeAll(skill.skill_md);

    try writeSection(w, "REFERENCE: VIRTUAL MODULES", skill.virtual_modules_md);
    try writeSection(w, "REFERENCE: TESTING & REPLAY", skill.testing_replay_md);
    try writeSection(w, "REFERENCE: JSX PATTERNS", skill.jsx_patterns_md);

    try writeBanner(w, "LIVE SNAPSHOT: RULES (ZTS0xx-ZTS3xx)");
    try writeRuleSnapshot(w);

    try writeBanner(w, "LIVE SNAPSHOT: FEATURES MATRIX");
    try json_diagnostics.writeFeaturesText(w);

    try writeBanner(w, "LIVE SNAPSHOT: VIRTUAL MODULES");
    try json_diagnostics.writeModulesText(w);

    try writeBanner(w, "CANONICAL EXAMPLES");
    try writeExample(w, "basic handler (examples/handler/handler.ts)", skill.basic_handler);
    try writeExample(w, "routing with virtual modules (examples/routing/router.ts)", skill.routing_router);
    try writeExample(w, "cache + service + routing (examples/system/users.ts)", skill.system_users);

    try writeBanner(w, "POLICY METADATA");
    const info = expert_meta.compute();
    try expert_meta.writeText(w, &info);

    try writeBanner(w, "AVAILABLE SKILLS");
    try w.writeAll("The user may invoke skills via /skill:<name>. Each skill injects a focused\n");
    try w.writeAll("prompt that you will receive as a user message. Available skills:\n\n");
    inline for (skills_catalog.catalog) |s| {
        try w.print("  /skill:{s}\n    {s}\n\n", .{ s.name, s.description });
    }

    try writeBanner(w, "AVAILABLE TEMPLATES");
    try w.writeAll("The user may invoke templates via /template:<name> [args...]. Templates expand\n");
    try w.writeAll("positional args ({{1}}, {{2}}, {{args}}) before being sent as your user message.\n\n");
    inline for (prompts_catalog.catalog) |t| {
        try w.print("  /template:{s}\n    {s}\n\n", .{ t.name, t.description });
    }

    // PROJECT MEMORY (cross-session fact store) sits between the persona
    // snapshots and PROJECT CONTEXT. It is third in the truncation order
    // (after PROJECT CONTEXT and any future WITNESSED FAILURES section),
    // so we compute its budget against the remaining headroom but cap it
    // softly so a runaway corpus cannot crowd out PROJECT CONTEXT.
    //
    // Byte budget formula:
    //   remaining = PROMPT_CAP_BYTES - persona_so_far
    //                - CTX_TRUNCATION_RESERVED  (epilogue + context marker)
    //                - MEMORY_SECTION_OVERHEAD  (memory banner)
    //                - len(project_context or 0) (so CONTEXT keeps room)
    //   memory_budget = min(remaining, MEMORY_SECTION_SOFT_CAP)
    if (project_root) |root| {
        try writeMemorySection(allocator, w, aw.writer.end, root, project_context);
    }

    // Project context is appended after persona+snapshots but before the
    // epilogue, so the veto reminder still has the last word. The cap guard
    // truncates *only* this section, never persona content above it.
    const persona_len_before_ctx = aw.writer.end;
    if (project_context) |ctx| {
        if (ctx.len > 0) {
            try writeBanner(w, "PROJECT CONTEXT (read-only, from AGENTS.md / CLAUDE.md)");
            const room = if (PROMPT_CAP_BYTES > persona_len_before_ctx)
                PROMPT_CAP_BYTES - persona_len_before_ctx
            else
                0;
            if (room > CTX_TRUNCATION_RESERVED) {
                const allowed = room - CTX_TRUNCATION_RESERVED;
                if (ctx.len <= allowed) {
                    try w.writeAll(ctx);
                    if (ctx.len == 0 or ctx[ctx.len - 1] != '\n') try w.writeByte('\n');
                } else {
                    try w.writeAll(ctx[0..allowed]);
                    try w.print(
                        "\n\n[project-context truncated: {d} of {d} bytes kept to fit {d} KiB prompt cap]\n",
                        .{ allowed, ctx.len, PROMPT_CAP_BYTES / 1024 },
                    );
                }
            } else {
                try w.writeAll("[project-context omitted: persona already at prompt cap]\n");
            }
        }
    }

    try w.writeAll(epilogue);

    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

fn writeSection(writer: anytype, title: []const u8, body: []const u8) !void {
    try writeBanner(writer, title);
    try writer.writeAll(body);
}

/// Render the PROJECT MEMORY section (or skip it cleanly when the store
/// is empty or the budget is exhausted). The selector is reused as-is
/// from `memory_store`, so changes to the per-entry rendering shape stay
/// in one place.
fn writeMemorySection(
    allocator: std.mem.Allocator,
    writer: anytype,
    persona_len_so_far: usize,
    project_root: []const u8,
    project_context: ?[]const u8,
) !void {
    const entries = memory_store.loadAll(allocator, project_root) catch return;
    defer memory_store.freeEntries(allocator, entries);
    if (entries.len == 0) return;

    const ctx_len = if (project_context) |ctx| ctx.len else 0;
    const fixed_reserve = CTX_TRUNCATION_RESERVED + MEMORY_SECTION_OVERHEAD;
    const need_under = fixed_reserve + ctx_len;
    if (persona_len_so_far + need_under >= PROMPT_CAP_BYTES) return;
    const remaining = PROMPT_CAP_BYTES - persona_len_so_far - need_under;
    const budget = @min(remaining, MEMORY_SECTION_SOFT_CAP);
    if (budget == 0) return;

    const selection = memory_store.selectForPersona(allocator, entries, budget) catch return;
    defer allocator.free(selection);
    if (selection.len == 0) return;

    try writeBanner(writer, "PROJECT MEMORY (read-only, from .zigttp/memory.jsonl)");
    try writer.writeAll("Cross-session facts the agent has recorded for this project. Pinned\n");
    try writer.writeAll("facts come first; unpinned facts are listed most-recent first. Use\n");
    try writer.writeAll("pi_recall_facts to read the full corpus; pi_remember_fact to append.\n\n");
    for (selection) |e| {
        try writer.writeAll("  ");
        if (e.pinned) try writer.writeAll("[pin] ");
        try writer.writeAll(e.fact);
        try writer.writeByte('\n');
    }
}

fn writeBanner(writer: anytype, title: []const u8) !void {
    try writer.writeAll("\n\n============================================================\n");
    try writer.writeAll(title);
    try writer.writeAll("\n============================================================\n\n");
}

fn writeExample(writer: anytype, title: []const u8, body: []const u8) !void {
    try writer.writeAll("--- ");
    try writer.writeAll(title);
    try writer.writeAll(" ---\n");
    try writer.writeAll(body);
    if (body.len == 0 or body[body.len - 1] != '\n') try writer.writeAll("\n");
    try writer.writeAll("\n");
}

fn writeRuleSnapshot(writer: anytype) !void {
    for (&rule_registry.all_rules) |*rule| {
        try writer.print("{s} {s} ({s})\n", .{ rule.code, rule.name, rule.category.label() });
        try writer.print("  {s}\n", .{rule.description});
        if (rule.example) |ex| {
            try writer.print("  example: {s}\n", .{ex});
        }
        try writer.print("  {s}\n\n", .{rule.help});
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "buildSystemPrompt returns an owned non-empty slice" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(prompt.len > 1024);
}

test "persona contains the prologue identity statement" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "native zigts coding agent") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "compiler veto") != null);
}

test "persona embeds the four skill and reference documents" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "REFERENCE: VIRTUAL MODULES") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "REFERENCE: TESTING & REPLAY") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "REFERENCE: JSX PATTERNS") != null);
    // Proof the SKILL.md content ran through @embedFile and reached here -
    // check for both the opening banner and a mid-file needle that we know
    // lives inside the skill markdown.
    try testing.expect(std.mem.indexOf(u8, prompt, "\nSKILL\n") != null);
}

test "persona contains live rule-registry entries" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "LIVE SNAPSHOT: RULES") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "ZTS001") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "ZTS303") != null);
}

test "persona contains the features matrix section" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "LIVE SNAPSHOT: FEATURES MATRIX") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "Allowed:") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "Blocked:") != null);
}

test "persona routes draft property validation through the veto proof_card" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "returns on-disk proof.properties") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "Use the proof_card returned by") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "after to confirm the property was") == null);
}

test "persona contains virtual module specifiers from the live catalog" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "LIVE SNAPSHOT: VIRTUAL MODULES") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "zigttp:env") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "zigttp:crypto") != null);
}

test "persona pins compiler and policy versions via expert_meta.writeText" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "POLICY METADATA") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "0.1.0-beta") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "2026.04.2") != null);
}

test "persona closes with the veto reminder" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "END OF PERSONA") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "mechanical") != null);
    try testing.expectEqual(@as(u8, '\n'), prompt[prompt.len - 1]);
}

test "persona fits under the 128 KiB size ceiling" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(prompt.len <= 128 * 1024);
}

test "persona includes canonical profile guidance" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "One-Way ZigTS") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "default strict profile") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "ZTS608") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "Effects<...>") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "Proof<...>") != null);
    try testing.expect(prompt.len <= PROMPT_CAP_BYTES);
}

test "persona embeds the three canonical example handlers" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "CANONICAL EXAMPLES") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "examples/handler/handler.ts") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "examples/routing/router.ts") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "examples/system/users.ts") != null);
    // Prove the file contents flowed through @embedFile, not just the labels.
    // Every canonical handler must contain `function handler` at minimum.
    const handler_needle = "function handler";
    var count: usize = 0;
    var search_from: usize = 0;
    while (std.mem.indexOfPos(u8, prompt, search_from, handler_needle)) |pos| {
        count += 1;
        search_from = pos + handler_needle.len;
    }
    try testing.expect(count >= 3);
}

test "persona includes tool dispatch guidance" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "Tool dispatch") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "pre-existing violation") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "training data lags") != null);
}

test "persona lists explicit contract and system proof tools" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "zigts_expert_prove_patch") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "zigts_expert_system_proof") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "Contract-pair compatibility proof") != null);
}

test "persona routes new route requests through Route Forge" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "pi_feature_plan") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "pi_forge_route") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "pi_apply_feature_plan") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "Proof-first route authoring") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "verified_patch") != null);
}

test "persona routes spec questions through pi_specs_status" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "pi_specs_status") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "Spec-driven repair") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "ZTS500") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "ZTS501") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "ZTS502") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "Spec<\"idempotent\">") != null);
}

test "persona does not include project context markers" {
    const prompt = try buildSystemPrompt(testing.allocator);
    defer testing.allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "PROJECT CONTEXT") == null);
}

test "persona appends project context when supplied" {
    const ctx = "## /tmp/AGENTS.md\n\nproject rule sentinel\n";
    const prompt = try buildSystemPromptWithContext(testing.allocator, ctx);
    defer testing.allocator.free(prompt);

    const banner_pos = std.mem.indexOf(u8, prompt, "PROJECT CONTEXT") orelse return error.TestExpected;
    const rule_pos = std.mem.indexOf(u8, prompt, "project rule sentinel") orelse return error.TestExpected;
    const epilogue_pos = std.mem.indexOf(u8, prompt, "END OF PERSONA") orelse return error.TestExpected;

    // Project context lives between the banner and the epilogue, never above
    // the persona prologue.
    try testing.expect(banner_pos < rule_pos);
    try testing.expect(rule_pos < epilogue_pos);
    try testing.expect(prompt.len <= PROMPT_CAP_BYTES);
}

test "persona truncates project context to fit the 128 KiB cap" {
    // Build an intentionally oversized project-context string. The persona
    // prologue + rule snapshot already consume most of the 128 KiB budget;
    // anything we append here must be clipped.
    const pad_len: usize = 256 * 1024;
    const big = try testing.allocator.alloc(u8, pad_len);
    defer testing.allocator.free(big);
    @memset(big, 'A');

    const prompt = try buildSystemPromptWithContext(testing.allocator, big);
    defer testing.allocator.free(prompt);

    try testing.expect(prompt.len <= PROMPT_CAP_BYTES);
    try testing.expect(std.mem.indexOf(u8, prompt, "project-context truncated") != null);
    try testing.expect(std.mem.indexOf(u8, prompt, "END OF PERSONA") != null);
}

test "empty project context is ignored" {
    const prompt_with = try buildSystemPromptWithContext(testing.allocator, "");
    defer testing.allocator.free(prompt_with);
    try testing.expect(std.mem.indexOf(u8, prompt_with, "PROJECT CONTEXT") == null);
}

test "PROJECT MEMORY section appears when the store has entries" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    const root = buf[0..len];

    try memory_store.append(allocator, root, .{
        .id = "mem-1",
        .fact = "always use Result<T> for fallible helpers",
        .source = "manual_entry",
        .timestamp_unix_ms = 1,
        .pinned = true,
    });
    try memory_store.append(allocator, root, .{
        .id = "mem-2",
        .fact = "session id generator lives in session/session_id.zig",
        .source = "tool",
        .timestamp_unix_ms = 2,
        .pinned = false,
    });

    const prompt = try buildSystemPromptWithContextAndMemory(allocator, null, root);
    defer allocator.free(prompt);

    const banner = std.mem.indexOf(u8, prompt, "PROJECT MEMORY") orelse return error.TestExpected;
    const pinned_marker = std.mem.indexOf(u8, prompt, "[pin] always use Result<T>") orelse
        return error.TestExpected;
    const unpinned = std.mem.indexOf(u8, prompt, "session id generator lives") orelse
        return error.TestExpected;
    const epilogue_pos = std.mem.indexOf(u8, prompt, "END OF PERSONA") orelse return error.TestExpected;

    try testing.expect(banner < pinned_marker);
    try testing.expect(pinned_marker < unpinned);
    try testing.expect(unpinned < epilogue_pos);
    try testing.expect(prompt.len <= PROMPT_CAP_BYTES);
}

test "PROJECT MEMORY section is omitted when the store is missing" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    const root = buf[0..len];

    const prompt = try buildSystemPromptWithContextAndMemory(allocator, null, root);
    defer allocator.free(prompt);
    try testing.expect(std.mem.indexOf(u8, prompt, "PROJECT MEMORY") == null);
}

test "PROJECT MEMORY sits before PROJECT CONTEXT in the prompt" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    const root = buf[0..len];

    try memory_store.append(allocator, root, .{
        .id = "m",
        .fact = "memory ordering sentinel",
        .source = "manual_entry",
        .timestamp_unix_ms = 1,
        .pinned = true,
    });

    const ctx = "## CLAUDE.md\n\nproject context sentinel\n";
    const prompt = try buildSystemPromptWithContextAndMemory(allocator, ctx, root);
    defer allocator.free(prompt);

    const mem_pos = std.mem.indexOf(u8, prompt, "PROJECT MEMORY") orelse return error.TestExpected;
    const ctx_pos = std.mem.indexOf(u8, prompt, "PROJECT CONTEXT") orelse return error.TestExpected;
    try testing.expect(mem_pos < ctx_pos);
    try testing.expect(prompt.len <= PROMPT_CAP_BYTES);
}
