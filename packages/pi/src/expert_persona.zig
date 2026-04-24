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
    \\  zigts_expert_edit_simulate  - dry-run a proposed edit
    \\  zigts_expert_review_patch   - diff-aware violation review
    \\  zigts_expert_prove_patch    - classify a before/after contract pair
    \\  zigts_expert_system_proof   - run cross-handler system linking proof
    \\  zigts_expert_verify_modules - audit a virtual module file
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
    \\  Contract-pair compatibility proof      -> zigts_expert_prove_patch
    \\  Cross-handler system proof             -> zigts_expert_system_proof
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
    \\Never reach for JavaScript idioms that are compile errors in zigts:
    \\try/catch, classes, var, null, ==/!=, ++/--. Use Result types, plain
    \\objects, let/const, undefined, ===/!==, and explicit increments.
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
    try testing.expect(std.mem.indexOf(u8, prompt, "0.16.0") != null);
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
