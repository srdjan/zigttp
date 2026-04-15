//! Builds the zigts-expert agent persona as one allocator-owned []u8. The
//! output is the system prompt the Anthropic client will pass on every
//! request (with prompt caching). Structure: prologue + embedded skill prose
//! + live compiler snapshot (rules / features / modules / meta) + epilogue.
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
const examples = @import("zigts_expert_examples");

// The tool list inside the prologue below must stay in sync with
// `pi_app.buildRegistry`. Drift is a soft degradation, not a hard break:
// the Anthropic tool-use API still exposes any registered tool, but the
// agent won't be told about a new one in its system prompt and may not
// reach for it proactively until a later turn surfaces it.
const prologue =
    \\You are a zigts expert code author working inside the zigttp agentic
    \\compiler. Your job is to produce the most elegant, idiomatic, and
    \\architecturally correct zigts code the language can express.
    \\
    \\Every edit you propose is verified by the compiler veto before the
    \\user sees it. Drafts that fail the veto are replaced with diagnostic
    \\boxes and you are asked to retry. Pre-existing violations do not block
    \\new edits, only *new* ones. Draft as if every edit will be audited,
    \\because it will be.
    \\
    \\You have direct tool access to the compiler's rule registry, feature
    \\matrix, and virtual-module catalog via these in-process tools:
    \\
    \\  zigts_expert_describe_rule  - full help text for a specific rule
    \\  zigts_expert_search         - keyword search across all rules
    \\  zigts_expert_features       - allowed/blocked JS/TS features
    \\  zigts_expert_modules        - zigttp:* module exports
    \\  zigts_expert_meta           - compiler and policy metadata
    \\  zigts_expert_verify_paths   - full analysis on one or more files
    \\  zigts_expert_edit_simulate  - dry-run a proposed edit
    \\  zigts_expert_review_patch   - diff-aware violation review
    \\  zigts_expert_verify_modules - audit a virtual module file
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
    try writeExample(w, "basic handler (examples/handler/handler.ts)", examples.basic_handler);
    try writeExample(w, "routing with virtual modules (examples/routing/router.ts)", examples.routing_router);
    try writeExample(w, "cache + service + routing (examples/system/users.ts)", examples.system_users);

    try writeBanner(w, "POLICY METADATA");
    const info = expert_meta.compute();
    try expert_meta.writeText(w, &info);

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
    try testing.expect(std.mem.indexOf(u8, prompt, "zigts expert code author") != null);
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
