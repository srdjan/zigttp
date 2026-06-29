//! expert_codegen_eval - measure the pi expert's code-generation QUALITY.
//!
//! The sibling expert_eval.zig pins the routing surface (does an ask reach the
//! right compiler-native tool). This module measures the next thing: given the
//! model's drafts, how good is the generated zigts code? It drives a real turn
//! through the real compiler veto in an isolated workspace and reports the
//! headline number - first-draft veto-pass rate - plus round-trips-to-green and
//! a per-ZTS-code gap histogram that ranks which teaching gap to close next.
//!
//! Where the drafts come from is the client's job: scripted replies in the unit
//! tests below (deterministic, free), or cassette replay of recorded model
//! output for the committed baseline (recorded once, replayed forever). Either
//! way the veto, the apply path, and the metrics are the production loop, so the
//! score is a measured count over a fixed sample, not an estimate.
//!
//! First-slice scope: length-1 (edit-only) cases, criteria passes_veto and
//! reaches_green. Multi-roundtrip cases (explore->edit, retry-to-green) and a
//! discharges_property criterion are deferred until the baseline histogram says
//! they are worth the cassette cost.

const std = @import("std");
const loop = @import("loop.zig");
const turn = @import("turn.zig");
const transcript_mod = @import("transcript.zig");
const registry_mod = @import("registry/registry.zig");
const expert_workflow = @import("expert_workflow.zig");
const IsolatedTmp = @import("test_support/tmp.zig").IsolatedTmp;

pub const SeedFile = struct {
    path: []const u8,
    bytes: []const u8,
};

pub const Criterion = enum {
    /// The first model draft must pass the compiler veto with no retries.
    passes_veto,
    /// The handler must reach a verified applied edit within max_attempts.
    reaches_green,
};

pub const CodegenCase = struct {
    name: []const u8,
    prompt: []const u8,
    /// Bytes written into the tmp workspace before the turn. Lets a case model
    /// "edit an existing handler" or seed a zigttp.json so the SQL veto path
    /// resolves.
    seed_files: []const SeedFile = &.{},
    /// The task kind this prompt should route to. Ties the codegen corpus to the
    /// routing eval so the two cannot drift on the same prompts.
    expected_kind: expert_workflow.TaskKind,
    criterion: Criterion,
    max_attempts: u8 = 1,
};

pub const CaseResult = struct {
    name: []const u8,
    /// classify(prompt) matched expected_kind.
    routed: bool,
    first_draft_pass: bool,
    applied: bool,
    passed_criterion: bool,
    roundtrips: u8,
    tool_calls: u32,
    proven_guarantees: u32,
    /// Leading ZTS code of the first diagnostic when the case missed its
    /// criterion; empty otherwise. A fixed buffer keeps CaseResult allocation
    /// free (ZTS codes are short, e.g. "ZTS303").
    failing_code_buf: [8]u8 = undefined,
    failing_code_len: u8 = 0,

    pub fn failingCode(self: *const CaseResult) ?[]const u8 {
        return if (self.failing_code_len == 0) null else self.failing_code_buf[0..self.failing_code_len];
    }
};

/// Run one case against a model client and the real compiler veto in an
/// isolated tmp workspace. The client supplies the model's drafts; everything
/// else - the veto, the apply path, the metrics - is the production loop.
pub fn runCase(
    allocator: std.mem.Allocator,
    case: CodegenCase,
    client: loop.ModelClient,
    registry: *const registry_mod.Registry,
) !CaseResult {
    var tmp = try IsolatedTmp.init(allocator, "codegen-eval");
    defer tmp.cleanup(allocator);
    for (case.seed_files) |sf| try tmp.writeFile(allocator, sf.path, sf.bytes);

    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(allocator);

    const result = try loop.runTurnWith(allocator, client, registry, &tr, case.prompt, .{
        .workspace_root = tmp.abs_path,
        .max_attempts = case.max_attempts,
        .approval_fn = loop.ApprovalFn.fromFn(loop.autoApprove),
        .replay_mode = false,
        .turn_timeout_ms = 0,
    });

    const passed = switch (case.criterion) {
        .passes_veto => result.first_draft_veto_pass,
        .reaches_green => result.applied_edit,
    };

    var cr: CaseResult = .{
        .name = case.name,
        .routed = expert_workflow.classify(case.prompt).kind == case.expected_kind,
        .first_draft_pass = result.first_draft_veto_pass,
        .applied = result.applied_edit,
        .passed_criterion = passed,
        .roundtrips = result.roundtrips,
        .tool_calls = result.tool_call_count,
        .proven_guarantees = result.proven_guarantees,
    };
    if (!passed) {
        if (firstZtsCode(&tr)) |code| {
            const n = @min(code.len, cr.failing_code_buf.len);
            @memcpy(cr.failing_code_buf[0..n], code[0..n]);
            cr.failing_code_len = @intCast(n);
        }
    }
    return cr;
}

pub const CodegenSummary = struct {
    total: usize,
    routed: usize,
    first_draft_passes: usize,
    greens: usize,
    criterion_passes: usize,

    /// First-draft veto-pass rate in percent (0-100), 0 when empty. Integer to
    /// keep the eval free of float-formatting noise; the count fields carry the
    /// exact numerator/denominator for callers that want a precise ratio.
    pub fn firstDraftPassPercent(self: CodegenSummary) usize {
        if (self.total == 0) return 0;
        return self.first_draft_passes * 100 / self.total;
    }
};

pub fn summarize(results: []const CaseResult) CodegenSummary {
    var s: CodegenSummary = .{
        .total = results.len,
        .routed = 0,
        .first_draft_passes = 0,
        .greens = 0,
        .criterion_passes = 0,
    };
    for (results) |r| {
        if (r.routed) s.routed += 1;
        if (r.first_draft_pass) s.first_draft_passes += 1;
        if (r.applied) s.greens += 1;
        if (r.passed_criterion) s.criterion_passes += 1;
    }
    return s;
}

/// How many failing cases reported a given leading ZTS code. This is the gap
/// histogram primitive: ask it per code that appears and the answer ranks which
/// teaching gap accounts for the most retries.
pub fn countFailingCode(results: []const CaseResult, code: []const u8) usize {
    var n: usize = 0;
    for (results) |r| {
        if (r.failingCode()) |c| {
            if (std.mem.eql(u8, c, code)) n += 1;
        }
    }
    return n;
}

fn firstZtsCode(tr: *const transcript_mod.Transcript) ?[]const u8 {
    for (tr.entries.items) |entry| {
        const text: []const u8 = switch (entry) {
            .diagnostic_box => |b| b.llm_text,
            else => continue,
        };
        if (findZts(text)) |code| return code;
    }
    return null;
}

fn findZts(text: []const u8) ?[]const u8 {
    var i: usize = 0;
    while (std.mem.indexOfPos(u8, text, i, "ZTS")) |pos| {
        var end = pos + 3;
        while (end < text.len and std.ascii.isDigit(text[end])) end += 1;
        if (end > pos + 3) return text[pos..end];
        i = pos + 3;
    }
    return null;
}

const testing = std.testing;

// A model client that returns one fixed reply, ignoring the transcript. Stands
// in for cassette replay in the deterministic self-test: it exercises runCase,
// the real veto, and the metrics with zero network and zero model cost.
const ScriptedClient = struct {
    reply: turn.AssistantReply,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        tr: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!loop.ModelCallResult {
        const self: *ScriptedClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = tr;
        _ = extra_user_text;
        return .{ .reply = self.reply };
    }

    pub fn asClient(self: *ScriptedClient) loop.ModelClient {
        return .{ .context = self, .request_fn = requestFn };
    }
};

const clean_health =
    "function handler(req: Request): Response & Spec<\"deterministic\"> { return Response.json({ ok: true }); }";

const unchecked_result =
    "import { validateJson } from \"zigttp:validate\";\n" ++
    "function handler(req: Request): Response & Spec<\"deterministic\"> {\n" ++
    "  const result = validateJson(\"item\", req.body);\n" ++
    "  const data = result.value;\n" ++
    "  return Response.json({ data });\n" ++
    "}\n";

test "runCase scores a clean first draft as a veto pass" {
    var client: ScriptedClient = .{ .reply = .{ .response = .{ .edit = .{
        .file = "handler.ts",
        .content = clean_health,
        .before = null,
    } } } };
    const case: CodegenCase = .{
        .name = "health-scaffold",
        .prompt = "scaffold a minimal GET /health handler",
        .expected_kind = .route_add,
        .criterion = .passes_veto,
    };
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);
    const r = try runCase(testing.allocator, case, client.asClient(), &registry);
    try testing.expect(r.first_draft_pass);
    try testing.expect(r.applied);
    try testing.expect(r.passed_criterion);
    try testing.expect(r.failingCode() == null);
}

test "runCase records the failing ZTS code for a bad first draft" {
    var client: ScriptedClient = .{ .reply = .{ .response = .{ .edit = .{
        .file = "handler.ts",
        .content = unchecked_result,
        .before = null,
    } } } };
    const case: CodegenCase = .{
        .name = "unchecked-result",
        .prompt = "fix the ZTS303 violation",
        .expected_kind = .violation_fix,
        .criterion = .passes_veto,
    };
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);
    const r = try runCase(testing.allocator, case, client.asClient(), &registry);
    try testing.expect(!r.first_draft_pass);
    try testing.expect(!r.passed_criterion);
    try testing.expectEqualStrings("ZTS303", r.failingCode().?);
}

test "summarize aggregates pass rate and failing-code histogram" {
    var bad: CaseResult = .{
        .name = "b",
        .routed = true,
        .first_draft_pass = false,
        .applied = false,
        .passed_criterion = false,
        .roundtrips = 1,
        .tool_calls = 0,
        .proven_guarantees = 0,
    };
    std.mem.copyForwards(u8, bad.failing_code_buf[0..6], "ZTS303");
    bad.failing_code_len = 6;

    const results = [_]CaseResult{
        .{
            .name = "a",
            .routed = true,
            .first_draft_pass = true,
            .applied = true,
            .passed_criterion = true,
            .roundtrips = 1,
            .tool_calls = 0,
            .proven_guarantees = 1,
        },
        bad,
    };
    const s = summarize(&results);
    try testing.expectEqual(@as(usize, 2), s.total);
    try testing.expectEqual(@as(usize, 1), s.first_draft_passes);
    try testing.expectEqual(@as(usize, 1), s.greens);
    try testing.expectEqual(@as(usize, 50), s.firstDraftPassPercent());
    try testing.expectEqual(@as(usize, 1), countFailingCode(&results, "ZTS303"));
    try testing.expectEqual(@as(usize, 0), countFailingCode(&results, "ZTS999"));
}
