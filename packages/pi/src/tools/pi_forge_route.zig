//! pi_forge_route - closed-loop compiler-native route authoring.
//!
//! The forge composes route synthesis and compiler proof into a single
//! inspectable run graph. It does not write files; approved candidates flow
//! through `pi_apply_feature_plan`, which reruns the compiler veto.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const ui_payload = @import("../ui_payload.zig");
const proof_enrichment = @import("../proof_enrichment.zig");
const common = @import("common.zig");
const feature_plan = @import("pi_feature_plan.zig");
const repair_apply = @import("repair_apply.zig");
const repair_plan_tool = @import("pi_repair_plan.zig");

const json_utils = zigts.json_utils;
const ir = zigts.parser;
const repair_plan = zigts.repair_plan;
const handler_verifier = zigts.handler_verifier;

const name = "pi_forge_route";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "forge-route",
    .description =
    \\Run a compiler-native route forge loop. Input matches /feature route:
    \\file, method, path, optional body_schema, response_schema, and status.
    \\The tool generates a candidate, proves it in memory, records each step,
    \\and returns an approved-for-apply candidate when no new compiler
    \\violations are introduced.
    ,
    .input_schema =
    \\{"type":"object","properties":{"kind":{"type":"string","enum":["route"]},"file":{"type":"string"},"method":{"type":"string"},"path":{"type":"string"},"body_schema":{"type":"string"},"response_schema":{"type":"string"},"status":{"type":"integer","minimum":100,"maximum":599}},"required":["kind","file","method","path"]}
    ,
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    const trimmed = std.mem.trim(u8, args_json, " \t\r\n");
    if (trimmed.len == 0) return error.InvalidToolArgsJson;
    const out = try allocator.alloc([]const u8, 1);
    out[0] = try allocator.dupe(u8, trimmed);
    return out;
}

pub fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    var spec_arena = std.heap.ArenaAllocator.init(allocator);
    defer spec_arena.deinit();
    const spec = feature_plan.parseRouteSpecArgs(spec_arena.allocator(), args) catch |err| switch (err) {
        error.InvalidToolArgsJson => return registry_mod.ToolResult.err(allocator, usage()),
        else => return err,
    };
    if (!feature_plan.validRouteSpec(spec)) {
        return registry_mod.ToolResult.err(allocator, name ++ ": method must be an HTTP verb and path must start with /\n");
    }

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, spec.file);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    const source = zigts.file_io.readFile(allocator, absolute, common.default_output_limit) catch |e| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to read {s}: {s}\n",
            .{ absolute, @errorName(e) },
        );
    };
    defer allocator.free(source);

    const handler_name = try feature_plan.routeHandlerName(allocator, spec.method, spec.path);
    defer allocator.free(handler_name);

    const proposed = try feature_plan.synthesizeRoute(allocator, source, spec, handler_name);
    defer allocator.free(proposed);

    var analysis = try proof_enrichment.analyzePatch(
        allocator,
        root,
        relative,
        source,
        proposed,
        null,
    );
    defer analysis.deinit(allocator);

    var steps: std.ArrayList(ui_payload.ForgeRunStep) = .empty;
    defer {
        for (steps.items) |*step| step.deinit(allocator);
        steps.deinit(allocator);
    }
    try appendGenerateStep(allocator, &steps, spec, handler_name);
    try appendProofStep(allocator, &steps, "prove_candidate", "prove candidate", analysis.stats, analysis.stats.new == 0);

    if (analysis.stats.new != 0) {
        if (try deriveAndApplyFirstVerifierRepair(allocator, proposed)) |attempt_value| {
            var attempt = attempt_value;
            defer attempt.deinit(allocator);
            try appendRepairStep(allocator, &steps, attempt, "passed");

            var repaired_analysis = try proof_enrichment.analyzePatch(
                allocator,
                root,
                relative,
                source,
                attempt.proposed,
                null,
            );
            defer repaired_analysis.deinit(allocator);
            const repaired_success = repaired_analysis.stats.new == 0;
            try appendProofStep(
                allocator,
                &steps,
                "prove_repaired_candidate",
                "prove repaired candidate",
                repaired_analysis.stats,
                repaired_success,
            );
            return try buildResult(
                allocator,
                runIdPrefix(),
                spec,
                relative,
                handler_name,
                steps.items,
                attempt.proposed,
                repaired_analysis,
                if (repaired_success)
                    "repair pass closed all new compiler violations; ready for approval"
                else
                    "repair pass completed but compiler proof still reports new violations",
            );
        }
        try appendStep(
            allocator,
            &steps,
            "derive_repair",
            "derive verifier repair",
            "failed",
            "No supported verifier repair intent was derivable for the candidate.",
        );
    } else {
        try appendStep(
            allocator,
            &steps,
            "repair_candidate",
            "repair candidate",
            "skipped",
            "Proof passed; no repair pass needed.",
        );
    }

    return try buildResult(
        allocator,
        runIdPrefix(),
        spec,
        relative,
        handler_name,
        steps.items,
        proposed,
        analysis,
        if (analysis.stats.new == 0)
            "candidate has zero new compiler violations; ready for approval"
        else
            "candidate still introduces compiler violations; no safe repair was applied",
    );
}

fn buildResult(
    allocator: std.mem.Allocator,
    run_id_prefix: []const u8,
    spec: feature_plan.RouteSpec,
    relative: []const u8,
    handler_name: []const u8,
    steps: []const ui_payload.ForgeRunStep,
    final_content: []const u8,
    analysis: proof_enrichment.PatchAnalysis,
    terminal_reason: []const u8,
) !registry_mod.ToolResult {
    const success = analysis.stats.new == 0;
    const verification_summary = try std.fmt.allocPrint(
        allocator,
        "{d} total, {d} new, {d} preexisting",
        .{ analysis.stats.total, analysis.stats.new, analysis.stats.preexisting orelse 0 },
    );
    defer allocator.free(verification_summary);

    const run_id = try std.fmt.allocPrint(allocator, "{s}:{s}:{s}", .{ run_id_prefix, spec.method, spec.path });
    defer allocator.free(run_id);

    var payload: ui_payload.UiPayload = .{ .forge_run = try ui_payload.ForgeRunPayload.init(
        allocator,
        run_id,
        relative,
        "route",
        spec.method,
        spec.path,
        handler_name,
        steps,
        final_content,
        analysis.unified_diff,
        success,
        terminal_reason,
        verification_summary,
        analysis.stats,
    ) };
    errdefer payload.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("{\"ok\":");
    try w.writeAll(if (success) "true" else "false");
    try w.writeAll(",\"run_id\":");
    try json_utils.writeJsonString(w, run_id);
    try w.writeAll(",\"file\":");
    try json_utils.writeJsonString(w, relative);
    try w.writeAll(",\"handler_name\":");
    try json_utils.writeJsonString(w, handler_name);
    try w.writeAll(",\"terminal_reason\":");
    try json_utils.writeJsonString(w, terminal_reason);
    try w.writeAll(",\"verification_summary\":");
    try json_utils.writeJsonString(w, verification_summary);
    try w.writeAll("}\n");
    buf = aw.toArrayList();

    return .{
        .ok = success,
        .llm_text = try buf.toOwnedSlice(allocator),
        .ui_payload = payload,
    };
}

fn runIdPrefix() []const u8 {
    return "forge:route";
}

fn usage() []const u8 {
    return name ++ ": usage: /forge route file=<handler.ts> method=<GET|POST|...> path=</path> [body=<schema>] [response=<schema>] [status=<code>]\n";
}

fn appendGenerateStep(
    allocator: std.mem.Allocator,
    steps: *std.ArrayList(ui_payload.ForgeRunStep),
    spec: feature_plan.RouteSpec,
    handler_name: []const u8,
) !void {
    const generate_detail = try std.fmt.allocPrint(
        allocator,
        "Synthesized {s} for {s} {s}.",
        .{ handler_name, spec.method, spec.path },
    );
    defer allocator.free(generate_detail);
    try appendStep(
        allocator,
        steps,
        "generate_candidate",
        "generate route candidate",
        "passed",
        generate_detail,
    );
}

fn appendProofStep(
    allocator: std.mem.Allocator,
    steps: *std.ArrayList(ui_payload.ForgeRunStep),
    id: []const u8,
    title: []const u8,
    stats: ui_payload.ProofStats,
    success: bool,
) !void {
    const prove_detail = try std.fmt.allocPrint(
        allocator,
        "Compiler proof found {d} total, {d} new, {d} preexisting violation(s).",
        .{ stats.total, stats.new, stats.preexisting orelse 0 },
    );
    defer allocator.free(prove_detail);
    try appendStep(
        allocator,
        steps,
        id,
        title,
        if (success) "passed" else "failed",
        prove_detail,
    );
}

fn appendRepairStep(
    allocator: std.mem.Allocator,
    steps: *std.ArrayList(ui_payload.ForgeRunStep),
    attempt: RepairAttempt,
    state: []const u8,
) !void {
    const detail = try std.fmt.allocPrint(
        allocator,
        "{s}: {s}",
        .{ attempt.plan_id, attempt.summary },
    );
    defer allocator.free(detail);
    try appendStep(
        allocator,
        steps,
        "repair_candidate",
        "repair candidate",
        state,
        detail,
    );
}

fn appendStep(
    allocator: std.mem.Allocator,
    steps: *std.ArrayList(ui_payload.ForgeRunStep),
    id: []const u8,
    title: []const u8,
    state: []const u8,
    detail: []const u8,
) !void {
    try steps.append(allocator, try ui_payload.ForgeRunStep.init(
        allocator,
        id,
        title,
        state,
        detail,
    ));
}

const RepairAttempt = struct {
    plan_id: []u8,
    intent_kind: []u8,
    summary: []u8,
    proposed: []u8,

    fn deinit(self: *RepairAttempt, allocator: std.mem.Allocator) void {
        allocator.free(self.plan_id);
        allocator.free(self.intent_kind);
        allocator.free(self.summary);
        allocator.free(self.proposed);
        self.* = .{ .plan_id = &.{}, .intent_kind = &.{}, .summary = &.{}, .proposed = &.{} };
    }
};

fn deriveAndApplyFirstVerifierRepair(
    allocator: std.mem.Allocator,
    source: []const u8,
) !?RepairAttempt {
    var strip_result = zigts.strip(allocator, source, .{ .comptime_env = .{} }) catch return null;
    defer strip_result.deinit();

    var atoms = zigts.context.AtomTable.init(allocator);
    defer atoms.deinit();
    var js_parser = zigts.parser.JsParser.init(allocator, strip_result.code);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);

    const program_root = js_parser.parse() catch return null;
    const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
    const handler_fn = handler_verifier.findHandlerFunction(ir_view, program_root) orelse return null;

    var verifier = zigts.HandlerVerifier.init(allocator, ir_view, &atoms, null, null);
    defer verifier.deinit();
    _ = try verifier.verify(handler_fn);

    var plan_index: usize = 0;
    for (verifier.getDiagnostics()) |diag| {
        const loc = ir_view.getLoc(diag.node) orelse continue;
        const plan = repair_plan.fromVerifierDiagnostic(diag, .{ .line = loc.line, .column = loc.column }) orelse continue;
        plan_index += 1;
        const subject_name = repair_plan_tool.repairSubjectName(ir_view, &atoms, diag);
        const template = try repair_plan_tool.concreteTemplate(allocator, plan, subject_name);
        defer allocator.free(template);
        const plan_id = try std.fmt.allocPrint(allocator, "forge_rp_{d:0>3}", .{plan_index});
        errdefer allocator.free(plan_id);
        const proposed = repair_apply.applyIntent(allocator, source, .{
            .plan_id = plan_id,
            .intent_kind = plan.edit_intent.kind.asString(),
            .line = plan.edit_intent.line,
            .template = template,
        }) catch |err| switch (err) {
            error.UnsupportedRepairIntent, error.InvalidRepairLine => {
                allocator.free(plan_id);
                continue;
            },
            else => return err,
        };
        errdefer allocator.free(proposed);
        const intent_kind = try allocator.dupe(u8, plan.edit_intent.kind.asString());
        errdefer allocator.free(intent_kind);
        const summary = try allocator.dupe(u8, plan.summary);
        errdefer allocator.free(summary);
        return .{
            .plan_id = plan_id,
            .intent_kind = intent_kind,
            .summary = summary,
            .proposed = proposed,
        };
    }
    return null;
}

const testing = std.testing;

test "forge steps mark successful proof as ready for approval" {
    var steps: std.ArrayList(ui_payload.ForgeRunStep) = .empty;
    defer {
        for (steps.items) |*step| step.deinit(testing.allocator);
        steps.deinit(testing.allocator);
    }
    try appendGenerateStep(testing.allocator, &steps, .{
        .file = "handler.ts",
        .method = "GET",
        .path = "/health",
        .status = 200,
    }, "handleGetHealth");
    try appendProofStep(testing.allocator, &steps, "prove_candidate", "prove candidate", .{ .total = 0, .new = 0, .preexisting = 0 }, true);
    try appendStep(testing.allocator, &steps, "repair_candidate", "repair candidate", "skipped", "Proof passed; no repair pass needed.");

    try testing.expectEqual(@as(usize, 3), steps.items.len);
    try testing.expectEqualStrings("passed", steps.items[1].state);
    try testing.expectEqualStrings("skipped", steps.items[2].state);
}

test "forge derives and applies missing return repair in memory" {
    const source =
        \\function handler(req) {
        \\    if (req.method === "GET") return Response.json({ ok: true });
        \\}
        \\
    ;
    if (try deriveAndApplyFirstVerifierRepair(testing.allocator, source)) |attempt_value| {
        var attempt = attempt_value;
        defer attempt.deinit(testing.allocator);
        try testing.expectEqualStrings("add_trailing_return", attempt.intent_kind);
        try testing.expect(std.mem.indexOf(u8, attempt.proposed, "return Response.text(\"Not Found\"") != null);
    } else {
        return error.TestFailed;
    }
}
