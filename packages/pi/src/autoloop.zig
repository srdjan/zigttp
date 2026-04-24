//! Property-goal-driven convergence orchestrator.
//!
//! The autoloop invokes three compiler tools in a fixed rhythm: `pi_goal_check`
//! (to decide if we are done and extract witnesses), `pi_repair_plan` (to
//! produce typed edit intents for the still-unmet goals), and
//! `pi_apply_repair_plan` (to compiler-verify each candidate as a dry-run).
//! When a candidate verifies, the orchestrator writes it to disk and emits a
//! chained VerifiedPatch event. It continues until every goal flips to true or
//! a budget trips.
//!
//! The orchestrator is intentionally decoupled from the turn state machine and
//! from the LLM. The model is not in the loop here; the compiler is. A live
//! TUI can still run `/goal` mid-session and fall back to conversational mode
//! on exit, but that wiring lives in app.zig and is not this module's concern.
//!
//! Budgets are three independent stop conditions: max_iterations counts full
//! goal-check + repair-plan cycles; max_wall_time_ms caps elapsed time; and
//! max_patches_without_progress trips when a run of iterations lands patches
//! but flips no new goal green. Any one of the three ends the session with a
//! durable `autoloop_outcome` event so the ledger has a receipt either way.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("registry/registry.zig");
const transcript_mod = @import("transcript.zig");
const ui_payload = @import("ui_payload.zig");
const proof_enrichment = @import("proof_enrichment.zig");
const session_state = @import("session_state.zig");
const session_events = @import("session/events.zig");
const persister = @import("session/persister.zig");
const tools_common = @import("tools/common.zig");
const json_writer = @import("providers/anthropic/json_writer.zig");

pub const AutoloopVerdict = session_events.AutoloopVerdict;

pub const Budget = struct {
    max_iterations: u32 = 8,
    max_wall_time_ms: i64 = 120_000,
    /// Patches applied without flipping any new goal green. A stuck autoloop
    /// can burn iterations that all succeed at the tool level but make no
    /// progress on the user's actual goals; this catches that.
    max_patches_without_progress: u32 = 3,
};

pub const DriveOptions = struct {
    workspace_root: []const u8,
    file: []const u8,
    goals: []const []const u8,
    budget: Budget = .{},
    events_path: ?[]const u8 = null,
    policy_hash: []const u8 = "",
};

pub const Outcome = struct {
    verdict: AutoloopVerdict,
    iterations: u32,
    final_patch_hash: ?[32]u8 = null,
    goals_met_count: u32 = 0,
    goals_unmet_count: u32 = 0,
};

pub const DriveError = error{
    ToolFailed,
    InvalidToolOutput,
    FileWriteFailed,
} || std.mem.Allocator.Error;

pub fn drive(
    allocator: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    transcript: *transcript_mod.Transcript,
    options: DriveOptions,
) !Outcome {
    const start_ms = tools_common.nowUnixMs();
    var iter: u32 = 0;
    var patches_without_progress: u32 = 0;
    var last_goals_met_count: u32 = countMetGoals(transcript, options.file, options.goals);

    while (iter < options.budget.max_iterations) : (iter += 1) {
        if (tools_common.nowUnixMs() - start_ms > options.budget.max_wall_time_ms) {
            return finalize(allocator, transcript, options, .exhausted_time, iter);
        }

        const check = try invokePathGoalsTool(allocator, registry, "pi_goal_check", options.file, options.goals);
        defer allocator.free(check);
        if ((try parseGoalCheck(allocator, check)).ok) {
            return finalize(allocator, transcript, options, .achieved, iter);
        }

        const plans_json = try invokePathGoalsTool(allocator, registry, "pi_repair_plan", options.file, options.goals);
        defer allocator.free(plans_json);
        var plans = try parseRepairPlans(allocator, plans_json);
        defer plans.deinit(allocator);

        if (plans.items.len == 0) {
            return finalize(allocator, transcript, options, .stalled, iter);
        }

        const applied_this_iter = try applyPlans(
            allocator,
            registry,
            transcript,
            options,
            plans.items,
        );

        const current_met = countMetGoals(transcript, options.file, options.goals);
        if (current_met <= last_goals_met_count and applied_this_iter > 0) {
            patches_without_progress += applied_this_iter;
            if (patches_without_progress >= options.budget.max_patches_without_progress) {
                return finalize(allocator, transcript, options, .stalled, iter + 1);
            }
        } else if (current_met > last_goals_met_count) {
            patches_without_progress = 0;
        }
        last_goals_met_count = current_met;
    }

    return finalize(allocator, transcript, options, .exhausted_iters, iter);
}

fn countMetGoals(
    transcript: *const transcript_mod.Transcript,
    file: []const u8,
    goals: []const []const u8,
) u32 {
    const props = session_state.currentProperties(transcript, file) orelse return 0;
    var count: u32 = 0;
    for (goals) |goal| {
        if (session_state.propertyByName(props, goal)) count += 1;
    }
    return count;
}

fn finalize(
    allocator: std.mem.Allocator,
    transcript: *const transcript_mod.Transcript,
    options: DriveOptions,
    verdict: AutoloopVerdict,
    iterations: u32,
) !Outcome {
    const props = session_state.currentProperties(transcript, options.file);
    var met_buf: std.ArrayList([]const u8) = .empty;
    defer met_buf.deinit(allocator);
    var unmet_buf: std.ArrayList([]const u8) = .empty;
    defer unmet_buf.deinit(allocator);

    for (options.goals) |goal| {
        const satisfied = if (props) |p| session_state.propertyByName(p, goal) else false;
        if (satisfied) {
            try met_buf.append(allocator, goal);
        } else {
            try unmet_buf.append(allocator, goal);
        }
    }

    const final_hash = session_state.lastPatchHash(transcript, options.file);

    if (options.events_path) |path| {
        try session_events.appendEvent(allocator, path, .{ .autoloop_outcome = .{
            .verdict = verdict,
            .final_patch_hash = final_hash,
            .goals_met = met_buf.items,
            .goals_unmet = unmet_buf.items,
            .iterations = iterations,
        } });
    }

    return .{
        .verdict = verdict,
        .iterations = iterations,
        .final_patch_hash = final_hash,
        .goals_met_count = @intCast(met_buf.items.len),
        .goals_unmet_count = @intCast(unmet_buf.items.len),
    };
}

const GoalCheckResult = struct { ok: bool };

// pi_goal_check and pi_repair_plan both set `ToolResult.ok` to reflect the
// state of the thing they checked (goals met / no repair needed) rather than
// "did the tool run". An unsatisfied goal returns ok=false with a populated
// JSON body - that is exactly the signal the autoloop wants to drive its
// next iteration against. Parse the body regardless; real invocation errors
// surface through the Registry error set.
fn invokePathGoalsTool(
    allocator: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    tool_name: []const u8,
    file: []const u8,
    goals: []const []const u8,
) ![]u8 {
    const args_json = try buildPathGoalsJson(allocator, file, goals);
    defer allocator.free(args_json);
    var result = try registry.invokeJson(allocator, tool_name, args_json);
    defer result.deinit(allocator);
    return allocator.dupe(u8, result.llm_text);
}

fn buildPathGoalsJson(
    allocator: std.mem.Allocator,
    file: []const u8,
    goals: []const []const u8,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"path\":");
    try json_writer.writeString(w, file);
    try w.writeAll(",\"goals\":[");
    for (goals, 0..) |goal, i| {
        if (i > 0) try w.writeByte(',');
        try json_writer.writeString(w, goal);
    }
    try w.writeAll("]}");

    buf = aw.toArrayList();
    return buf.toOwnedSlice(allocator);
}

fn parseGoalCheck(allocator: std.mem.Allocator, json_text: []const u8) !GoalCheckResult {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_text, .{}) catch {
        return error.InvalidToolOutput;
    };
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidToolOutput;
    const ok_val = parsed.value.object.get("ok") orelse return error.InvalidToolOutput;
    if (ok_val != .bool) return error.InvalidToolOutput;
    return .{ .ok = ok_val.bool };
}

const RepairPlan = struct {
    id: []u8,
    /// Verbatim JSON source of the plan object, suitable for re-serialization
    /// into pi_apply_repair_plan's `plan` input.
    raw_json: []u8,
};

const RepairPlanList = struct {
    items: []RepairPlan,

    pub fn deinit(self: *RepairPlanList, allocator: std.mem.Allocator) void {
        for (self.items) |plan| {
            allocator.free(plan.id);
            allocator.free(plan.raw_json);
        }
        allocator.free(self.items);
        self.items = &.{};
    }
};

fn parseRepairPlans(allocator: std.mem.Allocator, json_text: []const u8) !RepairPlanList {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_text, .{}) catch {
        return error.InvalidToolOutput;
    };
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidToolOutput;
    const plans_val = parsed.value.object.get("plans") orelse
        return .{ .items = try allocator.alloc(RepairPlan, 0) };
    if (plans_val != .array) return .{ .items = try allocator.alloc(RepairPlan, 0) };

    const items = try allocator.alloc(RepairPlan, plans_val.array.items.len);
    errdefer allocator.free(items);
    var next: usize = 0;
    errdefer {
        var j: usize = 0;
        while (j < next) : (j += 1) {
            allocator.free(items[j].id);
            allocator.free(items[j].raw_json);
        }
    }

    for (plans_val.array.items) |plan_val| {
        if (plan_val != .object) return error.InvalidToolOutput;
        const id_val = plan_val.object.get("id") orelse return error.InvalidToolOutput;
        if (id_val != .string) return error.InvalidToolOutput;

        const id_copy = try allocator.dupe(u8, id_val.string);
        errdefer allocator.free(id_copy);

        var raw_buf: std.ArrayList(u8) = .empty;
        defer raw_buf.deinit(allocator);
        var raw_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &raw_buf);
        try std.json.Stringify.value(plan_val, .{}, &raw_aw.writer);
        raw_buf = raw_aw.toArrayList();

        items[next] = .{
            .id = id_copy,
            .raw_json = try raw_buf.toOwnedSlice(allocator),
        };
        next += 1;
    }

    return .{ .items = items };
}

const ApplyResult = struct {
    ok: bool,
    proposed_content: []u8,

    pub fn deinit(self: *ApplyResult, allocator: std.mem.Allocator) void {
        allocator.free(self.proposed_content);
    }
};

fn invokeApply(
    allocator: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    file: []const u8,
    plan_raw_json: []const u8,
) !ApplyResult {
    const args_json = try buildApplyArgsJson(allocator, file, plan_raw_json);
    defer allocator.free(args_json);
    var result = try registry.invokeJson(allocator, "pi_apply_repair_plan", args_json);
    defer result.deinit(allocator);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, result.llm_text, .{}) catch {
        return error.InvalidToolOutput;
    };
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidToolOutput;

    const ok_val = parsed.value.object.get("ok") orelse return error.InvalidToolOutput;
    if (ok_val != .bool) return error.InvalidToolOutput;
    const content_val = parsed.value.object.get("proposed_content") orelse {
        return .{ .ok = false, .proposed_content = try allocator.alloc(u8, 0) };
    };
    if (content_val != .string) return error.InvalidToolOutput;

    return .{
        .ok = ok_val.bool,
        .proposed_content = try allocator.dupe(u8, content_val.string),
    };
}

fn buildApplyArgsJson(
    allocator: std.mem.Allocator,
    file: []const u8,
    plan_raw_json: []const u8,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"path\":");
    try json_writer.writeString(w, file);
    try w.writeAll(",\"plan\":");
    try w.writeAll(plan_raw_json);
    try w.writeByte('}');

    buf = aw.toArrayList();
    return buf.toOwnedSlice(allocator);
}

fn applyPlans(
    allocator: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    transcript: *transcript_mod.Transcript,
    options: DriveOptions,
    plans: []const RepairPlan,
) !u32 {
    var applied: u32 = 0;
    for (plans) |plan| {
        var candidate = invokeApply(allocator, registry, options.file, plan.raw_json) catch |err| switch (err) {
            error.InvalidToolOutput, error.ToolFailed => continue,
            else => return err,
        };
        defer candidate.deinit(allocator);

        if (!candidate.ok) continue;

        const absolute = try tools_common.resolveInsideWorkspace(allocator, options.workspace_root, options.file);
        defer allocator.free(absolute);

        const before = zigts.file_io.readFile(allocator, absolute, 16 * 1024 * 1024) catch |err| switch (err) {
            error.FileNotFound => try allocator.alloc(u8, 0),
            else => return err,
        };
        defer allocator.free(before);

        zigts.file_io.writeFile(allocator, absolute, candidate.proposed_content) catch {
            return error.FileWriteFailed;
        };

        const parent_hash = session_state.lastPatchHash(transcript, options.file);
        const plan_ids = [_][]const u8{plan.id};

        var payload = try proof_enrichment.buildVerifiedPatchPayload(allocator, .{
            .workspace_root = options.workspace_root,
            .file = options.file,
            .before = before,
            .after = candidate.proposed_content,
            .policy_hash = options.policy_hash,
            .applied_at_unix_ms = tools_common.nowUnixMs(),
            .post_apply_ok = true,
            .repair_plan_ids = &plan_ids,
            .parent_hash = parent_hash,
            .goal_context = options.goals,
        });
        errdefer payload.deinit(allocator);

        const summary = try std.fmt.allocPrint(
            allocator,
            "autoloop verified: {s} (plan {s})",
            .{ options.file, plan.id },
        );
        errdefer allocator.free(summary);

        const ui: ui_payload.UiPayload = .{ .verified_patch = payload };
        try transcript.entries.append(allocator, .{ .verified_patch = .{
            .llm_text = summary,
            .ui_payload = ui,
        } });

        if (options.events_path) |path| {
            try session_events.appendEvent(allocator, path, .{ .verified_patch = .{
                .llm_text = summary,
                .ui_payload = ui,
            } });
        }

        applied += 1;
    }
    return applied;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

const StubTool = struct {
    pub fn alwaysOkGoalCheck(
        allocator: std.mem.Allocator,
        args: []const []const u8,
    ) anyerror!registry_mod.ToolResult {
        _ = args;
        return .{ .ok = true, .llm_text = try allocator.dupe(u8, "{\"ok\":true,\"goals\":[],\"witnesses\":[]}") };
    }

    pub fn alwaysFailingGoalCheck(
        allocator: std.mem.Allocator,
        args: []const []const u8,
    ) anyerror!registry_mod.ToolResult {
        _ = args;
        return .{ .ok = true, .llm_text = try allocator.dupe(u8, "{\"ok\":false,\"goals\":[\"retry_safe\"],\"witnesses\":[]}") };
    }

    pub fn emptyRepairPlan(
        allocator: std.mem.Allocator,
        args: []const []const u8,
    ) anyerror!registry_mod.ToolResult {
        _ = args;
        return .{ .ok = true, .llm_text = try allocator.dupe(u8, "{\"ok\":false,\"plans\":[]}") };
    }

    pub fn decodePassthrough(
        allocator: std.mem.Allocator,
        args_json: []const u8,
    ) anyerror![]const []const u8 {
        const out = try allocator.alloc([]const u8, 1);
        out[0] = try allocator.dupe(u8, args_json);
        return out;
    }
};

const StubExecute = *const fn (
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult;

fn goalCheckTool(exec: StubExecute) registry_mod.ToolDef {
    return .{
        .name = "pi_goal_check",
        .label = "goal-check",
        .description = "stub",
        .input_schema = "{}",
        .decode_json = StubTool.decodePassthrough,
        .execute = exec,
    };
}

fn repairPlanTool(exec: StubExecute) registry_mod.ToolDef {
    return .{
        .name = "pi_repair_plan",
        .label = "repair-plan",
        .description = "stub",
        .input_schema = "{}",
        .decode_json = StubTool.decodePassthrough,
        .execute = exec,
    };
}

test "drive returns .achieved when goal_check reports ok on the first iteration" {
    const allocator = testing.allocator;
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(allocator);

    try registry.register(allocator, goalCheckTool(StubTool.alwaysOkGoalCheck));
    try registry.register(allocator, repairPlanTool(StubTool.emptyRepairPlan));

    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(allocator);

    const outcome = try drive(allocator, &registry, &transcript, .{
        .workspace_root = ".",
        .file = "handler.ts",
        .goals = &.{ "retry_safe", "pure" },
    });

    try testing.expectEqual(AutoloopVerdict.achieved, outcome.verdict);
    try testing.expectEqual(@as(u32, 0), outcome.iterations);
}

test "drive returns .stalled when no plans are available but goals are unmet" {
    const allocator = testing.allocator;
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(allocator);

    try registry.register(allocator, goalCheckTool(StubTool.alwaysFailingGoalCheck));
    try registry.register(allocator, repairPlanTool(StubTool.emptyRepairPlan));

    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(allocator);

    const outcome = try drive(allocator, &registry, &transcript, .{
        .workspace_root = ".",
        .file = "handler.ts",
        .goals = &.{"retry_safe"},
        .budget = .{ .max_iterations = 4, .max_wall_time_ms = 60_000 },
    });

    try testing.expectEqual(AutoloopVerdict.stalled, outcome.verdict);
    try testing.expectEqual(@as(u32, 1), outcome.goals_unmet_count);
}

test "drive returns .exhausted_iters when budget runs out" {
    const allocator = testing.allocator;

    const never_ok = struct {
        fn run(
            alloc: std.mem.Allocator,
            args: []const []const u8,
        ) anyerror!registry_mod.ToolResult {
            _ = args;
            return .{ .ok = true, .llm_text = try alloc.dupe(u8, "{\"ok\":false,\"goals\":[\"retry_safe\"],\"witnesses\":[]}") };
        }
    };
    const plans_exist = struct {
        fn run(
            alloc: std.mem.Allocator,
            args: []const []const u8,
        ) anyerror!registry_mod.ToolResult {
            _ = args;
            return .{ .ok = true, .llm_text = try alloc.dupe(
                u8,
                "{\"ok\":false,\"plans\":[{\"id\":\"p1\",\"kind\":\"noop\",\"edit_intent\":{\"kind\":\"noop\",\"line\":1,\"column\":1,\"template\":\"\"}}]}",
            ) };
        }
    };
    const apply_fails = struct {
        fn run(
            alloc: std.mem.Allocator,
            args: []const []const u8,
        ) anyerror!registry_mod.ToolResult {
            _ = args;
            return .{ .ok = true, .llm_text = try alloc.dupe(u8, "{\"ok\":false,\"applied\":false}") };
        }
    };

    var registry: registry_mod.Registry = .{};
    defer registry.deinit(allocator);
    try registry.register(allocator, goalCheckTool(never_ok.run));
    try registry.register(allocator, repairPlanTool(plans_exist.run));
    try registry.register(allocator, .{
        .name = "pi_apply_repair_plan",
        .label = "apply",
        .description = "stub",
        .input_schema = "{}",
        .decode_json = StubTool.decodePassthrough,
        .execute = apply_fails.run,
    });

    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(allocator);

    const outcome = try drive(allocator, &registry, &transcript, .{
        .workspace_root = ".",
        .file = "handler.ts",
        .goals = &.{"retry_safe"},
        .budget = .{ .max_iterations = 3, .max_wall_time_ms = 60_000 },
    });

    try testing.expectEqual(AutoloopVerdict.exhausted_iters, outcome.verdict);
    try testing.expectEqual(@as(u32, 3), outcome.iterations);
}

test "parseRepairPlans handles missing plans field as empty list" {
    const allocator = testing.allocator;
    var plans = try parseRepairPlans(allocator, "{\"ok\":true}");
    defer plans.deinit(allocator);
    try testing.expectEqual(@as(usize, 0), plans.items.len);
}

test "parseRepairPlans extracts ids and re-serializes plan bodies" {
    const allocator = testing.allocator;
    const input =
        \\{"plans":[
        \\  {"id":"p1","edit_intent":{"kind":"insert_guard_before_line","line":3,"column":1,"template":"if (x.ok) {"}},
        \\  {"id":"p2","edit_intent":{"kind":"add_trailing_return","line":10,"column":1,"template":"return Response.text(\"ok\");"}}
        \\]}
    ;
    var plans = try parseRepairPlans(allocator, input);
    defer plans.deinit(allocator);

    try testing.expectEqual(@as(usize, 2), plans.items.len);
    try testing.expectEqualStrings("p1", plans.items[0].id);
    try testing.expectEqualStrings("p2", plans.items[1].id);
    try testing.expect(std.mem.indexOf(u8, plans.items[0].raw_json, "insert_guard_before_line") != null);
    try testing.expect(std.mem.indexOf(u8, plans.items[1].raw_json, "add_trailing_return") != null);
}

test "buildPathGoalsJson escapes special characters in the path" {
    const allocator = testing.allocator;
    const out = try buildPathGoalsJson(allocator, "a\"b.ts", &.{"retry_safe"});
    defer allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "\"path\":\"a\\\"b.ts\"") != null);
    try testing.expect(std.mem.indexOf(u8, out, "\"retry_safe\"") != null);
}
