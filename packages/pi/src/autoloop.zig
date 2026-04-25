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
const witness_replay = @import("witness_replay.zig");
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
    /// Optional stable witness key. When set, the autoloop terminates
    /// `.achieved` as soon as no witness with this key remains, even
    /// if other witnesses for the same property tag are still live.
    /// Lets the witness pane drive convergence on one specific
    /// counterexample instead of the whole property class.
    focus_witness_key: ?[]const u8 = null,
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
        const check_result = try parseGoalCheckResult(allocator, check);
        defer ui_payload.freeWitnessBodySlice(allocator, check_result.witnesses);
        if (focusedAchieved(check_result, options.focus_witness_key)) {
            return finalize(allocator, transcript, options, .achieved, iter);
        }

        const plans_json = try invokePathGoalsTool(allocator, registry, "pi_repair_plan", options.file, options.goals);
        defer allocator.free(plans_json);
        var plans = try parseRepairPlans(allocator, plans_json);
        defer plans.deinit(allocator);

        if (plans.items.len == 0) {
            return finalize(allocator, transcript, options, .stalled, iter);
        }

        const apply_outcome = try applyPlans(
            allocator,
            registry,
            transcript,
            options,
            plans.items,
            check_result.witnesses,
        );

        if (apply_outcome.regression) {
            return finalize(allocator, transcript, options, .regression_blocked, iter + 1);
        }

        const current_met = countMetGoals(transcript, options.file, options.goals);
        if (current_met <= last_goals_met_count and apply_outcome.applied > 0) {
            patches_without_progress += apply_outcome.applied;
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

/// Convergence predicate. Without a focus key the unfiltered
/// `pi_goal_check` verdict wins; with one, drive declares achieved iff
/// the focused witness has been defeated, regardless of unrelated
/// witnesses still live for the same property tag.
fn focusedAchieved(result: GoalCheckResult, focus_key: ?[]const u8) bool {
    const key = focus_key orelse return result.ok;
    for (result.witnesses) |w| {
        if (std.mem.eql(u8, w.key, key)) return false;
    }
    return true;
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

const GoalCheckResult = struct {
    ok: bool,
    /// Counterexample bodies for every property tag the tool checked. Owned
    /// by the caller; free via `ui_payload.freeWitnessBodySlice`.
    witnesses: []ui_payload.WitnessBody,
};

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

/// Single-pass parse of `pi_goal_check` output: extracts both the `ok`
/// flag and every counterexample witness body. Tolerant of malformed
/// witness entries (drops them) so the autoloop keeps making progress
/// even if one witness payload is corrupt.
fn parseGoalCheckResult(allocator: std.mem.Allocator, json_text: []const u8) !GoalCheckResult {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, json_text, .{}) catch {
        return error.InvalidToolOutput;
    };
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidToolOutput;
    const obj = parsed.value.object;
    const ok_val = obj.get("ok") orelse return error.InvalidToolOutput;
    if (ok_val != .bool) return error.InvalidToolOutput;

    const witnesses_value = obj.get("witnesses") orelse {
        return .{ .ok = ok_val.bool, .witnesses = try allocator.alloc(ui_payload.WitnessBody, 0) };
    };
    if (witnesses_value != .array) return error.InvalidToolOutput;

    var bodies: std.ArrayList(ui_payload.WitnessBody) = .empty;
    errdefer {
        for (bodies.items) |*body| body.deinit(allocator);
        bodies.deinit(allocator);
    }

    for (witnesses_value.array.items) |item| {
        const body = parseSingleGoalCheckWitness(allocator, item) catch continue;
        try bodies.append(allocator, body);
    }

    return .{ .ok = ok_val.bool, .witnesses = try bodies.toOwnedSlice(allocator) };
}

fn parseSingleGoalCheckWitness(
    allocator: std.mem.Allocator,
    value: std.json.Value,
) !ui_payload.WitnessBody {
    if (value != .object) return error.InvalidToolOutput;
    const obj = value.object;

    const key_str = ui_payload.getString(obj, "key") orelse return error.InvalidToolOutput;
    const property_str = ui_payload.getString(obj, "property") orelse return error.InvalidToolOutput;
    const summary_str = (ui_payload.getOptionalString(obj, "summary") catch return error.InvalidToolOutput) orelse "";

    const origin_obj_value = obj.get("origin") orelse return error.InvalidToolOutput;
    if (origin_obj_value != .object) return error.InvalidToolOutput;
    const sink_obj_value = obj.get("sink") orelse return error.InvalidToolOutput;
    if (sink_obj_value != .object) return error.InvalidToolOutput;
    const request_obj_value = obj.get("request") orelse return error.InvalidToolOutput;
    if (request_obj_value != .object) return error.InvalidToolOutput;

    const origin_line = try requireU32(origin_obj_value.object, "line");
    const origin_column = try requireU32(origin_obj_value.object, "column");
    const sink_line = try requireU32(sink_obj_value.object, "line");
    const sink_column = try requireU32(sink_obj_value.object, "column");

    const method_str = ui_payload.getString(request_obj_value.object, "method") orelse return error.InvalidToolOutput;
    const url_str = ui_payload.getString(request_obj_value.object, "url") orelse return error.InvalidToolOutput;
    const has_auth_value = request_obj_value.object.get("has_auth_header") orelse return error.InvalidToolOutput;
    if (has_auth_value != .bool) return error.InvalidToolOutput;
    const body_text = ui_payload.getOptionalString(request_obj_value.object, "body") catch return error.InvalidToolOutput;

    const stubs_value = obj.get("io_stubs") orelse return error.InvalidToolOutput;
    if (stubs_value != .array) return error.InvalidToolOutput;

    const key_copy = try allocator.dupe(u8, key_str);
    errdefer allocator.free(key_copy);
    const property_copy = try allocator.dupe(u8, property_str);
    errdefer allocator.free(property_copy);
    const summary_copy = try allocator.dupe(u8, summary_str);
    errdefer allocator.free(summary_copy);
    const method_copy = try allocator.dupe(u8, method_str);
    errdefer allocator.free(method_copy);
    const url_copy = try allocator.dupe(u8, url_str);
    errdefer allocator.free(url_copy);
    const body_copy: ?[]u8 = if (body_text) |t| try allocator.dupe(u8, t) else null;
    errdefer if (body_copy) |b| allocator.free(b);

    const stubs = try allocator.alloc(ui_payload.WitnessStub, stubs_value.array.items.len);
    errdefer allocator.free(stubs);
    for (stubs) |*stub| stub.* = undefined;
    var si: usize = 0;
    errdefer {
        while (si > 0) {
            si -= 1;
            stubs[si].deinit(allocator);
        }
    }
    while (si < stubs_value.array.items.len) : (si += 1) {
        const stub_value = stubs_value.array.items[si];
        if (stub_value != .object) return error.InvalidToolOutput;
        const stub_obj = stub_value.object;
        const stub_seq = try requireU32(stub_obj, "seq");
        // pi_goal_check emits the field as `fn` (the JS keyword) rather
        // than `func`, and the value as a raw JSON literal under
        // `result`. Translate to the persisted shape.
        const stub_module = ui_payload.getString(stub_obj, "module") orelse return error.InvalidToolOutput;
        const stub_func_value = stub_obj.get("fn") orelse return error.InvalidToolOutput;
        if (stub_func_value != .string) return error.InvalidToolOutput;
        const stub_result_value = stub_obj.get("result") orelse return error.InvalidToolOutput;

        var result_buf: std.ArrayList(u8) = .empty;
        defer result_buf.deinit(allocator);
        var result_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &result_buf);
        try std.json.Stringify.value(stub_result_value, .{}, &result_aw.writer);
        result_buf = result_aw.toArrayList();

        const module_copy = try allocator.dupe(u8, stub_module);
        errdefer allocator.free(module_copy);
        const func_copy = try allocator.dupe(u8, stub_func_value.string);
        errdefer allocator.free(func_copy);
        const result_copy = try result_buf.toOwnedSlice(allocator);

        stubs[si] = .{
            .seq = stub_seq,
            .module = module_copy,
            .func = func_copy,
            .result_json = result_copy,
        };
    }

    return .{
        .key = key_copy,
        .property = property_copy,
        .summary = summary_copy,
        .origin_line = origin_line,
        .origin_column = origin_column,
        .sink_line = sink_line,
        .sink_column = sink_column,
        .request_method = method_copy,
        .request_url = url_copy,
        .request_has_auth = has_auth_value.bool,
        .request_body = body_copy,
        .io_stubs = stubs,
    };
}

fn requireU32(obj: std.json.ObjectMap, key: []const u8) !u32 {
    const value = ui_payload.getUnsigned(obj, key) orelse return error.InvalidToolOutput;
    return std.math.cast(u32, value) orelse error.InvalidToolOutput;
}

/// Return the subset of `a` whose `.key` does not appear in `b`. The result
/// references the bodies in `a` in place; the caller still owns `a` and
/// must not free it until done with the returned slice. Pass the slice
/// through `ui_payload.WitnessBody.clone` when you need to outlive the
/// source array.
fn diffWitnessKeys(
    allocator: std.mem.Allocator,
    a: []const ui_payload.WitnessBody,
    b: []const ui_payload.WitnessBody,
) ![]const ui_payload.WitnessBody {
    if (a.len == 0) return &.{};

    var seen: std.StringHashMapUnmanaged(void) = .empty;
    defer seen.deinit(allocator);
    for (b) |body| try seen.put(allocator, body.key, {});

    var out: std.ArrayList(ui_payload.WitnessBody) = .empty;
    errdefer out.deinit(allocator);
    for (a) |body| {
        if (seen.contains(body.key)) continue;
        try out.append(allocator, body);
    }
    return out.toOwnedSlice(allocator);
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

const ApplyOutcome = struct {
    applied: u32,
    regression: bool = false,
};

fn applyPlans(
    allocator: std.mem.Allocator,
    registry: *const registry_mod.Registry,
    transcript: *transcript_mod.Transcript,
    options: DriveOptions,
    plans: []const RepairPlan,
    initial_pre_witnesses: []const ui_payload.WitnessBody,
) !ApplyOutcome {
    var applied: u32 = 0;
    // The first plan's "before" snapshot is the witness set drive() already
    // computed for this iteration. Each subsequent plan inherits the
    // previous plan's post-witnesses as its own pre-witnesses, so we never
    // re-run pi_goal_check on a file whose state we already know.
    var pre_witnesses: []ui_payload.WitnessBody = try ui_payload.cloneWitnessBodySlice(allocator, initial_pre_witnesses);
    defer ui_payload.freeWitnessBodySlice(allocator, pre_witnesses);

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

        // Tolerant of post-check failures: if pi_goal_check is unavailable
        // or returns garbage, fall back to an empty post-set so the patch
        // still lands - the ledger just won't carry a defeated/new diff
        // for this plan.
        const post_check_json = invokePathGoalsTool(
            allocator,
            registry,
            "pi_goal_check",
            options.file,
            options.goals,
        ) catch null;
        defer if (post_check_json) |text| allocator.free(text);
        var post_result: ?GoalCheckResult = if (post_check_json) |text|
            parseGoalCheckResult(allocator, text) catch null
        else
            null;
        defer if (post_result) |*r| ui_payload.freeWitnessBodySlice(allocator, r.witnesses);

        const post_witnesses: []const ui_payload.WitnessBody = if (post_result) |r| r.witnesses else &.{};
        const defeated_view = try diffWitnessKeys(allocator, pre_witnesses, post_witnesses);
        defer allocator.free(defeated_view);
        const new_view = try diffWitnessKeys(allocator, post_witnesses, pre_witnesses);
        defer allocator.free(new_view);

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
            .witnesses_defeated = defeated_view,
            .witnesses_new = new_view,
        });
        errdefer payload.deinit(allocator);

        const regressed = detectRegression(payload);

        const summary = try std.fmt.allocPrint(
            allocator,
            "autoloop {s}: {s} (plan {s})",
            .{
                if (regressed) "reverted" else "verified",
                options.file,
                plan.id,
            },
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

        // Auto-replay: confirm in the engine what the proof claimed about
        // each defeated and new witness. The note lands in the transcript
        // immediately after the verified_patch event so the user sees the
        // executed verdicts alongside the proof badges. Skipped silently
        // when no replay implementation is registered (unit-test paths,
        // headless builds).
        if (!regressed and witness_replay.isConfigured()) {
            try emitWitnessReplaySummary(allocator, transcript, options, absolute, payload);
        }

        if (regressed) {
            // Roll the file back. The verified_patch entry stays in the
            // transcript as an audit record of the attempt; the autoloop
            // signals regression_blocked up to drive() so the session
            // stops rather than keep trying past a property demotion.
            zigts.file_io.writeFile(allocator, absolute, before) catch {
                return error.FileWriteFailed;
            };
            if (options.events_path) |path| {
                const note = try std.fmt.allocPrint(
                    allocator,
                    "autoloop: plan {s} demoted a property; reverted {s} to the pre-patch snapshot.",
                    .{ plan.id, options.file },
                );
                defer allocator.free(note);
                try session_events.appendEvent(allocator, path, .{ .system_note = note });
            }
            return .{ .applied = applied, .regression = true };
        }

        applied += 1;

        // Hand off post-witnesses to become the next plan's pre-set: swap
        // them with our pre_witnesses so the per-iteration defer frees the
        // old pre and the function-exit defer frees the final pre. No
        // clone, no extra pi_goal_check call.
        if (post_result) |*r| {
            const stolen = r.witnesses;
            r.witnesses = pre_witnesses;
            pre_witnesses = stolen;
        }
    }
    return .{ .applied = applied };
}

/// Replay every defeated and new witness against the post-patch handler
/// and append a single system_note summarising the engine-confirmed
/// verdicts. Defeated witnesses that still reproduce are surfaced as
/// regressions; new witnesses that reproduce confirm the proof's claim
/// that the patch introduced a real failure mode. Best-effort: a replay
/// failure for any single witness is folded into the count rather than
/// aborting the whole pass, so the system note always lands.
///
/// Capped at `max_witness_replays` so a patch with many witnesses cannot
/// stall the autoloop on the slowest leg of an iteration. The truncated
/// count is reported as `truncated=N` in the note so the user knows
/// they are seeing a partial picture.
const max_witness_replays: usize = 8;

const ReplayCounts = struct {
    reproduced: u32 = 0,
    not_reproduced: u32 = 0,
    errors: u32 = 0,
    consumed: usize = 0,
};

/// Replay each witness in `bodies` (up to `budget` runs) and tally the
/// outcomes. Errors and non-runs are counted separately so the caller
/// can surface them distinctly. The function never aborts on a bad
/// witness; the call always lands a tally.
fn accumulateReplays(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    bodies: []const ui_payload.WitnessBody,
    budget: usize,
) ReplayCounts {
    var counts: ReplayCounts = .{};
    for (bodies) |body| {
        if (counts.consumed >= budget) break;
        counts.consumed += 1;
        const verdict_or_err = witness_replay.replay(allocator, handler_path, body);
        if (verdict_or_err) |raw| {
            var verdict = raw;
            defer verdict.deinit(allocator);
            if (!verdict.ran) {
                counts.errors += 1;
            } else if (verdict.reproducedViolation(body)) {
                counts.reproduced += 1;
            } else {
                counts.not_reproduced += 1;
            }
        } else |_| {
            counts.errors += 1;
        }
    }
    return counts;
}

fn emitWitnessReplaySummary(
    allocator: std.mem.Allocator,
    transcript: *transcript_mod.Transcript,
    options: DriveOptions,
    handler_path: []const u8,
    payload: ui_payload.VerifiedPatchPayload,
) !void {
    const total = payload.witnesses_defeated.len + payload.witnesses_new.len;
    if (total == 0) return;

    const defeated = accumulateReplays(
        allocator,
        handler_path,
        payload.witnesses_defeated,
        max_witness_replays,
    );
    const new_budget = if (max_witness_replays > defeated.consumed)
        max_witness_replays - defeated.consumed
    else
        0;
    const new_w = accumulateReplays(
        allocator,
        handler_path,
        payload.witnesses_new,
        new_budget,
    );

    const done = defeated.consumed + new_w.consumed;
    const truncated: usize = if (total > done) total - done else 0;
    const note = try std.fmt.allocPrint(
        allocator,
        "auto-replay {s}: defeated still-defeated={d} regressed={d}; new reproduces={d} unreached={d}; errors={d}; truncated={d}",
        .{
            options.file,
            defeated.not_reproduced,
            defeated.reproduced,
            new_w.reproduced,
            new_w.not_reproduced,
            defeated.errors + new_w.errors,
            truncated,
        },
    );
    var note_owned_by_transcript = false;
    errdefer if (!note_owned_by_transcript) allocator.free(note);

    try transcript.entries.append(allocator, .{ .system_note = note });
    note_owned_by_transcript = true;

    if (options.events_path) |path| {
        try session_events.appendEvent(allocator, path, .{ .system_note = note });
    }
}

/// A patch regresses if any bool property that was true in `before` is
/// false in `after`. The witness diff (`witnesses_new`) is now populated
/// per patch but is not consulted here: a patch that introduces a new
/// witness while preserving every property bool is considered safe at
/// this layer; the witness pane surfaces it instead.
fn detectRegression(payload: ui_payload.VerifiedPatchPayload) bool {
    const after = payload.after_properties orelse return false;
    const Visitor = struct {
        found: bool = false,
        pub fn visit(self: *@This(), change: ui_payload.PropertiesSnapshot.Change) !void {
            if (change.kind == .demoted) self.found = true;
        }
    };
    var visitor: Visitor = .{};
    ui_payload.PropertiesSnapshot.forEachChange(
        payload.before_properties,
        after,
        *Visitor,
        &visitor,
    ) catch return false;
    return visitor.found;
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

test "detectRegression is true when any bool property demotes" {
    const before: ui_payload.PropertiesSnapshot = .{
        .pure = true, .read_only = true, .stateless = true, .retry_safe = true,
        .deterministic = true, .has_egress = false, .no_secret_leakage = true,
        .no_credential_leakage = true, .input_validated = true, .pii_contained = true,
        .idempotent = true, .max_io_depth = 0, .injection_safe = true,
        .state_isolated = true, .fault_covered = true, .result_safe = true,
        .optional_safe = true,
    };
    var after = before;
    after.pure = false;
    const payload: ui_payload.VerifiedPatchPayload = .{
        .file = @constCast(""),
        .policy_hash = @constCast(""),
        .applied_at_unix_ms = 0,
        .stats = .{ .total = 0, .new = 0, .preexisting = 0 },
        .before = null,
        .after = @constCast(""),
        .unified_diff = @constCast(""),
        .hunks = &.{},
        .violations = &.{},
        .before_properties = before,
        .after_properties = after,
        .prove = null,
        .system = null,
        .rule_citations = &.{},
        .post_apply_ok = true,
        .post_apply_summary = null,
    };
    try testing.expect(detectRegression(payload));
}

test "detectRegression is false when only promotions occur" {
    var before: ui_payload.PropertiesSnapshot = .{
        .pure = false, .read_only = false, .stateless = false, .retry_safe = false,
        .deterministic = false, .has_egress = false, .no_secret_leakage = false,
        .no_credential_leakage = false, .input_validated = false, .pii_contained = false,
        .idempotent = false, .max_io_depth = null, .injection_safe = false,
        .state_isolated = false, .fault_covered = false, .result_safe = false,
        .optional_safe = false,
    };
    var after = before;
    after.retry_safe = true;
    after.no_secret_leakage = true;
    const payload: ui_payload.VerifiedPatchPayload = .{
        .file = @constCast(""),
        .policy_hash = @constCast(""),
        .applied_at_unix_ms = 0,
        .stats = .{ .total = 0, .new = 0, .preexisting = 0 },
        .before = null,
        .after = @constCast(""),
        .unified_diff = @constCast(""),
        .hunks = &.{},
        .violations = &.{},
        .before_properties = before,
        .after_properties = after,
        .prove = null,
        .system = null,
        .rule_citations = &.{},
        .post_apply_ok = true,
        .post_apply_summary = null,
    };
    _ = &before;
    try testing.expect(!detectRegression(payload));
}

test "detectRegression is false when before_properties is absent" {
    const after: ui_payload.PropertiesSnapshot = .{
        .pure = false, .read_only = false, .stateless = false, .retry_safe = false,
        .deterministic = false, .has_egress = false, .no_secret_leakage = false,
        .no_credential_leakage = false, .input_validated = false, .pii_contained = false,
        .idempotent = false, .max_io_depth = null, .injection_safe = false,
        .state_isolated = false, .fault_covered = false, .result_safe = false,
        .optional_safe = false,
    };
    const payload: ui_payload.VerifiedPatchPayload = .{
        .file = @constCast(""),
        .policy_hash = @constCast(""),
        .applied_at_unix_ms = 0,
        .stats = .{ .total = 0, .new = 0, .preexisting = 0 },
        .before = null,
        .after = @constCast(""),
        .unified_diff = @constCast(""),
        .hunks = &.{},
        .violations = &.{},
        .before_properties = null,
        .after_properties = after,
        .prove = null,
        .system = null,
        .rule_citations = &.{},
        .post_apply_ok = true,
        .post_apply_summary = null,
    };
    try testing.expect(!detectRegression(payload));
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

test "parseGoalCheckResult returns ok=true with empty witnesses when absent" {
    const allocator = testing.allocator;
    const result = try parseGoalCheckResult(allocator, "{\"ok\":true,\"goals\":[]}");
    defer ui_payload.freeWitnessBodySlice(allocator, result.witnesses);
    try testing.expect(result.ok);
    try testing.expectEqual(@as(usize, 0), result.witnesses.len);
}

test "parseGoalCheckResult extracts ok=false and a full witness body" {
    const allocator = testing.allocator;
    const input =
        \\{"ok":false,"goals":["no_secret_leakage"],"witnesses":[
        \\{"key":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        \\"property":"no_secret_leakage",
        \\"origin":{"line":3,"column":9},
        \\"sink":{"line":5,"column":12},
        \\"summary":"DB_KEY in response",
        \\"request":{"method":"GET","url":"/","has_auth_header":false},
        \\"io_stubs":[{"seq":0,"module":"env","fn":"env","result":"sentinel"}]}
        \\]}
    ;
    const result = try parseGoalCheckResult(allocator, input);
    defer ui_payload.freeWitnessBodySlice(allocator, result.witnesses);

    try testing.expect(!result.ok);
    try testing.expectEqual(@as(usize, 1), result.witnesses.len);
    try testing.expectEqualStrings("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", result.witnesses[0].key);
    try testing.expectEqualStrings("no_secret_leakage", result.witnesses[0].property);
    try testing.expectEqualStrings("DB_KEY in response", result.witnesses[0].summary);
    try testing.expectEqual(@as(u32, 3), result.witnesses[0].origin_line);
    try testing.expectEqual(@as(u32, 5), result.witnesses[0].sink_line);
    try testing.expectEqualStrings("GET", result.witnesses[0].request_method);
    try testing.expectEqualStrings("/", result.witnesses[0].request_url);
    try testing.expect(!result.witnesses[0].request_has_auth);
    try testing.expect(result.witnesses[0].request_body == null);
    try testing.expectEqual(@as(usize, 1), result.witnesses[0].io_stubs.len);
    try testing.expectEqualStrings("env", result.witnesses[0].io_stubs[0].module);
    try testing.expectEqualStrings("env", result.witnesses[0].io_stubs[0].func);
    // The parsed `result` value is re-encoded as JSON before storing, so
    // the persisted bytes are the JSON form (with surrounding quotes).
    try testing.expectEqualStrings("\"sentinel\"", result.witnesses[0].io_stubs[0].result_json);
}

test "parseGoalCheckResult skips malformed witness entries instead of failing" {
    const allocator = testing.allocator;
    const input =
        \\{"ok":false,"witnesses":[
        \\{"property":"no_secret_leakage"},
        \\{"key":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
        \\"property":"injection_safe",
        \\"origin":{"line":1,"column":1},
        \\"sink":{"line":1,"column":1},
        \\"summary":"x",
        \\"request":{"method":"POST","url":"/x","has_auth_header":true,"body":"{}"},
        \\"io_stubs":[]}
        \\]}
    ;
    const result = try parseGoalCheckResult(allocator, input);
    defer ui_payload.freeWitnessBodySlice(allocator, result.witnesses);

    try testing.expectEqual(@as(usize, 1), result.witnesses.len);
    try testing.expectEqualStrings("injection_safe", result.witnesses[0].property);
    try testing.expectEqualStrings("POST", result.witnesses[0].request_method);
    try testing.expect(result.witnesses[0].request_has_auth);
    try testing.expect(result.witnesses[0].request_body != null);
    try testing.expectEqualStrings("{}", result.witnesses[0].request_body.?);
}

test "diffWitnessKeys returns bodies in a but not b, keyed by stable hex" {
    const allocator = testing.allocator;
    const a = [_]ui_payload.WitnessBody{
        .{
            .key = @constCast("aa" ** 32),
            .property = @constCast("no_secret_leakage"),
            .summary = @constCast(""),
            .origin_line = 1, .origin_column = 1, .sink_line = 1, .sink_column = 1,
            .request_method = @constCast("GET"),
            .request_url = @constCast("/"),
            .request_has_auth = false,
            .request_body = null,
            .io_stubs = &.{},
        },
        .{
            .key = @constCast("bb" ** 32),
            .property = @constCast("injection_safe"),
            .summary = @constCast(""),
            .origin_line = 1, .origin_column = 1, .sink_line = 1, .sink_column = 1,
            .request_method = @constCast("GET"),
            .request_url = @constCast("/"),
            .request_has_auth = false,
            .request_body = null,
            .io_stubs = &.{},
        },
    };
    const b = [_]ui_payload.WitnessBody{
        .{
            .key = @constCast("aa" ** 32),
            .property = @constCast("no_secret_leakage"),
            .summary = @constCast(""),
            .origin_line = 1, .origin_column = 1, .sink_line = 1, .sink_column = 1,
            .request_method = @constCast("GET"),
            .request_url = @constCast("/"),
            .request_has_auth = false,
            .request_body = null,
            .io_stubs = &.{},
        },
    };

    const diff = try diffWitnessKeys(allocator, &a, &b);
    defer allocator.free(diff);
    try testing.expectEqual(@as(usize, 1), diff.len);
    try testing.expectEqualStrings("bb" ** 32, diff[0].key);
    try testing.expectEqualStrings("injection_safe", diff[0].property);
}

test "diffWitnessKeys returns empty slice when a is empty" {
    const allocator = testing.allocator;
    const a: []const ui_payload.WitnessBody = &.{};
    const b: []const ui_payload.WitnessBody = &.{};
    const diff = try diffWitnessKeys(allocator, a, b);
    defer allocator.free(diff);
    try testing.expectEqual(@as(usize, 0), diff.len);
}

test "focusedAchieved without focus key defers to result.ok" {
    const result_ok: GoalCheckResult = .{ .ok = true, .witnesses = &.{} };
    const result_unmet: GoalCheckResult = .{ .ok = false, .witnesses = &.{} };
    try testing.expect(focusedAchieved(result_ok, null));
    try testing.expect(!focusedAchieved(result_unmet, null));
}

test "focusedAchieved returns true when focused witness is gone, even if others remain" {
    const others = [_]ui_payload.WitnessBody{
        .{
            .key = @constCast("cc" ** 32),
            .property = @constCast("no_secret_leakage"),
            .summary = @constCast(""),
            .origin_line = 1, .origin_column = 1, .sink_line = 1, .sink_column = 1,
            .request_method = @constCast("GET"),
            .request_url = @constCast("/"),
            .request_has_auth = false,
            .request_body = null,
            .io_stubs = &.{},
        },
    };
    const result: GoalCheckResult = .{ .ok = false, .witnesses = @constCast(&others) };
    try testing.expect(focusedAchieved(result, "aa" ** 32));
}

test "focusedAchieved returns false when focused witness is still live" {
    const live = [_]ui_payload.WitnessBody{
        .{
            .key = @constCast("aa" ** 32),
            .property = @constCast("no_secret_leakage"),
            .summary = @constCast(""),
            .origin_line = 1, .origin_column = 1, .sink_line = 1, .sink_column = 1,
            .request_method = @constCast("GET"),
            .request_url = @constCast("/"),
            .request_has_auth = false,
            .request_body = null,
            .io_stubs = &.{},
        },
        .{
            .key = @constCast("bb" ** 32),
            .property = @constCast("injection_safe"),
            .summary = @constCast(""),
            .origin_line = 1, .origin_column = 1, .sink_line = 1, .sink_column = 1,
            .request_method = @constCast("POST"),
            .request_url = @constCast("/"),
            .request_has_auth = false,
            .request_body = null,
            .io_stubs = &.{},
        },
    };
    const result: GoalCheckResult = .{ .ok = false, .witnesses = @constCast(&live) };
    try testing.expect(!focusedAchieved(result, "aa" ** 32));
    try testing.expect(!focusedAchieved(result, "bb" ** 32));
}
