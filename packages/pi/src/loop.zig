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

pub const ApprovalFn = union(enum) {
    bare: *const fn (file: []const u8) anyerror!bool,
    contextual: struct {
        context: *anyopaque,
        func: *const fn (context: *anyopaque, file: []const u8) anyerror!bool,
    },

    pub fn fromFn(func: *const fn (file: []const u8) anyerror!bool) ApprovalFn {
        return .{ .bare = func };
    }

    pub fn withContext(
        context: *anyopaque,
        func: *const fn (context: *anyopaque, file: []const u8) anyerror!bool,
    ) ApprovalFn {
        return .{ .contextual = .{ .context = context, .func = func } };
    }

    pub fn call(self: ApprovalFn, file: []const u8) anyerror!bool {
        return switch (self) {
            .bare => |func| func(file),
            .contextual => |wrapped| wrapped.func(wrapped.context, file),
        };
    }
};

pub const ApprovalPolicy = enum { ask, auto_approve, auto_reject };

pub fn autoApprove(file: []const u8) anyerror!bool {
    _ = file;
    return true;
}

pub fn autoReject(file: []const u8) anyerror!bool {
    _ = file;
    return false;
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
};

pub const RunOptions = struct {
    max_attempts: u8 = 3,
    workspace_root: []const u8 = ".",
    approval_fn: ?ApprovalFn = null,
    max_model_roundtrips_per_turn: u8 = 8,
    max_tool_calls_per_turn: usize = 16,
    max_tool_batch_size: usize = 8,
    replay_mode: bool = false,
};

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

    var machine: turn.TurnMachine = .{ .max_attempts = options.max_attempts };
    var next_event: turn.TurnEvent = .{ .user_submitted = user_text };
    var model_roundtrips: u8 = 0;
    var tool_calls_used: usize = 0;
    var turn_usage: turn.Usage = .{};

    while (true) {
        const action = machine.transition(next_event);
        switch (action) {
            .request_model => {
                if (model_roundtrips >= options.max_model_roundtrips_per_turn) {
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
                    next_event = .budget_exhausted;
                    continue;
                }
                model_roundtrips += 1;
                const prompt = try std.fmt.allocPrint(
                    ta,
                    "Your previous edit failed compiler verification (attempt {d}/{d}). " ++
                        "Here is the diagnostic envelope. Fix the flagged violations and " ++
                        "emit a new edit.\n\n{s}",
                    .{ payload.attempt, payload.max_attempts, payload.diagnostic },
                );
                const result = try callModel(allocator, ta, client, transcript, prompt);
                turn_usage.add(result.usage);
                next_event = .{ .model_replied = result.reply };
            },
            .run_veto => |edit| {
                const prepared = try prepareEdit(ta, options.workspace_root, edit);
                const veto_result = try veto.runVeto(ta, .{
                    .file = prepared.edit.file,
                    .content = prepared.edit.content,
                    .before = prepared.edit.before,
                });
                if (veto_result.outcome.ok and !options.replay_mode) {
                    if (options.approval_fn) |approve| {
                        if (!try approve.call(prepared.edit.file)) {
                            try transcript.append(allocator, .{ .tool_result = .{
                                .tool_use_id = "approval",
                                .tool_name = "apply_edit",
                                .ok = false,
                                .llm_text = "edit verified but not applied by user approval policy",
                                .ui_payload = null,
                            } });
                            return .{ .final_state = .done, .attempt = machine.attempt, .usage = turn_usage };
                        }
                    }
                    try applyPreparedEdit(ta, prepared);
                    const post_apply = try postApplyCheck(allocator, ta, registry, transcript, prepared);
                    defer if (post_apply.summary) |s| allocator.free(s);
                    try appendVerifiedPatchEntry(
                        allocator,
                        transcript,
                        options.workspace_root,
                        prepared,
                        veto_result.report,
                        post_apply,
                    );
                }
                next_event = .{ .edit_verified = veto_result.outcome };
            },
            .invoke_tool_batch => |calls| {
                try transcript.append(allocator, .{ .assistant_tool_use = calls });

                const mixed_apply_edit = containsApplyEdit(calls) and calls.len > 1;
                const over_budget = calls.len > options.max_tool_batch_size or
                    tool_calls_used + calls.len > options.max_tool_calls_per_turn;

                if (mixed_apply_edit or over_budget) {
                    for (calls) |call| {
                        const message = if (mixed_apply_edit)
                            "apply_edit must be the only tool call in a single assistant response"
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
                try transcript.append(allocator, msg);
                return .{ .final_state = machine.state, .attempt = machine.attempt, .usage = turn_usage };
            },
            .prompt_user => |question| {
                try transcript.append(allocator, .{ .diagnostic_box = .{ .llm_text = question } });
                return .{ .final_state = machine.state, .attempt = machine.attempt, .usage = turn_usage };
            },
            .end_turn => return .{ .final_state = machine.state, .attempt = machine.attempt, .usage = turn_usage },
            .none => return .{ .final_state = machine.state, .attempt = machine.attempt, .usage = turn_usage },
        }
    }
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
    const root_path = try std.fs.path.resolve(allocator, &.{workspace_root});
    const target_path = if (std.fs.path.isAbsolute(edit.file))
        try std.fs.path.resolve(allocator, &.{edit.file})
    else
        try std.fs.path.resolve(allocator, &.{ root_path, edit.file });

    if (!isPathInsideRoot(root_path, target_path)) return error.EditPathOutsideWorkspace;

    const before = edit.before orelse blk: {
        const current = file_io.readFile(allocator, target_path, 1024 * 1024) catch |err| switch (err) {
            error.FileNotFound => break :blk null,
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
) !void {
    const parent = std.fs.path.dirname(prepared.resolved_path);
    if (parent) |dir_path| {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), dir_path);
    }
    try file_io.writeFile(allocator, prepared.resolved_path, prepared.edit.content);
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

    if (registry.findByName("zigts_expert_verify_paths") != null) {
        const args_json = try std.fmt.allocPrint(
            arena,
            "{{\"paths\":[\"{s}\"]}}",
            .{prepared.edit.file},
        );
        var vr = registry.invokeJson(arena, "zigts_expert_verify_paths", args_json) catch return report;
        defer vr.deinit(arena);
        if (!vr.ok) {
            const note = try std.fmt.allocPrint(
                allocator,
                "post-apply regression: verify_paths found violations\n{s}",
                .{vr.llm_text},
            );
            defer allocator.free(note);
            try transcript.append(allocator, .{ .diagnostic_box = .{
                .llm_text = note,
                .ui_payload = if (vr.ui_payload) |payload|
                    try payload.clone(allocator)
                else
                    null,
            } });
            report.ok = false;
            if (report.summary == null) {
                report.summary = try allocator.dupe(u8, "verify_paths regressed");
            }
        }
    }

    if (prepared.edit.before != null and registry.findByName("zigts_expert_review_patch") != null) {
        const args_json = try std.fmt.allocPrint(
            arena,
            "{{\"file\":\"{s}\",\"diff_only\":true}}",
            .{prepared.edit.file},
        );
        var rr = registry.invokeJson(arena, "zigts_expert_review_patch", args_json) catch return report;
        defer rr.deinit(arena);
        if (!rr.ok) {
            const note = try std.fmt.allocPrint(
                allocator,
                "post-apply diff review: new violations found\n{s}",
                .{rr.llm_text},
            );
            defer allocator.free(note);
            try transcript.append(allocator, .{ .diagnostic_box = .{
                .llm_text = note,
                .ui_payload = if (rr.ui_payload) |payload|
                    try payload.clone(allocator)
                else
                    null,
            } });
            report.ok = false;
            if (report.summary) |s| allocator.free(s);
            report.summary = try allocator.dupe(u8, "review_patch flagged new violations");
        }
    }

    return report;
}

fn appendVerifiedPatchEntry(
    allocator: std.mem.Allocator,
    transcript: *transcript_mod.Transcript,
    workspace_root: []const u8,
    prepared: PreparedEdit,
    report: veto.VetoReport,
    post_apply: PostApplyReport,
) !void {
    const workspace_root_abs = try std.fs.path.resolve(allocator, &.{workspace_root});
    defer allocator.free(workspace_root_abs);
    var repair_links = try collectRecentRepairLinks(
        allocator,
        transcript,
        workspace_root_abs,
        prepared.edit.file,
        prepared.edit.content,
    );
    defer repair_links.deinit(allocator);

    const payload: ui_payload_mod.UiPayload = .{ .verified_patch = try proof_enrichment.buildVerifiedPatchPayload(
        allocator,
        .{
            .workspace_root = workspace_root_abs,
            .file = prepared.edit.file,
            .before = prepared.edit.before,
            .after = prepared.edit.content,
            .policy_hash = report.policy_hash,
            .applied_at_unix_ms = tools_common.nowUnixMs(),
            .post_apply_ok = post_apply.ok,
            .post_apply_summary = post_apply.summary,
            .transcript = transcript,
            .repair_plan_ids = repair_links.repair_plan_ids,
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

fn isPathInsideRoot(root: []const u8, candidate: []const u8) bool {
    if (!std.mem.startsWith(u8, candidate, root)) return false;
    if (candidate.len == root.len) return true;
    return candidate[root.len] == std.fs.path.sep;
}

const testing = std.testing;
const Tag = transcript_mod.Tag;

const CannedClient = struct {
    reply: turn.AssistantReply,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!ModelCallResult {
        const self: *CannedClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
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
    "function handler(req: Request): Response { var x = 1; return Response.json({x}); }";
const clean_handler =
    "function handler(req: Request): Response { return Response.json({ok: true}); }";

test "text reply path: user -> model text -> render" {
    var canned: CannedClient = .{ .reply = .{
        .response = .{ .final_text = "here is the plan" },
    } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(testing.allocator, canned.asClient(), &registry, &tr, "add a GET route", .{});
    try testing.expectEqual(turn.TurnState.done, result.final_state);
    switch (tr.at(0).*) {
        .user_text => |body| try testing.expectEqualStrings("add a GET route", body),
        else => return error.TestFailed,
    }
    switch (tr.at(1).*) {
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
    switch (tr.at(tr.len() - 1).*) {
        .diagnostic_box => |body| try testing.expect(std.mem.indexOf(u8, body.llm_text, "\"ZTS001\"") != null),
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

test "edit path outside the workspace is rejected" {
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

    try testing.expectError(
        error.EditPathOutsideWorkspace,
        runTurnWith(testing.allocator, canned.asClient(), &registry, &tr, "escape the workspace", .{}),
    );
}

test "autoApprove returns true for any file" {
    try testing.expect(try autoApprove("any/file.zig"));
}

test "autoReject returns false for any file" {
    try testing.expect(!try autoReject("any/file.zig"));
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
