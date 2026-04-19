//! Expert loop driver. Pumps transcript state, Anthropic replies, compiler
//! veto results, and structured tool batches through `turn.TurnMachine`.

const std = @import("std");
const turn = @import("turn.zig");
const veto = @import("veto.zig");
const transcript_mod = @import("transcript.zig");
const registry_mod = @import("registry/registry.zig");
const zigts = @import("zigts");
const file_io = zigts.file_io;
const apply_edit = @import("anthropic/apply_edit.zig");

pub const ModelClient = struct {
    context: *anyopaque,
    request_fn: *const fn (
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!turn.AssistantReply,

    pub fn request(
        self: ModelClient,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) !turn.AssistantReply {
        return self.request_fn(self.context, arena, transcript, extra_user_text);
    }
};

pub const ApprovalFn = *const fn (file: []const u8) anyerror!bool;

pub const ApprovalPolicy = enum { ask, auto_approve, auto_reject };

pub fn autoApprove(file: []const u8) anyerror!bool {
    _ = file;
    return true;
}

pub fn autoReject(file: []const u8) anyerror!bool {
    _ = file;
    return false;
}

pub const TurnResult = struct {
    final_state: turn.TurnState,
    attempt: u8,
};

pub const RunOptions = struct {
    max_attempts: u8 = 3,
    workspace_root: []const u8 = ".",
    approval_fn: ?ApprovalFn = null,
    max_model_roundtrips_per_turn: u8 = 8,
    max_tool_calls_per_turn: usize = 16,
    max_tool_batch_size: usize = 8,
};

const PreparedEdit = struct {
    edit: turn.Edit,
    resolved_path: []const u8,
};

pub fn runTurn(
    allocator: std.mem.Allocator,
    client: ModelClient,
    registry: *const registry_mod.Registry,
    transcript: *transcript_mod.Transcript,
    user_text: []const u8,
) !TurnResult {
    return runTurnWith(allocator, client, registry, transcript, user_text, .{});
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

    while (true) {
        const action = machine.transition(next_event);
        switch (action) {
            .request_model => {
                if (model_roundtrips >= options.max_model_roundtrips_per_turn) {
                    next_event = .budget_exhausted;
                    continue;
                }
                model_roundtrips += 1;
                const reply = try client.request(ta, transcript, null);
                if (reply.preamble) |text| {
                    if (text.len > 0) try transcript.append(allocator, .{ .model_text = text });
                }
                next_event = .{ .model_replied = reply };
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
                const reply = try client.request(ta, transcript, prompt);
                if (reply.preamble) |text| {
                    if (text.len > 0) try transcript.append(allocator, .{ .model_text = text });
                }
                next_event = .{ .model_replied = reply };
            },
            .run_veto => |edit| {
                const prepared = try prepareEdit(ta, options.workspace_root, edit);
                const outcome = try veto.runVeto(ta, .{
                    .file = prepared.edit.file,
                    .content = prepared.edit.content,
                    .before = prepared.edit.before,
                });
                if (outcome.ok) {
                    if (options.approval_fn) |approve| {
                        if (!try approve(prepared.edit.file)) {
                            try transcript.append(allocator, .{ .tool_result = .{
                                .tool_use_id = "approval",
                                .tool_name = "apply_edit",
                                .ok = false,
                                .body = "edit verified but not applied by user approval policy",
                            } });
                            return .{ .final_state = .done, .attempt = machine.attempt };
                        }
                    }
                    try applyPreparedEdit(ta, prepared);
                }
                next_event = .{ .edit_verified = outcome };
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
                            .body = message,
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
                        .body = result.body,
                    } });
                }
                next_event = .tool_batch_completed;
            },
            .render => |msg| {
                try transcript.append(allocator, msg);
                return .{ .final_state = machine.state, .attempt = machine.attempt };
            },
            .prompt_user => |question| {
                try transcript.append(allocator, .{ .diagnostic_box = question });
                return .{ .final_state = machine.state, .attempt = machine.attempt };
            },
            .end_turn => return .{ .final_state = machine.state, .attempt = machine.attempt },
            .none => return .{ .final_state = machine.state, .attempt = machine.attempt },
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
    ) anyerror!turn.AssistantReply {
        const self: *CannedClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
        return self.reply;
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
    ) anyerror!turn.AssistantReply {
        const self: *SequenceClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
        if (self.index >= self.replies.len) return error.TestSequenceExhausted;
        const reply = self.replies[self.index];
        self.index += 1;
        return reply;
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
    return .{ .ok = true, .body = try allocator.dupe(u8, "{\"stub\":\"ok\"}\n") };
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

    const result = try runTurn(testing.allocator, canned.asClient(), &registry, &tr, "add a GET route");
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
        .proof_card => |body| try testing.expect(std.mem.indexOf(u8, body, "\"total\":0") != null),
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
        .diagnostic_box => |body| try testing.expect(std.mem.indexOf(u8, body, "\"ZTS001\"") != null),
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

    const result = try runTurn(testing.allocator, seq.asClient(), &registry, &tr, "run the stub");
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
            .approval_fn = autoReject,
        },
    );

    switch (tr.at(tr.len() - 1).*) {
        .tool_result => |result| try testing.expect(std.mem.indexOf(u8, result.body, "not applied") != null),
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
        runTurn(testing.allocator, canned.asClient(), &registry, &tr, "escape the workspace"),
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
