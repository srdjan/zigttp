//! Expert loop driver. Pumps events from a `ModelClient` + the compiler veto
//! + the tool registry into the `turn.TurnMachine`, executes returned actions,
//! applies verified edits to the workspace after optional user approval, and
//! appends rendered messages to a `Transcript`.
//!
//! Memory model: every intra-turn allocation (model reply bodies, veto
//! envelope, tool result body) goes into a per-turn `ArenaAllocator` that
//! dies when the turn ends. The transcript uses the long-lived caller
//! allocator and dupes on append, so transcript entries survive the arena's
//! deinit.
//!
//! What is deferred to later slices: real Anthropic HTTPS client
//! (`ModelClient` is the plug-in point), retry-on-failed-veto budgeting
//! (the machine's `budget_exhausted` event exists but retries require a
//! machine redesign), capability-gated tool approval (`prompt_user` action
//! is wired but no tool currently triggers it).

const std = @import("std");
const turn = @import("turn.zig");
const veto = @import("veto.zig");
const transcript_mod = @import("transcript.zig");
const registry_mod = @import("registry/registry.zig");
const zigts = @import("zigts");
const file_io = zigts.file_io;

/// Opaque model-client interface. Lets the loop driver run end-to-end under
/// test with a synthetic client returning canned replies, while leaving a
/// clean drop-in point for a real Anthropic streaming client.
pub const ModelClient = struct {
    context: *anyopaque,
    request_fn: *const fn (
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        user_text: []const u8,
    ) anyerror!turn.ModelReply,

    pub fn request(
        self: ModelClient,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        user_text: []const u8,
    ) !turn.ModelReply {
        return self.request_fn(self.context, arena, transcript, user_text);
    }
};

pub const ApprovalFn = *const fn (file: []const u8) anyerror!bool;

pub const TurnResult = struct {
    final_state: turn.TurnState,
    /// Final attempt counter at the moment the turn returned. 1 for the
    /// happy path, 2..max_attempts when a retry landed the final edit,
    /// and `max_attempts` for an exhausted retry budget.
    attempt: u8,
};

pub const RunOptions = struct {
    max_attempts: u8 = 3,
    workspace_root: []const u8 = ".",
    approval_fn: ?ApprovalFn = null,
};

const PreparedEdit = struct {
    edit: turn.ModelReply.Edit,
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

    while (true) {
        const action = machine.transition(next_event);

        switch (action) {
            .send_to_model => |text| {
                const reply = try client.request(ta, transcript, text);
                next_event = .{ .model_replied = reply };
            },
            .run_veto => |edit| {
                const prepared = try prepareEdit(ta, options.workspace_root, edit);
                const outcome = try veto.runVeto(ta, prepared.edit);
                if (outcome.ok) {
                    if (options.approval_fn) |approve| {
                        if (!try approve(prepared.edit.file)) {
                            const note = try std.fmt.allocPrint(
                                ta,
                                "edit verified but not applied: {s}",
                                .{prepared.edit.file},
                            );
                            try transcript.append(allocator, .{ .tool_result = note });
                            return .{ .final_state = .done, .attempt = machine.attempt };
                        }
                    }
                    try applyPreparedEdit(ta, prepared);
                }
                next_event = .{ .edit_verified = outcome };
            },
            .retry_draft => |payload| {
                const prompt = try std.fmt.allocPrint(
                    ta,
                    "Your previous edit failed compiler verification (attempt {d}/{d}). " ++
                        "Here is the diagnostic envelope. Fix the flagged violations and " ++
                        "emit a new edit.\n\n{s}",
                    .{ payload.attempt, payload.max_attempts, payload.diagnostic },
                );
                const reply = try client.request(ta, transcript, prompt);
                next_event = .{ .model_replied = reply };
            },
            .invoke_tool => |tc| {
                const args = try decodeToolArgs(ta, tc.args_json);
                const result = try registry.invoke(ta, tc.name, args);
                next_event = .{ .tool_completed = .{
                    .ok = result.ok,
                    .body = result.body,
                } };
            },
            .render => |msg| {
                // msg.body is borrowed from the arena; transcript.append
                // dupes it using the long-lived allocator before we return
                // and `defer arena.deinit()` fires below.
                try transcript.append(allocator, msg);
                return .{ .final_state = machine.state, .attempt = machine.attempt };
            },
            .prompt_user => return .{ .final_state = machine.state, .attempt = machine.attempt },
            .end_turn => return .{ .final_state = machine.state, .attempt = machine.attempt },
            .none => return .{ .final_state = machine.state, .attempt = machine.attempt },
        }
    }
}

fn decodeToolArgs(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    const trimmed = std.mem.trim(u8, args_json, " \t\r\n");
    if (trimmed.len == 0 or std.mem.eql(u8, trimmed, "{}")) return &.{};

    const parsed = std.json.parseFromSliceLeaky(std.json.Value, allocator, trimmed, .{}) catch {
        return &.{trimmed};
    };
    if (parsed != .object) return &.{trimmed};

    if (parsed.object.get("input")) |value| {
        if (value == .string) return &.{value.string};
    }

    var it = parsed.object.iterator();
    var only_string: ?[]const u8 = null;
    while (it.next()) |entry| {
        const value = entry.value_ptr.*;
        if (value != .string) return &.{trimmed};
        if (only_string != null) return &.{trimmed};
        only_string = value.string;
    }
    if (only_string) |value| return &.{value};
    return &.{};
}

fn prepareEdit(
    allocator: std.mem.Allocator,
    workspace_root: []const u8,
    edit: turn.ModelReply.Edit,
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

// ---------------------------------------------------------------------------
// Tests
//
// The synthetic `CannedClient` returns a hard-coded `ModelReply` regardless
// of the user prompt. That's enough to exercise every path through the
// state machine deterministically without a network.
// ---------------------------------------------------------------------------

const testing = std.testing;
const Tag = transcript_mod.Tag;

const CannedClient = struct {
    reply: turn.ModelReply,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        user_text: []const u8,
    ) anyerror!turn.ModelReply {
        const self: *CannedClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        _ = user_text;
        return self.reply;
    }

    pub fn asClient(self: *CannedClient) ModelClient {
        return .{
            .context = self,
            .request_fn = requestFn,
        };
    }
};

/// Iterates through a fixed list of canned replies, one per request.
/// Used by the retry tests to script "bad, good" and "bad, bad, bad"
/// scenarios without touching a real model.
const SequenceClient = struct {
    replies: []const turn.ModelReply,
    index: usize = 0,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        user_text: []const u8,
    ) anyerror!turn.ModelReply {
        const self: *SequenceClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        _ = user_text;
        if (self.index >= self.replies.len) return error.TestSequenceExhausted;
        const reply = self.replies[self.index];
        self.index += 1;
        return reply;
    }

    pub fn asClient(self: *SequenceClient) ModelClient {
        return .{
            .context = self,
            .request_fn = requestFn,
        };
    }
};

fn stubExecute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    _ = args;
    const body = try allocator.dupe(u8, "{\"stub\":\"ok\"}\n");
    return .{ .ok = true, .body = body };
}

const stub_tool: registry_mod.ToolDef = .{
    .name = "stub",
    .label = "stub",
    .description = "Test stub; echoes a canned body.",
    .execute = stubExecute,
};

test "text reply path: user -> model text -> render" {
    var canned: CannedClient = .{ .reply = .{ .text = "here is the plan" } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurn(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add a GET route",
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(@as(usize, 2), tr.len());
    try testing.expectEqual(Tag.user_text, tr.at(0).tag);
    try testing.expectEqualStrings("add a GET route", tr.at(0).body);
    try testing.expectEqual(Tag.model_text, tr.at(1).tag);
    try testing.expectEqualStrings("here is the plan", tr.at(1).body);
}

test "clean edit path: veto passes, proof card in transcript" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/src/handler.ts", .{tmp.sub_path});
    defer testing.allocator.free(written_path);

    var canned: CannedClient = .{ .reply = .{ .edit = .{
        .file = "src/handler.ts",
        .content = "function handler(req: Request): Response { return Response.json({ok: true}); }",
        .before = null,
    } } };
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
        .{ .workspace_root = tmp.sub_path[0..] },
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(@as(usize, 2), tr.len());
    try testing.expectEqual(Tag.proof_card, tr.at(1).tag);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "\"total\":0") != null);
    const written = try file_io.readFile(testing.allocator, written_path, 1024 * 1024);
    defer testing.allocator.free(written);
    try testing.expectEqualStrings(canned.reply.edit.content, written);
}

test "broken edit path: veto fails, diagnostic box in transcript" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/src/handler.ts", .{tmp.sub_path});
    defer testing.allocator.free(written_path);

    var canned: CannedClient = .{ .reply = .{ .edit = .{
        .file = "src/handler.ts",
        .content = "function handler(req: Request): Response { var x = 1; return Response.json({x}); }",
        .before = null,
    } } };
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
        .{ .workspace_root = tmp.sub_path[0..] },
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(Tag.diagnostic_box, tr.at(1).tag);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "\"ZTS001\"") != null);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "\"introduced_by_patch\":true") != null);
    try testing.expect(!file_io.fileExists(testing.allocator, written_path));
}

test "tool call path: invoke_tool -> tool_result in transcript" {
    var canned: CannedClient = .{ .reply = .{ .tool_call = .{
        .name = "stub",
        .args_json = "{}",
    } } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);
    try registry.register(testing.allocator, stub_tool);

    const result = try runTurn(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "run the stub",
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(Tag.tool_result, tr.at(1).tag);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "\"stub\":\"ok\"") != null);
}

fn rejectEdit(_: []const u8) anyerror!bool {
    return false;
}

test "user text is always the first transcript entry regardless of reply shape" {
    // Independent of which branch the machine takes, the user's original
    // intent should be preserved as entry 0. This protects against a future
    // regression where runTurn forgets to append the user_text.
    var canned: CannedClient = .{ .reply = .{ .text = "noop" } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    _ = try runTurn(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "original intent",
    );

    try testing.expectEqual(Tag.user_text, tr.at(0).tag);
    try testing.expectEqualStrings("original intent", tr.at(0).body);
}

// ---------------------------------------------------------------------------
// Retry loop integration tests. Each drives a real `runVeto` through the
// compiler so the retry path exercises the full diagnostic envelope shape,
// not a hand-rolled mock. SequenceClient swaps replies between attempts.
// ---------------------------------------------------------------------------

const bad_handler =
    "function handler(req: Request): Response { var x = 1; return Response.json({x}); }";
const clean_handler =
    "function handler(req: Request): Response { return Response.json({ok: true}); }";

test "retry: one bad draft then one good draft lands a proof card" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/handler.ts", .{tmp.sub_path});
    defer testing.allocator.free(written_path);

    var seq: SequenceClient = .{ .replies = &.{
        .{ .edit = .{ .file = "handler.ts", .content = bad_handler, .before = null } },
        .{ .edit = .{ .file = "handler.ts", .content = clean_handler, .before = null } },
    } };
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
        .{ .workspace_root = tmp.sub_path[0..] },
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(@as(u8, 2), result.attempt);
    try testing.expectEqual(@as(usize, 2), tr.len());
    try testing.expectEqual(Tag.proof_card, tr.at(1).tag);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "\"total\":0") != null);
    try testing.expectEqual(@as(usize, 2), seq.index);
    const written = try file_io.readFile(testing.allocator, written_path, 1024 * 1024);
    defer testing.allocator.free(written);
    try testing.expectEqualStrings(clean_handler, written);
}

test "retry: budget exhausted after three bad drafts surfaces diagnostic" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/handler.ts", .{tmp.sub_path});
    defer testing.allocator.free(written_path);

    var seq: SequenceClient = .{ .replies = &.{
        .{ .edit = .{ .file = "handler.ts", .content = bad_handler, .before = null } },
        .{ .edit = .{ .file = "handler.ts", .content = bad_handler, .before = null } },
        .{ .edit = .{ .file = "handler.ts", .content = bad_handler, .before = null } },
    } };
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
        .{ .max_attempts = 3, .workspace_root = tmp.sub_path[0..] },
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(@as(u8, 3), result.attempt);
    try testing.expectEqual(@as(usize, 2), tr.len());
    try testing.expectEqual(Tag.diagnostic_box, tr.at(1).tag);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "\"ZTS001\"") != null);
    try testing.expectEqual(@as(usize, 3), seq.index);
    try testing.expect(!file_io.fileExists(testing.allocator, written_path));
}

test "retry: max_attempts=2 blocks a third call" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/handler.ts", .{tmp.sub_path});
    defer testing.allocator.free(written_path);

    var seq: SequenceClient = .{ .replies = &.{
        .{ .edit = .{ .file = "handler.ts", .content = bad_handler, .before = null } },
        .{ .edit = .{ .file = "handler.ts", .content = bad_handler, .before = null } },
    } };
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
        .{ .max_attempts = 2, .workspace_root = tmp.sub_path[0..] },
    );

    try testing.expectEqual(@as(u8, 2), result.attempt);
    try testing.expectEqual(Tag.diagnostic_box, tr.at(1).tag);
    try testing.expectEqual(@as(usize, 2), seq.index);
    try testing.expect(!file_io.fileExists(testing.allocator, written_path));
}

test "approval callback can block an otherwise verified edit from being written" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const written_path = try std.fmt.allocPrint(testing.allocator, "{s}/handler.ts", .{tmp.sub_path});
    defer testing.allocator.free(written_path);

    var canned: CannedClient = .{ .reply = .{ .edit = .{
        .file = "handler.ts",
        .content = clean_handler,
        .before = null,
    } } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurnWith(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add a GET route",
        .{
            .workspace_root = tmp.sub_path[0..],
            .approval_fn = rejectEdit,
        },
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(Tag.tool_result, tr.at(1).tag);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "not applied") != null);
    try testing.expect(!file_io.fileExists(testing.allocator, written_path));
}

test "retry: clean first draft has attempt=1 (regression guard for happy path)" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var canned: CannedClient = .{ .reply = .{ .edit = .{
        .file = "handler.ts",
        .content = clean_handler,
        .before = null,
    } } };
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
        .{ .workspace_root = tmp.sub_path[0..] },
    );

    try testing.expectEqual(@as(u8, 1), result.attempt);
    try testing.expectEqual(Tag.proof_card, tr.at(1).tag);
}

test "edit path outside the workspace is rejected" {
    var canned: CannedClient = .{ .reply = .{ .edit = .{
        .file = "../outside-handler.ts",
        .content = clean_handler,
        .before = null,
    } } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    try testing.expectError(
        error.EditPathOutsideWorkspace,
        runTurn(testing.allocator, canned.asClient(), &registry, &tr, "escape the workspace"),
    );
}
