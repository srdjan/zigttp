//! Non-interactive `--print` mode for `zigts expert`.
//!
//! Runs exactly one turn through the agent, emits either the rendered text
//! or an NDJSON event stream, and returns. Bypasses the REPL and TUI.
//!
//! JSON mode reuses the `{"v","k","d"}` envelope from `session/events.zig`
//! for transcript events. Live stdout also appends a success-only `end`
//! sentinel that is not written to persisted `events.jsonl`.

const std = @import("std");

const agent = @import("agent.zig");
const loop = @import("loop.zig");
const registry_mod = @import("registry/registry.zig");
const transcript_mod = @import("transcript.zig");
const session_events = @import("session/events.zig");
const app = @import("app.zig");

const Registry = registry_mod.Registry;
const ExpertFlags = app.ExpertFlags;
const EventRecord = session_events.EventRecord;

pub fn run(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    flags: ExpertFlags,
    policy: loop.ApprovalPolicy,
) !void {
    var session = try agent.initFromEnvWithSessionConfig(allocator, registry, .{
        .no_session = flags.no_session,
        .no_persist_tool_output = flags.no_persist_tool_output,
        .no_context_files = flags.no_context_files,
        .session_id = flags.session_id,
        .resume_latest = flags.resume_latest,
        .fork_session_id = flags.fork_session_id,
    });
    defer session.deinit(allocator);

    try runWithSession(allocator, &session, registry, flags, policy, null, null);
}

/// Test-facing variant that accepts an explicit `ModelClient` plus an
/// optional writer. When `out_writer` is null, events go to stdout.
pub fn runWithClient(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    client: loop.ModelClient,
    flags: ExpertFlags,
    policy: loop.ApprovalPolicy,
    out_writer: ?*std.Io.Writer,
) !void {
    var session = agent.AgentSession.initStub();
    defer session.deinit(allocator);

    try runWithSession(allocator, &session, registry, flags, policy, client, out_writer);
}

fn runWithSession(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    registry: *const Registry,
    flags: ExpertFlags,
    policy: loop.ApprovalPolicy,
    client_override: ?loop.ModelClient,
    out_writer: ?*std.Io.Writer,
) !void {
    const approval_fn = loop.resolveApprovalFn(policy, null);
    const prompt = flags.print orelse return error.MissingPrintPrompt;

    // Emit the prompt first so JSON consumers see it even if the turn errors
    // before the user_text entry lands.
    if (flags.json_mode) try emitRecord(allocator, out_writer, .{ .user_text = prompt });

    const start_len = session.transcript.len();

    const rendered = if (client_override) |client|
        try runOneTurnWithClient(allocator, session, registry, client, prompt, approval_fn)
    else
        try agent.runOneTurn(allocator, session, registry, prompt, approval_fn);
    defer allocator.free(rendered);

    if (!flags.json_mode) {
        try writeOut(out_writer, rendered);
        if (rendered.len == 0 or rendered[rendered.len - 1] != '\n') {
            try writeOut(out_writer, "\n");
        }
        return;
    }

    const tr = &session.transcript;
    var idx: usize = start_len;
    while (idx < tr.len()) : (idx += 1) {
        const entry = tr.at(idx);
        if (entry.* == .user_text) continue; // already emitted above
        try emitEntry(allocator, out_writer, entry);
    }

    try emitEndEvent(allocator, out_writer);
}

fn runOneTurnWithClient(
    allocator: std.mem.Allocator,
    session: *agent.AgentSession,
    registry: *const Registry,
    client: loop.ModelClient,
    user_text: []const u8,
    approval_fn: ?loop.ApprovalFn,
) ![]u8 {
    const replay = session.replay_next_turn;
    session.replay_next_turn = false;

    _ = try loop.runTurnWith(
        allocator,
        client,
        registry,
        &session.transcript,
        user_text,
        .{ .approval_fn = approval_fn, .replay_mode = replay },
    );
    const tr = &session.transcript;
    std.debug.assert(tr.len() >= 1);
    return transcript_mod.renderRichEntryToOwned(allocator, tr.at(tr.len() - 1));
}

fn writeOut(out: ?*std.Io.Writer, bytes: []const u8) !void {
    if (out) |w| {
        try w.writeAll(bytes);
    } else {
        _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
    }
}

fn emitRecord(allocator: std.mem.Allocator, out: ?*std.Io.Writer, record: EventRecord) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try session_events.writeEventLine(&aw.writer, record);
    buf = aw.toArrayList();
    try writeOut(out, buf.items);
}

fn emitEntry(allocator: std.mem.Allocator, out: ?*std.Io.Writer, entry: *const transcript_mod.OwnedEntry) !void {
    switch (entry.*) {
        .user_text => |body| try emitRecord(allocator, out, .{ .user_text = body }),
        .model_text => |body| try emitRecord(allocator, out, .{ .model_text = body }),
        .proof_card => |body| try emitRecord(allocator, out, .{ .proof_card = body }),
        .diagnostic_box => |body| try emitRecord(allocator, out, .{ .diagnostic_box = body }),
        .assistant_tool_use => |calls| {
            for (calls) |call| {
                try emitRecord(allocator, out, .{ .tool_use = .{
                    .id = call.id,
                    .name = call.name,
                    .args_json = call.args_json,
                } });
            }
        },
        .tool_result => |tr| try emitRecord(allocator, out, .{ .tool_result = .{
            .tool_use_id = tr.tool_use_id,
            .tool_name = tr.tool_name,
            .ok = tr.ok,
            .body = tr.body,
        } }),
        .system_note => {},
    }
}

fn emitEndEvent(allocator: std.mem.Allocator, out: ?*std.Io.Writer) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    var s: std.json.Stringify = .{ .writer = &aw.writer };
    try s.beginObject();
    try s.objectField("v");
    try s.write(session_events.schema_version);
    try s.objectField("k");
    try s.write("end");
    try s.endObject();
    try aw.writer.writeByte('\n');
    buf = aw.toArrayList();
    try writeOut(out, buf.items);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;
const turn = @import("turn.zig");

const CannedClient = struct {
    reply: turn.AssistantReply,

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!loop.ModelCallResult {
        const self: *CannedClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
        return .{ .reply = self.reply };
    }

    fn asModelClient(self: *CannedClient) loop.ModelClient {
        return .{ .context = self, .request_fn = requestFn };
    }
};

const meta_tool = @import("tools/zigts_expert_meta.zig");

fn buildMiniRegistry(allocator: std.mem.Allocator) !Registry {
    var reg: Registry = .{};
    errdefer reg.deinit(allocator);
    try reg.register(allocator, meta_tool.tool);
    return reg;
}

fn findLine(lines: []const []const u8, needle: []const u8) ?usize {
    for (lines, 0..) |line, i| {
        if (std.mem.indexOf(u8, line, needle) != null) return i;
    }
    return null;
}

test "runWithClient: json mode emits user_text, model_text, end in order" {
    const allocator = testing.allocator;
    var reg = try buildMiniRegistry(allocator);
    defer reg.deinit(allocator);

    var client: CannedClient = .{ .reply = .{
        .response = .{ .final_text = "hi" },
    } };

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try runWithClient(
        allocator,
        &reg,
        client.asModelClient(),
        .{ .print = "hello", .json_mode = true, .no_session = true },
        .auto_reject,
        &aw.writer,
    );

    buf = aw.toArrayList();

    var lines: std.ArrayList([]const u8) = .empty;
    defer lines.deinit(allocator);
    var it = std.mem.splitScalar(u8, buf.items, '\n');
    while (it.next()) |line| {
        if (line.len == 0) continue;
        try lines.append(allocator, line);
    }
    try testing.expect(lines.items.len >= 3);

    const user_line = findLine(lines.items, "\"k\":\"user_text\"") orelse return error.TestFailed;
    const model_line = findLine(lines.items, "\"k\":\"model_text\"") orelse return error.TestFailed;
    const end_line = findLine(lines.items, "\"k\":\"end\"") orelse return error.TestFailed;
    try testing.expect(user_line < model_line);
    try testing.expect(model_line < end_line);
    try testing.expect(std.mem.indexOf(u8, lines.items[user_line], "hello") != null);
    try testing.expect(std.mem.indexOf(u8, lines.items[model_line], "hi") != null);
    try testing.expect(std.mem.indexOf(u8, lines.items[user_line], "\"v\":1") != null);
}

test "runWithClient: non-json mode writes rendered text" {
    const allocator = testing.allocator;
    var reg = try buildMiniRegistry(allocator);
    defer reg.deinit(allocator);

    var client: CannedClient = .{ .reply = .{
        .response = .{ .final_text = "hi there" },
    } };

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try runWithClient(
        allocator,
        &reg,
        client.asModelClient(),
        .{ .print = "hello", .json_mode = false, .no_session = true },
        .auto_reject,
        &aw.writer,
    );

    buf = aw.toArrayList();
    try testing.expect(std.mem.indexOf(u8, buf.items, "hi there") != null);
    try testing.expect(buf.items[buf.items.len - 1] == '\n');
}
