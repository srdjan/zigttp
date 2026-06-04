//! Non-interactive `--print` mode for `zigttp expert`.
//!
//! Runs exactly one turn through the agent, emits either the rendered text
//! or an NDJSON event stream, and returns. Bypasses the interactive REPL.
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

/// Streams transcript entries to the NDJSON sink as they are appended, so a
/// `--print --mode json` turn that later crashes or is killed still leaves its
/// events on stdout. The leading `user_text` is emitted before the turn, so the
/// observer skips it to avoid a duplicate.
const JsonStreamCtx = struct {
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,

    fn onAppend(context: *anyopaque, entry: *const transcript_mod.OwnedEntry) void {
        const self: *JsonStreamCtx = @ptrCast(@alignCast(context));
        if (entry.* == .user_text) return; // already emitted before the turn
        emitEntry(self.allocator, self.out, entry) catch {};
    }
};

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

    // In JSON mode, stream every transcript entry the moment it is appended,
    // via a Transcript observer, rather than replaying the transcript after the
    // turn returns. A turn that crashes or is killed mid-flight then still
    // leaves its events on stdout. The initial user_text is emitted above, so
    // the observer skips it. Cleared before return so it never outlives ctx.
    var stream_ctx = JsonStreamCtx{ .allocator = allocator, .out = out_writer };
    if (flags.json_mode) {
        session.transcript.observer = .{
            .context = &stream_ctx,
            .on_append = JsonStreamCtx.onAppend,
        };
    }
    defer session.transcript.observer = null;

    const turn_start_len = session.transcript.len();
    const turn_result = if (client_override) |client|
        runOneTurnWithClient(allocator, session, registry, client, prompt, approval_fn)
    else
        agent.runOneTurn(allocator, session, registry, prompt, approval_fn);
    const rendered = turn_result catch |err| {
        // --print is the non-interactive/CI entrypoint: emit a clean, parseable
        // error (and close the JSON stream) and exit non-zero, rather than
        // aborting the process with a Zig error-return trace.
        loop.writeTurnErrorToStderr(err);
        if (flags.json_mode) emitEndEvent(allocator, out_writer) catch {};
        std.process.exit(1);
    };
    defer allocator.free(rendered);

    if (!flags.json_mode) {
        // The last transcript entry after a verified edit is the proof card;
        // the legible "verified:" summary lives in the verified_patch entry
        // just before it. Surface that summary first so `--print` shows the
        // applied file and verdict, not just the proof card.
        try writeVerifiedSummary(allocator, out_writer, &session.transcript, turn_start_len);
        try writeOutLine(out_writer, rendered);
        return;
    }

    // Entries were streamed live by the observer; just close the stream.
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
        .{
            .approval_fn = approval_fn,
            .replay_mode = replay,
            .max_attempts = loop.interactive_max_attempts,
        },
    );
    const tr = &session.transcript;
    std.debug.assert(tr.len() >= 1);
    return transcript_mod.renderRichEntryToOwned(allocator, tr.at(tr.len() - 1));
}

/// In non-JSON `--print`, emit the legible verified-patch summary (the "verified:"
/// line plus stats) if the current turn applied an edit. Renders the most recent
/// current-turn verified_patch entry's `llm_text` summary; no-op when no edit was
/// applied this turn.
fn writeVerifiedSummary(
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,
    transcript: *const transcript_mod.Transcript,
    turn_start_len: usize,
) !void {
    const end = transcript.len();
    const start = if (turn_start_len > end) end else turn_start_len;
    var i = end;
    while (i > start) {
        i -= 1;
        const entry = transcript.at(i);
        if (entry.* == .verified_patch) {
            const rendered = try transcript_mod.renderRichEntryToOwned(allocator, entry);
            defer allocator.free(rendered);
            try writeOutLine(out, rendered);
            return;
        }
    }
}

fn writeOut(out: ?*std.Io.Writer, bytes: []const u8) !void {
    if (out) |w| {
        try w.writeAll(bytes);
    } else {
        _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
    }
}

/// Write `bytes`, then a trailing newline unless one is already present.
fn writeOutLine(out: ?*std.Io.Writer, bytes: []const u8) !void {
    try writeOut(out, bytes);
    if (bytes.len == 0 or bytes[bytes.len - 1] != '\n') {
        try writeOut(out, "\n");
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
        .proof_card => |body| try emitRecord(allocator, out, .{ .proof_card = .{
            .llm_text = body.llm_text,
            .ui_payload = body.ui_payload,
        } }),
        .diagnostic_box => |body| try emitRecord(allocator, out, .{ .diagnostic_box = .{
            .llm_text = body.llm_text,
            .ui_payload = body.ui_payload,
        } }),
        .verified_patch => |body| try emitRecord(allocator, out, .{ .verified_patch = .{
            .llm_text = body.llm_text,
            .ui_payload = body.ui_payload,
        } }),
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
            .llm_text = tr.llm_text,
            .ui_payload = tr.ui_payload,
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
    try testing.expect(std.mem.indexOf(u8, lines.items[user_line], "\"v\":2") != null);
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

test "writeVerifiedSummary ignores verified patches before current turn" {
    const allocator = testing.allocator;
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(allocator);

    try tr.entries.append(allocator, .{ .verified_patch = .{
        .llm_text = try allocator.dupe(u8, "verified: old.ts"),
        .ui_payload = null,
    } });

    const turn_start_len = tr.len();
    try tr.append(allocator, .{ .user_text = "resume diagnostics" });
    try tr.append(allocator, .{ .model_text = "no edit this turn" });

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try writeVerifiedSummary(allocator, &aw.writer, &tr, turn_start_len);
    buf = aw.toArrayList();

    try testing.expectEqual(@as(usize, 0), buf.items.len);
}

test "writeVerifiedSummary emits verified patch from current turn" {
    const allocator = testing.allocator;
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(allocator);

    try tr.entries.append(allocator, .{ .verified_patch = .{
        .llm_text = try allocator.dupe(u8, "verified: old.ts"),
        .ui_payload = null,
    } });

    const turn_start_len = tr.len();
    try tr.append(allocator, .{ .user_text = "apply edit" });
    try tr.entries.append(allocator, .{ .verified_patch = .{
        .llm_text = try allocator.dupe(u8, "verified: current.ts"),
        .ui_payload = null,
    } });

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try writeVerifiedSummary(allocator, &aw.writer, &tr, turn_start_len);
    buf = aw.toArrayList();

    try testing.expect(std.mem.indexOf(u8, buf.items, "current.ts") != null);
    try testing.expect(std.mem.indexOf(u8, buf.items, "old.ts") == null);
}
