//! Non-interactive `--print` mode for `zigts expert`.
//!
//! Builds a session, runs exactly one turn through the agent, emits either
//! the rendered text or an NDJSON event stream, then returns. Bypasses the
//! REPL and TUI entirely.
//!
//! The approval-fn seam lives here (not in `loop`) so it can default to
//! `auto_reject` when no TTY is available to prompt on. Callers that want
//! their verified edits to land must pass `--yes` alongside `--print`.

const std = @import("std");

const agent = @import("agent.zig");
const loop = @import("loop.zig");
const registry_mod = @import("registry/registry.zig");
const transcript_mod = @import("transcript.zig");
const app = @import("app.zig");

const Registry = registry_mod.Registry;
const ExpertFlags = app.ExpertFlags;

/// Drive one non-interactive turn through the real session factory
/// (`agent.initFromEnvWithSessionConfig`). Emits to stdout.
pub fn run(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    flags: ExpertFlags,
    policy: loop.ApprovalPolicy,
) !void {
    var session = try agent.initFromEnvWithSessionConfig(allocator, registry, .{
        .no_session = flags.no_session,
        .no_persist_tool_output = flags.no_persist_tool_output,
        .session_id = flags.session_id,
        .resume_latest = flags.resume_latest,
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
    const approval_fn = selectApprovalFn(policy);
    const prompt = flags.print orelse return error.MissingPrintPrompt;

    var emitter: Emitter = .init(allocator, out_writer);

    // Emit the prompt up-front so JSON consumers always see it even if the
    // turn errors out before the user_text entry lands.
    if (flags.json_mode) try emitSimpleEvent(&emitter, "user_text", prompt);

    const start_len = session.transcript.len();

    const rendered = if (client_override) |client|
        try runOneTurnWithClient(allocator, session, registry, client, prompt, approval_fn)
    else
        try agent.runOneTurn(allocator, session, registry, prompt, approval_fn);
    defer allocator.free(rendered);

    if (!flags.json_mode) {
        try emitter.write(rendered);
        if (rendered.len == 0 or rendered[rendered.len - 1] != '\n') {
            try emitter.write("\n");
        }
        return;
    }

    const tr = &session.transcript;
    var idx: usize = start_len;
    while (idx < tr.len()) : (idx += 1) {
        const entry = tr.at(idx);
        // The prompt was already emitted; skip any user_text entry the
        // agent appended for this turn to avoid duplicates.
        if (entry.* == .user_text) continue;
        try emitEntryEvent(&emitter, entry);
    }

    try emitEndEvent(&emitter);
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

fn selectApprovalFn(policy: loop.ApprovalPolicy) loop.ApprovalFn {
    return switch (policy) {
        .auto_approve => loop.autoApprove,
        .auto_reject => loop.autoReject,
        // No TTY is available in --print mode. Warn once, then fall back
        // to the safer default so a stray `.ask` policy can't hang.
        .ask => blk: {
            const msg = "warning: --print has no TTY to prompt on; defaulting to auto_reject\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            break :blk loop.autoReject;
        },
    };
}

// ---------------------------------------------------------------------------
// Output sink: either an injected writer (for tests) or stdout via std.c.write.
// ---------------------------------------------------------------------------

const Emitter = struct {
    allocator: std.mem.Allocator,
    out: ?*std.Io.Writer,

    fn init(allocator: std.mem.Allocator, out: ?*std.Io.Writer) Emitter {
        return .{ .allocator = allocator, .out = out };
    }

    fn write(self: *Emitter, bytes: []const u8) !void {
        if (self.out) |w| {
            try w.writeAll(bytes);
        } else {
            _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
        }
    }

    /// Build one line into a scratch buffer, then flush it in one write.
    /// Takes a closure `writeFn(*std.Io.Writer, ctx)` that streams the
    /// line body (without the trailing newline).
    fn emitLine(
        self: *Emitter,
        ctx: anytype,
        comptime writeFn: fn (*std.Io.Writer, @TypeOf(ctx)) anyerror!void,
    ) !void {
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(self.allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(self.allocator, &buf);
        try writeFn(&aw.writer, ctx);
        try aw.writer.writeByte('\n');
        buf = aw.toArrayList();
        try self.write(buf.items);
    }
};

// ---------------------------------------------------------------------------
// NDJSON event shapes.
// ---------------------------------------------------------------------------

const SimpleEvent = struct { kind: []const u8, body: []const u8 };

fn emitSimpleEvent(emitter: *Emitter, kind: []const u8, body: []const u8) !void {
    try emitter.emitLine(
        SimpleEvent{ .kind = kind, .body = body },
        writeSimpleEvent,
    );
}

fn writeSimpleEvent(writer: *std.Io.Writer, ev: SimpleEvent) !void {
    var s: std.json.Stringify = .{ .writer = writer };
    try s.beginObject();
    try s.objectField("k");
    try s.write(ev.kind);
    try s.objectField("d");
    try s.write(ev.body);
    try s.endObject();
}

fn emitEndEvent(emitter: *Emitter) !void {
    try emitter.emitLine({}, writeEndEvent);
}

fn writeEndEvent(writer: *std.Io.Writer, _: void) !void {
    var s: std.json.Stringify = .{ .writer = writer };
    try s.beginObject();
    try s.objectField("k");
    try s.write("end");
    try s.endObject();
}

fn emitEntryEvent(emitter: *Emitter, entry: *const transcript_mod.OwnedEntry) !void {
    switch (entry.*) {
        .user_text => |body| try emitSimpleEvent(emitter, "user_text", body),
        .model_text => |body| try emitSimpleEvent(emitter, "assistant_text", body),
        .proof_card => |body| try emitter.emitLine(
            BoxEvent{ .kind = "proof_card", .body = body },
            writeBoxEvent,
        ),
        .diagnostic_box => |body| try emitter.emitLine(
            BoxEvent{ .kind = "diagnostic_box", .body = body },
            writeBoxEvent,
        ),
        .assistant_tool_use => |calls| {
            // One event per call to match the session persister's shape.
            for (calls) |call| {
                try emitter.emitLine(
                    ToolUseEvent{ .id = call.id, .name = call.name, .args = call.args_json },
                    writeToolUseEvent,
                );
            }
        },
        .tool_result => |tr| try emitter.emitLine(
            ToolResultEvent{ .id = tr.tool_use_id, .ok = tr.ok, .body = tr.body },
            writeToolResultEvent,
        ),
    }
}

const BoxEvent = struct { kind: []const u8, body: []const u8 };

fn writeBoxEvent(writer: *std.Io.Writer, ev: BoxEvent) !void {
    var s: std.json.Stringify = .{ .writer = writer };
    try s.beginObject();
    try s.objectField("k");
    try s.write(ev.kind);
    try s.objectField("d");
    try s.beginObject();
    try s.objectField("body");
    try s.write(ev.body);
    try s.endObject();
    try s.endObject();
}

const ToolUseEvent = struct { id: []const u8, name: []const u8, args: []const u8 };

fn writeToolUseEvent(writer: *std.Io.Writer, ev: ToolUseEvent) !void {
    var s: std.json.Stringify = .{ .writer = writer };
    try s.beginObject();
    try s.objectField("k");
    try s.write("tool_use");
    try s.objectField("d");
    try s.beginObject();
    try s.objectField("id");
    try s.write(ev.id);
    try s.objectField("name");
    try s.write(ev.name);
    try s.objectField("args");
    try s.write(ev.args);
    try s.endObject();
    try s.endObject();
}

const ToolResultEvent = struct { id: []const u8, ok: bool, body: []const u8 };

fn writeToolResultEvent(writer: *std.Io.Writer, ev: ToolResultEvent) !void {
    var s: std.json.Stringify = .{ .writer = writer };
    try s.beginObject();
    try s.objectField("k");
    try s.write("tool_result");
    try s.objectField("d");
    try s.beginObject();
    try s.objectField("id");
    try s.write(ev.id);
    try s.objectField("ok");
    try s.write(ev.ok);
    try s.objectField("body");
    try s.write(ev.body);
    try s.endObject();
    try s.endObject();
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
    ) anyerror!turn.AssistantReply {
        const self: *CannedClient = @ptrCast(@alignCast(ctx));
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
        return self.reply;
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

test "runWithClient: json mode emits user_text, assistant_text, end in order" {
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
    const asst_line = findLine(lines.items, "\"k\":\"assistant_text\"") orelse return error.TestFailed;
    const end_line = findLine(lines.items, "\"k\":\"end\"") orelse return error.TestFailed;
    try testing.expect(user_line < asst_line);
    try testing.expect(asst_line < end_line);
    try testing.expect(std.mem.indexOf(u8, lines.items[user_line], "hello") != null);
    try testing.expect(std.mem.indexOf(u8, lines.items[asst_line], "hi") != null);
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
