//! Expert session wrapper around `loop.runTurn`. Carries a backend union
//! that is either a `StubClient` (default, zero-config, fixed reply) or an
//! Anthropic client built from an API key and a system prompt. Callers swap
//! backends at session construction; the rest of the loop is agnostic.
//!
//! The session owns a long-lived `Transcript` that grows across turns
//! plus, for the Anthropic backend, an allocator-owned copy of the system
//! prompt so the persona bytes outlive whatever buffer produced them.
//! `runOneTurn` drives one pass through the loop driver, then renders
//! the latest appended transcript entry to an owned `[]u8` the caller
//! frees. Rendering the whole transcript per turn would re-print
//! history on every submission.

const std = @import("std");
const loop = @import("loop.zig");
const turn = @import("turn.zig");
const transcript_mod = @import("transcript.zig");
const registry_mod = @import("registry/registry.zig");
const anthropic_client = @import("providers/anthropic/client.zig");
const tools_schema = @import("providers/anthropic/tools_schema.zig");
const expert_persona = @import("expert_persona.zig");
const context_loader = @import("context/loader.zig");
const session_id_mod = @import("session/session_id.zig");
const session_paths = @import("session/paths.zig");
const session_events = @import("session/events.zig");
const persister = @import("session/persister.zig");
const reconstructor = @import("session/reconstructor.zig");

const Registry = registry_mod.Registry;
const Transcript = transcript_mod.Transcript;

pub const SessionConfig = struct {
    no_session: bool = false,
    no_persist_tool_output: bool = false,
    /// Explicit session id. If both this and `resume_latest` are null/false,
    /// a fresh id is generated on init.
    session_id: ?[]const u8 = null,
    /// Load the newest session for this cwd. Mutually exclusive with
    /// `session_id`.
    resume_latest: bool = false,
};

pub const stub_reply_text =
    "expert offline: no live model backend configured; set ANTHROPIC_API_KEY to enable expert mode";

/// Zero-state client used when no Anthropic credentials are present.
/// Returns a fixed reply regardless of the prompt so the loop still
/// exercises every path from keyboard to transcript.
pub const StubClient = struct {
    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!turn.AssistantReply {
        _ = ctx;
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
        return .{ .response = .{ .final_text = stub_reply_text } };
    }

    pub fn asClient(self: *StubClient) loop.ModelClient {
        return .{
            .context = self,
            .request_fn = requestFn,
        };
    }
};

pub const Backend = union(enum) {
    stub: StubClient,
    anthropic: anthropic_client.Client,
};

pub const AgentSession = struct {
    transcript: Transcript = .{},
    backend: Backend = .{ .stub = .{} },
    /// Allocator-owned copy of the system prompt bytes backing the
    /// Anthropic client's Config. Null for the stub path.
    system_prompt_owned: ?[]u8 = null,
    tools_json_owned: ?[]u8 = null,

    session_id: ?[]u8 = null,
    session_dir: ?[]u8 = null,
    events_path: ?[]u8 = null,
    meta_path: ?[]u8 = null,
    persist_opts: persister.AppendOptions = .{},
    /// /resume sets this; the first `runOneTurn` after resume passes
    /// `replay_mode = true` to the loop and then clears the flag.
    replay_next_turn: bool = false,
    /// Cursor into `transcript.entries`. Entries from this index forward
    /// are the ones that need to be persisted next.
    last_persisted_len: usize = 0,

    pub fn initStub() AgentSession {
        return .{};
    }

    /// Constructs a session whose backend is a real Anthropic client.
    /// Dupes the system prompt so the caller's buffer can be freed
    /// independently. Dupes the API key for the same reason.
    pub fn initAnthropic(
        allocator: std.mem.Allocator,
        api_key: []const u8,
        system_prompt: []const u8,
        tools_json: ?[]const u8,
    ) !AgentSession {
        const prompt_owned = try allocator.dupe(u8, system_prompt);
        errdefer allocator.free(prompt_owned);
        const key_owned = try allocator.dupe(u8, api_key);
        errdefer allocator.free(key_owned);
        const tools_owned = if (tools_json) |json|
            try allocator.dupe(u8, json)
        else
            null;
        errdefer if (tools_owned) |json| allocator.free(json);
        return .{
            .backend = .{ .anthropic = anthropic_client.Client.init(.{
                .api_key = key_owned,
                .system_prompt = prompt_owned,
                .tools_json = tools_owned,
            }) },
            .system_prompt_owned = prompt_owned,
            .tools_json_owned = tools_owned,
        };
    }

    pub fn deinit(self: *AgentSession, allocator: std.mem.Allocator) void {
        self.transcript.deinit(allocator);
        if (self.system_prompt_owned) |s| allocator.free(s);
        if (self.tools_json_owned) |json| allocator.free(json);
        if (self.session_id) |s| allocator.free(s);
        if (self.session_dir) |s| allocator.free(s);
        if (self.events_path) |s| allocator.free(s);
        if (self.meta_path) |s| allocator.free(s);
        switch (self.backend) {
            .stub => {},
            .anthropic => |*c| allocator.free(c.config.api_key),
        }
    }

    pub fn modelClient(self: *AgentSession) loop.ModelClient {
        return switch (self.backend) {
            .stub => (&self.backend.stub).asClient(),
            .anthropic => (&self.backend.anthropic).asModelClient(),
        };
    }
};

/// Build a session from the environment (`ANTHROPIC_API_KEY` -> anthropic,
/// else stub) and, unless `config.no_session` is true, materialize the
/// on-disk session directory, write `meta.json` + `workspace.txt`, and
/// wire up `events.jsonl` persistence.
///
/// When `config.resume_latest` is set, the newest existing session for
/// this cwd is loaded and the transcript is replaced with a reconstruction
/// of its events log; the first subsequent turn runs in replay mode.
pub fn initFromEnvWithSessionConfig(
    allocator: std.mem.Allocator,
    registry: ?*const Registry,
    config: SessionConfig,
) !AgentSession {
    std.debug.assert(!(config.resume_latest and config.session_id != null));

    var session = blk: {
        const api_key = envVar("ANTHROPIC_API_KEY") orelse break :blk AgentSession.initStub();
        const ctx = try context_loader.loadProjectContext(allocator);
        defer if (ctx) |b| allocator.free(b);
        const system_prompt = try expert_persona.buildSystemPromptWithContext(allocator, ctx);
        defer allocator.free(system_prompt);
        const tools_json = if (registry) |reg|
            try buildToolsJson(allocator, reg)
        else
            null;
        defer if (tools_json) |json| allocator.free(json);
        break :blk try AgentSession.initAnthropic(allocator, api_key, system_prompt, tools_json);
    };
    errdefer session.deinit(allocator);

    if (config.no_session) return session;

    session.persist_opts = .{ .no_persist_tool_output = config.no_persist_tool_output };

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const realpath = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io, ".", allocator);
    defer allocator.free(realpath);

    var resumed = false;
    const sid: []u8 = pick: {
        if (config.session_id) |id| break :pick try allocator.dupe(u8, id);
        if (config.resume_latest) {
            const root = try session_paths.sessionRoot(allocator);
            defer allocator.free(root);
            const hash = try session_paths.cwdHashFull(allocator);
            const entries = try session_paths.listSessions(allocator, root, hash[0..]);
            defer {
                for (entries) |*e| e.deinit(allocator);
                allocator.free(entries);
            }
            if (entries.len > 0) {
                resumed = true;
                break :pick try allocator.dupe(u8, entries[0].session_id);
            }
        }
        break :pick try session_id_mod.generate(allocator);
    };
    session.session_id = sid;

    const dir = try session_paths.sessionDir(allocator, sid);
    session.session_dir = dir;
    try session_paths.writeWorkspacePointer(allocator, dir, realpath);

    session.events_path = try std.fs.path.join(allocator, &.{ dir, "events.jsonl" });
    session.meta_path = try std.fs.path.join(allocator, &.{ dir, "meta.json" });

    if (resumed) {
        var tr = try reconstructor.reconstructTranscript(allocator, session.events_path.?, null);
        session.transcript.deinit(allocator);
        session.transcript = tr;
        session.last_persisted_len = tr.len();
        session.replay_next_turn = true;
    } else {
        try session_events.writeMeta(allocator, session.meta_path.?, .{
            .session_id = sid,
            .workspace_realpath = realpath,
            .created_at_unix_ms = nowUnixMs(),
        });
    }

    return session;
}

fn envVar(name_z: [:0]const u8) ?[]const u8 {
    const raw = std.c.getenv(name_z) orelse return null;
    return std.mem.sliceTo(raw, 0);
}

fn nowUnixMs() i64 {
    var ts: std.posix.timespec = undefined;
    _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
    return @as(i64, ts.sec) * 1000 + @divTrunc(@as(i64, ts.nsec), 1_000_000);
}

fn buildToolsJson(allocator: std.mem.Allocator, registry: *const Registry) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try tools_schema.writeToolsArray(&aw.writer, registry);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

/// Runs one turn through the loop driver and returns an owned slice holding
/// the rendered plain-text form of the message the turn appended. Caller
/// frees with `allocator.free`.
///
/// Invariant: every turn appends at least the user_text entry, so the
/// transcript length is guaranteed to grow by at least one.
pub fn runOneTurn(
    allocator: std.mem.Allocator,
    session: *AgentSession,
    registry: *const Registry,
    user_text: []const u8,
    approval_fn: ?loop.ApprovalFn,
) ![]u8 {
    const replay = session.replay_next_turn;
    session.replay_next_turn = false;

    _ = try loop.runTurnWith(
        allocator,
        session.modelClient(),
        registry,
        &session.transcript,
        user_text,
        .{ .approval_fn = approval_fn, .replay_mode = replay },
    );
    const tr = &session.transcript;
    std.debug.assert(tr.len() >= 1);

    if (session.events_path) |path| {
        const entries = tr.entries.items;
        while (session.last_persisted_len < entries.len) : (session.last_persisted_len += 1) {
            try persister.appendEntry(
                allocator,
                path,
                &entries[session.last_persisted_len],
                session.persist_opts,
            );
        }
    }

    return transcript_mod.renderRichEntryToOwned(allocator, tr.at(tr.len() - 1));
}

/// Tear down `session` and rebuild it in place from the same environment,
/// carrying forward `no_session` / `no_persist_tool_output` and honoring
/// `resume_latest`. Used by `/resume` (pass true) and `/new` (pass false).
pub fn rebuildSession(
    allocator: std.mem.Allocator,
    session: *AgentSession,
    registry: *const Registry,
    no_session: bool,
    no_persist_tool_output: bool,
    resume_latest: bool,
) !void {
    session.deinit(allocator);
    session.* = try initFromEnvWithSessionConfig(allocator, registry, .{
        .no_session = no_session,
        .no_persist_tool_output = no_persist_tool_output,
        .resume_latest = resume_latest,
    });
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "runOneTurn: fresh stub session grows transcript by 2 and renders model reply" {
    var session = AgentSession.initStub();
    defer session.deinit(testing.allocator);
    var registry: Registry = .{};
    defer registry.deinit(testing.allocator);

    const rendered = try runOneTurn(testing.allocator, &session, &registry, "add a GET route", null);
    defer testing.allocator.free(rendered);

    try testing.expectEqual(@as(usize, 2), session.transcript.len());
    switch (session.transcript.at(0).*) {
        .user_text => |body| try testing.expectEqualStrings("add a GET route", body),
        else => return error.TestFailed,
    }
    switch (session.transcript.at(1).*) {
        .model_text => |body| try testing.expectEqualStrings(stub_reply_text, body),
        else => return error.TestFailed,
    }
    try testing.expect(std.mem.startsWith(u8, rendered, "model: "));
    try testing.expect(std.mem.indexOf(u8, rendered, stub_reply_text) != null);
    try testing.expect(rendered[rendered.len - 1] == '\n');
}

test "runOneTurn: two turns back-to-back accumulate in the transcript" {
    var session = AgentSession.initStub();
    defer session.deinit(testing.allocator);
    var registry: Registry = .{};
    defer registry.deinit(testing.allocator);

    const first = try runOneTurn(testing.allocator, &session, &registry, "first intent", null);
    defer testing.allocator.free(first);
    const second = try runOneTurn(testing.allocator, &session, &registry, "second intent", null);
    defer testing.allocator.free(second);

    try testing.expectEqual(@as(usize, 4), session.transcript.len());
    switch (session.transcript.at(0).*) {
        .user_text => |body| try testing.expectEqualStrings("first intent", body),
        else => return error.TestFailed,
    }
    switch (session.transcript.at(2).*) {
        .user_text => |body| try testing.expectEqualStrings("second intent", body),
        else => return error.TestFailed,
    }
    // The second render is the second turn's model reply only, not a
    // cumulative dump of the whole transcript.
    try testing.expect(std.mem.indexOf(u8, second, "first intent") == null);
    try testing.expect(std.mem.indexOf(u8, second, stub_reply_text) != null);
}

test "StubClient ignores user_text and always returns the stub reply" {
    var stub: StubClient = .{};
    const client = stub.asClient();
    var transcript: Transcript = .{};
    defer transcript.deinit(testing.allocator);
    const reply = try client.request(testing.allocator, &transcript, "whatever the user typed");
    switch (reply.response) {
        .final_text => |t| try testing.expectEqualStrings(stub_reply_text, t),
        else => return error.TestFailed,
    }
}

test "initAnthropic dupes api_key and system_prompt, deinit releases both" {
    var session = try AgentSession.initAnthropic(
        testing.allocator,
        "sk-ant-test",
        "you are a zigts expert",
        "[{\"name\":\"zigts_expert_meta\",\"description\":\"d\",\"input_schema\":{}}]",
    );
    defer session.deinit(testing.allocator);

    try testing.expect(session.backend == .anthropic);
    try testing.expectEqualStrings("sk-ant-test", session.backend.anthropic.config.api_key);
    try testing.expectEqualStrings("you are a zigts expert", session.backend.anthropic.config.system_prompt);
    try testing.expect(session.backend.anthropic.config.tools_json != null);
    try testing.expect(session.system_prompt_owned != null);
}

test "modelClient returns an anthropic client vtable when backend is anthropic" {
    var session = try AgentSession.initAnthropic(testing.allocator, "k", "p", null);
    defer session.deinit(testing.allocator);

    const mc = session.modelClient();
    try testing.expect(mc.context == @as(*anyopaque, @ptrCast(&session.backend.anthropic)));
}
