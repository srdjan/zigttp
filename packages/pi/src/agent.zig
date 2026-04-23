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
const zigts_cli = @import("zigts_cli");
const expert_meta = zigts_cli.expert_meta;
const session_id_mod = @import("session/session_id.zig");
const session_paths = @import("session/paths.zig");
const session_events = @import("session/events.zig");
const persister = @import("session/persister.zig");
const reconstructor = @import("session/reconstructor.zig");
const project_context = @import("context/project_context.zig");
const theme_mod = @import("tui/theme.zig");

const Registry = registry_mod.Registry;
const Transcript = transcript_mod.Transcript;

pub const AuthKind = enum {
    stub,
    anthropic_api_key,
};

pub const BackendDescriptor = struct {
    auth_label: []const u8,
    provider_label: []const u8,
};

pub const SessionConfig = struct {
    no_session: bool = false,
    no_persist_tool_output: bool = false,
    /// Skip AGENTS.md / CLAUDE.md project-context loading. Persona is
    /// unchanged; only the appended read-only project-context section is
    /// suppressed.
    no_context_files: bool = false,
    /// Explicit session id. If both this and `resume_latest` are null/false,
    /// a fresh id is generated on init.
    session_id: ?[]const u8 = null,
    /// Load the newest session for this cwd. Mutually exclusive with
    /// `session_id`.
    resume_latest: bool = false,
    /// Fork from the given session id: copy its transcript into a new session
    /// with `parent_id` pointing to the source. Mutually exclusive with
    /// `resume_latest` and `session_id`.
    fork_session_id: ?[]const u8 = null,
};

const stub_reply_text =
    "expert offline: no live model backend configured; set ANTHROPIC_API_KEY to enable expert mode";

/// Zero-state client used when no Anthropic credentials are present.
/// Returns a fixed reply regardless of the prompt so the loop still
/// exercises every path from keyboard to transcript.
const StubClient = struct {
    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!loop.ModelCallResult {
        _ = ctx;
        _ = arena;
        _ = transcript;
        _ = extra_user_text;
        return .{ .reply = .{ .response = .{ .final_text = stub_reply_text } } };
    }

    fn asClient(self: *StubClient) loop.ModelClient {
        return .{
            .context = self,
            .request_fn = requestFn,
        };
    }
};

const Backend = union(enum) {
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
    /// Running total of tokens consumed by all turns in this session.
    token_totals: turn.Usage = .{},
    /// TUI theme in use for this session. Defaults to the built-in "default"
    /// palette; `/settings theme <name>` swaps the pointer.
    theme: *const theme_mod.Theme = &theme_mod.default,

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

    /// Returns the model id currently in use, or null for the stub backend.
    pub fn currentModel(self: *const AgentSession) ?[]const u8 {
        return switch (self.backend) {
            .stub => null,
            .anthropic => |c| c.config.model,
        };
    }

    pub fn authKind(self: *const AgentSession) AuthKind {
        return switch (self.backend) {
            .stub => .stub,
            .anthropic => .anthropic_api_key,
        };
    }

    /// Single source of truth for the backend's display labels. One switch
    /// on `self.backend` replaces what used to be three: authKind, then
    /// authLabel (re-dispatching on authKind), then providerLabel.
    pub fn backendDescriptor(self: *const AgentSession) BackendDescriptor {
        return switch (self.backend) {
            .stub => .{ .auth_label = "stub", .provider_label = "stub" },
            .anthropic => .{ .auth_label = "api-key", .provider_label = "anthropic" },
        };
    }

    /// Switches the Anthropic backend's model. `model_id` must outlive the
    /// session (use a compile-time const or an allocator-owned dupe). No-op
    /// for the stub backend.
    pub fn setModel(self: *AgentSession, model_id: []const u8) void {
        switch (self.backend) {
            .anthropic => |*c| c.config.model = model_id,
            .stub => {},
        }
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
    std.debug.assert(!(config.fork_session_id != null and config.resume_latest));
    std.debug.assert(!(config.fork_session_id != null and config.session_id != null));

    // Load project context (AGENTS.md / CLAUDE.md) from cwd upward unless
    // the caller disabled it. Best-effort: a load failure is logged as a
    // silent skip so a broken file does not make the agent unusable.
    const project_ctx: ?[]u8 = if (config.no_context_files)
        null
    else
        project_context.loadFromCwd(allocator) catch null;
    defer if (project_ctx) |p| allocator.free(p);

    var session = blk: {
        const api_key = envVar("ANTHROPIC_API_KEY") orelse break :blk AgentSession.initStub();
        const system_prompt = try expert_persona.buildSystemPromptWithContext(allocator, project_ctx);
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

    const current_hash_bytes = expert_meta.compute().policy_hash;
    const current_hash = current_hash_bytes[0..];

    if (resumed) {
        var tr = try reconstructor.reconstructTranscript(allocator, session.events_path.?, null);
        session.transcript.deinit(allocator);
        session.transcript = tr;
        session.last_persisted_len = tr.len();
        session.replay_next_turn = true;

        // Detect policy drift: if the resumed session's meta.json stamps a
        // different hash than the current binary, prepend a system_note to
        // the transcript so both the model and the user see the mismatch.
        try injectDriftNote(allocator, &session, current_hash);
    } else if (config.fork_session_id) |fork_id| {
        const src_dir = try session_paths.sessionDir(allocator, fork_id);
        defer allocator.free(src_dir);
        const src_events = try std.fs.path.join(allocator, &.{ src_dir, "events.jsonl" });
        defer allocator.free(src_events);
        const tr = try reconstructor.reconstructTranscript(allocator, src_events, null);
        session.transcript.deinit(allocator);
        session.transcript = tr;
        // Re-persist forked transcript to the new session's events.jsonl.
        for (session.transcript.entries.items) |*entry| {
            try persister.appendEntry(allocator, session.events_path.?, entry, session.persist_opts);
        }
        session.last_persisted_len = session.transcript.len();
        try session_events.writeMeta(allocator, session.meta_path.?, .{
            .session_id = sid,
            .workspace_realpath = realpath,
            .created_at_unix_ms = nowUnixMs(),
            .parent_id = fork_id,
            .policy_hash = current_hash,
        });
    } else {
        try session_events.writeMeta(allocator, session.meta_path.?, .{
            .session_id = sid,
            .workspace_realpath = realpath,
            .created_at_unix_ms = nowUnixMs(),
            .policy_hash = current_hash,
        });
    }

    return session;
}

/// Compare the resumed session's stored policy_hash against the current
/// binary's hash. On mismatch (or on a pre-Phase-2 session with no stamped
/// hash), append a `system_note` to the transcript so the model is aware the
/// reasoning in prior turns was produced under a different rule set.
const POLICY_DRIFT_PREFIX = "[policy drift]";

fn injectDriftNote(
    allocator: std.mem.Allocator,
    session: *AgentSession,
    current_hash: []const u8,
) !void {
    const meta_path = session.meta_path orelse return;
    // An unreadable meta.json is surfaced as a silent skip: the session can
    // still run; the next successful write rebuilds the file.
    var meta = session_events.readMeta(allocator, meta_path) catch return;
    defer session_events.freeMeta(allocator, &meta);

    // Pre-Phase-2 sessions (no saved hash) and matching hashes both skip the
    // note; only the drift case appends + persists a system_note. All three
    // cases fall through to a single forward-stamp at the bottom so the next
    // resume has an accurate baseline.
    if (meta.policy_hash) |saved| {
        if (std.mem.eql(u8, saved, current_hash)) return;

        const note = try std.fmt.allocPrint(
            allocator,
            "{s} Resumed session was created under policy_hash {s} but the current binary is {s}. Prior rule citations in this transcript may be stale against today's compiler policy.\n",
            .{ POLICY_DRIFT_PREFIX, saved, current_hash },
        );
        errdefer allocator.free(note);
        try session.transcript.entries.append(allocator, .{ .system_note = note });

        if (session.events_path) |path| {
            try persister.appendEntry(
                allocator,
                path,
                &session.transcript.entries.items[session.transcript.entries.items.len - 1],
                session.persist_opts,
            );
            session.last_persisted_len = session.transcript.len();
        }
    }

    try session_events.writeMeta(allocator, meta_path, .{
        .session_id = meta.session_id,
        .workspace_realpath = meta.workspace_realpath,
        .created_at_unix_ms = meta.created_at_unix_ms,
        .parent_id = meta.parent_id,
        .policy_hash = current_hash,
    });
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

    const turn_result = try loop.runTurnWith(
        allocator,
        session.modelClient(),
        registry,
        &session.transcript,
        user_text,
        .{ .approval_fn = approval_fn, .replay_mode = replay },
    );
    session.token_totals.add(turn_result.usage);
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

/// Compact the session transcript: render all existing entries to plain text,
/// replace them with a single `system_note` carrying that rendered history,
/// and reset the persistence cursor so the note is persisted next time.
/// The model will see the compacted history as a user-role context block.
pub fn compact(
    allocator: std.mem.Allocator,
    session: *AgentSession,
) ![]u8 {
    const tr = &session.transcript;
    if (tr.len() == 0) {
        return allocator.dupe(u8, "Nothing to compact.\n");
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try aw.writer.writeAll("[COMPACTED CONVERSATION HISTORY]\n");
    for (tr.entries.items) |*entry| {
        try transcript_mod.renderPlain(&aw.writer, entry);
    }
    buf = aw.toArrayList();
    const note = try buf.toOwnedSlice(allocator);
    errdefer allocator.free(note);

    const entry_count = tr.len();
    for (tr.entries.items) |*entry| entry.deinit(allocator);
    tr.entries.clearAndFree(allocator);
    try tr.entries.append(allocator, .{ .system_note = note });
    session.last_persisted_len = 0;

    return std.fmt.allocPrint(
        allocator,
        "Compacted {d} entries into a single context note.\n",
        .{entry_count},
    );
}

/// Branch the current session: create a new session directory, copy the
/// current transcript's persisted events to it, write a meta.json with
/// `parent_id` pointing at the current session, then update the session's
/// on-disk handles to the new location. The in-memory transcript is unchanged.
/// Returns an owned summary message; caller frees.
pub fn fork(
    allocator: std.mem.Allocator,
    session: *AgentSession,
) ![]u8 {
    const old_sid = session.session_id orelse {
        return allocator.dupe(u8, "Session is ephemeral (--no-session); nothing to fork.\n");
    };

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const realpath = try std.Io.Dir.realPathFileAlloc(std.Io.Dir.cwd(), io, ".", allocator);
    defer allocator.free(realpath);

    const new_sid = try session_id_mod.generate(allocator);
    const new_dir = try session_paths.sessionDir(allocator, new_sid);
    errdefer allocator.free(new_dir);
    try session_paths.writeWorkspacePointer(allocator, new_dir, realpath);

    const new_events_path = try std.fs.path.join(allocator, &.{ new_dir, "events.jsonl" });
    errdefer allocator.free(new_events_path);
    const new_meta_path = try std.fs.path.join(allocator, &.{ new_dir, "meta.json" });
    errdefer allocator.free(new_meta_path);

    for (session.transcript.entries.items) |*entry| {
        try persister.appendEntry(allocator, new_events_path, entry, session.persist_opts);
    }

    try session_events.writeMeta(allocator, new_meta_path, .{
        .session_id = new_sid,
        .workspace_realpath = realpath,
        .created_at_unix_ms = nowUnixMs(),
        .parent_id = old_sid,
    });

    if (session.session_id) |s| allocator.free(s);
    if (session.session_dir) |s| allocator.free(s);
    if (session.events_path) |s| allocator.free(s);
    if (session.meta_path) |s| allocator.free(s);

    session.session_id = new_sid;
    session.session_dir = new_dir;
    session.events_path = new_events_path;
    session.meta_path = new_meta_path;
    session.last_persisted_len = session.transcript.len();

    return std.fmt.allocPrint(
        allocator,
        "Forked to new session: {s}\nParent: {s}\n",
        .{ new_sid, old_sid },
    );
}

/// Tear down `session` and rebuild it in place from the same environment.
/// Used by `/resume` and `/new`.
pub fn rebuildSession(
    allocator: std.mem.Allocator,
    session: *AgentSession,
    registry: *const Registry,
    config: SessionConfig,
) !void {
    session.deinit(allocator);
    session.* = try initFromEnvWithSessionConfig(allocator, registry, config);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

const IsolatedTmp = @import("test_support/tmp.zig").IsolatedTmp;
const EnvOverride = @import("test_support/env.zig").EnvOverride;
const cwdPathAlloc = @import("test_support/cwd.zig").cwdPathAlloc;

fn initTmp(allocator: std.mem.Allocator) !IsolatedTmp {
    return IsolatedTmp.init(allocator, "agent");
}

fn writeTestFile(
    allocator: std.mem.Allocator,
    root_abs: []const u8,
    sub_path: []const u8,
    data: []const u8,
) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    var root = try std.Io.Dir.openDirAbsolute(io, root_abs, .{});
    defer root.close(io);
    if (std.fs.path.dirname(sub_path)) |parent| {
        try std.Io.Dir.createDirPath(root, io, parent);
    }
    try root.writeFile(io, .{ .sub_path = sub_path, .data = data });
}

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
    const result = try client.request(testing.allocator, &transcript, "whatever the user typed");
    switch (result.reply.response) {
        .final_text => |t| try testing.expectEqualStrings(stub_reply_text, t),
        else => return error.TestFailed,
    }
}

test "initAnthropic dupes api_key and system_prompt, deinit releases both" {
    var session = try AgentSession.initAnthropic(
        testing.allocator,
        "test-fixture-key",
        "you are a zigts expert",
        "[{\"name\":\"zigts_expert_meta\",\"description\":\"d\",\"input_schema\":{}}]",
    );
    defer session.deinit(testing.allocator);

    try testing.expect(session.backend == .anthropic);
    try testing.expectEqualStrings("test-fixture-key", session.backend.anthropic.config.api_key);
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

test "compact: empty transcript returns early message" {
    var session = AgentSession.initStub();
    defer session.deinit(testing.allocator);

    const msg = try compact(testing.allocator, &session);
    defer testing.allocator.free(msg);
    try testing.expectEqualStrings("Nothing to compact.\n", msg);
    try testing.expectEqual(@as(usize, 0), session.transcript.len());
}

test "compact: collapses entries into one system_note" {
    var session = AgentSession.initStub();
    defer session.deinit(testing.allocator);
    var registry: Registry = .{};
    defer registry.deinit(testing.allocator);

    const r1 = try runOneTurn(testing.allocator, &session, &registry, "first turn", null);
    defer testing.allocator.free(r1);
    const r2 = try runOneTurn(testing.allocator, &session, &registry, "second turn", null);
    defer testing.allocator.free(r2);
    const before_len = session.transcript.len();
    try testing.expect(before_len >= 2);

    const msg = try compact(testing.allocator, &session);
    defer testing.allocator.free(msg);

    try testing.expectEqual(@as(usize, 1), session.transcript.len());
    switch (session.transcript.at(0).*) {
        .system_note => |body| {
            try testing.expect(std.mem.indexOf(u8, body, "first turn") != null);
            try testing.expect(std.mem.indexOf(u8, body, "[COMPACTED CONVERSATION HISTORY]") != null);
        },
        else => return error.TestFailed,
    }
    try testing.expect(std.mem.indexOf(u8, msg, "Compacted") != null);
    try testing.expectEqual(@as(usize, 0), session.last_persisted_len);
}

test "fork: ephemeral session returns error message" {
    var session = AgentSession.initStub();
    defer session.deinit(testing.allocator);

    const msg = try fork(testing.allocator, &session);
    defer testing.allocator.free(msg);
    try testing.expect(std.mem.indexOf(u8, msg, "ephemeral") != null);
}

test "initFromEnvWithSessionConfig appends AGENTS and CLAUDE files as read-only project context" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    try writeTestFile(allocator, tmp.abs_path, "AGENTS.md", "AGENTS_MARKER_XYZ");
    try writeTestFile(allocator, tmp.abs_path, "CLAUDE.md", "CLAUDE_MARKER_ABC");

    const saved_cwd = try cwdPathAlloc(allocator);
    defer allocator.free(saved_cwd);
    try std.Io.Threaded.chdir(tmp.abs_path);
    defer std.Io.Threaded.chdir(saved_cwd) catch {};

    var api_override = try EnvOverride.set(allocator, "ANTHROPIC_API_KEY", "test-fixture-key");
    defer api_override.restore(allocator);

    var session = try initFromEnvWithSessionConfig(allocator, null, .{ .no_session = true });
    defer session.deinit(allocator);

    try testing.expect(session.backend == .anthropic);
    const actual = session.system_prompt_owned orelse return error.TestUnexpectedResult;
    try testing.expect(std.mem.indexOf(u8, actual, "AGENTS_MARKER_XYZ") != null);
    try testing.expect(std.mem.indexOf(u8, actual, "CLAUDE_MARKER_ABC") != null);
    try testing.expect(std.mem.indexOf(u8, actual, "PROJECT CONTEXT") != null);
    // Persona identity still intact - project context never overrides it.
    try testing.expect(std.mem.indexOf(u8, actual, "native zigts coding agent") != null);
    try testing.expect(std.mem.indexOf(u8, actual, "END OF PERSONA") != null);
}

test "initFromEnvWithSessionConfig: no_context_files suppresses AGENTS and CLAUDE loading" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    try writeTestFile(allocator, tmp.abs_path, "AGENTS.md", "SHOULD_NOT_APPEAR_A");
    try writeTestFile(allocator, tmp.abs_path, "CLAUDE.md", "SHOULD_NOT_APPEAR_B");

    const saved_cwd = try cwdPathAlloc(allocator);
    defer allocator.free(saved_cwd);
    try std.Io.Threaded.chdir(tmp.abs_path);
    defer std.Io.Threaded.chdir(saved_cwd) catch {};

    var api_override = try EnvOverride.set(allocator, "ANTHROPIC_API_KEY", "test-fixture-key");
    defer api_override.restore(allocator);

    var session = try initFromEnvWithSessionConfig(allocator, null, .{
        .no_session = true,
        .no_context_files = true,
    });
    defer session.deinit(allocator);

    const actual = session.system_prompt_owned orelse return error.TestUnexpectedResult;
    try testing.expect(std.mem.indexOf(u8, actual, "SHOULD_NOT_APPEAR_A") == null);
    try testing.expect(std.mem.indexOf(u8, actual, "SHOULD_NOT_APPEAR_B") == null);
    try testing.expect(std.mem.indexOf(u8, actual, "PROJECT CONTEXT") == null);
}

test "initFromEnvWithSessionConfig stamps current policy_hash into meta.json" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const sessions_dir = try std.fs.path.join(allocator, &.{ tmp.abs_path, "sessions" });
    defer allocator.free(sessions_dir);
    var env_override = try EnvOverride.set(allocator, "ZIGTTP_SESSIONS_DIR", sessions_dir);
    defer env_override.restore(allocator);

    const ws_dir = try std.fs.path.join(allocator, &.{ tmp.abs_path, "ws" });
    defer allocator.free(ws_dir);
    {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), ws_dir);
    }

    const saved_cwd = try cwdPathAlloc(allocator);
    defer allocator.free(saved_cwd);
    try std.Io.Threaded.chdir(ws_dir);
    defer std.Io.Threaded.chdir(saved_cwd) catch {};

    var session = try initFromEnvWithSessionConfig(allocator, null, .{});
    defer session.deinit(allocator);

    const meta_path = session.meta_path orelse return error.TestUnexpectedResult;
    var meta = try session_events.readMeta(allocator, meta_path);
    defer session_events.freeMeta(allocator, &meta);

    const saved = meta.policy_hash orelse return error.TestExpected;
    const current = expert_meta.compute().policy_hash;
    try testing.expectEqualStrings(current[0..], saved);
}

test "initFromEnvWithSessionConfig: resume with drifted hash injects a system_note" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const sessions_dir = try std.fs.path.join(allocator, &.{ tmp.abs_path, "sessions" });
    defer allocator.free(sessions_dir);
    var env_override = try EnvOverride.set(allocator, "ZIGTTP_SESSIONS_DIR", sessions_dir);
    defer env_override.restore(allocator);

    const ws_dir = try std.fs.path.join(allocator, &.{ tmp.abs_path, "ws" });
    defer allocator.free(ws_dir);
    {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), ws_dir);
    }

    const saved_cwd = try cwdPathAlloc(allocator);
    defer allocator.free(saved_cwd);
    try std.Io.Threaded.chdir(ws_dir);
    defer std.Io.Threaded.chdir(saved_cwd) catch {};

    // Build + run one stub turn so events.jsonl exists for reconstruction,
    // then capture the meta path and tear down.
    const captured_meta_path: []u8 = blk: {
        var s = try initFromEnvWithSessionConfig(allocator, null, .{});
        defer s.deinit(allocator);
        var registry: Registry = .{};
        defer registry.deinit(allocator);
        const rendered = try runOneTurn(allocator, &s, &registry, "seed", null);
        allocator.free(rendered);
        break :blk try allocator.dupe(u8, s.meta_path orelse return error.TestUnexpectedResult);
    };
    defer allocator.free(captured_meta_path);

    // Forge a drifted hash into the stored meta, then resume.
    var original = try session_events.readMeta(allocator, captured_meta_path);
    defer session_events.freeMeta(allocator, &original);
    const drifted_hash = "b" ** 64;
    try session_events.writeMeta(allocator, captured_meta_path, .{
        .session_id = original.session_id,
        .workspace_realpath = original.workspace_realpath,
        .created_at_unix_ms = original.created_at_unix_ms,
        .parent_id = original.parent_id,
        .policy_hash = drifted_hash,
    });

    var resumed = try initFromEnvWithSessionConfig(allocator, null, .{ .resume_latest = true });
    defer resumed.deinit(allocator);

    var found_note = false;
    for (resumed.transcript.entries.items) |*entry| {
        switch (entry.*) {
            .system_note => |body| {
                if (std.mem.indexOf(u8, body, POLICY_DRIFT_PREFIX) != null) found_note = true;
            },
            else => {},
        }
    }
    try testing.expect(found_note);

    // After drift handling, meta should carry the current hash so a second
    // resume does not re-warn against the already-acknowledged drift.
    var post = try session_events.readMeta(allocator, captured_meta_path);
    defer session_events.freeMeta(allocator, &post);
    const current = expert_meta.compute().policy_hash;
    const stamped = post.policy_hash orelse return error.TestExpected;
    try testing.expectEqualStrings(current[0..], stamped);
}
