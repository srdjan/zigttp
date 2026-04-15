//! Agent-mode session wrapper around `loop.runTurn`. Carries a backend
//! union that is either a `StubClient` (default, zero-config, fixed
//! reply) or a live Anthropic `client.Client` built from an API key and
//! a system prompt. Callers swap backends at session construction; the
//! rest of the loop is agnostic.
//!
//! The session owns a long-lived `Transcript` that grows across turns
//! plus, in the live path, an allocator-owned copy of the system prompt
//! so the persona bytes outlive whatever buffer produced them.
//! `runOneTurn` drives one pass through the loop driver, then renders
//! the latest appended transcript entry to an owned `[]u8` the caller
//! frees. Rendering the whole transcript per turn would re-print
//! history on every submission.

const std = @import("std");
const loop = @import("loop.zig");
const turn = @import("turn.zig");
const transcript_mod = @import("transcript.zig");
const registry_mod = @import("registry/registry.zig");
const anthropic_client = @import("anthropic/client.zig");
const expert_persona = @import("expert_persona.zig");

const Registry = registry_mod.Registry;
const Transcript = transcript_mod.Transcript;

pub const stub_reply_text =
    "agent stub: set ZIGTTP_PI_LIVE=1 and ANTHROPIC_API_KEY to enable the real model";

/// Meta command that toggles agent mode on/off in the REPL and TUI.
pub const toggle_command = ":agent";

/// Zero-state client used when no Anthropic credentials are present.
/// Returns a fixed reply regardless of the prompt so the loop still
/// exercises every path from keyboard to transcript.
pub const StubClient = struct {
    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        user_text: []const u8,
    ) anyerror!turn.ModelReply {
        _ = ctx;
        _ = arena;
        _ = user_text;
        return .{ .text = stub_reply_text };
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
    live: anthropic_client.Client,
};

pub const AgentSession = struct {
    transcript: Transcript = .{},
    backend: Backend = .{ .stub = .{} },
    /// Allocator-owned copy of the system prompt bytes backing the live
    /// client's Config. Null for the stub path.
    system_prompt_owned: ?[]u8 = null,

    pub fn initStub() AgentSession {
        return .{};
    }

    /// Constructs a session whose backend is a real Anthropic client.
    /// Dupes the system prompt so the caller's buffer can be freed
    /// independently. Dupes the API key for the same reason.
    pub fn initLive(
        allocator: std.mem.Allocator,
        api_key: []const u8,
        system_prompt: []const u8,
    ) !AgentSession {
        const prompt_owned = try allocator.dupe(u8, system_prompt);
        errdefer allocator.free(prompt_owned);
        const key_owned = try allocator.dupe(u8, api_key);
        errdefer allocator.free(key_owned);
        return .{
            .backend = .{ .live = anthropic_client.Client.init(.{
                .api_key = key_owned,
                .system_prompt = prompt_owned,
            }) },
            .system_prompt_owned = prompt_owned,
        };
    }

    pub fn deinit(self: *AgentSession, allocator: std.mem.Allocator) void {
        self.transcript.deinit(allocator);
        if (self.system_prompt_owned) |s| allocator.free(s);
        switch (self.backend) {
            .stub => {},
            .live => |*c| allocator.free(c.config.api_key),
        }
    }

    pub fn modelClient(self: *AgentSession) loop.ModelClient {
        return switch (self.backend) {
            .stub => (&self.backend.stub).asClient(),
            .live => (&self.backend.live).asModelClient(),
        };
    }

    pub fn isLive(self: *const AgentSession) bool {
        return self.backend == .live;
    }
};

/// Reads `ZIGTTP_PI_LIVE` and `ANTHROPIC_API_KEY` from the environment. If
/// both are set (and `ZIGTTP_PI_LIVE=1`), builds the persona bundle and
/// returns a live session. Otherwise returns a stub session. Callers see
/// a single constructor regardless of which path activated.
pub fn initFromEnv(allocator: std.mem.Allocator) !AgentSession {
    const opt_in = envVar("ZIGTTP_PI_LIVE") orelse return initStub();
    if (!std.mem.eql(u8, opt_in, "1")) return initStub();
    const api_key = envVar("ANTHROPIC_API_KEY") orelse return initStub();

    const system_prompt = try expert_persona.buildSystemPrompt(allocator);
    defer allocator.free(system_prompt);
    return try AgentSession.initLive(allocator, api_key, system_prompt);
}

fn envVar(name_z: [:0]const u8) ?[]const u8 {
    const raw = std.c.getenv(name_z) orelse return null;
    return std.mem.sliceTo(raw, 0);
}

fn initStub() AgentSession {
    return AgentSession.initStub();
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
) ![]u8 {
    _ = try loop.runTurn(
        allocator,
        session.modelClient(),
        registry,
        &session.transcript,
        user_text,
    );

    const tr = &session.transcript;
    std.debug.assert(tr.len() >= 1);
    return transcript_mod.renderEntryToOwned(allocator, tr.at(tr.len() - 1));
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

    const rendered = try runOneTurn(testing.allocator, &session, &registry, "add a GET route");
    defer testing.allocator.free(rendered);

    try testing.expectEqual(@as(usize, 2), session.transcript.len());
    try testing.expectEqual(transcript_mod.Tag.user_text, session.transcript.at(0).tag);
    try testing.expectEqualStrings("add a GET route", session.transcript.at(0).body);
    try testing.expectEqual(transcript_mod.Tag.model_text, session.transcript.at(1).tag);
    try testing.expect(std.mem.startsWith(u8, rendered, "model: "));
    try testing.expect(std.mem.indexOf(u8, rendered, stub_reply_text) != null);
    try testing.expect(rendered[rendered.len - 1] == '\n');
}

test "runOneTurn: two turns back-to-back accumulate in the transcript" {
    var session = AgentSession.initStub();
    defer session.deinit(testing.allocator);
    var registry: Registry = .{};
    defer registry.deinit(testing.allocator);

    const first = try runOneTurn(testing.allocator, &session, &registry, "first intent");
    defer testing.allocator.free(first);
    const second = try runOneTurn(testing.allocator, &session, &registry, "second intent");
    defer testing.allocator.free(second);

    try testing.expectEqual(@as(usize, 4), session.transcript.len());
    try testing.expectEqualStrings("first intent", session.transcript.at(0).body);
    try testing.expectEqualStrings("second intent", session.transcript.at(2).body);
    // The second render is the second turn's model reply only, not a
    // cumulative dump of the whole transcript.
    try testing.expect(std.mem.indexOf(u8, second, "first intent") == null);
    try testing.expect(std.mem.indexOf(u8, second, stub_reply_text) != null);
}

test "StubClient ignores user_text and always returns the stub reply" {
    var stub: StubClient = .{};
    const client = stub.asClient();
    const reply = try client.request(testing.allocator, "whatever the user typed");
    switch (reply) {
        .text => |t| try testing.expectEqualStrings(stub_reply_text, t),
        else => return error.TestFailed,
    }
}

test "initLive dupes api_key and system_prompt, deinit releases both" {
    var session = try AgentSession.initLive(
        testing.allocator,
        "sk-ant-test",
        "you are a zigts expert",
    );
    defer session.deinit(testing.allocator);

    try testing.expect(session.backend == .live);
    try testing.expectEqualStrings("sk-ant-test", session.backend.live.config.api_key);
    try testing.expectEqualStrings("you are a zigts expert", session.backend.live.config.system_prompt);
    try testing.expect(session.system_prompt_owned != null);
}

test "modelClient returns a live client vtable when backend is live" {
    var session = try AgentSession.initLive(testing.allocator, "k", "p");
    defer session.deinit(testing.allocator);

    const mc = session.modelClient();
    try testing.expect(mc.context == @as(*anyopaque, @ptrCast(&session.backend.live)));
}
