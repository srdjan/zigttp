//! Agent-mode session wrapper around `loop.runTurn`. Ships today with a
//! `StubClient` that returns a fixed text reply regardless of the prompt;
//! slice 3 of the phase-2 plan replaces the stub with a real Anthropic
//! streaming client by swapping the `ModelClient` stored on the session.
//!
//! The session owns a long-lived `Transcript` that grows across turns.
//! `runOneTurn` drives one pass through the loop driver, then renders just
//! the latest appended transcript entry (the turn's rendered output) to an
//! owned `[]u8` the caller frees. Rendering the whole transcript per turn
//! would re-print history on every submission.

const std = @import("std");
const loop = @import("loop.zig");
const turn = @import("turn.zig");
const transcript_mod = @import("transcript.zig");
const registry_mod = @import("registry/registry.zig");

const Registry = registry_mod.Registry;
const Transcript = transcript_mod.Transcript;

pub const stub_reply_text =
    "agent stub: real model lands with the Anthropic client slice";

/// Meta command that toggles agent mode on/off in the REPL and TUI.
pub const toggle_command = ":agent";

/// Production-side client used until the Anthropic client lands. Lives in
/// `agent.zig` rather than `loop.zig` so `loop.CannedClient` (test-only) and
/// this one are kept visibly distinct.
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

pub const AgentSession = struct {
    transcript: Transcript = .{},
    stub: StubClient = .{},

    pub fn init() AgentSession {
        return .{};
    }

    pub fn deinit(self: *AgentSession, allocator: std.mem.Allocator) void {
        self.transcript.deinit(allocator);
    }
};

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
        session.stub.asClient(),
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

test "runOneTurn: fresh session grows transcript by 2 and renders model reply" {
    var session = AgentSession.init();
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
    var session = AgentSession.init();
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
