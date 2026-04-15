//! Phase-2 loop driver. Pumps events from a `ModelClient` + the compiler veto
//! + the tool registry into the `turn.TurnMachine`, executes returned actions,
//! and appends rendered messages to a `Transcript`. Closes the phase-2 spine
//! end-to-end for one turn per call to `runTurn`.
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

/// Opaque model-client interface. Lets the loop driver run end-to-end under
/// test with a synthetic client returning canned replies, while leaving a
/// clean drop-in point for a real Anthropic streaming client.
pub const ModelClient = struct {
    context: *anyopaque,
    request_fn: *const fn (
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        user_text: []const u8,
    ) anyerror!turn.ModelReply,

    pub fn request(
        self: ModelClient,
        arena: std.mem.Allocator,
        user_text: []const u8,
    ) !turn.ModelReply {
        return self.request_fn(self.context, arena, user_text);
    }
};

pub const TurnResult = struct {
    final_state: turn.TurnState,
};

pub fn runTurn(
    allocator: std.mem.Allocator,
    client: ModelClient,
    registry: *const registry_mod.Registry,
    transcript: *transcript_mod.Transcript,
    user_text: []const u8,
) !TurnResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const ta = arena.allocator();

    try transcript.append(allocator, .{ .user_text = user_text });

    var machine: turn.TurnMachine = .{};
    var next_event: turn.TurnEvent = .{ .user_submitted = user_text };

    while (true) {
        const action = machine.transition(next_event);

        switch (action) {
            .send_to_model => |text| {
                const reply = try client.request(ta, text);
                next_event = .{ .model_replied = reply };
            },
            .run_veto => |edit| {
                const outcome = try veto.runVeto(ta, edit);
                next_event = .{ .edit_verified = outcome };
            },
            .invoke_tool => |tc| {
                const args = [_][]const u8{tc.args_json};
                const result = try registry.invoke(ta, tc.name, &args);
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
                return .{ .final_state = machine.state };
            },
            .prompt_user => return .{ .final_state = machine.state },
            .end_turn => return .{ .final_state = machine.state },
            .none => return .{ .final_state = machine.state },
        }
    }
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
        user_text: []const u8,
    ) anyerror!turn.ModelReply {
        const self: *CannedClient = @ptrCast(@alignCast(ctx));
        _ = arena;
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
    var canned: CannedClient = .{ .reply = .{ .edit = .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { return Response.json({ok: true}); }",
        .before = null,
    } } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurn(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add an ok response",
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(@as(usize, 2), tr.len());
    try testing.expectEqual(Tag.proof_card, tr.at(1).tag);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "\"total\":0") != null);
}

test "broken edit path: veto fails, diagnostic box in transcript" {
    var canned: CannedClient = .{ .reply = .{ .edit = .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { var x = 1; return Response.json({x}); }",
        .before = null,
    } } };
    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(testing.allocator);
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);

    const result = try runTurn(
        testing.allocator,
        canned.asClient(),
        &registry,
        &tr,
        "add a bad handler",
    );

    try testing.expectEqual(turn.TurnState.done, result.final_state);
    try testing.expectEqual(Tag.diagnostic_box, tr.at(1).tag);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "\"ZTS001\"") != null);
    try testing.expect(std.mem.indexOf(u8, tr.at(1).body, "\"introduced_by_patch\":true") != null);
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
