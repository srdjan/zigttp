//! Phase 2 turn state machine. Pure `(state, event) -> (state', action)`; no
//! I/O, no allocator. The eventual agent loop feeds events from the user, the
//! model client, the compiler veto, and the tool registry, then executes the
//! returned `Action`. The machine owns the turn's lifecycle; when `state ==
//! .done`, the loop constructs a fresh machine for the next turn.
//!
//! Design notes:
//!
//! 1. Edits are first-class events, distinct from tool calls. The veto sits on
//!    the edit path, not on every tool invocation. A model that writes via the
//!    edit path gets the compiler's veto; a model that invokes a read-only
//!    tool does not.
//!
//! 2. `budget_exhausted` is handled uniformly regardless of state because it
//!    can fire during draft/verify/execute alike. It transitions to
//!    `awaiting_user`, which is the only state that renders the current draft
//!    before the turn ends.
//!
//! 3. Out-of-order events (e.g., `tool_completed` received while `.idle`)
//!    return `.none` rather than erroring. Phase-2 logic will decide whether
//!    to treat that as an assertion failure; this machine stays permissive.

const std = @import("std");

pub const TurnState = enum {
    /// No turn in progress. Waiting for user input.
    idle,
    /// User input submitted; waiting for the model to reply.
    awaiting_model,
    /// Model proposed an edit; compiler is running edit-simulate.
    verifying_edit,
    /// Model requested a non-edit tool call; tool is executing.
    executing_tool,
    /// Blocked on explicit user approval (capability grant, veto exhausted).
    awaiting_user,
    /// Turn complete; caller may discard the machine.
    done,
};

pub const ModelReply = union(enum) {
    text: []const u8,
    tool_call: ToolCall,
    edit: Edit,

    pub const ToolCall = struct {
        name: []const u8,
        args_json: []const u8,
    };

    pub const Edit = struct {
        file: []const u8,
        content: []const u8,
        before: ?[]const u8,
    };
};

pub const EditOutcome = struct {
    /// True iff edit-simulate reported `new_count == 0` — the veto semantics
    /// pinned in docs/zigts-expert-contract.md.
    ok: bool,
    /// Serialized body to render: proof card on `ok`, diagnostic box otherwise.
    body: []const u8,
};

pub const ToolOutcome = struct {
    ok: bool,
    body: []const u8,
};

pub const TurnEvent = union(enum) {
    user_submitted: []const u8,
    model_replied: ModelReply,
    edit_verified: EditOutcome,
    tool_completed: ToolOutcome,
    user_approved: bool,
    budget_exhausted,
};

pub const Message = union(enum) {
    user_text: []const u8,
    model_text: []const u8,
    proof_card: []const u8,
    diagnostic_box: []const u8,
    tool_result: []const u8,
};

pub const Action = union(enum) {
    /// No side effect; caller waits for the next event.
    none,
    /// Forward user input to the model client.
    send_to_model: []const u8,
    /// Run edit-simulate against a proposed edit before it reaches the user.
    run_veto: ModelReply.Edit,
    /// Execute a non-edit tool through the registry.
    invoke_tool: ModelReply.ToolCall,
    /// Append a message to the transcript.
    render: Message,
    /// Block the loop and ask the user to approve.
    prompt_user: []const u8,
    /// Turn complete; caller should stop driving this machine.
    end_turn,
};

pub const TurnMachine = struct {
    state: TurnState = .idle,

    pub fn transition(self: *TurnMachine, event: TurnEvent) Action {
        // Cross-cutting events fire uniformly regardless of the current state.
        switch (event) {
            .budget_exhausted => {
                self.state = .awaiting_user;
                return .{ .prompt_user = "retry budget exhausted - approve last draft?" };
            },
            else => {},
        }

        return switch (self.state) {
            .idle => self.fromIdle(event),
            .awaiting_model => self.fromAwaitingModel(event),
            .verifying_edit => self.fromVerifyingEdit(event),
            .executing_tool => self.fromExecutingTool(event),
            .awaiting_user => self.fromAwaitingUser(event),
            .done => .none,
        };
    }

    fn fromIdle(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .user_submitted => |text| {
                self.state = .awaiting_model;
                return .{ .send_to_model = text };
            },
            else => return .none,
        }
    }

    fn fromAwaitingModel(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .model_replied => |reply| switch (reply) {
                .text => |t| {
                    self.state = .done;
                    return .{ .render = .{ .model_text = t } };
                },
                .tool_call => |tc| {
                    self.state = .executing_tool;
                    return .{ .invoke_tool = tc };
                },
                .edit => |e| {
                    self.state = .verifying_edit;
                    return .{ .run_veto = e };
                },
            },
            else => return .none,
        }
    }

    fn fromVerifyingEdit(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .edit_verified => |outcome| {
                self.state = .done;
                const msg: Message = if (outcome.ok)
                    .{ .proof_card = outcome.body }
                else
                    .{ .diagnostic_box = outcome.body };
                return .{ .render = msg };
            },
            else => return .none,
        }
    }

    fn fromExecutingTool(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .tool_completed => |outcome| {
                self.state = .done;
                return .{ .render = .{ .tool_result = outcome.body } };
            },
            else => return .none,
        }
    }

    fn fromAwaitingUser(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .user_approved => {
                self.state = .done;
                return .end_turn;
            },
            else => return .none,
        }
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "idle + user_submitted -> awaiting_model with send_to_model" {
    var m: TurnMachine = .{};
    const action = m.transition(.{ .user_submitted = "add a GET route" });

    try testing.expectEqual(TurnState.awaiting_model, m.state);
    switch (action) {
        .send_to_model => |text| try testing.expectEqualStrings("add a GET route", text),
        else => return error.TestFailed,
    }
}

test "awaiting_model + model_replied(text) -> done with render(model_text)" {
    var m: TurnMachine = .{ .state = .awaiting_model };
    const action = m.transition(.{ .model_replied = .{ .text = "here's the plan" } });

    try testing.expectEqual(TurnState.done, m.state);
    switch (action) {
        .render => |msg| switch (msg) {
            .model_text => |t| try testing.expectEqualStrings("here's the plan", t),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}

test "awaiting_model + model_replied(tool_call) -> executing_tool with invoke_tool" {
    var m: TurnMachine = .{ .state = .awaiting_model };
    const action = m.transition(.{ .model_replied = .{ .tool_call = .{
        .name = "zigts_expert_meta",
        .args_json = "{}",
    } } });

    try testing.expectEqual(TurnState.executing_tool, m.state);
    switch (action) {
        .invoke_tool => |tc| {
            try testing.expectEqualStrings("zigts_expert_meta", tc.name);
            try testing.expectEqualStrings("{}", tc.args_json);
        },
        else => return error.TestFailed,
    }
}

test "awaiting_model + model_replied(edit) -> verifying_edit with run_veto" {
    var m: TurnMachine = .{ .state = .awaiting_model };
    const action = m.transition(.{ .model_replied = .{ .edit = .{
        .file = "handler.ts",
        .content = "...",
        .before = null,
    } } });

    try testing.expectEqual(TurnState.verifying_edit, m.state);
    switch (action) {
        .run_veto => |e| {
            try testing.expectEqualStrings("handler.ts", e.file);
            try testing.expect(e.before == null);
        },
        else => return error.TestFailed,
    }
}

test "verifying_edit + edit_verified(ok) -> done with render(proof_card)" {
    var m: TurnMachine = .{ .state = .verifying_edit };
    const action = m.transition(.{ .edit_verified = .{ .ok = true, .body = "proof-body" } });

    try testing.expectEqual(TurnState.done, m.state);
    switch (action) {
        .render => |msg| switch (msg) {
            .proof_card => |b| try testing.expectEqualStrings("proof-body", b),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}

test "verifying_edit + edit_verified(fail) -> done with render(diagnostic_box)" {
    var m: TurnMachine = .{ .state = .verifying_edit };
    const action = m.transition(.{ .edit_verified = .{ .ok = false, .body = "ZTS001 ..." } });

    try testing.expectEqual(TurnState.done, m.state);
    switch (action) {
        .render => |msg| switch (msg) {
            .diagnostic_box => |b| try testing.expectEqualStrings("ZTS001 ...", b),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}

test "executing_tool + tool_completed -> done with render(tool_result)" {
    var m: TurnMachine = .{ .state = .executing_tool };
    const action = m.transition(.{ .tool_completed = .{ .ok = true, .body = "{\"ok\":true}\n" } });

    try testing.expectEqual(TurnState.done, m.state);
    switch (action) {
        .render => |msg| switch (msg) {
            .tool_result => |b| try testing.expectEqualStrings("{\"ok\":true}\n", b),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}

test "budget_exhausted from any state -> awaiting_user with prompt_user" {
    const states = [_]TurnState{ .awaiting_model, .verifying_edit, .executing_tool };
    for (states) |initial| {
        var m: TurnMachine = .{ .state = initial };
        const action = m.transition(.budget_exhausted);
        try testing.expectEqual(TurnState.awaiting_user, m.state);
        switch (action) {
            .prompt_user => |q| try testing.expect(std.mem.indexOf(u8, q, "budget exhausted") != null),
            else => return error.TestFailed,
        }
    }
}

test "awaiting_user + user_approved -> done with end_turn" {
    var m: TurnMachine = .{ .state = .awaiting_user };
    const action = m.transition(.{ .user_approved = true });
    try testing.expectEqual(TurnState.done, m.state);
    try testing.expect(action == .end_turn);
}

test "out-of-order events return .none without changing state" {
    var m: TurnMachine = .{}; // idle
    const action = m.transition(.{ .tool_completed = .{ .ok = true, .body = "" } });
    try testing.expectEqual(TurnState.idle, m.state);
    try testing.expect(action == .none);
}

test ".done is terminal: any event returns .none" {
    var m: TurnMachine = .{ .state = .done };
    const user_action = m.transition(.{ .user_submitted = "hi" });
    try testing.expect(user_action == .none);
    try testing.expectEqual(TurnState.done, m.state);
}

test "full happy-path edit sequence drives through all three states" {
    var m: TurnMachine = .{};

    _ = m.transition(.{ .user_submitted = "add a route" });
    try testing.expectEqual(TurnState.awaiting_model, m.state);

    _ = m.transition(.{ .model_replied = .{ .edit = .{
        .file = "handler.ts",
        .content = "function handler(req) { return Response.json({}); }",
        .before = null,
    } } });
    try testing.expectEqual(TurnState.verifying_edit, m.state);

    const final = m.transition(.{ .edit_verified = .{ .ok = true, .body = "proof" } });
    try testing.expectEqual(TurnState.done, m.state);
    switch (final) {
        .render => |msg| try testing.expect(msg == .proof_card),
        else => return error.TestFailed,
    }
}
