//! Turn state machine for the native coding-agent loop.
//!
//! Pure `(state, event) -> (state', action)`; no I/O, no allocator. The loop
//! driver owns tool execution, Anthropic requests, retry prompt framing, and
//! transcript mutation. The machine only decides what happens next.

const std = @import("std");
const ui_payload = @import("ui_payload.zig");

/// Per-request token accounting returned by the model provider.
pub const Usage = struct {
    input_tokens: u64 = 0,
    output_tokens: u64 = 0,
    cache_read_input_tokens: u64 = 0,
    cache_creation_input_tokens: u64 = 0,

    pub fn add(self: *Usage, other: Usage) void {
        self.input_tokens += other.input_tokens;
        self.output_tokens += other.output_tokens;
        self.cache_read_input_tokens += other.cache_read_input_tokens;
        self.cache_creation_input_tokens += other.cache_creation_input_tokens;
    }
};

pub const TurnState = enum {
    idle,
    awaiting_model,
    verifying_edit,
    executing_tools,
    awaiting_user,
    done,
};

pub const ToolCall = struct {
    id: []const u8,
    name: []const u8,
    args_json: []const u8,
};

pub const Edit = struct {
    file: []const u8,
    content: []const u8,
    before: ?[]const u8,
};

pub const DisplayMessage = struct {
    llm_text: []const u8,
    ui_payload: ?ui_payload.UiPayload = null,

    pub fn deinit(self: *DisplayMessage, allocator: std.mem.Allocator) void {
        allocator.free(self.llm_text);
        if (self.ui_payload) |*payload| payload.deinit(allocator);
        self.* = .{ .llm_text = &.{}, .ui_payload = null };
    }
};

pub const AssistantReply = struct {
    preamble: ?[]const u8 = null,
    response: Response,

    pub const Response = union(enum) {
        final_text: []const u8,
        tool_calls: []const ToolCall,
        edit: Edit,
    };
};

pub const EditOutcome = struct {
    ok: bool,
    llm_text: []const u8,
    ui_payload: ?ui_payload.UiPayload = null,

    pub fn deinit(self: *EditOutcome, allocator: std.mem.Allocator) void {
        allocator.free(self.llm_text);
        if (self.ui_payload) |*payload| payload.deinit(allocator);
        self.* = .{ .ok = false, .llm_text = &.{}, .ui_payload = null };
    }
};

pub const ToolResultMessage = struct {
    tool_use_id: []const u8,
    tool_name: []const u8,
    ok: bool,
    llm_text: []const u8,
    ui_payload: ?ui_payload.UiPayload = null,
};

pub const TurnEvent = union(enum) {
    user_submitted: []const u8,
    model_replied: AssistantReply,
    edit_verified: EditOutcome,
    tool_batch_completed,
    user_approved: bool,
    budget_exhausted,
};

pub const Message = union(enum) {
    user_text: []const u8,
    model_text: []const u8,
    assistant_tool_use: []const ToolCall,
    proof_card: DisplayMessage,
    diagnostic_box: DisplayMessage,
    tool_result: ToolResultMessage,
    /// Compacted history note injected by /compact. Replaces one or more older
    /// entries so the model still sees the gist without the full token cost.
    system_note: []const u8,
};

pub const RetryPayload = struct {
    diagnostic: []const u8,
    attempt: u8,
    max_attempts: u8,
};

pub const Action = union(enum) {
    none,
    request_model,
    run_veto: Edit,
    invoke_tool_batch: []const ToolCall,
    retry_draft: RetryPayload,
    render: Message,
    prompt_user: []const u8,
    end_turn,
};

pub const TurnMachine = struct {
    state: TurnState = .idle,
    attempt: u8 = 1,
    max_attempts: u8 = 3,

    pub fn transition(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .budget_exhausted => {
                self.state = .awaiting_user;
                return .{ .prompt_user = "agent budget exhausted before a final answer" };
            },
            else => {},
        }

        return switch (self.state) {
            .idle => self.fromIdle(event),
            .awaiting_model => self.fromAwaitingModel(event),
            .verifying_edit => self.fromVerifyingEdit(event),
            .executing_tools => self.fromExecutingTools(event),
            .awaiting_user => self.fromAwaitingUser(event),
            .done => .none,
        };
    }

    fn fromIdle(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .user_submitted => {
                self.state = .awaiting_model;
                return .request_model;
            },
            else => return .none,
        }
    }

    fn fromAwaitingModel(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .model_replied => |reply| switch (reply.response) {
                .final_text => |text| {
                    self.state = .done;
                    return .{ .render = .{ .model_text = text } };
                },
                .tool_calls => |calls| {
                    self.state = .executing_tools;
                    return .{ .invoke_tool_batch = calls };
                },
                .edit => |edit| {
                    self.state = .verifying_edit;
                    return .{ .run_veto = edit };
                },
            },
            else => return .none,
        }
    }

    fn fromVerifyingEdit(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .edit_verified => |outcome| {
                if (outcome.ok) {
                    self.state = .done;
                    return .{ .render = .{ .proof_card = .{
                        .llm_text = outcome.llm_text,
                        .ui_payload = outcome.ui_payload,
                    } } };
                }
                if (self.attempt >= self.max_attempts) {
                    self.state = .done;
                    return .{ .render = .{ .diagnostic_box = .{
                        .llm_text = outcome.llm_text,
                        .ui_payload = outcome.ui_payload,
                    } } };
                }
                self.attempt += 1;
                self.state = .awaiting_model;
                return .{ .retry_draft = .{
                    .diagnostic = outcome.llm_text,
                    .attempt = self.attempt,
                    .max_attempts = self.max_attempts,
                } };
            },
            else => return .none,
        }
    }

    fn fromExecutingTools(self: *TurnMachine, event: TurnEvent) Action {
        switch (event) {
            .tool_batch_completed => {
                self.state = .awaiting_model;
                return .request_model;
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

const testing = std.testing;

test "idle + user_submitted -> awaiting_model with request_model" {
    var m: TurnMachine = .{};
    const action = m.transition(.{ .user_submitted = "add a GET route" });

    try testing.expectEqual(TurnState.awaiting_model, m.state);
    try testing.expect(action == .request_model);
}

test "awaiting_model + final_text -> done with render(model_text)" {
    var m: TurnMachine = .{ .state = .awaiting_model };
    const action = m.transition(.{ .model_replied = .{
        .response = .{ .final_text = "here's the plan" },
    } });

    try testing.expectEqual(TurnState.done, m.state);
    switch (action) {
        .render => |msg| switch (msg) {
            .model_text => |text| try testing.expectEqualStrings("here's the plan", text),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}

test "awaiting_model + tool_calls -> executing_tools with invoke_tool_batch" {
    var m: TurnMachine = .{ .state = .awaiting_model };
    const calls = [_]ToolCall{
        .{ .id = "toolu_1", .name = "zigts_expert_meta", .args_json = "{}" },
        .{ .id = "toolu_2", .name = "zigts_expert_features", .args_json = "{}" },
    };
    const action = m.transition(.{ .model_replied = .{
        .preamble = "I'll inspect the compiler surface first.",
        .response = .{ .tool_calls = &calls },
    } });

    try testing.expectEqual(TurnState.executing_tools, m.state);
    switch (action) {
        .invoke_tool_batch => |batch| {
            try testing.expectEqual(@as(usize, 2), batch.len);
            try testing.expectEqualStrings("toolu_1", batch[0].id);
            try testing.expectEqualStrings("zigts_expert_features", batch[1].name);
        },
        else => return error.TestFailed,
    }
}

test "executing_tools + tool_batch_completed -> awaiting_model with request_model" {
    var m: TurnMachine = .{ .state = .executing_tools };
    const action = m.transition(.tool_batch_completed);
    try testing.expectEqual(TurnState.awaiting_model, m.state);
    try testing.expect(action == .request_model);
}

test "awaiting_model + edit -> verifying_edit with run_veto" {
    var m: TurnMachine = .{ .state = .awaiting_model };
    const action = m.transition(.{ .model_replied = .{
        .response = .{ .edit = .{
            .file = "handler.ts",
            .content = "...",
            .before = null,
        } },
    } });

    try testing.expectEqual(TurnState.verifying_edit, m.state);
    switch (action) {
        .run_veto => |edit| {
            try testing.expectEqualStrings("handler.ts", edit.file);
            try testing.expect(edit.before == null);
        },
        else => return error.TestFailed,
    }
}

test "verifying_edit + edit_verified(ok) -> done with proof_card" {
    var m: TurnMachine = .{ .state = .verifying_edit };
    const action = m.transition(.{ .edit_verified = .{
        .ok = true,
        .llm_text = "proof-body",
    } });

    try testing.expectEqual(TurnState.done, m.state);
    switch (action) {
        .render => |msg| switch (msg) {
            .proof_card => |card| try testing.expectEqualStrings("proof-body", card.llm_text),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}

test "verifying_edit + edit_verified(fail) with budget remaining -> retry_draft" {
    var m: TurnMachine = .{ .state = .verifying_edit, .max_attempts = 3 };
    const action = m.transition(.{ .edit_verified = .{
        .ok = false,
        .llm_text = "ZTS001 diag",
    } });

    try testing.expectEqual(TurnState.awaiting_model, m.state);
    try testing.expectEqual(@as(u8, 2), m.attempt);
    switch (action) {
        .retry_draft => |payload| {
            try testing.expectEqualStrings("ZTS001 diag", payload.diagnostic);
            try testing.expectEqual(@as(u8, 2), payload.attempt);
            try testing.expectEqual(@as(u8, 3), payload.max_attempts);
        },
        else => return error.TestFailed,
    }
}

test "verifying_edit + edit_verified(fail) at max_attempts -> done with diagnostic_box" {
    var m: TurnMachine = .{ .state = .verifying_edit, .attempt = 3, .max_attempts = 3 };
    const action = m.transition(.{ .edit_verified = .{
        .ok = false,
        .llm_text = "final diag",
    } });

    try testing.expectEqual(TurnState.done, m.state);
    switch (action) {
        .render => |msg| switch (msg) {
            .diagnostic_box => |box| try testing.expectEqualStrings("final diag", box.llm_text),
            else => return error.TestFailed,
        },
        else => return error.TestFailed,
    }
}

test "budget_exhausted from any active state -> awaiting_user with prompt_user" {
    const states = [_]TurnState{ .awaiting_model, .verifying_edit, .executing_tools };
    for (states) |initial| {
        var m: TurnMachine = .{ .state = initial };
        const action = m.transition(.budget_exhausted);
        try testing.expectEqual(TurnState.awaiting_user, m.state);
        switch (action) {
            .prompt_user => |question| try testing.expect(std.mem.indexOf(u8, question, "budget exhausted") != null),
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
    var m: TurnMachine = .{};
    const action = m.transition(.tool_batch_completed);
    try testing.expectEqual(TurnState.idle, m.state);
    try testing.expect(action == .none);
}

test ".done is terminal" {
    var m: TurnMachine = .{ .state = .done };
    try testing.expect(m.transition(.{ .user_submitted = "hi" }) == .none);
    try testing.expectEqual(TurnState.done, m.state);
}
