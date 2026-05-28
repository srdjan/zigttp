//! Typed event variants produced by the OpenAI Responses API streaming SSE
//! parser. Mirrors `providers/anthropic/events.zig` in spirit, but the
//! Responses API has a different vocabulary: events are addressed by an
//! `output_index` (the position of a top-level output item: an assistant
//! message, a function_call, etc.), and the `response.completed` event
//! carries the final usage totals (Anthropic emits them piecewise).
//!
//! Every borrowed slice on the returned events lives as long as the arena
//! passed to the parser; callers that need lifetimes longer than the arena
//! must dupe the strings themselves.

const std = @import("std");

pub const Usage = struct {
    input_tokens: u64 = 0,
    output_tokens: u64 = 0,
};

pub const Event = union(enum) {
    /// `response.created` or `response.in_progress`. Carries no payload we
    /// consume today; tracked so the parser can validate the start of stream.
    response_started,

    /// `response.output_item.added`. A new top-level output item begins at
    /// `output_index`. The `kind` discriminator tells us whether to start
    /// accumulating text deltas or function-call argument deltas.
    output_item_added: OutputItemAdded,

    /// `response.output_text.delta`. Streamed text fragment for the message
    /// at `output_index`. The OpenAI shape also exposes `content_index`
    /// because a message can carry multiple content parts, but in practice
    /// the assistant emits one `output_text` part per message; we keep the
    /// field for fidelity to the wire format.
    output_text_delta: OutputTextDelta,

    /// `response.function_call_arguments.delta`. Streamed JSON-argument
    /// fragment for the function call at `output_index`.
    function_call_arguments_delta: FunctionCallArgumentsDelta,

    /// `response.output_item.done`. Marks the end of the item at
    /// `output_index`. The Responses API repeats the final arguments string
    /// inside this event for function_call items, but our assembler joins
    /// the streamed deltas, so we ignore the snapshot.
    output_item_done: OutputItemDone,

    /// `response.completed`. Terminal success event; carries the final
    /// usage totals.
    response_completed: ResponseCompleted,

    /// `response.failed`, `response.incomplete`, or a bare `error` event.
    api_error: ApiError,
};

pub const OutputItemAdded = struct {
    output_index: u32,
    kind: Kind,

    pub const Kind = union(enum) {
        message,
        function_call: FunctionCallHeader,
    };
};

pub const FunctionCallHeader = struct {
    call_id: []const u8,
    name: []const u8,
};

pub const OutputTextDelta = struct {
    output_index: u32,
    content_index: u32,
    text: []const u8,
};

pub const FunctionCallArgumentsDelta = struct {
    output_index: u32,
    delta: []const u8,
};

pub const OutputItemDone = struct {
    output_index: u32,
};

pub const ResponseCompleted = struct {
    usage: Usage = .{},
    stop_reason: ?[]const u8 = null,
};

pub const ApiError = struct {
    kind: []const u8,
    message: []const u8,
};
