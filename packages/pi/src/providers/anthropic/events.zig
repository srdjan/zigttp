//! Typed event variants produced by the Anthropic Messages streaming SSE
//! parser. Every slice borrows either from the arena passed to the parser
//! or from the input buffer; callers that need event lifetimes longer than
//! the arena must dupe the strings themselves.

const std = @import("std");

pub const Event = union(enum) {
    message_start,
    content_block_start: ContentBlockStart,
    content_block_delta: ContentBlockDelta,
    content_block_stop: ContentBlockStop,
    message_delta: MessageDelta,
    message_stop,
    ping,
    api_error: ApiError,
};

pub const ContentBlockStart = struct {
    index: u32,
    kind: Kind,

    pub const Kind = union(enum) {
        text,
        tool_use: ToolUseHeader,
    };
};

pub const ToolUseHeader = struct {
    id: []const u8,
    name: []const u8,
};

pub const ContentBlockDelta = struct {
    index: u32,
    payload: Payload,

    pub const Payload = union(enum) {
        text: []const u8,
        input_json: []const u8,
    };
};

pub const ContentBlockStop = struct {
    index: u32,
};

pub const MessageDelta = struct {
    stop_reason: ?[]const u8,
    output_tokens: ?u64,
};

pub const ApiError = struct {
    kind: []const u8,
    message: []const u8,
};
