//! Non-streaming HTTPS wire layer for the OpenAI Chat Completions API.
//!
//! Mirrors the public surface of `providers/anthropic/client.zig` so the
//! `Backend` union in `agent.zig` can swap providers without leaking
//! vendor specifics into the loop. We POST a single JSON request and
//! parse a single JSON response. Streaming/SSE was intentionally skipped
//! at this step; the loop runs one turn at a time and only consumes the
//! final reply or the structured tool-call list, both of which the
//! non-streaming envelope already carries.

const std = @import("std");
const loop = @import("../../loop.zig");
const turn = @import("../../turn.zig");
const transcript_mod = @import("../../transcript.zig");
const registry_mod = @import("../../registry/registry.zig");

const default_base_url = "https://api.openai.com/v1/chat/completions";
pub const default_model = "gpt-4o-mini";
pub const default_max_tokens: u32 = 8192;
const max_response_body_bytes: usize = 16 * 1024 * 1024;

pub const Config = struct {
    api_key: []const u8,
    system_prompt: []const u8,
    model: []const u8 = default_model,
    max_tokens: u32 = default_max_tokens,
    tools_json: ?[]const u8 = null,
    base_url: []const u8 = default_base_url,
};

pub const ClientError = error{
    HttpNotOk,
    UnexpectedResponseShape,
};

pub const Client = struct {
    config: Config,

    pub fn init(config: Config) Client {
        return .{ .config = config };
    }

    pub fn asModelClient(self: *Client) loop.ModelClient {
        return .{ .context = self, .request_fn = requestFn };
    }

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!loop.ModelCallResult {
        const self: *Client = @ptrCast(@alignCast(ctx));
        return self.sendTurn(arena, transcript, extra_user_text);
    }

    pub fn sendTurn(
        self: *Client,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) !loop.ModelCallResult {
        const body = try buildRequestBody(arena, self.config, transcript, extra_user_text);
        const response_body = try post(arena, self.config, body);
        return try parseResponse(arena, response_body);
    }
};

// -----------------------------------------------------------------------
// Request body
// -----------------------------------------------------------------------

pub fn buildRequestBody(
    arena: std.mem.Allocator,
    config: Config,
    transcript: *const transcript_mod.Transcript,
    extra_user_text: ?[]const u8,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    var aw: std.Io.Writer.Allocating = .fromArrayList(arena, &buf);
    const w = &aw.writer;

    try w.writeByte('{');
    try w.writeAll("\"model\":");
    try writeJsonString(w, config.model);
    try w.print(",\"max_tokens\":{d}", .{config.max_tokens});
    try w.writeAll(",\"stream\":false");

    try w.writeAll(",\"messages\":[");
    // System prompt as the first message; OpenAI uses role=system rather
    // than a dedicated top-level field.
    try w.writeAll("{\"role\":\"system\",\"content\":");
    try writeJsonString(w, config.system_prompt);
    try w.writeByte('}');

    for (transcript.entries.items) |*entry| {
        try writeTranscriptEntry(w, entry);
    }
    if (extra_user_text) |body| {
        try w.writeAll(",{\"role\":\"user\",\"content\":");
        try writeJsonString(w, body);
        try w.writeByte('}');
    }
    try w.writeByte(']');

    if (config.tools_json) |tools| {
        try w.writeAll(",\"tools\":");
        try w.writeAll(tools);
    }

    try w.writeByte('}');
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(arena);
}

fn writeTranscriptEntry(w: anytype, entry: *const transcript_mod.OwnedEntry) !void {
    switch (entry.*) {
        .user_text => |body| {
            try w.writeAll(",{\"role\":\"user\",\"content\":");
            try writeJsonString(w, body);
            try w.writeByte('}');
        },
        .system_note => |body| {
            // System notes are surfaced to the model as user-role context
            // blocks to match the Anthropic provider's mapping; OpenAI's
            // role=system is reserved for the persona prompt.
            try w.writeAll(",{\"role\":\"user\",\"content\":");
            try writeJsonString(w, body);
            try w.writeByte('}');
        },
        .model_text => |body| {
            try w.writeAll(",{\"role\":\"assistant\",\"content\":");
            try writeJsonString(w, body);
            try w.writeByte('}');
        },
        .assistant_tool_use => |calls| {
            try w.writeAll(",{\"role\":\"assistant\",\"content\":null,\"tool_calls\":[");
            for (calls, 0..) |call, i| {
                if (i > 0) try w.writeByte(',');
                try w.writeAll("{\"id\":");
                try writeJsonString(w, call.id);
                try w.writeAll(",\"type\":\"function\",\"function\":{\"name\":");
                try writeJsonString(w, call.name);
                try w.writeAll(",\"arguments\":");
                try writeJsonString(w, call.args_json);
                try w.writeAll("}}");
            }
            try w.writeAll("]}");
        },
        .tool_result => |result| {
            try w.writeAll(",{\"role\":\"tool\",\"tool_call_id\":");
            try writeJsonString(w, result.tool_use_id);
            try w.writeAll(",\"content\":");
            try writeJsonString(w, result.llm_text);
            try w.writeByte('}');
        },
        .proof_card, .diagnostic_box, .verified_patch => {},
    }
}

// -----------------------------------------------------------------------
// Tools schema
// -----------------------------------------------------------------------

pub fn writeToolsArray(writer: anytype, registry: *const registry_mod.Registry) !void {
    try writer.writeByte('[');
    for (registry.list(), 0..) |entry, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"type\":\"function\",\"function\":{\"name\":");
        try writeJsonString(writer, entry.name);
        try writer.writeAll(",\"description\":");
        try writeJsonString(writer, entry.description);
        try writer.writeAll(",\"parameters\":");
        try writer.writeAll(entry.input_schema);
        try writer.writeAll("}}");
    }
    try writer.writeByte(']');
}

// -----------------------------------------------------------------------
// Response parsing
// -----------------------------------------------------------------------

pub fn parseResponse(arena: std.mem.Allocator, body: []const u8) !loop.ModelCallResult {
    const parsed = std.json.parseFromSlice(std.json.Value, arena, body, .{}) catch
        return error.UnexpectedResponseShape;
    if (parsed.value != .object) return error.UnexpectedResponseShape;
    const root = parsed.value.object;

    var usage: turn.Usage = .{};
    if (root.get("usage")) |usage_value| {
        if (usage_value == .object) {
            const u = usage_value.object;
            if (u.get("prompt_tokens")) |v| if (v == .integer) {
                usage.input_tokens = @intCast(v.integer);
            };
            if (u.get("completion_tokens")) |v| if (v == .integer) {
                usage.output_tokens = @intCast(v.integer);
            };
        }
    }

    const choices = root.get("choices") orelse return error.UnexpectedResponseShape;
    if (choices != .array or choices.array.items.len == 0) return error.UnexpectedResponseShape;
    const first = choices.array.items[0];
    if (first != .object) return error.UnexpectedResponseShape;
    const message = first.object.get("message") orelse return error.UnexpectedResponseShape;
    if (message != .object) return error.UnexpectedResponseShape;
    const msg_obj = message.object;

    // Tool-call branch takes priority when present; OpenAI sets
    // `finish_reason = "tool_calls"` but inspecting `tool_calls` directly
    // is enough.
    if (msg_obj.get("tool_calls")) |tcs_value| {
        if (tcs_value == .array and tcs_value.array.items.len > 0) {
            const items = try arena.alloc(turn.ToolCall, tcs_value.array.items.len);
            for (tcs_value.array.items, 0..) |item, i| {
                if (item != .object) return error.UnexpectedResponseShape;
                const tc = item.object;
                const id = (tc.get("id") orelse return error.UnexpectedResponseShape);
                if (id != .string) return error.UnexpectedResponseShape;
                const func = tc.get("function") orelse return error.UnexpectedResponseShape;
                if (func != .object) return error.UnexpectedResponseShape;
                const fn_obj = func.object;
                const name_v = fn_obj.get("name") orelse return error.UnexpectedResponseShape;
                if (name_v != .string) return error.UnexpectedResponseShape;
                const args_v = fn_obj.get("arguments") orelse return error.UnexpectedResponseShape;
                if (args_v != .string) return error.UnexpectedResponseShape;

                // Dupe into the arena: parser borrows from `body`, but the
                // arena's lifetime matches the turn so this is safe.
                items[i] = .{
                    .id = try arena.dupe(u8, id.string),
                    .name = try arena.dupe(u8, name_v.string),
                    .args_json = try arena.dupe(u8, args_v.string),
                };
            }
            return .{
                .reply = .{ .response = .{ .tool_calls = items } },
                .usage = usage,
            };
        }
    }

    const content = msg_obj.get("content") orelse return error.UnexpectedResponseShape;
    if (content != .string) return error.UnexpectedResponseShape;
    const text = try arena.dupe(u8, content.string);
    return .{
        .reply = .{ .response = .{ .final_text = text } },
        .usage = usage,
    };
}

// -----------------------------------------------------------------------
// HTTPS POST (mirrors the anthropic client's pattern)
// -----------------------------------------------------------------------

fn post(arena: std.mem.Allocator, config: Config, body: []const u8) ![]u8 {
    const uri = try std.Uri.parse(config.base_url);

    var io_backend = std.Io.Threaded.init(arena, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var client = std.http.Client{ .allocator = arena, .io = io };
    defer client.deinit();

    const now = std.Io.Clock.real.now(io);
    client.ca_bundle.rescan(arena, io, now) catch return error.CertificateBundleLoadFailure;
    client.now = now;

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse return error.UnsupportedProtocol;
    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = try uri.getHost(&host_buf);
    const connection = try client.connectTcpOptions(.{
        .host = host,
        .port = uri.port orelse switch (protocol) {
            .plain => 80,
            .tls => 443,
        },
        .protocol = protocol,
        .timeout = .none,
    });

    const auth_header = try std.fmt.allocPrint(arena, "Bearer {s}", .{config.api_key});
    const extra_headers = [_]std.http.Header{
        .{ .name = "authorization", .value = auth_header },
        .{ .name = "content-type", .value = "application/json" },
        .{ .name = "accept", .value = "application/json" },
    };

    var req = try client.request(.POST, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        .extra_headers = &extra_headers,
    });
    defer req.deinit();

    req.transfer_encoding = .{ .content_length = body.len };
    var req_body = try req.sendBodyUnflushed(&.{});
    try req_body.writer.writeAll(body);
    try req_body.end();
    try req.connection.?.flush();

    var response = try req.receiveHead(&.{});
    if (response.head.status != .ok) return ClientError.HttpNotOk;

    var read_buf: [64]u8 = undefined;
    var reader = response.reader(&read_buf);
    return try reader.allocRemaining(arena, .limited(max_response_body_bytes));
}

// -----------------------------------------------------------------------
// JSON string escape (local copy; the anthropic provider has a parallel
// helper but cross-importing across providers/ would invert the layering).
// -----------------------------------------------------------------------

fn writeJsonString(writer: anytype, s: []const u8) !void {
    try writer.writeByte('"');
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x00...0x08, 0x0b...0x0c, 0x0e...0x1f => {
                try writer.print("\\u{x:0>4}", .{@as(u16, c)});
            },
            else => try writer.writeByte(c),
        }
    }
    try writer.writeByte('"');
}

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

const testing = std.testing;

test "buildRequestBody: first turn carries system + one user message and no tools" {
    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(testing.allocator);
    try transcript.append(testing.allocator, .{ .user_text = "add a GET route" });

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const body = try buildRequestBody(arena.allocator(), .{
        .api_key = "test-fixture-key",
        .system_prompt = "you are a zigts expert",
    }, &transcript, null);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, body, .{});
    defer parsed.deinit();

    const root = parsed.value.object;
    try testing.expectEqualStrings(default_model, root.get("model").?.string);
    try testing.expect(root.get("tools") == null);
    try testing.expect(!root.get("stream").?.bool);

    const msgs = root.get("messages").?.array.items;
    try testing.expectEqual(@as(usize, 2), msgs.len);
    try testing.expectEqualStrings("system", msgs[0].object.get("role").?.string);
    try testing.expectEqualStrings("you are a zigts expert", msgs[0].object.get("content").?.string);
    try testing.expectEqualStrings("user", msgs[1].object.get("role").?.string);
    try testing.expectEqualStrings("add a GET route", msgs[1].object.get("content").?.string);
}

test "buildRequestBody: tool-use and tool-result entries serialize round-trip into OpenAI shape" {
    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(testing.allocator);

    const calls = [_]turn.ToolCall{
        .{ .id = "call_1", .name = "zigts_expert_meta", .args_json = "{}" },
    };
    try transcript.append(testing.allocator, .{ .user_text = "inspect" });
    try transcript.append(testing.allocator, .{ .assistant_tool_use = &calls });
    try transcript.append(testing.allocator, .{ .tool_result = .{
        .tool_use_id = "call_1",
        .tool_name = "zigts_expert_meta",
        .ok = true,
        .llm_text = "{\"version\":\"x\"}",
    } });

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const body = try buildRequestBody(arena.allocator(), .{
        .api_key = "k",
        .system_prompt = "s",
    }, &transcript, null);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, body, .{});
    defer parsed.deinit();
    const msgs = parsed.value.object.get("messages").?.array.items;
    // [system, user, assistant w/ tool_calls, tool]
    try testing.expectEqual(@as(usize, 4), msgs.len);

    try testing.expectEqualStrings("assistant", msgs[2].object.get("role").?.string);
    const tcs = msgs[2].object.get("tool_calls").?.array.items;
    try testing.expectEqualStrings("call_1", tcs[0].object.get("id").?.string);
    try testing.expectEqualStrings("function", tcs[0].object.get("type").?.string);
    try testing.expectEqualStrings("zigts_expert_meta", tcs[0].object.get("function").?.object.get("name").?.string);

    try testing.expectEqualStrings("tool", msgs[3].object.get("role").?.string);
    try testing.expectEqualStrings("call_1", msgs[3].object.get("tool_call_id").?.string);
}

test "parseResponse: final_text path extracts content" {
    const body =
        \\{"id":"x","choices":[{"index":0,"message":{"role":"assistant","content":"hello world"},"finish_reason":"stop"}],"usage":{"prompt_tokens":12,"completion_tokens":3,"total_tokens":15}}
    ;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const result = try parseResponse(arena.allocator(), body);
    switch (result.reply.response) {
        .final_text => |t| try testing.expectEqualStrings("hello world", t),
        else => return error.TestFailed,
    }
    try testing.expectEqual(@as(u64, 12), result.usage.input_tokens);
    try testing.expectEqual(@as(u64, 3), result.usage.output_tokens);
}

test "parseResponse: tool_calls path lifts id + name + arguments" {
    const body =
        \\{"choices":[{"message":{"role":"assistant","content":null,"tool_calls":[{"id":"call_abc","type":"function","function":{"name":"zigts_expert_meta","arguments":"{}"}}]},"finish_reason":"tool_calls"}]}
    ;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const result = try parseResponse(arena.allocator(), body);
    switch (result.reply.response) {
        .tool_calls => |calls| {
            try testing.expectEqual(@as(usize, 1), calls.len);
            try testing.expectEqualStrings("call_abc", calls[0].id);
            try testing.expectEqualStrings("zigts_expert_meta", calls[0].name);
            try testing.expectEqualStrings("{}", calls[0].args_json);
        },
        else => return error.TestFailed,
    }
}

test "writeToolsArray: wraps registry entries in OpenAI tool shape" {
    const echo_tool: registry_mod.ToolDef = .{
        .name = "echo",
        .label = "Echo",
        .description = "Concatenate args with spaces",
        .input_schema = "{\"type\":\"object\",\"properties\":{}}",
        .decode_json = registry_mod.helpers.decodeNoArgs,
        .execute = stubExecute,
    };
    var registry: registry_mod.Registry = .{};
    defer registry.deinit(testing.allocator);
    try registry.register(testing.allocator, echo_tool);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeToolsArray(&aw.writer, &registry);
    buf = aw.toArrayList();

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, buf.items, .{});
    defer parsed.deinit();
    const items = parsed.value.array.items;
    try testing.expectEqual(@as(usize, 1), items.len);
    try testing.expectEqualStrings("function", items[0].object.get("type").?.string);
    try testing.expectEqualStrings("echo", items[0].object.get("function").?.object.get("name").?.string);
}

fn stubExecute(_: std.mem.Allocator, _: []const []const u8) anyerror!registry_mod.ToolResult {
    return error.NotImplemented;
}

test "Client.asModelClient exposes the loop-facing request signature" {
    var client = Client.init(.{ .api_key = "k", .system_prompt = "s" });
    const mc = client.asModelClient();
    try testing.expect(mc.context == @as(*anyopaque, @ptrCast(&client)));
    try testing.expect(@intFromPtr(mc.request_fn) != 0);
}
