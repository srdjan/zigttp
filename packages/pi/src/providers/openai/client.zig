//! Streaming HTTPS wire layer for the OpenAI Responses API.
//!
//! Mirrors the shape of `providers/anthropic/client.zig`: post a request,
//! parse the streamed SSE body, fold the events into a `turn.AssistantReply`
//! identical to the Anthropic one. The loop sees one provider-shaped
//! result regardless of which backend produced it.
//!
//! The earlier non-streaming Chat Completions implementation lived here. The
//! Responses API streaming endpoint keeps the OpenAI path aligned with
//! Anthropic's "watch tool calls assemble in real time" experience.

const std = @import("std");
const builtin = @import("builtin");
const loop = @import("../../loop.zig");
const turn = @import("../../turn.zig");
const transcript_mod = @import("../../transcript.zig");
const registry_mod = @import("../../registry/registry.zig");
const sse_parser = @import("sse_parser.zig");
const response_assembler = @import("response_assembler.zig");
const http_errors = @import("../http_errors.zig");

const default_base_url = "https://api.openai.com/v1/responses";
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
        const event_list = try sse_parser.parseAll(arena, response_body);
        const outcome = try response_assembler.assemble(arena, event_list);
        return .{ .reply = outcome.reply, .usage = outcome.usage };
    }
};

// -----------------------------------------------------------------------
// Request body
//
// The Responses API uses a flatter, "input + tools" shape compared to the
// Chat Completions one we used before. The system prompt is hoisted into a
// top-level `instructions` field; everything else (user, assistant, tool
// calls, tool results) becomes an entry in the `input` array.
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
    try w.print(",\"max_output_tokens\":{d}", .{config.max_tokens});
    try w.writeAll(",\"stream\":true");
    try w.writeAll(",\"instructions\":");
    try writeJsonString(w, config.system_prompt);

    try w.writeAll(",\"input\":[");
    var first_entry = true;
    for (transcript.entries.items) |*entry| {
        try writeTranscriptEntry(w, entry, &first_entry);
    }
    if (extra_user_text) |body| {
        if (!first_entry) try w.writeByte(',');
        first_entry = false;
        try writeUserMessage(w, body);
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

fn writeUserMessage(w: anytype, body: []const u8) !void {
    try w.writeAll("{\"role\":\"user\",\"content\":[{\"type\":\"input_text\",\"text\":");
    try writeJsonString(w, body);
    try w.writeAll("}]}");
}

fn writeAssistantText(w: anytype, body: []const u8) !void {
    try w.writeAll("{\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":");
    try writeJsonString(w, body);
    try w.writeAll("}]}");
}

fn writeTranscriptEntry(
    w: anytype,
    entry: *const transcript_mod.OwnedEntry,
    first_entry: *bool,
) !void {
    switch (entry.*) {
        .user_text => |body| {
            if (!first_entry.*) try w.writeByte(',');
            first_entry.* = false;
            try writeUserMessage(w, body);
        },
        .system_note => |body| {
            // System notes are surfaced as additional user-role context
            // blocks; the top-level `instructions` field carries the
            // persona prompt exclusively.
            if (!first_entry.*) try w.writeByte(',');
            first_entry.* = false;
            try writeUserMessage(w, body);
        },
        .model_text => |body| {
            if (!first_entry.*) try w.writeByte(',');
            first_entry.* = false;
            try writeAssistantText(w, body);
        },
        .assistant_tool_use => |calls| {
            // The Responses API expects each function call to be its own
            // top-level input item (no wrapping assistant message).
            for (calls) |call| {
                if (!first_entry.*) try w.writeByte(',');
                first_entry.* = false;
                try w.writeAll("{\"type\":\"function_call\",\"call_id\":");
                try writeJsonString(w, call.id);
                try w.writeAll(",\"name\":");
                try writeJsonString(w, call.name);
                try w.writeAll(",\"arguments\":");
                try writeJsonString(w, call.args_json);
                try w.writeByte('}');
            }
        },
        .tool_result => |result| {
            if (!first_entry.*) try w.writeByte(',');
            first_entry.* = false;
            try w.writeAll("{\"type\":\"function_call_output\",\"call_id\":");
            try writeJsonString(w, result.tool_use_id);
            try w.writeAll(",\"output\":");
            try writeJsonString(w, result.llm_text);
            try w.writeByte('}');
        },
        .proof_card, .diagnostic_box, .verified_patch => {},
    }
}

// -----------------------------------------------------------------------
// Tools schema
//
// The Responses API expects each tool to be `{type, name, description,
// parameters}` flat (no nested `function` wrapper that Chat Completions
// used). The signature is unchanged so call sites in agent.zig keep
// working without modification.
// -----------------------------------------------------------------------

pub fn writeToolsArray(writer: anytype, registry: *const registry_mod.Registry) !void {
    try writer.writeByte('[');
    for (registry.list(), 0..) |entry, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"type\":\"function\",\"name\":");
        try writeJsonString(writer, entry.name);
        try writer.writeAll(",\"description\":");
        try writeJsonString(writer, entry.description);
        try writer.writeAll(",\"parameters\":");
        try writer.writeAll(entry.input_schema);
        try writer.writeByte('}');
    }
    try writer.writeByte(']');
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
    const connection = try http_errors.connect(&client, host, uri.port, protocol);

    const auth_header = try std.fmt.allocPrint(arena, "Bearer {s}", .{config.api_key});
    const extra_headers = [_]std.http.Header{
        .{ .name = "authorization", .value = auth_header },
        .{ .name = "content-type", .value = "application/json" },
        .{ .name = "accept", .value = "text/event-stream" },
    };

    var req = try client.request(.POST, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        // Ask for an identity (uncompressed) body; std.http.Client otherwise
        // advertises gzip/deflate itself. We also decode transparently below so
        // a proxy that compresses anyway is still handled.
        .headers = .{ .accept_encoding = .omit },
        .extra_headers = &extra_headers,
    });
    defer req.deinit();

    req.transfer_encoding = .{ .content_length = body.len };
    var req_body = try req.sendBodyUnflushed(&.{});
    try req_body.writer.writeAll(body);
    try req_body.end();
    try req.connection.?.flush();

    var response = try req.receiveHead(&.{});
    const status = response.head.status;

    // Decode Content-Encoding/Transfer-Encoding transparently: a raw gzip/chunked
    // body fed to the SSE parser splits on stray newline bytes and surfaces as a
    // bogus parse error. readerDecompressing handles gzip/deflate/zstd; identity
    // passes straight through.
    var transfer_buf: [4096]u8 = undefined;
    var decompress: std.http.Decompress = undefined;
    var decompress_buf: [std.compress.flate.max_window_len]u8 = undefined;
    const reader = response.readerDecompressing(&transfer_buf, &decompress, &decompress_buf);
    const response_body = try http_errors.readBody(reader, arena, max_response_body_bytes);

    if (status != .ok) {
        // The error body is a small JSON payload carrying the real reason (bad
        // key, exceeded quota, rate limit). Surface it instead of collapsing
        // every failure into a bare HttpNotOk.
        if (!builtin.is_test) {
            std.log.err("openai API: HTTP {d} {s}: {s}", .{
                @intFromEnum(status),
                status.phrase() orelse "",
                response_body,
            });
        }
        return http_errors.classify(@intFromEnum(status), response_body);
    }

    return response_body;
}

// -----------------------------------------------------------------------
// JSON string escape (local copy; the anthropic provider has a parallel
// helper but cross-importing across providers/ would invert the layering).
// -----------------------------------------------------------------------

fn writeJsonString(writer: anytype, s: []const u8) !void {
    try writer.writeByte('"');
    var i: usize = 0;
    while (i < s.len) {
        const c = s[i];
        if (c < 0x80) {
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
            i += 1;
            continue;
        }
        // Emit a multi-byte sequence only if it is a complete, valid UTF-8
        // codepoint. SSE deltas can split a surrogate pair, which the JSON
        // parser decodes to invalid CESU-8 the API rejects ("surrogates not
        // allowed"); replace any invalid byte with U+FFFD so the body stays valid.
        const seq_len = std.unicode.utf8ByteSequenceLength(c) catch {
            try writer.writeAll("\u{FFFD}");
            i += 1;
            continue;
        };
        if (i + seq_len > s.len or !std.unicode.utf8ValidateSlice(s[i .. i + seq_len])) {
            try writer.writeAll("\u{FFFD}");
            i += 1;
            continue;
        }
        try writer.writeAll(s[i .. i + seq_len]);
        i += seq_len;
    }
    try writer.writeByte('"');
}

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

const testing = std.testing;

test "buildRequestBody: first turn carries instructions + one user input item and no tools" {
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
    try testing.expect(root.get("stream").?.bool);
    try testing.expectEqualStrings("you are a zigts expert", root.get("instructions").?.string);

    const input = root.get("input").?.array.items;
    try testing.expectEqual(@as(usize, 1), input.len);
    try testing.expectEqualStrings("user", input[0].object.get("role").?.string);
    const content = input[0].object.get("content").?.array.items;
    try testing.expectEqualStrings("input_text", content[0].object.get("type").?.string);
    try testing.expectEqualStrings("add a GET route", content[0].object.get("text").?.string);
}

test "buildRequestBody: tool-use and tool-result entries serialize as Responses-API items" {
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
    const items = parsed.value.object.get("input").?.array.items;
    // [user, function_call, function_call_output]
    try testing.expectEqual(@as(usize, 3), items.len);

    try testing.expectEqualStrings("user", items[0].object.get("role").?.string);

    try testing.expectEqualStrings("function_call", items[1].object.get("type").?.string);
    try testing.expectEqualStrings("call_1", items[1].object.get("call_id").?.string);
    try testing.expectEqualStrings("zigts_expert_meta", items[1].object.get("name").?.string);
    try testing.expectEqualStrings("{}", items[1].object.get("arguments").?.string);

    try testing.expectEqualStrings("function_call_output", items[2].object.get("type").?.string);
    try testing.expectEqualStrings("call_1", items[2].object.get("call_id").?.string);
    try testing.expectEqualStrings("{\"version\":\"x\"}", items[2].object.get("output").?.string);
}

test "buildRequestBody: multiple tool calls in one assistant turn unroll into separate items" {
    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(testing.allocator);
    const calls = [_]turn.ToolCall{
        .{ .id = "call_a", .name = "tool_a", .args_json = "{}" },
        .{ .id = "call_b", .name = "tool_b", .args_json = "{\"x\":1}" },
    };
    try transcript.append(testing.allocator, .{ .user_text = "u" });
    try transcript.append(testing.allocator, .{ .assistant_tool_use = &calls });

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const body = try buildRequestBody(arena.allocator(), .{
        .api_key = "k",
        .system_prompt = "s",
    }, &transcript, null);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, body, .{});
    defer parsed.deinit();
    const items = parsed.value.object.get("input").?.array.items;
    // [user, function_call_a, function_call_b]
    try testing.expectEqual(@as(usize, 3), items.len);
    try testing.expectEqualStrings("call_a", items[1].object.get("call_id").?.string);
    try testing.expectEqualStrings("call_b", items[2].object.get("call_id").?.string);
}

test "writeToolsArray: wraps registry entries in flat Responses-API tool shape" {
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
    try testing.expectEqualStrings("echo", items[0].object.get("name").?.string);
    try testing.expectEqualStrings("Concatenate args with spaces", items[0].object.get("description").?.string);
    // The schema is inlined as-is.
    try testing.expect(items[0].object.get("parameters").? == .object);
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
