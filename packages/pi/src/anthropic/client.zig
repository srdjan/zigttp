//! HTTPS wire layer for the Anthropic Messages API. Implements a
//! `loop.ModelClient` by POSTing a streaming request to `api.anthropic.com`,
//! reading the full SSE response body, feeding it through the slice-2
//! parser, and assembling the events into a `turn.ModelReply`.
//!
//! This slice reads the body in full before parsing rather than streaming
//! events as they arrive. That simplification costs incremental UI
//! feedback for long replies but avoids writing a streaming
//! `RecordIterator` variant before the TUI can do anything useful with
//! partial events. Real SSE streaming is a later slice.
//!
//! Unit tests cover header assembly and the end-to-end glue through
//! synthetic bodies. The live-network test is gated on both
//! `ANTHROPIC_API_KEY` and `ZIGTTP_EXPERT_LIVE=1` so an ambient key in a
//! shell can't accidentally bill tokens during `zig build test`.

const std = @import("std");
const loop = @import("../loop.zig");
const turn = @import("../turn.zig");
const request_mod = @import("request.zig");
const sse_parser = @import("sse_parser.zig");
const response_assembler = @import("response_assembler.zig");
const apply_edit = @import("apply_edit.zig");

const default_base_url = "https://api.anthropic.com/v1/messages";
const default_anthropic_version = "2023-06-01";
const max_response_body_bytes: usize = 16 * 1024 * 1024;

pub const Config = struct {
    api_key: []const u8,
    system_prompt: []const u8,
    model: []const u8 = request_mod.default_model,
    max_tokens: u32 = request_mod.default_max_tokens,
    tools_json: ?[]const u8 = null,
    base_url: []const u8 = default_base_url,
    anthropic_version: []const u8 = default_anthropic_version,
};

pub const ClientError = error{
    HttpNotOk,
};

pub const Client = struct {
    config: Config,

    pub fn init(config: Config) Client {
        return .{ .config = config };
    }

    pub fn asModelClient(self: *Client) loop.ModelClient {
        return .{
            .context = self,
            .request_fn = requestFn,
        };
    }

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        user_text: []const u8,
    ) anyerror!turn.ModelReply {
        const self: *Client = @ptrCast(@alignCast(ctx));
        return self.sendTurn(arena, user_text);
    }

    pub fn sendTurn(
        self: *Client,
        arena: std.mem.Allocator,
        user_text: []const u8,
    ) !turn.ModelReply {
        const body = try buildRequestBody(arena, self.config, user_text);
        const response_body = try postAnthropic(arena, self.config, body);
        const event_list = try sse_parser.parseAll(arena, response_body);
        const outcome = try response_assembler.assemble(arena, event_list);
        return try apply_edit.maybeRemap(arena, outcome.reply);
    }
};

pub fn buildRequestBody(
    arena: std.mem.Allocator,
    config: Config,
    user_text: []const u8,
) ![]u8 {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    var aw: std.Io.Writer.Allocating = .fromArrayList(arena, &buf);
    try request_mod.writeRequestBody(&aw.writer, .{
        .model = config.model,
        .max_tokens = config.max_tokens,
        .system_prompt = config.system_prompt,
        .user_text = user_text,
        .tools_json = config.tools_json,
        .stream = true,
    });
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(arena);
}

fn postAnthropic(
    arena: std.mem.Allocator,
    config: Config,
    body: []const u8,
) ![]u8 {
    const uri = try std.Uri.parse(config.base_url);

    var io_backend = std.Io.Threaded.init(arena, .{ .environ = .empty });
    defer io_backend.deinit();

    var client = std.http.Client{
        .allocator = arena,
        .io = io_backend.io(),
    };
    defer client.deinit();

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

    const extra_headers = [_]std.http.Header{
        .{ .name = "x-api-key", .value = config.api_key },
        .{ .name = "anthropic-version", .value = config.anthropic_version },
        .{ .name = "content-type", .value = "application/json" },
        .{ .name = "accept", .value = "text/event-stream" },
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

// ---------------------------------------------------------------------------
// Tests
//
// These tests exercise everything except the actual HTTPS call. The live
// end-to-end smoke test is gated on both an API key and an explicit
// opt-in env var so `zig build test` is safe to run in any shell that
// happens to have ANTHROPIC_API_KEY exported.
// ---------------------------------------------------------------------------

const testing = std.testing;

test "buildRequestBody: emits valid JSON with model, system, user, no tools" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const body = try buildRequestBody(arena.allocator(), .{
        .api_key = "sk-ant-test",
        .system_prompt = "you are a zigts expert",
    }, "add a GET route");

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, body, .{});
    defer parsed.deinit();

    const root = parsed.value.object;
    try testing.expectEqualStrings(request_mod.default_model, root.get("model").?.string);
    try testing.expect(root.get("tools") == null);

    const system = root.get("system").?.array.items;
    try testing.expectEqual(@as(usize, 1), system.len);
    try testing.expectEqualStrings("you are a zigts expert", system[0].object.get("text").?.string);

    const msgs = root.get("messages").?.array.items;
    try testing.expectEqualStrings("add a GET route", msgs[0].object.get("content").?.string);
}

test "buildRequestBody: includes tools_json verbatim when provided" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const tools = "[{\"name\":\"nop\",\"description\":\"d\",\"input_schema\":{}}]";
    const body = try buildRequestBody(arena.allocator(), .{
        .api_key = "k",
        .system_prompt = "s",
        .tools_json = tools,
    }, "u");

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, body, .{});
    defer parsed.deinit();

    const tools_array = parsed.value.object.get("tools").?.array.items;
    try testing.expectEqual(@as(usize, 1), tools_array.len);
    try testing.expectEqualStrings("nop", tools_array[0].object.get("name").?.string);
}

test "Client.asModelClient: returns a ModelClient with a live function pointer" {
    var client = Client.init(.{
        .api_key = "sk-ant-test",
        .system_prompt = "s",
    });
    const mc = client.asModelClient();
    try testing.expect(mc.context == @as(*anyopaque, @ptrCast(&client)));
    try testing.expect(@intFromPtr(mc.request_fn) != 0);
}

fn envVar(name_z: [:0]const u8) ?[]const u8 {
    const raw = std.c.getenv(name_z) orelse return null;
    return std.mem.sliceTo(raw, 0);
}

test "live: round-trip one turn against api.anthropic.com" {
    // Skip unless both env vars are set. This keeps the default test run
    // free of network dependencies and protects against surprise billing
    // from an ambient ANTHROPIC_API_KEY in the developer's shell.
    const opt_in = envVar("ZIGTTP_EXPERT_LIVE") orelse return error.SkipZigTest;
    if (!std.mem.eql(u8, opt_in, "1")) return error.SkipZigTest;
    const api_key = envVar("ANTHROPIC_API_KEY") orelse return error.SkipZigTest;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var client = Client.init(.{
        .api_key = api_key,
        .system_prompt = "You are a terse assistant. Reply with a single short sentence.",
        .max_tokens = 128,
    });

    const reply = try client.sendTurn(arena.allocator(), "Say hello in three words.");
    try testing.expect(reply == .text);
    try testing.expect(reply.text.len > 0);
}
