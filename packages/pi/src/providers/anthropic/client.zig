//! HTTPS wire layer for the Anthropic Messages API.

const std = @import("std");
const loop = @import("../../loop.zig");
const turn = @import("../../turn.zig");
const transcript_mod = @import("../../transcript.zig");
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
        return .{ .context = self, .request_fn = requestFn };
    }

    fn requestFn(
        ctx: *anyopaque,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) anyerror!turn.AssistantReply {
        const self: *Client = @ptrCast(@alignCast(ctx));
        return self.sendTurn(arena, transcript, extra_user_text);
    }

    pub fn sendTurn(
        self: *Client,
        arena: std.mem.Allocator,
        transcript: *const transcript_mod.Transcript,
        extra_user_text: ?[]const u8,
    ) !turn.AssistantReply {
        const body = try buildRequestBody(arena, self.config, transcript, extra_user_text);
        const response_body = try postAnthropic(arena, self.config, body);
        const event_list = try sse_parser.parseAll(arena, response_body);
        const outcome = try response_assembler.assemble(arena, event_list);
        return try apply_edit.maybeRemap(arena, outcome.reply);
    }
};

pub fn buildRequestBody(
    arena: std.mem.Allocator,
    config: Config,
    transcript: *const transcript_mod.Transcript,
    extra_user_text: ?[]const u8,
) ![]u8 {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    var aw: std.Io.Writer.Allocating = .fromArrayList(arena, &buf);
    try request_mod.writeRequestBody(&aw.writer, arena, .{
        .model = config.model,
        .max_tokens = config.max_tokens,
        .system_prompt = config.system_prompt,
        .transcript = transcript,
        .extra_user_text = extra_user_text,
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

const testing = std.testing;

test "buildRequestBody: emits structured messages with model, system, user, no tools" {
    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(testing.allocator);
    try transcript.append(testing.allocator, .{ .user_text = "add a GET route" });

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const body = try buildRequestBody(arena.allocator(), .{
        .api_key = "sk-ant-test",
        .system_prompt = "you are a zigts expert",
    }, &transcript, null);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, body, .{});
    defer parsed.deinit();

    const root = parsed.value.object;
    try testing.expectEqualStrings(request_mod.default_model, root.get("model").?.string);
    try testing.expect(root.get("tools") == null);

    const system = root.get("system").?.array.items;
    try testing.expectEqualStrings("you are a zigts expert", system[0].object.get("text").?.string);

    const msgs = root.get("messages").?.array.items;
    try testing.expectEqualStrings("user", msgs[0].object.get("role").?.string);
}

test "buildRequestBody: includes tools_json verbatim when provided" {
    var transcript: transcript_mod.Transcript = .{};
    defer transcript.deinit(testing.allocator);
    try transcript.append(testing.allocator, .{ .user_text = "u" });

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const body = try buildRequestBody(arena.allocator(), .{
        .api_key = "k",
        .system_prompt = "s",
        .tools_json = "[{\"name\":\"nop\",\"description\":\"d\",\"input_schema\":{}}]",
    }, &transcript, null);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, body, .{});
    defer parsed.deinit();
    const tools_array = parsed.value.object.get("tools").?.array.items;
    try testing.expectEqualStrings("nop", tools_array[0].object.get("name").?.string);
}

test "Client.asModelClient exposes the new request signature" {
    var client = Client.init(.{
        .api_key = "sk-ant-test",
        .system_prompt = "s",
    });
    const mc = client.asModelClient();
    try testing.expect(mc.context == @as(*anyopaque, @ptrCast(&client)));
    try testing.expect(@intFromPtr(mc.request_fn) != 0);
}
