//! HTTPS wire layer for the Anthropic Messages API.

const std = @import("std");
const builtin = @import("builtin");
const loop = @import("../../loop.zig");
const turn = @import("../../turn.zig");
const transcript_mod = @import("../../transcript.zig");
const request_mod = @import("request.zig");
const sse_parser = @import("sse_parser.zig");
const response_assembler = @import("response_assembler.zig");
const apply_edit = @import("apply_edit.zig");
const http_errors = @import("../http_errors.zig");
const cassette_record = @import("../cassette_record.zig");

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
    /// When set, every model roundtrip's raw SSE body is teed to a cassette
    /// under `<record_dir>/<record_scenario>/step_<N>.jsonl`, one per call.
    /// Used by the codegen-eval recorder to capture a faithful baseline that
    /// the replay client plays back deterministically. null disables recording
    /// (the default), so the live path is unchanged for normal sessions.
    record_dir: ?[]const u8 = null,
    record_scenario: []const u8 = "",
    record_step: usize = 0,

    pub fn init(config: Config) Client {
        return .{ .config = config };
    }

    /// Point this client at a per-scenario cassette directory and reset the
    /// step counter. The recorder calls this once per corpus case.
    pub fn enableRecording(self: *Client, dir: []const u8, scenario: []const u8) void {
        self.record_dir = dir;
        self.record_scenario = scenario;
        self.record_step = 0;
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
        const response_body = try postAnthropic(arena, self.config, body);
        if (self.record_dir) |dir| {
            // Best-effort: a recording failure must never break a live turn.
            recordCassette(arena, dir, self.record_scenario, self.record_step, self.config.model, response_body) catch {};
            self.record_step += 1;
        }
        const event_list = try sse_parser.parseAll(arena, response_body);
        const outcome = try response_assembler.assemble(arena, event_list);
        const reply = try apply_edit.maybeRemap(arena, outcome.reply, outcome.stop_reason);
        return .{ .reply = reply, .usage = outcome.usage, .stop_reason = outcome.stop_reason };
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

/// Tee one roundtrip's SSE body to `<dir>/<scenario>/step_<step>.jsonl`. The
/// cassette stores the stream verbatim; replay parses it through the same
/// sse_parser/response_assembler this client uses, so the recorded reply and
/// the live reply are identical bytes.
fn recordCassette(
    arena: std.mem.Allocator,
    dir: []const u8,
    scenario: []const u8,
    step: usize,
    model: []const u8,
    sse_body: []const u8,
) !void {
    const scenario_dir = try std.fs.path.join(arena, &.{ dir, scenario });
    var io_backend = std.Io.Threaded.init(arena, .{ .environ = .empty });
    defer io_backend.deinit();
    try std.Io.Dir.createDirPath(std.Io.Dir.cwd(), io_backend.io(), scenario_dir);

    const path = try std.fmt.allocPrint(arena, "{s}/step_{d}.jsonl", .{ scenario_dir, step });
    try cassette_record.writeCassette(arena, path, sse_body, .{
        .provider = .anthropic,
        .scenario = scenario,
        .stream = true,
        .model = model,
    });
}

fn postAnthropic(
    arena: std.mem.Allocator,
    config: Config,
    body: []const u8,
) ![]u8 {
    const uri = try std.Uri.parse(config.base_url);

    var io_backend = std.Io.Threaded.init(arena, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var client = std.http.Client{
        .allocator = arena,
        .io = io,
    };
    defer client.deinit();

    // std.http.Client's `request()` auto-populates `now` + `ca_bundle`
    // together when `now == null`, but we pre-connect below via
    // `connectTcpOptions` which performs the TLS handshake before
    // `request()` runs. Do both ourselves: scan the system trust store
    // and stamp `now` so the TLS handshake has what it needs.
    const now = std.Io.Clock.real.now(io);
    client.ca_bundle.rescan(arena, io, now) catch return error.CertificateBundleLoadFailure;
    client.now = now;

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse return error.UnsupportedProtocol;
    var host_buf: [std.Io.net.HostName.max_len]u8 = undefined;
    const host = try uri.getHost(&host_buf);
    const connection = try http_errors.connect(&client, host, uri.port, protocol);

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
        // std.http.Client otherwise advertises `accept-encoding: gzip,
        // deflate` itself; `.omit` asks for an identity (uncompressed) body.
        // We also decode transparently below, so a proxy that compresses
        // anyway is still handled.
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

    // Decode `Content-Encoding`/`Transfer-Encoding` transparently. The plain
    // `response.reader()` returns the raw (still-chunked, still-compressed)
    // body; feeding a gzip stream to the SSE parser splits on stray newline
    // bytes and surfaces as a bogus `MalformedSse`. `readerDecompressing`
    // handles gzip/deflate/zstd; an identity body passes straight through.
    var transfer_buf: [4096]u8 = undefined;
    var decompress: std.http.Decompress = undefined;
    var decompress_buf: [std.compress.flate.max_window_len]u8 = undefined;
    const reader = response.readerDecompressing(&transfer_buf, &decompress, &decompress_buf);
    const response_body = try http_errors.readBody(reader, arena, max_response_body_bytes);

    if (status != .ok) {
        // The success path is an SSE stream; an error is a small JSON body
        // carrying the real reason (bad key, unknown model, rate limit). Map the
        // status to a typed error first: when it carries one-line remediation the
        // catch site prints that actionable message, so emitting the raw body here
        // (during the request, ahead of the catch site) would only bury it. Keep
        // the raw `err` line solely for the unclassified `HttpNotOk` fallthrough,
        // where the body is the only signal the user gets.
        const err = http_errors.classify(@intFromEnum(status), response_body);
        if (!builtin.is_test and loop.providerErrorRemediation(err) == null) {
            std.log.err("anthropic API: HTTP {d} {s}: {s}", .{
                @intFromEnum(status),
                status.phrase() orelse "",
                response_body,
            });
        }
        return err;
    }

    return response_body;
}

const testing = std.testing;

test "buildRequestBody: emits structured messages with model, system, user, no tools" {
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
        .api_key = "test-fixture-key",
        .system_prompt = "s",
    });
    const mc = client.asModelClient();
    try testing.expect(mc.context == @as(*anyopaque, @ptrCast(&client)));
    try testing.expect(@intFromPtr(mc.request_fn) != 0);
}
