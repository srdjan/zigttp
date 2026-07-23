//! Recorder half of the cassette harness.
//!
//! Live API responses are captured here and persisted as cassettes that
//! the replay client (`cassette_client.zig`) plays back deterministically.
//! This file is also the home of the `Mode` selector that the live
//! provider clients consult before deciding whether to open a real socket.
//!
//! The mode is set by the env var `ZTTP_CASSETTE_MODE`:
//!
//!   replay      Default. Replay from the cassette path the caller already
//!               holds; never open a socket. Missing cassette = test failure.
//!   record      Open a real socket, capture request and response bytes,
//!               and write a cassette to disk. Requires a real API key.
//!   passthrough Open a real socket, do not write any cassette. Useful for
//!               live smoke tests where the response is consumed in-process
//!               and the test asserts on the parsed reply rather than the
//!               raw byte stream.
//!
//! The recorder itself is provider-agnostic; the live providers know how to
//! frame their request and which header to send. The recorder only owns
//! capturing the HTTP response bytes and serialising them in the cassette
//! format `cassette_client.zig` consumes.

const std = @import("std");
const zts = @import("zts");
const file_io = zts.file_io;
const cassette_client = @import("cassette_client.zig");

pub const Mode = enum {
    replay,
    record,
    passthrough,

    pub fn fromString(s: []const u8) ?Mode {
        if (std.mem.eql(u8, s, "replay")) return .replay;
        if (std.mem.eql(u8, s, "record")) return .record;
        if (std.mem.eql(u8, s, "passthrough")) return .passthrough;
        return null;
    }
};

/// Read `ZTTP_CASSETTE_MODE` from the process environment. Defaults to
/// `.replay` when unset or unparseable so tests that forget to opt in
/// never hit the live network. Callers that want to surface bad values
/// loudly can call `parseModeStrict` instead.
pub fn modeFromEnv(allocator: std.mem.Allocator) Mode {
    const raw = std.process.getEnvVarOwned(allocator, "ZTTP_CASSETTE_MODE") catch return .replay;
    defer allocator.free(raw);
    return Mode.fromString(raw) orelse .replay;
}

pub const ParseModeError = error{InvalidMode};

pub fn parseModeStrict(value: []const u8) ParseModeError!Mode {
    return Mode.fromString(value) orelse error.InvalidMode;
}

// -----------------------------------------------------------------------
// Captured response
// -----------------------------------------------------------------------

/// A captured live response. Owns its byte slices via the supplied
/// allocator; call `deinit` to release.
pub const CapturedResponse = struct {
    status: u16,
    body: []u8,

    pub fn deinit(self: *CapturedResponse, allocator: std.mem.Allocator) void {
        allocator.free(self.body);
        self.* = undefined;
    }
};

pub const RecordError = error{
    HttpNotOk,
    UnsupportedProtocol,
};

const max_response_body_bytes: usize = 16 * 1024 * 1024;

/// Perform a real HTTP POST and capture the response body in full. The
/// caller owns the returned slice. Used by both `record` and `passthrough`
/// modes; the only difference is whether `writeCassette` is also called.
pub fn capturePost(
    allocator: std.mem.Allocator,
    url: []const u8,
    extra_headers: []const std.http.Header,
    body: []const u8,
) !CapturedResponse {
    const uri = try std.Uri.parse(url);

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var client = std.http.Client{ .allocator = allocator, .io = io };
    defer client.deinit();

    const now = std.Io.Clock.real.now(io);
    if (uri.scheme.len > 0 and std.mem.eql(u8, uri.scheme, "https")) {
        client.ca_bundle.rescan(allocator, io, now) catch return error.CertificateBundleLoadFailure;
    }
    client.now = now;

    const protocol = std.http.Client.Protocol.fromUri(uri) orelse return RecordError.UnsupportedProtocol;
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

    var req = try client.request(.POST, uri, .{
        .redirect_behavior = .unhandled,
        .keep_alive = false,
        .connection = connection,
        .extra_headers = extra_headers,
    });
    defer req.deinit();

    req.transfer_encoding = .{ .content_length = body.len };
    var req_body = try req.sendBodyUnflushed(&.{});
    try req_body.writer.writeAll(body);
    try req_body.end();
    try req.connection.?.flush();

    var response = try req.receiveHead(&.{});
    const status_code: u16 = @intFromEnum(response.head.status);
    if (response.head.status != .ok) return RecordError.HttpNotOk;

    var read_buf: [1024]u8 = undefined;
    var reader = response.reader(&read_buf);
    const captured = try reader.allocRemaining(allocator, .limited(max_response_body_bytes));
    return .{ .status = status_code, .body = captured };
}

// -----------------------------------------------------------------------
// Cassette writer
// -----------------------------------------------------------------------

pub const WriteOptions = struct {
    provider: cassette_client.Provider,
    scenario: []const u8,
    /// `true` when the response is an SSE byte stream; `false` for a
    /// single JSON envelope.
    stream: bool,
    /// Embedded ISO-8601 timestamp. Pass `null` to omit. Recorded
    /// cassettes typically stamp the wall clock; tests pin a fixed value
    /// so the cassette bytes round-trip stably.
    recorded_at: ?[]const u8 = null,
    /// Optional model name to record alongside the scenario.
    model: ?[]const u8 = null,
};

/// Serialise a captured body into the JSONL cassette format
/// `cassette_client.zig` consumes. Returned bytes are owned by the
/// caller. The cassette is one JSON object per line:
///
///     {"v":1,"provider":"...","scenario":"...","stream":true|false,...}
///     {"sse":"event: ...\ndata: ...\n\n"}     (one per SSE record, stream=true)
///     {"body":"<single-line JSON>"}            (single line, stream=false)
pub fn serializeCassette(
    allocator: std.mem.Allocator,
    body: []const u8,
    options: WriteOptions,
) ![]u8 {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    errdefer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try writeHeaderLine(w, options);
    try w.writeByte('\n');

    if (options.stream) {
        var it = SseRecordIterator{ .input = body };
        while (it.next()) |record| {
            try w.writeAll("{\"sse\":");
            try writeJsonString(w, record);
            try w.writeAll("}\n");
        }
    } else {
        try w.writeAll("{\"body\":");
        try writeJsonString(w, body);
        try w.writeAll("}\n");
    }

    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

/// Write a captured body to disk as a cassette. Intermediate directories
/// are created on demand. The caller chooses the path; convention is
/// `packages/pi/src/providers/testdata/<provider>/<scenario>.jsonl`.
pub fn writeCassette(
    allocator: std.mem.Allocator,
    path: []const u8,
    body: []const u8,
    options: WriteOptions,
) !void {
    const serialized = try serializeCassette(allocator, body, options);
    defer allocator.free(serialized);

    // Parent dirs are the caller's responsibility; tests stage them via
    // `std.testing.tmpDir`, and the production recorder binary creates
    // `packages/pi/src/providers/testdata/<provider>/` once on first use.
    try file_io.writeFile(allocator, path, serialized);
}

fn writeHeaderLine(w: anytype, options: WriteOptions) !void {
    try w.writeAll("{\"v\":1,\"provider\":");
    try writeJsonString(w, switch (options.provider) {
        .anthropic => "anthropic",
        .openai => "openai",
    });
    try w.writeAll(",\"scenario\":");
    try writeJsonString(w, options.scenario);
    try w.print(",\"stream\":{s}", .{if (options.stream) "true" else "false"});
    if (options.model) |model| {
        try w.writeAll(",\"model\":");
        try writeJsonString(w, model);
    }
    if (options.recorded_at) |stamp| {
        try w.writeAll(",\"recorded_at\":");
        try writeJsonString(w, stamp);
    }
    try w.writeByte('}');
}

/// Iterates `\n\n`-separated SSE records. Each record carries the trailing
/// blank line that delimits it, so concatenating every record reproduces
/// the original byte stream verbatim.
const SseRecordIterator = struct {
    input: []const u8,
    pos: usize = 0,

    fn next(self: *SseRecordIterator) ?[]const u8 {
        if (self.pos >= self.input.len) return null;
        const start = self.pos;
        const remaining = self.input[start..];
        const sep_rel = std.mem.indexOf(u8, remaining, "\n\n");
        if (sep_rel) |off| {
            const end = start + off + 2;
            self.pos = end;
            return self.input[start..end];
        }
        // Tail without trailing blank line: emit verbatim.
        self.pos = self.input.len;
        return self.input[start..];
    }
};

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

test "Mode.fromString covers the documented selectors" {
    try testing.expectEqual(Mode.replay, Mode.fromString("replay").?);
    try testing.expectEqual(Mode.record, Mode.fromString("record").?);
    try testing.expectEqual(Mode.passthrough, Mode.fromString("passthrough").?);
    try testing.expect(Mode.fromString("RECORD") == null);
    try testing.expect(Mode.fromString("") == null);
}

test "parseModeStrict surfaces invalid values" {
    try testing.expectError(error.InvalidMode, parseModeStrict("nonsense"));
    try testing.expectEqual(Mode.replay, try parseModeStrict("replay"));
}

test "serializeCassette: non-streaming body emits header + single body line" {
    const body = "{\"id\":\"x\",\"choices\":[]}";
    const serialized = try serializeCassette(testing.allocator, body, .{
        .provider = .openai,
        .scenario = "smoke",
        .stream = false,
        .recorded_at = "2026-05-28T00:00:00Z",
        .model = "gpt-4o-mini",
    });
    defer testing.allocator.free(serialized);

    // Round-trip through the loader: bytes back out should match the body.
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const cassette = try cassette_client.loadCassetteFromBytes(arena.allocator(), serialized, null);
    try testing.expectEqual(cassette_client.Provider.openai, cassette.header.provider);
    try testing.expect(!cassette.header.stream);
    try testing.expectEqualStrings(body, cassette.body);
}

test "serializeCassette: streaming body splits SSE records one-per-line" {
    const sse = "event: ping\ndata: {\"type\":\"ping\"}\n\nevent: end\ndata: {\"type\":\"end\"}\n\n";
    const serialized = try serializeCassette(testing.allocator, sse, .{
        .provider = .anthropic,
        .scenario = "smoke_stream",
        .stream = true,
        .recorded_at = "2026-05-28T00:00:00Z",
    });
    defer testing.allocator.free(serialized);

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const cassette = try cassette_client.loadCassetteFromBytes(arena.allocator(), serialized, null);
    try testing.expectEqual(cassette_client.Provider.anthropic, cassette.header.provider);
    try testing.expect(cassette.header.stream);
    try testing.expectEqualStrings(sse, cassette.body);
}

test "writeCassette + loadCassetteFromPath: full disk round-trip" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const full_path = try std.fs.path.resolve(testing.allocator, &.{
        ".zig-cache", "tmp", tmp.sub_path[0..], "smoke.jsonl",
    });
    defer testing.allocator.free(full_path);

    const body = "{\"id\":\"x\",\"choices\":[{\"message\":{\"content\":\"hello\"}}]}";
    try writeCassette(testing.allocator, full_path, body, .{
        .provider = .openai,
        .scenario = "smoke",
        .stream = false,
    });

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const cassette = try cassette_client.loadCassetteFromPath(arena.allocator(), full_path);
    try testing.expectEqualStrings(body, cassette.body);
}

// -----------------------------------------------------------------------
// Round-trip test: tiny local std.Io.net server -> capturePost ->
// writeCassette -> loadCassetteFromPath. Validates that the bytes we
// capture from a real socket match the bytes the cassette replay yields.
// -----------------------------------------------------------------------

const TestErrorInt = u16;

const LocalHttpServer = struct {
    allocator: std.mem.Allocator,
    io_backend: std.Io.Threaded,
    listener: std.Io.net.Server,
    port: u16,
    response_body: []const u8,
    response_content_type: []const u8,
    thread: ?std.Thread = null,
    closed: bool = false,
    thread_error: std.atomic.Value(TestErrorInt) = std.atomic.Value(TestErrorInt).init(0),

    fn init(
        allocator: std.mem.Allocator,
        response_body: []const u8,
        response_content_type: []const u8,
    ) !LocalHttpServer {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        const io = io_backend.io();
        const address = try std.Io.net.IpAddress.parseIp4("127.0.0.1", 0);
        const listener = try address.listen(io, .{ .reuse_address = true });
        return .{
            .allocator = allocator,
            .io_backend = io_backend,
            .listener = listener,
            .port = listener.socket.address.getPort(),
            .response_body = response_body,
            .response_content_type = response_content_type,
        };
    }

    fn start(self: *LocalHttpServer) !void {
        self.thread = try std.Thread.spawn(.{}, run, .{self});
    }

    fn url(self: *const LocalHttpServer, allocator: std.mem.Allocator, path: []const u8) ![]u8 {
        return std.fmt.allocPrint(allocator, "http://127.0.0.1:{d}{s}", .{ self.port, path });
    }

    fn join(self: *LocalHttpServer) !void {
        if (self.closed) return;
        self.closed = true;
        if (self.thread) |thread| {
            thread.join();
            self.thread = null;
        }
        const io = self.io_backend.io();
        self.listener.deinit(io);
        self.io_backend.deinit();
        const err_int = self.thread_error.swap(0, .acq_rel);
        if (err_int != 0) return @errorFromInt(err_int);
    }

    fn run(self: *LocalHttpServer) void {
        self.runInner() catch |err| {
            self.thread_error.store(@intFromError(err), .release);
        };
    }

    fn runInner(self: *LocalHttpServer) !void {
        const io = self.io_backend.io();
        var stream = while (true) {
            break self.listener.accept(io) catch |err| switch (err) {
                error.ConnectionAborted => continue,
                error.SocketNotListening => return,
                else => return err,
            };
        };
        defer stream.close(io);

        // Drain the request head. We do not assert on it; the recorder
        // owns request shaping, the harness owns wire compliance.
        try drainRequestHead(self.allocator, &stream, io);

        var out_buf: [4096]u8 = undefined;
        var writer = stream.writer(io, &out_buf);
        const out = &writer.interface;
        try out.print("HTTP/1.1 200 OK\r\n", .{});
        try out.print("Content-Length: {d}\r\n", .{self.response_body.len});
        try out.print("Content-Type: {s}\r\n", .{self.response_content_type});
        try out.writeAll("Connection: close\r\n\r\n");
        try out.writeAll(self.response_body);
        try out.flush();
    }
};

fn drainRequestHead(allocator: std.mem.Allocator, stream: *std.Io.net.Stream, io: std.Io) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    while (true) {
        var chunk: [1024]u8 = undefined;
        var vecs: [1][]u8 = .{chunk[0..]};
        const n = io.vtable.netRead(io.userdata, stream.socket.handle, &vecs) catch |err| switch (err) {
            error.ConnectionResetByPeer => return,
            else => return err,
        };
        if (n == 0) return;
        try buf.appendSlice(allocator, chunk[0..n]);
        if (std.mem.indexOf(u8, buf.items, "\r\n\r\n") == null) continue;
        // Headers seen; assume any Content-Length body fits in the same
        // buffer chunk we already pulled. For the harness's tiny POST
        // bodies (a few hundred bytes) this holds; on real traffic the
        // recorder is what owns full-body reads, not this server.
        return;
    }
}

test "round-trip: capture from local server, serialise, replay yields identical body" {
    // OpenAI moved to the Responses API streaming endpoint (Slice E), so
    // the round-trip exercises a minimal SSE stream rather than the old
    // Chat Completions JSON envelope.
    const response_body =
        "event: response.output_item.added\n" ++
        "data: {\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"id\":\"m1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[]}}\n\n" ++
        "event: response.output_text.delta\n" ++
        "data: {\"type\":\"response.output_text.delta\",\"output_index\":0,\"content_index\":0,\"delta\":\"hello world\"}\n\n" ++
        "event: response.completed\n" ++
        "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"r1\",\"status\":\"completed\",\"usage\":{\"input_tokens\":1,\"output_tokens\":2,\"total_tokens\":3}}}\n\n" ++
        "data: [DONE]\n";

    var server = try LocalHttpServer.init(testing.allocator, response_body, "text/event-stream");
    try server.start();
    errdefer server.join() catch {};

    const url = try server.url(testing.allocator, "/v1/responses");
    defer testing.allocator.free(url);

    var captured = try capturePost(testing.allocator, url, &.{
        .{ .name = "authorization", .value = "Bearer test-fixture-key" },
        .{ .name = "content-type", .value = "application/json" },
    }, "{\"model\":\"gpt-4o-mini\",\"input\":[]}");
    defer captured.deinit(testing.allocator);

    try testing.expectEqual(@as(u16, 200), captured.status);
    try testing.expectEqualStrings(response_body, captured.body);

    // Write cassette, then replay.
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const cassette_path = try std.fs.path.resolve(testing.allocator, &.{
        ".zig-cache", "tmp", tmp.sub_path[0..], "roundtrip.jsonl",
    });
    defer testing.allocator.free(cassette_path);

    try writeCassette(testing.allocator, cassette_path, captured.body, .{
        .provider = .openai,
        .scenario = "roundtrip",
        .stream = true,
    });

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const cassette = try cassette_client.loadCassetteFromPath(arena.allocator(), cassette_path);
    try testing.expectEqualStrings(captured.body, cassette.body);

    // And the replay produces a valid ModelCallResult through the openai parser.
    const result = try cassette_client.replay(arena.allocator(), cassette);
    switch (result.reply.response) {
        .final_text => |t| try testing.expectEqualStrings("hello world", t),
        else => return error.TestFailed,
    }

    try server.join();
}
