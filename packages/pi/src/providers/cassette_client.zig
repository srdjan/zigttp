//! Deterministic model client backed by recorded provider cassettes.
//!
//! This is the replay half of the cassette harness. Live provider clients
//! (`anthropic/client.zig`, `openai/client.zig`) open sockets and POST to
//! real APIs; this client instead reads canned response bytes from a
//! cassette file and feeds them through the same parser + assembler the
//! live clients use. The `ModelClient` vtable is identical, so callers
//! can swap implementations without behavior change.
//!
//! Cassette format (one JSONL file per scenario):
//!
//!     {"v":1,"provider":"anthropic","scenario":"text_simple","stream":true,"sse_path":"text_simple.sse.txt", ...}
//!     {"v":1,"provider":"openai","scenario":"chat_completion","stream":false, ...}
//!     {"body":"<single-line JSON>"}            (non-streaming bodies inline)
//!     {"sse":"event: ...\ndata: ...\n\n"}      (streaming bodies inline, one record per line)
//!
//! For streaming cassettes the body may also be parked in a sibling
//! `.sse.txt` file named via the `sse_path` header field; this keeps the
//! cassette diff-readable on real-world streams that exceed a few hundred
//! bytes per JSONL line.

const std = @import("std");
const zigts = @import("zigts");
const file_io = zigts.file_io;
const loop = @import("../loop.zig");
const transcript_mod = @import("../transcript.zig");
const anthropic_sse_parser = @import("anthropic/sse_parser.zig");
const anthropic_response_assembler = @import("anthropic/response_assembler.zig");
const anthropic_apply_edit = @import("anthropic/apply_edit.zig");
const openai_sse_parser = @import("openai/sse_parser.zig");
const openai_response_assembler = @import("openai/response_assembler.zig");

const max_cassette_bytes: usize = 16 * 1024 * 1024;
const max_sse_sidecar_bytes: usize = 16 * 1024 * 1024;

pub const Provider = enum {
    anthropic,
    openai,
};

pub const CassetteError = error{
    InvalidCassette,
    MissingProvider,
    MissingBody,
    UnsupportedProvider,
    SidecarNotFound,
    SidecarUnreadable,
    NonStreamingOpenAINotSupported,
};

pub const Header = struct {
    provider: Provider,
    stream: bool,
    scenario: ?[]const u8 = null,
    sse_path: ?[]const u8 = null,
};

/// In-memory cassette: header plus the bytes the live transport would have
/// produced on the response socket. For streaming providers, `body` is the
/// concatenated SSE stream. For non-streaming providers, `body` is the JSON
/// response body.
pub const Cassette = struct {
    header: Header,
    body: []const u8,
};

pub const Client = struct {
    cassette_path: []const u8,

    pub fn init(cassette_path: []const u8) Client {
        return .{ .cassette_path = cassette_path };
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
        _ = transcript;
        _ = extra_user_text;
        const self: *Client = @ptrCast(@alignCast(ctx));
        return self.sendTurn(arena);
    }

    pub fn sendTurn(self: *Client, arena: std.mem.Allocator) !loop.ModelCallResult {
        const cassette = try loadCassetteFromPath(arena, self.cassette_path);
        return try replay(arena, cassette);
    }
};

// -----------------------------------------------------------------------
// Loading
// -----------------------------------------------------------------------

/// Load a cassette from disk. Sidecar `.sse.txt` files are resolved
/// relative to the cassette's directory.
pub fn loadCassetteFromPath(arena: std.mem.Allocator, path: []const u8) !Cassette {
    const raw = file_io.readFile(arena, path, max_cassette_bytes) catch
        return CassetteError.InvalidCassette;
    const dir = std.fs.path.dirname(path) orelse ".";
    return try loadCassetteFromBytes(arena, raw, dir);
}

/// Parse a cassette from bytes. `cassette_dir` is consulted only when the
/// header points to a sidecar SSE file. Passing `null` disables sidecar
/// resolution (callers that don't have a directory should ensure the
/// cassette's body is inlined).
pub fn loadCassetteFromBytes(
    arena: std.mem.Allocator,
    raw: []const u8,
    cassette_dir: ?[]const u8,
) !Cassette {
    var line_it = std.mem.splitScalar(u8, raw, '\n');
    const header_line = blk: {
        while (line_it.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r");
            if (trimmed.len != 0) break :blk trimmed;
        }
        return CassetteError.InvalidCassette;
    };

    const header = try parseHeader(arena, header_line);

    if (header.sse_path) |rel| {
        if (cassette_dir == null) return CassetteError.SidecarNotFound;
        const joined = try std.fs.path.join(arena, &.{ cassette_dir.?, rel });
        const body = file_io.readFile(arena, joined, max_sse_sidecar_bytes) catch
            return CassetteError.SidecarNotFound;
        return .{ .header = header, .body = body };
    }

    // Inline body: concatenate every subsequent `{"sse":"..."}` or
    // single `{"body":"..."}` line into the response byte stream.
    var body_buf: std.ArrayListUnmanaged(u8) = .empty;
    var saw_body = false;
    while (line_it.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;
        const parsed = std.json.parseFromSliceLeaky(std.json.Value, arena, trimmed, .{}) catch
            return CassetteError.InvalidCassette;
        if (parsed != .object) return CassetteError.InvalidCassette;
        if (parsed.object.get("sse")) |v| {
            if (v != .string) return CassetteError.InvalidCassette;
            try body_buf.appendSlice(arena, v.string);
            saw_body = true;
        } else if (parsed.object.get("body")) |v| {
            if (v != .string) return CassetteError.InvalidCassette;
            try body_buf.appendSlice(arena, v.string);
            saw_body = true;
        }
    }
    if (!saw_body) return CassetteError.MissingBody;
    return .{ .header = header, .body = try body_buf.toOwnedSlice(arena) };
}

fn parseHeader(arena: std.mem.Allocator, line: []const u8) !Header {
    const parsed = std.json.parseFromSliceLeaky(std.json.Value, arena, line, .{}) catch
        return CassetteError.InvalidCassette;
    if (parsed != .object) return CassetteError.InvalidCassette;
    const root = parsed.object;

    const provider_value = root.get("provider") orelse return CassetteError.MissingProvider;
    if (provider_value != .string) return CassetteError.MissingProvider;
    const provider: Provider = if (std.mem.eql(u8, provider_value.string, "anthropic"))
        .anthropic
    else if (std.mem.eql(u8, provider_value.string, "openai"))
        .openai
    else
        return CassetteError.UnsupportedProvider;

    var stream = false;
    if (root.get("stream")) |v| {
        if (v == .bool) stream = v.bool;
    }

    var scenario: ?[]const u8 = null;
    if (root.get("scenario")) |v| {
        if (v == .string) scenario = try arena.dupe(u8, v.string);
    }

    var sse_path: ?[]const u8 = null;
    if (root.get("sse_path")) |v| {
        if (v == .string) sse_path = try arena.dupe(u8, v.string);
    }

    return .{
        .provider = provider,
        .stream = stream,
        .scenario = scenario,
        .sse_path = sse_path,
    };
}

// -----------------------------------------------------------------------
// Replay
// -----------------------------------------------------------------------

/// Replay a cassette's body bytes through the matching provider parser
/// and return a `ModelCallResult` shaped exactly like the live client.
pub fn replay(arena: std.mem.Allocator, cassette: Cassette) !loop.ModelCallResult {
    return switch (cassette.header.provider) {
        .anthropic => {
            const event_list = try anthropic_sse_parser.parseAll(arena, cassette.body);
            const outcome = try anthropic_response_assembler.assemble(arena, event_list);
            const reply = try anthropic_apply_edit.maybeRemap(arena, outcome.reply);
            return .{ .reply = reply, .usage = outcome.usage };
        },
        .openai => {
            // The live OpenAI client only speaks the streaming Responses API
            // (Slice E); a non-streaming cassette body has no parser to drive
            // and would silently misframe through openai_sse_parser. Fail
            // loudly so the recording side knows to re-record with stream=true.
            if (!cassette.header.stream) return CassetteError.NonStreamingOpenAINotSupported;
            const event_list = try openai_sse_parser.parseAll(arena, cassette.body);
            const outcome = try openai_response_assembler.assemble(arena, event_list);
            return .{ .reply = outcome.reply, .usage = outcome.usage };
        },
    };
}

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

const testing = std.testing;

const cassette_anthropic_text_simple = @embedFile("testdata/anthropic/text_simple.sse.txt");
const cassette_openai_chat_completion = @embedFile("testdata/openai/chat_completion.jsonl");

test "loadCassetteFromBytes resolves an inline openai cassette" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const cassette = try loadCassetteFromBytes(arena.allocator(), cassette_openai_chat_completion, null);
    try testing.expectEqual(Provider.openai, cassette.header.provider);
    try testing.expect(cassette.header.stream);
    try testing.expect(cassette.body.len > 0);
}

test "replay: openai cassette produces final_text reply" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const cassette = try loadCassetteFromBytes(arena.allocator(), cassette_openai_chat_completion, null);
    const result = try replay(arena.allocator(), cassette);
    switch (result.reply.response) {
        .final_text => |t| try testing.expectEqualStrings("hello world", t),
        else => return error.TestFailed,
    }
    try testing.expectEqual(@as(u64, 12), result.usage.input_tokens);
    try testing.expectEqual(@as(u64, 3), result.usage.output_tokens);
}

test "replay: anthropic cassette streams SSE bytes through the live parser" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    // Synthesize the cassette by stamping the sidecar bytes directly as
    // the body; this exercises the same code path as `sse_path` resolution
    // without touching the filesystem from inside the test.
    const cassette: Cassette = .{
        .header = .{ .provider = .anthropic, .stream = true },
        .body = cassette_anthropic_text_simple,
    };
    const result = try replay(arena.allocator(), cassette);
    switch (result.reply.response) {
        .final_text => |t| try testing.expectEqualStrings("Hello, world!", t),
        else => return error.TestFailed,
    }
    try testing.expectEqual(@as(u64, 12), result.usage.input_tokens);
    try testing.expectEqual(@as(u64, 5), result.usage.output_tokens);
}

test "loadCassetteFromBytes rejects an empty cassette" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    try testing.expectError(CassetteError.InvalidCassette, loadCassetteFromBytes(arena.allocator(), "   \n\n", null));
}

test "loadCassetteFromBytes rejects an unknown provider" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const raw =
        \\{"v":1,"provider":"bogus","stream":false}
        \\{"body":"{}"}
    ;
    try testing.expectError(CassetteError.UnsupportedProvider, loadCassetteFromBytes(arena.allocator(), raw, null));
}

test "loadCassetteFromBytes flags missing body lines" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const raw =
        \\{"v":1,"provider":"openai","stream":false}
    ;
    try testing.expectError(CassetteError.MissingBody, loadCassetteFromBytes(arena.allocator(), raw, null));
}
