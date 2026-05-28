//! OpenAI Responses API SSE parser. Splits a byte buffer into SSE records
//! (event name + data payload, separated by blank lines), then decodes each
//! record's JSON data field into a typed `events.Event`.
//!
//! The SSE record framing matches Anthropic's (and the SSE spec): `event:`
//! sets the name, `data:` lines join with `\n`, blank lines dispatch.
//! Where OpenAI differs is the JSON shape inside `data:` — event names like
//! `response.output_text.delta` map to one of the variants in
//! `events.Event`. Like the Anthropic parser we trust the `type` field in
//! the JSON payload as the source of truth (the `event:` line is
//! convenience), so a missing `event:` does not break parsing.
//!
//! Callers pass an arena allocator; both the record iterator and the JSON
//! decode allocate into it, and every borrowed slice on the returned
//! events lives as long as the arena.

const std = @import("std");
const events = @import("events.zig");

const ParseError = error{
    MalformedSse,
    MissingType,
    UnknownEventType,
    UnexpectedJsonShape,
};

const LineRecord = struct {
    event_name: []const u8,
    data: []const u8,
};

const RecordIterator = struct {
    input: []const u8,
    pos: usize = 0,

    fn next(self: *RecordIterator, arena: std.mem.Allocator) !?LineRecord {
        var event_name: []const u8 = "";
        var data_buf: std.ArrayListUnmanaged(u8) = .empty;
        var have_any_field = false;

        while (self.pos < self.input.len) {
            const line = self.readLine();
            if (line.len == 0) {
                if (have_any_field) {
                    return .{
                        .event_name = event_name,
                        .data = try data_buf.toOwnedSlice(arena),
                    };
                }
                continue;
            }

            if (line[0] == ':') continue;

            const colon = std.mem.indexOfScalar(u8, line, ':') orelse {
                have_any_field = true;
                continue;
            };
            const field = line[0..colon];
            var value = line[colon + 1 ..];
            if (value.len > 0 and value[0] == ' ') value = value[1..];

            if (std.mem.eql(u8, field, "event")) {
                event_name = value;
                have_any_field = true;
            } else if (std.mem.eql(u8, field, "data")) {
                if (data_buf.items.len > 0) try data_buf.append(arena, '\n');
                try data_buf.appendSlice(arena, value);
                have_any_field = true;
            } else {
                have_any_field = true;
            }
        }

        if (have_any_field) {
            return .{
                .event_name = event_name,
                .data = try data_buf.toOwnedSlice(arena),
            };
        }
        return null;
    }

    fn readLine(self: *RecordIterator) []const u8 {
        const start = self.pos;
        while (self.pos < self.input.len and self.input[self.pos] != '\n') : (self.pos += 1) {}
        var end = self.pos;
        if (self.pos < self.input.len) self.pos += 1;
        if (end > start and self.input[end - 1] == '\r') end -= 1;
        return self.input[start..end];
    }
};

pub fn parseAll(arena: std.mem.Allocator, input: []const u8) ![]events.Event {
    var out: std.ArrayListUnmanaged(events.Event) = .empty;
    var it: RecordIterator = .{ .input = input };
    while (try it.next(arena)) |record| {
        // The Responses API streams a terminal `[DONE]` line (carried as the
        // `data:` payload, never JSON) before closing the socket. Treat it
        // as an iteration terminator rather than a parse error.
        if (std.mem.eql(u8, std.mem.trim(u8, record.data, " \t\r\n"), "[DONE]")) break;
        const decoded = try decodeRecord(arena, record);
        if (decoded) |ev| try out.append(arena, ev);
    }
    return try out.toOwnedSlice(arena);
}

fn decodeRecord(
    arena: std.mem.Allocator,
    record: LineRecord,
) !?events.Event {
    if (record.data.len == 0) return ParseError.MalformedSse;

    const root_value = try std.json.parseFromSliceLeaky(
        std.json.Value,
        arena,
        record.data,
        .{},
    );
    if (root_value != .object) return ParseError.UnexpectedJsonShape;
    const root = root_value.object;

    const type_value = root.get("type") orelse return ParseError.MissingType;
    if (type_value != .string) return ParseError.UnexpectedJsonShape;
    const type_str = type_value.string;

    // Start-of-stream signals we record but do not surface to the assembler.
    if (std.mem.eql(u8, type_str, "response.created")) return .response_started;
    if (std.mem.eql(u8, type_str, "response.in_progress")) return .response_started;

    // Per-item / per-part events that bracket the body deltas. We treat the
    // *.added and *.done sub-events for content_part as no-ops: a message
    // item always carries one `output_text` content part in practice, and
    // the assembler only cares about the deltas inside it.
    if (std.mem.eql(u8, type_str, "response.content_part.added")) return null;
    if (std.mem.eql(u8, type_str, "response.content_part.done")) return null;
    if (std.mem.eql(u8, type_str, "response.output_text.done")) return null;
    if (std.mem.eql(u8, type_str, "response.function_call_arguments.done")) return null;

    if (std.mem.eql(u8, type_str, "response.output_item.added")) {
        const output_index = try readU32(root, "output_index");
        const item = root.get("item") orelse return ParseError.UnexpectedJsonShape;
        if (item != .object) return ParseError.UnexpectedJsonShape;
        const item_type = item.object.get("type") orelse return ParseError.UnexpectedJsonShape;
        if (item_type != .string) return ParseError.UnexpectedJsonShape;

        if (std.mem.eql(u8, item_type.string, "message")) {
            return .{ .output_item_added = .{
                .output_index = output_index,
                .kind = .message,
            } };
        }
        if (std.mem.eql(u8, item_type.string, "function_call")) {
            const call_id = try readString(item.object, "call_id");
            const name = try readString(item.object, "name");
            return .{ .output_item_added = .{
                .output_index = output_index,
                .kind = .{ .function_call = .{ .call_id = call_id, .name = name } },
            } };
        }
        // Reasoning / refusal / other item types are not consumed by the
        // assembler today. Skip silently.
        return null;
    }

    if (std.mem.eql(u8, type_str, "response.output_text.delta")) {
        const output_index = try readU32(root, "output_index");
        const content_index = try readU32(root, "content_index");
        const delta = try readString(root, "delta");
        return .{ .output_text_delta = .{
            .output_index = output_index,
            .content_index = content_index,
            .text = delta,
        } };
    }

    if (std.mem.eql(u8, type_str, "response.function_call_arguments.delta")) {
        const output_index = try readU32(root, "output_index");
        const delta = try readString(root, "delta");
        return .{ .function_call_arguments_delta = .{
            .output_index = output_index,
            .delta = delta,
        } };
    }

    if (std.mem.eql(u8, type_str, "response.output_item.done")) {
        return .{ .output_item_done = .{
            .output_index = try readU32(root, "output_index"),
        } };
    }

    if (std.mem.eql(u8, type_str, "response.completed")) {
        var usage: events.Usage = .{};
        var stop_reason: ?[]const u8 = null;
        if (root.get("response")) |resp_value| {
            if (resp_value == .object) {
                const resp = resp_value.object;
                if (resp.get("usage")) |usage_value| {
                    if (usage_value == .object) {
                        usage.input_tokens = readU64(usage_value.object, "input_tokens");
                        usage.output_tokens = readU64(usage_value.object, "output_tokens");
                    }
                }
                if (resp.get("status")) |status_value| {
                    if (status_value == .string) stop_reason = status_value.string;
                }
            }
        }
        return .{ .response_completed = .{ .usage = usage, .stop_reason = stop_reason } };
    }

    if (std.mem.eql(u8, type_str, "response.failed") or
        std.mem.eql(u8, type_str, "response.incomplete"))
    {
        var kind: []const u8 = type_str;
        var message: []const u8 = "";
        if (root.get("response")) |resp_value| {
            if (resp_value == .object) {
                const resp = resp_value.object;
                if (resp.get("error")) |err_value| {
                    if (err_value == .object) {
                        if (err_value.object.get("code")) |v| if (v == .string) {
                            kind = v.string;
                        };
                        if (err_value.object.get("message")) |v| if (v == .string) {
                            message = v.string;
                        };
                    }
                }
                if (resp.get("incomplete_details")) |details| {
                    if (details == .object) {
                        if (details.object.get("reason")) |v| if (v == .string) {
                            if (message.len == 0) message = v.string;
                        };
                    }
                }
            }
        }
        return .{ .api_error = .{ .kind = kind, .message = message } };
    }

    if (std.mem.eql(u8, type_str, "error")) {
        var kind: []const u8 = "error";
        var message: []const u8 = "";
        if (root.get("code")) |v| if (v == .string) {
            kind = v.string;
        };
        if (root.get("message")) |v| if (v == .string) {
            message = v.string;
        };
        return .{ .api_error = .{ .kind = kind, .message = message } };
    }

    return ParseError.UnknownEventType;
}

fn readU64(obj: std.json.ObjectMap, key: []const u8) u64 {
    const v = obj.get(key) orelse return 0;
    if (v != .integer) return 0;
    if (v.integer < 0) return 0;
    return @intCast(v.integer);
}

fn readU32(obj: std.json.ObjectMap, key: []const u8) !u32 {
    const v = obj.get(key) orelse return ParseError.UnexpectedJsonShape;
    if (v != .integer) return ParseError.UnexpectedJsonShape;
    if (v.integer < 0) return ParseError.UnexpectedJsonShape;
    return @intCast(v.integer);
}

fn readString(obj: std.json.ObjectMap, key: []const u8) ![]const u8 {
    const v = obj.get(key) orelse return ParseError.UnexpectedJsonShape;
    if (v != .string) return ParseError.UnexpectedJsonShape;
    return v.string;
}

// ---------------------------------------------------------------------------
// Tests
//
// Cassettes live in `cassettes/` as hand-written SSE streams that mirror
// the wire format of the OpenAI Responses API. Each cassette is embedded
// via @embedFile so the test binary has zero runtime dependencies on the
// cwd. Where the upstream API gets verbose (reasoning items, refusals,
// extra metadata) the cassettes carry only the events the parser and
// assembler actually consume.
// ---------------------------------------------------------------------------

const testing = std.testing;

const cassette_text_simple = @embedFile("cassettes/text_simple.sse");
const cassette_text_multi_delta = @embedFile("cassettes/text_multi_delta.sse");
const cassette_tool_use = @embedFile("cassettes/tool_use.sse");
const cassette_multi_tool = @embedFile("cassettes/multi_tool.sse");
const cassette_error_overloaded = @embedFile("cassettes/error_overloaded.sse");

test "text_simple cassette: full happy path produces expected event shape" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try parseAll(arena.allocator(), cassette_text_simple);

    // response.created, output_item.added (message), one delta,
    // output_item.done, response.completed.
    try testing.expectEqual(@as(usize, 5), list.len);
    try testing.expect(list[0] == .response_started);
    try testing.expect(list[1] == .output_item_added);
    try testing.expect(list[1].output_item_added.kind == .message);
    try testing.expect(list[2] == .output_text_delta);
    try testing.expectEqualStrings("Hello, world!", list[2].output_text_delta.text);
    try testing.expect(list[3] == .output_item_done);
    try testing.expect(list[4] == .response_completed);
    try testing.expectEqual(@as(u64, 12), list[4].response_completed.usage.input_tokens);
    try testing.expectEqual(@as(u64, 5), list[4].response_completed.usage.output_tokens);
}

test "text_multi_delta cassette: text deltas concatenate into the full reply" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try parseAll(arena.allocator(), cassette_text_multi_delta);

    var assembled: std.ArrayListUnmanaged(u8) = .empty;
    for (list) |ev| {
        switch (ev) {
            .output_text_delta => |d| try assembled.appendSlice(arena.allocator(), d.text),
            else => {},
        }
    }
    try testing.expectEqualStrings("Hello, how can I help?", assembled.items);
}

test "tool_use cassette: header carries call_id+name, arg deltas concatenate to JSON" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try parseAll(arena.allocator(), cassette_tool_use);

    var saw_header = false;
    var assembled: std.ArrayListUnmanaged(u8) = .empty;
    for (list) |ev| {
        switch (ev) {
            .output_item_added => |added| switch (added.kind) {
                .function_call => |h| {
                    saw_header = true;
                    try testing.expectEqualStrings("call_abc123", h.call_id);
                    try testing.expectEqualStrings("zigts_expert_describe_rule", h.name);
                },
                else => {},
            },
            .function_call_arguments_delta => |d| try assembled.appendSlice(arena.allocator(), d.delta),
            else => {},
        }
    }
    try testing.expect(saw_header);
    try testing.expectEqualStrings("{\"rule\":\"ZTS303\"}", assembled.items);
}

test "multi_tool cassette: two function_call items, each addressed by output_index" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try parseAll(arena.allocator(), cassette_multi_tool);

    var calls_seen: u32 = 0;
    for (list) |ev| {
        switch (ev) {
            .output_item_added => |added| switch (added.kind) {
                .function_call => calls_seen += 1,
                else => {},
            },
            else => {},
        }
    }
    try testing.expectEqual(@as(u32, 2), calls_seen);
}

test "error_overloaded cassette: response.failed carries kind and message" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try parseAll(arena.allocator(), cassette_error_overloaded);

    try testing.expectEqual(@as(usize, 1), list.len);
    try testing.expect(list[0] == .api_error);
    try testing.expectEqualStrings("rate_limit_exceeded", list[0].api_error.kind);
    try testing.expectEqualStrings("Rate limit reached for requests", list[0].api_error.message);
}

test "RecordIterator: terminal [DONE] sentinel stops iteration without a parse error" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const input =
        "event: response.created\n" ++
        "data: {\"type\":\"response.created\"}\n" ++
        "\n" ++
        "data: [DONE]\n" ++
        "\n";
    const list = try parseAll(arena.allocator(), input);
    try testing.expectEqual(@as(usize, 1), list.len);
    try testing.expect(list[0] == .response_started);
}

test "RecordIterator: CRLF line endings are tolerated" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const input = "event: response.created\r\ndata: {\"type\":\"response.created\"}\r\n\r\n";
    const list = try parseAll(arena.allocator(), input);
    try testing.expectEqual(@as(usize, 1), list.len);
    try testing.expect(list[0] == .response_started);
}

test "decodeRecord: missing type field yields MissingType" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const rec: LineRecord = .{ .event_name = "", .data = "{\"foo\":\"bar\"}" };
    try testing.expectError(ParseError.MissingType, decodeRecord(arena.allocator(), rec));
}

test "decodeRecord: unknown type field yields UnknownEventType" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const rec: LineRecord = .{ .event_name = "", .data = "{\"type\":\"not_a_real_event\"}" };
    try testing.expectError(ParseError.UnknownEventType, decodeRecord(arena.allocator(), rec));
}

test "decodeRecord: content_part.added and *.done sub-events are ignored, not errors" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const rec_added: LineRecord = .{
        .event_name = "",
        .data = "{\"type\":\"response.content_part.added\",\"output_index\":0,\"content_index\":0}",
    };
    try testing.expect((try decodeRecord(arena.allocator(), rec_added)) == null);
    const rec_done: LineRecord = .{
        .event_name = "",
        .data = "{\"type\":\"response.output_text.done\",\"output_index\":0,\"content_index\":0}",
    };
    try testing.expect((try decodeRecord(arena.allocator(), rec_done)) == null);
}
