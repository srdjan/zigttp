//! Anthropic Messages SSE parser. Splits a byte buffer into SSE records
//! (event name + data payload, separated by blank lines), then decodes each
//! record's JSON data field into a typed `events.Event`.
//!
//! Callers pass an arena allocator; both the record iterator and the JSON
//! decode allocate into it, and every borrowed slice on the returned
//! events lives as long as the arena. The real streaming client in slice 3
//! will wrap `RecordIterator` with a growable byte buffer; slice 2's tests
//! operate on whole cassette files via `parseAll`.

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

/// Walks a `[]const u8` one SSE record at a time. Lines are `\n`-terminated;
/// `\r` preceding `\n` is stripped. An SSE field is `<name>:<value>` with an
/// optional leading space after the colon. A blank line dispatches the
/// accumulated record. `event:` sets the name; `data:` lines are joined with
/// `\n`. `:`-prefixed comment lines and unknown fields are ignored.
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
        const ev = try decodeRecord(arena, record);
        try out.append(arena, ev);
    }
    return try out.toOwnedSlice(arena);
}

fn decodeRecord(
    arena: std.mem.Allocator,
    record: LineRecord,
) !events.Event {
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

    if (std.mem.eql(u8, type_str, "message_start")) return .message_start;
    if (std.mem.eql(u8, type_str, "ping")) return .ping;
    if (std.mem.eql(u8, type_str, "message_stop")) return .message_stop;

    if (std.mem.eql(u8, type_str, "content_block_start")) {
        const index = try readIndex(root);
        const block = root.get("content_block") orelse return ParseError.UnexpectedJsonShape;
        if (block != .object) return ParseError.UnexpectedJsonShape;
        const block_type = block.object.get("type") orelse return ParseError.UnexpectedJsonShape;
        if (block_type != .string) return ParseError.UnexpectedJsonShape;

        if (std.mem.eql(u8, block_type.string, "text")) {
            return .{ .content_block_start = .{ .index = index, .kind = .text } };
        }
        if (std.mem.eql(u8, block_type.string, "tool_use")) {
            const id = try readString(block.object, "id");
            const name = try readString(block.object, "name");
            return .{ .content_block_start = .{
                .index = index,
                .kind = .{ .tool_use = .{ .id = id, .name = name } },
            } };
        }
        return ParseError.UnknownEventType;
    }

    if (std.mem.eql(u8, type_str, "content_block_delta")) {
        const index = try readIndex(root);
        const delta = root.get("delta") orelse return ParseError.UnexpectedJsonShape;
        if (delta != .object) return ParseError.UnexpectedJsonShape;
        const delta_type = delta.object.get("type") orelse return ParseError.UnexpectedJsonShape;
        if (delta_type != .string) return ParseError.UnexpectedJsonShape;

        if (std.mem.eql(u8, delta_type.string, "text_delta")) {
            const text = try readString(delta.object, "text");
            return .{ .content_block_delta = .{
                .index = index,
                .payload = .{ .text = text },
            } };
        }
        if (std.mem.eql(u8, delta_type.string, "input_json_delta")) {
            const partial = try readString(delta.object, "partial_json");
            return .{ .content_block_delta = .{
                .index = index,
                .payload = .{ .input_json = partial },
            } };
        }
        return ParseError.UnknownEventType;
    }

    if (std.mem.eql(u8, type_str, "content_block_stop")) {
        return .{ .content_block_stop = .{ .index = try readIndex(root) } };
    }

    if (std.mem.eql(u8, type_str, "message_delta")) {
        const delta = root.get("delta") orelse return ParseError.UnexpectedJsonShape;
        if (delta != .object) return ParseError.UnexpectedJsonShape;
        const stop_reason: ?[]const u8 = if (delta.object.get("stop_reason")) |v|
            (if (v == .string) v.string else null)
        else
            null;
        const output_tokens: ?u64 = if (root.get("usage")) |usage| blk: {
            if (usage != .object) break :blk null;
            const tok = usage.object.get("output_tokens") orelse break :blk null;
            if (tok != .integer) break :blk null;
            break :blk @intCast(tok.integer);
        } else null;
        return .{ .message_delta = .{
            .stop_reason = stop_reason,
            .output_tokens = output_tokens,
        } };
    }

    if (std.mem.eql(u8, type_str, "error")) {
        const inner = root.get("error") orelse return ParseError.UnexpectedJsonShape;
        if (inner != .object) return ParseError.UnexpectedJsonShape;
        return .{ .api_error = .{
            .kind = try readString(inner.object, "type"),
            .message = try readString(inner.object, "message"),
        } };
    }

    return ParseError.UnknownEventType;
}

fn readIndex(obj: std.json.ObjectMap) !u32 {
    const v = obj.get("index") orelse return ParseError.UnexpectedJsonShape;
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
// Cassettes live in `cassettes/` as hand-written SSE streams that match the
// real Anthropic Messages API wire format. Each cassette is embedded via
// @embedFile so the test binary has zero runtime dependencies on the cwd.
// ---------------------------------------------------------------------------

const testing = std.testing;

const cassette_text_simple = @embedFile("cassettes/text_simple.sse");
const cassette_text_multi_delta = @embedFile("cassettes/text_multi_delta.sse");
const cassette_tool_use = @embedFile("cassettes/tool_use.sse");
const cassette_error_overloaded = @embedFile("cassettes/error_overloaded.sse");

test "text_simple cassette: full happy path produces expected event shape" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try parseAll(arena.allocator(), cassette_text_simple);

    try testing.expectEqual(@as(usize, 7), list.len);
    try testing.expectEqual(events.Event.message_start, list[0]);
    try testing.expect(list[1] == .content_block_start);
    try testing.expectEqual(@as(u32, 0), list[1].content_block_start.index);
    try testing.expect(list[1].content_block_start.kind == .text);
    try testing.expectEqual(events.Event.ping, list[2]);
    try testing.expect(list[3] == .content_block_delta);
    try testing.expect(list[3].content_block_delta.payload == .text);
    try testing.expectEqualStrings("Hello, world!", list[3].content_block_delta.payload.text);
    try testing.expect(list[4] == .content_block_stop);
    try testing.expectEqual(@as(u32, 0), list[4].content_block_stop.index);
    try testing.expect(list[5] == .message_delta);
    try testing.expectEqualStrings("end_turn", list[5].message_delta.stop_reason.?);
    try testing.expectEqual(@as(u64, 5), list[5].message_delta.output_tokens.?);
    try testing.expectEqual(events.Event.message_stop, list[6]);
}

test "text_multi_delta cassette: deltas concatenate into the full reply" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try parseAll(arena.allocator(), cassette_text_multi_delta);

    var assembled: std.ArrayListUnmanaged(u8) = .empty;
    for (list) |ev| {
        switch (ev) {
            .content_block_delta => |d| switch (d.payload) {
                .text => |t| try assembled.appendSlice(arena.allocator(), t),
                else => {},
            },
            else => {},
        }
    }
    try testing.expectEqualStrings("Hello, how can I help?", assembled.items);
}

test "tool_use cassette: header carries id+name, input_json deltas assemble to valid JSON" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try parseAll(arena.allocator(), cassette_tool_use);

    var saw_header = false;
    var assembled: std.ArrayListUnmanaged(u8) = .empty;
    var stop_reason: ?[]const u8 = null;
    for (list) |ev| {
        switch (ev) {
            .content_block_start => |s| switch (s.kind) {
                .tool_use => |h| {
                    saw_header = true;
                    try testing.expectEqualStrings("toolu_01ABCDEF", h.id);
                    try testing.expectEqualStrings("zigts_expert_describe_rule", h.name);
                },
                else => {},
            },
            .content_block_delta => |d| switch (d.payload) {
                .input_json => |j| try assembled.appendSlice(arena.allocator(), j),
                else => {},
            },
            .message_delta => |md| stop_reason = md.stop_reason,
            else => {},
        }
    }
    try testing.expect(saw_header);
    try testing.expectEqualStrings("{\"rule\":\"ZTS303\"}", assembled.items);
    try testing.expectEqualStrings("tool_use", stop_reason.?);
}

test "error_overloaded cassette: api_error event carries kind and message" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const list = try parseAll(arena.allocator(), cassette_error_overloaded);

    try testing.expectEqual(@as(usize, 1), list.len);
    try testing.expect(list[0] == .api_error);
    try testing.expectEqualStrings("overloaded_error", list[0].api_error.kind);
    try testing.expectEqualStrings("Overloaded", list[0].api_error.message);
}

test "RecordIterator: CRLF line endings are tolerated" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const input = "event: ping\r\ndata: {\"type\":\"ping\"}\r\n\r\n";
    const list = try parseAll(arena.allocator(), input);
    try testing.expectEqual(@as(usize, 1), list.len);
    try testing.expectEqual(events.Event.ping, list[0]);
}

test "RecordIterator: comment lines beginning with colon are ignored" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const input =
        ": this is a comment\n" ++
        "event: ping\n" ++
        "data: {\"type\":\"ping\"}\n" ++
        "\n";
    const list = try parseAll(arena.allocator(), input);
    try testing.expectEqual(@as(usize, 1), list.len);
    try testing.expectEqual(events.Event.ping, list[0]);
}

test "RecordIterator: missing colon field is ignored, not a parse error" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const input =
        "event: ping\n" ++
        "garbage_no_colon\n" ++
        "data: {\"type\":\"ping\"}\n" ++
        "\n";
    const list = try parseAll(arena.allocator(), input);
    try testing.expectEqual(@as(usize, 1), list.len);
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
