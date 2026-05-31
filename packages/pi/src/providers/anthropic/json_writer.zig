//! Small JSON string-escape helper shared by the request + tools_schema
//! serializers. Kept local to `anthropic/` rather than re-exporting from
//! zigts to avoid widening the named-module surface for one function.

const std = @import("std");

pub fn writeString(writer: anytype, s: []const u8) !void {
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
        // Multi-byte: emit the sequence verbatim only if it is a complete,
        // valid UTF-8 codepoint. Model output streamed as SSE deltas can split
        // a surrogate pair across two deltas; the JSON parser then decodes each
        // half to its 3-byte CESU-8 form, which is invalid UTF-8 and the
        // Anthropic API rejects the whole body ("surrogates not allowed").
        // Replace any invalid byte with U+FFFD so the request stays valid.
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn roundtrip(input: []const u8) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeString(&aw.writer, input);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(testing.allocator);
}

test "writeString: ascii passthrough is wrapped in quotes" {
    const out = try roundtrip("hello");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\"hello\"", out);
}

test "writeString: escapes quote, backslash, and newline" {
    const out = try roundtrip("a\"b\\c\nd");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\"a\\\"b\\\\c\\nd\"", out);
}

test "writeString: low control bytes use \\u escape" {
    const out = try roundtrip("\x01\x07\x1f");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\"\\u0001\\u0007\\u001f\"", out);
}

test "writeString: valid multi-byte UTF-8 passes through verbatim" {
    const out = try roundtrip("caf\u{00e9} \u{1F600}");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\"caf\u{00e9} \u{1F600}\"", out);
}

test "writeString: lone surrogate (CESU-8) is replaced, output stays valid UTF-8" {
    // ED A0 BD is the CESU-8 encoding of the high surrogate U+D83D, which is
    // what the JSON parser emits for a `\uD83D` split from its low half. The
    // serializer must not pass it through; it would make the request body
    // invalid UTF-8 and the API rejects it ("surrogates not allowed"). The
    // property that matters is that the output is valid UTF-8 carrying no raw
    // surrogate bytes; the exact number of U+FFFD replacements is incidental.
    const out = try roundtrip("a\xED\xA0\xBDb");
    defer testing.allocator.free(out);
    try testing.expect(std.unicode.utf8ValidateSlice(out));
    try testing.expect(std.mem.indexOf(u8, out, "\xED\xA0\xBD") == null);
    try testing.expect(std.mem.indexOf(u8, out, "\u{FFFD}") != null);
    try testing.expectEqual(@as(u8, 'a'), out[1]);
    try testing.expectEqual(@as(u8, 'b'), out[out.len - 2]);
}

test "writeString: truncated multi-byte tail is replaced" {
    // 0xF0 starts a 4-byte sequence but the string ends early.
    const out = try roundtrip("x\xF0");
    defer testing.allocator.free(out);
    try testing.expect(std.unicode.utf8ValidateSlice(out));
    try testing.expect(std.mem.indexOf(u8, out, "\u{FFFD}") != null);
    try testing.expectEqual(@as(u8, 'x'), out[1]);
}
