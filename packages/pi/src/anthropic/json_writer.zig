//! Small JSON string-escape helper shared by the request + tools_schema
//! serializers. Kept local to `anthropic/` rather than re-exporting from
//! zigts to avoid widening the named-module surface for one function.

const std = @import("std");

pub fn writeString(writer: anytype, s: []const u8) !void {
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
