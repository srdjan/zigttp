//! Shared JSON string utilities used by handler_contract, type_checker,
//! api_schema, contract_diff, and other modules that serialize JSON.

const std = @import("std");

pub fn writeJsonStringContent(writer: anytype, s: []const u8) !void {
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
}

pub fn writeJsonString(writer: anytype, s: []const u8) !void {
    try writer.writeByte('"');
    try writeJsonStringContent(writer, s);
    try writer.writeByte('"');
}

/// Write a fixed-size byte array as a quoted lowercase-hex JSON string.
pub fn writeJsonHex(writer: anytype, bytes: anytype) !void {
    try writer.writeByte('"');
    try writer.writeAll(&std.fmt.bytesToHex(bytes, .lower));
    try writer.writeByte('"');
}

pub fn containsString(items: []const []const u8, needle: []const u8) bool {
    for (items) |item| {
        if (std.mem.eql(u8, item, needle)) return true;
    }
    return false;
}
