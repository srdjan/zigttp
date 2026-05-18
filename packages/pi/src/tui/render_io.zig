//! Tiny writer helpers shared by every TUI render path.

const std = @import("std");
const ansi = @import("ansi.zig");

pub fn writeFitted(w: *std.Io.Writer, text: []const u8, width: usize) !void {
    if (width == 0) return;
    if (text.len <= width) {
        try w.writeAll(text);
        try ansi.writeSpaces(w, width - text.len);
        return;
    }
    if (width <= 3) {
        try w.writeAll(text[0..width]);
        return;
    }
    try w.writeAll(text[0 .. width - 3]);
    try w.writeAll("...");
}

pub const writeSpaces = ansi.writeSpaces;

pub fn writeCrlf(w: *std.Io.Writer) !void {
    try w.writeAll("\r\n");
}

pub fn moveCursor(w: *std.Io.Writer, row: usize, col: usize) !void {
    try w.print("\x1b[{d};{d}H", .{ row, col });
}
