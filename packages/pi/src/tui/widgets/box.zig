//! Minimal box-drawing primitive. Renders a titled, bordered box with one
//! line per body row and optional ANSI color on the border. No wrapping,
//! no truncation — the caller is responsible for pre-sizing body lines.
//! Between rows the widget emits bare `\n`; the raw-mode TUI's `printBody`
//! translates those into `\r\n` at write time, so the same byte output
//! works in both the line-buffered REPL and the raw-mode TUI.
//!
//! Character set: box-drawing code points from Unicode (U+2500 range).
//! Width is computed in bytes, which equals display width for ASCII but
//! undercounts for multi-byte UTF-8. Slice 4 bodies are ASCII JSON so
//! this is accurate; richer content will need a proper width helper.

const std = @import("std");

pub const Color = enum {
    none,
    red,
    green,
    yellow,
    cyan,

    pub fn ansi(self: Color) []const u8 {
        return switch (self) {
            .none => "",
            .red => "\x1b[31m",
            .green => "\x1b[32m",
            .yellow => "\x1b[33m",
            .cyan => "\x1b[36m",
        };
    }
};

pub const reset = "\x1b[0m";

/// Unicode box-drawing characters. Kept as constants so any future
/// switch to ASCII-only rendering is a single-file edit.
const top_left = "\xe2\x94\x8c"; // ┌
const top_right = "\xe2\x94\x90"; // ┐
const bottom_left = "\xe2\x94\x94"; // └
const bottom_right = "\xe2\x94\x98"; // ┘
const horizontal = "\xe2\x94\x80"; // ─
const vertical = "\xe2\x94\x82"; // │

pub const Options = struct {
    title: []const u8,
    color: Color = .none,
    /// Maximum byte width for a body line. Longer lines are accepted
    /// as-is; the box simply widens to fit the longest one. A true
    /// wrap-or-truncate policy can land alongside the terminal-width
    /// query in a later slice.
    min_width: usize = 40,
};

/// Writes the box to `writer`. The body is split on `\n`; each segment
/// becomes one row. Trailing empty lines are preserved so the caller can
/// force extra padding.
pub fn writeBox(writer: anytype, opts: Options, body: []const u8) !void {
    const inner_width = computeInnerWidth(opts, body);
    try writeTop(writer, opts, inner_width);
    try writeBody(writer, opts, body, inner_width);
    try writeBottom(writer, opts, inner_width);
}

fn computeInnerWidth(opts: Options, body: []const u8) usize {
    // +2 for title padding on both sides of the label in the top border.
    var widest: usize = opts.title.len + 2;
    var it = std.mem.splitScalar(u8, body, '\n');
    while (it.next()) |line| {
        if (line.len > widest) widest = line.len;
    }
    // +2 for the left/right inner padding inside vertical borders.
    const padded = widest + 2;
    if (padded < opts.min_width) return opts.min_width;
    return padded;
}

fn writeTop(writer: anytype, opts: Options, inner_width: usize) !void {
    const c = opts.color.ansi();
    if (c.len != 0) try writer.writeAll(c);
    try writer.writeAll(top_left);
    try writer.writeAll(horizontal);
    try writer.writeByte(' ');
    try writer.writeAll(opts.title);
    try writer.writeByte(' ');
    // inner_width counts the title label (with its two spaces) plus any
    // remaining horizontal fill. Subtract the bytes we already wrote.
    const already = opts.title.len + 2;
    var remaining: isize = @as(isize, @intCast(inner_width)) - @as(isize, @intCast(already));
    while (remaining > 0) : (remaining -= 1) try writer.writeAll(horizontal);
    try writer.writeAll(top_right);
    if (c.len != 0) try writer.writeAll(reset);
    try writer.writeByte('\n');
}

fn writeBody(writer: anytype, opts: Options, body: []const u8, inner_width: usize) !void {
    const c = opts.color.ansi();
    var it = std.mem.splitScalar(u8, body, '\n');
    var saw_any = false;
    while (it.next()) |line| {
        saw_any = true;
        if (c.len != 0) try writer.writeAll(c);
        try writer.writeAll(vertical);
        if (c.len != 0) try writer.writeAll(reset);
        try writer.writeByte(' ');
        try writer.writeAll(line);
        const used: isize = @as(isize, @intCast(line.len)) + 1; // +1 for leading space
        var pad: isize = @as(isize, @intCast(inner_width)) - used - 1; // -1 for trailing space
        while (pad > 0) : (pad -= 1) try writer.writeByte(' ');
        try writer.writeByte(' ');
        if (c.len != 0) try writer.writeAll(c);
        try writer.writeAll(vertical);
        if (c.len != 0) try writer.writeAll(reset);
        try writer.writeByte('\n');
    }
    if (!saw_any) {
        // Empty body still gets a single blank interior row so the box
        // isn't degenerate.
        if (c.len != 0) try writer.writeAll(c);
        try writer.writeAll(vertical);
        if (c.len != 0) try writer.writeAll(reset);
        var pad: usize = 0;
        while (pad < inner_width) : (pad += 1) try writer.writeByte(' ');
        if (c.len != 0) try writer.writeAll(c);
        try writer.writeAll(vertical);
        if (c.len != 0) try writer.writeAll(reset);
        try writer.writeByte('\n');
    }
}

fn writeBottom(writer: anytype, opts: Options, inner_width: usize) !void {
    const c = opts.color.ansi();
    if (c.len != 0) try writer.writeAll(c);
    try writer.writeAll(bottom_left);
    var i: usize = 0;
    while (i < inner_width) : (i += 1) try writer.writeAll(horizontal);
    try writer.writeAll(bottom_right);
    if (c.len != 0) try writer.writeAll(reset);
    try writer.writeByte('\n');
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn render(opts: Options, body: []const u8) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try writeBox(&aw.writer, opts, body);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(testing.allocator);
}

test "writeBox: single-line body with default color produces three rows terminated by LF" {
    const out = try render(.{ .title = "proof", .min_width = 0 }, "ok");
    defer testing.allocator.free(out);

    var line_count: usize = 0;
    for (out) |c| if (c == '\n') {
        line_count += 1;
    };
    try testing.expectEqual(@as(usize, 3), line_count);
    try testing.expect(std.mem.indexOf(u8, out, "proof") != null);
    try testing.expect(std.mem.indexOf(u8, out, "ok") != null);
    // Default color: no ANSI escape should appear.
    try testing.expect(std.mem.indexOf(u8, out, "\x1b[") == null);
}

test "writeBox: red color wraps border chars in ANSI sequences" {
    const out = try render(.{ .title = "veto", .color = .red, .min_width = 0 }, "fail");
    defer testing.allocator.free(out);

    try testing.expect(std.mem.indexOf(u8, out, "\x1b[31m") != null);
    try testing.expect(std.mem.indexOf(u8, out, "\x1b[0m") != null);
}

test "writeBox: multi-line body produces one row per line plus border rows" {
    const out = try render(.{ .title = "t", .min_width = 0 }, "line1\nline2\nline3");
    defer testing.allocator.free(out);

    var newlines: usize = 0;
    for (out) |c| if (c == '\n') {
        newlines += 1;
    };
    // 1 top + 3 body + 1 bottom = 5 rows
    try testing.expectEqual(@as(usize, 5), newlines);
    try testing.expect(std.mem.indexOf(u8, out, "line1") != null);
    try testing.expect(std.mem.indexOf(u8, out, "line2") != null);
    try testing.expect(std.mem.indexOf(u8, out, "line3") != null);
}

test "writeBox: min_width is respected when body is shorter" {
    const out = try render(.{ .title = "t", .min_width = 30 }, "short");
    defer testing.allocator.free(out);

    // Find the first row containing "short". Its byte length between
    // the opening and closing vertical bars must be at least min_width.
    const top_end = std.mem.indexOfScalar(u8, out, '\n') orelse return error.TestFailed;
    const body_line_end = std.mem.indexOfScalarPos(u8, out, top_end + 1, '\n') orelse return error.TestFailed;
    const body_line = out[top_end + 1 .. body_line_end];
    // Rough check: the body line must contain the minimum span of spaces.
    try testing.expect(body_line.len > 30);
}

test "writeBox: empty body still produces a valid three-row box" {
    const out = try render(.{ .title = "empty", .min_width = 0 }, "");
    defer testing.allocator.free(out);

    var newlines: usize = 0;
    for (out) |c| if (c == '\n') {
        newlines += 1;
    };
    try testing.expectEqual(@as(usize, 3), newlines);
}

test "writeBox: title appears inside the top border" {
    const out = try render(.{ .title = "my-title", .min_width = 0 }, "x");
    defer testing.allocator.free(out);

    const top_end = std.mem.indexOfScalar(u8, out, '\n') orelse return error.TestFailed;
    const top_row = out[0..top_end];
    try testing.expect(std.mem.indexOf(u8, top_row, "my-title") != null);
}
