//! ANSI escape vocabulary for the TUI. Kept as string literals and small
//! emitter helpers so callers write semantic code (`ansi.eraseLine(w)`) rather
//! than hand-rolled `"\x1b[2K"` sprinkled everywhere. No state, no allocation.

const std = @import("std");

/// SGR reset ("all attributes off").
pub const reset = "\x1b[0m";

/// Clear the current line (cursor position unchanged).
pub const erase_line = "\r\x1b[2K";

/// Move cursor up N rows, column 1.
pub fn eraseLineUp(w: *std.Io.Writer, rows: usize) !void {
    try w.print("\r\x1b[2K\x1b[{d}A\x1b[2K", .{rows});
}

pub fn cursorLeft(w: *std.Io.Writer, cols: usize) !void {
    if (cols == 0) return;
    try w.print("\x1b[{d}D", .{cols});
}

/// CSI ?2026h / ?2026l: synchronized-output begin/end. Terminals that
/// support it buffer any redraws between the two codes into one atomic
/// flush, eliminating tearing. Terminals that ignore the sequence still
/// render correctly, just without the guarantee.
pub const sync_begin = "\x1b[?2026h";
pub const sync_end = "\x1b[?2026l";

/// UTF-8 encoding of U+00B7 "middle dot", used as the visual separator
/// between status-line sections and other field groups.
pub const middle_dot = " \xc2\xb7 ";

/// Approximate the column count a styled byte string occupies on the
/// terminal. Skips SGR escape sequences (CSI ... m) and counts each
/// UTF-8 codepoint as one cell.
///
/// LIMITATIONS: this is a quick estimate, not a precise width
/// calculation. It does NOT account for:
///   - East Asian wide characters (CJK ideographs count as 1 here, but
///     terminals render them as 2).
///   - Combining characters / zero-width joiners (counted as 1 each).
///   - Other CSI sequences that are not the SGR `\x1b[ ... m` form.
///
/// Suitable for the project's current renderers (ASCII text, box-drawing
/// glyphs, and the middle-dot separator). Callers rendering arbitrary
/// user content should consider a fuller wcwidth implementation.
pub fn visibleCellEstimate(text: []const u8) usize {
    var i: usize = 0;
    var visible: usize = 0;
    while (i < text.len) {
        if (text[i] == 0x1b) {
            i += 1;
            if (i < text.len and text[i] == '[') i += 1;
            while (i < text.len and text[i] != 'm') i += 1;
            if (i < text.len) i += 1;
            continue;
        }
        if ((text[i] & 0x80) == 0) {
            visible += 1;
            i += 1;
        } else if ((text[i] & 0xe0) == 0xc0) {
            visible += 1;
            i += 2;
        } else if ((text[i] & 0xf0) == 0xe0) {
            visible += 1;
            i += 3;
        } else if ((text[i] & 0xf8) == 0xf0) {
            visible += 1;
            i += 4;
        } else {
            visible += 1;
            i += 1;
        }
    }
    return visible;
}

/// Write a styled byte string capped to `width` visible cells, padding with
/// spaces when shorter. SGR CSI sequences (`\x1b[...m`) are copied without
/// counting toward width. If truncation happens while a style is active, the
/// output is reset so the next row starts clean.
pub fn writeFitted(w: *std.Io.Writer, text: []const u8, width: usize) !void {
    if (width == 0) return;

    var i: usize = 0;
    var visible: usize = 0;
    var truncated = false;
    while (i < text.len) {
        if (text[i] == 0x1b) {
            const start = i;
            i += 1;
            if (i < text.len and text[i] == '[') i += 1;
            while (i < text.len and text[i] != 'm') i += 1;
            if (i < text.len) i += 1;
            try w.writeAll(text[start..i]);
            continue;
        }

        if (visible >= width) {
            truncated = true;
            break;
        }

        const next = utf8SequenceEnd(text, i);
        try w.writeAll(text[i..next]);
        visible += 1;
        i = next;
    }

    if (i < text.len) truncated = true;
    if (truncated) try w.writeAll(reset);
    if (visible < width) try writeSpaces(w, width - visible);
}

fn utf8SequenceEnd(text: []const u8, start: usize) usize {
    const b = text[start];
    const len: usize = if ((b & 0x80) == 0)
        1
    else if ((b & 0xe0) == 0xc0)
        2
    else if ((b & 0xf0) == 0xe0)
        3
    else if ((b & 0xf8) == 0xf0)
        4
    else
        1;
    return @min(text.len, start + len);
}

fn writeSpaces(w: *std.Io.Writer, count: usize) !void {
    var remaining = count;
    while (remaining > 0) : (remaining -= 1) {
        try w.writeByte(' ');
    }
}

/// SGR segment: emit "\x1b[<params>m". Empty params emit a reset.
pub fn sgr(w: *std.Io.Writer, params: []const u8) !void {
    if (params.len == 0) {
        try w.writeAll(reset);
        return;
    }
    try w.print("\x1b[{s}m", .{params});
}

/// Write `text` wrapped in the SGR segment `params`, resetting at the end.
/// Empty `params` emits just `text`.
pub fn styled(w: *std.Io.Writer, params: []const u8, text: []const u8) !void {
    if (params.len == 0) {
        try w.writeAll(text);
        return;
    }
    try sgr(w, params);
    try w.writeAll(text);
    try w.writeAll(reset);
}

/// Same as `styled` but the inner content is produced via `std.fmt`-style
/// formatting. Lets callers render integers (or any non-string value)
/// through the SGR wrapper without hand-rolling a `\x1b[{s}m` print.
pub fn styledFmt(
    w: *std.Io.Writer,
    params: []const u8,
    comptime fmt: []const u8,
    args: anytype,
) !void {
    if (params.len == 0) {
        try w.print(fmt, args);
        return;
    }
    try sgr(w, params);
    try w.print(fmt, args);
    try w.writeAll(reset);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn collect(f: anytype) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    errdefer aw.deinit();
    try f(&aw.writer);
    var out = aw.toArrayList();
    return try out.toOwnedSlice(testing.allocator);
}

test "sgr: empty params writes the reset sequence" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try sgr(w, "");
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings(reset, out);
}

test "sgr: non-empty params wrapped as CSI <params> m" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try sgr(w, "1;34");
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\x1b[1;34m", out);
}

test "styled: wraps the text with SGR and resets at the end" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try styled(w, "32", "ok");
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\x1b[32mok\x1b[0m", out);
}

test "styledFmt: formats through SGR with params" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try styledFmt(w, "36", "{d}", .{42});
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\x1b[36m42\x1b[0m", out);
}

test "styledFmt: empty params drops the SGR wrap" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try styledFmt(w, "", "{d}", .{7});
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("7", out);
}

test "styled: empty SGR emits only the text" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try styled(w, "", "hi");
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("hi", out);
}

test "eraseLineUp: clears current + N rows above" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try eraseLineUp(w, 2);
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\r\x1b[2K\x1b[2A\x1b[2K", out);
}

test "cursorLeft: emits CSI D only when movement is non-zero" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try cursorLeft(w, 3);
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\x1b[3D", out);
}

test "cursorLeft: zero columns is a no-op" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try cursorLeft(w, 0);
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("", out);
}

test "visibleCellEstimate: counts ASCII bytes one-for-one" {
    try testing.expectEqual(@as(usize, 5), visibleCellEstimate("hello"));
    try testing.expectEqual(@as(usize, 0), visibleCellEstimate(""));
}

test "visibleCellEstimate: skips SGR escape sequences" {
    try testing.expectEqual(@as(usize, 5), visibleCellEstimate("\x1b[31mhello\x1b[0m"));
    try testing.expectEqual(@as(usize, 5), visibleCellEstimate("\x1b[1;38;5;33mhello\x1b[0m"));
}

test "visibleCellEstimate: counts UTF-8 sequences as one cell each" {
    // Middle-dot is 3 ASCII cells: " " + "\xc2\xb7" + " "
    try testing.expectEqual(@as(usize, 3), visibleCellEstimate(middle_dot));
    // Box-drawing vertical bar (3-byte UTF-8) counts as 1 cell.
    try testing.expectEqual(@as(usize, 1), visibleCellEstimate("\xe2\x94\x82"));
}

test "writeFitted: truncates styled text by visible cells" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try writeFitted(w, "\x1b[31mhello world\x1b[0m", 5);
        }
    }.call);
    defer testing.allocator.free(out);

    try testing.expect(std.mem.indexOf(u8, out, "hello") != null);
    try testing.expect(std.mem.indexOf(u8, out, " world") == null);
    try testing.expectEqual(@as(usize, 5), visibleCellEstimate(out));
    try testing.expect(std.mem.endsWith(u8, out, reset));
}

test "writeFitted: pads styled text when shorter than width" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try writeFitted(w, "\x1b[32mok\x1b[0m", 5);
        }
    }.call);
    defer testing.allocator.free(out);

    try testing.expectEqual(@as(usize, 5), visibleCellEstimate(out));
    try testing.expect(std.mem.endsWith(u8, out, "   "));
}
