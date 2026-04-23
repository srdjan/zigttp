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

/// CSI ?2026h / ?2026l: synchronized-output begin/end. Terminals that
/// support it buffer any redraws between the two codes into one atomic
/// flush, eliminating tearing. Terminals that ignore the sequence still
/// render correctly, just without the guarantee.
pub const sync_begin = "\x1b[?2026h";
pub const sync_end = "\x1b[?2026l";

/// Move cursor to absolute column on the current row.
pub fn cursorColumn(w: *std.Io.Writer, col: usize) !void {
    try w.print("\r\x1b[{d}C", .{col});
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

test "cursorColumn: absolute positioning via \\r + CUF" {
    const out = try collect(struct {
        fn call(w: *std.Io.Writer) !void {
            try cursorColumn(w, 10);
        }
    }.call);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\r\x1b[10C", out);
}
