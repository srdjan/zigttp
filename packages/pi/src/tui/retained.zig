//! Bottom-anchored retained-mode manager.
//!
//! Two lines at the bottom of the terminal stay "sticky" across the
//! session: the status line (session, model, token totals) and the input
//! line (prompt label + editable buffer). Scrollback above scrolls naturally
//! as turns print their output through `beforeScrollback` / `afterScrollback`.
//!
//! Flow:
//!   - `redraw(state)` erases both rows, writes status line, writes input
//!     line, and leaves the cursor at the end of the input buffer. Wrapped
//!     in CSI `?2026h` / `?2026l` (synchronized output) so a supporting
//!     terminal coalesces the update into one atomic flush, no flicker.
//!
//!   - `beforeScrollback()` erases the retained region and moves the cursor
//!     to the start so the caller can emit arbitrary scrollback bytes.
//!
//!   - `afterScrollback(state)` writes a CRLF and then redraws the retained
//!     region, anchoring it at the new bottom.
//!
//! No frame buffer, no per-cell diff: the retained region is small and
//! redrawing all of it per keystroke is cheap (~150 bytes of ANSI).
//! Flicker is eliminated by the synchronized-output wrap, not by diffing.

const std = @import("std");

const ansi = @import("ansi.zig");
const theme_mod = @import("theme.zig");
const status_line_widget = @import("widgets/status_line.zig");

pub const State = struct {
    status: status_line_widget.State,
    /// Text displayed on the input row after the prompt label.
    input: []const u8,
    /// Prompt label (with trailing separator/space, e.g. "expert> ").
    prompt_label: []const u8,
};

/// Writer abstraction: production passes a small shim that forwards to
/// `std.c.write` on stdout; tests pass an `std.Io.Writer.Allocating`.
pub const Writer = *std.Io.Writer;

/// Emit the full retained region: sync_begin, erase rows, status row, CRLF,
/// prompt label, user input, sync_end. Caller is expected to have just
/// printed scrollback (or nothing) immediately above.
pub fn redraw(w: Writer, palette: *const theme_mod.Theme, state: State) !void {
    try w.writeAll(ansi.sync_begin);
    try ansi.eraseLineUp(w, 1);
    try status_line_widget.render(w, palette, state.status);
    try w.writeAll("\r\n");
    try writeInputLine(w, palette, state.prompt_label, state.input);
    try w.writeAll(ansi.sync_end);
}

/// Erase the retained region so the caller can print scrollback bytes over
/// the same rows. After the caller finishes, it must issue `afterScrollback`.
pub fn beforeScrollback(w: Writer) !void {
    try w.writeAll(ansi.sync_begin);
    try ansi.eraseLineUp(w, 1);
    try w.writeAll(ansi.sync_end);
}

/// Emit a CRLF, then redraw the retained region anchored at the new bottom.
pub fn afterScrollback(w: Writer, palette: *const theme_mod.Theme, state: State) !void {
    try w.writeAll("\r\n");
    try redraw(w, palette, state);
}

fn writeInputLine(
    w: Writer,
    palette: *const theme_mod.Theme,
    prompt_label: []const u8,
    input: []const u8,
) !void {
    try w.writeAll(ansi.erase_line);
    try ansi.styled(w, palette.prompt_label, prompt_label);
    if (input.len > 0) try ansi.styled(w, palette.input_text, input);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;
const turn = @import("../turn.zig");

fn capture(comptime f: anytype, palette: *const theme_mod.Theme, state: State) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    errdefer aw.deinit();
    try f(&aw.writer, palette, state);
    var out = aw.toArrayList();
    return try out.toOwnedSlice(testing.allocator);
}

fn simpleState() State {
    return .{
        .status = .{
            .session_id = "01HF2J5K6M",
            .model = "claude-opus-4-6",
            .tokens = .{ .input_tokens = 100, .output_tokens = 20 },
        },
        .input = "hello",
        .prompt_label = "expert> ",
    };
}

test "redraw: wraps output in sync_begin/sync_end" {
    const out = try capture(redraw, &theme_mod.default, simpleState());
    defer testing.allocator.free(out);
    try testing.expect(std.mem.startsWith(u8, out, ansi.sync_begin));
    try testing.expect(std.mem.endsWith(u8, out, ansi.sync_end));
}

test "redraw: contains erase-up, status row, CRLF, and prompt label" {
    const out = try capture(redraw, &theme_mod.default, simpleState());
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "\x1b[2K") != null);
    try testing.expect(std.mem.indexOf(u8, out, "sess ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "\r\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "expert> ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "hello") != null);
}

test "redraw: input_text placed after the prompt label" {
    const out = try capture(redraw, &theme_mod.default, simpleState());
    defer testing.allocator.free(out);
    const prompt_pos = std.mem.indexOf(u8, out, "expert> ") orelse return error.TestExpected;
    const input_pos = std.mem.indexOf(u8, out, "hello") orelse return error.TestExpected;
    try testing.expect(prompt_pos < input_pos);
}

test "redraw: empty input still emits the prompt label" {
    var s = simpleState();
    s.input = "";
    const out = try capture(redraw, &theme_mod.default, s);
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "expert> ") != null);
}

test "beforeScrollback: erases region and is idempotent" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try beforeScrollback(&aw.writer);
    try beforeScrollback(&aw.writer);
    const bytes = aw.writer.buffered();
    // Two calls, both wrapping their erase in sync_begin/sync_end, so the
    // bytes should contain exactly two opening markers.
    var count: usize = 0;
    var scan_from: usize = 0;
    while (std.mem.indexOfPos(u8, bytes, scan_from, ansi.sync_begin)) |pos| {
        count += 1;
        scan_from = pos + ansi.sync_begin.len;
    }
    try testing.expectEqual(@as(usize, 2), count);
}

test "afterScrollback: starts with CRLF so the retained region drops below" {
    const out = try capture(afterScrollback, &theme_mod.default, simpleState());
    defer testing.allocator.free(out);
    try testing.expect(std.mem.startsWith(u8, out, "\r\n"));
}

test "themed output: default vs solarized-dark diverge on SGR params" {
    const state = simpleState();
    const a = try capture(redraw, &theme_mod.default, state);
    defer testing.allocator.free(a);
    const b = try capture(redraw, &theme_mod.solarized_dark, state);
    defer testing.allocator.free(b);
    try testing.expect(!std.mem.eql(u8, a, b));
}
