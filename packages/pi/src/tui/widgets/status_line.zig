//! Status line: the themed single-row widget showing session metadata.
//! Format (with the default theme):
//!
//!     sess 01HF2J... · model claude-opus-4-6 · tokens in:12345 out:678
//!
//! Entirely pure: takes a state struct + theme + writer, emits one line of
//! ANSI. No allocation, no I/O. Wrapped once by the retained manager and
//! written wholesale each redraw.

const std = @import("std");
const ansi = @import("../ansi.zig");
const theme_mod = @import("../theme.zig");
const turn = @import("../../turn.zig");

pub const State = struct {
    session_id: ?[]const u8,
    model: ?[]const u8,
    tokens: turn.Usage,
};

/// Render one status row to `writer` using `palette`. Caller is responsible
/// for positioning the cursor; this writes bytes from the current column.
/// Adds a trailing `ansi.reset` so the next write starts clean.
pub fn render(
    writer: *std.Io.Writer,
    palette: *const theme_mod.Theme,
    state: State,
) !void {
    try ansi.styled(writer, palette.status_key, "sess ");
    try ansi.styled(writer, palette.status_value, formatSessionShort(state.session_id));

    try ansi.styled(writer, palette.dim, " \xc2\xb7 "); // middle dot U+00B7
    try ansi.styled(writer, palette.status_key, "model ");
    try ansi.styled(writer, palette.status_value, state.model orelse "stub");

    try ansi.styled(writer, palette.dim, " \xc2\xb7 ");
    try ansi.styled(writer, palette.status_key, "tokens in:");
    try writer.print("\x1b[{s}m{d}\x1b[0m", .{ palette.status_value, state.tokens.input_tokens });
    try ansi.styled(writer, palette.status_key, " out:");
    try writer.print("\x1b[{s}m{d}\x1b[0m", .{ palette.status_value, state.tokens.output_tokens });
}

/// Truncates a session id to a short display form. Session ids are 26-char
/// ULIDs; the first 10 chars are sufficient to disambiguate and fit on a
/// status row alongside other fields.
fn formatSessionShort(id: ?[]const u8) []const u8 {
    const full = id orelse return "ephemeral";
    if (full.len <= 10) return full;
    return full[0..10];
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;
const default_theme = &theme_mod.default;

fn captureRender(state: State) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    errdefer aw.deinit();
    try render(&aw.writer, default_theme, state);
    var out = aw.toArrayList();
    return try out.toOwnedSlice(testing.allocator);
}

test "render: shows session id, model, and token totals" {
    const state: State = .{
        .session_id = "01HF2J5K6M7N8P9Q0R1S2T3V4W",
        .model = "claude-opus-4-6",
        .tokens = .{ .input_tokens = 12345, .output_tokens = 678 },
    };
    const out = try captureRender(state);
    defer testing.allocator.free(out);

    try testing.expect(std.mem.indexOf(u8, out, "sess ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "01HF2J5K6M") != null);
    try testing.expect(std.mem.indexOf(u8, out, "model ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "claude-opus-4-6") != null);
    // The number is wrapped in SGR so "in:" and "12345" are not adjacent
    // in the byte stream. Check both halves individually.
    try testing.expect(std.mem.indexOf(u8, out, "in:") != null);
    try testing.expect(std.mem.indexOf(u8, out, "12345") != null);
    try testing.expect(std.mem.indexOf(u8, out, "out:") != null);
    try testing.expect(std.mem.indexOf(u8, out, "678") != null);
}

test "render: null session id renders as 'ephemeral'" {
    const state: State = .{
        .session_id = null,
        .model = "claude-opus-4-6",
        .tokens = .{},
    };
    const out = try captureRender(state);
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "ephemeral") != null);
}

test "render: null model falls back to 'stub'" {
    const state: State = .{
        .session_id = "abcd",
        .model = null,
        .tokens = .{},
    };
    const out = try captureRender(state);
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "stub") != null);
}

test "render: every SGR segment is followed by a reset" {
    const state: State = .{
        .session_id = "sid",
        .model = "m",
        .tokens = .{ .input_tokens = 1, .output_tokens = 2 },
    };
    const out = try captureRender(state);
    defer testing.allocator.free(out);

    // For every `\x1b[<Nm` SGR opener in the output that is not itself the
    // reset form (which ends with `[0m`), there must be a matching reset
    // somewhere after it. We assert the simple invariant: the byte stream
    // ends cleanly (last byte is the reset's trailing `m`).
    try testing.expect(out.len >= 4);
    try testing.expectEqualStrings("\x1b[0m", out[out.len - 4 ..]);
}

test "formatSessionShort: short ids pass through, long ids clip at 10" {
    try testing.expectEqualStrings("short", formatSessionShort("short"));
    try testing.expectEqualStrings("0123456789", formatSessionShort("0123456789abcdef"));
    try testing.expectEqualStrings("ephemeral", formatSessionShort(null));
}
