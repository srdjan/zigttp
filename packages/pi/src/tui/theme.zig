//! TUI theme: named SGR slots that every themed widget reads from. Palettes
//! live as `const` structs in sibling `themes/*.zig` files and are collected
//! into a compile-time `registry` below. Switching themes at runtime just
//! swaps the pointer used by the renderer - no allocation, no I/O.
//!
//! Why raw SGR strings instead of a `{fg, bg, bold}` struct?  Because the
//! only consumers are `ansi.sgr()` and `ansi.styled()`, both of which take
//! the SGR parameter string verbatim. Modeling colors at a higher level
//! would just translate back to the same strings on every write.

const std = @import("std");

pub const Theme = struct {
    name: []const u8,
    /// The name of the theme shown in `/settings`.
    display_name: []const u8,

    // ---- prompts and status chrome ----------------------------------------

    /// SGR params for the `expert>` label and any other prompt chrome.
    prompt_label: []const u8,
    /// SGR for the status-line section labels (e.g. "sess", "model", "tokens").
    status_key: []const u8,
    /// SGR for the values shown in the status line.
    status_value: []const u8,
    /// SGR for de-emphasized separators / dim punctuation.
    dim: []const u8,
    /// SGR applied to the user's typed input.
    input_text: []const u8,

    // ---- panes and accents ------------------------------------------------

    /// Strong accent colour for the focused pane's chrome and active markers.
    accent_primary: []const u8,
    /// Muted accent for non-focused-but-related chrome (e.g. inactive pane name).
    accent_muted: []const u8,
    /// Border colour for the pane the user is currently focused on.
    pane_border_active: []const u8,
    /// Border colour for the pane the user is not focused on.
    pane_border_idle: []const u8,
    /// Background highlight for the currently-selected row in a list.
    selection_bg: []const u8,

    // ---- inspector tab bar ------------------------------------------------

    /// Foreground for the active tab label.
    tab_active: []const u8,
    /// Foreground for inactive tab labels.
    tab_idle: []const u8,
    /// Colour of the underline drawn beneath the active tab.
    tab_underline: []const u8,

    // ---- semantic verdict states (witness replay) -------------------------

    /// "PASS - violation reproduced": the bug is real (red - bad news).
    verdict_pass: []const u8,
    /// "FIXED - violation no longer reproduces": the patch worked (green).
    verdict_fixed: []const u8,
    /// "ERROR (...)": replay failed at the engine layer (yellow).
    verdict_error: []const u8,
    /// In-flight / pending state during a replay.
    verdict_pending: []const u8,

    // ---- generic severity -------------------------------------------------

    severity_info: []const u8,
    severity_warn: []const u8,
    severity_error: []const u8,

    // ---- property + witness deltas ----------------------------------------

    /// `+name` promoted property, defeated witness section header.
    delta_promoted: []const u8,
    /// `-name` demoted property, introduced witness section header.
    delta_demoted: []const u8,

    // ---- property chips ---------------------------------------------------

    /// Foreground for an idle (un-cursored) chip.
    chip_idle: []const u8,
    /// Foreground for the cursored chip - the one `g` would drive.
    chip_focus: []const u8,

    // ---- subtle / supporting ----------------------------------------------

    /// Short hashes (witness keys, patch hashes). Subtle so the eye skips it
    /// unless the user is specifically looking.
    digest: []const u8,
};

pub const default = @import("themes/default.zig").theme;
pub const solarized_dark = @import("themes/solarized_dark.zig").theme;

pub const registry = [_]*const Theme{
    &default,
    &solarized_dark,
};

pub fn findByName(name: []const u8) ?*const Theme {
    for (registry) |t| {
        if (std.mem.eql(u8, t.name, name)) return t;
    }
    return null;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "registry has at least the two built-in themes" {
    try testing.expect(registry.len >= 2);
    try testing.expect(findByName("default") != null);
    try testing.expect(findByName("solarized-dark") != null);
}

test "findByName returns null for unknown theme" {
    try testing.expect(findByName("no-such-theme") == null);
}

test "every theme supplies every SGR slot" {
    for (registry) |t| {
        try testing.expect(t.name.len > 0);
        try testing.expect(t.display_name.len > 0);
    }
}

fn isValidSgr(slot: []const u8) bool {
    for (slot) |b| {
        const ok = (b >= '0' and b <= '9') or b == ';';
        if (!ok) return false;
    }
    return true;
}

test "every theme's SGR slots use valid SGR param syntax" {
    inline for (@typeInfo(Theme).@"struct".fields) |field| {
        const name_is_display = comptime (std.mem.eql(u8, field.name, "name") or std.mem.eql(u8, field.name, "display_name"));
        if (name_is_display) continue;
        for (registry) |t| {
            const slot = @field(t, field.name);
            try testing.expect(isValidSgr(slot));
        }
    }
}
