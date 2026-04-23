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
        // Slots may legitimately be empty strings (monochrome). The contract
        // is they are *present* as fields - which the struct guarantees.
        _ = t.prompt_label;
        _ = t.status_key;
        _ = t.status_value;
        _ = t.dim;
        _ = t.input_text;
    }
}

test "default theme non-empty slots use valid SGR param syntax" {
    for ([_][]const u8{
        default.prompt_label,
        default.status_key,
        default.status_value,
        default.dim,
        default.input_text,
    }) |slot| {
        for (slot) |b| {
            const ok = (b >= '0' and b <= '9') or b == ';';
            try testing.expect(ok);
        }
    }
}
