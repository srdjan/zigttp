//! Solarized Dark palette (Ethan Schoonover, solarized.io) mapped onto
//! 256-colour values for accuracy on terminals with a Solarized-aware
//! palette; graceful fallback is the user's responsibility (most terminals
//! approximate). Reference colours:
//!   base03=#002b36 (darkest bg)   base02=#073642   base01=#586e75
//!   base00=#657b83 base0=#839496 base1=#93a1a1 base2=#eee8d5 base3=#fdf6e3
//!   yellow=#b58900 orange=#cb4b16 red=#dc322f magenta=#d33682 violet=#6c71c4
//!   blue=#268bd2 cyan=#2aa198 green=#859900

const theme_mod = @import("../theme.zig");

pub const theme: theme_mod.Theme = .{
    .name = "solarized-dark",
    .display_name = "Solarized Dark",

    // Prompt and status chrome.
    .prompt_label = "1;38;5;33", // blue, bold
    .status_key = "38;5;240", // base01 grey
    .status_value = "38;5;37", // cyan
    .dim = "38;5;239", // base02
    .input_text = "",

    // Panes and accents. Solarized's blue is the canonical accent.
    .accent_primary = "1;38;5;33", // blue, bold
    .accent_muted = "38;5;33",
    .pane_border_active = "38;5;33", // blue
    .pane_border_idle = "38;5;240", // base01
    .selection_bg = "48;5;235", // slightly lighter than base03

    // Tabs.
    .tab_active = "1;38;5;33", // blue, bold
    .tab_idle = "38;5;240", // base01 grey
    .tab_underline = "38;5;33",

    // Verdicts: red / green / yellow / dim.
    .verdict_pass = "1;38;5;160", // red
    .verdict_fixed = "1;38;5;64", // green
    .verdict_error = "1;38;5;136", // yellow
    .verdict_pending = "38;5;240",

    // Severity ladder.
    .severity_info = "38;5;37", // cyan
    .severity_warn = "38;5;136", // yellow
    .severity_error = "1;38;5;160", // red

    // Property + witness delta badges.
    .delta_promoted = "38;5;64", // green
    .delta_demoted = "38;5;160", // red

    // Property chips.
    .chip_idle = "38;5;245",
    .chip_focus = "1;38;5;33",

    // Digests.
    .digest = "38;5;240",
};
