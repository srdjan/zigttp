//! Solarized Dark palette (Ethan Schoonover, solarized.io) mapped onto the
//! eight standard ANSI colors. Uses 256-color values for accuracy on
//! terminals with a Solarized-aware palette; graceful fallback is the
//! user's responsibility (most terminals approximate).

const theme_mod = @import("../theme.zig");

pub const theme: theme_mod.Theme = .{
    .name = "solarized-dark",
    .display_name = "Solarized Dark",
    // Blue (#268bd2). Bold emphasizes the prompt chrome.
    .prompt_label = "1;38;5;33",
    // base01 gray for chrome labels.
    .status_key = "38;5;240",
    // Cyan (#2aa198) for values.
    .status_value = "38;5;37",
    // base02 very dim for punctuation separators.
    .dim = "38;5;239",
    // base0 (default text) - let the terminal decide.
    .input_text = "",
};
