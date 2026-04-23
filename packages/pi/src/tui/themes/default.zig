//! Default palette. Subdued foreground colors suitable for most terminal
//! backgrounds (both dark and light), no bright backgrounds, no blinking.

const theme_mod = @import("../theme.zig");

pub const theme: theme_mod.Theme = .{
    .name = "default",
    .display_name = "Default",
    // Bold cyan for the prompt label - stands out without screaming.
    .prompt_label = "1;36",
    // Dim so labels read as chrome, not content.
    .status_key = "2;37",
    .status_value = "36",
    .dim = "2;90",
    .input_text = "",
};
