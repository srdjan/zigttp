//! Default palette. Subdued foreground colors suitable for most terminal
//! backgrounds (both dark and light), no bright backgrounds, no blinking.

const theme_mod = @import("../theme.zig");

pub const theme: theme_mod.Theme = .{
    .name = "default",
    .display_name = "Default",

    // Prompt and status chrome.
    .prompt_label = "1;36",
    .status_key = "2;37",
    .status_value = "36",
    .dim = "2;90",
    .input_text = "",

    // Panes and accents. Bold cyan as the primary accent gives the focused
    // pane a clear "this one's live" cue. Idle borders are dim grey.
    .accent_primary = "1;36",
    .accent_muted = "2;36",
    .pane_border_active = "36",
    .pane_border_idle = "2;90",
    .selection_bg = "48;5;236",

    // Tabs: active is bold + bright cyan with a same-colour underline; idle
    // is plain dim text so the active tab visibly leads.
    .tab_active = "1;38;5;51",
    .tab_idle = "2;37",
    .tab_underline = "38;5;51",

    // Verdicts: PASS = red (violation reproduced is bad), FIXED = green,
    // ERROR = yellow, pending = bright black so a queued replay reads as
    // "in-flight, please wait".
    .verdict_pass = "1;31",
    .verdict_fixed = "1;32",
    .verdict_error = "1;33",
    .verdict_pending = "1;90",

    // Severity ladder for system notes / status row.
    .severity_info = "36",
    .severity_warn = "33",
    .severity_error = "1;31",

    // Property + witness delta badges. Promoted reads as "the patch closed
    // something" (green); demoted reads as "the patch broke something" (red).
    .delta_promoted = "32",
    .delta_demoted = "31",

    // Property chips. Idle chips read as quiet labels; focused chip stands
    // out via bold + bright accent.
    .chip_idle = "37",
    .chip_focus = "1;38;5;51",

    // Hashes / witness keys: dim so they sit in the background.
    .digest = "2;90",
};
