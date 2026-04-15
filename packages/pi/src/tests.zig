//! Test aggregator for the zigttp_pi package. Root module for `test-pi`.

comptime {
    _ = @import("registry/tool.zig");
    _ = @import("registry/registry.zig");
    _ = @import("tools/zigts_expert_meta.zig");
    _ = @import("tools/zigts_expert_verify_paths.zig");
    _ = @import("tools/zigts_expert_describe_rule.zig");
    _ = @import("tools/zigts_expert_search.zig");
    _ = @import("tools/zigts_expert_edit_simulate.zig");
    _ = @import("tools/zigts_expert_review_patch.zig");
    _ = @import("tools/zigts_expert_features.zig");
    _ = @import("tools/zigts_expert_modules.zig");
    _ = @import("tools/zigts_expert_verify_modules.zig");
    _ = @import("repl.zig");
    _ = @import("tui/term.zig");
    _ = @import("tui/line_editor.zig");
    _ = @import("tui/app.zig");
    _ = @import("turn.zig");
    _ = @import("veto.zig");
    _ = @import("transcript.zig");
    _ = @import("expert_persona.zig");
    _ = @import("loop.zig");
    _ = @import("app.zig");
}
