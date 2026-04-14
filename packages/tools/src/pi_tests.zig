//! Test aggregator for the in-process tool registry and its first tools.
//! Root module for the `test-pi` step.
//!
//! This file deliberately lives one level above packages/tools/src/pi/ so
//! the module path anchors at packages/tools/src/. That lets the pi tool
//! files reach sibling shared cores (expert_meta.zig, verify_paths_core.zig,
//! describe_rule.zig) via `../../X.zig` without escaping the module subtree
//! that Zig 0.16's module sandboxing enforces.

comptime {
    _ = @import("pi/registry/tool.zig");
    _ = @import("pi/registry/registry.zig");
    _ = @import("pi/tools/zigts_expert_meta.zig");
    _ = @import("pi/tools/zigts_expert_verify_paths.zig");
    _ = @import("pi/tools/zigts_expert_describe_rule.zig");
    _ = @import("pi/tools/zigts_expert_search.zig");
    _ = @import("pi/tools/zigts_expert_edit_simulate.zig");
    _ = @import("pi/tools/zigts_expert_review_patch.zig");
    _ = @import("pi/tools/zigts_expert_features.zig");
    _ = @import("pi/tools/zigts_expert_modules.zig");
    _ = @import("pi/tools/zigts_expert_verify_modules.zig");
    _ = @import("pi/repl.zig");
    _ = @import("pi_app.zig");
}
