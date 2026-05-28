//! Typed `RepairIntent` enum attached to veto-able diagnostics so the
//! `zigttp expert` agent can pick an apply primitive directly instead of
//! parsing the prose `help` text.
//!
//! Slice B of the expert-strategy §5 roadmap. The seed set covers the
//! canonical ZigTS diagnostics (ZTS6xx), the handler verifier's structural
//! checks (ZTS3xx), and the data-label flow checker (ZTS4xx). Variants
//! `insert_guard_before_line` and `add_trailing_return` already exist as
//! lower-level `EditIntentKind` primitives in `repair_plan.zig`; this enum
//! is the higher-level *intent* a checker emits, which a planner then
//! lowers to one of those edit primitives.
//!
//! Importable from `analyzer_only` builds: pure enum, no I/O.

const std = @import("std");

pub const RepairIntent = enum {
    // ZTS6xx — canonical ZigTS profile (strict_checker)
    replace_ternary_with_if,
    lift_default_to_body,
    name_const_above_template,
    replace_let_with_const,
    replace_arrow_with_function,
    replace_export_arrow_with_function,
    replace_compound_assign_with_explicit,
    lead_with_spread,
    widen_signature_drop_spread,
    flatten_destructure,
    add_capability_declaration,
    add_spec_assertion,

    // ZTS3xx / ZTS4xx — structural and flow checkers
    insert_guard_before_line,
    add_trailing_return,

    // Slice G additions (expert-strategy §5 Wave 1) — variants emitted by the
    // `zigts_expert_ast_rewrite` tool when the model targets canonicalize
    // refactors that the existing `replace_let_with_const` variant does not
    // distinguish at the call site. Appended at the end so existing variants
    // keep their `@intFromEnum` value; adding them to a rule's `.repair`
    // changes `policyHash`, but merely defining the variants does not.
    canonicalize_for_of_const,
    canonicalize_capability_key_alias,

    pub fn asString(self: RepairIntent) []const u8 {
        return @tagName(self);
    }

    /// Parse a tag name back into a `RepairIntent`. Returns `null` for
    /// unknown strings; callers decide whether that is a hard error
    /// (`zigts_expert_ast_rewrite`) or a soft skip.
    pub fn fromString(s: []const u8) ?RepairIntent {
        return std.meta.stringToEnum(RepairIntent, s);
    }
};

test "RepairIntent.asString returns enum tag name" {
    try std.testing.expectEqualStrings(
        "replace_ternary_with_if",
        RepairIntent.replace_ternary_with_if.asString(),
    );
    try std.testing.expectEqualStrings(
        "insert_guard_before_line",
        RepairIntent.insert_guard_before_line.asString(),
    );
}

test "RepairIntent.fromString round-trips known tags" {
    try std.testing.expectEqual(
        RepairIntent.replace_let_with_const,
        RepairIntent.fromString("replace_let_with_const").?,
    );
    try std.testing.expectEqual(
        RepairIntent.canonicalize_for_of_const,
        RepairIntent.fromString("canonicalize_for_of_const").?,
    );
    try std.testing.expectEqual(
        RepairIntent.canonicalize_capability_key_alias,
        RepairIntent.fromString("canonicalize_capability_key_alias").?,
    );
    try std.testing.expect(RepairIntent.fromString("not_a_real_intent") == null);
}
