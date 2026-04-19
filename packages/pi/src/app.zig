//! Top-level entrypoint for `zigts expert`.
//!
//! This file lives at src/ level (not inside pi/) so its module path anchors
//! at packages/tools/src/ — the same anchoring used by pi_tests.zig. That
//! lets the pi subtree's sibling imports (`../../expert_meta.zig` etc.) stay
//! inside the module sandbox Zig 0.16 enforces.

const std = @import("std");
const registry_mod = @import("registry/registry.zig");
const repl = @import("repl.zig");
const tui_app = @import("tui/app.zig");

const meta_tool = @import("tools/zigts_expert_meta.zig");
const verify_paths_tool = @import("tools/zigts_expert_verify_paths.zig");
const describe_rule_tool = @import("tools/zigts_expert_describe_rule.zig");
const search_tool = @import("tools/zigts_expert_search.zig");
const edit_simulate_tool = @import("tools/zigts_expert_edit_simulate.zig");
const review_patch_tool = @import("tools/zigts_expert_review_patch.zig");
const features_tool = @import("tools/zigts_expert_features.zig");
const modules_tool = @import("tools/zigts_expert_modules.zig");
const verify_modules_tool = @import("tools/zigts_expert_verify_modules.zig");
const workspace_list_files_tool = @import("tools/workspace_list_files.zig");
const workspace_read_file_tool = @import("tools/workspace_read_file.zig");
const workspace_search_text_tool = @import("tools/workspace_search_text.zig");
const zigts_check_tool = @import("tools/zigts_check.zig");
const zig_build_step_tool = @import("tools/zig_build_step.zig");
const zig_test_step_tool = @import("tools/zig_test_step.zig");

const Registry = registry_mod.Registry;

pub fn buildRegistry(allocator: std.mem.Allocator) !Registry {
    var reg: Registry = .{};
    errdefer reg.deinit(allocator);

    try reg.register(allocator, meta_tool.tool);
    try reg.register(allocator, verify_paths_tool.tool);
    try reg.register(allocator, describe_rule_tool.tool);
    try reg.register(allocator, search_tool.tool);
    try reg.register(allocator, edit_simulate_tool.tool);
    try reg.register(allocator, review_patch_tool.tool);
    try reg.register(allocator, features_tool.tool);
    try reg.register(allocator, modules_tool.tool);
    try reg.register(allocator, verify_modules_tool.tool);
    try reg.register(allocator, workspace_list_files_tool.tool);
    try reg.register(allocator, workspace_read_file_tool.tool);
    try reg.register(allocator, workspace_search_text_tool.tool);
    try reg.register(allocator, zigts_check_tool.tool);
    try reg.register(allocator, zig_build_step_tool.tool);
    try reg.register(allocator, zig_test_step_tool.tool);

    return reg;
}

pub fn run(allocator: std.mem.Allocator) !void {
    var registry = try buildRegistry(allocator);
    defer registry.deinit(allocator);

    const is_tty = std.c.isatty(std.c.STDIN_FILENO) != 0;
    if (is_tty) {
        try tui_app.run(allocator, &registry);
    } else {
        try repl.run(allocator, &registry);
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "buildRegistry registers every first-party compiler primitive" {
    var reg = try buildRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 15), reg.count());
    for ([_][]const u8{
        "zigts_expert_meta",
        "zigts_expert_verify_paths",
        "zigts_expert_describe_rule",
        "zigts_expert_search",
        "zigts_expert_edit_simulate",
        "zigts_expert_review_patch",
        "zigts_expert_features",
        "zigts_expert_modules",
        "zigts_expert_verify_modules",
        "workspace_list_files",
        "workspace_read_file",
        "workspace_search_text",
        "zigts_check",
        "zig_build_step",
        "zig_test_step",
    }) |expected| {
        if (reg.findByName(expected) == null) {
            std.debug.print("missing tool: {s}\n", .{expected});
            return error.TestFailed;
        }
    }
}

fn expectOkContains(outcome: *repl.DispatchOutcome, allocator: std.mem.Allocator, needle: []const u8) !void {
    switch (outcome.*) {
        .result => |*r| {
            defer r.deinit(allocator);
            try testing.expect(r.ok);
            try testing.expect(std.mem.indexOf(u8, r.body, needle) != null);
        },
        else => return error.TestFailed,
    }
}

test "buildRegistry + dispatchLine end-to-end against every tool" {
    var reg = try buildRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var meta_outcome = try repl.dispatchLine(testing.allocator, &reg, "zigts_expert_meta");
    try expectOkContains(&meta_outcome, testing.allocator, "\"compiler_version\"");

    var rule_outcome = try repl.dispatchLine(testing.allocator, &reg, "zigts_expert_describe_rule ZTS303");
    try expectOkContains(&rule_outcome, testing.allocator, "\"ZTS303\"");

    var search_outcome = try repl.dispatchLine(testing.allocator, &reg, "zigts_expert_search result");
    try expectOkContains(&search_outcome, testing.allocator, "\"code\":");
}
