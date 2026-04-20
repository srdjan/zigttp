const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const zigts_dep = b.dependency("zigts", .{
        .target = target,
        .optimize = optimize,
    });
    const zigts_mod = zigts_dep.module("zigts");

    // project_config module (shared between CLI tools and runtime)
    const project_config_mod = b.addModule("project_config", .{
        .root_source_file = b.path("src/project_config.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    project_config_mod.addImport("zigts", zigts_mod);

    // zigts CLI module
    const zigts_cli_mod = b.addModule("zigts_cli", .{
        .root_source_file = b.path("src/zigts_cli.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    zigts_cli_mod.addImport("zigts", zigts_mod);
    zigts_cli_mod.addImport("project_config", project_config_mod);

    // Embedded zigts-expert skill content, consumed by the pi package's
    // `expert_persona` bundle builder. Rooted inside the skill directory so
    // `@embedFile` can reach the sibling markdown files directly.
    _ = b.addModule("zigts_expert_skill", .{
        .root_source_file = b.path("src/skills/zigts-expert/skill_data.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Canonical zigts handler examples consumed by expert_persona as
    // few-shot content. Rooted at examples/ so @embedFile reaches the
    // sibling subdirectories without escaping the module-path sandbox.
    _ = b.addModule("zigts_expert_examples", .{
        .root_source_file = b.path("../../examples/data.zig"),
        .target = target,
        .optimize = optimize,
    });
}
