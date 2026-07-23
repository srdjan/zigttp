const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const perf_histogram = b.option(bool, "perf_histogram", "Enable interpreter opcode histogram collection") orelse false;
    const zts_dep = b.dependency("zts", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram,
    });
    const zts_mod = zts_dep.module("zts");

    // project_config module (shared between CLI tools and runtime)
    const project_config_mod = b.addModule("project_config", .{
        .root_source_file = b.path("src/project_config.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    project_config_mod.addImport("zts", zts_mod);

    // zts CLI module
    const zts_cli_mod = b.addModule("zts_cli", .{
        .root_source_file = b.path("src/zts_cli.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    zts_cli_mod.addImport("zts", zts_mod);
    zts_cli_mod.addImport("project_config", project_config_mod);

    // Embedded zts-expert catalog (skill prose + vendored canonical
    // handler examples), consumed by the pi package's `expert_persona`
    // bundle builder. Rooted inside the skill directory so @embedFile
    // reaches the sibling markdown and example subtrees directly.
    _ = b.addModule("zts_expert_skill", .{
        .root_source_file = b.path("src/skills/zts-expert/skill_data.zig"),
        .target = target,
        .optimize = optimize,
    });
}
