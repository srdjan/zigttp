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

    const tools_dep = b.dependency("zttp_tools", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram,
    });
    const zts_cli_mod = tools_dep.module("zts_cli");
    const zts_expert_skill_mod = tools_dep.module("zts_expert_skill");
    const project_config_mod = tools_dep.module("project_config");

    // Top-level pi entrypoint consumed by the runtime binary.
    const pi_app_mod = b.addModule("pi_app", .{
        .root_source_file = b.path("src/app.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    pi_app_mod.addImport("zts", zts_mod);
    pi_app_mod.addImport("zts_cli", zts_cli_mod);
    pi_app_mod.addImport("zts_expert_skill", zts_expert_skill_mod);
    pi_app_mod.addImport("project_config", project_config_mod);
}
