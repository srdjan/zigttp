const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zigts_dep = b.dependency("zigts", .{
        .target = target,
        .optimize = optimize,
    });
    const zigts_mod = zigts_dep.module("zigts");

    const tools_dep = b.dependency("zigttp_tools", .{
        .target = target,
        .optimize = optimize,
    });
    const zigts_cli_mod = tools_dep.module("zigts_cli");
    const zigts_expert_skill_mod = tools_dep.module("zigts_expert_skill");

    // Top-level pi entrypoint consumed by the runtime binary.
    const pi_app_mod = b.addModule("pi_app", .{
        .root_source_file = b.path("src/app.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    pi_app_mod.addImport("zigts", zigts_mod);
    pi_app_mod.addImport("zigts_cli", zigts_cli_mod);
    pi_app_mod.addImport("zigts_expert_skill", zigts_expert_skill_mod);
}
