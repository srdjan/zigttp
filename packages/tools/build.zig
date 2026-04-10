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

    // Precompile module (for root build to create precompile exe)
    const precompile_mod = b.addModule("precompile", .{
        .root_source_file = b.path("src/precompile.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    precompile_mod.addImport("zigts", zigts_mod);
}
