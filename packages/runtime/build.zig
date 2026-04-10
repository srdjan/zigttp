const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const zigts_dep = b.dependency("zigts", .{
        .target = target,
        .optimize = optimize,
    });
    const tools_dep = b.dependency("zigttp_tools", .{
        .target = target,
        .optimize = optimize,
    });

    const mod = b.addModule("runtime", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    mod.addImport("zigts", zigts_dep.module("zigts"));
    mod.addImport("zigts_cli", tools_dep.module("zigts_cli"));
    mod.addImport("project_config", tools_dep.module("project_config"));
}
