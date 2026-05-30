const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const perf_histogram = b.option(bool, "perf_histogram", "Enable interpreter opcode histogram collection") orelse false;

    const zigts_dep = b.dependency("zigts", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram,
    });
    const zigts_mod = zigts_dep.module("zigts");

    const tools_dep = b.dependency("zigttp_tools", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram,
    });
    const zigts_cli_mod = tools_dep.module("zigts_cli");

    _ = b.addModule("zigttp_deploy", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
            .{ .name = "zigts_cli", .module = zigts_cli_mod },
        },
    });

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test_root.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{ .name = "zigts", .module = zigts_mod },
                .{ .name = "zigts_cli", .module = zigts_cli_mod },
            },
        }),
    });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run zigttp proof-review package tests");
    test_step.dependOn(&run_tests.step);
}
