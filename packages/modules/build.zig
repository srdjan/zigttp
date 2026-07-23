const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const sdk_dep = b.dependency("zttp_sdk", .{
        .target = target,
        .optimize = optimize,
    });
    const sdk_mod = sdk_dep.module("zttp-sdk");
    const test_shim_mod = b.createModule(.{
        .root_source_file = sdk_dep.path("src/test_shim.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "zttp-sdk", .module = sdk_mod },
        },
    });

    _ = b.addModule("zttp-modules", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
        .imports = &.{
            .{ .name = "zttp-sdk", .module = sdk_mod },
        },
    });

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test_root.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{ .name = "zttp-sdk", .module = sdk_mod },
                .{ .name = "zttp-sdk-test-shim", .module = test_shim_mod },
            },
        }),
    });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run zttp-modules tests");
    test_step.dependOn(&run_tests.step);
}
