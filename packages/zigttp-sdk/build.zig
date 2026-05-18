const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const sdk_mod = b.addModule("zigttp-sdk", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_shim_mod = b.createModule(.{
        .root_source_file = b.path("src/test_shim.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "zigttp-sdk", .module = sdk_mod },
        },
    });

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test_root.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zigttp-sdk", .module = sdk_mod },
                .{ .name = "zigttp-sdk-test-shim", .module = test_shim_mod },
            },
        }),
    });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run zigttp-sdk tests");
    test_step.dependOn(&run_tests.step);
}
