const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const sdk_dep = b.dependency("zigttp_sdk", .{
        .target = target,
        .optimize = optimize,
    });

    _ = b.addModule("zigttp-ext-demo", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "zigttp-sdk", .module = sdk_dep.module("zigttp-sdk") },
        },
    });
}
