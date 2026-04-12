const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const sdk_dep = b.dependency("zigttp_sdk", .{
        .target = target,
        .optimize = optimize,
    });
    const ext_demo_dep = b.dependency("zigttp_ext_demo", .{
        .target = target,
        .optimize = optimize,
    });

    const mod = b.addModule("zigts", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    mod.addCSourceFile(.{
        .file = b.path("deps/sqlite/sqlite3.c"),
        .flags = &.{ "-D_GNU_SOURCE", "-DHAVE_MREMAP=0", "-DSQLITE_THREADSAFE=0", "-DSQLITE_OMIT_LOAD_EXTENSION", "-DSQLITE_DQS=0" },
    });
    mod.addIncludePath(b.path("deps/sqlite"));
    mod.addImport("zigttp-sdk", sdk_dep.module("zigttp-sdk"));
    mod.addImport("zigttp-ext-demo", ext_demo_dep.module("zigttp-ext-demo"));
}
