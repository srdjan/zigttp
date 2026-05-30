const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const perf_histogram = b.option(bool, "perf_histogram", "Enable interpreter opcode histogram collection") orelse false;
    // analyzer_only strips the VM, SQLite, and libc from the build graph so the
    // static analysis pipeline can target wasm64-freestanding. The normal
    // build leaves it false and behaves exactly as before.
    const analyzer_only = b.option(bool, "analyzer_only", "Build the static analyzer only (no interpreter, SQLite, or libc) for wasm/freestanding targets") orelse false;
    const sdk_dep = b.dependency("zigttp_sdk", .{
        .target = target,
        .optimize = optimize,
    });
    const modules_dep = b.dependency("zigttp_modules", .{
        .target = target,
        .optimize = optimize,
    });
    const mod = b.addModule("zigts", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = !analyzer_only,
    });
    const build_options = b.addOptions();
    build_options.addOption(bool, "perf_histogram", perf_histogram);
    build_options.addOption(bool, "analyzer_only", analyzer_only);
    mod.addOptions("build_options", build_options);
    if (!analyzer_only) {
        mod.addCSourceFile(.{
            .file = b.path("deps/sqlite/sqlite3.c"),
            .flags = &.{ "-D_GNU_SOURCE", "-DHAVE_MREMAP=0", "-DSQLITE_THREADSAFE=0", "-DSQLITE_OMIT_LOAD_EXTENSION", "-DSQLITE_DQS=0" },
        });
        mod.addIncludePath(b.path("deps/sqlite"));
    }
    mod.addImport("zigttp-sdk", sdk_dep.module("zigttp-sdk"));
    mod.addImport("zigttp-modules", modules_dep.module("zigttp-modules"));
}
