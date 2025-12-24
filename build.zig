const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // MQuickJS C library
    const mquickjs = b.addLibrary(.{
        .name = "mquickjs",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    // C source files from bellard/mquickjs
    const c_sources = &[_][]const u8{
        "mquickjs/mquickjs.c",
        "mquickjs/cutils.c",
        "mquickjs/dtoa.c",
        "mquickjs/libm.c",
        "src/mqjs_stdlib_data.c",
    };

    const c_flags = &[_][]const u8{
        "-std=gnu11",
        "-Wall",
        "-Wextra",
        "-Wno-unused-parameter",
        "-Wno-sign-compare",
        "-fno-sanitize=undefined", // mquickjs uses some UB-ish patterns
    };

    mquickjs.addCSourceFiles(.{
        .files = c_sources,
        .flags = c_flags,
    });

    mquickjs.addIncludePath(b.path("mquickjs"));
    mquickjs.linkLibC();

    // Main server executable
    const exe = b.addExecutable(.{
        .name = "zigttp-server",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    exe.linkLibrary(mquickjs);
    exe.addIncludePath(b.path("mquickjs"));
    exe.linkLibC();

    b.installArtifact(exe);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the server");
    run_step.dependOn(&run_cmd.step);

    // Tests
    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    unit_tests.linkLibrary(mquickjs);
    unit_tests.addIncludePath(b.path("mquickjs"));
    unit_tests.linkLibC();

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    // Benchmark executable (optional if src/bench.zig exists)
    const bench_exists = blk: {
        std.fs.cwd().access("src/bench.zig", .{}) catch |err| {
            if (err == error.FileNotFound) break :blk false;
            @panic("failed to stat src/bench.zig");
        };
        break :blk true;
    };

    if (bench_exists) {
        const bench = b.addExecutable(.{
            .name = "mqjs-bench",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/bench.zig"),
                .target = target,
                .optimize = .ReleaseFast,
            }),
        });

        bench.linkLibrary(mquickjs);
        bench.addIncludePath(b.path("mquickjs"));
        bench.linkLibC();

        b.installArtifact(bench);

        const bench_cmd = b.addRunArtifact(bench);
        const bench_step = b.step("bench", "Run benchmarks");
        bench_step.dependOn(&bench_cmd.step);
    }
}
