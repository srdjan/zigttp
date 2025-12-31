const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // zts module (Zig TypeScript compiler - the primary JS engine)
    const zts_mod = b.addModule("zts", .{
        .root_source_file = b.path("zts/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // zts tests
    const zts_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("zts/root.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_zts_tests = b.addRunArtifact(zts_tests);
    const zts_test_step = b.step("test-zts", "Run zts unit tests");
    zts_test_step.dependOn(&run_zts_tests.step);

    // Main server executable
    const exe = b.addExecutable(.{
        .name = "zigttp-server",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Add zts module to main executable
    exe.root_module.addImport("zts", zts_mod);

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

    // Add zts module to tests
    unit_tests.root_module.addImport("zts", zts_mod);

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    // ZRuntime tests (native Zig runtime)
    const zruntime_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zruntime.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    zruntime_tests.root_module.addImport("zts", zts_mod);
    const run_zruntime_tests = b.addRunArtifact(zruntime_tests);
    const zruntime_test_step = b.step("test-zruntime", "Run ZRuntime unit tests");
    zruntime_test_step.dependOn(&run_zruntime_tests.step);
}
