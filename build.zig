const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Handler path option (required for main build)
    const handler_path = b.option([]const u8, "handler", "Handler file to precompile (required)");

    // zts module (Zig TypeScript compiler - the primary JS engine)
    const zts_mod = b.addModule("zts", .{
        .root_source_file = b.path("zts/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    // zts tests
    const zts_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("zts/root.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    const run_zts_tests = b.addRunArtifact(zts_tests);
    const zts_test_step = b.step("test-zts", "Run zts unit tests");
    zts_test_step.dependOn(&run_zts_tests.step);

    // Precompile tool (build-time compiler with full zts)
    // Build for host since it runs at build time
    const precompile_exe = b.addExecutable(.{
        .name = "precompile",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/precompile.zig"),
            .target = b.graph.host,
            .optimize = .ReleaseFast,
        }),
    });
    precompile_exe.root_module.addImport("zts", zts_mod);

    // Install precompile tool (optional, for manual use)
    const install_precompile = b.addInstallArtifact(precompile_exe, .{});
    const precompile_step = b.step("precompile", "Build the precompile tool");
    precompile_step.dependOn(&install_precompile.step);

    // Main server executable
    const exe = b.addExecutable(.{
        .name = "zigttp-server",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    // Add zts module to main executable
    exe.root_module.addImport("zts", zts_mod);

    // If handler is specified, precompile it and add as dependency
    if (handler_path) |path| {
        // Run precompile tool to generate embedded handler
        const run_precompile = b.addRunArtifact(precompile_exe);
        run_precompile.addArg(path);
        run_precompile.addArg("src/generated/embedded_handler.zig");

        // Create the generated directory if it doesn't exist
        const mkdir_step = b.addSystemCommand(&.{ "mkdir", "-p", "src/generated" });
        run_precompile.step.dependOn(&mkdir_step.step);

        // Main exe depends on precompile completing
        exe.step.dependOn(&run_precompile.step);

        // Add the generated module
        exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = b.path("src/generated/embedded_handler.zig"),
        });
    } else {
        // No handler specified - create a stub module
        exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = b.path("src/embedded_handler_stub.zig"),
        });
    }

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
    unit_tests.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = b.path("src/embedded_handler_stub.zig"),
    });

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

    // Benchmark executable
    const bench_exe = b.addExecutable(.{
        .name = "zigttp-bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/benchmark.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    bench_exe.root_module.addImport("zts", zts_mod);
    b.installArtifact(bench_exe);

    // Benchmark run command
    const bench_cmd = b.addRunArtifact(bench_exe);
    bench_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        bench_cmd.addArgs(args);
    }
    const bench_step = b.step("bench", "Run performance benchmarks");
    bench_step.dependOn(&bench_cmd.step);
}
