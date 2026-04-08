const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const zigttp_sdk_dep = b.dependency("zigttp_sdk", .{
        .target = target,
        .optimize = optimize,
    });
    const zigttp_sdk_mod = zigttp_sdk_dep.module("zigttp-sdk");
    const ext_demo_dep = b.dependency("zigttp_ext_demo", .{
        .target = target,
        .optimize = optimize,
    });
    const ext_demo_mod = ext_demo_dep.module("zigttp-ext-demo");

    // Handler path option (required for main build)
    const handler_path = b.option([]const u8, "handler", "Handler file to precompile (required)");
    const aot_enabled = b.option(bool, "aot", "Enable native AOT handler generation") orelse false;
    const verify_enabled = b.option(bool, "verify", "Enable compile-time handler verification") orelse false;
    const contract_enabled = b.option(bool, "contract", "Emit handler contract manifest (contract.json)") orelse false;
    const openapi_enabled = b.option(bool, "openapi", "Emit OpenAPI manifest (openapi.json)") orelse false;
    const sdk_target = b.option([]const u8, "sdk", "Emit generated SDK artifact (values: ts)");
    const sql_schema_path = b.option([]const u8, "sql-schema", "SQLite schema snapshot (.sqlite) or schema SQL file for zigttp:sql validation");
    const policy_path = b.option([]const u8, "policy", "Capability policy JSON file for precompiled handlers");
    const system_path = b.option([]const u8, "system", "System definition file for cross-handler contract linking");
    const deploy_target = b.option([]const u8, "deploy", "Generate deployment manifest (values: aws)");
    const replay_path = b.option([]const u8, "replay", "Replay trace file for regression verification at build time");
    const test_file_path = b.option([]const u8, "test-file", "Run handler tests from JSONL file at build time");
    const prove_spec = b.option([]const u8, "prove", "Prove upgrade safety (format: contract.json or contract.json:traces.jsonl)");
    const generate_tests = b.option(bool, "generate-tests", "Generate exhaustive test cases from path analysis") orelse false;

    // External enrichment flags (optional, for cross-referencing with code generators)
    const manifest_path = b.option([]const u8, "manifest", "External manifest JSON for cross-referencing against handler contract");
    const expect_properties_path = b.option([]const u8, "expect-properties", "Expected handler properties JSON for build-time verification");
    const data_labels_path = b.option([]const u8, "data-labels", "External data label declarations JSON for flow checker enrichment");
    const fault_severity_path = b.option([]const u8, "fault-severity", "External fault severity overrides JSON for coverage analysis");
    const generator_pack_path = b.option([]const u8, "generator-pack", "Generator integration pack JSON for external manifest/property/data-label/replay/report wiring");
    const report_format = b.option([]const u8, "report", "Emit structured build report (values: json)");

    // zigts module (Zig TypeScript compiler - the primary JS engine)
    const zigts_mod = b.addModule("zigts", .{
        .root_source_file = b.path("zigts/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    zigts_mod.addCSourceFile(.{
        .file = b.path("deps/sqlite/sqlite3.c"),
        .flags = &.{ "-DSQLITE_THREADSAFE=0", "-DSQLITE_OMIT_LOAD_EXTENSION", "-DSQLITE_DQS=0" },
    });
    zigts_mod.addIncludePath(b.path("deps/sqlite"));
    zigts_mod.addImport("zigttp-sdk", zigttp_sdk_mod);
    zigts_mod.addImport("zigttp-ext-demo", ext_demo_mod);

    // zigts tests
    const zigts_tests_root = b.createModule(.{
        .root_source_file = b.path("zigts/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    zigts_tests_root.addImport("zigttp-sdk", zigttp_sdk_mod);
    zigts_tests_root.addImport("zigttp-ext-demo", ext_demo_mod);
    const zigts_tests = b.addTest(.{
        .root_module = zigts_tests_root,
    });
    zigts_tests.root_module.addCSourceFile(.{
        .file = b.path("deps/sqlite/sqlite3.c"),
        .flags = &.{ "-DSQLITE_THREADSAFE=0", "-DSQLITE_OMIT_LOAD_EXTENSION", "-DSQLITE_DQS=0" },
    });
    zigts_tests.root_module.addIncludePath(b.path("deps/sqlite"));
    const run_zigts_tests = b.addRunArtifact(zigts_tests);
    const zigts_test_step = b.step("test-zigts", "Run zigts unit tests");
    zigts_test_step.dependOn(&run_zigts_tests.step);

    const precompile_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/precompile.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    precompile_tests.root_module.addImport("zigts", zigts_mod);
    const run_precompile_tests = b.addRunArtifact(precompile_tests);
    const precompile_test_step = b.step("test-precompile", "Run precompile tool tests");
    precompile_test_step.dependOn(&run_precompile_tests.step);

    const prop_expect_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/property_expectations.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    prop_expect_tests.root_module.addImport("zigts", zigts_mod);
    const run_prop_expect_tests = b.addRunArtifact(prop_expect_tests);
    const prop_expect_test_step = b.step("test-property-expectations", "Run property expectations tool tests");
    prop_expect_test_step.dependOn(&run_prop_expect_tests.step);

    const rollout_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/system_rollout.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    rollout_tests.root_module.addImport("zigts", zigts_mod);
    const run_rollout_tests = b.addRunArtifact(rollout_tests);
    const rollout_test_step = b.step("test-rollout", "Run rollout planner tests");
    rollout_test_step.dependOn(&run_rollout_tests.step);

    const zigts_cli_mod = b.createModule(.{
        .root_source_file = b.path("tools/zigts_cli.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    zigts_cli_mod.addImport("zigts", zigts_mod);
    const project_config_mod = b.createModule(.{
        .root_source_file = b.path("src/project_config.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    project_config_mod.addImport("zigts", zigts_mod);
    zigts_cli_mod.addImport("project_config", project_config_mod);

    // Internal precompile tool used by build steps and the zigts CLI.
    const precompile_exe = b.addExecutable(.{
        .name = "precompile",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/precompile.zig"),
            .target = b.graph.host,
            .optimize = .ReleaseFast,
        }),
    });
    precompile_exe.root_module.addImport("zigts", zigts_mod);

    // Runtime/project CLI
    const exe = b.addExecutable(.{
        .name = "zigttp",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    // Add zigts module to main executable
    exe.root_module.addImport("zigts", zigts_mod);
    exe.root_module.addImport("zigts_cli", zigts_cli_mod);
    exe.root_module.addImport("project_config", project_config_mod);

    // If handler is specified, precompile it and add as dependency
    if (handler_path) |path| {
        // Run precompile tool to generate embedded handler
        const run_precompile = b.addRunArtifact(precompile_exe);
        if (aot_enabled) {
            run_precompile.addArg("--aot");
        }
        if (verify_enabled) {
            run_precompile.addArg("--verify");
        }
        if (openapi_enabled) {
            run_precompile.addArg("--openapi");
        }
        if (sdk_target) |sdk| {
            run_precompile.addArg("--sdk");
            run_precompile.addArg(sdk);
        }
        if (sql_schema_path) |sql_schema| {
            run_precompile.addArg("--sql-schema");
            run_precompile.addArg(sql_schema);
        }
        if (system_path) |system| {
            run_precompile.addArg("--system");
            run_precompile.addArg(system);
        }
        if (contract_enabled or deploy_target != null) {
            run_precompile.addArg("--contract");
        }
        if (deploy_target) |dt| {
            run_precompile.addArg("--deploy");
            run_precompile.addArg(dt);
        }
        if (policy_path) |policy| {
            run_precompile.addArg("--policy");
            run_precompile.addArg(policy);
        }
        if (replay_path) |rp| {
            run_precompile.addArg("--replay");
            run_precompile.addArg(rp);
        }
        if (test_file_path) |tf| {
            run_precompile.addArg("--test-file");
            run_precompile.addArg(tf);
        }
        if (prove_spec) |ps| {
            run_precompile.addArg("--prove");
            run_precompile.addArg(ps);
        }
        if (generate_tests) {
            run_precompile.addArg("--generate-tests");
        }
        if (manifest_path) |mp| {
            run_precompile.addArg("--manifest");
            run_precompile.addArg(mp);
        }
        if (expect_properties_path) |ep| {
            run_precompile.addArg("--expect-properties");
            run_precompile.addArg(ep);
        }
        if (data_labels_path) |dl| {
            run_precompile.addArg("--data-labels");
            run_precompile.addArg(dl);
        }
        if (fault_severity_path) |fs| {
            run_precompile.addArg("--fault-severity");
            run_precompile.addArg(fs);
        }
        if (generator_pack_path) |gp| {
            run_precompile.addArg("--generator-pack");
            run_precompile.addArg(gp);
        }
        if (report_format) |rf| {
            run_precompile.addArg("--report");
            run_precompile.addArg(rf);
        }
        run_precompile.addArg(path);
        run_precompile.addArg("src/generated/embedded_handler.zig");

        // Create the generated directories if they don't exist
        const mkdir_step = b.addSystemCommand(&.{ "/bin/mkdir", "-p", "src/generated" });
        run_precompile.step.dependOn(&mkdir_step.step);

        if (deploy_target != null) {
            const mkdir_deploy = b.addSystemCommand(&.{ "/bin/mkdir", "-p", "src/generated/deploy" });
            run_precompile.step.dependOn(&mkdir_deploy.step);
        }

        // Main exe depends on precompile completing
        exe.step.dependOn(&run_precompile.step);

        // Add the generated module (with zigts dependency for transpiled handlers)
        exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = b.path("src/generated/embedded_handler.zig"),
            .imports = &.{
                .{ .name = "zigts", .module = zigts_mod },
            },
        });
    } else {
        // No handler specified - create a stub module
        exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = b.path("src/embedded_handler_stub.zig"),
            .imports = &.{
                .{ .name = "zigts", .module = zigts_mod },
            },
        });
    }

    b.installArtifact(exe);

    // Compiler/analyzer CLI
    const zigts_exe = b.addExecutable(.{
        .name = "zigts",
        .root_module = zigts_cli_mod,
    });
    b.installArtifact(zigts_exe);

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

    // Add zigts module to tests
    unit_tests.root_module.addImport("zigts", zigts_mod);
    unit_tests.root_module.addImport("zigts_cli", zigts_cli_mod);
    unit_tests.root_module.addImport("project_config", project_config_mod);
    unit_tests.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = b.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
        },
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
    test_step.dependOn(&run_precompile_tests.step);
    test_step.dependOn(&run_prop_expect_tests.step);
    test_step.dependOn(&run_rollout_tests.step);

    // ZRuntime tests (native Zig runtime)
    const zruntime_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zruntime.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    zruntime_tests.root_module.addImport("zigts", zigts_mod);
    zruntime_tests.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = b.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
        },
    });
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
    bench_exe.root_module.addImport("zigts", zigts_mod);
    bench_exe.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = b.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
        },
    });
    b.installArtifact(bench_exe);

    // Benchmark run command
    const bench_cmd = b.addRunArtifact(bench_exe);
    bench_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        bench_cmd.addArgs(args);
    }

    // Release build step (with handler precompilation if provided)
    const release_step = b.step("release", "Build optimized release binaries (zigttp, zigts)");
    release_step.dependOn(b.getInstallStep());
    if (handler_path) |_| {
        const release_note = b.addSystemCommand(if (aot_enabled) &.{
            "echo",
            "Release build with embedded bytecode + AOT: zig-out/bin/zigttp",
        } else &.{
            "echo",
            "Release build with embedded bytecode: zig-out/bin/zigttp",
        });
        release_note.step.dependOn(release_step);
    } else {
        const release_note = b.addSystemCommand(&.{
            "echo",
            "Release build: zig-out/bin/zigttp and zig-out/bin/zigts",
        });
        release_note.step.dependOn(release_step);
    }
    const bench_step = b.step("bench", "Run performance benchmarks");
    bench_step.dependOn(&bench_cmd.step);

    // System linking step (cross-handler contract verification)
    if (system_path) |sys_path| {
        const run_system = b.addRunArtifact(zigts_exe);
        run_system.addArg("link");
        run_system.addArg(sys_path);
        if (b.args) |args| {
            run_system.addArgs(args);
        }
        const system_step = b.step("system", "Cross-handler contract linking");
        system_step.dependOn(&run_system.step);
    }
}
