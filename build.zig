const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Handler path option (required for main build)
    const handler_path = b.option([]const u8, "handler", "Handler file to precompile (required)");
    const aot_enabled = b.option(bool, "aot", "Enable native AOT handler generation") orelse false;
    const verify_enabled = b.option(bool, "verify", "Enable compile-time handler verification") orelse false;
    const contract_enabled = b.option(bool, "contract", "Emit handler contract manifest (contract.json)") orelse false;
    const openapi_enabled = b.option(bool, "openapi", "Emit OpenAPI manifest (openapi.json)") orelse false;
    const sql_schema_path = b.option([]const u8, "sql-schema", "SQLite schema snapshot (.sqlite) or schema SQL file for zigttp:sql validation");
    const policy_path = b.option([]const u8, "policy", "Capability policy JSON file for precompiled handlers");
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

    // zts module (Zig TypeScript compiler - the primary JS engine)
    const zts_mod = b.addModule("zts", .{
        .root_source_file = b.path("zts/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    zts_mod.linkSystemLibrary("sqlite3", .{});

    // zts tests
    const zts_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("zts/root.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    zts_tests.root_module.linkSystemLibrary("sqlite3", .{});
    const run_zts_tests = b.addRunArtifact(zts_tests);
    const zts_test_step = b.step("test-zts", "Run zts unit tests");
    zts_test_step.dependOn(&run_zts_tests.step);

    const precompile_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/precompile.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    precompile_tests.root_module.addImport("zts", zts_mod);
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
    prop_expect_tests.root_module.addImport("zts", zts_mod);
    const run_prop_expect_tests = b.addRunArtifact(prop_expect_tests);
    const prop_expect_test_step = b.step("test-property-expectations", "Run property expectations tool tests");
    prop_expect_test_step.dependOn(&run_prop_expect_tests.step);

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
        if (aot_enabled) {
            run_precompile.addArg("--aot");
        }
        if (verify_enabled) {
            run_precompile.addArg("--verify");
        }
        if (openapi_enabled) {
            run_precompile.addArg("--openapi");
        }
        if (sql_schema_path) |sql_schema| {
            run_precompile.addArg("--sql-schema");
            run_precompile.addArg(sql_schema);
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

        // Add the generated module (with zts dependency for transpiled handlers)
        exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = b.path("src/generated/embedded_handler.zig"),
            .imports = &.{
                .{ .name = "zts", .module = zts_mod },
            },
        });
    } else {
        // No handler specified - create a stub module
        exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = b.path("src/embedded_handler_stub.zig"),
            .imports = &.{
                .{ .name = "zts", .module = zts_mod },
            },
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
        .imports = &.{
            .{ .name = "zts", .module = zts_mod },
        },
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
    test_step.dependOn(&run_precompile_tests.step);
    test_step.dependOn(&run_prop_expect_tests.step);

    // ZRuntime tests (native Zig runtime)
    const zruntime_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/zruntime.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    zruntime_tests.root_module.addImport("zts", zts_mod);
    zruntime_tests.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = b.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zts", .module = zts_mod },
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
    bench_exe.root_module.addImport("zts", zts_mod);
    bench_exe.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = b.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zts", .module = zts_mod },
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
    const release_step = b.step("release", "Build optimized release binary (use -Dhandler=<path> for embedded bytecode)");
    release_step.dependOn(b.getInstallStep());
    if (handler_path) |_| {
        const release_note = b.addSystemCommand(if (aot_enabled) &.{
            "echo",
            "Release build with embedded bytecode + AOT: zig-out/bin/zigttp-server",
        } else &.{
            "echo",
            "Release build with embedded bytecode: zig-out/bin/zigttp-server",
        });
        release_note.step.dependOn(release_step);
    } else {
        const release_note = b.addSystemCommand(&.{
            "echo",
            "Release build: zig-out/bin/zigttp-server (use -Dhandler=<path> for 16% faster cold starts)",
        });
        release_note.step.dependOn(release_step);
    }
    const bench_step = b.step("bench", "Run performance benchmarks");
    bench_step.dependOn(&bench_cmd.step);

    // Mock server (serves responses from generated test JSONL)
    const mock_exe = b.addExecutable(.{
        .name = "mock",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/mock_server.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    mock_exe.root_module.addImport("zts", zts_mod);
    const mock_cmd = b.addRunArtifact(mock_exe);
    if (b.args) |run_args| {
        mock_cmd.addArgs(run_args);
    }
    const mock_step = b.step("mock", "Serve mock responses from generated tests (mock <tests.jsonl> [--port PORT])");
    mock_step.dependOn(&mock_cmd.step);

    // Standalone prove tool (contract diff + classification)
    const prove_exe = b.addExecutable(.{
        .name = "prove",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/prove.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    prove_exe.root_module.addImport("zts", zts_mod);
    const prove_cmd = b.addRunArtifact(prove_exe);
    if (b.args) |run_args| {
        prove_cmd.addArgs(run_args);
    }
    const prove_step = b.step("prove", "Compare two handler contracts (prove <old.json> <new.json> [output-dir/])");
    prove_step.dependOn(&prove_cmd.step);
}
