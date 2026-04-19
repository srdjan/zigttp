const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const zigts_dep = b.dependency("zigts", .{
        .target = target,
        .optimize = optimize,
    });
    const zigts_mod = zigts_dep.module("zigts");
    const zigts_host_dep = b.dependency("zigts", .{
        .target = b.graph.host,
        .optimize = optimize,
    });
    const zigts_host_mod = zigts_host_dep.module("zigts");

    const tools_dep = b.dependency("zigttp_tools", .{
        .target = target,
        .optimize = optimize,
    });
    const zigts_cli_mod = tools_dep.module("zigts_cli");
    const project_config_mod = tools_dep.module("project_config");

    const runtime_dep = b.dependency("zigttp_runtime", .{
        .target = target,
        .optimize = optimize,
    });

    const pi_dep = b.dependency("zigttp_pi", .{
        .target = target,
        .optimize = optimize,
    });
    const pi_app_mod = pi_dep.module("pi_app");

    // Canonical zigts handler examples consumed by expert_persona as
    // few-shot content. Rooted at examples/ so @embedFile reaches the
    // sibling subdirectories without escaping the module-path sandbox.
    const examples_mod = b.addModule("zigts_expert_examples", .{
        .root_source_file = b.path("examples/data.zig"),
        .target = target,
        .optimize = optimize,
    });
    pi_app_mod.addImport("zigts_expert_examples", examples_mod);

    // Sub-dependencies needed for zigts test module construction
    const zigttp_sdk_dep = b.dependency("zigttp_sdk", .{
        .target = target,
        .optimize = optimize,
    });
    const zigttp_modules_dep = b.dependency("zigttp_modules", .{
        .target = target,
        .optimize = optimize,
    });
    const ext_demo_dep = b.dependency("zigttp_ext_demo", .{
        .target = target,
        .optimize = optimize,
    });

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

    // zigts tests
    const zigts_tests_root = b.createModule(.{
        .root_source_file = zigts_dep.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    zigts_tests_root.addImport("zigttp-sdk", zigttp_sdk_dep.module("zigttp-sdk"));
    zigts_tests_root.addImport("zigttp-modules", zigttp_modules_dep.module("zigttp-modules"));
    zigts_tests_root.addImport("zigttp-ext-demo", ext_demo_dep.module("zigttp-ext-demo"));
    zigts_tests_root.addCSourceFile(.{
        .file = zigts_dep.path("deps/sqlite/sqlite3.c"),
        .flags = &.{ "-D_GNU_SOURCE", "-DHAVE_MREMAP=0", "-DSQLITE_THREADSAFE=0", "-DSQLITE_OMIT_LOAD_EXTENSION", "-DSQLITE_DQS=0" },
    });
    zigts_tests_root.addIncludePath(zigts_dep.path("deps/sqlite"));
    const zigts_tests = b.addTest(.{
        .root_module = zigts_tests_root,
    });
    const run_zigts_tests = b.addRunArtifact(zigts_tests);
    const zigts_test_step = b.step("test-zigts", "Run zigts unit tests");
    zigts_test_step.dependOn(&run_zigts_tests.step);

    const precompile_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = tools_dep.path("src/precompile.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    precompile_tests.root_module.addImport("zigts", zigts_host_mod);
    const run_precompile_tests = b.addRunArtifact(precompile_tests);
    const precompile_test_step = b.step("test-precompile", "Run precompile tool tests");
    precompile_test_step.dependOn(&run_precompile_tests.step);

    const prop_expect_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = tools_dep.path("src/property_expectations.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    prop_expect_tests.root_module.addImport("zigts", zigts_host_mod);
    const run_prop_expect_tests = b.addRunArtifact(prop_expect_tests);
    const prop_expect_test_step = b.step("test-property-expectations", "Run property expectations tool tests");
    prop_expect_test_step.dependOn(&run_prop_expect_tests.step);

    const rollout_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = tools_dep.path("src/system_rollout.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    rollout_tests.root_module.addImport("zigts", zigts_host_mod);
    const run_rollout_tests = b.addRunArtifact(rollout_tests);
    const rollout_test_step = b.step("test-rollout", "Run rollout planner tests");
    rollout_test_step.dependOn(&run_rollout_tests.step);

    const expert_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = tools_dep.path("src/expert.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    expert_tests.root_module.addImport("zigts", zigts_host_mod);
    const run_expert_tests = b.addRunArtifact(expert_tests);
    const expert_test_step = b.step("test-expert", "Run zigts expert v1 contract tripwires");
    expert_test_step.dependOn(&run_expert_tests.step);

    // Pi in-process tool registry tests. The pi package owns its own module
    // graph; shared tool cores (expert_meta, verify_paths_core, etc.) are
    // consumed through the `zigts_cli` named module rather than relatively
    // imported, so the file graphs stay disjoint.
    const pi_host_tools_dep = b.dependency("zigttp_tools", .{
        .target = b.graph.host,
        .optimize = optimize,
    });
    const pi_zigts_cli_host_mod = pi_host_tools_dep.module("zigts_cli");
    const pi_zigts_expert_skill_host_mod = pi_host_tools_dep.module("zigts_expert_skill");
    const pi_examples_host_mod = b.createModule(.{
        .root_source_file = b.path("examples/data.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    const pi_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = pi_dep.path("src/tests.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    pi_tests.root_module.addImport("zigts", zigts_host_mod);
    pi_tests.root_module.addImport("zigts_cli", pi_zigts_cli_host_mod);
    pi_tests.root_module.addImport("zigts_expert_skill", pi_zigts_expert_skill_host_mod);
    pi_tests.root_module.addImport("zigts_expert_examples", pi_examples_host_mod);
    const run_pi_tests = b.addRunArtifact(pi_tests);
    const expert_app_test_step = b.step("test-expert-app", "Run zigts expert in-process app tests");
    expert_app_test_step.dependOn(&run_pi_tests.step);

    const capability_audit = b.addSystemCommand(&.{ "/bin/bash", "scripts/check-capability-helpers.sh" });
    const capability_audit_step = b.step("test-capability-audit", "Run capability helper audit");
    capability_audit_step.dependOn(&capability_audit.step);

    // Internal precompile tool used by build steps and the zigts CLI.
    const precompile_exe = b.addExecutable(.{
        .name = "precompile",
        .root_module = b.createModule(.{
            .root_source_file = tools_dep.path("src/precompile.zig"),
            .target = b.graph.host,
            .optimize = .ReleaseFast,
        }),
    });
    precompile_exe.root_module.addImport("zigts", zigts_host_mod);

    // Runtime template binary — used for self-contained outputs and direct
    // runtime tests. Minimal dependencies:
    // only what's needed to serve HTTP and execute a (possibly embedded)
    // handler. No pi_app, no deploy, no zigts_cli.
    const runtime_exe = b.addExecutable(.{
        .name = "zigttp-runtime",
        .root_module = b.createModule(.{
            .root_source_file = runtime_dep.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    runtime_exe.root_module.addImport("zigts", zigts_mod);
    runtime_exe.root_module.addImport("project_config", project_config_mod);
    // Live reload (`serve --watch --prove`) uses precompile + upgrade_verifier
    // from zigts_cli. Needed in the runtime binary only for developer-local
    // `zigttp serve --watch`; production FaaS instances never enable --watch.
    // Cost: binary size. Alternative (move live_reload out of runtime) is a
    // larger refactor — deferred.
    runtime_exe.root_module.addImport("zigts_cli", zigts_cli_mod);

    var embedded_handler_step: ?*std.Build.Step = null;

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
        if (contract_enabled) {
            run_precompile.addArg("--contract");
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
        run_precompile.addArg("packages/runtime/generated/embedded_handler.zig");

        // Create the generated directories if they don't exist
        const mkdir_step = b.addSystemCommand(&.{ "/bin/mkdir", "-p", "packages/runtime/generated" });
        run_precompile.step.dependOn(&mkdir_step.step);
        embedded_handler_step = &run_precompile.step;

        // Runtime and user-facing CLI both depend on precompile completing.
        runtime_exe.step.dependOn(&run_precompile.step);

        // Add the generated module (with zigts dependency for transpiled handlers)
        runtime_exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = b.path("packages/runtime/generated/embedded_handler.zig"),
            .imports = &.{
                .{ .name = "zigts", .module = zigts_mod },
            },
        });
    } else {
        // No handler specified - create a stub module
        runtime_exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = runtime_dep.path("src/embedded_handler_stub.zig"),
            .imports = &.{
                .{ .name = "zigts", .module = zigts_mod },
            },
        });
    }

    b.installArtifact(runtime_exe);

    // Developer CLI — the primary user-facing `zigttp` binary. Contains init,
    // dev, serve, check, compile, prove, mock, link, expert, deploy, login,
    // logout, review, grants, revoke-grant, doctor. Links pi_app and the full
    // deploy subtree.
    const cli_exe = b.addExecutable(.{
        .name = "zigttp",
        .root_module = b.createModule(.{
            .root_source_file = runtime_dep.path("src/cli_main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    cli_exe.root_module.addImport("zigts", zigts_mod);
    cli_exe.root_module.addImport("zigts_cli", zigts_cli_mod);
    cli_exe.root_module.addImport("pi_app", pi_app_mod);
    cli_exe.root_module.addImport("project_config", project_config_mod);
    if (embedded_handler_step) |step| {
        cli_exe.step.dependOn(step);
        cli_exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = b.path("packages/runtime/generated/embedded_handler.zig"),
            .imports = &.{
                .{ .name = "zigts", .module = zigts_mod },
            },
        });
    } else {
        cli_exe.root_module.addAnonymousImport("embedded_handler", .{
            .root_source_file = runtime_dep.path("src/embedded_handler_stub.zig"),
            .imports = &.{
                .{ .name = "zigts", .module = zigts_mod },
            },
        });
    }
    b.installArtifact(cli_exe);

    // Compiler/analyzer + interactive expert CLI
    const zigts_exe = b.addExecutable(.{
        .name = "zigts",
        .root_module = b.createModule(.{
            .root_source_file = b.path("zigts_main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    zigts_exe.root_module.addImport("zigts_cli", zigts_cli_mod);
    zigts_exe.root_module.addImport("pi_app", pi_app_mod);
    b.installArtifact(zigts_exe);

    const run_module_governance = b.addRunArtifact(zigts_exe);
    run_module_governance.addArgs(&.{ "verify-modules", "--builtins", "--strict", "--json" });
    const module_governance_step = b.step("test-module-governance", "Run built-in module governance audit");
    module_governance_step.dependOn(&run_module_governance.step);

    // Golden-output checks that run the built `zigts` binary and assert
    // stdout is byte-identical to a fixture. Covers the direct-command v1 JSON
    // contract for `meta`, `verify-paths`, and `describe-rule`.
    // Regenerate the fixtures with `scripts/update-expert-goldens.sh` (or by
    // rerunning each command and redirecting into
    // packages/tools/tests/fixtures/expert/) after a deliberate contract
    // change; see docs/zigts-expert-contract.md.
    const expert_golden_step = b.step("test-expert-golden", "Check zigts direct tool contract against golden fixtures");
    const fixtures_root = "packages/tools/tests/fixtures/expert";
    addExpertGolden(b, expert_golden_step, zigts_exe, &.{ "meta", "--json" }, fixtures_root ++ "/meta.golden.json", 0);
    addExpertGolden(b, expert_golden_step, zigts_exe, &.{
        "verify-paths",
        fixtures_root ++ "/clean_handler.ts",
        "--json",
    }, fixtures_root ++ "/verify_paths_clean.golden.json", 0);
    addExpertGolden(b, expert_golden_step, zigts_exe, &.{
        "verify-paths",
        fixtures_root ++ "/missing.ts",
        "--json",
    }, fixtures_root ++ "/verify_paths_missing.golden.json", 1);
    addExpertGolden(b, expert_golden_step, zigts_exe, &.{ "describe-rule", "ZTS303", "--json" }, fixtures_root ++ "/describe_rule_ZTS303.golden.json", 0);
    addExpertGolden(b, expert_golden_step, zigts_exe, &.{
        "verify-paths",
        fixtures_root ++ "/clean_handler.ts",
    }, fixtures_root ++ "/verify_paths_clean_text.golden.txt", 0);
    addExpertGolden(b, expert_golden_step, zigts_exe, &.{
        "verify-paths",
        fixtures_root ++ "/missing.ts",
    }, fixtures_root ++ "/verify_paths_missing_text.golden.txt", 1);

    // Exit-code contract for help/error paths. Stdout isn't pinned because
    // help text edits should not break tests; only the exit code is part of
    // the contract.
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{ "expert", "--help" }, 0);
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{ "meta", "--help" }, 0);
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{ "verify-paths", "--help" }, 0);
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{ "verify-paths", fixtures_root ++ "/clean_handler.ts", "--help" }, 0);
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{ "expert", "no-such-sub" }, 1);
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{"verify-paths"}, 1);

    // Run command: runs the runtime binary directly, without triggering the
    // full install step (which would also link the dev CLI and bench binaries).
    const run_cmd = b.addRunArtifact(runtime_exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the server");
    run_step.dependOn(&run_cmd.step);

    // Dev CLI run command for convenience: `zig build cli -- expert`
    const cli_run_cmd = b.addRunArtifact(cli_exe);
    if (b.args) |args| {
        cli_run_cmd.addArgs(args);
    }
    const cli_run_step = b.step("cli", "Run the zigttp CLI");
    cli_run_step.dependOn(&cli_run_cmd.step);

    // Tests
    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = runtime_dep.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Runtime-side tests (main.zig root) — covers runtime_cli, zruntime,
    // server, proof_adapter, cli_shared via the test block in main.zig.
    unit_tests.root_module.addImport("zigts", zigts_mod);
    unit_tests.root_module.addImport("project_config", project_config_mod);
    unit_tests.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = runtime_dep.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
        },
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);

    // Dev-CLI-side tests (cli_main.zig root) — covers dev_cli and its
    // dependencies (deploy, pi_app wiring, zigts_cli delegation).
    const cli_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = runtime_dep.path("src/cli_main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    cli_tests.root_module.addImport("zigts", zigts_mod);
    cli_tests.root_module.addImport("zigts_cli", zigts_cli_mod);
    cli_tests.root_module.addImport("pi_app", pi_app_mod);
    cli_tests.root_module.addImport("project_config", project_config_mod);
    cli_tests.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = runtime_dep.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
        },
    });
    const run_cli_tests = b.addRunArtifact(cli_tests);
    const cli_test_step = b.step("test-cli", "Run developer CLI unit tests");
    cli_test_step.dependOn(&run_cli_tests.step);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
    test_step.dependOn(&run_cli_tests.step);
    test_step.dependOn(&run_precompile_tests.step);
    test_step.dependOn(&run_prop_expect_tests.step);
    test_step.dependOn(&run_rollout_tests.step);
    test_step.dependOn(&run_expert_tests.step);
    test_step.dependOn(&run_pi_tests.step);
    test_step.dependOn(&capability_audit.step);
    test_step.dependOn(&run_module_governance.step);
    test_step.dependOn(&run_zigts_tests.step);
    test_step.dependOn(expert_golden_step);

    // ZRuntime tests (native Zig runtime)
    const zruntime_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = runtime_dep.path("src/zruntime.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    zruntime_tests.root_module.addImport("zigts", zigts_mod);
    zruntime_tests.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = runtime_dep.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
        },
    });
    const run_zruntime_tests = b.addRunArtifact(zruntime_tests);
    const zruntime_test_step = b.step("test-zruntime", "Run ZRuntime unit tests");
    zruntime_test_step.dependOn(&run_zruntime_tests.step);
    test_step.dependOn(&run_zruntime_tests.step);

    // Benchmark executable
    const bench_exe = b.addExecutable(.{
        .name = "zigttp-bench",
        .root_module = b.createModule(.{
            .root_source_file = runtime_dep.path("src/benchmark.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    bench_exe.root_module.addImport("zigts", zigts_mod);
    bench_exe.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = runtime_dep.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
        },
    });
    // Bench is not installed by default. `zig build bench` still builds and
    // runs it; the artifact is available via the cache or an explicit install.

    // Benchmark run command
    const bench_cmd = b.addRunArtifact(bench_exe);
    if (b.args) |args| {
        bench_cmd.addArgs(args);
    }

    // Release build step (with handler precompilation if provided)
    const release_step = b.step("release", "Build optimized release binaries (zigttp, zigttp-runtime, zigts)");
    release_step.dependOn(b.getInstallStep());
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

fn addExpertGolden(
    b: *std.Build,
    step: *std.Build.Step,
    zigts_exe: *std.Build.Step.Compile,
    args: []const []const u8,
    golden_rel: []const u8,
    expected_exit: u8,
) void {
    addExpertRun(b, step, zigts_exe, args, expected_exit, golden_rel);
}

/// When `golden_rel` is null, only the exit code is asserted — help/error
/// text is not part of the contract, so editorial changes don't break the
/// build.
fn addExpertExitCheck(
    b: *std.Build,
    step: *std.Build.Step,
    zigts_exe: *std.Build.Step.Compile,
    args: []const []const u8,
    expected_exit: u8,
) void {
    addExpertRun(b, step, zigts_exe, args, expected_exit, null);
}

fn addExpertRun(
    b: *std.Build,
    step: *std.Build.Step,
    zigts_exe: *std.Build.Step.Compile,
    args: []const []const u8,
    expected_exit: u8,
    golden_rel: ?[]const u8,
) void {
    const run = b.addRunArtifact(zigts_exe);
    run.addArgs(args);
    run.expectExitCode(expected_exit);
    if (golden_rel) |rel| {
        const expected = b.build_root.handle.readFileAlloc(b.graph.io, rel, b.allocator, .unlimited) catch |err| {
            std.debug.panic("missing expert golden fixture {s}: {s}", .{ rel, @errorName(err) });
        };
        run.expectStdOutEqual(expected);
    }
    step.dependOn(&run.step);
}
