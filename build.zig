const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const bench_optimize: std.builtin.OptimizeMode = .ReleaseFast;
    const perf_histogram_enabled = b.option(bool, "perf_histogram", "Enable interpreter opcode histogram collection") orelse false;
    const studio_enabled = b.option(bool, "studio", "Compile the browser proof workbench (zigttp studio) into the dev CLI") orelse false;
    const edge_enabled = b.option(bool, "edge", "Compile the in-process edge runtime (zigttp edge) into the binaries") orelse false;
    const strip_enabled = b.option(bool, "strip", "Strip debug info from the installed zigttp/zigts/zigttp-runtime binaries (release artifacts)") orelse false;
    const zigts_dep = b.dependency("zigts", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram_enabled,
    });
    const zigts_mod = zigts_dep.module("zigts");
    const zigts_host_dep = b.dependency("zigts", .{
        .target = b.graph.host,
        .optimize = optimize,
        .perf_histogram = perf_histogram_enabled,
    });
    const zigts_host_mod = zigts_host_dep.module("zigts");

    const tools_dep = b.dependency("zigttp_tools", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram_enabled,
    });
    const zigts_cli_mod = tools_dep.module("zigts_cli");
    const project_config_mod = tools_dep.module("project_config");

    const runtime_dep = b.dependency("zigttp_runtime", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram_enabled,
        .studio = studio_enabled,
        .edge = edge_enabled,
    });
    const runtime_bench_dep = b.dependency("zigttp_runtime", .{
        .target = target,
        .optimize = bench_optimize,
        .perf_histogram = perf_histogram_enabled,
    });
    // The benchmark exe is built ReleaseFast. Its embedded_handler import must
    // resolve `zigts` to the matching ReleaseFast module - wiring the Debug
    // `zigts` here collides the module graph (file exists in modules zigts and
    // zigts0) and breaks `zig build bench` on a Debug-default toolchain.
    const zigts_bench_dep = b.dependency("zigts", .{
        .target = target,
        .optimize = bench_optimize,
        .perf_histogram = perf_histogram_enabled,
    });
    const zigts_bench_mod = zigts_bench_dep.module("zigts");

    // Pi dependency: used for the in-process expert tool tests below. The
    // `pi_app` module itself is linked into the developer `zigttp` binary via
    // packages/runtime/build.zig (cli_main), not here — the standalone `zigts`
    // analyzer binary is intentionally pi-free.
    const pi_dep = b.dependency("zigttp_pi", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram_enabled,
    });

    // Sub-dependencies needed for zigts test module construction
    const zigttp_sdk_dep = b.dependency("zigttp_sdk", .{
        .target = target,
        .optimize = optimize,
    });
    const zigttp_modules_dep = b.dependency("zigttp_modules", .{
        .target = target,
        .optimize = optimize,
    });
    // Capture git commit for reproducible build metadata. Failure to read
    // git is non-fatal: the precompile binary falls back to the sentinel
    // "unknown", so tarball builds and CI environments without a .git
    // directory still produce a well-formed `__GIT_COMMIT__` substitution.
    const git_commit_sha = detectGitCommit(b);

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
    const zigts_build_options = b.addOptions();
    zigts_build_options.addOption(bool, "perf_histogram", perf_histogram_enabled);
    zigts_build_options.addOption(bool, "analyzer_only", false);
    zigts_tests_root.addOptions("build_options", zigts_build_options);
    zigts_tests_root.addImport("zigttp-sdk", zigttp_sdk_dep.module("zigttp-sdk"));
    zigts_tests_root.addImport("zigttp-modules", zigttp_modules_dep.module("zigttp-modules"));
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

    const sdk_test_shim_mod = b.createModule(.{
        .root_source_file = zigttp_sdk_dep.path("src/test_shim.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "zigttp-sdk", .module = zigttp_sdk_dep.module("zigttp-sdk") },
        },
    });
    const sdk_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = zigttp_sdk_dep.path("src/test_root.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zigttp-sdk", .module = zigttp_sdk_dep.module("zigttp-sdk") },
                .{ .name = "zigttp-sdk-test-shim", .module = sdk_test_shim_mod },
            },
        }),
    });
    const run_sdk_tests = b.addRunArtifact(sdk_tests);
    const sdk_test_step = b.step("test-sdk", "Run zigttp-sdk tests");
    sdk_test_step.dependOn(&run_sdk_tests.step);

    const modules_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = zigttp_modules_dep.path("src/test_root.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{ .name = "zigttp-sdk", .module = zigttp_sdk_dep.module("zigttp-sdk") },
                .{ .name = "zigttp-sdk-test-shim", .module = sdk_test_shim_mod },
            },
        }),
    });
    const run_modules_tests = b.addRunArtifact(modules_tests);
    const modules_test_step = b.step("test-modules", "Run zigttp-modules tests");
    modules_test_step.dependOn(&run_modules_tests.step);

    // zigttp proof-review package tests
    // Pass perf_histogram so the build-graph dedups this dep with the one
    // runtime threads through its own modules. Without it the option-set
    // hashes diverge and Zig instantiates zigttp_proof_review twice, splitting
    // type identity across the proof-review/runtime boundary.
    const proof_review_pkg_dep = b.dependency("zigttp_proof_review", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram_enabled,
    });
    const proof_review_pkg_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = proof_review_pkg_dep.path("src/test_root.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{ .name = "zigts", .module = zigts_dep.module("zigts") },
                .{ .name = "zigts_cli", .module = zigts_cli_mod },
            },
        }),
    });
    const run_proof_review_pkg_tests = b.addRunArtifact(proof_review_pkg_tests);
    const proof_review_pkg_test_step = b.step("test-proof-review", "Run zigttp proof-review package tests");
    proof_review_pkg_test_step.dependOn(&run_proof_review_pkg_tests.step);

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

    // Canonicalize/normalize tool tests. The tools `canonicalize.zig` is only
    // reached through the `zigts_cli` named module, so its `test {}` blocks are
    // not collected by any other test root; this step roots at it directly.
    const canonicalize_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = tools_dep.path("src/canonicalize.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    canonicalize_tests.root_module.addImport("zigts", zigts_host_mod);
    // edit_simulate.zig (reached from canonicalize.zig's test graph) resolves
    // the project SQL schema through the shared project_config module.
    canonicalize_tests.root_module.addImport("project_config", project_config_mod);
    const run_canonicalize_tests = b.addRunArtifact(canonicalize_tests);
    const canonicalize_test_step = b.step("test-canonicalize", "Run canonicalize/normalize tool tests");
    canonicalize_test_step.dependOn(&run_canonicalize_tests.step);

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

    // Analyzer dispatch + machine-command modules. zigts_cli.zig and the
    // command files it imports (describe_rule.zig, search_rules.zig, ...) are
    // only reached through the `zigts_cli` named module, which is never an
    // addTest root, so their `test {}` blocks were collected by no suite. This
    // step roots at the dispatcher directly. Same rationale as canonicalize_tests.
    const zigts_cli_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = tools_dep.path("src/zigts_cli.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    zigts_cli_tests.root_module.addImport("zigts", zigts_host_mod);
    zigts_cli_tests.root_module.addImport("project_config", project_config_mod);
    const run_zigts_cli_tests = b.addRunArtifact(zigts_cli_tests);
    const zigts_cli_test_step = b.step("test-zigts-cli", "Run analyzer dispatch + machine-command module tests");
    zigts_cli_test_step.dependOn(&run_zigts_cli_tests.step);

    const deploy_manifest_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = tools_dep.path("src/deploy_manifest.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    deploy_manifest_tests.root_module.addImport("zigts", zigts_host_mod);
    const run_deploy_manifest_tests = b.addRunArtifact(deploy_manifest_tests);
    const deploy_manifest_test_step = b.step("test-deploy-manifest", "Run deploy manifest renderer tests");
    deploy_manifest_test_step.dependOn(&run_deploy_manifest_tests.step);

    // Pi in-process tool registry tests. The pi package owns its own module
    // graph; shared tool cores (expert_meta, verify_paths_core, etc.) are
    // consumed through the `zigts_cli` named module rather than relatively
    // imported, so the file graphs stay disjoint.
    const pi_host_tools_dep = b.dependency("zigttp_tools", .{
        .target = b.graph.host,
        .optimize = optimize,
        .perf_histogram = perf_histogram_enabled,
    });
    const pi_zigts_cli_host_mod = pi_host_tools_dep.module("zigts_cli");
    const pi_zigts_expert_skill_host_mod = pi_host_tools_dep.module("zigts_expert_skill");
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
    pi_tests.root_module.addImport("project_config", project_config_mod);
    const run_pi_tests = b.addRunArtifact(pi_tests);
    const expert_app_test_step = b.step("test-expert-app", "Run zigts expert in-process app tests");
    expert_app_test_step.dependOn(&run_pi_tests.step);

    // Cassette harness tests. Focused subset that only covers the
    // record/replay layer; runs offline, never needs an API key, and
    // does not transitively pull in tools/skills tests so it stays fast.
    const cassette_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = pi_dep.path("src/cassette_tests.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    cassette_tests.root_module.addImport("zigts", zigts_host_mod);
    cassette_tests.root_module.addImport("zigts_cli", pi_zigts_cli_host_mod);
    cassette_tests.root_module.addImport("zigts_expert_skill", pi_zigts_expert_skill_host_mod);
    cassette_tests.root_module.addImport("project_config", project_config_mod);
    const run_cassette_tests = b.addRunArtifact(cassette_tests);
    const cassette_test_step = b.step("test-cassette", "Run pi provider cassette harness tests (offline)");
    cassette_test_step.dependOn(&run_cassette_tests.step);

    const capability_audit = b.addSystemCommand(&.{ "/bin/bash", "scripts/check-capability-helpers.sh" });
    const capability_audit_step = b.step("test-capability-audit", "Run capability helper audit");
    capability_audit_step.dependOn(&capability_audit.step);

    const docs_drift = b.addSystemCommand(&.{ "/bin/bash", "scripts/check-docs-drift.sh" });
    const docs_drift_step = b.step("test-docs-drift", "Check docs against current registry and build paths");
    docs_drift_step.dependOn(&docs_drift.step);

    const doc_links = b.addSystemCommand(&.{ "/bin/bash", "scripts/audit-docs.sh" });
    const doc_links_step = b.step("test-doc-links", "Check docs for broken relative links");
    doc_links_step.dependOn(&doc_links.step);

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
        .root_module = runtime_dep.module("runtime_main"),
    });

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
        if (git_commit_sha) |sha| {
            run_precompile.addArg("--git-commit");
            run_precompile.addArg(sha);
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
    // dev, serve, check, compile, prove, mock, link, expert, local deploy,
    // doctor, and the proof/proof-ledger tools. Hosted deploy account verbs
    // are intentionally absent from CLI dispatch in the beta.
    const cli_exe = b.addExecutable(.{
        .name = "zigttp",
        .root_module = runtime_dep.module("cli_main"),
    });
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

    // Compiler/analyzer CLI installed for IDE and CI integrations that call
    // the analyzer directly. Pi-free by design: the interactive `expert` and
    // session `ledger` commands live only in the developer `zigttp` binary, so
    // the ~37 KLOC agent (and its network/credential surface) is compiled
    // exactly once across the whole build.
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
    b.installArtifact(zigts_exe);

    // Strip debug info from the three installed binaries when -Dstrip is set.
    // The release workflow passes -Dstrip so shipped tarballs stay small; local
    // builds keep symbols by default. Stripping happens at link time, so it is
    // correct for every cross-compiled -Dtarget.
    if (strip_enabled) {
        runtime_exe.root_module.strip = true;
        cli_exe.root_module.strip = true;
        zigts_exe.root_module.strip = true;
    }

    // Runtime purity guard: the deployable `zigttp-runtime` template and the
    // pi-free `zigts` analyzer must carry no expert-agent / model-provider
    // surface; the developer `zigttp` binary is the sole pi host. Enforces the
    // invariant against future regressions. See scripts/check-runtime-purity.sh.
    const runtime_purity_cmd = b.addSystemCommand(&.{ "/bin/bash", "scripts/check-runtime-purity.sh" });
    runtime_purity_cmd.addFileArg(cli_exe.getEmittedBin());
    runtime_purity_cmd.addFileArg(runtime_exe.getEmittedBin());
    runtime_purity_cmd.addFileArg(zigts_exe.getEmittedBin());
    const runtime_purity_step = b.step("test-runtime-purity", "Assert the deployed runtime and analyzer carry no agent/provider surface");
    runtime_purity_step.dependOn(&runtime_purity_cmd.step);

    // WebAssembly analyzer — the zigts static analysis pipeline compiled to
    // wasm64-freestanding for the in-browser proof playground. It runs the
    // same `runCheckOnlyFromSource` path as `zigts check --json`, so the
    // playground renders the real compiler's verdict, not an approximation.
    // wasm64 (not wasm32) because the value layer's NaN-boxing assumes 64-bit
    // pointers. `analyzer_only` strips the interpreter, JIT, GC, SQLite, libc.
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm64,
        .os_tag = .freestanding,
    });
    const wasm_zigts_dep = b.dependency("zigts", .{
        .target = wasm_target,
        .optimize = .ReleaseSmall,
        .analyzer_only = true,
    });
    const wasm_exe = b.addExecutable(.{
        .name = "zigts-analyzer",
        .root_module = b.createModule(.{
            .root_source_file = tools_dep.path("src/wasm_analyzer.zig"),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
        }),
    });
    wasm_exe.root_module.addImport("zigts", wasm_zigts_dep.module("zigts"));
    // Reactor-style module: no _start, exported functions only.
    wasm_exe.entry = .disabled;
    wasm_exe.rdynamic = true;
    const wasm_install = b.addInstallArtifact(wasm_exe, .{
        .dest_dir = .{ .override = .{ .custom = "wasm" } },
    });
    const wasm_step = b.step("wasm", "Build the zigts analyzer as a wasm64-freestanding module for the web playground");
    wasm_step.dependOn(&wasm_install.step);

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
        "canonicalize",
        fixtures_root ++ "/canonicalize_mixed.ts",
        "--json",
    }, fixtures_root ++ "/canonicalize_mixed.golden.json", 0);
    addExpertGolden(b, expert_golden_step, zigts_exe, &.{
        "canonicalize",
        fixtures_root ++ "/canonicalize_mixed.ts",
        "--json",
        "--simulate",
    }, fixtures_root ++ "/canonicalize_mixed_simulate.golden.json", 0);
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
    // the contract. The `expert` command now lives only in the developer
    // `zigttp` binary (cli_exe); analyzer commands stay on `zigts` (zigts_exe).
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "expert", "--help" }, 0);
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{ "meta", "--help" }, 0);
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{ "verify-paths", "--help" }, 0);
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{ "verify-paths", fixtures_root ++ "/clean_handler.ts", "--help" }, 0);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "expert", "no-such-sub" }, 1);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "ledger", "--help" }, 0);
    addExpertExitCheck(b, expert_golden_step, zigts_exe, &.{"verify-paths"}, 1);

    // Machine-command unknown-flag contract (plan 009): a typo'd flag is a loud
    // non-zero exit via the clean dev-CLI mapping, not a silently-ignored arg
    // that yields wrong output for tool/CI callers; valid invocations stay
    // exit 0. These run the developer `zigttp` binary (cli_exe), which owns the
    // invalid-arguments message; the analyzer `zigts` binary shares the same
    // dispatch. Stdout is intentionally not pinned.
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "features", "--josn" }, 1);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "features", "--json" }, 0);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "modules", "--josn" }, 1);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "modules", "--json" }, 0);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "meta", "--josn" }, 1);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "meta", "--json" }, 0);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "describe-rule", "--josn" }, 1);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "describe-rule", "ZTS303" }, 0);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "search", "--josn" }, 1);
    addExpertExitCheck(b, expert_golden_step, cli_exe, &.{ "search", "guard" }, 0);

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
        .root_module = runtime_dep.module("runtime_main_tests"),
    });

    // Runtime-side tests (main.zig root) — covers runtime_cli, zruntime,
    // server, proof_adapter, cli_shared via the test block in main.zig.
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
        .root_module = runtime_dep.module("cli_main_tests"),
        .test_runner = .{
            .path = runtime_dep.path("src/cli_test_runner.zig"),
            .mode = .simple,
        },
    });
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
    test_step.dependOn(&run_canonicalize_tests.step);
    test_step.dependOn(&run_prop_expect_tests.step);
    test_step.dependOn(&run_rollout_tests.step);
    test_step.dependOn(&run_expert_tests.step);
    test_step.dependOn(&run_zigts_cli_tests.step);
    test_step.dependOn(&run_pi_tests.step);
    test_step.dependOn(&run_deploy_manifest_tests.step);
    test_step.dependOn(&capability_audit.step);
    test_step.dependOn(&docs_drift.step);
    test_step.dependOn(&run_module_governance.step);
    test_step.dependOn(&run_zigts_tests.step);
    test_step.dependOn(&run_sdk_tests.step);
    test_step.dependOn(&run_modules_tests.step);
    test_step.dependOn(&run_proof_review_pkg_tests.step);
    test_step.dependOn(expert_golden_step);
    test_step.dependOn(&runtime_purity_cmd.step);

    // ZRuntime tests (native Zig runtime)
    const zruntime_tests = b.addTest(.{
        .root_module = runtime_dep.module("zruntime"),
    });
    zruntime_tests.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = runtime_dep.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
        },
    });
    const run_zruntime_tests = b.addRunArtifact(zruntime_tests);
    const zruntime_test_step = b.step("test-zruntime", "Run ZRuntime unit tests");
    zruntime_test_step.dependOn(&run_zruntime_tests.step);
    // main.zig already imports zruntime.zig in the aggregate runtime test root.
    // Keep test-zruntime as a focused standalone target, but do not run the same
    // pool-heavy tests twice inside zig build test; parallel duplicate roots
    // have produced intermittent libc/JIT/arena teardown TRAPs on macOS.

    // test-server: server/runtime facade integration suite (Phase 0b gate).
    // Tests through public entry points (Server.init/deinit, HandlerPool
    // execute*, RuntimeConfig) — never interpreter/JIT internals.
    const server_tests = b.addTest(.{
        .root_module = runtime_dep.module("server_tests"),
    });
    server_tests.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = runtime_dep.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_mod },
        },
    });
    const run_server_tests = b.addRunArtifact(server_tests);
    const server_test_step = b.step("test-server", "Run server/runtime facade integration tests");
    server_test_step.dependOn(&run_server_tests.step);
    test_step.dependOn(&run_server_tests.step);

    // Benchmark executable
    const bench_exe = b.addExecutable(.{
        .name = "zigttp-bench",
        .root_module = runtime_bench_dep.module("benchmark"),
    });
    bench_exe.root_module.addAnonymousImport("embedded_handler", .{
        .root_source_file = runtime_bench_dep.path("src/embedded_handler_stub.zig"),
        .imports = &.{
            .{ .name = "zigts", .module = zigts_bench_mod },
        },
    });
    // Bench is not installed by default. `zig build bench` still builds and
    // runs it; the artifact is available via the cache or an explicit install.

    // Benchmark run command
    const bench_cmd = b.addRunArtifact(bench_exe);
    if (b.args) |args| {
        bench_cmd.addArgs(args);
    }

    // Run the benchmark binary multiple times through bench-diff.sh directly
    // (best-of-N handling lives in the script to tame microbench variance).
    const bench_check_cmd = b.addSystemCommand(&.{ "/bin/bash", "scripts/bench-diff.sh" });
    bench_check_cmd.addArg("--baseline");
    bench_check_cmd.addFileArg(b.path("benchmarks/perf-baseline.json"));
    bench_check_cmd.addArg("--bench");
    bench_check_cmd.addFileArg(bench_exe.getEmittedBin());
    bench_check_cmd.has_side_effects = true;

    // Release build step (with handler precompilation if provided)
    const release_step = b.step("release", "Build optimized release binaries (zigttp, zigttp-runtime, zigts)");
    release_step.dependOn(b.getInstallStep());
    const bench_step = b.step("bench", "Run performance benchmarks");
    bench_step.dependOn(&bench_cmd.step);
    const bench_check_step = b.step("bench-check", "Compare benchmark output against the checked-in perf baseline");
    bench_check_step.dependOn(&bench_check_cmd.step);

    // End-to-end smoke for the v1 user flow:
    // init -> doctor -> check -> build -> deploy.
    // The script builds the CLI itself; the step does not reference cli_exe so
    // CI can invoke it as a single command without depending on install steps.
    // studio is compiled out by default, so it is smoke-tested separately by
    // `zig build smoke-studio` (which builds with -Dstudio).
    const smoke_v1_cmd = b.addSystemCommand(&.{ "/bin/bash", "scripts/smoke-v1.sh" });
    smoke_v1_cmd.has_side_effects = true;
    const smoke_v1_step = b.step("smoke-v1", "Run the v1 user-flow smoke test in a temp dir");
    smoke_v1_step.dependOn(&smoke_v1_cmd.step);

    const panic_isolation_cmd = b.addSystemCommand(&.{ "/bin/bash", "scripts/test-panic-isolation.sh", "--skip-build", "--zigttp" });
    panic_isolation_cmd.addFileArg(cli_exe.getEmittedBin());
    panic_isolation_cmd.has_side_effects = true;
    const panic_isolation_step = b.step("test-panic-isolation", "Run handler panic isolation E2E test");
    panic_isolation_step.dependOn(&panic_isolation_cmd.step);

    const smoke_studio_cmd = b.addSystemCommand(&.{ "/bin/bash", "scripts/smoke-studio.sh" });
    smoke_studio_cmd.has_side_effects = true;
    const smoke_studio_step = b.step("smoke-studio", "Run the opt-in studio smoke test (-Dstudio) in a temp dir");
    smoke_studio_step.dependOn(&smoke_studio_cmd.step);

    const smoke_getting_started_cmd = b.addSystemCommand(&.{ "/bin/bash", "scripts/smoke-getting-started.sh" });
    smoke_getting_started_cmd.has_side_effects = true;
    const smoke_getting_started_step = b.step("smoke-getting-started", "Run the Getting Started guide smoke test in a temp dir");
    smoke_getting_started_step.dependOn(&smoke_getting_started_cmd.step);

    const smoke_demo_cmd = b.addSystemCommand(&.{ "/bin/bash", "scripts/smoke-demo.sh" });
    smoke_demo_cmd.has_side_effects = true;
    const smoke_demo_step = b.step("smoke-demo", "Run the Proof Theater demo smoke test in a temp dir");
    smoke_demo_step.dependOn(&smoke_demo_cmd.step);

    // Compile-time microbench: parse + codegen ns/bytes/IR-nodes per compile
    // across a small synthesized corpus. Scaffolding for Phase 8 tuning of
    // reserveCapacity and intern_pool capacity hints.
    const compile_bench_exe = b.addExecutable(.{
        .name = "zigttp-compile-bench",
        .root_module = runtime_dep.module("compile_benchmark"),
    });

    const compile_bench_cmd = b.addRunArtifact(compile_bench_exe);
    // Bench runs are measurements, not cacheable build products.
    compile_bench_cmd.has_side_effects = true;
    if (b.args) |args| {
        compile_bench_cmd.addArgs(args);
    }
    const compile_bench_step = b.step("compile-bench", "Run compile-time microbenchmarks");
    compile_bench_step.dependOn(&compile_bench_cmd.step);

    const compile_bench_tests = b.addTest(.{
        .root_module = runtime_dep.module("compile_benchmark"),
    });
    const run_compile_bench_tests = b.addRunArtifact(compile_bench_tests);
    const compile_bench_test_step = b.step("test-compile-bench", "Run compile-time microbench harness tests");
    compile_bench_test_step.dependOn(&run_compile_bench_tests.step);
    test_step.dependOn(&run_compile_bench_tests.step);

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

/// Run `git rev-parse --short=12 HEAD` once at configure time so the
/// precompile binary can stamp embedded handlers with a real commit hash.
/// Returns null on any failure (missing git, detached worktree, snapshot
/// tarball with no .git directory). The caller treats null as "fall back to
/// the precompile sentinel" rather than failing the build.
fn detectGitCommit(b: *std.Build) ?[]const u8 {
    const result = std.process.run(b.allocator, b.graph.io, .{
        .argv = &.{ "git", "rev-parse", "--short=12", "HEAD" },
        .cwd = if (b.build_root.path) |p| .{ .path = p } else .inherit,
    }) catch return null;
    defer b.allocator.free(result.stdout);
    defer b.allocator.free(result.stderr);

    switch (result.term) {
        .exited => |code| if (code != 0) return null,
        else => return null,
    }
    const trimmed = std.mem.trim(u8, result.stdout, " \t\r\n");
    if (trimmed.len == 0) return null;
    return b.allocator.dupe(u8, trimmed) catch null;
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
    // Zig 0.16's run-step still fails non-zero commands that write to stderr
    // unless a stderr check exists. Matching the empty string keeps the
    // contract at "only the exit code matters" for exit-only checks.
    if (golden_rel == null and expected_exit != 0) {
        run.expectStdErrMatch("");
    }
    if (golden_rel) |rel| {
        const expected = b.build_root.handle.readFileAlloc(b.graph.io, rel, b.allocator, .unlimited) catch |err| {
            std.debug.panic("missing expert golden fixture {s}: {s}", .{ rel, @errorName(err) });
        };
        run.expectStdOutEqual(expected);
    }
    step.dependOn(&run.step);
}
