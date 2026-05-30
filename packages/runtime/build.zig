const std = @import("std");

const RuntimeFeatureConfig = struct {
    enable_live_reload: bool,
    enable_studio: bool,
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const perf_histogram = b.option(bool, "perf_histogram", "Enable interpreter opcode histogram collection") orelse false;

    const zigts_dep = b.dependency("zigts", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram,
    });
    const zigts_mod = zigts_dep.module("zigts");

    const tools_dep = b.dependency("zigttp_tools", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram,
    });
    const zigts_cli_mod = tools_dep.module("zigts_cli");
    const project_config_mod = tools_dep.module("project_config");

    const pi_dep = b.dependency("zigttp_pi", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram,
    });
    const pi_app_mod = pi_dep.module("pi_app");

    const deploy_dep = b.dependency("zigttp_deploy", .{
        .target = target,
        .optimize = optimize,
        .perf_histogram = perf_histogram,
    });
    const deploy_mod = deploy_dep.module("zigttp_deploy");

    const runtime_features = runtimeFeatureOptions(b, .{
        .enable_live_reload = false,
        .enable_studio = false,
    });
    const cli_features = runtimeFeatureOptions(b, .{
        .enable_live_reload = true,
        .enable_studio = true,
    });

    const runtime_main = b.addModule("runtime_main", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    runtime_main.addImport("zigts", zigts_mod);
    runtime_main.addImport("project_config", project_config_mod);
    runtime_main.addOptions("runtime_feature_options", runtime_features);

    const runtime_main_tests = b.addModule("runtime_main_tests", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    runtime_main_tests.addImport("zigts", zigts_mod);
    runtime_main_tests.addImport("project_config", project_config_mod);
    runtime_main_tests.addOptions("runtime_feature_options", runtime_features);

    const cli_main = b.addModule("cli_main", .{
        .root_source_file = b.path("src/cli_main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    cli_main.addImport("zigts", zigts_mod);
    cli_main.addImport("zigts_cli", zigts_cli_mod);
    cli_main.addImport("pi_app", pi_app_mod);
    cli_main.addImport("project_config", project_config_mod);
    cli_main.addImport("zigttp_deploy", deploy_mod);
    cli_main.addOptions("runtime_feature_options", cli_features);

    const cli_main_tests = b.addModule("cli_main_tests", .{
        .root_source_file = b.path("src/cli_main.zig"),
        .target = target,
        .optimize = optimize,
    });
    cli_main_tests.addImport("zigts", zigts_mod);
    cli_main_tests.addImport("zigts_cli", zigts_cli_mod);
    cli_main_tests.addImport("pi_app", pi_app_mod);
    cli_main_tests.addImport("project_config", project_config_mod);
    cli_main_tests.addImport("zigttp_deploy", deploy_mod);
    cli_main_tests.addOptions("runtime_feature_options", cli_features);

    const zruntime = b.addModule("zruntime", .{
        .root_source_file = b.path("src/zruntime.zig"),
        .target = target,
        .optimize = optimize,
    });
    zruntime.addImport("zigts", zigts_mod);
    zruntime.addOptions("runtime_feature_options", runtime_features);

    const benchmark = b.addModule("benchmark", .{
        .root_source_file = b.path("src/benchmark.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    benchmark.addImport("zigts", zigts_mod);
    benchmark.addOptions("runtime_feature_options", runtime_features);

    const compile_benchmark = b.addModule("compile_benchmark", .{
        .root_source_file = b.path("src/compile_benchmark.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    compile_benchmark.addImport("zigts", zigts_mod);

    const witness_replay = b.addModule("witness_replay", .{
        .root_source_file = b.path("src/witness_replay_lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    witness_replay.addImport("zigts", zigts_mod);
    witness_replay.addImport("pi_app", pi_app_mod);
    witness_replay.addImport("zigttp_deploy", deploy_mod);
}

fn runtimeFeatureOptions(b: *std.Build, config: RuntimeFeatureConfig) *std.Build.Step.Options {
    const options = b.addOptions();
    options.addOption(bool, "enable_live_reload", config.enable_live_reload);
    options.addOption(bool, "enable_studio", config.enable_studio);
    return options;
}
