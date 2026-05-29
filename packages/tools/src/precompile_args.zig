//! Precompile CLI argument parsing.

const std = @import("std");
const builtin = @import("builtin");
const zigts = @import("zigts");

const readFilePosix = zigts.file_io.readFile;

/// stderr printer that no-ops under `zig build test` so unit tests for
/// arg-parse error paths do not pollute the test-runner IPC stream.
fn errPrint(comptime fmt: []const u8, args: anytype) void {
    if (builtin.is_test) return;
    std.debug.print(fmt, args);
}

fn takeArg(index: *usize, argv: []const []const u8, comptime message: []const u8) ![]const u8 {
    index.* += 1;
    if (index.* >= argv.len) {
        errPrint(message ++ "\n", .{});
        return error.MissingArgument;
    }
    return argv[index.*];
}

pub const PrecompileOptions = struct {
    handler_path: []const u8,
    output_path: []const u8,
    emit_aot: bool = false,
    emit_verify: bool = false,
    emit_contract: bool = false,
    emit_openapi: bool = false,
    sdk_target: ?[]const u8 = null,
    sql_schema_path: ?[]const u8 = null,
    system_path: ?[]const u8 = null,
    policy_path: ?[]const u8 = null,
    replay_trace_path: ?[]const u8 = null,
    test_file_path: ?[]const u8 = null,
    prove_spec: ?[]const u8 = null,
    generate_tests: bool = false,
    manifest_path: ?[]const u8 = null,
    expect_properties_path: ?[]const u8 = null,
    data_labels_path: ?[]const u8 = null,
    fault_severity_path: ?[]const u8 = null,
    generator_pack_path: ?[]const u8 = null,
    report_format: ?[]const u8 = null,
    /// ISO-8601 build timestamp injected as `__BUILD_TIME__` during TS strip.
    /// When null, compileHandler falls back to the wall clock at compile time.
    build_time: ?[]const u8 = null,
    /// Short git commit hash injected as `__GIT_COMMIT__` during TS strip.
    /// When null, compileHandler falls back to the string "unknown".
    git_commit: ?[]const u8 = null,
};

pub fn parsePrecompileArgs(args_vector: std.process.Args) !PrecompileOptions {
    const allocator = std.heap.smp_allocator;
    const argv = try collectArgs(allocator, args_vector);
    defer {
        for (argv) |arg| allocator.free(arg);
        allocator.free(argv);
    }
    return try parsePrecompileArgSlice(argv[1..]);
}

pub fn parsePrecompileArgSlice(argv: []const []const u8) !PrecompileOptions {
    var opts = PrecompileOptions{ .handler_path = "", .output_path = "" };
    var handler_path: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;

    var index: usize = 0;
    while (index < argv.len) : (index += 1) {
        const arg = argv[index];
        if (std.mem.eql(u8, arg, "--aot")) {
            opts.emit_aot = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--verify")) {
            opts.emit_verify = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--generate-tests")) {
            opts.generate_tests = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--contract")) {
            opts.emit_contract = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--openapi")) {
            opts.emit_openapi = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--sdk")) {
            opts.sdk_target = try takeArg(&index, argv, "Missing target after --sdk (values: ts)");
            continue;
        }
        if (std.mem.eql(u8, arg, "--sql-schema")) {
            opts.sql_schema_path = try takeArg(&index, argv, "Missing path after --sql-schema");
            continue;
        }
        if (std.mem.eql(u8, arg, "--system")) {
            opts.system_path = try takeArg(&index, argv, "Missing path after --system");
            continue;
        }
        if (std.mem.eql(u8, arg, "--policy")) {
            opts.policy_path = try takeArg(&index, argv, "Missing path after --policy");
            continue;
        }
        if (std.mem.eql(u8, arg, "--replay")) {
            opts.replay_trace_path = try takeArg(&index, argv, "Missing path after --replay");
            continue;
        }
        if (std.mem.eql(u8, arg, "--test-file")) {
            opts.test_file_path = try takeArg(&index, argv, "Missing path after --test-file");
            continue;
        }
        if (std.mem.eql(u8, arg, "--prove")) {
            opts.prove_spec = try takeArg(&index, argv, "Missing spec after --prove (format: contract.json or contract.json:traces.jsonl)");
            continue;
        }
        if (std.mem.eql(u8, arg, "--manifest")) {
            opts.manifest_path = try takeArg(&index, argv, "Missing path after --manifest");
            continue;
        }
        if (std.mem.eql(u8, arg, "--expect-properties")) {
            opts.expect_properties_path = try takeArg(&index, argv, "Missing path after --expect-properties");
            continue;
        }
        if (std.mem.eql(u8, arg, "--data-labels")) {
            opts.data_labels_path = try takeArg(&index, argv, "Missing path after --data-labels");
            continue;
        }
        if (std.mem.eql(u8, arg, "--fault-severity")) {
            opts.fault_severity_path = try takeArg(&index, argv, "Missing path after --fault-severity");
            continue;
        }
        if (std.mem.eql(u8, arg, "--generator-pack")) {
            opts.generator_pack_path = try takeArg(&index, argv, "Missing path after --generator-pack");
            continue;
        }
        if (std.mem.eql(u8, arg, "--report")) {
            opts.report_format = try takeArg(&index, argv, "Missing format after --report (values: json)");
            continue;
        }
        if (std.mem.eql(u8, arg, "--build-time")) {
            opts.build_time = try takeArg(&index, argv, "Missing value after --build-time");
            continue;
        }
        if (std.mem.eql(u8, arg, "--git-commit")) {
            opts.git_commit = try takeArg(&index, argv, "Missing value after --git-commit");
            continue;
        }
        if (std.mem.eql(u8, arg, "--module-manifest")) {
            _ = try takeArg(&index, argv, "Missing path after --module-manifest");
            continue;
        }
        if (handler_path == null) {
            handler_path = arg;
            continue;
        }
        if (output_path == null) {
            output_path = arg;
            continue;
        }
        errPrint("Unexpected argument: {s}\n", .{arg});
        return error.InvalidArgument;
    }

    const usage = "Usage: precompile [--aot] [--verify] [--contract] [--openapi] [--sdk ts] [--sql-schema path] [--system path] [--prove spec] [--policy policy.json] [--module-manifest path] <handler.ts> <output.zig>\n";
    opts.handler_path = handler_path orelse {
        errPrint(usage, .{});
        errPrint("\nCompiles a TypeScript/JavaScript handler to bytecode.\n", .{});
        return error.MissingArgument;
    };
    opts.output_path = output_path orelse {
        errPrint(usage, .{});
        errPrint("\nMissing output path.\n", .{});
        return error.MissingArgument;
    };

    return opts;
}

/// Scan argv for occurrences of `--module-manifest <path>`. Returns a
/// newly-allocated slice of borrowed argv slices. The caller owns the outer
/// slice (free with allocator.free) but the inner strings stay alive as long
/// as argv does.
pub fn collectModuleManifestPaths(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
) ![]const []const u8 {
    var out: std.ArrayList([]const u8) = .empty;
    errdefer out.deinit(allocator);

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        if (std.mem.eql(u8, argv[i], "--module-manifest")) {
            try out.append(allocator, try takeArg(&i, argv, "Missing path after --module-manifest"));
        }
    }
    return out.toOwnedSlice(allocator);
}

/// Build a manifest registry from a list of file paths. Each path is read,
/// validated via the existing parser (fail-loud on schema errors), and
/// registered. Paths resolve relative to cwd, matching every other
/// path-taking flag in the precompile CLI.
pub fn buildManifestRegistryFromPaths(
    allocator: std.mem.Allocator,
    paths: []const []const u8,
) !zigts.manifest_registry.Registry {
    var registry = zigts.manifest_registry.Registry.init(allocator);
    errdefer registry.deinit();

    for (paths) |path| {
        const bytes = readFilePosix(allocator, path, 1024 * 1024) catch |err| {
            errPrint("Error reading module manifest '{s}': {}\n", .{ path, err });
            return err;
        };
        defer allocator.free(bytes);

        var manifest = zigts.module_manifest.parse(allocator, bytes) catch |err| {
            errPrint("Error parsing module manifest '{s}': {}\n", .{ path, err });
            return err;
        };
        errdefer manifest.deinit(allocator);

        registry.register(manifest) catch |err| {
            errPrint("Error registering module manifest '{s}': {}\n", .{ path, err });
            return err;
        };
    }

    return registry;
}

test "parsePrecompileArgSlice captures build_time and git_commit" {
    const argv = [_][]const u8{
        "--build-time",
        "2026-05-25T12:34:56Z",
        "--git-commit",
        "abc1234",
        "handler.ts",
        "embedded_handler.zig",
    };
    const opts = try parsePrecompileArgSlice(&argv);
    try std.testing.expectEqualStrings("2026-05-25T12:34:56Z", opts.build_time.?);
    try std.testing.expectEqualStrings("abc1234", opts.git_commit.?);
    try std.testing.expectEqualStrings("handler.ts", opts.handler_path);
}

test "parsePrecompileArgSlice rejects missing build_time value" {
    const argv = [_][]const u8{ "--build-time", "handler.ts", "out.zig", "--build-time" };
    try std.testing.expectError(error.MissingArgument, parsePrecompileArgSlice(&argv));
}

test "parsePrecompileArgSlice consumes module manifest flags" {
    const argv = [_][]const u8{
        "--module-manifest",
        "zigttp-module.json",
        "handler.ts",
        "embedded_handler.zig",
    };
    const opts = try parsePrecompileArgSlice(&argv);
    try std.testing.expectEqualStrings("handler.ts", opts.handler_path);
    try std.testing.expectEqualStrings("embedded_handler.zig", opts.output_path);
}

test "parsePrecompileArgSlice rejects missing module manifest path" {
    const argv = [_][]const u8{ "--module-manifest", "handler.ts", "embedded_handler.zig", "--module-manifest" };
    try std.testing.expectError(error.MissingArgument, parsePrecompileArgSlice(&argv));
}

pub fn collectArgs(allocator: std.mem.Allocator, args_vector: std.process.Args) ![]const []const u8 {
    var args_iter = std.process.Args.Iterator.init(args_vector);
    defer args_iter.deinit();

    var args = std.ArrayList([]const u8).empty;
    errdefer args.deinit(allocator);

    while (args_iter.next()) |arg| {
        try args.append(allocator, try allocator.dupe(u8, arg));
    }
    return args.toOwnedSlice(allocator);
}
