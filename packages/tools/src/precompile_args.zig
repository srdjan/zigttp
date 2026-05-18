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
            index += 1;
            opts.sdk_target = if (index < argv.len) argv[index] else {
                errPrint("Missing target after --sdk (values: ts)\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--sql-schema")) {
            index += 1;
            opts.sql_schema_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --sql-schema\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--system")) {
            index += 1;
            opts.system_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --system\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--policy")) {
            index += 1;
            opts.policy_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --policy\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--replay")) {
            index += 1;
            opts.replay_trace_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --replay\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--test-file")) {
            index += 1;
            opts.test_file_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --test-file\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--prove")) {
            index += 1;
            opts.prove_spec = if (index < argv.len) argv[index] else {
                errPrint("Missing spec after --prove (format: contract.json or contract.json:traces.jsonl)\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--manifest")) {
            index += 1;
            opts.manifest_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --manifest\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--expect-properties")) {
            index += 1;
            opts.expect_properties_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --expect-properties\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--data-labels")) {
            index += 1;
            opts.data_labels_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --data-labels\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--fault-severity")) {
            index += 1;
            opts.fault_severity_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --fault-severity\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--generator-pack")) {
            index += 1;
            opts.generator_pack_path = if (index < argv.len) argv[index] else {
                errPrint("Missing path after --generator-pack\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--report")) {
            index += 1;
            opts.report_format = if (index < argv.len) argv[index] else {
                errPrint("Missing format after --report (values: json)\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--module-manifest")) {
            index += 1;
            if (index >= argv.len) {
                errPrint("Missing path after --module-manifest\n", .{});
                return error.MissingArgument;
            }
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
            i += 1;
            if (i >= argv.len) {
                errPrint("Missing path after --module-manifest\n", .{});
                return error.MissingArgument;
            }
            try out.append(allocator, argv[i]);
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
